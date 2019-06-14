//
//
//  UNIT      : Contains TDailyDiversionGaugeSQLAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 13/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionGaugeSQLAgent;

interface
uses
  Classes,
  VCL.Controls,
  UDailyDiversionGaugeData,
  UAbstractObject;
type
  TDailyDiversionGaugeSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetLastStationIDSQL : string;
    function GetLastMaxDailyDataIDSQL(AStationID : integer) : string;
    function GetLastMaxWRYMDataSQL(AStationID : integer) : string;

    function GetLastMaxDailyInstreamDataIDSQL(AStationID : integer) : string;
    function GetLastStationID : integer;
    function GetLastMaxDailyDataID(AStationID : integer) : integer;
    function GetLastMaxWRYMDataID(AStationID : integer) : integer;


    function InsertDailyDiversionStationSQL(AStationNo : string;AStationID : integer) : string;
    function InsertDailyDiversionCompensationValuesSQL(AStationID : integer): string;
    function InsertDailyDiversionThresholdValuesSQL(AStationID : integer): string;

    function RenameDailyDiversionStationSQL(ANewStationNo:string) : string;

    {function RenameDailyDiversionCompensationValuesSQL(ANewStationNo:string) : string;
    function RenameDailyDiversionFileDataSQL(ANewStationNo:string) : string;
    function RenameDailyDiversionFlowRelationshipSQL(ANewStationNo:string) : string;
    function RenameDailyDiversionWRYMDataSQL(ANewStationNo:string) : string;
    function RenameDailyInstreamFileDataSQL(ANewStationNo:string) : string;
    }
    function InsertDailyDataSQL(AStationID : integer; AIdentifier : integer): string;
    function InsertDailyInstreamDataSQL: string;
    function DeleteDailyDiversionStationSQL(AStationID : integer) : string;
    function DeleteFlowDiversionRelationSQL(AStationID : integer) : string;
    function DeleteDailyDiversionWRYMDataSQL(AStationID : integer) : string;

    function DeleteDailyDiversionCompensationValuesSQL(AStationID : integer) : string;
    function DeleteDailyDiversionThresholdValuesSQL(AStationID : integer) : string;
    function DeleteDailyDataSQL(AStationID : integer): string;
    function DeleteDailyInstreamDataSQL(AStationID : integer): string;
    function DeleteDailyDataRecordSQL(AStationID : integer;AIdentifier: integer): string;
    function DeleteDailyInstreamDataRecordSQL(AStationID : integer;AIdentifier: integer): string;
    function GetDailyDiversionStationByStationNoSQL(AStationNo : string) : string;
    function GetDiversionFeaturesSQL : string;
    function GetDiversionFeaturesType2SQL(AIdetifier : integer) : string;
    function InsertRelationshipDataSQL : string;
    function DeleteWRYMDataSQL(AIdentifier : integer) : string;
    function InsertWRYMDataSQL : string;
    function UpdateWRYMDataSQL(AIdentifier,ADiversionCode : integer) : string;
  public
    function GetExclSusReferencePeriod(AStationID : integer) : string;    
    function GetDailyDiversionStationSQL : string;
    function GetLastMaxDailyInstreamDataID(AStationID : integer) : integer;
    function GetDailyDiversionStationByIDSQL(AStationID : integer): string;
    function GetStartEndDateSQL(AStationID : integer): string;
    function GetCompensationValuesSQL(AStationID : integer): string;
    function GetThresholdValuesSQL(AStationID : integer) : string;
    function GetDailyDiversionFileDataSQL(AStationID : integer) : string;
    function GetDailyInstreamFileDataSQL(AStationID : integer) : string;
    function GetDailyDiversionWRYMDataSQL(AStationID : integer) : string;
    function InsertDailyDiversionStation(AStationNo : string; var AStationID : integer) : boolean;
    function RenameDailyDiversionStation(ANewStationNo: string; AOldStationNo: string): boolean;
    function InsertDailyData(AStationID: integer; var AIdentifier : integer): boolean;
    function InsertDailyInstreamData(AStationID: integer; AIdentifier : integer;ADate : TDateTime; AAvgFlow : double; QCode : integer): boolean;
    function DeleteDailyDiversionStation(AStationID : integer): boolean;
    function InsertDailyDiversionFlowUnrankedRelationshipSQL(AStationID : integer) : string;
    function GetRankedFlowDiversionRelationshipSQL(AStationID : integer) : string;
    function GetUnRankedFlowDiversionRelationSQL(AStationID : integer) : string;

    function GetDiversionPeriod(AStationID : integer) : string;
    function GetReferencePeriod(AStationID : integer) : string;

    function DeleteDailyDataRecord(AStationID : integer;AIdentifier : integer): boolean;
    function DeleteDailyInstreamDataRecord(AStationID : integer;AIdentifier : integer): boolean;
    function DeleteDailyInstreamData(AStationID : integer): boolean;
    function DeleteDailyFlowDataFromCSVFile(AStationID : integer): boolean;
    function DeleteFlowDiversionRelation(AStationID : integer): boolean;
    function WRYMDataIsLoaded(AChanelNames : TStringlist) : boolean;
    function InsertRelationshipData(AStationID : integer; var AID : integer) : boolean;
    function InsertWRYMData(ADiversionGauge : TDiversionGauge;AIdentifier : integer) : boolean;
    procedure LoadStationContextData(AContextData : TStringList;AStationID : integer);
    procedure LoadContextData_DailyDataFlow(AContextData : TStringList;AIdentifier,AStationID : string);
    procedure LoadCompensationContextData(AContextData: TStringList; AStationID, AFieldNameID: string);

end;

implementation
uses
  System.UITypes,
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  VCL.Dialogs,
//  UDailyDiversionDataObject,
  UErrorHandlingOperations, DB;
  { TDailyDiversionGaugeSQLAgent }

function TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionStation(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionStation';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteDailyDiversionStationSQL(AStationID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDataSQL(AStationID));
        LDataset.ExecSQL;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyInstreamDataSQL(AStationID));
        LDataset.ExecSQL;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDiversionCompensationValuesSQL(AStationID));
        LDataset.ExecSQL;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDiversionThresholdValuesSQL(AStationID));
        LDataset.ExecSQL;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteFlowDiversionRelationSQL(AStationID));
        LDataset.ExecSQL;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDiversionWRYMDataSQL(AStationID));
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyDataRecord(AStationID : integer;AIdentifier : integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDataRecord';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDataRecordSQL(AStationID,AIdentifier));
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamDataRecord(AStationID : integer;AIdentifier : integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamDataRecord';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyInstreamDataRecordSQL(AStationID,AIdentifier));
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteFlowDiversionRelation(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteFlowDiversionRelation';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteFlowDiversionRelationSQL(AStationID));
        LDataset.ExecSQL;
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDiversionWRYMDataSQL(AStationID));
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamData(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamData';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyInstreamDataSQL(AStationID));
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyFlowDataFromCSVFile(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyFlowDataFromCSVFile';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteDailyDataSQL(AStationID));
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.WRYMDataIsLoaded(AChanelNames : TStringlist) : boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.WRYMDataIsLoaded';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) and Assigned(AChanelNames) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(GetDiversionFeaturesSQL);
        LDataset.DataSet.Open;
        while not LDataset.DataSet.Eof do
        begin
          AChanelNames.Add(Trim(LDataSet.DataSet.FieldByName('Identifier').AsString) +' '+
                           Trim(LDataSet.DataSet.FieldByName('DivChannelNumber').AsString) +' '+
                           Trim(LDataSet.DataSet.FieldByName('DivChannelName').AsString) +' '+
                           Trim(LDataSet.DataSet.FieldByName('DivChannelType').AsString));
                           
          LDataset.DataSet.Next;
        end;
        Result := (AChanelNames.Count > 0);
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertWRYMData(ADiversionGauge : TDiversionGauge;AIdentifier : integer) : boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertWRYMData';
var
  LDataSet : TAbstractModelDataset;
  LWRYMChannelData : TWRYMChannelData;
  LCount,
  LIndex : integer;
  LUpdateWRYMDataSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) and (ADiversionGauge <> nil) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(DeleteWRYMDataSQL(AIdentifier));
        LDataset.ExecSQL;

        LDataSet.ClearSQL;
        LDataSet.SetSQL(InsertWRYMDataSQL);
        for LIndex := 1 to 2 do
        begin

          LDataset.ClearQueryParams();
          LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','DiversionCode'],
          [CYield,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(AIdentifier), IntToStr(LIndex)]);
          LDataset.ExecSQL;
        end;
        for LIndex := 1 to 2 do
        begin
          LDataSet.ClearSQL;
          LUpdateWRYMDataSQL := UpdateWRYMDataSQL(AIdentifier,LIndex);
          LDataSet.SetSQL(LUpdateWRYMDataSQL);
          for LCount := MinMonths to MaxMonths do
          begin
            LWRYMChannelData := ADiversionGauge.WRYMDataByIndex[LCount-1];
            if LWRYMChannelData <> nil then
            begin
              if LIndex = 1 then
                LDataSet.SetParamValue(Format('ADivFactor%2.2d',[LCount]),FloatToStr(LWRYMChannelData.ReferenceFlow),ftFloat)
              else
                LDataSet.SetParamValue(Format('ADivFactor%2.2d',[LCount]),FloatToStr(LWRYMChannelData.DiversionFlow),ftFloat);
            end;
          end;
          LDataSet.ExecSQL;
        end;

        end;
        Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertRelationshipData(AStationID : integer; var AID : integer) : boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertRelationshipData';
var
  LDataSet    : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        AID := GetLastMaxWRYMDataID(AStationID) + 1;
        LDataSet.ClearSQL;
        LDataSet.SetSQL(InsertRelationshipDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
        ['Model','StudyAreaName','SubArea','Scenario','Identifier','StationID'],
        [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
        FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
        IntToStr(AID), IntToStr(AStationID)]);
        LDataset.ExecSQL;
        Result := True;
      end;
    finally
      FreeAndNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionGaugeSQLAgent.GetDiversionFeaturesType2SQL(AIdetifier : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDiversionFeaturesType2SQL';
begin
  Result := '';
  try
    Result := ' SELECT Identifier,DiversionCode,DivFactor01,DivFactor02,'+
              ' DivFactor03,DivFactor04,DivFactor05,DivFactor06,DivFactor07,DivFactor08,DivFactor09,'+
              ' DivFactor10,DivFactor11,DivFactor12 '+
              ' FROM DiversionFeaturesType1n2 WHERE '+
              ' (Model         = ' + QuotedStr(CYield)+ ') AND ' +
              ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' (Identifier = '+IntToStr(AIdetifier)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TDailyDiversionGaugeSQLAgent.UpdateWRYMDataSQL(AIdentifier,ADiversionCode : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.UpdateWRYMDataSQL';
var
  LIndex : integer;
begin
  Result := '';
  try
    Result := 'UPDATE DiversionFeaturesType1n2 SET ';

    for LIndex := MinMonths to MaxMonths do
    begin
      if LIndex = MaxMonths then
        Result := Result + Format('DivFactor%2.2d = :ADivFactor%2.2d WHERE ',[LIndex,LIndex])
      else
       Result := Result + Format('DivFactor%2.2d = :ADivFactor%2.2d , ',[LIndex,LIndex]);
    end;
    Result := Result +
              ' (Model         = ' + QuotedStr(CYield)+ ') AND ' +
              ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' (Identifier = '+IntToStr(AIdentifier)+') AND ' +
              ' (DiversionCode = '+IntToStr(ADiversionCode)+')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertWRYMDataSQL : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertWRYMDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DiversionFeaturesType1n2 '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier,DiversionCode' +
              ' ) VALUES '+
              ' ( :Model, :StudyAreaName, :SubArea, :Scenario, :Identifier,:DiversionCode )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertRelationshipDataSQL: string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertRelationshipDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DailyDiversionWRYMData '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier,StationID' +
              ' ) VALUES '+
              ' ( :Model, :StudyAreaName, :SubArea, :Scenario, :Identifier,:StationID )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionGaugeSQLAgent.DeleteWRYMDataSQL(AIdentifier : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteWRYMDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM DiversionFeaturesType1n2 WHERE '+
              ' (Model         = ' + QuotedStr(CYield)+ ') AND ' +
              ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' (Identifier = '+IntToStr(AIdentifier)+')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDiversionFeaturesSQL : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDiversionFeaturesSQL';
begin
  Result := '';
  try
    Result := ' SELECT Identifier,DivChannelName,DivChannelNumber,DivChannelType FROM DiversionFeatures WHERE '+
      ' (Model         = ' + QuotedStr(CYield)+ ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
      ' (DivChannelType in (2,4))';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionStationSQL(AStationID: integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionStationSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionStation WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteFlowDiversionRelationSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteFlowDiversionRelationSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionFlowRelationship WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionWRYMDataSQL(AStationID :integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionWRYMDataSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionWRYMData WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionCompensationValuesSQL(AStationID:integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionCompensationValuesSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionCompensationValues WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionThresholdValuesSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDiversionThresholdValuesSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionMonthlyThreshold WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyDataSQL(AStationID:integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDataSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionFileData WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamDataSQL(AStationID:integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamDataSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyInstreamFileData WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyDataRecordSQL(AStationID : integer;AIdentifier: integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyDataRecordSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyDiversionFileData WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')'+
              ' AND (Identifier = '+IntToStr(AIdentifier)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamDataRecordSQL(AStationID : integer;AIdentifier: integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.DeleteDailyInstreamDataRecordSQL';
begin
  Result := '';
  try
    Result := ' DELETE * FROM DailyInstreamFileData WHERE '+
              GetScenarioWhereClause +
              ' AND (StationID = '+IntToStr(AStationID)+')'+
              ' AND (Identifier = '+IntToStr(AIdentifier)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDailyDiversionStationByStationNoSQL(AStationNo: string): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDailyDiversionStationByStationNoSQL';
begin
  Result := '';
  try
    Result := GetDailyDiversionStationSQL + ' AND (StationNo = '+QuotedStr(AStationNo)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDailyDiversionStationSQL: string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDailyDiversionStationSQL';
begin
  Result := '';
  try
    Result := ' SELECT  Place,Latitude,Longitude,CatchmentArea,StationNo,StationID, CatchmentScaleFactor ' +
              ' FROM DailyDiversionStation ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDailyDiversionStationByIDSQL(AStationID : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDailyDiversionStationByIDSQL';
begin
  Result := '';
  try
    Result := ' SELECT  Place,Latitude,Longitude,CatchmentArea,StationNo,StationID, CatchmentScaleFactor ' +
              ' FROM DailyDiversionStation ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)                   +' )';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionGaugeSQLAgent.GetCompensationValuesSQL(AStationID : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetCompensationValuesSQL';
begin
  Result := '';
  try
    Result := ' SELECT  CapacityOfDiversion,ScaleFactor,StartDate,EndDate,Value01,Value02,Value03, ' +
              ' Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12 ' +
              ' FROM DailyDiversionCompensationValues ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID) +')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetThresholdValuesSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetThresholdValuesSQL';
begin
  Result := '';
  try
    Result := ' SELECT  Value01,Value02,Value03, ' +
              ' Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12 ' +
              ' FROM DailyDiversionMonthlyThreshold ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID) +')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionGaugeSQLAgent.GetDailyDiversionFileDataSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDailyDiversionFileDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT  StationID, Identifier, DiversionDate,AvgFlow, QualityCode' +
              ' FROM DailyDiversionFileData ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' ) ORDER BY DiversionDate ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDailyInstreamFileDataSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDailyInstreamFileDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT  StationID, Identifier, InstreamDate, AvgFlow, QualityCode' +
              ' FROM DailyInstreamFileData ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' ) ORDER BY InstreamDate ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDailyDiversionWRYMDataSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDailyDiversionWRYMDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT  StationID, Identifier, ReferenceFlow, RefFlowValueEdited, DiversionFlow,DivFlowValueedited,' +
              ' NonDiversionFlow,NonDivFlowValueEdited '+
              ' FROM DailyDiversionWRYMData ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' ) ORDER BY ReferenceFlow ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetLastStationID: integer;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastStationID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetLastStationIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxStationID').AsInteger;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetLastMaxDailyDataID(AStationID : integer) : integer;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastMaxDailyDataID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetLastMaxDailyDataIDSQL(AStationID));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetLastMaxWRYMDataID(AStationID : integer) : integer;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastMaxWRYMDataID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetLastMaxWRYMDataSQL(AStationID));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetLastMaxDailyInstreamDataID(AStationID : integer) : integer;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastMaxDailyInstreamDataID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetLastMaxDailyInstreamDataIDSQL(AStationID));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDailyDiversionGaugeSQLAgent.GetLastStationIDSQL: string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastStationIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(StationID) AS MaxStationID FROM DailyDiversionStation WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetStartEndDateSQL(AStationID : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetStartEndDateSQL';
begin
  Result := '';
  try
    Result := 'SELECT MIN(DiversionDate) AS StartDate,MAX(DiversionDate) AS EndDate FROM DailyDiversionFileData WHERE ' +
              GetScenarioWhereClause +
              ' AND (stationID = '+ IntToStr(AStationID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDailyDiversionGaugeSQLAgent.GetLastMaxDailyDataIDSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastMaxDailyDataIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxID FROM DailyDiversionFileData WHERE ' +
              GetScenarioWhereClause +
              ' AND (stationID = '+ IntToStr(AStationID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetLastMaxWRYMDataSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastMaxWRYMDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxID FROM DailyDiversionWRYMData WHERE ' +
              GetScenarioWhereClause +
              ' AND (stationID = '+ intToStr(AStationID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetLastMaxDailyInstreamDataIDSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetLastMaxDailyInstreamDataIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxID FROM DailyInstreamFileData WHERE ' +
              GetScenarioWhereClause +
              ' AND (stationID = '+ IntToStr(AStationID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionGaugeSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetScenarioWhereClause';
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

function TDailyDiversionGaugeSQLAgent.InsertDailyDiversionStation(AStationNo: string; var AStationID : integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyDiversionStation';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetDailyDiversionStationByStationNoSQL(AStationNo));
        LDataset.DataSet.Open;
        if not (LDataset.DataSet.FieldByName('StationNo').IsNull) then
        begin
          MessageDlg(FAppModules.Language.GetString('Message.StationNoExist'),mtWarning,[mbOK],0);
          Exit;
        end;
        AStationID := GetLastStationID + 1;
        LDataset.DataSet.Close;
        LSQL := InsertDailyDiversionStationSQL(AStationNo, AStationID);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LDataset.DataSet.Close;
        LDataset.ClearSQL;
        LSQL := InsertDailyDiversionCompensationValuesSQL(AStationID);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LDataset.DataSet.Close;
        LDataset.ClearSQL;
        LSQL := InsertDailyDiversionThresholdValuesSQL(AStationID);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.RenameDailyDiversionStation(ANewStationNo: string; AOldStationNo: string): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.RenameDailyDiversionStation';
var
  LDataSet    : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := RenameDailyDiversionStationSQL(AOldStationNo);
        LDataSet.SetSQL(LSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['ANewStationNo'],[ANewStationNo]);
        LDataset.ExecSQL;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyData(AStationID: integer; var AIdentifier : integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyData';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        AIdentifier := GetLastMaxDailyDataID(AStationID) + 1;
        LDataset.DataSet.Close;
        LSQL := InsertDailyDataSQL(AStationID, AIdentifier);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;
        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyInstreamData(AStationID: integer; AIdentifier : integer;ADate : TDateTime; AAvgFlow : double; QCode : integer): boolean;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyInstreamData';
var
  LDataSet    : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        LDataSet.SetSQL(InsertDailyInstreamDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','StationID'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(AIdentifier), IntToStr(AStationID)]);
          LDataSet.SetParams(['InstreamDate'],[FormatDateTime('yyyy/mm/dd', ADate)]);
          if AAvgFlow <> NullFloat then
            LDataSet.SetParams(['AvgFlow'],[FloatToStr(AAvgFlow)]);

          LDataSet.SetParams(['QualityCode'],[IntToStr(QCode)]);
        LDataset.ExecSQL;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TDailyDiversionGaugeSQLAgent.GetRankedFlowDiversionRelationshipSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetRankedFlowDiversionRelationshipSQL';
begin
  Result := '';
  try
    Result := ' SELECT  StationID, Identifier, RelationshipDate, ReferenceFlow, DiversionFlow,NonDiversionFlow' +
              ' FROM DailyDiversionFlowRelationship ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( ReferenceFlow > 0 ) AND ' +
              ' ( DiversionFlow > 0 ) AND ' +
              ' ( ReferenceFlow is not null ) AND' +
              ' ( DiversionFlow is not null ) AND' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' ) ORDER BY ReferenceFlow,DiversionFlow ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetUnRankedFlowDiversionRelationSQL(AStationID : Integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetUnRankedFlowDiversionRelationSQL';
begin
  Result := '';
  try
    Result := ' SELECT  StationID, Identifier, RelationshipDate, ReferenceFlow, DiversionFlow,NonDiversionFlow' +
              ' FROM DailyDiversionFlowRelationship ' +
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' ) ORDER BY Identifier ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetDiversionPeriod(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetDiversionPeriod';
begin
  Result := '';
  try
    Result := ' SELECT Min(InstreamDate) AS DivStartDate, Max(InstreamDate) AS DivEndDate '+
              ' FROM  DailyInstreamFileData'+
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' )';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetReferencePeriod(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetReferencePeriod';
begin
  Result := '';
  try
    Result := ' SELECT Min(DiversionDate) AS RefStartDate, Max(DiversionDate) AS RefEndDate '+
              ' FROM DailyDiversionFileData '+
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ' )';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.GetExclSusReferencePeriod(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.GetExclSusReferencePeriod';
begin
  Result := '';
  try
    Result := ' SELECT Min(DiversionDate) AS RefStartDate, Max(DiversionDate) AS RefEndDate '+
              ' FROM DailyDiversionFileData '+
              ' WHERE ' +
              ' ( Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              ' ( StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              ' ( SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              ' ( Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ( StationID     = ' + IntToStr(AStationID)  + ') AND '+
              ' ( QualityCode in (1,2,3,4,5,6,7,50,60,65,66,91,150) )';


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyDiversionFlowUnrankedRelationshipSQL(AStationID : integer) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyDiversionFlowUnrankedRelationshipSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DailyDiversionFlowRelationship '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier,StationID,RelationshipDate,ReferenceFlow,DiversionFlow,NonDiversionFlow' +
              ' ) VALUES '+
              ' ( :Model, :StudyAreaName, :SubArea, :Scenario, :Identifier,:StationID,:RelationshipDate,:ReferenceFlow,:DiversionFlow,:NonDiversionFlow )';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyDiversionStationSQL(AStationNo : string;AStationID : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyDiversionStationSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DailyDiversionStation '+
              '(Model, StudyAreaName, SubArea, Scenario, StationNo, StationID,CatchmentScaleFactor' +
              ' ) ' +
              'VALUES ('+
    QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
    QuotedStr(AStationNo)+','+
    IntToStr(AStationID)+',1.0)';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyDiversionCompensationValuesSQL(AStationID : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyDiversionCompensationValuesSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DailyDiversionCompensationValues '+
              '(Model, StudyAreaName, SubArea, Scenario, StationID,ScaleFactor' +
              ' ) ' +
              'VALUES ('+
    QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
    IntToStr(AStationID)+',1.0)';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyDiversionThresholdValuesSQL(AStationID : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyDiversionThresholdValuesSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DailyDiversionMonthlyThreshold '+
              '(Model, StudyAreaName, SubArea, Scenario, StationID,value01,' +
              ' value02,value03,value04,value05,value06,value07,value08,value09,value10,value11,value12 ) ' +
              'VALUES ('+
    QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
    IntToStr(AStationID)+',7,7,7,7,7,7,7,7,7,7,7,7)';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.RenameDailyDiversionStationSQL(ANewStationNo:string) : string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.RenameDailyDiversionStationSQL';
begin
  Result := '';
  try
    Result := 'UPDATE DailyDiversionStation '+
              ' Set StationNo = :ANewStationNo WHERE ' +
              GetScenarioWhereClause +
              ' AND (StationNo = '+QuotedStr(ANewStationNo)+')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyDataSQL(AStationID: integer; AIdentifier : integer): string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO DailyDiversionFileData '+
              '(Model, StudyAreaName, SubArea, Scenario, StationID, Identifier,DiversionDate' +
              ' ) ' +
              'VALUES ('+
    QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
    QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
    IntToStr(AStationID)+','+
    IntToStr(AIdentifier)+','+
    FormatDateTime('yyyy/mm/dd', Now)+')';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeSQLAgent.InsertDailyInstreamDataSQL: string;
const OPNAME = 'TDailyDiversionGaugeSQLAgent.InsertDailyInstreamDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DailyInstreamFileData '+
              ' (Model,StudyAreaName,SubArea,Scenario,StationID,Identifier,InstreamDate,AvgFlow,QualityCode)'+
              ' VALUES '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:StationID,:Identifier,:InstreamDate,:AvgFlow,:QualityCode)';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionGaugeSQLAgent.LoadStationContextData(AContextData : TStringList;AStationID :integer);
const OPNAME = 'TDailyDiversionGaugeSQLAgent.LoadStationContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('StationID='    + IntToStr(AStationID));

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDailyDiversionGaugeSQLAgent.LoadContextData_DailyDataFlow(AContextData : TStringList;AIdentifier,AStationID : string);
const OPNAME = 'TDailyDiversionGaugeSQLAgent.LoadContextData_DailyDataFlow';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AIdentifier);
    AContextData.Add('StationID='          + AStationID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDailyDiversionGaugeSQLAgent.LoadCompensationContextData(AContextData: TStringList; AStationID, AFieldNameID: string);
const OPNAME = 'TDailyDiversionGaugeSQLAgent.LoadCompensationContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('StationID='          + AStationID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
