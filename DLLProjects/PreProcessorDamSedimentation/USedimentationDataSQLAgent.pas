

unit USedimentationDataSQLAgent;

interface
uses
  Classes,
  VCL.Controls,
  UDamSedimentationDataObject,
  UAbstractObject;
type
  TSedimentationDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    function GetMaxDamSedimentationDataID: integer;
    function GetSediCapacitySurveyDataSQL : string;
    function GetSediAreaSurveyDataSQL : string;
    function GetDamSedimentationDataSQL : string;
    function SaveSedimentationData(ADamSedimentationData:TDamSedimentationDataObject):boolean;
    function DeleteSedimentationData(ADamSedimentationData:TDamSedimentationDataObject):boolean;
end;

implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  VCL.Dialogs,
  UErrorHandlingOperations, DB;
  { TSedimentationDataSQLAgent }

function TSedimentationDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TSedimentationDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    if FAppModules.StudyArea.ModelCode = CYield then
      Result :=
        ' (Model         = ' + QuotedStr('DamSedimentation')     + ') AND ' +
        ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ')'
    else
      Result :=
        ' (Model         = ' + QuotedStr('DamSedimentation')     + ') AND ' +
        ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
        ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
        ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationDataSQLAgent.GetMaxDamSedimentationDataID: integer;
const OPNAME = 'TSedimentationDataSQLAgent.GetMaxDamSedimentationDataID';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := 'SELECT MAX(Identifier) AS MaxID FROM DamSedimentation WHERE ' +
                GetScenarioWhereClause;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationDataSQLAgent.GetDamSedimentationDataSQL: string;
const OPNAME = 'TSedimentationDataSQLAgent.GetDamSedimentationDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier,DamCode, DamName, Source,'+
              ' SurveyFileName, XCoord, YCoord '+
              ' FROM DamSedimentation WHERE '+ GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationDataSQLAgent.GetSediCapacitySurveyDataSQL: string;
const OPNAME = 'TSedimentationDataSQLAgent.GetSediCapacitySurveyDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, DamIdentifier,Year_Time, DifferenceInYears, OriginalCapacity, CapacityReduction'+
              ' FROM DamSediCapacitySurvey WHERE '+ GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationDataSQLAgent.GetSediAreaSurveyDataSQL: string;
const OPNAME = 'TSedimentationDataSQLAgent.GetSediAreaSurveyDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, DamIdentifier,Year_Time, DifferenceInYears, AverageArea, IncreaseInArea,InitialArea'+
              ' FROM DamSediAreaSurvey WHERE '+ GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TSedimentationDataSQLAgent.DeleteSedimentationData(ADamSedimentationData:TDamSedimentationDataObject): boolean;
const OPNAME = 'TSedimentationDataSQLAgent.DeleteSedimentationData';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := False;
  try
    if (ADamSedimentationData = nil) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE * FROM DamSedimentation WHERE ' + GetScenarioWhereClause + ' AND Identifier = ' + IntToStr(ADamSedimentationData.DamIdentifier);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM DamSediCapacitySurvey WHERE ' + GetScenarioWhereClause + ' AND DamIdentifier = ' + IntToStr(ADamSedimentationData.DamIdentifier);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM DamSediAreaSurvey WHERE ' + GetScenarioWhereClause + ' AND DamIdentifier = ' + IntToStr(ADamSedimentationData.DamIdentifier);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationDataSQLAgent.SaveSedimentationData(ADamSedimentationData:TDamSedimentationDataObject): boolean;
const OPNAME = 'TSedimentationDataSQLAgent.SaveIFRData';
var
  LDataSet: TAbstractModelDataset;
  //LFieldName: string;
  LSQL: string;
 // LRow,
 // LCol: integer;
begin
  Result := False;
  try
    if (ADamSedimentationData = nil) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        if not DeleteSedimentationData(ADamSedimentationData) then Exit;
        FAppModules.Database.StartTransaction;
        try
          //Clear before insert

          lSQL := 'INSERT INTO DamSedimentation'+
                  ' (Model, StudyAreaName, SubArea, Scenario, Identifier, DamName, DamCode, SurveyFileName, Source, XCoord, YCoord)'+
                  ' VALUES (:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :DamName, :DamCode, :SurveyFileName,'+
                  ' :Source,  :XCoord, :YCoord)';
          LDataSet.SetSQL(lSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(['Model'], ['DamSedimentation']);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(ADamSedimentationData.DamIdentifier)]);
          LDataSet.SetParams(['DamName'], [ADamSedimentationData.DamName]);
          LDataSet.SetParams(['DamCode'], [ADamSedimentationData.DamCode]);
          LDataSet.SetParams(['SurveyFileName'], [ADamSedimentationData.SurveyFileName]);

          LDataSet.SetParams(['Source'], [ADamSedimentationData.DataSource]);

          LDataSet.SetParams(['XCoord'], [FloatTostr(ADamSedimentationData.XCoord)]);
          LDataSet.SetParams(['YCoord'], [FloatTostr(ADamSedimentationData.YCoord)]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
         {
          for LRow := 0 to High(AIFRSiteData.ExceedencePercentageArray) do
          begin
            lSQL := 'INSERT INTO IFRSiteFlows'+
                    ' (Model, StudyAreaName, SubArea, Scenario, Identifier, FlowIdentifier, ExceedProbability,'+
                    '  Flow01, Flow02, Flow03, Flow04, Flow05, Flow06, Flow07, Flow08, Flow09, Flow10, Flow11, Flow12)'+
                    ' VALUES (:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :FlowIdentifier, :ExceedProbability,'+
                    ' :Flow01, :Flow02, :Flow03, :Flow04, :Flow05, :Flow06, :Flow07, :Flow08, :Flow09, :Flow10, :Flow11, :Flow12)';
            LDataSet.SetSQL(lSQL);
            LDataset.ClearQueryParams();
            LDataSet.SetParams(['Model'], ['IFRPreProcessor']);
            LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LDataSet.SetParams(['Identifier'], [IntToStr(AIFRSiteData.SiteIdentifier)]);
            LDataSet.SetParams(['FlowIdentifier'], [IntToStr(LRow+1)]);
            LDataSet.SetParams(['ExceedProbability'], [FloatToStr(AIFRSiteData.ExceedencePercentageArray[LRow])]);
            for LCol := 1 to 12 do
            begin
              LFieldName := Format('%s%2.2d',['Flow',LCol]);
              LDataSet.SetParams([LFieldName], [FloatToStr(AIFRSiteData.RequiredFlowsArray[LRow,LCol-1])]);
            end;
            LDataset.ExecSQL;
            LDataset.DataSet.Close;
          end;
                                        }
          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
