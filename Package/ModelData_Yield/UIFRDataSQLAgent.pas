//
//
//  UNIT      : Contains TIFRDataSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRDataSQLAgent;

interface
uses
  Classes,
  VCL.Controls,
  UIFRDataObject,
  UAbstractObject;
type
  TIFRDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    function GetMaxIFRDataID: integer;
    function GetIFRDataSQL : string;
    function GetIFRFlowDataSQL : string;
    function SaveIFRData(AIFRSiteData:TIFRSiteDataObject):boolean;
    function DeleteIFRData(AIFRSiteData:TIFRSiteDataObject):boolean;
end;

implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  VCL.Dialogs,
  UErrorHandlingOperations, DB;
  { TIFRDataSQLAgent }

function TIFRDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TIFRDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    if FAppModules.StudyArea.ModelCode = CYield then
      Result :=
        ' (Model         = ' + QuotedStr('IFRPreProcessor')     + ') AND ' +
        ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ')'
    else
      Result :=
        ' (Model         = ' + QuotedStr('IFRPreProcessor')     + ') AND ' +
        ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
        ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
        ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRDataSQLAgent.GetMaxIFRDataID: integer;
const OPNAME = 'TIFRDataSQLAgent.GetMaxIFRDataID';
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
        LSQL := 'SELECT MAX(Identifier) AS MaxID FROM IFRSite WHERE ' +
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

function TIFRDataSQLAgent.GetIFRDataSQL: string;
const OPNAME = 'TIFRDataSQLAgent.GetIFRDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, SiteName, SiteDescr,'+
              ' CSVFileName, QuaternaryCatchment, RiverName, AssociatedEMC, LevelDetail,'+
              ' LevelConfidence, Source, MonthNames, XCoord, YCoord '+
              ' FROM IFRSite WHERE '+ GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRDataSQLAgent.GetIFRFlowDataSQL: string;
const OPNAME = 'TIFRDataSQLAgent.GetIFRFlowDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, FlowIdentifier, ExceedProbability,'+
              ' Flow01, Flow02, Flow03, Flow04, Flow05, Flow06, Flow07, Flow08, Flow09, Flow10, Flow11, Flow12'+
              ' FROM IFRSiteFlows WHERE '+ GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRDataSQLAgent.DeleteIFRData(AIFRSiteData: TIFRSiteDataObject): boolean;
const OPNAME = 'TIFRDataSQLAgent.DeleteIFRData';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
begin
  Result := False;
  try
    if (AIFRSiteData = nil) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE * FROM IFRSite WHERE ' + GetScenarioWhereClause + ' AND Identifier = ' + IntToStr(AIFRSiteData.SiteIdentifier);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lSQL := 'DELETE * FROM IFRSiteFlows WHERE ' + GetScenarioWhereClause + ' AND Identifier = ' + IntToStr(AIFRSiteData.SiteIdentifier);
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

function TIFRDataSQLAgent.SaveIFRData(AIFRSiteData: TIFRSiteDataObject): boolean;
const OPNAME = 'TIFRDataSQLAgent.SaveIFRData';
var
  LDataSet: TAbstractModelDataset;
  LFieldName: string;
  LSQL: string;
  LRow,
  LCol: integer;
begin
  Result := False;
  try
    if (AIFRSiteData = nil) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        if not DeleteIFRData(AIFRSiteData) then Exit;
        FAppModules.Database.StartTransaction;
        try
          //Clear before insert

          lSQL := 'INSERT INTO IFRSite'+
                  ' (Model, StudyAreaName, SubArea, Scenario, Identifier, SiteName, SiteDescr, CSVFileName, QuaternaryCatchment,'+
                  ' RiverName, AssociatedEMC, LevelDetail, LevelConfidence, Source, MonthNames, XCoord, YCoord)'+
                  ' VALUES (:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :SiteName, :SiteDescr, :CSVFileName,'+
                  ' :QuaternaryCatchment, :RiverName, :AssociatedEMC, :LevelDetail, :LevelConfidence, :Source, :MonthNames,'+
                  ' :XCoord, :YCoord)';
          LDataSet.SetSQL(lSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(['Model'], ['IFRPreProcessor']);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(AIFRSiteData.SiteIdentifier)]);
          LDataSet.SetParams(['SiteName'], [AIFRSiteData.SiteName]);
          LDataSet.SetParams(['SiteDescr'], [AIFRSiteData.SiteDescription]);
          LDataSet.SetParams(['CSVFileName'], [AIFRSiteData.CSVFileName]);
          LDataSet.SetParams(['QuaternaryCatchment'], [AIFRSiteData.QuaternaryCatchment]);
          LDataSet.SetParams(['RiverName'], [AIFRSiteData.RiverName]);
          LDataSet.SetParams(['AssociatedEMC'], [AIFRSiteData.AssociatedEMC]);
          LDataSet.SetParams(['LevelDetail'], [AIFRSiteData.LevelOfDetail]);
          LDataSet.SetParams(['LevelConfidence'], [AIFRSiteData.LevelOfConfidence]);
          LDataSet.SetParams(['Source'], [AIFRSiteData.DataSource]);
          LDataSet.SetParams(['MonthNames'], [AIFRSiteData.MonthNamesCommaText]);
          LDataSet.SetParams(['XCoord'], [FloatTostr(AIFRSiteData.XCoord)]);
          LDataSet.SetParams(['YCoord'], [FloatTostr(AIFRSiteData.YCoord)]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

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
