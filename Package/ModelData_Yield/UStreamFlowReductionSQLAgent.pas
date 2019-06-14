//
//
//  UNIT      : Contains TStreamFlowReductionSQLAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 22/11/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//

unit UStreamFlowReductionSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TStreamFlowReductionSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetMaxIdentifierSQL       : string;
    function GetMaxNodeNumberSQL       : string;
    function DeleteStreamFlowReductionSQL(AIdentifier: Integer) : string;
    function InsertStreamFlowReductionSQL: string;
  public
    procedure LoadContextData(AContextData : TStringList; AIdentifier: string);
    function GetStreamFlowReductionSQL : string;

    function GetMaxIdentifier : Integer;
    function GetMaxNodeNumber : Integer;
    function InsertStreamFlowReduction(AStreamFlowReduction: IStreamFlowReduction): Boolean;
    function DeleteStreamFlowReduction(AStreamFlowReduction: IStreamFlowReduction): Boolean;
    function CopySFRFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             ASFRList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations, DB;

function TStreamFlowReductionSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TStreamFlowReductionSQLAgent.GetScenarioWhereClause';
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

procedure TStreamFlowReductionSQLAgent.LoadContextData(AContextData : TStringList; AIdentifier: string);
const OPNAME = 'TStreamFlowReductionSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStreamFlowReductionSQLAgent.GetStreamFlowReductionSQL: string;
const OPNAME = 'TStreamFlowReductionSQLAgent.GetStreamFlowReductionSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier, InflowNodeNumber,CoveredArea'+
              ', UnitRunoffFileName, SoilMoistureFileName, SFRName, SFRDescr '+
              ' FROM SFRSubCatchment WHERE '+
              GetScenarioWhereClause +
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.DeleteStreamFlowReductionSQL(AIdentifier: Integer): string;
const OPNAME = 'TStreamFlowReductionSQLAgent.DeleteStreamFlowReductionSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM SFRSubCatchment A WHERE ' +
                GetScenarioWhereClause +
                'AND Identifier = '+ IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.GetMaxIdentifierSQL: string;
const OPNAME = 'TStreamFlowReductionSQLAgent.GetMaxIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM SFRSubCatchment A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.GetMaxNodeNumberSQL: string;
const OPNAME = 'TStreamFlowReductionSQLAgent.GetMaxNodeNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(NodeNumber) AS MaxNodeNumber FROM SFRSubCatchment A WHERE' + GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.InsertStreamFlowReductionSQL: string;
const OPNAME = 'TStreamFlowReductionSQLAgent.InsertStreamFlowReductionSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO SFRSubCatchment '+
      ' (Model, StudyAreaName, SubArea, Scenario, Identifier, '+
      ' InflowNodeNumber,CoveredArea, UnitRunoffFileName, SoilMoistureFileName, SFRName, SFRDescr)'+
      ' VALUES ('+
      ' :Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, '+
      ' :InflowNodeNumber,:CoveredArea, :UnitRunoffFileName, :SoilMoistureFileName, :SFRName, :SFRDescr ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.GetMaxIdentifier: Integer;
const OPNAME = 'TStreamFlowReductionSQLAgent.GetMaxIdentifier';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxIdentifierSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.GetMaxNodeNumber: Integer;
const OPNAME = 'TStreamFlowReductionSQLAgent.GetMaxNodeNumber';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxNodeNumberSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxNodeNumber').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStreamFlowReductionSQLAgent.DeleteStreamFlowReduction(AStreamFlowReduction: IStreamFlowReduction): Boolean;
const OPNAME = 'TStreamFlowReductionSQLAgent.DeleteStreamFlowReduction';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(DeleteStreamFlowReductionSQL(AStreamFlowReduction.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TStreamFlowReductionSQLAgent.InsertStreamFlowReduction(AStreamFlowReduction: IStreamFlowReduction) :  Boolean;
const OPNAME = 'TStreamFlowReductionSQLAgent.InsertStreamFlowReduction';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertStreamFlowReductionSQL);
          LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'],    [IntToStr(AStreamFlowReduction.Identifier)]);

          LDataSet.SetParams(['InflowNodeNumber'],    [IntToStr(AStreamFlowReduction.InflowNodeNumber)]);
          LDataSet.SetParams(['CoveredArea'],         [FloatToStr(AStreamFlowReduction.CoveredArea)]);
          LDataSet.SetParams(['UnitRunoffFileName'],  [AStreamFlowReduction.UnitRunoffFileName]);
          LDataSet.SetParams(['SoilMoistureFileName'],[AStreamFlowReduction.SoilMoistureFileName]);
          LDataSet.SetParams(['SFRName'],             [AStreamFlowReduction.SFRName]);
          LDataSet.SetParams(['SFRDescr'],            [AStreamFlowReduction.SFRDescription]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = nullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := True;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TStreamFlowReductionSQLAgent.CopySFRFromScenario(ASourceStudyAreaName, ASourceSubArea, ASourceScenario: string;
                                                          ASFRList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TStreamFlowReductionSQLAgent.CopySFRFromScenario';
      SFRSQL = 'SELECT * FROM SFRSubCatchment WHERE ';
var
  LStop                             : boolean;
  LIndex                            : integer;
  LSourceDataSet                    : TAbstractModelDataset;
  LDestinationDataSet               : TAbstractModelDataset;
  LImportDate                       : TDateTime;
  LModel                            : string;
  LFieldName                        : string;
  LDestinationStudyAreaName         : string;
  LDestinationSubArea               : string;
  LDestinationScenario              : string;
  LSourceSQL                        : string;
  LDestinationSQL                   : string;
  LSourceWhereClause                : string;
  LDestinationWhereClause           : string;
  LMessage                          : string;
  LSFRIndex                         : integer;
  LSFRName                          : string;
  LIdentifier                       : integer;
  LNewRecordIdentifier              : integer;
begin
  Result := False;
  try
    if not Assigned(ASFRList) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    try
      if Assigned(LSourceDataSet) and Assigned(LDestinationDataSet) then
      begin
        LModel                    := FAppModules.StudyArea.ModelCode;
        LDestinationStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
        LDestinationSubArea       := FAppModules.StudyArea.SubAreaCode;
        LDestinationScenario      := FAppModules.StudyArea.ScenarioCode;

        LSourceWhereClause  :=     ' (Model         = ' + QuotedStr(LModel)                   + ') AND ' +
                                   ' (StudyAreaName = ' + QuotedStr(ASourceStudyAreaName)     + ') AND ' +
                                   ' (SubArea       = ' + QuotedStr(ASourceSubArea)           + ') AND ' +
                                   ' (Scenario      = ' + QuotedStr(ASourceScenario)          + ')';
        LDestinationWhereClause := ' (Model         = ' + QuotedStr(LModel)                   + ') AND ' +
                                   ' (StudyAreaName = ' + QuotedStr(LDestinationStudyAreaName)+ ') AND ' +
                                   ' (SubArea       = ' + QuotedStr(LDestinationSubArea)      + ') AND ' +
                                   ' (Scenario      = ' + QuotedStr(LDestinationScenario)     + ')';
        FAppModules.Database.StartTransaction;
        try
          LNewRecordIdentifier     := GetMaxIdentifier;
          for LSFRIndex := 0 to ASFRList.Count-1 do
          begin
            LSFRName := ASFRList[LSFRIndex];
            LIdentifier := Integer(ASFRList.Objects[LSFRIndex]);
            LNewRecordIdentifier := LNewRecordIdentifier +1;
            LMessage := 'Copying SFR ('+LSFRName+') ' + IntToStr(LSFRIndex+1) + ' of '+ IntToStr(ASFRList.Count);
            AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
            if LStop then
            begin
              FAppModules.Database.Rollback;
              Exit;
            end;
            //________________________________________________________ SFRSubCatchment ____________________________
            LSourceSQL := SFRSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;

            LDestinationSQL := SFRSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table SFRSubCatchment cannot be set to updateble.');
            end
            else
            begin
              LDestinationDataSet.DataSet.Append;
              for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
              begin
                 LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                 LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                   LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
              end;
              LDestinationDataSet.DataSet.FieldByName('Model').AsString          := LModel;
              LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString  := LDestinationStudyAreaName;
              LDestinationDataSet.DataSet.FieldByName('SubArea').AsString        := LDestinationSubArea;
              LDestinationDataSet.DataSet.FieldByName('Scenario').AsString       := LDestinationScenario;
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewRecordIdentifier;
              LDestinationDataSet.DataSet.FieldByName('InflowNodeNumber').AsInteger := 0;
              LDestinationDataSet.DataSet.Post;
            end;
          end;
          FAppModules.StudyArea.LastUpdateDate := Now();
          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          FAppModules.Database.Commit;
          Result := True;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      FreeAndNil(LDestinationDataSet);
      FreeAndNil(LSourceDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.




