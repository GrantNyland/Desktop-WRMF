{
  UNIT      : Contains TWetlandSQLAgent Class
  AUTHOR    : Maurice Marinus
  DATE      : 2006/08/07
  COPYRIGHT : Copyright © 2006 DWAF
}

unit UWetlandSQLAgent;

interface

uses
  Classes,
  UWetland,
  UAbstractObject;

type
  TWetlandSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData_FeatureID (AContextData : TStringList; AFeatureID: string);

    function GetMaxIdentifierSQL                    : string;
    function GetMaxNodeNumberSQL                    : string;
    function GetWetlandSQL                          : string;
    function DeleteWetlandSQL(ANodeNumber: Integer) : string;
    function InsertWetlandSQL                       : string;

    function GetMaxIdentifier                       : Integer;
    function GetMaxNodeNumber                       : Integer;
    function InsertWetland(AWetland: TWetland)      : Boolean;
    function DeleteWetland(ANodeNumber: Integer)    : Boolean;
    function CopyWetlandFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AWetlandList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UErrorHandlingOperations, DB;

function TWetlandSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TWetlandSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandSQLAgent.LoadContextData_FeatureID (AContextData : TStringList; AFeatureID: string);
const OPNAME = 'TWetlandSQLAgent.LoadContextData_FeatureID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TWetlandSQLAgent.GetWetlandSQL: string;
const OPNAME = 'TWetlandSQLAgent.GetWetlandSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier, NodeNumber, '+
    ' WetlandName, UpstreamThreshold, InflowProportion, '+
    ' Storage, OutflowProportion '+
    ' FROM Wetland A WHERE '+
    GetScenarioWhereClause +
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandSQLAgent.DeleteWetlandSQL(ANodeNumber: Integer): string;
const OPNAME = 'TWetlandSQLAgent.DeleteWetlandSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM Wetland A WHERE ' +
                GetScenarioWhereClause +
                'AND A.NodeNumber = '+ IntToStr(ANodeNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandSQLAgent.GetMaxIdentifierSQL: string;
const OPNAME = 'TWetlandSQLAgent.GetMaxIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM Wetland A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandSQLAgent.InsertWetlandSQL: string;
const OPNAME = 'TWetlandSQLAgent.InsertWetlandSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO Wetland '+
      ' (Model, StudyAreaName, SubArea, Scenario, Identifier, '+
      ' NodeNumber, WetlandName, UpstreamThreshold, InflowProportion, '+
      ' Storage, OutflowProportion, InflowChannelNumber, OutflowChannelNumber )'+
      ' VALUES ('+
      ' :Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, '+
      ' :NodeNumber, :WetlandName, :UpstreamThreshold, :InflowProportion, '+
      ' :Storage, :OutflowProportion, :InflowChannelNumber, :OutflowChannelNumber ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandSQLAgent.GetMaxIdentifier: Integer;
const OPNAME = 'TWetlandSQLAgent.GetMaxIdentifier';
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

function TWetlandSQLAgent.DeleteWetland(ANodeNumber: Integer): Boolean;
const OPNAME = 'TWetlandSQLAgent.DeleteWetland';
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
          LDataSet.SetSQL(DeleteWetlandSQL(ANodeNumber));
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


function TWetlandSQLAgent.InsertWetland(AWetland: TWetland) :  Boolean;
const OPNAME = 'TWetlandSQLAgent.InsertWetland';
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
          LDataSet.SetSQL(InsertWetlandSQL);
          LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'],    [IntToStr(AWetland.Identifier)]);

          LDataSet.SetParams(['NodeNumber'],        [IntToStr(AWetland.NodeNumber)]);
          LDataSet.SetParams(['WetlandName'],       [AWetland.Name]);
          LDataSet.SetParams(['UpstreamThreshold'], [FloatToStr(AWetland.UpstreamThreshold)]);
          LDataSet.SetParams(['InflowProportion'],  [FloatToStr(AWetland.InflowProportion)]);
          LDataSet.SetParams(['Storage'],           [FloatToStr(AWetland.StorageVolume)]);
          LDataSet.SetParams(['OutflowProportion'], [FloatToStr(AWetland.OutflowProportion)]);
          LDataSet.SetParams(['InflowChannelNumber'], [IntToStr(AWetland.InflowChannelNr)]);
          LDataSet.SetParams(['OutflowChannelNumber'], [IntToStr(AWetland.OutflowChannelNr)]);

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

function TWetlandSQLAgent.GetMaxNodeNumber: Integer;
const OPNAME = 'TWetlandSQLAgent.GetMaxNodeNumber';
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

function TWetlandSQLAgent.GetMaxNodeNumberSQL: string;
const OPNAME = 'TWetlandSQLAgent.GetMaxNodeNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(NodeNumber) AS MaxNodeNumber FROM Wetland A WHERE' + GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandSQLAgent.CopyWetlandFromScenario(ASourceStudyAreaName,ASourceSubArea, ASourceScenario: string; AWetlandList: TStrings;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWetlandSQLAgent.CopyWetlandFromScenario';
      WetlandSQL = 'SELECT * FROM Wetland WHERE  ';
      ChannelDetailsSQL = 'SELECT * FROM ChannelDetails WHERE ';
var
  LOldInflowChannelNumber      : integer;
  LOldOutflowChannelNumber     : integer;
  LNewInflowChannelNumber      : integer;
  LNewOutflowChannelNumber     : integer;
  LNewNodeNumber               : integer;
  LNodeNumber                  : integer;
  LNewWetlandIdentifier        : integer;
  LStop                        : boolean;
  LIndex                       : integer;
  LSourceDataSet               : TAbstractModelDataset;
  LDestinationDataSet          : TAbstractModelDataset;
  LChannelSourceDataSet        : TAbstractModelDataset;
  LChannelDestinationDataSet   : TAbstractModelDataset;
  LImportDate                  : TDateTime;
  LReservoirDataSQLAgent       : TReservoirDataSQLAgent;
  LChannelDataSQLAgent         : TChannelDataSQLAgent;
  LModel                       : string;
  LFieldName                   : string;
  LDestinationStudyAreaName    : string;
  LDestinationSubArea          : string;
  LDestinationScenario         : string;
  LSourceSQL                   : string;
  LDestinationSQL              : string;
  LSourceWhereClause           : string;
  LDestinationWhereClause      : string;
  LMessage                     : string;
  LWetlandName                 : string;
  LWetlandIndex                : integer;
  LNewChannelID                : integer;
begin
  Result := False;
  try
    if not Assigned(AWetlandList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
    LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
    try
      if Assigned(LSourceDataSet) and Assigned(LDestinationDataSet) and Assigned(LChannelSourceDataSet) and
        Assigned(LChannelDestinationDataSet) then
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
      end;
      try
        FAppModules.Database.StartTransaction;
        LNewWetlandIdentifier := LReservoirDataSQLAgent.GetMaxReservoirIdentifier;
        LNewNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;
        LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                               AWetlandList,AProgressUpdateFuntion);
        LNewInflowChannelNumber := 0;
        LNewOutflowChannelNumber := 0;
        for LWetlandIndex := 0 to AWetlandList.Count-1 do
        begin
          LNodeNumber := integer(AWetlandList.Objects[LWetlandIndex]);
          LWetlandName := AWetlandList[LWetlandIndex];
          LNewNodeNumber := LNewNodeNumber + 1;
          LNewWetlandIdentifier := LNewWetlandIdentifier + 1;
          LMessage := 'Copying Wetland ('+LWetlandName+') ' + IntToStr(LWetlandIndex+1) + ' of '+ IntToStr(AWetlandList.Count);
          AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
          if LStop then
          begin
            if FAppModules.Database.InTransaction then
              FAppModules.Database.Rollback;
            Exit;
          end;
          //________________________________________________________ Wetland ____________________________
          LSourceSQL := WetlandSQL + LSourceWhereClause + ' AND NodeNumber = '+ IntToStr(LNodeNumber);
          LSourceDataSet.DataSet.Close;
          LSourceDataSet.SetSQL(LSourceSQL);
          LSourceDataSet.DataSet.Open;
          if LSourceDataSet.DataSet.Eof then Continue;

          LDestinationSQL := WetlandSQL + LDestinationWhereClause;
          LDestinationDataSet.DataSet.Close;
          LDestinationDataSet.SetSQL(LDestinationSQL);
          LDestinationDataSet.SetReadOnly(False);
          LDestinationDataSet.DataSet.Open;
          if LDestinationDataSet.IsReadOnly  then
          begin
            LDestinationDataSet.DataSet.Close;
            raise Exception.Create('Query to table Wetland cannot be set to updatable.');
          end
          else
          begin
            LDestinationDataSet.DataSet.Append;
            LOldInflowChannelNumber := LSourceDataSet.DataSet.FieldByName('InflowChannelNumber').AsInteger;
            LOldOutflowChannelNumber := LSourceDataSet.DataSet.FieldByName('OutflowChannelNumber').AsInteger;
            for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
            begin
              LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
              LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
              LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
            end;

            if (LOldInflowChannelNumber > 0) and (LOldOutflowChannelNumber > 0) then
            begin
              //________________________________________________________ ChannelDetails ____________________________
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldInflowChannelNumber);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := ChannelDetailsSQL + LDestinationWhereClause;
                LChannelDestinationDataSet.DataSet.Close;
                LChannelDestinationDataSet.SetSQL(LDestinationSQL);
                LChannelDestinationDataSet.SetReadOnly(False);
                LChannelDestinationDataSet.DataSet.Open;
                if LChannelDestinationDataSet.IsReadOnly  then
                begin
                  LChannelDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table ChannelDetails cannot be set to updatable.');
                end
                else
                begin
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                  LNewInflowChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                  LChannelDestinationDataSet.DataSet.Append;
                  for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                  begin
                    LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                    LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                    LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;
                  LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewChannelID;
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewInflowChannelNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := LNewNodeNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := 0;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;  
              //________________________________________________________ ChannelDetails ____________________________
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldOutflowChannelNumber);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := ChannelDetailsSQL + LDestinationWhereClause;
                LChannelDestinationDataSet.DataSet.Close;
                LChannelDestinationDataSet.SetSQL(LDestinationSQL);
                LChannelDestinationDataSet.SetReadOnly(False);
                LChannelDestinationDataSet.DataSet.Open;
                if LChannelDestinationDataSet.IsReadOnly  then
                begin
                  LChannelDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table ChannelDetails cannot be set to updatable.');
                end
                else
                begin
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                  LNewOutflowChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                  LChannelDestinationDataSet.DataSet.Append;
                  for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                  begin
                    LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                    LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                    LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;
                  LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewChannelID;
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewOutflowChannelNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := LNewNodeNumber;

                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;  
              LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
              LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
              LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
              LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewWetlandIdentifier;
              LDestinationDataSet.DataSet.FieldByName('InflowChannelNumber').AsInteger      := LNewInflowChannelNumber;
              LDestinationDataSet.DataSet.FieldByName('OutflowChannelNumber').AsInteger     := LNewOutflowChannelNumber;
              LDestinationDataSet.DataSet.FieldByName('NodeNumber').AsInteger               := LNewNodeNumber;
              LDestinationDataSet.DataSet.Post;
            end;
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
    finally
      LSourceDataSet.Free;
      LDestinationDataSet.Free;
      LChannelSourceDataSet.Free;
      LChannelDestinationDataSet.Free;
      LReservoirDataSQLAgent.Free;
      LChannelDataSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.




