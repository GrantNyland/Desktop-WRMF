//
//
//  UNIT      : Contains TReservoirHeadingSQLAgent Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/11
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirZoneDataSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UReservoirData,
  UReservoirPenaltyStructureData,
  UReservoirZoneElevationData,
  UAbstractObject;

type
  TReservoirZoneDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function Get_HighestReservoirLevelRecordIdentifier: integer;
    function GetNoAliasScenarioWhereClause: string;
  public
    function CreateReservoirsZone(AReservoirDataList: TReservoirDataList;
             AReservoirPenaltyStructureList:TReservoirPenaltyStructureList;
             AZoneLevel: integer; ACurrentReservoirNumber: integer = 0):IReservoirPenaltyZoneData ;
    function DeleteReservoirsZone(AReservoirDataList: TReservoirDataList;
             AReservoirPenaltyStructureList:TReservoirPenaltyStructureList;AZoneLevel: integer): boolean ; 
  end;

implementation

uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations, Variants;


function TReservoirZoneDataSQLAgent.CreateReservoirsZone(
  AReservoirDataList: TReservoirDataList;
  AReservoirPenaltyStructureList: TReservoirPenaltyStructureList;
  AZoneLevel,
  ACurrentReservoirNumber: integer): IReservoirPenaltyZoneData;
const OPNAME = 'TReservoirZoneDataSQLAgent.CreateReservoirsZone';
      ReservoirLevelsSQL       = 'SELECT * FROM ReservoirLevels WHERE ';
      StorageZonesSQL          = 'SELECT * FROM StorageZones WHERE ';
      StorageZoneDetailsSQL    = 'SELECT * FROM StorageZoneDetails WHERE ';
var
  LReservoirZoneElevationsData: IReservoirZoneElevationsData;
  LReservoirPenaltyZoneData: IReservoirPenaltyZoneData;
  LTempReservoirPenaltyZoneData: IReservoirPenaltyZoneData;
  LReservoirData:IReservoirData;
  LDrawDownElevation:IDrawDownElevation;
  LMonthlyElevations: TAbstractFieldProperty;

  LReservoirPenaltyStructureData:IReservoirPenalty;
  LDataSet: TAbstractModelDataset;
  LCount,LIndex: integer;
  LStart,LEnd: integer;
  LRecordID: integer;
  LReservoirCount: integer;
  LFieldName,
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
  LFirstZoneAdded: boolean;
  LDeleteAction: TDeleteAction;
  LImportDate : TDateTime;
begin
  Result := nil;
  try
    if not Assigned(AReservoirDataList) then
       raise Exception.Create('Reservoir Data List parameter is not assigned.');
    if not Assigned(AReservoirPenaltyStructureList) then
       raise Exception.Create('Reservoir Penalty Structure List parameter is not assigned.');
    if (AZoneLevel < 1) or ((AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount > 0) and
    (AZoneLevel > AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount-2))then
       raise Exception.Create('Zone Level parameter ('+ IntToStr(AZoneLevel)+ ') cannot be deleted as it is a fixed zone or does not exist.');

    LMonthlyElevations := FAppModules.FieldProperties.FieldProperty('ReservoirLev');
    if not Assigned(LMonthlyElevations) then
      raise Exception.Create('Field (ReservoirLev) not found in field properties');

    LFirstZoneAdded := (AReservoirPenaltyStructureList.PenaltyZoneCount = 0);

    LReservoirZoneElevationsData := AReservoirDataList.CreateReservoirsZone(AZoneLevel,ACurrentReservoirNumber);
    LReservoirPenaltyZoneData    := AReservoirPenaltyStructureList.CreatePenaltyZone(AZoneLevel);
    if Assigned(LReservoirZoneElevationsData) then;
    if not Assigned(LReservoirPenaltyZoneData) then
    begin
      AReservoirDataList.DeleteReservoirsZone(AZoneLevel,LDeleteAction);
      AReservoirPenaltyStructureList.DeletePenaltyZone(AZoneLevel,LDeleteAction);
    end
    else
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LModel         := FAppModules.StudyArea.ModelCode;
          LStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
          LSubArea       := FAppModules.StudyArea.SubAreaCode;
          LScenario      := FAppModules.StudyArea.ScenarioCode;

          FAppModules.Database.StartTransaction;
          try
            //___________________________Reservoir Levels_____________________________________
            LSQL := ReservoirLevelsSQL + GetNoAliasScenarioWhereClause + ' ORDER BY ReservoirIdentifier,LevelIdentifier DESC';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirLevels cannot be set to updateble.');
            end
            else
            begin
              while not LDataset.DataSet.Eof do
              begin
                if(LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger >= AZoneLevel) then
                begin
                  LDataset.DataSet.Edit;
                  LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger := LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger + 1;
                  LDataset.DataSet.Post;
                end;
                LDataset.DataSet.Next;
              end;

              LStart := AZoneLevel;
              LEnd   := AZoneLevel;
              if LFirstZoneAdded then
              begin
                LStart := 1;
                LEnd   := AReservoirPenaltyStructureList.PenaltyZoneCount-3;
              end;

              LRecordID := Get_HighestReservoirLevelRecordIdentifier;
              for LReservoirCount := 0 to AReservoirDataList.ReservoirCount -1 do
              begin
                LReservoirData := AReservoirDataList.ReservoirByIndex[LReservoirCount];
                for LIndex := LStart to LEnd do
                begin
                  LDrawDownElevation := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[LIndex-1];
                  LDataset.DataSet.Append;
                  LDataset.DataSet.FieldByName('Model').AsString := LModel;
                  LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
                  LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
                  LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

                  LRecordID := LRecordID + 1;
                  LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger := LRecordID;
                  LDataset.DataSet.FieldByName('ReservoirIdentifier').AsInteger := LDrawDownElevation.ReservoirIdentifier;
                  LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger      := LDrawDownElevation.LevelIdentifier;

                  for LCount := LMonthlyElevations.ArrayLow to LMonthlyElevations.ArrayHigh do
                  begin
                    if(LDrawDownElevation.MonthlyElevationByIndex[LCount] <> NullFloat) then
                    begin
                      LFieldName := Format('%s%2.2d',['ReservoirLev',LCount]);
                      LDataset.DataSet.FieldByName(LFieldName).AsFloat := LDrawDownElevation.MonthlyElevationByIndex[LCount];
                    end;
                  end;
                  LDataset.DataSet.FieldByName('ResLevelsComment').AsString := '';
                  LDataset.DataSet.Post;
                end;
              end;
            end;

            //___________________________StorageZoneDetails_____________________________________
            LSQL := StorageZoneDetailsSQL + GetNoAliasScenarioWhereClause + ' ORDER BY Identifier DESC';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table StorageZoneDetails cannot be set to updateble.');
            end
            else
            begin
              while not LDataset.DataSet.Eof do
              begin
                if(LDataset.DataSet.FieldByName('Identifier').AsInteger >= LReservoirPenaltyZoneData.RecordIdentifier) then
                begin
                  LDataset.DataSet.Edit;
                  LDataset.DataSet.FieldByName('Identifier').AsInteger := LDataset.DataSet.FieldByName('Identifier').AsInteger + 1;
                  LDataset.DataSet.Post;
                end;
                LDataset.DataSet.Next;
              end;

              LStart := AZoneLevel;
              LEnd   := AZoneLevel;
              if LFirstZoneAdded then
              begin
                LStart := 0;
                LEnd   := AReservoirPenaltyStructureList.PenaltyZoneCount-1;
              end;

              for LIndex := LStart to LEnd do
              begin
                LTempReservoirPenaltyZoneData := AReservoirPenaltyStructureList.ReservoirPenaltyZoneByIndex[LIndex];
                LDataset.DataSet.Append;
                LDataset.DataSet.FieldByName('Model').AsString := LModel;
                LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
                LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
                LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

                LDataset.DataSet.FieldByName('Identifier').AsInteger        := LTempReservoirPenaltyZoneData.RecordIdentifier;
                LDataset.DataSet.FieldByName('ReservoirZoneName').AsString  := LTempReservoirPenaltyZoneData.ZoneName;
                LDataset.DataSet.FieldByName('StrategyIndicator').AsInteger := LTempReservoirPenaltyZoneData.StrategyIndicator;
                LDataset.DataSet.FieldByName('BalancingVariable').AsInteger := LTempReservoirPenaltyZoneData.BalancingVariable;
                LDataset.DataSet.FieldByName('BalancingPolicy').AsInteger   := LTempReservoirPenaltyZoneData.BalancingPolicy;
                for LCount := 0 to AReservoirPenaltyStructureList.PenaltyCount -1 do
                begin
                  LReservoirPenaltyStructureData := AReservoirPenaltyStructureList.ReservoirPenaltyByIndex[LCount];
                  if (LReservoirPenaltyStructureData.ReservoirPenaltyValueByIndex[LIndex+1] <> NullFloat) then
                  begin
                    LFieldName := Format('%s%2.2d',['BalRef',LCount+1]);
                    LDataset.DataSet.FieldByName(LFieldName).AsFloat := LReservoirPenaltyStructureData.ReservoirPenaltyValueByIndex[LIndex+1];
                  end;
                end;
                LDataset.DataSet.FieldByName('StorageZoneDetailsComment').AsString := ' ';
                LDataset.DataSet.Post;
              end;
            end;

            //___________________________StorageZones_____________________________________
            LSQL := StorageZonesSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table StorageZones cannot be set to updateble.');
            end
            else
            begin
              if(LDataset.DataSet.RecordCount = 0) then
              begin
                LDataset.DataSet.Append;
                LDataset.DataSet.FieldByName('Model').AsString := LModel;
                LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
                LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
                LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

                LDataset.DataSet.FieldByName('StorageZoneCount').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount;
                LDataset.DataSet.FieldByName('ZoneLowerBoundary').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.ZoneRuleCurve;
                LDataset.DataSet.FieldByName('PenaltyStructureCount').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.PenaltyStructureCount;
                LDataset.DataSet.FieldByName('NodesCount').AsInteger := AReservoirDataList.ReservoirCount;
                LDataset.DataSet.FieldByName('ReservoirLevelsCount').AsInteger :=AReservoirDataList.ReservoirCount *
                 (AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount -3);
                LDataset.DataSet.FieldByName('StorageZonesComment').AsString := ' ';
                LDataset.DataSet.FieldByName('ZeroComment').AsString := ' ';
              end
              else
              begin
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('StorageZoneCount').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount;
                LDataset.DataSet.FieldByName('ZoneLowerBoundary').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.ZoneRuleCurve;
                LDataset.DataSet.FieldByName('PenaltyStructureCount').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.PenaltyStructureCount;
                LDataset.DataSet.FieldByName('NodesCount').AsInteger := AReservoirDataList.ReservoirCount;
                LDataset.DataSet.FieldByName('ReservoirLevelsCount').AsInteger :=AReservoirDataList.ReservoirCount *
                 (AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount -3);
              end;
              LDataset.DataSet.Post;
            end;

            FAppModules.StudyArea.LastUpdateDate := Now();

            LImportDate := FAppModules.StudyArea.GetStudyImportDate;
            if LImportDate = NullDateTime then
              FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            FAppModules.Database.Commit;
            Result := LReservoirPenaltyZoneData;
          except
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
      finally
        LDataset.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneDataSQLAgent.DeleteReservoirsZone(AReservoirDataList: TReservoirDataList;
         AReservoirPenaltyStructureList: TReservoirPenaltyStructureList;
         AZoneLevel: integer): boolean;
const OPNAME = 'TReservoirZoneDataSQLAgent.DeleteReservoirsZone';
      ReservoirLevelsSQL       = 'SELECT * FROM ReservoirLevels WHERE ';
      StorageZonesSQL          = 'SELECT * FROM StorageZones WHERE ';
      StorageZoneDetailsSQL    = 'SELECT * FROM StorageZoneDetails WHERE ';
var
  LDeleteAction: TDeleteAction;
  LDataSet: TAbstractModelDataset;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
  LFieldName: string;
  LIndex,
  LZoneLevelDifference : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if not Assigned(AReservoirDataList) then
       raise Exception.Create('Reservoir Data List parameter is not assigned.');
    if not Assigned(AReservoirPenaltyStructureList) then
       raise Exception.Create('Reservoir Penalty Structure List parameter is not assigned.');
    if (AZoneLevel < 1) or (AZoneLevel >= AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount-1)then
       raise Exception.Create('Zone Level parameter ('+ IntToStr(AZoneLevel)+ ') cannot be deleted as it is a fixed zone or does not exist.');

    LDeleteAction := daContinue;
    Result := AReservoirPenaltyStructureList.DeletePenaltyZone(AZoneLevel,LDeleteAction);
    if(LDeleteAction = daCancel) then
      Exit;
    Result := Result and AReservoirDataList.DeleteReservoirsZone(AZoneLevel,LDeleteAction);
    if Result then
    begin
      if((AReservoirPenaltyStructureList.PenaltyZoneCount + 1 - 3) < AZoneLevel) then
        LZoneLevelDifference := AZoneLevel - 1
      else
        LZoneLevelDifference := 0;

      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LModel         := FAppModules.StudyArea.ModelCode;
          LStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
          LSubArea       := FAppModules.StudyArea.SubAreaCode;
          LScenario      := FAppModules.StudyArea.ScenarioCode;

          FAppModules.Database.StartTransaction;
          try
            //___________________________Reservoir Levels_____________________________________
            LSQL := ReservoirLevelsSQL + GetNoAliasScenarioWhereClause + ' ORDER BY ReservoirIdentifier,LevelIdentifier';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirLevels cannot be set to updateble.');
            end
            else
            begin
              while not LDataset.DataSet.Eof do
              begin
                case LDeleteAction of
                  daDeleteAll,daContinue:
                    begin
                      if(LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger = (AZoneLevel - LZoneLevelDifference)) then
                      begin
                        LDataset.DataSet.Delete;
                      end
                      else
                      begin
                        if(LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger > AZoneLevel) then
                        begin
                          LDataset.DataSet.Edit;
                          LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger := LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger -1;
                          LDataset.DataSet.Post;
                        end;
                        LDataset.DataSet.Next;
                      end;
                    end;
                  daClearData:
                    begin
                      if(LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger = AZoneLevel) then
                      begin
                        LDataset.DataSet.Edit;
                        for LIndex := 1 to 12 do
                        begin
                          LFieldName := Format('%s%2.2d',['ReservoirLev',LIndex]);
                          LDataset.DataSet.FieldByName(LFieldName).AsFloat := 0.0
                        end;
                        LDataset.DataSet.Post;
                        LDataset.DataSet.Next;
                      end
                    end;
                end;//Case
              end;
            end;

            //___________________________StorageZoneDetails_____________________________________
            LSQL := StorageZoneDetailsSQL + GetNoAliasScenarioWhereClause + ' ORDER BY Identifier';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table StorageZoneDetails cannot be set to updateble.');
            end
            else
            begin
              while not LDataset.DataSet.Eof do
              begin
                case LDeleteAction of
                  daDeleteAll:
                    begin
                      LDataset.DataSet.Delete;
                    end;
                  daClearData:
                    begin
                      if(LDataset.DataSet.FieldByName('Identifier').AsInteger = (AZoneLevel+1)) then
                      begin
                        LDataset.DataSet.Edit;
                        for LIndex := 1 to 5 do
                        begin
                          LFieldName := Format('%s%2.2d',['BalRef',LIndex]);
                          if(LIndex <= AReservoirPenaltyStructureList.CastReservoirPenaltyCounts.PenaltyStructureCount) then
                            LDataset.DataSet.FieldByName(LFieldName).AsFloat := 0.0
                          else
                            LDataset.DataSet.FieldByName(LFieldName).Clear;
                        end;
                        LDataset.DataSet.Post;
                        LDataset.DataSet.Next;
                      end
                      else
                        LDataset.DataSet.Next;
                    end;
                  daContinue:
                  begin
                    if(LDataset.DataSet.FieldByName('Identifier').AsInteger = (AZoneLevel+1)) then
                    begin
                      LDataset.DataSet.Delete;
                    end
                    else
                    if(LDataset.DataSet.FieldByName('Identifier').AsInteger > (AZoneLevel+1)) then
                    begin
                      LDataset.DataSet.Edit;
                      LDataset.DataSet.FieldByName('Identifier').AsInteger := LDataset.DataSet.FieldByName('Identifier').AsInteger - 1;
                      LDataset.DataSet.Post;
                      LDataset.DataSet.Next;
                    end
                    else
                      LDataset.DataSet.Next;
                  end;
                end;
              end;
            end;

            //___________________________StorageZones_____________________________________
            LSQL := StorageZonesSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table StorageZones cannot be set to updateble.');
            end
            else
            begin
              if(LDeleteAction =daDeleteAll) then
                LDataset.DataSet.Delete
              else
              begin
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('StorageZoneCount').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount;
                LDataset.DataSet.FieldByName('ZoneLowerBoundary').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.ZoneRuleCurve;
                LDataset.DataSet.FieldByName('PenaltyStructureCount').AsInteger := AReservoirPenaltyStructureList.ReservoirPenaltyCounts.PenaltyStructureCount;
                LDataset.DataSet.FieldByName('NodesCount').AsInteger := AReservoirDataList.ReservoirCount;
                LDataset.DataSet.FieldByName('ReservoirLevelsCount').AsInteger :=AReservoirDataList.ReservoirCount *
                 (AReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount -3);
                LDataset.DataSet.Post;
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
        LDataset.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneDataSQLAgent.GetNoAliasScenarioWhereClause: string;
const OPNAME = 'TReservoirZoneDataSQLAgent.GetNoAliasScenarioWhereClause';
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

function TReservoirZoneDataSQLAgent.Get_HighestReservoirLevelRecordIdentifier: integer;
const OPNAME = 'TReservoirZoneDataSQLAgent.GetNoAliasScenarioWhereClause';
var
  LSQL     : string;
  LDataSet : TAbstractModelDataset;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(RecordIdentifier) AS MaxID FROM ReservoirLevels WHERE ' + GetNoAliasScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(LSQL);
      LDataset.DataSet.Open;
      Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


