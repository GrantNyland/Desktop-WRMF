//
//
//  UNIT      : Contains TReservoirHeadingSQLAgent Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/11
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirPenaltyStructureDataSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TReservoirPenaltyStructureDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetNoAliasScenarioWhereClause: string;
  public
    procedure LoadStorageZonesContextData(AContextData: TStringList);
    procedure LoadStorageZoneDetailsContextData(AContextData: TStringList; ARecordIdentifier: string);
    procedure LoadStorageZoneNamesContextData(AContextData: TStringList; ARecordIdentifier,APenaltyStruct: string);
    function GetPenaltyStructureSQL: string;
    function GetPenaltyCountsSQL: string;
    function AddPenaltyStructure(APenaltyCount: integer;APenalty:IReservoirPenalty): boolean;
    function DeletePenaltyStructure(APenaltyCount: integer;APenalty:IReservoirPenalty): boolean;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

function TReservoirPenaltyStructureDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.GetScenarioWhereClause';
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

function TReservoirPenaltyStructureDataSQLAgent.GetNoAliasScenarioWhereClause: string;
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.GetNoAliasScenarioWhereClause';
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

function TReservoirPenaltyStructureDataSQLAgent.GetPenaltyStructureSQL: string;
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.GetPenaltyStructureSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT ' +
      ' Identifier AS RecordIdentifier, ' +
      ' ReservoirZoneName, StrategyIndicator, BalancingVariable, BalancingPolicy, ' +
      ' BalRef01, BalRef02, BalRef03, BalRef04, BalRef05, ' +
      ' BalRef06, BalRef07, BalRef08, BalRef09, BalRef10, ' +
      ' BalRef11, BalRef12, BalRef13, BalRef14, BalRef15, ' +
      ' BalRef16, BalRef17, BalRef18, BalRef19, BalRef20 ' +
      ' FROM StorageZoneDetails A        ' +
      ' WHERE ' + GetScenarioWhereClause +
      ' ORDER BY A.Identifier              ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyStructureDataSQLAgent.GetPenaltyCountsSQL: string;
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.GetPenaltyCountsSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT                    ' +
      '   StorageZoneCount,       ' +
      '   ZoneLowerBoundary,      ' +
      '   PenaltyStructureCount   ' +
      ' FROM StorageZones A       ' +
      ' WHERE ' +
      GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyStructureDataSQLAgent.LoadStorageZonesContextData(AContextData: TStringList);
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.LoadStorageZonesContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirPenaltyStructureDataSQLAgent.LoadStorageZoneDetailsContextData(AContextData: TStringList; ARecordIdentifier: string);
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.LoadStorageZoneDetailsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirPenaltyStructureDataSQLAgent.LoadStorageZoneNamesContextData(AContextData: TStringList; ARecordIdentifier,APenaltyStruct: string);
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.LoadStorageZoneNamesContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('PenaltyStruct=' + APenaltyStruct);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirPenaltyStructureDataSQLAgent.AddPenaltyStructure(APenaltyCount: integer;
  APenalty: IReservoirPenalty): boolean;
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.AddPenaltyStructure';
var
  LCount,
  LIndex: integer;
  LDataSet: TAbstractModelDataset;
  LCurrentFieldName,
  LFromFieldName,
  LToFieldName,
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(APenalty) AND
       (APenalty.ReservoirPenaltyID > 0) AND
       (APenalty.ReservoirPenaltyID <= 20) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LSQL := 'SELECT * FROM  StorageZoneDetails WHERE '+ GetNoAliasScenarioWhereClause;
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
            LCount := 1;
            while (not LDataset.DataSet.Eof) do
            begin
              LDataset.DataSet.Edit;
              LCurrentFieldName := Format('%s%2.2d',['BalRef',APenalty.ReservoirPenaltyID]);
              for LIndex := APenalty.ReservoirPenaltyID+1 to 20 do
              begin
                LToFieldName := Format('%s%2.2d',['BalRef',LIndex]);
                LFromFieldName := Format('%s%2.2d',['BalRef',LIndex-1]);
                if LDataset.DataSet.FieldByName(LFromFieldName).IsNull then
                  LDataset.DataSet.FieldByName(LToFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LToFieldName).AsFloat :=
                  LDataset.DataSet.FieldByName(LFromFieldName).AsFloat;
              end;
              if(APenalty.ReservoirPenaltyValueByIndex[LCount] < 0.0) then
                LDataset.DataSet.FieldByName(LCurrentFieldName).Clear
              else
                LDataset.DataSet.FieldByName(LCurrentFieldName).AsFloat :=
                APenalty.ReservoirPenaltyValueByIndex[LCount];
              LDataset.DataSet.Post;
              LDataset.DataSet.Next;
              LCount := LCount + 1;
            end;
          end;
          LDataset.DataSet.Close;
          LSQL := 'UPDATE StorageZones SET PenaltyStructureCount = '+IntToStr(APenaltyCount+1) +' WHERE '+ GetNoAliasScenarioWhereClause;
          LDataSet.SetSQL(LSQL);
          LDataset.ExecSQL;

          FAppModules.StudyArea.LastUpdateDate := now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = nullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := True;
        end;
      finally
        LDataset.Free;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirPenaltyStructureDataSQLAgent.DeletePenaltyStructure(APenaltyCount: integer;
  APenalty: IReservoirPenalty): boolean;
const OPNAME = 'TReservoirPenaltyStructureDataSQLAgent.DeletePenaltyStructure';
var
  LIndex: integer;
  LDataSet: TAbstractModelDataset;
  LFromFieldName,
  LToFieldName,
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(APenalty) and
    (APenalty.ReservoirPenaltyID > 0) and
    (APenalty.ReservoirPenaltyID <= 20) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LSQL := 'SELECT * FROM  StorageZoneDetails WHERE '+ GetNoAliasScenarioWhereClause;
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
            while (not LDataset.DataSet.Eof) do
            begin
              LDataset.DataSet.Edit;
              for LIndex := APenalty.ReservoirPenaltyID to 4 do
              begin
                LToFieldName := Format('%s%2.2d',['BalRef',LIndex]);
                LFromFieldName := Format('%s%2.2d',['BalRef',LIndex+1]);
                if(LDataset.DataSet.FieldByName(LFromFieldName).IsNull) then
                  LDataset.DataSet.FieldByName(LToFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LToFieldName).AsFloat :=
                  LDataset.DataSet.FieldByName(LFromFieldName).AsFloat;
              end;
              LDataset.DataSet.FieldByName('BalRef20').Clear;
              LDataset.DataSet.Post;
              LDataset.DataSet.Next;
            end;
          end;
          LDataset.DataSet.Close;
          LSQL := 'UPDATE StorageZones SET PenaltyStructureCount = '+IntToStr(APenaltyCount-1) +' WHERE '+ GetNoAliasScenarioWhereClause;
          LDataSet.SetSQL(LSQL);
          LDataset.ExecSQL;

          FAppModules.StudyArea.LastUpdateDate := now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = nullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := True;
        end;
      finally
        LDataset.Free;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
