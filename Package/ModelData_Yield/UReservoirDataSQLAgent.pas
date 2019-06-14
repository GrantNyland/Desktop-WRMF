//
//
//  UNIT      : Contains TReservoirHeadingSQLAgent Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/11
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirDataSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TReservoirDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetNoAliasScenarioWhereClause: string;
    function Get_HighestReservoirLevelRecordIdentifier: integer;
  public
    procedure LoadReservoirNamesContextData(AContextData: TStringList;
      ARecordIdentifier, ANodeCount, APenaltyStructure: string);

    procedure LoadFixedElevationsContextData(AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
    procedure LoadNodesDetailsContextData(AContextData: TStringList; AReservoirIdentifier:string);
    procedure LoadInitialLevelsContextData(AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
    procedure LoadDrawDownElevationsContextData(AContextData: TStringList; AReservoirIdentifier, ALevelIdentifier, AFieldNameIdentifier: string);
    procedure LoadEvaporationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
    procedure LoadElevationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
    procedure LoadVolumesContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
    procedure LoadAreasContextData(AContextData: TStringList; ARecordIdentifier,AFieldNameIdentifier: string);
    procedure LoadHistoricDamLevel(AContextData: TStringList; ARecordIdentifier,AFieldNameIdentifier: string);
    procedure LoadContextData_ReservoirGroupAreaID(AContextData   : TStringList;
                                                  AGroupID : string);
    function GetNodesWithInflowNamesDataSQL: string;
    function GetNodesWithoutInflowNamesDataSQL: string;
    function GetIrrigationNodesNamesDataSQL: string;
    function GetWetlandsNodesNamesDataSQL: string;
    function GetReservoirNamesDataSQL: string;
    function GetDownStreamPowerChannelSQL: string;
    function GetReservoirAndNodesNamesDataSQL: string;
    function GetReservoirAreaDataSQL: string;
    function GetReservoirVolumeDataSQL: string;
    function GetReservoirEvaporationDataSQL: string;
    function GetReservoirElevationsDataSQL: string;
    function GetReservoirInitialLevelsDataSQL: string;
    function GetDrawDownElevationsSQL: string;
    function GetHistoricDamLevelsDataSQL: string;
    function GetReservoirAreaGroupSQL: string;
    function InsertReservoirAreaGroupSQL(AGroupID: integer): string;
    function DeleteReservoirAreaGroup(AGroupID: integer): boolean;
    function DeleteReservoirAreaGroupSQL(AGroupID: integer): string;

    function CreateReservoir(AReservoir:IReservoirData;AReservoirsCount: integer; AFirstReservoir: boolean): boolean;
    function DeleteReservoir(AReservoir:IReservoirData;AReservoirsCount: integer; ALastReservoir: boolean): boolean;
    function CreateNodeWithInflow(ANode:IReservoirData): boolean;
    function DeleteNodeWithInflow(ANode:IReservoirData): boolean;
    function CreateNodeWithoutInflow(ANode:IReservoirData): boolean;
    function UpdatePhysicalCharacteristicsRow(AReservoir:IReservoirData): boolean;

    function DeleteNodeWithoutInflow(ANode:IReservoirData): boolean;
    function DeleteFromNVDrawingInstSQL (AElementID : integer): string;
    function DeleteFromNVDrawingInstCaptionSQL (AElementID : integer): string;
    function DeleteFromNVDrawingInstChannelSQL (AElementID : integer): string;
    function DeleteFromNVDrawingPosSQL (AElementID : integer): string;
    function DeleteFromNVDrawingPropSQL (AElementID : integer): string;
    procedure DeleteFromNVTables (AElementID : integer);
    function GetMaxReservoirNumber: integer;
    function GetMaxReservoirIdentifier: integer;
    function GetMaxReservoirInitialLevelsIdentifier: integer;
    function GetReservoirAndNodesWithInflowCount: integer;

    function CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AReservoirNumberList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;

    function InsertReservoirAreaGroup(var AGroupID: integer): boolean;
    function GetMaxReservoirAreaGroupId: integer;
    function GetMaxReservoirAreaGroupIdSQL: string;
  end;

implementation

uses
  Math,
  Data.DB,
  SysUtils,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

procedure TReservoirDataSQLAgent.LoadNodesDetailsContextData(AContextData: TStringList; AReservoirIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadNodesDetailsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('NodeNumberStorage='    + AReservoirIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadReservoirNamesContextData(AContextData: TStringList;
  ARecordIdentifier, ANodeCount, APenaltyStructure: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadReservoirNamesContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('NodeCount='     + ANodeCount);
    AContextData.Add('ModelElementID='+ ANodeCount);
    AContextData.Add('PenaltyStruct=' + APenaltyStructure);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadInitialLevelsContextData(AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadInitialLevelsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ReservoirNodeNumber='    + AReservoirIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadEvaporationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadEvaporationsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadElevationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadElevationsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadVolumesContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadVolumesContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadAreasContextData(AContextData: TStringList; ARecordIdentifier,AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadAreasContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadFixedElevationsContextData(
  AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadFixedElevationsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('NodeNumberStorage=' + AReservoirIdentifier);
    AContextData.Add('ModelElementID=' + AReservoirIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataSQLAgent.LoadDrawDownElevationsContextData(
  AContextData: TStringList; AReservoirIdentifier, ALevelIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadDrawDownElevationsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ReservoirIdentifier=' + AReservoirIdentifier);
    AContextData.Add('LevelIdentifier=' + ALevelIdentifier);
    AContextData.Add('ModelElementID=' + AReservoirIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TReservoirDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetNoAliasScenarioWhereClause: string;
const OPNAME = 'TReservoirDataSQLAgent.GetNoAliasScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirNamesDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirNamesDataSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario,  ' +
      '   A.Identifier AS RecordIdentifier,   ' +
      '   A.NodeCount AS ReservoirIdentifier, ' +
      '   A.PenaltyStruct, ' +
      '   A.ReservoirName,  ' +
      '   A.IncludeSummary,  ' +
      '   A.PointsCount,  ' +
      '   A.DrainageScale,  ' +
      '   A.AfforestationScale,  ' +
      '   A.UrbanRunoff,  ' +
      '   A.NaturalInflowChannel,  ' +
      '   A.IrrigationScale,  ' +
      '   A.AreaFull,  ' +
      '   A.CatchmentRef,  ' +
      '   A.RainCoef, ' +
      '   A.NodeType,  ' +
      '   A.DamLevelsFileName,'+
      '   A.XCoord,'+
      '   A.YCoord,'+
      '   A.GroupID,'+
      '   B.NodeNumberStorage,  ' +
      '   B.StatusIndicator,  ' +
      '   B.BottomOfReservoir,  ' +
      '   B.DeadStorageLevel,  ' +
      '   B.ReservoirPriority,  ' +
      '   B.FullSupplyLevel  ' +
      ' FROM ReservoirDetails A, NodesDetails B WHERE ' +
        GetScenarioWhereClause + ' AND ' +
      ' (A.NodeType in  (1,5,7,8,10))  AND ' +
      ' (B.Model             = A.Model) AND ' +
      ' (B.StudyAreaName     = A.StudyAreaName) AND ' +
      ' (B.SubArea           = A.SubArea) AND ' +
      ' (B.Scenario          = A.Scenario) AND ' +
      ' (B.NodeNumberStorage = A.NodeCount)' +
      ' ORDER BY A.NodeCount ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetNodesWithInflowNamesDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetNodesWithInflowNamesDataSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario,  ' +
      '   A.Identifier AS RecordIdentifier,   ' +
      '   A.NodeCount AS ReservoirIdentifier, ' +
      '   A.PenaltyStruct, ' +
      '   A.ReservoirName,  ' +
      '   A.IncludeSummary,  ' +
      '   A.PointsCount,  ' +
      '   A.DrainageScale,  ' +
      '   A.AfforestationScale,  ' +
      '   A.IrrigationScale,  ' +
      '   A.UrbanRunoff,  ' +
      '   A.NaturalInflowChannel,  ' +
      '   A.CatchmentRef,  ' +
      '   A.NodeType,  ' +
      '   A.RainCoef, ' +
      '   A.DamLevelsFileName,'+
      '   A.XCoord,'+
      '   A.YCoord,'+
      '   A.GroupID,'+
      '   0 AS NodeNumberStorage,  ' +
      '   0 AS StatusIndicator,  ' +
      '   0.0 AS BottomOfReservoir,  ' +
      '   0.0 AS DeadStorageLevel,  ' +
      '   0.0 AS FullSupplyLevel,  ' +
      '   0.0 AS ReservoirPriority  ' +
      ' FROM ReservoirDetails A WHERE ' +
        GetScenarioWhereClause + ' AND ' +
      '   (A.NodeType = 2) ' +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetNodesWithoutInflowNamesDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetNodesWithoutInflowNamesDataSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario,  ' +
      '   A.Identifier AS RecordIdentifier,   ' +
      '   A.NodeCount AS ReservoirIdentifier, ' +
      '   0 AS PenaltyStruct, ' +
      '   A.ReservoirName,  ' +
      QuotedStr('N') + ' AS IncludeSummary,  ' +
      '   0 AS PointsCount,  ' +
      '   0.0 AS DrainageScale,  ' +
      '   0.0 AS AfforestationScale,  ' +
      '   0.0 AS IrrigationScale,  ' +
      '   0.0 AS UrbanRunoff,  ' +
      '   A.NaturalInflowChannel,  ' +
      '   0 AS CatchmentRef,  ' +
      '   A.NodeType,  ' +
      '   A.RainCoef, ' +
      '   A.DamLevelsFileName,'+
      '   A.XCoord,'+
      '   A.YCoord,'+
      '   0 AS GroupID,'+
      '   0 AS NodeNumberStorage,  ' +
      '   0 AS StatusIndicator,  ' +
      '   0.0 AS BottomOfReservoir,  ' +
      '   0.0 AS DeadStorageLevel,  ' +
      '   0.0 AS FullSupplyLevel,  ' +
      '   0.0 AS ReservoirPriority  ' +
      ' FROM ReservoirDetails A WHERE ' +
        GetScenarioWhereClause + ' AND ' +
      '   (A.NodeType in (3,4,6,9,11,12,13)) ' +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirAreaDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirAreaDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              ' Area01, Area02, Area03, Area04, Area05, Area06, Area07, Area08, ' +
              ' Area09, Area10, Area11, Area12, Area13, Area14, Area15 FROM ReservoirArea A WHERE ' +
        GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.InsertReservoirAreaGroupSQL(AGroupID: integer): string;
const OPNAME = 'TReservoirDataSQLAgent.InsertReservoirAreaGroupSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO ReservoirGroup '+
      '(Model, StudyAreaName, SubArea, Scenario, GroupID,GroupName) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AGroupID) + ',' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('TField.ReservoirAreaGroupName')) + ' ' + IntToStr(AGroupID)) +
      ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.DeleteReservoirAreaGroup(AGroupID: integer): boolean;
const OPNAME = 'TReservoirDataSQLAgent.DeleteReservoirAreaGroup';
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
        LDataSet.SetSQL(DeleteReservoirAreaGroupSQL(AGroupID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataSQLAgent.DeleteReservoirAreaGroupSQL(AGroupID: integer): string;
const OPNAME = 'TReservoirDataSQLAgent.DeleteReservoirAreaGroupSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ReservoirGroup A WHERE ' +
                GetScenarioWhereClause +
                'AND A.GroupID = '+ IntToStr(AGroupID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TReservoirDataSQLAgent.GetReservoirVolumeDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirVolumeDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              ' Volume01, Volume02, Volume03, Volume04, Volume05, Volume06, Volume07, Volume08, ' +
              ' Volume09, Volume10, Volume11, Volume12, Volume13, Volume14, Volume15 FROM ReservoirVolume A WHERE ' +
        GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirElevationsDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirElevationsDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              ' ReservoirElev01, ReservoirElev02, ReservoirElev03, ReservoirElev04, ReservoirElev05, ReservoirElev06, ReservoirElev07, ReservoirElev08, ' +
              ' ReservoirElev09, ReservoirElev10, ReservoirElev11, ReservoirElev12, ReservoirElev13, ReservoirElev14, ReservoirElev15 FROM ReservoirElevation A WHERE ' +
        GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirInitialLevelsDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirInitialLevelsDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT ReservoirNodeNumber AS ReservoirIdentifier, ' +
              ' ResInitialLevelsLev01, ResInitialLevelsLev02, ResInitialLevelsLev03, ResInitialLevelsLev04,' +
              ' ResInitialLevelsLev05, ResInitialLevelsLev06, ResInitialLevelsLev07, ResInitialLevelsLev08 FROM ReservoirInitialLevels A WHERE ' +
        GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirEvaporationDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirEvaporationDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              ' Evapo01, Evapo02, Evapo03, Evapo04, Evapo05, Evapo06, Evapo07, Evapo08, ' +
              ' Evapo09, Evapo10, Evapo11, Evapo12 FROM ReservoirEvap A WHERE ' +
        GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetDrawDownElevationsSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetDrawDownElevationsSQL';
begin
  Result := '';
  try
    Result := 'SELECT LevelIdentifier,ReservoirIdentifier,' +
              '  ReservoirLev01,ReservoirLev02,ReservoirLev03,ReservoirLev04,ReservoirLev05,ReservoirLev06,ReservoirLev07,ReservoirLev08,ReservoirLev09,ReservoirLev10,ReservoirLev11,ReservoirLev12 ' +
              'FROM ReservoirLevels A WHERE '+
                   GetScenarioWhereClause +
      '   ORDER BY A.ReservoirIdentifier, A.LevelIdentifier' ;

{
    // This order must the same as the order in the reservoir details table
    // (the outer join is required)
    // or the records could get out of sync
    Result := '  SELECT LevelIdentifier,ReservoirIdentifier,                          ' +
              '    ReservoirLev01,ReservoirLev02,ReservoirLev03,ReservoirLev04,       ' +
              '    ReservoirLev05, ReservoirLev06,ReservoirLev07,ReservoirLev08,      ' +
              '    ReservoirLev09,ReservoirLev10,ReservoirLev11,ReservoirLev12        ' +
              '  FROM ReservoirLevels A                                               ' +
              '    Left Outer Join ReservoirDetails B on                              ' +
              '      (A.ReservoirIdentifier = B.NodeCount                             ' +
              '      and A.Model = B.Model and A.StudyAreaName = B.StudyAreaName      ' +
              '      and A.SubArea = B.SubArea and A.Scenario = B.Scenario)           ' +
              '  WHERE ' +
              GetScenarioWhereClause +
              '  ORDER BY B.Identifier; ' ;
 }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirAndNodesNamesDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirAndNodesNamesDataSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario,  ' +
      '   A.Identifier AS RecordIdentifier,   ' +
      '   A.NodeCount AS ReservoirIdentifier, ' +
      '   A.PenaltyStruct, ' +
      '   A.ReservoirName,  ' +
      '   A.IncludeSummary,  ' +
      '   A.PointsCount,  ' +
      '   A.DrainageScale,  ' +
      '   A.AfforestationScale,  ' +
      '   A.IrrigationScale,  ' +
      '   A.UrbanRunoff,  ' +
      '   A.NaturalInflowChannel,  ' +
      '   A.CatchmentRef,  ' +
      '   A.NodeType,  ' +
      '   A.RainCoef, ' +
      '   A.DamLevelsFileName,'+
      '   A.XCoord,'+
      '   A.YCoord,'+
      '   A.GroupID,'+
      '   B.NodeNumberStorage,  ' +
      '   B.StatusIndicator,  ' +
      '   B.BottomOfReservoir,  ' +
      '   B.DeadStorageLevel,  ' +
      '   B.ReservoirPriority,  ' +
      '   B.FullSupplyLevel  ' +
      ' FROM ReservoirDetails A, NodesDetails B WHERE ' +
        GetScenarioWhereClause + ' AND ' +
      ' (B.Model             = A.Model) AND ' +
      ' (B.StudyAreaName     = A.StudyAreaName) AND ' +
      ' (B.SubArea           = A.SubArea) AND ' +
      ' (B.Scenario          = A.Scenario) AND ' +
      ' (B.NodeNumberStorage = A.NodeCount)' +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.CreateReservoir(AReservoir: IReservoirData;
         AReservoirsCount: integer;AFirstReservoir: boolean): boolean;
const OPNAME = 'TReservoirDataSQLAgent.CreateReservoir';
      ReservoirDetailsSQL       = 'SELECT * FROM ReservoirDetails WHERE ';
      ReservoirAreaSQL          = 'SELECT * FROM ReservoirArea WHERE ';
      ReservoirElevationSQL     = 'SELECT * FROM ReservoirElevation WHERE ';
      ReservoirEvaporationsSQL  = 'SELECT * FROM ReservoirEvap WHERE ';
      ReservoirVolumeSQL        = 'SELECT * FROM ReservoirVolume WHERE ';
      ReservoirCountsSQL        = 'SELECT * FROM Reservoir WHERE ';
      ReservoirChannelsSQL      = 'SELECT * FROM ReservoirChannels WHERE ';
      ReservoirLevelSQL         = 'SELECT * FROM ReservoirLevels WHERE ';
      ReservoirInitialLevelsSQL = 'SELECT * FROM ReservoirInitialLevels WHERE ';
      NodesDetailsSQL           = 'SELECT * FROM NodesDetails WHERE ';
      RunTitleSQL               = 'SELECT * FROM RunTitle WHERE ';
      StorageZonesSQL           = 'SELECT * FROM StorageZones WHERE ';
var
  LRecordID,
  LIndentifier,
  LCount,
  LIndex: integer;
  LDataSet: TAbstractModelDataset;
  LFieldName,
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
  LImportDate : TDateTime;
 begin
  Result := False;
  try
    if Assigned(AReservoir) then
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

            LSQL := NodesDetailsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirArea cannot be set to updateble.');
            end
            else
            begin
              LIndentifier := 0;
              while not LDataset.DataSet.Eof do
              begin
                LIndentifier := Max(LIndentifier,LDataset.DataSet.FieldByName('Identifier').AsInteger);
                LDataset.DataSet.Next;
              end;
              LIndentifier := LIndentifier + 1;

              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := LIndentifier;
              //AReservoir.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('NodeNumberStorage').AsInteger := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
              LDataset.DataSet.FieldByName('StatusIndicator').AsInteger := AReservoir.ReservoirConfigurationData.StatusIndicator;
              LDataset.DataSet.FieldByName('ReservoirPriority').AsFloat := AReservoir.ReservoirConfigurationData.Priority;
              LDataset.DataSet.FieldByName('FullSupplyLevel').AsFloat := AReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
              LDataset.DataSet.FieldByName('DeadStorageLevel').AsFloat :=  AReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
              LDataset.DataSet.FieldByName('BottomOfReservoir').AsFloat := AReservoir.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirLevelSQL + GetNoAliasScenarioWhereClause;
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
              LRecordID := Get_HighestReservoirLevelRecordIdentifier;
              for LCount := 0 to AReservoir.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount -1 do
              begin
                LDataset.DataSet.Append;
                LDataset.DataSet.FieldByName('Model').AsString := LModel;
                LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
                LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
                LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

                LRecordID := LRecordID + 1;
                LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger := LRecordID;
                LDataset.DataSet.FieldByName('ReservoirIdentifier').AsInteger := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
                //AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
                LDataset.DataSet.FieldByName('LevelIdentifier').AsInteger := LCount+1;
                for LIndex := 1 to 12 do
                begin
                  LFieldName := Format('%s%2.2d',['ReservoirLev',LIndex]);
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat := AReservoir.ReservoirZoneElevationsData.DrawDownLevelByIndex[LCount].MonthlyElevationByIndex[LIndex];
                end;
                LDataset.DataSet.Post;
              end;
            end;


            LSQL := ReservoirInitialLevelsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirInitialLevels cannot be set to updateble.');
            end
            else
            begin
              LIndentifier := -1;
              while not LDataset.DataSet.Eof do
              begin
                LIndentifier := Max(LIndentifier,LDataset.DataSet.FieldByName('Identifier').AsInteger);
                LDataset.DataSet.Next;
              end;
              LIndentifier := LIndentifier + 1;

              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := LIndentifier;
              LDataset.DataSet.FieldByName('ReservoirNodeNumber').AsInteger := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
              LDataset.DataSet.FieldByName('ResInitialLevelsLev01').AsFloat := 0.0;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirDetailsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirDetails cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := AReservoir.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('ReservoirName').AsString := AReservoir.ReservoirConfigurationData.ReservoirName;
              LDataset.DataSet.FieldByName('IncludeSummary').AsString := AReservoir.ReservoirConfigurationData.IncludeSummary;
              LDataset.DataSet.FieldByName('NodeCount').AsInteger := AReservoir.ReservoirConfigurationData.ReservoirIdentifier;
              LDataset.DataSet.FieldByName('PenaltyStruct').AsInteger := AReservoir.ReservoirConfigurationData.PenaltyStructIdentifier;
              LDataset.DataSet.FieldByName('PointsCount').AsInteger := AReservoir.ReservoirConfigurationData.PointsCount;
              LDataset.DataSet.FieldByName('DrainageScale').AsFloat:= AReservoir.ReservoirConfigurationData.DrainageScale;
              LDataset.DataSet.FieldByName('AfforestationScale').AsFloat:= AReservoir.ReservoirConfigurationData.AfforestationScale;
              LDataset.DataSet.FieldByName('IrrigationScale').AsFloat:= AReservoir.ReservoirConfigurationData.IrrigationScale;
              LDataset.DataSet.FieldByName('UrbanRunoff').AsFloat:= AReservoir.ReservoirConfigurationData.UrbanRunOff;
              LDataset.DataSet.FieldByName('AreaFull').AsFloat:= AReservoir.ReservoirConfigurationData.AreaWhenFull;
              LDataset.DataSet.FieldByName('RainCoef').AsFloat:= AReservoir.ReservoirConfigurationData.RainCoef;
              LDataset.DataSet.FieldByName('CatchmentRef').AsInteger := AReservoir.ReservoirConfigurationData.CatchmentRef;
              LDataset.DataSet.FieldByName('ChannelsCount').AsInteger := 0;
              LDataSet.DataSet.FieldByName('RainCoef').AsFloat := AReservoir.ReservoirConfigurationData.RainCoef;
              LDataset.DataSet.FieldByName('NodeType').AsInteger := Integer(AReservoir.ReservoirConfigurationData.NodeType);
              LDataset.DataSet.FieldByName('GroupID').AsInteger := Integer(AReservoir.ReservoirConfigurationData.GroupID);
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirAreaSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirArea cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := AReservoir.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('Area01').AsFloat := AReservoir.ReservoirAreasData.ReservoirAreasByIndex[1];
              LDataset.DataSet.FieldByName('Area02').AsFloat := AReservoir.ReservoirAreasData.ReservoirAreasByIndex[2];
              LDataset.DataSet.FieldByName('Area03').AsFloat := AReservoir.ReservoirAreasData.ReservoirAreasByIndex[3];

              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirElevationSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirElevation cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := AReservoir.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('ReservoirElev01').AsFloat := AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[1];
              LDataset.DataSet.FieldByName('ReservoirElev02').AsFloat := AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[2];
              LDataset.DataSet.FieldByName('ReservoirElev03').AsFloat := AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[3];

              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirChannelsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirChannels cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
              LDataset.DataSet.FieldByName('Identifier').AsInteger := AReservoir.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirEvaporationsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirEvaporations cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := AReservoir.ReservoirConfigurationData.RecordIdentifier;
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['Evapo',LIndex]);
                LDataset.DataSet.FieldByName(LFieldName).AsFloat := AReservoir.ReservoirEvaporationsData.MonthlyEvaporationsByIndex[LIndex];
              end;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirVolumeSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirVolume cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := AReservoir.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('Volume01').AsFloat := AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[1];
              LDataset.DataSet.FieldByName('Volume02').AsFloat := AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[2];
              LDataset.DataSet.FieldByName('Volume03').AsFloat := AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[3];

              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirCountsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDataset.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table Reservoir returned more than one record to update reservoir count.');

              if (LDataset.DataSet.RecordCount = 0) then
              begin
                LDataset.DataSet.Append;
                LDataset.DataSet.FieldByName('Model').AsString := LModel;
                LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
                LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
                LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
                LDataset.DataSet.FieldByName('ReservoirCount').AsInteger := 1;
                LDataset.DataSet.FieldByName('HydroUnitsCode').AsString := 'MCM';
              end
              else
              begin
                LCount := 0;
                if not LDataset.DataSet.FieldByName('ReservoirCount').IsNull then
                  LCount := LDataset.DataSet.FieldByName('ReservoirCount').AsInteger;
                LCount := LCount + 1;
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('ReservoirCount').AsInteger := LCount;
              end;
              LDataset.DataSet.Post;
            end;

            LSQL := StorageZonesSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDataset.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table StorageZones returned more than one record to update reservoir count.');

              if (LDataset.DataSet.RecordCount = 1) then
              begin
                LCount := AReservoir.ReservoirZoneElevationsData.ReservoirZoneLevelsCount -3;
                if(LCount < 0) then
                  LCount := 0;
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('NodesCount').AsInteger := AReservoirsCount;
                LDataset.DataSet.FieldByName('ReservoirLevelsCount').AsInteger := LCount*AReservoirsCount;
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

function TReservoirDataSQLAgent.DeleteReservoir (AReservoir       : IReservoirData;
                                                 AReservoirsCount : integer;
                                                 ALastReservoir   : boolean): boolean;
const OPNAME = 'TReservoirDataSQLAgent.DeleteReservoir';
      ReservoirDetailsSQL       = 'DELETE FROM ReservoirDetails WHERE ';
      NodesDetailsSQL           = 'DELETE FROM NodesDetails WHERE ';
      ReservoirLevelSQL         = 'DELETE FROM ReservoirLevels WHERE ';
      ReservoirAreaSQL          = 'DELETE FROM ReservoirArea WHERE ';
      ReservoirElevationSQL     = 'DELETE FROM ReservoirElevation WHERE ';
      ReservoirEvaporationsSQL  = 'DELETE FROM ReservoirEvap WHERE ';
      ReservoirInitialLevelsSQL = 'DELETE FROM ReservoirInitialLevels WHERE ';
      ReservoirVolumeSQL        = 'DELETE FROM ReservoirVolume WHERE ';
      ReservoirChannelsSQL      = 'DELETE FROM ReservoirChannels WHERE ';
      ReservoirCountsSQL        = 'SELECT * FROM Reservoir WHERE ';
      StorageZonesSQL           = 'SELECT * FROM StorageZones WHERE ';
      RunTitleSQL               = 'DELETE FROM RunTitle WHERE ';
var
  LCount: integer;
  LDataSet: TAbstractModelDataset;
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(AReservoir) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin

          FAppModules.Database.StartTransaction;
          try

            LSQL := ReservoirDetailsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := NodesDetailsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (NodeNumberStorage = '+ IntToStr(AReservoir.ReservoirConfigurationData.ReservoirIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirLevelSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (ReservoirIdentifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.ReservoirIdentifier) +
                           ') AND (LevelIdentifier >= 0)';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirAreaSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirElevationSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirEvaporationsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirInitialLevelsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (ReservoirNodeNumber = '+ IntToStr(AReservoir.ReservoirConfigurationData.ReservoirIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirVolumeSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirChannelsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(AReservoir.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            If ALastReservoir then
            begin
              LSQL := RunTitleSQL + GetNoAliasScenarioWhereClause;
              LDataSet.DataSet.Close;
              LDataSet.SetSQL(LSQL);
              LDataset.ExecSQL;
            end;

            LSQL := ReservoirCountsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDataset.DataSet.RecordCount = 0) then
                raise Exception.Create('Query to table Reservoir returned no record to update reservoir count.');
              if (LDataset.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table Reservoir returned more than one record to update reservoir count.');

              LCount := 1;
              if not LDataset.DataSet.FieldByName('ReservoirCount').IsNull then
                LCount := LDataset.DataSet.FieldByName('ReservoirCount').AsInteger;
              LCount := LCount -1;

              if (LCount = 0) then
                LDataset.DataSet.Delete
              else
              begin
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('ReservoirCount').AsInteger := LCount;
                LDataset.DataSet.Post;
              end;
            end;

            LSQL := StorageZonesSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDataset.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table StorageZones returned more than one record to update reservoir count.');

              if (LDataset.DataSet.RecordCount = 1) then
              begin
                LCount := AReservoir.ReservoirZoneElevationsData.ReservoirZoneLevelsCount -3;
                if(LCount < 0) then
                  LCount := 0;
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('NodesCount').AsInteger := AReservoirsCount;
                LDataset.DataSet.FieldByName('ReservoirLevelsCount').AsInteger := LCount*AReservoirsCount;
                LDataset.DataSet.Post;
              end;
            end;

            LSQL := 'DELETE FROM ReservoirTimeControl WHERE ' +
                    GetNoAliasScenarioWhereClause +
                    ' AND (ReservoirNumber = ' +
                    IntToStr(AReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

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

function TReservoirDataSQLAgent.CreateNodeWithInflow(ANode: IReservoirData): boolean;
const OPNAME = 'TReservoirDataSQLAgent.CreateNodeWithInflow';
      ReservoirDetailsSQL       = 'SELECT * FROM ReservoirDetails WHERE ';
      ReservoirAreaSQL          = 'SELECT * FROM ReservoirArea WHERE ';
      ReservoirElevationSQL     = 'SELECT * FROM ReservoirElevation WHERE ';
      ReservoirEvaporationsSQL  = 'SELECT * FROM ReservoirEvap WHERE ';
      ReservoirVolumeSQL        = 'SELECT * FROM ReservoirVolume WHERE ';
      ReservoirCountsSQL        = 'SELECT * FROM Reservoir WHERE ';
      ReservoirChannelsSQL      = 'SELECT * FROM ReservoirChannels WHERE ';
var
  LCount : integer;
  LDataSet: TAbstractModelDataset;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(ANode) then
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

            LSQL := ReservoirDetailsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirDetails cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('ReservoirName').AsString := ANode.ReservoirConfigurationData.ReservoirName;
              LDataset.DataSet.FieldByName('IncludeSummary').AsString := ANode.ReservoirConfigurationData.IncludeSummary;
              LDataset.DataSet.FieldByName('NodeCount').AsInteger := ANode.ReservoirConfigurationData.ReservoirIdentifier;
              LDataset.DataSet.FieldByName('PenaltyStruct').AsInteger := ANode.ReservoirConfigurationData.PenaltyStructIdentifier;
              LDataset.DataSet.FieldByName('PointsCount').AsInteger := ANode.ReservoirConfigurationData.PointsCount;
              LDataset.DataSet.FieldByName('DrainageScale').AsFloat:= ANode.ReservoirConfigurationData.DrainageScale;
              LDataset.DataSet.FieldByName('AfforestationScale').AsFloat:= ANode.ReservoirConfigurationData.AfforestationScale;
              LDataset.DataSet.FieldByName('IrrigationScale').AsFloat:= ANode.ReservoirConfigurationData.IrrigationScale;
              LDataset.DataSet.FieldByName('UrbanRunoff').AsFloat:= ANode.ReservoirConfigurationData.UrbanRunOff;
              LDataset.DataSet.FieldByName('AreaFull').AsFloat:= ANode.ReservoirConfigurationData.AreaWhenFull;
              LDataset.DataSet.FieldByName('RainCoef').AsFloat:= 0.0;
              LDataset.DataSet.FieldByName('CatchmentRef').AsInteger := ANode.ReservoirConfigurationData.CatchmentRef;
              LDataset.DataSet.FieldByName('ChannelsCount').AsInteger := 0;
              LDataset.DataSet.FieldByName('NodeType').AsInteger := Integer(ANode.ReservoirConfigurationData.NodeType);
              LDataset.DataSet.FieldByName('GroupID').AsInteger := Integer(ANode.ReservoirConfigurationData.GroupID);
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirAreaSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirArea cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirElevationSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirElevation cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirEvaporationsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirEvaporations cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.Post;
            end;


            LSQL := ReservoirVolumeSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirVolume cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirChannelsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirVolume cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.Post;
            end;

            LSQL := ReservoirCountsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDataset.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table Reservoir returned more than one record to update reservoir count.');

              if (LDataset.DataSet.RecordCount = 0) then
              begin
                LDataset.DataSet.Append;
                LDataset.DataSet.FieldByName('Model').AsString := LModel;
                LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
                LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
                LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
                LDataset.DataSet.FieldByName('ReservoirCount').AsInteger := 1;
                LDataset.DataSet.FieldByName('HydroUnitsCode').AsString := 'MCM';
              end
              else
              begin
                LCount := 0;
                if not LDataset.DataSet.FieldByName('ReservoirCount').IsNull then
                  LCount := LDataset.DataSet.FieldByName('ReservoirCount').AsInteger;
                LCount := LCount + 1;
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('ReservoirCount').AsInteger := LCount;
              end;
              LDataset.DataSet.Post;
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

function TReservoirDataSQLAgent.DeleteNodeWithInflow(ANode: IReservoirData): boolean;
const OPNAME = 'TReservoirDataSQLAgent.DeleteNodeWithInflow';
      ReservoirDetailsSQL       = 'DELETE FROM ReservoirDetails WHERE ';
      ReservoirAreaSQL          = 'DELETE FROM ReservoirArea WHERE ';
      ReservoirElevationSQL     = 'DELETE FROM ReservoirElevation WHERE ';
      ReservoirEvaporationsSQL  = 'DELETE FROM ReservoirEvap WHERE ';
      ReservoirVolumeSQL        = 'DELETE FROM ReservoirVolume WHERE ';
      ReservoirChannelsSQL      = 'DELETE FROM ReservoirChannels WHERE ';
      ReservoirCountsSQL        = 'SELECT * FROM Reservoir WHERE ';
var
  LCount: integer;
  LDataSet: TAbstractModelDataset;
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(ANode) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin

          FAppModules.Database.StartTransaction;
          try

            LSQL := ReservoirDetailsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirAreaSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirElevationSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirEvaporationsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirVolumeSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

            LSQL := ReservoirChannelsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;
            

            LSQL := ReservoirCountsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDataset.DataSet.RecordCount = 0) then
                raise Exception.Create('Query to table Reservoir returned no record to update reservoir count.');
              if (LDataset.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table Reservoir returned more than one record to update reservoir count.');

              LCount := 1;
              if not LDataset.DataSet.FieldByName('ReservoirCount').IsNull then
                LCount := LDataset.DataSet.FieldByName('ReservoirCount').AsInteger;
              LCount := LCount -1;

              if (LCount = 0) then
                LDataset.DataSet.Delete
              else
              begin
                LDataset.DataSet.Edit;
                LDataset.DataSet.FieldByName('ReservoirCount').AsInteger := LCount;
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

function TReservoirDataSQLAgent.CreateNodeWithoutInflow(ANode: IReservoirData): boolean;
const OPNAME = 'TReservoirDataSQLAgent.CreateNodeWithoutInflow';
      ReservoirDetailsSQL       = 'SELECT * FROM ReservoirDetails WHERE ';
var
  LDataSet: TAbstractModelDataset;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(ANode) then
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

            LSQL := ReservoirDetailsSQL + GetNoAliasScenarioWhereClause;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirDetails cannot be set to updateble.');
            end
            else
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Identifier').AsInteger := ANode.ReservoirConfigurationData.RecordIdentifier;
              LDataset.DataSet.FieldByName('ReservoirName').AsString := ANode.ReservoirConfigurationData.ReservoirName;
              LDataset.DataSet.FieldByName('NodeCount').AsInteger := ANode.ReservoirConfigurationData.ReservoirIdentifier;
              LDataset.DataSet.FieldByName('NodeType').AsInteger := Integer(ANode.ReservoirConfigurationData.NodeType);
              LDataset.DataSet.Post;
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

function TReservoirDataSQLAgent.DeleteNodeWithoutInflow(ANode: IReservoirData): boolean;
const OPNAME = 'TReservoirDataSQLAgent.DeleteNodeWithoutInflow';
      ReservoirDetailsSQL       = 'DELETE FROM ReservoirDetails WHERE ';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(ANode) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin

          FAppModules.Database.StartTransaction;
          try

            LSQL := ReservoirDetailsSQL + GetNoAliasScenarioWhereClause;
            LSQL := LSQL + ' AND (Identifier = '+ IntToStr(ANode.ReservoirConfigurationData.RecordIdentifier) +
                           ')';
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.ExecSQL;

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

function TReservoirDataSQLAgent.GetIrrigationNodesNamesDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetIrrigationNodesNamesDataSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario,  ' +
      '   A.Identifier AS RecordIdentifier,   ' +
      '   A.NodeCount AS ReservoirIdentifier, ' +
      '   0 AS PenaltyStruct, ' +
      '   A.ReservoirName,  ' +
      QuotedStr('N') + ' AS IncludeSummary,  ' +
      '   0 AS PointsCount,  ' +
      '   0.0 AS DrainageScale,  ' +
      '   0.0 AS AfforestationScale,  ' +
      '   0.0 AS IrrigationScale,  ' +
      '   0.0 AS UrbanRunoff,  ' +
      '   A.NaturalInflowChannel,  ' +
      '   0 AS CatchmentRef,  ' +
      '   A.NodeType,  ' +
      '   0 AS NodeNumberStorage,  ' +
      '   0 AS StatusIndicator,  ' +
      '   0.0 AS BottomOfReservoir,  ' +
      '   0.0 AS DeadStorageLevel,  ' +
      '   0.0 AS FullSupplyLevel,  ' +
      '   0.0 AS ReservoirPriority,  ' +
      '   0.0 AS RainCoef, ' +
      '   XCoord,'+
      '   YCoord,'+
      '   0 AS GroupID,'+
      '   A.DamLevelsFileName' +
      ' FROM ReservoirDetails A WHERE ' +
        GetScenarioWhereClause + ' AND ' +
      '   (A.NodeType = 4) ' +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.DeleteFromNVDrawingInstSQL (AElementID : integer): string;
const OPNAME = 'TReservoirDataSQLAgent.DeleteFromNVDrawingInstSQL';
begin
  Result := '';
  try
    Result :=
      'DELETE FROM NVDrawingInst A WHERE ' +
      GetScenarioWhereClause +
      ' AND A.ElementID = ' + IntToStr(AElementID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.DeleteFromNVDrawingInstCaptionSQL (AElementID : integer): string;
const OPNAME = 'TReservoirDataSQLAgent.DeleteFromNVDrawingInstCaptionSQL';
begin
  Result := '';
  try
    Result :=
      'DELETE FROM NVDrawingInstCaption A WHERE ' +
      GetScenarioWhereClause +
      ' AND A.ElementID = ' + IntToStr(AElementID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.DeleteFromNVDrawingInstChannelSQL (AElementID : integer): string;
const OPNAME = 'TReservoirDataSQLAgent.DeleteFromNVDrawingInstChannelSQL';
begin
  Result := '';
  try
    Result :=
      'DELETE FROM NVDrawingInstChannel A WHERE ' +
      GetScenarioWhereClause +
      ' AND A.ElementID = ' + IntToStr(AElementID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.DeleteFromNVDrawingPosSQL (AElementID : integer): string;
const OPNAME = 'TReservoirDataSQLAgent.DeleteFromNVDrawingPosSQL';
begin
  Result := '';
  try
    Result :=
      'DELETE FROM NVDrawingPos A WHERE ' +
      GetScenarioWhereClause +
      ' AND A.ElementID = ' + IntToStr(AElementID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.DeleteFromNVDrawingPropSQL (AElementID : integer): string;
const OPNAME = 'TReservoirDataSQLAgent.DeleteFromNVDrawingPropSQL';
begin
  Result := '';
  try
    Result :=
      'DELETE FROM NVDrawingProp A WHERE ' +
      GetScenarioWhereClause +
      ' AND A.ElementID = ' + IntToStr(AElementID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.UpdatePhysicalCharacteristicsRow(AReservoir: IReservoirData): boolean;
const OPNAME = 'TReservoirDataSQLAgent.UpdatePhysicalCharacteristicsRow';
      ReservoirAreaSQL       = 'SELECT * FROM ReservoirArea WHERE ';
      ReservoirElevationSQL  = 'SELECT * FROM ReservoirElevation WHERE ';
      ReservoirVolumeSQL     = 'SELECT * FROM ReservoirVolume WHERE ';
var
  LCount,
  LIndex,
  LIdentifier: integer;
  LDataSet: TAbstractModelDataset;
  LFieldName,
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    if Assigned(AReservoir) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LModel         := FAppModules.StudyArea.ModelCode;
          LStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
          LSubArea       := FAppModules.StudyArea.SubAreaCode;
          LScenario      := FAppModules.StudyArea.ScenarioCode;
          LIdentifier    := AReservoir.ReservoirConfigurationData.RecordIdentifier;

          FAppModules.Database.StartTransaction;
          try
            // ReservoirArea
            LSQL := ReservoirAreaSQL + GetNoAliasScenarioWhereClause + ' AND Identifier = '+ IntToStr(LIdentifier);
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirArea cannot be set to updateble.');
            end
            else
            if (LDataset.DataSet.RecordCount = 0)   then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirArea returned no records.');
            end
            else
            if (LDataset.DataSet.RecordCount > 1)   then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirArea returned multiple records when a single record is expected.');
            end
            else
            begin
              LDataset.DataSet.Edit;
              LIndex := 0;
              for LCount := AReservoir.ReservoirAreasData.StartArea  downto 1 do
              begin
                LIndex := LIndex + 1;
                LFieldName := Format('%s%2.2d',['Area',LIndex]);
                if(AReservoir.ReservoirAreasData.ReservoirAreasByIndex[LCount] = NullFloat) then
                  LDataset.DataSet.FieldByName(LFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat :=
                  AReservoir.ReservoirAreasData.ReservoirAreasByIndex[LCount];
              end;

              for LIndex := AReservoir.ReservoirAreasData.StartArea + 1 to 15 do
              begin
                LFieldName := Format('%s%2.2d',['Area',LIndex]);
                if(AReservoir.ReservoirAreasData.ReservoirAreasByIndex[LIndex] = NullFloat) then
                  LDataset.DataSet.FieldByName(LFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat :=
                    AReservoir.ReservoirAreasData.ReservoirAreasByIndex[LIndex];
              end;
              LDataset.DataSet.Post;
            end;

            // ReservoirElevation
            LSQL := ReservoirElevationSQL + GetNoAliasScenarioWhereClause + ' AND Identifier = '+ IntToStr(LIdentifier);
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirElevation cannot be set to updateble.');
            end
            else
            if (LDataset.DataSet.RecordCount = 0)   then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirElevation returned no records.');
            end
            else
            if (LDataset.DataSet.RecordCount > 1)   then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirElevation returned multiple records when a single record is expected.');
            end
            else
            begin
              LDataset.DataSet.Edit;
              LIndex := 0;
              for LCount := AReservoir.ReservoirElevationsData.StartElevation  downto 1 do
              begin
                LIndex := LIndex + 1;
                LFieldName := Format('%s%2.2d',['ReservoirElev',LIndex]);
                if(AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[LCount] = NullFloat) then
                  LDataset.DataSet.FieldByName(LFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat :=
                  AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[LCount];
              end;

              for LIndex := AReservoir.ReservoirElevationsData.StartElevation + 1 to 15 do
              begin
                LFieldName := Format('%s%2.2d',['ReservoirElev',LIndex]);
                if(AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex] = NullFloat) then
                  LDataset.DataSet.FieldByName(LFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat :=
                    AReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
              end;
              LDataset.DataSet.Post;
            end;

            // ReservoirVolume
            LSQL := ReservoirVolumeSQL + GetNoAliasScenarioWhereClause + ' AND Identifier = '+ IntToStr(LIdentifier);
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataset.SetReadOnly(False);
            LDataset.DataSet.Open;
            if LDataset.IsReadOnly  then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirVolume cannot be set to updateble.');
            end
            else
            if (LDataset.DataSet.RecordCount = 0)   then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirVolume returned no records.');
            end
            else
            if (LDataset.DataSet.RecordCount > 1)   then
            begin
              LDataset.DataSet.Close;
              raise Exception.Create('Query to table ReservoirVolume returned multiple records when a single record is expected.');
            end
            else
            begin
              LDataset.DataSet.Edit;
              LIndex := 0;
              for LCount := AReservoir.ReservoirVolumesData.StartVolume  downto 1 do
              begin
                LIndex := LIndex + 1;
                LFieldName := Format('%s%2.2d',['Volume',LIndex]);
                if(AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[LCount] = NullFloat) then
                  LDataset.DataSet.FieldByName(LFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat :=
                  AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[LCount];
              end;

              for LIndex := AReservoir.ReservoirVolumesData.StartVolume + 1 to 15 do
              begin
                LFieldName := Format('%s%2.2d',['Volume',LIndex]);
                if(AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex] = NullFloat) then
                  LDataset.DataSet.FieldByName(LFieldName).Clear
                else
                  LDataset.DataSet.FieldByName(LFieldName).AsFloat :=
                    AReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex];
              end;
              LDataset.DataSet.Post;
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

procedure TReservoirDataSQLAgent.DeleteFromNVTables (AElementID : integer);
const OPNAME = 'TReservoirDataSQLAgent.DeleteFromNVTables';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(DeleteFromNVDrawingInstSQL(AElementID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteFromNVDrawingInstCaptionSQL(AElementID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteFromNVDrawingInstChannelSQL(AElementID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteFromNVDrawingPosSQL(AElementID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteFromNVDrawingPropSQL(AElementID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;
procedure TReservoirDataSQLAgent.LoadHistoricDamLevel(AContextData: TStringList; ARecordIdentifier,
                                                      AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadHistoricDamLevel';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirDataSQLAgent.GetHistoricDamLevelsDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetHistoricDamLevelsDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT'+
      ' Model,StudyAreaName,SubArea,Scenario,FileName,YearValue'+
      ' ,MonthValue01,MonthValue02,MonthValue03,MonthValue04,MonthValue05,MonthValue06'+
      ' ,MonthValue07,MonthValue08,MonthValue09,MonthValue10,MonthValue11,MonthValue12'+
      ' FROM  HistoricDamLevels '+
      ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
      ' AND FileName      = :FileName'+
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileName,YearValue';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirAreaGroupSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirAreaGroupSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT GroupID, ' +
      'GroupName ' +
      'FROM ReservoirGroup A WHERE ' +
      GetScenarioWhereClause +
      ' ORDER BY A.GroupID ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirDataSQLAgent.GetDownStreamPowerChannelSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetDownStreamPowerChannelSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier, Channel01, Channel02, Channel03, Channel04, ' +
              'Channel05, Channel06, Channel07, Channel08, Channel09, Channel10, Channel11, ' +
              'Channel12, Channel13, Channel14, Channel15, Channel16, Channel17, Channel18, ' +
              'Channel19, Channel20 ' +
              'FROM ReservoirChannels A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetWetlandsNodesNamesDataSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetWetlandsNodesNamesDataSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT           ' +
      '   A.Model,A.StudyAreaName, A.Scenario,  ' +
      '   A.Identifier AS RecordIdentifier,   ' +
      '   A.NodeCount AS ReservoirIdentifier, ' +
      '   0 AS PenaltyStruct, ' +
      '   A.ReservoirName,  ' +
      QuotedStr('N') + ' AS IncludeSummary,  ' +
      '   0 AS PointsCount,  ' +
      '   0.0 AS DrainageScale,  ' +
      '   0.0 AS AfforestationScale,  ' +
      '   0.0 AS IrrigationScale,  ' +
      '   0.0 AS UrbanRunoff,  ' +
      '   A.NaturalInflowChannel,  ' +
      '   0 AS CatchmentRef,  ' +
      '   A.NodeType,  ' +
      '   0 AS NodeNumberStorage,  ' +
      '   0 AS StatusIndicator,  ' +
      '   0.0 AS BottomOfReservoir,  ' +
      '   0.0 AS DeadStorageLevel,  ' +
      '   0.0 AS FullSupplyLevel,  ' +
      '   0.0 AS ReservoirPriority,  ' +
      '   0.0 AS RainCoef, ' +
      '   XCoord,'+
      '   YCoord,'+
      '   0 AS GroupId,'+
      '   A.DamLevelsFileName' +
      ' FROM ReservoirDetails A WHERE ' +
        GetScenarioWhereClause + ' AND ' +
      '   (A.NodeType = 5) ' +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetMaxReservoirIdentifier: integer;
const OPNAME = 'TReservoirDataSQLAgent.GetMaxReservoirIdentifier';
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
        LSQL := 'SELECT MAX(Identifier) AS RecordID FROM ReservoirDetails WHERE ' + GetNoAliasScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          Result := LDataSet.DataSet.FieldByName('RecordID').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetMaxReservoirNumber: integer;
const OPNAME = 'TReservoirDataSQLAgent.GetMaxReservoirNumber';
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
        LSQL := 'SELECT MAX(NodeCount) AS ReservoirNumber FROM ReservoirDetails WHERE ' + GetNoAliasScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          Result := LDataSet.DataSet.FieldByName('ReservoirNumber').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetMaxReservoirInitialLevelsIdentifier: integer;
const OPNAME = 'TReservoirDataSQLAgent.GetMaxReservoirInitialLevelsIdentifier';
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
        LSQL := 'SELECT MAX(Identifier) AS RecordID FROM ReservoirInitialLevels WHERE ' + GetNoAliasScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          Result := LDataSet.DataSet.FieldByName('RecordID').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetReservoirAndNodesWithInflowCount: integer;
const OPNAME = 'TReservoirDataSQLAgent.GetReservoirAndNodesWithInflowCount';
var
  LDataSet: TAbstractModelDataset;
  LTypesList,
  LSQL: string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LTypesList := IntToStr(ntReservoir)+','+ IntToStr(ntWetlandNode)+','+ IntToStr(ntNodeWithInflow)+','+
                      IntToStr(ntMinePolutionControlDam)+','+ IntToStr(ntMineUndergroundDam) +','+ IntToStr(ntGroundWater) ;
        LSQL := 'SELECT COUNT(*) AS ReservoirCount FROM ReservoirDetails WHERE ' + GetNoAliasScenarioWhereClause +
                ' AND NodeType in ('+ LTypesList + ')';
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
          Result := LDataSet.DataSet.FieldByName('ReservoirCount').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
         AReservoirNumberList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;

const OPNAME = 'TReservoirDataSQLAgent.CopyReservoirsFromScenario';
      ReservoirDetailsSQL       = 'SELECT * FROM ReservoirDetails WHERE ';
      ReservoirAreaSQL          = 'SELECT * FROM ReservoirArea WHERE ';
      ReservoirElevationSQL     = 'SELECT * FROM ReservoirElevation WHERE ';
      ReservoirEvaporationsSQL  = 'SELECT * FROM ReservoirEvap WHERE ';
      ReservoirVolumeSQL        = 'SELECT * FROM ReservoirVolume WHERE ';
      ReservoirChannelsSQL      = 'SELECT * FROM ReservoirChannels WHERE ';
      ReservoirLevelSQL         = 'SELECT * FROM ReservoirLevels WHERE ';
      ReservoirInitialLevelsSQL = 'SELECT * FROM ReservoirInitialLevels WHERE ';
      NodesDetailsSQL           = 'SELECT * FROM NodesDetails WHERE ';
      ReservoirCountsSQL        = 'SELECT * FROM Reservoir WHERE ';
      StorageZonesSQL           = 'SELECT * FROM StorageZones WHERE ';
      RunTitleSQL               = 'SELECT * FROM RunTitle WHERE ';
var
  LStop                    : boolean;
  LRecordID                : integer;
  LIndex                   : integer;
  LReservoirIndex          : integer;
  LCurrentReservoirNumber  : integer;
  LCurrentRecordIdentifier : integer;
  LNewRecordIdentifier     : integer;
  LNewReservoirNumber      : integer;
  LResevoirCount           : integer;
  LReservoirZoneLevelsCount: integer;
  LNewReservoirInitialLevelsIdentifier     : integer;

  LSourceDataSet      : TAbstractModelDataset;
  LDestinationDataSet : TAbstractModelDataset;

  LImportDate     : TDateTime;

  LModel                     : string;
  LFieldName                 : string;
  LDestinationStudyAreaName  : string;
  LDestinationSubArea        : string;
  LDestinationScenario       : string;
  LSourceSQL                 : string;
  LDestinationSQL            : string;
  LSourceWhereClause         : string;
  LDestinationWhereClause    : string;
  LMessage                   : string;
  LReservoirName             : string;
  LInTransaction             : boolean;
 begin
  Result := False;
  try
    if not Assigned(AReservoirNumberList) then Exit;

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
        LInTransaction := FAppModules.Database.InTransaction;
        if not LInTransaction then
          FAppModules.Database.StartTransaction;
        try
          //LCurrentRecordIdentifier := 0;
          LNewRecordIdentifier     := GetMaxReservoirIdentifier;
          LNewReservoirNumber      := GetMaxReservoirNumber;
          for LReservoirIndex := 0 to AReservoirNumberList.Count-1 do
          begin
            LReservoirName           := AReservoirNumberList[LReservoirIndex];
            LCurrentReservoirNumber  := Integer(AReservoirNumberList.Objects[LReservoirIndex]);
            LNewRecordIdentifier     := LNewRecordIdentifier + 1;
            LNewReservoirNumber      := LNewReservoirNumber + 1;

            LMessage := 'Copying Reservoir ('+LReservoirName+') ' + IntToStr(LReservoirIndex+1) + ' of '+ IntToStr(AReservoirNumberList.Count);
            AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
            if LStop then
            begin
              FAppModules.Database.Rollback;
              Exit;
            end;

            //________________________________________________________ ReservoirDetails ____________________________
            LSourceSQL := ReservoirDetailsSQL + LSourceWhereClause + ' AND NodeCount = '+ IntToStr(LCurrentReservoirNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;


            LDestinationSQL := ReservoirDetailsSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table ReservoirDetails cannot be set to updateble.');
            end
            else
            begin
              LCurrentRecordIdentifier := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
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
              LDestinationDataSet.DataSet.FieldByName('NodeCount').AsInteger     := LNewReservoirNumber;
              LDestinationDataSet.DataSet.FieldByName('PenaltyStruct').AsInteger := 0;
              LDestinationDataSet.DataSet.FieldByName('CatchmentRef').AsInteger  := 0;

              LDestinationDataSet.DataSet.Post;
            end;

            //________________________________________________________ NodesDetails _______________________________
            LSourceSQL := NodesDetailsSQL + LSourceWhereClause + ' AND NodeNumberStorage = '+ IntToStr(LCurrentReservoirNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := NodesDetailsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirArea cannot be set to updateble.');
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

                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewRecordIdentifier;
                LDestinationDataSet.DataSet.FieldByName('NodeNumberStorage').AsInteger   := LNewReservoirNumber;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //________________________________________________________ ReservoirArea _______________________________
            LSourceSQL := ReservoirAreaSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentRecordIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirAreaSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirArea cannot be set to updateble.');
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

                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewRecordIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //________________________________________________________ ReservoirElevation __________________________
            LSourceSQL := ReservoirElevationSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentRecordIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirElevationSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirElevation cannot be set to updateble.');
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

                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewRecordIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;


            //_____________________________________________________ ReservoirEvaporations __________________________
            LSourceSQL := ReservoirEvaporationsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentRecordIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirEvaporationsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirEvap cannot be set to updateble.');
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

                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewRecordIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //________________________________________________________ ReservoirVolume _____________________________
            LSourceSQL := ReservoirVolumeSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentRecordIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirVolumeSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirEvap cannot be set to updateble.');
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

                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewRecordIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //______________________________________________________ ReservoirChannels _____________________________
            LSourceSQL := ReservoirChannelsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentRecordIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirChannelsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirEvap cannot be set to updateble.');
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

                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewRecordIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //______________________________________________________ ReservoirLevel ________________________________
            LSourceSQL := ReservoirLevelSQL + LSourceWhereClause + ' AND ReservoirIdentifier = '+ IntToStr(LCurrentReservoirNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirLevelSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirEvap cannot be set to updateble.');
              end
              else
              begin
                LRecordID := Get_HighestReservoirLevelRecordIdentifier;
                while  not LSourceDataSet.DataSet.Eof do
                begin
                  LDestinationDataSet.DataSet.Append;

                  for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
                  begin
                     LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                     LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                       LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;

                  LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('ReservoirIdentifier').AsInteger   := LNewReservoirNumber;
                  LRecordID := LRecordID + 1;
                  LDestinationDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger := LRecordID;
                  LDestinationDataSet.DataSet.Post;

                  LSourceDataSet.DataSet.Next;
                end;
              end;
            end;
            //______________________________________________ ReservoirInitialLevels ________________________________
            LNewReservoirInitialLevelsIdentifier := GetMaxReservoirInitialLevelsIdentifier;
            LSourceSQL := ReservoirInitialLevelsSQL + LSourceWhereClause + ' AND ReservoirNodeNumber = '+ IntToStr(LCurrentReservoirNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ReservoirInitialLevelsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ReservoirEvap cannot be set to updateble.');
              end
              else
              begin
                while  not LSourceDataSet.DataSet.Eof do
                begin
                  LNewReservoirInitialLevelsIdentifier := LNewReservoirInitialLevelsIdentifier + 1;
                  LDestinationDataSet.DataSet.Append;

                  for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
                  begin
                     LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                     LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                       LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;

                  LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewReservoirInitialLevelsIdentifier;
                  LDestinationDataSet.DataSet.FieldByName('ReservoirNodeNumber').AsInteger   := LNewReservoirNumber;
                  LDestinationDataSet.DataSet.Post;

                  LSourceDataSet.DataSet.Next;
                end;
              end;
            end;
          end;
          //______________________________________________ ReservoirCounts ________________________________
          LResevoirCount := GetReservoirAndNodesWithInflowCount;
          if(LResevoirCount > 0) then
          begin
            LDestinationSQL := ReservoirCountsSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDestinationDataSet.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table Reservoir returned more than one record to update reservoir count.');

              if (LDestinationDataSet.DataSet.RecordCount = 0) then
              begin
                LDestinationDataSet.DataSet.Append;
                LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('ReservoirCount').AsInteger := LResevoirCount;
                LDestinationDataSet.DataSet.FieldByName('HydroUnitsCode').AsString := 'MCM';
              end
              else
              begin
                LDestinationDataSet.DataSet.Edit;
                LDestinationDataSet.DataSet.FieldByName('ReservoirCount').AsInteger := LResevoirCount;
              end;
              LDestinationDataSet.DataSet.Post;
            end;

            if (LResevoirCount =  AReservoirNumberList.Count)then
            begin
              LDestinationSQL := RunTitleSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if(LDestinationDataSet.DataSet.RecordCount = 0) then
              begin
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table RunTitle cannot be set to updateble.');
                end
                else
                begin
                  LDestinationDataSet.DataSet.Append;
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString         := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString       := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString      := LDestinationScenario;

                  LDestinationDataSet.DataSet.FieldByName('Title1').AsString := 'Title1';
                  LDestinationDataSet.DataSet.FieldByName('Title2').AsString := 'Title2';
                  LDestinationDataSet.DataSet.FieldByName('Title3').AsString := 'Title3';
                  LDestinationDataSet.DataSet.Post;
                end;
              end;
            end;

            LDestinationSQL := StorageZonesSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table Reservoir cannot be set to updateble.');
            end
            else
            begin
              if (LDestinationDataSet.DataSet.RecordCount > 1) then
                raise Exception.Create('Query to table StorageZones returned more than one record to update reservoir count.');

              if (LDestinationDataSet.DataSet.RecordCount = 1) then
              begin
                LReservoirZoneLevelsCount := LDestinationDataSet.DataSet.FieldByName('StorageZoneCount').AsInteger -3;
                if(LReservoirZoneLevelsCount < 0) then
                  LReservoirZoneLevelsCount := 0;
                LDestinationDataSet.DataSet.Edit;
                LDestinationDataSet.DataSet.FieldByName('NodesCount').AsInteger := LResevoirCount;
                LDestinationDataSet.DataSet.FieldByName('ReservoirLevelsCount').AsInteger := LReservoirZoneLevelsCount*LResevoirCount;
                LDestinationDataSet.DataSet.Post;
              end;
            end;
          end;
          FAppModules.StudyArea.LastUpdateDate := Now();
          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          if not LInTransaction then
            FAppModules.Database.Commit;
          Result := True;
        except
          if not LInTransaction then
            FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LSourceDataSet.Free;
      LDestinationDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.InsertReservoirAreaGroup(var AGroupID: integer): boolean;
const OPNAME = 'TReservoirDataSQLAgent.InsertReservoirAreaGroup';
var
  LDataSet : TAbstractModelDataset;
  LGroupID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LGroupID := GetMaxReservoirAreaGroupId + 1;

        LDataSet.SetSQL(InsertReservoirAreaGroupSQL(LGroupID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();
        AGroupID := LGroupID;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataSQLAgent.GetMaxReservoirAreaGroupIdSQL: string;
const OPNAME = 'TReservoirDataSQLAgent.GetMaxReservoirAreaGroupIdSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(GroupID) AS MaxIdentifier FROM ReservoirGroup A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataSQLAgent.GetMaxReservoirAreaGroupId: integer;
const OPNAME = 'TReservoirDataSQLAgent.GetMaxReservoirAreaGroupId';
var
  LDataSet  : TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxReservoirAreaGroupIdSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirDataSQLAgent.LoadContextData_ReservoirGroupAreaID(AContextData: TStringList;
                                                                      AGroupID: string);
const OPNAME = 'TReservoirDataSQLAgent.LoadContextData_ReservoirGroupAreaID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('GroupID='            + AGroupID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirDataSQLAgent.Get_HighestReservoirLevelRecordIdentifier: integer;
const OPNAME = 'TReservoirDataSQLAgent.GetNoAliasScenarioWhereClause';
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


