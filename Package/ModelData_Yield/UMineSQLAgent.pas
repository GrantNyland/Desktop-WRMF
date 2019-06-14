unit UMineSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UMiningData,
  UAbstractObject;
type
  TMineSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData_MineData (AContextData : TStringList;AIdentifier : integer);
    procedure LoadContextData_OpenCast(AContextData : TStringList; AMineIdentifier,AIdentifier : integer);
    procedure LoadContextData_Underground(AContextData : TStringList; AMineIdentifier,AIdentifier : integer);
    procedure LoadContextData_SlurryDump(AContextData : TStringList; AMineIdentifier,AIdentifier : integer);
    procedure LoadContextData_MineEvaporation(AContextData : TStringList;AMineIdentifier : integer;
                                              AFieldNameIdentifier: string);
    procedure LoadContextData_RechargeFactor(AContextData : TStringList;AMineIdentifier,ARFParentType,AParentID,
                                             ARFType : integer; AFieldNameIdentifier: string);
    procedure LoadContextData_UGUpstreamRunoff(AContextData : TStringList;AMineIdentifier,AUGIdentifier : integer;
                                               AFieldNameIdentifier : string);
    procedure LoadContextData_MineSubCatchmentVolumeFactors(AContextData : TStringList;
                                                            AMineSubCatchmentIdentifier : integer;
                                                            AFieldNameIdentifier: string);

    function GetMineSQL: string;
    function GetOpenCastSQL(AMineIdentifier: integer)  : string;
    function GetUndergroundSQL(AMineIdentifier: integer)   : string;
    function GetSlurryDumpSQL(AMineIdentifier: integer)    : string;
    function GetRechargeFactorSQL(AMineID,ARFParentType,AParentID,ARFType : integer): string;

    function GetUGUpstreamRunoffSQL(AMineIdentifier,AUGIdentifier: integer) : string;
    function GetPanEvaporationSQL(AMineIdentifier: integer) : string;
    function GetLakeEvaporationSQL(AMineIdentifier: integer) : string;

    function InsertMineSQL(AIdentifier: integer): string;
    function InsertOpenCastSQL (AMineID: integer): string;
    function InsertUndergroundSQL (AMineID: integer) : string;
    function InsertSlurryDumpSQL (AMineID: integer) : string;
    function InsertRechargeFactorSQL(AMineID,ARFParentType,AParentID,ARFType : integer) : string;
    function InsertUGUpstreamRunoffSQL(AMineID,AUGIdentifier : integer) : string;
    function InsertPanEvaporationSQL(AMineIdentifier : integer) : string;
    function InsertLakeEvaporationSQL(AMineIdentifier : integer) : string;
    function InsertMineSubCatchmentFlowVolumeSQL(AMineSubCatchmentID: integer) : string;

    function GetDeleteMineSQL(AIdentifier: integer): string;
    function GetDeleteOpenCastSQL (AMineIdentifier,AIdentifier : integer) : string;
    function GetDeleteUndergroundSQL(AMineIdentifier,AIdentifier : integer) : string;
    function GetDeleteSlurryDumpSQL(AMineIdentifier,AIdentifier : integer): string;
    function GetDeleteRechargeFactorSQL(AMineID,ARFParentType,AParentID,ARFType : integer) : string;
    function GetDeleteUGUpstreamRunoffSQL(AMineID,AUGIdentifier : integer) : string;
    function GetDeleteLakeEvaporationSQL(AMineIdentifier : integer) : string;
    function GetDeletePanEvaporationSQL(AMineIdentifier : integer) : string;
    function GetDeleteMinimunGroundwaterFlowVolumeSQL(AMineIdentifier : integer) : string;

    function AddMine(AMine: TMine) : boolean;
    function AddOpenCast(AOpencast : TOpenCast): boolean;
    function AddUnderground(AUnderground: TUnderground): boolean;
    function AddSlurryDump(ASlurryDump : TSlurryDump): boolean;

    function DeleteMine(AMine: TMine): boolean;
    function DeleteOpenCast(AOpencast : TOpenCast): boolean;
    function DeleteUnderground(AUnderground: TUnderground): boolean;
    function DeleteSlurryDump(ASlurryDump : TSlurryDump): boolean;

    function GetMaxMineID : integer;
    function GetMaxOpenCastID : integer;
    function GetMaxUndergroundID : integer;
    function GetMaxSlurryDumpID  : integer;

    function AddMineSubCatchment(var AIdentifier: integer;ACatchmentRefNr: integer;ACatchmentRefName: string) : boolean;
    function InsertMineSubCatchmentSQL(AIdentifier,ACatchmentRefNr: integer;ACatchmentRefName: string): string;
    function GetMineSubCatchmentSQL: string;
    function GetMaxMineSubCatchmentID : integer;
    function GetMineSubCatchmentFlowVolumeSQL(AMineSubCatchmentIdentifier: integer): string;

    function GetMineCountSQL : string;
    function GetOpenCastCountSQL(AMineIdentifier : integer) : string;
    function GetUndergroundCountSQL(AMineIdentifier : integer) : string;
    function GetSlurryDumpCountSQL(AMineIdentifier : integer) : string;
    function CopyMineFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AMineList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  end;

implementation
uses
  Math,
  SysUtils,
  Data.DB,
  UConstants,
  UDataSetType,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UErrorHandlingOperations;

{ TMineSQLAgent }

function TMineSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TMineSQLAgent.GetScenarioWhereClause';
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

procedure TMineSQLAgent.LoadContextData_MineData(AContextData: TStringList;AIdentifier: integer);
const OPNAME = 'TMineSQLAgent.LoadContextData_MineData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + IntToStr(AIdentifier));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TMineSQLAgent.GetMineSQL: string;
const OPNAME = 'TMineSQLAgent.GetMineSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' NodeNumber, MineName, RiverChannelNumber, PCDChannelNumber, ' +
              ' HydrologyNodeNumber, BeneficiationPlantArea, BeneficiationRunOffFactor'+
              ' FROM Mine ' +
              ' WHERE '+
              GetScenarioWhereClause +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertMineSQL(AIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.InsertMineSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO Mine '+
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, NodeNumber, ' +
              ' MineName, RiverChannelNumber, PCDChannelNumber, HydrologyNodeNumber, ' +
              ' BeneficiationPlantArea, BeneficiationRunOffFactor'+
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AIdentifier) + ',' +
              ' :NodeNumber, :MineName, :RiverChannelNumber, :PCDChannelNumber, '+
              ' :HydrologyNodeNumber, :BeneficiationPlantArea, :BeneficiationRunOffFactor) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetOpenCastSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetOpenCastSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, ' +
              ' PitName, CoalReserveArea, WorkingsArea, DisturbedWorkingsArea, ' +
              ' DisturbedArea, WaterSurfaceEvapArea, DisturbedAreaRunOff, DisturbedWorkingsAreaRunOff, ' +
              ' DecantVolume, SeepageVolume, AnalysisStartVolume, MaximumSeepageRate, ' +
              ' SeepageExponent, PCDSurfaceArea, PCDStorageCapacity, PCDAnalysisStartVolume ' +
              ' FROM  MineOpenCast ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier) +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetUndergroundSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetUndergroundSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, '+
              ' UnderGroundSectionName, ChannelNumberToUGDam, UpstreamCatchmentArea, '+
              ' BoardPillarCatchmentArea, HighExtractionCatchmentArea, HighExtractionAreaRunoffFactor ' +
              ' FROM  MineUnderGround ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier) +
              ' ORDER BY Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetSlurryDumpSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetSlurryDumpSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, '+
              ' DumpName, DumpSurfaceArea, RunoffFactorToPCD, '+
              ' SeepageSplitFactor, PCDStorageCapacity, PCDSurfaceArea, PCDAnalysisStartVolume ' +
              ' FROM  MineSlurryDump ' +
              ' WHERE '+
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier) +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertOpenCastSQL(AMineID: integer): string;
const OPNAME = 'TMineSQLAgent.InsertOpenCastSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineOpenCast ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, PitName, ' +
              ' CoalReserveArea, WorkingsArea, DisturbedWorkingsArea, DisturbedArea, WaterSurfaceEvapArea, ' +
              ' DisturbedAreaRunOff, DisturbedWorkingsAreaRunOff, DecantVolume, ' +
              ' SeepageVolume, AnalysisStartVolume, MaximumSeepageRate, SeepageExponent, ' +
              ' PCDSurfaceArea, PCDStorageCapacity, PCDAnalysisStartVolume ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineID) + ','+
              ' :Identifier, :PitName, :CoalReserveArea, :WorkingsArea, :DisturbedWorkingsArea, '+
              ' :DisturbedArea, :WaterSurfaceEvapArea, :DisturbedAreaRunOff, :DisturbedWorkingsAreaRunOff, '+
              ' :DecantVolume, :SeepageVolume, :AnalysisStartVolume, :MaximumSeepageRate, '+
              ' :SeepageExponent, :PCDSurfaceArea, :PCDStorageCapacity, :PCDAnalysisStartVolume ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertUndergroundSQL(AMineID: integer): string;
const OPNAME = 'TMineSQLAgent.InsertUndergroundSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineUnderGround ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, UnderGroundSectionName, ' +
              ' ChannelNumberToUGDam, UpstreamCatchmentArea, BoardPillarCatchmentArea, ' +
              ' HighExtractionCatchmentArea, HighExtractionAreaRunoffFactor ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineID) + ',' +
              ' :Identifier, :UnderGroundSectionName, :ChannelNumberToUGDam, :UpstreamCatchmentArea, '+
              ' :BoardPillarCatchmentArea, :HighExtractionCatchmentArea, :HighExtractionAreaRunoffFactor) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertSlurryDumpSQL(AMineID: integer): string;
const OPNAME = 'TMineSQLAgent.InsertSlurryDumpSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineSlurryDump ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, DumpName, ' +
              ' DumpSurfaceArea, RunoffFactorToPCD, SeepageSplitFactor, ' +
              ' PCDStorageCapacity, PCDSurfaceArea, PCDAnalysisStartVolume ' +
              ' ) VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineID) + ',' +
              ' :Identifier, :DumpName, :DumpSurfaceArea, :RunoffFactorToPCD, '+
              ' :SeepageSplitFactor, :PCDStorageCapacity, :PCDSurfaceArea, :PCDAnalysisStartVolume) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteMineSQL(AIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteMineSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM Mine WHERE ' +
              GetScenarioWhereClause +
              ' AND Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteOpenCastSQL(AMineIdentifier,AIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteOpenCastSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MineOpenCast WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineIdentifier)+
              ' AND Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteUndergroundSQL(AMineIdentifier,AIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteUndergroundSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MineUnderGround WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineIdentifier)+
              ' AND Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteSlurryDumpSQL(AMineIdentifier,AIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteSlurryDumpSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MineSlurryDump WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineIdentifier)+
              ' AND Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetUGUpstreamRunoffSQL(AMineIdentifier,AUGIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetUGUpstreamRunoffSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, UGIdentifier, ' +
              ' RunoffFactor01, RunoffFactor02, RunoffFactor03, RunoffFactor04, RunoffFactor05, ' +
              ' RunoffFactor06, RunoffFactor07, RunoffFactor08, RunoffFactor09, RunoffFactor10, ' +
              ' RunoffFactor11, RunoffFactor12 ' +
              ' FROM  MineUGUpstreamRunoff ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier) +
              ' AND UGIdentifier   = '+ IntToStr(AUGIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetRechargeFactorSQL(AMineID,ARFParentType,AParentID,ARFType : integer) : string;
const OPNAME = 'TMineSQLAgent.GetRechargeFactorSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, RechargeFactorParentType, ' +
              ' ParentIdentifier, RechargeFactorType, RechargeFactor01, RechargeFactor02, RechargeFactor03, ' +
              ' RechargeFactor04, RechargeFactor05, RechargeFactor06, RechargeFactor07, RechargeFactor08, ' +
              ' RechargeFactor09, RechargeFactor10, RechargeFactor11, RechargeFactor12 ' +
              ' FROM  MineRechargeFactor ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineID) +
              ' AND RechargeFactorParentType = ' + IntToStr(ARFParentType) +
              ' AND ParentIdentifier = ' + IntToStr(AParentID) +
              ' AND RechargeFactorType = ' + IntToStr(ARFType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertRechargeFactorSQL(AMineID, ARFParentType, AParentID, ARFType: integer): string;
const OPNAME = 'TMineSQLAgent.InsertRechargeFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineRechargeFactor ' +
                ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, ' +
                ' RechargeFactorParentType, ParentIdentifier, RechargeFactorType, ' +
                ' RechargeFactor01, RechargeFactor02, RechargeFactor03, RechargeFactor04, ' +
                ' RechargeFactor05, RechargeFactor06, RechargeFactor07, RechargeFactor08, ' +
                ' RechargeFactor09, RechargeFactor10, RechargeFactor11, RechargeFactor12  ' +
                ') VALUES (' +
                QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
                QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
                QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
                QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
                IntToStr(AMineID) + ','+
                IntToStr(ARFParentType) + ','+
                IntToStr(AParentID) + ','+
                IntToStr(ARFType) + ','+
                ' :RechargeFactor01, :RechargeFactor02, :RechargeFactor03, :RechargeFactor04, '+
                ' :RechargeFactor05, :RechargeFactor06, :RechargeFactor07, :RechargeFactor08, '+
                ' :RechargeFactor09, :RechargeFactor10, :RechargeFactor11, :RechargeFactor12 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertUGUpstreamRunoffSQL(AMineID,AUGIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.InsertUGUpstreamRunoffSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineUGUpstreamRunoff ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, UGIdentifier, ' +
              ' RunoffFactor01, RunoffFactor02, RunoffFactor03, RunoffFactor04, ' +
              ' RunoffFactor05, RunoffFactor06, RunoffFactor07, RunoffFactor08, ' +
              ' RunoffFactor09, RunoffFactor10, RunoffFactor11, RunoffFactor12  ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineID) + ','+
              IntToStr(AUGIdentifier) + ','+
              ' :RunoffFactor01, :RunoffFactor02, :RunoffFactor03, :RunoffFactor04, ' +
              ' :RunoffFactor05, :RunoffFactor06, :RunoffFactor07, :RunoffFactor08, ' +
              ' :RunoffFactor09, :RunoffFactor10, :RunoffFactor11, :RunoffFactor12 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteRechargeFactorSQL(AMineID, ARFParentType,AParentID, ARFType: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteRechargeFactorSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MineRechargeFactor WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineID) +
              ' AND RechargeFactorParentType = ' + IntToStr(ARFParentType) +
              ' AND ParentIdentifier = ' + IntToStr(AParentID) +
              ' AND RechargeFactorType = '+ IntToStr(ARFType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteUGUpstreamRunoffSQL(AMineID,AUGIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteUGUpstreamRunoffSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MineUGUpstreamRunoff WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineID)+
              ' AND UGIdentifier = ' + IntToStr(AUGIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertLakeEvaporationSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.InsertLakeEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineLakeEvaporation ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, ' +
              ' LakeEvaporation01, LakeEvaporation02, LakeEvaporation03, LakeEvaporation04, ' +
              ' LakeEvaporation05, LakeEvaporation06, LakeEvaporation07, LakeEvaporation08, ' +
              ' LakeEvaporation09, LakeEvaporation10, LakeEvaporation11, LakeEvaporation12  ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineIdentifier) + ','+
              ' :LakeEvaporation01, :LakeEvaporation02, :LakeEvaporation03, :LakeEvaporation04, ' +
              ' :LakeEvaporation05, :LakeEvaporation06, :LakeEvaporation07, :LakeEvaporation08, ' +
              ' :LakeEvaporation09, :LakeEvaporation10, :LakeEvaporation11, :LakeEvaporation12 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertPanEvaporationSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.InsertPanEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MinePanEvaporation ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, ' +
              ' PanEvaporation01, PanEvaporation02, PanEvaporation03, PanEvaporation04, ' +
              ' PanEvaporation05, PanEvaporation06, PanEvaporation07, PanEvaporation08, ' +
              ' PanEvaporation09, PanEvaporation10, PanEvaporation11, PanEvaporation12  ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineIdentifier) + ','+
              ' :PanEvaporation01, :PanEvaporation02, :PanEvaporation03, :PanEvaporation04, ' +
              ' :PanEvaporation05, :PanEvaporation06, :PanEvaporation07, :PanEvaporation08, ' +
              ' :PanEvaporation09, :PanEvaporation10, :PanEvaporation11, :PanEvaporation12 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteLakeEvaporationSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteLakeEvaporationSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MineLakeEvaporation WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeletePanEvaporationSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeletePanEvaporationSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MinePanEvaporation WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.AddMine(AMine: TMine): boolean;
const OPNAME = 'TMineSQLAgent.AddMine';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LIndex       : integer;
begin
  Result := False;
  try
    if (AMine = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin

        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertMineSQL((AMine.Identifier)));
          LDataSet.SetParams(['NodeNumber'],                  [IntToStr(AMine.NodeNumber)]);
          LDataSet.SetParams(['MineName'],                    [AMine.MineName]);
          LDataSet.SetParams(['RiverChannelNumber'],          [IntToStr(AMine.RiverChannelNumber)]);
          LDataSet.SetParams(['PCDChannelNumber'],            [IntToStr(AMine.PCDChannelNumber)]);
          LDataSet.SetParams(['HydrologyNodeNumber'],         [IntToStr(AMine.HydrologyNodeNumber)]);
          LDataSet.SetParams(['BeneficiationPlantArea'],      [FloatToStr(AMine.BeneficiationPlantArea)]);
          LDataSet.SetParams(['BeneficiationRunOffFactor'],   [FloatToStr(AMine.BeneficiationRunoffFactor)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertPanEvaporationSQL(AMine.Identifier));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('PanEvaporation%2.2d',[LIndex])],[FloatToStr(AMine.PanEvaporation[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertLakeEvaporationSQL(AMine.Identifier));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('LakeEvaporation%2.2d',[LIndex])],[FloatToStr(AMine.LakeEvaporation[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

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
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.AddOpenCast(AOpencast: TOpenCast): boolean;
const OPNAME = 'TMineSQLAgent.AddOpenCast';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LMine        : TMine;
  LIndex       : integer;
begin
  Result := False;
  try
    if (AOpenCast = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    LMine := TMine.Create(FAppModules);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertOpenCastSQL(AOpencast.MineIdentifier));
          LDataSet.SetParams(['Identifier'],                  [IntToStr(AOpencast.Identifier)]);
          LDataSet.SetParams(['PitName'],                     [AOpenCast.PitName]);
          LDataSet.SetParams(['CoalReserveArea'],             [FloatToStr(AOpenCast.CoalReserveArea)]);
          LDataSet.SetParams(['WorkingsArea'],                [FloatToStr(AOpenCast.WorkingsArea)]);
          LDataSet.SetParams(['DisturbedWorkingsArea'],       [FloatToStr(AOpenCast.DisturbedWorkingsArea)]);
          LDataSet.SetParams(['DisturbedArea'],               [FloatToStr(AOpenCast.DisturbedArea)]);
          LDataSet.SetParams(['WaterSurfaceEvapArea'],        [FloatToStr(AOpenCast.WaterSurfaceEvapArea)]);
          LDataSet.SetParams(['DisturbedAreaRunOff'],         [FloatToStr(AOpenCast.DisturbedAreaRunoff)]);
          LDataSet.SetParams(['DisturbedWorkingsAreaRunOff'], [FloatToStr(AOpenCast.DisturbedWorkingsAreaRunoff)]);
          LDataSet.SetParams(['DecantVolume'],                [FloatToStr(AOpenCast.DecantVolume)]);
          LDataSet.SetParams(['SeepageVolume'],               [FloatToStr(AOpenCast.SeepageVolume)]);
          LDataSet.SetParams(['AnalysisStartVolume'],         [FloatToStr(AOpenCast.AnalysisStartVolume)]);
          LDataSet.SetParams(['MaximumSeepageRate'],          [FloatToStr(AOpenCast.MaximumSeepageRate)]);
          LDataSet.SetParams(['SeepageExponent'],             [FloatToStr(AOpenCast.SeepageExponent)]);
          LDataSet.SetParams(['PCDSurfaceArea'],              [FloatToStr(AOpenCast.PCDSurfaceArea)]);
          LDataSet.SetParams(['PCDStorageCapacity'],          [FloatToStr(AOpenCast.PCDStorageCapacity)]);
          LDataSet.SetParams(['PCDAnalysisStartVolume'],      [FloatToStr(AOpenCast.PCDAnalysisStartVolume)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertRechargeFactorSQL(AOpencast.MineIdentifier,1,AOpencast.Identifier,1));
          for LIndex := 0 to 11 do
            LDataSet.SetParams([Format('RechargeFactor%2.2d',[LIndex+1])],[FloatToStr(AOpenCast.DisturbedRechargeFactor[LIndex])]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertRechargeFactorSQL(AOpencast.MineIdentifier,1,AOpencast.Identifier,2));
          for LIndex := 0 to 11 do
            LDataSet.SetParams([Format('RechargeFactor%2.2d',[LIndex+1])],[FloatToStr(AOpenCast.WorkingAreaRechargeFactor[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

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
      LMine.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.AddSlurryDump(ASlurryDump: TSlurryDump): boolean;
const OPNAME = 'TMineSQLAgent.AddSlurryDump';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQLAgent    : TMineSQLAgent;
  LIndex       : integer;
begin
  Result := False;
  try
    if (ASlurryDump = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(LSQLAgent.InsertSlurryDumpSQL(ASlurryDump.MineIdentifier));
          LDataSet.SetParams(['Identifier'],             [IntToStr(ASlurryDump.Identifier)]);
          LDataSet.SetParams(['DumpName'],               [ASlurryDump.DumpName]);
          LDataSet.SetParams(['DumpSurfaceArea'],        [FloatToStr(ASlurryDump.DumpSurfaceArea)]);
          LDataSet.SetParams(['RunoffFactorToPCD'],      [FloatToStr(ASlurryDump.RunoffFactorToPCD)]);
          LDataSet.SetParams(['SeepageSplitFactor'],     [FloatToStr(ASlurryDump.SeepageSplitFactor)]);
          LDataSet.SetParams(['PCDStorageCapacity'],     [FloatToStr(ASlurryDump.PCDStorageCapacity)]);
          LDataSet.SetParams(['PCDSurfaceArea'],         [FloatToStr(ASlurryDump.PCDSurfaceArea)]);
          LDataSet.SetParams(['PCDAnalysisStartVolume'], [FloatToStr(ASlurryDump.PCDAnalysisStartVolume)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(LSQLAgent.InsertRechargeFactorSQL(ASlurryDump.MineIdentifier,3,ASlurryDump.Identifier,5));
          for LIndex := 0 to 11 do
            LDataSet.SetParams([Format('RechargeFactor%2.2d',[LIndex+1])],[FloatToStr(ASlurryDump.RechargeFactor[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

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
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.AddUnderground(AUnderground: TUnderground): boolean;
const OPNAME = 'TMineSQLAgent.AddUnderground';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQLAgent    : TMineSQLAgent;
  LIndex       : integer;
begin
  Result := False;
  try
    if (AUnderground = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(LSQLAgent.InsertUndergroundSQL(AUnderground.MineIdentifier));

          LDataSet.SetParams(['Identifier'],                     [IntToStr(AUnderground.Identifier)]);
          LDataSet.SetParams(['UnderGroundSectionName'],         [AUnderground.UndergroundSectionName]);
          LDataSet.SetParams(['ChannelNumberToUGDam'],           [IntToStr(AUnderground.ChannelNumberToUGDam)]);
          LDataSet.SetParams(['UpstreamCatchmentArea'],          [FloatToStr(AUnderground.UpstreamCatchmentArea)]);
          LDataSet.SetParams(['BoardPillarCatchmentArea'],       [FloatToStr(AUnderground.BoardPillarCatchmentArea)]);
          LDataSet.SetParams(['HighExtractionCatchmentArea'],    [FloatToStr(AUnderground.HighExtractionCatchmentArea)]);
          LDataSet.SetParams(['HighExtractionAreaRunoffFactor'], [FloatToStr(AUnderground.HighExtractionAreaRunoffFactor)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(LSQLAgent.InsertUGUpstreamRunoffSQL(AUnderground.MineIdentifier,AUnderground.Identifier));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('RunoffFactor%2.2d',[LIndex])],[FloatToStr(AUnderground.UpstreamRunoffPortion[LIndex])]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(LSQLAgent.InsertRechargeFactorSQL(AUnderground.MineIdentifier,2,AUnderground.Identifier,3));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('RechargeFactor%2.2d',[LIndex])],[FloatToStr(AUnderground.BoardAndPilarRechargeFactor[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(LSQLAgent.InsertRechargeFactorSQL(AUnderground.MineIdentifier,2,AUnderground.Identifier,4));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('RechargeFactor%2.2d',[LIndex])],[FloatToStr(AUnderground.HighExtractionRechargeFactor[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

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
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.DeleteMine(AMine: TMine): boolean;
const OPNAME = 'TMineSQLAgent.DeleteMine';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQL         : string;
begin
  Result := False;
  try
    if(AMine = nil) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(GetDeleteMineSQL(AMine.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeletePanEvaporationSQL(AMine.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteLakeEvaporationSQL(AMine.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteMinimunGroundwaterFlowVolumeSQL(AMine.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LSQL := 'DELETE FROM MineOpenCast WHERE '+ GetScenarioWhereClause + ' AND MineIdentifier ='+ IntToStr(AMine.Identifier);
          LDataSet.SetSQL(LSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LSQL := 'DELETE FROM MineUnderGround WHERE '+ GetScenarioWhereClause + ' AND MineIdentifier ='+ IntToStr(AMine.Identifier);
          LDataSet.SetSQL(LSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LSQL := 'DELETE FROM MineSlurryDump WHERE '+ GetScenarioWhereClause + ' AND MineIdentifier ='+ IntToStr(AMine.Identifier);
          LDataSet.SetSQL(LSQL);
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
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.DeleteOpenCast(AOpencast : TOpenCast): boolean;
const OPNAME = 'TMineSQLAgent.DeleteOpenCast';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
begin
  Result := False;
  try
    if(AOpencast = nil) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try

          LDataSet.SetSQL(GetDeleteOpenCastSQL(AOpencast.MineIdentifier,AOpencast.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteRechargeFactorSQL(AOpencast.MineIdentifier,1,AOpencast.Identifier,1));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteRechargeFactorSQL(AOpencast.MineIdentifier,1,AOpencast.Identifier,2));
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
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.DeleteSlurryDump(ASlurryDump : TSlurryDump): boolean;
const OPNAME = 'TMineSQLAgent.DeleteSlurryDump';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
begin
  Result := False;
  try
    if(ASlurryDump = nil) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(GetDeleteSlurryDumpSQL(ASlurryDump.MineIdentifier,ASlurryDump.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteRechargeFactorSQL(ASlurryDump.MineIdentifier,3,ASlurryDump.Identifier,5));
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
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.DeleteUnderground(AUnderground: TUnderground): boolean;
const OPNAME = 'TMineSQLAgent.DeleteUnderground';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
begin
  Result := False;
  try
    if(AUnderground = nil) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(GetDeleteUndergroundSQL(AUnderground.MineIdentifier,AUnderground.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteUGUpstreamRunoffSQL(AUnderground.MineIdentifier,AUnderground.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteRechargeFactorSQL(AUnderground.MineIdentifier,2,AUnderground.Identifier,3));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteRechargeFactorSQL(AUnderground.MineIdentifier,2,AUnderground.Identifier,4));
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
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSQLAgent.GetMaxOpenCastID: integer;
const OPNAME = 'TMineSQLAgent.GetMaxOpenCastID';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM MineOpenCast WHERE ' + GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('MaxID').IsNull then
          Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetMaxMineID: integer;
const OPNAME = 'TMineSQLAgent.GetMaxMineID';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM Mine WHERE ' + GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('MaxID').IsNull then
          Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetMaxSlurryDumpID: integer;
const OPNAME = 'TMineSQLAgent.GetMaxSlurryDumpID';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM MineSlurryDump WHERE ' + GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('MaxID').IsNull then
          Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetMaxUndergroundID: integer;
const OPNAME = 'TMineSQLAgent.GetMaxUndergroundID';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM MineUnderGround WHERE ' +  GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('MaxID').IsNull then
          Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetLakeEvaporationSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetLakeEvaporationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, ' +
              ' LakeEvaporation01, LakeEvaporation02, LakeEvaporation03, LakeEvaporation04, '+
              ' LakeEvaporation05, LakeEvaporation06, LakeEvaporation07, LakeEvaporation08, '+
              ' LakeEvaporation09, LakeEvaporation10, LakeEvaporation11, LakeEvaporation12 ' +
              ' FROM  MineLakeEvaporation ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetPanEvaporationSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetPanEvaporationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, ' +
              ' PanEvaporation01, PanEvaporation02, PanEvaporation03, PanEvaporation04, ' +
              ' PanEvaporation05, PanEvaporation06, PanEvaporation07, PanEvaporation08, ' +
              ' PanEvaporation09, PanEvaporation10, PanEvaporation11, PanEvaporation12 ' +
              ' FROM  MinePanEvaporation ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSQLAgent.LoadContextData_OpenCast(AContextData: TStringList;AMineIdentifier, AIdentifier: integer);
const OPNAME = 'TMineSQLAgent.LoadContextData_OpenCast';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('Identifier=' + IntToStr(AIdentifier));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMineSQLAgent.LoadContextData_Underground(AContextData: TStringList; AMineIdentifier, AIdentifier: integer);
const OPNAME = 'TMineSQLAgent.LoadContextData_Underground';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('Identifier=' + IntToStr(AIdentifier));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMineSQLAgent.LoadContextData_SlurryDump(AContextData: TStringList; AMineIdentifier, AIdentifier: integer);
const OPNAME = 'TMineSQLAgent.LoadContextData_SlurryDump';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('Identifier=' + IntToStr(AIdentifier));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMineSQLAgent.LoadContextData_MineEvaporation(AContextData: TStringList; AMineIdentifier: integer;
                                                        AFieldNameIdentifier: string);
const OPNAME = 'TMineSQLAgent.LoadContextData_MineEvaporation';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('FieldNameIdentifier='    + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMineSQLAgent.LoadContextData_RechargeFactor(AContextData: TStringList;AMineIdentifier,ARFParentType,AParentID,
                                                       ARFType: integer;AFieldNameIdentifier: string);
const OPNAME = 'TMineSQLAgent.LoadContextData_RechargeFactor';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier=' + IntToStr(AMineIdentifier));
    AContextData.Add('RechargeFactorParentType=' + IntToStr(ARFParentType));
    AContextData.Add('ParentIdentifier=' + IntToStr(AParentID));
    AContextData.Add('RechargeFactorType=' + IntToStr(ARFType));
    AContextData.Add('FieldNameIdentifier=' +  AFieldNameIdentifier);

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMineSQLAgent.LoadContextData_UGUpstreamRunoff(AContextData: TStringList;AMineIdentifier,AUGIdentifier: integer;
                                                         AFieldNameIdentifier: string);
const OPNAME = 'TMineSQLAgent.LoadContextData_UGUpstreamRunoff';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier=' + IntToStr(AMineIdentifier));
    AContextData.Add('UGIdentifier=' + IntToStr(AUGIdentifier));
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TMineSQLAgent.GetMineCountSQL: string;
const OPNAME = 'TMineSQLAgent.GetMineCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS MineCount FROM Mine WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetOpenCastCountSQL (AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetOpenCastCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS OpenCastCount FROM MineOpenCast WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier =' + IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetSlurryDumpCountSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetSlurryDumpCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS SlurryDumpCount FROM MineSlurryDump WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier =' + IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetUndergroundCountSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetUndergroundCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS UndergroundCount FROM MineUnderGround WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier =' + IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.CopyMineFromScenario(ASourceStudyAreaName,ASourceSubArea, ASourceScenario: string; AMineList: TStrings;
                                            AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TMineSQLAgent.CopyMineFromScenario';
      MineSQL = 'SELECT * FROM Mine WHERE  ';
      MineLakeEvaporationSQL = 'SELECT * FROM MineLakeEvaporation WHERE ';
      MineOpenCastSQL = 'SELECT * FROM MineOpenCast WHERE ';
      MinePanEvaporationSQL = 'SELECT * FROM MinePanEvaporation WHERE ';
      MineRechargeFactorSQL = 'SELECT * FROM MineRechargeFactor WHERE ';
      MinimunGroundwaterFlowVolumeSQL = 'SELECT * FROM MinimunGroundwaterFlowVolume WHERE ';
      MineSlurryDumpSQL = 'SELECT * FROM MineSlurryDump WHERE ';
      MineUGUpstreamRunoffSQL = 'SELECT * FROM MineUGUpstreamRunoff WHERE ';
      MineUnderGroundSQL = 'SELECT * FROM MineUnderGround WHERE ';
      ChannelDetailsSQL = 'SELECT * FROM ChannelDetails WHERE ';
      MinMaxChannelSQL = 'SELECT * FROM MinMaxChannel WHERE ';
var
  LOldRiverChannelNumber       : integer;
  LOldPCDChannelNumber         : integer;
  LNewRiverChannelNumber       : integer;
  LNewPCDChannelNumber         : integer;
  LNewNodeNumber               : integer;
  LNodeNumber                  : integer;
  LNewMineIdentifier           : integer;
  LMineIdentifier              : integer;
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
  LMineName                    : string;
  LMineIndex                   : integer;
  LNewChannelID                : integer;
  LUndergroundID               : integer;
  LMineList,
  LPCDNodeList                 : TStringList;
  LOldDownStreamNodeNumber : integer;
  LNewDownStreamNodeNumber : integer;
  LOldDownStreamNodeName   : string;
  function CopyMineUnderGround:boolean;
  const OPNAME = 'CopyMineUnderGround';
  var
    LUGSourceDataSet         : TAbstractModelDataset;
    LUGDestinationDataSet    : TAbstractModelDataset;
    LOldChannelNumberToUGDam : integer;
    LNewChannelNumberToUGDam : integer;
    LUGNodeList              : TStringList;
    LIndex                   : integer;
  begin
    Result := False;
    try
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LUGSourceDataSet);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LUGDestinationDataSet);
      LUGNodeList := TStringList.Create;
      try
        //________________________________________________________ MineUnderGround ____________________________
        LSourceSQL := MineUnderGroundSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
        LUGSourceDataSet.DataSet.Close;
        LUGSourceDataSet.SetSQL(LSourceSQL);
        LUGSourceDataSet.DataSet.Open;
        LNewChannelNumberToUGDam := 0;
        while not LUGSourceDataSet.DataSet.Eof do
        begin
          LUndergroundID := GetMaxUndergroundID+1;
          LDestinationSQL := MineUnderGroundSQL + LDestinationWhereClause;
          LUGDestinationDataSet.DataSet.Close;
          LUGDestinationDataSet.SetSQL(LDestinationSQL);
          LUGDestinationDataSet.SetReadOnly(False);
          LUGDestinationDataSet.DataSet.Open;
          if LUGDestinationDataSet.IsReadOnly  then
          begin
            LUGDestinationDataSet.DataSet.Close;
            raise Exception.Create('Query to table Mine cannot be set to updatable.');
          end
          else
          begin
            LUGDestinationDataSet.DataSet.Append;
            LOldChannelNumberToUGDam := LUGSourceDataSet.DataSet.FieldByName('ChannelNumberToUGDam').AsInteger;
            for LIndex := 0 to  LUGDestinationDataSet.DataSet.FieldCount-1 do
            begin
              LFieldName := LUGDestinationDataSet.DataSet.Fields[LIndex].FieldName;
              LUGDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
              LUGSourceDataSet.DataSet.FieldByName(LFieldName).Value;
            end;

            if (LOldChannelNumberToUGDam > 0) then
            begin
              //________________________________________________________ ChannelDetails ____________________________
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldChannelNumberToUGDam);
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
                  LOldDownStreamNodeNumber := LChannelSourceDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
                  LOldDownStreamNodeName := LChannelSourceDataSet.DataSet.FieldByName('ChannelName').AsString;
                  LUGNodeList.Clear;
                  LUGNodeList.AddObject(LOldDownStreamNodeName,TObject(LOldDownStreamNodeNumber));
                  LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                                                                    LUGNodeList,AProgressUpdateFuntion);
                  LNewDownStreamNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;

                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                  LNewChannelNumberToUGDam := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewChannelNumberToUGDam;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := LNewDownStreamNodeNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := LNewNodeNumber;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;
              //________________________________________________________ MinMaxChannel ____________________________
              LSourceSQL := MinMaxChannelSQL + LSourceWhereClause + ' AND MinMaxChannelNumber = '+ IntToStr(LOldChannelNumberToUGDam);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MinMaxChannelSQL + LDestinationWhereClause;
                LChannelDestinationDataSet.DataSet.Close;
                LChannelDestinationDataSet.SetSQL(LDestinationSQL);
                LChannelDestinationDataSet.SetReadOnly(False);
                LChannelDestinationDataSet.DataSet.Open;
                if LChannelDestinationDataSet.IsReadOnly  then
                begin
                  LChannelDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinMaxChannel cannot be set to updatable.');
                end
                else
                begin
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LChannelDataSQLAgent.GetMaxMinMaxFlowConstraintID+1;
                  LChannelDestinationDataSet.DataSet.FieldByName('MinMaxChannelNumber').AsInteger      := LNewChannelNumberToUGDam;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;
              LUGDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
              LUGDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
              LUGDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
              LUGDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
              LUGDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LUndergroundID;
              LUGDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LMineIdentifier;
              LUGDestinationDataSet.DataSet.FieldByName('ChannelNumberToUGDam').AsInteger     := LNewChannelNumberToUGDam;
              LUGDestinationDataSet.DataSet.Post;
            end;
            //___________________________________________________ MineUGUpstreamRunoff ____________________________________
            LSourceSQL := MineUGUpstreamRunoffSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := MineUGUpstreamRunoffSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table MineUGUpstreamRunoff cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                LDestinationDataSet.DataSet.FieldByName('UGIdentifier').AsInteger             := LUndergroundID;
                LDestinationDataSet.DataSet.Post;
              end;
            end;
          end;
          LUGSourceDataSet.DataSet.Next;
        end;
      finally
        FreeAndNil(LUGSourceDataSet);
        FreeAndNil(LUGDestinationDataSet);
        FreeAndNil(LUGNodeList);
      end;
      Result := True;
    except on E: Exception do HandleError ( E, OPNAME ) end;
  end;
begin
  Result := False;
  try
    if not Assigned(AMineList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
    LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
    LPCDNodeList := TStringList.Create;
    LMineList    := TStringList.Create;
    try
      if Assigned(LSourceDataSet) and Assigned(LDestinationDataSet) and Assigned(LChannelSourceDataSet)
        and Assigned(LChannelDestinationDataSet) then
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
        LNewMineIdentifier := GetMaxMineID;
        FAppModules.Database.StartTransaction;
        LNewRiverChannelNumber := 0;
        LNewPCDChannelNumber := 0;
        LMineIdentifier := 0;
        for LMineIndex := 0 to AMineList.Count-1 do
        begin
          LMineList.Clear;
          LMineList.AddObject(AMineList[LMineIndex],AMineList.Objects[LMineIndex]);
          LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                         LMineList,AProgressUpdateFuntion);
          LNodeNumber := integer(AMineList.Objects[LMineIndex]);
          LMineName := AMineList[LMineIndex];
          LNewNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;
          LNewMineIdentifier := LNewMineIdentifier + 1;
          LMessage := 'Copying Mine ('+LMineName+') ' + IntToStr(LMineIndex+1) + ' of '+ IntToStr(AMineList.Count);
          AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
          if LStop then
          begin
            if FAppModules.Database.InTransaction then
              FAppModules.Database.Rollback;
            Exit;
          end;
          //________________________________________________________ Mine ____________________________
          LSourceSQL := MineSQL + LSourceWhereClause + ' AND NodeNumber = '+ IntToStr(LNodeNumber);
          LSourceDataSet.DataSet.Close;
          LSourceDataSet.SetSQL(LSourceSQL);
          LSourceDataSet.DataSet.Open;
          if LSourceDataSet.DataSet.Eof then Continue;

          LDestinationSQL := MineSQL + LDestinationWhereClause;
          LDestinationDataSet.DataSet.Close;
          LDestinationDataSet.SetSQL(LDestinationSQL);
          LDestinationDataSet.SetReadOnly(False);
          LDestinationDataSet.DataSet.Open;
          if LDestinationDataSet.IsReadOnly  then
          begin
            LDestinationDataSet.DataSet.Close;
            raise Exception.Create('Query to table Mine cannot be set to updatable.');
          end
          else
          begin
            LDestinationDataSet.DataSet.Append;
            LOldRiverChannelNumber := LSourceDataSet.DataSet.FieldByName('RiverChannelNumber').AsInteger;
            LOldPCDChannelNumber := LSourceDataSet.DataSet.FieldByName('PCDChannelNumber').AsInteger;
            LMineIdentifier := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
            for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
            begin
              LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
              LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
              LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
            end;

            if (LOldRiverChannelNumber > 0) and (LOldPCDChannelNumber > 0) then
            begin
              //________________________________________________________ ChannelDetails ____________________________
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldPCDChannelNumber);
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
                  LOldDownStreamNodeNumber := LChannelSourceDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
                  LOldDownStreamNodeName := Trim(LChannelSourceDataSet.DataSet.FieldByName('ChannelName').AsString);
                  LPCDNodeList.Clear;
                  LPCDNodeList.AddObject(LOldDownStreamNodeName,TObject(LOldDownStreamNodeNumber));
                  LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                                                                    LPCDNodeList,AProgressUpdateFuntion);
                  LNewDownStreamNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;

                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                  LNewPCDChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewPCDChannelNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := LNewDownStreamNodeNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := 0;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;
              //________________________________________________________ MinMaxChannel ____________________________
              LSourceSQL := MinMaxChannelSQL + LSourceWhereClause + ' AND MinMaxChannelNumber = '+ IntToStr(LOldPCDChannelNumber);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MinMaxChannelSQL + LDestinationWhereClause;
                LChannelDestinationDataSet.DataSet.Close;
                LChannelDestinationDataSet.SetSQL(LDestinationSQL);
                LChannelDestinationDataSet.SetReadOnly(False);
                LChannelDestinationDataSet.DataSet.Open;
                if LChannelDestinationDataSet.IsReadOnly  then
                begin
                  LChannelDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinMaxChannel cannot be set to updatable.');
                end
                else
                begin
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LChannelDataSQLAgent.GetMaxMinMaxFlowConstraintID+1;
                  LChannelDestinationDataSet.DataSet.FieldByName('MinMaxChannelNumber').AsInteger      := LNewPCDChannelNumber;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;
              //________________________________________________________ ChannelDetails ____________________________
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldRiverChannelNumber);
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
                  LNewRiverChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewRiverChannelNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := LNewNodeNumber;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;  
              //________________________________________________________ MinMaxChannel ____________________________
              LSourceSQL := MinMaxChannelSQL + LSourceWhereClause + ' AND MinMaxChannelNumber = '+ IntToStr(LOldPCDChannelNumber);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MinMaxChannelSQL + LDestinationWhereClause;
                LChannelDestinationDataSet.DataSet.Close;
                LChannelDestinationDataSet.SetSQL(LDestinationSQL);
                LChannelDestinationDataSet.SetReadOnly(False);
                LChannelDestinationDataSet.DataSet.Open;
                if LChannelDestinationDataSet.IsReadOnly  then
                begin
                  LChannelDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinMaxChannel cannot be set to updatable.');
                end
                else
                begin
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LChannelDataSQLAgent.GetMaxMinMaxFlowConstraintID+1;
                  LChannelDestinationDataSet.DataSet.FieldByName('MinMaxChannelNumber').AsInteger      := LNewRiverChannelNumber;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;
              LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
              LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
              LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
              LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewMineIdentifier;
              LDestinationDataSet.DataSet.FieldByName('RiverChannelNumber').AsInteger       := LNewRiverChannelNumber;
              LDestinationDataSet.DataSet.FieldByName('PCDChannelNumber').AsInteger         := LNewPCDChannelNumber;
              LDestinationDataSet.DataSet.FieldByName('NodeNumber').AsInteger               := LNewNodeNumber;
              LDestinationDataSet.DataSet.Post;
              //___________________________________________________ MineLakeEvaporation ____________________________________
              LSourceSQL := MineLakeEvaporationSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MineLakeEvaporationSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MineLakeEvaporation cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;
              //___________________________________________________ MineOpenCast ____________________________________
              LSourceSQL := MineOpenCastSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := MineOpenCastSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MineOpenCast cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := GetMaxOpenCastID+1;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
              //___________________________________________________ MinePanEvaporation ____________________________________
              LSourceSQL := MinePanEvaporationSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MinePanEvaporationSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinePanEvaporation cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;
              //___________________________________________________ MinimunGroundwaterFlowVolume ____________________________________
              LSourceSQL := MinimunGroundwaterFlowVolumeSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MinimunGroundwaterFlowVolumeSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinimunGroundwaterFlowVolume cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;
             //___________________________________________________ MineRechargeFactor ____________________________________
              LSourceSQL := MineRechargeFactorSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := MineRechargeFactorSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MineRechargeFactor cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
              //___________________________________________________ MineSlurryDump ____________________________________
              LSourceSQL := MineSlurryDumpSQL + LSourceWhereClause + ' AND MineIdentifier = '+ IntToStr(LMineIdentifier);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := MineSlurryDumpSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MineSlurryDump cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                  LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                  LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                  LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                  LDestinationDataSet.DataSet.FieldByName('MineIdentifier').AsInteger           := LNewMineIdentifier;
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := GetMaxSlurryDumpID+1;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
              CopyMineUnderGround;
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
      LPCDNodeList.Free;
      LMineList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.InsertMineSubCatchmentFlowVolumeSQL(AMineSubCatchmentID: integer) : string;
const OPNAME = 'TMineSQLAgent.InsertMineSubCatchmentFlowVolumeSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineSubCatchmentFlowVolume ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineSubCatchmentIdentifier, ' +
              ' Volume01, Volume02, Volume03, Volume04, ' +
              ' Volume05, Volume06, Volume07, Volume08, ' +
              ' Volume09, Volume10, Volume11, Volume12  ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineSubCatchmentID) + ','+
              ' :Volume01, :Volume02, :Volume03, :Volume04, ' +
              ' :Volume05, :Volume06, :Volume07, :Volume08, ' +
              ' :Volume09, :Volume10, :Volume11, :Volume12 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetDeleteMinimunGroundwaterFlowVolumeSQL(AMineIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetDeleteMinimunGroundwaterFlowVolumeSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM MinimunGroundwaterFlowVolume WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = ' + IntToStr(AMineIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.AddMineSubCatchment(var AIdentifier: integer;
                                           ACatchmentRefNr: integer;
                                           ACatchmentRefName: string): boolean;
const OPNAME = 'TMineSQLAgent.AddMineSubCatchment';
var
  LIdentifier : integer;
  LIndex      : integer;
  LDataSet    : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetMaxMineSubCatchmentID + 1;

        LDataSet.SetSQL(InsertMineSubCatchmentSQL(LIdentifier,ACatchmentRefNr,ACatchmentRefName));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(InsertMineSubCatchmentFlowVolumeSQL(LIdentifier));
        for LIndex := 1 to 12 do
          LDataSet.SetParams([Format('Volume%2.2d',[LIndex])],['0.0']);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;


        FAppModules.StudyArea.LastUpdateDate := Now();
        AIdentifier := LIdentifier;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TMineSQLAgent.InsertMineSubCatchmentSQL(AIdentifier,ACatchmentRefNr: integer;
                                                 ACatchmentRefName: string): string;
const OPNAME = 'TMineSQLAgent.InsertMineSubCatchmentSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO MineSubCatchment '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier,CatchmentReferenceNumber,CatchmentReferenceName,' +
      ' ProportionAntecedentFlow,GroundwaterFlowVolume,AntecedentRunoffDecayFactor,InUse'+
      ')VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AIdentifier) + ',' +
      IntToStr(ACatchmentRefNr) + ',' +
      QuotedStr(ACatchmentRefName) + ',' + '0.0'+ ',' +  '0.0'+ ',' +'0.0'+ ',' + '1' + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetMaxMineSubCatchmentID: integer;
const OPNAME = 'TMineSQLAgent.GetMaxMineSubCatchmentID';
var
  lSQL     : string;
  lDataSet : TAbstractModelDataset;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM MineSubCatchment WHERE ' + GetScenarioWhereClause;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('MaxID').IsNull then
          Result := LDataset.DataSet.FieldByName('MaxID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSQLAgent.GetMineSubCatchmentFlowVolumeSQL(AMineSubCatchmentIdentifier: integer): string;
const OPNAME = 'TMineSQLAgent.GetMineSubCatchmentFlowVolumeSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineSubCatchmentIdentifier, ' +
              ' Volume01, Volume02, Volume03, Volume04, '+
              ' Volume05, Volume06, Volume07, Volume08, '+
              ' Volume09, Volume10, Volume11, Volume12 ' +
              ' FROM  MineSubCatchmentFlowVolume ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineSubCatchmentIdentifier = '+ IntToStr(AMineSubCatchmentIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMineSQLAgent.GetMineSubCatchmentSQL: string;
const OPNAME = 'TMineSQLAgent.GetMineSubCatchmentSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' CatchmentReferenceNumber,CatchmentReferenceName, ProportionAntecedentFlow,'+
              ' GroundwaterFlowVolume, AntecedentRunoffDecayFactor,InUse' +
              ' FROM MineSubCatchment ' +
              ' WHERE '+
              GetScenarioWhereClause +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSQLAgent.LoadContextData_MineSubCatchmentVolumeFactors(AContextData: TStringList;
                                                                      AMineSubCatchmentIdentifier: integer;
                                                                      AFieldNameIdentifier: string);
const OPNAME = 'TMineSQLAgent.LoadContextData_MineSubCatchmentVolumeFactors';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineSubCatchmentIdentifier='    + IntToStr(AMineSubCatchmentIdentifier));
    AContextData.Add('FieldNameIdentifier='    + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
