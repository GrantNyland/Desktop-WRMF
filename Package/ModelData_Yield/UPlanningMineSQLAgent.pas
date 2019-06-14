unit UPlanningMineSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  ULoadGeneration,
  UPlanningMineData,
  UPlanningMineGrowthFactor,
  UPlanningOpenCast,
  UPlanningUnderGroundMine,
  UPlanningSlurryDump,
  UMineSQLAgent,
  UMiningData;

type
  TPlanningMineSQLAgent = class(TMineSQLAgent)
  public
    function GetMineSQL: string;
    function GetOpenCastSQL(AMineIdentifier: integer): string;
    function GetSlurryDumpSQL(AMineIdentifier: integer): string;
    function InsertMineSQL(AIdentifier: integer): string;
    function InsertOpenCastSQL (AMineID: integer): string;
    function InsertSlurryDumpSQL (AMineID: integer) : string;
    function InsertGrowthFactorSQL: string;
    function InsertLoadGenerationFlowSQL: string;
    function InsertLoadGenerationMeanOfSaltSQL: string;
    function getGrowthTypeDescription(AType : integer): string;
    function getLoadGenTypeDescription(AType : integer): string;

    function getMaxGrowthFactorID: integer;
    function getMaxLoadGenerationID: integer;
    function AddMine(AMine: TPlanningMine): boolean; overload;
    function AddOpenCast(AOpencast : TPlanningOpenCast): boolean; overload;
    function AddUnderground(AUnderground: TPlanningUnderGroundMine): boolean; overload;
    function AddSlurryDump(ASlurryDump: TPlanningSlurryDump): boolean; overload;
    function AddGrowthFactor(AGrowthFactor: TPlanningMineGrowthFactor): boolean;
    function AddLoadGeneration(ALoadGeneration: TLoadGeneration): boolean;
    function DeleteMine(AMine: TPlanningMine) : boolean; overload;
    function DeleteOpenCast(AOpencast : TPlanningOpenCast): boolean; overload;
    function DeleteGrowthFactor(AGrowthFactor : TPlanningMineGrowthFactor): wordbool;
    function DeleteLoadGeneration(ALoadGeneration : TLoadGeneration): wordbool;
    procedure LoadLoadContextData_GrowthFactor(AContextData: TStringList; AIdentifier: integer; AMineIdentifier: integer; AOpenCastIdentifier: integer=0;
                                               AUnderGroundIdentifier: integer=0; ASlurryDumpIdentifier: integer=0);


    procedure LoadContextData_GrowthFactorMine(AContextData : TStringList; AIdentifier, AFactorType: Integer);
    procedure LoadContextData_GrowthFactorOpenCast(AContextData : TStringList; AMineIdentifier, AOpenCastIdentifier, AFactorType: Integer);
    procedure LoadContextData_GrowthFactorUnderGround(AContextData: TStringList; AMineIdentifier,AUnderGroundIdentifier,AFactorType: integer);
    procedure LoadContextData_GrowthFactorSlurryDump(AContextData: TStringList; AMineIdentifier,ASlurryDumpIdentifier,AFactorType: integer);
    procedure LoadContextData_LoadGenerationOpenCast(AContextData: TStringList; AMineIdentifier, AOpenCastIdentifier,
                                             AFactorType: Integer; AFieldNameIdentifier: string); overload;
    procedure LoadContextData_LoadGeneration(AContextData: TstringList; AIdentifier: integer; AMineIdentifier: integer; AOpenCastIdentifier: integer = 0;
                                             AUnderGroundIdentifier: integer=0; ASlurryDumpIdentifier: integer=0; AFieldNameIdentifier: integer= -1);
    procedure LoadContextData_LoadGenerationOpenCast(AContextData: TStringList; AMineIdentifier, AOpenCastIdentifier,AFactorType: Integer); overload;
    procedure LoadContextData_LoadGenerationSlurryDump(AContextData: TStringList; AMineIdentifier, ASlurryDumpIdentifier, AFactorType: Integer); overload;
    procedure LoadContextData_LoadGenerationSlurryDump(AContextData: TStringList; AMineIdentifier, ASlurryDumpIdentifier, AFactorType: Integer; AFieldNameIdentifier: string); overload;
    procedure LoadContextData_LoadGenerationUnderGround(AContextData: TStringList; AMineIdentifier, AUnderGroundIdentifier, AFactorType: integer) overload;
    procedure LoadContextData_LoadGenerationUnderGround(AContextData: TStringList; AMineIdentifier, AUnderGroundIdentifier,AFactorType: integer; AFieldNameIdentifier: string); overload;
  end;

implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UAbstractObject,
  UConditionalOpp,
  UErrorHandlingOperations;


function TPlanningMineSQLAgent.GetMineSQL: string;
const OPNAME ='TPlanningMineSQLAgent.GetMineSQL';
begin
  Result := '';
  try
    Result := ' Select Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' NodeNumber, MineName, RiverChannelNumber, PCDChannelNumber, ' +
              ' HydrologyNodeNumber,BeneficiationPlantArea, BeneficiationRunOffFactor, ' +
              ' SaltWashoffNo, MeanAnnPrecip, SaltBuildUpRate, SaltWashOffEfficiencyFactor, ' +
              ' IniSaltStore, RainfallFileName'  +
              ' FROM Mine ' +
              ' WHERE ' + GetScenarioWhereClause +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.GetOpenCastSQL(AMineIdentifier: integer): string;
const OPNAME = 'TPlanningMineSQLAgent.GetOpenCastSQL';
begin
  Result:= '';
  try
     Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, ' +
              ' PitName, CoalReserveArea, WorkingsArea, DisturbedWorkingsArea, ' +
              ' DisturbedArea, WaterSurfaceEvapArea, DisturbedAreaRunOff, DisturbedWorkingsAreaRunOff, ' +
              ' DecantVolume, SeepageVolume, AnalysisStartVolume, MaximumSeepageRate, ' +
              ' SeepageExponent, PCDSurfaceArea, PCDStorageCapacity, PCDAnalysisStartVolume, ' +
              ' Abstraction, PCDIniConcentration, WorkingCommYear, WorkingCommMonth, WorkingDecommYear,' +
              ' WorkingDecommMonth, RunoffSaltWashOffEfficiencyFactor, IniSaltStore, ReChargeRate,' +
              ' AbstractToEvap, AbstractToRiver, AbstractToPCD, AbstractMonthTimeSeriesFile' +
              ' FROM  MineOpenCast ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND MineIdentifier = '+ IntToStr(AMineIdentifier) +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.GetSlurryDumpSQL(AMineIdentifier: integer): string;
const OPNAME = 'TPlanningMineSQLAgent.GetSlurryDumpSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, '+
                ' DumpName, DumpSurfaceArea, RunoffFactorToPCD, '+
                ' SeepageSplitFactor, PCDStorageCapacity, PCDSurfaceArea, PCDAnalysisStartVolume, SaltConcentration ' +
                ' FROM  MineSlurryDump ' +
                ' WHERE '+ GetScenarioWhereClause +
                ' AND MineIdentifier = '+ IntToStr(AMineIdentifier) +
                ' ORDER BY Identifier';
  except on E: Exception do HandleError(E,OPNAME);end;
end;

function TPlanningMineSQLAgent.InsertMineSQL(AIdentifier: integer): string;
const OPNAME = 'TPlanningMineSQLAgent.InsertMineSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO Mine '+
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, NodeNumber, ' +
              ' MineName, RiverChannelNumber, PCDChannelNumber, HydrologyNodeNumber, ' +
              ' BeneficiationPlantArea, BeneficiationRunOffFactor, SaltWashoffNo, ' +
              ' RainfallFileName, MeanAnnPrecip, SaltBuildUpRate,' +
              ' SaltWashOffEfficiencyFactor, IniSaltStore' +
              ') VALUES(' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AIdentifier) + ',' +
              ' :NodeNumber, :MineName, :RiverChannelNumber, :PCDChannelNumber, '+
              ' :HydrologyNodeNumber, :BeneficiationPlantArea, :BeneficiationRunOffFactor,' +
              ' :AssocSalt, :RainfallFineName, :MeanAnnualPrecipitation, :SaltBuildUpRate, '+
              ' :SaltWashOffEfficiencyFactor, :IniSaltStore) ';
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningMineSQLAgent.InsertGrowthFactorSQL: string;
const OPNAME = 'TPlanningMineSQLAgent.InsertGrowthFactorSQL';
begin
  Result := '';
  try
  Result := 'INSERT INTO MineGrowthFactors(Model, StudyAreaName,SubArea,Scenario, Identifier,' +
            'MineIdentifier, OCIdentifier, SlurryIdentifier, UDGIdentifier, NoOfPoints, FactorType,' +
            'InterpolationMethod, NoOfYears, GrowthFactors' + ')Values (' +
            QuotedStr(FAppModules.StudyArea.ModelCode) + ', ' +
            QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ', ' +
            QuotedStr(FAppModules.StudyArea.SubAreaCode) + ', ' +
            QuotedStr(FAppModules.StudyArea.ScenarioCode) + ', ' +
            ':Identifier, :MineIdentifier, :OCIIdentifier, :SlurryIdentifier, :UDGIdentifier,' +
            ':NoOfPoints, :FactorType, :InterpolationMethod, :NoOfYears, :GrowthFactors)';
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.InsertLoadGenerationFlowSQL: string;
const OPNAME = 'TPlanningMineSQLAgent.InsertLoadGenerationFlowSQL';
begin
  Result := '';
  try
    Result:= ' INSERT INTO MineLoadGenerationFlow(Model, StudyAreaName, SubArea, Scenario, LoadGenType,MineIdentifier,' +
             ' Identifier,OpenCastIdentifier,UDGIdentifier,SDIdentifier, StdDeviation,Flow01,Flow02,Flow03,Flow04,Flow05,' +
             ' Flow06,Flow07,Flow08,Flow09,Flow10)' +
             ' Values(' +
             QuotedStr(FAppModules.StudyArea.ModelCode) + ', ' +
             QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ', ' +
             QuotedStr(FAppModules.StudyArea.SubAreaCode) + ', ' +
             QuotedStr(FAppModules.StudyArea.ScenarioCode) + ', ' +
             ' :LoadGenType,:MineIdentifier, :Identifier,:OpenCastIdentifier,:UDGIdentifier,:SDIdentifier,:StdDeviation,' +
             ':Flow01, :Flow02, :Flow03, :Flow04, :Flow05, :Flow06, :Flow07, :Flow08, :Flow09, :Flow10' + ')';
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMineSQLAgent.InsertLoadGenerationMeanOfSaltSQL: string;
const OPNAME = 'TPlanningMineSQLAgent.InsertLoadGenerationMeanOfSaltSQL';
begin
  Result := '';
  try
    Result:= ' INSERT INTO MineLoadGenerationMeanOfSalt(Model, StudyAreaName, SubArea, Scenario, LoadGenType,MineIdentifier,' +
             ' Identifier,OpenCastIdentifier,UDGIdentifier,SDIdentifier, StdDeviation,MeanOfSalt01,MeanOfSalt02,MeanOfSalt03,MeanOfSalt04,MeanOfSalt05,' +
             ' MeanOfSalt06,MeanOfSalt07,MeanOfSalt08,MeanOfSalt09,MeanOfSalt10)' +
             ' Values(' +
             QuotedStr(FAppModules.StudyArea.ModelCode) + ', ' +
             QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ', ' +
             QuotedStr(FAppModules.StudyArea.SubAreaCode) + ', ' +
             QuotedStr(FAppModules.StudyArea.ScenarioCode) + ', ' +
             ':LoadGenType,:MineIdentifier,:Identifier, :OpenCastIdentifier,:UDGIdentifier,:SDIdentifier, :StdDeviation,' +
             ':MeanOfSalt01, :MeanOfSalt02, :MeanOfSalt03, :MeanOfSalt04, :MeanOfSalt05, :MeanOfSalt06, :MeanOfSalt07, :MeanOfSalt08, :MeanOfSalt09, :MeanOfSalt10' + ')';
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.getGrowthTypeDescription(AType : integer): string;
const OPNAME = 'TPlanningMineSQLAgent.getGrowthTypeDescription';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := '';
  try
   lSQL := 'SELECT Description FROM MineGrowthFactorType WHERE Type=:type';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataSet.SetParams(['type'], [IntToStr(AType)]);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('Description').IsNull then
          Result := LDataset.DataSet.FieldByName('Description').AsString;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E,OPNAME);  end;
end;

function TPlanningMineSQLAgent.getLoadGenTypeDescription(
  AType: integer): string;
const OPNAME = 'TPlanningMineSQLAgent.getLoadGenTypeDescription';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := '';
  try
   lSQL := 'SELECT Description FROM MineLoadGenerationType WHERE LoadGenType=:type';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(lSQL);
        LDataSet.SetParams(['type'], [IntToStr(AType)]);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.FieldByName('Description').IsNull then
          Result := LDataset.DataSet.FieldByName('Description').AsString;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E,OPNAME);  end;
end;

function TPlanningMineSQLAgent.getMaxGrowthFactorID: integer;
const OPNAME = 'TPlanningMineSQLAgent.getMaxGrowthFactorID';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM MineGrowthFactors WHERE ' + GetScenarioWhereClause;
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

function TPlanningMineSQLAgent.getMaxLoadGenerationID: integer;
const OPNAME = 'TPlanningMineSQLAgent.getMaxLoadGenerationID';
var
  lDataSet      : TAbstractModelDataset;
  lSQL          : string;
begin
  Result := 0;
  try
    lSQL := 'SELECT MAX(Identifier) AS MaxID FROM MineLoadGenerationFlow WHERE ' + GetScenarioWhereClause;
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

function TPlanningMineSQLAgent.AddGrowthFactor(AGrowthFactor: TPlanningMineGrowthFactor): boolean;
const OPNAME = 'TPlanningMineSQLAgent.AddGrowthFactor';
var
  LDataSet : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LMaxId : integer;
begin
   Result := False;
  try
    if (AGrowthFactor = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL),LDataSet);
    try
      if Assigned(LDataSet) then
      begin
       FAppModules.Database.StartTransaction;
       try
         LDataSet.SetSQL(InsertGrowthFactorSQL);
         LMaxId := getMaxGrowthFactorID + 1;
         LDataSet.SetParams(['Identifier'], [IntToStr(LMaxId)]);
         AGrowthFactor.Identifier := LMaxId;
         LDataSet.SetParams(['MineIdentifier'], [IntToStr(AGrowthFactor.MineIdentifier)]);
         LDataSet.SetParams(['OCIIdentifier'], [IntToStr(AGrowthFactor.OpenCastIdentifier)]);
         LDataSet.SetParams(['SlurryIdentifier'],[IntToStr(AGrowthFactor.SlurryDumpIdentifier)]);
         LDataSet.SetParams(['UDGIdentifier'], [IntToStr(AGrowthFactor.UnderGroundIdentifier)]);
         LDataSet.SetParams(['NoOfPoints'], [IntToStr(AGrowthFactor.NoOfPoints)]);
         LDataSet.SetParams(['FactorType'], [IntToStr(AGrowthFactor.FactorType)]);
         LDataSet.SetParams(['InterpolationMethod'],[IntToStr(AGrowthFactor.InterpolationMethod)]);   //[ConditionalOpp(AGrowthFactor.InterpolationMethod, 2,1)] );
         LDataSet.SetParams(['NoOfYears'], [AGrowthFactor.ListToStringYears]);
         LDataSet.SetParams(['GrowthFactors'], [AGrowthFactor.ListToStringGrowthFactor]);

         LDataSet.ExecSQL;
         LDataSet.DataSet.Close;

         FAppModules.StudyArea.LastUpdateDate := Now();
         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          FAppModules.Database.Commit;
          Result := true;
       except
          FAppModules.Database.Rollback;
       end;
      end;
    finally
      LDataSet.Free
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_GrowthFactorMine(
  AContextData: TStringList; AIdentifier, AFactorType: Integer);
const OPNAME ='TPlanningMineSQLAgent.LoadContextData_GrowthFactorMine';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier=' + IntToStr(AIdentifier));
    AContextData.Add('Identifier=');
    AContextData.Add('SlurryIdentier=-1');
    //AContextData.Add('UDGIdentifier=-1');
    //AContextData.Add('OpenCastIdentifier=-1');
    //AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_GrowthFactorOpenCast(
  AContextData: TStringList; AMineIdentifier, AOpenCastIdentifier, AFactorType: Integer);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_GrowthFactorOpenCast';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    //AContextData.Add('SlurryIdentier=-1');
    //AContextData.Add('UDGIdentifier=-1');
    AContextData.Add('OpenCastIdentifier= ' + IntToStr(AOpenCastIdentifier));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_GrowthFactorSlurryDump(
  AContextData: TStringList; AMineIdentifier, ASlurryDumpIdentifier,
  AFactorType: integer);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_GrowthFactorSlurryDump';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('SlurryIdentier=' + IntToStr(ASlurryDumpIdentifier)) ;
    //AContextData.Add('UDGIdentifier=' + IntToStr(-1));
    //AContextData.Add('OpenCastIdentifier= ' + IntToStr(-1));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_GrowthFactorUnderGround(
  AContextData: TStringList; AMineIdentifier, AUnderGroundIdentifier,
  AFactorType: integer);
const OPNAME = 'PlanningMineSQLAgent.LoadContextData_GrowthFactorUnderGround';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    //AContextData.Add('SlurryIdentier=-1');
    AContextData.Add('UDGIdentifier=' + IntToStr(AUnderGroundIdentifier));
    //AContextData.Add('OpenCastIdentifier= ' + IntToStr(-1));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_LoadGenerationOpenCast(AContextData: TStringList; AMineIdentifier: Integer; AOpenCastIdentifier: Integer;
                                                                        AFactorType: Integer;
                                                                        AFieldNameIdentifier: string);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGenerationOpenCast';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('OpenCastIdentifier= ' + IntToStr(AOpenCastIdentifier));
    //AContextData.Add('UNGIdentifier=-1');
    //AContextData.Add('SDIdentifier=-1');
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_LoadGeneration(
  AContextData: TstringList; AIdentifier, AMineIdentifier, AOpenCastIdentifier,
  AUnderGroundIdentifier, ASlurryDumpIdentifier: integer; AFieldNameIdentifier: integer);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGeneration';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier=' + IntToStr(AIdentifier));
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('OCIdentifier= ' + IntToStr(AOpenCastIdentifier));
    AContextData.Add('UDGIdentifier=' +  IntToStr(AUnderGroundIdentifier));
    AContextData.Add('SDIdentifier=' + IntToStr(ASlurryDumpIdentifier));
    if AFieldNameIdentifier >= 0 then
    AContextData.Add('FieldNameIdentifier=' + IntToStr(AFieldNameIdentifier +1));

  except on E: Exception do HandleError(E, OPNAME); end;

end;


procedure TPlanningMineSQLAgent.LoadContextData_LoadGenerationOpenCast(AContextData: TStringList; AMineIdentifier: Integer; AOpenCastIdentifier: Integer;
                                                                        AFactorType: Integer);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGenerationOpenCast';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('OpenCastIdentifier= ' + IntToStr(AOpenCastIdentifier));
    //AContextData.Add('UNGIdentifier=-1');
    //AContextData.Add('SDIdentifier=-1');
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineSQLAgent.LoadContextData_LoadGenerationSlurryDump(
  AContextData: TStringList; AMineIdentifier, ASlurryDumpIdentifier,
  AFactorType: Integer; AFieldNameIdentifier: string);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGenerationSlurryDump';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    //AContextData.Add('OpenCastIdentifier= ' + IntToStr(-1));
    //AContextData.Add('UNGIdentifier=-1');
    AContextData.Add('SDIdentifier=' + IntToStr(ASlurryDumpIdentifier));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_LoadGenerationUnderGround(
  AContextData: TStringList; AMineIdentifier, AUnderGroundIdentifier,
  AFactorType: integer);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGenerationUnderGround';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    //AContextData.Add('OpenCastIdentifier= ' + IntToStr(-1));
    AContextData.Add('UNGIdentifier=' + IntToStr(AUnderGroundIdentifier));
    //AContextData.Add('SDIdentifier=' + IntToStr(-1));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_LoadGenerationUnderGround(
  AContextData: TStringList; AMineIdentifier, AUnderGroundIdentifier,
  AFactorType: integer; AFieldNameIdentifier: string);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGenerationUnderGround';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    //AContextData.Add('OpenCastIdentifier= ' + IntToStr(-1));
    AContextData.Add('UNGIdentifier=' + AFieldNameIdentifier);
    //AContextData.Add('SDIdentifier=' + IntToStr(-1));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadLoadContextData_GrowthFactor(
  AContextData: TStringList; AIdentifier, AMineIdentifier, AOpenCastIdentifier,
  AUnderGroundIdentifier, ASlurryDumpIdentifier: integer);
const OPNAME =  ' TPlanningMineSQLAgent.LoadLoadContextData_GrowthFactor';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier=' + IntToStr(AIdentifier));
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    AContextData.Add('OCIdentifier= ' + IntToStr(AOpenCastIdentifier));
    AContextData.Add('UDGIdentifier=' + IntToStr(AUnderGroundIdentifier));
    AContextData.Add('SlurryIdentifier=' + IntToStr(ASlurryDumpIdentifier));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineSQLAgent.LoadContextData_LoadGenerationSlurryDump(
  AContextData: TStringList; AMineIdentifier, ASlurryDumpIdentifier,
  AFactorType: Integer);
const OPNAME = 'TPlanningMineSQLAgent.LoadContextData_LoadGenerationSlurryDump';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('MineIdentifier='    + IntToStr(AMineIdentifier));
    //AContextData.Add('OpenCastIdentifier= ' + IntToStr(-1));
    //AContextData.Add('UNGIdentifier=' + IntToStr(-1));
    AContextData.Add('SDIdentifier=' + IntToStr(ASlurryDumpIdentifier));
    AContextData.Add('FactorType=' + IntToStr(AFactorType));
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.AddLoadGeneration(
  ALoadGeneration: TLoadGeneration): boolean;
const OPNAME = 'TPlanningMineSQLAgent.AddLoadGeneration';
var
  LDataSet : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LMaxId : integer;
  LIndex : integer;
  LFieldName : string;
begin
   Result := False;
  try
    if (ALoadGeneration = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL),LDataSet);
    try
      if Assigned(LDataSet) then
      begin
       FAppModules.Database.StartTransaction;
       try
         LDataSet.SetSQL(InsertLoadGenerationFlowSQL);
         LMaxId := getMaxLoadGenerationID + 1;
         LDataSet.SetParams(['Identifier'], [IntToStr(LMaxId)]);
         ALoadGeneration.Identifier := LMaxId;
         LDataSet.SetParams(['MineIdentifier'], [IntToStr(ALoadGeneration.MineIdentifier)]);
         LDataSet.SetParams(['OpenCastIdentifier'], [IntToStr(ALoadGeneration.OpenCastIdentifier)]);
         LDataSet.SetParams(['UDGIdentifier'],[IntToStr(ALoadGeneration.UnderGroudIdentifier)]);
         LDataSet.SetParams(['SDIdentifier'], [IntToStr(ALoadGeneration.SlurryDumpIdentifier)]);
         LDataSet.SetParams(['StdDeviation'], [FloatToStr(ALoadGeneration.StandardDeviation)]);
         LDataSet.SetParams(['LoadGenType'], [IntToStr(ALoadGeneration.type_)]);
         for LIndex := 0 to 9 do
         begin
           LFieldName := Format('%s%2.2d',['Flow',LIndex+1]);
           LDataSet.SetParams([LFieldName],[FloatToStrF(ALoadGeneration.FlowByIndex[LIndex],ffNumber,5,2)]);
         end;

         LDataSet.ExecSQL;
         LDataSet.DataSet.Close;

         LDataSet.SetSQL(InsertLoadGenerationMeanOfSaltSQL);
         LDataSet.SetParams(['Identifier'], [IntToStr(LMaxId)]);
         ALoadGeneration.Identifier := LMaxId;
         LDataSet.SetParams(['MineIdentifier'], [IntToStr(ALoadGeneration.MineIdentifier)]);
         LDataSet.SetParams(['OpenCastIdentifier'], [IntToStr(ALoadGeneration.OpenCastIdentifier)]);
         LDataSet.SetParams(['UDGIdentifier'],[IntToStr(ALoadGeneration.UnderGroudIdentifier)]);
         LDataSet.SetParams(['SDIdentifier'], [IntToStr(ALoadGeneration.SlurryDumpIdentifier)]);
         LDataSet.SetParams(['StdDeviation'], [FloatToStr(ALoadGeneration.StandardDeviation)]);
         LDataSet.SetParams(['LoadGenType'], [IntToStr(ALoadGeneration.type_)]);
         for LIndex := 0 to 9 do
         begin
          LFieldName :=    Format('%s%2.2d',['MeanOfSalt',LIndex+1]);
          LDataSet.SetParams([LFieldName],[FloatToStrF(ALoadGeneration.MeanOfSaltByIndex[LIndex],ffNumber,5,2)]);
         end;

         LDataSet.ExecSQL;
         LDataSet.DataSet.Close;

         FAppModules.StudyArea.LastUpdateDate := Now();
         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          FAppModules.Database.Commit;
          Result := true;
       except
          FAppModules.Database.Rollback;
       end;
      end;
    finally
      LDataSet.Free
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.AddMine(AMine: TPlanningMine): boolean;
const OPNAME = 'TPlanningMineSQLAgent.AddMine';
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
          LDataSet.SetParams(['AssocSalt'],                   [IntToStr(AMine.AssocSaltWashoff)]);
          LDataSet.SetParams(['RainfallFineName'],            [AMine.RainfallFileName]);
          LDataSet.SetParams(['MeanAnnualPrecipitation'],     [FloatToStr(AMine.MeanAnnualPrecipitation)]);
          LDataSet.SetParams(['SaltBuildUpRate'],             [FloatToStr(AMine.SaltBuildUpRate)]);
          LDataSet.SetParams(['SaltWashOffEfficiencyFactor'], [FloatToStr(AMine.SaltWashOffEfficiencyFactor)]);
          LDataSet.SetParams(['IniSaltStore'],                [FloatToStr(AMine.IniSaltStore)]);

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
//            if not AddGrowthFactor(AMine.GrowthFactor) then
//            begin
//              FAppModules.Database.Rollback;
//              Result := False;
//              Exit;
//            end;

          

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

function TPlanningMineSQLAgent.DeleteGrowthFactor(
  AGrowthFactor: TPlanningMineGrowthFactor): wordbool;
const OPNAME = 'TPlanningMineSQLAgent.DeleteGrowthFactor';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQL         : string;
begin
  Result := false;
  try

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      FAppModules.Database.StartTransaction;
      if Assigned(LDataSet) then
      try
        LSQL := 'DELETE FROM MineGrowthFactors WHERE ' + GetScenarioWhereClause +
        ' AND (MineIdentifier= ' + IntToStr(AGrowthFactor.MineIdentifier) + ') AND ' +
        ' (OCIdentifier= ' + IntToStr(AGrowthFactor.OpenCastIdentifier) +  ' ) AND ' +
        ' (SlurryIdentifier= ' + IntToStr(AGrowthFactor.SlurryDumpIdentifier) + ') AND ' +
        ' (UDGIdentifier= ' + IntToStr(AGrowthFactor.UnderGroundIdentifier) + ') AND ' +
        ' (Identifier= ' + IntToStr(AGrowthFactor.Identifier) + ')';
        LDataSet.SetSQL(LSQL);
        LDataSet.ExecSQL();
        LDataSet.DataSet.Close;

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
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.DeleteLoadGeneration(ALoadGeneration : TLoadGeneration): wordbool;
const OPNAME = 'TPlanningMineSQLAgent.DeleteLoadGeneration';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQL         : string;
begin
  Result := false;
  try

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      FAppModules.Database.StartTransaction;
      if Assigned(LDataSet) then
      try
        LSQL := 'DELETE FROM MineLoadGenerationFlow WHERE ' + GetScenarioWhereClause +
        ' AND (MineIdentifier= ' + IntToStr(ALoadGeneration.MineIdentifier) + ') AND ' +
        ' (OpenCastIdentifier= ' + IntToStr(ALoadGeneration.OpenCastIdentifier) +  ' ) AND ' +
        ' (SDIdentifier= ' + IntToStr(ALoadGeneration.SlurryDumpIdentifier) + ') AND ' +
        ' (UDGIdentifier= ' + IntToStr(ALoadGeneration.UnderGroudIdentifier) + ') AND ' +
        ' (Identifier= ' + IntToStr(ALoadGeneration.Identifier) + ')';
        LDataSet.SetSQL(LSQL);
        LDataSet.ExecSQL();
        LDataSet.DataSet.Close;

        LSQL := 'DELETE FROM MineLoadGenerationMeanOfSalt WHERE ' + GetScenarioWhereClause +
        ' AND (MineIdentifier= ' + IntToStr(ALoadGeneration.MineIdentifier) + ') AND ' +
        ' (OpenCastIdentifier= ' + IntToStr(ALoadGeneration.OpenCastIdentifier) +  ' ) AND ' +
        ' (SDIdentifier= ' + IntToStr(ALoadGeneration.SlurryDumpIdentifier) + ') AND ' +
        ' (UDGIdentifier= ' + IntToStr(ALoadGeneration.UnderGroudIdentifier) + ') AND ' +
        ' (Identifier= ' + IntToStr(ALoadGeneration.Identifier) + ')';
        LDataSet.SetSQL(LSQL);
        LDataSet.ExecSQL();
        LDataSet.DataSet.Close;

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
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.DeleteMine(AMine: TPlanningMine) : boolean;
const OPNAME = 'TPlanningMineSQLAgent.DeleteMine';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQL         : string;
begin
  Result := inherited DeleteMine(AMine);
  try
    if Result then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        FAppModules.Database.StartTransaction;
        if Assigned(LDataSet) then
        try
          LSQL := 'DELETE FROM MineGrowthFactors WHERE ' + GetScenarioWhereClause + ' AND (MineIdentifier= ' +
                  IntToStr(AMine.Identifier) + ')';
          LDataSet.SetSQL(LSQL);
          LDataSet.ExecSQL();
          LDataSet.DataSet.Close;

          LSQL := 'DELETE FROM MineLoadGenerationFlow WHERE ' + GetScenarioWhereClause + ' AND (MineIdentifier= ' +
                  IntToStr(AMine.Identifier) + ')';
          LDataSet.SetSQL(LSQL);
          LDataSet.ExecSQL();
          LDataSet.DataSet.Close;

          LSQL := 'DELETE FROM MineLoadGenerationMeanOfSalt WHERE ' + GetScenarioWhereClause + ' AND (MineIdentifier= ' +
                  IntToStr(AMine.Identifier) + ')';
          LDataSet.SetSQL(LSQL);
          LDataSet.ExecSQL();
          LDataSet.DataSet.Close;

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
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.DeleteOpenCast(AOpencast: TPlanningOpenCast): boolean;
const OPNAME = 'TPlanningMineSQLAgent.DeleteOpenCast';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQL         : string;
begin
  Result := inherited DeleteOpenCast(AOpencast);
  try
    if Result then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        FAppModules.Database.StartTransaction;
        if Assigned(LDataSet) then
        try
          LSQL := 'DELETE FROM MineGrowthFactors WHERE ' + GetScenarioWhereClause + ' AND (MineIdentifier= ' +
                  IntToStr(AOpencast.MineIdentifier) +')' + ' AND (OCIdentifier= ' + IntToStr(AOpencast.Identifier) + ')';
          LDataSet.SetSQL(LSQL);
          LDataSet.ExecSQL();
          LDataSet.DataSet.Close;

          LSQL := 'DELETE FROM MineLoadGenerationMeanOfSalt WHERE ' + GetScenarioWhereClause + ' AND (MineIdentifier= ' +
                  IntToStr(AOpencast.MineIdentifier) +')' + ' AND (OpenCastIdentifier= ' + IntToStr(AOpencast.Identifier) + ')';
          LDataSet.SetSQL(LSQL);
          LDataSet.ExecSQL();
          LDataSet.DataSet.Close;

          LSQL := 'DELETE FROM MineLoadGenerationFlow WHERE ' + GetScenarioWhereClause + ' AND (MineIdentifier= ' +
                  IntToStr(AOpencast.MineIdentifier) +')' + ' AND (OpenCastIdentifier= ' + IntToStr(AOpencast.Identifier) + ')';
          LDataSet.SetSQL(LSQL);
          LDataSet.ExecSQL();
          LDataSet.DataSet.Close;

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
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.InsertOpenCastSQL(AMineID: Integer): string;
const OPNAME = 'TPlanningMineSQLAgent.InsertOpenCastSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineOpenCast ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, PitName, ' +
              ' CoalReserveArea, WorkingsArea, DisturbedWorkingsArea, DisturbedArea, WaterSurfaceEvapArea, ' +
              ' DisturbedAreaRunOff, DisturbedWorkingsAreaRunOff, DecantVolume, ' +
              ' SeepageVolume, AnalysisStartVolume, MaximumSeepageRate, SeepageExponent, ' +
              ' PCDSurfaceArea, PCDStorageCapacity, PCDAnalysisStartVolume, Abstraction, PCDIniConcentration, ' +
              ' WorkingCommYear, WorkingCommMonth, WorkingDecommYear, WorkingDecommMonth,' +
              ' RunoffSaltWashOffEfficiencyFactor, IniSaltStore, ReChargeRate, AbstractToEvap,' +
              ' AbstractToRiver, AbstractToPCD, AbstractMonthTimeSeriesFile) VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineID) + ','+
              ' :Identifier, :PitName, :CoalReserveArea, :WorkingsArea, :DisturbedWorkingsArea, '+
              ' :DisturbedArea, :WaterSurfaceEvapArea, :DisturbedAreaRunOff, :DisturbedWorkingsAreaRunOff, '+
              ' :DecantVolume, :SeepageVolume, :AnalysisStartVolume, :MaximumSeepageRate, '+
              ' :SeepageExponent, :PCDSurfaceArea, :PCDStorageCapacity, :PCDAnalysisStartVolume,' +
              ' :Abstraction, :PCDIniConcentration, ' +
              ' :WorkingCommYear, :WorkingCommMonth, :WorkingDecommYear, :WorkingDecommMonth,' +
              ' :RunoffSaltWashOffEfficiencyFactor, :IniSaltStore, :ReChargeRate, :AbstractToEvap,' +
              ' :AbstractToRiver, :AbstractToPCD, :AbstractMonthTimeSeriesFile' +
              ' ) ';
  except on E: Exception do HandleError(E,OPNAME);  end;
end;

function TPlanningMineSQLAgent.InsertSlurryDumpSQL(AMineID: integer): string;
const OPNAME = 'TPlanningMineSQLAgent.InsertSlurryDumpSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineSlurryDump ' +
              ' (Model, StudyAreaName, SubArea, Scenario, MineIdentifier, Identifier, DumpName, ' +
              ' DumpSurfaceArea, RunoffFactorToPCD, SeepageSplitFactor, ' +
              ' PCDStorageCapacity, PCDSurfaceArea, PCDAnalysisStartVolume, SaltConcentration ' +
              ' ) VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AMineID) + ',' +
              ' :Identifier, :DumpName, :DumpSurfaceArea, :RunoffFactorToPCD, '+
              ' :SeepageSplitFactor, :PCDStorageCapacity, :PCDSurfaceArea, :PCDAnalysisStartVolume, :SaltConcentration) ';
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.AddOpenCast(AOpencast: TPlanningOpenCast): boolean;
const  OPNAME = 'TPlanningMineSQLAgent.AddOpenCast';
var
  LDataSet : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LIndex   : Integer;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LLoadGeneration : TLoadGeneration;
begin
  Result := False;
  try
    if (AOpenCast = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
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
          LDataSet.SetParams(['Abstraction'], [ConditionalOpp(AOpencast.Abstraction,1,0)]);
          LDataSet.SetParams(['PCDIniConcentration'],      [FloatToStr(AOpenCast.PCDIniConcentration)]);
          LDataSet.SetParams(['WorkingCommYear'],      [IntToStr(AOpenCast.WorkingCommYear)]);
          LDataSet.SetParams(['WorkingCommMonth'],      [IntToStr(AOpenCast.WorkingCommMonth)]);
          LDataSet.SetParams(['WorkingDecommYear'],      [IntToStr(AOpenCast.WorkingDecommYear)]);
          LDataSet.SetParams(['WorkingDecommMonth'],      [IntToStr(AOpenCast.WorkingDecommMonth)]);
          LDataSet.SetParams(['RunoffSaltWashOffEfficiencyFactor'],      [FloatToStr(AOpenCast.RunOffSaltWashOffEfficiencyFactor)]);
          LDataSet.SetParams(['IniSaltStore'],      [FloatToStr(AOpenCast.IniSaltStore)]);
          LDataSet.SetParams(['ReChargeRate'],      [FloatToStr(AOpenCast.ReChargeRate)]);
          LDataSet.SetParams(['AbstractToEvap'],      [FloatToStr(AOpenCast.AbstractToEvap)]);
          LDataSet.SetParams(['AbstractToRiver'],      [FloatToStr(AOpenCast.AbstractToRiver)]);
          LDataSet.SetParams(['AbstractToPCD'],      [FloatToStr(AOpenCast.AbstractToCPD)]);
          LDataSet.SetParams(['AbstractMonthTimeSeriesFile'],      [AOpenCast.AbstractMonthTimeSeriesFile]);
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

          for LIndex := 2 to 7 do
          begin
            LGrowthFactor := AOpencast.GrowthFactorByType(LIndex);
            if(Assigned(LGrowthFactor)) then
              if not AddGrowthFactor(LGrowthFactor) then
              begin
                FAppModules.Database.Rollback;
                Exit;
              end;
          end;
          for LIndex := 1 to 2 do
            begin
              LLoadGeneration := AOpencast.LoadGenerationByType(LIndex);
              if Assigned(LLoadGeneration) then
                if not AddLoadGeneration(LLoadGeneration) then
                begin
                  FAppModules.Database.Rollback;
                  Exit;
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
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineSQLAgent.AddSlurryDump(
  ASlurryDump: TPlanningSlurryDump): boolean;
const OPNAME = 'TPlanningMineSQLAgent.AddSlurryDump';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LSQLAgent    : TPlanningMineSQLAgent;
  LIndex       : integer;
begin
  Result := False;
  try
    if (ASlurryDump = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
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

        AddGrowthFactor(ASlurryDump.GrowthFactor);
        AddLoadGeneration(ASlurryDump.LoadGeneration);

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

function TPlanningMineSQLAgent.AddUnderground(
  AUnderground: TPlanningUnderGroundMine): boolean;
const OPNAME = 'TPlanningMineSQLAgent.AddUnderground';
begin
  Result := inherited AddUnderground(AUnderground);
  try
    if Result then
    begin
      //AddGrowthFactor(AUnderground.GrowthFactor);
      //AddLoadGeneration(AUnderground.LoadGeneration);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
