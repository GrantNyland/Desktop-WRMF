unit UGroundWaterSQLAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UGroundWater,
  UAbstractObject;
type
  TGroundWaterSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetMaxGroundWaterSubCatchmentID : integer;
    function GetGroundWaterSubCatchmentCountSQL : string;
    function CopyGroundWaterSubCatchment(AModel,ADestinationStudyAreaName,ADestinationSubArea,ADestinationScenario,
             ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
             ASourceWhereClause,ADestinationWhereClause : string; AGroundWaterIdentifier, ANewGroundWaterIdentifier : integer;
             ANewAquiferNodeNumber: integer; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  public
    //GroundWater Features
    function GetGroundWaterSQL: string;
    function GetGroundWaterPitmanSQL(AGroundWaterID: integer)  : string;
    function GetGroundWaterEvaporationSQL(AGroundWaterID: integer)   : string;
    function GetGroundWaterUsageFactorSQL(AGroundWaterID: integer)    : string;
    function GetGroundWaterSubCatchmentSQL(AGroundWaterIdentifier: integer)    : string;

    function InsertGroundWaterSQL(AIdentifier: integer): string;
    function InsertGroundWaterPitmanSQL(AGroundWaterID: integer): string;
    function InsertGroundWaterEvaporationSQL(AGroundWaterID: integer) : string;
    function InsertGroundWaterUsageFactorSQL(AGroundWaterID: integer) : string;
    function InsertGroundWaterSubCatchmentSQL(AGroundWaterIdentifier: integer) : string;

    function GetDeleteGroundWaterSQL(AIdentifier: integer): string;
    function GetDeleteGroundWaterPitmanSQL(AGroundWaterID : integer) : string;
    function GetDeleteGroundWaterEvaporationSQL(AGroundWaterID : integer) : string;
    function GetDeleteGroundWaterUsageFactorSQL(AGroundWaterID : integer): string;
    function GetDeleteGroundWaterSubCatchmentSQL(AGroundWaterIdentifier : integer): string;
    procedure LoadContextData_GroundWaterEvaporation(AContextData: TStringList;
                                                     AGroundWaterIdentifier: integer;
                                                     AFieldNameIdentifier: string);
    procedure LoadContextData_GroundWaterIdentifier(AContextData: TStringList;
                                                    AGroundWaterIdentifier: string);
    procedure LoadContextData_Identifier(AContextData: TStringList;
                                                    AGroundWaterIdentifier: string);

    function AddGroundWater(AGroundWater: TGroundWater) : boolean;
    function DeleteGroundWater(AGroundWater: TGroundWater): boolean;

    function GetMaxGroundWaterID : integer;

    function GetGroundWaterCountSQL : string;

    function CopyGroundWaterFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AGroundWaterList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  end;

implementation
uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UErrorHandlingOperations, DB;

{ TGroundWaterSQLAgent }

function TGroundWaterSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TGroundWaterSQLAgent.GetScenarioWhereClause';
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

function TGroundWaterSQLAgent.GetGroundWaterSQL: string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' AquiferNodeNumber, GroundWaterName, GroundWaterDescription, AquiferStorativity, AquiferStaticWaterlevel, ' +
              ' UnsaturatedStorageCapacity, InitialUnsaturatedStorage, MaximumAquiferRecharge, MovingAverageRecharge, ' +
              ' MaximumBaseFlowRate, HeadBaseFlowPower, MaximumHydrologicalGradient, AquiferTransmissivity, ' +
              ' BoreHoleDistanceToRiver, MaximumWaterAbstraction, ParameterK2, ParameterK3, WaterEvaporationArea ' +
              ' FROM  GroundWater A' +
              ' WHERE '+
              GetScenarioWhereClause +
              ' ORDER BY A.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetGroundWaterPitmanSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterPitmanSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' SoilMoistureCapacity, SoilMoistureStorageCapacity, SoilMoistureFlowState, ' +
              ' SoilMoistureFlowEquation, MaximumGroundWaterFlow, SoilMoistureRechargeEquation, GroundWaterFlow ' +
              ' FROM  GroundWaterPitman A' +
              ' WHERE '+
              GetScenarioWhereClause +
              ' AND A.GroundWaterIdentifier = '+ IntToStr(AGroundWaterID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetGroundWaterEvaporationSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterEvaporationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Evaporation01, Evaporation02, Evaporation03, Evaporation04, ' +
              ' Evaporation05, Evaporation06, Evaporation07, Evaporation08, ' +
              ' Evaporation09, Evaporation10, Evaporation11, Evaporation12 ' +
              ' FROM  GroundWaterEvaporation A' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND A.GroundWaterIdentifier = '+ IntToStr(AGroundWaterID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetGroundWaterUsageFactorSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, ' +
              ' Factor07, Factor08, Factor09, Factor10, Factor11, Factor12 ' +
              ' FROM  GroundWaterUsageFactor A ' +
              ' WHERE ' +
              GetScenarioWhereClause +
              ' AND A.GroundWaterIdentifier = '+ IntToStr(AGroundWaterID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TGroundWaterSQLAgent.InsertGroundWaterSQL(AIdentifier: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.InsertGroundWaterSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO GroundWater '+
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, AquiferNodeNumber, ' +
              ' GroundWaterName, GroundWaterDescription, AquiferStorativity, AquiferStaticWaterlevel, ' +
              ' UnsaturatedStorageCapacity, InitialUnsaturatedStorage, MaximumAquiferRecharge, ' +
              ' MovingAverageRecharge, MaximumBaseFlowRate, HeadBaseFlowPower, MaximumHydrologicalGradient, ' +
              ' AquiferTransmissivity, BoreHoleDistanceToRiver, MaximumWaterAbstraction, ' +
              ' ParameterK2, ParameterK3, WaterEvaporationArea' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AIdentifier) + ',' +
              ' :AquiferNodeNumber, :GroundWaterName, :GroundWaterDescription, :AquiferStorativity, :AquiferStaticWaterlevel, ' +
              ' :UnsaturatedStorageCapacity, :InitialUnsaturatedStorage, :MaximumAquiferRecharge, ' +
              ' :MovingAverageRecharge, :MaximumBaseFlowRate, :HeadBaseFlowPower, :MaximumHydrologicalGradient, ' +
              ' :AquiferTransmissivity, :BoreHoleDistanceToRiver, :MaximumWaterAbstraction, ' +
              ' :ParameterK2, :ParameterK3, :WaterEvaporationArea ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.InsertGroundWaterPitmanSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.InsertGroundWaterPitmanSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO GroundWaterPitman '+
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' SoilMoistureCapacity, SoilMoistureStorageCapacity, SoilMoistureFlowState, ' +
              ' SoilMoistureFlowEquation, MaximumGroundWaterFlow, SoilMoistureRechargeEquation, ' +
              ' GroundWaterFlow ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AGroundWaterID) + ',' +
              ' :SoilMoistureCapacity, :SoilMoistureStorageCapacity, :SoilMoistureFlowState, ' +
              ' :SoilMoistureFlowEquation, :MaximumGroundWaterFlow, :SoilMoistureRechargeEquation, ' +
              ' :GroundWaterFlow )';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.InsertGroundWaterEvaporationSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.InsertGroundWaterEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GroundWaterEvaporation ' +
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Evaporation01, Evaporation02, Evaporation03, Evaporation04, ' +
              ' Evaporation05, Evaporation06, Evaporation07, Evaporation08, ' +
              ' Evaporation09, Evaporation10, Evaporation11, Evaporation12  ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AGroundWaterID) + ','+
              ' :Evaporation01, :Evaporation02, :Evaporation03, :Evaporation04, ' +
              ' :Evaporation05, :Evaporation06, :Evaporation07, :Evaporation08, ' +
              ' :Evaporation09, :Evaporation10, :Evaporation11, :Evaporation12  )';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.InsertGroundWaterUsageFactorSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.InsertGroundWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GroundWaterUsageFactor ' +
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, ' +
              ' Factor07, Factor08, Factor09, Factor10, Factor11, Factor12 ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AGroundWaterID) + ','+
              ' :Factor01, :Factor02, :Factor03, :Factor04, :Factor05, :Factor06, ' +
              ' :Factor07, :Factor08, :Factor09, :Factor10, :Factor11, :Factor12 )';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetDeleteGroundWaterSQL(AIdentifier: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetDeleteGroundWaterSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GroundWater WHERE ' +
              GetScenarioWhereClause +
              ' AND Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetDeleteGroundWaterPitmanSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetDeleteGroundWaterPitmanSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GroundWaterPitman WHERE ' +
              GetScenarioWhereClause +
              ' AND GroundWaterIdentifier = ' + IntToStr(AGroundWaterID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetDeleteGroundWaterEvaporationSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetDeleteGroundWaterEvaporationSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GroundWaterEvaporation WHERE ' +
              GetScenarioWhereClause +
              ' AND GroundWaterIdentifier = ' + IntToStr(AGroundWaterID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetDeleteGroundWaterUsageFactorSQL(AGroundWaterID: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetDeleteGroundWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GroundWaterUsageFactor WHERE ' +
              GetScenarioWhereClause +
              ' AND GroundWaterIdentifier = ' + IntToStr(AGroundWaterID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.AddGroundWater(AGroundWater: TGroundWater): boolean;
const OPNAME = 'TGroundWaterSQLAgent.AddGroundWater';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  LIndex,
  LIdentifier  : integer;
begin
  Result := False;
  try
    if (AGroundWater = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertGroundWaterSQL((AGroundWater.Identifier)));
          LDataSet.SetParams(['AquiferNodeNumber'],             [IntToStr(AGroundWater.AquiferNodeNr)]);
          LDataSet.SetParams(['GroundWaterName'],               [AGroundWater.Name]);
          LDataSet.SetParams(['GroundWaterDescription'],        [AGroundWater.Description]);
          LDataSet.SetParams(['AquiferStorativity'],            [FloatToStr(AGroundWater.AquiferStorativity)]);
          LDataSet.SetParams(['AquiferStaticWaterlevel'],       [FloatToStr(AGroundWater.AquiferStaticWaterLevel)]);
          LDataSet.SetParams(['UnsaturatedStorageCapacity'],    [FloatToStr(AGroundWater.UnsaturatedStorageCapacity)]);
          LDataSet.SetParams(['InitialUnsaturatedStorage'],     [FloatToStr(AGroundWater.InitialUnsaturatedStorage)]);
          LDataSet.SetParams(['MaximumAquiferRecharge'],        [FloatToStr(AGroundWater.MaximumDischargeRate)]);
          LDataSet.SetParams(['MovingAverageRecharge'],         [FloatToStr(AGroundWater.MovingAverageRecharge)]);
          LDataSet.SetParams(['MaximumBaseFlowRate'],           [FloatToStr(AGroundWater.MaximumRateOfGroundwaterBaseFlow)]);
          LDataSet.SetParams(['HeadBaseFlowPower'],             [FloatToStr(AGroundWater.PowerHeadDifferenceBaseFlowEquation)]);
          LDataSet.SetParams(['MaximumHydrologicalGradient'],   [FloatToStr(AGroundWater.MaximumHydrologicalGradient)]);
          LDataSet.SetParams(['AquiferTransmissivity'],         [FloatToStr(AGroundWater.AquiferTransmissivity)]);
          LDataSet.SetParams(['BoreHoleDistanceToRiver'],       [FloatToStr(AGroundWater.BoreHoleDistanceToRiver)]);
          LDataSet.SetParams(['MaximumWaterAbstraction'],       [FloatToStr(AGroundWater.MaximumGroundwaterAbstraction)]);
          LDataSet.SetParams(['ParameterK2'],                   [FloatToStr(AGroundWater.ParameterK2)]);
          LDataSet.SetParams(['ParameterK3'],                   [FloatToStr(AGroundWater.ParameterK3)]);
          LDataSet.SetParams(['WaterEvaporationArea'],          [FloatToStr(AGroundWater.GroundWaterEvaporationArea)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertGroundWaterPitmanSQL(AGroundWater.Identifier));
          LDataSet.SetParams(['SoilMoistureCapacity'],            [FloatToStr(AGroundWater.PitmanSoilMoistureCapacity)]);
          LDataSet.SetParams(['SoilMoistureStorageCapacity'],     [FloatToStr(AGroundWater.PitmanSoilMoistureStorageCapacity)]);
          LDataSet.SetParams(['SoilMoistureFlowState'],           [FloatToStr(AGroundWater.PitmansoilMoistureFlowState)]);
          LDataSet.SetParams(['SoilMoistureFlowEquation'],        [FloatToStr(AGroundWater.PitmanSoilMoistureFlowEquation)]);
          LDataSet.SetParams(['MaximumGroundWaterFlow'],          [FloatToStr(AGroundWater.PitmanMaximumGroundwaterFlow)]);
          LDataSet.SetParams(['SoilMoistureRechargeEquation'],    [FloatToStr(AGroundWater.PitmanSoilMoistureRechargeEquation)]);
          LDataSet.SetParams(['GroundWaterFlow'],                 [FloatToStr(AGroundWater.PitmanGroundwaterFlow)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LIdentifier := GetMaxGroundWaterSubCatchmentID + 1;
          LDataSet.SetSQL(InsertGroundWaterSubCatchmentSQL(LIdentifier));
          LDataSet.SetParams(['RefNodeNumber'],                       [IntToStr(AGroundWater.RefNodeNumber)]);
          LDataSet.SetParams(['AquiferNodeNumber'],                   [IntToStr(AGroundWater.AquiferNodeNr)]);
          LDataSet.SetParams(['AbstractionNodeNumber'],               [IntToStr(AGroundWater.AbstractionNodeNr)]);
          LDataSet.SetParams(['CollectionNodeNumber'],                [IntToStr(AGroundWater.CollectionNodeNr)]);
          LDataSet.SetParams(['BaseFlowNodeNumber'],                  [IntToStr(AGroundWater.BaseFlowNodeNr)]);
          LDataSet.SetParams(['AquiferInflowChannelNr'],              [IntToStr(AGroundWater.AquiferInflowChannelNr)]);
          LDataSet.SetParams(['AquferExcessChannelNr'],               [IntToStr(AGroundWater.AquiferExcessInterflowChannelNr)]);
          LDataSet.SetParams(['GroundWaterBaseFlowChannelNr'],        [IntToStr(AGroundWater.GroundWaterBaseflowChannelNr)]);
          LDataSet.SetParams(['RegulationFromAquiferChannelNr'],      [IntToStr(AGroundWater.AbstractionFromAquiferChannelNr)]);
          LDataSet.SetParams(['RegulationFromBaseFlowChannelNr'],     [IntToStr(AGroundWater.AbstractionFromBaseflowChannelNr)]);
          LDataSet.SetParams(['UpstreamChannelNr'],                   [IntToStr(AGroundWater.InflowFromUpstreamAquiferChannelNr)]);
          LDataSet.SetParams(['DownstreamChannelNr'],                 [IntToStr(AGroundWater.OutflowToDownstreamAquiferChannelNr)]);
          LDataSet.SetParams(['SurfaceRunOffChannelNr'],              [IntToStr(AGroundWater.SurfaceRunoffAndSoilInterflowChannelNr)]);
          LDataSet.SetParams(['BaseFlowRemainderChannelNr'],          [IntToStr(AGroundWater.GroundWaterBaseFlowRemainderChannelNr)]);
          LDataSet.SetParams(['GroundWaterAbstractionChannelNr'],     [IntToStr(AGroundWater.GroundWaterAbstractionChannelNr)]);
          LDataSet.SetParams(['OutflowToNetworkChannelNr'],           [IntToStr(AGroundWater.OutflowToNetworkChannelNr)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertGroundWaterEvaporationSQL(AGroundWater.Identifier));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('Evaporation%2.2d',[LIndex])],[FloatToStr(AGroundWater.MonthlyWaterEvaporation[LIndex])]);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertGroundWaterUsageFactorSQL(AGroundWater.Identifier));
          for LIndex := 1 to 12 do
            LDataSet.SetParams([Format('Factor%2.2d',[LIndex])],[FloatToStr(AGroundWater.MonthlyWaterUsageFactors[LIndex])]);
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

function TGroundWaterSQLAgent.DeleteGroundWater(AGroundWater: TGroundWater): boolean;
const OPNAME = 'TGroundWaterSQLAgent.DeleteGroundWater';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
begin
  Result := False;
  try
    if(AGroundWater = nil) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(GetDeleteGroundWaterSQL(AGroundWater.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteGroundWaterPitmanSQL(AGroundWater.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteGroundWaterEvaporationSQL(AGroundWater.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteGroundWaterUsageFactorSQL(AGroundWater.Identifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteGroundWaterSubCatchmentSQL(AGroundWater.Identifier));
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

function TGroundWaterSQLAgent.GetGroundWaterCountSQL: string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM GroundWater A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetGroundWaterSubCatchmentCountSQL: string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterSubCatchmentCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(GroundWaterIdentifier) AS MaxIdentifier FROM GroundWaterSubCatchment A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetMaxGroundWaterID: integer;
const OPNAME = 'TGroundWaterSQLAgent.GetMaxGroundWaterID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetGroundWaterCountSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetMaxGroundWaterSubCatchmentID: integer;
const OPNAME = 'TGroundWaterSQLAgent.GetMaxGroundWaterSubCatchmentID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetGroundWaterSubCatchmentCountSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetGroundWaterSubCatchmentSQL(AGroundWaterIdentifier: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario,GroundWaterIdentifier,' +
              ' RefNodeNumber,AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,BaseFlowNodeNumber,' +
              ' AquiferInflowChannelNr,AquferExcessChannelNr,' +
              ' GroundWaterBaseFlowChannelNr,RegulationFromAquiferChannelNr,' +
              ' RegulationFromBaseFlowChannelNr,UpstreamChannelNr,DownstreamChannelNr,' +
              ' SurfaceRunOffChannelNr, BaseFlowRemainderChannelNr,' +
              ' GroundWaterAbstractionChannelNr, OutflowToNetworkChannelNr' +
              ' FROM GroundWaterSubCatchment A' +
              ' WHERE '+
              GetScenarioWhereClause +
              ' AND A.GroundWaterIdentifier = '+ IntToStr(AGroundWaterIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.InsertGroundWaterSubCatchmentSQL(AGroundWaterIdentifier: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.InsertGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO GroundWaterSubCatchment ' +
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' RefNodeNumber,AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,BaseFlowNodeNumber, ' +
              ' AquiferInflowChannelNr,AquferExcessChannelNr,' +
              ' GroundWaterBaseFlowChannelNr,RegulationFromAquiferChannelNr,'+
              ' RegulationFromBaseFlowChannelNr,UpstreamChannelNr,DownstreamChannelNr,' +
              ' SurfaceRunOffChannelNr,BaseFlowRemainderChannelNr,' +
              ' GroundWaterAbstractionChannelNr,OutflowToNetworkChannelNr' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AGroundWaterIdentifier) + ',' +
              ' :RefNodeNumber,:AquiferNodeNumber,:AbstractionNodeNumber,:CollectionNodeNumber,:BaseFlowNodeNumber, ' +
              ' :AquiferInflowChannelNr,:AquferExcessChannelNr,' +
              ' :GroundWaterBaseFlowChannelNr,:RegulationFromAquiferChannelNr,'+
              ' :RegulationFromBaseFlowChannelNr,:UpstreamChannelNr,:DownstreamChannelNr,' +
              ' :SurfaceRunOffChannelNr,:BaseFlowRemainderChannelNr,' +
              ' :GroundWaterAbstractionChannelNr,:OutflowToNetworkChannelNr)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterSQLAgent.GetDeleteGroundWaterSubCatchmentSQL(AGroundWaterIdentifier: integer): string;
const OPNAME = 'TGroundWaterSQLAgent.GetDeleteGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GroundWaterSubCatchment WHERE ' +
              GetScenarioWhereClause +
              ' AND GroundWaterIdentifier = ' + IntToStr(AGroundWaterIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterSQLAgent.LoadContextData_GroundWaterEvaporation(AContextData: TStringList;
                                                                      AGroundWaterIdentifier: integer;
                                                                      AFieldNameIdentifier: string);
const OPNAME = 'TGroundWaterSQLAgent.LoadContextData_GroundWaterEvaporation';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('GroundWaterIdentifier='    + IntToStr(AGroundWaterIdentifier));
    AContextData.Add('FieldNameIdentifier='    + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGroundWaterSQLAgent.LoadContextData_GroundWaterIdentifier(AContextData: TStringList;
                                                                     AGroundWaterIdentifier: string);
const OPNAME = 'TGroundWaterSQLAgent.LoadContextData_GroundWaterIdentifier';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + AGroundWaterIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGroundWaterSQLAgent.LoadContextData_Identifier(AContextData: TStringList;
                                                          AGroundWaterIdentifier: string);
const OPNAME = 'TGroundWaterSQLAgent.LoadContextData_Identifier';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('GroundWaterIdentifier='    + AGroundWaterIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TGroundWaterSQLAgent.CopyGroundWaterFromScenario(ASourceStudyAreaName, ASourceSubArea, ASourceScenario: string;
                                                          AGroundWaterList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TGroundWaterSQLAgent.CopyGroundWaterFromScenario';
      GroundWaterSQL             = 'SELECT * FROM GroundWater WHERE ';
      GroundWaterPitmanSQL       = 'SELECT * FROM GroundWaterPitman WHERE ';
      GroundWaterUsageFactorSQL  = 'SELECT * FROM GroundWaterUsageFactor WHERE ';
      GroundWaterEvaporationSQL  = 'SELECT * FROM GroundWaterEvaporation WHERE ';
var
  LNewGroundWaterIdentifier                   : integer;
  LGroundWaterIdentifier                      : integer;
  LNewAquiferNodeNumber                       : integer;
  LStop                                       : boolean;
  LIndex                                      : integer;
  LSourceDataSet                              : TAbstractModelDataset;
  LDestinationDataSet                         : TAbstractModelDataset;
  LImportDate                                 : TDateTime;
  LReservoirDataSQLAgent                      : TReservoirDataSQLAgent;
  LModel                                      : string;
  LFieldName                                  : string;
  LDestinationStudyAreaName                   : string;
  LDestinationSubArea                         : string;
  LDestinationScenario                        : string;
  LSourceSQL                                  : string;
  LDestinationSQL                             : string;
  LSourceWhereClause                          : string;
  LDestinationWhereClause                     : string;
  LMessage                                    : string;
  LGroundWaterName                            : string;
  LGroundWaterIndex                           : integer;
  LGroundWaterList                            : TStringList;
begin
  Result := False;
  try
    if not Assigned(AGroundWaterList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    LGroundWaterList       := TStringList.Create;
    try
      if (Assigned(LSourceDataSet)) and (Assigned(LDestinationDataSet)) then
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
        LNewGroundWaterIdentifier := GetMaxGroundWaterID;
        FAppModules.Database.StartTransaction;
        for LGroundWaterIndex := 0 to AGroundWaterList.Count - 1 do
        begin
          LGroundWaterList.Clear;
          LGroundWaterList.AddObject(AGroundWaterList[LGroundWaterIndex],AGroundWaterList.Objects[LGroundWaterIndex]);
          LGroundWaterIdentifier    := integer(AGroundWaterList.Objects[LGroundWaterIndex]);
          LGroundWaterName          := AGroundWaterList[LGroundWaterIndex];
          LNewAquiferNodeNumber     := LReservoirDataSQLAgent.GetMaxReservoirNumber + 1;
          LNewGroundWaterIdentifier := LNewGroundWaterIdentifier + 1;
          LMessage                  := 'Copying GroundWater ('+LGroundWaterName+') ' + IntToStr(LGroundWaterIndex + 1) + ' of '+ IntToStr(AGroundWaterList.Count);
          AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
          if LStop then
          begin
            if FAppModules.Database.InTransaction then
              FAppModules.Database.Rollback;
            Exit;
          end;

          //________________________________________________________ GroundWater ____________________________
          LSourceSQL := GroundWaterSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LGroundWaterIdentifier);
          LSourceDataSet.DataSet.Close;
          LSourceDataSet.SetSQL(LSourceSQL);
          LSourceDataSet.DataSet.Open;
          if LSourceDataSet.DataSet.Eof then Continue;

          LDestinationSQL := GroundWaterSQL + LDestinationWhereClause;
          LDestinationDataSet.DataSet.Close;
          LDestinationDataSet.SetSQL(LDestinationSQL);
          LDestinationDataSet.SetReadOnly(False);
          LDestinationDataSet.DataSet.Open;
          if LDestinationDataSet.IsReadOnly  then
          begin
            LDestinationDataSet.DataSet.Close;
            raise Exception.Create('Query to table GroundWater cannot be set to updatable.');
          end
          else
          begin
            LDestinationDataSet.DataSet.Append;
            for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount - 1 do
            begin
              LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
              LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
              LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
            end;
            LDestinationDataSet.DataSet.FieldByName('Model').AsString              := LModel;
            LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString      := LDestinationStudyAreaName;
            LDestinationDataSet.DataSet.FieldByName('SubArea').AsString            := LDestinationSubArea;
            LDestinationDataSet.DataSet.FieldByName('Scenario').AsString           := LDestinationScenario;
            LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger        := LNewGroundWaterIdentifier;
            LDestinationDataSet.DataSet.FieldByName('AquiferNodeNumber').AsInteger := LNewAquiferNodeNumber;
            LDestinationDataSet.DataSet.Post;

            //___________________________________________________ GroundWaterPitman ____________________________________
            LSourceSQL := GroundWaterPitmanSQL + LSourceWhereClause + ' AND GroundWaterIdentifier = '+ IntToStr(LGroundWaterIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := GroundWaterPitmanSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table GroundWaterPitman cannot be set to updatable.');
              end
              else
              begin
                LDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount - 1 do
                begin
                  LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                  := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString          := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString               := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('GroundWaterIdentifier').AsInteger := LNewGroundWaterIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //___________________________________________________ GroundWaterEvaporation ____________________________________
            LSourceSQL := GroundWaterEvaporationSQL + LSourceWhereClause + ' AND GroundWaterIdentifier = '+ IntToStr(LGroundWaterIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := GroundWaterEvaporationSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table GroundWaterEvaporation cannot be set to updatable.');
              end
              else
              begin
                LDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount - 1 do
                begin
                  LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                  := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString          := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString               := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('GroundWaterIdentifier').AsInteger := LNewGroundWaterIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //___________________________________________________ GroundWaterUsageFactor ____________________________________
            LSourceSQL := GroundWaterUsageFactorSQL + LSourceWhereClause + ' AND GroundWaterIdentifier = '+ IntToStr(LGroundWaterIdentifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := GroundWaterUsageFactorSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table GroundWaterUsageFactor cannot be set to updatable.');
              end
              else
              begin
                LDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount - 1 do
                begin
                  LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                  := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString          := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString               := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('GroundWaterIdentifier').AsInteger := LNewGroundWaterIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            CopyGroundWaterSubCatchment(LModel,LDestinationStudyAreaName,LDestinationSubArea,LDestinationScenario,
                                        ASourceStudyAreaName,ASourceSubArea,ASourceScenario,LSourceWhereClause,
                                        LDestinationWhereClause,LGroundWaterIdentifier,
                                        LNewGroundWaterIdentifier,LNewAquiferNodeNumber,AProgressUpdateFuntion);
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
      LReservoirDataSQLAgent.Free;
      LGroundWaterList.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TGroundWaterSQLAgent.CopyGroundWaterSubCatchment(AModel,ADestinationStudyAreaName,ADestinationSubArea,
                                                          ADestinationScenario,ASourceStudyAreaName,ASourceSubArea,
                                                          ASourceScenario,ASourceWhereClause,ADestinationWhereClause : string;
                                                          AGroundWaterIdentifier,ANewGroundWaterIdentifier : integer;
                                                          ANewAquiferNodeNumber: integer; AProgressUpdateFuntion: TProgressUpdateFuntion) : boolean;
const OPNAME = 'TGroundWaterSQLAgent.CopyGroundWaterSubCatchment';
      ChannelDetailsSQL          = 'SELECT * FROM ChannelDetails WHERE ';
      GroundWaterSubCatchmentSQL = 'SELECT * FROM GroundWaterSubCatchment WHERE ';
var
  LSubCatchmentSourceDataSet                : TAbstractModelDataset;
  LSubCatchmentDestinationDataSet           : TAbstractModelDataset;
  LChannelSourceDataSet                     : TAbstractModelDataset;
  LChannelDestinationDataSet                : TAbstractModelDataset;
  LReservoirDataSQLAgent                    : TReservoirDataSQLAgent;
  LChannelDataSQLAgent                      : TChannelDataSQLAgent;

  LOldAquiferNodeNr                         : integer;
  LOldAbstractionNodeNr                     : integer;
  LNewAbstractionNodeNr                     : integer;
  LOldCollectionNodeNr                      : integer;
  LNewCollectionNodeNr                      : integer;
  LOldBaseFlowNodeNr                        : integer;
  LNewBaseFlowNodeNr                        : integer;

  LOldAquiferInflowChannelNr                : integer;
  LNewAquiferInflowChannelNr                : integer;
  LOldInflowFromUpstreamAquiferChannelNr    : integer;
  LNewInflowFromUpstreamAquiferChannelNr    : integer;
  LOldOutflowToDownstreamAquiferChannelNr   : integer;
  LNewOutflowToDownstreamAquiferChannelNr   : integer;
  LOldGroundWaterBaseflowChannelNr          : integer;
  LNewGroundWaterBaseflowChannelNr          : integer;
  LOldAbstractionFromBaseFlowChannelNr      : integer;
  LNewAbstractionFromBaseFlowChannelNr      : integer;
  LOldAbstractionFromAquiferChannelNr       : integer;
  LNewAbstractionFromAquiferChannelNr       : integer;
  LOldAquiferExcessInterflowChannelNr       : integer;
  LNewAquiferExcessInterflowChannelNr       : integer;
  LOldSurfaceRunoffChannelNr                : integer;
  LNewSurfaceRunoffChannelNr                : integer;
  LOldGroundWaterBaseFlowRemainderChannelNr : integer;
  LNewGroundWaterBaseFlowRemainderChannelNr : integer;
  LOldGroundWaterAbstractionChannelNr       : integer;
  LNewGroundWaterAbstractionChannelNr       : integer;
  LOldOutflowToNetworkChannelNr             : integer;
  LNewOutflowToNetworkChannelNr             : integer;

  LAquiferNodeList                          : TStringList;
  LAbstractionNodeList                      : TStringList;
  LCollectionNodeList                       : TStringList;
  LBaseFlowNodeList                         : TStringList;

  LIndex                                    : integer;
  LNewChannelID                             : integer;
  LName                                     : string;
  LSourceSQL                                : string;
  LDestinationSQL                           : string;
  LFieldName                                : string;
begin
    Result := False;
    try
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubCatchmentSourceDataSet);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubCatchmentDestinationDataSet);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
      LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
      LAquiferNodeList       := TStringList.Create;
      LAbstractionNodeList   := TStringList.Create;
      LCollectionNodeList    := TStringList.Create;
      LBaseFlowNodeList      := TStringList.Create;
      try
        LNewAbstractionNodeNr                     := 0;
        LNewCollectionNodeNr                      := 0;
        LNewBaseFlowNodeNr                        := 0;
        LNewAquiferInflowChannelNr                := 0;
        LNewAquiferExcessInterflowChannelNr       := 0;
        LNewGroundWaterBaseflowChannelNr          := 0;
        LNewAbstractionFromAquiferChannelNr       := 0;
        LNewAbstractionFromBaseFlowChannelNr      := 0;
        LNewOutflowToDownstreamAquiferChannelNr   := 0;
        LNewInflowFromUpstreamAquiferChannelNr    := 0;
        LNewSurfaceRunoffChannelNr                := 0;
        LNewGroundWaterBaseFlowRemainderChannelNr := 0;
        LNewGroundWaterAbstractionChannelNr       := 0;
        LNewOutflowToNetworkChannelNr             := 0;

        //________________________________________________________ GroundWaterSubCatchment ____________________________
        LSourceSQL := GroundWaterSubCatchmentSQL + ASourceWhereClause + ' AND GroundWaterIdentifier = '+ IntToStr(AGroundWaterIdentifier);
        LSubCatchmentSourceDataSet.DataSet.Close;
        LSubCatchmentSourceDataSet.SetSQL(LSourceSQL);
        LSubCatchmentSourceDataSet.DataSet.Open;

        while not LSubCatchmentSourceDataSet.DataSet.Eof do
        begin
          LDestinationSQL := GroundWaterSubCatchmentSQL + ADestinationWhereClause;
          LSubCatchmentDestinationDataSet.DataSet.Close;
          LSubCatchmentDestinationDataSet.SetSQL(LDestinationSQL);
          LSubCatchmentDestinationDataSet.SetReadOnly(False);
          LSubCatchmentDestinationDataSet.DataSet.Open;
          if LSubCatchmentDestinationDataSet.IsReadOnly  then
          begin
            LSubCatchmentDestinationDataSet.DataSet.Close;
            raise Exception.Create('Query to table GroundWaterSubCatchment cannot be set to updatable.');
          end
          else
          begin
            LSubCatchmentDestinationDataSet.DataSet.Append;

            LOldAquiferNodeNr                         := LSubCatchmentSourceDataSet.DataSet.FieldByName('AquiferNodeNumber').AsInteger;
            LOldAbstractionNodeNr                     := LSubCatchmentSourceDataSet.DataSet.FieldByName('AbstractionNodeNumber').AsInteger;
            LOldCollectionNodeNr                      := LSubCatchmentSourceDataSet.DataSet.FieldByName('CollectionNodeNumber').AsInteger;
            LOldBaseFlowNodeNr                        := LSubCatchmentSourceDataSet.DataSet.FieldByName('BaseFlowNodeNumber').AsInteger;

            LOldAquiferInflowChannelNr                := LSubCatchmentSourceDataSet.DataSet.FieldByName('AquiferInflowChannelNr').AsInteger;
            LOldOutflowToDownstreamAquiferChannelNr   := LSubCatchmentSourceDataSet.DataSet.FieldByName('DownstreamChannelNr').AsInteger;
            LOldInflowFromUpstreamAquiferChannelNr    := LSubCatchmentSourceDataSet.DataSet.FieldByName('UpstreamChannelNr').AsInteger;
            LOldGroundWaterBaseflowChannelNr          := LSubCatchmentSourceDataSet.DataSet.FieldByName('GroundWaterBaseFlowChannelNr').AsInteger;
            LOldAbstractionFromBaseFlowChannelNr      := LSubCatchmentSourceDataSet.DataSet.FieldByName('RegulationFromBaseFlowChannelNr').AsInteger;
            LOldAbstractionFromAquiferChannelNr       := LSubCatchmentSourceDataSet.DataSet.FieldByName('RegulationFromAquiferChannelNr').AsInteger;
            LOldAquiferExcessInterflowChannelNr       := LSubCatchmentSourceDataSet.DataSet.FieldByName('AquferExcessChannelNr').AsInteger;
            LOldSurfaceRunoffChannelNr                := LSubCatchmentSourceDataSet.DataSet.FieldByName('SurfaceRunOffChannelNr').AsInteger;
            LOldGroundWaterBaseFlowRemainderChannelNr := LSubCatchmentSourceDataSet.DataSet.FieldByName('BaseFlowRemainderChannelNr').AsInteger;
            LOldGroundWaterAbstractionChannelNr       := LSubCatchmentSourceDataSet.DataSet.FieldByName('GroundWaterAbstractionChannelNr').AsInteger;
            LOldOutflowToNetworkChannelNr             := LSubCatchmentSourceDataSet.DataSet.FieldByName('OutflowToNetworkChannelNr').AsInteger;

            LAquiferNodeList.Clear;
            LName := 'Aquifer Node';
            LAquiferNodeList.AddObject(LName,TObject(LOldAquiferNodeNr));
            if(LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
               LAquiferNodeList,AProgressUpdateFuntion)) then
            begin
              //__________________________________________Aquifer Inflow Channel Details _______________________________________________
              LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldAquiferInflowChannelNr);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                  LNewAquiferInflowChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                  LChannelDestinationDataSet.DataSet.Append;
                  for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                  begin
                    LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                    LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                    LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;
                  LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                  LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                  LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                  LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewAquiferInflowChannelNr;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := ANewAquiferNodeNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := 0;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;

              //________________________________________Inflow From Upstream Aquifer Channel Details _________________________________________
              LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldInflowFromUpstreamAquiferChannelNr);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                  LNewInflowFromUpstreamAquiferChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                  LChannelDestinationDataSet.DataSet.Append;
                  for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                  begin
                    LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                    LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                    LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;
                  LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                  LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                  LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                  LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewInflowFromUpstreamAquiferChannelNr;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := ANewAquiferNodeNumber;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := 0;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;
              
              //________________________________________Outflow To Downstream Aquifer Channel Details _________________________________________
              LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldOutflowToDownstreamAquiferChannelNr);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if not LChannelSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                  LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                  LNewOutflowToDownstreamAquiferChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                  LChannelDestinationDataSet.DataSet.Append;
                  for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                  begin
                    LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                    LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                    LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                  end;
                  LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                  LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                  LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                  LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                  LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewOutflowToDownstreamAquiferChannelNr;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := ANewAquiferNodeNumber;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
              end;

              LBaseFlowNodeList.Clear;
              LName := 'Base Flow Node';
              LBaseFlowNodeList.AddObject(LName,TObject(LOldBaseFlowNodeNr));
              if(LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                 LBaseFlowNodeList,AProgressUpdateFuntion)) then
              begin
                //______________________________________________GroundWater Base flow Channel Details _____________________________________
                LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldGroundWaterBaseflowChannelNr);
                LChannelSourceDataSet.DataSet.Close;
                LChannelSourceDataSet.SetSQL(LSourceSQL);
                LChannelSourceDataSet.DataSet.Open;
                if not LChannelSourceDataSet.DataSet.Eof then
                begin
                  LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                    LNewBaseFlowNodeNr := LReservoirDataSQLAgent.GetMaxReservoirNumber;

                    LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                    LNewGroundWaterBaseflowChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                    LChannelDestinationDataSet.DataSet.Append;
                    for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                    begin
                      LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                      LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                      LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                    end;
                    LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                    LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                    LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                    LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                    LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                    LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewGroundWaterBaseflowChannelNr;
                    LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                    LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := LNewBaseFlowNodeNr;
                    LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := ANewAquiferNodeNumber;
                    LChannelDestinationDataSet.DataSet.Post;
                  end;
                end;

                LAbstractionNodeList.Clear;
                LName := 'Abstraction Node';
                LAbstractionNodeList.AddObject(LName,TObject(LOldAbstractionNodeNr));
                if(LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                   LAbstractionNodeList,AProgressUpdateFuntion)) then
                begin
                  //__________________________________________________Abstraction From Base Flow Channel Details _______________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldAbstractionFromBaseFlowChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewAbstractionNodeNr := LReservoirDataSQLAgent.GetMaxReservoirNumber;

                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewAbstractionFromBaseFlowChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewAbstractionFromBaseFlowChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := LNewAbstractionNodeNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := LNewBaseFlowNodeNr;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;

                  //_______________________________________________Abstraction From Aquifer Channel Details ____________________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldAbstractionFromAquiferChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewAbstractionFromAquiferChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewAbstractionFromAquiferChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := LNewAbstractionNodeNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := ANewAquiferNodeNumber;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;

                  //_______________________________________________GroundWater Abstraction Channel Details ____________________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldGroundWaterAbstractionChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewGroundWaterAbstractionChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewGroundWaterAbstractionChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := LNewAbstractionNodeNr;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;
                end;

                LCollectionNodeList.Clear;
                LName := 'Collection Node';
                LCollectionNodeList.AddObject(LName,TObject(LOldCollectionNodeNr));
                if(LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                   LCollectionNodeList,AProgressUpdateFuntion)) then
                begin
                  //__________________________________________________Aquifer Excess Interflow Channel Details ________________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldAquiferExcessInterflowChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewCollectionNodeNr := LReservoirDataSQLAgent.GetMaxReservoirNumber;

                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewAquiferExcessInterflowChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewAquiferExcessInterflowChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := LNewCollectionNodeNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := ANewAquiferNodeNumber;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;

                  //________________________________________________GroundWater Base Flow Remainder Channel Details ___________________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldGroundWaterBaseFlowRemainderChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewGroundWaterBaseFlowRemainderChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewGroundWaterBaseFlowRemainderChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := LNewCollectionNodeNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := LNewBaseFlowNodeNr;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;

                  //___________________________________________________Surface Runoff Channel Details _________________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldSurfaceRunoffChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewSurfaceRunoffChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewSurfaceRunoffChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := LNewCollectionNodeNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := 0;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;

                  //________________________________________________OutFlow To Network Channel Details ___________________________________
                  LSourceSQL := ChannelDetailsSQL + ASourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldOutflowToNetworkChannelNr);
                  LChannelSourceDataSet.DataSet.Close;
                  LChannelSourceDataSet.SetSQL(LSourceSQL);
                  LChannelSourceDataSet.DataSet.Open;
                  if not LChannelSourceDataSet.DataSet.Eof then
                  begin
                    LDestinationSQL := ChannelDetailsSQL + ADestinationWhereClause;
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
                      LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier + 1;
                      LNewOutflowToNetworkChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber + 1;
                      LChannelDestinationDataSet.DataSet.Append;
                      for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount - 1 do
                      begin
                        LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                        LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                        LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                      end;
                      LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString           := AModel;
                      LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString   := ADestinationStudyAreaName;
                      LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString         := ADestinationSubArea;
                      LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString        := ADestinationScenario;
                      LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger     := LNewChannelID;
                      LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger  := LNewOutflowToNetworkChannelNr;
                      LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger  := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger := 0;
                      LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger   := LNewCollectionNodeNr;
                      LChannelDestinationDataSet.DataSet.Post;
                    end;
                  end;
                end;
              end;
            end;

            for LIndex := 0 to  LSubCatchmentDestinationDataSet.DataSet.FieldCount - 1 do
            begin
              LFieldName := LSubCatchmentDestinationDataSet.DataSet.Fields[LIndex].FieldName;
              LSubCatchmentDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
              LSubCatchmentSourceDataSet.DataSet.FieldByName(LFieldName).Value;
            end;

            LSubCatchmentDestinationDataSet.DataSet.FieldByName('Model').AsString                            := AModel;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString                    := ADestinationStudyAreaName;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('SubArea').AsString                          := ADestinationSubArea;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('Scenario').AsString                         := ADestinationScenario;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('GroundWaterIdentifier').AsInteger           := ANewGroundWaterIdentifier;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('AquiferNodeNumber').AsInteger               := ANewAquiferNodeNumber;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('AbstractionNodeNumber').AsInteger           := LNewAbstractionNodeNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('CollectionNodeNumber').AsInteger            := LNewCollectionNodeNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('BaseFlowNodeNumber').AsInteger              := LNewBaseFlowNodeNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('AquiferInflowChannelNr').AsInteger          := LNewAquiferInflowChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('AquferExcessChannelNr').AsInteger           := LNewAquiferExcessInterflowChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('GroundWaterBaseFlowChannelNr').AsInteger    := LNewGroundWaterBaseflowChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('RegulationFromAquiferChannelNr').AsInteger  := LNewAbstractionFromAquiferChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('RegulationFromBaseFlowChannelNr').AsInteger := LNewAbstractionFromBaseFlowChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('UpstreamChannelNr').AsInteger               := LNewInflowFromUpstreamAquiferChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('DownstreamChannelNr').AsInteger             := LNewOutflowToDownstreamAquiferChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('SurfaceRunOffChannelNr').AsInteger          := LNewSurfaceRunoffChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('BaseFlowRemainderChannelNr').AsInteger      := LNewGroundWaterBaseFlowRemainderChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('GroundWaterAbstractionChannelNr').AsInteger := LNewGroundWaterAbstractionChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.FieldByName('OutflowToNetworkChannelNr').AsInteger       := LNewOutflowToNetworkChannelNr;
            LSubCatchmentDestinationDataSet.DataSet.Post;
          end;
          LSubCatchmentSourceDataSet.DataSet.Next;
        end;
      finally
        FreeAndNil(LSubCatchmentSourceDataSet);
        FreeAndNil(LSubCatchmentDestinationDataSet);
        FreeAndNil(LChannelSourceDataSet);
        FreeAndNil(LChannelDestinationDataSet);
        FreeAndNil(LReservoirDataSQLAgent);
        FreeAndNil(LChannelDataSQLAgent);
        FreeAndNil(LAquiferNodeList);
        FreeAndNil(LAbstractionNodeList);
        FreeAndNil(LCollectionNodeList);
        FreeAndNil(LBaseFlowNodeList);
      end;
      Result := True;
    except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
