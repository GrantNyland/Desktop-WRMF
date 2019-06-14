//
//
//  UNIT      : Contains TFile22DatabaseAgent Class
//  AUTHOR    : Presley Mudau
//  DATE      : 02/10/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFile22DatabaseAgent;

interface

uses
  Classes,
  sysutils,
  Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UGroundWaterFileObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile22DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadGroundWaterSQL: string;
    function ReadGroundWaterPitmanSQL(AGroundWaterID: integer): string;
    function ReadGroundWaterEvaporationSQL(AGroundWaterID: integer): string;
    function ReadGroundWaterUsageFactorSQL(AGroundWaterID: integer): string;
    function ReadGroundWaterSubCatchmentSQL(AGroundWaterID: integer): string;
    function ReadF22UnkownDataSQL: string;

    function WriteGroundWaterSQL: string;
    function WriteGroundWaterPitmanSQL: string;
    function WriteGroundWaterEvaporationSQL: string;
    function WriteGroundWaterUsageFactorSQL: string;
    function WriteGroundWaterSubCatchmentSQL: string;
    function WriteF22UnkownDataSQL: string;
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TFile22DatabaseAgent.ReadGroundWaterSQL: string;
const OPNAME = 'TFile22DatabaseAgent.ReadGroundWaterSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' AquiferNodeNumber, GroundWaterName, GroundWaterDescription, AquiferStorativity, AquiferStaticWaterlevel, ' +
              ' UnsaturatedStorageCapacity, InitialUnsaturatedStorage, MaximumAquiferRecharge, MovingAverageRecharge, ' +
              ' MaximumBaseFlowRate, HeadBaseFlowPower, MaximumHydrologicalGradient, AquiferTransmissivity, ' +
              ' BoreHoleDistanceToRiver, MaximumWaterAbstraction, ParameterK2, ParameterK3, WaterEvaporationArea ' +
              ' FROM GroundWater WHERE' +
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.ReadGroundWaterEvaporationSQL(AGroundWaterID: integer): string;
const OPNAME = 'TFile22DatabaseAgent.ReadGroundWaterEvaporationSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier'+
              ' ,Evaporation01,Evaporation02,Evaporation03,Evaporation04,Evaporation05'+
              ' ,Evaporation06,Evaporation07,Evaporation08,Evaporation09,Evaporation10'+
              ' ,Evaporation11,Evaporation12'+
              ' FROM GroundWaterEvaporation WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND' +
              ' (GroundWaterIdentifier   =' + IntTostr(AGroundWaterID)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.ReadGroundWaterUsageFactorSQL(AGroundWaterID: integer): string;
const OPNAME = 'TFile22DatabaseAgent.ReadGroundWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, ' +
              ' Factor07, Factor08, Factor09, Factor10, Factor11, Factor12 ' +
              ' FROM GroundWaterUsageFactor WHERE' +
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (GroundWaterIdentifier   =' + IntTostr(AGroundWaterID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.ReadGroundWaterSubCatchmentSQL(AGroundWaterID: integer): string;
const OPNAME = 'TFile22DatabaseAgent.ReadGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario,GroundWaterIdentifier,' +
              ' RefNodeNumber,AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,BaseFlowNodeNumber,' +
              ' AquiferInflowChannelNr,AquferExcessChannelNr,' +
              ' GroundWaterBaseFlowChannelNr,RegulationFromAquiferChannelNr,' +
              ' RegulationFromBaseFlowChannelNr,DownstreamChannelNr,' +
              ' SurfaceRunOffChannelNr' +
              ' FROM GroundWaterSubCatchment WHERE' +
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (GroundWaterIdentifier =' + IntToStr(AGroundWaterID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile22DatabaseAgent.ReadGroundWaterPitmanSQL(AGroundWaterID: integer): string;
const OPNAME = 'TFile22DatabaseAgent.ReadGroundWaterPitmanSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' SoilMoistureCapacity, SoilMoistureStorageCapacity, SoilMoistureFlowState, ' +
              ' SoilMoistureFlowEquation, MaximumGroundWaterFlow, SoilMoistureRechargeEquation, GroundWaterFlow ' +
              ' FROM GroundWaterPitman WHERE' +
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (GroundWaterIdentifier   =' + IntTostr(AGroundWaterID)  +')';
   except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.ReadF22UnkownDataSQL: string;
const OPNAME = 'TFile22DatabaseAgent.ReadF22UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData,FileType'+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
 except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteGroundWaterSQL: string;
const OPNAME = 'TFile22DatabaseAgent.WriteGroundWaterSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO GroundWater '+
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, AquiferNodeNumber, ' +
              ' GroundWaterName, GroundWaterDescription, AquiferStorativity, AquiferStaticWaterlevel, ' +
              ' UnsaturatedStorageCapacity, InitialUnsaturatedStorage, MaximumAquiferRecharge, ' +
              ' MovingAverageRecharge, MaximumBaseFlowRate, HeadBaseFlowPower, MaximumHydrologicalGradient, ' +
              ' AquiferTransmissivity, BoreHoleDistanceToRiver, MaximumWaterAbstraction, ' +
              ' ParameterK2, ParameterK3, WaterEvaporationArea)' +
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,'+
              ' :AquiferNodeNumber, :GroundWaterName, :GroundWaterDescription, :AquiferStorativity, :AquiferStaticWaterlevel, ' +
              ' :UnsaturatedStorageCapacity, :InitialUnsaturatedStorage, :MaximumAquiferRecharge, ' +
              ' :MovingAverageRecharge, :MaximumBaseFlowRate, :HeadBaseFlowPower, :MaximumHydrologicalGradient, ' +
              ' :AquiferTransmissivity, :BoreHoleDistanceToRiver, :MaximumWaterAbstraction, ' +
              ' :ParameterK2, :ParameterK3, :WaterEvaporationArea ) ';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteGroundWaterPitmanSQL: string;
const OPNAME = 'TFile22DatabaseAgent.WriteGroundWaterPitmanSQL';
begin
  Result := '';
  try
    Result := ' INSERT INTO GroundWaterPitman '+
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' SoilMoistureCapacity, SoilMoistureStorageCapacity, SoilMoistureFlowState, ' +
              ' SoilMoistureFlowEquation, MaximumGroundWaterFlow, SoilMoistureRechargeEquation, ' +
              ' GroundWaterFlow)' +
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:GroundWaterIdentifier,'+
              ' :SoilMoistureCapacity, :SoilMoistureStorageCapacity, :SoilMoistureFlowState, ' +
              ' :SoilMoistureFlowEquation, :MaximumGroundWaterFlow, :SoilMoistureRechargeEquation, ' +
              ' :GroundWaterFlow)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteGroundWaterEvaporationSQL: string;
const OPNAME = 'TFile22DatabaseAgent.WriteGroundWaterEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GroundWaterEvaporation ' +
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Evaporation01, Evaporation02, Evaporation03, Evaporation04, ' +
              ' Evaporation05, Evaporation06, Evaporation07, Evaporation08, ' +
              ' Evaporation09, Evaporation10, Evaporation11, Evaporation12)  ' +
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:GroundWaterIdentifier,'+
              ' :Evaporation01, :Evaporation02, :Evaporation03, :Evaporation04, ' +
              ' :Evaporation05, :Evaporation06, :Evaporation07, :Evaporation08, ' +
              ' :Evaporation09, :Evaporation10, :Evaporation11, :Evaporation12  )';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteGroundWaterUsageFactorSQL: string;
const OPNAME = 'TFile22DatabaseAgent.WriteGroundWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GroundWaterUsageFactor ' +
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, ' +
              ' Factor07, Factor08, Factor09, Factor10, Factor11, Factor12) ' +
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,  :GroundWaterIdentifier,'+
              ' :Factor01, :Factor02, :Factor03, :Factor04, :Factor05, :Factor06, ' +
              ' :Factor07, :Factor08, :Factor09, :Factor10, :Factor11, :Factor12 )';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteGroundWaterSubCatchmentSQL: string;
const OPNAME = 'TFile22DatabaseAgent.WriteGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
     Result := ' INSERT INTO GroundWaterSubCatchment ' +
              ' (Model, StudyAreaName, SubArea, Scenario, GroundWaterIdentifier, ' +
              ' RefNodeNumber,AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,BaseFlowNodeNumber, ' +
              ' AquiferInflowChannelNr,AquferExcessChannelNr,' +
              ' GroundWaterBaseFlowChannelNr,RegulationFromAquiferChannelNr,'+
              ' RegulationFromBaseFlowChannelNr,DownstreamChannelNr,' +
              ' SurfaceRunOffChannelNr)' +
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:GroundWaterIdentifier,'+
              ' :RefNodeNumber,:AquiferNodeNumber,:AbstractionNodeNumber,:CollectionNodeNumber,:BaseFlowNodeNumber, ' +
              ' :AquiferInflowChannelNr,:AquferExcessChannelNr,' +
              ' :GroundWaterBaseFlowChannelNr,:RegulationFromAquiferChannelNr,'+
              ' :RegulationFromBaseFlowChannelNr,:DownstreamChannelNr,' +
              ' :SurfaceRunOffChannelNr)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteF22UnkownDataSQL: string;
const OPNAME = 'TFile22DatabaseAgent.WriteF22UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile22DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName             : string;
  LMessage               : string;
  LDataSet               : TAbstractModelDataset;
  LGroundWaterDataSet    : TAbstractModelDataset;
  LPitmanDataSet         : TAbstractModelDataset;
  LMonthlyValuesDataSet  : TAbstractModelDataset;
  LGroundWater           : TGroundWaterFileObject;
  LGroundWaterList       : TGroundWaterListFileObject;
  LIndex                 : integer;
  LGroundWaterIdentifier : integer;
  LStop                  : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile22DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LGroundWaterList := ADataObject.FGroundWaterListFileObject;
    if not LGroundWaterList.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LGroundWaterDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LPitmanDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyValuesDataSet);
    try
      LGroundWaterDataSet.SetSQL(ReadGroundWaterSQL);
      LGroundWaterDataSet.DataSet.Open;

      //Check if there is any data.
      if (LGroundWaterDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile22DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LGroundWaterDataSet.DataSet.Eof do
        begin
          LGroundWater := LGroundWaterList.AddGroundWaterObject;

          LGroundWaterIdentifier := LGroundWaterDataSet.DataSet.FieldByName('Identifier').AsInteger;

          //Read Line 1
          if not LGroundWaterDataSet.DataSet.FieldByName('AquiferNodeNumber').IsNull then
          begin
            LGroundWater.AquiferNodeNumber.FData := LGroundWaterDataSet.DataSet.FieldByName('AquiferNodeNumber').AsInteger;
            LGroundWater.AquiferNodeNumber.FInitalised := True;
          end;

          //Read Line 2
          if not LGroundWaterDataSet.DataSet.FieldByName('AquiferStorativity').IsNull then
          begin
            LGroundWater.AquiferStorativity.FData := LGroundWaterDataSet.DataSet.FieldByName('AquiferStorativity').AsFloat;
            LGroundWater.AquiferStorativity.FInitalised := True;
          end;

          if not LGroundWaterDataSet.DataSet.FieldByName('AquiferStaticWaterlevel').IsNull then
          begin
            LGroundWater.AquiferStaticWaterLevel.FData := LGroundWaterDataSet.DataSet.FieldByName('AquiferStaticWaterlevel').AsFloat;
            LGroundWater.AquiferStaticWaterLevel.FInitalised := True;
          end;

          //Read Line 3
          if not LGroundWaterDataSet.DataSet.FieldByName('UnsaturatedStorageCapacity').IsNull then
          begin
            LGroundWater.UnsaturatedStorageCapacity.FData := LGroundWaterDataSet.DataSet.FieldByName('UnsaturatedStorageCapacity').AsFloat;
            LGroundWater.UnsaturatedStorageCapacity.FInitalised := True;
          end;

          if not LGroundWaterDataSet.DataSet.FieldByName('InitialUnsaturatedStorage').IsNull then
          begin
            LGroundWater.InitialUnsaturatedStorage.FData := LGroundWaterDataSet.DataSet.FieldByName('InitialUnsaturatedStorage').AsFloat;
            LGroundWater.InitialUnsaturatedStorage.FInitalised := True;
          end;

          //Read Line 4
          if not LGroundWaterDataSet.DataSet.FieldByName('MaximumAquiferRecharge').IsNull then
          begin
            LGroundWater.MaximumDischargeRate.FData := LGroundWaterDataSet.DataSet.FieldByName('MaximumAquiferRecharge').AsFloat;
            LGroundWater.MaximumDischargeRate.FInitalised := True;
          end;

          if not LGroundWaterDataSet.DataSet.FieldByName('MovingAverageRecharge').IsNull then
          begin
            LGroundWater.MovingAverageRecharge.FData := LGroundWaterDataSet.DataSet.FieldByName('MovingAverageRecharge').AsFloat;
            LGroundWater.MovingAverageRecharge.FInitalised := True;
          end;

          //Read Line 5
          LPitmanDataSet.DataSet.Close;
          LPitmanDataSet.SetSQL(ReadGroundWaterPitmanSQL(LGroundWaterIdentifier));
          LPitmanDataSet.DataSet.Open;
          if not (LPitmanDataSet.DataSet.Eof and LPitmanDataSet.DataSet.Bof) then
          begin
            if not LPitmanDataSet.DataSet.FieldByName('SoilMoistureCapacity').IsNull then
            begin
              LGroundWater.PitmanSoilMoistureCapacity.FData := LPitmanDataSet.DataSet.FieldByName('SoilMoistureCapacity').AsFloat;
              LGroundWater.PitmanSoilMoistureCapacity.FInitalised := True;
            end;

            if not LPitmanDataSet.DataSet.FieldByName('SoilMoistureStorageCapacity').IsNull then
            begin
              LGroundWater.PitmanSoilMoistureStorageCapacity.FData := LPitmanDataSet.DataSet.FieldByName('SoilMoistureStorageCapacity').AsFloat;
              LGroundWater.PitmanSoilMoistureStorageCapacity.FInitalised := True;
            end;

            if not LPitmanDataSet.DataSet.FieldByName('SoilMoistureFlowState').IsNull then
            begin
              LGroundWater.PitmansoilMoistureFlowState.FData := LPitmanDataSet.DataSet.FieldByName('SoilMoistureFlowState').AsFloat;
              LGroundWater.PitmansoilMoistureFlowState.FInitalised := True;
            end;

            if not LPitmanDataSet.DataSet.FieldByName('SoilMoistureFlowEquation').IsNull then
            begin
              LGroundWater.PitmanSoilMoistureFlowEquation.FData := LPitmanDataSet.DataSet.FieldByName('SoilMoistureFlowEquation').AsFloat;
              LGroundWater.PitmanSoilMoistureFlowEquation.FInitalised := True;
            end;

            if not LPitmanDataSet.DataSet.FieldByName('MaximumGroundWaterFlow').IsNull then
            begin
              LGroundWater.PitmanMaximumGroundwaterFlow.FData := LPitmanDataSet.DataSet.FieldByName('MaximumGroundWaterFlow').AsFloat;
              LGroundWater.PitmanMaximumGroundwaterFlow.FInitalised := True;
            end;

            if not LPitmanDataSet.DataSet.FieldByName('SoilMoistureRechargeEquation').IsNull then
            begin
              LGroundWater.PitmanSoilMoistureRechargeEquation.FData := LPitmanDataSet.DataSet.FieldByName('SoilMoistureRechargeEquation').AsFloat;
              LGroundWater.PitmanSoilMoistureRechargeEquation.FInitalised := True;
            end;

            if not LPitmanDataSet.DataSet.FieldByName('GroundWaterFlow').IsNull then
            begin
              LGroundWater.PitmanGroundwaterFlow.FData := LPitmanDataSet.DataSet.FieldByName('GroundWaterFlow').AsFloat;
              LGroundWater.PitmanGroundwaterFlow.FInitalised := True;
            end;
          end;

          //Read Line 6

          if not LGroundWaterDataSet.DataSet.FieldByName('MaximumBaseFlowRate').IsNull then
          begin
            LGroundWater.MaximumRateOfGroundwaterBaseFlow.FData := LGroundWaterDataSet.DataSet.FieldByName('MaximumBaseFlowRate').AsFloat;
            LGroundWater.MaximumRateOfGroundwaterBaseFlow.FInitalised := True;
          end;

          if not LGroundWaterDataSet.DataSet.FieldByName('HeadBaseFlowPower').IsNull then
          begin
            LGroundWater.PowerHeadDifferenceBaseFlowEquation.FData := LGroundWaterDataSet.DataSet.FieldByName('HeadBaseFlowPower').AsFloat;
            LGroundWater.PowerHeadDifferenceBaseFlowEquation.FInitalised := True;
          end;

          //Read Line 7
          if not LGroundWaterDataSet.DataSet.FieldByName('MaximumHydrologicalGradient').IsNull then
          begin
            LGroundWater.MaximumHydrologicalGradient.FData := LGroundWaterDataSet.DataSet.FieldByName('MaximumHydrologicalGradient').AsFloat;
            LGroundWater.MaximumHydrologicalGradient.FInitalised := True;
          end;

          //Read Line 8
          if not LGroundWaterDataSet.DataSet.FieldByName('AquiferTransmissivity').IsNull then
          begin
            LGroundWater.AquiferTransmissivity.FData := LGroundWaterDataSet.DataSet.FieldByName('AquiferTransmissivity').AsFloat;
            LGroundWater.AquiferTransmissivity.FInitalised := True;
          end;

          //Read Line 9
          if not LGroundWaterDataSet.DataSet.FieldByName('BoreHoleDistanceToRiver').IsNull then
          begin
            LGroundWater.BoreHoleDistanceToRiver.FData := LGroundWaterDataSet.DataSet.FieldByName('BoreHoleDistanceToRiver').AsFloat;
            LGroundWater.BoreHoleDistanceToRiver.FInitalised := True;
          end;

          //Read Line 10
          if not LGroundWaterDataSet.DataSet.FieldByName('MaximumWaterAbstraction').IsNull then
          begin
            LGroundWater.MaximumGroundwaterAbstraction.FData := LGroundWaterDataSet.DataSet.FieldByName('MaximumWaterAbstraction').AsFloat;
            LGroundWater.MaximumGroundwaterAbstraction.FInitalised := True;
          end;

          if not LGroundWaterDataSet.DataSet.FieldByName('ParameterK2').IsNull then
          begin
            LGroundWater.ParameterK2.FData := LGroundWaterDataSet.DataSet.FieldByName('ParameterK2').AsFloat;
            LGroundWater.ParameterK2.FInitalised := True;
          end;

          if not LGroundWaterDataSet.DataSet.FieldByName('ParameterK3').IsNull then
          begin
            LGroundWater.ParameterK3.FData := LGroundWaterDataSet.DataSet.FieldByName('ParameterK3').AsFloat;
            LGroundWater.ParameterK3.FInitalised := True;
          end;

          //Read Line 11
          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(ReadGroundWaterEvaporationSQL(LGroundWaterIdentifier));
          LMonthlyValuesDataSet.DataSet.Open;
          if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
          begin
            for LIndex := 1 to 12 do
            begin
              LFieldName := Format('%s%2.2d',['Evaporation',LIndex]);
              if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LGroundWater.MonthlyWaterEvaporation[LIndex].FData := LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                LGroundWater.MonthlyWaterEvaporation[LIndex].FInitalised := True;
              end;
            end;
          end;

          //Read Line 12
          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(ReadGroundWaterUsageFactorSQL(LGroundWaterIdentifier));
          LMonthlyValuesDataSet.DataSet.Open;
          if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
          begin
            for LIndex := 1 to 12 do
            begin
              LFieldName := Format('%s%2.2d',['Factor',LIndex]);
              if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LGroundWater.MonthlyWaterUsageFactors[LIndex].FData := LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                LGroundWater.MonthlyWaterUsageFactors[LIndex].FInitalised := True;
              end;
            end;
          end;

          //Read Line 13
          if not LGroundWaterDataSet.DataSet.FieldByName('WaterEvaporationArea').IsNull then
          begin
            LGroundWater.GroundWaterEvaporationArea.FData := LGroundWaterDataSet.DataSet.FieldByName('WaterEvaporationArea').AsFloat;
            LGroundWater.GroundWaterEvaporationArea.FInitalised := True;
          end;
          LGroundWaterDataSet.DataSet.Next;
        end;
        LGroundWaterDataSet.DataSet.Close;


        //Line 14 on wards+++++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadF22UnkownDataSQL);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.DataSet.Open;

        while not LDataSet.DataSet.Eof do
        begin
          LGroundWaterList.Comment.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;

      Result :=  True;
      LMessage := FAppModules.Language.GetString('TFile22DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      LDataSet.Free;
      LGroundWaterDataSet.Free;
      LPitmanDataSet.Free;
      LMonthlyValuesDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile22DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName                 : string;
  LMessage                   : string;
  LGroundWaterName           : string;
  LStop                      : boolean;
  LDataSet                   : TAbstractModelDataset;
  LGroundWaterDataSet        : TAbstractModelDataset;
  LPitmanDataSet             : TAbstractModelDataset;
  LMonthlyValuesDataSet      : TAbstractModelDataset;
  LIndex                     : integer;
  LGroundWaterIndex          : integer;
  LGroundWaterIdentifier     : integer;
  LGroundWater               : TGroundWaterFileObject;
  LGroundWaterList           : TGroundWaterListFileObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile22DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LGroundWaterList := ADataObject.FGroundWaterListFileObject;
    if not Assigned(LGroundWaterList) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LGroundWaterDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LPitmanDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyValuesDataSet);
    try
      for LGroundWaterIndex := 0 to LGroundWaterList.GroundWaterObjectCount -1 do
      begin
        LGroundWater := LGroundWaterList.GroundWaterObjectByIndex[LGroundWaterIndex];
        LGroundWaterIdentifier := LGroundWaterIndex + 1;

        LGroundWaterDataSet.DataSet.Close;
        LGroundWaterDataSet.SetSQL(WriteGroundWaterSQL);
        LGroundWaterDataSet.ClearQueryParams();
        LGroundWaterDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LGroundWaterDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LGroundWaterDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LGroundWaterDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LGroundWaterDataSet.SetParams(['Identifier'], [IntToStr(LGroundWaterIdentifier)]);

        LGroundWaterName := 'Groundwater '+ IntToStr(LGroundWaterIdentifier);
        LGroundWaterDataSet.SetParams(['GroundWaterName'], [LGroundWaterName]);

        if LGroundWater.AquiferNodeNumber.FInitalised  then
          LGroundWaterDataSet.SetParams(['AquiferNodeNumber'], [IntToStr(LGroundWater.AquiferNodeNumber.FData)]);

        if LGroundWater.AquiferStorativity.FInitalised then
         LGroundWaterDataSet.SetParams(['AquiferStorativity'], [FloatToStr(LGroundWater.AquiferStorativity.FData)]);

        if LGroundWater.AquiferStaticWaterLevel.FInitalised then
         LGroundWaterDataSet.SetParams(['AquiferStaticWaterlevel'], [FloatToStr(LGroundWater.AquiferStaticWaterLevel.FData)]);

        if LGroundWater.UnsaturatedStorageCapacity.FInitalised then
         LGroundWaterDataSet.SetParams(['UnsaturatedStorageCapacity'], [FloatToStr(LGroundWater.UnsaturatedStorageCapacity.FData)]);

        if LGroundWater.InitialUnsaturatedStorage.FInitalised then
         LGroundWaterDataSet.SetParams(['InitialUnsaturatedStorage'], [FloatToStr(LGroundWater.InitialUnsaturatedStorage.FData)]);

        if LGroundWater.MaximumDischargeRate.FInitalised then
         LGroundWaterDataSet.SetParams(['MaximumAquiferRecharge'], [FloatToStr(LGroundWater.MaximumDischargeRate.FData)]);

        if LGroundWater.MovingAverageRecharge.FInitalised then
         LGroundWaterDataSet.SetParams(['MovingAverageRecharge'], [FloatToStr(LGroundWater.MovingAverageRecharge.FData)]);

        if LGroundWater.MaximumRateOfGroundwaterBaseFlow.FInitalised then
         LGroundWaterDataSet.SetParams(['MaximumBaseFlowRate'], [FloatToStr(LGroundWater.MaximumRateOfGroundwaterBaseFlow.FData)]);

        if LGroundWater.PowerHeadDifferenceBaseFlowEquation.FInitalised then
         LGroundWaterDataSet.SetParams(['HeadBaseFlowPower'], [FloatToStr(LGroundWater.PowerHeadDifferenceBaseFlowEquation.FData)]);

        if LGroundWater.MaximumHydrologicalGradient.FInitalised then
         LGroundWaterDataSet.SetParams(['MaximumHydrologicalGradient'], [FloatToStr(LGroundWater.MaximumHydrologicalGradient.FData)]);

        if LGroundWater.AquiferTransmissivity.FInitalised then
         LGroundWaterDataSet.SetParams(['AquiferTransmissivity'], [FloatToStr(LGroundWater.AquiferTransmissivity.FData)]);

        if LGroundWater.BoreHoleDistanceToRiver.FInitalised then
         LGroundWaterDataSet.SetParams(['BoreHoleDistanceToRiver'], [FloatToStr(LGroundWater.BoreHoleDistanceToRiver.FData)]);

        if LGroundWater.MaximumGroundwaterAbstraction.FInitalised then
         LGroundWaterDataSet.SetParams(['MaximumWaterAbstraction'], [FloatToStr(LGroundWater.MaximumGroundwaterAbstraction.FData)]);

        if LGroundWater.ParameterK2.FInitalised then
         LGroundWaterDataSet.SetParams(['ParameterK2'], [FloatToStr(LGroundWater.ParameterK2.FData)]);

        if LGroundWater.ParameterK3.FInitalised then
         LGroundWaterDataSet.SetParams(['ParameterK3'], [FloatToStr(LGroundWater.ParameterK3.FData)]);

        if LGroundWater.GroundWaterEvaporationArea.FInitalised then
         LGroundWaterDataSet.SetParams(['WaterEvaporationArea'], [FloatToStr(LGroundWater.GroundWaterEvaporationArea.FData)]);

        LGroundWaterDataSet.ExecSQL;

        LMonthlyValuesDataSet.DataSet.Close;
        LMonthlyValuesDataSet.SetSQL(WriteGroundWaterEvaporationSQL);
        LMonthlyValuesDataSet.ClearQueryParams();
        LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LMonthlyValuesDataSet.SetParams(['GroundWaterIdentifier'], [IntToStr(LGroundWaterIdentifier)]);

        for LIndex := 1 to 12 do
        begin
          if LGroundWater.MonthlyWaterEvaporation[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Evaporation',LIndex]);
            LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr(LGroundWater.MonthlyWaterEvaporation[LIndex].FData)]);
          end;
        end;
        LMonthlyValuesDataSet.ExecSQL;

        LMonthlyValuesDataSet.DataSet.Close;
        LMonthlyValuesDataSet.SetSQL(WriteGroundWaterUsageFactorSQL);
        LMonthlyValuesDataSet.ClearQueryParams();
        LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LMonthlyValuesDataSet.SetParams(['GroundWaterIdentifier'], [IntToStr(LGroundWaterIdentifier)]);

        for LIndex := 1 to 12 do
        begin
          if LGroundWater.MonthlyWaterUsageFactors[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Factor',LIndex]);
            LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr(LGroundWater.MonthlyWaterUsageFactors[LIndex].FData)]);
          end;
        end;
        LMonthlyValuesDataSet.ExecSQL;

        LPitmanDataSet.DataSet.Close;
        LPitmanDataSet.SetSQL(WriteGroundWaterPitmanSQL);
        LPitmanDataSet.ClearQueryParams();
        LPitmanDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LPitmanDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LPitmanDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LPitmanDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LPitmanDataSet.SetParams(['GroundWaterIdentifier'], [IntToStr(LGroundWaterIdentifier)]);

        if LGroundWater.PitmanSoilMoistureCapacity.FInitalised then
         LPitmanDataSet.SetParams(['SoilMoistureCapacity'], [FloatToStr(LGroundWater.PitmanSoilMoistureCapacity.FData)]);

        if LGroundWater.PitmanSoilMoistureStorageCapacity.FInitalised then
         LPitmanDataSet.SetParams(['SoilMoistureStorageCapacity'], [FloatToStr(LGroundWater.PitmanSoilMoistureStorageCapacity.FData)]);

        if LGroundWater.PitmansoilMoistureFlowState.FInitalised then
         LPitmanDataSet.SetParams(['SoilMoistureFlowState'], [FloatToStr(LGroundWater.PitmansoilMoistureFlowState.FData)]);

        if LGroundWater.PitmanSoilMoistureFlowEquation.FInitalised then
         LPitmanDataSet.SetParams(['SoilMoistureFlowEquation'], [FloatToStr(LGroundWater.PitmanSoilMoistureFlowEquation.FData)]);

        if LGroundWater.PitmanMaximumGroundwaterFlow.FInitalised then
         LPitmanDataSet.SetParams(['MaximumGroundWaterFlow'], [FloatToStr(LGroundWater.PitmanMaximumGroundwaterFlow.FData)]);

        if LGroundWater.PitmanSoilMoistureRechargeEquation.FInitalised then
         LPitmanDataSet.SetParams(['SoilMoistureRechargeEquation'], [FloatToStr(LGroundWater.PitmanSoilMoistureRechargeEquation.FData)]);

        if LGroundWater.PitmanGroundwaterFlow.FInitalised then
         LPitmanDataSet.SetParams(['GroundWaterFlow'], [FloatToStr(LGroundWater.PitmanGroundwaterFlow.FData)]);

        LPitmanDataSet.ExecSQL;
      end;

      //line 14++++++++++++++++++++++++++++
      for LIndex := 0 to LGroundWaterList.Comment.Count - 1 do
      begin
        LGroundWaterDataSet.DataSet.Close;
        LGroundWaterDataSet.SetSQL(WriteF22UnkownDataSQL);
        LGroundWaterDataSet.ClearQueryParams();
        LGroundWaterDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LGroundWaterDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LGroundWaterDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LGroundWaterDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LGroundWaterDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LGroundWaterDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LGroundWaterDataSet.SetParams(['LineNumber'], [IntToStr(1+ LIndex)]);
        LGroundWaterDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LGroundWaterDataSet.SetParams(['LineData'], [LGroundWaterList.Comment[LIndex]]);

        LGroundWaterDataSet.ExecSQL;
      end;
      LGroundWaterDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile22DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
      LGroundWaterDataSet.Free;
      LPitmanDataSet.Free;
      LMonthlyValuesDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
                                                       AQuetly: boolean = False): boolean;
const OPNAME = 'TFile22DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    LTableNames := 'GroundWater,GroundWaterEvaporation,GroundWaterPitman,GroundWaterUsageFactor';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
