//
//
//  UNIT      : Contains TIrrigationBlockSQLAgent Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 2006/07/12
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UIrrigationBlockSQLAgent;

interface

uses
  Classes,
  UIrrigationBlock,
  UAbstractObject;

type
  TIrrigationBlockSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function InsertIrrigationBlockAPanConvFactor(AIrrigationBlock    : TIrrigationBlock) : Boolean;
    function InsertIrrigationBlockPanEvaporation(AIrrigationBlock    : TIrrigationBlock) : Boolean;
    function InsertIrrigationBlockRainfallFactor(AIrrigationBlock    : TIrrigationBlock) : Boolean;
  public
    procedure LoadContextData_FeatureID (AContextData : TStringList; AFeatureID   : string);
    procedure LoadContextData_FeatureIDWaterUse(AContextData : TStringList; AFeatureID, ASubFeatureID : string);
    procedure LoadContextData_Factor(AContextData : TStringList; AFeatureID, AFieldNameID : string);
    procedure LoadContextData_FactorWaterUse(AContextData : TStringList; AFeatureID, ASubFeatureID, AFieldNameID : string);

    function GetMaxIdentifierSQL(ATableName: string)          : string;
    function GetMaxBlockNumberSQL                             : string;
    function GetMaxWaterUsageIDSQL(ABlockidentifier: Integer) : string;
    function GetMaxIdentifier(ATableName: string)             : Integer;
    function GetMaxBlockNumber                                : Integer;
    function GetMaxWaterUsageID(ABlockidentifier: Integer)    : Integer;
    function GetIdentifierByBlockNumberSQL(ABlockNumber: Integer) : string;
    function GetIdentifierByBlockNumber(ABlockNumber: Integer)    : Integer;

    function GetIrrigationBlockSQL : string;
    function GetIrrigationBlockAPanConvFactorSQL(ABlockIdentifier: Integer) : string;
    function GetIrrigationBlockPanEvaporationSQL(ABlockIdentifier: Integer) : string;
    function GetIrrigationBlockRainfallFactorSQL(ABlockIdentifier: Integer) : string;
    function GetIrrigationBlockWaterUsageFactorSQL(ABlockIdentifier: Integer) : string;
    function GetDiversionChannelsMaxDemandSQL(ABlockNumber: Integer) : string;

    function DeleteIrrigationBlockSQL(ABlockNumber: Integer) : string;
    function DeleteIrrigationBlockAPanConvFactorSQL(AIdentifier: Integer) : string;
    function DeleteIrrigationBlockPanEvaporationSQL(AIdentifier: Integer) : string;
    function DeleteIrrigationBlockRainfallFactorSQL(AIdentifier: Integer) : string;
    function DeleteIrrigationBlockWaterUsageFactorSQL(ABlockIdentifier: Integer) : string;
    function DeleteIrrigationBlockSingleWaterUsageFactorSQL(ABlockIdentifier, AIdentifier: Integer) : string;
    function DeleteDiversionChannelMaxDemandSQL(AIrrigationBlockNumber,AChannelNumber: Integer) : string;

    function InsertIrrigationBlockSQL : string;
    function InsertIrrigationBlockAPanConvFactorSQL   : string;
    function InsertIrrigationBlockPanEvaporationSQL   : string;
    function InsertIrrigationBlockRainfallFactorSQL   : string;
    function InsertIrrigationBlockWaterUsageFactorSQL : string;
    function InsertDiversionChannelMaxDemandSQLSQL    : string;

    function InsertIrrigationBlock(AIrrigationBlock    : TIrrigationBlock) :  Boolean;
    function InsertIrrigationBlockWaterUsageFactor(AWaterUsage:TWaterUsage) : Boolean;

    function DeleteIrrigationBlock(ABlockNumber: Integer) : Boolean;
    function DeleteIrrigationBlockMonthlyWaterUse(ABlockIdentifier, AIdentifier: Integer) : Boolean;

    function CopyIrrigationBlockFromScenario(ASourceStudyAreaName, ASourceSubArea, ASourceScenario: string;
             AIrrigationBlockList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
    function SaveDiversionChannelMaxDemand(AIrrigationBlockNumber,AChannelNumber : integer;
             AMaxDemands: TMonthlyDoubleArray; AInUse : boolean): Boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UErrorHandlingOperations, DB;

function TIrrigationBlockSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetScenarioWhereClause';
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

procedure TIrrigationBlockSQLAgent.LoadContextData_FeatureID (AContextData : TStringList;
                                                               AFeatureID   : string);
const OPNAME = 'TIrrigationBlockSQLAgent.LoadContextData_FeatureID';
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

procedure TIrrigationBlockSQLAgent.LoadContextData_Factor(AContextData : TStringList; AFeatureID, AFieldNameID : string);
const OPNAME = 'TIrrigationBlockSQLAgent.LoadContextData_Factor';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AFeatureID);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TIrrigationBlockSQLAgent.GetIrrigationBlockSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIrrigationBlockSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
    'Identifier, BlockNumber, '+
    'BlockName, MaxWaterAllocation, FileName, '+
    'NodeNumber, CanalTransportLoss, EfficiencyFactor, '+
    'ReturnFlowFactor, UpperZoneReturnFlow, LowerZoneReturnFlow, '+
    'ReturnFlowLoss, UpperZoneSoilMoistureCapacity, LowerZoneSoilMoistureCapacity, '+
    'UpperZoneSoilMoistureTarget, InitialSoilMoistureStorage, '+
    'RainAboveRainFactorSpecValue, RainBelowRainFactor, '+
    'RainCatchmentScalingFactor, AllocatedIrrigationArea, CropWaterUseType, '+
    'DroughtApplicable,DiversionChannelNumber,ReturnFlowChannelNumber,[Description]'+
    'FROM IrrigationBlock A WHERE '+
    GetScenarioWhereClause +
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetIrrigationBlockAPanConvFactorSQL(ABlockIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIrrigationBlockAPanConvFactorSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, '+
              'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, '+
              'Factor07, Factor08, Factor09, Factor10, Factor11, Factor12  '+
              'FROM IrrigationBlockAPanConvFactor A WHERE '+
    GetScenarioWhereClause + ' AND (A.Identifier = ' + IntToStr(ABlockIdentifier) + ') '+
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetIrrigationBlockPanEvaporationSQL(ABlockIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIrrigationBlockPanEvaporationSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, '+
              'Evaporation01, Evaporation02, Evaporation03, Evaporation04, Evaporation05, Evaporation06, '+
              'Evaporation07, Evaporation08, Evaporation09, Evaporation10, Evaporation11, Evaporation12  '+
              'FROM IrrigationBlockPanEvaporation A WHERE '+
    GetScenarioWhereClause + ' AND (A.Identifier = ' + IntToStr(ABlockIdentifier) + ') '+
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetIrrigationBlockRainfallFactorSQL(ABlockIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIrrigationBlockRainfallFactorSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, '+
              'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, '+
              'Factor07, Factor08, Factor09, Factor10, Factor11, Factor12  '+
              'FROM IrrigationBlockRainfallFactor A WHERE '+
    GetScenarioWhereClause + ' AND (A.Identifier = ' + IntToStr(ABlockIdentifier) + ') '+
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetIrrigationBlockWaterUsageFactorSQL(ABlockIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIrrigationBlockWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'SELECT [Model], [StudyAreaName], [SubArea], [Scenario], [BlockIdentifier], [Identifier], '+
              '[Factor01], [Factor02], [Factor03], [Factor04], [Factor05], [Factor06], '+
              '[Factor07], [Factor08], [Factor09], [Factor10], [Factor11], [Factor12], '+
              '[PercAreaUnderCropType], [CropName] '+
              'FROM IrrigationBlockWaterUsageFactor A WHERE '+
    GetScenarioWhereClause + ' AND (A.BlockIdentifier = ' + IntToStr(ABlockIdentifier) + ') '+
    ' ORDER BY A.BlockIdentifier, Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TIrrigationBlockSQLAgent.GetDiversionChannelsMaxDemandSQL(ABlockNumber: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetDiversionChannelsMaxDemandSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber,ChannelNumber, '+
              'Value01, Value02, Value03, Value04, Value05, Value06, '+
              'Value07, Value08, Value09, Value10, Value11, Value12, InUse  '+
              'FROM IrrigationBlockMaxDemand A WHERE '+
              GetScenarioWhereClause +
              ' AND A.BlockNumber = ' + IntToStr(ABlockNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockSQLAgent.LoadContextData_FactorWaterUse(AContextData: TStringList; AFeatureID, ASubFeatureID, AFieldNameID: string);
const OPNAME = 'TIrrigationBlockSQLAgent.LoadContextData_FactorWaterUse';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('BlockIdentifier='    + AFeatureID);
    AContextData.Add('Identifier='         + ASubFeatureID);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TIrrigationBlockSQLAgent.LoadContextData_FeatureIDWaterUse(AContextData: TStringList; AFeatureID, ASubFeatureID: string);
const OPNAME = 'TIrrigationBlockSQLAgent.LoadContextData_FeatureIDWaterUse';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('BlockIdentifier='    + AFeatureID);    
    AContextData.Add('Identifier='         + ASubFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockSQL(ABlockNumber: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlock A WHERE ' +
                GetScenarioWhereClause +
                'AND A.BlockNumber = '+ IntToStr(ABlockNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.DeleteDiversionChannelMaxDemandSQL(AIrrigationBlockNumber,AChannelNumber: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteDiversionChannelMaxDemandSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlockMaxDemand A WHERE ' +
                GetScenarioWhereClause +
                ' AND A.BlockNumber = '+ IntToStr(AIrrigationBlockNumber)+
                ' AND A.ChannelNumber = '+ IntToStr(AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockAPanConvFactorSQL(AIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockAPanConvFactorSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlockAPanConvFactor A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockPanEvaporationSQL(AIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockPanEvaporationSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlockPanEvaporation A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockRainfallFactorSQL(AIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockRainfallFactorSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlockRainfallFactor A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockWaterUsageFactorSQL(ABlockIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlockWaterUsageFactor A WHERE ' +
                GetScenarioWhereClause +
                'AND A.BlockIdentifier = '+ IntToStr(ABlockIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetMaxIdentifierSQL(ATableName: string): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetMaxIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM '+ ATableName +' A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockAPanConvFactorSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockAPanConvFactorSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO IrrigationBlockAPanConvFactor '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'Factor01, Factor02, Factor03, Factor04, '+
    'Factor05, Factor06, Factor07, Factor08, '+
    'Factor09, Factor10, Factor11, Factor12) '+
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, :AIdentifier, '+
    ':Factor01, :Factor02, :Factor03, :Factor04, '+
    ':Factor05, :Factor06, :Factor07, :Factor08, '+
    ':Factor09, :Factor10, :Factor11, :Factor12) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockPanEvaporationSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockPanEvaporationSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO IrrigationBlockPanEvaporation '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'Evaporation01, Evaporation02, Evaporation03, Evaporation04, '+
    'Evaporation05, Evaporation06, Evaporation07, Evaporation08, '+
    'Evaporation09, Evaporation10, Evaporation11, Evaporation12) '+
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, :AIdentifier, '+
    ':Evaporation01, :Evaporation02, :Evaporation03, :Evaporation04, '+
    ':Evaporation05, :Evaporation06, :Evaporation07, :Evaporation08, '+
    ':Evaporation09, :Evaporation10, :Evaporation11, :Evaporation12) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockRainfallFactorSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockRainfallFactorSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO IrrigationBlockRainfallFactor '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'Factor01, Factor02, Factor03, Factor04, '+
    'Factor05, Factor06, Factor07, Factor08, '+
    'Factor09, Factor10, Factor11, Factor12) '+
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, :AIdentifier, '+
    ':Factor01, :Factor02, :Factor03, :Factor04, '+
    ':Factor05, :Factor06, :Factor07, :Factor08, '+
    ':Factor09, :Factor10, :Factor11, :Factor12) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertDiversionChannelMaxDemandSQLSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertDiversionChannelMaxDemandSQLSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO IrrigationBlockMaxDemand '+
    '(Model, StudyAreaName, SubArea, Scenario, BlockNumber,ChannelNumber, ' +
    'Value01, Value02, Value03, Value04, '+
    'Value05, Value06, Value07, Value08, '+
    'Value09, Value10, Value11, Value12,InUse) '+
    'VALUES '+
    '(:Model, :StudyAreaName, :SubArea, :Scenario,:BlockNumber, :ChannelNumber, '+
    ':Value01, :Value02, :Value03, :Value04, '+
    ':Value05, :Value06, :Value07, :Value08, '+
    ':Value09, :Value10, :Value11, :Value12, :InUse) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO IrrigationBlock '+
      ' (Model, StudyAreaName, SubArea, Scenario, Identifier, '+
      ' BlockNumber, BlockName, MaxWaterAllocation, FileName, NodeNumber, CanalTransportLoss, '+
      ' EfficiencyFactor, ReturnFlowFactor, UpperZoneReturnFlow, LowerZoneReturnFlow, '+
      ' ReturnFlowLoss, UpperZoneSoilMoistureCapacity, LowerZoneSoilMoistureCapacity, '+
      ' UpperZoneSoilMoistureTarget, InitialSoilMoistureStorage, RainAboveRainFactorSpecValue, '+
      ' RainBelowRainFactor, RainCatchmentScalingFactor, AllocatedIrrigationArea, [Description], '+
      ' DiversionChannelNumber, ReturnFlowChannelNumber) '+
      ' VALUES ('+
      ' :Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, '+
      ' :BlockNumber, :BlockName, :MaxWaterAllocation, :FileName, :NodeNumber, :CanalTransportLoss, '+
      ' :EfficiencyFactor, :ReturnFlowFactor, :UpperZoneReturnFlow, :LowerZoneReturnFlow, '+
      ' :ReturnFlowLoss, :UpperZoneSoilMoistureCapacity, :LowerZoneSoilMoistureCapacity, '+
      ' :UpperZoneSoilMoistureTarget, :InitialSoilMoistureStorage, :RainAboveRainFactorSpecValue, '+
      ' :RainBelowRainFactor, :RainCatchmentScalingFactor, :AllocatedIrrigationArea, :IBDescription, '+
      ' :DiversionChannelNumber, :ReturnFlowChannelNumber) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockWaterUsageFactorSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO IrrigationBlockWaterUsageFactor '+
    '(Model, StudyAreaName, SubArea, Scenario, BlockIdentifier, Identifier, ' +
    'Factor01, Factor02, Factor03, Factor04, '+
    'Factor05, Factor06, Factor07, Factor08, '+
    'Factor09, Factor10, Factor11, Factor12, PercAreaUnderCropType, CropName) '+
    'VALUES '+
    '(:Model, :StudyAreaName, :SubArea, :Scenario, :BlockIdentifier, :Identifier, '+
    ' :Factor01, :Factor02, :Factor03, :Factor04, '+
    ' :Factor05, :Factor06, :Factor07, :Factor08, '+
    ' :Factor09, :Factor10, :Factor11, :Factor12, :PercAreaUnderCropType, :CropName) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetMaxIdentifier(ATableName: string): Integer;
const OPNAME = 'TIrrigationBlockSQLAgent.GetMaxIdentifier';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxIdentifierSQL(ATableName));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TIrrigationBlockSQLAgent.SaveDiversionChannelMaxDemand(AIrrigationBlockNumber,AChannelNumber: integer;
         AMaxDemands: TMonthlyDoubleArray; AInUse : boolean): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.SaveDiversionChannelMaxDemand';
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
          LDataSet.SetSQL(DeleteDiversionChannelMaxDemandSQL(AIrrigationBlockNumber,AChannelNumber));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertDiversionChannelMaxDemandSQLSQL);
          LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['BlockNumber'],    [IntToStr(AIrrigationBlockNumber)]);
          LDataSet.SetParams(['ChannelNumber'],    [IntToStr(AChannelNumber)]);

          LDataSet.SetParams(['Value01'],  [FloatToStr(AMaxDemands[1])]);
          LDataSet.SetParams(['Value02'],  [FloatToStr(AMaxDemands[2])]);
          LDataSet.SetParams(['Value03'],  [FloatToStr(AMaxDemands[3])]);
          LDataSet.SetParams(['Value04'],  [FloatToStr(AMaxDemands[4])]);
          LDataSet.SetParams(['Value05'],  [FloatToStr(AMaxDemands[5])]);
          LDataSet.SetParams(['Value06'],  [FloatToStr(AMaxDemands[6])]);
          LDataSet.SetParams(['Value07'],  [FloatToStr(AMaxDemands[7])]);
          LDataSet.SetParams(['Value08'],  [FloatToStr(AMaxDemands[8])]);
          LDataSet.SetParams(['Value09'],  [FloatToStr(AMaxDemands[9])]);
          LDataSet.SetParams(['Value10'],  [FloatToStr(AMaxDemands[10])]);
          LDataSet.SetParams(['Value11'],  [FloatToStr(AMaxDemands[11])]);
          LDataSet.SetParams(['Value12'],  [FloatToStr(AMaxDemands[12])]);
          if AInUse then
            LDataSet.SetParams(['InUse'],  ['1'])
          else
            LDataSet.SetParams(['InUse'],  ['0']);
          LDataset.ExecSQL;

          LDataset.DataSet.Close;
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
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlockSQLAgent.DeleteIrrigationBlock(ABlockNumber: Integer): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlock';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LIdentifier : Integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LIdentifier := GetIdentifierByBlockNumber(ABlockNumber);

          LDataSet.SetSQL(DeleteIrrigationBlockSQL(ABlockNumber));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteIrrigationBlockAPanConvFactorSQL(LIdentifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteIrrigationBlockPanEvaporationSQL(LIdentifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteIrrigationBlockRainfallFactorSQL(LIdentifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteIrrigationBlockWaterUsageFactorSQL(LIdentifier));
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

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockMonthlyWaterUse(ABlockIdentifier, AIdentifier: Integer): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockMonthlyWaterUse';
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
          LDataSet.SetSQL(DeleteIrrigationBlockSingleWaterUsageFactorSQL(ABlockIdentifier,AIdentifier));
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

function TIrrigationBlockSQLAgent.DeleteIrrigationBlockSingleWaterUsageFactorSQL(ABlockIdentifier, AIdentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.DeleteIrrigationBlockSingleWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationBlockWaterUsageFactor A WHERE ' +
                GetScenarioWhereClause +
                'AND A.BlockIdentifier = '+ IntToStr(ABlockIdentifier) +
                'AND A.Identifier = '+ IntToStr(AIdentifier);  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetMaxWaterUsageIDSQL(ABlockidentifier: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetMaxWaterUsageIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier '+
              'FROM IrrigationBlockWaterUsageFactor A '+
              'WHERE A.BlockIdentifier =' + IntToStr(ABlockidentifier) +' AND '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetMaxWaterUsageID(ABlockidentifier: Integer): Integer;
const OPNAME = 'TIrrigationBlockSQLAgent.GetMaxWaterUsageID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxWaterUsageIDSQL(ABlockidentifier));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlock(AIrrigationBlock    : TIrrigationBlock) :  Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlock';
var
  LDataSet        : TAbstractModelDataset;
  LImportDate     : TDateTime;
  LBlockID        : Integer;
  lBlockName      : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LBlockID       := GetMaxIdentifier('IrrigationBlock') + 1;
        lBlockName     := 'Irrigation Block (' + IntToStr(LBlockID)+ ')';
        AIrrigationBlock.PopulateIDs(LBlockID,lBlockName);
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertIrrigationBlockSQL);
          LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'],    [IntToStr(AIrrigationBlock.BlockIdentifier)]);

          LDataSet.SetParams(['BlockNumber'],        [IntToStr(AIrrigationBlock.BlockNodeNumber)]);
          LDataSet.SetParams(['BlockName'],          [AIrrigationBlock.BlockName]);
          LDataSet.SetParams(['MaxWaterAllocation'], [FloatToStr(AIrrigationBlock.MaxWaterAllocation)]);
          LDataSet.SetParams(['FileName'],           [AIrrigationBlock.FileName]);
          LDataSet.SetParams(['NodeNumber'],         [IntToStr(AIrrigationBlock.HydrologyNodeNumber)]);
          LDataSet.SetParams(['CanalTransportLoss'], [FloatToStr(AIrrigationBlock.CanalTransportLoss)]);

          LDataSet.SetParams(['EfficiencyFactor'],             [FloatToStr(AIrrigationBlock.EfficiencyFactor)]);
          LDataSet.SetParams(['ReturnFlowFactor'],             [FloatToStr(AIrrigationBlock.ReturnFlowFactor)]);
          LDataSet.SetParams(['UpperZoneReturnFlow'],          [FloatToStr(AIrrigationBlock.UpperZoneReturnFlow)]);
          LDataSet.SetParams(['LowerZoneReturnFlow'],          [FloatToStr(AIrrigationBlock.LowerZoneReturnFlow)]);
          LDataSet.SetParams(['ReturnFlowLoss'],               [FloatToStr(AIrrigationBlock.ReturnFlowLoss)]);
          LDataSet.SetParams(['UpperZoneSoilMoistureCapacity'],[FloatToStr(AIrrigationBlock.UpperZoneSoilMoistureCapacity)]);
          LDataSet.SetParams(['LowerZoneSoilMoistureCapacity'],[FloatToStr(AIrrigationBlock.LowerZoneSoilMoistureCapacity)]);
          LDataSet.SetParams(['UpperZoneSoilMoistureTarget'],  [FloatToStr(AIrrigationBlock.UpperZoneSoilMoistureTarget)]);
          LDataSet.SetParams(['InitialSoilMoistureStorage'],   [FloatToStr(AIrrigationBlock.InitialSoilMoistureStorage)]);
          LDataSet.SetParams(['RainAboveRainFactorSpecValue'], [FloatToStr(AIrrigationBlock.RainAboveRainFactorSpecValue)]);
          LDataSet.SetParams(['RainBelowRainFactor'],          [FloatToStr(AIrrigationBlock.RainBelowRainFactor)]);
          LDataSet.SetParams(['RainCatchmentScalingFactor'],   [FloatToStr(AIrrigationBlock.RainCatchmentScalingFactor)]);
          LDataSet.SetParams(['AllocatedIrrigationArea'],      [FloatToStr(AIrrigationBlock.AllocatedIrrigationArea)]);
          LDataSet.SetParams(['IBDescription'],                [AIrrigationBlock.BlockDescription]);
          LDataSet.SetParams(['DiversionChannelNumber'],       [IntToStr(AIrrigationBlock.DiversionChannelNr)]);
          LDataSet.SetParams(['ReturnFlowChannelNumber'],      [IntToStr(AIrrigationBlock.ReturnFlowChannelNr)]);
          LDataSet.SetParams(['CropWaterUseType'],             [IntToStr(AIrrigationBlock.CropWaterUseType)]);
          LDataSet.SetParams(['DroughtApplicable'],            [IntToStr(AIrrigationBlock.DroughtApplicable)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          InsertIrrigationBlockAPanConvFactor(AIrrigationBlock);
          InsertIrrigationBlockPanEvaporation(AIrrigationBlock);
          InsertIrrigationBlockRainfallFactor(AIrrigationBlock);

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

function TIrrigationBlockSQLAgent.InsertIrrigationBlockAPanConvFactor(AIrrigationBlock    : TIrrigationBlock): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockAPanConvFactor';
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
        LDataSet.SetSQL(InsertIrrigationBlockAPanConvFactorSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);

        LDataSet.SetParams(['AIdentifier'],[IntToStr(AIrrigationBlock.BlockIdentifier)]);
        LDataSet.SetParams(['Factor01'],  [FloatToStr(AIrrigationBlock.APanConvFactor[1])]);
        LDataSet.SetParams(['Factor02'],  [FloatToStr(AIrrigationBlock.APanConvFactor[2])]);
        LDataSet.SetParams(['Factor03'],  [FloatToStr(AIrrigationBlock.APanConvFactor[3])]);
        LDataSet.SetParams(['Factor04'],  [FloatToStr(AIrrigationBlock.APanConvFactor[4])]);
        LDataSet.SetParams(['Factor05'],  [FloatToStr(AIrrigationBlock.APanConvFactor[5])]);
        LDataSet.SetParams(['Factor06'],  [FloatToStr(AIrrigationBlock.APanConvFactor[6])]);
        LDataSet.SetParams(['Factor07'],  [FloatToStr(AIrrigationBlock.APanConvFactor[7])]);
        LDataSet.SetParams(['Factor08'],  [FloatToStr(AIrrigationBlock.APanConvFactor[8])]);
        LDataSet.SetParams(['Factor09'],  [FloatToStr(AIrrigationBlock.APanConvFactor[9])]);
        LDataSet.SetParams(['Factor10'],  [FloatToStr(AIrrigationBlock.APanConvFactor[10])]);
        LDataSet.SetParams(['Factor11'],  [FloatToStr(AIrrigationBlock.APanConvFactor[11])]);
        LDataSet.SetParams(['Factor12'],  [FloatToStr(AIrrigationBlock.APanConvFactor[12])]);

        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          Result := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockPanEvaporation(AIrrigationBlock    : TIrrigationBlock): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockPanEvaporation';
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
        LDataSet.SetSQL(InsertIrrigationBlockPanEvaporationSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);

        LDataSet.SetParams(['AIdentifier'],[IntToStr(AIrrigationBlock.BlockIdentifier)]);
        LDataSet.SetParams(['Evaporation01'],  [FloatToStr(AIrrigationBlock.PanEvaporation[1])]);
        LDataSet.SetParams(['Evaporation02'],  [FloatToStr(AIrrigationBlock.PanEvaporation[2])]);
        LDataSet.SetParams(['Evaporation03'],  [FloatToStr(AIrrigationBlock.PanEvaporation[3])]);
        LDataSet.SetParams(['Evaporation04'],  [FloatToStr(AIrrigationBlock.PanEvaporation[4])]);
        LDataSet.SetParams(['Evaporation05'],  [FloatToStr(AIrrigationBlock.PanEvaporation[5])]);
        LDataSet.SetParams(['Evaporation06'],  [FloatToStr(AIrrigationBlock.PanEvaporation[6])]);
        LDataSet.SetParams(['Evaporation07'],  [FloatToStr(AIrrigationBlock.PanEvaporation[7])]);
        LDataSet.SetParams(['Evaporation08'],  [FloatToStr(AIrrigationBlock.PanEvaporation[8])]);
        LDataSet.SetParams(['Evaporation09'],  [FloatToStr(AIrrigationBlock.PanEvaporation[9])]);
        LDataSet.SetParams(['Evaporation10'],  [FloatToStr(AIrrigationBlock.PanEvaporation[10])]);
        LDataSet.SetParams(['Evaporation11'],  [FloatToStr(AIrrigationBlock.PanEvaporation[11])]);
        LDataSet.SetParams(['Evaporation12'],  [FloatToStr(AIrrigationBlock.PanEvaporation[12])]);

        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          Result := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockRainfallFactor(AIrrigationBlock    : TIrrigationBlock): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockRainfallFactor';
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
        LDataSet.SetSQL(InsertIrrigationBlockRainfallFactorSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);

        LDataSet.SetParams(['AIdentifier'],[IntToStr(AIrrigationBlock.BlockIdentifier)]);
        LDataSet.SetParams(['Factor01'],  [FloatToStr(AIrrigationBlock.RainfallFactor[1])]);
        LDataSet.SetParams(['Factor02'],  [FloatToStr(AIrrigationBlock.RainfallFactor[2])]);
        LDataSet.SetParams(['Factor03'],  [FloatToStr(AIrrigationBlock.RainfallFactor[3])]);
        LDataSet.SetParams(['Factor04'],  [FloatToStr(AIrrigationBlock.RainfallFactor[4])]);
        LDataSet.SetParams(['Factor05'],  [FloatToStr(AIrrigationBlock.RainfallFactor[5])]);
        LDataSet.SetParams(['Factor06'],  [FloatToStr(AIrrigationBlock.RainfallFactor[6])]);
        LDataSet.SetParams(['Factor07'],  [FloatToStr(AIrrigationBlock.RainfallFactor[7])]);
        LDataSet.SetParams(['Factor08'],  [FloatToStr(AIrrigationBlock.RainfallFactor[8])]);
        LDataSet.SetParams(['Factor09'],  [FloatToStr(AIrrigationBlock.RainfallFactor[9])]);
        LDataSet.SetParams(['Factor10'],  [FloatToStr(AIrrigationBlock.RainfallFactor[10])]);
        LDataSet.SetParams(['Factor11'],  [FloatToStr(AIrrigationBlock.RainfallFactor[11])]);
        LDataSet.SetParams(['Factor12'],  [FloatToStr(AIrrigationBlock.RainfallFactor[12])]);

        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          Result := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TIrrigationBlockSQLAgent.InsertIrrigationBlockWaterUsageFactor(AWaterUsage:TWaterUsage): Boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.InsertIrrigationBlockWaterUsageFactor';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lNewIdentifier      : Integer;
  LCropName           : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lNewIdentifier  := GetMaxWaterUsageID(AWaterUsage.BlockIdentifier) + 1;
        LCropName    := 'Crop Name ' + IntToStr(AWaterUsage.BlockIdentifier)+ '-' + IntToStr(lNewIdentifier);
        AWaterUsage.PopulateIDs(lNewIdentifier,LCropName);

        LDataSet.SetSQL(InsertIrrigationBlockWaterUsageFactorSQL);
        LDataSet.SetParams(['Model'],          [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'],  [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'],        [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'],       [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['BlockIdentifier'],[IntToStr(AWaterUsage.BlockIdentifier)]);
        LDataSet.SetParams(['Identifier'],     [IntToStr(AWaterUsage.Identifier)]);
        LDataSet.SetParams(['CropName'],[AWaterUsage.CropName]);
        LDataSet.SetParams(['PercAreaUnderCropType'],[FloatToStr(AWaterUsage.PercAreaUnderCropType)]);
        LDataSet.SetParams(['Factor01'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[1])]);
        LDataSet.SetParams(['Factor02'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[2])]);
        LDataSet.SetParams(['Factor03'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[3])]);
        LDataSet.SetParams(['Factor04'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[4])]);
        LDataSet.SetParams(['Factor05'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[5])]);
        LDataSet.SetParams(['Factor06'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[6])]);
        LDataSet.SetParams(['Factor07'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[7])]);
        LDataSet.SetParams(['Factor08'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[8])]);
        LDataSet.SetParams(['Factor09'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[9])]);
        LDataSet.SetParams(['Factor10'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[10])]);
        LDataSet.SetParams(['Factor11'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[11])]);
        LDataSet.SetParams(['Factor12'],  [FloatToStr(AWaterUsage.MonthlyWaterUse[12])]);
        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          Result := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TIrrigationBlockSQLAgent.GetMaxBlockNumber: Integer;
const OPNAME = 'TIrrigationBlockSQLAgent.GetMaxBlockNumber';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxBlockNumberSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxBlockNumber').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetMaxBlockNumberSQL: string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetMaxBlockNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(BlockNumber) AS MaxBlockNumber FROM IrrigationBlock A WHERE' + GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetIdentifierByBlockNumber(ABlockNumber: Integer): Integer;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIdentifierByBlockNumber';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetIdentifierByBlockNumberSQL(ABlockNumber));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.GetIdentifierByBlockNumberSQL(ABlockNumber: Integer): string;
const OPNAME = 'TIrrigationBlockSQLAgent.GetIdentifierByBlockNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier FROM IrrigationBlock A WHERE' +
    GetScenarioWhereClause +
    ' AND (A.BlockNumber = '+IntToStr(ABlockNumber)+ ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockSQLAgent.CopyIrrigationBlockFromScenario(ASourceStudyAreaName, ASourceSubArea, ASourceScenario: string;
                                  AIrrigationBlockList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIrrigationBlockSQLAgent.CopyIrrigationBlockFromScenario';
      ChannelDetailsSQL = 'SELECT * FROM ChannelDetails WHERE ';
      IrrigationBlockSQL = 'SELECT * FROM IrrigationBlock  WHERE ';
      IrrigationBlockAPanConvFactorSQL = 'SELECT * FROM IrrigationBlockAPanConvFactor WHERE ';
      IrrigationBlockPanEvaporationSQL = 'SELECT * FROM IrrigationBlockPanEvaporation WHERE ';
      IrrigationBlockRainfallFactorSQL = 'SELECT * FROM IrrigationBlockRainfallFactor WHERE ';
      IrrigationBlockWaterUsageFactorSQL = 'SELECT * FROM IrrigationBlockWaterUsageFactor WHERE ';
var
  LSourceDataSet                   : TAbstractModelDataset;
  LDestinationDataSet              : TAbstractModelDataset;
  LChannelSourceDataSet            : TAbstractModelDataset;
  LChannelDestinationDataSet       : TAbstractModelDataset;
  LCurrentBlockNumber              : integer;
  LCurrentIdetifier                : integer;
  LNewBlockNumber                  : integer;
  LNewBlockIdentifier              : integer;
  LOldDiversionChannel             : integer;
  LOldReturnFlowChannel            : integer;
  LNewDiversionChannel             : integer;
  LNewReturnFlowChannel            : integer;
  LMessage                         : string;
  LBlockName                       : string;
  LModel                           : string;
  LFieldName                       : string;
  LDestinationStudyAreaName        : string;
  LDestinationSubArea              : string;
  LDestinationScenario             : string;
  LSourceWhereClause               : string;
  LDestinationWhereClause          : string;
  LSourceSQL                       : string;
  LDestinationSQL                  : string;
  LStop                            : boolean;
  LIrrigationBlockIndex            : integer;
  LIndex                           : integer;
  LImportDate                      : TDateTime;
  LChannelDataSQLAgent             : TChannelDataSQLAgent;
  LReservoirDataSQLAgent           : TReservoirDataSQLAgent;
  LNewChannelID                    : integer;
  LNewIrrigationNodeNumber         : integer;
begin
  Result := False;
  try
    if not Assigned(AIrrigationBlockList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
    LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
    LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      if Assigned(LSourceDataSet) and Assigned(LDestinationDataSet) and Assigned(LChannelSourceDataSet)
        and Assigned(LChannelDestinationDataSet)then
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
        try
          FAppModules.Database.StartTransaction;
          LNewBlockNumber := GetMaxBlockNumber;
          LNewBlockIdentifier := GetIdentifierByBlockNumber(LNewBlockNumber);
          LNewDiversionChannel := 0;
          LNewReturnFlowChannel := 0;
          LNewIrrigationNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;
          LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                                 AIrrigationBlockList,AProgressUpdateFuntion);
          for LIrrigationBlockIndex := 0 to AIrrigationBlockList.Count-1 do
          begin
            LCurrentBlockNumber := integer(AIrrigationBlockList.Objects[LIrrigationBlockIndex]);
            LBlockName := AIrrigationBlockList[LIrrigationBlockIndex];
            LNewIrrigationNodeNumber := LNewIrrigationNodeNumber + 1;
            LNewBlockIdentifier := LNewBlockIdentifier + 1;
            LMessage := 'Copying Irrigation Block ('+LBlockName+') ' + IntToStr(LIrrigationBlockIndex+1) + ' of '+ IntToStr(AIrrigationBlockList.Count);
            AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
            //________________________________________________________IrrigationBlock____________________________
            LSourceSQL := IrrigationBlockSQL + LSourceWhereClause + ' AND BlockNumber = '+ IntToStr(LCurrentBlockNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;

            LDestinationSQL := IrrigationBlockSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table IrrigationBlock cannot be set to updatable.');
            end
            else
            begin
              LOldDiversionChannel := LSourceDataSet.DataSet.FieldByName('DiversionChannelNumber').AsInteger;
              LOldReturnFlowChannel := LSourceDataSet.DataSet.FieldByName('ReturnFlowChannelNumber').AsInteger;
              LCurrentIdetifier := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LDestinationDataSet.DataSet.Append;
              for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
              begin
                LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
              end;

              if (LOldDiversionChannel > 0) and (LOldReturnFlowChannel > 0) then
              begin
                //________________________________________________________ ChannelDetails ____________________________
                LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldDiversionChannel);
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
                    LNewDiversionChannel := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                    LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewDiversionChannel;
                    LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                    LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := LNewIrrigationNodeNumber;
                    LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := 0;
                    LChannelDestinationDataSet.DataSet.Post;
                  end;
                end;  
                //________________________________________________________ ChannelDetails ____________________________
                LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldReturnFlowChannel);
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
                    LNewReturnFlowChannel := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                    LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewReturnFlowChannel;
                    LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                    LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                    LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := LNewIrrigationNodeNumber;

                    LChannelDestinationDataSet.DataSet.Post;
                  end;
                end;  
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('NodeNumber').AsInteger               := 0;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewBlockIdentifier;
                LDestinationDataSet.DataSet.FieldByName('DiversionChannelNumber').AsInteger   := LNewDiversionChannel;
                LDestinationDataSet.DataSet.FieldByName('ReturnFlowChannelNumber').AsInteger  := LNewReturnFlowChannel;
                LDestinationDataSet.DataSet.FieldByName('BlockNumber').AsInteger              := LNewIrrigationNodeNumber;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            
            //________________________________________________________ IrrigationBlockAPanConvFactor ____________________________
            LSourceSQL := IrrigationBlockAPanConvFactorSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            while not LSourceDataSet.DataSet.Eof do
            begin
              LDestinationSQL := IrrigationBlockAPanConvFactorSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table IrrigationBlockAPanConvFactor cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewBlockIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
              LSourceDataSet.DataSet.Next;
            end;
            //________________________________________________________ IrrigationBlockPanEvaporation ____________________________
            LSourceSQL := IrrigationBlockPanEvaporationSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            while not LSourceDataSet.DataSet.Eof do
            begin
              LDestinationSQL := IrrigationBlockPanEvaporationSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table IrrigationBlockPanEvaporation cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewBlockIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
              LSourceDataSet.DataSet.Next;
            end;

            //________________________________________________________ IrrigationBlockRainfallFactor ____________________________
            LSourceSQL := IrrigationBlockRainfallFactorSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            while not LSourceDataSet.DataSet.Eof do
            begin
              LDestinationSQL := IrrigationBlockRainfallFactorSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table IrrigationBlockRainfallFactor cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewBlockIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
              LSourceDataSet.DataSet.Next;
            end;

            //________________________________________________________ IrrigationBlockWaterUsageFactor ____________________________
            LSourceSQL := IrrigationBlockWaterUsageFactorSQL + LSourceWhereClause + ' AND BlockIdentifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            while not LSourceDataSet.DataSet.Eof do
            begin
              LDestinationSQL := IrrigationBlockWaterUsageFactorSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table IrrigationBlockWaterUsageFactor cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('BlockIdentifier').AsInteger          := LNewBlockIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
              LSourceDataSet.DataSet.Next;
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
      LSourceDataSet.Free;
      LDestinationDataSet.Free;
      LChannelSourceDataSet.Free;
      LChannelDestinationDataSet.Free;
      LReservoirDataSQLAgent.Free;
      LChannelDataSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.




