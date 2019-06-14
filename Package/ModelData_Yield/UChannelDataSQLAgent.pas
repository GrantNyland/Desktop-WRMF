//
//
//  UNIT      : Contains TChannelDataSQLAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/08/29
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UChannelDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TChannelDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetScenarioWhereClauseB: string;

    function GetChannelCount: integer;
    function GetChannelPenaltyCount: integer;
    function GetMaxChannelPenaltyIdentifier: integer;

    function GetMaxPumpingFeatureID : integer;

    function GetMaxLossFeatureID : integer;
    function GetMaxMinimumFlowConstraintID : integer;
    function GetMaxSpecifiedDemandFeatureID : integer;

    function GetMaxMinMaxFlowConstraintFlowIDSQL : string;
    function GetMaxLossFeatureValueIDSQL : string;
    function GetMaxMinimumFlowConstraintValueIDSQL : string;
    function GetMaxDiversionFeatureID : integer;
    function GetMaxDiversionFeatureIDSQL : string;
    function GetMaxIFRFeatureID : integer;
    function GetMaxIFRFeatureIDSQL : string;
    function GetMaxWaterDemandFeatureIDSQL : string;
    function GetMaxWaterDemandCategoryIDSQL : string;
    function GetMaxWaterDemandCategoryID : integer;
    function GetMaxWaterDemandFeatureID : integer;
  public
    function GetMaxMinMaxFlowConstraintID : integer;
    function GetMaxChannelIdentifier: integer;
    function GetMaxChannelNumber: integer;
    function GetInterBasinSupportChannelNumbersCommaText: string;
    procedure LoadChannelDetailsContextData(AContextData: TStringList;ARecordIdentifier, AChannelNumber,AChannelType: string);
    procedure LoadChannelConfigContextData(AContextData: TStringList);

    procedure LoadChannelContextData (AContextData   : TStringList;
                                      AChannelID     : string;
                                      AChannelNumber : string);
    function MinMaxArcRecordFoundSQL(AFeatureID, AArc : integer) : string;
    function MinMaxArcRecordFound(AFeatureID, AArc : integer) : boolean;
    procedure LoadWaterSupplyDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
    procedure LoadContextData_FeatureIDSubIDFieldNameID (AContextData : TStringList;
                                                         AFeatureID   : string;
                                                         ASubID       : string;
                                                         AFieldNameID : string);
    procedure LoadContextData_FeatureIDFieldNameID (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
    procedure LoadContextData_FeatureID(AContextData : TStringList;
                                        AFeatureID   : string);
    procedure LoadChannelPenaltyValueContextData(AContextData: TStringList;
              APenaltyNumber,AFieldNameIdentifier: string);
    procedure LoadChannelPenaltyContextData (AContextData : TStringList;
                                             APenaltyID   : string);
    procedure LoadContextData (AContextData : TStringList);
    function GetChannelPenaltyStructureDataSQL: string;
    function GetChannelInflowPenaltyNoSQL : string;
    function GetChannelInflowPenaltyNoCountSQL : string;
    function InsertChannelInflowPenaltyNoRecordSQL : string;

    function GetChannelPenaltyStructureDataByIDSQL(AFeatureID : integer): string;
    function GetGeneralFlowChannelDataSQL: string;
    function GetMasterControlChannelDataSQL: string;
    function GetDemandChannelCentreSQL(AChanneNr : integer) : string;

    function GetChannelsSpecifiedForSummarySQL: string;

    function GetSpecifiedDemandChannelDataSQL: string;
    function GetMaxSpecifiedDemandFeatureIDSQL : string;
    function InsertSpecifiedDemandFeatureSQL (AFeatureID : integer) : string;
    function DeleteSpecifiedDemandFeatureSQL (AFeatureID : integer) : string;

    function GetMinimumFlowChannelDataSQL: string;
    function GetMaxMinimumFlowConstraintIDSQL : string;

    function GetMaxSummaryChannelSql:string;
    function GetMaxSummaryChannelID : string;


    function InsertMinimumFlowConstraintSQL (AFeatureID : integer) : string;

    function InsertSummaryChannelsSQL(AChannelNumber : integer) : string;
    function DeleteSummaryOutputSQL(AChannelNumber : integer) : string;

    function InsertMinimumFlowConstraintValueSQL (AFeatureID : integer) : string;
    function DeleteMinimumFlowConstraintSQL (AFeatureID : integer) : string;
    function DeleteMinimumFlowConstraintValueSQL (AFeatureID : integer) : string;

    function GetMaxMinMaxFlowConstraintIDSQL : string;
    function GetMinMaxChannelDataSQL: string;
    function GetMinMaxChannelValuesSQL (AFeatureID : integer) : string;
    function GetMinMaxChannelDistributionValuesSQL(AFeatureID : integer) : string;
    function InsertMinMaxFlowConstraintSQL (AFeatureID : integer) : string;
    function DeleteMinMaxFlowConstraintSQL (AFeatureID : integer) : string;
    function DeleteMinMaxFlowConstraintValueSQL (AFeatureID : integer) : string;

    function GetMaxPumpingFeatureIDSQL : string;
    function InsertPumpingFeatureSQL (AFeatureID : integer) : string;
    function DeletePumpingFeatureSQL (AFeatureID : integer) : string;


    function GetMaxLossFeatureIDSQL : string;
    function GetLossChannelDataSQL: string;
    function GetLossChannelValuesSQL (AFeatureID : integer) : string;
    function InsertLossFeatureSQL (AFeatureID : integer) : string;
    function InsertLossFeatureValueSQL (AFeatureID : integer) : string;
    function DeleteLossFeatureSQL (AFeatureID : integer) : string;
    function DeleteLossFeatureValueSQL (AFeatureID : integer) : string;

    function GetDeleteOutputChannelDataSQL(AChannelNumber: string): string;
    function GetInsertOutputChannelDataSQL: string;

    function GetDeleteChannelPenaltySQL(AIdentifier: integer): string;
    function GetNewChannelPenaltySQL(AIdentifier: integer): string;
    function GetChannelPenaltyCountSQL: string;
    function GetMaxChannelPenaltyIdentifierSQL: string;
    function GetUpdateChannelArcCountSQL(APenaltyIdentifier,ANewCount: integer): string;

    function InsertChannelSQL (AChannelID : integer;
                               AChannelNr : integer): string;
    function DeleteChannelSQL (AChannelID : integer): string;
    function GetChannelCountSQL: string;
    function GetMaxChannelIdentifierSQL: string;
    function GetMaxChannelNumberSQL: string;

    function InsertMonthlyConstraintSQL : string;
    function InsertMinMaxChannelDistributionValuesSQL : string;
    function InsertMinMaxChannelDistributionRowValuesSQL : string;
    function DeleteMonthlyConstraintSQL (AFeatureID : integer;
                                         AIndex     : integer) : string;
    function DeleteMonthlyDistributionSQL (AFeatureID : integer;
                                           AIndex     : integer) : string;
    function DeleteAllMonthlyDistributionSQL (AFeatureID : integer) : string;

    function InsertChannelCommentsSQL : string;
    function DeleteChannelCommentsSQL : string;
    function SelectChannelCommentsSQL : string;

    function InsertChannelPenalty(var ACreatedPenaltyId: integer): boolean;
    function DeleteChannelPenalty(AChannelPenaltyNumber: integer): boolean;
    function InsertChannel(var AChannelID : integer;
                           var AChannelNr : integer): boolean;
    function DeleteChannel(AChannelID : integer): boolean;
    function UpdateChannelArcCount(APenaltyIdentifier,ANewCount: integer): boolean;

    function AddMonthlyConstraint (AFeatureID : integer;
                                   AIndex     : integer): boolean;
    function AddMinMaxChannelDistributionValues (AFeatureID : integer;
                                                 AIndex     : integer): boolean;
    function AddMinMaxChannelDistributionBlockValues(AFeatureID , ARowCount     : integer;
                                                      ADistributionByArcMonth : TMinMaxMonthlyDoublesArray): boolean;

    function DeleteMonthlyConstraint (AFeatureID : integer;
                                      AIndex     : integer): boolean;
    function DeleteMonthlyDistribution (AFeatureID : integer;
                                                        AIndex     : integer): boolean;

    function InsertLossFeature (var AFeatureID : integer): boolean;
    function DeleteLossFeature (AFeatureID : integer): boolean;

    function InsertMinMaxFlowConstraint (var AFeatureID : integer): boolean;
    function DeleteMinMaxFlowConstraint (AFeatureID : integer): boolean;

    function InsertPumpingFeature (var AFeatureID : integer): boolean;
    function DeletePumpingFeature (AFeatureID : integer): boolean;


    function InsertMinimumFlowConstraint (var AFeatureID : integer): boolean;
    function DeleteMinimumFlowConstraint (AFeatureID : integer): boolean;

    function InsertSpecifiedDemandFeature (var AFeatureID : integer): boolean;
    function DeleteSpecifiedDemandFeature (AFeatureID : integer): boolean;

    function InsertChannelComments : boolean;
    function DeleteChannelComments : boolean;

    function InsertSummaryOutput(AChannelNo : integer):boolean;
    function RemoveSummaryOutput(AChannelNo : integer):boolean;

    function CopyChannelFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AChannelNumberList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  end;
implementation

uses
  Math,
  SysUtils,
  Data.DB,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;


function TChannelDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TChannelDataSQLAgent.GetScenarioWhereClause';
var
  LModelCode : string;
begin
  Result := '';
  try
    if FAppModules.StudyArea.ModelCode = CDailyDiversion then
      LModelCode := CYield
    else
      LModelCode := FAppModules.StudyArea.ModelCode;
    Result :=
      ' (A.Model         = ' + QuotedStr(LModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetScenarioWhereClauseB: string;
const OPNAME = 'TChannelDataSQLAgent.GetScenarioWhereClauseB';
var
  LModelCode : string;
begin
  Result := '';
  try
    if FAppModules.StudyArea.ModelCode = CDailyDiversion then
      LModelCode := CYield
    else
      LModelCode := FAppModules.StudyArea.ModelCode;
    Result :=
      ' (ChannelDetails.Model         = ' + QuotedStr(LModelCode)     + ') AND ' +
      ' (ChannelDetails.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (ChannelDetails.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (ChannelDetails.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelPenaltyStructureDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelPenaltyStructureDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS PenaltyNumber, PenaltyName, ' +
              ' Penalty01, Penalty02, Penalty03, Penalty04, Penalty05 FROM ChannelArcPenalty A WHERE ' +
                GetScenarioWhereClause +
              ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertChannelInflowPenaltyNoRecordSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelInflowPenaltyNoCountSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO ChannelConfig'+
    '(Model, StudyAreaName, SubArea, Scenario, InflowPenaltyNo) values (:AModel, :AStudyAreaName, :ASubArea, :AScenario, :InflowPenaltyNo)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelInflowPenaltyNoCountSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelInflowPenaltyNoCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS RecordCount ' +
              ' FROM ChannelConfig A WHERE ' +
                GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.GetChannelInflowPenaltyNoSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelInflowPenaltyNoSQL';
begin
  Result := '';
  try
    Result := 'SELECT InflowPenaltyNo ' +
              ' FROM ChannelConfig A WHERE ' +
                GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelPenaltyStructureDataByIDSQL(AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelPenaltyStructureDataByIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT ArcCount FROM ChannelArcPenalty A WHERE ' +
                GetScenarioWhereClause +
              ' AND A.Identifier = ' + IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetGeneralFlowChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetGeneralFlowChannelDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'ChannelName, ChannelNumber, ChannelSubType, ChannelType, ' +
              'UpNodeNumber, DownNodeNumber, PenaltyNumber, SummaryOutput, FirmYieldCalc, FlowOutput, ' +
              'ChannelAreaID ' +
              'FROM ChannelDetails A WHERE ' +
               GetScenarioWhereClause + ' AND ' +
              '(A.ChannelType in (5,8,12,14,15,16,17,19,20,21,25,26,27,28,29,30,31,32,33,34,35,36)) ' +
              ' ORDER BY A.ChannelNumber ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetDeleteOutputChannelDataSQL(AChannelNumber: string): string;
const OPNAME = 'TChannelDataSQLAgent.GetDeleteOutputChannelDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ChannelDetails A WHERE ' +
               GetScenarioWhereClause + ' AND ' +
              '(A.ChannelType = 13) AND ' +
              '(A.ChannelNumber =' + AChannelNumber + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetInsertOutputChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetInsertOutputChannelDataSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO ChannelDetails'+
    '(Model, StudyAreaName, SubArea, Scenario, ChannelType, Identifier, ' +
    'ChannelName, ChannelNumber) '+
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, :AChannelType, ' +
    ':AIdentifier, :AChannelName, :AChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMasterControlChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMasterControlChannelDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Identifier AS RecordIdentifier, ' +
              'A.ChannelName,A.ChannelNumber,A.ChannelSubType,A.ChannelType, ' +
              'A.UpNodeNumber, A.DownNodeNumber, A.PenaltyNumber, A.SummaryOutput, A.FirmYieldCalc, A.Flowoutput, ' +
              'A.ChannelAreaID, ' +
              'B.Identifier, B.FeatureName, B.MasterControlType, ' +
              'B.Value01, B.Value02, B.Value03, B.Value04, B.Value05, B.Value06, ' +
              'B.Value07, B.Value08, B.Value09, B.Value10, B.Value11, B.Value12 ' +
              'FROM ChannelDetails A, MasterControlFeature B WHERE ' +
                GetScenarioWhereClause + ' AND ' +
              '(A.ChannelType = 2)  AND ' +
              '(B.Model             = A.Model) AND ' +
              '(B.StudyAreaName     = A.StudyAreaName) AND ' +
              '(B.SubArea           = A.SubArea) AND ' +
              '(B.Scenario          = A.Scenario) AND ' +
              '(B.ChannelNumber     = A.ChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetDemandChannelCentreSQL(AChanneNr: integer): string;
const OPNAME = 'TChannelDataSQLAgent.GetDemandChannelCentreSQL';
begin
  Result := '';
  try
    Result := 'SELECT * FROM DemandCentre A WHERE ' +
               GetScenarioWhereClause + ' AND ' +
              '(A.ChannelNumber =' + IntToStr(AChanneNr)+ ' )';

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.GetMinimumFlowChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMinimumFlowChannelDataSQL';
begin
  Result := '';
  try
    Result := '  SELECT ' +
               'ChannelDetails.Identifier AS RecordIdentifier, ChannelDetails.ChannelName, ' +
               'ChannelDetails.ChannelNumber, ChannelDetails.ChannelSubType, ' +
               'ChannelDetails.ChannelType, ChannelDetails.UpNodeNumber, ' +
               'ChannelDetails.DownNodeNumber, ChannelDetails.PenaltyNumber, ' +
               'ChannelDetails.SummaryOutput, ChannelDetails.FirmYieldCalc, ChannelDetails.Flowoutput, ChannelDetails.ChannelAreaID, ' +
               'MinFlowChannel.MinFlowChannelNumber, ' +
               'MinFlowChannel.MinFlowChannelName, MinFlowChannelValue.Identifier AS MinFlowRecordIdentifier, ' +
               'MinFlowChannelValue.Value01, MinFlowChannelValue.Value02, MinFlowChannelValue.Value03, ' +
               'MinFlowChannelValue.Value04, MinFlowChannelValue.Value05, MinFlowChannelValue.Value06, ' +
               'MinFlowChannelValue.Value07, MinFlowChannelValue.Value08, MinFlowChannelValue.Value09, ' +
               'MinFlowChannelValue.Value10, MinFlowChannelValue.Value11, MinFlowChannelValue.Value12 ' +
               ' FROM  ' +
               '  (ChannelDetails  LEFT JOIN MinFlowChannel ON ' +
               '  (ChannelDetails.Model = MinFlowChannel.Model) AND ' +
               '  (ChannelDetails.StudyAreaName = MinFlowChannel.StudyAreaName) AND ' +
               '  (ChannelDetails.SubArea = MinFlowChannel.SubArea) AND ' +
               '  (ChannelDetails.Scenario = MinFlowChannel.Scenario) AND ' +
               '  (ChannelDetails.ChannelNumber = MinFlowChannel.MinFlowChannelNumber)) ' +
               '  LEFT JOIN MinFlowChannelValue ON  ' +
               '  (MinFlowChannel.Model = MinFlowChannelValue.Model) AND ' +
               '  (MinFlowChannel.StudyAreaName = MinFlowChannelValue.StudyAreaName) AND  ' +
               '  (MinFlowChannel.SubArea = MinFlowChannelValue.SubArea) AND ' +
               '  (MinFlowChannel.Scenario = MinFlowChannelValue.Scenario) AND ' +
               '  (MinFlowChannel.Identifier = MinFlowChannelValue.Identifier) ' +
               ' WHERE ' +
                 GetScenarioWhereClauseB + ' AND ' +
               '   (ChannelDetails.ChannelType =6) ' +
               ' ORDER BY ChannelNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelsSpecifiedForSummarySQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelsSpecifiedForSummarySQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier,ChannelType, ' +
              ' ChannelName,ChannelNumber,FirmYieldCalc,FlowOutput, ChannelAreaID FROM ChannelDetails A WHERE ' +
               GetScenarioWhereClause + ' AND ' +
              '(A.ChannelType = 13) ' +
              ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMinMaxChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMinMaxChannelDataSQL';
begin
  Result := '';
  try
    Result :=
    'SELECT ChannelDetails.Identifier AS RecordIdentifier, ChannelDetails.ChannelName, ' +
    'ChannelDetails.ChannelNumber, ChannelDetails.ChannelType, ChannelDetails.ChannelSubType, ' +
    'ChannelDetails.DownNodeNumber, ChannelDetails.UpNodeNumber, ChannelDetails.PenaltyNumber, ' +
    'ChannelDetails.FirmYieldCalc, ChannelDetails.SummaryOutput, ChannelDetails.FirmYieldCalc, ChannelDetails.Flowoutput, ChannelDetails.ChannelAreaID, ' +
    'PumpingFeature.Identifier AS PumpID, PumpingFeature.PumpingHead, PumpingFeature.PumpingEfficiency, ' +
    'PumpingFeature.FeatureName, ' +
    'MinMaxChannel.MinMaxChannelName, MinMaxChannel.Identifier AS MinMaxID ' +
    'FROM (ChannelDetails ' +
    'LEFT JOIN PumpingFeature ON ' +
    '  (ChannelDetails.Model = PumpingFeature.Model) AND ' +
    '  (ChannelDetails.StudyAreaName = PumpingFeature.StudyAreaName) AND ' +
    '  (ChannelDetails.SubArea = PumpingFeature.SubArea) AND ' +
    '  (ChannelDetails.Scenario = PumpingFeature.Scenario) AND ' +
    '  (ChannelDetails.ChannelNumber = PumpingFeature.ChannelNumber)) ' +
    'INNER JOIN MinMaxChannel ON ' +
    '  (ChannelDetails.ChannelNumber = MinMaxChannel.MinMaxChannelNumber) AND ' +
    '  (ChannelDetails.Scenario = MinMaxChannel.Scenario) AND ' +
    '  (ChannelDetails.SubArea = MinMaxChannel.SubArea) AND ' +
    '  (ChannelDetails.StudyAreaName = MinMaxChannel.StudyAreaName) AND ' +
    '  (ChannelDetails.Model = MinMaxChannel.Model) ' +
    'WHERE ((ChannelDetails.Model          = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
    '       (ChannelDetails.StudyAreaName  = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
    '       (ChannelDetails.SubArea        = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
    '       (ChannelDetails.Scenario       = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
//    '        (ChannelType in (8,9,18,19))) ' +
    '       (ChannelType in (8,9,22,23,24))) '+
//    '       OR (ChannelDetails.ChannelType = 18))) ' +
    'ORDER BY ChannelDetails.ChannelNumber';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetSpecifiedDemandChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetSpecifiedDemandChannelDataSQL';
begin
  Result := '';
  try
    Result :=
    'SELECT ChannelDetails.Identifier AS RecordIdentifier, ChannelDetails.ChannelType, ' +
    'ChannelDetails.ChannelName, ChannelDetails.ChannelNumber, ' +
    'ChannelDetails.ChannelSubType, ChannelDetails.UpNodeNumber, ChannelDetails.DownNodeNumber, ' +
    'ChannelDetails.PenaltyNumber, ChannelDetails.SummaryOutput, ChannelDetails.FirmYieldCalc, ChannelDetails.Flowoutput, ChannelDetails.ChannelAreaID, ' +
    'SpecifiedDemandFeature.Identifier AS DemandID, SpecifiedDemandFeature.Featurename, ' +
    'SpecifiedDemandFeature.GaugeNumber, SpecifiedDemandFeature.Fullname, ' +
    'SpecifiedDemandFeature.Stochastic ' +
    'FROM (ChannelDetails ' +
    'LEFT JOIN SpecifiedDemandFeature ON ' +
    '  (ChannelDetails.Model = SpecifiedDemandFeature.Model) AND ' +
    '  (ChannelDetails.StudyAreaName = SpecifiedDemandFeature.StudyAreaName) AND ' +
    '  (ChannelDetails.SubArea = SpecifiedDemandFeature.SubArea) AND ' +
    '  (ChannelDetails.Scenario = SpecifiedDemandFeature.Scenario) AND ' +
    '  (ChannelDetails.ChannelNumber = SpecifiedDemandFeature.ChannelNumber)) ' +
    'WHERE ((ChannelDetails.Model          = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
    '       (ChannelDetails.StudyAreaName  = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
    '       (ChannelDetails.SubArea        = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
    '       (ChannelDetails.Scenario       = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
    '       (ChannelDetails.ChannelType = 11)) ' +
    'ORDER BY ChannelDetails.ChannelNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.MinMaxArcRecordFoundSQL(AFeatureID, AArc : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.MinMaxArcRecordFoundSQL';
begin
  Result := '';
  try
    Result := ' SELECT ' +
               ' SubIdentifier ' +
               'FROM MinMaxChannelDistribution ' +
               'WHERE ' +
               '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
               '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
               '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
               '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
               '(Identifier = ' + IntToStr(AFeatureID)                              + ') AND ' +
               '(SubIdentifier = '+ IntToStr(AArc)                              + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMinMaxChannelValuesSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.GetMinMaxChannelValuesSQL';
begin
  Result := '';
  try
    Result := ' SELECT ' +
               'Identifier AS MinMaxFlowIdentifier, SubIdentifier, ' +
               'MFlow01, MFlow02, MFlow03, MFlow04, MFlow05, MFlow06, ' +
               'MFlow07, MFlow08, MFlow09, MFlow10, MFlow11, MFlow12 ' +
               'FROM MinMaxChannelFlow ' +
               'WHERE ' +
               '(MinMaxChannelFlow.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
               '(MinMaxChannelFlow.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
               '(MinMaxChannelFlow.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
               '(MinMaxChannelFlow.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
               '(Identifier = ' + IntToStr(AFeatureID) + ') ' +
               ' ORDER BY SubIdentifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMinMaxChannelDistributionValuesSQL(AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.GetMinMaxChannelDistributionValuesSQL';
begin
  Result := '';
  try
    Result := ' SELECT ' +
               'Identifier AS MinMaxFlowIdentifier, SubIdentifier, ' +
               'Distribution01, Distribution02, Distribution03, Distribution04, Distribution05, Distribution06, ' +
               'Distribution07, Distribution08, Distribution09, Distribution10, Distribution11, Distribution12 ' +
               'FROM MinMaxChannelDistribution ' +
               'WHERE ' +
               '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
               '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
               '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
               '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
               '(Identifier = ' + IntToStr(AFeatureID) + ') ' +
               ' ORDER BY SubIdentifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.GetMaxMinMaxFlowConstraintIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxMinMaxFlowConstraintIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM MinMaxChannel A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxMinMaxFlowConstraintFlowIDSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxMinMaxFlowConstraintFlowIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM MinMaxChannelFlow A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMinMaxFlowConstraintSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertMinMaxFlowConstraintSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO MinMaxChannel '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'MinMaxChannelName, MinMaxChannelNumber) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.MinMaxFlowFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteMinMaxFlowConstraintSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteMinMaxFlowConstraintSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinMaxChannel A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteMinMaxFlowConstraintValueSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteMinMaxFlowConstraintValueSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinMaxChannelFlow A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxMinimumFlowConstraintIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxMinimumFlowConstraintIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM MinFlowChannel A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxMinimumFlowConstraintValueIDSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxMinimumFlowConstraintValueIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM MinFlowChannelValue A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMinimumFlowConstraintSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertMinimumFlowConstraintSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO MinFlowChannel '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'MinFlowChannelName, MinFlowChannelNumber) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.MinimumFlowFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxSummaryChannelSql : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxSummaryChannelSql';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM SummaryChannels A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxSummaryChannelID : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxSummaryChannelID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := '';
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxSummaryChannelSql);
        LDataset.DataSet.Open;
        Result := IntToStr(LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger+1);
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.InsertSummaryChannelsSQL (AChannelNumber : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertSummaryChannelsSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO SummaryChannels '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ChannelNumber) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      GetMaxSummaryChannelID + ','+
      IntToStr(AChannelNumber) + ')';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteSummaryOutputSQL(AChannelNumber : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.DeleteSummaryOutputSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM SummaryChannels A WHERE ' +
                GetScenarioWhereClause +
                'AND A.ChannelNumber = '+ IntToStr(AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.InsertMinimumFlowConstraintValueSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertMinimumFlowConstraintValueSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO MinFlowChannelValue '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'Value01, Value02, Value03, Value04, Value05, Value06, ' +
      'Value07, Value08, Value09, Value10, Value11, Value12) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) +
      ',0,0,0,0,0,0,0,0,0,0,0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteMinimumFlowConstraintSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteMinimumFlowConstraintSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinFlowChannel A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteMinimumFlowConstraintValueSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteMinimumFlowConstraintValueSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinFlowChannelValue A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetLossChannelDataSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetLossChannelDataSQL';
begin
  Result := '';
  try
    Result := ' SELECT ' +
               'ChannelDetails.Identifier AS RecordIdentifier, ChannelDetails.ChannelName, ' +
               'ChannelDetails.ChannelNumber, ChannelDetails.ChannelType, ChannelDetails.ChannelSubType, ' +
               'ChannelDetails.UpNodeNumber, ChannelDetails.DownNodeNumber, ' +
               'ChannelDetails.PenaltyNumber, ChannelDetails.SummaryOutput, ChannelDetails.FirmYieldCalc, ChannelDetails.Flowoutput, ChannelDetails.ChannelAreaID, ' +
               'LossFeature.Identifier, LossFeature.FeatureName, ' +
               'LossFeature.Reference, LossFeature.LossType '  +
               'FROM ChannelDetails, LossFeature ' +
               ' WHERE ' + GetScenarioWhereClauseB + ' AND ' +
               ' (ChannelDetails.ChannelType = 7) AND ' +
               ' (ChannelDetails.Model          = LossFeature.Model) AND ' +
               ' (ChannelDetails.StudyAreaName  = LossFeature.StudyAreaName) AND ' +
               ' (ChannelDetails.SubArea        = LossFeature.SubArea) AND ' +
               ' (ChannelDetails.Scenario       = LossFeature.Scenario) AND ' +
               ' (ChannelDetails.ChannelNumber  = LossFeature.ChannelNumber) ' +
               ' ORDER BY  ' +
               ' ChannelDetails.ChannelNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetLossChannelValuesSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.GetLossChannelValuesSQL';
begin
  Result := '';
  try
    Result := ' SELECT ' +
               'Identifier, SubIdentifier, ' +
               'Value01, Value02, Value03, Value04, Value05, Value06, ' +
               'Value07, Value08, Value09, Value10, Value11, Value12 ' +
               'FROM LossFeatureValue ' +
               'WHERE ' +
               '(LossFeatureValue.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
               '(LossFeatureValue.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
               '(LossFeatureValue.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
               '(LossFeatureValue.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
               '(Identifier = ' + IntToStr(AFeatureID) + ') ' +
               ' ORDER BY SubIdentifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxLossFeatureIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxLossFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM LossFeature A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxLossFeatureValueIDSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxLossFeatureValueIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM LossFeatureValue A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertLossFeatureSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertLossFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO LossFeature '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'FeatureName, ChannelNumber, LossType) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.LossFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertLossFeatureValueSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertLossFeatureValueSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO LossFeatureValue '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'SubIdentifier, Value01, Value02, Value03, Value04, Value05, Value06, ' +
      'Value07, Value08, Value09, Value10, Value11, Value12) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) +
      ',0,0,0,0,0,0,0,0,0,0,0,0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteLossFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteLossFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM LossFeature A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteLossFeatureValueSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteLossFeatureValueSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM LossFeatureValue A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDataSQLAgent.LoadChannelDetailsContextData(AContextData: TStringList;
  ARecordIdentifier, AChannelNumber, AChannelType: string);
const OPNAME = 'TChannelDataSQLAgent.LoadChannelDetailsContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + ARecordIdentifier);
    AContextData.Add('ChannelNumber='    + AChannelNumber);
    AContextData.Add('ChannelType='+ AChannelType);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadChannelConfigContextData(AContextData: TStringList);
const OPNAME = 'TChannelDataSQLAgent.LoadChannelConfigContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadChannelContextData (AContextData   : TStringList;
                                                       AChannelID     : string;
                                                       AChannelNumber : string);
const OPNAME = 'TChannelDataSQLAgent.LoadChannelContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + AChannelID);
    AContextData.Add('ChannelNumber='    + AChannelNumber);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadWaterSupplyDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
const OPNAME = 'TChannelDataSQLAgent.LoadWaterSupplyDataContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadContextData_FeatureIDSubIDFieldNameID
                                                        (AContextData : TStringList;
                                                         AFeatureID   : string;
                                                         ASubID       : string;
                                                         AFieldNameID : string);
const OPNAME = 'TChannelDataSQLAgent.LoadContextData_FeatureIDSubIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='         + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='       + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='      + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + AFeatureID);
    AContextData.Add('SubIdentifier=' + ASubID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadContextData_FeatureID
                                                   (AContextData : TStringList;
                                                    AFeatureID   : string);
const OPNAME = 'TChannelDataSQLAgent.LoadContextData_FeatureID';
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

procedure TChannelDataSQLAgent.LoadContextData_FeatureIDFieldNameID
                                                   (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
const OPNAME = 'TChannelDataSQLAgent.LoadContextData_FeatureIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='               + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='       + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='             + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='            + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='          + AFeatureID);
    AContextData.Add('SubIdentifier=0');
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadChannelPenaltyValueContextData(AContextData: TStringList;
  APenaltyNumber,AFieldNameIdentifier: string);
const OPNAME = 'TChannelDataSQLAgent.LoadChannelPenaltyValueContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + APenaltyNumber);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataSQLAgent.LoadChannelPenaltyContextData (AContextData : TStringList;
                                                              APenaltyID   : string);
const OPNAME = 'TChannelDataSQLAgent.LoadChannelPenaltyContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='    + APenaltyID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChannelDataSQLAgent.GetDeleteChannelPenaltySQL(AIdentifier: integer): string;
const OPNAME = 'TChannelDataSQLAgent.GetDeleteChannelPenaltySQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ChannelArcPenalty A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxChannelPenaltyIdentifierSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxChannelPenaltyIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM ChannelArcPenalty A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetNewChannelPenaltySQL(AIdentifier: integer): string;
const OPNAME = 'TChannelDataSQLAgent.GetNewChannelPenaltySQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO ChannelArcPenalty '+
              '(Model, StudyAreaName, SubArea, Scenario, ' +
              'PenaltyName, Identifier, ArcCount, Penalty01) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              QuotedStr(UpperCase(FAppModules.Language.GetString('TField.Cpenalty')) + ' ' + IntToStr(AIdentifier)) + ',' +
              IntToStr(AIdentifier) + ','+
              '1, 0)'
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMonthlyConstraintSQL : string;
const OPNAME = 'TChannelDataSQLAgent.InsertMonthlyConstraintSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO MinMaxChannelFlow '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'SubIdentifier, MFlow01, MFlow02, MFlow03, MFlow04, MFlow05, MFlow06, ' +
    'MFlow07, MFlow08, MFlow09, MFlow10, MFlow11, MFlow12) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ASubIdentifier, 0,0,0,0,0,0,0,0,0,0,0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMinMaxChannelDistributionRowValuesSQL: string;
const OPNAME = 'TChannelDataSQLAgent.InsertMinMaxChannelDistributionRowValuesSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO MinMaxChannelDistribution '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'SubIdentifier, Distribution01, Distribution02, Distribution03, Distribution04, Distribution05, Distribution06, ' +
    'Distribution07, Distribution08, Distribution09, Distribution10, Distribution11, Distribution12) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ASubIdentifier,:ADistribution01, :ADistribution02, :ADistribution03, :ADistribution04, :ADistribution05, :ADistribution06, ' +
    ':ADistribution07, :ADistribution08, :ADistribution09, :ADistribution10, :ADistribution11, :ADistribution12)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMinMaxChannelDistributionValuesSQL : string;
const OPNAME = 'TChannelDataSQLAgent.InsertMinMaxChannelDistributionValuesSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO MinMaxChannelDistribution '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'SubIdentifier, Distribution01, Distribution02, Distribution03, Distribution04, Distribution05, Distribution06, ' +
    'Distribution07, Distribution08, Distribution09, Distribution10, Distribution11, Distribution12) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ASubIdentifier,0,0,0,0,0,0,0,0,0,0,0,0)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.DeleteMonthlyConstraintSQL (AFeatureID : integer;
                                                          AIndex     : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.DeleteMonthlyConstraintSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinMaxChannelFlow A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')' +
              ' AND (A.SubIdentifier = ' + IntToStr(AIndex) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteMonthlyDistributionSQL (AFeatureID : integer;
                                                          AIndex     : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.DeleteMonthlyDistributionSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinMaxChannelDistribution A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')' +
              ' AND (A.SubIdentifier = ' + IntToStr(AIndex) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.DeleteAllMonthlyDistributionSQL(AFeatureID: integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteAllMonthlyDistributionSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MinMaxChannelDistribution A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelPenaltyCountSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelPenaltyCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS PenaltyCount FROM ChannelArcPenalty A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetUpdateChannelArcCountSQL(APenaltyIdentifier, ANewCount: integer): string;
const OPNAME = 'TChannelDataSQLAgent.GetUpdateChannelArcCountSQL';
begin
  Result := '';
  try
    Result := 'UPDATE  ChannelArcPenalty A SET A.ArcCount = '+
              IntToStr(ANewCount) +
              ' WHERE ' + GetScenarioWhereClause +
              ' AND Identifier = '+ IntToStr(APenaltyIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxChannelIdentifierSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxChannelIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM ChannelDetails A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxChannelNumberSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxChannelNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(ChannelNumber) AS MaxChannelNumber FROM ChannelDetails A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelCountSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetChannelCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count(*) AS ChannelCount FROM ChannelDetails A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertChannelSQL (AChannelID : integer;
                                                AChannelNr : integer): string;
const OPNAME = 'TChannelDataSQLAgent.InsertChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO ChannelDetails ' +
              '(Model, StudyAreaName, SubArea, Scenario, ChannelType, ' +
              'Identifier, ChannelName, ChannelNumber, ' +
              'UpNodeNumber, DownNodeNumber, PenaltyNumber, ChannelAreaID,SummaryOutput) ' +
              'VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              '12,' +
              IntToStr(AChannelID) + ','+
              QuotedStr(UpperCase(FAppModules.Language.GetString('Channel.Channel')) + ' ' + IntToStr(AChannelNr)) + ',' +
              IntToStr(AChannelNr) + ',0,0,0,0,'+ QuotedStr('Y')+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteChannelSQL (AChannelID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ChannelDetails A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AChannelID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxPumpingFeatureIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxPumpingFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM PumpingFeature A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertPumpingFeatureSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertPumpingFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO PumpingFeature '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'FeatureName, ChannelNumber, PumpingEfficiency, PumpingHead) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.PumpingFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0, 0, 0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeletePumpingFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeletePumpingFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM PumpingFeature A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxSpecifiedDemandFeatureIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxSpecifiedDemandFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM SpecifiedDemandFeature A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertSpecifiedDemandFeatureSQL (AFeatureID : integer) : string;
const OPNAME = 'TChannelDataSQLAgent.InsertSpecifiedDemandFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO SpecifiedDemandFeature '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'FeatureName, ChannelNumber, GaugeNumber, Stochastic) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.SpecifiedDemand')) + ' ' + IntToStr(AFeatureID)) +
      ',0,' +

      '0,' +
      QuotedSTr('H') +
      ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteSpecifiedDemandFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TChannelDataSQLAgent.DeleteSpecifiedDemandFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM SpecifiedDemandFeature A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertChannelCommentsSQL : string;
const OPNAME = 'TChannelDataSQLAgent.InsertChannelCommentsSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO ChannelComments '+
      '(Model, StudyAreaName, SubArea, Scenario, Comment01, ' +
      'Comment02, Comment03, Comment04, Comment05, Comment06, Comment07, ' +
      'Comment08, Comment09, Comment10, Comment11, Comment12, Comment13,Comment22) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
      QuotedStr('\CHANNEL PENALTIES') + ',' +
      QuotedStr('\MASTER CONTROL FEATURES') + ',' +
      QuotedStr('\POWER PLANTS') + ',' +
      QuotedStr('\IRRIGATION AREAS') + ',' +
      QuotedStr('\DIVERSION CHANNELS') + ',' +
      QuotedStr('\MINIMUM FLOW CHANNELS') + ',' +
      QuotedStr('\LOSS CHANNELS') + ',' +
      QuotedStr('\MIN-MAX CHANNELS') + ',' +
      QuotedStr('\PUMPING CHANNELS') + ',' +
      QuotedStr('\SPECIFIED INFLOW CHANNELS') + ',' +
      QuotedStr('\SPECIFIED DEMAND CHANNELS') + ',' +
      QuotedStr('\GENERAL FLOW CHANNELS') + ',' +
      QuotedStr('\CHANNELS INCLUDED IN SUMMARY OUTPUT') + ',' +
      QuotedStr('\GROUNDWATER CHANNELS') + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.DeleteChannelCommentsSQL : string;
const OPNAME = 'TChannelDataSQLAgent.DeleteChannelCommentsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ChannelComments A WHERE ' + GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.SelectChannelCommentsSQL : string;
const OPNAME = 'TChannelDataSQLAgent.SelectChannelCommentsSQL';
begin
  Result := '';
  try
    Result := 'SELECT * FROM ChannelComments A WHERE ' + GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.MinMaxArcRecordFound(AFeatureID, AArc : integer) : boolean;
const OPNAME = 'TChannelDataSQLAgent.MinMaxArcRecordFound';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(MinMaxArcRecordFoundSQL(AFeatureID, AArc));
        LDataset.DataSet.Open;
        Result := (not LDataset.DataSet.FieldByName('SubIdentifier').IsNull);
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.InsertChannel (var AChannelID : integer;
                                              var AChannelNr : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertChannel';
var
  LDataSet       : TAbstractModelDataset;
  LChannelID     : integer;
  lChannelNumber : integer;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LChannelID     := GetMaxChannelIdentifier + 1;
        lChannelNumber := GetMaxChannelNumber + 1;

        LDataSet.SetSQL(InsertChannelSQL(LChannelID, lChannelNumber));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AChannelID := LChannelID;
        AChannelNr := lChannelNumber;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteChannel(AChannelID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteChannel';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteChannelSQL(AChannelID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxChannelIdentifier: integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxChannelIdentifier';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxChannelIdentifierSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxChannelNumber: integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxChannelNumber';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxChannelNumberSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxChannelNumber').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelCount: integer;
const OPNAME = 'TChannelDataSQLAgent.GetChannelCount';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetChannelCountSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('ChannelCount').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertSpecifiedDemandFeature (var AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertSpecifiedDemandFeature';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxSpecifiedDemandFeatureID + 1;

        LDataSet.SetSQL(InsertSpecifiedDemandFeatureSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteSpecifiedDemandFeature (AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteSpecifiedDemandFeature';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteSpecifiedDemandFeatureSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxSpecifiedDemandFeatureID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxSpecifiedDemandFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxSpecifiedDemandFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMinMaxFlowConstraint (var AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertMinMaxFlowConstraint';
var
  LDataSet    : TAbstractModelDataset;
  LFeatureID  : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxMinMaxFlowConstraintID + 1;

        LDataSet.SetSQL(InsertMinMaxFlowConstraintSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteMinMaxFlowConstraint (AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteMinMaxFlowConstraint';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteMinMaxFlowConstraintSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteMinMaxFlowConstraintValueSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxMinMaxFlowConstraintID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxMinMaxFlowConstraintID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxMinMaxFlowConstraintIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
        LDataSet.SetSQL(GetMaxMinMaxFlowConstraintFlowIDSQL);
        LDataset.DataSet.Open;
        Result := Max(Result,LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger);
        LDataset.DataSet.Close;

      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.AddMonthlyConstraint (AFeatureID : integer;
                                                     AIndex     : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.AddMonthlyConstraint';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(InsertMonthlyConstraintSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);
        LDataSet.SetParams(['AIdentifier','ASubIdentifier'],
                           [IntToStr(AFeatureID), IntToStr(AIndex)]);
        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          Result := True;
          LDataset.DataSet.Close;
        end;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.AddMinMaxChannelDistributionBlockValues(AFeatureID, ARowCount: integer;
                                                                      ADistributionByArcMonth: TMinMaxMonthlyDoublesArray): boolean;
const OPNAME = 'TChannelDataSQLAgent.AddMinMaxChannelDistributionBlockValues';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LRow,LCol   : integer;
  LRowData    : array [1..12] of String;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(DeleteAllMonthlyDistributionSQL(AFeatureID));
          LDataSet.ExecSQL;
          for LRow := 1 to ARowCount do
          begin
            for LCol := MinMonths to MaxMonths do
              LRowData[LCol] := FloatToStr(ADistributionByArcMonth[LRow,LCol]);
            LDataset.DataSet.Close;
            LDataSet.SetSQL(InsertMinMaxChannelDistributionRowValuesSQL);
            FAppModules.StudyArea.SetDefaultParams(LDataSet);
            LDataSet.SetParams(['AIdentifier','ASubIdentifier'],
                               [IntToStr(AFeatureID), IntToStr(LRow)]);

            LDataSet.SetParams(['ADistribution01','ADistribution02','ADistribution03','ADistribution04','ADistribution05','ADistribution06',
                                'ADistribution07','ADistribution08','ADistribution09','ADistribution10','ADistribution11','ADistribution12'],
                               [LRowData[01],LRowData[02],LRowData[03],LRowData[04],LRowData[05],LRowData[06],
                                LRowData[07],LRowData[08],LRowData[09],LRowData[10],LRowData[12],LRowData[12]]);
            LDataset.ExecSQL;
          end;
          FAppModules.Database.Commit;
        except
          on E:Exception do FAppModules.Database.Rollback;
        end;
        Result := True;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.AddMinMaxChannelDistributionValues(AFeatureID : integer;
                                                     AIndex     : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.AddMinMaxChannelDistributionValues';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(InsertMinMaxChannelDistributionValuesSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);
        LDataSet.SetParams(['AIdentifier','ASubIdentifier'],
                           [IntToStr(AFeatureID), IntToStr(AIndex)]);
        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          Result := True;
          LDataset.DataSet.Close;
        end;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TChannelDataSQLAgent.DeleteMonthlyConstraint (AFeatureID : integer;
                                                        AIndex     : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteMonthlyConstraint';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteMonthlyConstraintSQL(AFeatureID, AIndex));
        LDataset.ExecSQL;
        Result := True;
      end;
      LDataset.DataSet.Close;
      FAppModules.StudyArea.LastUpdateDate := Now();

      LImportDate := FAppModules.StudyArea.GetStudyImportDate;
      if LImportDate = NullDateTime then
        FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

     Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteMonthlyDistribution (AFeatureID : integer;
                                                        AIndex     : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteMonthlyDistribution';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteMonthlyDistributionSQL(AFeatureID, AIndex));
        LDataset.ExecSQL;
        Result := True;
      end;
      LDataset.DataSet.Close;
      FAppModules.StudyArea.LastUpdateDate := Now();

      LImportDate := FAppModules.StudyArea.GetStudyImportDate;
      if LImportDate = NullDateTime then
        FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

     Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TChannelDataSQLAgent.InsertPumpingFeature (var AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertPumpingFeature';
var
  LDataSet    : TAbstractModelDataset;
  LFeatureID  : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxPumpingFeatureID + 1;

        LDataSet.SetSQL(InsertPumpingFeatureSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeletePumpingFeature (AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeletePumpingFeature';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeletePumpingFeatureSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxPumpingFeatureID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxPumpingFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxPumpingFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertMinimumFlowConstraint (var AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertMinimumFlowConstraint';
var
  LDataSet    : TAbstractModelDataset;
  LFeatureID  : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxMinimumFlowConstraintID + 1;

        LDataSet.SetSQL(InsertMinimumFlowConstraintSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(InsertMinimumFlowConstraintValueSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteMinimumFlowConstraint (AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteMinimumFlowConstraint';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteMinimumFlowConstraintSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteMinimumFlowConstraintValueSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxMinimumFlowConstraintID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxMinimumFlowConstraintID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxMinimumFlowConstraintIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
        LDataSet.SetSQL(GetMaxMinimumFlowConstraintValueIDSQL);
        LDataset.DataSet.Open;
        Result := Max(Result,LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger);
        LDataset.DataSet.Close;

      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertLossFeature (var AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertLossFeature';
var
  LDataSet    : TAbstractModelDataset;
  LFeatureID  : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxLossFeatureID + 1;

        LDataSet.SetSQL(InsertLossFeatureSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(InsertLossFeatureValueSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteLossFeature (AFeatureID : integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteLossFeature';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteLossFeatureSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteLossFeatureValueSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxLossFeatureID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxLossFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxLossFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
        LDataSet.SetSQL(GetMaxLossFeatureValueIDSQL);
        LDataset.DataSet.Open;
        Result := Max(Result,LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger);
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.InsertChannelPenalty(var ACreatedPenaltyId: integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertChannelPenalty';
var
  LDataSet           : TAbstractModelDataset;
  LPenaltyIdentifier : integer;
  LImportDate        : TDateTime;
begin
  Result := False;
  ACreatedPenaltyId := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LPenaltyIdentifier := GetMaxChannelPenaltyIdentifier + 1;

        LDataSet.SetSQL(GetNewChannelPenaltySQL(LPenaltyIdentifier));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        ACreatedPenaltyId := LPenaltyIdentifier;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteChannelPenalty(AChannelPenaltyNumber: integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteChannelPenalty';
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
        LDataSet.SetSQL(GetDeleteChannelPenaltySQL(AChannelPenaltyNumber));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.GetMaxChannelPenaltyIdentifier: integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxChannelPenaltyIdentifier';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxChannelPenaltyIdentifierSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetChannelPenaltyCount: integer;
const OPNAME = 'TChannelDataSQLAgent.GetChannelPenaltyCount';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetChannelPenaltyCountSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('PenaltyCount').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.UpdateChannelArcCount(APenaltyIdentifier,ANewCount: integer): boolean;
const OPNAME = 'TChannelDataSQLAgent.UpdateChannelArcCount';
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
        LDataSet.SetSQL(GetUpdateChannelArcCountSQL(APenaltyIdentifier,ANewCount));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.InsertChannelComments : boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertChannelComments';
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
        LDataSet.SetSQL(SelectChannelCommentsSQL);
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount = 0) then
        begin
          LDataSet.SetSQL(InsertChannelCommentsSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
        end;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.DeleteChannelComments : boolean;
const OPNAME = 'TChannelDataSQLAgent.DeleteChannelComments';
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
        LDataSet.SetSQL(DeleteChannelCommentsSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.InsertSummaryOutput(AChannelNo : integer):boolean;
const OPNAME = 'TChannelDataSQLAgent.InsertSummaryOutput';
var
  LDataSet    : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(InsertSummaryChannelsSQL(AChannelNo));
        LDataset.ExecSQL;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataSQLAgent.RemoveSummaryOutput(AChannelNo : integer):boolean;
const OPNAME = 'TChannelDataSQLAgent.RemoveSummaryOutput';
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
        LDataSet.SetSQL(DeleteSummaryOutputSQL(AChannelNo));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TChannelDataSQLAgent.LoadContextData(AContextData: TStringList);
const OPNAME = 'TChannelDataSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChannelDataSQLAgent.CopyChannelFromScenario(ASourceStudyAreaName,ASourceSubArea,
                              ASourceScenario: string; AChannelNumberList: TStrings;
                              AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TChannelDataSQLAgent.CopyChannelFromScenario';
      ChannelDetailsSQL         = 'SELECT * FROM ChannelDetails WHERE ';
      MasterControlFeatureSQL   = 'SELECT * FROM MasterControlFeature WHERE ';
      MinFlowFeatureSQL         = 'SELECT * FROM MinFlowChannel WHERE ';
      MinFlowFeatureValueSQL    = 'SELECT * FROM MinFlowChannelValue WHERE ';
      LossFeatureSQL            = 'SELECT * FROM LossFeature WHERE ';
      LossFeatureValueSQL       = 'SELECT * FROM LossFeatureValue WHERE ';
      MinMaxFeatureSQL          = 'SELECT * FROM MinMaxChannel WHERE ';
      MinMaxFeatureValueSQL     = 'SELECT * FROM MinMaxChannelFlow WHERE ';
      MinMaxDistributionSQL     = 'SELECT * FROM MinMaxChannelDistribution WHERE ';
      PumpingFeatureSQL         = 'SELECT * FROM PumpingFeature WHERE ';
      SpecifiedInflowFeatureSQL = 'SELECT * FROM SpecifiedInflowFeature WHERE ';
      SpecifiedDemandFeatureSQL = 'SELECT * FROM SpecifiedDemandFeature WHERE ';
      DiversionFeatureSQL       = 'SELECT * FROM DiversionFeatures WHERE ';
      DiversionFeatureType1n2SQL = 'SELECT * FROM DiversionFeaturesType1n2 WHERE ';
      DiversionFeatureType3SQL   = 'SELECT * FROM DiversionFeaturesType3 WHERE ';
      DiversionFeaturesType3ProportionsSQL = 'SELECT * FROM DiversionFeaturesType3Proportions WHERE ';
      FlowConstraintsSQL       = 'SELECT * FROM FlowConstraints WHERE ';
      FlowConstraintsDeltaHSQL = 'SELECT * FROM FlowConstraintsDeltaH WHERE ';
      FlowConstraintsDischargeSQL = 'SELECT * FROM FlowConstraintsDischarge WHERE ';
      FlowConstraintsDiversionSQL = 'SELECT * FROM FlowConstraintsDiversion WHERE ';
      FlowConstraintsElevationSQL = 'SELECT * FROM FlowConstraintsElevation WHERE ';
      FlowConstraintsType11DepthSQL = 'SELECT * FROM FlowConstraintsType11Depth WHERE ';
      FlowConstraintsType11FlowSQL = 'SELECT * FROM FlowConstraintsType11Flow WHERE ';
      FlowConstraintsValueSQL = 'SELECT * FROM FlowConstraintsValue WHERE ';
      IFRFeaturesSQL = 'SELECT * FROM IFRFeatures WHERE ';
      IFRFeaturesDetailsSQL = 'SELECT * FROM IFRFeaturesDetails WHERE ';
      IFRReferenceSQL = 'SELECT * FROM IFRReference WHERE ';
      WaterDemandFeaturesSQL = 'SELECT * FROM WaterDemandFeatures WHERE ';
var
  LNewChannelID         : integer;
  LNewChannelNumber     : integer;
  LCurrentChannelID     : integer;
  LCurrentChannelNumber : integer;

  LCurrentMinFlowChannelID : integer;
  LNewMinFlowChannelID     : integer;

  LCurrentLossChannelID  : integer;
  LNewLossChannelID      : integer;

  LCurrentMinMaxChannelID : integer;
  LNewMinMaxChannelID     : integer;

  LNewPumpingFeaturelID : integer;
  LNewSpecifiedDemandID : integer;

  LCurrentDiversionChannelID : integer;
  LNewDiversionChannelID     : integer;

  LCurrentFlowConstraintsID : integer;
  LNewFlowConstraintsID     : integer;

  LCurrentIFRID : integer;
  LNewIFRID     : integer;

  LCurrentWaterDemandID : integer;
  LNewWaterDemandID     : integer;
  //LCurrentCategoryID    : integer;
  //LNewCategoryID        : integer;

  LChannelName          : string;

  LChannelIndex         : integer;
  LIndex                : integer;
  LImportDate           : TDateTime;

  LModel                    : string;
  LFieldName                : string;
  LDestinationStudyAreaName : string;
  LDestinationSubArea       : string;
  LDestinationScenario      : string;
  LSourceWhereClause        : string;
  LDestinationWhereClause   : string;
  LMessage                  : string;
  LSourceSQL                : string;
  LDestinationSQL           : string;
  LStop                     : boolean;

  LSourceDataSet      : TAbstractModelDataset;
  LDestinationDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if not Assigned(AChannelNumberList) then Exit;

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
          LCurrentChannelID := 0;
          LNewChannelID := GetMaxChannelIdentifier;
          LNewChannelNumber:= GetMaxChannelNumber;
          for LChannelIndex := 0 to AChannelNumberList.Count-1 do
          begin
            LChannelName := AChannelNumberList[LChannelIndex];
            LCurrentChannelNumber  := Integer(AChannelNumberList.Objects[LChannelIndex]);
            LNewChannelID     := LNewChannelID + 1;
            LNewChannelNumber := LNewChannelNumber + 1;

            LMessage := 'Copying Channel ('+LChannelName+') ' + IntToStr(LChannelIndex+1) + ' of '+ IntToStr(AChannelNumberList.Count);
            AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
            if LStop then
            begin
              FAppModules.Database.Rollback;
              Exit;
            end;

            //________________________________________________________ ChannelDetails ____________________________
            LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;

            LDestinationSQL := ChannelDetailsSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table ChannelDetails cannot be set to updatable.');
            end
            else
            begin
              LCurrentChannelID := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
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
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewChannelID;
              LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
              LDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger := 0;
              LDestinationDataSet.DataSet.Post;
            end;

            //______________________________________________________ MasterControlFeature _____________________________
            LSourceSQL := MasterControlFeatureSQL + LSourceWhereClause + 'AND ChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := MasterControlFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table MasterControlFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewChannelID;
                LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //______________________________________________________ MinFlowChannel _____________________________
            LSourceSQL := MinFlowFeatureSQL + LSourceWhereClause + ' AND MinFlowChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentMinFlowChannelID :=  LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LNewMinFlowChannelID := GetMaxMinimumFlowConstraintID + 1;
              LDestinationSQL := MinFlowFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table MinFlowFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewMinFlowChannelID;
                LDestinationDataSet.DataSet.FieldByName('MinFlowChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;

              //______________________________________________________ MinFlowChannelValue _____________________________
              LSourceSQL := MinFlowFeatureValueSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentMinFlowChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := MinFlowFeatureValueSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinFlowFeatureValue cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewMinFlowChannelID;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;
            end;

            //______________________________________________________ LossFeature_____________________________
            LSourceSQL := LossFeatureSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentLossChannelID  := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LNewLossChannelID := GetMaxLossFeatureID + 1;
              LDestinationSQL := LossFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table LossFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewLossChannelID;
                LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;

             //______________________________________________________LossFeatureValue _____________________________
              LSourceSQL := LossFeatureValueSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentLossChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := LossFeatureValueSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table LossFeatureValue cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger   := LNewLossChannelID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
            end;


            //______________________________________________________MinMaxChannel_____________________________
            LSourceSQL := MinMaxFeatureSQL + LSourceWhereClause + ' AND MinMaxChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentMinMaxChannelID := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LNewMinMaxChannelID := GetMaxMinMaxFlowConstraintID +1;
              LDestinationSQL := MinMaxFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table MinMaxFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewMinMaxChannelID;
                LDestinationDataSet.DataSet.FieldByName('MinMaxChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;

              //______________________________________________________MinMaxChannelFlow_____________________________
              LSourceSQL := MinMaxFeatureValueSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentMinMaxChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := MinMaxFeatureValueSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinMaxFeatureValue cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewMinMaxChannelID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

              //______________________________________________________MinMaxChannelDistribution_____________________________
              LSourceSQL := MinMaxDistributionSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentMinMaxChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := MinMaxDistributionSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table MinMaxDistribution cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewMinMaxChannelID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

            end;

            //______________________________________________________PumpingFeature_____________________________
            LSourceSQL := PumpingFeatureSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LNewPumpingFeaturelID := GetMaxPumpingFeatureID + 1;
              LDestinationSQL := PumpingFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table PumpingFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewPumpingFeaturelID;
                LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;
            end;


            //not done Presley comeback
            //______________________________________________________SpecifiedInflowFeature_____________________________
            LSourceSQL := SpecifiedInflowFeatureSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := SpecifiedInflowFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table SpecifiedInflowFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewChannelID;
                LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //______________________________________________________SpecifiedDemandFeature_____________________________
            LSourceSQL := SpecifiedDemandFeatureSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LNewSpecifiedDemandID := GetMaxSpecifiedDemandFeatureID + 1;
              LDestinationSQL := SpecifiedDemandFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table SpecifiedDemandFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewSpecifiedDemandID;
                LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;
            end;

            //______________________________________________________DiversionFeatures_____________________________
            LSourceSQL := DiversionFeatureSQL + LSourceWhereClause + ' AND DivChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentDiversionChannelID := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LNewDiversionChannelID := GetMaxDiversionFeatureID + 1;
              LDestinationSQL := DiversionFeatureSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table DiversionFeature cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewDiversionChannelID;
                LDestinationDataSet.DataSet.FieldByName('DivChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;

              //______________________________________________________DiversionFeaturesType1n2_____________________________
              LSourceSQL := DiversionFeatureType1n2SQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentDiversionChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := DiversionFeatureType1n2SQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table DiversionFeatureType1n2 cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewDiversionChannelID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

              //______________________________________________________DiversionFeaturesType3_____________________________
              LSourceSQL := DiversionFeatureType3SQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentDiversionChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := DiversionFeatureType3SQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table DiversionFeatureType3 cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewDiversionChannelID;
                  LDestinationDataSet.DataSet.FieldByName('NodeNumber').AsInteger    := 0;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;

              //______________________________________________________DiversionFeaturesType3Proportions_____________________________
              LSourceSQL := DiversionFeaturesType3ProportionsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentDiversionChannelID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := DiversionFeaturesType3ProportionsSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table DiversionFeaturesType3Proportions cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewDiversionChannelID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
            end;

            //______________________________________________________FlowConstraints_____________________________
            LSourceSQL := FlowConstraintsSQL + LSourceWhereClause + ' AND ConstraintsChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentFlowConstraintsID  := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LNewFlowConstraintsID := GetMaxLossFeatureID + 1;
              LDestinationSQL := FlowConstraintsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table FlowConstraints cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                LDestinationDataSet.DataSet.FieldByName('ConstraintsChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.FieldByName('UpStreamReservoirNumber').AsInteger := 0;
                LDestinationDataSet.DataSet.FieldByName('DownStreamReservoirNumber').AsInteger := 0;
                LDestinationDataSet.DataSet.Post;
              end;

              //______________________________________________________FlowConstraintsDeltaH_____________________________
              LSourceSQL := FlowConstraintsDeltaHSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := FlowConstraintsDeltaHSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsDeltaH cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;

              //______________________________________________________FlowConstraintsDischarge_____________________________
              LSourceSQL := FlowConstraintsDischargeSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              if not LSourceDataSet.DataSet.Eof then
              begin
                LDestinationSQL := FlowConstraintsDischargeSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsDischarge cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
              end;

              //______________________________________________________FlowConstraintsDiversion_____________________________
              LSourceSQL := FlowConstraintsDiversionSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := FlowConstraintsDiversionSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsDiversion cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

              //______________________________________________________FlowConstraintsElevation_____________________________
              LSourceSQL := FlowConstraintsElevationSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := FlowConstraintsElevationSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsElevation cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

              //______________________________________________________FlowConstraintsType11Depth_____________________________
              LSourceSQL := FlowConstraintsType11DepthSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := FlowConstraintsType11DepthSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsType11Depth cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

              //______________________________________________________FlowConstraintsType11Flow_____________________________
              LSourceSQL := FlowConstraintsType11FlowSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := FlowConstraintsType11FlowSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsType11Flow cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;

              //______________________________________________________FlowConstraintsValue_____________________________
              LSourceSQL := FlowConstraintsValueSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFlowConstraintsID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := FlowConstraintsValueSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table FlowConstraintsValue cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewFlowConstraintsID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
            end;

            //______________________________________________________IFRFeatures_____________________________
            LSourceSQL := IFRFeaturesSQL + LSourceWhereClause + ' AND IFRChannelNumber = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentIFRID := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LNewIFRID := GetMaxIFRFeatureID + 1;
              LDestinationSQL := IFRFeaturesSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table IFRFeatures cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewIFRID;
                LDestinationDataSet.DataSet.FieldByName('IFRChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.Post;
              end;

              //______________________________________________________IFRFeaturesDetails_____________________________
              LSourceSQL := IFRFeaturesDetailsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIFRID);
              LSourceDataSet.DataSet.Close;
              LSourceDataSet.SetSQL(LSourceSQL);
              LSourceDataSet.DataSet.Open;
              while not LSourceDataSet.DataSet.Eof do
              begin
                LDestinationSQL := IFRFeaturesDetailsSQL + LDestinationWhereClause;
                LDestinationDataSet.DataSet.Close;
                LDestinationDataSet.SetSQL(LDestinationSQL);
                LDestinationDataSet.SetReadOnly(False);
                LDestinationDataSet.DataSet.Open;
                if LDestinationDataSet.IsReadOnly  then
                begin
                  LDestinationDataSet.DataSet.Close;
                  raise Exception.Create('Query to table IFRFeaturesDetails cannot be set to updatable.');
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
                  LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger    := LNewIFRID;
                  LDestinationDataSet.DataSet.Post;
                end;
                LSourceDataSet.DataSet.Next;
              end;
            end;

            //______________________________________________________WaterDemandFeatures_____________________________
            LSourceSQL := WaterDemandFeaturesSQL + LSourceWhereClause + ' AND ChannelNumber  = '+ IntToStr(LCurrentChannelNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if not LSourceDataSet.DataSet.Eof then
            begin
              LCurrentWaterDemandID := LSourceDataSet.DataSet.FieldByName('FeatureID').AsInteger;
              LNewWaterDemandID := GetMaxWaterDemandFeatureID + 1;
              LDestinationSQL := WaterDemandFeaturesSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table WaterDemandFeatures cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('FeatureID').AsInteger     := LNewWaterDemandID;
                LDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LNewChannelNumber;
                LDestinationDataSet.DataSet.FieldByName('CategoryID').AsInteger := 0;
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
      end;
    finally
      LSourceDataSet.Free;
      LDestinationDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChannelDataSQLAgent.GetMaxDiversionFeatureID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxDiversionFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxDiversionFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxDiversionFeatureIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxDiversionFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM DiversionFeatures A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxIFRFeatureID: integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxIFRFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxIFRFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxIFRFeatureIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxIFRFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM IFRFeatures A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxWaterDemandCategoryIDSQL: string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxWaterDemandCategoryIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(CategoryID) AS MaxIdentifier FROM WaterDemandCategories A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxWaterDemandFeatureIDSQL : string;
const OPNAME = 'TChannelDataSQLAgent.GetMaxWaterDemandFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(FeatureID) AS MaxIdentifier FROM WaterDemandFeatures A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelDataSQLAgent.GetMaxWaterDemandCategoryID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxWaterDemandCategoryID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxWaterDemandCategoryIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetMaxWaterDemandFeatureID : integer;
const OPNAME = 'TChannelDataSQLAgent.GetMaxWaterDemandFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxWaterDemandFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDataSQLAgent.GetInterBasinSupportChannelNumbersCommaText: string;
const OPNAME = 'TChannelDataSQLAgent.GetInterBasinSupportChannelNumbersCommaText';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := '';
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('SELECT * FROM InterBasinSupport A WHERE ' + GetScenarioWhereClause);
        LDataset.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          Result := Result + ',' + Trim(LDataset.DataSet.FieldByName('ChannelNumber').AsString);
          LDataSet.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        if(Result <> '') then
          Delete(Result,1,1);
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


