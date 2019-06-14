//
//
//  UNIT      : Contains TNetworkFeaturesSQLAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/08/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UNetworkFeaturesSQLAgent;

interface

uses
  Classes,
  UAbstractObject,
  VoaimsCom_TLB,
  UCurtailmentAndDrought,
  UGroundWater,
  UPhysicalFlowConstraints;

type
  TNetworkFeaturesSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetDivesionPreprocessorWhereClause : string;
    function GetMaxMasterControlFeatureID : integer;
    function GetMaxDemandCentreID : integer;
    function GetMaxDiversionFeatureID : integer;
    function GetMaxIFRFeatureID : integer;
    function GetMaxIrrigationAreaID : integer;
    function GetMaxSpecifiedInflowFeatureID : integer;
    function GetMaxPowerPlantID : integer;
    function GetMaxWaterDemandFeatureID : integer;
    function GetMaxChannelAreaID : integer;
    function GetMaxWaterDemandCategoryID : integer;
    function GetMaxCurtailedChannelID : integer;
    function GetStationIDByNameSQL(AStationNo : string) : string;
  public
    procedure LoadContextData (AContextData : TStringList);
    procedure LoadContextData_WaterUseOutputProportion (AContextData : TStringList;
                                                        AChannelNr,AFieldNameID : string );
    procedure LoadContextData_FieldNameID (AContextData : TStringList;
                                           AFieldNameID : string);
    procedure LoadContextData_FeatureID (AContextData : TStringList;
                                         AFeatureID   : string);
    procedure LoadContextData_ChannelNr (AContextData : TStringList;
                                         AChannelNr   : string);
    procedure LoadContextData_WaterFeatureID(AContextData: TStringList;
                                         AFeatureID: string);
    procedure LoadContextData_WaterDemandCategoryIDFieldNameID(AContextData : TStringList;
                                                    ACategoryID   : string;
                                                    AFieldNameID : string);
    procedure LoadContextData_WaterDemandCategoryID(AContextData : TStringList;
                                                    ACategoryID   : string);
    procedure LoadContextData_WaterFeatureIDFieldNameID (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
    procedure LoadContextData_FeatureIDFieldNameID (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
    procedure LoadContextData_ChannelID (AContextData : TStringList;
                                         AChannelID   : string);
    procedure LoadContextData_ChannelIDFieldNameID (AContextData : TStringList;
                                                    AChannelID   : string;
                                                    AFieldNameID : string);
    procedure LoadContextData_DroughtRestrictionIdentifier(AContextData: TStringList;
                                                    AIdentifier   : string;
                                                    AFieldNameIdentifier : string);

    procedure LoadCurtailmentDataContextData(AContextData: TStringList;
                                             AFieldNameIdentifier: string);
    procedure LoadContextData_ChannelAreaID(AContextData   : TStringList;
                                            AChannelAreaID : string);

    { Master Control feature }
    function InsertMasterControlFeature (var AFeatureID      : integer;
                                         var ADemandCentreID : integer;
                                         AChannelNumber      : integer;
                                         AMasterType         : string): boolean;
    function DeleteMasterControlFeature (AFeatureID : integer): boolean;
    { Diversion feature }
    function InsertDiversionFeature (var AFeatureID : integer): boolean;
    function DeleteDiversionFeature (AFeatureID : integer): boolean;
    function AddDiversionProportions (AFeatureID : integer;
                                      AIndex     : integer): boolean;
    function DeleteDiversionProportions (AFeatureID : integer;
                                         AIndex     : integer): boolean;
    function AddType1_2DiversionData (AFeatureID : integer): boolean;
    function AddType3DiversionLevels (AFeatureID        : integer;
                                      AControllingResID : integer;
                                      AResLevelsCount   : integer;
                                      ARefFlowsCount    : integer): boolean;
    function AddType3DiversionProportions (AFeatureID     : integer;
                                           ARefFlowsCount : integer): boolean;
    function DivesionPreprocessorRelationshipSQL(AStationID : integer) : string;
    function GetStationIDByName(AStationNo : string) : integer;
    function ImportRelationship(AStationID : integer; AFeatureID : string;
                                ADiversionDemands,ADivertedFlows : TDivMonthlyDoublesArray) : boolean;
    { Physical Flow Constraint }
    function GetMaxPhysicalFlowConstraintIDSQL : string;
    function GetPhysicalFlowConstraintsSQL : string;
    function GetPhysicalFlowConstraintValuesSQL(ARecordID : integer) : string;
    function InsertPhysicalFlowConstraintSQL : string;
    function DeletePhysicalFlowConstraintSQL (AFeatureID : integer): string;
    function InsertPhysicalFlowConstraintValueSQL : string;
    function DeletePhysicalFlowConstraintValueSQL (AFeatureID : integer): string;

    function GetMaxPhysicalFlowConstraintID : integer;
    procedure LoadContextData_PhysicalFlowConstraint (AContextData : TStringList;
              AFeatureID, AConstraintGroup, AConstraintType, ALineNumber, AValueIndex : integer);
    function InsertPhysicalFlowConstraint (AFeature : TPhysicalFlowConstraint): boolean;
    function DeletePhysicalFlowConstraint (AFeature : TPhysicalFlowConstraint): boolean;
    function InsertPhysicalFlowConstraintValue(AFeature : TPhysicalFlowConstraint): boolean;
    function DeletePhysicalFlowConstraintValue(AFeature : TPhysicalFlowConstraint): boolean;

    { IFR feature }
    function InsertIFRFeature (var AFeatureID : integer; ACalculationOption: double;AReferenceFlowType: integer;
                               AIFRFeatureExists: integer ): boolean;
    function DeleteIFRFeature (AFeatureID : integer): boolean;
    function AddIFRDetails (AFeatureID : integer;
                            AIndex     : integer): boolean;
    function DeleteIFRDetails (AFeatureID : integer;
                               AIndex     : integer): boolean;
    function InsertIFRDetails (AInflowOption      : integer): boolean;
    { Irrigation Areas }
    function InsertIrrigationArea (var AFeatureID        : integer;
                                   ANodeNumber           : integer;
                                   ADiversionChannelNr   : integer;
                                   AConsumptiveChannelNr : integer;
                                   AReturnFlowChannelNr  : integer): boolean;
    function DeleteIrrigationArea (AFeatureID : integer): boolean;
    function CopyIrrigationAreaFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AIrrigationNodeNumberList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
    { PowerPlants }
    function InsertPowerPlant (var AFeatureID  : integer;
                               APowerChannelNr : integer;
                               ASpillChannelNr : integer): boolean;
    function DeletePowerPlant (AFeatureID : integer): boolean;
    function CopyPowerPlantsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             APowerPlantsList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;

    { Specified Inflow Features }
    function InsertSpecifiedInflowFeature (var AFeatureID : integer): boolean;
    function DeleteSpecifiedInflowFeature (AFeatureID : integer): boolean;
    { Water Demand Feature }
    function InsertWaterDemandFeature (var AFeatureID : integer): boolean;
    function InsertImplementReconciliationFile: boolean;
    function DeleteImplementReconciliationFile: boolean;
    function DeleteImplementReconciliationFileSQL: string;
    function DeleteWaterDemandFeature (AFeatureID : integer): boolean;
    function InsertWaterDemandCategory (var ACategoryID : integer): boolean;
    function DeleteWaterDemandCategory (ACategoryID : integer): boolean;
    function CreateScenarioCount: boolean;
    function CreateRiskCriteriaRecord: boolean;
    function AddWaterDemandPortionTotal(ACategoryID : integer; var ATotal : double): boolean;

    { WaterUse Output Proportions }
    function GetWaterUseProportionsSQL : string;
    function InsertWaterUseOutputProportionSQL (AChannelNumber : integer) : string;
    function InsertWaterUseOutputProportion(AChannelNo : integer) : boolean;
    function DeleteWaterUseOutputProportion (AChannelNumber : integer) : boolean;
    function UpdateWaterUsePortion(ACategoryID : integer; AChannelNumber : integer): boolean;

    { Channel Areas }
    function GetChannelAreaSQL: string;
    function InsertChannelArea (var AChannelAreaID : integer): boolean;
    function DeleteChannelArea (AChannelAreaID : integer): boolean;

    {Curtailment}
    function GetCurtailmentSQL: string;
    function InsertCurtailment(ACurtailmentPeriodCount: integer): boolean;
    function InsertCurtailmentSQL(AStartMonths: string) : string;
    function DeleteCurtailmentSQL : string;
    function DeleteCurtailment: boolean;

    {CurtailmentChannel}
    function GetCurtailmentChannelSQL : string;

    function DeleteCurtailmentChannelSQL(AChannelNumber: Integer) : string;
    function DeleteCurtailmentChannel(AChannelNumber: Integer): Boolean;

    function InsertCurtailmentChannelSQL(AIdentifier: integer;
                                         AChannelNumber: integer): string;
    function InsertCurtailmentChannel(ACurtailedChannel: TCurtailedChannel) : Boolean;

    //Drought Restriction
    function InsertDroughtRestriction (ADroughtRestriction: TDroughtRestriction): boolean;
    function DeleteDroughtRestriction(AIdentifier: integer): boolean;

    function InsertDroughtRestrictionSQL (AIdentifier : integer) : string;
    function GetDeleteDroughtRestrictionSQL(AIdentifier: integer): string;
    function GetDeleteDroughtRestrictionFactorsSQL(AIdentifier: integer): string;
    function GetDeleteDroughtRestrictionStorageVolumeSQL(AIdentifier: integer): string;

    function GetDroughtRestrictionSQL : string;
    function GetMaxDroughtRestrictionID: integer;
    function GetMaxDroughtRestrictionIDSQL : string;
    function GetDroughtRestrictionFactorsSQL(AIdentifier: integer): string;
    function GetDroughtRestrictionVolumeSQL(AIdentifier: integer): string;

    function InsertDroughtRestrictionFactorsSQL(AIdentifier: integer): string;
    function InsertDroughtRestrictionStorageVolumeSQL(AIdentifier: integer): string;

    //    function DeleteChannelAreaSQL(AChannelAreaID : integer): string;


    function InsertMasterControlFeatureSQL (AFeatureID     : integer;
                                            AMasterType    : string;
                                            AChannelNumber : integer) : string;
    function InsertDemandCentreSQL (ADemandCentreID : integer;
                                    AChannelNumber  : integer) : string;
    function DeleteMasterControlFeatureSQL (AFeatureID : integer): string;
    function GetMaxMasterControlFeatureIDSQL : string;
    function GetMaxDemandCentreIDSQL : string;

    function GetDiversionFeaturesSQL: string;
    function GetDiversionGaugeListSQL : string;

    function GetDiversionFeatureNameSQL (AChannelNumber : integer) : string;
    function GetDivType1n2DataSQL (ARecordID : integer): string;
    function GetReservoirLevelsSQL (ARecordID : integer): string;
    function GetDiversionProportionsSQL (ARecordID : integer): string;
    function InsertDiversionFeatureSQL (AFeatureID : integer) : string;
    function DeleteDiversionFeatureSQL (AFeatureID : integer): string;
    function GetMaxDiversionFeatureIDSQL : string;
    function InsertDiversionProportionsSQL : string;
    function DeleteDiversionProportionsSQL (AFeatureID : integer;
                                            AIndex     : integer): string;
    function DeleteType3DiversionProportionsSQL (AFeatureID : integer): string;
    function InsertType1_2DiversionDataSQL : string;
    function DeleteType1_2DiversionDataSQL (AFeatureID : integer) : string;
    function InsertType3DiversionLevelsSQL : string;
    function DeleteType3DiversionlevelsSQL (AFeatureID : integer) : string;

    function GetIrrigationAreasSQL : string;
    function GetMonthlyDiversionFlowsSQL (ARecordID : integer) : string;
    function GetMonthlyReturnFlowsSQL (ARecordID : integer) : string;
    function InsertIrrigationAreaSQL (AFeatureID            : integer;
                                      ANodeNumber           : integer;
                                      ADiversionChannelNr   : integer;
                                      AConsumptiveChannelNr : integer;
                                      AReturnFlowChannelNr  : integer) : string;
    function DeleteIrrigationAreaSQL (AFeatureID : integer): string;
    function InsertIrrigationDiversionSQL (AFeatureID : integer) : string;
    function DeleteIrrigationDiversionSQL (AFeatureID : integer): string;
    function InsertIrrigationReturnFlowSQL (AFeatureID : integer) : string;
    function DeleteIrrigationReturnFlowSQL (AFeatureID : integer): string;
    function GetMaxIrrigationAreaIDSQL : string;


    function GetIFRReferenceSQL: string;
    function GetIFRFeaturesSQL: string;
    function GetMonthlyInflowIFRPairsSQL (ARecordID : integer) : string;
    function InsertIFRDetailsSQL : string;
    function DeleteIFRDetailsSQL (AFeatureID : integer;
                                  AIndex     : integer): string;
    function InsertIFRFeatureSQL (AFeatureID : integer; ACalculationOption: double;AReferenceFlowType: integer;
                                  AIFRFeatureExists: integer) : string;
    function InsertIFRRefrenceSQL (AInflowOption: integer) : string;
    function DeleteIFRFeatureSQL (AFeatureID : integer): string;
    function DeleteIFRFeaturesDetailsSQL (AFeatureID : integer): string;
    function GetMaxIFRFeatureIDSQL : string;

    function GetPowerPlantDataSQL : string;
    function GetPowerPlantDetailsSQL (ARecordID : integer): string;
    function GetPowerDemandSQL (AChannelNr : integer): string;
    function GetMaxPowerPlantIDSQL : string;
    function InsertPowerPlantSQL (AFeatureID      : integer;
                                  APowerChannelNr : integer;
                                  ASpillChannelNr : integer) : string;
    function DeletePowerPlantSQL (AFeatureID : integer): string;
    function InsertPowerPlantDetailsSQL (AFeatureID  : integer;
                                         AFactorCode : integer) : string;
    function DeletePowerPlantDetailsSQL (AFeatureID : integer): string;
    function InsertPowerDemandsSQL (AFeatureID : integer;
                                    APowerCode : integer) : string;
    function DeletePowerDemandsSQL (AFeatureID : integer): string;

    function GetSpecifiedInflowDataSQL: string;
    function InsertSpecifiedInflowFeatureSQL (AFeatureID : integer) : string;
    function DeleteSpecifiedInflowFeatureSQL (AFeatureID : integer): string;
    function GetMaxSpecifiedInflowFeatureIDSQL : string;

    function GetWaterDemandCategoriesSQL: string;
    function GetWaterDemandRiskCriteriaSQL: string;
    function GetWaterDemandFeaturesSQL: string;
    function GetWaterDemandCountsSQL: string;
    function GetWaterDemandFileCreateSQL: string;
    function InsertWaterDemandFeatureSQL (AFeatureID : integer) : string;
    function DeleteWaterDemandFeatureSQL (AFeatureID : integer): string;
    function GetMaxWaterDemandFeatureIDSQL : string;
    function GetMaxWaterDemandCategoryIDSQL : string;
    function InsertWaterDemandCategorySQL (ACategoryID : integer) : string;
    function DeleteWaterDemandCategorySQL (ACategoryID : integer): string;
    function InsertChannelAreaSQL (AChannelAreaID : integer) : string;
    function DeleteChannelAreaSQL (AChannelAreaID : integer): string;
    function GetMaxChannelAreaSQL : string;
    function GetMaxCurtailedChannelSQL : string;
    function GetMaxDroughtRestrictionSQL : string;

    function GetChannelByNumberSQL  (AChannelNr : integer) : string;
    function InsertImplementReconciliationFileSQL : string;
  end;

implementation

uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UUtilities,
  UDataSetType,
  UConstants,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UErrorHandlingOperations,
  DB;


function TNetworkFeaturesSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetScenarioWhereClause';
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

function TNetworkFeaturesSQLAgent.GetDivesionPreprocessorWhereClause : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDivesionPreprocessorWhereClause';
begin
  try
   Result := ' (A.Model         = ' + QuotedStr(CDailyDiversion)     + ') AND ' +
             ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ')';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{******************************************************************************}
{* Diversion Features                                                         *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetDiversionGaugeListSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDiversionGaugeListSQL';
begin
  Result := '';
  try
    Result :=  ' SELECT StationNo FROM DailyDiversionStation A WHERE '+
               GetDivesionPreprocessorWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDiversionFeaturesSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDiversionFeaturesSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'ChannelName, ChannelNumber, ChannelSubType, ChannelType, ' +
              'UpNodeNumber, DownNodeNumber, PenaltyNumber, SummaryOutput, FirmYieldCalc, FlowOutput,ChannelAreaID ' +
              'FROM ChannelDetails A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.ChannelType = 5) ' +
              ' ORDER BY A.ChannelNumber ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDiversionFeatureNameSQL (AChannelNumber : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDiversionFeatureNameSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'DivChannelNumber, DivChannelName, DivChannelType,DivStation ' +
              'FROM DiversionFeatures A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.DivChannelNumber = ' + IntToStr(AChannelNumber) + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDivType1n2DataSQL (ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDivType1n2DataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'DiversionCode, DivFactor01, DivFactor02, DivFactor03, DivFactor04, ' +
              'DivFactor05, DivFactor06, DivFactor07, DivFactor08, DivFactor09, ' +
              'DivFactor10, DivFactor11, DivFactor12 ' +
              'FROM DiversionFeaturesType1n2 A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetReservoirLevelsSQL (ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetReservoirLevelsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'NodeNumber, ResLevelsCount, RefFlowsCount, ' +
              'DivLevel01, DivLevel02, DivLevel03, DivLevel04, DivLevel05, DivLevel06, ' +
              'DivLevel07, DivLevel08, DivLevel09, DivLevel10, DivLevel11, DivLevel12 ' +
              'FROM DiversionFeaturesType3 A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDiversionProportionsSQL (ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDiversionProportionsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, DiversionIndex, FlowValue, '+
              'DivProp01, DivProp02, DivProp03, DivProp04, DivProp05, DivProp06, ' +
              'DivProp07, DivProp08, DivProp09, DivProp10, DivProp11, DivProp12 ' +
              'FROM DiversionFeaturesType3Proportions A WHERE '+
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ' +
              ' ORDER BY A.DiversionIndex';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertDiversionFeatureSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDiversionFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO DiversionFeatures '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'DivChannelName, DivChannelNumber, DivChannelType) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.DiversionFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0,1)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteDiversionFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteDiversionFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DiversionFeatures A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxDiversionFeatureIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDiversionFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM DiversionFeatures A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertDiversionProportionsSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDiversionProportionsSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO DiversionFeaturesType3Proportions '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'DiversionIndex, FlowValue, DivProp01, DivProp02, DivProp03, DivProp04, ' +
    'DivProp05, DivProp06, DivProp07, DivProp08, DivProp09, DivProp10, ' +
    'DivProp11, DivProp12) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ADiversionIndex, 0, 0,0,0,0,0,0,0,0,0,0,0,0)' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteDiversionProportionsSQL (AFeatureID : integer;
                                                                 AIndex     : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteDiversionProportionsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DiversionFeaturesType3Proportions A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')' +
              ' AND (A.DiversionIndex = ' + IntToStr(AIndex) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteType3DiversionProportionsSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteType3DiversionProportionsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DiversionFeaturesType3Proportions A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertType1_2DiversionDataSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertType1_2DiversionDataSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO DiversionFeaturesType1n2 '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'DiversionCode, DivFactor01, DivFactor02, DivFactor03, DivFactor04, ' +
    'DivFactor05, DivFactor06, DivFactor07, DivFactor08, DivFactor09, ' +
    'DivFactor10, DivFactor11, DivFactor12) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ADiversionCode, 0,0,0,0,0,0,0,0,0,0,0,0)' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteType1_2DiversionDataSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteType1_2DiversionDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DiversionFeaturesType1n2 A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertType3DiversionLevelsSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertType3DiversionLevelsSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO DiversionFeaturesType3 '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'NodeNumber, ResLevelsCount, RefFlowsCount) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ANodeNumber, :AResLevelsCount, :ARefFlowsCount)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteType3DiversionlevelsSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteType3DiversionlevelsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DiversionFeaturesType3 A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* Irrigation Areas                                                           *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetIrrigationAreasSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetIrrigationAreasSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'AreaName, IrrigationNodeNumber, DiversionChannelNumber, ' +
              'ConsumptiveChannelNumber, ReturnFlowChannelNumber, RelaxationDemand ' +
              'FROM IrrigationAreas A WHERE ' +
              GetScenarioWhereClause +
              ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMonthlyDiversionFlowsSQL (ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMonthlyDiversionFlowsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'DFlow01, DFlow02, DFlow03, DFlow04, DFlow05, DFlow06, DFlow07, ' +
              'DFlow08, DFlow09, DFlow10, DFlow11, DFlow12 ' +
              'FROM IrrigationAreasDiversionFlow A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMonthlyReturnFlowsSQL (ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMonthlyReturnFlowsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'RFlow01, RFlow02, RFlow03, RFlow04, RFlow05, RFlow06, RFlow07, ' +
              'RFlow08, RFlow09, RFlow10, RFlow11, RFlow12 ' +
              'FROM IrrigationAreasReturnFlow A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertIrrigationAreaSQL (AFeatureID            : integer;
                                                           ANodeNumber           : integer;
                                                           ADiversionChannelNr   : integer;
                                                           AConsumptiveChannelNr : integer;
                                                           AReturnFlowChannelNr  : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIrrigationAreaSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO IrrigationAreas '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'AreaName, IrrigationNodeNumber, DiversionChannelNumber, ConsumptiveChannelNumber, ' +
      'ReturnFlowChannelNumber, RelaxationDemand) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.IrrigationArea')) + ' ' + IntToStr(AFeatureID)) +
      ',' + IntToStr(ANodeNumber) +
      ',' + IntToStr(ADiversionChannelNr) +
      ',' + IntToStr(AConsumptiveChannelNr) +
      ',' + IntToStr(AReturnFlowChannelNr) +
      ',0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIrrigationAreaSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIrrigationAreaSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationAreas A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertIrrigationDiversionSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIrrigationDiversionSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO IrrigationAreasDiversionFlow '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'DFlow01, DFlow02, DFlow03, DFlow04, DFlow05, DFlow06, ' +
      'DFlow07, DFlow08, DFlow09, DFlow10, DFlow11, DFlow12) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) +
      ',0,0,0,0,0,0,0,0,0,0,0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIrrigationDiversionSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIrrigationDiversionSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationAreasDiversionFlow A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertIrrigationReturnFlowSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIrrigationReturnFlowSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO IrrigationAreasReturnFlow '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'RFlow01, RFlow02, RFlow03, RFlow04, RFlow05, RFlow06, ' +
      'RFlow07, RFlow08, RFlow09, RFlow10, RFlow11, RFlow12) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) +
      ',0,0,0,0,0,0,0,0,0,0,0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIrrigationReturnFlowSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIrrigationReturnFlowSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IrrigationAreasReturnFlow A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxIrrigationAreaIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxIrrigationAreaIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM IrrigationAreas A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* Physical Flow Constraints                                                  *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetPhysicalFlowConstraintsSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetPhysicalFlowConstraintsSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Identifier AS RecordIdentifier, ' +
      'ConstraintsChannelNumber, FeatureName, UpStreamReservoirNumber, ' +
      'DownStreamReservoirNumber, PointsElevationNumber, SillElevation, ' +
      'GateHeight, StructureType, DischargeCoefficient, ControlStructureLength,  ' +
      'WaterLevelAtDownstreamNode, ReferenceElevation  '+

      'FROM FlowConstraints A WHERE ' +
      GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetPhysicalFlowConstraintValuesSQL(ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetPhysicalFlowConstraintValuesSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Identifier AS RecordIdentifier, '+
      ' GroupNumber,SubGroupNumber, LineNumber, '+
      ' Value01, Value02, Value03, Value04, Value05, '+
      ' Value06, Value07, Value08, Value09, Value10  '+
      ' FROM FlowConstraintsValue A WHERE ' +
      GetScenarioWhereClause +
      ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') '+
      ' ORDER BY A.Identifier ,A.GroupNumber, A.SubGroupNumber, A.LineNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraintSQL  : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraintSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO FlowConstraints ' +
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ConstraintsChannelNumber, FeatureName, UpStreamReservoirNumber, ' +
      'DownStreamReservoirNumber, PointsElevationNumber, SillElevation, ' +
      'GateHeight, StructureType, DischargeCoefficient, ControlStructureLength, ' +
      'WaterLevelAtDownStreamNode, ReferenceElevation )' +

      'Values'+
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ':ConstraintsChannelNumber, :FeatureName, :UpStreamReservoirNumber, ' +
      ':DownStreamReservoirNumber, :PointsElevationNumber, :SillElevation, ' +
      ':GateHeight, :StructureType, :DischargeCoefficient, :ControlStructureLength, '+
      ':WaterLevelAtDownstreamNode, :ReferenceElevation )';                                 
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraintSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraintSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM FlowConstraints A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraintValueSQL(AFeatureID: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraintValueSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM FlowConstraintsValue A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraintValueSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraintValueSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO FlowConstraintsValue ' +
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      ' GroupNumber,SubGroupNumber, LineNumber, '+
      ' Value01, Value02, Value03, Value04, Value05, '+
      ' Value06, Value07, Value08, Value09, Value10  )'+
      ' Values '+
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ' :GroupNumber, :SubGroupNumber, :LineNumber, '+
      ' :Value01, :Value02, :Value03, :Value04, :Value05, '+
      ' :Value06, :Value07, :Value08, :Value09, :Value10  )';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxPhysicalFlowConstraintIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxPhysicalFlowConstraintIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM FlowConstraints A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* IFR Features                                                               *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetIFRReferenceSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetIFRReferenceSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT InflowOption,ChannelCount ' +
      'FROM IFRReference A WHERE ' +
      GetScenarioWhereClause ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetIFRFeaturesSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetIFRFeaturesSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Identifier AS RecordIdentifier,' +
      ' IFRChannelNumber, FeatureName, ReferenceNodeCount, LagInMonthsCount, PointsCount,' +
      ' RefNodeNumbers,CalculationOption,IFRSiteID,ExceedencePerc,ReferenceFlowType, IFRStatusIndicator, IFRLoss,MonthlyIFRLoss' +
      ' FROM IFRFeatures A WHERE ' +
        GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMonthlyInflowIFRpairsSQL (ARecordID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMonthlyInflowIFRpairsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, LineNumber,AnnualInflow, ' +
              'InflowVar01, InflowVar02, InflowVar03, InflowVar04, InflowVar05, ' +
              'InflowVar06, InflowVar07, InflowVar08, InflowVar09, InflowVar10, ' +
              'InflowVar11, InflowVar12, ReleaseVar01, ReleaseVar02, ReleaseVar03, ' +
              'ReleaseVar04, ReleaseVar05, ReleaseVar06, ReleaseVar07, ReleaseVar08, ' +
              'ReleaseVar09, ReleaseVar10, ReleaseVar11, ReleaseVar12 ' +
              'FROM IFRFeaturesDetails A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ' +
              'ORDER BY LineNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertIFRDetailsSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIFRDetailsSQL';
begin
  Result := '';
  try
    Result :=
    'INSERT INTO IFRFeaturesDetails '+
    '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
    'LineNumber) ' +
    'VALUES '+
    '(:AModelCode, :AStudyAreaCode, :ASubAreaCode, :AScenarioCode, ' +
    ':AIdentifier, :ALineNumber)' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIFRDetailsSQL (AFeatureID : integer;
                                                       AIndex     : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIFRDetailsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IFRFeaturesDetails A WHERE ' +
               GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AFeatureID) + ')' +
              ' AND (A.LineNumber = ' + IntToStr(AIndex) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertIFRRefrenceSQL(AInflowOption: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIFRRefrenceSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO IFRReference '+
      '(Model, StudyAreaName, SubArea, Scenario, InflowOption,ChannelCount) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
      IntToStr(AInflowOption) + ',0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertIFRFeatureSQL (AFeatureID : integer; ACalculationOption: double;
         AReferenceFlowType: integer; AIFRFeatureExists: integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIFRFeatureSQL';
begin
  Result := '';
  try
    if(ACalculationOption >= 0) then
    Result :=
      'INSERT INTO IFRFeatures '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'IFRChannelNumber, FeatureName, ReferenceNodeCount, LagInMonthsCount, PointsCount,CalculationOption,IFRSiteID,ReferenceFlowType, IFRStatusIndicator) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
      IntToStr(AFeatureID) +
      ',0,' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.IFRFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0,0,0,'+ FloatToStr(ACalculationOption) + ',0,'+ IntToStr(AReferenceFlowType)+ ',0'+')'
    else
    Result :=
      'INSERT INTO IFRFeatures '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'IFRChannelNumber, FeatureName, ReferenceNodeCount, LagInMonthsCount, PointsCount,IFRSiteID,ReferenceFlowType, IFRStatusIndicator) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
      IntToStr(AFeatureID) +
      ',0,' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.IFRFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0,0,0,0,'+ IntToStr(AReferenceFlowType)+ ','+IntToStr(AIFRFeatureExists)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIFRFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIFRFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IFRFeatures A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIFRFeaturesDetailsSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIFRFeaturesDetailsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM IFRFeaturesDetails A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxIFRFeatureIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxIFRFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM IFRFeatures A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* Power Plants                                                               *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetPowerPlantDataSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetPowerPlantDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'PowerPlantName, PowerChannelNumber, SpillChannelNumber, MaxCapGenerator, ' +
              'MaxCapTurbine, Efficiency, PowerPlantStatus, HeadLoss, DesignHead, ' +
              'MaxNetHead, MinNetHead, PointsCount, TailWaterCount, TailWaterTypeCode, ' +
              'DownStreamPowerChannelCount, Channel01, Channel02, Channel03, Channel04, ' +
              'Channel05, Channel06, Channel07, Channel08, Channel09, Channel10, Channel11, ' +
              'Channel12, Channel13, Channel14, Channel15, Channel16, Channel17, Channel18, ' +
              'Channel19, Channel20 ' +
              'FROM PowerPlants A WHERE ' +
              GetScenarioWhereClause +
              ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetChannelByNumberSQL (AChannelNr : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetChannelByNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ChannelType, ChannelSubType, ' +
              'ChannelName, ChannelNumber, UpNodeNumber, DownNodeNumber, ' +
              'PenaltyNumber, SummaryOutput, FirmYieldCalc, FlowOutput,ChannelAreaID ' +
              'FROM ChannelDetails A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.ChannelNumber = ' + IntToStr(AChannelNr) + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetPowerPlantDetailsSQL (ARecordID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetPowerPlantDetailsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, ' +
              'FactorCode, Factor01, Factor02, Factor03, Factor04, Factor05, ' +
              'Factor06, Factor07, Factor08, Factor09, Factor10 ' +
              'FROM PowerPlantsDetails A WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(ARecordID) + ') ' +
              'ORDER BY FactorCode ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetPowerDemandSQL (AChannelNr : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetPowerDemandSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Identifier AS RecordIdentifier, PowerChannelNumber, ' +
              'PowerCode, MinPower01, MinPower02, MinPower03, MinPower04, ' +
              'MinPower05, MinPower06, MinPower07, MinPower08, MinPower09, ' +
              'MinPower10, MinPower11, MinPower12 ' +
              'FROM PowerPlantsDemands AS A, PowerPlants AS B WHERE ' +
              GetScenarioWhereClause +
              ' AND (A.Identifier = B.Identifier) ' +
              ' AND (PowerChannelNumber = ' + IntToStr(AChannelNr) + ') ' +
              'ORDER BY PowerCode ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertPowerPlantSQL (AFeatureID      : integer;
                                                       APowerChannelNr : integer;
                                                       ASpillChannelNr : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPowerPlantSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO PowerPlants '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'PowerPlantName, PowerChannelNumber, SpillChannelNumber, MaxCapGenerator, ' +
      'MaxCapTurbine, Efficiency, PowerPlantStatus, HeadLoss, DesignHead, ' +
      'MaxNetHead, MinNetHead, PointsCount, TailwaterCount, TailwaterTypeCode, ' +
      'DownStreamPowerChannelCount) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.PowerPlant')) + ' ' + IntToStr(AFeatureID)) + ',' +
      IntToStr(APowerChannelNr) + ',' +
      IntToStr(ASpillChannelNr) +
      ',0,0,0,1,0,0,0,0,0,0,1,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeletePowerPlantSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePowerPlantSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM PowerPlants A WHERE ' +
                GetScenarioWhereClause +
                ' AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertPowerPlantDetailsSQL (AFeatureID  : integer;
                                                              AFactorCode : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPowerPlantDetailsSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO PowerPlantsDetails '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'FactorCode) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      IntToStr(AFactorCode) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeletePowerPlantDetailsSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePowerPlantDetailsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM PowerPlantsDetails A WHERE ' +
                GetScenarioWhereClause +
                ' AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertPowerDemandsSQL (AFeatureID : integer;
                                                         APowerCode : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPowerDemandsSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO PowerPlantsDemands '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, PowerCode, ' +
      'MinPower01, MinPower02, MinPower03, MinPower04, MinPower05, MinPower06, ' +
      'MinPower07, MinPower08, MinPower09, MinPower10, MinPower11, MinPower12) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      IntToStr(APowerCode) +
      ',0,0,0,0,0,0,0,0,0,0,0,0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeletePowerDemandsSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePowerDemandsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM PowerPlantsDemands A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxPowerPlantIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxPowerPlantIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM PowerPlants A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* Specified Inflow                                                           *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetSpecifiedInflowDataSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetSpecifiedInflowDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT A.Identifier AS RecordIdentifier, ' +
              'A.ChannelName, A.ChannelNumber, A.ChannelType, ' +
              'A.ChannelSubType, A.UpNodeNumber, A.DownNodeNumber, ' +
              'A.PenaltyNumber, A.SummaryOutput, A.FirmYieldCalc, A.ChannelAreaID, ' +
              'B.Identifier, B.FeatureName, B.InflowFileName, A.FlowOutput ' +
              'FROM ChannelDetails A, SpecifiedInflowFeature B WHERE ' +
              GetScenarioWhereClause + ' AND ' +
              ' (A.ChannelType = 10) AND ' +
              ' (A.Model         = B.Model) AND ' +
              ' (A.StudyAreaName = B.StudyAreaName) AND ' +
              ' (A.SubArea       = B.SubArea) AND ' +
              ' (A.Scenario      = B.Scenario) AND ' +
              ' (A.ChannelNumber = B.ChannelNumber) ' +
              ' ORDER BY A.ChannelNumber ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertSpecifiedInflowFeatureSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertSpecifiedInflowFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO SpecifiedInflowFeature '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'FeatureName, ChannelNumber) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ','+
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.SpecifiedInflow')) + ' ' + IntToStr(AFeatureID)) +
      ',0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteSpecifiedInflowFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteSpecifiedInflowFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM SpecifiedInflowFeature A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxSpecifiedInflowFeatureIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxSpecifiedInflowFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM SpecifiedInflowFeature A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertMasterControlFeatureSQL (AFeatureID     : integer;
                                                                 AMasterType    : string;
                                                                 AChannelNumber : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertMasterControlFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO MasterControlFeature '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ChannelNumber, FeatureName, MasterControlType, Value01, Value02, ' +
      'Value03, Value04, Value05, Value06, Value07, Value08, Value09, Value10, ' +
      'Value11, Value12) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AFeatureID) + ',' +
      IntToStr(AChannelNumber) + ',' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.MasterControlFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',' + QuotedStr(Trim(UpperCase(AMasterType))) +
      ',1,1,1,1,1,1,1,1,1,1,1,1)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertDemandCentreSQL (ADemandCentreID : integer;
                                                         AChannelNumber  : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDemandCentreSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO DemandCentre '+
      '(Model, StudyAreaName, SubArea, Scenario, DemandCentreID, ' +
      'ChannelNumber, DemandCentreType, AnnualDemand, MinimumDemand, IncludeInOutput) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(ADemandCentreID) + ','+
      IntToStr(AChannelNumber) +  ',' +
      QuotedStr('D') + ',0.0,0.0,' + QuotedStr('N') + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNetworkFeaturesSQLAgent.DeleteMasterControlFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteMasterControlFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM MasterControlFeature A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxMasterControlFeatureIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxMasterControlFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM MasterControlFeature A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxDemandCentreIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDemandCentreIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(DemandCentreID) AS MaxDemandCentreID FROM DemandCentre A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TNetworkFeaturesSQLAgent.LoadContextData_FeatureID (AContextData : TStringList;
                                                               AFeatureID   : string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_FeatureID';
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

procedure TNetworkFeaturesSQLAgent.LoadContextData_WaterFeatureID (AContextData : TStringList;
                                                               AFeatureID   : string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_WaterFeatureID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('FeatureID='  + AFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData(AContextData: TStringList);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;



procedure TNetworkFeaturesSQLAgent.LoadContextData_FieldNameID
                                                   (AContextData : TStringList;
                                                    AFieldNameID : string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_FieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_WaterUseOutputProportion (AContextData : TStringList;
                                                                             AChannelNr ,AFieldNameID : string );
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_WaterUseOutputProportion';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ChannelNumber='      + AChannelNr);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TNetworkFeaturesSQLAgent.LoadContextData_FeatureIDFieldNameID
                                                   (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_FeatureIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AFeatureID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_WaterFeatureIDFieldNameID(
  AContextData: TStringList; AFeatureID, AFieldNameID: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_WaterFeatureIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('FeatureID='  + AFeatureID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_ChannelID (AContextData : TStringList;
                                                               AChannelID   : string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_ChannelID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='             + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='     + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='           + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='          + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='        + AChannelID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_ChannelIDFieldNameID
                                                   (AContextData : TStringList;
                                                    AChannelID   : string;
                                                    AFieldNameID : string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_ChannelIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='             + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='     + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='           + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='          + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='        + AChannelID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_DroughtRestrictionIdentifier(AContextData: TStringList;
                                                                                AIdentifier: string;
                                                                                AFieldNameIdentifier: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_DroughtRestrictionIdentifier';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='+FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='+FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='+FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='+ AIdentifier);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadCurtailmentDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadCurtailmentDataContextData';
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


function TNetworkFeaturesSQLAgent.InsertMasterControlFeature (var AFeatureID      : integer;
                                                              var ADemandCentreID : integer;
                                                              AChannelNumber      : integer;
                                                              AMasterType         : string): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertMasterControlFeature';
var
  LDataSet    : TAbstractModelDataset;
  LFeatureID  : integer;
  LImportDate : TDateTime;
  lDemandCentreID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LFeatureID := GetMaxMasterControlFeatureID + 1;
          LDataSet.SetSQL(InsertMasterControlFeatureSQL(LFeatureID, AMasterType, AChannelNumber));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          lDemandCentreID := 0;
          if (FAppModules.Model.ModelName = CPlanning) then
          begin
            lDemandCentreID := GetMaxDemandCentreID + 1;
            LDataSet.SetSQL(InsertDemandCentreSQL(LFeatureID, AChannelNumber));
            LDataset.ExecSQL;
            LDataset.DataSet.Close;
          end;

          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = nullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          AFeatureID := LFeatureID;
          ADemandCentreID := lDemandCentreID;
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

function TNetworkFeaturesSQLAgent.DeleteMasterControlFeature (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteMasterControlFeature';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lChannelNr  : integer;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'SELECT * FROM MasterControlFeature A WHERE ' +
                GetScenarioWhereClause +
                'AND A.Identifier = '+ IntToStr(AFeatureID);
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if (NOT LDataSet.DataSet.Eof) then
        begin
          lChannelNr := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LDataSet.DataSet.Close;
          FAppModules.Database.StartTransaction;
          try
            LDataSet.SetSQL(DeleteMasterControlFeatureSQL(AFeatureID));
            LDataset.ExecSQL;
            LDataset.DataSet.Close;

            lSQL := 'DELETE FROM DemandCentre A WHERE ' +
                GetScenarioWhereClause +
                'AND A.ChannelNumber = ' + IntToStr(lChannelNr);
            LDataSet.SetSQL(lSQL);
            LDataset.ExecSQL;
            LDataset.DataSet.Close;

            FAppModules.Database.Commit;
            Result := TRUE;
          except
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
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

function TNetworkFeaturesSQLAgent.GetMaxMasterControlFeatureID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxMasterControlFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxMasterControlFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxDemandCentreID: integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDemandCentreID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxDemandCentreIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxDemandCentreID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNetworkFeaturesSQLAgent.InsertDiversionFeature (var AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDiversionFeature';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxDiversionFeatureID + 1;

        LDataSet.SetSQL(InsertDiversionFeatureSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = nullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := AddType1_2DiversionData(LFeatureID);
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteDiversionFeature (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteDiversionFeature';
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
        LDataSet.SetSQL(DeleteDiversionFeatureSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteType1_2DiversionDataSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteType3DiversionlevelsSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteType3DiversionProportionsSQL(AFeatureID));
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

function TNetworkFeaturesSQLAgent.GetMaxDiversionFeatureID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDiversionFeatureID';
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

function TNetworkFeaturesSQLAgent.AddDiversionProportions (AFeatureID : integer;
                                                            AIndex     : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.AddDiversionProportions';
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
        LDataSet.SetSQL(InsertDiversionProportionsSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);
        LDataSet.SetParams(['AIdentifier','ADiversionIndex'],
                           [IntToStr(AFeatureID), IntToStr(AIndex)]);
        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        end;
      end;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteDiversionProportions (AFeatureID : integer;
                                                               AIndex     : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteDiversionProportions';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteDiversionProportionsSQL(AFeatureID, AIndex));
        LDataset.ExecSQL;
        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

      end;
      LDataset.DataSet.Close;
     Result := True;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.AddType1_2DiversionData (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.AddType1_2DiversionData';
var
  LDataSet : TAbstractModelDataset;
  lCode    : integer;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetDivType1n2DataSQL(AFeatureID));
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount = 0) then
        begin
          LDataSet.ClearSQL ;
          LDataSet.SetSQL(InsertType1_2DiversionDataSQL);
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          for lCode := 1 to 2 do
          begin
            LDataSet.SetParams(['AIdentifier','ADiversionCode'],
                               [IntToStr(AFeatureID), IntToStr(lCode)]);
            if LDataset.AreAllParamsBound then
            begin
              LDataset.ExecSQL;
              LDataset.DataSet.Close;
              FAppModules.StudyArea.LastUpdateDate := Now();

               LImportDate := FAppModules.StudyArea.GetStudyImportDate;
               if LImportDate = nullDateTime then
                 FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            end;
          end;
        end;
      end;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.AddType3DiversionLevels (AFeatureID        : integer;
                                                            AControllingResID : integer;
                                                            AResLevelsCount   : integer;
                                                            ARefFlowsCount    : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.AddType3DiversionLevels';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetReservoirLevelsSQL(AFeatureID));
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount = 0) then
        begin
          LDataSet.ClearSQL ;
          LDataSet.SetSQL(InsertType3DiversionLevelsSQL);
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          LDataSet.SetParams(
            ['AIdentifier','ANodeNumber', 'AResLevelsCount', 'ARefFlowsCount'],
            [IntToStr(AFeatureID), IntToStr(AControllingResID),
             IntToStr(AResLevelsCount),
             IntToStr(ARefFlowsCount)]);
          if LDataset.AreAllParamsBound then
          begin
            LDataset.ExecSQL;
            LDataset.DataSet.Close;
            FAppModules.StudyArea.LastUpdateDate := Now();

             LImportDate := FAppModules.StudyArea.GetStudyImportDate;
             if LImportDate = nullDateTime then
               FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          end;
        end;
      end;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.AddType3DiversionProportions (AFeatureID     : integer;
                                                                 ARefFlowsCount : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.AddType3DiversionProportions';
var
  LDataSet : TAbstractModelDataset;
  lIndex   : integer;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetDiversionProportionsSQL(AFeatureID));
        LDataset.DataSet.Open;
        for lIndex := LDataset.DataSet.RecordCount + 1 to ARefFlowsCount do
        begin
          LDataSet.ClearSQL ;
          LDataSet.SetSQL(InsertDiversionProportionsSQL);
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          LDataSet.SetParams(['AIdentifier','ADiversionIndex'],
                             ['0', IntToStr(AFeatureID), IntToStr(lIndex)]);
          if LDataset.AreAllParamsBound then
          begin
            LDataset.ExecSQL;
            LDataset.DataSet.Close;
            FAppModules.StudyArea.LastUpdateDate := Now();

             LImportDate := FAppModules.StudyArea.GetStudyImportDate;
             if LImportDate = nullDateTime then
               FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          end;
        end;
      end;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.ImportRelationship(AStationID : integer;AFeatureID : string;
                                                     ADiversionDemands,ADivertedFlows : TDivMonthlyDoublesArray) : boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.ImportRelationship';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LIndex : integer;
  LMesgDlgResult  : Word;
  LMessage : string;
  LNonDiv : Boolean;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) and (Length(ADiversionDemands)>0) and (Length(ADivertedFlows)>0) then
      begin
        LDataSet.SetSQL(DivesionPreprocessorRelationshipSQL(AStationID));
        LDataset.DataSet.Open;
        LDataset.DataSet.Last;
        LDataset.DataSet.First;
        LNonDiv := False;
        if LDataset.DataSet.RecordCount>0 then
        begin
          LMessage := FAppModules.Language.GetString('Message.NonOrDivImport');
          LMesgDlgResult  := WRMFMessageDialog(LMessage,
          mtConfirmation,mbYesNoCancel,[FAppModules.Language.GetString('VNV.Diversion'),
          FAppModules.Language.GetString('Message.NonDiv')]);
          if (LMesgDlgResult = mrCancel) then
            Exit;
          LNonDiv := (LMesgDlgResult = mrNo);
        end;

        LIndex := 1;
        while not LDataset.DataSet.Eof do
        begin
          ADiversionDemands[LIndex] := LDataset.DataSet.FieldByName('ReferenceFlow').AsFloat;
          if LNonDiv then
            ADivertedFlows[LIndex] := LDataset.DataSet.FieldByName('NonDiversionFlow').AsFloat
          else
            ADivertedFlows[LIndex] := LDataset.DataSet.FieldByName('DiversionFlow').AsFloat;

          LIndex := LIndex +1;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = nullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
      end;
      Result := True;
    finally
      FreeAndNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TNetworkFeaturesSQLAgent.DivesionPreprocessorRelationshipSQL(AStationID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DivesionPreprocessorRelationshipSQL';
begin
  Result := '';
  try
    Result :=  ' SELECT ReferenceFlow,DiversionFlow,NonDiversionFlow FROM DailyDiversionWRYMData A WHERE '+
               GetDivesionPreprocessorWhereClause+
               ' AND StationID = ' + IntToStr(AStationID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TNetworkFeaturesSQLAgent.GetStationIDByName(AStationNo : string) : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetStationIDByName';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetStationIDByNameSQL(AStationNo));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('StationID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNetworkFeaturesSQLAgent.GetStationIDByNameSQL(AStationNo : string) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetStationIDByNameSQL';
begin
  Result := '';
  try
    Result :=  ' SELECT StationID FROM DailyDiversionStation A WHERE '+
               GetDivesionPreprocessorWhereClause+
               ' AND StationNo = ' + QuotedStr(AStationNo);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_PhysicalFlowConstraint(AContextData: TStringList;
          AFeatureID, AConstraintGroup, AConstraintType, ALineNumber, AValueIndex: integer);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_PhysicalFlowConstraint';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='               + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='       + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='             + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='            + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='          + IntToStr(AFeatureID));
    AContextData.Add('GroupNumber='         + IntToStr(AConstraintGroup));
    AContextData.Add('SubGroupNumber='      + IntToStr(AConstraintType));
    AContextData.Add('LineNumber='          + IntToStr(ALineNumber));
    AContextData.Add('FieldNameIdentifier=' + IntToStr(AValueIndex));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraint (AFeature   : TPhysicalFlowConstraint): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraint';
var
  LDataSet   : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(InsertPhysicalFlowConstraintSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(AFeature.FeatureID)]);
        LDataSet.SetParams(['FeatureName'], [AFeature.FeatureName]);
        LDataSet.SetParams(['ConstraintsChannelNumber'], [IntToStr(AFeature.ChannelNumber)]);
        LDataSet.SetParams(['UpStreamReservoirNumber'], [IntToStr(AFeature.UpstreamReservoirNr)]);
        LDataSet.SetParams(['DownStreamReservoirNumber'], [IntToStr(AFeature.DownstreamReservoirNr)]);
        LDataSet.SetParams(['PointsElevationNumber'], [IntToStr(AFeature.NrOfPoints)]);
        LDataSet.SetParams(['SillElevation'], [FloatToStr(AFeature.ElevationOfSill)]);
        LDataSet.SetParams(['GateHeight'], [FloatToStr(AFeature.MaximumGateHeight)]);
        LDataSet.SetParams(['StructureType'], [IntToStr(AFeature.StructureType)]);
        LDataSet.SetParams(['DischargeCoefficient'], [FloatToStr(AFeature.DischargeCoefficient)]);
        LDataSet.SetParams(['ControlStructureLength'], [FloatToStr(AFeature.StructureLength)]);
        LDataSet.SetParams(['WaterLevelAtDownstreamNode'], [FloatToStr(AFeature.Get_WaterLevelAtDownstreamNode)]);
        LDataSet.SetParams(['ReferenceElevation'], [FloatToStr(AFeature.ReferenceElevation)]);
        LDataSet.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraint (AFeature : TPhysicalFlowConstraint): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraint';
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
        LDataSet.SetSQL(DeletePhysicalFlowConstraintSQL(AFeature.FeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeletePhysicalFlowConstraintValueSQL(AFeature.FeatureID));
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

function TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraintValue(AFeature: TPhysicalFlowConstraint): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePhysicalFlowConstraintValue';
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
        LDataSet.SetSQL(DeletePhysicalFlowConstraintValueSQL(AFeature.FeatureID));
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

function TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraintValue(AFeature: TPhysicalFlowConstraint): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPhysicalFlowConstraintValue';
procedure PrepareInsertValues(ADataSet : TAbstractModelDataset;AFeature: TPhysicalFlowConstraint);
const OPNAME = 'UNetworkFeaturesSQLAgent.PrepareInsertValues';
begin
  ADataSet.DataSet.Close;
  ADataSet.SetSQL(InsertPhysicalFlowConstraintValueSQL);
  ADataSet.ClearQueryParams();
  ADataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
  ADataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
  ADataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
  ADataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
  ADataSet.SetParams(['Identifier'], [IntToStr(AFeature.FeatureID)]);
end;
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LRow,LCol   : integer;
begin
  Result := False;
  try
    if not (AFeature.StructureType in [4,5,7,8,9,10,11,12,13,14]) then
    begin
      Result := True;
      Exit;
    end;

    DeletePhysicalFlowConstraintValue(AFeature);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        case AFeature.StructureType of
          4,5,7,8,9,14:
          begin
            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgDischargeCurve)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstElevation)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.DischargeCurve.ElevationByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgDischargeCurve)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstDischarge)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.DischargeCurve.DischargeByIndex[LCol])]);
            LDataSet.ExecSQL;
          end;

          10:
          begin
            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgKFactors)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstChannelNumber)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.KFactors.ChannelNumberByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgKFactors)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstKFactor)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.KFactors.KFactorByIndex[LCol])]);
            LDataSet.ExecSQL;
          end;

          11:
          begin
            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstHeadDifference)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SandAquifer.HeadDifferenceByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstAquiferFlow)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SandAquifer.AquiferFlowByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstDownStreamNodeInflow)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SandAquifer.DownStreamNodeInflowByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstRiverDepth)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SandAquifer.RiverDepthByIndex[LCol])]);
            LDataSet.ExecSQL;
          end;

          12:
          begin
            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSubmergedOutlet)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstElevationDifference)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to 10 do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SubmergedOutlet.ElevationDifferenceByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSubmergedOutlet)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstMonthlyAverageInflow)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to 10 do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SubmergedOutlet.MonthlyAverageInflowByIndex[LCol])]);
            LDataSet.ExecSQL;

            for LRow := 1 to 10 do
            begin
              PrepareInsertValues(LDataSet,AFeature);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSubmergedOutlet)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstMonthlyAverageDivertedFlow)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(LRow)]);
              for LCol := 1 to 10 do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.SubmergedOutlet.MonthlyAverageDivertedFlowByIndex[LRow,LCol])]);
              LDataSet.ExecSQL;
            end;
          end;

          13:
          begin
            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgPumpStation)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstPumpingHead)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.PumpStation.PumpingHeadByIndex[LCol])]);
            LDataSet.ExecSQL;

            PrepareInsertValues(LDataSet,AFeature);
            LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgPumpStation)]);
            LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstPumpingDischarge)]);
            LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
            for LCol := 1 to AFeature.NrOfPoints do
              LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(AFeature.PumpStation.PumpingDischargeByIndex[LCol])]);
            LDataSet.ExecSQL;
          end;
        end;

        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxPhysicalFlowConstraintID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxPhysicalFlowConstraintID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxPhysicalFlowConstraintIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.AddIFRDetails (AFeatureID : integer;
                                                  AIndex     : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.AddIFRDetails';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(InsertIFRDetailsSQL);
        FAppModules.StudyArea.SetDefaultParams(LDataSet);
        LDataSet.SetParams(['AIdentifier','ALineNumber'],
                           [IntToStr(AFeatureID), IntToStr(AIndex)]);
        if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

       end;
      end;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteIFRDetails (AFeatureID : integer;
                                                     AIndex     : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIFRDetails';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteIFRDetailsSQL(AFeatureID, AIndex));
        LDataset.ExecSQL;
        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

      end;
      LDataset.DataSet.Close;
    finally
      LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.InsertIFRFeature (var AFeatureID : integer; ACalculationOption: double;
         AReferenceFlowType: integer; AIFRFeatureExists: integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIFRFeature';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxIFRFeatureID + 1;

        LDataSet.SetSQL(InsertIFRFeatureSQL(LFeatureID,ACalculationOption,AReferenceFlowType,AIFRFeatureExists));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AFeatureID := LFeatureID;
        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.InsertIFRDetails(AInflowOption: integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIFRDetails';
var
  LDataSet   : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetIFRReferenceSQL);
        LDataset.DataSet.Open;
        LDataset.DataSet.Last;
        LDataset.DataSet.First;

        if LDataset.DataSet.RecordCount > 0 then
          Exit;

        LDataSet.SetSQL(InsertIFRRefrenceSQL(AInflowOption));
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

function TNetworkFeaturesSQLAgent.DeleteIFRFeature (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIFRFeature';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteIFRFeatureSQL(AFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        LDataSet.SetSQL(DeleteIFRFeaturesDetailsSQL(AFeatureID));
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

function TNetworkFeaturesSQLAgent.GetMaxIFRFeatureID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxIFRFeatureID';
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

function TNetworkFeaturesSQLAgent.InsertIrrigationArea (var AFeatureID        : integer;
                                                        ANodeNumber           : integer;
                                                        ADiversionChannelNr   : integer;
                                                        AConsumptiveChannelNr : integer;
                                                        AReturnFlowChannelNr  : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertIrrigationArea';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxIrrigationAreaID + 1;
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertIrrigationAreaSQL(LFeatureID, ANodeNumber, ADiversionChannelNr, AConsumptiveChannelNr, AReturnFlowChannelNr));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertIrrigationDiversionSQL(LFeatureID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertIrrigationReturnFlowSQL(LFeatureID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          AFeatureID := LFeatureID;
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

function TNetworkFeaturesSQLAgent.DeleteIrrigationArea (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteIrrigationArea';
var
  LDataSet : TAbstractModelDataset;
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
          LDataSet.SetSQL(DeleteIrrigationAreaSQL(AFeatureID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteIrrigationDiversionSQL(AFeatureID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeleteIrrigationreturnFlowSQL(AFeatureID));
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

function TNetworkFeaturesSQLAgent.GetMaxIrrigationAreaID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxIrrigationAreaID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxIrrigationAreaIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertPowerPlant (var AFeatureID  : integer;
                                                    APowerChannelNr : integer;
                                                    ASpillChannelNr : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertPowerPlant';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
  lCode      : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxPowerPlantID + 1;
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertPowerPlantSQL(LFeatureID, APowerChannelNr, ASpillChannelNr));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          for lCode := 1 to 4 do
          begin
            LDataSet.SetSQL(InsertPowerPlantDetailsSQL(LFeatureID, lCode));
            LDataset.ExecSQL;
            LDataset.DataSet.Close;
          end;

          for lCode := 1 to 2 do
          begin
            LDataSet.SetSQL(InsertPowerDemandsSQL(LFeatureID, lCode));
            LDataset.ExecSQL;
            LDataset.DataSet.Close;
          end;

          AFeatureID := LFeatureID;
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

function TNetworkFeaturesSQLAgent.DeletePowerPlant (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeletePowerPlant';
var
  LDataSet : TAbstractModelDataset;
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
          LDataSet.SetSQL(DeletePowerPlantSQL(AFeatureID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeletePowerPlantDetailsSQL(AFeatureID));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(DeletePowerDemandsSQL(AFeatureID));
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

function TNetworkFeaturesSQLAgent.GetMaxPowerPlantID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxPowerPlantID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxPowerPlantIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertSpecifiedInflowFeature (var AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertSpecifiedInflowFeature';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxSpecifiedInflowFeatureID + 1;

        LDataSet.SetSQL(InsertSpecifiedInflowFeatureSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        AFeatureID := LFeatureID;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteSpecifiedInflowFeature (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteSpecifiedInflowFeature';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteSpecifiedInflowFeatureSQL(AFeatureID));
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

function TNetworkFeaturesSQLAgent.GetMaxSpecifiedInflowFeatureID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxSpecifiedInflowFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxSpecifiedInflowFeatureIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* Water Demand Feature                                                       *}
{******************************************************************************}

function TNetworkFeaturesSQLAgent.GetWaterDemandCategoriesSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetWaterDemandCategoriesSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT CategoryID AS RecordIdentifier, ' +
      'CategoryDescription, ' +
      'CriteriaPortion01, CriteriaPortion02, CriteriaPortion03, CriteriaPortion04, ' +
      'CriteriaPortion05, CriteriaPortion06, CriteriaPortion07, CriteriaPortion08, ' +
      'CriteriaPortion09, CriteriaPortion10 ' +
      'FROM WaterDemandCategories A WHERE ' +
      GetScenarioWhereClause +
      ' ORDER BY A.CategoryID ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetWaterDemandRiskCriteriaSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetWaterDemandRiskCriteriaSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Interval01, Interval02, Interval03, Interval04, Interval05, ' +
      'Interval06, Interval07, Interval08, Interval09, Interval10 ' +
      'FROM WaterDemandRiskCriteria A WHERE ' +
      GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetWaterDemandFeaturesSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetWaterDemandFeaturesSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT FeatureID AS RecordIdentifier, ' +
      'ChannelNumber, FeatureName, CategoryID, ' +
      'ScenarioPortion01, ScenarioPortion02, ScenarioPortion03, ScenarioPortion04, ' +
      'ScenarioPortion05, ScenarioPortion06, ScenarioPortion07, ScenarioPortion08, ' +
      'ScenarioPortion09, ScenarioPortion10 ' +
      'FROM WaterDemandFeatures A WHERE ' +
      GetScenarioWhereClause +
      ' ORDER BY A.FeatureID ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertWaterDemandFeatureSQL (AFeatureID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertWaterDemandFeatureSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO WaterDemandFeatures '+
      '(Model, StudyAreaName, SubArea, Scenario, FeatureID, ' +
      'ChannelNumber, FeatureName, CategoryID) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) +  ',' +
      IntToStr(AFeatureID) +
      ',0,' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('NetworkFeatures.WaterDemandFeature')) + ' ' + IntToStr(AFeatureID)) +
      ',0)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteWaterDemandFeatureSQL (AFeatureID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteWaterDemandFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM WaterDemandFeatures A WHERE ' +
                GetScenarioWhereClause +
                'AND A.FeatureID = '+ IntToStr(AFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxWaterDemandFeatureIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxWaterDemandFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(FeatureID) AS MaxIdentifier FROM WaterDemandFeatures A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertWaterDemandFeature (var AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertWaterDemandFeature';
var
  LDataSet   : TAbstractModelDataset;
  LFeatureID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxWaterDemandFeatureID + 1;

        LDataSet.SetSQL(InsertWaterDemandFeatureSQL(LFeatureID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();
        AFeatureID := LFeatureID;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteWaterDemandFeature (AFeatureID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteWaterDemandFeature';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteWaterDemandFeatureSQL(AFeatureID));
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

function TNetworkFeaturesSQLAgent.GetMaxWaterDemandFeatureID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxWaterDemandFeatureID';
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

function TNetworkFeaturesSQLAgent.InsertWaterDemandCategorySQL (ACategoryID : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertWaterDemandCategorySQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO WaterDemandCategories '+
      '(Model, StudyAreaName, SubArea, Scenario, CategoryID, ' +
      'CategoryDescription) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(ACategoryID) + ',' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('TField.WaterDemandCategory')) + ' ' + IntToStr(ACategoryID)) +
      ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteWaterDemandCategorySQL (ACategoryID : integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteWaterDemandCategorySQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM WaterDemandCategories A WHERE ' +
                GetScenarioWhereClause +
                'AND A.CategoryID = '+ IntToStr(ACategoryID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxWaterDemandCategoryIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxWaterDemandCategoryIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(CategoryID) AS MaxIdentifier FROM WaterDemandCategories A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxWaterDemandCategoryID : integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxWaterDemandCategoryID';
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

function TNetworkFeaturesSQLAgent.InsertWaterDemandCategory (var ACategoryID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertWaterDemandCategory';
var
  LDataSet   : TAbstractModelDataset;
  LCategoryID : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LCategoryID := GetMaxWaterDemandCategoryID + 1;

        LDataSet.SetSQL(InsertWaterDemandCategorySQL(LCategoryID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

         LImportDate := FAppModules.StudyArea.GetStudyImportDate;
         if LImportDate = nullDateTime then
           FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        ACategoryID := LCategoryID;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteWaterDemandCategory (ACategoryID : integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteWaterDemandCategory';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteWaterDemandCategorySQL(ACategoryID));
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

function TNetworkFeaturesSQLAgent.GetWaterDemandCountsSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetWaterDemandCountsSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT ScenarioCount ' +
      'FROM WaterDemandCounts A WHERE ' +
      GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.CreateScenarioCount: boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.CreateScenarioCount';
var
  LDataSet   : TAbstractModelDataset;
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('SELECT * FROM WaterDemandCounts A WHERE '+ GetScenarioWhereClause);
        LDataset.DataSet.Open;
        if not LDataSet.DataSet.Eof then
          Result := True
        else
        begin
          LDataset.DataSet.Close;
          LSQL :=
          'INSERT INTO WaterDemandCounts '+
          '(Model, StudyAreaName, SubArea, Scenario, ScenarioCount) ' +
          'VALUES (' +
          QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
          QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
          QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
          QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
          '1)';
          LDataset.SetSQL(LSQL);
          LDataset.ExecSQL;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := TRUE;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_WaterDemandCategoryIDFieldNameID(
  AContextData: TStringList; ACategoryID, AFieldNameID: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_WaterDemandCategoryIDFieldNameID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('CategoryID='  + ACategoryID);
    AContextData.Add('FieldNameIdentifier=' + AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_WaterDemandCategoryID(
  AContextData: TStringList; ACategoryID: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_WaterDemandCategoryID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('CategoryID='  + ACategoryID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkFeaturesSQLAgent.CreateRiskCriteriaRecord: boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.CreateRiskCriteriaRecord';
var
  LDataSet   : TAbstractModelDataset;
  LSQL: string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('SELECT * FROM WaterDemandRiskCriteria A WHERE '+ GetScenarioWhereClause);
        LDataset.DataSet.Open;
        if not LDataSet.DataSet.Eof then
          Result := True
        else
        begin
          LDataset.DataSet.Close;
          LSQL :=
          'INSERT INTO WaterDemandRiskCriteria '+
          '(Model, StudyAreaName, SubArea, Scenario) ' +
          'VALUES (' +
          QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
          QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
          QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
          QuotedStr(FAppModules.StudyArea.ScenarioCode) + ')';
          LDataset.SetSQL(LSQL);
          LDataset.ExecSQL;
          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := TRUE;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.AddWaterDemandPortionTotal ( ACategoryID : integer; var ATotal : double ) : boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.AddWaterDemandPortionTotal';
var
  LDataSet           : TAbstractModelDataset;
  LIndex             : integer;
  LPortionTotal      : double;
  lProperties        : TAbstractFieldProperty;
begin
   Result := False;
  try
    lProperties := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if not Assigned(lProperties) then
      raise Exception.Create('Field (RecurrenceInterval) not found in field properties');
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL( 'SELECT * from WaterDemandCategories A WHERE ' +
                GetScenarioWhereClause +
                'AND A.CategoryID = '+ IntToStr(ACategoryID) );
        LDataset.DataSet.Open;
        if not LDataSet.DataSet.Eof then
        begin
           LPortionTotal := 0;
           for LIndex := lProperties.ArrayLow to lProperties.ArrayHigh do
           begin
            if (Trim(LDataset.DataSet.FieldByName(Format('CriteriaPortion%2.2d',[lIndex])).AsString) <> '') then
              LPortionTotal := LPortionTotal + LDataset.DataSet.FieldByName(Format('CriteriaPortion%2.2d',[lIndex])).AsFloat
           end;
           ATotal := LPortionTotal;
        end;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


procedure TNetworkFeaturesSQLAgent.LoadContextData_ChannelAreaID(AContextData: TStringList;
                                                                 AChannelAreaID: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_ChannelAreaID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('AreaID='             + AChannelAreaID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkFeaturesSQLAgent.InsertChannelArea(var AChannelAreaID: integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertChannelArea';
var
  LDataSet       : TAbstractModelDataset;
  LChannelAreaID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LChannelAreaID := GetMaxChannelAreaID + 1;

        LDataSet.SetSQL(InsertChannelAreaSQL(LChannelAreaID));
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();
        AChannelAreaID := LChannelAreaID;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxChannelAreaID: integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxChannelAreaID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxChannelAreaSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertChannelAreaSQL(AChannelAreaID: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertChannelAreaSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO ChannelArea '+
      '(Model, StudyAreaName, SubArea, Scenario, AreaID, ' +
      'AreaName) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AChannelAreaID) + ',' +
      QuotedStr(UpperCase(FAppModules.Language.GetString('TField.ChannelArea')) + ' ' + IntToStr(AChannelAreaID)) +
      ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteChannelArea(AChannelAreaID: integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteChannelArea';
var
  LDataSet : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteChannelAreaSQL(AChannelAreaID));
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

function TNetworkFeaturesSQLAgent.DeleteChannelAreaSQL(AChannelAreaID: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteChannelAreaSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM ChannelArea A WHERE ' +
                GetScenarioWhereClause +
                'AND A.AreaID = '+ IntToStr(AChannelAreaID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxChannelAreaSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxChannelAreaSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(AreaID) AS MaxIdentifier FROM ChannelArea A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetChannelAreaSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetChannelAreaSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT AreaID, ' +
      'AreaName ' +
      'FROM ChannelArea A WHERE ' +
      GetScenarioWhereClause +
      ' ORDER BY A.AreaID ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetWaterUseProportionsSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetWaterUseProportionsSQL';
begin
  Result := '';
  try
    Result := 'SELECT ChannelNumber, ' +
      'Category01, Category02, Category03, Category04, ' +
      'Category05, Category06, Category07, Category08, ' +
      'Category09, Category10 ' +
      'FROM WaterUseProportions A WHERE ' +
      GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TNetworkFeaturesSQLAgent.InsertWaterUseOutputProportionSQL (AChannelNumber : integer) : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertWaterUseOutputProportionSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO WaterUseProportions '+
      '(Model, StudyAreaName, SubArea, Scenario, ChannelNumber)' +
      'VALUES (' +
      QuotedStr ( FAppModules.StudyArea.ModelCode ) + ','+
      QuotedStr ( FAppModules.StudyArea.StudyAreaCode ) + ','+
      QuotedStr ( FAppModules.StudyArea.SubAreaCode ) + ','+
      QuotedStr ( FAppModules.StudyArea.ScenarioCode ) + ','+
      IntToStr ( AChannelNumber ) + ')';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertWaterUseOutputProportion (AChannelNo : integer ): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertWaterUseOutputProportion';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned ( LDataSet ) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL ( InsertWaterUseOutputProportionSQL (AChannelNo ) );
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteWaterUseOutputProportion (AChannelNumber : integer) : boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteWaterUseOutputProportion';
var
  LDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset ( integer ( dtExecSQL ), LDataSet );
    try
      if Assigned ( LDataSet ) then
      begin
        lSQL := 'DELETE FROM WaterUseProportions A WHERE ' + GetScenarioWhereClause +
                ' AND A.ChannelNumber = ' + IntToStr(AChannelNumber);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.UpdateWaterUsePortion ( ACategoryID : integer; AChannelNumber : integer ) : boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.UpdateWaterUsePortion';
var
  LSQL               : string;
  LIndex             : integer;
  LProperties        : TAbstractFieldProperty;
  LDataSet           : TAbstractModelDataset;
begin
   Result := False;
  try
    LProperties := FAppModules.FieldProperties.FieldProperty ( 'WaterUsePortion' );
    if not Assigned(lProperties) then
      raise Exception.Create('Field (WaterUsePortion) not found in field properties');
    if ( ACategoryID >= LProperties.ArrayLow ) and ( ACategoryID < LProperties.ArrayHigh ) then
    begin
       FAppModules.Database.CreateDataset ( integer ( dtExecSQL ), LDataSet );
      try
        if Assigned ( LDataSet ) then
        begin
          LSQL := 'Update WaterUseProportions A Set ';
          for LIndex := ACategoryID + 1 to LProperties.ArrayHigh do
            if LIndex = LProperties.ArrayHigh then
              LSQL := LSQL + Format ( ' A.Category%2.2d = 0 ', [ LIndex ] )
            else
              LSQL := LSQL + Format ( ' A.Category%2.2d = 0, ', [ LIndex ] );
          LSQL := LSQL + ' WHERE ';
          LSQL := LSQL + GetScenarioWhereClause;
          LSQL := LSQL + 'AND A.ChannelNumber = '+ IntToStr ( AChannelNumber );
          LDataSet.SetSQL( LSQL );
          LDataSet.ExecSQL;
          Result := True;
        end;
      finally
        FreeAndNil ( LDataSet );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TNetworkFeaturesSQLAgent.LoadContextData_ChannelNr(AContextData: TStringList; AChannelNr: string);
const OPNAME = 'TNetworkFeaturesSQLAgent.LoadContextData_ChannelNr';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ChannelNumber='      + AChannelNr);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TNetworkFeaturesSQLAgent.GetCurtailmentSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetCurtailmentSQL';
begin
  Result := '';
  try
    Result := 'SELECT CurtailmentPeriodCount, Month01, Month02, Month03, Month04, Month05, ' +
              'Month06, Month07, Month08, Month09, Month10,InUse ' +
              'FROM Curtailment A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertCurtailment(ACurtailmentPeriodCount: integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertCurtailment';
var
  LDataSet    : TAbstractModelDataset;
  LSQL        : string;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL('SELECT * FROM Curtailment A WHERE '+ GetScenarioWhereClause);
        LDataset.DataSet.Open;
        if not LDataSet.DataSet.Eof then
          Result := True
        else
        begin
          LDataset.DataSet.Close;
          LSQL :=
          'INSERT INTO Curtailment '+
          '(Model, StudyAreaName, SubArea, Scenario,CurtailmentPeriodCount,InUse)' +
          'VALUES (' +
          QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
          QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
          QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
          QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
          IntToStr(ACurtailmentPeriodCount) + ',' + '1' + ')';
          LDataset.SetSQL(LSQL);
          LDataset.ExecSQL;
          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = nullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := TRUE;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.InsertCurtailmentSQL(AStartMonths: string): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertCurtailmentSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO Curtailment '+
      '(Model, StudyAreaName, SubArea, Scenario, StartMonths)' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      QuotedStr(AStartMonths) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.DeleteCurtailmentChannel(AChannelNumber: Integer): Boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteCurtailmentChannel';
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
        LDataSet.SetSQL(DeleteCurtailmentChannelSQL(AChannelNumber));
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

function TNetworkFeaturesSQLAgent.DeleteCurtailmentChannelSQL(AChannelNumber: Integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteCurtailmentChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM CurtailmentChannel A WHERE ' +
                GetScenarioWhereClause +
                'AND A.ChannelNumber = '+ IntToStr(AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetCurtailmentChannelSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetCurtailmentChannelSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier,ChannelNumber,'+
              'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, ' +
              'Factor07, Factor08, Factor09, Factor10 ' +
              'FROM CurtailmentChannel A WHERE  '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertCurtailmentChannel(ACurtailedChannel: TCurtailedChannel) : Boolean;

const OPNAME = 'TNetworkFeaturesSQLAgent.InsertCurtailmentChannel';
var
  LIndex      : integer;
  LDataSet    : TAbstractModelDataset;
  LIdentifier : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetMaxCurtailedChannelID + 1;

        LDataSet.SetSQL(InsertCurtailmentChannelSQL(LIdentifier,ACurtailedChannel.ChannelNumber));
        for LIndex := 1 to 10 do
          LDataSet.SetParams([Format('Factor%2.2d',[LIndex])],['0.0']);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        ACurtailedChannel.PopulateSome(LIdentifier);

        FAppModules.StudyArea.LastUpdateDate := Now();
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.InsertCurtailmentChannelSQL(AIdentifier: integer;
                                                              AChannelNumber: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertCurtailmentChannelSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO CurtailmentChannel '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ChannelNumber,Factor01, Factor02, Factor03, Factor04, Factor05,' +
      'Factor06, Factor07, Factor08, Factor09, Factor10) ' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AIdentifier) + ',' +
      IntToStr(AChannelNumber) + ',' +
      ' :Factor01, :Factor02, :Factor03, :Factor04, :Factor05, ' +
      ' :Factor06, :Factor07, :Factor08, :Factor09, :Factor10 ) ';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxCurtailedChannelID: integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxCurtailedChannelID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxCurtailedChannelSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxCurtailedChannelSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxCurtailedChannelSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM CurtailmentChannel A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM DroughtRestriction A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertDroughtRestriction(ADroughtRestriction : TDroughtRestriction): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDroughtRestriction';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LIndex      : integer;
begin
  Result := False;
  try
    if (ADroughtRestriction = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin

        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertDroughtRestrictionSQL((ADroughtRestriction.Identifier)));
          LDataSet.SetParams(['Name'],            [ADroughtRestriction.DroughtRestrictionName]);
          LDataSet.SetParams(['ReservoirNumbers'],['']);
          LDataSet.SetParams(['ChannelNumbers'],  ['']);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertDroughtRestrictionFactorsSQL(ADroughtRestriction.Identifier));
          for LIndex := 1 to 10 do
            LDataSet.SetParams([Format('Factor%2.2d',[LIndex])],['0.0']);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(InsertDroughtRestrictionStorageVolumeSQL(ADroughtRestriction.Identifier));
          for LIndex := 1 to 10 do
            LDataSet.SetParams([Format('Volume%2.2d',[LIndex])],['0.0']);
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
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.DeleteDroughtRestriction(AIdentifier: integer): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteDroughtRestriction';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(GetDeleteDroughtRestrictionSQL(AIdentifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteDroughtRestrictionFactorsSQL(AIdentifier));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          LDataSet.SetSQL(GetDeleteDroughtRestrictionStorageVolumeSQL(AIdentifier));
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

function TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionID: integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxDroughtRestrictionSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertDroughtRestrictionSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDroughtRestrictionSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO DroughtRestriction '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, Name, ReservoirNumbers, ChannelNumbers)' +
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
      IntToStr(AIdentifier) + ',' +
      ' :Name, :ReservoirNumbers, :ChannelNumbers) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDeleteDroughtRestrictionSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDeleteDroughtRestrictionSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DroughtRestriction A WHERE ' +
              GetScenarioWhereClause +
              ' AND A.Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDeleteDroughtRestrictionFactorsSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDeleteDroughtRestrictionFactorsSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DroughtRestrictionFactors A WHERE ' +
              GetScenarioWhereClause +
              ' AND A.Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDeleteDroughtRestrictionStorageVolumeSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDeleteDroughtRestrictionStorageVolumeSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM DroughtRestrictionStorageVolumes A WHERE ' +
              GetScenarioWhereClause +
              ' AND A.Identifier = ' + IntToStr(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDroughtRestrictionSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDroughtRestrictionSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Identifier,Name, ' +
      'ReservoirNumbers, ChannelNumbers ' +
      'FROM DroughtRestriction A WHERE ' +
      GetScenarioWhereClause +
      ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNetworkFeaturesSQLAgent.InsertDroughtRestrictionFactorsSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDroughtRestrictionFactorsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DroughtRestrictionFactors ' +
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' Factor01, Factor02, Factor03, Factor04,Factor05, ' +
              ' Factor06, Factor07, Factor08, Factor09,Factor10 ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AIdentifier) + ','+
              ' :Factor01, :Factor02, :Factor03, :Factor04, :Factor05, ' +
              ' :Factor06, :Factor07, :Factor08, :Factor09, :Factor10 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNetworkFeaturesSQLAgent.InsertDroughtRestrictionStorageVolumeSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertDroughtRestrictionStorageVolumeSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DroughtRestrictionStorageVolumes ' +
              ' (Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              ' Volume01, Volume02, Volume03, Volume04,Volume05, ' +
              ' Volume06, Volume07, Volume08, Volume09,Volume10 ' +
              ') VALUES (' +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
              IntToStr(AIdentifier) + ','+
              ' :Volume01, :Volume02, :Volume03, :Volume04, :Volume05, ' +
              ' :Volume06, :Volume07, :Volume08, :Volume09, :Volume10 ) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDroughtRestrictionFactorsSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDroughtRestrictionFactorsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, '+
              'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, ' +
              'Factor07, Factor08, Factor09, Factor10 ' +
              'FROM DroughtRestrictionFactors A WHERE  '+
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AIdentifier) + ') ' +
              ' ORDER BY A.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetDroughtRestrictionVolumeSQL(AIdentifier: integer): string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetDroughtRestrictionVolumeSQL';
begin
  Result := '';
  try
    Result := 'SELECT Identifier AS RecordIdentifier, '+
              'Volume01, Volume02, Volume03, Volume04,Volume05, Volume06, ' +
              'Volume07, Volume08, Volume09, Volume10 ' +
              'FROM DroughtRestrictionStorageVolumes A WHERE '+
              GetScenarioWhereClause +
              ' AND (A.Identifier = ' + IntToStr(AIdentifier) + ') ' +
              ' ORDER BY A.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionID: integer;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxDemandCentreIDSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionIDSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetMaxDroughtRestrictionIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifer) FROM DroughtRestriction A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.GetWaterDemandFileCreateSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.GetWaterDemandFileCreateSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT ImplementFile ' +
      'FROM FileCreate A WHERE ' +
      GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertImplementReconciliationFileSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertImplementReconciliationFileSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO FileCreate'+
      '(Model, StudyAreaName, SubArea, Scenario, ImplementFile)'+
      'VALUES (' +
      QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
      QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
      QuotedStr(FAppModules.StudyArea.ScenarioCode) +  ',' +
      '1)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesSQLAgent.InsertImplementReconciliationFile: boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.InsertImplementReconciliationFile';
var
  LDataSet   : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(InsertImplementReconciliationFileSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesSQLAgent.CopyIrrigationAreaFromScenario(ASourceStudyAreaName, ASourceSubArea, ASourceScenario: string;
                                  AIrrigationNodeNumberList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.CopyIrrigationAreaFromScenario';
      IrrigationAreasSQL = 'SELECT * FROM IrrigationAreas  WHERE ';
      IrrigationAreasDiversionFlowSQL = 'SELECT * FROM IrrigationAreasDiversionFlow WHERE ';
      IrrigationAreasReturnFlowSQL = 'SELECT * FROM IrrigationAreasReturnFlow WHERE ';
      ChannelDetailsSQL         = 'SELECT * FROM ChannelDetails WHERE ';
var
  LSourceDataSet                   : TAbstractModelDataset;
  LDestinationDataSet              : TAbstractModelDataset;
  LChannelSourceDataSet            : TAbstractModelDataset;
  LChannelDestinationDataSet       : TAbstractModelDataset;
  LModel                           : string;
  LFieldName                       : string;
  LDestinationStudyAreaName        : string;
  LDestinationSubArea              : string;
  LDestinationScenario             : string;
  LSourceWhereClause               : string;
  LDestinationWhereClause          : string;
  LIrrigationAreaName              : string;
  LMessage                         : string;
  LSourceSQL                       : string;
  LDestinationSQL                  : string;
  LStop                            : boolean;
  LCurrentFeatureID                : integer;
  LCurrentNodeNumber               : integer;
  LNewIrrigationNodeNumber         : integer;
  LNewFeatureID                    : integer;
  LReservoirDataSQLAgent           : TReservoirDataSQLAgent;
  LIrrigationNodeIndex             : integer;
  LImportDate                      : TDateTime;
  LChannelDataSQLAgent             : TChannelDataSQLAgent;
  LDiversionChannelNr              : integer;
  LConsumptiveChannelNr            : integer;
  LReturnFlowChannelNr             : integer;
  LNewChannelID                    : integer;
  LNewChannelNumber                : integer;
  LIndex                           : integer;
begin
  Result := False;
  try
    if not Assigned(AIrrigationNodeNumberList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
    LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
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
        try
          FAppModules.Database.StartTransaction;
          //LCurrentFeatureID := 0;
          LNewFeatureID := GetMaxIrrigationAreaID;
          LNewIrrigationNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;
          LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                                 AIrrigationNodeNumberList,AProgressUpdateFuntion);
          for LIrrigationNodeIndex := 0 to AIrrigationNodeNumberList.Count-1 do
          begin
            LNewFeatureID            := LNewFeatureID + 1;
            LNewIrrigationNodeNumber := LNewIrrigationNodeNumber + 1;
            LIrrigationAreaName      := AIrrigationNodeNumberList[LIrrigationNodeIndex];
            LCurrentNodeNumber       := integer(AIrrigationNodeNumberList.Objects[LIrrigationNodeIndex]);
            LMessage := 'Copying Irrigation Area ('+LIrrigationAreaName+') ' + IntToStr(LIrrigationNodeIndex+1) + ' of '+ IntToStr(AIrrigationNodeNumberList.Count);
            AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
            if LStop then
            begin
              if FAppModules.Database.InTransaction then
                FAppModules.Database.Rollback;
              Exit;
            end;
            //________________________________________________________ IrrigationArea ____________________________
            LSourceSQL := IrrigationAreasSQL + LSourceWhereClause + ' AND IrrigationNodeNumber = '+ IntToStr(LCurrentNodeNumber);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;


            LDestinationSQL := IrrigationAreasSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table IrrigationArea cannot be set to updatable.');
            end
            else
            begin
              LCurrentFeatureID := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LDestinationDataSet.DataSet.Append;
              for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
              begin
                LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
              end;

              LDiversionChannelNr := LSourceDataSet.DataSet.FieldByName('DiversionChannelNumber').AsInteger;
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LDiversionChannelNr);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if LChannelSourceDataSet.DataSet.Eof then Continue;

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
                LChannelDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                begin
                  LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                LNewChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                 := LModel;
                LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString         := LDestinationStudyAreaName;
                LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString               := LDestinationSubArea;
                LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString              := LDestinationScenario;
                LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger           := LNewChannelID;
                LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger        := LNewChannelNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger        := 0;
                LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger       := LNewIrrigationNodeNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger         := 0;
                LChannelDestinationDataSet.DataSet.Post;
                LDestinationDataSet.DataSet.FieldByName('DiversionChannelNumber').AsInteger   := LNewChannelNumber;
              end;
              LConsumptiveChannelNr := LSourceDataSet.DataSet.FieldByName('ConsumptiveChannelNumber').AsInteger;
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LConsumptiveChannelNr);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if LChannelSourceDataSet.DataSet.Eof then Continue;

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
                LChannelDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                begin
                  LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                LNewChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                 := LModel;
                LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString         := LDestinationStudyAreaName;
                LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString               := LDestinationSubArea;
                LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString              := LDestinationScenario;
                LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger           := LNewChannelID;
                LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger        := LNewChannelNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger        := 0;
                LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger         := LNewIrrigationNodeNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger       := 0;
                LChannelDestinationDataSet.DataSet.Post;
                LDestinationDataSet.DataSet.FieldByName('ConsumptiveChannelNumber').AsInteger := LNewChannelNumber;
              end;

              LReturnFlowChannelNr := LSourceDataSet.DataSet.FieldByName('ReturnFlowChannelNumber').AsInteger;
              LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LReturnFlowChannelNr);
              LChannelSourceDataSet.DataSet.Close;
              LChannelSourceDataSet.SetSQL(LSourceSQL);
              LChannelSourceDataSet.DataSet.Open;
              if LChannelSourceDataSet.DataSet.Eof then Continue;

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
                LChannelDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                begin
                  LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                LNewChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                 := LModel;
                LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString         := LDestinationStudyAreaName;
                LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString               := LDestinationSubArea;
                LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString              := LDestinationScenario;
                LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger           := LNewChannelID;
                LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger        := LNewChannelNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger        := 0;
                LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger         := LNewIrrigationNodeNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger       := 0;
                LChannelDestinationDataSet.DataSet.Post;
                LDestinationDataSet.DataSet.FieldByName('ReturnFlowChannelNumber').AsInteger  := LNewChannelNumber;
              end;
              LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
              LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
              LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
              LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewFeatureID;
              LDestinationDataSet.DataSet.FieldByName('IrrigationNodeNumber').AsInteger     := LNewIrrigationNodeNumber;
              LDestinationDataSet.DataSet.Post;
            end;
            //________________________________________________________ IrrigationAreasDiversionFlow ____________________________
            LSourceSQL := IrrigationAreasDiversionFlowSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFeatureID);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;

            LDestinationSQL := IrrigationAreasDiversionFlowSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table IrrigationAreasDiversionFlow cannot be set to updatable.');
            end
            else
            begin
              LCurrentFeatureID := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
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
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewFeatureID;
              LDestinationDataSet.DataSet.Post;
            end;

           //________________________________________________________ IrrigationAreasReturnFlow ____________________________
            LSourceSQL := IrrigationAreasReturnFlowSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentFeatureID);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;

            LDestinationSQL := IrrigationAreasReturnFlowSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table IrrigationAreasReturnFlow cannot be set to updatable.');
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
              LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewFeatureID;
              LDestinationDataSet.DataSet.Post;
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

function TNetworkFeaturesSQLAgent.CopyPowerPlantsFromScenario(ASourceStudyAreaName, ASourceSubArea, ASourceScenario: string;
                                  APowerPlantsList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.CopyPowerPlantsFromScenario';
      PowerPlantsSQL = 'SELECT * FROM PowerPlants  WHERE ';
      PowerPlantsDemandsSQL = 'SELECT * FROM PowerPlantsDemands WHERE ';
      PowerPlantsDetailsSQL = 'SELECT * FROM PowerPlantsDetails WHERE ';
      ChannelDetailsSQL         = 'SELECT * FROM ChannelDetails WHERE ';
var
  LSourceDataSet                   : TAbstractModelDataset;
  LDestinationDataSet              : TAbstractModelDataset;
  LChannelSourceDataSet            : TAbstractModelDataset;
  LChannelDestinationDataSet       : TAbstractModelDataset;
  LCurrentIdetifier                : integer;
  LNewFeatureID                    : integer;
  LOldPowerChannel                 : integer;
  LOldSpillChannel                 : integer;
  LNewPowerChannel                 : integer;
  LNewSpillChannel                 : integer;
  LMessage                         : string;
  LPowerPlantName                  : string;
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
  LPowerPlantIndex                 : integer;
  LIndex                           : integer;
  LImportDate                      : TDateTime;
  LChannelDataSQLAgent             : TChannelDataSQLAgent;
  LNewChannelID                    : integer;
begin
  Result := False;
  try
    if not Assigned(APowerPlantsList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
    LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
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
        try
          FAppModules.Database.StartTransaction;
          LNewFeatureID := GetMaxPowerPlantID;
          //LNewPowerChannel := 0;
          //LNewSpillChannel := 0;
          for LPowerPlantIndex := 0 to APowerPlantsList.Count-1 do
          begin
            LCurrentIdetifier := integer(APowerPlantsList.Objects[LPowerPlantIndex]);
            LNewFeatureID := LNewFeatureID + 1;
            LPowerPlantName := APowerPlantsList[LPowerPlantIndex];
            LMessage := 'Copying Power Plant ('+LPowerPlantName+') ' + IntToStr(LPowerPlantIndex+1) + ' of '+ IntToStr(APowerPlantsList.Count);
            AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
            if LStop then
            begin
              if FAppModules.Database.InTransaction then
                FAppModules.Database.Rollback;
              Exit;
            end;

            //________________________________________________________ PowerPlants ____________________________
            LSourceSQL := PowerPlantsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;

            LDestinationSQL := PowerPlantsSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table PowerPlants cannot be set to updatable.');
            end
            else
            begin
              LDestinationDataSet.DataSet.Append;
              LOldPowerChannel := LSourceDataSet.DataSet.FieldByName('PowerChannelNumber').AsInteger;
              LOldSpillChannel := LSourceDataSet.DataSet.FieldByName('SpillChannelNumber').AsInteger;
              for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
              begin
                LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
              end;

              if (LOldPowerChannel > 0) and (LOldSpillChannel > 0) then
              begin
                //________________________________________________________ ChannelDetails ____________________________
                LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldPowerChannel);
                LChannelSourceDataSet.DataSet.Close;
                LChannelSourceDataSet.SetSQL(LSourceSQL);
                LChannelSourceDataSet.DataSet.Open;
                if LChannelSourceDataSet.DataSet.Eof then Continue;

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
                  LNewPowerChannel := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewPowerChannel;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := 0;
                  LChannelDestinationDataSet.DataSet.Post;
                end;
                //________________________________________________________ ChannelDetails ____________________________
                LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldSpillChannel);
                LChannelSourceDataSet.DataSet.Close;
                LChannelSourceDataSet.SetSQL(LSourceSQL);
                LChannelSourceDataSet.DataSet.Open;
                if LChannelSourceDataSet.DataSet.Eof then Continue;

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
                  LNewSpillChannel := LChannelDataSQLAgent.GetMaxChannelNumber+1;
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
                  LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewSpillChannel;
                  LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                  LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := 0;

                  LChannelDestinationDataSet.DataSet.Post;
                end;
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewFeatureID;
                LDestinationDataSet.DataSet.FieldByName('PowerChannelNumber').AsInteger       := LNewPowerChannel;
                LDestinationDataSet.DataSet.FieldByName('SpillChannelNumber').AsInteger       := LNewSpillChannel;
                LDestinationDataSet.DataSet.Post;
              end;
            end;
            //________________________________________________________ PowerPlantsDemands ____________________________
            LSourceSQL := PowerPlantsDemandsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;
            while not LSourceDataSet.DataSet.Eof do
            begin
              LDestinationSQL := PowerPlantsDemandsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table PowerPlantsDemands cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewFeatureID;
                LDestinationDataSet.DataSet.Post;
              end;
              LSourceDataSet.DataSet.Next;
            end;
            //________________________________________________________ PowerPlantsDetails ____________________________
            LSourceSQL := PowerPlantsDetailsSQL + LSourceWhereClause + ' AND Identifier = '+ IntToStr(LCurrentIdetifier);
            LSourceDataSet.DataSet.Close;
            LSourceDataSet.SetSQL(LSourceSQL);
            LSourceDataSet.DataSet.Open;
            if LSourceDataSet.DataSet.Eof then Continue;
            while not LSourceDataSet.DataSet.Eof do
            begin
              LDestinationSQL := PowerPlantsDetailsSQL + LDestinationWhereClause;
              LDestinationDataSet.DataSet.Close;
              LDestinationDataSet.SetSQL(LDestinationSQL);
              LDestinationDataSet.SetReadOnly(False);
              LDestinationDataSet.DataSet.Open;
              if LDestinationDataSet.IsReadOnly  then
              begin
                LDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table PowerPlantsDemands cannot be set to updatable.');
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
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewFeatureID;
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
      LChannelDataSQLAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TNetworkFeaturesSQLAgent.DeleteCurtailment: boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteCurtailment';
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
        LDataSet.SetSQL(DeleteCurtailmentSQL);
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

function TNetworkFeaturesSQLAgent.DeleteCurtailmentSQL : string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteCurtailmentSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM Curtailment A WHERE ' +
                GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TNetworkFeaturesSQLAgent.DeleteImplementReconciliationFile: boolean;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteImplementReconciliationFile';
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
        LDataSet.SetSQL(DeleteImplementReconciliationFileSQL);
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

function TNetworkFeaturesSQLAgent.DeleteImplementReconciliationFileSQL: string;
const OPNAME = 'TNetworkFeaturesSQLAgent.DeleteImplementReconciliationFileSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM FileCreate A WHERE ' +
                GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.





