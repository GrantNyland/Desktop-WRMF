//
//  UNIT      : Contains field update SQL.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/09/21
//  COPYRIGHT : Copyright © 2007 DWAF
//
unit UFieldUpdateSQL_YieldModel_GroundWater;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string) of object;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

const
  CWhereScenario =
    ' WHERE                                           ' +
    '   (A.Model         = :AModelCode          ) AND ' +
    '   (A.StudyAreaName = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea       = :ASubAreaCode        ) AND ' +
    '   (A.Scenario      = :AScenarioCode       )     ';

  CGroundWaterDetails =
     ' FROM GroundWater A                  ' +
       CWhereScenario +
     ' AND (A.Identifier    = :AIdentifier)     ' ;

  CGroundWaterEvaporation =
     ' FROM GroundWaterEvaporation A               ' +
       CWhereScenario +
     ' AND (A.GroundWaterIdentifier    = :AGroundWaterIdentifier)     ' ;

  CGroundWaterUsageFactor =
     ' FROM GroundWaterUsageFactor A                ' +
       CWhereScenario +
     ' AND (A.GroundWaterIdentifier    = :AGroundWaterIdentifier)     ' ;

  CGroundWaterPitman =
     ' FROM GroundWaterPitman A                ' +
       CWhereScenario +
     ' AND (A.GroundWaterIdentifier    = :AGroundWaterIdentifier)' ;

  CGroundWaterSubCatchment =
     ' FROM GroundWaterSubCatchment A               ' +
       CWhereScenario +
     ' AND (A.GroundWaterIdentifier    = :AGroundWaterIdentifier)     ' ;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

//GroundWater Details
  AAdd(0,'GroundWaterName','GroundWater','GroundWaterName','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'GroundWaterDescription','GroundWater','GroundWaterDescription','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'AquiferStorativity','GroundWater','AquiferStorativity','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'AquiferStaticWaterlevel','GroundWater','AquiferStaticWaterlevel','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'UnsaturatedStorageCapacity','GroundWater','UnsaturatedStorageCapacity','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'InitialUnsaturatedStorage','GroundWater','InitialUnsaturatedStorage','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'MaximumDischargeRate','GroundWater','MaximumAquiferRecharge','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'MovingAverageRecharge','GroundWater','MovingAverageRecharge','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'MaximumRateOfGroundwaterBaseFlow','GroundWater','MaximumBaseFlowRate','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'PowerHeadDifferenceBaseFlowEquation','GroundWater','HeadBaseFlowPower','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'MaximumHydrologicalGradient','GroundWater','MaximumHydrologicalGradient','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'AquiferTransmissivity','GroundWater','AquiferTransmissivity','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'BoreHoleDistanceToRiver','GroundWater','BoreHoleDistanceToRiver','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'MaximumGroundwaterAbstraction','GroundWater','MaximumWaterAbstraction','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'ParameterK2','GroundWater','ParameterK2','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'ParameterK3','GroundWater','ParameterK3','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'GroundWaterEvaporationArea','GroundWater','WaterEvaporationArea','SELECT *'+ CGroundWaterDetails, '');
  AAdd(0,'GroundWaterRefNodeNumber','GroundWaterSubCatchment','RefNodeNumber','SELECT *'+ CGroundWaterSubCatchment, '');

  //GroundWater Pitman
  AAdd(0,'PitmanSoilMoistureCapacity','GroundWaterPitman','SoilMoistureCapacity','SELECT *'+ CGroundWaterPitman, '');
  AAdd(0,'PitmanSoilMoistureStorageCapacity','GroundWaterPitman','SoilMoistureStorageCapacity','SELECT *'+ CGroundWaterPitman, '');
  AAdd(0,'PitmansoilMoistureFlowState','GroundWaterPitman','SoilMoistureFlowState','SELECT *'+ CGroundWaterPitman, '');
  AAdd(0,'PitmanSoilMoistureFlowEquation','GroundWaterPitman','SoilMoistureFlowEquation','SELECT *'+ CGroundWaterPitman, '');
  AAdd(0,'PitmanMaximumGroundwaterFlow','GroundWaterPitman','MaximumGroundWaterFlow','SELECT *'+ CGroundWaterPitman, '');
  AAdd(0,'PitmanSoilMoistureRechargeEquation','GroundWaterPitman','SoilMoistureRechargeEquation','SELECT *'+ CGroundWaterPitman, '');
  AAdd(0,'PitmanGroundwaterFlow','GroundWaterPitman','GroundWaterFlow','SELECT *'+ CGroundWaterPitman, '');

  //GroundWater Monthly Water Evaporation
  AAdd(0,'MonthlyWaterEvaporation','GroundWaterEvaporation','FieldNameIdentifier,1=Evaporation%2.2d',
        ' SELECT *' + CGroundWaterEvaporation, '');

//GroundWater Usage Factor
  AAdd(0,'MonthlyWaterUsageFactors','GroundWaterUsageFactor','FieldNameIdentifier,1=Factor%2.2d',
        ' SELECT * ' + CGroundWaterUsageFactor, '');

end;

end.
