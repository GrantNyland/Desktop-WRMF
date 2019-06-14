//
//  UNIT      : Contains field update SQL.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/20
//  COPYRIGHT : Copyright © 2007 DWAF
//
unit UFieldUpdateSQL_YieldModel_Mining;

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

  CWhereScenarioIdentifier =
    ' WHERE                                           ' +
    '   (A.Model          = :AModelCode          ) AND ' +
    '   (A.StudyAreaName  = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea        = :ASubAreaCode        ) AND ' +
    '   (A.Scenario       = :AScenarioCode       ) AND ' +
    '   (A.MineIdentifier = :AMineIdentifier     )     ';

  CMineDetails =
     ' FROM Mine A                  ' +
       CWhereScenario +
     ' AND (A.Identifier    = :AIdentifier)     ' ;

  CMineLakeEvaporation =
     ' FROM MineLakeEvaporation A               ' +
       CWhereScenario +
     ' AND (A.MineIdentifier    = :AMineIdentifier)     ' ;

  CMinePanEvaporation =
     ' FROM MinePanEvaporation A                ' +
       CWhereScenario +
     ' AND (A.MineIdentifier    = :AMineIdentifier)     ' ;

  CMineOpenCast =
     ' FROM MineOpenCast A                ' +
       CWhereScenarioIdentifier +
     ' AND (A.Identifier    = :AIdentifier)     ' ;

  CMineRechargeFactor =
     ' FROM MineRechargeFactor A                ' +
       CWhereScenarioIdentifier +
     ' AND (A.RechargeFactorParentType = :ARechargeFactorParentType)  '+
     ' AND (A.ParentIdentifier         = :AParentIdentifier)          '+
     ' AND (A.RechargeFactorType       = :ARechargeFactorType)';

  CMineUndergroundSection =
     ' FROM MineUnderGround A                ' +
       CWhereScenarioIdentifier                +
     ' AND (A.Identifier    = :AIdentifier)  ';

  CMineUndergroundUpStreamRunoff =
     ' FROM MineUGUpstreamRunoff A                ' +
       CWhereScenarioIdentifier +
     ' AND (A.UGIdentifier = :AUGIdentifier)  ';

  CMineSlurryDump =
     ' FROM MineSlurryDump A                ' +
       CWhereScenarioIdentifier               +
     ' AND (A.Identifier    = :AIdentifier)  ';

  CMineSubCatchment =
     ' FROM MineSubCatchment A                ' +
       CWhereScenario +
     ' AND (A.Identifier    = :AIdentifier)     ' ;

  CMineSubCatchmentFlowVolume =
     ' FROM MineSubCatchmentFlowVolume A                ' +
       CWhereScenario +
     ' AND (A.MineSubCatchmentIdentifier    = :AMineSubCatchmentIdentifier)     ' ;


procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

//Mine Details
  AAdd(0,'NodeNumber','Mine','NodeNumber','SELECT *'+ CMineDetails, '');
  AAdd(0,'MineName','Mine','MineName','SELECT *'+ CMineDetails, '');
  AAdd(0,'RiverChannelNumber','Mine','RiverChannelNumber','SELECT *'+ CMineDetails, '');
  AAdd(0,'PCDChannelNumber','Mine','PCDChannelNumber','SELECT *'+ CMineDetails, '');
  AAdd(0,'HydrologyNodeNumber','Mine','HydrologyNodeNumber','SELECT *'+ CMineDetails, '');
  AAdd(0,'BeneficiationPlantArea','Mine','BeneficiationPlantArea','SELECT *'+ CMineDetails, '');
  AAdd(0,'BeneficiationRunOffFactor','Mine','BeneficiationRunOffFactor','SELECT *'+ CMineDetails, '');

//MinePanEvaporation
  AAdd(0,'MinePanEvaporationFactors','MinePanEvaporation','FieldNameIdentifier,1=PanEvaporation%2.2d',
        ' SELECT * ' +  CMinePanEvaporation, '');

//MinePanEvaporation
  AAdd(0,'MineLakeEvaporationFactors','MineLakeEvaporation','FieldNameIdentifier,1=LakeEvaporation%2.2d',
        ' SELECT * ' + CMineLakeEvaporation, '');

//MineSubCatchment
  AAdd(0,'MineCatchmentReferenceNr','MineSubCatchment','CatchmentReferenceNumber','SELECT *'+ CMineSubCatchment, '');
  AAdd(0,'ProportionAntecedentFlow','MineSubCatchment','ProportionAntecedentFlow','SELECT *'+ CMineSubCatchment, '');
  AAdd(0,'GroundwaterFlowVolume','MineSubCatchment','GroundwaterFlowVolume','SELECT *'+ CMineSubCatchment, '');
  AAdd(0,'AntecedentRunoffDecayFactor','MineSubCatchment','AntecedentRunoffDecayFactor','SELECT *'+ CMineSubCatchment, '');
  AAdd(0,'CatchmentRefInUse','MineSubCatchment','InUse','SELECT *'+ CMineSubCatchment, '');

//MineSubCatchmentFlowVolume
  AAdd(0,'MineSubCatchmentFlowVolume','MineSubCatchmentFlowVolume','FieldNameIdentifier,1=Volume%2.2d',
        ' SELECT *' +  CMineSubCatchmentFlowVolume, '');

//Mine Open Cast
  AAdd(0,'PitName','MineOpenCast','PitName','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'CoalReserveArea','MineOpenCast','CoalReserveArea','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'WorkingsArea','MineOpenCast','WorkingsArea','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'DisturbedWorkingsArea','MineOpenCast','DisturbedWorkingsArea','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'DisturbedArea','MineOpenCast','DisturbedArea','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'WaterSurfaceEvapArea','MineOpenCast','WaterSurfaceEvapArea','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'DisturbedAreaRunOff','MineOpenCast','DisturbedAreaRunOff','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'DisturbedWorkingsAreaRunOff','MineOpenCast','DisturbedWorkingsAreaRunOff','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'DecantVolume','MineOpenCast','DecantVolume','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'SeepageVolume','MineOpenCast','SeepageVolume','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'AnalysisStartVolume','MineOpenCast','AnalysisStartVolume','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'MaximumSeepageRate','MineOpenCast','MaximumSeepageRate','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'SeepageExponent','MineOpenCast','SeepageExponent','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'OpenCastPCDSurfaceArea','MineOpenCast','PCDSurfaceArea','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'OpenCastPCDStorageCapacity','MineOpenCast','PCDStorageCapacity','SELECT *'+ CMineOpenCast, '');
  AAdd(0,'OpenCastPCDAnalysisStartVolume','MineOpenCast','PCDAnalysisStartVolume','SELECT *'+ CMineOpenCast, '');

//Underground Mining Section
  AAdd(0,'UndergroundSectionName','MineUnderGround','UnderGroundSectionName','SELECT *'+ CMineUndergroundSection, '');
  AAdd(0,'ChannelNumberToUGDam','MineUnderGround','ChannelNumberToUGDam','SELECT *'+ CMineUndergroundSection, '');
  AAdd(0,'UpstreamCatchmentArea','MineUnderGround','UpstreamCatchmentArea','SELECT *'+ CMineUndergroundSection, '');
  AAdd(0,'BoardPillarCatchmentArea','MineUnderGround','BoardPillarCatchmentArea','SELECT *'+ CMineUndergroundSection, '');
  AAdd(0,'HighExtractionCatchmentArea','MineUnderGround','HighExtractionCatchmentArea','SELECT *'+ CMineUndergroundSection, '');
  AAdd(0,'HighExtractionAreaRunoffFactor','MineUnderGround','HighExtractionAreaRunoffFactor','SELECT *'+ CMineUndergroundSection, '');

//Mine Underground UpStream Runoff
  AAdd(0,'MineUGUpstreamRunoff','MineUGUpstreamRunoff','FieldNameIdentifier,1=RunoffFactor%2.2d',
        ' SELECT *' +  CMineUndergroundUpStreamRunoff, '');

//Mining Discard/Slurry Dump
  AAdd(0,'DumpName','MineSlurryDump','DumpName','SELECT *'+ CMineSlurryDump, '');
  AAdd(0,'DumpSurfaceArea','MineSlurryDump','DumpSurfaceArea','SELECT *'+ CMineSlurryDump, '');
  AAdd(0,'RunoffFactorToPCD','MineSlurryDump','RunoffFactorToPCD','SELECT *'+ CMineSlurryDump, '');
  AAdd(0,'SeepageSplitFactor','MineSlurryDump','SeepageSplitFactor','SELECT *'+ CMineSlurryDump, '');
  AAdd(0,'DumpPCDStorageCapacity','MineSlurryDump','PCDStorageCapacity','SELECT *'+ CMineSlurryDump, '');
  AAdd(0,'DumpPCDSurfaceArea','MineSlurryDump','PCDSurfaceArea','SELECT *'+ CMineSlurryDump, '');
  AAdd(0,'DumpPCDAnalysisStartVolume','MineSlurryDump','PCDAnalysisStartVolume','SELECT *'+ CMineSlurryDump, '');

  // Mining Recharge Factors
  AAdd(0,'RechargeFactors','MineRechargeFactor','FieldNameIdentifier,1=RechargeFactor%2.2d',
        ' SELECT * ' +  CMineRechargeFactor, '');
end;

end.
