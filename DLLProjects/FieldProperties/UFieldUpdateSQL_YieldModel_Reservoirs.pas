//
//  UNIT      : Contains field update SQL.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UFieldUpdateSQL_YieldModel_Reservoirs;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string) of object;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

//
// General reservoir where clauses.
//
const
  {CWhereNVModelDataLinkReservoirs =
    ' WHERE                                       ' +
    '   (A.Model          = :AModelCode     ) AND ' +
    '   (A.StudyAreaName  = :AStudyAreaCode ) AND ' +
    '   (A.SubArea        = :ASubAreaCode   ) AND ' +
    '   (A.ModelElementID = :AModelElementID) AND ' +
    '   (A.ElementType    = 140             )     ' ;
  CWhereNVModelDataLinkReservoirsLevelIdentifier =
    ' WHERE                                        ' +
    '   (A.Model           = :AModelCode      ) AND ' +
    '   (A.StudyAreaName   = :AStudyAreaCode  ) AND ' +
    '   (A.SubArea         = :ASubAreaCode    ) AND ' +
    '   (A.ModelElementID  = :AModelElementID ) AND ' +
    '   (A.ElementType     = 140              ) AND ' +
    '   (B.LevelIdentifier = :ALevelIdentifier)     ' ;
    }
  CWhereScenarioReservoirIdentifier =
    ' WHERE                                                 ' +
    '   (A.Model               = :AModelCode          ) AND ' +
    '   (A.StudyAreaName       = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea             = :ASubAreaCode        ) AND ' +
    '   (A.Scenario            = :AScenarioCode       ) AND ' +
    '   (A.ReservoirIdentifier = :AReservoirIdentifier)     ' ;
  CWhereScenarioReservoirIdentifierLevelIdentifier =
    ' WHERE                                                 ' +
    '   (A.Model               = :AModelCode          ) AND ' +
    '   (A.StudyAreaName       = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea             = :ASubAreaCode        ) AND ' +
    '   (A.Scenario            = :AScenarioCode       ) AND ' +
    '   (A.ReservoirIdentifier = :AReservoirIdentifier) AND ' +
    '   (A.LevelIdentifier     = :ALevelIdentifier    )     ' ;
  CWhereScenarioNodeCount =
    ' WHERE                                                 ' +
    '   (A.Model               = :AModelCode          ) AND ' +
    '   (A.StudyAreaName       = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea             = :ASubAreaCode        ) AND ' +
    '   (A.Scenario            = :AScenarioCode       ) AND ' +
    '   (A.NodeCount           = :ANodeCount)     ' ;
  CWhereScenarioNodeNumber =
    ' WHERE                                              ' +
    '   (A.Model             = :AModelCode         ) AND ' +
    '   (A.StudyAreaName     = :AStudyAreaCode     ) AND ' +
    '   (A.SubArea           = :ASubAreaCode       ) AND ' +
    '   (A.Scenario          = :AScenarioCode      ) AND ' +
    '   (A.Identifier IS NOT NULL                  ) AND ' +
    '   (A.NodeNumberStorage = :ANodeNumberStorage )     ' ;

  CWhereScenarioReservoirNodeNumber =
    ' WHERE                                              ' +
    '   (A.Model             = :AModelCode         ) AND ' +
    '   (A.StudyAreaName     = :AStudyAreaCode     ) AND ' +
    '   (A.SubArea           = :ASubAreaCode       ) AND ' +
    '   (A.Scenario          = :AScenarioCode      ) AND ' +
    '   (A.ReservoirNodeNumber = :AReservoirNodeNumber )     ' ;


  CWhereScenarioDDTSDetail =
    ' WHERE                                                 ' +
    '   (A.Model               = :AModelCode          ) AND ' +
    '   (A.StudyAreaName       = :AStudyAreaCode      ) AND ' +
    '   (A.SubArea             = :ASubAreaCode        ) AND ' +
    '   (A.Scenario            = :AScenarioCode       ) AND ' +
    '   (A.Identifier          = :AIdentifier)     ' ;

//
// Table accessors.
//
const
  CReservoirArea           = ' FROM ReservoirArea A      ' + CWhereScenarioIdentifier;
  CReservoirElevation      = ' FROM ReservoirElevation A ' + CWhereScenarioIdentifier;
  CReservoirEvap           = ' FROM ReservoirEvap A    ' + CWhereScenarioIdentifier;
  CReservoirVolume         = ' FROM ReservoirVolume A    ' + CWhereScenarioIdentifier;
  CNodesDetails            = ' FROM NodesDetails A       ' + CWhereScenarioNodeNumber;
  CNodesDetailsRes         = ' FROM NodesDetails A       ' + CWhereScenarioNodeNumber;
  //CNVModelDataLink         = ' FROM NVModelDataLink A    ' + CWhereNVModelDataLinkReservoirs;
  CReservoirDetails        = ' FROM ReservoirDetails A   ' + CWhereScenarioNodeCount;
  CReservoirInitialLevels  = ' FROM ReservoirInitialLevels A ' + CWhereScenarioReservoirNodeNumber;
//  CReservoirZonePenalty    = ' FROM ReservoirZonePenalty A ' + CWhereScenarioIdentifier;
  CStorageZoneDetails      = ' FROM StorageZoneDetails A ' + CWhereScenarioIdentifier;
  CStorageZones            = ' FROM StorageZones A       ' + CWhereScenario;
  CReservoir               = ' FROM Reservoir A          ' + CWhereScenario;
  CReservoirLevels         = ' FROM ReservoirLevels A    ' + CWhereScenarioReservoirIdentifierLevelIdentifier;
  CDDTSDetail              = ' FROM DDTSDetails A        ' + CWhereScenarioDDTSDetail;
  CDDTSInputMinMax         = ' FROM DDTSInputMinMax A    ' + CWhereScenarioDDTSDetail;

  {CNVModelDataLinkElevation =
    ' SELECT *   ' + CNVModelDataLink;
    }
  CReservoirLevelsAverage =
    ' SELECT                                                                         ' +
    '   First(                                                                       ' +
    '     (A.ReservoirLev01 + A.ReservoirLev02 + A.ReservoirLev03 +                  ' +
    '      A.ReservoirLev04 + A.ReservoirLev05 + A.ReservoirLev06 +                  ' +
    '      A.ReservoirLev07 + A.ReservoirLev08 + A.ReservoirLev09 +                  ' +
    '      A.ReservoirLev10 + A.ReservoirLev11 + A.ReservoirLev12)/12)               ' + CReservoirLevels;

  CReservoirAreaGroup =
    ' FROM ReservoirGroup A      ' +
  CWhereScenario +
    ' AND (A.GroupID = :AGroupID)' ;


procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin

  //
  // NodesDetails
  //
  AAdd(0,'StatusIndicator',  'NodesDetails',   'StatusIndicator',  ' SELECT *   ' + CNodesDetails, '');
  AAdd(0,'ReservoirPriority','NodesDetails',   'ReservoirPriority',' SELECT * ' + CNodesDetails, '');
  AAdd(0,'BottomOfReservoir','NodesDetails',   'BottomOfReservoir',' SELECT * ' + CNodesDetailsRes, '');
  //AAdd(1,'BottomOfReservoir','NVModelDataLink','PenaltyValue01',   ' SELECT *    ' + CNVModelDataLink, '');
  AAdd(0,'DeadStorageLevel', 'NodesDetails',   'DeadStorageLevel', ' SELECT *  ' + CNodesDetailsRes, '');
  //AAdd(1,'DeadStorageLevel', 'NVModelDataLink','PenaltyValue02',   ' SELECT *    ' + CNVModelDataLink, '');
  AAdd(0,'FullSupplyLevel',  'NodesDetails',   'FullSupplyLevel',  ' SELECT *   ' + CNodesDetailsRes, '');
//  AAdd(1,'FullSupplyLevel',  'NVModelDataLink','PenaltyValue04',   ' SELECT *    ' + CNVModelDataLink);
//  AAdd(1,'FullSupplyLevel','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d',
//    ' SELECT                                                                                  ' +
//    '   PenaltyValue01,PenaltyValue02,PenaltyValue03,PenaltyValue04,PenaltyValue05,           ' +
//    '   PenaltyValue06,PenaltyValue07,PenaltyValue08,PenaltyValue09,PenaltyValue10            ' + CNVModelDataLink, '');

  //
  // ReservoirDetails
  //
  AAdd(0,'IncludeSummary',        'ReservoirDetails','IncludeSummary',    ' SELECT *     ' + CReservoirDetails, '');
  AAdd(0,'DrainageScale',         'ReservoirDetails','DrainageScale',     ' SELECT *      ' + CReservoirDetails, '');
  AAdd(0,'AfforestationScale',    'ReservoirDetails','AfforestationScale',' SELECT * ' + CReservoirDetails, '');
  AAdd(0,'IrrigationScale',       'ReservoirDetails','IrrigationScale',   ' SELECT *    ' + CReservoirDetails, '');
  AAdd(0,'UrbanRunOff',           'ReservoirDetails','UrbanRunoff',       ' SELECT *        ' + CReservoirDetails, '');
  AAdd(0,'NaturalInflowChannel',  'ReservoirDetails','NaturalInflowChannel', ' SELECT *    ' + CReservoirDetails, '');

  AAdd(0,'AreaFull',              'ReservoirDetails','AreaFull',          ' SELECT *           ' + CReservoirDetails, '');
  AAdd(0,'RainCoef',              'ReservoirDetails','RainCoef',          ' SELECT *           ' + CReservoirDetails, '');
  AAdd(0,'CatchmentRef',          'ReservoirDetails','CatchmentRef',      ' SELECT *       ' + CReservoirDetails, '');
  AAdd(0,'PointsCount',           'ReservoirDetails','PointsCount',       ' SELECT *        ' + CReservoirDetails, '');
  AAdd(0,'PenaltyStruct',         'ReservoirDetails','PenaltyStruct',     ' SELECT *      ' + CReservoirDetails, '');
  AAdd(0,'ReservoirName',         'ReservoirDetails','ReservoirName',     ' SELECT *      ' + CReservoirDetails, '');
  AAdd(0,'DamLevelsFileName',     'ReservoirDetails','DamLevelsFileName', ' SELECT *  ' + CReservoirDetails, '');
  AAdd(0,'XCoord',                'ReservoirDetails','XCoord',            ' SELECT *             ' + CReservoirDetails, '');
  AAdd(0,'YCoord',                'ReservoirDetails','YCoord',            ' SELECT *             ' + CReservoirDetails, '');
  AAdd(0,'ReservoirAreaGroupID',  'ReservoirDetails','GroupID',           'SELECT *             ' + CReservoirDetails, '');
//  AAdd(1,'ReservoirName',     'NVModelDataLink', 'Caption',           ' SELECT Caption            ' + CNVModelDataLink, '');

  //
  // StorageZoneDetails
  //
  AAdd(0,'StrategyIndicator','StorageZoneDetails','StrategyIndicator',' SELECT * ' + CStorageZoneDetails, '');
  AAdd(0,'BalancingVariable','StorageZoneDetails','BalancingVariable',' SELECT * ' + CStorageZoneDetails, '');
  AAdd(0,'BalancingPolicy',  'StorageZoneDetails','BalancingPolicy',  ' SELECT *   ' + CStorageZoneDetails, '');
  AAdd(0,'ReservoirZoneName','StorageZoneDetails','ReservoirZoneName',' SELECT * ' + CStorageZoneDetails, '');
  AAdd(0,'ReservoirPenalty','StorageZoneDetails','PenaltyStruct,1=BalRef%2.2d',
    ' SELECT *' +
    CStorageZoneDetails, '');
(*
  AAdd(0,'ReservoirPenalty','ReservoirZonePenalty','PenaltyStruct,1=ZonePenalty%2.2d',
    ' SELECT * ' +
    CReservoirZonePenalty, '');
*)
  //
  // Reservoir
  //
  AAdd(0,'HydroUnitsCode','Reservoir','HydroUnitsCode',' SELECT * ' + CReservoir, '');

  //
  // ReservoirLevels
  //
  AAdd(0,'ReservoirLev01','ReservoirLevels','ReservoirLev01',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev01','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev02','ReservoirLevels','ReservoirLev02',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev02','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev03','ReservoirLevels','ReservoirLev03',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev03','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev04','ReservoirLevels','ReservoirLev04',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev04','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev05','ReservoirLevels','ReservoirLev05',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev05','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev06','ReservoirLevels','ReservoirLev06',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev06','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev07','ReservoirLevels','ReservoirLev07',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev07','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev08','ReservoirLevels','ReservoirLev08',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev08','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev09','ReservoirLevels','ReservoirLev09',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev09','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev10','ReservoirLevels','ReservoirLev10',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev10','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev11','ReservoirLevels','ReservoirLev11',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev11','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);
  AAdd(0,'ReservoirLev12','ReservoirLevels','ReservoirLev12',' SELECT * ' + CReservoirLevels, '');
//  AAdd(1,'ReservoirLev12','NVModelDataLink','FieldNameIdentifier,1=PenaltyValue%2.2d', CNVModelDataLinkElevation, CReservoirLevelsAverage);

  //
  // ReservoirArea
  //

  AAdd(0,'Area','ReservoirArea','FieldNameIdentifier,1=Area%2.2d',
    ' SELECT *' +
    CReservoirArea, '');

  //
  // ReservoirElevation
  //
  AAdd(0,'SurfaceElevation','ReservoirElevation','FieldNameIdentifier,1=ReservoirElev%2.2d',
    ' SELECT *  ' +
    CReservoirElevation, '');

  // ReservoirEvaporations
  //
  AAdd(0,'Evaporation','ReservoirEvap','FieldNameIdentifier,1=Evapo%2.2d',
    ' SELECT *  ' +
    CReservoirEvap, '');
  //
  // ReservoirVolume
  //
  AAdd(0,'Volume','ReservoirVolume','FieldNameIdentifier,1=Volume%2.2d',
    ' SELECT * ' +
    CReservoirVolume, '');

 // Reservoir Initial Levels
  //
  AAdd(0,'ResInitialLevelsLev','ReservoirInitialLevels','FieldNameIdentifier,1=ResInitialLevelsLev%2.2d',
    ' SELECT * ' +
    CReservoirInitialLevels, '');
  //
  // Storage zones
  //
  AAdd(0,'ZoneRuleCurve','StorageZones','ZoneLowerBoundary',' SELECT ZoneLowerBoundary ' + CStorageZones, '');


  {****************************************************************************}
  {* Reservoir Area Group                                                             *}
  {****************************************************************************}
  AAdd(0,'ReservoirAreaGroupName', 'ReservoirGroup', 'GroupName', 'SELECT GroupName '   + CReservoirAreaGroup, '');
   {****************************************************************************}
  {* Dam Daily Time Step                                                            *}
  {****************************************************************************}
  AAdd(0,'RunoffScaleFactor', 'DDTSDetails', 'RunoffScaleFactor', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'OtherInflowScaleFactor', 'DDTSDetails', 'OtherInflowScaleFactor', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'EWRScaleFactor', 'DDTSDetails', 'EWRScaleFactor', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'TargetDraft', 'DDTSDetails', 'TargetDraft', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'DSRequirment', 'DDTSDetails', 'DSRequirment', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'DSPercRelease', 'DDTSDetails', 'DSPercRelease', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'SpillPercRelease', 'DDTSDetails', 'SpillPercRelease', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'EWRPercRelease', 'DDTSDetails', 'EWRPercRelease', 'SELECT * '   + CDDTSDetail, '');
  AAdd(0,'ImportHeadlines', 'DDTSDetails', 'ImportHeadlines', 'SELECT * '   + CDDTSDetail, '');

  AAdd(0,'MinRunoff', 'DDTSInputMinMax', 'MinRunoff', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MaxRunoff', 'DDTSInputMinMax', 'MaxRunoff', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MinOtherInflow', 'DDTSInputMinMax', 'MinOtherInflow', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MaxOtherInflow', 'DDTSInputMinMax', 'MaxOtherInflow', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MinRainfall', 'DDTSInputMinMax', 'MinRainfall', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MaxRainfall', 'DDTSInputMinMax', 'MaxRainfall', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MinEvaporation', 'DDTSInputMinMax', 'MinEvaporation', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MaxEvaporation', 'DDTSInputMinMax', 'MaxEvaporation', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MaxIncreamentalRunoff', 'DDTSInputMinMax', 'MaxIncreamentalRunoff', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MinIncreamentalRunoff', 'DDTSInputMinMax', 'MinIncreamentalRunoff', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MaxEWR', 'DDTSInputMinMax', 'MaxEWR', 'SELECT * '   + CDDTSInputMinMax, '');
  AAdd(0,'MinEWR', 'DDTSInputMinMax', 'MinEWR', 'SELECT * '   + CDDTSInputMinMax, '');

end;

end.
