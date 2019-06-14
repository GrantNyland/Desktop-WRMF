//
//  UNIT      : Contains TAbstractDbTableProperty Class
//  AUTHOR    : Philemon Setshedi(PDNA)
//  DATE      : 2004/03/23
//  COPYRIGHT : Copyright © 2004 DWAF
//
unit UDbTableProperty;
interface
uses
  DB,
  UDWADBComponents,
  Classes,
  UAbstractObject;
  const
    tgNone        = 0;
    tgModelData   = 1;
    tgSystem      = 2;
    tgSpecial     = 3;
    tgBaseData    = 4;
    tgOutputData  = 5;
type

  TDbTableProperty = class(TAbstractDbTableProperty)
  protected
    FTableName       : string;
    FTableIndex      : integer;
    FModelNames      : TStringList;
    FFieldNames      : TStringList;
    FIndexFieldNames : TStringList;
    FTableGroup      : integer;
    FTableFilter     : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetTableName: string; override;
    function GetTableIndex: integer; override;
    function GetModelNames: TStringList; override;
    function GetFieldNames: TStringList; override;
    function GetIndexFieldNames: TStringList; override;
    function GetTableGroup: integer; override;
    function GetTableFilter: string; override;
    procedure SetTableFilter(AFilter: string); override;
    function GetDelimitedFieldNamesCommatext: String; override;
  public
    function FieldsParams: string;override;
    function Initialise: boolean; override;
    procedure SetMemberVariables(ATableName,AModelNames : string; ATableIndex : integer;
              AIndexFieldNames: string; AFieldNames: String; ATableGroup : integer;
              ATableFilter      : string);
  end;

  TDbTablePropertyItemAddFunction = procedure (
  ATableName        : string;
  AModelNames       : string;
  ATableIndex       : integer;
  AIndexFieldNames  : string;
  AFieldNames       : String;
  ATableGroup       : integer;
  ATableFilter      : string) of object;

procedure LoadTablePropertyData(AAdd: TDbTablePropertyItemAddFunction);
procedure LoadTablePropertyData1(AAdd: TDbTablePropertyItemAddFunction);
procedure LoadTablePropertyData2(AAdd: TDbTablePropertyItemAddFunction);

implementation
uses
  SysUtils,
  UErrorHandlingOperations;


{ TDbTableProperty }

procedure TDbTableProperty.CreateMemberObjects;
const OPNAME = 'TDbTableProperty.CreateMemberObjects';
begin
  inherited;
  try
    FTableName       := '';
    FTableIndex      := -1;
    FModelNames      := TStringList.Create;
    FIndexFieldNames := TStringList.Create;
    FFieldNames      := TStringList.Create;
    FTableGroup      := tgNone;
    FTableFilter     := '';
    FModelNames.CaseSensitive      := False;
    FIndexFieldNames.CaseSensitive := False;
    FFieldNames.CaseSensitive      := False;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TDbTableProperty.DestroyMemberObjects;
const OPNAME = 'TDbTableProperty.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FModelNames);
    FreeAndNil(FIndexFieldNames);
    FreeAndNil(FFieldNames);
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.Initialise: boolean;
const OPNAME = 'TDbTableProperty.Initialise';
begin
  Result := inherited Initialise;
  try
    FTableName       := '';
    FTableIndex      := -1;
    FModelNames.Clear;
    FIndexFieldNames.Clear;
    FFieldNames.Clear;
    Result := True;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TDbTableProperty.SetMemberVariables(ATableName,AModelNames : string;
  ATableIndex : integer; AIndexFieldNames: string; AFieldNames: String;
  ATableGroup : integer;ATableFilter      : string);
const OPNAME = 'TDbTableProperty.SetMemberVariables';
begin
  try
    FTableName := ATableName;
    FTableIndex := ATableIndex;
    FModelNames.CommaText := AModelNames;
    FIndexFieldNames.CommaText := AIndexFieldNames;
    FFieldNames.CommaText := AFieldNames;
    FTableGroup := ATableGroup;
    FTableFilter := ATableFilter;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetDelimitedFieldNamesCommatext: String;
const OPNAME = 'TDbTableProperty.GetDelimitedFieldNamesCommatext';
begin
  Result := FFieldNames.CommaText;
  try
    Result := '['+Result+']';
    Result := StringReplace(Result,',','],[',[rfReplaceAll]);
  except on E: Exception do HandleError(E,OPNAME) end;

end;

function TDbTableProperty.GetFieldNames: TStringList;
const OPNAME = 'TDbTableProperty.GetFieldNames';
begin
  Result := nil;
  try
    Result := FFieldNames;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetIndexFieldNames: TStringList;
const OPNAME = 'TDbTableProperty.GetIndexFieldNames';
begin
  Result := nil;
  try
    Result := FIndexFieldNames;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetModelNames: TStringList;
const OPNAME = 'TDbTableProperty.GetModelNames';
begin
  Result := nil;
  try
    Result := FModelNames;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetTableIndex: integer;
const OPNAME = 'TDbTableProperty.GetTableIndex';
begin
    Result := -1;
  try
    Result := FTableIndex;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetTableName: string;
const OPNAME = 'TDbTableProperty.GetTableName';
begin
  Result := '';
  try
    Result := FTableName;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetTableGroup: integer;
const OPNAME = 'TDbTableProperty.GetTableGroup';
begin
  Result := tgNone;
  try
    Result := FTableGroup;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.GetTableFilter: string;
const OPNAME = 'TDbTableProperty.GetTableFilter';
begin
  Result := '';
  try
    Result := FTableFilter;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TDbTableProperty.SetTableFilter(AFilter: string);
const OPNAME = 'TDbTableProperty.SetTableFilter';
begin
  try
    FTableFilter := AFilter;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TDbTableProperty.FieldsParams: string;
const OPNAME = 'TDbTableProperty.FieldsParams';
var
  LIndex: integer;
begin
  Result := '';
  try
    if (FFieldNames.Count > 0) then
    begin
      Result := '(';
      for LIndex := 0 to FFieldNames.Count -1 do
      begin
        if(LIndex = (FFieldNames.Count -1)) then
          Result := Result + ':'+ FFieldNames[LIndex]
        else
          Result := Result + ':'+ FFieldNames[LIndex] +',';
      end;
      Result := Result + ')';
    end;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure LoadTablePropertyData(AAdd: TDbTablePropertyItemAddFunction);
const OPNAME = 'UDbTableProperty.LoadTablePropertyData';
begin
  AAdd('PumpingFeature','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'FeatureName,PumpingEfficiency,PumpingHead,ChannelNumber',tgModelData,'');

  AAdd('RainfallDailyData','Rainfall',0,
       'StationID',
       'StationID,OldStationID,StationNumber,DataStartYear,DataStartMonth,DataEndYear,DataEndMonth,' +
       'NoOfYears,NoOfMonths,BlobUncompressedSize,RainfallData',
       tgBaseData,'');
  AAdd('RainfallMonthlyRAWData','Rainfall',0,
       'StationID,Year',
       'StationID,Year,Source,Value01,Flag01,Value02,Flag02,Value03,Flag03,'+
       'Value04,Flag04,Value05,Flag05,Value06,Flag06,Value07,Flag07,Value08,Flag08,'+
       'Value09,Flag09,Value10,Flag10,Value11,Flag11,Value12,Flag12',
       tgBaseData,'');
  AAdd('RainfallStations','Rainfall',0,
       'StationID',
       'StationID,StationNumber,StationOwner,StationLongitude,StationLatitude,StationName,'+
       'Source,WR90,StationHeight,StationType',
       tgBaseData,'');
  AAdd('RainfallPatchType','Rainfall',0,
       'PatchTypeID',
       'PatchTypeID,Description',
       tgSystem,'');
  AAdd('RainfallPatchWRC','Rainfall',0,
       'PatchID',
       'PatchID,StationID,PatchTypeID,StartYear,EndYear,Description',
       tgBaseData,'');
  AAdd('RainfallMonthlyWRCData','Rainfall',0,
       'PatchID,StationID,Year',
       'PatchID,StationID,Year,Source,Value01,Flag01,Value02,Flag02,Value03,Flag03,'+
       'Value04,Flag04,Value05,Flag05,Value06,Flag06,Value07,Flag07,Value08,Flag08,'+
       'Value09,Flag09,Value10,Flag10,Value11,Flag11,Value12,Flag12',
       tgBaseData,'');
  AAdd('RainfallCatchment','Rainfall',0,
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID',
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,ChangeDate,RunDate,' +
       'OutputFileName,CatchmentFileName,OutputFileData,CatchmentFileData',
       tgModelData,'');
  AAdd('RainfallCatchmentFileData','Rainfall',0,
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,Year',
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,Year,GaugesUsed,NoOfGauges,'+
       'PercentageOfMAP01,PercentageOfMAP02,PercentageOfMAP03,PercentageOfMAP04,PercentageOfMAP05,PercentageOfMAP06,'+
       'PercentageOfMAP07,PercentageOfMAP08,PercentageOfMAP09,PercentageOfMAP10,PercentageOfMAP11,PercentageOfMAP12',
       tgModelData,'');

  AAdd('RainfallCatchmentFileDetail','Rainfall',0,
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,StationID,SourceID',
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,StationID,SourceID,Section,Position,MAP,PeriodOfRecord,Longitude,Latitude',
       tgModelData,'');       {8267}

  AAdd('RainfallCatchmentSource','Rainfall',0,
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,SourcePatchID,StationID,SplitID',
       'Model,StudyAreaName,SubArea,Scenario,CatchmentID,SourcePatchID,StationID,SplitID,HydroStartYear,HydroEndYear',
       tgModelData,'');
  AAdd('RainfallProjectGauges','Rainfall',0,
       'Model,StudyAreaName,SubArea,Scenario,StationID',
       'Model,StudyAreaName,SubArea,Scenario,StationID',
       tgModelData,'');
  AAdd('RainfallPatchR','Rainfall',0,
       'StudyAreaName,SubArea,PatchID',
       'StudyAreaName,SubArea,PatchID,PatchTypeID,Description,PatchMultipleStations,' +
       'PatchStartYear,PatchEndYear,PatchChangeDate,ClassRDate,'+
       'ClassRInputFileName,ClassRInput,ClassROutputFileName,ClassROutput,PatchRInputFileName,'+
       'PatchRInput,PatchRInputDate,PatchROutputDate,'+
       'PatchRPrintedFileName,PatchRPrinted,PatchRPrintedDate,' +
       'PatchRPlottedFileName,PatchRPlotted,PatchRPlottedDate',
       tgModelData,'');
  AAdd('RainfallPatchSource','Rainfall',0,
       'PatchID,SourceStationID,SourcePatchID',
       'PatchID,SourceStationID,SourcePatchID,TargetStation,HydroStartYear,HydroEndYear',
       tgModelData,'');
  AAdd('RainfallMonthlyPatchData','Rainfall',0,
       'PatchID,StationID,Year',
       'PatchID,StationID,Year,Source,Value01,Flag01,Value02,Flag02,Value03,Flag03,'+
       'Value04,Flag04,Value05,Flag05,Value06,Flag06,Value07,Flag07,Value08,Flag08,'+
       'Value09,Flag09,Value10,Flag10,Value11,Flag11,Value12,Flag12',
       tgModelData,'');
  AAdd('RainfallRAWFlags','Rainfall',0,
       'RAWFlags',
       'RAWFlags,WetSeasonZeros,WetSeasonMonths,MonthlyGreaterProportion,MonthlyProportionValue,'+
       'AnnualGreater,AnnualLess,MontlyGreaterAbsolute,MonthlyGreaterAbsoluteValue,RoundedValues,RepeatingValues',
       tgModelData,'');
  AAdd('RainfallRAWSplits','Rainfall',0,
       'StationID,HydroStartYear,HydroEndYear',
       'StationID,HydroStartYear,HydroEndYear',
       tgModelData,'');
  AAdd('RainfallScaledDownPatchValues','Rainfall',0,
       'PatchID,StationID,Identifier',
       'PatchID,StationID,Identifier,HydroYear,Month,PatchScaledDownStatus',
       tgModelData,'');

  AAdd('RainfallUserStations','Rainfall',0,
       'StationID',
       'StationID,Source,StationNumber,StationLongitude,StationLatitude,StationName,'+
       'StationHeight,StationType,StationLinkID,StationLinkNumber',
       tgModelData,'');
  AAdd('RainfallUserMonthlyData','Rainfall',0,
       'StationID,Year',
       'StationID,Year,Value01,Flag01,Value02,Flag02,Value03,Flag03,'+
       'Value04,Flag04,Value05,Flag05,Value06,Flag06,Value07,Flag07,Value08,Flag08,'+
       'Value09,Flag09,Value10,Flag10,Value11,Flag11,Value12,Flag12',
       tgModelData,'');

  AAdd('Reservoir','Yield,Planning,DDTS',18,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,ReservoirCount,HydroUnitsCode,ReservoirComment',tgModelData,'');
  AAdd('ReservoirArea','Yield,Planning,DDTS',19,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,Area01,Area02,Area03,Area04,Area05,Area06,Area07,Area08,Area09,Area10,Area11,Area12,Area13,Area14,Area15',tgModelData,'');
  AAdd('ReservoirChannels','Yield,Planning,DDTS',20,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,Channel01,Channel02,Channel03,Channel04,Channel05,Channel06,Channel07,Channel08,Channel09,Channel10,Channel11,Channel12,Channel13,Channel14,Channel15,Channel16,'+
       'Channel17,Channel18,Channel19,Channel20',tgModelData,'');
  AAdd('ReservoirDetails','Yield,Planning,DDTS',21,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirName,IncludeSummary,NodeCount,' +
       'PenaltyStruct,PointsCount,DrainageScale,AfforestationScale,IrrigationScale,AreaFull,RainCoef,'+
       'CatchmentRef,ChannelsCount,ReservoirDetailsComment,NodeType,DamLevelsFileName,UrbanRunoff,NaturalInflowChannel,'+
       'Comment02,Comment03,Comment04,XCoord,YCoord,GroupID',tgModelData,'');

  AAdd('ReservoirElevation','Yield,Planning,DDTS',22,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirElev01,ReservoirElev02,ReservoirElev03,ReservoirElev04,ReservoirElev05,ReservoirElev06,ReservoirElev07,ReservoirElev08,ReservoirElev09,ReservoirElev10,'+
       'ReservoirElev11,ReservoirElev12,ReservoirElev13,ReservoirElev14,ReservoirElev15',tgModelData,'');
  AAdd('ReservoirEvap','Yield,Planning,DDTS',23,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,Evapo01,Evapo02,Evapo03,Evapo04,Evapo05,Evapo06,Evapo07,Evapo08,Evapo09,Evapo10,Evapo11,Evapo12',tgModelData,'');

  AAdd('ReservoirGroup','Yield,Planning',23,'Model,StudyAreaName,SubArea,Scenario,GroupID','Model,StudyAreaName,SubArea,Scenario,GroupID,GroupName',tgModelData,'');

  AAdd('ReservoirHydrologyFile','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,CatchmentRef','Model,StudyAreaName,SubArea,Scenario,Identifier,CatchmentRef,FileName',tgModelData,'');
  AAdd('ReservoirInitialLevels','Yield,Planning,DDTS',91,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirNodeNumber,ResInitialLevelsLev01,ResInitialLevelsLev02,ResInitialLevelsLev03,ResInitialLevelsLev04,ResInitialLevelsLev05,ResInitialLevelsLev06,ResInitialLevelsLev07,'+
       'ResInitialLevelsLev08,ResInitialLevelsComment',tgModelData,'');
  AAdd('ReservoirLevels','Yield,Planning,DDTS',39,'Model,StudyAreaName,SubArea,Scenario,RecordIdentifier','Model,StudyAreaName,SubArea,Scenario,RecordIdentifier,ReservoirIdentifier,LevelIdentifier,ReservoirLev01,ReservoirLev02,ReservoirLev03,ReservoirLev04,ReservoirLev05,ReservoirLev06,ReservoirLev07,'+
       'ReservoirLev08,ReservoirLev09,ReservoirLev10,ReservoirLev11,ReservoirLev12,ResLevelsComment',tgModelData,'');
  AAdd('ReservoirTimeControl','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirNumber,BaseNodeNumber,ReservoirStartYear,'+
       'ReservoirStartMonth,ReservoirEndYear,ReservoirEndMonth,ReservoirEconomicLife,ReservoirCapitalCost,'+
       'ReservoirOMCost,ReservoirCostSchedule',tgModelData,'');
  AAdd('ReservoirVolume','Yield,Planning,DDTS',24,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,Volume01,Volume02,Volume03,Volume04,Volume05,Volume06,Volume07,Volume08,Volume09,Volume10,Volume11,Volume12,Volume13,Volume14,Volume15',tgModelData,'');
  AAdd('ReturnFlowChannelDetail','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,GaugeNumber,MonthlyAvrgFactor,'+
       'CalibrationFactor,MonthlyAvrgFactorEvap,RoutingConstant,CurtailmentFactor,MultiplicationFactor',tgModelData,'');
  AAdd('ReturnFlowCorrespondingChannel','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber,AbstractionChannel,AssumedFactor',tgModelData,'');
  AAdd('ReturnFlowMonthlyEvaporation','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,PotentialMonthlyEvap01,PotentialMonthlyEvap02,PotentialMonthlyEvap03,'+
       'PotentialMonthlyEvap04,PotentialMonthlyEvap05,PotentialMonthlyEvap06,PotentialMonthlyEvap07,PotentialMonthlyEvap08,PotentialMonthlyEvap09,'+
       'PotentialMonthlyEvap10,PotentialMonthlyEvap11,PotentialMonthlyEvap12',tgModelData,'');
  AAdd('RunParameters','Yield,Planning',11,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,NumPeriods,StartYearG,StartYearO,' +
       'DebugInit,DebugFinal,DebugLevel,SummaryLevel,SummaryOut,StoreYield,RandomOpt,PlotOpt,LimitOpt,MultPeriodOpt,CalcHistoryOpt,ReduceSeqOpt,'+
       'YearsCount,HydroSeqCount,LoadCasesCount,StartMonthNo,RunType,StartType,ParamFile,TargetRecurrenceInterval,DetailedOption,SupplyOption,' +
       'AnnualSummary,EconomicOption,PlanningSummary,InputSummary,WaterQualityOption,PeriodsPerYear,CalendarStartMonth,ShortTermPlanningOption,HydroPowerOption,' +
       'AllocationControlOption',tgModelData,'');
  AAdd('RunTitle','Yield,Planning',10,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,Title1,Title2,Title3',tgModelData,'');
  AAdd('SFRSubCatchment','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'InflowNodeNumber,CoveredArea,UnitRunoffFileName,SoilMoistureFileName,SFRName,SFRDescr,Comment1,Comment2,Comment3',tgModelData,'');

  AAdd('SpecifiedDemandFeature','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'FeatureName,ChannelNumber,Fullname,GaugeNumber,Stochastic',tgModelData,'');
  AAdd('SpecifiedInflowFeature','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'FeatureName,ChannelNumber,InflowFileName',tgModelData,'');
  AAdd('StorageZoneDetails','Yield,Planning',37,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirZoneName,StrategyIndicator,BalancingVariable,BalancingPolicy,' +
       'BalRef01,BalRef02,BalRef03,BalRef04,BalRef05,' +
       'BalRef06,BalRef07,BalRef08,BalRef09,BalRef10,' +
       'BalRef11,BalRef12,BalRef13,BalRef14,BalRef15,' +
       'BalRef16,BalRef17,BalRef18,BalRef19,BalRef20,' +
       'StorageZoneDetailsComment',tgModelData,'');
  AAdd('StomsaArea','Stomsa',1,'Model,StudyAreaName,SubArea,Scenario,FileName','Model,StudyAreaName,SubArea,Scenario,FileName,Area',tgModelData,'');
  AAdd('StomsaProjectFile','Stomsa',2,'Model,StudyAreaName,SubArea,Scenario,FileName','Model,StudyAreaName,SubArea,Scenario,FileName,FileData',tgModelData,'');
  AAdd('StorageZones','Yield,Planning',36,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,StorageZoneCount,ZoneLowerBoundary,PenaltyStructureCount,NodesCount,ReservoirLevelsCount,StorageZonesComment,ZeroComment',tgModelData,'');
  AAdd('StudyArea','Yield,Planning,Rainfall,Hydrology,DailyDiversion,IFRPreProcessor,Stomsa,RWH,DDTS,DamSedimentation',122,
       'Model,StudyAreaName',
       'Model,StudyAreaName,StudyDate,Consultant,Client,StudyNumber,StudyLabel,StudyAreaDescr,ShapeFileName',tgModelData,'');
  AAdd('StudyModel','Yield,Planning,Rainfall,Hydrology,DDTS',0,'Model','Model,ModelLabel,ModelDescr',tgSystem,'');
  AAdd('StudyScenario','Yield,Planning,Rainfall,Hydrology,DailyDiversion,IFRPreProcessor,Stomsa,RWH,DDTS,DamSedimentation',120,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,ScenarioLabel,ScenarioDescr,DataFilesPrefix,DataFilesPath,FilesLoaded,CalenderStartMonth,Version,StudyImportDate,LastUpdateDate,DataImported',tgModelData,'');
  AAdd('StudySubArea','Yield,Planning,Rainfall,Hydrology,DailyDiversion,IFRPreProcessor,Stomsa,RWH,DDTS,DamSedimentation',121,
       'Model,StudyAreaName,SubArea',
       'Model,StudyAreaName,SubArea,SubAreaLabel,SubAreaDescr,ShapeFileName,' +
       'TopLeftCoord,TopRightCoord,BottomLeftCoord,BottomRightCoord',tgModelData,'');
  AAdd('StudyTableNames','Yield,Planning',0,'Model,StudyTableName','Model,StudyTableName,Sequence,Context,IndexLevel',tgSystem,'');
  AAdd('StudyScenarioLock','Yield,Planning,Rainfall',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,Usename,LockDate,InstanceID',tgModelData,'');
  AAdd('suBlockAnualAverageInflowValues','Yield,Planning',40,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,AverageInflowValue01,AverageInflowValue02,AverageInflowValue03,AverageInflowValue04,'+
       'AverageInflowValue05,AverageInflowValue06,AverageInflowValue07,AverageInflowValue08,AverageInflowValue09,AverageInflowValue10,InflowType',tgOutputData,'');
  AAdd('suBlockAnualSummaryValues','Yield,Planning',41,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,FirstInteger,YearPeriod,SystemSupplyVolume,SelectedYieldLabel,AnualSummaryCaption,'+
       'AnualSummaryValue01,AnualSummaryValue02,AnualSummaryValue03,AnualSummaryValue04,AnualSummaryValue05,AnualSummaryValue06,AnualSummaryValue07,AnualSummaryValue08,AnualSummaryValue09,AnualSummaryValue10',tgOutputData,'');
  AAdd('suBlockAvarageValues','Yield,Planning',42,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,AverageValue01,AverageValue02,AverageValue03,AverageValue04,AverageValue05,AverageValue06,AverageValue07,'+
       'AverageValue08,AverageValue09,AverageValue10,AverageValue11,AverageValue12,AverageValue13,AverageValue14,AverageValue15,Violations',tgOutputData,'');
  AAdd('suBlockCriticalPeriodsValues','Yield,Planning',43,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,FirstInteger,CriticalPeriodValCaption,CriticalPeriodsValue01,CriticalPeriodsValue02,CriticalPeriodsValue03,CriticalPeriodsValue04,'+
       'CriticalPeriodsValue05,CriticalPeriodsValue06,CriticalPeriodsValue07,CriticalPeriodsValue08,CriticalPeriodsValue09,CriticalPeriodsValue10',tgOutputData,'');
  AAdd('suBlockDescription','Yield,Planning',44,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,BlockType,ElementID,AnnualWaterDemand,AnnualPowerDemand,BlockHeading,BlockTitle',tgOutputData,'');
  AAdd('suBlockGenericValues','Yield,Planning',45,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,GenericValue01,GenericValue02,GenericValue03,'+
       'GenericValue04,GenericValue05,GenericValue06,GenericValue07,GenericValue08,GenericValue09,GenericValue10,GenericValue11,GenericValue12,GenericValue13,GenericValue14,GenericValue15',tgOutputData,'');
  AAdd('suBlockHeader','Yield,Planning',49,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,LineData',tgOutputData,'');
  AAdd('suBlockOutputSummaryValues','Yield,Planning',46,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,ReserviorName,OutputSummaryValue01,'+
       'OutputSummaryValue02,OutputSummaryValue03,OutputSummaryValue04,OutputSummaryValue05,OutputSummaryValue06,OutputSummaryValue07,OutputSummaryValue08,OutputSummaryValue09,OutputSummaryValue10,OutputSummaryValue11,OutputSummaryValue12,OutputSummaryValue13,'+
       'OutputSummaryValue14,OutputSummaryValue15',tgOutputData,'');
  AAdd('suBlockSequencesWithFailuresValues','Yield,Planning',47,'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,LoadCaseNumber,SequenceNumber,Identifier,SequenceFailuresValue01,'+
       'SequenceFailuresValue02,SequenceFailuresValue03,SequenceFailuresValue04,SequenceFailuresValue05,SequenceFailuresValue06,SequenceFailuresValue07,SequenceFailuresValue08,SequenceFailuresValue09,SequenceFailuresValue10',tgOutputData,'');
  AAdd('suBlockTypes','Yield,Planning',0,'BlockType','BlockType,Description',tgSystem,'');
  AAdd('suUnknownData','Yield,Planning',48,'Model,StudyAreaName,SubArea,Scenario,FileNumber,LineNumber,LineSection','Model,StudyAreaName,SubArea,Scenario,FileNumber,LineNumber,LineSection,LineData',tgOutputData,'');
  AAdd('suComplete','Yield,Planning',48,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,BlobName,BlobData',tgOutputData,'');

  AAdd('SwitchDefinition','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,SwitchDefID',
       'Model,StudyAreaName,SubArea,Scenario,SwitchDefID,' +
       'SwitchDefStartYear,SwitchDefStartMonth,SwitchDefFileName',tgModelData,'');
  AAdd('SummaryChannels','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'ChannelNumber',tgModelData,'');


  AAdd('TargetPower','Yield,Planning',17,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,TPower1,TPower2,TPower3,TPower4,TPower5,TPower6,TPower7,TPower8,TPower9,TPower10',tgModelData,'');
  AAdd('TargetYield','Yield,Planning',15,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,TYield1,TYield2,TYield3,TYield4,TYield5,TYield6,TYield7,TYield8,TYield9,TYield10',tgModelData,'');
  AAdd('TSCChart','Yield,Planning',0,'Model,StudyAreaName,SubArea,ChartName','Model,StudyAreaName,SubArea,ChartName,CurrentSeriesName,DateCreated,HeaderCaption,FooterCaption',tgModelData,'');
  AAdd('TSCChartSeries','Yield,Planning',0,'Model,StudyAreaName,SubArea,ChartName,SeriesName','Model,StudyAreaName,SubArea,ChartName,SeriesName',tgModelData,'');
  AAdd('TSCSeries','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,ChartName,SeriesName,ViewID,ParentID,TopParentID','Model,StudyAreaName,SubArea,Scenario,ChartName,SeriesName,ViewID,ParentID,TopParentID,CommaTextCaption,Color,YAxisPosition',tgModelData,'');
  AAdd('TSCView','Yield,Planning',0,'Model,StudyAreaName,SubArea,ViewName','Model,StudyAreaName,SubArea,ViewName,CurrentChartName,DateCreated,HeaderCaption,FooterCaption',tgModelData,'');
  AAdd('TSCViewChart','Yield,Planning',0,'Model,StudyAreaName,SubArea,ViewName,ChartName','Model,StudyAreaName,SubArea,ViewName,ChartName,LeftAxisMin,LeftAxisMax,BottomAxisMin,BottomAxisMax,RightAxisMin,RightAxisMax',tgModelData,'');
  AAdd('TSCViewScenario','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,ViewName','Model,StudyAreaName,SubArea,Scenario,ViewName',tgModelData,'');
  AAdd('UserRights','Yield,Planning',0,'UserRights','UserRights,RightsDescr',tgSystem,'');

  AAdd('Users','Yield,Planning',0,'UserId','UserId,Password,Initials,FirstName,SecondName,LastName,PreferedLanguage,UserRights,UserType,Model,AutoLogon,AutoSelectStudy,CreatedBy',tgSystem,'');
  AAdd('UserType','Yield,Planning',0,'UserRights','UserRights,RightsDescr',tgSystem,'');
  AAdd('ViewDataJump','Yield,Planning',0,'ViewID,JumpToID','ViewID,JumpToID',tgSystem,'');
  AAdd('ViewDataNode','Yield,Planning',0,'Model,ViewID,ParentID','Model,ViewID,ParentID,Weighting,ShowSQL,BitmapName,DatasetID,SubNodesDatasetID',tgSystem,'');
  AAdd('ViewDataSet','Yield,Planning',0,'DatasetID','DatasetID,Editable,SQLType,NoDataMessage,ViewSQL',tgSystem,'');
//  AAdd('VNVDefaultDrawing','Yield,Planning',0,'DrawingName','DrawingName,DrawingObject',tgSystem,'');
  AAdd('VNVDrawing','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingID','Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingID,DrawingName,ReadOnly,GISMode',tgModelData,'');
  AAdd('VNVDrawingGroup','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,DrawingGroupID','Model,StudyAreaName,SubArea,Scenario,DrawingGroupID,DrawingGroupName',tgModelData,'');

  AAdd('WaterDemandCategories','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,CategoryID',
       'Model,StudyAreaName,SubArea,Scenario,CategoryID,CategoryDescription,CriteriaPortion01,CriteriaPortion02,CriteriaPortion03,'+
       'CriteriaPortion04,CriteriaPortion05,CriteriaPortion06,CriteriaPortion07,CriteriaPortion08,CriteriaPortion09,CriteriaPortion10',tgSystem,'');
  AAdd('WeatherEvents','Rainfall',0,'EventID','EventID,Area,StartDateTime,EndDateTime,Event,Comments,Latitude,Longitude',tgSystem,'');

  AAdd('Wetland','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,NodeNumber,WetlandName,UpstreamThreshold,InflowProportion,Storage,'+
       'OutflowProportion,InflowChannelNumber,OutflowChannelNumber',tgModelData,'');

  AAdd('YMDemandCentre','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,NodeNumber,CentreName,'+
       'NodeRefNr,AveReturnFlowFactor,AveEvaporation,StdDeviationFactor,RoutingConstant,'+
       'RainfallScalingFactor,TotalFlowLost,EvapoTranspiration01,EvapoTranspiration02,'+
       'EvapoTranspiration03,EvapoTranspiration04,EvapoTranspiration05,EvapoTranspiration06,'+
       'EvapoTranspiration07,EvapoTranspiration08,EvapoTranspiration09,EvapoTranspiration10,'+
       'EvapoTranspiration11,EvapoTranspiration12,CentreDescription,ConsumptiveChannelNr,ReclaimationChannelNr',tgModelData,'');

  AAdd('YMDemandCentreReturnFlowChannel','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,DemandCentreID,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,DemandCentreID,Identifier,ChannelNr,TotalReturnFlow,FlowDiversion',tgModelData,'');

  AAdd('WaterDemandCounts','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,ScenarioCount',tgModelData,'');
  AAdd('WaterDemandFeatures','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,FeatureID',
       'Model,StudyAreaName,SubArea,Scenario,FeatureID,FeatureName,ChannelNumber,CategoryID,'+
       'ScenarioPortion01,ScenarioPortion02,ScenarioPortion03,ScenarioPortion04,ScenarioPortion05,ScenarioPortion06,ScenarioPortion07,ScenarioPortion08,ScenarioPortion09,ScenarioPortion10',tgModelData,'');
  AAdd('WaterDemandRiskCriteria','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,Interval01,Interval02,Interval03,Interval04,Interval05,Interval06,Interval07,Interval08,Interval09,Interval10',tgModelData,'');
  AAdd('WaterUseProportions','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,ChannelNumber',
       'Model,StudyAreaName,SubArea,Scenario,ChannelNumber,Category01,Category02,Category03,Category04,Category05,Category06,Category07,Category08,Category09,Category10',tgModelData,'');
  AAdd('WRYMDat','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,FilePrefix,PrefixComment,InputPath,InputComment,OutputPath,OutputComment,HydrologyPath,SpecifiedDemandPath,Comment',tgModelData,'');
  AAdd('WRYMFileLines','Yield,Planning',50,'Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection','Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData',tgModelData,'');
  AAdd('yrcChart','Yield,Planning',101,'Model,StudyAreaName,SubArea,Scenario,ChartID','Model,StudyAreaName,SubArea,Scenario,ChartID,DateCreated,ChartName,CurrentChart,PlaneIndex,RequiredPlaneIndex',tgModelData,'');
  AAdd('yrcChartProperty','Yield,Planning',102,'Model,StudyAreaName,SubArea,Scenario,ChartID','Model,StudyAreaName,SubArea,Scenario,ChartID,PlanesCount,YearsCount,SequencesCount,MarginBottom,MarginLeft,MarginRight,MarginTop,BottomAxisIncrement,'+
       'BottomAxisMaximum,BottomAxisMinorTickCount,LeftAxisIncrement,LeftAxisMaximum,LeftAxisMinorTickCount,RightAxisIncrement,RightAxisMaximum,RightAxisMinorTickCount,LegendTopPos,LegendVertMargin,Label,BottomAxisMinimum,LeftAxisMinimum,RightAxisMinimum',tgModelData,'');
  AAdd('yrcChartViewType','Yield,Planning',0,'ChartViewType','ChartViewType,ChartViewTypeDesc',tgSystem,'');
  AAdd('yrcLanguageStrings','Yield,Planning',103,'Model,StudyAreaName,SubArea,Scenario,ChartID','Model,StudyAreaName,SubArea,Scenario,ChartID,CaptionFormat1,CaptionFormat2,YearsFormat,YieldFormat,LegendCaption,BottomAxisCaption,LeftAxisCaption',tgModelData,'');

  AAdd('yrcPlane','Yield,Planning',104,
        'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber','Model,StudyAreaName,'+
        'SubArea,Scenario,ChartID,PlaneNumber,YearNumber,TargetDraftIndex',tgModelData,'');

  AAdd('yrcTargetDraft','Yield,Planning',105,'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID','Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,SavedMode,TargetDraftXValue,TargetDraftYValue,'+
       'TargetDraftYears,TargetDraftRecurance,IndexOf100,DetPointsChanged,ForceCurveThrough100,Point1X,Point1XT,Point2X,Point2XT,LabelText,LabelLeftTopX,LabelLeftTopY,LabelArrowToX,LabelArrowToY,LabelCustom',tgModelData,'');
  AAdd('yrcTargetDraftConst','Yield,Planning',106,'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,CurveType','Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,CurveType,AConstValue,BConstValue,'+
       'CConstValue,DConstValue',tgModelData,'');
  AAdd('yrcTargetDraftPoint','Yield,Planning',107,'Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,CurveType,PointIndex','Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,CurveType,PointIndex,'+
       'XValue,YValue',tgModelData,'');
  AAdd('StudyErrorMetaData','Yield,Planning',108,'ErrorType','ErrorType,ErrorName',tgSystem,'');
  AAdd('StudyMetaData','Yield,Planning',109,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ImportedBy,ErrorType,ErrorDescription,StudyErrors,CorrectiveAction,ReadOnly',tgModelData,'');

  LoadTablePropertyData1(AAdd);
end;

procedure LoadTablePropertyData1(AAdd: TDbTablePropertyItemAddFunction);
const OPNAME = 'LoadTablePropertyData1';
begin
  //Special tables must be processed first as they may need data in other tables.
  AAdd('ChangeGroup',       'Yield,Planning,Rainfall',1,
       'Model,StudyAreaName,SubArea,Scenario,GroupID',
       'Model,StudyAreaName,SubArea,Scenario,GroupID,GroupName',
       tgSpecial,'');
  AAdd('ChangeGroupElement','Yield,Planning,Rainfall',1,
       'Model,StudyAreaName,SubArea,Scenario,GroupID,ElementID,IsElementGroup',
       'Model,StudyAreaName,SubArea,Scenario,GroupID,ElementID,IsElementGroup,ElementActive,ElementOrder',
       tgSpecial,'');
  AAdd('ChangeList',    'Yield,Planning,Rainfall',1,
       'Model,StudyAreaName,SubArea,Scenario,ChangeListID',
       'Model,StudyAreaName,SubArea,Scenario,ChangeListID,ListName,DateCreated,CreatedBy,ListDescr,ChangeListKey',
       tgSpecial,'');
  AAdd('ChangeParameter','Yield,Planning,Rainfall',1,
       'Model,StudyAreaName,SubArea,Scenario,ChangeListID,ParamField,KeyValues,FieldIndex',
       'Model,StudyAreaName,SubArea,Scenario,ChangeListID,ParamField,KeyValues,FieldIndex,Absolut,Change,ParamDescr,Filtered',
       tgSpecial,'');
  AAdd('HydrologyFileData','Yield,Planning',0,'StudyAreaName,FileName,Identifier','StudyAreaName,FileName,Identifier,HydroYearValue,HydroYearValuePatch,HydroMonthValue01,HydroMonthValue01Patch,HydroMonthValue02,HydroMonthValue02Patch,HydroMonthValue03,HydroMonthValue03Patch,HydroMonthValue04,'+
       'HydroMonthValue04Patch,HydroMonthValue05,HydroMonthValue05Patch,HydroMonthValue06,HydroMonthValue06Patch,HydroMonthValue07,HydroMonthValue07Patch,HydroMonthValue08,HydroMonthValue08Patch,HydroMonthValue09,HydroMonthValue09Patch,HydroMonthValue10,'+
       'HydroMonthValue10Patch,HydroMonthValue11,HydroMonthValue11Patch,HydroMonthValue12,HydroMonthValue12Patch,HydroTotalValue,Comment',tgSpecial,'');
  AAdd('MetaDataList', 'Yield,Planning,Rainfall',1,
       'MetaDataListKey',
       'MetaDataListKey,MetaDataListID',
       tgSpecial,'');
  AAdd('MetaDataItem','Yield,Planning,Rainfall',1,
       'MetaDataListID,ParamField,KeyValues,FieldIndex',
       'MetaDataListID,ParamField,KeyValues,FieldIndex,DateCreated,CreatedBy,Comment',
       tgSpecial,'');
  AAdd('StudyDocuments','Yield,Planning,Rainfall',0,'StudyAreaName,Category,Identifier','StudyAreaName,Category,Identifier,Filename,MenuCaption',tgSpecial,'');
  AAdd('StudyScenarioDocuments','Yield,Planning,Rainfall',0,'StudyAreaName,SubArea,Category,Identifier','Model,StudyAreaName,SubArea,Scenario,Category,Identifier,Bookmark,PageNumber',tgSpecial,'');
  AAdd('StudyDocumentCategory','Yield,Planning',0,'CategoryName',
       'CategoryName,CategoryDesc',tgSystem,'');
  //Do not change this ordering

  AAdd('AnlySequences','Yield,Planning',14,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,Seq1,Seq2,Seq3,Seq4,Seq5,Seq6,Seq7,Seq8,Seq9,Seq10',tgModelData,'');
  AAdd('ChannelArcPenalty','Yield,Planning',25,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'ArcCount,Penalty01,Penalty02,Penalty03,Penalty04,Penalty05,PenaltyName',tgModelData,'');
  AAdd('ChannelComments','Yield,Planning',26,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,Comment01,Comment02,Comment03,Comment04,Comment05,'+
       'Comment06,Comment07,Comment08,Comment09,Comment10,Comment11,Comment12,Comment13,Comment14,'+
       'Comment15,Comment16,Comment17,Comment22',tgModelData,'');

  AAdd('ChannelDetails','Yield,Planning',27,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'ChannelType,ChannelName,ChannelNumber,ChannelSubType,UpNodeNumber,DownNodeNumber,' +
       'PenaltyNumber,FirmYieldCalc,ChannelAreaID,SummaryOutput,Comment,FlowOutput,OutputComment',tgModelData,'');

  AAdd('ChannelTypes','Planning',0,'ChannelType','ChannelType,Description',tgSystem,'');

  AAdd('ChannelArea','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AreaID',
       'Model,StudyAreaName,SubArea,Scenario,AreaID,AreaName',tgModelData,'');

  AAdd('ChannelSwitchControl','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,ChannelSwitchID',
       'Model,StudyAreaName,SubArea,Scenario,ChannelSwitchID,ChannelNumber,'+
       'SwitchDefinitionID,SwitchAssociatedNodeNr,SwitchWaterLevel,SwitchType,SwitchInitialStatus',tgModelData,'');

  AAdd('ChannelSwitchDefinition','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,SwitchDefID',
       'Model,StudyAreaName,SubArea,Scenario,SwitchDefID,SwitchDefStartYear,SwitchDefStartMonth,SwitchDefFileName',
        tgModelData,'');

  AAdd('ChannelTimeControl','Yield,Planning',0,
        'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ChannelType',
        'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ChannelType,DataYears,' +
        'ChannelStartYear,ChannelStartMonth,ChannelEndYear,ChannelEndMonth,'+
        'ChannelEconomicLife,ChannelCapitalCost,ChannelFixedOMCost,ChannelVariableOMCost,ConstructionYears,'+
        'ChannelCostSchedule,ChannelEscalationCost',tgModelData,'');

  AAdd('Curtailment','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,CurtailmentPeriodCount,' +
       'Month01,Month02,Month03,Month04,Month05,Month06,' +
       'Month07,Month08,Month09,Month10,InUse',tgModelData,'');

 AAdd('DDTSDetails','DDTS',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'RunoffScaleFactor,OtherInflowScaleFactor,EWRScaleFactor,TargetDraft,DSRequirment,ImportHeadlines',tgModelData,'');

AAdd('DDTSInputData','DDTS',0,
       'Model,StudyAreaName,SubArea,Scenario,RowID',
       'Model,StudyAreaName,SubArea,Scenario,RowID,' +
       'DataDate,Runoff,OtherInflow,Rainfall,Evaporation,IncreamentalRunoff,EWR',tgModelData,'');

 AAdd('DDTSInputMinMax','DDTS',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'MinRunoff,MaxRunoff,MinOtherInflow,MaxOtherInflow,MinRainfall,'+
       'MaxRainfall,MinEvaporation,MaxEvaporation,MaxIncreamentalRunoff,MinIncreamentalRunoff,MaxEWR,MinEWR',tgModelData,'');


  AAdd('CurtailmentChannel','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,' +
       'Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,' +
       'Factor07,Factor08,Factor09,Factor10',tgModelData,'');

  AAdd('Disbenefit','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,YearsCount',tgModelData,'');

  AAdd('DroughtRestriction','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,Name,ReservoirNumbers,ChannelNumbers',tgModelData,'');

  AAdd('DroughtRestrictionFactors','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,' +
       'Factor07,Factor08,Factor09,Factor10',tgModelData,'');

  AAdd('DroughtRestrictionStorageVolumes','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'Volume01,Volume02,Volume03,Volume04,Volume05,Volume06,' +
       'Volume07,Volume08,Volume09,Volume10',tgModelData,'');


  AAdd('DaysPerMonth','Yield,Planning',13,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,Days1,Days2,Days3,Days4,Days5,Days6,Days7,Days8,Days9,Days10,Days11,Days12',tgModelData,'');
  AAdd('DecisionDate','Yield,Planning',1,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,NrOfDecisionDates,' +
       'DecisionMonth01,DecisionMonth02,DecisionMonth03,DecisionMonth04,DecisionMonth05,DecisionMonth06,' +
       'DecisionMonth07,DecisionMonth08,DecisionMonth09,DecisionMonth10,DecisionMonth11,DecisionMonth12,' +
       'DecisionType01,DecisionType02,DecisionType03,DecisionType04,DecisionType05,DecisionType06,DecisionType07,DecisionType08,DecisionType09,DecisionType10,DecisionType11,DecisionType12,' +
       'HydroPowerIndicator01,HydroPowerIndicator02,HydroPowerIndicator03,HydroPowerIndicator04,HydroPowerIndicator05,HydroPowerIndicator06,' +
       'HydroPowerIndicator07,HydroPowerIndicator08,HydroPowerIndicator09,HydroPowerIndicator10,HydroPowerIndicator11,HydroPowerIndicator12',
        tgModelData,'');
  AAdd('DemandCentre','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,DemandCentreID',
       'Model,StudyAreaName,SubArea,Scenario,DemandCentreID,DemandCentreType,ChannelNumber,AnnualDemand,MinimumDemand,IncludeInOutput,Comment',tgModelData,'');
  AAdd('DemandFileData','Yield,Planning',82,'Model,StudyAreaName,SubArea,Scenario,Identifier,FileType,FileNumber','Model,StudyAreaName,SubArea,Scenario,Identifier,FileType,FileNumber,DemandYearValue,DemandYearValuePatch,DemandMonthValue01,DemandMonthValue01Patch,DemandMonthValue02,DemandMonthValue02Patch,DemandMonthValue03,DemandMonthValue03Patch,'+
       'DemandMonthValue04,DemandMonthValue04Patch,DemandMonthValue05,DemandMonthValue05Patch,DemandMonthValue06,DemandMonthValue06Patch,DemandMonthValue07,DemandMonthValue07Patch,DemandMonthValue08,DemandMonthValue08Patch,DemandMonthValue09,'+
       'DemandMonthValue09Patch,DemandMonthValue10,DemandMonthValue10Patch,DemandMonthValue11,DemandMonthValue11Patch,DemandMonthValue12,DemandMonthValue12Patch,DemandTotalValue',tgModelData,'');
  AAdd('DisbenefitFunction','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,YearActive,MonthActive,YearObsolete,MonthObsolete,EquationDisbenefitX,'+
       'EquationDisbenefitY,EquationDisbenefitCost,EquationDisbenefitNonSupply,WQConstraint,TDSConcentration01,TDSConcentration02,'+
       'TDSConcentration03,TDSConcentration04,EscalationRate,EscalationFactors',tgModelData,'');
  AAdd('DiscountRate','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,NrOfDiscountRates,' +
       'DiscountRate01,DiscountRate02,DiscountRate03,DiscountRate04,DiscountRate05,DiscountRate06,' +
       'DiscountRate07,DiscountRate08,DiscountRate09,DiscountRate10',tgModelData,'');
  AAdd('DiversionFeatures','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,DivChannelName,DivChannelNumber,DivChannelType,DivStation',tgModelData,'');
  AAdd('DiversionFeaturesType1n2','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode,' +
       'DivFactor01,DivFactor02,DivFactor03,DivFactor04,DivFactor05,DivFactor06,' +
       'DivFactor07,DivFactor08,DivFactor09,DivFactor10,DivFactor11,DivFactor12',tgModelData,'');
  AAdd('DiversionFeaturesType3','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'NodeNumber,ResLevelsCount,RefFlowsCount,' +
       'DivLevel01,DivLevel02,DivLevel03,DivLevel04,DivLevel05,DivLevel06,' +
       'DivLevel07,DivLevel08,DivLevel09,DivLevel10,DivLevel11,DivLevel12',tgModelData,'');
  AAdd('DiversionFeaturesType3Proportions','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionIndex',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionIndex,' +
       'FlowValue,DivProp01,DivProp02,DivProp03,DivProp04,DivProp05,DivProp06,' +
       'DivProp07,DivProp08,DivProp09,DivProp10,DivProp11,DivProp12',tgModelData,'');

  AAdd('DailyInstreamFileData','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID,InstreamDate,AvgFlow,QualityCode',tgModelData,'');
  AAdd('DailyDiversionStation','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,StationNo,StationID',
       'Model,StudyAreaName,SubArea,Scenario,StationNo,StationID,Place,Latitude,Longitude,CatchmentArea,CatchmentScaleFactor',tgModelData,'');
  AAdd('DailyDiversionQualityCode','DailyDiversion',0,'QualityCode','QualityCode,QualityDescription',tgSystem,'');
  AAdd('DailyDiversionFlowRelationship','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID,RelationshipDate,ReferenceFlow,DiversionFlow,NonDiversionFlow',tgModelData,'');
  AAdd('DailyDiversionFileData','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID,DiversionDate,AvgFlow,QualityCode',tgModelData,'');
  AAdd('DailyDiversionCompensationValues','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,StationID',
       'Model,StudyAreaName,SubArea,Scenario,StationID,CapacityOfDiversion,ScaleFactor,StartDate,EndDate,Value01,Value02,'+
       'Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12',tgModelData,'');
  AAdd('DailyDiversionWRYMData','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,StationID,ReferenceFlow,RefFlowValueEdited,DiversionFlow,DivFlowValueedited,NonDiversionFlow,NonDivFlowValueEdited',
       tgModelData,'');
  AAdd('DailyDiversionMonthlyThreshold','DailyDiversion',0,'Model,StudyAreaName,SubArea,Scenario,StationID',
       'Model,StudyAreaName,SubArea,Scenario,StationID,Value01,Value02,'+
       'Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12',tgModelData,'');



  AAdd('HydrologyDataType','Yield,Planning',0,'DataType','DataType,DataTypeDescr',tgSystem,'');
  AAdd('HistoricDamLevels','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,FileName,YearValue',
       'Model,StudyAreaName,SubArea,Scenario,FileName,YearValue,' +
       'MonthValue01,MonthValue02,MonthValue03,MonthValue04,MonthValue05,MonthValue06,' +
       'MonthValue07,MonthValue08,MonthValue09,MonthValue10,MonthValue11,MonthValue12',tgModelData,'');
  AAdd('HydrologyDetails','Yield,Planning',83,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,FileType,FileNumber,LinesCount',tgModelData,'');
  AAdd('FileGroup','Yield,Planning',0,'FileGroup','FileGroup,FileGroupDescr',tgSystem,'');
  AAdd('FileCreate','Yield,Planning',99,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,ImplementFile',tgModelData,'');
  AAdd('FileNames','Yield,Planning',99,'Model,StudyAreaName,SubArea,Scenario,Identifier,FileGroup','Model,StudyAreaName,SubArea,Scenario,Identifier,FileGroup,FileName,ImportDate,FileDate',tgModelData,'');
  AAdd('FileTypes','Yield,Planning',0,'FileType','FileType,FileExtention,EFileName,FileTypeDescr',tgSystem,'');
  AAdd('FlowConstraints','Yield,Planning',31,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'ConstraintsChannelNumber,FeatureName,UpStreamReservoirNumber,' +
       'DownStreamReservoirNumber,PointsElevationNumber,SillElevation,' +
       'GateHeight,StructureType,DischargeCoefficient,ControlStructureLength,WaterLevelAtDownstreamNode,ReferenceElevation',tgModelData,'');
  AAdd('FlowConstraintsValue','Yield,Planning',32,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,GroupNumber,SubGroupNumber,LineNumber',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,GroupNumber,SubGroupNumber,LineNumber,'+
       'Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10',tgModelData,'');

  AAdd('FlowConstraintsDeltaH','Yield,Planning',34,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,DeltaH01,DeltaH02,DeltaH03,DeltaH04,DeltaH05,DeltaH06,DeltaH07,DeltaH08,DeltaH09,DeltaH10',tgModelData,'');
  AAdd('FlowConstraintsDischarge','Yield,Planning',33,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,Disch01,Disch02,' +
       'Disch03,Disch04,Disch05,Disch06,Disch07,Disch08,Disch09,Disch10',tgModelData,'');
  AAdd('FlowConstraintsDiversion','Yield,Planning',35,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier,' +
       'DFlow01,DFlow02,DFlow03,DFlow04,DFlow05,DFlow06,DFlow07,DFlow08,' +
       'DFlow09,DFlow10,DFlow11',tgModelData,'');
  AAdd('FlowConstraintsType11Depth','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber,' +
       'DepthValue01,DepthValue02,DepthValue03,DepthValue04,DepthValue05,'+
       'DepthValue06,DepthValue07,DepthValue08,DepthValue09,DepthValue10',tgModelData,'');
  AAdd('FlowConstraintsType11Flow','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber,' +
       'FlowValue01,FlowValue02,FlowValue03,FlowValue04,FlowValue05,FlowValue06,'+
       'FlowValue07,FlowValue08,FlowValue09,FlowValue10',tgModelData,'');

  AAdd('FlowConstraintsElevation','Yield,Planning',32,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ConstraintsElev01,' +
       'ConstraintsElev02,ConstraintsElev03,ConstraintsElev04,ConstraintsElev05,' +
       'ConstraintsElev06,ConstraintsElev07,ConstraintsElev08,ConstraintsElev09,'+
       'ConstraintsElev10,ConstraintsComment',tgModelData,'');

  AAdd('FMAllocationDefinition','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,' +
       'AllocDefName,AllocDefStartYear,AllocDefStartMonth,AllocDefEndYear,AllocDefEndMonth,FamilyFile,NrOfReliabilityClasses,'+
       'NrOfLoadCases,NrOfStartStoragePercs,NrOfCurveSets,PeriodLength,ImplemntationDecision,SupportStrategy,BalancingOption,'+
       'RIValue01,RIValue02,RIValue03,RIValue04,RIValue05,RILabel01,'+
       'RILabel02,RILabel03,RILabel04,RILabel05,MonthCurveSet01,MonthCurveSet02,'+
       'MonthCurveSet03,MonthCurveSet04,MonthCurveSet05,MonthCurveSet06,MonthCurveSet07,MonthCurveSet08,'+
       'MonthCurveSet09,MonthCurveSet10,MonthCurveSet11,MonthCurveSet12,'+
       'StartStoragePerc01,StartStoragePerc02,StartStoragePerc03,StartStoragePerc04,StartStoragePerc05,'+
       'StartStoragePerc06,StartStoragePerc07,StartStoragePerc08,StartStoragePerc09,StartStoragePerc10',tgModelData,'');

  AAdd('FMAllocationLevel','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,AllocLevelID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,AllocLevelID,' +
       'AllocLevelName,Curtailment01,Curtailment02,Curtailment03,Curtailment04,Curtailment05,Comment',tgModelData,'');

  AAdd('FMCoefficients','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SubSystemID,StartStorageNr,CurveSetNr,LoadCaseNr',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SubSystemID,StartStorageNr,CurveSetNr,LoadCaseNr,' +
       'LoadCase,CoefficientA,CoefficientB,CoefficientC,CoefficientD,RiskProportion',tgModelData,'');

  AAdd('FMDemandDefinition','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,DemandDefID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,DemandDefID,' +
       'DemandDefOrder,DemandDefName,DemandCentreID,ParentSubSystemID,GrowthType,TargetDemand,DCUserCategoryID,SupportArc1,' +
       'SupportArc2,SupportSystemNr,Comment',tgModelData,'');

  AAdd('FMSolveFixedPosition','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,FixedPositionID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,FixedPositionID,' +
       'FixedPositionNr,FixedPosSubSystemID',tgModelData,'');

  AAdd('FMSolveSpecificOrder','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SpecificOrderID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SpecificOrderID,' +
       'BeforeSubSystemID,AfterSubSystemID',tgModelData,'');

  AAdd('FMSubSystem','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SubSystemID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SubSystemID,' +
       'SubSystemName,SubSystemOrder,SubSystemStartYear,SubSystemStartMonth,SubSystemEndYear,' +
       'SubSystemEndMonth,SubtractedSubSystemID,SupportingSubSystemID,SupportingChannelNr,' +
       'ShortTermYield,LongTermYield,LowestStreamFlow,FirmYield,SupportCalcType,' +
       'RoutingChannelNr01,RoutingChannelNr02,RoutingChannelNr03,RoutingChannelNr04,RoutingChannelNr05,' +
       'SubSystemReservoirNrs',tgModelData,'');

  AAdd('FMSupportChannel','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SupportChannelID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,SupportChannelID,' +
       'ChannelNumber,NrOfCntrlSubSystems,CntrlSubSystemID01,CntrlSubSystemID02,' +
       'CntrlSubSystemID03,CntrlSubSystemID04,CntrlSubSystemID05,CntrlSubSystemID06,' +
       'CntrlSubSystemID07,CntrlSubSystemID08,CntrlSubSystemID09,CntrlSubSystemID10,CntrlFactor01,' +
       'CntrlFactor02,CntrlFactor03,CntrlFactor04,CntrlFactor05,CntrlFactor06,CntrlFactor07,CntrlFactor08,' +
       'CntrlFactor09,CntrlFactor10',tgModelData,'');

  LoadTablePropertyData2(AAdd);
end;
procedure LoadTablePropertyData2(AAdd: TDbTablePropertyItemAddFunction);
const OPNAME = 'LoadTablePropertyData2';
      CRWHRunConfig = 'Model,StudyAreaName,SubArea,Scenario,Identifier,RunName,PeriodStartDate,PeriodEndDate,RunTypeID,RunStartVolume,RunStartLevel,RunStopLevel,RoofArea,RoofRunoffCoef,HouseHoldNumber,HouseHoldMembers,HouseHoldDemandPP,'+
                      'TankSize01,TankSize02,TankSize03,TankSize04,TankSize05,TankSize06,TankSize07,TankSize08,TankSize09,TankSize10';
begin
  AAdd('FMSupportSubSystem','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,DemandDefID,SupportSubSystemID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,DemandDefID,SupportSubSystemID,' +
       'SupSubSystemID,SupSubSysChannelNr01,SupSubSysChannelNr02,SupSubSysChannelNr03,' +
       'SupSubSysChannelNr04,SupSubSysChannelNr05',tgModelData,'');

  AAdd('FMUserCategory','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,UserCategoryID',
       'Model,StudyAreaName,SubArea,Scenario,AllocDefID,UserCategoryID,' +
       'UserCategoryName,Distribution01,Distribution02,Distribution03,Distribution04,' +
       'Distribution05,Comment',tgModelData,'');
  AAdd('GrowthFactorConfig','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,YearsCount',tgModelData,'');
  AAdd('GrowthFactorDemand','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ValidFactors,Comment,Factors',tgModelData,'');
  AAdd('GrowthFactorHydrology','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,GaugeNumber,Comment,AFFFactors,IRRFactors,URBFactors',tgModelData,'');
  AAdd('GrowthFactorMinMax','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ArcNumber,ValidFactors,Comment,Factors',tgModelData,'');

  AAdd('GrowthFactorExcelConfig','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,BaseYear,StartYear,YearsCount,DataStartYear',tgModelData,'');

  AAdd('GrowthFactorExcelDemand','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,Institution,WaterUser,Factors',tgModelData,'');

  AAdd('GrowthFactorExcelHydrology','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,GaugeNumber,Institution,Institution1,Institution2,'+
       'WaterUser,WaterUser1,WaterUser2,AFFFactors,IRRFactors,URBFactors',tgModelData,'');

  AAdd('GrowthFactorExcelMinMax','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ArcNumber,Institution,WaterUser,Factors',tgModelData,'');

  AAdd('GroundWater','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,AquiferNodeNumber,GroundWaterName,GroundWaterDescription,AquiferStorativity,'+
       'AquiferStaticWaterlevel,UnsaturatedStorageCapacity,InitialUnsaturatedStorage,MaximumAquiferRecharge,MovingAverageRecharge,'+
       'MaximumBaseFlowRate,HeadBaseFlowPower,MaximumHydrologicalGradient,AquiferTransmissivity,BoreHoleDistanceToRiver,MaximumWaterAbstraction,'+
       'ParameterK2,ParameterK3,WaterEvaporationArea',tgModelData,'');

  AAdd('GroundWaterPitman','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier,SoilMoistureCapacity,SoilMoistureStorageCapacity,SoilMoistureFlowState,SoilMoistureFlowEquation,'+
       'MaximumGroundWaterFlow,SoilMoistureRechargeEquation,GroundWaterFlow',tgModelData,'');

  AAdd('GroundWaterEvaporation','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier,Evaporation01,Evaporation02,Evaporation03,Evaporation04,'+
       'Evaporation05,Evaporation06,Evaporation07,Evaporation08,Evaporation09,Evaporation10,Evaporation11,Evaporation12',tgModelData,'');

  AAdd('GroundWaterUsageFactor','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,'+
       'Factor07,Factor08,Factor09,Factor10,Factor11,Factor12',tgModelData,'');

  AAdd('GroundWaterSubCatchment','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,GroundWaterIdentifier,RefNodeNumber,AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,'+
       'BaseFlowNodeNumber,AquiferInflowChannelNr,AquferExcessChannelNr,GroundWaterBaseFlowChannelNr,RegulationFromAquiferChannelNr,RegulationFromBaseFlowChannelNr,'+
       'DownstreamChannelNr,UpstreamChannelNr,SurfaceRunOffChannelNr,BaseFlowRemainderChannelNr,GroundWaterAbstractionChannelNr,OutflowToNetworkChannelNr',tgModelData,'');

  AAdd('InterBasinSupport','Yield,Planning',13,
       'Model,StudyAreaName,SubArea,Scenario,InterBasinSupportID',
       'Model,StudyAreaName,SubArea,Scenario,InterBasinSupportID,SummaryRequired,ChannelNumber,UpperLimit,DemandCentreID,Comment',tgModelData,'');

  AAdd('IFRSite','Yield,Planning,IFRPreProcessor',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,SiteName,SiteDescr,CSVFileName,QuaternaryCatchment,RiverName,AssociatedEMC,'+
       'LevelDetail,LevelConfidence,Source,XCoord,YCoord,MonthNames',tgModelData,'');

  AAdd('IFRSiteFlows','Yield,Planning,IFRPreProcessor',13,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,FlowIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,FlowIdentifier,ExceedProbability,Flow01,Flow02,Flow03,Flow04,Flow05,'+
       'Flow06,Flow07,Flow08,Flow09,Flow10,Flow11,Flow12',tgModelData,'');

  AAdd('IFRFeatures','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'IFRChannelNumber,FeatureName,ReferenceNodeCount,LagInMonthsCount,PointsCount,' +
       'RefNodeNumbers,CalculationOption,IFRSiteID,ExceedencePerc,ReferenceFlowType,IFRStatusIndicator,IFRLoss,IFRUnknown,MonthlyIFRLoss',tgModelData,'');
  AAdd('IFRFeaturesDetails','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber','Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber,AnnualInflow,InflowVar01,InflowVar02,InflowVar03,InflowVar04,InflowVar05,InflowVar06,InflowVar07,'+
       'InflowVar08,InflowVar09,InflowVar10,InflowVar11,InflowVar12,ReleaseVar01,ReleaseVar02,ReleaseVar03,ReleaseVar04,ReleaseVar05,ReleaseVar06,ReleaseVar07,ReleaseVar08,ReleaseVar09,ReleaseVar10,ReleaseVar11,ReleaseVar12',tgModelData,'');
  AAdd('IFRReference','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,InflowOption,ChannelCount',tgModelData,'');
  AAdd('IrrigationAreas','Yield,Planning',85,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'AreaName,IrrigationNodeNumber,DiversionChannelNumber,ConsumptiveChannelNumber,' +
       'ReturnFlowChannelNumber,RelaxationDemand',tgModelData,'');
  AAdd('IrrigationAreasDiversionFlow','Yield,Planning',84,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,DFlow01,DFlow02,DFlow03,DFlow04,DFlow05,DFlow06,DFlow07,DFlow08,DFlow09,DFlow10,DFlow11,DFlow12',tgModelData,'');
  AAdd('IrrigationAreasReturnFlow','Yield,Planning',93,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,RFlow01,RFlow02,RFlow03,RFlow04,RFlow05,RFlow06,RFlow07,RFlow08,RFlow09,RFlow10,RFlow11,RFlow12',tgModelData,'');

  AAdd('IrrigationBlock','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,BlockNumber,BlockName,MaxWaterAllocation,FileName,NodeNumber,'+
       'CanalTransportLoss,EfficiencyFactor,ReturnFlowFactor,UpperZoneReturnFlow,LowerZoneReturnFlow,ReturnFlowLoss,'+
       'UpperZoneSoilMoistureCapacity,LowerZoneSoilMoistureCapacity,UpperZoneSoilMoistureTarget,InitialSoilMoistureStorage,'+
       'RainAboveRainFactorSpecValue,RainBelowRainFactor,RainCatchmentScalingFactor,AllocatedIrrigationArea,'+
       'Description,DiversionChannelNumber,ReturnFlowChannelNumber,CatchmentFileName,CropWaterUseType,DroughtApplicable',tgModelData,'');

  AAdd('IrrigationBlockDetails','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,BlockNumber,IrrigationBlockType,'+
       'CurtailIrrigationAbstraction,CanalSeepageLoss,CanalTransmissionLoss,UpperSoilOutflow,MaxUpperZoneMoisture,MinUpperZoneMoisture,'+
       'CropTypesCount,IrrigationSupplyCapacity,AllocatedAreaPointsCount,MethodIrrigatedAreas,MaxWaterAllocationCount,'+
       'MethodMaxWaterAllocation,ReturnFlowVolumePointsCount,MethodReturnFlowVolume,SupplyCapacityPointsCount,'+
       'MethodSupplyCapacity,IrrigationEfficienciesPointsCount,MethodIrrigationEfficiencies,ReturnFlowFactorsCount,MethodReturnFlowFactors,'+
       'IABreakpointYear,IABreakpointArea,MWABreakpointYear,MWAWaterAllocation,MWAWaterAllocationGrowth,'+
       'RFVBreakpointYear,RFVFlowVolume,SCBreakpointYear,SCSupplyCapacity,IEBreakpointYear,IEIrrigationEfficiency,'+
       'RFFBreakpointYear,RFFReturnFlowFactor',tgModelData,'');

  AAdd('IrrigationBlockAPanConvFactor','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,'+
       'Factor07,Factor08,Factor09,Factor10,Factor11,Factor12',tgModelData,'');

  AAdd('IrrigationBlockPanEvaporation','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,Evaporation01,Evaporation02,Evaporation03,Evaporation04,'+
       'Evaporation05,Evaporation06,Evaporation07,Evaporation08,Evaporation09,Evaporation10,Evaporation11,Evaporation12',tgModelData,'');

  AAdd('IrrigationBlockRainfallFactor','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,'+
       'Factor07,Factor08,Factor09,Factor10,Factor11,Factor12',tgModelData,'');

  AAdd('IrrigationBlockWaterUsageFactor','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,BlockIdentifier,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,BlockIdentifier,Identifier,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,'+
       'Factor07,Factor08,Factor09,Factor10,Factor11,Factor12,PercAreaUnderCropType,CropName',tgModelData,'');

  AAdd('IrrigationBlockMaxDemand','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,BlockNumber',
       'Model,StudyAreaName,SubArea,Scenario,BlockNumber,ChannelNumber,Value01,Value02,Value03,Value04,Value05,'+
       'Value06,Value07,Value08,Value09,Value10,Value11,Value12,InUse',tgModelData,'');

  AAdd('LossFeature','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'FeatureName,ChannelNumber,Reference,LossType',tgModelData,'');
  AAdd('LossFeatureValue','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier,' +
       'Value01,Value02,Value03,Value04,Value05,Value06,Value07,' +
       'Value08,Value09,Value10,Value11,Value12',tgModelData,'');
  AAdd('MasterControlFeature','Yield,Planning',90,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'ChannelNumber,FeatureName,MasterControlType,DistributionPattern,StorageFraction,Value01,Value02,' +
       'Value03,Value04,Value05,Value06,Value07,Value08,Value09,' +
       'Value10,Value11,Value12',tgModelData,'');
  AAdd('MaxYield','Yield,Planning',16,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,' +
       'MYield1,MYield2,MYield3,MYield4,MYield5,MYield6,MYield7,MYield8,MYield9,MYield10',tgModelData,'');

  AAdd('MinimunGroundwaterFlowVolume','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,'+
       'FlowVolume01,FlowVolume02,FlowVolume03,FlowVolume04,FlowVolume05,FlowVolume06,'+
       'FlowVolume07,FlowVolume08,FlowVolume09,FlowVolume10,FlowVolume11,FlowVolume12',tgModelData,'');

  AAdd('Mine','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,'+
       'NodeNumber,MineName,RiverChannelNumber,PCDChannelNumber,HydrologyNodeNumber,BeneficiationPlantArea,'+
       'BeneficiationRunOffFactor,NodeRefNr,ProportionAntecedentFlow,GroundwaterFlowVolume,AntecedentRunoffDecayFactor',tgModelData,'');

  AAdd('MineSubCatchment','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,'+
       'CatchmentReferenceNumber,CatchmentReferenceName,ProportionAntecedentFlow,GroundwaterFlowVolume,AntecedentRunoffDecayFactor,InUse',tgModelData,'');

  AAdd('MineSubCatchmentFlowVolume','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineSubCatchmentIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,MineSubCatchmentIdentifier,'+
       'Volume01,Volume02,Volume03,Volume04,Volume05,Volume06,'+
       'Volume07,Volume08,Volume09,Volume10,Volume11,Volume12',tgModelData,'');


  AAdd('MineLakeEvaporation','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,' +
       'LakeEvaporation01,LakeEvaporation02,LakeEvaporation03,LakeEvaporation04,LakeEvaporation05,' +
       'LakeEvaporation06,LakeEvaporation07,LakeEvaporation08,LakeEvaporation09,LakeEvaporation10,' +
       'LakeEvaporation11,LakeEvaporation12',tgModelData,'');
  AAdd('MineOpenCast','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier,' +
       'PitName,CoalReserveArea,WorkingsArea,DisturbedWorkingsArea,DisturbedArea,WaterSurfaceEvapArea,' +
       'DisturbedAreaRunOff,DisturbedWorkingsAreaRunOff,DecantVolume,SeepageVolume,AnalysisStartVolume,' +
       'MaximumSeepageRate,SeepageExponent,PCDSurfaceArea,PCDStorageCapacity,PCDAnalysisStartVolume',tgModelData,'');
  AAdd('MinePanEvaporation','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,' +
       'PanEvaporation01,PanEvaporation02,PanEvaporation03,PanEvaporation04,PanEvaporation05,' +
       'PanEvaporation06,PanEvaporation07,PanEvaporation08,PanEvaporation09,PanEvaporation10,' +
       'PanEvaporation11,PanEvaporation12',tgModelData,'');
  AAdd('MineRechargeFactor','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,RechargeFactorParentType,ParentIdentifier,RechargeFactorType',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,RechargeFactorParentType,ParentIdentifier,RechargeFactorType,' +
       'RechargeFactor01,RechargeFactor02,RechargeFactor03,RechargeFactor04,RechargeFactor05,' +
       'RechargeFactor06,RechargeFactor07,RechargeFactor08,RechargeFactor09,RechargeFactor10,' +
       'RechargeFactor11,RechargeFactor12',tgModelData,'');
  AAdd('MineRechargeFactorParentType','Yield,Planning',0,
       'RechargeFactorParentType','RechargeFactorParentType,Description',tgModelData,'');
  AAdd('MineRechargeFactorType','Yield,Planning',0,
       'RechargeFactorType','RechargeFactorType,Description',tgModelData,'');
  AAdd('MineSlurryDump','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier,DumpName,DumpSurfaceArea,' +
       'RunoffFactorToPCD,SeepageSplitFactor,PCDStorageCapacity,PCDSurfaceArea,PCDAnalysisStartVolume',tgModelData,'');
  AAdd('MineUGUpstreamRunoff','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,UGIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,UGIdentifier,' +
       'RunoffFactor01,RunoffFactor02,RunoffFactor03,RunoffFactor04,RunoffFactor05,RunoffFactor06,' +
       'RunoffFactor07,RunoffFactor08,RunoffFactor09,RunoffFactor10,RunoffFactor11,RunoffFactor12',tgModelData,'');
  AAdd('MineUnderGround','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier,' +
       'UnderGroundSectionName,ChannelNumberToUGDam,UpstreamCatchmentArea,BoardPillarCatchmentArea,' +
       'HighExtractionCatchmentArea,HighExtractionAreaRunoffFactor',tgModelData,'');
  AAdd('MinFlowChannel','Yield,Planning',86,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'MinFlowChannelName,MinFlowChannelNumber',tgModelData,'');
  AAdd('MinFlowChannelValue','Yield,Planning',80,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'Value01,Value02,Value03,Value04,Value05,Value06,' +
       'Value07,Value08,Value09,Value10,Value11,Value12',tgModelData,'');
  AAdd('MinMaxChannel','Yield,Planning',88,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,MinMaxChannelName,MinMaxChannelNumber,Comment',tgModelData,'');
  AAdd('MinMaxChannelFlow','Yield,Planning',89,'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier','Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier,MFlow01,MFlow02,MFlow03,MFlow04,MFlow05,MFlow06,MFlow07,MFlow08,MFlow09,MFlow10,MFlow11,MFlow12,Comment',tgModelData,'');

  AAdd('MinMaxChannelDistribution','Yield,Planning',90,'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier,Distribution01,Distribution02,Distribution03,Distribution04,'+
       'Distribution05,Distribution06,Distribution07,Distribution08,Distribution09,Distribution10,Distribution11,Distribution12',tgModelData,'');

  AAdd('MinMaxBoundChannel','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,' +
       'ReferenceChannelCount,ReferenceChannels',tgModelData,'');

  AAdd('MinMaxWQConstrain','Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,' +
       'WQTarget,BlendingRefChannelCount,ReservoirRef,WQConType,ReferenceChannels,' +
       'ReferenceChannelFactors,SlopeLimit,EstimatedRelease,Concentration',tgModelData,'');

  AAdd('MonthNames','Yield,Planning',12,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,Month1,Month2,Month3,Month4,Month5,Month6,Month7,Month8,Month9,Month10,Month11,Month12',tgModelData,'');
  AAdd('NodesDetails','Yield,Planning,DDTS',38,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,NodeNumberStorage,StatusIndicator,PlottingOption,ReservoirPriority,FullSupplyLevel,DeadStorageLevel,BottomOfReservoir,FullSupplyAllocation,NodesDetailsComment',tgModelData,'');
  AAdd('ParamHeader','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,GaugeCount,GaugeComment,KeyGaugeCount,'+
       'KeyGauges',tgModelData,'');
  AAdd('ParamMatrix','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier,MatrixType','Model,StudyAreaName,SubArea,Scenario,Identifier,MatrixType,Matrix01,Matrix02,Matrix03,Matrix04,Matrix05,Matrix06,Matrix07,Matrix08,Matrix09,Matrix10',tgModelData,'');
  AAdd('ParamMatrixComm','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario','Model,StudyAreaName,SubArea,Scenario,MatrixBComment,MatrixB0Comment,MatrixB1Comment,MatrixAComment,MatrixCComment',tgModelData,'');
  AAdd('ParamMatrixType','Yield,Planning',0,'MatrixType','MatrixType,MatrixTypeDesc',tgSystem,'');
  AAdd('ParamStochastics','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier','Model,StudyAreaName,SubArea,Scenario,Identifier,GaugePathName,YearsNumber,YearStart,Residual1,Residual2,Variate1,Variate2,TransformType,TransformGamma,'+
       'TransformDelta,TransformXlam,TransformXi,ResidualMean,ResidualStdDev,ArmaPhi1,ArmaPhi2,ArmaTheta1,ArmaTheta2,PhiZero,ZTVariates,ParamXA,ParamXSD,ParamAIC,ParamANC,CatchmentArea',tgModelData,'');
  AAdd('pltElement','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ElementType,ElementNumber,ElementName',tgModelData,'');
  AAdd('pltElementCount','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,ReservoirCount,ChannelCount,LoadCaseCount,SequenceCount,MonthCount,Comment',tgModelData,'');
  AAdd('pltFileData','Yield,Planning',0,'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,LoadCaseNumber,SequenceNumber,ElementType,ElementNumber,'+
       'Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12',tgModelData,'');
  AAdd('pltFileType','Yield,Planning',0,'ElementType',
       'ElementType,ElemenetTypeDesc',tgSystem,'');
  AAdd('PowerPlants','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,' +
       'PowerPlantName,PowerChannelNumber,SpillChannelNumber,MaxCapGenerator,' +
       'MaxCapTurbine,Efficiency,PowerPlantStatus,HeadLoss,DesignHead,' +
       'MaxNetHead,MinNetHead,HydropowerMinLevel,PointsCount,TailWaterCount,TailWaterTypeCode,' +
       'DownStreamPowerChannelCount,Channel01,Channel02,Channel03,Channel04,' +
       'Channel05,Channel06,Channel07,Channel08,Channel09,Channel10,Channel11,' +
       'Channel12,Channel13,Channel14,Channel15,Channel16,Channel17,Channel18,' +
       'Channel19,Channel20',tgModelData,'');
  AAdd('PowerPlantsDemands','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,PowerCode',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,PowerCode,' +
       'MinPower01,MinPower02,MinPower03,MinPower04,MinPower05,MinPower06,' +
       'MinPower07,MinPower08,MinPower09,MinPower10,MinPower11,MinPower12',tgModelData,'');
  AAdd('PowerPlantsDetails','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier,FactorCode',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,FactorCode,' +
       'Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,Factor07,Factor08,' +
       'Factor09,Factor10',tgModelData,'');
  AAdd('PowerChannels','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelName,ChannelNumber',tgModelData,'');
  AAdd('ReservoirSwitchDefinition','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID',
       'Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,StartYear,StartMonth',tgModelData,'');
  AAdd('ReservoirSwitchFileGroup','Yield,Planning',0,'FileGroupID',
       'FileGroupID,FileGroupName',tgSystem,'');
  AAdd('ReservoirSwitchFileName','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier',
       'Model,StudyAreaName,SubArea,Scenario,FileGroupID,SwitchDefID,FileIdentifier,FileName',tgModelData,'');
  AAdd('TariffCalculation','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',
       'Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,Tariff,EscalationFactors',tgModelData,'');
  AAdd('TariffCalculationConfig','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,DataYears',tgModelData,'');
  AAdd('ChannelConfig','Yield,Planning',0,
       'Model,StudyAreaName,SubArea,Scenario',
       'Model,StudyAreaName,SubArea,Scenario,InflowPenaltyNo',tgModelData,'');

  AAdd('RWHRunConfig','RWH',1,
       'Model,StudyAreaName,SubArea,Scenario,Identifier',CRWHRunConfig,tgModelData,'');
  AAdd('RWHSelectedStations','RWH',1,
       'Model,StudyAreaName,SubArea,Scenario,StationID',
       'Model,StudyAreaName,SubArea,Scenario,StationID,StationNumber,StationName',tgModelData,'');
end;

end.


