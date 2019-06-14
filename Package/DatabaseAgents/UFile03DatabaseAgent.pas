//
//
//  UNIT      : Contains TFile03DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UFile03DatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject, VCL.dialogs,
  UChannelDescriptionObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject,
  Contnrs;

type

  TFile03DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    procedure MergeIrrBlockData(IrrBlockList: TObjectList);
    procedure MergeWetlandData(WetlandList: TObjectList);

    function ReadInflowPenaltyNoSQL : string;

    function ReadF03CountsSQL : string;
    function ReadDivType4CountSQL : string;
    function ReadF03CommentsSQL : string;
    function ReadF03KnownDataSQL: string;
    function ReadF03PenaltiesSQL: string;
    function ReadF03UnkownDataSQL: string;
    function ReadSpillChannelSQL (APowerChannelNr : integer): string;
    function ReadDownstreamPowerChannelsSQL (APowerChannelNr : integer): string;
    function ReadConsumptiveChannelSQL (ADivChannelNr : integer): string;
    function ReadReturnFlowChannelSQL (ADivChannelNr : integer): string;
    function ReadChannelOutputDataSQL : string;
    function ReadPlanningChannelOutputDataSQL: string;
    function ReadSummaryOutputCountSQL : string;
    function ReadPlanningChannelOutputCountSQL: string;

    function ReadFirmYieldCalcCountSQL : string;
    function ReadDemandCentreSQL : string;
    function ReadGroundWaterSubCatchmentSQL: string;
    function ReadChannelPenaltyNumberSQL(AChannelNr : integer): string;

    function WriteChannelArcPenaltyDataSQL: string;
    function WriteChannelConfigSQL : string;
    function WriteChannelCommentsDataSQL: string;
    function WriteChannelDetailsDataSQL: string;
    function WriteChannelOutputDataSQL: string;
    function WritePlanningChannelOutputDataSQL: string;
    function WriteUnknownDataSQL: string;
    function WritePumpingFeatureDataSQL: string;
    function WriteSpecifiedInflowFeatureDataSQL: string;
    function WriteSpecifiedDemandFeatureDataSQL: string;
    function WriteGroundWaterSubCatchmentSQL: string;
    function UpdateChannelNames(ADataObject: TDataFileObjects): boolean;
    function UpdateChannelNamesFromOutputChannel(ADataObject: TDataFileObjects): boolean;
    function UpdateChannelNamesFromMinFlowLoss(ADataObject: TDataFileObjects): boolean;
    function UpdateChannelNamesFromMinMax(ADataObject: TDataFileObjects): boolean;
    function UpdateChannelNamesFromPowerMasterControl(ADataObject: TDataFileObjects): boolean;
    function UpdateChannelNamesFromDiversionFeatures(ADataObject: TDataFileObjects): boolean;
    function UpdateChannelNamesFromIrrigationBlock(ADataObject: TDataFileObjects): boolean;
    function OverrideChannelNamesFromOutputChannel(ADataObject: TDataFileObjects): boolean;

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
     UPowerDemandObject,
     UChannelMinMaxObject,
     UDivChannelDemandObject,
     UMinFlowChannelObject,
     UDataSetType,
     UErrorHandlingOperations, UBasicObjects;

function TFile03DatabaseAgent.ReadF03UnkownDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadF03UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData '+
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

function TFile03DatabaseAgent.ReadF03CountsSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadF03CountsSQL';
begin
  Result := '';

  try
  Result :=
    'SELECT ChannelType, Count(ChannelType) AS ChannelTypeCounter ' +
    'FROM ChannelDetails ' +
    ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
    ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
    ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
    ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
    ' AND ((ChannelSubType <= 1) OR (ChannelSubType IS Null)) ' +
    ' GROUP BY ChannelType';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadDivType4CountSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadDivType4CountSQL';
begin
  Result := '';
  try
  Result :=
    'SELECT Count(*) AS CounterAll  ' +
    'FROM DiversionFeatures ' +
    ' WHERE Model       =  ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
    ' AND StudyAreaName =  ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
    ' AND SubArea       =  ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
    ' AND Scenario      =  ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
    ' AND DivChannelType = 4';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile03DatabaseAgent.ReadInflowPenaltyNoSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadInflowPenaltyNoSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT InflowPenaltyNo ' +
      'FROM ChannelConfig ' +
      'WHERE (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND'+
      '      (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND'+
      '      (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND'+
      '      (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadF03CommentsSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadF03CommentsSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Comment01, Comment02, Comment03, Comment04, Comment05, Comment06, ' +
      'Comment07, Comment08, Comment09, Comment10, Comment11, Comment12, Comment13, ' +
      'Comment14, Comment15, Comment16, Comment17, Comment22 ' +
      'FROM ChannelComments ' +
      'WHERE (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND'+
      '      (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND'+
      '      (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND'+
      '      (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadF03KnownDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadF03KnownDataSQL';
begin
  Result := '';
  try
  Result :=
    'SELECT ChannelDetails.Model, ChannelDetails.StudyAreaName, ChannelDetails.SubArea, ' +
    'ChannelDetails.Scenario, ChannelDetails.Identifier, ' +
    'ChannelDetails.ChannelName, ChannelDetails.ChannelNumber, ' +
    'ChannelDetails.ChannelType, ChannelDetails.ChannelSubType, ChannelDetails.Comment, ' +
    'ChannelDetails.DownNodeNumber, ChannelDetails.PenaltyNumber,ChannelDetails.OutputComment, ' +
    'ChannelDetails.UpNodeNumber, ChannelDetails.FirmYieldCalc, ChannelDetails.SummaryOutput, ' +
    'PumpingFeature.PumpingEfficiency, PumpingFeature.PumpingHead, ' +
    'SpecifiedDemandFeature.GaugeNumber, SpecifiedDemandFeature.Fullname, ' +
    'SpecifiedDemandFeature.Stochastic, ' +
    'LossFeature.Reference, LossFeature.LossType, ' +
    'DiversionFeatures.DivChannelType, ' +
    'MasterControlFeature.MasterControlType ' +
    'FROM ((((ChannelDetails ' +
    'LEFT JOIN PumpingFeature ON ' +
      '(ChannelDetails.ChannelNumber = PumpingFeature.ChannelNumber) AND ' +
      '(ChannelDetails.Scenario = PumpingFeature.Scenario) AND ' +
      '(ChannelDetails.SubArea = PumpingFeature.SubArea) AND ' +
      '(ChannelDetails.StudyAreaName = PumpingFeature.StudyAreaName) AND ' +
      '(ChannelDetails.Model = PumpingFeature.Model)) ' +
    'LEFT JOIN SpecifiedDemandFeature ON ' +
      '(ChannelDetails.ChannelNumber = SpecifiedDemandFeature.ChannelNumber) AND ' +
      '(ChannelDetails.Scenario = SpecifiedDemandFeature.Scenario) AND ' +
      '(ChannelDetails.SubArea = SpecifiedDemandFeature.SubArea) AND ' +
      '(ChannelDetails.StudyAreaName = SpecifiedDemandFeature.StudyAreaName) AND ' +
      '(ChannelDetails.Model = SpecifiedDemandFeature.Model)) ' +
    'LEFT JOIN LossFeature ON ' +
      '(ChannelDetails.ChannelNumber = LossFeature.ChannelNumber) AND ' +
      '(ChannelDetails.Scenario = LossFeature.Scenario) AND ' +
      '(ChannelDetails.SubArea = LossFeature.SubArea) AND ' +
      '(ChannelDetails.StudyAreaName = LossFeature.StudyAreaName) AND ' +
      '(ChannelDetails.Model = LossFeature.Model)) ' +
    'LEFT JOIN DiversionFeatures ON ' +
      '(ChannelDetails.ChannelNumber = DiversionFeatures.DivChannelNumber) AND ' +
      '(ChannelDetails.Scenario = DiversionFeatures.Scenario) AND ' +
      '(ChannelDetails.SubArea = DiversionFeatures.SubArea) AND ' +
      '(ChannelDetails.StudyAreaName = DiversionFeatures.StudyAreaName) AND ' +
      '(ChannelDetails.Model = DiversionFeatures.Model)) ' +
    'LEFT JOIN MasterControlFeature ON ' +
      '(ChannelDetails.ChannelNumber = MasterControlFeature.ChannelNumber) AND ' +
      '(ChannelDetails.Scenario = MasterControlFeature.Scenario) AND ' +
      '(ChannelDetails.SubArea = MasterControlFeature.SubArea) AND ' +
      '(ChannelDetails.StudyAreaName = MasterControlFeature.StudyAreaName) AND ' +
      '(ChannelDetails.Model = MasterControlFeature.Model) ' +
    ' WHERE ((ChannelDetails.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND' +
    '        (ChannelDetails.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND' +
    '        (ChannelDetails.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND' +
    '        (ChannelDetails.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + '))' +
    ' ORDER BY ChannelDetails.Model,ChannelDetails.StudyAreaName,ChannelDetails.SubArea,ChannelDetails.Scenario,ChannelDetails.ChannelType,ChannelDetails.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadF03PenaltiesSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadF03PenaltiesSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Penalty01, Penalty02, Penalty03, Penalty04, Penalty05, ' +
      'Identifier, ArcCount, PenaltyName ' +
      'FROM ChannelArcPenalty ' +
      'WHERE (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND'+
      '      (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND'+
      '      (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND'+
      '      (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ')'+
      ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadSpillChannelSQL (APowerChannelNr : integer): string;
const OPNAME = 'TFile03DatabaseAgent.ReadSpillChannelSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT B.SpillChannelNumber, B.DownStreamPowerChannelCount, ' +
      'A.UpNodeNumber, A.DownNodeNumber, A.PenaltyNumber ' +
      'FROM ChannelDetails A, PowerPlants B WHERE ' +
      '(B.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(B.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(B.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(B.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(B.PowerChannelNumber = ' + IntToStr(APowerChannelNr) + ') AND ' +
      '(A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(A.ChannelNumber  = B.SpillChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadDownstreamPowerChannelsSQL (APowerChannelNr : integer): string;
const OPNAME = 'TFile03DatabaseAgent.ReadDownstreamPowerChannelsSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT ' +
      'Channel01, Channel02, Channel03, Channel04, Channel05, ' +
      'Channel06, Channel07, Channel08, Channel09, Channel10, ' +
      'Channel11, Channel12, Channel13, Channel14, Channel15, ' +
      'Channel16, Channel17, Channel18, Channel19, Channel20 ' +
      'FROM PowerPlants WHERE ' +
      '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(PowerChannelNumber = ' + IntToStr(APowerChannelNr) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadConsumptiveChannelSQL (ADivChannelNr : integer): string;
const OPNAME = 'TFile03DatabaseAgent.ReadConsumptiveChannelSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT B.ConsumptiveChannelNumber, B.RelaxationDemand, ' +
      'A.UpNodeNumber, A.DownNodeNumber, A.PenaltyNumber ' +
      'FROM ChannelDetails A, IrrigationAreas B WHERE ' +
      '(B.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(B.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(B.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(B.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(B.DiversionChannelNumber = ' + IntToStr(ADivChannelNr) + ') AND ' +
      '(A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(A.ChannelNumber  = B.ConsumptiveChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadReturnFlowChannelSQL (ADivChannelNr : integer): string;
const OPNAME = 'TFile03DatabaseAgent.ReadReturnFlowChannelSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT B.ReturnFlowChannelNumber, B.RelaxationDemand, ' +
      'A.UpNodeNumber, A.DownNodeNumber, A.PenaltyNumber ' +
      'FROM ChannelDetails A, IrrigationAreas B WHERE ' +
      '(B.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(B.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(B.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(B.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(B.DiversionChannelNumber = ' + IntToStr(ADivChannelNr) + ') AND ' +
      '(A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '(A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '(A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '(A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND '+
      '(A.ChannelNumber  = B.ReturnFlowChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadChannelOutputDataSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadChannelOutputDataSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT ChannelNumber, ChannelName, FirmYieldCalc, FlowOutput, OutputComment ' +
      'FROM ChannelDetails WHERE ' +
      '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
      '(SummaryOutput = ' + QuotedStr('Y') + ') ' +
      'ORDER BY FirmYieldCalc DESC';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadPlanningChannelOutputDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadPlanningChannelOutputDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT distinct SummaryChannels.Identifier, ChannelDetails.ChannelNumber, ChannelDetails.ChannelName, '+
              'ChannelDetails.SummaryOutput, ChannelDetails.FirmYieldCalc, ChannelDetails.FlowOutput, ChannelDetails.OutputComment ' +
              'FROM ChannelDetails LEFT JOIN SummaryChannels ON (ChannelDetails.ChannelNumber = SummaryChannels.ChannelNumber) AND ' +
              '(ChannelDetails.Scenario = SummaryChannels.Scenario) AND (ChannelDetails.SubArea = SummaryChannels.SubArea) AND ' +
              '(ChannelDetails.StudyAreaName = SummaryChannels.StudyAreaName) AND (ChannelDetails.Model = SummaryChannels.Model) ' +
              ' WHERE          ' +
              '(ChannelDetails.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              '(ChannelDetails.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              '(ChannelDetails.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              '(ChannelDetails.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              '(ChannelDetails.SummaryOutput=  ' + QuotedStr('Y') + ') '+
              'ORDER BY ChannelDetails.FirmYieldCalc desc, SummaryChannels.Identifier ';
     (*
      'SELECT distinct sc.Identifier, c.ChannelNumber, c.ChannelName,c.SummaryOutput, c.FirmYieldCalc, c.FlowOutput, c.OutputComment ' +
      'FROM ChannelDetails c, SummaryChannels sc WHERE ' +
      '(c.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      '(c.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      '(c.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      '(c.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
      '(c.ChannelNumber = sc.ChannelNumber ) AND ' +
      '(c.Model         = sc.Model)         AND ' +
      '(c.StudyAreaName = sc.StudyAreaName) AND ' +
      '(c.SubArea       = sc.SubArea)       AND ' +
      '(c.Scenario      = sc.Scenario )     AND ' +
      '(c.SummaryOutput = ' + QuotedStr('Y') + ') ' +
      'ORDER BY c.FirmYieldCalc desc, sc.Identifier ';*)
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile03DatabaseAgent.ReadSummaryOutputCountSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadSummaryOutputCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count (*) AS SummaryCount FROM ChannelDetails ' +
              ' WHERE Model       = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND SummaryOutput = ' + QuotedStr('Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadPlanningChannelOutputCountSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadPlanningChannelOutputCountSQL';
begin
  Result := '';
  try
    Result :=
      ' SELECT Count (*) AS SummaryCount FROM ChannelDetails ' +
      ' WHERE Model       = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
      ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
      ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
      ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND SummaryOutput = ' + QuotedStr('Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile03DatabaseAgent.ReadFirmYieldCalcCountSQL : string;
const OPNAME = 'TFile03DatabaseAgent.ReadFirmYieldCalcCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT Count (*) AS FirmYieldCount FROM ChannelDetails ' +
              ' WHERE Model       = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND SummaryOutput = ' + QuotedStr('Y') +
              ' AND FirmYieldCalc = ' + QuotedStr('Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteChannelArcPenaltyDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteChannelArcPenaltyDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelArcPenalty '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,ArcCount,' +
              'Penalty01,Penalty02,Penalty03,Penalty04,Penalty05,PenaltyName) '+
              'Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ArcCount, '+
              ':Penalty01,:Penalty02,:Penalty03,:Penalty04,:Penalty05,:PenaltyName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteChannelCommentsDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteChannelCommentsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelComments'+
              ' (Model,StudyAreaName,SubArea,Scenario,Comment01,Comment02,Comment03,Comment04,Comment05,Comment06'+
              ' ,Comment07,Comment08,Comment09,Comment10,Comment11,Comment12,Comment13,Comment14, Comment15, Comment16, Comment17, Comment22)'+
              ' Values '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Comment01,:Comment02,:Comment03,:Comment04,:Comment05,:Comment06'+
              ' ,:Comment07,:Comment08,:Comment09,:Comment10,:Comment11,:Comment12,:Comment13,:Comment14, :Comment15, :Comment16, :Comment17,:Comment22)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteChannelConfigSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteChannelConfigSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelConfig'+
              ' (Model,StudyAreaName,SubArea,Scenario,InflowPenaltyNo )'+
              ' Values '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:InflowPenaltyNo )';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile03DatabaseAgent.WriteChannelDetailsDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteChannelDetailsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ChannelDetails '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelName, ' +
              'ChannelNumber, ChannelType, ChannelSubType, Comment, ' +
              'DownNodeNumber, PenaltyNumber, UpNodeNumber, FirmYieldCalc, SummaryOutput) '+
              'Values(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelName, ' +
              ':ChannelNumber, :ChannelType, :ChannelSubType, :Comment, '+
              ':DownNodeNumber, :PenaltyNumber, :UpNodeNumber, :FirmYieldCalc, :SummaryOutput)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WritePumpingFeatureDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WritePumpingFeatureDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO PumpingFeature '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              'FeatureName, PumpingHead, PumpingEfficiency, ChannelNumber) '+
              'Values(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
              ':FeatureName, :PumpingHead, :PumpingEfficiency, :ChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteSpecifiedInflowFeatureDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteSpecifiedInflowFeatureDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO SpecifiedInflowFeature '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              'FeatureName, ChannelNumber,InflowFileName) '+
              'Values(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
              ':FeatureName, :ChannelNumber, :InflowFileName)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteSpecifiedDemandFeatureDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteSpecifiedDemandFeatureDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO SpecifiedDemandFeature '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              'FeatureName, ChannelNumber, Stochastic, GaugeNumber, Fullname) '+
              'Values(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
              ':FeatureName, :ChannelNumber, :Stochastic, :GaugeNumber, :Fullname)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteChannelOutputDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteChannelOutputDataSQL';
begin
  Result := '';
  try
    Result := 'UPDATE ChannelDetails ' +
              'SET FirmYieldCalc = :FirmYieldCalc, ' +
              'SummaryOutput = :SummaryOutput, ' +
              'FlowOutput = :FlowOutput, ' +
              'OutputComment = :OutputComment ' +
              'WHERE ' +
              '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
              ' ChannelNumber = :ChannelNumber ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WritePlanningChannelOutputDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WritePlanningChannelOutputDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO SummaryChannels '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber) '+
              'Values(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadDemandCentreSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadDemandCentreSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT NodeNumber, ConsumptiveChannelNr ' +
      'FROM YMDemandCentre WHERE ' +
      '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ' +
      'ORDER BY NodeNumber DESC';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadGroundWaterSubCatchmentSQL: string;
const OPNAME = 'TFile03DatabaseAgent.ReadGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario,GroundWaterIdentifier,' +
              ' RefNodeNumber,AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,BaseFlowNodeNumber,' +
              ' AquiferInflowChannelNr,AquferExcessChannelNr,' +
              ' GroundWaterBaseFlowChannelNr,RegulationFromAquiferChannelNr,' +
              ' RegulationFromBaseFlowChannelNr,BaseFlowRemainderChannelNr,SurfaceRunOffChannelNr,' +
              ' DownstreamChannelNr,UpstreamChannelNr' +
              ' FROM GroundWaterSubCatchment WHERE ' +
              '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadChannelPenaltyNumberSQL(AChannelNr: integer): string;
const OPNAME = 'TFile03DatabaseAgent.ReadChannelPenaltyNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT PenaltyNumber FROM ChannelDetails  WHERE ' +
              '(Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
              '(StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
              '(SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
              '(Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND' +
              ' ChannelNumber = ' + IntToStr(AChannelNr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.WriteGroundWaterSubCatchmentSQL: string;
const OPNAME = 'TFile03DatabaseAgent.WriteGroundWaterSubCatchmentSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GroundWaterSubCatchment '+
              '(Model, StudyAreaName,SubArea,Scenario,GroundWaterIdentifier,RefNodeNumber,'+
              ' AquiferNodeNumber,AbstractionNodeNumber,CollectionNodeNumber,BaseFlowNodeNumber,'+
              ' AquiferInflowChannelNr,AquferExcessChannelNr,GroundWaterBaseFlowChannelNr,'+
              ' RegulationFromAquiferChannelNr,RegulationFromBaseFlowChannelNr,DownstreamChannelNr,'+
              ' BaseFlowRemainderChannelNr, UpstreamChannelNr,SurfaceRunOffChannelNr)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:GroundWaterIdentifier,:RefNodeNumber,' +
              ':AquiferNodeNumber,:AbstractionNodeNumber,:CollectionNodeNumber,:BaseFlowNodeNumber,'+
              ':AquiferInflowChannelNr,:AquferExcessChannelNr,:GroundWaterBaseFlowChannelNr,'+
              ':RegulationFromAquiferChannelNr,:RegulationFromBaseFlowChannelNr, :DownstreamChannelNr,' +
              ':BaseFlowRemainderChannelNr,:UpstreamChannelNr,:SurfaceRunOffChannelNr)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile03DatabaseAgent.ReadModelDataFromDatabase';
var
  LPath,
  LMessage: string;
  LFieldName: string;
  LCount,
  //LCount1,
  LIdCounter1,
  LIdCounter2,
  LIdCounter3,
  LIdCounter4,
  LIdCounter5,
  LIdCounter6,
  LIdCounter7,
  LIdCounter8,
  LIdCounter9,
  LIdCounter10,
  LIdCounter11,
  LIdCounter12,
  LIdCounter13,
  LIdCounter13a,
  LIdCounter14a,
  LIdCounter15a,
  LIdCounter15b,
  LIdCounter15,
  LIdCounter16,
  LArrayCount: integer;
  LNoData : Boolean;
  LChannelDescr: TChannelDescrObject;
  LStop: boolean;
  LDataSet    : TAbstractModelDataset;
  LDataSetP   : TAbstractModelDataset;
  LDataSetDC  : TAbstractModelDataset;
  lDataSetGW  : TAbstractModelDataset;
  lDataSetGWChannel : TAbstractModelDataset;
  lIrrChannelObj    : TIrrigationChannelObject;
  lPowChannelObj    : TPowerChannelObject;
  lSumChannelObj    : TSummaryChannelObject;
  lPumpChannelObj   : TPumpingChannelObject;
  lMasterChannelObj : TMasterChannelObject;
  lChannelType      : integer;
  lChannelCount     : integer;
  //LDemandCentreObj  : TDemandCentreObject;
  //LReclaimationChannelObj : TReclaimationChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile03DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LChannelDescr := ADataObject.FChannelDescrObject;


    if not LChannelDescr.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetP);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetDC);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetGW);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetGWChannel);
    try

      LDataSet.SetSQL(ReadF03KnownDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      LNoData := False;
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LNoData := True;
        LMessage := FAppModules.Language.GetString('TFile03DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
       // Exit;
      end;

      // Counters
      LChannelDescr.FSummaryChannelCount.FData := 0;
      LChannelDescr.FSummaryChannelCount.FInitalised := True;

      LChannelDescr.FPenaltyChannelCount.FData := 0;
      LChannelDescr.FPenaltyChannelCount.FInitalised := True;

      LChannelDescr.FMasterChannelCount.FData := 0;
      LChannelDescr.FMasterChannelCount.FInitalised := True;

      LChannelDescr.FPowerChannelCount.FData := 0;
      LChannelDescr.FPowerChannelCount.FInitalised := True;

      LChannelDescr.FIrrigationChannelCount.FData := 0;
      LChannelDescr.FIrrigationChannelCount.FInitalised := True;

      LChannelDescr.FDiversionChannelCount.FData := 0;
      LChannelDescr.FDiversionChannelCount.FInitalised := True;

      LChannelDescr.FMinFlowChannelCount.FData := 0;
      LChannelDescr.FMinFlowChannelCount.FInitalised := True;

      LChannelDescr.FLossChannelCount.FData := 0;
      LChannelDescr.FLossChannelCount.FInitalised := True;

      LChannelDescr.FMultiPurposeChannelCount.FData := 0;
      LChannelDescr.FMultiPurposeChannelCount.FInitalised := True;

      LChannelDescr.FPumpingChannelCount.FData := 0;
      LChannelDescr.FPumpingChannelCount.FInitalised := True;

      LChannelDescr.FInflowChannelCount.FData := 0;
      LChannelDescr.FInflowChannelCount.FInitalised := True;

      LChannelDescr.FDemandChannelCount.FData := 0;
      LChannelDescr.FDemandChannelCount.FInitalised := True;

      LChannelDescr.FGeneralChannelCount.FData := 0;
      LChannelDescr.FGeneralChannelCount.FInitalised := True;

      LChannelDescr.FIrrigationBlockCount.FData := 0;
      LChannelDescr.FIrrigationBlockCount.FInitalised := True;

      LChannelDescr.FWetlandCount.FData := 0;
      LChannelDescr.FWetlandCount.FInitalised := True;

      LChannelDescr.FReturnFlowChannelCount.FData := 0;
      LChannelDescr.FReturnFlowChannelCount.FInitalised := True;

      LChannelDescr.FDemandCentreCount.FData := 0;
      LChannelDescr.FDemandCentreCount.FInitalised := True;

      LChannelDescr.FReclaimationChannelCount.FData := 0;
      LChannelDescr.FReclaimationChannelCount.FInitalised := True;

      LChannelDescr.FGroundWaterCount.FData := 0;
      LChannelDescr.FGroundWaterCount.FInitalised := True;

      // Need to rewrite more efficiently
      LDataSetDC.DataSet.Close;
      LDataSetDC.SetSQL(ReadDemandCentreSQL);
      LDataSetDC.DataSet.Open;
      LDataSetDC.DataSet.First;
      while not LDataSetDC.DataSet.Eof do
      begin
        LChannelDescr.FDemandCentreCount.FData := LChannelDescr.FDemandCentreCount.FData;
        LDataSetDC.DataSet.Next;
      end;
      LDataSetDC.DataSet.Close;

      lDataSetGW.DataSet.Close;
      lDataSetGW.SetSQL(ReadGroundWaterSubCatchmentSQL);
      lDataSetGW.DataSet.Open;
      lDataSetGW.DataSet.First;
      while not lDataSetGW.DataSet.Eof do
      begin
        LChannelDescr.FGroundWaterCount.FData := LChannelDescr.FGroundWaterCount.FData + 1;
        lDataSetGW.DataSet.Next;
      end;
      lDataSetGW.DataSet.Close;


      LDataSetP.SetSQL(ReadF03CountsSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;
      while (NOT LDataSetP.DataSet.EOF) do
      begin
        lChannelType  := LDataSetP.DataSet.FieldByName('ChannelType').AsInteger;
        lChannelCount := LDataSetP.DataSet.FieldByName('ChannelTypeCounter').AsInteger;
        case lChannelType of
          2 : LChannelDescr.FMasterChannelCount.FData := lChannelCount;
          3 : LChannelDescr.FPowerChannelCount.FData  := lChannelCount;
          4 : LChannelDescr.FIrrigationChannelCount.FData := lChannelCount;
          5 : LChannelDescr.FDiversionChannelCount.FData := lChannelCount;
          6 : LChannelDescr.FMinFlowChannelCount.FData := lChannelCount;
          7 : LChannelDescr.FLossChannelCount.FData := lChannelCount;
          8,22,23,24 : LChannelDescr.FMultiPurposeChannelCount.FData :=
                       LChannelDescr.FMultiPurposeChannelCount.FData + lChannelCount;
          9 : LChannelDescr.FPumpingChannelCount.FData := lChannelCount;
         10 : LChannelDescr.FInflowChannelCount.FData := lChannelCount;
         11 : LChannelDescr.FDemandChannelCount.FData := lChannelCount;
         12 : LChannelDescr.FGeneralChannelCount.FData := lChannelCount;
         14, 15 : LChannelDescr.FIrrigationBlockCount.FData :=
                          LChannelDescr.FIrrigationBlockCount.FData + lChannelCount;
         16, 17 : LChannelDescr.FWetlandCount.FData :=
                          LChannelDescr.FWetlandCount.FData + lChannelCount;
         20 : LChannelDescr.FReturnFlowChannelCount.FData :=
                          LChannelDescr.FReturnFlowChannelCount.FData + lChannelCount;
         21 : LChannelDescr.FReclaimationChannelCount.FData :=
                          LChannelDescr.FReclaimationChannelCount.FData + lChannelCount;
        else
        end;
        LDataSetP.DataSet.Next;
      end;
      LDataSetP.DataSet.Close;

      {LChannelDescr.FMasterChannelCount.FData       :=  LChannelDescr.FMasterChannelCount.FData + 1;
      LChannelDescr.FPowerChannelCount.FData        :=  LChannelDescr.FPowerChannelCount.FData + 1;
      LChannelDescr.FIrrigationChannelCount.FData   :=  LChannelDescr.FIrrigationChannelCount.FData + 1;
      LChannelDescr.FDiversionChannelCount.FData    :=  LChannelDescr.FDiversionChannelCount.FData + 1;
      LChannelDescr.FMinFlowChannelCount.FData      :=  LChannelDescr.FMinFlowChannelCount.FData + 1;
      LChannelDescr.FLossChannelCount.FData         :=  LChannelDescr.FLossChannelCount.FData + 1;
      LChannelDescr.FMultiPurposeChannelCount.FData :=  LChannelDescr.FMultiPurposeChannelCount.FData + 1;
      LChannelDescr.FPumpingChannelCount.FData      :=  LChannelDescr.FPumpingChannelCount.FData + 1;
      LChannelDescr.FInflowChannelCount.FData       :=  LChannelDescr.FInflowChannelCount.FData + 1;
      LChannelDescr.FDemandChannelCount.FData       :=  LChannelDescr.FDemandChannelCount.FData + 1;
      LChannelDescr.FGeneralChannelCount.FData      :=  LChannelDescr.FGeneralChannelCount.FData + 1;
      LChannelDescr.FIrrigationBlockCount.FData     :=  LChannelDescr.FIrrigationBlockCount.FData + 1;
      LChannelDescr.FWetlandCount.FData             :=  LChannelDescr.FWetlandCount.FData + 1;
      LChannelDescr.FReturnFlowChannelCount.FData   :=  LChannelDescr.FReturnFlowChannelCount.FData + 1;
      LChannelDescr.FReclaimationChannelCount.FData :=  LChannelDescr.FReclaimationChannelCount.FData + 1; }

      LDataSetP.SetSQL(ReadDivType4CountSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;
      if (NOT LDataSetP.DataSet.EOF) then
      begin
        lChannelCount := LDataSetP.DataSet.FieldByName('CounterAll').AsInteger;
        LChannelDescr.FDiversionChannelCount.FData := LChannelDescr.FDiversionChannelCount.FData - lChannelCount;
        LChannelDescr.FLossChannelCount.FData      := LChannelDescr.FLossChannelCount.FData + lChannelCount;
      end;
      LDataSetP.DataSet.Close;

      if  (FAppModules.Model.ModelName = CPlanning) then
        LDataSetP.SetSQL(ReadPlanningChannelOutputCountSQL)
      else
        LDataSetP.SetSQL(ReadSummaryOutputCountSQL);

      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;
      if not LDataSetP.DataSet.FieldByName('SummaryCount').IsNull then
      begin
        LChannelDescr.FSummaryChannelCount.FData := LDataSetP.DataSet.FieldByName('SummaryCount').AsInteger;
        LChannelDescr.FSummaryChannelCount.FInitalised := True;
      end;
      LDataSetP.DataSet.Close;

      LDataSetP.SetSQL(ReadFirmYieldCalcCountSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;
      if not LDataSetP.DataSet.FieldByName('FirmYieldCount').IsNull then
      begin
        LChannelDescr.FFirmYieldAnalysesCount.FData := LDataSetP.DataSet.FieldByName('FirmYieldCount').AsInteger;
        LChannelDescr.FFirmYieldAnalysesCount.FInitalised := True;
      end;
      LDataSetP.DataSet.Close;

      if  (FAppModules.Model.ModelName = CPlanning) then
      begin
         LDataSetP.SetSQL(ReadInflowPenaltyNoSQL);
         LDataSetP.DataSet.Open;
         LDataSetP.DataSet.First;

         LChannelDescr.FInflowPenaltyNo.FData := LDataSetP.DataSet.FieldByName('InflowPenaltyNo').AsInteger;
         LChannelDescr.FInflowPenaltyNo.FInitalised := True;
         LDataSetP.DataSet.Close;
      end;

      LDataSetP.SetSQL(ReadF03CommentsSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;

      //line1 and all counts lines comments ++++++++++++++++++++++++++++

      if not LDataSetP.DataSet.FieldByName('Comment01').IsNull then
      begin
        LChannelDescr.FComment01.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment01').AsString);
        LChannelDescr.FComment01.FInitalised := True;
        LChannelDescr.FComment01.FLength := Length(LChannelDescr.FComment01.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment02').IsNull then
      begin
        LChannelDescr.FComment02.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment02').AsString);
        LChannelDescr.FComment02.FInitalised := True;
        LChannelDescr.FComment02.FLength := Length(LChannelDescr.FComment02.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment03').IsNull then
      begin
        LChannelDescr.FComment03.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment03').AsString);
        LChannelDescr.FComment03.FInitalised := True;
        LChannelDescr.FComment03.FLength := Length(LChannelDescr.FComment03.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment04').IsNull then
      begin
        LChannelDescr.FComment04.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment04').AsString);
        LChannelDescr.FComment04.FInitalised := True;
        LChannelDescr.FComment04.FLength := Length(LChannelDescr.FComment04.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment05').IsNull then
      begin
        LChannelDescr.FComment05.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment05').AsString);
        LChannelDescr.FComment05.FInitalised := True;
        LChannelDescr.FComment05.FLength := Length(LChannelDescr.FComment05.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment06').IsNull then
      begin
        LChannelDescr.FComment06.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment06').AsString);
        LChannelDescr.FComment06.FInitalised := True;
        LChannelDescr.FComment06.FLength := Length(LChannelDescr.FComment06.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment07').IsNull then
      begin
        LChannelDescr.FComment07.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment07').AsString);
        LChannelDescr.FComment07.FInitalised := True;
        LChannelDescr.FComment07.FLength := Length(LChannelDescr.FComment07.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment08').IsNull then
      begin
        LChannelDescr.FComment08.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment08').AsString);
        LChannelDescr.FComment08.FInitalised := True;
        LChannelDescr.FComment08.FLength := Length(LChannelDescr.FComment08.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment09').IsNull then
      begin
        LChannelDescr.FComment09.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment09').AsString);
        LChannelDescr.FComment09.FInitalised := True;
        LChannelDescr.FComment09.FLength := Length(LChannelDescr.FComment09.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment10').IsNull then
      begin
        LChannelDescr.FComment10.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment10').AsString);
        LChannelDescr.FComment10.FInitalised := True;
        LChannelDescr.FComment10.FLength := Length(LChannelDescr.FComment10.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment11').IsNull then
      begin
        LChannelDescr.FComment11.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment11').AsString);
        LChannelDescr.FComment11.FInitalised := True;
        LChannelDescr.FComment11.FLength := Length(LChannelDescr.FComment11.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment12').IsNull then
      begin
        LChannelDescr.FComment12.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment12').AsString);
        LChannelDescr.FComment12.FInitalised := True;
        LChannelDescr.FComment12.FLength := Length(LChannelDescr.FComment12.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment13').IsNull then
      begin
        LChannelDescr.FComment13.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment13').AsString);
        LChannelDescr.FComment13.FInitalised := True;
        LChannelDescr.FComment13.FLength := Length(LChannelDescr.FComment13.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment14').IsNull then
      begin
        LChannelDescr.FComment14.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment14').AsString);
        LChannelDescr.FComment14.FInitalised := True;
        LChannelDescr.FComment14.FLength := Length(LChannelDescr.FComment14.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment15').IsNull then
      begin
        LChannelDescr.FComment15.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment15').AsString);
        LChannelDescr.FComment15.FInitalised := True;
        LChannelDescr.FComment15.FLength := Length(LChannelDescr.FComment15.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment16').IsNull then
      begin
        LChannelDescr.FComment16.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment16').AsString);
        LChannelDescr.FComment16.FInitalised := True;
        LChannelDescr.FComment16.FLength := Length(LChannelDescr.FComment16.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment17').IsNull then
      begin
        LChannelDescr.FComment17.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment17').AsString);
        LChannelDescr.FComment17.FInitalised := True;
        LChannelDescr.FComment17.FLength := Length(LChannelDescr.FComment17.FData);
      end;

      if not LDataSetP.DataSet.FieldByName('Comment22').IsNull then
      begin
        LChannelDescr.FComment13.FData := TrimRight(LDataSetP.DataSet.FieldByName('Comment22').AsString);
        LChannelDescr.FComment13.FInitalised := True;
        LChannelDescr.FComment13.FLength := Length(LChannelDescr.FComment13.FData);
      end;

      LDataSetP.DataSet.Close;

      LDataSetP.SetSQL(ReadF03PenaltiesSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;
      LChannelDescr.FPenaltyChannelCount.FData := LDataSetP.DataSet.RecordCount;

      if(not LChannelDescr.AddPenaltyChannels) or
        (not LChannelDescr.AddMasterChannels) or
        (not LChannelDescr.AddPowerChannels) or
        (not LChannelDescr.AddIrrigationChannels) or
        (not LChannelDescr.AddDiversionChannels) or
        (not LChannelDescr.AddMinFlowChannels) or
        (not LChannelDescr.AddLossChannels) or
        (not LChannelDescr.AddMultiPurposeChannels) or
        (not LChannelDescr.AddPumpingChannels) or
        (not LChannelDescr.AddInflowChannels) or
        (not LChannelDescr.AddDemandChannels) or
        (not LChannelDescr.AddGeneralChannels) or
        (not LChannelDescr.AddSummaryChannels) or
        (not LChannelDescr.AddIrrigationBlockChannels) or
        (not LChannelDescr.AddWetlandChannels)  or
        (not LChannelDescr.AddReturnFlowChannels) or
        (not LChannelDescr.AddDemandCentreChannels) or
        (not LChannelDescr.AddReclaimationChannels)or
        (not LChannelDescr.AddGroundWaterChannels) then
      Exit;

      LIdCounter1  := 0;
      LIdCounter2  := 0;
      LIdCounter3  := 0;
      LIdCounter4  := 0;
      LIdCounter5  := 0;
      LIdCounter6  := 0;
      LIdCounter7  := 0;
      LIdCounter8  := 0;
      LIdCounter9  := 0;
      LIdCounter10 := 0;
      LIdCounter11 := 0;
      LIdCounter12 := 0;
      LIdCounter13 := 0;
      LIdCounter13a := 0;
      LIdCounter14a := 0;      
      LIdCounter15a := 0;
      LIdCounter15b := 0;
      LIdCounter15  := 0;
      LIdCounter16  := 0;

      //line1 details +++++++++++++++++++++++++++++
      while not LDataSetP.DataSet.Eof do
      begin
        LIdCounter1  := LIdCounter1 + 1;

        if not lDataSetP.DataSet.FieldByName('Identifier').IsNull then
        begin
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FPenaltyType.FData :=
            lDataSetP.DataSet.FieldByName('Identifier').AsInteger;
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FPenaltyType.FInitalised := True;
        end;

        if not lDataSetP.DataSet.FieldByName('ArcCount').IsNull then
        begin
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FArcCount.FData :=
            lDataSetP.DataSet.FieldByName('ArcCount').AsInteger;
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FArcCount.FInitalised := True;
        end;

        if not lDataSetP.DataSet.FieldByName('PenaltyName').IsNull then
        begin
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FComment.FData :=
            Trim(lDataSetP.DataSet.FieldByName('PenaltyName').AsString);
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FComment.FInitalised := True;
          TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FComment.FLength :=
            Length(Trim(lDataSetP.DataSet.FieldByName('PenaltyName').AsString));
        end;

        for LArrayCount :=  MinArcs to MaxArcs do
        begin
          LFieldName := Format('%s%2.2d',['Penalty',LArrayCount]);
          if not lDataSetP.DataSet.FieldByName(LFieldName).IsNull then
          begin
            TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FArcPenalty[LArrayCount].FData :=
              lDataSetP.DataSet.FieldByName(LFieldName).AsFloat;
            TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter1]).FArcPenalty[LArrayCount].FInitalised := True;
          end;
        end;
        LDataSetP.DataSet.Next;
      end;
      LDataSetP.DataSet.Close;

      LDataSet.DataSet.First;
      while not LDataSet.DataSet.Eof do
      begin
        //line2 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 2) then
        begin
          LIdCounter2  := LIdCounter2 + 1;
          lMasterChannelObj := TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter2]);

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            lMasterChannelObj.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            lMasterChannelObj.FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            lMasterChannelObj.FUpNodeNumber.FData := LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            lMasterChannelObj.FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            lMasterChannelObj.FDownNodeNumber.FData := LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            lMasterChannelObj.FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            lMasterChannelObj.FPenaltyStructType.FData := LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            lMasterChannelObj.FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MasterControlType').IsNull then
          begin
            lMasterChannelObj.FChannelType.FData := (Trim(LDataSet.DataSet.FieldByName('MasterControlType').AsString)+' ')[1];
            lMasterChannelObj.FChannelType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            lMasterChannelObj.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            lMasterChannelObj.FComment.FInitalised := True;
            lMasterChannelObj.FComment.FLength := Length(lMasterChannelObj.FComment.FData);
          end;
        end;

        //line3 details +++++++++++++++++++++++++++++
        if ((LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 3) AND
            (LDataSet.DataSet.FieldByName('ChannelSubType').AsInteger = 1)) then
        begin
          LIdCounter3  := LIdCounter3 + 1;
          lPowChannelObj := TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter3]);

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            lPowChannelObj.FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            lPowChannelObj.FChannelNumber.FInitalised := True;

            LDataSetP.SetSQL(ReadSpillChannelSQL(LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger));
            LDataSetP.DataSet.Open;
            if (LDataSetP.DataSet.RecordCount > 0) then
            begin

              if not lDataSetP.DataSet.FieldByName('SpillChannelNumber').IsNull then
              begin
                lPowChannelObj.FSpillChannelNumber.FData :=
                lDataSetP.DataSet.FieldByName('SpillChannelNumber').AsInteger;
                lPowChannelObj.FSpillChannelNumber.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('UpNodeNumber').IsNull then
              begin
                lPowChannelObj.FSpillUpNodeNumber.FData :=
                lDataSetP.DataSet.FieldByName('UpNodeNumber').AsInteger;
                lPowChannelObj.FSpillUpNodeNumber.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('DownNodeNumber').IsNull then
              begin
                lPowChannelObj.FSpillDownNodeNumber.FData :=
                lDataSetP.DataSet.FieldByName('DownNodeNumber').AsInteger;
                lPowChannelObj.FSpillDownNodeNumber.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('PenaltyNumber').IsNull then
              begin
                lPowChannelObj.FSpillPenaltyStructType.FData :=
                lDataSetP.DataSet.FieldByName('PenaltyNumber').AsInteger;
                lPowChannelObj.FSpillPenaltyStructType.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('DownStreamPowerChannelCount').IsNull then
              begin
                lPowChannelObj.FDownStreamPowerChannelCount.FData :=
                lDataSetP.DataSet.FieldByName('DownStreamPowerChannelCount').AsInteger;
                lPowChannelObj.FDownStreamPowerChannelCount.FInitalised := True;
              end;
            end;
            LDataSetP.DataSet.Close;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            lPowChannelObj.FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            lPowChannelObj.FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            lPowChannelObj.FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            lPowChannelObj.FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            lPowChannelObj.FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            lPowChannelObj.FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            lPowChannelObj.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            lPowChannelObj.FComment.FLength := Length(lPowChannelObj.FComment.FData);
            lPowChannelObj.FComment.FInitalised := True;
          end;

          LDataSetP.SetSQL(ReadDownstreamPowerChannelsSQL(LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger));
          LDataSetP.DataSet.Open;
          if (LDataSetP.DataSet.RecordCount > 0) then
          begin
            for LArrayCount :=  MinDownStreamPowerChannels to MaxDownStreamPowerChannels do
            begin
              LFieldName := Format('%s%2.2d',['Channel',LArrayCount]);
              if not lDataSetP.DataSet.FieldByName(LFieldName).IsNull then
              begin
                lPowChannelObj.FDownStreamPowerChannels[LArrayCount].FData :=
                lDataSetP.DataSet.FieldByName(LFieldName).AsInteger;
                lPowChannelObj.FDownStreamPowerChannels[LArrayCount].FInitalised := True;
              end;
            end;
          end;
          LDataSetP.DataSet.Close;
        end;

        //line4 details +++++++++++++++++++++++++++++
        if ((LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 4) AND
            (LDataSet.DataSet.FieldByName('ChannelSubType').AsInteger = 1)) then
        begin
          LIdCounter4  := LIdCounter4 + 1;
          lIrrChannelObj := TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter4]);

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            lIrrChannelObj.FIrrigationNodeNumber.FData := LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            lIrrChannelObj.FIrrigationNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            lIrrChannelObj.FUpstreamNodeNumber.FData := LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            lIrrChannelObj.FUpstreamNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            lIrrChannelObj.FIrrigationPenaltyStructType.FData := LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            lIrrChannelObj.FIrrigationPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            lIrrChannelObj.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            lIrrChannelObj.FComment.FLength := Length(lIrrChannelObj.FComment.FData);
            lIrrChannelObj.FComment.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            lIrrChannelObj.FDiversionChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            lIrrChannelObj.FDiversionChannelNumber.FInitalised := True;

            LDataSetP.SetSQL(ReadConsumptiveChannelSQL(LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger));
            LDataSetP.DataSet.Open;
            if (LDataSetP.DataSet.RecordCount > 0) then
            begin

              if not lDataSetP.DataSet.FieldByName('ConsumptiveChannelNumber').IsNull then
              begin
                lIrrChannelObj.FConsumptiveChannelNumber.FData := lDataSetP.DataSet.FieldByName('ConsumptiveChannelNumber').AsInteger;
                lIrrChannelObj.FConsumptiveChannelNumber.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('PenaltyNumber').IsNull then
              begin
                lIrrChannelObj.FConsumptivePenaltyStructType.FData := lDataSetP.DataSet.FieldByName('PenaltyNumber').AsInteger;
                lIrrChannelObj.FConsumptivePenaltyStructType.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('RelaxationDemand').IsNull then
              begin
                lIrrChannelObj.FRelaxationDemand.FData := lDataSetP.DataSet.FieldByName('RelaxationDemand').AsInteger;
                lIrrChannelObj.FRelaxationDemand.FInitalised := True;
              end;
            end;
            LDataSetP.DataSet.Close;

            LDataSetP.SetSQL(ReadReturnFlowChannelSQL(LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger));
            LDataSetP.DataSet.Open;
            if (LDataSetP.DataSet.RecordCount > 0) then
            begin

              if not lDataSetP.DataSet.FieldByName('ReturnFlowChannelNumber').IsNull then
              begin
                lIrrChannelObj.FReturnChannelNumber.FData := lDataSetP.DataSet.FieldByName('ReturnFlowChannelNumber').AsInteger;
                lIrrChannelObj.FReturnChannelNumber.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('DownNodeNumber').IsNull then
              begin
                lIrrChannelObj.FDownStreamNodeNumber.FData := lDataSetP.DataSet.FieldByName('DownNodeNumber').AsInteger;
                lIrrChannelObj.FDownStreamNodeNumber.FInitalised := True;
              end;

              if not lDataSetP.DataSet.FieldByName('PenaltyNumber').IsNull then
              begin
                lIrrChannelObj.FReturnPenaltyStructType.FData := lDataSetP.DataSet.FieldByName('PenaltyNumber').AsInteger;
                lIrrChannelObj.FReturnPenaltyStructType.FInitalised := True;
              end;
            end;
            LDataSetP.DataSet.Close;
          end;
        end;

        //line5 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 5) then
        begin
          if (NOT LDataSet.DataSet.FieldByName('DivChannelType').IsNull) AND
             (LDataSet.DataSet.FieldByName('DivChannelType').AsInteger = 4) then
          begin //Diversion type 4 = Loss type 1
            LIdCounter7  := LIdCounter7 + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelNumber.FData :=
              LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FUpNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FUpNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FDownNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FDownNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FPenaltyStructType.FData :=
              LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FPenaltyStructType.FInitalised := True;
            end;

            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelType.FData := 1;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelType.FInitalised := True;

            if not LDataSet.DataSet.FieldByName('Reference').IsNull then
            begin
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FReference.FData :=
              LDataSet.DataSet.FieldByName('Reference').AsInteger;
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FReference.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Comment').IsNull then
            begin
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FComment.FData :=
                TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FComment.FLength :=
                Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
              TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FComment.FInitalised := True;
            end;
          end
          else
          begin
            LIdCounter5  := LIdCounter5 + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FChannelNumber.FData :=
              LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FChannelNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FUpNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FUpNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FDownNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FDownNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FPenaltyStructType.FData :=
              LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FPenaltyStructType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DivChannelType').IsNull then
            begin
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FChannelType.FData :=
              LDataSet.DataSet.FieldByName('DivChannelType').AsInteger;
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FChannelType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Comment').IsNull then
            begin
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FComment.FData :=
                TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FComment.FLength :=
                Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
              TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter5]).FComment.FInitalised := True;
            end;
          end;
        end;

        //line6 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 6) then
        begin
          LIdCounter6  := LIdCounter6 + 1;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FComment.FData :=
              TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FComment.FInitalised := True;
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter6]).FComment.FLength :=
            Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
          end;
        end;

        //line7 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 7) then
        begin
          LIdCounter7  := LIdCounter7 + 1;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('LossType').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelType.FData :=
            LDataSet.DataSet.FieldByName('LossType').AsInteger;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FChannelType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Reference').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FReference.FData :=
            LDataSet.DataSet.FieldByName('Reference').AsInteger;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FReference.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FComment.FData :=
              TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FComment.FInitalised := True;
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter7]).FComment.FLength :=
              Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
          end;
        end;

        //line8 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [8,22,23,24]) then
        begin
          LIdCounter8  := LIdCounter8 + 1;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FComment.FData :=
              TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FComment.FInitalised := True;
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter8]).FComment.FLength :=
              Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
          end;
        end;

        //line9 details +++++++++++++++++++++++++++++
        if (LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 9) then
        begin
          LIdCounter9  := LIdCounter9 + 1;
          lPumpChannelObj := TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LIdCounter9]);

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            lPumpChannelObj.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            lPumpChannelObj.FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            lPumpChannelObj.FUpNodeNumber.FData := LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            lPumpChannelObj.FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            lPumpChannelObj.FDownNodeNumber.FData := LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            lPumpChannelObj.FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            lPumpChannelObj.FPenaltyStructType.FData := LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            lPumpChannelObj.FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PumpingHead').IsNull then
          begin
            lPumpChannelObj.FPumpingHead.FData := LDataSet.DataSet.FieldByName('PumpingHead').AsFloat;
            lPumpChannelObj.FPumpingHead.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PumpingEfficiency').IsNull then
          begin
            lPumpChannelObj.FEfficiency.FData := LDataSet.DataSet.FieldByName('PumpingEfficiency').AsFloat;
            lPumpChannelObj.FEfficiency.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            lPumpChannelObj.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            lPumpChannelObj.FComment.FInitalised := True;
            lPumpChannelObj.FComment.FLength := Length(lPumpChannelObj.FComment.FData);
          end;

        end;

        //line10 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 10) then
        begin
          LIdCounter10  := LIdCounter10 + 1;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FComment.FData :=
              TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FComment.FInitalised := True;
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter10]).FComment.FLength :=
              Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
          end;
        end;

        //line11 details +++++++++++++++++++++++++++++
        LPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath;
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 11) then
        begin
          LIdCounter11  := LIdCounter11 + 1;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('GaugeNumber').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FGaugeNumber.FData :=
            LDataSet.DataSet.FieldByName('GaugeNumber').AsInteger;
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FGaugeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Stochastic').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FStochastic.FData :=
              (Trim(LDataSet.DataSet.FieldByName('Stochastic').AsString)+' ')[1];
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FStochastic.FInitalised := True;
          end;


          if not LDataSet.DataSet.FieldByName('Fullname').IsNull then
          begin
            {if Pos(UpperCase(LPath),UpperCase(Trim(LDataSet.DataSet.FieldByName('Fullname').AsString))) = 0 then
              TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FData :=
                LPath + Trim(LDataSet.DataSet.FieldByName('Fullname').AsString)
            else
              TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FData :=
                Trim(LDataSet.DataSet.FieldByName('Fullname').AsString);
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FLength := 50;}

            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FData :=
              Trim(LDataSet.DataSet.FieldByName('Fullname').AsString);
            //TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FLength :=
            //Length(TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FData);
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FFullname.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FComment.FData :=
              TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FComment.FInitalised := True;
            TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter11]).FComment.FLength :=
              Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
          end;
        end;

        //line12 details +++++++++++++++++++++++++++++
        if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger = 12) then
        begin
          LIdCounter12  := LIdCounter12 + 1;
          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FChannelNumber.FData :=
            LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FChannelNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
          begin
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FUpNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FUpNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
          begin
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FDownNodeNumber.FData :=
            LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FDownNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
          begin
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FPenaltyStructType.FData :=
            LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FPenaltyStructType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FComment.FData :=
              TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FComment.FInitalised := True;
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter12]).FComment.FLength :=
              Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
          end;
        end;

        //line13a version 7 details +++++++++++++++++++++++++++++
        if FAppModules.StudyArea.ModelVersion = '7' then
        begin
          if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [14]) then
          begin
            LIdCounter13a  := LIdCounter13a + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FAbstractChannelNr.FData :=
              LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FAbstractChannelNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ChannelType').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FChannelType.FData :=
              LDataSet.DataSet.FieldByName('ChannelType').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FChannelType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FUpNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FUpNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FBlockNumber.FData :=
              LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FBlockNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FAbstractPenaltyType.FData :=
              LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FAbstractPenaltyType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Comment').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FComment.FData :=
                TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FComment.FInitalised := True;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FComment.FLength :=
                Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
            end;
          end;

          if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [15]) then
          begin
            LIdCounter13a  := LIdCounter13a + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FReturnFlowChannelNr.FData :=
              LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FReturnFlowChannelNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ChannelType').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FChannelType.FData :=
              LDataSet.DataSet.FieldByName('ChannelType').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FChannelType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FBlockNumber.FData :=
              LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FBlockNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FDownNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FDownNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FReturnFlowPenaltyType.FData :=
              LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FReturnFlowPenaltyType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('Comment').IsNull then
            begin
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FComment.FData :=
                TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FComment.FInitalised := True;
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter13a]).FComment.FLength :=
                Length(TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString));
            end;
          end;
        end;

        //line14a version 7 details +++++++++++++++++++++++++++++
        if FAppModules.StudyArea.ModelVersion = '7' then
        begin
          if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [16]) then
          begin
            LIdCounter14a  := LIdCounter14a + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FInflowChannelNr.FData :=
              LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FInflowChannelNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ChannelType').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FChannelType.FData :=
              LDataSet.DataSet.FieldByName('ChannelType').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FChannelType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FUpNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FUpNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FInflowPenaltyType.FData :=
              LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FInflowPenaltyType.FInitalised := True;
            end;
          end;

          if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [17]) then
          begin
            LIdCounter14a  := LIdCounter14a + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FOutflowChannelNr.FData :=
              LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FOutflowChannelNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ChannelType').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FChannelType.FData :=
              LDataSet.DataSet.FieldByName('ChannelType').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FChannelType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FDownNodeNumber.FData :=
              LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FDownNodeNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FOutflowPenaltyType.FData :=
              LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter14a]).FOutflowPenaltyType.FInitalised := True;
            end;
          end;
        end;

        //line15a version 7 details +++++++++++++++++++++++++++++
        if FAppModules.StudyArea.ModelVersion = '7' then
        begin
          if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [20]) then
          begin
            LIdCounter15a  := LIdCounter15a + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FChannelNr.FData :=
                LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FChannelNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FDemandCentreNodeNr.FData :=
                LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FDemandCentreNodeNr.FInitalised := True;

              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FUpstreamNodeNr.FData :=
                LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FUpstreamNodeNr.FInitalised := True;              
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FDownStreamNodeNr.FData :=
                LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FDownStreamNodeNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FPenaltyStructureType.FData :=
                LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter15a]).FPenaltyStructureType.FInitalised := True;
            end;
          end;
        end;

        if FAppModules.StudyArea.ModelVersion = '7' then
        begin
          if(LDataSet.DataSet.FieldByName('ChannelType').AsInteger in [21]) then
          begin
            LIdCounter15b  := LIdCounter15b + 1;
            if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
            begin
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FChannelNr.FData :=
                  LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FChannelNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('UpNodeNumber').IsNull then
            begin
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FDemandCentreNodeNr.FData :=
                  LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FDemandCentreNodeNr.FInitalised := True;

              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FUpstreamNodeNr.FData :=
                  LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FUpstreamNodeNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DownNodeNumber').IsNull then
            begin
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FDownStreamNodeNr.FData :=
                  LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FDownStreamNodeNr.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PenaltyNumber').IsNull then
            begin
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FPenaltyStructureType.FData :=
                  LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
              TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter15b]).FPenaltyStructureType.FInitalised := True;
            end;
          end;
        end;
        LDataSet.DataSet.Next;
      end;
      MergeIrrBlockData(LChannelDescr.FIrrigationBlockList);
      MergeWetlandData(LChannelDescr.FWetlandList);

      LDataSetP.DataSet.Close;
      LDataSetP.SetSQL(ReadDemandCentreSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;
      while not LDataSetP.DataSet.Eof do
      begin
        LIdCounter15 := LIdCounter15 + 1;
        if not LDataSetP.DataSet.FieldByName('NodeNumber').IsNull then
        begin
          TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNodeNumber.FData :=
            LDataSetP.DataSet.FieldByName('NodeNumber').AsInteger;
          TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNodeNumber.FInitalised := True;
        end;

        if not LDataSetP.DataSet.FieldByName('ConsumptiveChannelNr').IsNull then
        begin
          TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FConsumptiveChannelNr.FData :=
            LDataSetP.DataSet.FieldByName('ConsumptiveChannelNr').AsInteger;
          TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FConsumptiveChannelNr.FInitalised := True;
        end;

        TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNoOfReclaimationChannels.FData := 0;
        for LCount := 1 to LChannelDescr.FReclaimationChannelList.Count -1 do
        begin
          if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount]).FUpstreamNodeNr.FData =
              TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNodeNumber.FData then
          begin
            TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNoOfReclaimationChannels.FData :=
              TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNoOfReclaimationChannels.FData + 1;
            TDemandCentreObject(LChannelDescr.FDemandCentreList[LIdCounter15]).FNoOfReclaimationChannels.FInitalised := True;
          end;
        end;
        LDataSetP.DataSet.Next;
      end;
      LDataSetP.DataSet.Close;

     //line 16
      if FAppModules.StudyArea.ModelVersion = '7' then
      begin
        lDataSetGW.DataSet.Close;
        lDataSetGW.SetSQL(ReadGroundWaterSubCatchmentSQL);
        lDataSetGW.DataSet.Open;
        lDataSetGW.DataSet.First;
        while not lDataSetGW.DataSet.Eof do
        begin
          LIdCounter16  := LIdCounter16 + 1;
          if not lDataSetGW.DataSet.FieldByName('RefNodeNumber').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FReferenceNodeNr.FData :=
               lDataSetGW.DataSet.FieldByName('RefNodeNumber').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FReferenceNodeNr.FInitalised := True;
          end;

          if not lDataSetGW.DataSet.FieldByName('AquiferNodeNumber').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferNodeNumber.FData :=
               lDataSetGW.DataSet.FieldByName('AquiferNodeNumber').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferNodeNumber.FInitalised := True;
          end;

          if not lDataSetGW.DataSet.FieldByName('AbstractionNodeNumber').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionNodeNumber.FData :=
               lDataSetGW.DataSet.FieldByName('AbstractionNodeNumber').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionNodeNumber.FInitalised := True;
          end;

          if not lDataSetGW.DataSet.FieldByName('CollectionNodeNumber').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FCollectionNodeNumber.FData :=
               lDataSetGW.DataSet.FieldByName('CollectionNodeNumber').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FCollectionNodeNumber.FInitalised := True;
          end;

          if not lDataSetGW.DataSet.FieldByName('BaseFlowNodeNumber').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FBaseflowNodeNumber.FData :=
               lDataSetGW.DataSet.FieldByName('BaseFlowNodeNumber').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FBaseflowNodeNumber.FInitalised := True;
          end;

          if not lDataSetGW.DataSet.FieldByName('AquiferInflowChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferInflowChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('AquiferInflowChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferInflowChannelNr.FInitalised := True;

             lDataSetGWChannel.DataSet.Close;
             lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('AquiferInflowChannelNr').AsInteger));
             lDataSetGWChannel.DataSet.Open;
             if (lDataSetGWChannel.DataSet.RecordCount > 0) then
             begin
               if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
                begin
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferInflowPenaltyType.FData
                                     := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferInflowPenaltyType.FInitalised := True;
                end;
             end;
            lDataSetGWChannel.DataSet.Close;
          end;

          if not lDataSetGW.DataSet.FieldByName('AquferExcessChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferExcessInterflowChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('AquferExcessChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferExcessInterflowChannelNr.FInitalised := True;

             lDataSetGWChannel.DataSet.Close;
             lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('AquferExcessChannelNr').AsInteger));
             lDataSetGWChannel.DataSet.Open;
             if (lDataSetGWChannel.DataSet.RecordCount > 0) then
             begin
               if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
                begin
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferExcessInterflowPenaltyType.FData
                                     := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAquiferExcessInterflowPenaltyType.FInitalised := True;
                end;
             end;
            lDataSetGWChannel.DataSet.Close;
           end;

          if not lDataSetGW.DataSet.FieldByName('GroundWaterBaseFlowChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseflowChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('GroundWaterBaseFlowChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseflowChannelNr.FInitalised := True;

             lDataSetGWChannel.DataSet.Close;
             lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('GroundWaterBaseFlowChannelNr').AsInteger));
             lDataSetGWChannel.DataSet.Open;
             if (lDataSetGWChannel.DataSet.RecordCount > 0) then
             begin
               if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
                begin
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseflowPenaltyType.FData
                                     := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseflowPenaltyType.FInitalised := True;
                end;
             end;
            lDataSetGWChannel.DataSet.Close;
           end;

          if not lDataSetGW.DataSet.FieldByName('RegulationFromAquiferChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromAquiferChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('RegulationFromAquiferChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromAquiferChannelNr.FInitalised := True;

            lDataSetGWChannel.DataSet.Close;
            lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('RegulationFromAquiferChannelNr').AsInteger));
            lDataSetGWChannel.DataSet.Open;
            if (lDataSetGWChannel.DataSet.RecordCount > 0) then
            begin
              if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
               begin
                 TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromAquiferPenaltyType.FData
                                    := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                 TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromAquiferPenaltyType.FInitalised := True;
               end;
            end;
            lDataSetGWChannel.DataSet.Close;
           end;

          if not lDataSetGW.DataSet.FieldByName('RegulationFromBaseFlowChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromBaseflowChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('RegulationFromBaseFlowChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromBaseflowChannelNr.FInitalised := True;

            lDataSetGWChannel.DataSet.Close;
            lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('RegulationFromBaseFlowChannelNr').AsInteger));
            lDataSetGWChannel.DataSet.Open;
            if (lDataSetGWChannel.DataSet.RecordCount > 0) then
            begin
              if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
               begin
                 TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromBaseflowPenaltyType.FData
                                    := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                 TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FAbstractionFromBaseflowPenaltyType.FInitalised := True;
               end;
            end;
          lDataSetGWChannel.DataSet.Close;
          end;

          if not lDataSetGW.DataSet.FieldByName('BaseFlowRemainderChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseFlowRemainderChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('BaseFlowRemainderChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseFlowRemainderChannelNr.FInitalised := True;

            lDataSetGWChannel.DataSet.Close;
            lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('BaseFlowRemainderChannelNr').AsInteger));
            lDataSetGWChannel.DataSet.Open;
            if (lDataSetGWChannel.DataSet.RecordCount > 0) then
            begin
              if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
               begin
                 TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseFlowRemainderPenaltyType.FData
                                    := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                 TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FGroundWaterBaseFlowRemainderPenaltyType.FInitalised := True;
               end;
            end;
            lDataSetGWChannel.DataSet.Close;
          end;


          if not lDataSetGW.DataSet.FieldByName('SurfaceRunOffChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FSurfaceRunoffAndSoilInterflowChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('SurfaceRunOffChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised := True;

             lDataSetGWChannel.DataSet.Close;
             lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('SurfaceRunOffChannelNr').AsInteger));
             lDataSetGWChannel.DataSet.Open;
             if (lDataSetGWChannel.DataSet.RecordCount > 0) then
             begin
               if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
                begin
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FSurfaceRunoffAndSoilInterflowPenaltyType.FData
                                     := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FSurfaceRunoffAndSoilInterflowPenaltyType.FInitalised := True;
                end;
             end;
            lDataSetGWChannel.DataSet.Close;
           end;

          if not lDataSetGW.DataSet.FieldByName('UpstreamChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FInflowFromUpStreamAquiferChannelNr.FData :=
               lDataSetGW.DataSet.FieldByName('UpstreamChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FInflowFromUpStreamAquiferChannelNr.FInitalised := True;

             lDataSetGWChannel.DataSet.Close;
             lDataSetGWChannel.SetSQL(ReadChannelPenaltyNumberSQL(lDataSetGW.DataSet.FieldByName('UpstreamChannelNr').AsInteger));
             lDataSetGWChannel.DataSet.Open;
             if (lDataSetGWChannel.DataSet.RecordCount > 0) then
             begin
               if not lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').IsNull then
                begin
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FInflowFromUpStreamAquiferPenaltyType.FData
                                     := lDataSetGWChannel.DataSet.FieldByName('PenaltyNumber').AsInteger;
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FInflowFromUpStreamAquiferPenaltyType.FInitalised := True;
                end;
             end;
            lDataSetGWChannel.DataSet.Close;
          end;

          if not lDataSetGW.DataSet.FieldByName('DownstreamChannelNr').IsNull then
          begin
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FInflowFromUpStreamAquiferAquiferNumber.FData :=
               lDataSetGW.DataSet.FieldByName('DownstreamChannelNr').AsInteger;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter16]).FInflowFromUpStreamAquiferAquiferNumber.FInitalised := True;
          end;

          lDataSetGW.DataSet.Next;
        end;
        lDataSetGW.DataSet.Close;
      end;

      //line16 details +++++++++++++++++++++++++++++

      if  (FAppModules.Model.ModelName = CPlanning) then
        LDataSetP.SetSQL(ReadPlanningChannelOutputDataSQL)
      else
        LDataSetP.SetSQL(ReadChannelOutputDataSQL);
      LDataSetP.DataSet.Open;
      LDataSetP.DataSet.First;

      //Count the output channel again as there is an av in the planning model
      LCount := 0;
      while not LDataSetP.DataSet.Eof do
      begin
        LCount  := LCount + 1;
        LDataSetP.DataSet.Next;
      end;
      if(LCount > 0) then
      begin
        LChannelDescr.FSummaryChannelCount.FData := LCount;
        LChannelDescr.FSummaryChannelCount.FInitalised := True;
        LChannelDescr.FSummaryChannelList.Clear;
        LChannelDescr.AddSummaryChannels;
      end;

      LDataSetP.DataSet.First;
      while not LDataSetP.DataSet.Eof do
      begin
        LIdCounter13  := LIdCounter13 + 1;
        lSumChannelObj := TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LIdCounter13]);

        if not LDataSetP.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          lSumChannelObj.FChannelNumber.FData := lDataSetP.DataSet.FieldByName('ChannelNumber').AsInteger;
          lSumChannelObj.FChannelNumber.FInitalised := True;
        end;

        if (not LDataSetP.DataSet.FieldByName('FlowOutput').IsNull) and (UpperCase(Trim(LDataSetP.DataSet.FieldByName('FlowOutput').AsString)) = 'Y') then
          lSumChannelObj.FFlowOutput.FData := Trim(lDataSetP.DataSet.FieldByName('FlowOutput').AsString)
        else
          lSumChannelObj.FFlowOutput.FData := ' ';
        lSumChannelObj.FFlowOutput.FInitalised := True;

        if not lDataSetP.DataSet.FieldByName('OutputComment').IsNull then
        begin
          lSumChannelObj.FComment.FData := TrimRight(lDataSetP.DataSet.FieldByName('OutputComment').AsString);
          lSumChannelObj.FComment.FLength := Length(TrimRight(lDataSetP.DataSet.FieldByName('OutputComment').AsString));
          lSumChannelObj.FComment.FInitalised := True;
        end
        else
        if  (FAppModules.Model.ModelName = CPlanning) then
        begin
          lSumChannelObj.FComment.FData := Trim(lDataSetP.DataSet.FieldByName('ChannelNumber').AsString) + ' - ' + Trim(lDataSetP.DataSet.FieldByName('ChannelName').AsString);
          lSumChannelObj.FComment.FLength := Length(Trim(lDataSetP.DataSet.FieldByName('ChannelNumber').AsString) + ' - ' + Trim(lDataSetP.DataSet.FieldByName('ChannelName').AsString));
          lSumChannelObj.FComment.FInitalised := True;
        end;

        LDataSetP.DataSet.Next;
      end;
      LDataSetP.DataSet.Close;

      //line17 type on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF03UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      //check if there is unknown data
      LNoData := LNoData and (LDataSet.DataSet.RecordCount = 0);

      while not LDataSet.DataSet.Eof do
      begin
        ADataObject.FChannelDescrObject.FF03ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile03DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  not LNoData;
    finally
      LDataSetDC.Free;
      LDataSet.Free;
      LDataSetP.Free;
      lDataSetGW.Free;
      lDataSetGWChannel.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.UpdateChannelNames(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNames';
begin
  Result := False;
  try
    Result := UpdateChannelNamesFromOutputChannel(ADataObject);
    Result := Result and UpdateChannelNamesFromMinFlowLoss(ADataObject);
    Result := Result and UpdateChannelNamesFromMinMax(ADataObject);
    Result := Result and UpdateChannelNamesFromPowerMasterControl(ADataObject);
    Result := Result and UpdateChannelNamesFromDiversionFeatures(ADataObject);
    Result := Result and UpdateChannelNamesFromIrrigationBlock(ADataObject);
    if(FAppModules.Model.ModelName = CPlanning) then
      Result := Result and OverrideChannelNamesFromOutputChannel(ADataObject);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.OverrideChannelNamesFromOutputChannel(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.OverrideChannelNamesFromOutputChannel';
var
  LCount: integer;
  LChannelDescr       : TChannelDescrObject;
  LChannelName        : string;
  LMasterChannel      : TMasterChannelObject;
  LPowerChannels      : TPowerChannelObject;
  LIrrigationChannel  : TIrrigationChannelObject;
  LDiversionChannel   : TDiversionChannelObject;
  LMinFlowChannel     : TMinFlowChannelObject;
  LLossChannel        : TLossChannelObject;
  LMultiPurposeChannel: TMultiPurposeChannelObject;
  LPumpingChannel     : TPumpingChannelObject;
  LInflowChannel      : TInflowChannelObject;
  LDemandChannel      : TDemandChannelObject;
  LGeneralChannel     : TGeneralChannelObject;
  LSummaryChannel     : TSummaryChannelObject;
begin
  Result := False;
  try
    LChannelDescr := ADataObject.FChannelDescrObject;
    // Update channel names from output channel
    for LCount := 0 to LChannelDescr.FSummaryChannelList.Count - 1 do
    begin
      LSummaryChannel := TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]);
      LChannelName := LSummaryChannel.FChannelName.FData;
      
      LMasterChannel  := LChannelDescr.FindMasterChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LMasterChannel) then
      begin
        LMasterChannel.FChannelName.FData := LChannelName;
        LMasterChannel.FChannelName.FInitalised := True;
        LMasterChannel.FChannelName.FLength  := Length(LMasterChannel.FChannelName.FData);
      end;

      LPowerChannels  := LChannelDescr.FindPowerChannels(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LPowerChannels) then
      begin
        LPowerChannels.FChannelName.FData := LChannelName;
        LPowerChannels.FChannelName.FInitalised := True;
        LPowerChannels.FChannelName.FLength  := Length(LPowerChannels.FChannelName.FData);
      end;

      LIrrigationChannel := LChannelDescr.FindIrrigationChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LIrrigationChannel) then
      begin
        LIrrigationChannel.FConsumptiveChannelName.FData := LChannelName;
        LIrrigationChannel.FConsumptiveChannelName.FInitalised := True;
        LIrrigationChannel.FConsumptiveChannelName.FLength  := Length(LIrrigationChannel.FConsumptiveChannelName.FData);
      end;

      LDiversionChannel := LChannelDescr.FindDiversionChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LDiversionChannel) then
      begin
        LDiversionChannel.FChannelName.FData := LChannelName;
        LDiversionChannel.FChannelName.FInitalised := True;
        LDiversionChannel.FChannelName.FLength  := Length(LDiversionChannel.FChannelName.FData);
      end;

      LMinFlowChannel := LChannelDescr.FindMinFlowChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LMinFlowChannel) then
      begin
        LMinFlowChannel.FChannelName.FData := LChannelName;
        LMinFlowChannel.FChannelName.FInitalised := True;
        LMinFlowChannel.FChannelName.FLength  := Length(LMinFlowChannel.FChannelName.FData);
      end;

      LLossChannel := LChannelDescr.FindLossChannels(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LLossChannel) then
      begin
        LLossChannel.FChannelName.FData := LChannelName;
        LLossChannel.FChannelName.FInitalised := True;
        LLossChannel.FChannelName.FLength  := Length(LLossChannel.FChannelName.FData);
      end;

      LMultiPurposeChannel := LChannelDescr.FindMultiPurposeChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LMultiPurposeChannel) then
      begin
        LMultiPurposeChannel.FChannelName.FData := LChannelName;
        LMultiPurposeChannel.FChannelName.FInitalised := True;
        LMultiPurposeChannel.FChannelName.FLength  := Length(LMultiPurposeChannel.FChannelName.FData);
      end;

      LPumpingChannel := LChannelDescr.FindPumpingChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LPumpingChannel) then
      begin
        LPumpingChannel.FChannelName.FData := LChannelName;
        LPumpingChannel.FChannelName.FInitalised := True;
        LPumpingChannel.FChannelName.FLength  := Length(LPumpingChannel.FChannelName.FData);
      end;

      LInflowChannel := LChannelDescr.FindInflowChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LInflowChannel) then
      begin
        LInflowChannel.FChannelName.FData := LChannelName;
        LInflowChannel.FChannelName.FInitalised := True;
        LInflowChannel.FChannelName.FLength  := Length(LInflowChannel.FChannelName.FData);
      end;

      LDemandChannel := LChannelDescr.FindDemandChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LDemandChannel) then
      begin
        LDemandChannel.FChannelName.FData := LChannelName;
        LDemandChannel.FChannelName.FInitalised := True;
        LDemandChannel.FChannelName.FLength  := Length(LDemandChannel.FChannelName.FData);
      end;

      LGeneralChannel := LChannelDescr.FindGeneralChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LGeneralChannel) then
      begin
        LGeneralChannel.FChannelName.FData := LChannelName;
        LGeneralChannel.FChannelName.FInitalised := True;
        LGeneralChannel.FChannelName.FLength  := Length(LGeneralChannel.FChannelName.FData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.UpdateChannelNamesFromOutputChannel(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNamesFromOutputChannel';
var
  LCount: integer;
  LChannelDescr       : TChannelDescrObject;
  LChannelName        : string;
  LMasterChannel      : TMasterChannelObject;
  LPowerChannels      : TPowerChannelObject;
  LIrrigationChannel  : TIrrigationChannelObject;
  LDiversionChannel   : TDiversionChannelObject;
  LMinFlowChannel     : TMinFlowChannelObject;
  LLossChannel        : TLossChannelObject;
  LMultiPurposeChannel: TMultiPurposeChannelObject;
  LPumpingChannel     : TPumpingChannelObject;
  LInflowChannel      : TInflowChannelObject;
  LDemandChannel      : TDemandChannelObject;
  LGeneralChannel     : TGeneralChannelObject;
  LSummaryChannel     : TSummaryChannelObject;
begin
  Result := False;
  try
    LChannelDescr := ADataObject.FChannelDescrObject;
    // Update channel names from output channel
    for LCount := 0 to LChannelDescr.FSummaryChannelList.Count - 1 do
    begin
      LSummaryChannel := TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]);
      if(FAppModules.Model.ModelName = CPlanning) then
        LChannelName := LSummaryChannel.FChannelName.FData
      else
        LChannelName := ExtractChannelNameFromComment(LSummaryChannel.FComment.FData);
      LMasterChannel  := LChannelDescr.FindMasterChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LMasterChannel) and (not LMasterChannel.FChannelName.FInitalised)  then
      begin
        LMasterChannel.FChannelName.FData := LChannelName;
        LMasterChannel.FChannelName.FInitalised := True;
        LMasterChannel.FChannelName.FLength  := Length(LMasterChannel.FChannelName.FData);
      end;

      LPowerChannels  := LChannelDescr.FindPowerChannels(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LPowerChannels) and (not LPowerChannels.FChannelName.FInitalised) then
      begin
        LPowerChannels.FChannelName.FData := LChannelName;
        LPowerChannels.FChannelName.FInitalised := True;
        LPowerChannels.FChannelName.FLength  := Length(LPowerChannels.FChannelName.FData);
      end;

      LIrrigationChannel := LChannelDescr.FindIrrigationChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LIrrigationChannel) and (not LIrrigationChannel.FConsumptiveChannelName.FInitalised) then
      begin
        LIrrigationChannel.FConsumptiveChannelName.FData := LChannelName;
        LIrrigationChannel.FConsumptiveChannelName.FInitalised := True;
        LIrrigationChannel.FConsumptiveChannelName.FLength  := Length(LIrrigationChannel.FConsumptiveChannelName.FData);
      end;

      LDiversionChannel := LChannelDescr.FindDiversionChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LDiversionChannel) and (not LDiversionChannel.FChannelName.FInitalised) then
      begin
        LDiversionChannel.FChannelName.FData := LChannelName;
        LDiversionChannel.FChannelName.FInitalised := True;
        LDiversionChannel.FChannelName.FLength  := Length(LDiversionChannel.FChannelName.FData);
      end;

      LMinFlowChannel := LChannelDescr.FindMinFlowChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LMinFlowChannel) and (not LMinFlowChannel.FChannelName.FInitalised) then
      begin
        LMinFlowChannel.FChannelName.FData := LChannelName;
        LMinFlowChannel.FChannelName.FInitalised := True;
        LMinFlowChannel.FChannelName.FLength  := Length(LMinFlowChannel.FChannelName.FData);
      end;

      LLossChannel := LChannelDescr.FindLossChannels(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LLossChannel) and (not LLossChannel.FChannelName.FInitalised) then
      begin
        LLossChannel.FChannelName.FData := LChannelName;
        LLossChannel.FChannelName.FInitalised := True;
        LLossChannel.FChannelName.FLength  := Length(LLossChannel.FChannelName.FData);
      end;

      LMultiPurposeChannel := LChannelDescr.FindMultiPurposeChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LMultiPurposeChannel) and (not LMultiPurposeChannel.FChannelName.FInitalised) then
      begin
        LMultiPurposeChannel.FChannelName.FData := LChannelName;
        LMultiPurposeChannel.FChannelName.FInitalised := True;
        LMultiPurposeChannel.FChannelName.FLength  := Length(LMultiPurposeChannel.FChannelName.FData);
      end;

      LPumpingChannel := LChannelDescr.FindPumpingChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LPumpingChannel) and (not LPumpingChannel.FChannelName.FInitalised) then
      begin
        LPumpingChannel.FChannelName.FData := LChannelName;
        LPumpingChannel.FChannelName.FInitalised := True;
        LPumpingChannel.FChannelName.FLength  := Length(LPumpingChannel.FChannelName.FData);
      end;

      LInflowChannel := LChannelDescr.FindInflowChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LInflowChannel) and (not LInflowChannel.FChannelName.FInitalised) then
      begin
        LInflowChannel.FChannelName.FData := LChannelName;
        LInflowChannel.FChannelName.FInitalised := True;
        LInflowChannel.FChannelName.FLength  := Length(LInflowChannel.FChannelName.FData);
      end;

      LDemandChannel := LChannelDescr.FindDemandChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LDemandChannel) and (not LDemandChannel.FChannelName.FInitalised) then
      begin
        LDemandChannel.FChannelName.FData := LChannelName;
        LDemandChannel.FChannelName.FInitalised := True;
        LDemandChannel.FChannelName.FLength  := Length(LDemandChannel.FChannelName.FData);
      end;

      LGeneralChannel := LChannelDescr.FindGeneralChannel(LSummaryChannel.FChannelNumber.FData);
      if Assigned(LGeneralChannel) and (not LGeneralChannel.FChannelName.FInitalised) then
      begin
        LGeneralChannel.FChannelName.FData := LChannelName;
        LGeneralChannel.FChannelName.FInitalised := True;
        LGeneralChannel.FChannelName.FLength  := Length(LGeneralChannel.FChannelName.FData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.UpdateChannelNamesFromMinFlowLoss(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNamesFromMinFlowLoss';
var
  //LIsModelPlanning    : boolean;
  LCount              : integer;
  LChannelDescr       : TChannelDescrObject;

  LMinFlowChannel     : TMinFlowChannelObject;
  LLossChannel        : TLossChannelObject;

  LMinFlowAndLossChannels :TMinFlowAndLossChannelObject;
  LMinFlowChannelWithName :TMinFlowChannel;
  LLossChannelWithName    :TLossChannel;
begin
  Result := False;
  try
    LChannelDescr           := ADataObject.FChannelDescrObject;
    LMinFlowAndLossChannels := ADataObject.FMinFlowChannelObject;
    //LIsModelPlanning        := (FAppModules.Model.ModelName = CPlanning);

    // Update channel names from minimum flow channel demands
    for LCount := 0 to LMinFlowAndLossChannels.FMinFlowChannelsLines.Count - 1 do
    begin
      LMinFlowChannelWithName := TMinFlowChannel(LMinFlowAndLossChannels.FMinFlowChannelsLines[LCount]);

      LMinFlowChannel := LChannelDescr.FindMinFlowChannel(LMinFlowChannelWithName.FMinFlowChannelNumber.FData);
      if Assigned(LMinFlowChannel) and LMinFlowChannelWithName.FMinFlowChannelName.FInitalised then
      begin
        //if LIsModelPlanning and LMinFlowChannel.FChannelName.FInitalised then Continue; //Channel name should be same as output channel name in F03.dat
        LMinFlowChannel.FChannelName.FData := LMinFlowChannelWithName.FMinFlowChannelName.FData;
        LMinFlowChannel.FChannelName.FInitalised := True;
        LMinFlowChannel.FChannelName.FLength  := Length(LMinFlowChannel.FChannelName.FData);
      end;
    end;

    // Update channel names from water loss channel demands
    for LCount := 0 to LMinFlowAndLossChannels.FLossChannelsLines.Count - 1 do
    begin
      LLossChannelWithName := TLossChannel(LMinFlowAndLossChannels.FLossChannelsLines[LCount]);

      LLossChannel := LChannelDescr.FindLossChannels(LLossChannelWithName.FLossChannelNumber.FData);
      if Assigned(LLossChannel) and LLossChannelWithName.FLossChannelName.FInitalised then
      begin
        //if LIsModelPlanning and LLossChannel.FChannelName.FInitalised then Continue; //Channel name should be same as output channel name in F03.dat
        LLossChannel.FChannelName.FData := LLossChannelWithName.FLossChannelName.FData;
        LLossChannel.FChannelName.FInitalised := True;
        LLossChannel.FChannelName.FLength  := Length(LLossChannel.FChannelName.FData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.UpdateChannelNamesFromMinMax(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNamesFromMinMax';
var
  //LIsModelPlanning    : boolean;
  LCount              : integer;
  LChannelDescr       : TChannelDescrObject;
  LMultiPurposeChannel: TMultiPurposeChannelObject;

  LChannelMinMax       :  TChannelMinMax;
  LChannelMinMaxObject :TChannelMinMaxObject;
begin
  Result := False;
  try
    LChannelDescr        := ADataObject.FChannelDescrObject;
    LChannelMinMaxObject := ADataObject.FChannelMinMaxObject;
    //LIsModelPlanning     := (FAppModules.Model.ModelName = CPlanning);

    // Update channel names from minimum flow channel demands
    for LCount := 0 to LChannelMinMaxObject.FChannelMinMaxContainer.Count - 1 do
    begin
      LChannelMinMax := TChannelMinMax(LChannelMinMaxObject.FChannelMinMaxContainer[LCount]);

      LMultiPurposeChannel := LChannelDescr.FindMultiPurposeChannel(LChannelMinMax.FChannelMinMaxNumber.FData);
      if Assigned(LMultiPurposeChannel) and LChannelMinMax.FChannelMinMaxName.FInitalised then
      begin
        //if LIsModelPlanning and LMultiPurposeChannel.FChannelName.FInitalised then Continue; //Channel name should be same as output channel name in F03.dat
        LMultiPurposeChannel.FChannelName.FData := LChannelMinMax.FChannelMinMaxName.FData;
        LMultiPurposeChannel.FChannelName.FInitalised := True;
        LMultiPurposeChannel.FChannelName.FLength  := Length(LMultiPurposeChannel.FChannelName.FData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.UpdateChannelNamesFromPowerMasterControl(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNamesFromPowerMasterControl';
var
  //LIsModelPlanning   : boolean;
  LCount             : integer;
  LChannelDescr      : TChannelDescrObject;
  LMasterChannel     : TMasterChannelObject;
  LPowerDemand       : TPowerDemand;
  LPowerDemandObject : TPowerDemandObject;
begin
  Result := False;
  try
    LChannelDescr      := ADataObject.FChannelDescrObject;
    LPowerDemandObject := ADataObject.FPowerDemandObject;
    //LIsModelPlanning   := (FAppModules.Model.ModelName = CPlanning);

    // Update channel names from minimum flow channel demands
    for LCount := 0 to LPowerDemandObject.FPowerDemandsLines.Count - 1 do
    begin
      LPowerDemand := TPowerDemand(LPowerDemandObject.FPowerDemandsLines[LCount]);

      LMasterChannel := LChannelDescr.FindMasterChannel(LPowerDemand.FChannelNumber.FData);
      if Assigned(LMasterChannel) and LPowerDemand.FFeatureName.FInitalised then
      begin
        //if LIsModelPlanning and LMasterChannel.FChannelName.FInitalised then Continue; //Channel name should be same as output channel name in F03.dat
        LMasterChannel.FChannelName.FData := LPowerDemand.FFeatureName.FData;
        LMasterChannel.FChannelName.FInitalised := True;
        LMasterChannel.FChannelName.FLength  := Length(LMasterChannel.FChannelName.FData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.UpdateChannelNamesFromDiversionFeatures(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNamesFromDiversionFeatures';
var
  LIsModelPlanning         : boolean;
  LCount                   : integer;
  LChannelDescr            : TChannelDescrObject;
  LDiversionChannelObject  :TDiversionChannelObject;

  LDivChannelDemandObject  : TDivChannelDemandObject;
  LDiversionChannelData12  : TDiversionChannelData12;
  LDiversionChannelData3   : TDiversionChannelData3;
  LDiversionChannelHeading : TDiversionChannelHeading;
begin
  Result := False;
  try
    LChannelDescr           := ADataObject.FChannelDescrObject;
    LDivChannelDemandObject := ADataObject.FDivChannelDemandObject;
    LIsModelPlanning        := (FAppModules.Model.ModelName = CPlanning);

    // Update channel names from minimum flow channel demands
    for LCount := 0 to LDivChannelDemandObject.DiversionChannelCount - 1 do
    begin
      LDiversionChannelHeading := nil;

      LDiversionChannelData12 := LDivChannelDemandObject.DiversionChannelData12[LCount];
      if Assigned(LDiversionChannelData12) then
        LDiversionChannelHeading := LDiversionChannelData12.FHeading
      else
      begin
        LDiversionChannelData3 := LDivChannelDemandObject.DiversionChannelData3[LCount];
        if Assigned(LDiversionChannelData3) then
          LDiversionChannelHeading := LDiversionChannelData3.FHeading;
      end;

      if not Assigned(LDiversionChannelHeading) then
       Continue;

      LDiversionChannelObject := LChannelDescr.FindDiversionChannel(LDiversionChannelHeading.FDivChannelNumber.FData);
      if Assigned(LDiversionChannelObject) and LDiversionChannelHeading.FDivChannelName.FInitalised then
      begin
        if LIsModelPlanning and LDiversionChannelObject.FChannelName.FInitalised then Continue; //Channel name should be same as output channel name in F03.dat
        LDiversionChannelObject.FChannelName.FData := LDiversionChannelHeading.FDivChannelName.FData;
        LDiversionChannelObject.FChannelName.FInitalised := True;
        LDiversionChannelObject.FChannelName.FLength  := Length(LDiversionChannelHeading.FDivChannelName.FData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile03DatabaseAgent.UpdateChannelNamesFromIrrigationBlock(ADataObject: TDataFileObjects): boolean;
const OPNAME = 'TFile03DatabaseAgent.UpdateChannelNamesFromIrrigationBlock';
var
  LCount                  : integer;
  LIrrigationAreaChannel  : TIrrigationChannelObject;
  LIrrigationBlockChannel : TIrrigationBlockChannelObject;
  LChannelDescr           : TChannelDescrObject;
begin
  Result := False;
  try
    LChannelDescr     := ADataObject.FChannelDescrObject;
    for LCount := 0 to LChannelDescr.FIrrigationChannelList.Count-1 do
    begin
      LIrrigationAreaChannel := TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]);
      if LIrrigationAreaChannel.FDiversionChannelNumber.FInitalised then
      begin
        LIrrigationAreaChannel.FDiversionChannelName.FData         := IntToStr(LIrrigationAreaChannel.FDiversionChannelNumber.FData)
                                                                      + ' - Diversion Channel';
        LIrrigationAreaChannel.FDiversionChannelName.FInitalised := True;
        LIrrigationAreaChannel.FDiversionChannelName.FLength     := Length(LIrrigationAreaChannel.FDiversionChannelName.FData);
      end;

      if LIrrigationAreaChannel.FReturnChannelNumber.FInitalised then
      begin
        LIrrigationAreaChannel.FReturnChannelName.FData         := IntToStr(LIrrigationAreaChannel.FReturnChannelNumber.FData)
                                                                      + ' - Returnflow Channel';
        LIrrigationAreaChannel.FReturnChannelName.FInitalised := True;
        LIrrigationAreaChannel.FReturnChannelName.FLength     := Length(LIrrigationAreaChannel.FReturnChannelName.FData);
      end;

      if LIrrigationAreaChannel.FConsumptiveChannelNumber.FInitalised then
      begin
        LIrrigationAreaChannel.FConsumptiveChannelName.FData         := IntToStr(LIrrigationAreaChannel.FConsumptiveChannelNumber.FData)
                                                                      + ' - Consumptive Channel';
        LIrrigationAreaChannel.FConsumptiveChannelName.FInitalised := True;
        LIrrigationAreaChannel.FConsumptiveChannelName.FLength     := Length(LIrrigationAreaChannel.FReturnChannelName.FData);
      end;
    end;
    
    for LCount := 0 to LChannelDescr.FIrrigationBlockList.Count-1 do
    begin
      LIrrigationBlockChannel := TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]);
      if LIrrigationBlockChannel.FAbstractChannelNr.FInitalised then
      begin
        LIrrigationBlockChannel.FAbstractChannelName.FData         := IntToStr(LIrrigationBlockChannel.FAbstractChannelNr.FData)
                                                                      + ' - Abstraction Channel';
        LIrrigationBlockChannel.FAbstractChannelName.FInitalised := True;
        LIrrigationBlockChannel.FAbstractChannelName.FLength     := Length(LIrrigationBlockChannel.FAbstractChannelName.FData);
      end;
      if LIrrigationBlockChannel.FReturnFlowChannelNr.FInitalised then
      begin
        LIrrigationBlockChannel.FReturnFlowChannelName.FData         := IntToStr(LIrrigationBlockChannel.FReturnFlowChannelNr.FData)
                                                                      + ' - Returnflow Channel';
        LIrrigationBlockChannel.FReturnFlowChannelName.FInitalised := True;
        LIrrigationBlockChannel.FReturnFlowChannelName.FLength     := Length(LIrrigationBlockChannel.FReturnFlowChannelName.FData);
      end;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile03DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile03DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage           : string;
  LFieldName         : string;
  LPenaltyName       : string;
  LCount             : integer;
  LChannelType       : integer;
  LChannelIdCounter  : integer;
  LGroundwaterId     : integer;
  LIdCounter         : integer;
  LArrayCount        : integer;
  lDataSetDC         : TAbstractModelDataset;
  LDataSetGW         : TAbstractModelDataset;
  LDataSetGWCatchment : TAbstractModelDataset;
  LDataSet           : TAbstractModelDataset;
  LSubDataSet        : TAbstractModelDataset;
  LChannelDescr      : TChannelDescrObject;
  LStop              : boolean;
  lSumChannelObj     : TSummaryChannelObject;
  lNaturalInflow     : TNaturalInflowChannelObject;
  lPumpChannelObj    : TPumpingChannelObject;
  lDemandChannelObj  : TDemandChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile03DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    UpdateChannelNames(ADataObject);

    LChannelDescr := ADataObject.FChannelDescrObject;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSetDC);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetGW);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetGWCatchment);
    try

      LChannelIdCounter := 0;

      //line1 and all counts lines comments ++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteChannelCommentsDataSQL);
      LDataSet.ClearQueryParams(prString);
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

      if LChannelDescr.FComment01.FInitalised then
        LDataSet.SetParams(['Comment01'], [LChannelDescr.FComment01.FData]);

      if LChannelDescr.FComment02.FInitalised then
        LDataSet.SetParams(['Comment02'], [LChannelDescr.FComment02.FData]);

      if LChannelDescr.FComment03.FInitalised then
        LDataSet.SetParams(['Comment03'], [LChannelDescr.FComment03.FData]);

      if LChannelDescr.FComment04.FInitalised then
        LDataSet.SetParams(['Comment04'], [LChannelDescr.FComment04.FData]);

      if LChannelDescr.FComment05.FInitalised then
        LDataSet.SetParams(['Comment05'], [LChannelDescr.FComment05.FData]);

      if LChannelDescr.FComment06.FInitalised then
        LDataSet.SetParams(['Comment06'], [LChannelDescr.FComment06.FData]);

      if LChannelDescr.FComment07.FInitalised then
        LDataSet.SetParams(['Comment07'], [LChannelDescr.FComment07.FData]);

      if LChannelDescr.FComment08.FInitalised then
        LDataSet.SetParams(['Comment08'], [LChannelDescr.FComment08.FData]);

      if LChannelDescr.FComment09.FInitalised then
        LDataSet.SetParams(['Comment09'], [LChannelDescr.FComment09.FData]);

      if LChannelDescr.FComment10.FInitalised then
        LDataSet.SetParams(['Comment10'], [LChannelDescr.FComment10.FData]);

      if LChannelDescr.FComment11.FInitalised then
        LDataSet.SetParams(['Comment11'], [LChannelDescr.FComment11.FData]);

      if LChannelDescr.FComment12.FInitalised then
        LDataSet.SetParams(['Comment12'], [LChannelDescr.FComment12.FData]);

      if LChannelDescr.FComment13.FInitalised then
        LDataSet.SetParams(['Comment13'], [LChannelDescr.FComment13.FData]);

      if LChannelDescr.FComment14.FInitalised then
        LDataSet.SetParams(['Comment14'], [LChannelDescr.FComment14.FData]);

      if LChannelDescr.FComment15.FInitalised then
        LDataSet.SetParams(['Comment15'], [LChannelDescr.FComment15.FData]);

      if LChannelDescr.FComment16.FInitalised then
        LDataSet.SetParams(['Comment16'], [LChannelDescr.FComment16.FData]);

      if LChannelDescr.FComment17.FInitalised then
        LDataSet.SetParams(['Comment17'], [LChannelDescr.FComment17.FData]);

     LDataSet.ExecSQL;



      //line1 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FPenaltyChannelList.Count - 1 do
      begin
        LSubDataSet.DataSet.Close;
        LSubDataSet.SetSQL(WriteChannelArcPenaltyDataSQL);

        LSubDataSet.ClearQueryParams();
        //LSubDataSet.ClearQueryParams(prFloat);
        LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

        if TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FPenaltyType.FInitalised then
          LSubDataSet.SetParams(['Identifier'], [IntToStr(
            TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FPenaltyType.FData)]);

        if TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FArcCount.FInitalised then
          LSubDataSet.SetParams(['ArcCount'], [IntToStr(
            TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FArcCount.FData)]);

        if TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FComment.FInitalised then
        begin
          LPenaltyName := Trim(TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FComment.FData);
          if (Pos('\', LPenaltyName) = 1) then
            LPenaltyName := Trim(Copy(LPenaltyName, 2, Length(lPenaltyName) - 1));
          LSubDataSet.SetParams(['PenaltyName'], [lPenaltyName]);
        end;

        for LArrayCount :=  MinArcs to MaxArcs do
        begin
          LFieldName := Format('%s%2.2d',['Penalty',LArrayCount]);
          if TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FArcPenalty[LArrayCount].FInitalised then
            LSubDataSet.SetParams([LFieldName], [FloatToStr(TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LIdCounter]).FArcPenalty[LArrayCount].FData)]);
        end;

        LSubDataSet.ExecSQL;
      end;

      //line2 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FMasterChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(2)]);

        TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FChannelName.FData]);

        if TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TMasterChannelObject(LChannelDescr.FMasterChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;
      end;

      //line3 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FPowerChannelList.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);
        // Power Channel
        LChannelIdCounter := LChannelIdCounter + 1;
        LDataSet.ClearQueryParams(prInt);
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(3)]);
        LDataSet.SetParams(['ChannelSubType'], [IntToStr(1)]);

        TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FChannelName.FData]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;

        // Spill Channel
        LChannelIdCounter := LChannelIdCounter + 1;
        LDataSet.ClearQueryParams(prInt);
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(3)]);
        LDataSet.SetParams(['ChannelSubType'], [IntToStr(2)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillChannelNumber.FInitalised then
        begin
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillChannelNumber.FData)]);
          LDataSet.SetParams(['ChannelName'], [TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillChannelName.FData]);
        end;

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillUpNodeNumber.FData)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillDownNodeNumber.FData)]);

        if TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LIdCounter]).FSpillPenaltyStructType.FData)]);

        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
      end;

      //line4 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FIrrigationChannelList.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        // Diversion Channel
        LDataSet.ClearQueryParams(prInt);
        LChannelIdCounter := LChannelIdCounter + 1;
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(4)]);
        LDataSet.SetParams(['ChannelSubType'], [IntToStr(1)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FDiversionChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FDiversionChannelNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FDiversionChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FDiversionChannelName.FData]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FUpstreamNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FUpstreamNodeNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationNodeNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationPenaltyStructType.FData)]);

        LDataSet.ExecSQL;

        // Consumptive Channel
        LDataSet.ClearQueryParams(prInt);
        LChannelIdCounter := LChannelIdCounter + 1;
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(4)]);
        LDataSet.SetParams(['ChannelSubType'], [IntToStr(2)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FConsumptiveChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FConsumptiveChannelNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FConsumptiveChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FConsumptiveChannelName.FData]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationNodeNumber.FData)]);

        LDataSet.SetParams(['DownNodeNumber'], [IntToStr(0)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FConsumptivePenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FConsumptivePenaltyStructType.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;

        // Return Flow Channel
        LDataSet.ClearQueryParams(prInt);
        LChannelIdCounter := LChannelIdCounter + 1;
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(4)]);
        LDataSet.SetParams(['ChannelSubType'], [IntToStr(3)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FReturnChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FReturnChannelNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FReturnChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FReturnChannelName.FData]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FIrrigationNodeNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FDownStreamNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FDownStreamNodeNumber.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FReturnPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FReturnPenaltyStructType.FData)]);

        if TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FRelaxationDemand.FInitalised then
          LDataSet.SetParams(['RelaxationDemand'], [IntToStr(
            TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LIdCounter]).FRelaxationDemand.FData)]);

        LDataSet.ExecSQL;
      end;

      //line5 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FDiversionChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(5)]);

        TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FChannelName.FData]);

        if TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;
      end;

      //line6 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FMinFlowChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(6)]);

        TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FChannelName.FData]);

        if TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;
      end;

      //line7 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FLossChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(7)]);

        TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FChannelName.FData]);

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FChannelType.FInitalised then
        begin
          if (TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FChannelType.FData = 1) then
            LDataSet.SetParams(['ChannelType'], [IntToStr(5)]); //Loss type 1 = diversion feature
        end;

        if TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TLossChannelObject(LChannelDescr.FLossChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;
      end;

      //line8 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FMultiPurposeChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FChannelType)]);

        TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FChannelName.FData]);

        if TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;
      end;

      //line9 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FPumpingChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;
        lPumpChannelObj := TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LIdCounter]);

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);
        LDataSet.ClearQueryParams(prInt);
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(9)]);

        lPumpChannelObj.FIdentifier.FData := LChannelIdCounter;
        lPumpChannelObj.FIdentifier.FInitalised := True;

        if lPumpChannelObj.FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(lPumpChannelObj.FChannelNumber.FData)]);

        if lPumpChannelObj.FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [lPumpChannelObj.FChannelName.FData]);

        if lPumpChannelObj.FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(lPumpChannelObj.FUpNodeNumber.FData)]);

        if lPumpChannelObj.FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(lPumpChannelObj.FDownNodeNumber.FData)]);

        if lPumpChannelObj.FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(lPumpChannelObj.FPenaltyStructType.FData)]);

        if lPumpChannelObj.FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [lPumpChannelObj.FComment.FData]);

        LDataSet.ExecSQL;

        LSubDataSet.DataSet.Close;
        LSubDataSet.SetSQL(WritePumpingFeatureDataSQL);
        LSubDataSet.ClearQueryParams(prFloat);
        LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdCounter)]);

        if lPumpChannelObj.FComment.FInitalised then
          LSubDataSet.SetParams(['FeatureName'], [Trim(lPumpChannelObj.FComment.FData)]);

        if lPumpChannelObj.FPumpingHead.FInitalised then
          lSubDataSet.SetParams(['PumpingHead'], [FloatToStr(lPumpChannelObj.FPumpingHead.FData)]);

        if lPumpChannelObj.FEfficiency.FInitalised then
          lSubDataSet.SetParams(['PumpingEfficiency'], [FloatToStr(lPumpChannelObj.FEfficiency.FData)]);

        if lPumpChannelObj.FChannelNumber.FInitalised then
          lSubDataSet.SetParams(['ChannelNumber'], [IntToStr(lPumpChannelObj.FChannelNumber.FData)]);
        lSubDataSet.ExecSQL;
      end;

      //line10 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FInflowChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(10)]);

        TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FChannelName.FData]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;

        LSubDataSet.DataSet.Close;
        LSubDataSet.SetSQL(WriteSpecifiedInflowFeatureDataSQL);
        LSubDataSet.ClearQueryParams(prFloat);
        LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdCounter)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FComment.FInitalised then
          LSubDataSet.SetParams(['FeatureName'], [Trim(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FComment.FData)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LSubDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FInflowFileName.FInitalised then
          LSubDataSet.SetParams(['InflowFileName'], [Trim(
            TInflowChannelObject(LChannelDescr.FInflowChannelList[LIdCounter]).FInflowFileName.FData)]);

        lSubDataSet.ExecSQL;
      end;

      //line11 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FDemandChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;
        lDemandChannelObj := TDemandChannelObject(LChannelDescr.FDemandChannelList[LIdCounter]);

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);
        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(11)]);

        lDemandChannelObj.FIdentifier.FData := LChannelIdCounter;
        lDemandChannelObj.FIdentifier.FInitalised := True;

        if lDemandChannelObj.FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(lDemandChannelObj.FChannelNumber.FData)]);

        if lDemandChannelObj.FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [lDemandChannelObj.FChannelName.FData]);

        if lDemandChannelObj.FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(lDemandChannelObj.FUpNodeNumber.FData)]);

        if lDemandChannelObj.FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(lDemandChannelObj.FDownNodeNumber.FData)]);

        if lDemandChannelObj.FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(lDemandChannelObj.FPenaltyStructType.FData)]);

        if lDemandChannelObj.FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [lDemandChannelObj.FComment.FData]);

        LDataSet.ExecSQL;

        LSubDataSet.DataSet.Close;
        LSubDataSet.SetSQL(WriteSpecifiedDemandFeatureDataSQL);
        LSubDataSet.ClearQueryParams(prFloat);
        LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdCounter)]);

        if lDemandChannelObj.FComment.FInitalised then
          LSubDataSet.SetParams(['FeatureName'], [Trim(lDemandChannelObj.FComment.FData)]);

        if lDemandChannelObj.FGaugeNumber.FInitalised then
          lSubDataSet.SetParams(['GaugeNumber'], [IntToStr(lDemandChannelObj.FGaugeNumber.FData)]);

        if lDemandChannelObj.FStochastic.FInitalised then
          lSubDataSet.SetParams(['Stochastic'], [lDemandChannelObj.FStochastic.FData]);

        if lDemandChannelObj.FFullname.FInitalised then
          lSubDataSet.SetParams(['Fullname'], [lDemandChannelObj.FFullname.FData]);

        if lDemandChannelObj.FChannelNumber.FInitalised then
          lSubDataSet.SetParams(['ChannelNumber'], [IntToStr(lDemandChannelObj.FChannelNumber.FData)]);
        lSubDataSet.ExecSQL;
      end;

      //line12 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FGeneralChannelList.Count - 1 do
      begin
        LChannelIdCounter := LChannelIdCounter + 1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelDetailsDataSQL);

        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
        LDataSet.SetParams(['ChannelType'], [IntToStr(12)]);

        TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
        TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FIdentifier.FInitalised := True;

        if TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FChannelNumber.FData)]);

        if TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FChannelName.FInitalised then
          LDataSet.SetParams(['ChannelName'], [TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FChannelName.FData]);

        if TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FUpNodeNumber.FInitalised then
          LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FUpNodeNumber.FData)]);

        if TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FDownNodeNumber.FInitalised then
          LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FDownNodeNumber.FData)]);

        if TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FPenaltyStructType.FInitalised then
          LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FPenaltyStructType.FData)]);

        if TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FComment.FInitalised then
          LDataSet.SetParams(['Comment'], [
            TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LIdCounter]).FComment.FData]);

        LDataSet.ExecSQL;
      end;

      //line13a version 7 details +++++++++++++++++++++++++++++
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        for LIdCounter := 1 to LChannelDescr.FIrrigationBlockList.Count -1 do
        begin
          LChannelIdCounter := LChannelIdCounter + 1;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelDetailsDataSQL);

          LDataSet.ClearQueryParams(prInt);

          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
          LDataSet.SetParams(['ChannelType'], [IntToStr(14)]);

          TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FIdentifier.FData       := LChannelIdCounter;
          TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FIdentifier.FInitalised := True;
          //UpNodeNumber
          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FAbstractChannelName.FInitalised then
            LDataSet.SetParams(['ChannelName'], [TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FAbstractChannelName.FData]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FAbstractChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FAbstractChannelNr.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FUpNodeNumber.FInitalised then
            LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FUpNodeNumber.FData)]);
          //the down node number should equal the block number
          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FBlockNumber.FInitalised then
            LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FBlockNumber.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FAbstractPenaltyType.FInitalised then
            LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FAbstractPenaltyType.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FComment.FInitalised then
            LDataSet.SetParams(['Comment'], [
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FComment.FData]);

          LDataSet.ExecSQL;

          //DownNodeNumber
          LChannelIdCounter := LChannelIdCounter + 1;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelDetailsDataSQL);
          LDataSet.ClearQueryParams(prInt);
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
          LDataSet.SetParams(['ChannelType'], [IntToStr(15)]);

          TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FIdentifier.FData       := LChannelIdCounter;
          TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FIdentifier.FInitalised := True;

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FReturnFlowChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelName'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FReturnFlowChannelNr.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FReturnFlowChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FReturnFlowChannelNr.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FBlockNumber.FInitalised then
            LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FBlockNumber.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FDownNodeNumber.FInitalised then
            LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FDownNodeNumber.FData)]);

          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FReturnFlowPenaltyType.FInitalised then
            LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
              TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LIdCounter]).FReturnFlowPenaltyType.FData)]);

          LDataSet.ExecSQL;
        end;
      end;

      //line14a version 7 details +++++++++++++++++++++++++++++
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        for LIdCounter := 1 to LChannelDescr.FWetlandList.Count-1 do
        begin
          LChannelIdCounter := LChannelIdCounter + 1;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelDetailsDataSQL);

          LDataSet.ClearQueryParams(prInt);
          // inflow
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
          LDataSet.SetParams(['ChannelType'], [IntToStr(16)]);

          TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FIdentifier.FData       := LChannelIdCounter;
          TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FIdentifier.FInitalised := True;

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FInflowChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelName'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FInflowChannelNr.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FInflowChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FInflowChannelNr.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FUpNodeNumber.FInitalised then
            LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FUpNodeNumber.FData)]);
          //the down node number should equal the block number
          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FNodeNumber.FInitalised then
            LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FNodeNumber.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FInflowPenaltyType.FInitalised then
            LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FInflowPenaltyType.FData)]);

          LDataSet.ExecSQL;

          //Outflow
          LChannelIdCounter := LChannelIdCounter + 1;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelDetailsDataSQL);
          LDataSet.ClearQueryParams(prInt);
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
          LDataSet.SetParams(['ChannelType'], [IntToStr(17)]);

          TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FIdentifier.FData       := LChannelIdCounter;
          TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FIdentifier.FInitalised := True;

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FOutflowChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelName'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FOutflowChannelNr.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FOutflowChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FOutflowChannelNr.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FNodeNumber.FInitalised then
            LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FNodeNumber.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FDownNodeNumber.FInitalised then
            LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FDownNodeNumber.FData)]);

          if TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FOutflowPenaltyType.FInitalised then
            LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
              TWetlandChannelObject(LChannelDescr.FWetlandList[LIdCounter]).FOutflowPenaltyType.FData)]);

          LDataSet.ExecSQL;
        end;
      end;

      //line15b version 7 details +++++++++++++++++++++++++++++
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        for LIdCounter := 1 to LChannelDescr.FReturnFLowChannelList.Count-1 do
        begin
          LChannelIdCounter := LChannelIdCounter + 1;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelDetailsDataSQL);

          LDataSet.ClearQueryParams(prInt);
          // Return flow
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
          LDataSet.SetParams(['ChannelType'], [IntToStr(20)]);

          TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FIdentifier.FData       := LChannelIdCounter;
          TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FIdentifier.FInitalised := True;

          if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelName'], [IntToStr(
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FChannelNr.FData)]);

          if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FChannelNr.FInitalised then
            LDataSet.SetParams(['ChannelNumber'], [IntToStr(
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FChannelNr.FData)]);

          if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FDemandCentreNodeNr.FInitalised then
            LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FDemandCentreNodeNr.FData)]);

          if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FDownStreamNodeNr.FInitalised then
            LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FDownStreamNodeNr.FData)]);

          if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FPenaltyStructureType.FInitalised then
            LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
              TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LIdCounter]).FPenaltyStructureType.FData)]);

          LDataSet.ExecSQL;
        end;

        // Line 15c
        for LIdCounter := 1 to LChannelDescr.FReclaimationChannelList.Count - 1 do
        begin
          LChannelIdCounter := LChannelIdCounter + 1;
          begin
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(WriteChannelDetailsDataSQL);

            LDataSet.ClearQueryParams(prInt);
            // Reclaimation channel
            LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
            LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
            LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);
            LDataSet.SetParams(['Identifier'],    [IntToStr(LChannelIdCounter)]);
            LDataSet.SetParams(['ChannelType'],   [IntToStr(21)]);

            TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FIdentifier.FData := LChannelIdCounter;
            TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FIdentifier.FInitalised := True;

            if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FChannelNr.FInitalised then
              LDataSet.SetParams(['ChannelName'], [IntToStr(
                TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FChannelNr.FData)]);

            if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FChannelNr.FInitalised then
              LDataSet.SetParams(['ChannelNumber'], [IntToStr(
                TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FChannelNr.FData)]);

            if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FDemandCentreNodeNr.FInitalised then
              LDataSet.SetParams(['UpNodeNumber'], [IntToStr(
                TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FDemandCentreNodeNr.FData)]);

            if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FDownStreamNodeNr.FInitalised then
              LDataSet.SetParams(['DownNodeNumber'], [IntToStr(
                TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FDownStreamNodeNr.FData)]);

            if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FPenaltyStructureType.FInitalised then
              LDataSet.SetParams(['PenaltyNumber'], [IntToStr(
                TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LIdCounter]).FPenaltyStructureType.FData)]);
            LDataSet.ExecSQL;
          end;
        end;
      end;

      //line16 details +++++++++++++++++++++++++++++
      //Groundwater subcatchment
      LGroundwaterId := 0;
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        for LIdCounter := 1 to LChannelDescr.FGroundWaterList.Count -1 do
        begin
          LGroundwaterId := LGroundwaterId + 1;
          lDataSetDC.DataSet.Close;
          lDataSetDC.ClearQueryParams();
          lDataSetDC.SetSQL(WriteGroundWaterSubCatchmentSQL);

          lDataSetDC.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          lDataSetDC.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          lDataSetDC.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          lDataSetDC.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          lDataSetDC.SetParams(['GroundWaterIdentifier'], [IntToStr(LGroundwaterId)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FReferenceNodeNr.FInitalised then
             lDataSetDC.SetParams(['RefNodeNumber'],
                                 [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                                 FReferenceNodeNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FInitalised then
            lDataSetDC.SetParams(['AquiferNodeNumber'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FAquiferNodeNumber.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FInitalised then
             lDataSetDC.SetParams(['AbstractionNodeNumber'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FAbstractionNodeNumber.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FInitalised then
            lDataSetDC.SetParams(['CollectionNodeNumber'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FCollectionNodeNumber.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FBaseflowNodeNumber.FInitalised then
            lDataSetDC.SetParams(['BaseFlowNodeNumber'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FBaseflowNodeNumber.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowChannelNr.FInitalised then
            lDataSetDC.SetParams(['AquiferInflowChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FAquiferInflowChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowChannelNr.FInitalised then
            lDataSetDC.SetParams(['AquferExcessChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FAquiferExcessInterflowChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowChannelNr.FInitalised then
            lDataSetDC.SetParams(['GroundWaterBaseFlowChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FGroundWaterBaseflowChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferChannelNr.FInitalised then
            lDataSetDC.SetParams(['RegulationFromAquiferChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FAbstractionFromAquiferChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowChannelNr.FInitalised then
            lDataSetDC.SetParams(['RegulationFromBaseFlowChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FAbstractionFromBaseflowChannelNr.FData)]);


          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderChannelNr.FInitalised then
            lDataSetDC.SetParams(['BaseFlowRemainderChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FGroundWaterBaseFlowRemainderChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpstreamAquiferChannelNr.FInitalised then
            lDataSetDC.SetParams(['UpstreamChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FInflowFromUpstreamAquiferChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised then
            lDataSetDC.SetParams(['SurfaceRunOffChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FSurfaceRunoffAndSoilInterflowChannelNr.FData)]);

          if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferAquiferNumber.FInitalised then
            lDataSetDC.SetParams(['DownstreamChannelNr'],
                               [IntToStr(TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).
                               FInflowFromUpStreamAquiferAquiferNumber.FData)]);
          lDataSetDC.ExecSQL;
          lDataSetDC.Dataset.Close;

          for LCount := 1 to 8 do
          begin
            LChannelType := 24 + LCount;
            LChannelIdCounter := LChannelIdCounter + 1;
            LDataSetGWCatchment.DataSet.Close;
            LDataSetGWCatchment.SetSQL(WriteChannelDetailsDataSQL);

            LDataSetGWCatchment.ClearQueryParams(prInt);

            LDataSetGWCatchment.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LDataSetGWCatchment.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSetGWCatchment.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LDataSetGWCatchment.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LDataSetGWCatchment.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
            LDataSetGWCatchment.SetParams(['ChannelType'], [IntToStr(LChannelType)]);

            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FIdentifier.FData := LIdCounter;
            TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FIdentifier.FInitalised := True;

            case LChannelType of
              25 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowChannelNr.FInitalised then
                LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferInflowPenaltyType.FData)]);
              end;

              26 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowChannelNr.FInitalised then
                LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['UpNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferExcessInterflowPenaltyType.FData)]);
              end;

              27 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['UpNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FBaseflowNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FBaseflowNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseflowPenaltyType.FData)]);
              end;

              28 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferChannelNr.FInitalised then
                LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['UpNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromAquiferPenaltyType.FData)]);
              end;

              29 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FBaseflowNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['UpNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FBaseflowNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionFromBaseflowPenaltyType.FData)]);
              end;

              30 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['UpNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAquiferNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FAbstractionNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FGroundWaterBaseFlowRemainderPenaltyType.FData)]);
              end;

              31 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FSurfaceRunoffAndSoilInterflowPenaltyType.FData)]);
              end;

              32 :
              begin
                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelName'], [IntToStr(
                  TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferChannelNr.FInitalised then
                  LDataSetGWCatchment.SetParams(['ChannelNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferChannelNr.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferAquiferNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['UpNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferAquiferNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FInitalised then
                  LDataSetGWCatchment.SetParams(['DownNodeNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FCollectionNodeNumber.FData)]);

                if TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferPenaltyType.FInitalised then
                  LDataSetGWCatchment.SetParams(['PenaltyNumber'], [IntToStr(
                    TGroundWaterObject(LChannelDescr.FGroundWaterList[LIdCounter]).FInflowFromUpStreamAquiferPenaltyType.FData)]);
              end;
            end;
             LDataSetGWCatchment.ExecSQL;
          end;
        end;
      end;

      //line13 details +++++++++++++++++++++++++++++
      for LIdCounter := 1 to LChannelDescr.FSummaryChannelList.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelOutputDataSQL);
        LDataSet.ClearQueryParams(prInt);

        lSumChannelObj := TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LIdCounter]);
        LDataSet.SetParams(['ChannelNumber'], [IntToStr(lSumChannelObj.FChannelNumber.FData)]);
        LDataSet.SetParams(['OutputComment'], [lSumChannelObj.FComment.FData]);
        LDataSet.SetParams(['SummaryOutput'], ['Y']);
        LDataSet.SetParams(['FirmYieldCalc'], [lSumChannelObj.FCalculateFirmYield.FData]);
        LDataSet.SetParams(['FlowOutput'],    [lSumChannelObj.FFlowOutput.FData]);
        LDataSet.ExecSQL;
      end;

      if  (FAppModules.Model.ModelName = CPlanning) then
      begin
        for LIdCounter := 1 to LChannelDescr.FSummaryChannelCount.FData do
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WritePlanningChannelOutputDataSQL);
          LDataSet.ClearQueryParams(prInt);
          lSumChannelObj := TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LIdCounter]);
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LIdCounter)]);
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(lSumChannelObj.FChannelNumber.FData)]);

          LDataSet.ExecSQL;
        end;

        for LIdCounter := 0 to LChannelDescr.FNaturalInflowChannelList.Count -1 do
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelDetailsDataSQL);
          LDataSet.ClearQueryParams(prInt);
          LChannelIdCounter := LChannelIdCounter + 1;

          lNaturalInflow := TNaturalInflowChannelObject(LChannelDescr.FNaturalInflowChannelList[LIdCounter]);

          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdCounter)]);
          LDataSet.SetParams(['ChannelType'], [IntToStr(36)]);


          if lNaturalInflow.FChannelNumber.FInitalised then
            LDataSet.SetParams(['ChannelNumber'], [IntToStr(lNaturalInflow.FChannelNumber.FData)]);

          if lNaturalInflow.FChannelName.FInitalised then
            LDataSet.SetParams(['ChannelName'], [lNaturalInflow.FChannelName.FData]);

          if lNaturalInflow.FDownNodeNumber.FInitalised then
            LDataSet.SetParams(['DownNodeNumber'], [IntToStr(lNaturalInflow.FDownNodeNumber.FData)]);

          if lNaturalInflow.FPenaltyStructType.FInitalised then
            LDataSet.SetParams(['PenaltyNumber'], [IntToStr(lNaturalInflow.FPenaltyStructType.FData)]);

          LDataSet.ExecSQL;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelConfigSQL);
        LDataSet.ClearQueryParams(prInt);

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['InflowPenaltyNo'], [IntToStr(LChannelDescr.FInflowPenaltyNo.FData)]);

        LDataSet.ExecSQL;


      end;

      //line type 14 onwards++++++++++++++++++++++++++++
      for LCount := 0 to ADataObject.FChannelDescrObject.FF03ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteUnknownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [ADataObject.FChannelDescrObject.FF03ExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile03DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
      lDataSetDC.Free;
      LDataSetGW.Free;
      LDataSetGWCatchment.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile03DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'ChannelDetails,ChannelComments,ChannelArcPenalty,' +
                   'PumpingFeature,SpecifiedDemandFeature,'+
                   'SpecifiedInflowFeature,GroundWaterSubCatchment,ChannelArea,SummaryChannels,ChannelConfig';
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

procedure TFile03DatabaseAgent.MergeIrrBlockData(IrrBlockList: TObjectList);
const OPNAME = 'TFile03DatabaseAgent.MergeIrrBlockData';
var
  LCount,
  LFindCount,
  LIndex  : Integer;
begin
  try
    for LCount := IrrBlockList.Count - 1 downto 1 do
    begin
      if TIrrigationBlockChannelObject(IrrBlockList[LCount]).FChannelType.FData = 14 then
      begin
        LIndex := -1;
        for LFindCount := IrrBlockList.Count - 1 downto 1 do
        begin
          if TIrrigationBlockChannelObject(IrrBlockList[LFindCount]).FChannelType.FData = 15 then
            if (TIrrigationBlockChannelObject(IrrBlockList[LFindCount]).FBlockNumber.FData =
                                            TIrrigationBlockChannelObject(IrrBlockList[LCount]).FBlockNumber.FData) then
            begin
              LIndex := LFindCount;
              Break;
            end;
        end;

        if LIndex <> -1 then
        begin
          TIrrigationBlockChannelObject(IrrBlockList[LCount]).FDownNodeNumber.FData :=
                                                    TIrrigationBlockChannelObject(IrrBlockList[LIndex]).FDownNodeNumber.FData;
          TIrrigationBlockChannelObject(IrrBlockList[LCount]).FDownNodeNumber.FInitalised := True;

          TIrrigationBlockChannelObject(IrrBlockList[LCount]).FReturnFlowChannelNr.FData :=
                                                TIrrigationBlockChannelObject(IrrBlockList[LIndex]).FReturnFlowChannelNr.FData;
          TIrrigationBlockChannelObject(IrrBlockList[LCount]).FReturnFlowChannelNr.FInitalised := True;

          TIrrigationBlockChannelObject(IrrBlockList[LCount]).FReturnFlowPenaltyType.FData :=
                                               TIrrigationBlockChannelObject(IrrBlockList[LIndex]).FReturnFlowPenaltyType.FData;
          TIrrigationBlockChannelObject(IrrBlockList[LCount]).FReturnFlowPenaltyType.FInitalised := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFile03DatabaseAgent.MergeWetlandData(WetlandList: TObjectList);
const OPNAME = 'TFile03DatabaseAgent.MergeWetlandData';
var
  LCount,
  LFindCount,
  LIndex  : Integer;
begin
  try
    for LCount := WetlandList.Count - 1 downto 1 do
    begin
      if TWetlandChannelObject(WetlandList[LCount]).FChannelType.FData = 16 then
      begin
        LIndex := -1;
        for LFindCount := WetlandList.Count - 1 downto 1 do
        begin
          if TWetlandChannelObject(WetlandList[LFindCount]).FChannelType.FData = 17 then
            if (TWetlandChannelObject(WetlandList[LFindCount]).FNodeNumber.FData =
                                            TWetlandChannelObject(WetlandList[LCount]).FNodeNumber.FData) then
            begin
              LIndex := LFindCount;
              Break;
            end;
        end;

        if LIndex <> -1 then
        begin
          TWetlandChannelObject(WetlandList[LCount]).FDownNodeNumber.FData :=
                                                          TWetlandChannelObject(WetlandList[LIndex]).FDownNodeNumber.FData;
          TWetlandChannelObject(WetlandList[LCount]).FDownNodeNumber.FInitalised := True;

          TWetlandChannelObject(WetlandList[LCount]).FOutflowChannelNr.FData :=
                                                        TWetlandChannelObject(WetlandList[LIndex]).FOutflowChannelNr.FData;
          TWetlandChannelObject(WetlandList[LCount]).FOutflowChannelNr.FInitalised := True;

          TWetlandChannelObject(WetlandList[LCount]).FOutflowPenaltyType.FData :=
                                                      TWetlandChannelObject(WetlandList[LIndex]).FOutflowPenaltyType.FData;
          TWetlandChannelObject(WetlandList[LCount]).FOutflowPenaltyType.FInitalised := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
