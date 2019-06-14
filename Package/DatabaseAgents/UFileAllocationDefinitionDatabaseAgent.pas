//
//
//  UNIT      : Contains TFileAllocationDefinitionDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileAllocationDefinitionDatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects,
  UAllocationDefinitionFileDataObjects;

type

  TFileAllocationDefinitionDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadAllocationDefinitionSQL(AFileID:integer): string;
    function WriteAllocationDefinitionSQL: string;
    function ReadAllocationLevelSQL(AFileID:integer): string;
    function WriteAllocationLevelSQL: string;
    function ReadCoefficientsSQL(AFileID:integer): string;
    function WriteCoefficientsSQL: string;
    function ReadDemandDefSQL(AFileID:integer): string;
    function WriteDemandDefSQL: string;
    function ReadSolveFixedPositionSQL(AFileID:integer): string;
    function WriteSolveFixedPositionSQL: string;
    function ReadSolveSpecificOrderSQL(AFileID:integer): string;
    function WriteSolveSpecificOrderSQL: string;
    function ReadSubSystemSQL(AFileID:integer): string;
    function WriteSubSystemSQL: string;
    function ReadSupportChannelSQL(AFileID:integer): string;
    function WriteSupportChannelSQL: string;
    function ReadSupportSubSystemSQL(AFileID,ADemandDefID:integer): string;
    function WriteSupportSubSystemSQL: string;
    function ReadUserCategorySQL(AFileID:integer): string;
    function WriteUserCategorySQL: string;
    function ReadFMUnkownDataSQL(AFileID:integer): string;
    function WriteFMUnknownDataSQL: string;
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
     UFileNameConstants,
     UErrorHandlingOperations;

function TFileAllocationDefinitionDatabaseAgent.ReadFMUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadFMUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  '+IntToStr(AFileID)+
              ' AND FileGroup     =  '+IntToStr(fgAllocationDefinition)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteFMUnknownDataSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteFMUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadAllocationDefinitionSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadAllocationDefinitionSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, AllocDefName, AllocDefStartYear, AllocDefStartMonth, AllocDefEndYear,'+
              ' AllocDefEndMonth,FamilyFile, NrOfReliabilityClasses,NrOfLoadCases, NrOfStartStoragePercs,'+
              ' NrOfCurveSets, PeriodLength, ImplemntationDecision, SupportStrategy, BalancingOption,'+
              ' RIValue01, RIValue02, RIValue03, RIValue04, RIValue05,'+
              ' RILabel01, RILabel02, RILabel03, RILabel04, RILabel05,'+
              ' MonthCurveSet01, MonthCurveSet02, MonthCurveSet03, MonthCurveSet04, MonthCurveSet05,MonthCurveSet06,'+
              ' MonthCurveSet07, MonthCurveSet08, MonthCurveSet09, MonthCurveSet10, MonthCurveSet11, MonthCurveSet12,'+
              ' StartStoragePerc01, StartStoragePerc02, StartStoragePerc03, StartStoragePerc04, StartStoragePerc05,'+
              ' StartStoragePerc06, StartStoragePerc07, StartStoragePerc08, StartStoragePerc09, StartStoragePerc10'+
              ' FROM FMAllocationDefinition'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteAllocationDefinitionSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteAllocationDefinitionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMAllocationDefinition'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, AllocDefName, AllocDefStartYear, AllocDefStartMonth, AllocDefEndYear,'+
              ' AllocDefEndMonth,FamilyFile, NrOfReliabilityClasses,NrOfLoadCases, NrOfStartStoragePercs,'+
              ' NrOfCurveSets, PeriodLength, ImplemntationDecision, SupportStrategy, BalancingOption,'+
              ' RIValue01, RIValue02, RIValue03, RIValue04, RIValue05,'+
              ' RILabel01, RILabel02, RILabel03, RILabel04, RILabel05,'+
              ' MonthCurveSet01, MonthCurveSet02, MonthCurveSet03, MonthCurveSet04, MonthCurveSet05,MonthCurveSet06,'+
              ' MonthCurveSet07, MonthCurveSet08, MonthCurveSet09, MonthCurveSet10, MonthCurveSet11, MonthCurveSet12,'+
              ' StartStoragePerc01, StartStoragePerc02, StartStoragePerc03, StartStoragePerc04, StartStoragePerc05,'+
              ' StartStoragePerc06, StartStoragePerc07, StartStoragePerc08, StartStoragePerc09, StartStoragePerc10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :AllocDefName, :AllocDefStartYear, :AllocDefStartMonth, :AllocDefEndYear,'+
              ' :AllocDefEndMonth, :FamilyFile, :NrOfReliabilityClasses, :NrOfLoadCases, :NrOfStartStoragePercs,'+
              ' :NrOfCurveSets, :PeriodLength, :ImplemntationDecision, :SupportStrategy, :BalancingOption,'+
              ' :RIValue01, :RIValue02, :RIValue03, :RIValue04, :RIValue05,'+
              ' :RILabel01, :RILabel02, :RILabel03, :RILabel04, :RILabel05,'+
              ' :MonthCurveSet01, :MonthCurveSet02, :MonthCurveSet03, :MonthCurveSet04, :MonthCurveSet05, :MonthCurveSet06,'+
              ' :MonthCurveSet07, :MonthCurveSet08, :MonthCurveSet09, :MonthCurveSet10, :MonthCurveSet11, :MonthCurveSet12,'+
              ' :StartStoragePerc01, :StartStoragePerc02, :StartStoragePerc03, :StartStoragePerc04, :StartStoragePerc05,'+
              ' :StartStoragePerc06, :StartStoragePerc07, :StartStoragePerc08, :StartStoragePerc09, :StartStoragePerc10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadAllocationLevelSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadAllocationLevelSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,AllocDefID, AllocLevelID, AllocLevelName,'+
              ' Curtailment01, Curtailment02, Curtailment03, Curtailment04, Curtailment05, Comment'+
              ' FROM FMAllocationLevel'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, AllocLevelID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteAllocationLevelSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteAllocationLevelSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMAllocationLevel'+
              ' (Model,StudyAreaName,SubArea,Scenario,AllocDefID, AllocLevelID, AllocLevelName,'+
              ' Curtailment01, Curtailment02, Curtailment03, Curtailment04, Curtailment05, Comment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario, :AllocDefID, :AllocLevelID, :AllocLevelName,'+
              ' :Curtailment01, :Curtailment02, :Curtailment03, :Curtailment04, :Curtailment05, :Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadSupportChannelSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadSupportChannelSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, SupportChannelID, ChannelNumber,NrOfCntrlSubSystems,'+
              ' CntrlSubSystemID01, CntrlSubSystemID02, CntrlSubSystemID03, CntrlSubSystemID04, CntrlSubSystemID05,'+
              ' CntrlSubSystemID06, CntrlSubSystemID07, CntrlSubSystemID08, CntrlSubSystemID09, CntrlSubSystemID10,'+
              ' CntrlFactor01, CntrlFactor02, CntrlFactor03, CntrlFactor04, CntrlFactor05,'+
              ' CntrlFactor06, CntrlFactor07, CntrlFactor08, CntrlFactor09, CntrlFactor10 '+
              ' FROM FMSupportChannel'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, SupportChannelID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteSupportChannelSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteSupportChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMSupportChannel'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, SupportChannelID, ChannelNumber,NrOfCntrlSubSystems,'+
              ' CntrlSubSystemID01, CntrlSubSystemID02, CntrlSubSystemID03, CntrlSubSystemID04, CntrlSubSystemID05,'+
              ' CntrlSubSystemID06, CntrlSubSystemID07, CntrlSubSystemID08, CntrlSubSystemID09, CntrlSubSystemID10,'+
              ' CntrlFactor01, CntrlFactor02, CntrlFactor03, CntrlFactor04, CntrlFactor05,'+
              ' CntrlFactor06, CntrlFactor07, CntrlFactor08, CntrlFactor09, CntrlFactor10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :SupportChannelID, :ChannelNumber, :NrOfCntrlSubSystems,'+
              ' :CntrlSubSystemID01, :CntrlSubSystemID02, :CntrlSubSystemID03, :CntrlSubSystemID04, :CntrlSubSystemID05,'+
              ' :CntrlSubSystemID06, :CntrlSubSystemID07, :CntrlSubSystemID08, :CntrlSubSystemID09, :CntrlSubSystemID10,'+
              ' :CntrlFactor01, :CntrlFactor02, :CntrlFactor03, :CntrlFactor04, :CntrlFactor05,'+
              ' :CntrlFactor06, :CntrlFactor07, :CntrlFactor08, :CntrlFactor09, :CntrlFactor10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadUserCategorySQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadUserCategorySQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario, '+
              ' AllocDefID, UserCategoryID, UserCategoryName,'+
              ' Distribution01, Distribution02, Distribution03, Distribution04, Distribution05, Comment'+
              ' FROM FMUserCategory'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, UserCategoryID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteUserCategorySQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteUserCategorySQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMUserCategory'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, UserCategoryID, UserCategoryName,'+
              ' Distribution01, Distribution02, Distribution03, Distribution04, Distribution05, Comment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :UserCategoryID, :UserCategoryName,'+
              ' :Distribution01, :Distribution02, :Distribution03, :Distribution04, :Distribution05, :Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadCoefficientsSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadCoefficientsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario, '+
              ' AllocDefID, SubSystemID, StartStorageNr, CurveSetNr, LoadCaseNr, LoadCase,'+
              ' CoefficientA, CoefficientB, CoefficientC, CoefficientD, RiskProportion'+
              ' FROM FMCoefficients'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID,CurveSetNr,SubSystemID,StartStorageNr,LoadCaseNr';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteCoefficientsSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteCoefficientsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMCoefficients'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, SubSystemID, StartStorageNr, CurveSetNr, LoadCaseNr, LoadCase,'+
              ' CoefficientA, CoefficientB, CoefficientC, CoefficientD, RiskProportion)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :SubSystemID, :StartStorageNr, :CurveSetNr, :LoadCaseNr, :LoadCase,'+
              ' :CoefficientA, :CoefficientB, :CoefficientC, :CoefficientD, :RiskProportion)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadDemandDefSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadDemandDefSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario, '+
              ' AllocDefID, DemandDefID, DemandDefOrder,DemandDefName,DemandCentreID,'+
              ' ParentSubSystemID, GrowthType, TargetDemand, DCUserCategoryID, SupportArc1, SupportArc2,'+
              ' SupportSystemNr ,Comment '+
              ' FROM FMDemandDefinition'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,ParentSubSystemID,DemandDefOrder';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteDemandDefSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteDemandDefSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMDemandDefinition'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, DemandDefID, DemandDefOrder, DemandDefName, DemandCentreID,'+
              ' ParentSubSystemID, GrowthType, TargetDemand, DCUserCategoryID, SupportArc1, SupportArc2, SupportSystemNr, Comment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :DemandDefID, :DemandDefOrder, :DemandDefName, :DemandCentreID,'+
              ' :ParentSubSystemID, :GrowthType, :TargetDemand, :DCUserCategoryID, :SupportArc1, :SupportArc2, :SupportSystemNr, :Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadSolveFixedPositionSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadSolveFixedPositionSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,AllocDefID, FixedPositionID, FixedPositionNr, FixedPosSubSystemID '+
              ' FROM FMSolveFixedPosition'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, FixedPositionID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteSolveFixedPositionSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteSolveFixedPositionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMSolveFixedPosition'+
              ' (Model,StudyAreaName,SubArea,Scenario,AllocDefID, FixedPositionID, FixedPositionNr, FixedPosSubSystemID)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:AllocDefID, :FixedPositionID, :FixedPositionNr, :FixedPosSubSystemID)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadSolveSpecificOrderSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadSolveSpecificOrderSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario, AllocDefID, SpecificOrderID, BeforeSubSystemID, AfterSubSystemID'+
              ' FROM FMSolveSpecificOrder'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, SpecificOrderID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteSolveSpecificOrderSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteSolveSpecificOrderSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMSolveSpecificOrder'+
              ' (Model,StudyAreaName,SubArea,Scenario,AllocDefID, SpecificOrderID, BeforeSubSystemID, AfterSubSystemID)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:AllocDefID, :SpecificOrderID, :BeforeSubSystemID, :AfterSubSystemID)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadSubSystemSQL(AFileID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadSubSystemSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario, '+
              ' AllocDefID, SubSystemID, SubSystemName,SubSystemOrder, SubSystemStartYear, SubSystemStartMonth, '+
              ' SubSystemEndYear, SubSystemEndMonth, SubtractedSubSystemID, SupportingSubSystemID, '+
              ' SupportingChannelNr, ShortTermYield, LongTermYield, LowestStreamFlow, FirmYield, '+
              ' SupportCalcType, RoutingChannelNr01, RoutingChannelNr02, RoutingChannelNr03, '+
              ' RoutingChannelNr04,RoutingChannelNr05, SubSystemReservoirNrs '+
              ' FROM FMSubSystem'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID    =  '+IntToStr(AFileID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, SubSystemOrder';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteSubSystemSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteSubSystemSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMSubSystem'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, SubSystemID, SubSystemName, SubSystemOrder,SubSystemStartYear, SubSystemStartMonth, '+
              ' SubSystemEndYear, SubSystemEndMonth, SubtractedSubSystemID, SupportingSubSystemID, '+
              ' SupportingChannelNr, ShortTermYield, LongTermYield, LowestStreamFlow, FirmYield, '+
              ' SupportCalcType, RoutingChannelNr01, RoutingChannelNr02, RoutingChannelNr03, '+
              ' RoutingChannelNr04, RoutingChannelNr05, SubSystemReservoirNrs)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :SubSystemID, :SubSystemName, :SubSystemOrder, :SubSystemStartYear, :SubSystemStartMonth, '+
              ' :SubSystemEndYear, :SubSystemEndMonth, :SubtractedSubSystemID, :SupportingSubSystemID, '+
              ' :SupportingChannelNr, :ShortTermYield, :LongTermYield, :LowestStreamFlow, :FirmYield, '+
              ' :SupportCalcType, :RoutingChannelNr01, :RoutingChannelNr02, :RoutingChannelNr03, '+
              ' :RoutingChannelNr04, :RoutingChannelNr05, :SubSystemReservoirNrs)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadSupportSubSystemSQL(AFileID,ADemandDefID:integer): string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadSupportSubSystemSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario, '+
              ' AllocDefID, DemandDefID, SupportSubSystemID, SupSubSystemID, SupSubSysChannelNr01,'+
              ' SupSubSysChannelNr02, SupSubSysChannelNr03, SupSubSysChannelNr04, SupSubSysChannelNr05'+
              ' FROM FMSupportSubSystem'+
              ' WHERE Model        =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName  =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea        =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario       =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND AllocDefID     =  '+IntToStr(AFileID)+
              ' AND DemandDefID    =  '+IntToStr(ADemandDefID)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,AllocDefID, DemandDefID,SupportSubSystemID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteSupportSubSystemSQL: string;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteSupportSubSystemSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FMSupportSubSystem'+
              ' (Model,StudyAreaName,SubArea,Scenario,'+
              ' AllocDefID, DemandDefID, SupportSubSystemID, SupSubSystemID, SupSubSysChannelNr01,'+
              ' SupSubSysChannelNr02, SupSubSysChannelNr03, SupSubSysChannelNr04, SupSubSysChannelNr05)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,'+
              ' :AllocDefID, :DemandDefID, :SupportSubSystemID, :SupSubSystemID, :SupSubSysChannelNr01,'+
              ' :SupSubSysChannelNr02, :SupSubSysChannelNr03, :SupSubSysChannelNr04, :SupSubSysChannelNr05)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage       : string;
  LFieldName     : string;
  LDataSet       : TAbstractModelDataset;
  LDataSet2      : TAbstractModelDataset;
  LStop          : boolean;
  LCurveSetIndex,
  LSubSystemIndex,
  LStartStorageIndex,
  LLoadCaseIndex,
  LIndex,
  LIndex2         : integer;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LFamilyFileDataObject    : TAllocationDefinitionFileDataObject;
  LStartVolumePerc         : array[1..10] of double;
  LDistribution,
  LCurtailment             : TClassValuesArrayObject;
  LSupportChannel          : TSupportChannel;
  LFixedSubSystem          : TFixedSubSystem;
  LDemandSupportDefinition : TDemandSupportDefinition;
  LRoutingSupportChannelNumbers : TRoutingSupportChannelNumbers;
  LCoefficient             : TCoefficients;
  LNonFirmSubsystem        : TNonFirmSubsystem;
  LSystemFullPerNode       : TSystemFullPerNodes;
  LStringsContainer        : TStringList;
  LVolumeAndCoefficient    : TStartVolumeAndScenarioCoefficients;
  LScenarioCoefficient     : TScenarioCoefficients;
  LSubsystemList           : TSubsystemList;
  LSubsystem               : TSubsystem;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LFamilyFileDataObject    := LPlanningFileDataObject.AddAllocationDefinitionFileDataObject(AFilename.FileNumber);
    if(LFamilyFileDataObject = nil) then
      Exit;
    if not LFamilyFileDataObject.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet2);
    LStringsContainer := TStringList.Create;
    LSubsystemList    := TSubsystemList.Create(True);
    try

      LDataSet.SetSQL(ReadAllocationDefinitionSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin

        // go to the last record if there is more than one record.
        LDataSet.DataSet.Last;

        //line 1 +++++++++++++++++++++++++
          //line 1a +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('NrOfReliabilityClasses').IsNull then
        begin
          LFamilyFileDataObject.ClassCount.FData := LDataSet.DataSet.FieldByName('NrOfReliabilityClasses').AsInteger;
          LFamilyFileDataObject.ClassCount.FInitalised := True;
        end;

          //line 1d +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('PeriodLength').IsNull then
        begin
          LFamilyFileDataObject.PeriodLength.FData := LDataSet.DataSet.FieldByName('PeriodLength').AsInteger;
          LFamilyFileDataObject.PeriodLength.FInitalised := True;
        end;

        //line 1e +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('ImplemntationDecision').IsNull then
        begin
          LFamilyFileDataObject.ImplemntationDecision.FData := Trim(LDataSet.DataSet.FieldByName('ImplemntationDecision').AsString);
          LFamilyFileDataObject.ImplemntationDecision.FInitalised := True;
        end;

        //line 2 +++++++++++++++++++++++++
        for LIndex := 1 to 5 do
        begin
          LFieldName := Format('%s%2.2d',['RIValue',LIndex]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LFamilyFileDataObject.RIValueArray[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
            LFamilyFileDataObject.RIValueArray[LIndex].FInitalised := True;
          end;
        end;

        //line 3 +++++++++++++++++++++++++
        for LIndex := 1 to 5 do
        begin
          LFieldName := Format('%s%2.2d',['RILabel',LIndex]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LFamilyFileDataObject.RILabelArray[LIndex].FData := Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString);
            LFamilyFileDataObject.RILabelArray[LIndex].FInitalised := True;
          end;
        end;

        //line 8 +++++++++++++++++++++++++
          //line 8b +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('NrOfStartStoragePercs').IsNull then
        begin
          LFamilyFileDataObject.YieldCurveSetsCount.FData := LDataSet.DataSet.FieldByName('NrOfStartStoragePercs').AsInteger;
          LFamilyFileDataObject.YieldCurveSetsCount.FInitalised := True;
        end;
          //line 8c +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('NrOfLoadCases').IsNull then
        begin
          LFamilyFileDataObject.FamilyLoadCaseCount.FData := LDataSet.DataSet.FieldByName('NrOfLoadCases').AsInteger;
          LFamilyFileDataObject.FamilyLoadCaseCount.FInitalised := True;
        end;

          //line 8b +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('NrOfCurveSets').IsNull then
        begin
          LFamilyFileDataObject.CurveSetsPerMonthCount.FData := LDataSet.DataSet.FieldByName('NrOfCurveSets').AsInteger;
          LFamilyFileDataObject.CurveSetsPerMonthCount.FInitalised := True;
        end;

        //line 9 +++++++++++++++++++++++++
        for LIndex := 1 to 12 do
        begin
          LFieldName := Format('%s%2.2d',['MonthCurveSet',LIndex]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LFamilyFileDataObject.DecisionIndicatorArray[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
            LFamilyFileDataObject.DecisionIndicatorArray[LIndex].FInitalised := True;
          end;
        end;

        //line 12a +++++++++++++++++++++++++
        for LStartStorageIndex := 1 to 10 do
        begin
          LStartVolumePerc[LStartStorageIndex] := NullFloat;
          LFieldName := Format('%s%2.2d',['StartStoragePerc',LStartStorageIndex]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
           LStartVolumePerc[LStartStorageIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;

        //line 15 +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('SupportStrategy').IsNull then
        begin
          LFamilyFileDataObject.SupportStrategyType.FData := LDataSet.DataSet.FieldByName('SupportStrategy').AsInteger;
          LFamilyFileDataObject.SupportStrategyType.FInitalised := True;
        end;

        //line 22 +++++++++++++++++++++++++
        if not LDataSet.DataSet.FieldByName('BalancingOption').IsNull then
        begin
          LFamilyFileDataObject.BalancingOption.FData := LDataSet.DataSet.FieldByName('BalancingOption').AsInteger;
          LFamilyFileDataObject.BalancingOption.FInitalised := True;
        end;

        //line 4 +++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadAllocationLevelSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LCurtailment := LFamilyFileDataObject.AddCurtailment;
          for LIndex := 1 to 5 do
          begin
            LFieldName := Format('%s%2.2d',['Curtailment',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LCurtailment.ClassValuesArray[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LCurtailment.ClassValuesArray[LIndex].FInitalised := True;
            end;
          end;
          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            LCurtailment.Comment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            LCurtailment.Comment.FLength := Length(LCurtailment.Comment.FData);
            LCurtailment.Comment.FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;

        //line 5 +++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadUserCategorySQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDistribution := LFamilyFileDataObject.AddDistribution;
          for LIndex := 1 to 5 do
          begin
            LFieldName := Format('%s%2.2d',['Distribution',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LDistribution.ClassValuesArray[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LDistribution.ClassValuesArray[LIndex].FInitalised := True;
            end;
          end;
          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            LDistribution.Comment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);

            LDistribution.Comment.FLength := Length(LDistribution.Comment.FData);
            LDistribution.Comment.FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;

        //line 7 +++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadDemandDefSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDemandSupportDefinition := LFamilyFileDataObject.AddDemandSupportDefinition;

          LDemandSupportDefinition.SupportSubsystemsCount.FData := 1;
          LDemandSupportDefinition.SupportSubsystemsCount.FInitalised := True;

          if not LDataSet.DataSet.FieldByName('DemandDefName').IsNull then
          begin
            LDemandSupportDefinition.DemandDefName.FData := Trim(LDataSet.DataSet.FieldByName('DemandDefName').AsString);
            LDemandSupportDefinition.DemandDefName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DemandCentreID').IsNull then
          begin
            LDemandSupportDefinition.DemandCentreID.FData := LDataSet.DataSet.FieldByName('DemandCentreID').AsInteger;
            LDemandSupportDefinition.DemandCentreID.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ParentSubSystemID').IsNull then
          begin
            LDemandSupportDefinition.SubsystemNo1.FData := LDataSet.DataSet.FieldByName('ParentSubSystemID').AsInteger;
            LDemandSupportDefinition.SubsystemNo1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('GrowthType').IsNull then
          begin
            LDemandSupportDefinition.GrowthFlag.FData := LDataSet.DataSet.FieldByName('GrowthType').AsInteger;
            LDemandSupportDefinition.GrowthFlag.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TargetDemand').IsNull then
          begin
            LDemandSupportDefinition.TargetDemand.FData := LDataSet.DataSet.FieldByName('TargetDemand').AsFloat;
            LDemandSupportDefinition.TargetDemand.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DCUserCategoryID').IsNull then
          begin
            LDemandSupportDefinition.UserCategory.FData := LDataSet.DataSet.FieldByName('DCUserCategoryID').AsInteger;
            LDemandSupportDefinition.UserCategory.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SupportArc1').IsNull then
          begin
            LDemandSupportDefinition.SupportArc1.FData := LDataSet.DataSet.FieldByName('SupportArc1').AsInteger;
            LDemandSupportDefinition.SupportArc1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SupportArc2').IsNull then
          begin
            LDemandSupportDefinition.SupportArc2.FData := LDataSet.DataSet.FieldByName('SupportArc2').AsInteger;
            LDemandSupportDefinition.SupportArc2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SupportSystemNr').IsNull then
          begin
            LDemandSupportDefinition.SupportSystemNr.FData := LDataSet.DataSet.FieldByName('SupportSystemNr').AsInteger;
            LDemandSupportDefinition.SupportSystemNr.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment').IsNull then
          begin
            LDemandSupportDefinition.Comment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
            LDemandSupportDefinition.Comment.FLength := Length(LDemandSupportDefinition.Comment.FData);
            LDemandSupportDefinition.Comment.FInitalised := True;
          end;

          LDataSet2.DataSet.Close;
          LDataSet2.SetSQL(ReadSupportSubSystemSQL(AFilename.FileNumber,LDataSet.DataSet.FieldByName('DemandDefID').AsInteger));
          LDataSet2.DataSet.Open;
          LIndex := 1;
          while not LDataSet2.DataSet.Eof do
          begin
            LDemandSupportDefinition.SupportSubsystemsNumbers[LIndex].FData := LDataSet2.DataSet.FieldByName('SupSubSystemID').AsInteger;
            LDemandSupportDefinition.SupportSubsystemsNumbers[LIndex].FInitalised := True;
            LDemandSupportDefinition.SupportSubsystemsCount.FData := LDemandSupportDefinition.SupportSubsystemsCount.FData + 1;

            LRoutingSupportChannelNumbers := LDemandSupportDefinition.AddRoutingSupportChannelNumbers;
            for LIndex2 := 1 to 5 do
            begin
              LFieldName := Format('%s%2.2d',['SupSubSysChannelNr',LIndex2]);
              if not LDataSet2.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LRoutingSupportChannelNumbers.ChannelNumbers[LIndex2].FData := LDataSet2.DataSet.FieldByName(LFieldName).AsInteger;
                LRoutingSupportChannelNumbers.ChannelNumbers[LIndex2].FInitalised := True;
              end;
            end;
            LDataSet2.DataSet.Next;
            LIndex := LIndex + 1;
          end;
          LDataSet2.DataSet.Close;

          LDataSet.DataSet.Next;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadSubSystemSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          //line 10+++++++++++++++++++++++++
          LNonFirmSubsystem := LFamilyFileDataObject.AddNonFirmSubsystem;

          if not LDataSet.DataSet.FieldByName('SubSystemID').IsNull then
          begin
            LNonFirmSubsystem.NonFirmSubsystemNo.FData := LDataSet.DataSet.FieldByName('SubSystemID').AsInteger;
            LNonFirmSubsystem.NonFirmSubsystemNo.FInitalised := True;
          end;

          for LIndex := 1 to 5 do
          begin
            LFieldName := Format('%s%2.2d',['RoutingChannelNr',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LNonFirmSubsystem.RoutingChannelNoArray[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
              LNonFirmSubsystem.RoutingChannelNoArray[LIndex].FInitalised := True;
            end;
          end;

          if not LDataSet.DataSet.FieldByName('SupportCalcType').IsNull then
          begin
            LNonFirmSubsystem.RoutingChannelNoArray[5].FData:= LDataSet.DataSet.FieldByName('SupportCalcType').AsInteger;
            LNonFirmSubsystem.RoutingChannelNoArray[5].FInitalised := True;
          end;

          //line 14+++++++++++++++++++++++++
          LSystemFullPerNode := LFamilyFileDataObject.AddSystemFullPerNode;

          if not LDataSet.DataSet.FieldByName('SubSystemReservoirNrs').IsNull then
          begin
            LStringsContainer.CommaText := Trim(LDataSet.DataSet.FieldByName('SubSystemReservoirNrs').AsString);
            for LIndex := 0 to LStringsContainer.Count -1 do
            begin
              LSystemFullPerNode.NodeNoArray[LIndex+1].FData := StrToIntDef(LStringsContainer[LIndex],NullInteger);
              LSystemFullPerNode.NodeNoArray[LIndex+1].FInitalised        := (LSystemFullPerNode.NodeNoArray[LIndex+1].FData <> NullInteger);
            end;
          end;

          LSubsystem := LSubsystemList.AddSubsystem;
          //line 11+++++++++++++++++++++++++
          if not LDataSet.DataSet.FieldByName('SubSystemName').IsNull then
          begin
            LSubsystem.SubsystemName.FData := Trim(LDataSet.DataSet.FieldByName('SubSystemName').AsString);
            LSubsystem.SubsystemName.FInitalised := True;
            LSubsystem.SubsystemName.FLength := Length(Trim(LDataSet.DataSet.FieldByName('SubSystemName').AsString))+2;
          end;

          if not LDataSet.DataSet.FieldByName('SubSystemStartYear').IsNull then
          begin
            LSubsystem.FamilyCurveStartYear.FData := LDataSet.DataSet.FieldByName('SubSystemStartYear').AsInteger;
            LSubsystem.FamilyCurveStartYear.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SubSystemStartMonth').IsNull then
          begin
            LSubsystem.FamilyCurveStartMonth.FData := LDataSet.DataSet.FieldByName('SubSystemStartMonth').AsInteger;
            LSubsystem.FamilyCurveStartMonth.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SubSystemEndYear').IsNull then
          begin
            LSubsystem.FamilyCurveEndYear.FData := LDataSet.DataSet.FieldByName('SubSystemEndYear').AsInteger;
            LSubsystem.FamilyCurveEndYear.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SubSystemEndMonth').IsNull then
          begin
            LSubsystem.FamilyCurveEndMonth.FData := LDataSet.DataSet.FieldByName('SubSystemEndMonth').AsInteger;
            LSubsystem.FamilyCurveEndMonth.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SubtractedSubSystemID').IsNull then
          begin
            LSubsystem.SubsystemNo2.FData := LDataSet.DataSet.FieldByName('SubtractedSubSystemID').AsInteger;
            LSubsystem.SubsystemNo2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SupportingSubSystemID').IsNull then
          begin
            LSubsystem.SupportSubsystemNo.FData := LDataSet.DataSet.FieldByName('SupportingSubSystemID').AsInteger;
            LSubsystem.SupportSubsystemNo.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SupportingChannelNr').IsNull then
          begin
            LSubsystem.ChannelNo.FData := LDataSet.DataSet.FieldByName('SupportingChannelNr').AsInteger;
            LSubsystem.ChannelNo.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ShortTermYield').IsNull then
          begin
            LSubsystem.ShortTermFirmYield.FData := LDataSet.DataSet.FieldByName('ShortTermYield').AsFloat;
            LSubsystem.ShortTermFirmYield.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('LowestStreamFlow').IsNull then
          begin
            LSubsystem.StreamFlow.FData := LDataSet.DataSet.FieldByName('LowestStreamFlow').AsFloat;
            LSubsystem.StreamFlow.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('LongTermYield').IsNull then
          begin
            LSubsystem.LongTermFirmYield.FData := LDataSet.DataSet.FieldByName('LongTermYield').AsFloat;
            LSubsystem.LongTermFirmYield.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('FirmYield').IsNull then
          begin
            if (UpperCase(Trim(LDataSet.DataSet.FieldByName('FirmYield').AsString)) = 'Y')  then
              LSubsystem.FirmFlag.FData := 'FIRM'
            else
              LSubsystem.FirmFlag.FData := 'NON-FIRM';
            LSubsystem.FirmFlag.FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;


        LCurveSetIndex     := 0;
        LSubSystemIndex    := 0;
        LStartStorageIndex := 0;
        LLoadCaseIndex     := 0;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadCoefficientsSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
          //line 13+++++++++++++++++++++++++
        LCoefficient          := nil;
        LVolumeAndCoefficient := nil;
        while not LDataSet.DataSet.Eof do
        begin

          if (LDataSet.DataSet.FieldByName('CurveSetNr').AsInteger <> LCurveSetIndex) then
          begin
            LCurveSetIndex  := LDataSet.DataSet.FieldByName('CurveSetNr').AsInteger;
            LSubSystemIndex := 0;
          end;

          if (LDataSet.DataSet.FieldByName('SubSystemID').AsInteger <> LSubSystemIndex) then
          begin
            LSubSystemIndex := LDataSet.DataSet.FieldByName('SubSystemID').AsInteger;
            LCoefficient := LFamilyFileDataObject.AddCoefficient;
            LSubsystem := LSubsystemList.SubsystemByIndex(LSubSystemIndex-1);
            LCoefficient.Subsystem.Assign(LSubsystem);
            LCoefficient.CurveSetIndex   := LCurveSetIndex;
            LCoefficient.SubsystemIndex  := LSubSystemIndex;
            LStartStorageIndex           := 0;
          end;

          if (LDataSet.DataSet.FieldByName('StartStorageNr').AsInteger <> LStartStorageIndex) then
          begin
            LVolumeAndCoefficient := LCoefficient.AddStartVolumeAndScenarioCoefficients;

            LStartStorageIndex := LDataSet.DataSet.FieldByName('StartStorageNr').AsInteger;
            if(LStartVolumePerc[LStartStorageIndex] <> NullFloat) then
            begin
              LVolumeAndCoefficient.StartVolume.StartVolume.FData := LStartVolumePerc[LStartStorageIndex];
              LVolumeAndCoefficient.StartVolume.StartVolume.FInitalised := True;
            end;

            if(LCurveSetIndex <> 0) then
            begin
              LVolumeAndCoefficient.StartVolume.CurveSetNo.FData := LCurveSetIndex;
              LVolumeAndCoefficient.StartVolume.CurveSetNo.FInitalised := True;
            end;
          end;

          LScenarioCoefficient := LVolumeAndCoefficient.AddScenarioCoefficients;

          if not LDataSet.DataSet.FieldByName('LoadCase').IsNull then
          begin
            LScenarioCoefficient.LineTargetDraft.FData := LDataSet.DataSet.FieldByName('LoadCase').AsFloat;
            LScenarioCoefficient.LineTargetDraft.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('RiskProportion').IsNull then
          begin
            LScenarioCoefficient.RiskProp.FData := LDataSet.DataSet.FieldByName('RiskProportion').AsFloat;
            LScenarioCoefficient.RiskProp.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('CoefficientA').IsNull then
          begin
            LScenarioCoefficient.CoefArray[1].FData := LDataSet.DataSet.FieldByName('CoefficientA').AsFloat;
            LScenarioCoefficient.CoefArray[1].FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('CoefficientB').IsNull then
          begin
            LScenarioCoefficient.CoefArray[2].FData := LDataSet.DataSet.FieldByName('CoefficientB').AsFloat;
            LScenarioCoefficient.CoefArray[2].FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('CoefficientC').IsNull then
          begin
            LScenarioCoefficient.CoefArray[3].FData := LDataSet.DataSet.FieldByName('CoefficientC').AsFloat;
            LScenarioCoefficient.CoefArray[3].FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('CoefficientD').IsNull then
          begin
            LScenarioCoefficient.CoefArray[4].FData := LDataSet.DataSet.FieldByName('CoefficientD').AsFloat;
            LScenarioCoefficient.CoefArray[4].FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;

        //line 17 +++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadSolveFixedPositionSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LFixedSubSystem := LFamilyFileDataObject.AddFixedSubSystem;
          if not LDataSet.DataSet.FieldByName('FixedPositionNr').IsNull then
          begin
            LFixedSubSystem.SubsystemPos.FData := LDataSet.DataSet.FieldByName('FixedPositionNr').AsInteger;
            LFixedSubSystem.SubsystemPos.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('FixedPosSubSystemID').IsNull then
          begin
            LFixedSubSystem.SubsystemNo4.FData := LDataSet.DataSet.FieldByName('FixedPosSubSystemID').AsInteger;
            LFixedSubSystem.SubsystemNo4 .FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;

        //line 18 +++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadSolveSpecificOrderSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LFixedSubSystem := LFamilyFileDataObject.AddSequentialSubSystem;
          if not LDataSet.DataSet.FieldByName('BeforeSubSystemID').IsNull then
          begin
            LFixedSubSystem.SubsystemPos.FData := LDataSet.DataSet.FieldByName('BeforeSubSystemID').AsInteger;
            LFixedSubSystem.SubsystemPos.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('AfterSubSystemID').IsNull then
          begin
            LFixedSubSystem.SubsystemNo4.FData := LDataSet.DataSet.FieldByName('AfterSubSystemID').AsInteger;
            LFixedSubSystem.SubsystemNo4 .FInitalised := True;
          end;
          LDataSet.DataSet.Next;
        end;

        //line 21 +++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadSupportChannelSQL(AFilename.FileNumber));
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LSupportChannel := LFamilyFileDataObject.AddSupportChannel;

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LSupportChannel.SupportChannelNo.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LSupportChannel.SupportChannelNo.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('NrOfCntrlSubSystems').IsNull then
          begin
            LSupportChannel.SubsystemNo5.FData := LDataSet.DataSet.FieldByName('NrOfCntrlSubSystems').AsInteger;
            LSupportChannel.SubsystemNo5 .FInitalised := True;
          end;

          for LIndex := 1 to 10 do
          begin
            LFieldName := Format('%s%2.2d',['CntrlSubSystemID',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LSupportChannel.InfluencedSubSytem[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
              LSupportChannel.InfluencedSubSytem[LIndex].FInitalised := True;
            end;
            LFieldName := Format('%s%2.2d',['CntrlFactor',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LSupportChannel.InfluenceFactor[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LSupportChannel.InfluenceFactor[LIndex].FInitalised := True;
            end;
          end;
          LDataSet.DataSet.Next;
        end;

        // Line 1
        LFamilyFileDataObject.LevelCount.FData := LFamilyFileDataObject.CurtailmentCount;
        LFamilyFileDataObject.LevelCount.FInitalised := True;

        LFamilyFileDataObject.CategoryCount.FData := LFamilyFileDataObject.DistributionCount;
        LFamilyFileDataObject.CategoryCount.FInitalised := True;

        // Line 6
        LFamilyFileDataObject.DemandSupportDefCount.FData := LFamilyFileDataObject.DemandSupportDefinitionCount;
        LFamilyFileDataObject.DemandSupportDefCount.FInitalised := True;

        // Line 8
        LFamilyFileDataObject.YieldCurveSubsCount.FData := LFamilyFileDataObject.NonFirmSubsystemCount;
        LFamilyFileDataObject.YieldCurveSubsCount.FInitalised := True;

        // Line 16
        LFamilyFileDataObject.FixedPosSubsystemCount.FData := LFamilyFileDataObject.FixedSubSystemCount;
        LFamilyFileDataObject.FixedPosSubsystemCount.FInitalised := True;

        // Line 18
        LFamilyFileDataObject.SequentialPosSubsystemCount.FData := LFamilyFileDataObject.SequentialSubSystemCount;
        LFamilyFileDataObject.SequentialPosSubsystemCount.FInitalised := True;

        // Line 20
        LFamilyFileDataObject.SupportStructureCount.FData := LFamilyFileDataObject.SupportChannelCount;
        LFamilyFileDataObject.SupportStructureCount.FInitalised := True;


        //line13 on wards+++++++++++++++++++++++++++
        LDataSet.DataSet.Active := False;
        LDataSet.SetSQL(ReadFMUnkownDataSQL((AFilename.FileNumber)));
        LDataSet.DataSet.Open;

        while not LDataSet.DataSet.Eof do
        begin
          LFamilyFileDataObject.FMExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
      LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LStringsContainer.Free;
      LSubsystemList.Free;
      LDataSet.Free;
      LDataSet2.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage       : string;
  LFieldName     : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LLastIndex,
  LDemandDefID,
  LDemandDefOrder,
  LCount,
  LIndex,
  LIndex2,
  LIndex3                  : integer;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LFamilyFileDataObject    : TAllocationDefinitionFileDataObject;
  //LStartVolumePerc         : array[1..10] of double;
  LDistribution,
  LCurtailment             : TClassValuesArrayObject;
  LSupportChannel          : TSupportChannel;
  LFixedSubSystem          : TFixedSubSystem;
  LDemandSupportDefinition : TDemandSupportDefinition;
  LRoutingSupportChannelNumbers : TRoutingSupportChannelNumbers;
  LCoefficient             : TCoefficients;
  LNonFirmSubsystem        : TNonFirmSubsystem;
  LSystemFullPerNode       : TSystemFullPerNodes;
  LStringsContainer        : TStringList;
  LVolumeAndCoefficient    : TStartVolumeAndScenarioCoefficients;
  LScenarioCoefficient     : TScenarioCoefficients;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LFamilyFileDataObject    := LPlanningFileDataObject.AddAllocationDefinitionFileDataObject(AFilename.FileNumber);
    if(LFamilyFileDataObject = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    LStringsContainer := TStringList.Create;
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteAllocationDefinitionSQL);

      LDataset.ClearQueryParams(prFloat);
      LDataSet.SetParams(
        ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
        [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
         FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

      LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
      LDataSet.SetParams(['FamilyFile'], [UpperCase(AFilename.FileName)]);

      //Default value +++++++++++++++++++++++++
      LFieldName := Trim(ExtractFileName(AFileName.FileName));
      LFieldName := Copy(LFieldName,1,Length(LFieldName)-4);
      if(LFieldName = '') then
        LFieldName := 'Allocation Name';

      LDataSet.SetParams(['AllocDefName'], [LFieldName]);

      //line 1 +++++++++++++++++++++++++
        //line 1a +++++++++++++++++++++++++
      if LFamilyFileDataObject.ClassCount.FInitalised then
        LDataSet.SetParams(['NrOfReliabilityClasses'], [IntToStr(LFamilyFileDataObject.ClassCount.FData)]);

        //line 1d +++++++++++++++++++++++++
      if LFamilyFileDataObject.PeriodLength.FInitalised then
        LDataSet.SetParams(['PeriodLength'], [IntToStr(LFamilyFileDataObject.PeriodLength.FData)]);

        //line 1e +++++++++++++++++++++++++
      if LFamilyFileDataObject.ImplemntationDecision.FInitalised then
        LDataSet.SetParams(['ImplemntationDecision'], [LFamilyFileDataObject.ImplemntationDecision.FData]);

      //line 2 +++++++++++++++++++++++++
      for LIndex := 1 to 5 do
      begin
        LFieldName := Format('%s%2.2d',['RIValue',LIndex]);
        if LFamilyFileDataObject.RIValueArray[LIndex].FInitalised then
          LDataSet.SetParams([LFieldName], [IntToStr(LFamilyFileDataObject.RIValueArray[LIndex].FData)]);
      end;

      //line 3 +++++++++++++++++++++++++
      for LIndex := 1 to 5 do
      begin
        LFieldName := Format('%s%2.2d',['RILabel',LIndex]);
        if LFamilyFileDataObject.RILabelArray[LIndex].FInitalised then
          LDataSet.SetParams([LFieldName], [LFamilyFileDataObject.RILabelArray[LIndex].FData]);
      end;

      //line 8 +++++++++++++++++++++++++
        //line 8b +++++++++++++++++++++++++
      if LFamilyFileDataObject.YieldCurveSetsCount.FInitalised then
        LDataSet.SetParams(['NrOfStartStoragePercs'], [IntToStr(LFamilyFileDataObject.YieldCurveSetsCount.FData)]);

        //line 8c +++++++++++++++++++++++++
      if LFamilyFileDataObject.FamilyLoadCaseCount.FInitalised then
        LDataSet.SetParams(['NrOfLoadCases'], [IntToStr(LFamilyFileDataObject.FamilyLoadCaseCount.FData)]);

        //line 8b +++++++++++++++++++++++++
      if LFamilyFileDataObject.CurveSetsPerMonthCount.FInitalised then
        LDataSet.SetParams(['NrOfCurveSets'], [IntToStr(LFamilyFileDataObject.CurveSetsPerMonthCount.FData)]);

      //line 9 +++++++++++++++++++++++++
      for LIndex := 1 to 12 do
      begin
        LFieldName := Format('%s%2.2d',['MonthCurveSet',LIndex]);
        if LFamilyFileDataObject.DecisionIndicatorArray[LIndex].FInitalised then
          LDataSet.SetParams([LFieldName], [IntToStr(LFamilyFileDataObject.DecisionIndicatorArray[LIndex].FData)]);
      end;

      //line 12a +++++++++++++++++++++++++
      LCoefficient := LFamilyFileDataObject.CoefficientByIndex[0];
      for LIndex := 0 to LCoefficient.StartVolumeAndScenarioCoefficientCount -1 do
      begin
        LVolumeAndCoefficient := LCoefficient.StartVolumeAndScenarioCoefficientsByIndex[LIndex];
        if LVolumeAndCoefficient.StartVolume.StartVolume.FInitalised then
        begin
          LFieldName := Format('%s%2.2d',['StartStoragePerc',LIndex+1]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LVolumeAndCoefficient.StartVolume.StartVolume.FData)]);
        end;
      end;

      //line 15 +++++++++++++++++++++++++
      if LFamilyFileDataObject.SupportStrategyType.FInitalised then
        LDataSet.SetParams(['SupportStrategy'], [IntToStr(LFamilyFileDataObject.SupportStrategyType.FData)]);

      //line 22 +++++++++++++++++++++++++
      if LFamilyFileDataObject.BalancingOption.FInitalised then
        LDataSet.SetParams(['BalancingOption'], [IntToStr(LFamilyFileDataObject.BalancingOption.FData)]);

      LDataSet.ExecSQL;


      //line4++++++++++++++++++++++++++++
      for LIndex := 0 to LFamilyFileDataObject.CurtailmentCount-1 do
      begin
        LCurtailment := LFamilyFileDataObject.CurtailmentByIndex[LIndex];
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteAllocationLevelSQL);
        LDataset.ClearQueryParams(prFloat);
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
        LDataSet.SetParams(['AllocLevelID'], [IntToStr(LIndex+1)]);
        LDataSet.SetParams(['AllocLevelName'], ['Allocation Level '+ IntToStr(LIndex+1)]);

        if LCurtailment.Comment.FInitalised then
          LDataSet.SetParams(['Comment'], [LCurtailment.Comment.FData]);

        for LIndex2 := 1 to 5 do
        begin
          if LCurtailment.ClassValuesArray[LIndex2].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['Curtailment',LIndex2]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LCurtailment.ClassValuesArray[LIndex2].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;

      //line5++++++++++++++++++++++++++++
      for LIndex := 0 to LFamilyFileDataObject.DistributionCount-1 do
      begin
        LDistribution := LFamilyFileDataObject.DistributionByIndex[LIndex];
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteUserCategorySQL);
        LDataset.ClearQueryParams(prFloat);
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
        LDataSet.SetParams(['UserCategoryID'], [IntToStr(LIndex+1)]);
        LDataSet.SetParams(['UserCategoryName'], ['User Category '+ IntToStr(LIndex+1)]);

        if LDistribution.Comment.FInitalised then
          LDataSet.SetParams(['Comment'], [LDistribution.Comment.FData]);

        for LIndex2 := 1 to 5 do
        begin
          if LDistribution.ClassValuesArray[LIndex2].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['Distribution',LIndex2]);
            LDataSet.SetParams([LFieldName], [FloatToStr( LDistribution.ClassValuesArray[LIndex2].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;


      //line 7 +++++++++++++++++++++++++
      LDemandDefID    := 0;
      LDemandDefOrder := 0;
      for LIndex := 0 to LFamilyFileDataObject.DemandSupportDefinitionCount-1 do
      begin
        LDemandSupportDefinition := LFamilyFileDataObject.DemandSupportDefinitionByIndex[LIndex];
        if(LDemandDefID  <> LDemandSupportDefinition.SubsystemNo1.FData) then
        begin
          LDemandDefID    := LDemandSupportDefinition.SubsystemNo1.FData;
          LDemandDefOrder := 1;
        end
        else
        begin
          LDemandDefOrder := LDemandDefOrder + 1;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDemandDefSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFileName.FileNumber)]);

        LDataSet.SetParams(['DemandDefID'], [IntToStr(LIndex+1)]);

        LDataSet.SetParams(['DemandDefOrder'], [IntToStr(LDemandDefOrder)]);

        if LDemandSupportDefinition.DemandDefName.FInitalised then
          LDataSet.SetParams(['DemandDefName'], [LDemandSupportDefinition.DemandDefName.FData]);

        if LDemandSupportDefinition.DemandCentreID.FInitalised then
          LDataSet.SetParams(['DemandCentreID'], [IntToStr(LDemandSupportDefinition.DemandCentreID.FData)]);

        if LDemandSupportDefinition.SubsystemNo1.FInitalised then
          LDataSet.SetParams(['ParentSubSystemID'], [IntToStr(LDemandSupportDefinition.SubsystemNo1.FData)]);

        if LDemandSupportDefinition.GrowthFlag.FInitalised then
          LDataSet.SetParams(['GrowthType'], [IntToStr(Trunc(LDemandSupportDefinition.GrowthFlag.FData))]);

        if LDemandSupportDefinition.TargetDemand.FInitalised then
          LDataSet.SetParams(['TargetDemand'], [FloatToStr(LDemandSupportDefinition.TargetDemand.FData)]);

        if LDemandSupportDefinition.UserCategory.FInitalised then
          LDataSet.SetParams(['DCUserCategoryID'], [IntToStr(LDemandSupportDefinition.UserCategory.FData)]);

        if LDemandSupportDefinition.SupportArc1.FInitalised then
          LDataSet.SetParams(['SupportArc1'], [IntToStr(LDemandSupportDefinition.SupportArc1.FData)]);


        if LDemandSupportDefinition.SupportArc2.FInitalised then
          LDataSet.SetParams(['SupportArc2'], [IntToStr(LDemandSupportDefinition.SupportArc2.FData)]);

        if LDemandSupportDefinition.SupportSystemNr.FInitalised then
          LDataSet.SetParams(['SupportSystemNr'], [IntToStr(LDemandSupportDefinition.SupportSystemNr.FData)]);

        if LDemandSupportDefinition.Comment.FInitalised then
          LDataSet.SetParams(['Comment'], [LDemandSupportDefinition.Comment.FData]);


        LDataSet.ExecSQL;

        for LIndex2 := 0 to LDemandSupportDefinition.RoutingSupportChannelNumbersCount-1 do
        begin
          LRoutingSupportChannelNumbers := LDemandSupportDefinition.RoutingSupportChannelNumbersByIndex[LIndex2];
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteSupportSubSystemSQL);
          LDataset.ClearQueryParams(prFloat);
          LDataSet.SetParams(
            ['Model','StudyAreaName','SubArea','Scenario'],
            [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
          LDataSet.SetParams(['DemandDefID'], [IntToStr(LIndex+1)]);
          LDataSet.SetParams(['SupportSubSystemID'], [IntToStr(LIndex2+1)]);


          if LDemandSupportDefinition.SupportSubsystemsNumbers[LIndex2+1].FInitalised then
            LDataSet.SetParams(['SupSubSystemID'], [IntToStr(LDemandSupportDefinition.SupportSubsystemsNumbers[LIndex2+1].FData)]);

          for LCount := 1 to 5 do
          begin
            if LRoutingSupportChannelNumbers.ChannelNumbers[LCount].FInitalised then
            begin
              LFieldName := Format('%s%2.2d',['SupSubSysChannelNr',LCount]);
              LDataSet.SetParams([LFieldName], [IntToStr( LRoutingSupportChannelNumbers.ChannelNumbers[LCount].FData)]);
            end;
          end;
          LDataSet.ExecSQL;
        end;
      end;

      LLastIndex := LFamilyFileDataObject.CoefficientCount - LFamilyFileDataObject.NonFirmSubsystemCount;
      for LIndex := 0 to LFamilyFileDataObject.NonFirmSubsystemCount-1 do
      begin
        //line 10+++++++++++++++++++++++++
        LNonFirmSubsystem := LFamilyFileDataObject.NonFirmSubsystemByIndex[LIndex];
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteSubSystemSQL);
        LDataset.ClearQueryParams(prFloat);
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
        LDataSet.SetParams(['SubSystemOrder'], [IntToStr(LIndex+1)]);

        if LNonFirmSubsystem.NonFirmSubsystemNo.FInitalised then
          LDataSet.SetParams(['SubSystemID'], [IntToStr(LNonFirmSubsystem.NonFirmSubsystemNo.FData)]);

        for LCount := 1 to 4 do
        begin
          if LNonFirmSubsystem.RoutingChannelNoArray[LCount].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['RoutingChannelNr',LCount]);
            LDataSet.SetParams([LFieldName], [IntToStr( LNonFirmSubsystem.RoutingChannelNoArray[LCount].FData)]);
          end;
        end;

        if LNonFirmSubsystem.RoutingChannelNoArray[5].FInitalised then
          LDataSet.SetParams(['SupportCalcType'], [IntToStr(LNonFirmSubsystem.RoutingChannelNoArray[5].FData)]);

        //line 11+++++++++++++++++++++++++

        LCoefficient := LFamilyFileDataObject.CoefficientByIndex[LLastIndex];
        LLastIndex   := LLastIndex + 1;
        //LCoefficient := LFamilyFileDataObject.CoefficientByIndex[LIndex];

        if LCoefficient.Subsystem.SubsystemName.FInitalised then
          LDataSet.SetParams(['SubSystemName'], [LCoefficient.Subsystem.SubsystemName.FData]);

        if LCoefficient.Subsystem.FamilyCurveStartYear.FInitalised then
          LDataSet.SetParams(['SubSystemStartYear'], [IntToStr(LCoefficient.Subsystem.FamilyCurveStartYear.FData)]);

        if LCoefficient.Subsystem.FamilyCurveStartMonth.FInitalised then
          LDataSet.SetParams(['SubSystemStartMonth'], [IntToStr(LCoefficient.Subsystem.FamilyCurveStartMonth.FData)]);

        if LCoefficient.Subsystem.FamilyCurveEndYear.FInitalised then
          LDataSet.SetParams(['SubSystemEndYear'], [IntToStr(LCoefficient.Subsystem.FamilyCurveEndYear.FData)]);

        if LCoefficient.Subsystem.FamilyCurveEndMonth.FInitalised then
          LDataSet.SetParams(['SubSystemEndMonth'], [IntToStr(LCoefficient.Subsystem.FamilyCurveEndMonth.FData)]);

        if LCoefficient.Subsystem.SubsystemNo2.FInitalised then
          LDataSet.SetParams(['SubtractedSubSystemID'], [IntToStr(LCoefficient.Subsystem.SubsystemNo2.FData)]);

        if LCoefficient.Subsystem.SupportSubsystemNo.FInitalised then
          LDataSet.SetParams(['SupportingSubSystemID'], [IntToStr(LCoefficient.Subsystem.SupportSubsystemNo.FData)]);

        if LCoefficient.Subsystem.ChannelNo.FInitalised then
          LDataSet.SetParams(['SupportingChannelNr'], [IntToStr(LCoefficient.Subsystem.ChannelNo.FData)]);

        if LCoefficient.Subsystem.ShortTermFirmYield.FInitalised then
          LDataSet.SetParams(['ShortTermYield'], [FloatToStr(LCoefficient.Subsystem.ShortTermFirmYield.FData)]);

        if LCoefficient.Subsystem.StreamFlow.FInitalised then
          LDataSet.SetParams(['LowestStreamFlow'], [FloatToStr(LCoefficient.Subsystem.StreamFlow.FData)]);

        if LCoefficient.Subsystem.LongTermFirmYield.FInitalised then
          LDataSet.SetParams(['LongTermYield'], [FloatToStr(LCoefficient.Subsystem.LongTermFirmYield.FData)]);

        if LCoefficient.Subsystem.LongTermFirmYield.FInitalised then
          LDataSet.SetParams(['LongTermYield'], [FloatToStr(LCoefficient.Subsystem.LongTermFirmYield.FData)]);

        if LCoefficient.Subsystem.FirmFlag.FInitalised then
        begin
          if(UpperCase(LCoefficient.Subsystem.FirmFlag.FData) = 'FIRM') then
          LDataSet.SetParams(['FirmYield'], ['Y'])
          else
            LDataSet.SetParams(['FirmYield'], ['N']);
        end;

        //line 14+++++++++++++++++++++++++
        LSystemFullPerNode := LFamilyFileDataObject.SystemFullPerNodeByIndex[LIndex];
        LStringsContainer.Clear;
        for LCount := 1 to 20 do
        begin
          if LSystemFullPerNode.NodeNoArray[LCount].FInitalised then
            LStringsContainer.Add(IntToStr((LSystemFullPerNode.NodeNoArray[LCount].FData)))
          else
            Break;
        end;
        if (LStringsContainer.CommaText <> '') then
          LDataSet.SetParams(['SubSystemReservoirNrs'], [LStringsContainer.CommaText]);

        LDataSet.ExecSQL;
      end;

      for LIndex := 0 to LFamilyFileDataObject.CoefficientCount-1 do
      begin
        LCoefficient := LFamilyFileDataObject.CoefficientByIndex[LIndex];
          //line 13+++++++++++++++++++++++++
        for LIndex2 := 0 to LCoefficient.StartVolumeAndScenarioCoefficientCount-1 do
        begin
          LVolumeAndCoefficient := LCoefficient.StartVolumeAndScenarioCoefficientsByIndex[LIndex2];

          for LIndex3 := 0 to LVolumeAndCoefficient.ScenarioCoefficientCount-1 do
          begin
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(WriteCoefficientsSQL);
            LDataset.ClearQueryParams();
            LDataSet.SetParams(
              ['Model','StudyAreaName','SubArea','Scenario'],
              [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
               FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

            LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
            LDataSet.SetParams(['CurveSetNr'], [IntToStr(LCoefficient.CurveSetIndex)]);
            LDataSet.SetParams(['SubSystemID'], [IntToStr(LCoefficient.SubsystemIndex)]);

            LDataSet.SetParams(['StartStorageNr'], [IntToStr(LIndex2+1)]);
            LDataSet.SetParams(['LoadCaseNr'], [IntToStr(LIndex3+1)]);

            //if LVolumeAndCoefficient.StartVolume.CurveSetNo.FInitalised then
            //  LDataSet.SetParams(['CurveSetNr'], [IntToStr(LVolumeAndCoefficient.StartVolume.CurveSetNo.FData)]);

            LScenarioCoefficient := LVolumeAndCoefficient.ScenarioCoefficientsByIndex[LIndex3];

            if LScenarioCoefficient.LineTargetDraft.FInitalised then
              LDataSet.SetParams(['LoadCase'], [FloatToStr(LScenarioCoefficient.LineTargetDraft.FData)]);

            if LScenarioCoefficient.RiskProp.FInitalised then
              LDataSet.SetParams(['RiskProportion'], [FloatToStr(LScenarioCoefficient.RiskProp.FData)]);

            if LScenarioCoefficient.CoefArray[1].FInitalised then
              LDataSet.SetParams(['CoefficientA'], [FloatToStr(LScenarioCoefficient.CoefArray[1].FData)]);

            if LScenarioCoefficient.CoefArray[2].FInitalised then
              LDataSet.SetParams(['CoefficientB'], [FloatToStr(LScenarioCoefficient.CoefArray[2].FData)]);

            if LScenarioCoefficient.CoefArray[3].FInitalised then
              LDataSet.SetParams(['CoefficientC'], [FloatToStr(LScenarioCoefficient.CoefArray[3].FData)]);

            if LScenarioCoefficient.CoefArray[4].FInitalised then
              LDataSet.SetParams(['CoefficientD'], [FloatToStr(LScenarioCoefficient.CoefArray[4].FData)]);

            //line 12+++++++++++++++++++++++++
            //if LVolumeAndCoefficient.StartVolume.CurveSetNo.FInitalised then
            //  LDataSet.SetParams(['CurveSetNr'], [IntToStr(LVolumeAndCoefficient.StartVolume.CurveSetNo.FData)]);

            LDataSet.ExecSQL;
          end;
        end;
      end;

      //line 17 +++++++++++++++++++++++++
      for LIndex := 0 to LFamilyFileDataObject.FixedSubSystemCount-1 do
      begin
        LFixedSubSystem := LFamilyFileDataObject.FixedSubSystemsByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteSolveFixedPositionSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
        LDataSet.SetParams(['FixedPositionID'], [IntToStr(LIndex)]);

        if LFixedSubSystem.SubsystemNo4.FInitalised then
          LDataSet.SetParams(['FixedPositionNr'], [IntToStr(LFixedSubSystem.SubsystemNo4.FData)]);

        if LFixedSubSystem.SubsystemPos.FInitalised then
          LDataSet.SetParams(['FixedPosSubSystemID'], [IntToStr(LFixedSubSystem.SubsystemPos.FData)]);

        LDataSet.ExecSQL;
      end;


      //line 18 +++++++++++++++++++++++++
      for LIndex := 0 to LFamilyFileDataObject.SequentialSubSystemCount-1 do
      begin
        LFixedSubSystem := LFamilyFileDataObject.SequentialSubSytemByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteSolveSpecificOrderSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
        LDataSet.SetParams(['SpecificOrderID'], [IntToStr(LIndex)]);

        if LFixedSubSystem.SubsystemNo4.FInitalised then
          LDataSet.SetParams(['BeforeSubSystemID'], [IntToStr(LFixedSubSystem.SubsystemNo4.FData)]);

        if LFixedSubSystem.SubsystemPos.FInitalised then
          LDataSet.SetParams(['AfterSubSystemID'], [IntToStr(LFixedSubSystem.SubsystemPos.FData)]);

        LDataSet.ExecSQL;
      end;


      //line 21 +++++++++++++++++++++++++
      for LIndex := 0 to LFamilyFileDataObject.SupportChannelCount-1 do
      begin
        LSupportChannel := LFamilyFileDataObject.SupportChannelByIndex[LIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteSupportChannelSQL);
        LDataset.ClearQueryParams(prFloat);
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['AllocDefID'], [IntToStr(AFilename.FileNumber)]);
        LDataSet.SetParams(['SupportChannelID'], [IntToStr(LIndex+1)]);

        if LSupportChannel.SupportChannelNo.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LSupportChannel.SupportChannelNo.FData)]);

        if LSupportChannel.SubsystemNo5.FInitalised then
          LDataSet.SetParams(['NrOfCntrlSubSystems'], [IntToStr(LSupportChannel.SubsystemNo5.FData)]);

        for LIndex2 := 1 to 10 do
        begin
          if LSupportChannel.InfluencedSubSytem[LIndex2].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['CntrlSubSystemID',LIndex2]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LSupportChannel.InfluencedSubSytem[LIndex2].FData)]);
          end;
          if LSupportChannel.InfluenceFactor[LIndex2].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['CntrlFactor',LIndex2]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LSupportChannel.InfluenceFactor[LIndex2].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;

       //line13 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LFamilyFileDataObject.FMExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteFMUnknownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LFamilyFileDataObject.FMExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LStringsContainer.Free;
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileAllocationDefinitionDatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
  LAdditionalWhereClause: string;
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
    LAdditionalWhereClause := ' AND AllocDefID = '+ IntToStr(AFileName.FileNumber);
    LTableNames := 'FMAllocationDefinition,FMAllocationLevel,FMCoefficients,FMDemandDefinition,FMSolveFixedPosition,FMSolveSpecificOrder,FMSubSystem,FMSupportChannel,FMSupportSubSystem,FMUserCategory';
    Result := DeleteModelData(LTableNames,LAdditionalWhereClause,AProgressFunction,AQuetly);
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
