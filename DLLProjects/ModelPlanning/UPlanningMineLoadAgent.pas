unit UPlanningMineLoadAgent;

interface
uses
  Classes,
  VoaimsCom_TLB,
  UMineLoadAgent,
  UMiningData,
  UPlanningMineData,
  UPlanningOpenCast,
  UPlanningSlurryDump,
  UPlanningUnderGroundMine,
  UPlanningMineSQLAgent,
  UAbstractObject;

type
  TPlanningMineLoadAgent = class(TMineLoadAgent)
  protected
    FPlanningSQLAgent : TPlanningMineSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadOpenCast(APlanningMine : TPlanningMine) : boolean; overload;
    function LoadSlurryDump(APlanningMine : TPlanningMine): boolean; overload;
    function LoadUnderground(APlanningMine : TPlanningMine) : boolean; overload;
    function LoadGrowthFactor(APlanningMine: TPlanningMine): boolean; overload;
    function LoadGrowthFactor(APlanningOpenCast: TPlanningOpenCast): boolean; overload;
    function LoadGrowthFactor(APlanningSlurryDump: TPlanningSlurryDump): boolean; overload;
    function LoadGrowthFactor(APlanningUnderGroundMine: TPlanningUnderGroundMine): boolean; overload;
    function LoadLoadGeneration(APlanningOpenCast: TPlanningOpenCast): boolean; overload;
    function LoadLoadGeneration(APlanningSlurryDump: TPlanningSlurryDump): boolean; overload;
    function LoadLoadGeneration(APlanningUnderGroundMine: TPlanningUnderGroundMine): boolean; overload;

  public
    function ConstructData(AMineList: TMineList;
                         AMineSubCatchmentList: TMineSubCatchmentList): boolean; overload;
  end;

implementation
uses
  SysUtils,
  UDataSetType,
  UConstants,
  UPlanningMineGrowthFactor,
  ULoadGeneration,
  UErrorHandlingOperations, DB;

procedure TPlanningMineLoadAgent.CreateMemberObjects;
const OPNAME ='TPlanningMineLoadAgent.CreateMemberObjects';
begin
  inherited;
  try
    FPlanningSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError(E,OPNAME);  end;

end;

procedure TPlanningMineLoadAgent.DestroyMemberObjects;
const OPNAME = 'TPlanningMineLoadAgent.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FPlanningSQLAgent);
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMineLoadAgent.LoadGrowthFactor(
  APlanningMine: TPlanningMine): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadGrowthFactor';
var
  LDataSet      :TAbstractModelDataset;
  LNoOfYears    : string;
  LGrowthFactors       : string;
  LNoOfPoints     : integer;
  LInterpolation  : integer;
  LFactorType     : integer;
  LSQL            : string;
  LIdentifier : integer;
  LDescription: string;

begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
       (* LSQL := 'SELECT Identifier, NoOfPoints, FactorType, Description, InterpolationMethod, NoOfYears, GrowthFactors FROM  MineGrowthFactors, ' +
                'MineGrowthFactorType ' +
                'WHERE (Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
                '(StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                '(SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') AND ' +
                '(Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') AND ' +
                '(MineIdentifier=' + IntToStr(APlanningMine.Identifier) + ') AND ' +
                '(FactorType = 1)';
                     *)
         LSQL := 'SELECT g.Identifier, g.NoOfPoints, g.FactorType, ( Select Description From MineGrowthFactorType where Type = g.FactorType ) as Description,'+
              ' g.InterpolationMethod, g.NoOfYears, g.GrowthFactors FROM  MineGrowthFactors g ' +
                ' WHERE (g.Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
                '(g.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                '(g.SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') AND ' +
                '(g.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') AND ' +
                '(g.MineIdentifier=' + IntToStr(APlanningMine.Identifier) + ') AND ' +
                '(g.FactorType = 1)';

                //'(SlurryIdentifier=' + IntToStr(-1) + ') AND ' +
                //'(UDGIdentifier=' + IntToStr(-1) + ') AND ' +
                //'(OCIdentifier=' + IntToStr(-1) + ') AND (FactorType = Type)';
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        while NOT LDataSet.DataSet.Eof do
        begin
          LIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNoOfYears := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
          LGrowthFactors:= LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
          LNoOfPoints := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
          LInterpolation := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
          LFactorType := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
          LDescription := LDataSet.DataSet.FieldByName('Description').AsString;
          APlanningMine.NewGrowthFactor;
          APlanningMine.GrowthFactor.Populate(LIdentifier,APlanningMine.Identifier,0,0,0,LNoOfPoints,LFactorType,LInterpolation,
                                              LNoOfYears,LGrowthFactors,LDescription);
          LDataSet.DataSet.Next;
        end;
        Result := true;
      finally
        begin
          LDataSet.DataSet.Close;
          FreeAndNil(LDataSet);
        end;
      end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.LoadGrowthFactor(
  APlanningOpenCast: TPlanningOpenCast): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadGrowthFactor';
var
  LDataSet      :TAbstractModelDataset;
  LNoOfYears    : string;
  LGrowthFactors       : string;
  LNoOfPoints     : integer;
  LInterpolation  : integer;
  LFactorType     : integer;
  LSQL            : string;
  LTempGrowthFactor : TPlanningMineGrowthFactor;
  LIdentifier : integer;
  LDescription: string;

begin
  Result := false;
  try
     FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
     try
      (*LSQL := 'SELECT Identifier, NoOfPoints, FactorType, Description, InterpolationMethod, NoOfYears, GrowthFactors FROM  MineGrowthFactors, ' +
                'MineGrowthFactorType ' +
                'WHERE (Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
                '(StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                '(SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') AND ' +
                '(Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') AND ' +
                '(MineIdentifier=' + IntToStr(APlanningOpenCast.MineIdentifier) + ') AND ' +
                //'(SlurryIdentifier=' + IntToStr(-1) + ') AND ' +
                //'(UDGIdentifier=' + IntToStr(-1) + ') AND ' +
                '(OCIdentifier=' + IntToStr(APlanningOpenCast.Identifier) + ') AND ' +
                '(FactorType = Type)';
                      *)
        LSQL := 'SELECT g.Identifier, g.NoOfPoints, g.FactorType, ( Select Description From MineGrowthFactorType where Type = g.FactorType ) as Description,'+
              ' g.InterpolationMethod, g.NoOfYears, g.GrowthFactors FROM  MineGrowthFactors g ' +
                ' WHERE (g.Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
                '(g.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                '(g.SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') AND ' +
                '(g.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') AND ' +
                '(g.MineIdentifier=' + IntToStr(APlanningOpenCast.MineIdentifier) + ') AND ' +
                '(g.OCIdentifier=' + IntToStr(APlanningOpenCast.Identifier) + ') AND ' +
                '(g.FactorType in (2,3,4,5,6,7))';

        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        while NOT LDataSet.DataSet.Eof do
        begin
          LIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNoOfYears := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
          LGrowthFactors:= LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
          LNoOfPoints := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
          LInterpolation := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
          LFactorType := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
          LDescription := LDataSet.DataSet.FieldByName('Description').AsString;
          //LTempGrowthFactor := APlanningOpenCast.GrowthFactorByType(LFactorType);
          LTempGrowthFactor := APlanningOpenCast.NewGrowthFactor;
          LTempGrowthFactor.Populate(LIdentifier,APlanningOpenCast.MineIdentifier, APlanningOpenCast.Identifier,
            0,0,LNoOfPoints,LFactorType, LInterpolation, LNoOfYears, LGrowthFactors,LDescription);
          LDataSet.DataSet.Next;
        end;
        Result := true;
     finally
       FreeAndNil(LDataSet);
     end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.LoadOpenCast(
  APlanningMine: TPlanningMine): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadOpenCast';
var
  LIndex: integer;
  LRechargeFactorsDataset,
  LDataSet : TAbstractModelDataSet;
  LAbstraction : integer;
  LPCDIniConcentration : double;
  LWorkingCommYear : integer;
  LWorkingCommMonth : integer;
  LWorkingDecommYear : integer;
  LWorkingDecommMonth : integer;
  LRunoffSaltWashOffEfficiencyFactor : double;
  LIniSaltStore : double;
  LReChargeRate : double;
  LAbstractToEvap : double;
  LAbstractToRiver : double;
  LAbstractToCPD : double;
  LTimeSeriesFile : string;
  LOpenCastIdentifier        : integer;
  LPitName                   : string;
  LCoalReserveArea,
  LWorkingsArea,
  LDisturbedWorkingsArea,
  LDisturbedArea ,
  LWaterSurfaceEvapArea,
  LDisturbedAreaRunOff,
  LDisturbedWorkingsAreaRunOff,
  LDecantVolume,
  LSeepageVolume,
  LAnalysisStartVolume,
  LMaximumSeepageRate,
  LSeepageExponent,
  LPCDSurfaceArea,
  LPCDAnalysisStartVolume,
  LPCDStorageCapacity        : double;
  LDisturbedRechargeFactor,
  LWorkingAreaRechargeFactor : TMonthlyDoubleArray;
  LTempOpenCast :  TPlanningOpenCast;
begin
  //Result :=inherited LoadOpenCast(APlanningMine);
  //if (Result) then
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LRechargeFactorsDataset);
    try
      LDataSet.SetSQL(FPlanningSQLAgent.GetOpenCastSQL(APlanningMine.Identifier));
      LDataSet.DataSet.Open;
      while Not LDataSet.DataSet.Eof do
      begin
        LOpenCastIdentifier          := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LPitName                     := Trim(LDataset.DataSet.FieldByName('PitName').AsString);
        LCoalReserveArea             := LDataset.DataSet.FieldByName('CoalReserveArea').AsFloat;
        LWorkingsArea                := LDataset.DataSet.FieldByName('WorkingsArea').AsFloat;
        LDisturbedWorkingsArea       := LDataset.DataSet.FieldByName('DisturbedWorkingsArea').AsFloat;
        LDisturbedArea               := LDataset.DataSet.FieldByName('DisturbedArea').AsFloat;
        LWaterSurfaceEvapArea        := LDataset.DataSet.FieldByName('WaterSurfaceEvapArea').AsFloat;
        LDisturbedAreaRunoff         := LDataset.DataSet.FieldByName('DisturbedAreaRunoff').AsFloat;
        LDisturbedWorkingsAreaRunoff := LDataset.DataSet.FieldByName('DisturbedWorkingsAreaRunoff').AsFloat;
        LDecantVolume                := LDataset.DataSet.FieldByName('DecantVolume').AsFloat;
        LSeepageVolume               := LDataset.DataSet.FieldByName('SeepageVolume').AsFloat;
        LAnalysisStartVolume         := LDataset.DataSet.FieldByName('AnalysisStartVolume').AsFloat;
        LMaximumSeepageRate          := LDataset.DataSet.FieldByName('MaximumSeepageRate').AsFloat;
        LSeepageExponent             := LDataset.DataSet.FieldByName('SeepageExponent').AsFloat;
        LPCDSurfaceArea              := LDataset.DataSet.FieldByName('PCDSurfaceArea').AsFloat;
        LPCDStorageCapacity          := LDataset.DataSet.FieldByName('PCDStorageCapacity').AsFloat;
        LPCDAnalysisStartVolume      := LDataset.DataSet.FieldByName('PCDAnalysisStartVolume').AsFloat;
        LAbstraction := 0;
        if not (LDataSet.DataSet.FieldByName('Abstraction').IsNull or (LDataSet.DataSet.FieldByName('Abstraction').AsString = ' ')) then
          LAbstraction               := LDataSet.DataSet.FieldByName('Abstraction').AsInteger;

        LPCDIniConcentration         := LDataSet.DataSet.FieldByName('PCDIniConcentration').AsFloat;
        LWorkingCommYear             := LDataSet.DataSet.FieldByName('WorkingCommYear').AsInteger;
        LWorkingCommMonth            := LDataSet.DataSet.FieldByName('WorkingCommMonth').AsInteger;
        LWorkingDecommYear           := LDataSet.DataSet.FieldByName('WorkingDecommYear').AsInteger;
        LWorkingDecommMonth          := LDataSet.DataSet.FieldByName('WorkingDecommMonth').AsInteger;
        LRunoffSaltWashOffEfficiencyFactor := LDataSet.DataSet.FieldByName('RunoffSaltWashOffEfficiencyFactor').AsFloat;
        LIniSaltStore                      := LDataSet.DataSet.FieldByName('IniSaltStore').AsFloat;
        LReChargeRate                      := LDataSet.DataSet.FieldByName('ReChargeRate').AsFloat;
        LAbstractToEvap                    := LDataSet.DataSet.FieldByName('AbstractToEvap').AsFloat;
        LAbstractToRiver                   := LDataSet.DataSet.FieldByName('AbstractToRiver').AsFloat;
        LAbstractToCPD                     := LDataSet.DataSet.FieldByName('AbstractToPCD').AsFloat;
        LTimeSeriesFile                    := LDataSet.DataSet.FieldByName('AbstractMonthTimeSeriesFile').AsString;
        LTempOpenCast                      := APlanningMine.NewOpenCast;
        if LTempOpenCast <> nil then
        begin
          LTempOpenCast.Populate(APlanningMine.Identifier,LOpenCastIdentifier,LPitName,LCoalReserveArea,LWorkingsArea,LDisturbedWorkingsArea,
                          LDisturbedArea,LWaterSurfaceEvapArea,LDisturbedAreaRunOff,LDisturbedWorkingsAreaRunOff,
                          LDecantVolume,LSeepageVolume,LAnalysisStartVolume,LMaximumSeepageRate,LSeepageExponent,
                          LPCDSurfaceArea,LPCDStorageCapacity,LPCDAnalysisStartVolume);
          LTempOpenCast.Populate(LAbstraction, LPCDIniConcentration,LWorkingCommYear, LWorkingCommMonth,
                                 LWorkingDecommYear, LWorkingDecommMonth, LRunoffSaltWashOffEfficiencyFactor,
                                 LIniSaltStore, LReChargeRate, LAbstractToEvap, LAbstractToRiver, LAbstractToCPD,
                                 LTimeSeriesFile);
          LoadGrowthFactor(LTempOpenCast);
          LoadLoadGeneration(LTempOpenCast);
        end;
        LRechargeFactorsDataset.DataSet.Close;
        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(APlanningMine.Identifier,1,LOpenCastIdentifier,1));
        LRechargeFactorsDataset.DataSet.Open;
        if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LDisturbedRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LTempOpenCast.PopulateDisturbedRechargeFactors(LDisturbedRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;

        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(APlanningMine.Identifier,1,LOpenCastIdentifier,2));
        LRechargeFactorsDataset.DataSet.Open;
       if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LWorkingAreaRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LTempOpenCast.PopulateWorkingAreaRechargeFactors(LWorkingAreaRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;

        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.LoadSlurryDump(
  APlanningMine: TPlanningMine): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadSlurryDump';
var
  LRechargeFactorsDataset,
  LDataSet : TAbstractModelDataset;
  LSaltConcentration : double;
  LIndex : integer;
  LSlurryDump                : TPlanningSlurryDump;
  LSlurryDumpIdentifier      : integer;
  LDumpName                  : string;
  LDumpSurfaceArea           : double;
  LRunoffFactorToPCD         : double;
  LSeepageSplitFactor        : double;
  LPCDStorageCapacity        : double;
  LPCDSurfaceArea            : double;
  LPCDAnalysisStartVolume    : double;
  LRechargeFactor            : TMonthlyDoubleArray;
begin
  Result := false;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LRechargeFactorsDataset);
    try
      LDataSet.SetSQL(FPlanningSQLAgent.GetSlurryDumpSQL( APlanningMine.Identifier));
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LSlurryDumpIdentifier        := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LDumpName                    := Trim(LDataset.DataSet.FieldByName('DumpName').AsString);
        LDumpSurfaceArea             := LDataset.DataSet.FieldByName('DumpSurfaceArea').AsFloat;
        LRunoffFactorToPCD           := LDataset.DataSet.FieldByName('RunoffFactorToPCD').AsFloat;
        LSeepageSplitFactor          := LDataset.DataSet.FieldByName('SeepageSplitFactor').AsFloat;
        LPCDStorageCapacity          := LDataset.DataSet.FieldByName('PCDStorageCapacity').AsFloat;
        LPCDSurfaceArea              := LDataset.DataSet.FieldByName('PCDSurfaceArea').AsFloat;
        LPCDAnalysisStartVolume      := LDataset.DataSet.FieldByName('PCDAnalysisStartVolume').AsFloat;
        LSaltConcentration           := LDataSet.DataSet.FieldByName('SaltConcentration').AsFloat;

        LSlurryDump := APlanningMine.NewSlurryDump;
        LSlurryDump.Populate(APlanningMine.Identifier,LSlurryDumpIdentifier,LDumpName,LDumpSurfaceArea,LRunoffFactorToPCD,LSeepageSplitFactor,
                             LPCDStorageCapacity,LPCDSurfaceArea,LPCDAnalysisStartVolume);
        LSlurryDump.Populate(LSaltConcentration);

        LRechargeFactorsDataset.DataSet.Close;
        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(APlanningMine.Identifier,3,LSlurryDumpIdentifier,5));
        LRechargeFactorsDataset.DataSet.Open;
        if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LSlurryDump.PopulateRechargeFactors(LRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;
        LoadGrowthFactor(LSlurryDump);
        LoadLoadGeneration(LSlurryDump);
        LDataSet.DataSet.Next;

      end;
      Result := True;
      LDataSet.DataSet.Close;
    finally
      FreeAndNil(LDataSet);
      LRechargeFactorsDataset.Free;
    end;
  except on E:Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.LoadUnderground(
  APlanningMine: TPlanningMine): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadUnderground';
var
  LRechargeFactorsDataset,
  LDataSet                          : TAbstractModelDataset;
  LUnderground                      : TPlanningUnderGroundMine;
  LIndex,
  LUndergroundIdentifier            : integer;
  LUndergroundSectionName           : string;
  LChannelNumberToUGDam             : integer;
  LUpstreamCatchmentArea            : double;
  LBoardPillarCatchmentArea         : double;
  LHighExtractionCatchmentArea      : double;
  LHighExtractionAreaRunoffFactor   : double;
  LUpstreamRunoffPortion ,
  LBoardAndPilarRechargeFactor,
  LHighExtractionRechargeFactor     : TMonthlyDoubleArray;
begin
    //Result := inherited LoadUnderground(APlanningMine);
    Result := False;
  try
    if(APlanningMine = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LRechargeFactorsDataset);
    try
      lDataSet.SetSQL(FSQLAgent.GetUndergroundSQL(APlanningMine.Identifier));
      lDataset.DataSet.Open;
      while (NOT lDataset.DataSet.EOF) do
      begin
        LUndergroundIdentifier           := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LUndergroundSectionName          := Trim(LDataset.DataSet.FieldByName('UndergroundSectionName').AsString);
        LChannelNumberToUGDam            := LDataset.DataSet.FieldByName('ChannelNumberToUGDam').AsInteger;
        LUpstreamCatchmentArea           := LDataset.DataSet.FieldByName('UpstreamCatchmentArea').AsFloat;
        LBoardPillarCatchmentArea        := LDataset.DataSet.FieldByName('BoardPillarCatchmentArea').AsFloat;
        LHighExtractionCatchmentArea     := LDataset.DataSet.FieldByName('HighExtractionCatchmentArea').AsFloat;
        LHighExtractionAreaRunoffFactor  := LDataset.DataSet.FieldByName('HighExtractionAreaRunoffFactor').AsFloat;

        LUnderground := APlanningMine.NewUnderground;
        LUnderground.Populate(APlanningMine.Identifier,LUndergroundIdentifier,LUndergroundSectionName,LChannelNumberToUGDam,
                              LUpstreamCatchmentArea, LBoardPillarCatchmentArea,LHighExtractionCatchmentArea,
                              LHighExtractionAreaRunoffFactor);

        LRechargeFactorsDataset.DataSet.Close;
        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetUGUpstreamRunoffSQL(APlanningMine.Identifier,LUndergroundIdentifier));
        LRechargeFactorsDataset.DataSet.Open;
        if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LUpstreamRunoffPortion[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RunoffFactor%2.2d',[LIndex])).AsFloat;
          end;
          LUnderground.PopulateUpstreamRunoffPortions(LUpstreamRunoffPortion);
        end;
        LRechargeFactorsDataset.DataSet.Close;

        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(APlanningMine.Identifier,2,LUndergroundIdentifier,3));
        LRechargeFactorsDataset.DataSet.Open;
        if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LBoardAndPilarRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LUnderground.PopulateBoardAndPilarRechargeFactors(LBoardAndPilarRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;

        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(APlanningMine.Identifier,2,LUndergroundIdentifier,4));
        LRechargeFactorsDataset.DataSet.Open;
        if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LHighExtractionRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LUnderground.PopulateHighExtractionRechargeFactors(LHighExtractionRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;
        LoadGrowthFactor(LUnderground);
        LoadLoadGeneration(LUnderground);
        lDataset.DataSet.Next;
      end;
      lDataset.DataSet.Close;
      Result := True;
    finally
      LRechargeFactorsDataset.Free;
      lDataset.Free;
    end;
    except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.ConstructData(AMineList: TMineList; AMineSubCatchmentList: TMineSubCatchmentList): boolean;
const OPNAME ='TPlanningMineLoadAgent.ConstructData';
var
  LEvapDataset,
  LDataSet                   : TAbstractModelDataset;
  LMine                      : TPlanningMine;
  LIndex,
  LMineIdentifier,
  LMineNodeNumber            : integer;
  LMineName                  : string;
  LRiverChannelNumber,
  LPCDChannelNumber,
  LHydrologyNodeNumber       : integer;
  LBeneficiationPlantArea,
  LBeneficiationRunOffFactor: double;
  LPanEvaporation,
  LLakeEvaporation: TMonthlyDoubleArray;
  LSaltWashOffNo : integer;
  LRainfallFileName: string;
  LMeanAnnualPrecipitation: double;
  LSaltBuildUpRate : double;
  LSaltWashOffEfficiencyFactor: double;
  LIniSaltStore: double;


begin
  Result := FALSE;
  try
    if FAppModules.StudyArea.ModelVersion <> '7' then
    begin
      Result := True;
      Exit;
    end;

    if(AMineList = nil) then Exit;
    AMineList.Initialise;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LEvapDataset);
    try
      lDataSet.SetSQL(FPlanningSQLAgent.GetMineSQL);
      lDataset.DataSet.Open;
      while NOT lDataset.DataSet.EOF do
      begin
        LMineIdentifier              := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LMineNodeNumber              := LDataset.DataSet.FieldByName('NodeNumber').AsInteger;
        LMineName                    := Trim(LDataset.DataSet.FieldByName('MineName').AsString);
        LRiverChannelNumber          := LDataset.DataSet.FieldByName('RiverChannelNumber').AsInteger;
        LPCDChannelNumber            := LDataset.DataSet.FieldByName('PCDChannelNumber').AsInteger;
        LHydrologyNodeNumber         := LDataset.DataSet.FieldByName('HydrologyNodeNumber').AsInteger;
        LBeneficiationPlantArea      := LDataset.DataSet.FieldByName('BeneficiationPlantArea').AsFloat;
        LBeneficiationRunOffFactor   := LDataset.DataSet.FieldByName('BeneficiationRunOffFactor').AsFloat;
        LSaltWashOffNo               := LDataset.DataSet.FieldByName('SaltWashoffNo').AsInteger;
        LRainfallFileName            := LDataset.DataSet.FieldByName('RainfallFileName').AsString;
        LSaltBuildUpRate             := LDataset.DataSet.FieldByName('SaltBuildUpRate').AsFloat;
        LMeanAnnualPrecipitation     := LDataset.DataSet.FieldByName('MeanAnnPrecip').AsFloat;
        LSaltWashOffEfficiencyFactor := LDataSet.DataSet.FieldByName('SaltWashOffEfficiencyFactor').AsFloat;
        LIniSaltStore                := LDataSet.DataSet.FieldByName('IniSaltStore').AsFloat;


        LMine := TPlanningMine(AMineList.NewMine);
        LMine.Populate(LMineIdentifier,LMineNodeNumber,LMineName,LRiverChannelNumber,LPCDChannelNumber,
                       LHydrologyNodeNumber,LBeneficiationPlantArea,LBeneficiationRunOffFactor);
        LMine.Populate(LSaltWashoffNo, LRainfallFileName, LMeanAnnualPrecipitation,
                       LSaltBuildUpRate, LSaltWashOffEfficiencyFactor,LIniSaltStore);


        LEvapDataset.DataSet.Close;
        LEvapDataset.SetSQL(FSQLAgent.GetPanEvaporationSQL(LMineIdentifier));
        LEvapDataset.DataSet.Open;
        if Not LEvapDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LPanEvaporation[LIndex] := LEvapDataset.DataSet.FieldByName(Format('PanEvaporation%2.2d',[LIndex])).AsFloat;
          end;
          LMine.PopulatePanEvaporations(LPanEvaporation);
        end;
        LEvapDataset.DataSet.Close;

        LEvapDataset.SetSQL(FSQLAgent.GetLakeEvaporationSQL(LMineIdentifier));
        LEvapDataset.DataSet.Open;
        if Not LEvapDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LLakeEvaporation[LIndex] := LEvapDataset.DataSet.FieldByName(Format('LakeEvaporation%2.2d',[LIndex])).AsFloat;
          end;
          LMine.PopulateLakeEvaporations(LLakeEvaporation);
        end;
        LEvapDataset.DataSet.Close;
        LoadGrowthFactor(lMine);
        LoadOpenCast(LMine);
        LoadUnderground(LMine);
        LoadSlurryDump(LMine);

        lDataset.DataSet.Next;
      end;
      lDataset.DataSet.Close;

      Result := TRUE;
      LoadMineSubCatchment(AMineSubCatchmentList);
    finally
      lDataset.Free;
      LEvapDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TPlanningMineLoadAgent.LoadGrowthFactor(
  APlanningSlurryDump: TPlanningSlurryDump): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadGrowthFactor';
var
  LDataSet      :TAbstractModelDataset;
  LNoOfYears    : string;
  LGrowthFactors       : string;
  LNoOfPoints     : integer;
  LInterpolation  : integer;
  LFactorType     : integer;
  LSQL            : string;
  LTempGrowthFactor : TPlanningMineGrowthFactor;
  LIdentifier : integer;
  LDescription : string;

begin
  Result := false;
  try
     FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
     try
      LSQL := 'SELECT Identifier, NoOfPoints, FactorType, Description, InterpolationMethod, NoOfYears, GrowthFactors FROM  MineGrowthFactors INNER JOIN ' +
                'MineGrowthFactorType ON MineGrowthFactors.FactorType = MineGrowthFactorType.Type ' +
                'WHERE (Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
                '(StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                '(SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') AND ' +
                '(Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') AND ' +
                '(MineIdentifier=' + IntToStr(APlanningSlurryDump.MineIdentifier) + ') AND ' +
                '(SlurryIdentifier=' + IntToStr(APlanningSlurryDump.Identifier) + ')';
                //'(UDGIdentifier=' + IntToStr(-1) + ') AND ' +
                //'(OCIdentifier=' + IntToStr(-1) + ')';
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        while NOT LDataSet.DataSet.Eof do
        begin
          LIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNoOfYears := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
          LGrowthFactors:= LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
          LNoOfPoints := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
          LInterpolation := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
          LFactorType := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
          LDescription := LDataSet.DataSet.FieldByName('Description').AsString;
          LTempGrowthFactor := APlanningSlurryDump.NewGrowthFactor;
          LTempGrowthFactor.Populate(LIdentifier,APlanningSlurryDump.MineIdentifier, 0,
            APlanningSlurryDump.Identifier,0,LNoOfPoints,LFactorType, LInterpolation, LNoOfYears, LGrowthFactors,LDescription);
          LDataSet.DataSet.Next;
        end;
        Result := true;
     finally
       FreeAndNil(LDataSet);
     end;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMineLoadAgent.LoadLoadGeneration(
  APlanningSlurryDump: TPlanningSlurryDump): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadLoadGeneration';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier,
  LMineIdentifier: Integer;
  LOpenCastIdentifier: Integer;
  LUNGIdentifier: Integer;
  LSlurryDumpIdentifier: Integer;
  LLoadGenerationType : Integer;
  LStdDeviation: double;
  LFlow: TElevationsArray;
  LMeanOfSalt: TElevationsArray;
  LTempLoadGeneration : TLoadGeneration;
  LSQL, LFieldName : string;
  LIndex : integer;

begin
  Result:= false;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    SetLength(LFlow,10);
    SetLength(LMeanOfSalt,10);

    try
      LSQL := ' SELECT * FROM (SELECT DISTINCT MineLoadGenerationFlow.Identifier,' +
              ' MineLoadGenerationFlow.MineIdentifier, MineLoadGenerationFlow.OpenCastIdentifier,' +
              ' MineLoadGenerationFlow.UDGIdentifier, MineLoadGenerationFlow.SDIdentifier,  ' +
              ' MineLoadGenerationFlow.LoadGenType, MineLoadGenerationFlow.StdDeviation, Flow01, Flow02,' +
              ' Flow03, Flow04, Flow05, Flow06, Flow07, Flow08, Flow09, Flow10, ' +
              ' MeanOfSalt01, MeanOfSalt02, MeanOfSalt03, MeanOfSalt04, MeanOfSalt05, MeanOfSalt06, MeanOfSalt07, ' +
              ' MeanOfSalt08, MeanOfSalt09, MeanOfSalt10 FROM MineLoadGenerationMeanOfSalt' +
              ' LEFT JOIN  MineLoadGenerationFlow  ON (MineLoadGenerationFlow.Identifier = MineLoadGenerationMeanOfSalt.Identifier ) ' +
              ' WHERE ((MineLoadGenerationFlow.Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') And ' +              ' (MineLoadGenerationFlow.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') And ' +
              ' (MineLoadGenerationFlow.SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') And ' +
              ' (MineLoadGenerationFlow.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') And' +
              ' (MineLoadGenerationFlow.MineIdentifier=' + IntToStr(APlanningSlurryDump.MineIdentifier) + ') And ' +
              ' (MineLoadGenerationFlow.SDIdentifier=' + IntToStr(APlanningSlurryDump.Identifier) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.Model='+ QuotedStr(FAppModules.StudyArea.ModelCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.SubArea='+ QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') And  ' +
              ' (MineLoadGenerationMeanOfSalt.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.MineIdentifier=' + IntToStr(APlanningSlurryDump.MineIdentifier) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.SDIdentifier=' + IntToStr(APlanningSlurryDump.Identifier) + ') )) AS A LEFT JOIN MineLoadGenerationType ON ( A.LoadGenType = MineLoadGenerationType.LoadGenType)';
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      while (NOT LDataSet.DataSet.Eof) do
      begin
        Lidentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
        LMineIdentifier := LDataSet.DataSet.FieldByName('MineIdentifier').AsInteger;
        LOpenCastIdentifier := LDataSet.DataSet.FieldByName('OpenCastIdentifier').AsInteger;
        LUNGIdentifier := LDataSet.DataSet.FieldByName('UDGIdentifier').AsInteger;
        LSlurryDumpIdentifier := LDataSet.DataSet.FieldByName('SDIdentifier').AsInteger;
        LLoadGenerationType := LDataSet.DataSet.FieldByName('A.LoadGenType').AsInteger;
        LStdDeviation := LDataSet.DataSet.FieldByName('StdDeviation').AsFloat;
        for LIndex := Low(LFlow) to High(LFlow) do
        begin
          LFieldName := Format('%s%2.2d',['Flow',LIndex+1]);
          LFlow[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LFieldName := Format('%s%2.2d',['MeanOfSalt',LIndex+1]);
          LMeanOfSalt[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        LTempLoadGeneration := APlanningSlurryDump.NewLoadGeneration;
        LFieldName := LDataSet.DataSet.FieldByName('Description').AsString;
        LTempLoadGeneration.Populate(LIdentifier,LMineIdentifier,LOpenCastIdentifier, LUNGIdentifier,LSlurryDumpIdentifier,
        LLoadGenerationType,LStdDeviation,LFlow,LMeanOfSalt, LFieldName);
        LDataSet.DataSet.Next;
      end;
    finally
     LDataSet.DataSet.Free;
     Finalize(LFlow);
     Finalize(LMeanOfSalt);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.LoadLoadGeneration(APlanningOpenCast: TPlanningOpenCast): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadLoadGeneration';
var
  LDataSet : TAbstractModelDataset;
  Lidentifier,
  LMineIdentifier: Integer;
  LOpenCastIdentifier: Integer;
  LUNGIdentifier: Integer;
  LSlurryDumpIdentifier: Integer;
  LLoadGenerationType : Integer;
  LStdDeviation: double;
  LFlow: TElevationsArray;
  LMeanOfSalt: TElevationsArray;
  LTempLoadGeneration : TLoadGeneration;
  LSQL, LFieldName : string;
  LIndex : integer;

begin
  Result:= false;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    SetLength(LFlow,10);
    SetLength(LMeanOfSalt,10);

    try
      LSQL := ' SELECT * FROM (SELECT DISTINCT MineLoadGenerationFlow.Identifier,' +
              ' MineLoadGenerationFlow.MineIdentifier, MineLoadGenerationFlow.OpenCastIdentifier,' +
              ' MineLoadGenerationFlow.UDGIdentifier, MineLoadGenerationFlow.SDIdentifier,  ' +
              ' MineLoadGenerationFlow.LoadGenType, MineLoadGenerationFlow.StdDeviation, Flow01, Flow02,' +
              ' Flow03, Flow04, Flow05, Flow06, Flow07, Flow08, Flow09, Flow10, ' +
              ' MeanOfSalt01, MeanOfSalt02, MeanOfSalt03, MeanOfSalt04, MeanOfSalt05, MeanOfSalt06, MeanOfSalt07, ' +
              ' MeanOfSalt08, MeanOfSalt09, MeanOfSalt10 FROM MineLoadGenerationMeanOfSalt' +
              ' LEFT JOIN  MineLoadGenerationFlow  ON (MineLoadGenerationFlow.Identifier = MineLoadGenerationMeanOfSalt.Identifier ) ' +
              ' WHERE ((MineLoadGenerationFlow.Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') And ' +              ' (MineLoadGenerationFlow.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') And ' +
              ' (MineLoadGenerationFlow.SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') And ' +
              ' (MineLoadGenerationFlow.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') And' +
              ' (MineLoadGenerationFlow.MineIdentifier=' + IntToStr(APlanningOpenCast.MineIdentifier) + ') And ' +
              ' (MineLoadGenerationFlow.OpenCastIdentifier=' + IntToStr(APlanningOpenCast.Identifier) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.Model='+ QuotedStr(FAppModules.StudyArea.ModelCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.SubArea='+ QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') And  ' +
              ' (MineLoadGenerationMeanOfSalt.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.MineIdentifier=' + IntToStr(APlanningOpenCast.MineIdentifier) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.OpenCastIdentifier=' + IntToStr(APlanningOpenCast.Identifier) + ') )) AS A LEFT JOIN MineLoadGenerationType ON ( A.LoadGenType = MineLoadGenerationType.LoadGenType)';
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      while (NOT LDataSet.DataSet.Eof) do
      begin
        Lidentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
        LMineIdentifier := LDataSet.DataSet.FieldByName('MineIdentifier').AsInteger;
        LOpenCastIdentifier := LDataSet.DataSet.FieldByName('OpenCastIdentifier').AsInteger;
        LUNGIdentifier := LDataSet.DataSet.FieldByName('UDGIdentifier').AsInteger;
        LSlurryDumpIdentifier := LDataSet.DataSet.FieldByName('SDIdentifier').AsInteger;
        LLoadGenerationType := LDataSet.DataSet.FieldByName('A.LoadGenType').AsInteger;
        LStdDeviation := LDataSet.DataSet.FieldByName('StdDeviation').AsFloat;

        for LIndex := Low(LFlow) to High(LFlow) do
        begin
          LFieldName := Format('%s%2.2d',['Flow',LIndex+1]);
          LFlow[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LFieldName := Format('%s%2.2d',['MeanOfSalt',LIndex+1]);
          LMeanOfSalt[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        LFieldName := LDataSet.DataSet.FieldByName('Description').AsString;
        LTempLoadGeneration := APlanningOpenCast.NewLoadGeneration;
        LTempLoadGeneration.Populate(Lidentifier,LMineIdentifier,LOpenCastIdentifier, LUNGIdentifier,LSlurryDumpIdentifier,
        LLoadGenerationType,LStdDeviation,LFlow,LMeanOfSalt,LFieldName);
        LDataSet.DataSet.Next;
      end;
    finally
     LDataSet.DataSet.Free;
     Finalize(LFlow);
     Finalize(LMeanOfSalt);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineLoadAgent.LoadGrowthFactor(
  APlanningUnderGroundMine: TPlanningUnderGroundMine): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadGrowthFactor';
var
  LDataSet      :TAbstractModelDataset;
  LNoOfYears    : string;
  LGrowthFactors       : string;
  LNoOfPoints     : integer;
  LInterpolation  : integer;
  LFactorType     : integer;
  LSQL            : string;
  LTempGrowthFactor : TPlanningMineGrowthFactor;
  LIdentifier : integer;
  LDescription : string;

begin
  Result := false;
  try
     FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
     try
      LSQL := 'SELECT g.Identifier, g.NoOfPoints, g.FactorType, ( Select Description From MineGrowthFactorType where Type = g.FactorType ) as Description,'+
              ' g.InterpolationMethod, g.NoOfYears, g.GrowthFactors FROM  MineGrowthFactors g ' +
                ' ' +
                'WHERE (g.Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') AND ' +
                '(g.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                '(g.SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') AND ' +
                '(g.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') AND ' +
                '(g.MineIdentifier=' + IntToStr(APlanningUnderGroundMine.MineIdentifier) + ') AND ' +
                '(g.FactorType in (8,9)) AND '+
                //'(SlurryIdentifier=' + IntToStr(-1) + ') AND ' +
                '(g.UDGIdentifier=' + IntToStr(APlanningUnderGroundMine.Identifier) + ')';
                //'(OCIdentifier=' + IntToStr(-1) + ')';
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        while NOT LDataSet.DataSet.Eof do
        begin
          LIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNoOfYears := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
          LGrowthFactors:= LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
          LNoOfPoints := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
          LInterpolation := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
          LFactorType := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
          LDescription := LDataSet.DataSet.FieldByName('Description').AsString;
          LTempGrowthFactor := APlanningUnderGroundMine.NewGrowthFactor;
          LTempGrowthFactor.Populate(LIdentifier,APlanningUnderGroundMine.MineIdentifier, 0,
            0,APlanningUnderGroundMine.Identifier,LNoOfPoints,LFactorType, LInterpolation, LNoOfYears, LGrowthFactors,LDescription);
          LDataSet.DataSet.Next;
        end;
        Result := true;
     finally
       FreeAndNil(LDataSet);
     end;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMineLoadAgent.LoadLoadGeneration(
  APlanningUnderGroundMine: TPlanningUnderGroundMine): boolean;
const OPNAME = 'TPlanningMineLoadAgent.LoadLoadGeneration';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier,
  LMineIdentifier: Integer;
  LOpenCastIdentifier: Integer;
  LUNGIdentifier: Integer;
  LSlurryDumpIdentifier: Integer;
  LLoadGenerationType : Integer;
  LStdDeviation: double;
  LFlow: TElevationsArray;
  LMeanOfSalt: TElevationsArray;
  LTempLoadGeneration : TLoadGeneration;
  LSQL, LFieldName : string;
  LIndex : integer;

begin
  Result:= false;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    SetLength(LFlow,10);
    SetLength(LMeanOfSalt,10);

    try
      LSQL := ' SELECT * FROM (SELECT DISTINCT MineLoadGenerationFlow.Identifier,' +
              ' MineLoadGenerationFlow.MineIdentifier, MineLoadGenerationFlow.OpenCastIdentifier,' +
              ' MineLoadGenerationFlow.UDGIdentifier, MineLoadGenerationFlow.SDIdentifier,  ' +
              ' MineLoadGenerationFlow.LoadGenType, MineLoadGenerationFlow.StdDeviation, Flow01, Flow02,' +
              ' Flow03, Flow04, Flow05, Flow06, Flow07, Flow08, Flow09, Flow10, ' +
              ' MeanOfSalt01, MeanOfSalt02, MeanOfSalt03, MeanOfSalt04, MeanOfSalt05, MeanOfSalt06, MeanOfSalt07, ' +
              ' MeanOfSalt08, MeanOfSalt09, MeanOfSalt10 FROM MineLoadGenerationMeanOfSalt' +
              ' LEFT JOIN  MineLoadGenerationFlow  ON (MineLoadGenerationFlow.Identifier = MineLoadGenerationMeanOfSalt.Identifier ) ' +
              ' WHERE ((MineLoadGenerationFlow.Model=' + QuotedStr(FAppModules.StudyArea.ModelCode) + ') And ' +              ' (MineLoadGenerationFlow.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') And ' +
              ' (MineLoadGenerationFlow.SubArea=' + QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') And ' +
              ' (MineLoadGenerationFlow.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') And' +
              ' (MineLoadGenerationFlow.MineIdentifier=' + IntToStr(APlanningUnderGroundMine.MineIdentifier) + ') And ' +
              ' (MineLoadGenerationFlow.UDGIdentifier=' + IntToStr(APlanningUnderGroundMine.Identifier) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.Model='+ QuotedStr(FAppModules.StudyArea.ModelCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.StudyAreaName=' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.SubArea='+ QuotedStr(FAppModules.StudyArea.SubAreaCode) + ') And  ' +
              ' (MineLoadGenerationMeanOfSalt.Scenario=' + QuotedStr(FAppModules.StudyArea.ScenarioCode) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.MineIdentifier=' + IntToStr(APlanningUnderGroundMine.MineIdentifier) + ') And ' +
              ' (MineLoadGenerationMeanOfSalt.UDGIdentifier=' + IntToStr(APlanningUnderGroundMine.Identifier) + ') )) AS A LEFT JOIN MineLoadGenerationType ON ( A.LoadGenType = MineLoadGenerationType.LoadGenType)';
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      while (NOT LDataSet.DataSet.Eof) do
      begin
        Lidentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
        LMineIdentifier := LDataSet.DataSet.FieldByName('MineIdentifier').AsInteger;
        LOpenCastIdentifier := LDataSet.DataSet.FieldByName('OpenCastIdentifier').AsInteger;
        LUNGIdentifier := LDataSet.DataSet.FieldByName('UDGIdentifier').AsInteger;
        LSlurryDumpIdentifier := LDataSet.DataSet.FieldByName('SDIdentifier').AsInteger;
        LLoadGenerationType := LDataSet.DataSet.FieldByName('A.LoadGenType').AsInteger;
        LStdDeviation := LDataSet.DataSet.FieldByName('StdDeviation').AsFloat;
        for LIndex := Low(LFlow) to High(LFlow) do
        begin
          LFieldName := Format('%s%2.2d',['Flow',LIndex+1]);
          LFlow[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          LFieldName := Format('%s%2.2d',['MeanOfSalt',LIndex+1]);
          LMeanOfSalt[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        LTempLoadGeneration := APlanningUnderGroundMine.NewLoadGeneration;
        LFieldName := LDataSet.DataSet.FieldByName('Description').AsString;
        LTempLoadGeneration.Populate(LIdentifier,LMineIdentifier,LOpenCastIdentifier, LUNGIdentifier,LSlurryDumpIdentifier,
        LLoadGenerationType,LStdDeviation,LFlow,LMeanOfSalt, LFieldName);
        LDataSet.DataSet.Next;
      end;
    finally
     LDataSet.DataSet.Free;
     Finalize(LFlow);
     Finalize(LMeanOfSalt);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
