//
//
//  UNIT      : Contains TFileMIMMDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 12/03/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFileMIMMDatabaseAgent;

interface

uses
  Classes,
  sysutils,
  Db,

  // DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UMIMMFileObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects,
  UYieldModelDataObject;

type

  TGrowthType = (gtMine,gtOpenCast, gtUndergroung, gtSlurryDump);
  TLoadType = (lgOpenCast, lgUndergroung, lgSlurryDump);
  TFileMIMMDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadMineDataSQL(MineID : integer): string;
    function ReadOpenCastDataSQL(AMineID: integer): string;
    function ReadUnderGroundDataSQL(AMineID: integer): string;
    function ReadSlurryDumpDataSQL(AMineID: integer): string;
    function ReadLakeEvaporationDataSQL(AMineID: integer): string;
    function ReadPanEvaporationDataSQL(AMineID: integer): string;
    function ReadUGUpstreamRunoffDataSQL(AMineID,AUnderGroundID: integer): string;
    function ReadRechargeFactorDataSQL(AMineID,ARechargeFactorParentType,AParentIdentifier,ARechargeFactorType: integer): string;
    function ReadMineSubCatchmentDataSQL: string;
    function ReadMineSubCatchmentFlowVolumeDataSQL(AMineSubCatchmentID: integer): string;
    function ReadMIMMUnkownDataSQL: string;

    function ReadMineGrowthFactorDataSQL(AMineID, AOCID, AUGID, ASDID: integer; AGrowthType : TGrowthType): string;
    function ReadLoadGenerationDataSQL(AMineID, AOCID, AUGID, ASDID: integer;ALoadType : TLoadType): string;
    function GetMaxGrowthFactorID(MineID : integer): integer;
    function GetMaxLoadGenerationID(MineID : integer): integer;


    function WriteMineDataSQL: string;
    function WriteOpenCastDataSQL: string;
    function WriteUnderGroundDataSQL: string;
    function WriteSlurryDumpDataSQL: string;
    function WriteLakeEvaporationDataSQL: string;
    function WritePanEvaporationDataSQL: string;
    function WriteUGUpstreamRunoffDataSQL: string;
    function WriteRechargeFactorDataSQL: string;
    function WriteMIMMUnkownDataSQL: string;
    function WriteMineSubCatchmentDataSQL: string;
    function WriteMineSubCatchmentFlowVolumeDataSQL: string;

    function WriteGrowthFactorDataSQL: string;
    function WriteLoadGenerationFlowDataSQL: string;
    function WriteLoadGenerationMeanOfSaltsDataSQL: string;


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

uses
  System.Contnrs,
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

function TFileMIMMDatabaseAgent.ReadMineDataSQL(MineID : integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadMineDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              '  Model,StudyAreaName,SubArea,Scenario,Identifier'+
              '  ,NodeNumber,MineName,RiverChannelNumber,PCDChannelNumber'+
              '  ,HydrologyNodeNumber,BeneficiationPlantArea,BeneficiationRunOffFactor'+
              '  ,SaltWashoffNo,RainfallFileName,MeanAnnPrecip,SaltBuildUpRate,SaltWashOffEfficiencyFactor,IniSaltStore'+
              ' FROM Mine WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND' +
              ' (Identifier       =' + IntToStr(MineID) + ')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileMIMMDatabaseAgent.GetMaxGrowthFactorID(MineID : integer): integer;
const OPNAME = 'TFileMIMMDatabaseAgent.GetMaxGrowthFactorID';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := ' SELECT MAX(Identifier) AS MaxID FROM MineGrowthFactors ' +
                ' WHERE Model=' +QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND StudyAreaName=' +QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                ' AND SubArea=' +QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                ' AND Scenario= ' +QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                ' AND MineIdentifier=' +IntToStr(MineID);

        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        result := LDataSet.DataSet.FieldByName('MaxID').AsInteger;
      end;

    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TFileMIMMDatabaseAgent.GetMaxLoadGenerationID(MineID : integer): integer;
const OPNAME = 'TFileMIMMDatabaseAgent.GetMaxLoadGenerationID';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := ' SELECT MAX(Identifier) AS MaxID FROM MineLoadGenerationFlow ' +
                ' WHERE Model=' +QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND StudyAreaName=' +QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                ' AND SubArea=' +QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                ' AND Scenario= ' +QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                ' AND MineIdentifier=' +IntToStr(MineID);

        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        result := LDataSet.DataSet.FieldByName('MaxID').AsInteger;
      end;

    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TFileMIMMDatabaseAgent.ReadLakeEvaporationDataSQL(AMineID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadLakeEvaporationDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier'+
              ' ,LakeEvaporation01,LakeEvaporation02,LakeEvaporation03,LakeEvaporation04,LakeEvaporation05'+
              ' ,LakeEvaporation06,LakeEvaporation07,LakeEvaporation08,LakeEvaporation09,LakeEvaporation10'+
              ' ,LakeEvaporation11,LakeEvaporation12'+
              ' FROM MineLakeEvaporation WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadPanEvaporationDataSQL(AMineID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadPanEvaporationDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier'+
              ' ,PanEvaporation01,PanEvaporation02,PanEvaporation03,PanEvaporation04,PanEvaporation05'+
              ' ,PanEvaporation06,PanEvaporation07,PanEvaporation08,PanEvaporation09,PanEvaporation10'+
              ' ,PanEvaporation11,PanEvaporation12'+
              ' FROM MinePanEvaporation WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadOpenCastDataSQL(AMineID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadOpenCastDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,PitName,CoalReserveArea,WorkingsArea,DisturbedWorkingsArea,DisturbedArea'+
              ' ,WaterSurfaceEvapArea,DisturbedAreaRunOff,DisturbedWorkingsAreaRunOff'+
              ' ,DecantVolume,SeepageVolume,AnalysisStartVolume,MaximumSeepageRate'+
              ' ,SeepageExponent,PCDSurfaceArea,PCDStorageCapacity,PCDAnalysisStartVolume, Abstraction'+
              ' ,PCDIniConcentration, WorkingCommYear,WorkingCommMonth,WorkingDecommYear'+
              ' ,WorkingDecommMonth,RunoffSaltWashOffEfficiencyFactor,IniSaltStore,ReChargeRate'+
              ' ,AbstractToEvap,AbstractToRiver, AbstractToPCD,AbstractMonthTimeSeriesFile'+
              ' FROM MineOpenCast WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileMIMMDatabaseAgent.ReadMineGrowthFactorDataSQL(AMineID, AOCID, AUGID, ASDID: integer;AGrowthType : TGrowthType): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadMineGrowthFactorDataSQL';
var
  LGTHTypeSQL : string;
begin
  Result := '';
  try
     case AGrowthType of
       gtMine : LGTHTypeSQL := ' (FactorType       = 1)';
       gtOpenCast : LGTHTypeSQL := ' (FactorType  in (2,3,4,5,6,7))';
       gtUndergroung : LGTHTypeSQL := ' (FactorType in (8,9))';
       gtSlurryDump : LGTHTypeSQL := ' (FactorType = 10)';
     end;
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier '+
              '        , NoOfPoints, FactorType, InterpolationMethod,NoOfYears, GrowthFactors '+
              ' FROM MineGrowthFactors WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +') AND'+
              ' (OCIdentifier     =' + IntTostr(AOCID)  +') AND'+
              ' (UDGIdentifier     =' + IntTostr(AUGID)  +') AND'+
              ' (SlurryIdentifier =' + IntTostr(ASDID)  +') AND'+ LGTHTypeSQL+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TFileMIMMDatabaseAgent.ReadLoadGenerationDataSQL(AMineID, AOCID, AUGID, ASDID: integer;ALoadType : TLoadType): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadLoadGenerationDataSQL';
var
  LLGTypeSQL : string;
begin
  Result := '';
  try
     case ALoadType of
       lgOpenCast : LLGTypeSQL := ' (f.LoadGenType  in (1,2)) AND';
       lgUndergroung : LLGTypeSQL := ' (f.LoadGenType =3) AND';
       lgSlurryDump : LLGTypeSQL := ' (f.LoadGenType = 4) AND';
     end;

    Result := ' SELECT f.Model,f.StudyAreaName,f.SubArea,f.Scenario,f.MineIdentifier,f.Identifier,'+
              '  f.OpenCastIdentifier,f.UDGIdentifier,f.SDIdentifier, f.LoadGenType, f.StdDeviation, '+
              '  f.Flow01,f.Flow02 ,f.Flow03,f.Flow04,f.Flow05,f.Flow06,f.Flow07,f.Flow08,f.Flow09,f.Flow10,'+
              '  m.MeanOfSalt01,m.MeanOfSalt02,m.MeanOfSalt03,m.MeanOfSalt04,m.MeanOfSalt05,m.MeanOfSalt06,'+
              '  m.MeanOfSalt07,m.MeanOfSalt08,m.MeanOfSalt09,m.MeanOfSalt10 '+
              ' FROM MineLoadGenerationFlow f, MineLoadGenerationMeanOfSalt m WHERE'+
              ' (f.Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)     +') AND'+
              ' (f.StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (f.SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (f.Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (f.MineIdentifier   =' + IntTostr(AMineID)  +') AND'+
              ' (f.OpenCastIdentifier     =' + IntTostr(AOCID)  +') AND'+
              ' (f.UDGIdentifier     =' + IntTostr(AUGID)  +') AND'+
              ' (f.SDIdentifier =' + IntTostr(ASDID)  +') AND'+ LLGTypeSQL+
              ' (f.LoadGenType = m.LoadGenType) AND'+
              ' (f.Model  = m.Model) AND'+
              ' (f.StudyAreaName = m.StudyAreaName )AND '+
              ' (f.SubArea  = m.SubArea         ) AND '+
              ' (f.Scenario   = m.Scenario       ) AND '+
              ' (f.MineIdentifier = m.MineIdentifier  ) AND '+
              ' (f.OpenCastIdentifier = m.OpenCastIdentifier ) AND '+
              ' (f.UDGIdentifier  = m.UDGIdentifier   ) AND'+
              ' (f.SDIdentifier   = m.SDIdentifier  ) '+
              ' ORDER BY f.Model,f.StudyAreaName,f.SubArea,f.Scenario,f.MineIdentifier,f.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileMIMMDatabaseAgent.ReadUnderGroundDataSQL(AMineID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadUnderGroundDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,UnderGroundSectionName,ChannelNumberToUGDam,UpstreamCatchmentArea,BoardPillarCatchmentArea'+
              ' ,HighExtractionCatchmentArea,HighExtractionAreaRunoffFactor'+

              ' FROM MineUnderGround WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadSlurryDumpDataSQL(AMineID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadSlurryDumpDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,DumpName,DumpSurfaceArea,RunoffFactorToPCD,SeepageSplitFactor'+
              ' ,PCDStorageCapacity,PCDSurfaceArea,PCDAnalysisStartVolume'+
              ' FROM MineSlurryDump WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadRechargeFactorDataSQL(
         AMineID,ARechargeFactorParentType,AParentIdentifier,ARechargeFactorType: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadRechargeFactorDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier'+
              ' ,RechargeFactorParentType,ParentIdentifier,RechargeFactorType'+
              ' ,RechargeFactor01,RechargeFactor02,RechargeFactor03,RechargeFactor04,RechargeFactor05'+
              ' ,RechargeFactor06,RechargeFactor07,RechargeFactor08,RechargeFactor09,RechargeFactor10'+
              ' ,RechargeFactor11,RechargeFactor12'+
              ' FROM MineRechargeFactor WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier             =' + IntTostr(AMineID) +') AND'+
              ' (RechargeFactorParentType   =' + IntTostr(ARechargeFactorParentType) +') AND'+
              ' (ParentIdentifier           =' + IntTostr(AParentIdentifier) +') AND'+
              ' (RechargeFactorType         =' + IntTostr(ARechargeFactorType)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadUGUpstreamRunoffDataSQL(AMineID,AUnderGroundID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadUGUpstreamRunoffDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier,UGIdentifier'+
              ' ,RunoffFactor01,RunoffFactor02,RunoffFactor03,RunoffFactor04,RunoffFactor05'+
              ' ,RunoffFactor06,RunoffFactor07,RunoffFactor08,RunoffFactor09,RunoffFactor10'+
              ' ,RunoffFactor11,RunoffFactor12'+
              ' FROM MineUGUpstreamRunoff WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID) +') AND'+
              ' (UGIdentifier     =' + IntTostr(AUnderGroundID)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadMIMMUnkownDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadMIMMUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData,FileType'+
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

function TFileMIMMDatabaseAgent.WriteMineDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteMineDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO Mine'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              '  ,NodeNumber,MineName,RiverChannelNumber,PCDChannelNumber'+
              '  ,HydrologyNodeNumber,BeneficiationPlantArea,BeneficiationRunOffFactor'+
              '  ,SaltWashoffNo,RainfallFileName,MeanAnnPrecip,SaltBuildUpRate,SaltWashOffEfficiencyFactor,IniSaltStore)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              '  ,:NodeNumber,:MineName,:RiverChannelNumber,:PCDChannelNumber'+
              '  ,:HydrologyNodeNumber,:BeneficiationPlantArea,:BeneficiationRunOffFactor'+
              '  ,:SaltWashoffNo,:RainfallFileName,:MeanAnnPrecip,:SaltBuildUpRate,:SaltWashOffEfficiencyFactor,:IniSaltStore)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteLakeEvaporationDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteLakeEvaporationDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineLakeEvaporation'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier'+
              ' ,LakeEvaporation01,LakeEvaporation02,LakeEvaporation03,LakeEvaporation04,LakeEvaporation05'+
              ' ,LakeEvaporation06,LakeEvaporation07,LakeEvaporation08,LakeEvaporation09,LakeEvaporation10'+
              ' ,LakeEvaporation11,LakeEvaporation12)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier'+
              ' ,:LakeEvaporation01,:LakeEvaporation02,:LakeEvaporation03,:LakeEvaporation04,:LakeEvaporation05'+
              ' ,:LakeEvaporation06,:LakeEvaporation07,:LakeEvaporation08,:LakeEvaporation09,:LakeEvaporation10'+
              ' ,:LakeEvaporation11,:LakeEvaporation12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileMIMMDatabaseAgent.WriteGrowthFactorDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteGrowthFactorDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineGrowthFactors'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,MineIdentifier,OCIdentifier'+
              ' ,SlurryIdentifier,UDGIdentifier,NoOfPoints,FactorType,InterpolationMethod'+
              ' ,NoOfYears,GrowthFactors)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:MineIdentifier,:OCIdentifier'+
              ' ,:SlurryIdentifier,:UDGIdentifier,:NoOfPoints,:FactorType,:InterpolationMethod'+
              ' ,:NoOfYears,:GrowthFactors)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteLoadGenerationFlowDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteLoadGenerationFlowDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineLoadGenerationFlow'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,MineIdentifier,LoadGenType,OpenCastIdentifier'+
              ' ,UDGIdentifier,SDIdentifier,StdDeviation,Flow01,Flow02'+
              ' ,Flow03,Flow04,Flow05,Flow06,Flow07,Flow08,Flow09,Flow10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:MineIdentifier'+
              ' ,:LoadGenType,:OpenCastIdentifier'+
              ' ,:UDGIdentifier,:SDIdentifier,:StdDeviation,:Flow01,:Flow02'+
              ' ,:Flow03,:Flow04,:Flow05,:Flow06,:Flow07,:Flow08,:Flow09,:Flow10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;




function TFileMIMMDatabaseAgent.WriteLoadGenerationMeanOfSaltsDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteLoadGenerationMeanOfSaltsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineLoadGenerationMeanOfSalt'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,MineIdentifier,LoadGenType,OpenCastIdentifier'+
              ' ,UDGIdentifier,SDIdentifier,StdDeviation,MeanOfSalt01,MeanOfSalt02'+
              ' ,MeanOfSalt03,MeanOfSalt04,MeanOfSalt05,MeanOfSalt06,MeanOfSalt07,MeanOfSalt08,MeanOfSalt09,MeanOfSalt10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:MineIdentifier,:LoadGenType'+
              ' ,:OpenCastIdentifier  '+
              ' ,:UDGIdentifier,:SDIdentifier,:StdDeviation,:MeanOfSalt01,:MeanOfSalt02'+
              ' ,:MeanOfSalt03,:MeanOfSalt04,:MeanOfSalt05,:MeanOfSalt06,:MeanOfSalt07,:MeanOfSalt08,:MeanOfSalt09,:MeanOfSalt10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WritePanEvaporationDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WritePanEvaporationDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MinePanEvaporation'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier'+
              ' ,PanEvaporation01,PanEvaporation02,PanEvaporation03,PanEvaporation04,PanEvaporation05'+
              ' ,PanEvaporation06,PanEvaporation07,PanEvaporation08,PanEvaporation09,PanEvaporation10'+
              ' ,PanEvaporation11,PanEvaporation12)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier'+
              ' ,:PanEvaporation01,:PanEvaporation02,:PanEvaporation03,:PanEvaporation04,:PanEvaporation05'+
              ' ,:PanEvaporation06,:PanEvaporation07,:PanEvaporation08,:PanEvaporation09,:PanEvaporation10'+
              ' ,:PanEvaporation11,:PanEvaporation12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteOpenCastDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteOpenCastDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineOpenCast'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,PitName,CoalReserveArea,WorkingsArea,DisturbedWorkingsArea,DisturbedArea'+
              ' ,WaterSurfaceEvapArea,DisturbedAreaRunOff,DisturbedWorkingsAreaRunOff'+
              ' ,DecantVolume,SeepageVolume,AnalysisStartVolume,MaximumSeepageRate'+
              ' ,SeepageExponent,PCDSurfaceArea,PCDStorageCapacity,PCDAnalysisStartVolume, Abstraction'+
              ' ,PCDIniConcentration, WorkingCommYear,WorkingCommMonth,WorkingDecommYear'+
              ' ,WorkingDecommMonth,RunoffSaltWashOffEfficiencyFactor,IniSaltStore,ReChargeRate'+
              ' ,AbstractToEvap,AbstractToRiver, AbstractToPCD,AbstractMonthTimeSeriesFile)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier,:Identifier'+
              ' ,:PitName,:CoalReserveArea,:WorkingsArea,:DisturbedWorkingsArea,:DisturbedArea'+
              ' ,:WaterSurfaceEvapArea,:DisturbedAreaRunOff,:DisturbedWorkingsAreaRunOff'+
              ' ,:DecantVolume,:SeepageVolume,:AnalysisStartVolume,:MaximumSeepageRate'+
              ' ,:SeepageExponent,:PCDSurfaceArea,:PCDStorageCapacity,:PCDAnalysisStartVolume,:Abstraction'+
              ' ,:PCDIniConcentration, :WorkingCommYear,:WorkingCommMonth,:WorkingDecommYear'+
              ' ,:WorkingDecommMonth,:RunoffSaltWashOffEfficiencyFactor,:IniSaltStore,:ReChargeRate'+
              ' ,:AbstractToEvap,:AbstractToRiver, :AbstractToPCD,:AbstractMonthTimeSeriesFile)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteUnderGroundDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteUnderGroundDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineUnderGround'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,UnderGroundSectionName,ChannelNumberToUGDam,UpstreamCatchmentArea'+
              ' ,BoardPillarCatchmentArea,HighExtractionCatchmentArea,HighExtractionAreaRunoffFactor)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier,:Identifier'+
              ' ,:UnderGroundSectionName,:ChannelNumberToUGDam,:UpstreamCatchmentArea'+
              ' ,:BoardPillarCatchmentArea,:HighExtractionCatchmentArea,:HighExtractionAreaRunoffFactor)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteSlurryDumpDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteSlurryDumpDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineSlurryDump'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,DumpName,DumpSurfaceArea,RunoffFactorToPCD,SeepageSplitFactor'+
              ' ,PCDStorageCapacity,PCDSurfaceArea,PCDAnalysisStartVolume)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier,:Identifier'+
              ' ,:DumpName,:DumpSurfaceArea,:RunoffFactorToPCD,:SeepageSplitFactor'+
              ' ,:PCDStorageCapacity,:PCDSurfaceArea,:PCDAnalysisStartVolume)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteRechargeFactorDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteRechargeFactorDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineRechargeFactor'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier'+
              ' ,RechargeFactorParentType,ParentIdentifier,RechargeFactorType'+
              ' ,RechargeFactor01,RechargeFactor02,RechargeFactor03,RechargeFactor04,RechargeFactor05'+
              ' ,RechargeFactor06,RechargeFactor07,RechargeFactor08,RechargeFactor09,RechargeFactor10'+
              ' ,RechargeFactor11,RechargeFactor12)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier'+
              ' ,:RechargeFactorParentType,:ParentIdentifier,:RechargeFactorType'+
              ' ,:RechargeFactor01,:RechargeFactor02,:RechargeFactor03,:RechargeFactor04,:RechargeFactor05'+
              ' ,:RechargeFactor06,:RechargeFactor07,:RechargeFactor08,:RechargeFactor09,:RechargeFactor10'+
              ' ,:RechargeFactor11,:RechargeFactor12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteUGUpstreamRunoffDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteUGUpstreamRunoffDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineUGUpstreamRunoff'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier,UGIdentifier'+
              ' ,RunoffFactor01,RunoffFactor02,RunoffFactor03,RunoffFactor04,RunoffFactor05'+
              ' ,RunoffFactor06,RunoffFactor07,RunoffFactor08,RunoffFactor09,RunoffFactor10'+
              ' ,RunoffFactor11,RunoffFactor12)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier,:UGIdentifier'+
              ' ,:RunoffFactor01,:RunoffFactor02,:RunoffFactor03,:RunoffFactor04,:RunoffFactor05'+
              ' ,:RunoffFactor06,:RunoffFactor07,:RunoffFactor08,:RunoffFactor09,:RunoffFactor10'+
              ' ,:RunoffFactor11,:RunoffFactor12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteMIMMUnkownDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteMIMMUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet,
  LMineDataSet,
  LMiningDataSet,
  LMonthlyValuesDataSet    : TAbstractModelDataset;
  LOpenCast                : TMIMMOpenCastFileObject;
  LUndeground              : TMIMMUndegroundFileObject;
  LSlurryDump              : TMIMMSlurryDumpFileObject;
  LMine                    : TMIMMFileObject;
  LMineList                : TMIMMListFileObject;
  LGrowthFactors           : TGrowthFactors;
  LLoadGeneration          : TLoadGeneration;
  LIndex                   : integer;
  LMineIdentifier          : integer;
  LOpenCastIdentifier      : integer;
  LUnderGroundIdentifier   : integer;
  LSlurryDumpIdentifier    : integer;
  LStop                    : boolean;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileMIMMDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    //LMineList := ADataObject.FMineListFileObject;
    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LMineList    := LPlanningFileDataObject.AddMIMMFileObject(AFilename.FileNumber);
    if LMineList = nil then
      Exit;
    if not LMineList.Initialise then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMiningDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyValuesDataSet);
    //FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineSubCatchmentDataset);
    try
      LMineDataSet.SetSQL(ReadMineDataSQL(AFilename.FileNumber));
      LMineDataSet.DataSet.Open;

      //Check if there is any data.
      if (LMineDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileMIMMDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LMineDataSet.DataSet.Eof do
        begin

          LMine := LMineList.AddMineFileObject;

          LMineIdentifier := LMineDataSet.DataSet.FieldByName('Identifier').AsInteger;

          if not LMineDataSet.DataSet.FieldByName('NodeNumber').IsNull then
          begin
            LMine.NodeNumber.FData :=LMineDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
            LMine.NodeNumber.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('MineName').IsNull then
          begin
            LMine.MineName.FData := Trim(LMineDataSet.DataSet.FieldByName('MineName').AsString);
            LMine.MineName.FInitalised := True;
          end;



          if not LMineDataSet.DataSet.FieldByName('SaltWashoffNo').IsNull then
          begin
            LMine.FAssSaltsWashoffNo.FData :=LMineDataSet.DataSet.FieldByName('SaltWashoffNo').AsInteger;
            LMine.FAssSaltsWashoffNo.FInitalised := True;
          end;


          if not LMineDataSet.DataSet.FieldByName('RainfallFileName').IsNull then
          begin
            LMine.FRainfallFileName.FData :=LMineDataSet.DataSet.FieldByName('RainfallFileName').AsString;
            LMine.FRainfallFileName.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('MeanAnnPrecip').IsNull then
          begin
            LMine.FRainfallMAP.FData :=LMineDataSet.DataSet.FieldByName('MeanAnnPrecip').AsFloat;
            LMine.FRainfallMAP.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('SaltBuildUpRate').IsNull then
          begin
            LMine.FSaltsBuildUpRate.FData :=LMineDataSet.DataSet.FieldByName('SaltBuildUpRate').AsFloat;
            LMine.FSaltsBuildUpRate.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('SaltWashOffEfficiencyFactor').IsNull then
          begin
            LMine.FSaltsWashoffEfficiencyFactor.FData :=LMineDataSet.DataSet.FieldByName('SaltWashOffEfficiencyFactor').AsFloat;
            LMine.FSaltsWashoffEfficiencyFactor.FInitalised := True;
          end;

           if not LMineDataSet.DataSet.FieldByName('IniSaltStore').IsNull then
          begin
            LMine.FIniSaltStore.FData :=LMineDataSet.DataSet.FieldByName('IniSaltStore').AsFloat;
            LMine.FIniSaltStore.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('RiverChannelNumber').IsNull then
          begin
            LMine.RiverChannelNumber.FData :=LMineDataSet.DataSet.FieldByName('RiverChannelNumber').AsInteger;
            LMine.RiverChannelNumber.FInitalised := True;
          end;


          if not LMineDataSet.DataSet.FieldByName('PCDChannelNumber').IsNull then
          begin
            LMine.PCDChannelNumber.FData :=LMineDataSet.DataSet.FieldByName('PCDChannelNumber').AsInteger;
            LMine.PCDChannelNumber.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('HydrologyNodeNumber').IsNull then
          begin
            LMine.HydrologyNodeNumber.FData :=LMineDataSet.DataSet.FieldByName('HydrologyNodeNumber').AsInteger;
            LMine.HydrologyNodeNumber.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('BeneficiationPlantArea').IsNull then
          begin
            LMine.BeneficiationPlantArea.FData :=LMineDataSet.DataSet.FieldByName('BeneficiationPlantArea').AsFloat;
            LMine.BeneficiationPlantArea.FInitalised := True;
          end;

          if not LMineDataSet.DataSet.FieldByName('BeneficiationRunOffFactor').IsNull then
          begin
            LMine.BeneficiationRunOffFactor.FData :=LMineDataSet.DataSet.FieldByName('BeneficiationRunOffFactor').AsFloat;
            LMine.BeneficiationRunOffFactor.FInitalised := True;
          end;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(ReadPanEvaporationDataSQL(LMineIdentifier));
          LMonthlyValuesDataSet.DataSet.Open;
          if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
          begin
            for LIndex := 1 to 12 do
            begin
              LFieldName := Format('%s%2.2d',['PanEvaporation',LIndex]);
              if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LMine.PanEvaporation[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                LMine.PanEvaporation[LIndex].FInitalised := True;
              end;
            end;
          end;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(ReadLakeEvaporationDataSQL(LMineIdentifier));
          LMonthlyValuesDataSet.DataSet.Open;
          if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
          begin
            for LIndex := 1 to 12 do
            begin
              LFieldName := Format('%s%2.2d',['LakeEvaporation',LIndex]);
              if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LMine.LakeEvaporation[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                LMine.LakeEvaporation[LIndex].FInitalised := True;
              end;
            end;
          end;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(ReadMineGrowthFactorDataSQL(LMineIdentifier,0,0,0,gtMine));
          LDataSet.DataSet.Open;
          while not LDataSet.DataSet.Eof do
          begin
            LGrowthFactors := LMine.AddMIMMGrowthFactors;
            if LGrowthFactors <> nil then
            begin
               LGrowthFactors.Initialise;
               if not LDataSet.DataSet.FieldByName('NoOfPoints').IsNull then
               begin
                 LGrowthFactors.FNoOfPoints.FData := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
                 LGrowthFactors.FNoOfPoints.FInitalised := True;
               end;

               if not LDataSet.DataSet.FieldByName('FactorType').IsNull then
               begin
                 LGrowthFactors.FGrowthCode.FData := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
                 LGrowthFactors.FGrowthCode.FInitalised := True;
               end;

               if not LDataSet.DataSet.FieldByName('InterpolationMethod').IsNull then
               begin
                 LGrowthFactors.FInterpolationMethod.FData := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
                 LGrowthFactors.FInterpolationMethod.FInitalised := True;
               end;

               if not LDataSet.DataSet.FieldByName('NoOfYears').IsNull then
               begin
                 LGrowthFactors.FYearDatapoints.FData := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
                 LGrowthFactors.FYearDatapoints.FInitalised := True;
               end;

               if not LDataSet.DataSet.FieldByName('GrowthFactors').IsNull then
               begin
                 LGrowthFactors.FGrowthFactors.FData := LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
                 LGrowthFactors.FGrowthFactors.FInitalised := True;
               end;
            end;
            LDataSet.DataSet.Next;
          end;

          LMiningDataSet.DataSet.Close;
          LMiningDataSet.SetSQL(ReadOpenCastDataSQL(LMineIdentifier));
          LMiningDataSet.DataSet.Open;

          while not LMiningDataSet.DataSet.Eof do
          begin
            LOpenCast := LMine.AddOpenCast;

            LOpenCastIdentifier := LMiningDataSet.DataSet.FieldByName('Identifier').AsInteger;

            if not LMiningDataSet.DataSet.FieldByName('PitName').IsNull then
            begin
              LOpenCast.PitName.FData := Trim(LMiningDataSet.DataSet.FieldByName('PitName').AsString);
              LOpenCast.PitName.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('CoalReserveArea').IsNull then
            begin
              LOpenCast.CoalReserveArea.FData :=LMiningDataSet.DataSet.FieldByName('CoalReserveArea').AsFloat;
              LOpenCast.CoalReserveArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('WorkingsArea').IsNull then
            begin
              LOpenCast.WorkingsArea.FData :=LMiningDataSet.DataSet.FieldByName('WorkingsArea').AsFloat;
              LOpenCast.WorkingsArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('DisturbedWorkingsArea').IsNull then
            begin
              LOpenCast.DisturbedWorkingsArea.FData :=LMiningDataSet.DataSet.FieldByName('DisturbedWorkingsArea').AsFloat;
              LOpenCast.DisturbedWorkingsArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('DisturbedArea').IsNull then
            begin
              LOpenCast.DisturbedArea.FData :=LMiningDataSet.DataSet.FieldByName('DisturbedArea').AsFloat;
              LOpenCast.DisturbedArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('WaterSurfaceEvapArea').IsNull then
            begin
              LOpenCast.WaterSurfaceEvapArea.FData :=LMiningDataSet.DataSet.FieldByName('WaterSurfaceEvapArea').AsFloat;
              LOpenCast.WaterSurfaceEvapArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('DisturbedAreaRunOff').IsNull then
            begin
              LOpenCast.DisturbedAreaRunOff.FData :=LMiningDataSet.DataSet.FieldByName('DisturbedAreaRunOff').AsFloat;
              LOpenCast.DisturbedAreaRunOff.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('DisturbedWorkingsAreaRunOff').IsNull then
            begin
              LOpenCast.DisturbedWorkingsAreaRunOff.FData :=LMiningDataSet.DataSet.FieldByName('DisturbedWorkingsAreaRunOff').AsFloat;
              LOpenCast.DisturbedWorkingsAreaRunOff.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('DecantVolume').IsNull then
            begin
              LOpenCast.DecantVolume.FData :=LMiningDataSet.DataSet.FieldByName('DecantVolume').AsFloat;
              LOpenCast.DecantVolume.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('SeepageVolume').IsNull then
            begin
              LOpenCast.SeepageVolume.FData :=LMiningDataSet.DataSet.FieldByName('SeepageVolume').AsFloat;
              LOpenCast.SeepageVolume.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('AnalysisStartVolume').IsNull then
            begin
              LOpenCast.AnalysisStartVolume.FData :=LMiningDataSet.DataSet.FieldByName('AnalysisStartVolume').AsFloat;
              LOpenCast.AnalysisStartVolume.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('MaximumSeepageRate').IsNull then
            begin
              LOpenCast.MaximumSeepageRate.FData :=LMiningDataSet.DataSet.FieldByName('MaximumSeepageRate').AsFloat;
              LOpenCast.MaximumSeepageRate.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('SeepageExponent').IsNull then
            begin
              LOpenCast.SeepageExponent.FData :=LMiningDataSet.DataSet.FieldByName('SeepageExponent').AsFloat;
              LOpenCast.SeepageExponent.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDSurfaceArea').IsNull then
            begin
              LOpenCast.PCDSurfaceArea.FData :=LMiningDataSet.DataSet.FieldByName('PCDSurfaceArea').AsFloat;
              LOpenCast.PCDSurfaceArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDStorageCapacity').IsNull then
            begin
              LOpenCast.PCDStorageCapacity.FData :=LMiningDataSet.DataSet.FieldByName('PCDStorageCapacity').AsFloat;
              LOpenCast.PCDStorageCapacity.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDAnalysisStartVolume').IsNull then
            begin
              LOpenCast.PCDAnalysisStartVolume.FData :=LMiningDataSet.DataSet.FieldByName('PCDAnalysisStartVolume').AsFloat;
              LOpenCast.PCDAnalysisStartVolume.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('Abstraction').IsNull then
            begin
              LOpenCast.FAbstractionOpt.FData := Trim(LMiningDataSet.DataSet.FieldByName('Abstraction').AsString)[1];
              LOpenCast.FAbstractionOpt.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDIniConcentration').IsNull then
            begin
              LOpenCast.FIniConcentration.FData :=LMiningDataSet.DataSet.FieldByName('PCDIniConcentration').AsFloat;
              LOpenCast.FIniConcentration.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('WorkingCommYear').IsNull then
            begin
              LOpenCast.FWorkingCommYear.FData :=LMiningDataSet.DataSet.FieldByName('WorkingCommYear').AsInteger;
              LOpenCast.FWorkingCommYear.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('WorkingCommMonth').IsNull then
            begin
              LOpenCast.FWorkingCommMonth.FData :=LMiningDataSet.DataSet.FieldByName('WorkingCommMonth').AsInteger;
              LOpenCast.FWorkingCommMonth.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('WorkingDecommYear').IsNull then
            begin
              LOpenCast.FWorkingDecommYear.FData :=LMiningDataSet.DataSet.FieldByName('WorkingDecommYear').AsInteger;
              LOpenCast.FWorkingDecommYear.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('WorkingDecommMonth').IsNull then
            begin
              LOpenCast.FWorkingDecommMonth.FData :=LMiningDataSet.DataSet.FieldByName('WorkingDecommMonth').AsInteger;
              LOpenCast.FWorkingDecommMonth.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('RunoffSaltWashOffEfficiencyFactor').IsNull then
            begin
              LOpenCast.FRunoffSaltsWashoffEfficiencyFactor.FData :=LMiningDataSet.DataSet.FieldByName('RunoffSaltWashOffEfficiencyFactor').AsFloat;
              LOpenCast.FRunoffSaltsWashoffEfficiencyFactor.FInitalised := True;
            end;


            if not LMiningDataSet.DataSet.FieldByName('IniSaltStore').IsNull then
            begin
              LOpenCast.FIniSaltStore.FData :=LMiningDataSet.DataSet.FieldByName('IniSaltStore').AsFloat;
              LOpenCast.FIniSaltStore.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('ReChargeRate').IsNull then
            begin
              LOpenCast.FReChargeRate.FData :=LMiningDataSet.DataSet.FieldByName('ReChargeRate').AsFloat;
              LOpenCast.FReChargeRate.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('AbstractToEvap').IsNull then
            begin
              LOpenCast.FAbstractToEvap.FData :=LMiningDataSet.DataSet.FieldByName('AbstractToEvap').AsFloat;
              LOpenCast.FAbstractToEvap.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('AbstractToRiver').IsNull then
            begin
              LOpenCast.FAbstrctToRiver.FData :=LMiningDataSet.DataSet.FieldByName('AbstractToRiver').AsFloat;
              LOpenCast.FAbstrctToRiver.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('AbstractToPCD').IsNull then
            begin
              LOpenCast.FAbstrctToCPD.FData :=LMiningDataSet.DataSet.FieldByName('AbstractToPCD').AsFloat;
              LOpenCast.FAbstrctToCPD.FInitalised := True;
            end;

             if not LMiningDataSet.DataSet.FieldByName('AbstractMonthTimeSeriesFile').IsNull then
            begin
              LOpenCast.FAbstrctMonthTimeSeriesFile.FData := Trim(LMiningDataSet.DataSet.FieldByName('AbstractMonthTimeSeriesFile').AsString);
              LOpenCast.FAbstrctMonthTimeSeriesFile.FInitalised := True;
            end;



            LMonthlyValuesDataSet.DataSet.Close;
            LMonthlyValuesDataSet.SetSQL(ReadRechargeFactorDataSQL(LMineIdentifier,1,LOpenCastIdentifier,1));
            LMonthlyValuesDataSet.DataSet.Open;
            if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
            begin
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
                if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LOpenCast.DisturbedRechargeFactor[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LOpenCast.DisturbedRechargeFactor[LIndex].FInitalised := True;
                end;
              end;
            end;

            LMonthlyValuesDataSet.DataSet.Close;
            LMonthlyValuesDataSet.SetSQL(ReadRechargeFactorDataSQL(LMineIdentifier,1,LOpenCastIdentifier,2));
            LMonthlyValuesDataSet.DataSet.Open;
            if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
            begin
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
                if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LOpenCast.WorkingAreaRechargeFactor[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LOpenCast.WorkingAreaRechargeFactor[LIndex].FInitalised := True;
                end;
              end;
            end;


            LDataSet.DataSet.Close;
            LDataSet.SetSQL(ReadMineGrowthFactorDataSQL(LMineIdentifier,LOpenCastIdentifier,0,0,gtOpenCast));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LGrowthFactors := LOpenCast.AddGrowthFactors;
              if LGrowthFactors <> nil then
              begin
                LGrowthFactors.Initialise;
                 if not LDataSet.DataSet.FieldByName('NoOfPoints').IsNull then
                 begin
                   LGrowthFactors.FNoOfPoints.FData := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
                   LGrowthFactors.FNoOfPoints.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('FactorType').IsNull then
                 begin
                   LGrowthFactors.FGrowthCode.FData := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
                   LGrowthFactors.FGrowthCode.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('InterpolationMethod').IsNull then
                 begin
                   LGrowthFactors.FInterpolationMethod.FData := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
                   LGrowthFactors.FInterpolationMethod.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('NoOfYears').IsNull then
                 begin
                   LGrowthFactors.FYearDatapoints.FData := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
                   LGrowthFactors.FYearDatapoints.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('GrowthFactors').IsNull then
                 begin
                   LGrowthFactors.FGrowthFactors.FData := LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
                   LGrowthFactors.FGrowthFactors.FInitalised := True;
                 end;
              end;
              LDataSet.DataSet.Next;
            end;

            LDataSet.DataSet.Close;
            LDataSet.SetSQL(ReadLoadGenerationDataSQL(LMineIdentifier,LOpenCastIdentifier,0,0,lgOpenCast));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LLoadGeneration := LOpenCast.AddLoadGeneration;
              if LLoadGeneration <> nil then
              begin
                 LLoadGeneration.Initialise;
                 if not LDataSet.DataSet.FieldByName('LoadGenType').IsNull then
                 begin
                   LLoadGeneration.FLoadGenType.FData := LDataSet.DataSet.FieldByName('LoadGenType').AsInteger;
                   LLoadGeneration.FLoadGenType.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('StdDeviation').IsNull then
                 begin
                   LLoadGeneration.FStdDeviation.FData := LDataSet.DataSet.FieldByName('StdDeviation').AsFloat;
                   LLoadGeneration.FStdDeviation.FInitalised := True;
                 end;


                for LIndex := 1 to 10 do
                begin
                  LFieldName := Format('%s%2.2d',['Flow',LIndex]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LLoadGeneration.FFlow[LIndex].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LLoadGeneration.FFlow[LIndex].FInitalised := True;
                  end;
                end;

                for LIndex := 1 to 10 do
                begin
                  LFieldName := Format('%s%2.2d',['MeanOfSalt',LIndex]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LLoadGeneration.FMeanOfSalt[LIndex].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LLoadGeneration.FMeanOfSalt[LIndex].FInitalised := True;
                  end;
                end;

              end;
              LDataSet.DataSet.Next;

            end;


            LMiningDataSet.DataSet.Next;

          end;

          LMiningDataSet.DataSet.Close;
          LMiningDataSet.SetSQL(ReadUnderGroundDataSQL(LMineIdentifier));
          LMiningDataSet.DataSet.Open;
          while not LMiningDataSet.DataSet.Eof do
          begin
            LUndeground := LMine.AddUndeground;

            LUnderGroundIdentifier := LMiningDataSet.DataSet.FieldByName('Identifier').AsInteger;

            if not LMiningDataSet.DataSet.FieldByName('UnderGroundSectionName').IsNull then
            begin
              LUndeground.UnderGroundSectionName.FData := Trim(LMiningDataSet.DataSet.FieldByName('UnderGroundSectionName').AsString);
              LUndeground.UnderGroundSectionName.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('ChannelNumberToUGDam').IsNull then
            begin
              LUndeground.ChannelNumberToUGDam.FData :=LMiningDataSet.DataSet.FieldByName('ChannelNumberToUGDam').AsInteger;
              LUndeground.ChannelNumberToUGDam.FInitalised := True;
            end;

            //Read Line 19
            if not LMiningDataSet.DataSet.FieldByName('UpstreamCatchmentArea').IsNull then
            begin
              LUndeground.UpstreamCatchmentArea.FData :=LMiningDataSet.DataSet.FieldByName('UpstreamCatchmentArea').AsFloat;
              LUndeground.UpstreamCatchmentArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('BoardPillarCatchmentArea').IsNull then
            begin
              LUndeground.BoardPillarCatchmentArea.FData :=LMiningDataSet.DataSet.FieldByName('BoardPillarCatchmentArea').AsFloat;
              LUndeground.BoardPillarCatchmentArea.FInitalised := True;
            end;
            if not LMiningDataSet.DataSet.FieldByName('HighExtractionCatchmentArea').IsNull then
            begin
              LUndeground.HighExtractionCatchmentArea.FData :=LMiningDataSet.DataSet.FieldByName('HighExtractionCatchmentArea').AsFloat;
              LUndeground.HighExtractionCatchmentArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('HighExtractionAreaRunoffFactor').IsNull then
            begin
              LUndeground.HighExtractionAreaRunoffFactor.FData :=LMiningDataSet.DataSet.FieldByName('HighExtractionAreaRunoffFactor').AsFloat;
              LUndeground.HighExtractionAreaRunoffFactor.FInitalised := True;
            end;

            //Read Line 20
            LMonthlyValuesDataSet.DataSet.Close;
            LMonthlyValuesDataSet.SetSQL(ReadUGUpstreamRunoffDataSQL(LMineIdentifier,LUnderGroundIdentifier));
            LMonthlyValuesDataSet.DataSet.Open;
            if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
            begin
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['RunoffFactor',LIndex]);
                if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LUndeground.UpstreamRunoffPortion[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LUndeground.UpstreamRunoffPortion[LIndex].FInitalised := True;
                end;
              end;
            end;

            //Read Line 21
            LMonthlyValuesDataSet.DataSet.Close;
            LMonthlyValuesDataSet.SetSQL(ReadRechargeFactorDataSQL(LMineIdentifier,2,LUnderGroundIdentifier,3));
            LMonthlyValuesDataSet.DataSet.Open;
            if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
            begin
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
                if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LUndeground.BoardAndPilarRechargeFactor[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LUndeground.BoardAndPilarRechargeFactor[LIndex].FInitalised := True;
                end;
              end;
            end;

            //Read Line 22
            LMonthlyValuesDataSet.DataSet.Close;
            LMonthlyValuesDataSet.SetSQL(ReadRechargeFactorDataSQL(LMineIdentifier,2,LUnderGroundIdentifier,4));
            LMonthlyValuesDataSet.DataSet.Open;
            if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
            begin
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
                if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LUndeground.HighExtractionRechargeFactor[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LUndeground.HighExtractionRechargeFactor[LIndex].FInitalised := True;
                end;
              end;
            end;


            LDataSet.DataSet.Close;
            LDataSet.SetSQL(ReadMineGrowthFactorDataSQL(LMineIdentifier,0,LUnderGroundIdentifier,0,gtUndergroung));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LGrowthFactors := LUndeground.AddGrowthFactors;
              if LGrowthFactors <> nil then
              begin
                 LGrowthFactors.Initialise;
                 if not LDataSet.DataSet.FieldByName('NoOfPoints').IsNull then
                 begin
                   LGrowthFactors.FNoOfPoints.FData := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
                   LGrowthFactors.FNoOfPoints.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('FactorType').IsNull then
                 begin
                   LGrowthFactors.FGrowthCode.FData := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
                   LGrowthFactors.FGrowthCode.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('InterpolationMethod').IsNull then
                 begin
                   LGrowthFactors.FInterpolationMethod.FData := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
                   LGrowthFactors.FInterpolationMethod.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('NoOfYears').IsNull then
                 begin
                   LGrowthFactors.FYearDatapoints.FData := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
                   LGrowthFactors.FYearDatapoints.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('GrowthFactors').IsNull then
                 begin
                   LGrowthFactors.FGrowthFactors.FData := LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
                   LGrowthFactors.FGrowthFactors.FInitalised := True;
                 end;
              end;
              LDataSet.DataSet.Next;
            end;

            LDataSet.DataSet.Close;
            LDataSet.SetSQL(ReadLoadGenerationDataSQL(LMineIdentifier,0,LUnderGroundIdentifier,0,lgUndergroung));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LLoadGeneration := LUndeground.AddLoadGeneration;
              if LLoadGeneration <> nil then
              begin
                 LLoadGeneration.Initialise;
                 if not LDataSet.DataSet.FieldByName('LoadGenType').IsNull then
                 begin
                   LLoadGeneration.FLoadGenType.FData := LDataSet.DataSet.FieldByName('LoadGenType').AsInteger;
                   LLoadGeneration.FLoadGenType.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('StdDeviation').IsNull then
                 begin
                   LLoadGeneration.FStdDeviation.FData := LDataSet.DataSet.FieldByName('StdDeviation').AsFloat;
                   LLoadGeneration.FStdDeviation.FInitalised := True;
                 end;


                for LIndex := 1 to 10 do
                begin
                  LFieldName := Format('%s%2.2d',['Flow',LIndex]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LLoadGeneration.FFlow[LIndex].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LLoadGeneration.FFlow[LIndex].FInitalised := True;
                  end;
                end;

                for LIndex := 1 to 10 do
                begin
                  LFieldName := Format('%s%2.2d',['MeanOfSalt',LIndex]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LLoadGeneration.FMeanOfSalt[LIndex].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LLoadGeneration.FMeanOfSalt[LIndex].FInitalised := True;
                  end;
                end;

              end;
              LDataSet.DataSet.Next;

            end;

            LMiningDataSet.DataSet.Next;
          end;

          LMiningDataSet.DataSet.Close;
          LMiningDataSet.SetSQL(ReadSlurryDumpDataSQL(LMineIdentifier));
          LMiningDataSet.DataSet.Open;
          while not LMiningDataSet.DataSet.Eof do
          begin
            LSlurryDump := LMine.AddSlurryDump;

            LSlurryDumpIdentifier := LMiningDataSet.DataSet.FieldByName('Identifier').AsInteger;

            if not LMiningDataSet.DataSet.FieldByName('DumpName').IsNull then
            begin
              LSlurryDump.DumpName.FData := Trim(LMiningDataSet.DataSet.FieldByName('DumpName').AsString);
              LSlurryDump.DumpName.FInitalised := True;
            end;

            //Read Line 24
            if not LMiningDataSet.DataSet.FieldByName('DumpSurfaceArea').IsNull then
            begin
              LSlurryDump.DumpSurfaceArea.FData :=LMiningDataSet.DataSet.FieldByName('DumpSurfaceArea').AsFloat;
              LSlurryDump.DumpSurfaceArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('RunoffFactorToPCD').IsNull then
            begin
              LSlurryDump.RunoffFactorToPCD.FData :=LMiningDataSet.DataSet.FieldByName('RunoffFactorToPCD').AsFloat;
              LSlurryDump.RunoffFactorToPCD.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('SeepageSplitFactor').IsNull then
            begin
              LSlurryDump.SeepageSplitFactor.FData :=LMiningDataSet.DataSet.FieldByName('SeepageSplitFactor').AsFloat;
              LSlurryDump.SeepageSplitFactor.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDStorageCapacity').IsNull then
            begin
              LSlurryDump.PCDStorageCapacity.FData :=LMiningDataSet.DataSet.FieldByName('PCDStorageCapacity').AsFloat;
              LSlurryDump.PCDStorageCapacity.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDSurfaceArea').IsNull then
            begin
              LSlurryDump.PCDSurfaceArea.FData :=LMiningDataSet.DataSet.FieldByName('PCDSurfaceArea').AsFloat;
              LSlurryDump.PCDSurfaceArea.FInitalised := True;
            end;

            if not LMiningDataSet.DataSet.FieldByName('PCDAnalysisStartVolume').IsNull then
            begin
              LSlurryDump.PCDAnalysisStartVolume.FData :=LMiningDataSet.DataSet.FieldByName('PCDAnalysisStartVolume').AsFloat;
              LSlurryDump.PCDAnalysisStartVolume.FInitalised := True;
            end;

            //Read Line 21
            LMonthlyValuesDataSet.DataSet.Close;
            LMonthlyValuesDataSet.SetSQL(ReadRechargeFactorDataSQL(LMineIdentifier,3,LSlurryDumpIdentifier,5));
            LMonthlyValuesDataSet.DataSet.Open;
            if not (LMonthlyValuesDataSet.DataSet.Eof and LMonthlyValuesDataSet.DataSet.Bof) then
            begin
              for LIndex := 1 to 12 do
              begin
                LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
                if not LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LSlurryDump.RechargeFactor[LIndex].FData :=LMonthlyValuesDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LSlurryDump.RechargeFactor[LIndex].FInitalised := True;
                end;
              end;
            end;

            LDataSet.DataSet.Close;
            LDataSet.SetSQL(ReadMineGrowthFactorDataSQL(LMineIdentifier,0,0,LSlurryDumpIdentifier,gtUndergroung));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LGrowthFactors := LSlurryDump.AddGrowthFactors;
              if LGrowthFactors <> nil then
              begin
                LGrowthFactors.Initialise;
                 if not LDataSet.DataSet.FieldByName('NoOfPoints').IsNull then
                 begin
                   LGrowthFactors.FNoOfPoints.FData := LDataSet.DataSet.FieldByName('NoOfPoints').AsInteger;
                   LGrowthFactors.FNoOfPoints.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('FactorType').IsNull then
                 begin
                   LGrowthFactors.FGrowthCode.FData := LDataSet.DataSet.FieldByName('FactorType').AsInteger;
                   LGrowthFactors.FGrowthCode.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('InterpolationMethod').IsNull then
                 begin
                   LGrowthFactors.FInterpolationMethod.FData := LDataSet.DataSet.FieldByName('InterpolationMethod').AsInteger;
                   LGrowthFactors.FInterpolationMethod.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('NoOfYears').IsNull then
                 begin
                   LGrowthFactors.FYearDatapoints.FData := LDataSet.DataSet.FieldByName('NoOfYears').AsString;
                   LGrowthFactors.FYearDatapoints.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('GrowthFactors').IsNull then
                 begin
                   LGrowthFactors.FGrowthFactors.FData := LDataSet.DataSet.FieldByName('GrowthFactors').AsString;
                   LGrowthFactors.FGrowthFactors.FInitalised := True;
                 end;
              end;
              LDataSet.DataSet.Next;
            end;

            LDataSet.DataSet.Close;
            LDataSet.SetSQL(ReadLoadGenerationDataSQL(LMineIdentifier,0,0,LSlurryDumpIdentifier,lgSlurryDump));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.Eof do
            begin
              LLoadGeneration := LSlurryDump.AddLoadGeneration;
              if LLoadGeneration <> nil then
              begin
                 LLoadGeneration.Initialise;
                 if not LDataSet.DataSet.FieldByName('LoadGenType').IsNull then
                 begin
                   LLoadGeneration.FLoadGenType.FData := LDataSet.DataSet.FieldByName('LoadGenType').AsInteger;
                   LLoadGeneration.FLoadGenType.FInitalised := True;
                 end;

                 if not LDataSet.DataSet.FieldByName('StdDeviation').IsNull then
                 begin
                   LLoadGeneration.FStdDeviation.FData := LDataSet.DataSet.FieldByName('StdDeviation').AsFloat;
                   LLoadGeneration.FStdDeviation.FInitalised := True;
                 end;


                for LIndex := 1 to 10 do
                begin
                  LFieldName := Format('%s%2.2d',['Flow',LIndex]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LLoadGeneration.FFlow[LIndex].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LLoadGeneration.FFlow[LIndex].FInitalised := True;
                  end;
                end;

                for LIndex := 1 to 10 do
                begin
                  LFieldName := Format('%s%2.2d',['MeanOfSalt',LIndex]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LLoadGeneration.FMeanOfSalt[LIndex].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LLoadGeneration.FMeanOfSalt[LIndex].FInitalised := True;
                  end;
                end;

              end;
              LDataSet.DataSet.Next;

            end;

            LMiningDataSet.DataSet.Next;
          end;

          LMineDataSet.DataSet.Next;
        end;

        LMineDataSet.DataSet.Close;
      end;
      (*
      LMineSubCatchmentList := ADataObject.FMineSubCatchmentListObject;
      if not LMineSubCatchmentList.Initialise then
        Exit;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadMineSubCatchmentDataSQL);
      LDataSet.DataSet.Open;
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileMIMMDatabaseAgent.strMineSubCatchmentNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LMineSubCatchment := LMineSubCatchmentList.AddMineSubCatchmentFileObject;

          LMineSubCatchmentID := LDataSet.DataSet.FieldByName('Identifier').AsInteger;

          //Read Line 27
          if not LDataSet.DataSet.FieldByName('CatchmentReferenceNumber').IsNull then
          begin
            LMineSubCatchment.CatchmentRefNumber.FData := LDataSet.DataSet.FieldByName('CatchmentReferenceNumber').AsInteger;
            LMineSubCatchment.CatchmentRefNumber.FInitalised := True;
          end;

          //Read Line 28
          LMineSubCatchmentDataset.DataSet.Close;
          LMineSubCatchmentDataset.SetSQL(ReadMineSubCatchmentFlowVolumeDataSQL(LMineSubCatchmentID));
          LMineSubCatchmentDataset.DataSet.Open;
          if not (LMineSubCatchmentDataset.DataSet.Eof and LMineSubCatchmentDataset.DataSet.Bof) then
          begin
            for LIndex := 1 to 12 do
            begin
              LFieldName := Format('%s%2.2d',['Volume',LIndex]);
              if not LMineSubCatchmentDataset.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LMineSubCatchment.MineSubCatchmentFlowVolume[LIndex].FData := LMineSubCatchmentDataset.DataSet.FieldByName(LFieldName).AsFloat;
                LMineSubCatchment.MineSubCatchmentFlowVolume[LIndex].FInitalised := True;
              end;
            end;
          end;

         //Read Line 29
          if not LDataSet.DataSet.FieldByName('ProportionAntecedentFlow').IsNull then
          begin
            LMineSubCatchment.ProportionAntecedentFlow.FData := LDataSet.DataSet.FieldByName('ProportionAntecedentFlow').AsInteger;
            LMineSubCatchment.ProportionAntecedentFlow.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('GroundwaterFlowVolume').IsNull then
          begin
            LMineSubCatchment.GroundwaterFlowVolume.FData := LDataSet.DataSet.FieldByName('GroundwaterFlowVolume').AsInteger;
            LMineSubCatchment.GroundwaterFlowVolume.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('AntecedentRunoffDecayFactor').IsNull then
          begin
            LMineSubCatchment.AntecedentRunoffDecayFactor.FData := LDataSet.DataSet.FieldByName('AntecedentRunoffDecayFactor').AsInteger;
            LMineSubCatchment.AntecedentRunoffDecayFactor.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('InUse').IsNull then
          begin
            LMineSubCatchment.CatchmentRefUsed.FData := Trim(LDataSet.DataSet.FieldByName('InUse').AsString);
            LMineSubCatchment.CatchmentRefUsed.FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;
      end;
                *)
      //Line 5 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadMIMMUnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LMineList.Comment.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      Result :=  True;
      LMessage := FAppModules.Language.GetString('TFileMIMMDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      LDataSet.Free;
      LMineDataSet.Free;
      LMiningDataSet.Free;
      LMonthlyValuesDataSet.Free;
    //  LMineSubCatchmentDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage: string;
  LStop: boolean;
  LDataSet,
  LMineDataSet,
  LMiningDataSet,
//  LMineSubCatchmentDataSet,
  LMonthlyValuesDataSet      : TAbstractModelDataset;
  LIndex                     : integer;
  LMinesIndex                : integer;
  LOpenCastIndex             : integer;
  LUnderGroundIndex          : integer;
  LSlurryDumpIndex           : integer;
  LMineIdentifier            : integer;
  //LMineSubCatchmentID        : integer;
  LOpenCastIdentifier        : integer;
  LUnderGroundIdentifier     : integer;
  LSlurryDumpIdentifier      : integer;
  //LMineSubCatchmentIndex     : integer;
  LOpenCast                  : TMIMMOpenCastFileObject;
  LUndeground                : TMIMMUndegroundFileObject;
  LSlurryDump                : TMIMMSlurryDumpFileObject;
  LMine                      : TMIMMFileObject;
  LMineList                  : TMIMMListFileObject;
  LGrowthFactors             : TGrowthFactors;
  LLoadGeneration            : TLoadGeneration;
  LPlanningFileDataObject    : TPlanningFileDataObjects;
  LLoadGenType,
  LLoadIndex,
  LCount: Integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileMIMMDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

   // LMineList := ADataObject.FMineListFileObject;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LMineList    := LPlanningFileDataObject.AddMIMMFileObject(AFilename.FileNumber);
    if not Assigned(LMineList) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMiningDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyValuesDataSet);
    //FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineSubCatchmentDataSet);

    try
      for LMinesIndex := 0 to LMineList.MineFileObjectCount -1 do
      begin

        LMineIdentifier := AFilename.FileNumber;
        LMine := TMIMMFileObject(LMineList.MineFileObjectByIndex[LMinesIndex]);


        LMineDataSet.DataSet.Close;
        LMineDataSet.ClearQueryParams();
        LMineDataSet.SetSQL(WriteMineDataSQL);
        LMineDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LMineDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LMineDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LMineDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LMineDataSet.SetParams(['Identifier'], [IntToStr(LMineIdentifier)]);


        if LMine.NodeNumber.FInitalised  then
          LMineDataSet.SetParams(['NodeNumber'], [IntToStr(LMine.NodeNumber.FData)]);

        if LMine.MineName.FInitalised then
         LMineDataSet.SetParams(['MineName'], [LMine.MineName.FData]);

        if LMine.RiverChannelNumber.FInitalised then
         LMineDataSet.SetParams(['RiverChannelNumber'], [IntToStr(LMine.RiverChannelNumber.FData)]);

        if LMine.PCDChannelNumber.FInitalised then
         LMineDataSet.SetParams(['PCDChannelNumber'], [IntToStr(LMine.PCDChannelNumber.FData)]);

        if LMine.HydrologyNodeNumber.FInitalised then
         LMineDataSet.SetParams(['HydrologyNodeNumber'], [IntToStr(LMine.HydrologyNodeNumber.FData)]);

        if LMine.BeneficiationPlantArea.FInitalised then
         LMineDataSet.SetParams(['BeneficiationPlantArea'], [FloatToStr(LMine.BeneficiationPlantArea.FData)]);

        if LMine.BeneficiationRunOffFactor.FInitalised then
         LMineDataSet.SetParams(['BeneficiationRunOffFactor'], [FloatToStr(LMine.BeneficiationRunOffFactor.FData)]);

         //_____________________________MIMM_____________________________________//

         if LMine.FAssSaltsWashoffNo.FInitalised then
           LMineDataSet.SetParams(['SaltWashoffNo'], [IntToStr(LMine.FAssSaltsWashoffNo.FData)]);

         if LMine.FRainfallFileName.FInitalised then
           LMineDataSet.SetParams(['RainfallFileName'], [LMine.FRainfallFileName.FData]);

         if LMine.FRainfallMAP.FInitalised then
           LMineDataSet.SetParams(['MeanAnnPrecip'], [FloatToStr(LMine.FRainfallMAP.FData)]);

         if LMine.FSaltsBuildUpRate.FInitalised then
           LMineDataSet.SetParams(['SaltBuildUpRate'], [FloatToStr(LMine.FSaltsBuildUpRate.FData)]);

         if LMine.FSaltsWashoffEfficiencyFactor.FInitalised then
           LMineDataSet.SetParams(['SaltWashOffEfficiencyFactor'], [FloatToStr(LMine.FSaltsWashoffEfficiencyFactor.FData)]);

         if LMine.FIniSaltStore.FInitalised then
           LMineDataSet.SetParams(['IniSaltStore'], [FloatToStr(LMine.FIniSaltStore.FData)]);


        LMineDataSet.ExecSQL;

        LMonthlyValuesDataSet.DataSet.Close;
        LMonthlyValuesDataSet.SetSQL(WritePanEvaporationDataSQL);
        LMonthlyValuesDataSet.ClearQueryParams();
        LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

        for LIndex := 1 to 12 do
        begin
          if LMine.PanEvaporation[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['PanEvaporation',LIndex]);
            LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr(LMine.PanEvaporation[LIndex].FData)]);
          end;
        end;
        LMonthlyValuesDataSet.ExecSQL;

        LMonthlyValuesDataSet.DataSet.Close;
        LMonthlyValuesDataSet.SetSQL(WriteLakeEvaporationDataSQL);
        LMonthlyValuesDataSet.ClearQueryParams();
        LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

        for LIndex := 1 to 12 do
        begin
          if LMine.LakeEvaporation[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['LakeEvaporation',LIndex]);
            LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr(LMine.LakeEvaporation[LIndex].FData)]);
          end;
        end;

        LMonthlyValuesDataSet.ExecSQL;


        //  growth in benificiation plant area


        for LIndex := 0 to LMine.FMIMMGrowthFactorList.Count-1 do
        begin
          LGrowthFactors := TGrowthFactors(LMine.FMIMMGrowthFactorList[LIndex]);
          if LGrowthFactors <> nil then
          begin
            LMineDataSet.DataSet.Close;
            LMineDataSet.SetSQL(WriteGrowthFactorDataSQL);
            LMineDataSet.ClearQueryParams();
            LMineDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LMineDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LMineDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LMineDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

            LMineDataSet.SetParams(['Identifier'], [IntToStr(GetMaxGrowthFactorID(LMineIdentifier)+1)]);

            LMineDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

            LMineDataSet.SetParams(['OCIdentifier'],['0']);
            LMineDataSet.SetParams(['SlurryIdentifier'],['0']);
            LMineDataSet.SetParams(['UDGIdentifier'],['0']);

            if LGrowthFactors.FNoOfPoints.FInitalised then
              LMineDataSet.SetParams(['NoOfPoints'], [IntToStr(LGrowthFactors.FNoOfPoints.FData)]);

            if LGrowthFactors.FGrowthCode.FInitalised then
              LMineDataSet.SetParams(['FactorType'], [IntToStr(LGrowthFactors.FGrowthCode.FData)]);

            if LGrowthFactors.FInterpolationMethod.FInitalised then
              LMineDataSet.SetParams(['InterpolationMethod'], [IntToStr(LGrowthFactors.FInterpolationMethod.FData)]);

            if LGrowthFactors.FYearDatapoints.FInitalised then
              LMineDataSet.SetParams(['NoOfYears'], [LGrowthFactors.FYearDatapoints.FData]);

            if LGrowthFactors.FGrowthFactors.FInitalised then
              LMineDataSet.SetParams(['GrowthFactors'], [LGrowthFactors.FGrowthFactors.FData]);

            LMineDataSet.ExecSQL;
          end;

        end;


        for LOpenCastIndex := 0 to LMine.OpenCastCount-1 do
        begin
          LOpenCastIdentifier := LOpenCastIndex + 1;

          LOpenCast := TMIMMOpenCastFileObject(LMine.OpenCastByIndex[LOpenCastIndex]);
          LMiningDataSet.DataSet.Close;
          LMiningDataSet.ClearQueryParams();
          LMiningDataSet.SetSQL(WriteOpenCastDataSQL);

          LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMiningDataSet.SetParams(['Identifier'], [IntToStr(LOpenCastIdentifier)]);

          if LOpenCast.PitName.FInitalised then
           LMiningDataSet.SetParams(['PitName'], [LOpenCast.PitName.FData]);

          if LOpenCast.CoalReserveArea.FInitalised then
           LMiningDataSet.SetParams(['CoalReserveArea'], [FloatToStr(LOpenCast.CoalReserveArea.FData)]);

          if LOpenCast.WorkingsArea.FInitalised then
           LMiningDataSet.SetParams(['WorkingsArea'], [FloatToStr(LOpenCast.WorkingsArea.FData)]);

          if LOpenCast.DisturbedWorkingsArea.FInitalised then
           LMiningDataSet.SetParams(['DisturbedWorkingsArea'], [FloatToStr(LOpenCast.DisturbedWorkingsArea.FData)]);

          if LOpenCast.DisturbedArea.FInitalised then
           LMiningDataSet.SetParams(['DisturbedArea'], [FloatToStr(LOpenCast.DisturbedArea.FData)]);

          if LOpenCast.WaterSurfaceEvapArea.FInitalised then
           LMiningDataSet.SetParams(['WaterSurfaceEvapArea'], [FloatToStr(LOpenCast.WaterSurfaceEvapArea.FData)]);

          if LOpenCast.DisturbedAreaRunOff.FInitalised then
           LMiningDataSet.SetParams(['DisturbedAreaRunOff'], [FloatToStr(LOpenCast.DisturbedAreaRunOff.FData)]);

          if LOpenCast.DisturbedWorkingsAreaRunOff.FInitalised then
           LMiningDataSet.SetParams(['DisturbedWorkingsAreaRunOff'], [FloatToStr(LOpenCast.DisturbedWorkingsAreaRunOff.FData)]);

          if LOpenCast.DecantVolume.FInitalised then
           LMiningDataSet.SetParams(['DecantVolume'], [FloatToStr(LOpenCast.DecantVolume.FData)]);

          if LOpenCast.SeepageVolume.FInitalised then
           LMiningDataSet.SetParams(['SeepageVolume'], [FloatToStr(LOpenCast.SeepageVolume.FData)]);

          if LOpenCast.AnalysisStartVolume.FInitalised then
           LMiningDataSet.SetParams(['AnalysisStartVolume'], [FloatToStr(LOpenCast.AnalysisStartVolume.FData)]);

          if LOpenCast.MaximumSeepageRate.FInitalised then
           LMiningDataSet.SetParams(['MaximumSeepageRate'], [FloatToStr(LOpenCast.MaximumSeepageRate.FData)]);

          if LOpenCast.SeepageExponent.FInitalised then
           LMiningDataSet.SetParams(['SeepageExponent'], [FloatToStr(LOpenCast.SeepageExponent.FData)]);

          if LOpenCast.PCDSurfaceArea.FInitalised then
           LMiningDataSet.SetParams(['PCDSurfaceArea'], [FloatToStr(LOpenCast.PCDSurfaceArea.FData)]);

          if LOpenCast.PCDStorageCapacity.FInitalised then
           LMiningDataSet.SetParams(['PCDStorageCapacity'], [FloatToStr(LOpenCast.PCDStorageCapacity.FData)]);

          if LOpenCast.PCDAnalysisStartVolume.FInitalised then
           LMiningDataSet.SetParams(['PCDAnalysisStartVolume'], [FloatToStr(LOpenCast.PCDAnalysisStartVolume.FData)]);

          //________________________________MIMM_________________________________________//


          if LOpenCast.FAbstractionOpt.FInitalised then
            LMiningDataSet.SetParams(['Abstraction'], [LOpenCast.FAbstractionOpt.FData]);

          if LOpenCast.FIniConcentration.FInitalised then
            LMiningDataSet.SetParams(['PCDIniConcentration'], [FloatToStr(LOpenCast.FIniConcentration.FData)]);

          if LOpenCast.FWorkingCommYear.FInitalised then
            LMiningDataSet.SetParams(['WorkingCommYear'], [IntToStr(LOpenCast.FWorkingCommYear.FData)]);

          if LOpenCast.FWorkingCommMonth.FInitalised then
            LMiningDataSet.SetParams(['WorkingCommMonth'], [IntToStr(LOpenCast.FWorkingCommMonth.FData)]);


          if LOpenCast.FWorkingDecommYear.FInitalised then
            LMiningDataSet.SetParams(['WorkingDecommYear'], [IntToStr(LOpenCast.FWorkingDecommYear.FData)]);

          if LOpenCast.FWorkingDecommMonth.FInitalised then
            LMiningDataSet.SetParams(['WorkingDecommMonth'], [IntToStr(LOpenCast.FWorkingDecommMonth.FData)]);


          if LOpenCast.FRunoffSaltsWashoffEfficiencyFactor.FInitalised then
            LMiningDataSet.SetParams(['RunoffSaltWashOffEfficiencyFactor'], [FloatToStr(LOpenCast.FRunoffSaltsWashoffEfficiencyFactor.FData)]);

          if LOpenCast.FIniSaltStore.FInitalised then
            LMiningDataSet.SetParams(['IniSaltStore'], [FloatToStr(LOpenCast.FIniSaltStore.FData)]);

          if LOpenCast.FReChargeRate.FInitalised then
            LMiningDataSet.SetParams(['ReChargeRate'], [FloatToStr(LOpenCast.FReChargeRate.FData)]);

          if LOpenCast.FAbstractToEvap.FInitalised then
            LMiningDataSet.SetParams(['AbstractToEvap'], [FloatToStr(LOpenCast.FAbstractToEvap.FData)]);

          if LOpenCast.FAbstrctToRiver.FInitalised then
            LMiningDataSet.SetParams(['AbstractToRiver'], [FloatToStr(LOpenCast.FAbstrctToRiver.FData)]);

          if LOpenCast.FAbstrctToCPD.FInitalised then
            LMiningDataSet.SetParams(['AbstractToPCD'], [FloatToStr(LOpenCast.FAbstrctToCPD.FData)]);

          if LOpenCast.FAbstrctMonthTimeSeriesFile.FInitalised then
            LMiningDataSet.SetParams(['AbstractMonthTimeSeriesFile'], [LOpenCast.FAbstrctMonthTimeSeriesFile.FData]);

          LMiningDataSet.ExecSQL;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(WriteRechargeFactorDataSQL);
          LMonthlyValuesDataSet.ClearQueryParams();
          LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorParentType'], [IntToStr(1)]);
          LMonthlyValuesDataSet.SetParams(['ParentIdentifier'], [IntToStr(LOpenCastIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorType'], [IntToStr(1)]);

          for LIndex := 1 to 12 do
          begin
            if LMine.LakeEvaporation[LIndex].FInitalised  then
            begin
              LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
              LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr( LOpenCast.DisturbedRechargeFactor[LIndex].FData)]);
            end;
          end;
          LMonthlyValuesDataSet.ExecSQL;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(WriteRechargeFactorDataSQL);
          LMonthlyValuesDataSet.ClearQueryParams();
          LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorParentType'], [IntToStr(1)]);
          LMonthlyValuesDataSet.SetParams(['ParentIdentifier'], [IntToStr(LOpenCastIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorType'], [IntToStr(2)]);

          for LIndex := 1 to 12 do
          begin
            if LMine.LakeEvaporation[LIndex].FInitalised  then
            begin
              LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
              LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr( LOpenCast.WorkingAreaRechargeFactor[LIndex].FData)]);
            end;
          end;
          LMonthlyValuesDataSet.ExecSQL;

          for LIndex := 0 to LOpenCast.FGrowthFactors.Count -1 do
          begin
            LGrowthFactors := TGrowthFactors(LOpenCast.FGrowthFactors[LIndex]);
            if LGrowthFactors <> nil then
            begin

              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteGrowthFactorDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(GetMaxGrowthFactorID(LMineIdentifier)+1)]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

              LMiningDataSet.SetParams(['OCIdentifier'],[IntToStr(LOpenCastIdentifier)]);
              LMiningDataSet.SetParams(['SlurryIdentifier'],['0']);
              LMiningDataSet.SetParams(['UDGIdentifier'],['0']);

              if LGrowthFactors.FNoOfPoints.FInitalised then
                LMiningDataSet.SetParams(['NoOfPoints'], [IntToStr(LGrowthFactors.FNoOfPoints.FData)]);

              if LGrowthFactors.FGrowthCode.FInitalised then
                LMiningDataSet.SetParams(['FactorType'], [IntToStr(LGrowthFactors.FGrowthCode.FData)]);

              if LGrowthFactors.FInterpolationMethod.FInitalised then
                LMiningDataSet.SetParams(['InterpolationMethod'], [IntToStr(LGrowthFactors.FInterpolationMethod.FData)]);

              if LGrowthFactors.FYearDatapoints.FInitalised then
                LMiningDataSet.SetParams(['NoOfYears'], [LGrowthFactors.FYearDatapoints.FData]);

              if LGrowthFactors.FGrowthFactors.FInitalised then
                LMiningDataSet.SetParams(['GrowthFactors'], [LGrowthFactors.FGrowthFactors.FData]);

              LMiningDataSet.ExecSQL;

            end;


          end;

          for LIndex := 0 to LOpenCast.FLoadGeneration.Count -1 do
          begin
            LLoadGeneration := TLoadGeneration(LOpenCast.FLoadGeneration[LIndex]);
            LLoadIndex := GetMaxLoadGenerationID(LMineIdentifier)+1;

            LLoadGenType := 1;
            if (LIndex >= 1) then
              LLoadGenType := 2;

            if LLoadGeneration <> nil then
            begin
              LMiningDataSet.DataSet.Close;
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetSQL(WriteLoadGenerationFlowDataSQL);

              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(LLoadIndex)]);
              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
              LMiningDataSet.SetParams(['OpenCastIdentifier'],[IntToStr(LOpenCastIdentifier)]);
              LMiningDataSet.SetParams(['UDGIdentifier'],['0']);
              LMiningDataSet.SetParams(['SDIdentifier'],['0']);

              LMiningDataSet.SetParams(['LoadGenType'], [IntToStr(LLoadGenType)]);
              LMiningDataSet.SetParams(['StdDeviation'], [FloatToStr(LLoadGeneration.FStdDeviation.FData)]);



              for LCount := 1 to 10 do
              begin
                if LLoadGeneration.FFlow[LCount].FInitalised  then
                begin
                  LFieldName := Format('%s%2.2d',['Flow',LCount]);
                  LMiningDataSet.SetParams([LFieldName], [FloatToStr( LLoadGeneration.FFlow[LCount].FData)]);
                end;
              end;
              LMiningDataSet.ExecSQL;


              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteLoadGenerationMeanOfSaltsDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(LLoadIndex)]);
              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

              LMiningDataSet.SetParams(['LoadGenType'], [IntToStr(LLoadGenType)]);
              LMiningDataSet.SetParams(['StdDeviation'], [FloatToStr(LLoadGeneration.FStdDeviation.FData)]);

              LMiningDataSet.SetParams(['OpenCastIdentifier'],[IntToStr(LOpenCastIdentifier)]);
              LMiningDataSet.SetParams(['UDGIdentifier'],['0']);
              LMiningDataSet.SetParams(['SDIdentifier'],['0']);

              for LCount := 1 to 10 do
              begin
                if LLoadGeneration.FMeanOfSalt[LCount].FInitalised  then
                begin
                  LFieldName := Format('%s%2.2d',['MeanOfSalt',LCount]);
                  LMiningDataSet.SetParams([LFieldName], [FloatToStr(LLoadGeneration.FMeanOfSalt[LCount].FData)]);
                end;
              end;
              LMiningDataSet.ExecSQL;
            end;
          end;
        end;

        for LUnderGroundIndex := 0 to LMine.UndegroundCount-1 do
        begin
          LUnderGroundIdentifier := LUnderGroundIndex + 1;

          LUndeground := TMIMMUndegroundFileObject(LMine.UndegroundByIndex[LUnderGroundIndex]);
          LMiningDataSet.DataSet.Close;
          LMiningDataSet.SetSQL(WriteUnderGroundDataSQL);
          LMiningDataSet.ClearQueryParams();
          LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMiningDataSet.SetParams(['Identifier'], [IntToStr(LUnderGroundIdentifier)]);

          if LUndeground.UnderGroundSectionName.FInitalised then
           LMiningDataSet.SetParams(['UnderGroundSectionName'], [LUndeground.UnderGroundSectionName.FData]);

          if LUndeground.ChannelNumberToUGDam.FInitalised then
           LMiningDataSet.SetParams(['ChannelNumberToUGDam'], [IntToStr(LUndeground.ChannelNumberToUGDam.FData)]);

          if LUndeground.UpstreamCatchmentArea.FInitalised then
           LMiningDataSet.SetParams(['UpstreamCatchmentArea'], [FloatToStr(LUndeground.UpstreamCatchmentArea.FData)]);

          if LUndeground.BoardPillarCatchmentArea.FInitalised then
           LMiningDataSet.SetParams(['BoardPillarCatchmentArea'], [FloatToStr(LUndeground.BoardPillarCatchmentArea.FData)]);

          if LUndeground.HighExtractionCatchmentArea.FInitalised then
           LMiningDataSet.SetParams(['HighExtractionCatchmentArea'], [FloatToStr(LUndeground.HighExtractionCatchmentArea.FData)]);

          if LUndeground.HighExtractionAreaRunoffFactor.FInitalised then
           LMiningDataSet.SetParams(['HighExtractionAreaRunoffFactor'], [FloatToStr(LUndeground.HighExtractionAreaRunoffFactor.FData)]);




          LMiningDataSet.ExecSQL;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(WriteUGUpstreamRunoffDataSQL);
          LMonthlyValuesDataSet.ClearQueryParams();
          LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['UGIdentifier'], [IntToStr(LUnderGroundIdentifier)]);

          for LIndex := 1 to 12 do
          begin
            if LUndeground.UpstreamRunoffPortion[LIndex].FInitalised  then
            begin
              LFieldName := Format('%s%2.2d',['RunoffFactor',LIndex]);
              LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr( LUndeground.UpstreamRunoffPortion[LIndex].FData)]);
            end;
          end;
          LMonthlyValuesDataSet.ExecSQL;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(WriteRechargeFactorDataSQL);
          LMonthlyValuesDataSet.ClearQueryParams();
          LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorParentType'], [IntToStr(2)]);
          LMonthlyValuesDataSet.SetParams(['ParentIdentifier'], [IntToStr(LUnderGroundIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorType'], [IntToStr(3)]);

          for LIndex := 1 to 12 do
          begin
            if LUndeground.BoardAndPilarRechargeFactor[LIndex].FInitalised  then
            begin
              LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
              LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr( LUndeground.BoardAndPilarRechargeFactor[LIndex].FData)]);
            end;
          end;
          LMonthlyValuesDataSet.ExecSQL;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(WriteRechargeFactorDataSQL);
          LMonthlyValuesDataSet.ClearQueryParams();
          LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorParentType'], [IntToStr(2)]);
          LMonthlyValuesDataSet.SetParams(['ParentIdentifier'], [IntToStr(LUnderGroundIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorType'], [IntToStr(4)]);

          for LIndex := 1 to 12 do
          begin
            if LUndeground.HighExtractionRechargeFactor[LIndex].FInitalised  then
            begin
              LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
              LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr( LUndeground.HighExtractionRechargeFactor[LIndex].FData)]);
            end;
          end;
          LMonthlyValuesDataSet.ExecSQL;

          for LIndex := 0 to LUndeground.FGrowthFactors.Count -1 do
          begin
            LGrowthFactors := TGrowthFactors(LUndeground.FGrowthFactors[LIndex]);
            if LGrowthFactors <> nil then
            begin

              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteGrowthFactorDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(GetMaxGrowthFactorID(LMineIdentifier)+1)]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

              LMiningDataSet.SetParams(['OCIdentifier'],['0']);
              LMiningDataSet.SetParams(['SlurryIdentifier'],['0']);
              LMiningDataSet.SetParams(['UDGIdentifier'],[IntToStr(LUnderGroundIdentifier)]);

              if LGrowthFactors.FNoOfPoints.FInitalised then
                LMiningDataSet.SetParams(['NoOfPoints'], [IntToStr(LGrowthFactors.FNoOfPoints.FData)]);

              if LGrowthFactors.FGrowthCode.FInitalised then
                LMiningDataSet.SetParams(['FactorType'], [IntToStr(LGrowthFactors.FGrowthCode.FData)]);

              if LGrowthFactors.FInterpolationMethod.FInitalised then
                LMiningDataSet.SetParams(['InterpolationMethod'], [IntToStr(LGrowthFactors.FInterpolationMethod.FData)]);

              if LGrowthFactors.FYearDatapoints.FInitalised then
                LMiningDataSet.SetParams(['NoOfYears'], [LGrowthFactors.FYearDatapoints.FData]);

              if LGrowthFactors.FGrowthFactors.FInitalised then
                LMiningDataSet.SetParams(['GrowthFactors'], [LGrowthFactors.FGrowthFactors.FData]);

              LMiningDataSet.ExecSQL;

            end;
          end;

          for LIndex := 0 to LUndeground.FLoadGeneration.Count -1 do
          begin
            LLoadGeneration := TLoadGeneration(LUndeground.FLoadGeneration[LIndex]);
            LLoadIndex := GetMaxLoadGenerationID(LMineIdentifier)+1;
            if LLoadGeneration <> nil then
            begin
              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteLoadGenerationFlowDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(LLoadIndex)]);
              LMiningDataSet.SetParams(['LoadGenType'], ['3']);
              LMiningDataSet.SetParams(['StdDeviation'], [FloatToStr(LLoadGeneration.FStdDeviation.FData)]);
              LMiningDataSet.SetParams(['OpenCastIdentifier'],['0']);
              LMiningDataSet.SetParams(['UDGIdentifier'],[IntToStr(LUnderGroundIdentifier)]);
              LMiningDataSet.SetParams(['SDIdentifier'],['0']);

              for LCount := 1 to 10 do
              begin
                if LLoadGeneration.FFlow[LCount].FInitalised  then
                begin
                  LFieldName := Format('%s%2.2d',['Flow',LCount]);
                  LMiningDataSet.SetParams([LFieldName], [FloatToStr(LLoadGeneration.FFlow[LCount].FData)]);
                end;
              end;
              LMiningDataSet.ExecSQL;


              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteLoadGenerationMeanOfSaltsDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
              LMiningDataSet.SetParams(['StdDeviation'], [FloatToStr(LLoadGeneration.FStdDeviation.FData)]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(LLoadIndex)]);
              LMiningDataSet.SetParams(['LoadGenType'], ['3']);
              LMiningDataSet.SetParams(['OpenCastIdentifier'],['0']);
              LMiningDataSet.SetParams(['UDGIdentifier'],[IntToStr(LUnderGroundIdentifier)]);
              LMiningDataSet.SetParams(['SDIdentifier'],['0']);

              for LCount := 1 to 10 do
              begin
                if LLoadGeneration.FMeanOfSalt[LCount].FInitalised  then
                begin
                  LFieldName := Format('%s%2.2d',['MeanOfSalt',LCount]);
                  LMiningDataSet.SetParams([LFieldName], [FloatToStr(LLoadGeneration.FMeanOfSalt[LCount].FData)]);
                end;
              end;
              LMiningDataSet.ExecSQL;
            end;
          end;
        end;

        for LSlurryDumpIndex := 0 to LMine.SlurryDumpCount-1 do
        begin
          LSlurryDumpIdentifier := LSlurryDumpIndex + 1;

          LSlurryDump := TMIMMSlurryDumpFileObject(LMine.SlurryDumpByIndex[LSlurryDumpIndex]);

          LMiningDataSet.DataSet.Close;
          LMiningDataSet.SetSQL(WriteSlurryDumpDataSQL);
          LMiningDataSet.ClearQueryParams();
          LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMiningDataSet.SetParams(['Identifier'], [IntToStr(LSlurryDumpIdentifier)]);

          if LSlurryDump.DumpName.FInitalised then
           LMiningDataSet.SetParams(['DumpName'], [LSlurryDump.DumpName.FData]);

          if LSlurryDump.DumpSurfaceArea.FInitalised then
           LMiningDataSet.SetParams(['DumpSurfaceArea'], [FloatToStr(LSlurryDump.DumpSurfaceArea.FData)]);

          if LSlurryDump.RunoffFactorToPCD.FInitalised then
           LMiningDataSet.SetParams(['RunoffFactorToPCD'], [FloatToStr(LSlurryDump.RunoffFactorToPCD.FData)]);

          if LSlurryDump.SeepageSplitFactor.FInitalised then
           LMiningDataSet.SetParams(['SeepageSplitFactor'], [FloatToStr(LSlurryDump.SeepageSplitFactor.FData)]);

          if LSlurryDump.PCDStorageCapacity.FInitalised then
           LMiningDataSet.SetParams(['PCDStorageCapacity'], [FloatToStr(LSlurryDump.PCDStorageCapacity.FData)]);

          if LSlurryDump.PCDSurfaceArea.FInitalised then
           LMiningDataSet.SetParams(['PCDSurfaceArea'], [FloatToStr(LSlurryDump.PCDSurfaceArea.FData)]);

          if LSlurryDump.PCDAnalysisStartVolume.FInitalised then
           LMiningDataSet.SetParams(['PCDAnalysisStartVolume'], [FloatToStr(LSlurryDump.PCDAnalysisStartVolume.FData)]);

          LMiningDataSet.ExecSQL;

          LMonthlyValuesDataSet.DataSet.Close;
          LMonthlyValuesDataSet.SetSQL(WriteRechargeFactorDataSQL);
          LMonthlyValuesDataSet.ClearQueryParams();
          LMonthlyValuesDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMonthlyValuesDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMonthlyValuesDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMonthlyValuesDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMonthlyValuesDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorParentType'], [IntToStr(3)]);
          LMonthlyValuesDataSet.SetParams(['ParentIdentifier'], [IntToStr(LSlurryDumpIdentifier)]);
          LMonthlyValuesDataSet.SetParams(['RechargeFactorType'], [IntToStr(5)]);

          for LIndex := 1 to 12 do
          begin
            if LSlurryDump.RechargeFactor[LIndex].FInitalised  then
            begin
              LFieldName := Format('%s%2.2d',['RechargeFactor',LIndex]);
              LMonthlyValuesDataSet.SetParams([LFieldName], [FloatToStr( LSlurryDump.RechargeFactor[LIndex].FData)]);
            end;
          end;
          LMonthlyValuesDataSet.ExecSQL;

          // growth

          for LIndex := 0 to LSlurryDump.FGrowthFactors.Count -1 do
          begin
            LGrowthFactors := TGrowthFactors(LSlurryDump.FGrowthFactors[LIndex]);
            if LGrowthFactors <> nil then
            begin

              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteGrowthFactorDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(GetMaxGrowthFactorID(LMineIdentifier)+1)]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);

              LMiningDataSet.SetParams(['OCIdentifier'],['0']);
              LMiningDataSet.SetParams(['SlurryIdentifier'],[IntToStr(LSlurryDumpIdentifier)]);
              LMiningDataSet.SetParams(['UDGIdentifier'],['0']);

              if LGrowthFactors.FNoOfPoints.FInitalised then
                LMiningDataSet.SetParams(['NoOfPoints'], [IntToStr(LGrowthFactors.FNoOfPoints.FData)]);

              if LGrowthFactors.FGrowthCode.FInitalised then
                LMiningDataSet.SetParams(['FactorType'], [IntToStr(LGrowthFactors.FGrowthCode.FData)]);

              if LGrowthFactors.FInterpolationMethod.FInitalised then
                LMiningDataSet.SetParams(['InterpolationMethod'], [IntToStr(LGrowthFactors.FInterpolationMethod.FData)]);

              if LGrowthFactors.FYearDatapoints.FInitalised then
                LMiningDataSet.SetParams(['NoOfYears'], [LGrowthFactors.FYearDatapoints.FData]);

              if LGrowthFactors.FGrowthFactors.FInitalised then
                LMiningDataSet.SetParams(['GrowthFactors'], [LGrowthFactors.FGrowthFactors.FData]);

              LMiningDataSet.ExecSQL;
            end;
          end;

          for LIndex := 0 to LSlurryDump.FLoadGeneration.Count -1 do
          begin
            LLoadGeneration := TLoadGeneration(LSlurryDump.FLoadGeneration[LIndex]);
            LLoadIndex := GetMaxLoadGenerationID(LMineIdentifier)+1;
            if LLoadGeneration <> nil then
            begin

              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteLoadGenerationFlowDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(LLoadIndex)]);

              LMiningDataSet.SetParams(['LoadGenType'], ['4']);
               LMiningDataSet.SetParams(['StdDeviation'], [FloatToStr(LLoadGeneration.FStdDeviation.FData)]);

              LMiningDataSet.SetParams(['OpenCastIdentifier'],['0']);
              LMiningDataSet.SetParams(['UDGIdentifier'],['0']);
              LMiningDataSet.SetParams(['SDIdentifier'],[IntToStr(LSlurryDumpIdentifier)]);

              for LCount := 1 to 10 do
              begin
                if LLoadGeneration.FFlow[LCount].FInitalised  then
                begin
                  LFieldName := Format('%s%2.2d',['Flow',LCount]);
                  LMiningDataSet.SetParams([LFieldName], [FloatToStr(LLoadGeneration.FFlow[LCount].FData)]);
                end;
              end;
              LMiningDataSet.ExecSQL;


              LMiningDataSet.DataSet.Close;
              LMiningDataSet.SetSQL(WriteLoadGenerationMeanOfSaltsDataSQL);
              LMiningDataSet.ClearQueryParams();
              LMiningDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
              LMiningDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
              LMiningDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
              LMiningDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

              LMiningDataSet.SetParams(['MineIdentifier'], [IntToStr(LMineIdentifier)]);
              LMiningDataSet.SetParams(['Identifier'], [IntToStr(LLoadIndex)]);
              LMiningDataSet.SetParams(['LoadGenType'], ['4']);
              LMiningDataSet.SetParams(['StdDeviation'], [FloatToStr(LLoadGeneration.FStdDeviation.FData)]);
              LMiningDataSet.SetParams(['OpenCastIdentifier'],['0']);
              LMiningDataSet.SetParams(['UDGIdentifier'],['0']);
              LMiningDataSet.SetParams(['SDIdentifier'],[IntToStr(LSlurryDumpIdentifier)]);

              for LCount := 1 to 10 do
              begin
                if LLoadGeneration.FMeanOfSalt[LCount].FInitalised  then
                begin
                  LFieldName := Format('%s%2.2d',['MeanOfSalt',LCount]);
                  LMiningDataSet.SetParams([LFieldName], [FloatToStr(LLoadGeneration.FMeanOfSalt[LCount].FData)]);
                end;
              end;
              LMiningDataSet.ExecSQL;
            end;
          end;
        end;


         //line 5++++++++++++++++++++++++++++
         for LIndex := 0 to LMineList.Comment.Count - 1 do
         begin
          LMineDataSet.DataSet.Close;
          LMineDataSet.SetSQL(WriteMIMMUnkownDataSQL);
          LMineDataSet.ClearQueryParams();
          LMineDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LMineDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LMineDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LMineDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LMineDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
          LMineDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
          LMineDataSet.SetParams(['LineNumber'], [IntToStr(1+ LIndex)]);
          LMineDataSet.SetParams(['LineSection'], [IntToStr(0)]);
          LMineDataSet.SetParams(['LineData'], [LMineList.Comment[LIndex]]);

          LMineDataSet.ExecSQL;
        end;
        LMineDataSet.DataSet.Close;

        Result := InsertFileName(AFileName);
        if Result then
        begin
          LMessage := FAppModules.Language.GetString('TFileMIMMDatabaseAgent.strWriteEnded');
          AProgressFunction(LMessage,ptNone,LStop);
        end;
      end;
    finally
      LDataSet.Free;
      LMineDataSet.Free;
      LMiningDataSet.Free;
      LMonthlyValuesDataSet.Free;
    //  LMineSubCatchmentDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileMIMMDatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
  LWhereClause: string;
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

    LTableNames := 'Mine';
    LWhereClause := ' AND Identifier = '+ IntToStr(AFileName.FileNumber);
    Result := DeleteModelData(LTableNames,LWhereClause,AProgressFunction,AQuetly);

    LTableNames := ' MineOpenCast,MineUnderGround,MinePanEvaporation,MineRechargeFactor' +
                   ',MineSlurryDump,MineUGUpstreamRunoff,MineLakeEvaporation'+
                   ',MineGrowthFactors, MineLoadGenerationFlow,MineLoadGenerationMeanOfSalt';

    LWhereClause := ' AND MineIdentifier  = '+ IntToStr(AFileName.FileNumber);
    Result := DeleteModelData(LTableNames,LWhereClause,AProgressFunction,AQuetly);




   // Result := DeleteModelData(LTableNames,LWhereClause,AProgressFunction,AQuetly);

    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteMineSubCatchmentDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteMineSubCatchmentDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineSubCatchment'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              '  ,CatchmentReferenceNumber,ProportionAntecedentFlow,GroundwaterFlowVolume,AntecedentRunoffDecayFactor,InUse)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              '  ,:CatchmentReferenceNumber,:ProportionAntecedentFlow,:GroundwaterFlowVolume,:AntecedentRunoffDecayFactor,:InUse)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.WriteMineSubCatchmentFlowVolumeDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.WriteMineSubCatchmentFlowVolumeDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineSubCatchmentFlowVolume'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineSubCatchmentIdentifier'+
              ' ,Volume01,Volume02,Volume03,Volume04,Volume05'+
              ' ,Volume06,Volume07,Volume08,Volume09,Volume10'+
              ' ,Volume11,Volume12)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineSubCatchmentIdentifier'+
              ' ,:Volume01,:Volume02,:Volume03,:Volume04,:Volume05'+
              ' ,:Volume06,:Volume07,:Volume08,:Volume09,:Volume10'+
              ' ,:Volume11,:Volume12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMDatabaseAgent.ReadMineSubCatchmentDataSQL: string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadMineSubCatchmentDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,CatchmentReferenceNumber,ProportionAntecedentFlow,GroundwaterFlowVolume,AntecedentRunoffDecayFactor,InUse'+
              ' FROM MineSubCatchment WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND '+
              ' (InUse            =' + IntToStr(1) +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
   except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileMIMMDatabaseAgent.ReadMineSubCatchmentFlowVolumeDataSQL(AMineSubCatchmentID: integer): string;
const OPNAME = 'TFileMIMMDatabaseAgent.ReadMineSubCatchmentFlowVolumeDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineSubCatchmentIdentifier'+
              ' ,Volume01,Volume02,Volume03,Volume04,Volume05'+
              ' ,Volume06,Volume07,Volume08,Volume09,Volume10'+
              ' ,Volume11,Volume12'+
              ' FROM MineSubCatchmentFlowVolume WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineSubCatchmentIdentifier =' + IntTostr(AMineSubCatchmentID)  +')';
   except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
