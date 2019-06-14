//
//
//  UNIT      : Contains TFile21DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 12/03/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFile21DatabaseAgent;

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
  UMineFileObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile21DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadMineDataSQL: string;
    function ReadOpenCastDataSQL(AMineID: integer): string;
    function ReadUnderGroundDataSQL(AMineID: integer): string;
    function ReadSlurryDumpDataSQL(AMineID: integer): string;
    function ReadLakeEvaporationDataSQL(AMineID: integer): string;
    function ReadPanEvaporationDataSQL(AMineID: integer): string;
    function ReadUGUpstreamRunoffDataSQL(AMineID,AUnderGroundID: integer): string;
    function ReadRechargeFactorDataSQL(AMineID,ARechargeFactorParentType,AParentIdentifier,ARechargeFactorType: integer): string;
    function ReadMineSubCatchmentDataSQL: string;
    function ReadMineSubCatchmentFlowVolumeDataSQL(AMineSubCatchmentID: integer): string;
    function ReadF21UnkownDataSQL: string;

    function WriteMineDataSQL: string;
    function WriteOpenCastDataSQL: string;
    function WriteUnderGroundDataSQL: string;
    function WriteSlurryDumpDataSQL: string;
    function WriteLakeEvaporationDataSQL: string;
    function WritePanEvaporationDataSQL: string;
    function WriteUGUpstreamRunoffDataSQL: string;
    function WriteRechargeFactorDataSQL: string;
    function WriteF21UnkownDataSQL: string;
    function WriteMineSubCatchmentDataSQL: string;
    function WriteMineSubCatchmentFlowVolumeDataSQL: string;
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
     UErrorHandlingOperations;

function TFile21DatabaseAgent.ReadMineDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.ReadMineDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier'+
              '  ,NodeNumber,MineName,RiverChannelNumber,PCDChannelNumber'+
              '  ,HydrologyNodeNumber,BeneficiationPlantArea,BeneficiationRunOffFactor'+
              ' FROM Mine WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.ReadLakeEvaporationDataSQL(AMineID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadLakeEvaporationDataSQL';
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

function TFile21DatabaseAgent.ReadPanEvaporationDataSQL(AMineID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadPanEvaporationDataSQL';
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

function TFile21DatabaseAgent.ReadOpenCastDataSQL(AMineID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadOpenCastDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,PitName,CoalReserveArea,WorkingsArea,DisturbedWorkingsArea,DisturbedArea'+
              ' ,WaterSurfaceEvapArea,DisturbedAreaRunOff,DisturbedWorkingsAreaRunOff'+
              ' ,DecantVolume,SeepageVolume,AnalysisStartVolume,MaximumSeepageRate'+
              ' ,SeepageExponent,PCDSurfaceArea,PCDStorageCapacity,PCDAnalysisStartVolume'+
              ' FROM MineOpenCast WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (MineIdentifier   =' + IntTostr(AMineID)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.ReadUnderGroundDataSQL(AMineID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadUnderGroundDataSQL';
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

function TFile21DatabaseAgent.ReadSlurryDumpDataSQL(AMineID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadSlurryDumpDataSQL';
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

function TFile21DatabaseAgent.ReadRechargeFactorDataSQL(
         AMineID,ARechargeFactorParentType,AParentIdentifier,ARechargeFactorType: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadRechargeFactorDataSQL';
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

function TFile21DatabaseAgent.ReadUGUpstreamRunoffDataSQL(AMineID,AUnderGroundID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadUGUpstreamRunoffDataSQL';
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

function TFile21DatabaseAgent.ReadF21UnkownDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.ReadF21UnkownDataSQL';
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

function TFile21DatabaseAgent.WriteMineDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteMineDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO Mine'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              '  ,NodeNumber,MineName,RiverChannelNumber,PCDChannelNumber'+
              '  ,HydrologyNodeNumber,BeneficiationPlantArea,BeneficiationRunOffFactor)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              '  ,:NodeNumber,:MineName,:RiverChannelNumber,:PCDChannelNumber'+
              '  ,:HydrologyNodeNumber,:BeneficiationPlantArea,:BeneficiationRunOffFactor)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.WriteLakeEvaporationDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteLakeEvaporationDataSQL';
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

function TFile21DatabaseAgent.WritePanEvaporationDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WritePanEvaporationDataSQL';
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

function TFile21DatabaseAgent.WriteOpenCastDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteOpenCastDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MineOpenCast'+
              ' (Model,StudyAreaName,SubArea,Scenario,MineIdentifier,Identifier'+
              ' ,PitName,CoalReserveArea,WorkingsArea,DisturbedWorkingsArea,DisturbedArea'+
              ' ,WaterSurfaceEvapArea,DisturbedAreaRunOff,DisturbedWorkingsAreaRunOff'+
              ' ,DecantVolume,SeepageVolume,AnalysisStartVolume,MaximumSeepageRate'+
              ' ,SeepageExponent,PCDSurfaceArea,PCDStorageCapacity,PCDAnalysisStartVolume)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:MineIdentifier,:Identifier'+
              ' ,:PitName,:CoalReserveArea,:WorkingsArea,:DisturbedWorkingsArea,:DisturbedArea'+
              ' ,:WaterSurfaceEvapArea,:DisturbedAreaRunOff,:DisturbedWorkingsAreaRunOff'+
              ' ,:DecantVolume,:SeepageVolume,:AnalysisStartVolume,:MaximumSeepageRate'+
              ' ,:SeepageExponent,:PCDSurfaceArea,:PCDStorageCapacity,:PCDAnalysisStartVolume)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.WriteUnderGroundDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteUnderGroundDataSQL';
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

function TFile21DatabaseAgent.WriteSlurryDumpDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteSlurryDumpDataSQL';
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

function TFile21DatabaseAgent.WriteRechargeFactorDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteRechargeFactorDataSQL';
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

function TFile21DatabaseAgent.WriteUGUpstreamRunoffDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteUGUpstreamRunoffDataSQL';
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

function TFile21DatabaseAgent.WriteF21UnkownDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteF21UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile21DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet,
  LMineDataSet,
  LMiningDataSet,
  LMineSubCatchmentDataset,
  LMonthlyValuesDataSet    : TAbstractModelDataset;
  LOpenCast                : TMineOpenCastFileObject;
  LUndeground              : TMineUndegroundFileObject;
  LSlurryDump              : TMineSlurryDumpFileObject;
  LMine                    : TMineFileObject;
  LMineList                : TMineListFileObject;
  LMineSubCatchment        : TMineSubCatchmentFileObject;
  LMineSubCatchmentList    : TMineSubCatchmentListFileObject;
  LIndex                   : integer;
  LMineIdentifier          : integer;
  LOpenCastIdentifier      : integer;
  LUnderGroundIdentifier   : integer;
  LSlurryDumpIdentifier    : integer;
  LMineSubCatchmentID      : integer;
  LStop                    : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile21DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LMineList := ADataObject.FMineListFileObject;
    if not LMineList.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMiningDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyValuesDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineSubCatchmentDataset);
    try
      LMineDataSet.SetSQL(ReadMineDataSQL);
      LMineDataSet.DataSet.Open;

      //Check if there is any data.
      if (LMineDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile21DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LMineDataSet.DataSet.Eof do
        begin
          LMine := LMineList.AddMineFileObject;

          LMineIdentifier := LMineDataSet.DataSet.FieldByName('Identifier').AsInteger;

          //Read Line 2
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

          //Read Line 4
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

          //Read Line 7
          if not LMineDataSet.DataSet.FieldByName('HydrologyNodeNumber').IsNull then
          begin
            LMine.HydrologyNodeNumber.FData :=LMineDataSet.DataSet.FieldByName('HydrologyNodeNumber').AsInteger;
            LMine.HydrologyNodeNumber.FInitalised := True;
          end;

          //Read Line 8
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

          //Read Line 5
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

          //Read Line 6
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

          //Read Line 9
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

            //Read Line 10
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

            //Read Line 11
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

            //Read Line 12
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

            //Read Line 15
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

            //Read Line 16
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

            //Read Line 17
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

            //Read Line 13
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

            //Read Line 14
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
            LMiningDataSet.DataSet.Next;
          end;

          //Read Line 18
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
            LMiningDataSet.DataSet.Next;
          end;

          //Read Line 23
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
            LMiningDataSet.DataSet.Next;
          end;

          LMineDataSet.DataSet.Next;
        end;
        LMineDataSet.DataSet.Close;
      end;

      LMineSubCatchmentList := ADataObject.FMineSubCatchmentListObject;
      if not LMineSubCatchmentList.Initialise then
        Exit;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadMineSubCatchmentDataSQL);
      LDataSet.DataSet.Open;
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile21DatabaseAgent.strMineSubCatchmentNoDataReturned');
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

      //Line 5 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF21UnkownDataSQL);
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
      LMessage := FAppModules.Language.GetString('TFile21DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      LDataSet.Free;
      LMineDataSet.Free;
      LMiningDataSet.Free;
      LMonthlyValuesDataSet.Free;
      LMineSubCatchmentDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile21DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage: string;
  LStop: boolean;
  LDataSet,
  LMineDataSet,
  LMiningDataSet,
  LMineSubCatchmentDataSet,
  LMonthlyValuesDataSet      : TAbstractModelDataset;
  LIndex                     : integer;
  LMinesIndex                : integer;
  LOpenCastIndex             : integer;
  LUnderGroundIndex          : integer;
  LSlurryDumpIndex           : integer;
  LMineIdentifier            : integer;
  LMineSubCatchmentID        : integer;
  LOpenCastIdentifier        : integer;
  LUnderGroundIdentifier     : integer;
  LSlurryDumpIdentifier      : integer;
  LMineSubCatchmentIndex     : integer;
  LOpenCast                  : TMineOpenCastFileObject;
  LUndeground                : TMineUndegroundFileObject;
  LSlurryDump                : TMineSlurryDumpFileObject;
  LMine                      : TMineFileObject;
  LMineList                  : TMineListFileObject;
  LMineSubCatchment          : TMineSubCatchmentFileObject;
  LMineSubCatchmentList      : TMineSubCatchmentListFileObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile21DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LMineList := ADataObject.FMineListFileObject;
    if not Assigned(LMineList) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMiningDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyValuesDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMineSubCatchmentDataSet);
    try
      for LMinesIndex := 0 to LMineList.MineFileObjectCount -1 do
      begin
        LMine := LMineList.MineFileObjectByIndex[LMinesIndex];
        LMineIdentifier := LMinesIndex+1;

        LMineDataSet.DataSet.Close;
        LMineDataSet.SetSQL(WriteMineDataSQL);
        LMineDataSet.ClearQueryParams();
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

        for LOpenCastIndex := 0 to LMine.OpenCastCount-1 do
        begin
          LOpenCastIdentifier := LOpenCastIndex + 1;

          LOpenCast := LMine.OpenCastByIndex[LOpenCastIndex];
          LMiningDataSet.DataSet.Close;
          LMiningDataSet.SetSQL(WriteOpenCastDataSQL);
          LMiningDataSet.ClearQueryParams();
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
        end;

        for LUnderGroundIndex := 0 to LMine.UndegroundCount-1 do
        begin
          LUnderGroundIdentifier := LUnderGroundIndex + 1;

          LUndeground := LMine.UndegroundByIndex[LUnderGroundIndex];
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
        end;

        for LSlurryDumpIndex := 0 to LMine.SlurryDumpCount-1 do
        begin
          LSlurryDumpIdentifier := LSlurryDumpIndex + 1;

          LSlurryDump := LMine.SlurryDumpByIndex[LSlurryDumpIndex];
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
        end;
      end;

      LMineSubCatchmentList := ADataObject.FMineSubCatchmentListObject;
      if not Assigned(LMineSubCatchmentList) then
      Exit; 

      for LMineSubCatchmentIndex := 0 to LMineSubCatchmentList.MineSubCatchmentObjectCount -1 do
      begin
        LMineSubCatchment := LMineSubCatchmentList.MineSubCatchmentObjectByIndex[LMineSubCatchmentIndex];
        LMineSubCatchmentID := LMineSubCatchmentIndex+1;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteMineSubCatchmentDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LMineSubCatchmentID)]);

        if LMineSubCatchment.CatchmentRefNumber.FInitalised  then
          LDataSet.SetParams(['CatchmentRefNumber'], [IntToStr(LMineSubCatchment.CatchmentRefNumber.FData)]);

        if LMineSubCatchment.ProportionAntecedentFlow.FInitalised  then
          LDataSet.SetParams(['ProportionAntecedentFlow'], [FloatToStr(LMineSubCatchment.ProportionAntecedentFlow.FData)]);

        if LMineSubCatchment.GroundwaterFlowVolume.FInitalised  then
          LDataSet.SetParams(['GroundwaterFlowVolume'], [FloatToStr(LMineSubCatchment.GroundwaterFlowVolume.FData)]);

        if LMineSubCatchment.AntecedentRunoffDecayFactor.FInitalised  then
          LDataSet.SetParams(['AntecedentRunoffDecayFactor'], [FloatToStr(LMineSubCatchment.AntecedentRunoffDecayFactor.FData)]);

        if LMineSubCatchment.CatchmentRefUsed.FInitalised  then
          LDataSet.SetParams(['InUse'], LMineSubCatchment.CatchmentRefUsed.FData);

        LDataSet.ExecSQL;

        LMineSubCatchmentDataSet.DataSet.Close;
        LMineSubCatchmentDataSet.SetSQL(WriteMineSubCatchmentFlowVolumeDataSQL);
        LMineSubCatchmentDataSet.ClearQueryParams();
        LMineSubCatchmentDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LMineSubCatchmentDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LMineSubCatchmentDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LMineSubCatchmentDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LMineSubCatchmentDataSet.SetParams(['MineSubCatchmentIdentifier'], [IntToStr(LMineSubCatchmentID)]);

        for LIndex := 1 to 12 do
        begin
          if LMineSubCatchment.MineSubCatchmentFlowVolume[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Volume',LIndex]);
            LMineSubCatchmentDataSet.SetParams([LFieldName], [FloatToStr(LMineSubCatchment.MineSubCatchmentFlowVolume[LIndex].FData)]);
          end;
        end;
        LMineSubCatchmentDataSet.ExecSQL;
      end;

       //line 5++++++++++++++++++++++++++++
      for LIndex := 0 to LMineList.Comment.Count - 1 do
      begin
        LMineDataSet.DataSet.Close;
        LMineDataSet.SetSQL(WriteF21UnkownDataSQL);
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
        LMessage := FAppModules.Language.GetString('TFile21DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
      LMineDataSet.Free;
      LMiningDataSet.Free;
      LMonthlyValuesDataSet.Free;
      LMineSubCatchmentDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile21DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'Mine,MineOpenCast,MineUnderGround,MinePanEvaporation,MineRechargeFactor' +
                   ',MineSlurryDump,MineUGUpstreamRunoff,MineLakeEvaporation,MineSubCatchment,MineSubCatchmentFlowVolume';
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

function TFile21DatabaseAgent.WriteMineSubCatchmentDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteMineSubCatchmentDataSQL';
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

function TFile21DatabaseAgent.WriteMineSubCatchmentFlowVolumeDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.WriteMineSubCatchmentFlowVolumeDataSQL';
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

function TFile21DatabaseAgent.ReadMineSubCatchmentDataSQL: string;
const OPNAME = 'TFile21DatabaseAgent.ReadMineSubCatchmentDataSQL';
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


function TFile21DatabaseAgent.ReadMineSubCatchmentFlowVolumeDataSQL(AMineSubCatchmentID: integer): string;
const OPNAME = 'TFile21DatabaseAgent.ReadMineSubCatchmentFlowVolumeDataSQL';
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
