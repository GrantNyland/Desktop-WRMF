unit UMineLoadAgent;

interface
uses
  Classes,
  VoaimsCom_TLB,
  UMineSQLAgent,
  UMiningData,
  UAbstractObject;
type
  TMineLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent : TMineSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadOpenCast(AMine : TMine) : boolean;
    function LoadUnderground(AMine : TMine) : boolean;
    function LoadSlurryDump(AMine : TMine) : boolean;
    function LoadMineSubCatchment(AMineSubCatchmentList : TMineSubCatchmentList) : boolean;
  public
    function ConstructData(AMineList : TMineList;
                           AMineSubCatchmentList: TMineSubCatchmentList) : boolean;
  end;

implementation
uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations, DB;

{ TMineLoadAgent }

procedure TMineLoadAgent.CreateMemberObjects;
const OPNAME = 'TMineLoadAgent.CreateMemberObjects';
begin
  inherited;
  try
    FSQLAgent :=  TMineSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineLoadAgent.DestroyMemberObjects;
const OPNAME = 'TMineLoadAgent.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FSQLAgent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineLoadAgent.ConstructData(AMineList: TMineList;
                                      AMineSubCatchmentList: TMineSubCatchmentList): boolean;
const OPNAME = 'TMineLoadAgent.ConstructData';
var
  LEvapDataset,
  LDataSet                   : TAbstractModelDataset;
  LMine                      : TMine;
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
      lDataSet.SetSQL(FSQLAgent.GetMineSQL);
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

        LMine := AMineList.NewMine;
        LMine.Populate(LMineIdentifier,LMineNodeNumber,LMineName,LRiverChannelNumber,LPCDChannelNumber,
                       LHydrologyNodeNumber,LBeneficiationPlantArea,LBeneficiationRunOffFactor);

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

function TMineLoadAgent.LoadOpenCast(AMine: TMine): boolean;
const OPNAME = 'TMineLoadAgent.LoadOpenCast';
var
  LRechargeFactorsDataset,
  LDataSet                   : TAbstractModelDataset;
  LOpenCast                  : TOpenCast;
  LIndex,
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
begin
  Result := FALSE;
  try
    if(AMine = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LRechargeFactorsDataset);
    try
      lDataSet.SetSQL(FSQLAgent.GetOpenCastSQL(AMine.Identifier));
      lDataset.DataSet.Open;
      while (NOT lDataset.DataSet.EOF) do
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

        LOpenCast := AMine.NewOpenCast;
        LOpenCast.Populate(AMine.Identifier,LOpenCastIdentifier,LPitName,LCoalReserveArea,LWorkingsArea,LDisturbedWorkingsArea,
                          LDisturbedArea,LWaterSurfaceEvapArea,LDisturbedAreaRunOff,LDisturbedWorkingsAreaRunOff,
                          LDecantVolume,LSeepageVolume,LAnalysisStartVolume,LMaximumSeepageRate,LSeepageExponent,
                          LPCDSurfaceArea,LPCDStorageCapacity,LPCDAnalysisStartVolume);

        LRechargeFactorsDataset.DataSet.Close;
        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(AMine.Identifier,1,LOpenCastIdentifier,1));
        LRechargeFactorsDataset.DataSet.Open;
        if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LDisturbedRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LOpenCast.PopulateDisturbedRechargeFactors(LDisturbedRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;

        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(AMine.Identifier,1,LOpenCastIdentifier,2));
        LRechargeFactorsDataset.DataSet.Open;
       if Not LRechargeFactorsDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LWorkingAreaRechargeFactor[LIndex] := LRechargeFactorsDataset.DataSet.FieldByName(Format('RechargeFactor%2.2d',[LIndex])).AsFloat;
          end;
          LOpenCast.PopulateWorkingAreaRechargeFactors(LWorkingAreaRechargeFactor);
        end;
        LRechargeFactorsDataset.DataSet.Close;

        LDataset.DataSet.Next;
      end;

      lDataset.DataSet.Close;
      Result := TRUE;
    finally
      LRechargeFactorsDataset.Free;
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TMineLoadAgent.LoadUnderground(AMine: TMine): boolean;
const OPNAME = 'TMineLoadAgent.LoadUnderground';
var
  LRechargeFactorsDataset,
  LDataSet                          : TAbstractModelDataset;
  LUnderground                      : TUnderground;
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
  Result := False;
  try
    if(AMine = nil) then Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LRechargeFactorsDataset);
    try
      lDataSet.SetSQL(FSQLAgent.GetUndergroundSQL(AMine.Identifier));
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

        LUnderground := AMine.NewUnderground;
        LUnderground.Populate(AMine.Identifier,LUndergroundIdentifier,LUndergroundSectionName,LChannelNumberToUGDam,
                              LUpstreamCatchmentArea, LBoardPillarCatchmentArea,LHighExtractionCatchmentArea,
                              LHighExtractionAreaRunoffFactor);

        LRechargeFactorsDataset.DataSet.Close;
        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetUGUpstreamRunoffSQL(AMine.Identifier,LUndergroundIdentifier));
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

        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(AMine.Identifier,2,LUndergroundIdentifier,3));
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

        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(AMine.Identifier,2,LUndergroundIdentifier,4));
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

        lDataset.DataSet.Next;
      end;
      lDataset.DataSet.Close;
      Result := True;
    finally
      LRechargeFactorsDataset.Free;
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TMineLoadAgent.LoadSlurryDump(AMine: TMine): boolean;
const OPNAME = 'TMineLoadAgent.LoadSlurryDump';
var
  LRechargeFactorsDataset,
  LDataSet                   : TAbstractModelDataset;
  LSlurryDump                : TSlurryDump;
  LIndex,
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
  Result := FALSE;
  try
    if(AMine = nil) then Exit;
    
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LRechargeFactorsDataset);
    try
      lDataSet.SetSQL(FSQLAgent.GetSlurryDumpSQL(AMine.Identifier));
      lDataset.DataSet.Open;
      while (NOT lDataset.DataSet.EOF) do
      begin
        LSlurryDumpIdentifier        := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LDumpName                    := Trim(LDataset.DataSet.FieldByName('DumpName').AsString);
        LDumpSurfaceArea             := LDataset.DataSet.FieldByName('DumpSurfaceArea').AsFloat;
        LRunoffFactorToPCD           := LDataset.DataSet.FieldByName('RunoffFactorToPCD').AsFloat;
        LSeepageSplitFactor          := LDataset.DataSet.FieldByName('SeepageSplitFactor').AsFloat;
        LPCDStorageCapacity          := LDataset.DataSet.FieldByName('PCDStorageCapacity').AsFloat;
        LPCDSurfaceArea              := LDataset.DataSet.FieldByName('PCDSurfaceArea').AsFloat;
        LPCDAnalysisStartVolume      := LDataset.DataSet.FieldByName('PCDAnalysisStartVolume').AsFloat;

        LSlurryDump := AMine.NewSlurryDump;
        LSlurryDump.Populate(AMine.Identifier,LSlurryDumpIdentifier,LDumpName,LDumpSurfaceArea,LRunoffFactorToPCD,LSeepageSplitFactor,
                             LPCDStorageCapacity,LPCDSurfaceArea,LPCDAnalysisStartVolume);

        LRechargeFactorsDataset.DataSet.Close;
        LRechargeFactorsDataset.SetSQL(FSQLAgent.GetRechargeFactorSQL(AMine.Identifier,3,LSlurryDumpIdentifier,5));
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

        lDataset.DataSet.Next;
      end;

      Result := TRUE;
      lDataset.DataSet.Close;
    finally
      lDataset.Free;
      LRechargeFactorsDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TMineLoadAgent.LoadMineSubCatchment(AMineSubCatchmentList: TMineSubCatchmentList): boolean;
const OPNAME = 'TMineLoadAgent.LoadMineSubCatchment';
var
  LFlowVolumeDataset           : TAbstractModelDataset;
  LDataSet                     : TAbstractModelDataset;
  LIndex                       : integer;
  LIdentifier                  : integer;
  LCatchmentReferenceNumber    : integer;
  LCatchmentRefName            : string;
  LProportionAntecedentFlow    : double;
  LGroundwaterFlowVolume       : double;
  LAntecedentRunoffDecayFactor : double;
  LCatchmentRefUsed            : boolean;
  LMineSubCatchment            : TMineSubCatchment;
  LFlowVolume                  : TMonthlyDoubleArray;
begin
  Result := FALSE;
  try
    if(AMineSubCatchmentList = nil) then Exit;
    
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LFlowVolumeDataset);
    try
      LDataSet.SetSQL(FSQLAgent.GetMineSubCatchmentSQL);
      LDataSet.DataSet.Open;
      while (NOT LDataSet.DataSet.EOF) do
      begin
        LIdentifier                  := LDataset.DataSet.FieldByName('Identifier').AsInteger;
        LCatchmentReferenceNumber    := LDataset.DataSet.FieldByName('CatchmentReferenceNumber').AsInteger;
        LProportionAntecedentFlow    := LDataset.DataSet.FieldByName('ProportionAntecedentFlow').AsFloat;
        LGroundwaterFlowVolume       := LDataset.DataSet.FieldByName('GroundwaterFlowVolume').AsFloat;
        LAntecedentRunoffDecayFactor := LDataset.DataSet.FieldByName('AntecedentRunoffDecayFactor').AsFloat;
        LCatchmentRefName            := Trim(LDataSet.DataSet.FieldByName('CatchmentReferenceName').AsString);
        LCatchmentRefUsed            := Trim(LDataSet.DataSet.FieldByName('InUse').AsString) = '1';

        LMineSubCatchment := AMineSubCatchmentList.NewMineSubCatchment;
        LMineSubCatchment.Populate(LIdentifier,LCatchmentReferenceNumber,LCatchmentRefName,LProportionAntecedentFlow,
                                   LGroundwaterFlowVolume,LAntecedentRunoffDecayFactor,LCatchmentRefUsed);

        LFlowVolumeDataset.DataSet.Close;
        LFlowVolumeDataset.SetSQL(FSQLAgent.GetMineSubCatchmentFlowVolumeSQL(LIdentifier));
        LFlowVolumeDataset.DataSet.Open;
        if Not LFlowVolumeDataset.DataSet.Eof then
        begin
          for LIndex := 1 to 12 do
          begin
            LFlowVolume[LIndex] := LFlowVolumeDataset.DataSet.FieldByName(Format('Volume%2.2d',[LIndex])).AsFloat;
          end;
          LMineSubCatchment.PopulateMinimunGroundwaterFlowVolume(LFlowVolume);
        end;
        LFlowVolumeDataset.DataSet.Close;
        LDataSet.DataSet.Next;
      end;

      Result := TRUE;
      LDataSet.DataSet.Close;
    finally
      LDataSet.Free;
      LFlowVolumeDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
