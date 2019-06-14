//
//
//  UNIT      : Contains TNetworkFeaturesLoadAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/08/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UNetworkFeaturesLoadAgent;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VCL.Dialogs,
  UNetworkFeaturesSQLAgent,
  UReservoirData,
  UNetworkFeaturesData,
  UDiversionFeatures,
  UPhysicalFlowConstraints,
  UIFRFeatures,
  UIrrigationAreas,
  UIrrigationBlock,
  UPowerPlants,
  USpecifiedInflowFeatures,
  UWaterDemandFeatures,
  UChannelData,
  UChannelAreas,
  UIrrigationBlockSQLAgent,
  UWetland,
  UMiningData,
  UMineSQLAgent,
  UWetlandSQLAgent,
  UYMDemandCentre,
  UYMDemandCentreSQLAgent,
  UStreamFlowReduction,
  UStreamFlowReductionSQLAgent,
  UCurtailmentAndDrought,
  UGroundWater,
  UGroundWaterSQLAgent,
  VoaimsCom_TLB;

type
  TNetworkFeaturesLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent : TNetworkFeaturesSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    { Master Control feature }
    { Diversion feature }
    function LoadDiversionFeatures (AChannelList : TChannelList;
                                    AFeatureList : TDiversionFeatureList) : boolean; {F10}
    function LoadDiversionFeatureName (AChannelNr         : integer;
                                       var AFeatureID     : integer;
                                       var AFeatureName   : string;
                                       var ADiversionType : integer;
                                       var AStation       : string): boolean;
    function LoadDivType1or2Data (AFeatureID            : integer;
                                  var ADiversionDemands : TDivMonthlyDoublesArray;
                                  var ADivertedFlows    : TDivMonthlyDoublesArray): boolean;
    function LoadDivType3Data (AFeatureID               : integer;
                               var AElevationsCount     : integer;
                               var AFlowsCount          : integer;
                               var AReservoir           : TReservoirData;
                               var AReservoirElevations : TReservoirElevationsArray): boolean;
    function LoadDivType3Proportions (AFeatureID                    : integer;
                                      AElevationsCount              : integer;
                                      AReferenceFlowsCount          : integer;
                                      var AReferenceFlows           : TReferenceFlowsArray;
                                      var ADivertedFlowProportions  : TDivertedFlowProportionsArray): boolean;
    { Physical flow constraint }
    function LoadPhysicalFlowConstraints (AChannelList : TChannelList;
                                          AFeatureList : TPhysicalFlowConstraintList) : boolean; {F04}
    function LoadInflowIFRData ( AFeatureID    : integer;
                                var AInflows  : TIFRArray;
                                var AReleases : TIFRArray;
                                var AAnnualInflow : TExceedencePercentagesArray): boolean; {F14}
    { Irrigation area }
    function LoadIrrigationAreas (AChannelList : TChannelList;
                                  AFeatureList : TIrrigationAreaList): boolean; {F09}
    function LoadMonthlyDiversionFlowData (AFeatureID                 : integer;
                                           var AMonthlyDiversionFlows : TIrrMonthlyDoublesArray): boolean;
    function LoadMonthlyReturnFlowData (AFeatureID              : integer;
                                        var AMonthlyReturnFlows : TIrrMonthlyDoublesArray) : boolean;
    { Irrigation Block }
    function LoadIrrigationBlockList(AChannelList : TChannelList;
                                     AFeature     : TIrrigationBlockList): Boolean; {F17}
    { Wetland }
    function LoadWetlands(AChannelList : TChannelList;
                             AFeature     : TWetlandList): Boolean; {F18}
    { GroundWater }
    function LoadGroundWaters(AFeature     : TGroundWaterList): Boolean; {F22}
    function LoadGroundWaterSubCatchment(AGroundWater: TGroundWater): Boolean; {F22}
    { Demand Centre }
    function LoadYMDemandCentres(AChannelList : TChannelList;
                             AFeature     : TYMDemandCentreList): Boolean; {F19}
    { Stream Flow Reduction }
    function LoadStreamFlowReduction(AFeature : TStreamFlowReductionList): Boolean; {F20}

    { Power plant }
    function LoadPowerPlants (AChannelList : TChannelList;
                              AFeatureList : TPowerPlantList): boolean; {F03}
    function LoadPowerPlantDetails (AFeatureID                : integer;
                                    ANetHeadEfficiencyCount   : integer;
                                    ATailwaterElevationsCount : integer;
                                    var AEfficiencyFactors    : TEfficiencyFactorsArray;
                                    var ANetHeadFactors       : TNetHeadFactorsArray;
                                    var ADischarges           : TDischargesArray;
                                    var ATailwaterElevations  : TElevationsArray;
                                    var ATailwaterType        : integer): boolean; {F07}
    function LoadPowerDemand (APowerChannelNr                    : integer;
                              var AMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
                              var AMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray): boolean; {F08}
    { Specified Inflow }
    function LoadSpecifiedInflowData (AChannelList : TChannelList;
                                      AFeatureList : TSpecifiedInflowFeatureList): boolean; {F03}
    { Water Demand Configuration }
    function LoadWaterDemandConfiguration (AWaterDemandConfiguration : TWaterDemandConfiguration): boolean; {F16}

    { Water Demand Feature }
    function LoadWaterDemandFeatures (AChannelList : TChannelList;
                                      AFeatureList : TWaterDemandFeatureList): boolean; {F16}

   {Channel Areas}
    function LoadChannelArea(AChannelArea : TChannelAreaList): boolean;

    function LoadCurtailmentAndDrought(ACurtailmentAndDrought : TCurtailmentAndDrought): boolean;
    function LoadCurtailmentChannel(ACurtailmentAndDrought: TCurtailmentAndDrought): boolean;
    function LoadDroughtRestriction(ACurtailmentAndDrought: TCurtailmentAndDrought): boolean;

  public
    { IFR Feature (In-stream Flow Requirement) }
    function LoadIFRFeatures (AChannelList : TChannelList;
                              AFeatureList : TIFRFeatureList): boolean; {F14}

    function ConstructData (AChannelList                : TChannelList;
                            ADiversionFeatureList       : TDiversionFeatureList;
                            APhysicalFlowConstraintList : TPhysicalFlowConstraintList;
                            AIFRFeatureList             : TIFRFeatureList;
                            AIrrigationAreaList         : TIrrigationAreaList;
                            AIrrigationBlockList        : TIrrigationBlockList;
                            AWetlandList                : TWetlandList;
                            AYMDemandCentreList         : TYMDemandCentreList;
                            APowerPlantList             : TPowerPlantList;
                            ASpecifiedInflowFeatureList : TSpecifiedInflowFeatureList;
                            AWaterDemandConfiguration   : TWaterDemandConfiguration;
                            AWaterDemandFeatureList     : TWaterDemandFeatureList;
                            AChannelAreaList            : TChannelAreaList ;
                            AStreamFlowReductionList    : TStreamFlowReductionList;
                            ACurtailmentAndDrought      : TCurtailmentAndDrought;
                            AGroundWaterList            : TGroundWaterList): boolean;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UDataSetType,
  UYieldModelDataObject,
  UErrorHandlingOperations, DB;

procedure TNetworkFeaturesLoadAgent.CreateMemberObjects;
const OPNAME = 'TNetworkFeaturesLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent                 := TNetworkFeaturesSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TNetworkFeaturesLoadAgent.DestroyMemberObjects;
const OPNAME = 'TNetworkFeaturesLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TNetworkFeaturesLoadAgent.ConstructData (
                              AChannelList                : TChannelList;
                              ADiversionFeatureList       : TDiversionFeatureList;
                              APhysicalFlowConstraintList : TPhysicalFlowConstraintList;
                              AIFRFeatureList             : TIFRFeatureList;
                              AIrrigationAreaList         : TIrrigationAreaList;
                              AIrrigationBlockList        : TIrrigationBlockList;
                              AWetlandList                : TWetlandList;
                              AYMDemandCentreList         : TYMDemandCentreList;
                              APowerPlantList             : TPowerPlantList;
                              ASpecifiedInflowFeatureList : TSpecifiedInflowFeatureList;
                              AWaterDemandConfiguration   : TWaterDemandConfiguration;
                              AWaterDemandFeatureList     : TWaterDemandFeatureList;
                              AChannelAreaList            : TChannelAreaList;
                              AStreamFlowReductionList    : TStreamFlowReductionList;
                              ACurtailmentAndDrought      : TCurtailmentAndDrought;
                              AGroundWaterList            : TGroundWaterList) : boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.ConstructData';
begin
  Result := False;
  try
    LoadDiversionFeatures(AChannelList, ADiversionFeatureList);
    LoadPhysicalFlowConstraints(AChannelList, APhysicalFlowConstraintList);
    LoadIFRFeatures(AChannelList, AIFRFeatureList);
    LoadIrrigationAreas(AChannelList, AIrrigationAreaList);
    LoadPowerPlants(AChannelList, APowerPlantList);
    LoadSpecifiedInflowData(AChannelList, ASpecifiedInflowFeatureList);
    LoadWaterDemandConfiguration(AWaterDemandConfiguration);
    LoadWaterDemandFeatures(AChannelList, AWaterDemandFeatureList);
    LoadChannelArea(AChannelAreaList);
    LoadCurtailmentAndDrought(ACurtailmentAndDrought);

    if FAppModules.StudyArea.ModelVersion = '7' then
      LoadIrrigationBlockList(AChannelList,AIrrigationBlockList);
    if FAppModules.StudyArea.ModelVersion = '7' then
      LoadWetlands(AChannelList, AWetlandList);
    if FAppModules.StudyArea.ModelVersion = '7' then
      LoadYMDemandCentres(AChannelList, AYMDemandCentreList);
    if FAppModules.StudyArea.ModelVersion = '7' then
      LoadStreamFlowReduction(AStreamFlowReductionList);
    if FAppModules.StudyArea.ModelVersion = '7' then
      LoadGroundWaters(AGroundWaterList);
    Result := TRUE;
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

{******************************************************************************}
{* Diversion Features                                                         *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadDiversionFeatures(
                                     AChannelList : TChannelList;
                                     AFeatureList : TDiversionFeatureList): boolean; {F10}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadDiversionFeatures';
var
  lDataSet                  : TAbstractModelDataset;
  lDiversionFeature         : TDiversionFeature;
  lChannelNr                : integer;
  lResult                   : boolean;
  lNewChannel               : Boolean;
  lFeatureID                : integer;
  LStation,
  lFeatureName              : string;
  lChannel                  : TGeneralFlowChannel;
  lFeatureType              : integer;
  lFeatureSubType           : integer;
  lDiversionType            : integer;
  lDiversionDemands         : TDivMonthlyDoublesArray;
  lDivertedFlows            : TDivMonthlyDoublesArray;
  lControllingReservoir     : TReservoirData;
  lReservoirElevationsCount : integer;
  lReferenceFlowsCount      : integer;
  lReservoirElevations      : TReservoirElevationsArray;
  lReferenceFlows           : TReferenceFlowsArray;
  lDivertedFlowProportions  : TDivertedFlowProportionsArray;
  lIndexA                   : integer;
  lIndexB                   : integer;
  LDiversionDemandsField,
  LDivertedFlowsField,
  LReservoirElevationsField,
  LReferenceFlowsField,
  LDivertedFlowProportionsField: TAbstractFieldProperty;
  LDiversionGaugeList : TStringList;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LDiversionDemandsField := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if not Assigned(LDiversionDemandsField) then
      raise Exception.Create('Field (DiversionDemand) not found in field properties');
    SetLength(lDiversionDemands,LDiversionDemandsField.ArrayLength);

    LDivertedFlowsField := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if not Assigned(LDivertedFlowsField) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(lDivertedFlows,LDivertedFlowsField.ArrayLength);

    LReservoirElevationsField := FAppModules.FieldProperties.FieldProperty('ControllingResLevels');
    if not Assigned(LReservoirElevationsField) then
      raise Exception.Create('Field (ControllingResLevels) not found in field properties');
    SetLength(lReservoirElevations,LReservoirElevationsField.ArrayLength);

    LReferenceFlowsField := FAppModules.FieldProperties.FieldProperty('FlowValue');
    if not Assigned(LReferenceFlowsField) then
      raise Exception.Create('Field (FlowRange) not found in field properties');
    SetLength(lReferenceFlows,LReferenceFlowsField.ArrayLength);

    LDivertedFlowProportionsField := FAppModules.FieldProperties.FieldProperty('DivertedFlow');
    if not Assigned(LDivertedFlowProportionsField) then
      raise Exception.Create('Field (DivertedFlow) not found in field properties');
    SetLength(lDivertedFlowProportions,LDivertedFlowProportionsField.ArrayLength, LDivertedFlowProportionsField.ArrayLength(1));
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    LDiversionGaugeList := TStringList.Create;
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.ClearSQL;
        LDataSet.SetSQL(FSQLAgent.GetDiversionGaugeListSQL);
        LDataset.DataSet.Open;
        while not LDataset.DataSet.Eof do
        begin
          LDiversionGaugeList.Add(Trim(LDataSet.DataSet.FieldByName('StationNo').AsString));
          LDataset.DataSet.Next;
        end;
        if LDiversionGaugeList.Count > 0 then
          AFeatureList.Set_DiversionGaugeList(LDiversionGaugeList.CommaText);
        LDataset.DataSet.Close;
        LDataSet.ClearSQL;
        LDataSet.SetSQL(FSQLAgent.GetDiversionFeaturesSQL);
        LDataset.DataSet.Open;
        while (lResult AND (NOT LDataset.DataSet.EOF)) do
        begin
          lNewChannel := FALSE;
          lChannelNr  := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          lChannel    := AChannelList.CastChannelByChannelNumber[lChannelNr];
          lDiversionType  := -1;
          lFeatureType    := 5;
          lFeatureSubType := 0;
          if (NOT Assigned(lChannel)) then
          begin
            lChannel    := AChannelList.NewGeneralFlowChannel;
            lChannel.Initialise;
            lNewChannel := TRUE;
            if (NOT lChannel.PopulateGeneralFlowChannel
                      (LDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                       lChannelNr,
                       LDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                       LDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                       LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                       LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                       LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                       LDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('FirmYieldCalc').AsString),
                       Trim(LDataSet.DataSet.FieldByName('FlowOutput').AsString))) then
              lResult := FALSE;
          end;

          if (lResult) then
            lResult := LoadDiversionFeatureName(lChannelNr, lFeatureID, lFeatureName, lDiversionType,LStation);
          if (lResult) then
          begin
            if (lDiversionType in [1,2,4]) then
            begin
              for lIndexA := LDiversionDemandsField.ArrayLow to LDiversionDemandsField.ArrayHigh do
                lDiversionDemands[lIndexA] := NullFloat;
              for lIndexA := LDivertedFlowsField.ArrayLow to LDivertedFlowsField.ArrayHigh do
                lDivertedFlows[lIndexA]    := NullFloat;
              lResult := LoadDivType1or2Data(lFeatureID, lDiversionDemands, lDivertedFlows);
            end
            else
            begin
              for lIndexA := LReservoirElevationsField.ArrayLow to LReservoirElevationsField.ArrayHigh do
                lReservoirElevations[lIndexA] := NullFloat;
              for lIndexA := LReferenceFlowsField.ArrayLow to LReferenceFlowsField.ArrayHigh do
              begin
                lReferenceFlows[lIndexA] := NullFloat;
                for lIndexB := LReservoirElevationsField.ArrayLow to LReservoirElevationsField.ArrayHigh do
                  lDivertedFlowProportions[lIndexA, lIndexB] := NullFloat;
              end;
              lResult := LoadDivType3Data(lFeatureID, lReservoirElevationsCount,
                                          lReferenceFlowsCount, lControllingReservoir,
                                          lReservoirElevations) AND
                         LoadDivType3Proportions(lFeatureID, lReservoirElevationsCount,
                                                 lReferenceFlowsCount, lReferenceFlows,
                                                 lDivertedFlowProportions);
            end;
          end;
          if (lResult) then
          begin
            lDiversionFeature := AFeatureList.NewDiversionFeature;
            lDiversionFeature.Initialise;
            if (lDiversionType in [1,2,4]) then
              lResult := lDiversionFeature.PopulateType1or2
                           (lFeatureID, lFeatureName,LStation ,lChannel.ChannelNumber, lFeatureType, lFeatureSubType,
                            lDiversionType, lDiversionDemands, lDivertedFlows)
            else
              lResult := lDiversionFeature.PopulateType3
                           (lFeatureID, lFeatureName, lChannel.ChannelNumber, lFeatureType, lFeatureSubType,
                            lDiversionType, lControllingReservoir, lReservoirElevationsCount,
                            lReferenceFlowsCount, lReservoirElevations, lReferenceFlows,
                            lDivertedFlowProportions);
            if (lResult) then
            begin
              lChannel.DiversionFeature := lDiversionFeature;
            end
            else
            begin
              AFeatureList.DeleteDiversionFeatureWithID(lFeatureID);
              if (lNewChannel) then
                AChannelList.DeleteGeneralFlowChannelWithID(lChannel.ChannelID);
            end;
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
      LDiversionGaugeList.Free;
    end;
    Result := LResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadDiversionFeatureName (AChannelNr         : integer;
                                                             var AFeatureID     : integer;
                                                             var AFeatureName   : string;
                                                             var ADiversionType : integer;
                                                             var AStation       : string): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadDiversionFeatureName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result       := FALSE;
  AFeatureName := '';
  AFeatureID   := -1;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDiversionFeatureNameSQL(AChannelNr));
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount > 0) then
        begin
          AFeatureName   := Trim(LDataset.DataSet.FieldByName('DivChannelName').AsString);
          AFeatureID     := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          ADiversionType := LDataset.DataSet.FieldByName('DivChannelType').AsInteger;
          AStation       := Trim(LDataset.DataSet.FieldByName('DivStation').AsString);
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadDivType1or2Data (AFeatureID            : integer;
                                                        var ADiversionDemands : TDivMonthlyDoublesArray;
                                                        var ADivertedFlows    : TDivMonthlyDoublesArray): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadDivType1or2Data';
var
  LDataSet : TAbstractModelDataset;
  LIndex   : integer;
  lCode    : integer;
  lField   : string;
  LDiversionDemands,
  LDivertedFlows:TAbstractFieldProperty;
begin
  Result := FALSE;
  try
  LDiversionDemands := FAppModules.FieldProperties.FieldProperty('DiversionDemand');
    if not Assigned(LDiversionDemands) then
      raise Exception.Create('Field (DiversionDemand) not found in field properties');
    SetLength(ADiversionDemands,LDiversionDemands.ArrayLength);

    LDivertedFlows := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if not Assigned(LDivertedFlows) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(ADivertedFlows,LDivertedFlows.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDivType1n2DataSQL(AFeatureID));
        LDataset.DataSet.Open;
        while (NOT LDataset.DataSet.EOF) do
        begin
          lCode  := LDataset.DataSet.FieldByName('DiversionCode').AsInteger;
          case lCode of
          1 :
            begin
              for LIndex := 1 to 12 do
              begin
                lField := Trim(LDataset.DataSet.FieldByName(Format('DivFactor%2.2d',[LIndex])).AsString);
                if (Trim(lField) <> '') then
                  ADiversionDemands[LIndex] := StrToFloat(lField)
                else
                  ADiversionDemands[LIndex] := NullFloat;
              end;
            end;
          2 :
            begin
              for LIndex := 1 to 12 do
              begin
                lField := Trim(LDataset.DataSet.FieldByName(Format('DivFactor%2.2d',[LIndex])).AsString);
                if (Trim(lField) <> '') then
                  ADivertedFlows[LIndex] := StrToFloat(lField)
                else
                  ADivertedFlows[LIndex] := NullFloat;
              end;
            end;
          else
          end;
          lDataSet.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadDivType3Proportions
                                      (AFeatureID                    : integer;
                                       AElevationsCount              : integer;
                                       AReferenceFlowsCount          : integer;
                                       var AReferenceFlows           : TReferenceFlowsArray;
                                       var ADivertedFlowProportions  : TDivertedFlowProportionsArray): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadDivType3Proportions';
var
  LDataSet : TAbstractModelDataset;
  LCount   : integer;
  lIndex   : integer;
  lField   : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDiversionProportionsSQL(AFeatureID));
        LDataset.DataSet.Open;
        LCount := 0;
        while (NOT LDataset.DataSet.EOF) do
        begin
          LCount := LCount + 1;
          AReferenceFlows[LCount] := LDataset.DataSet.FieldByName('FlowValue').AsFloat;
          lIndex := 1;
          lField := Trim(LDataset.DataSet.FieldByName(Format('DivProp%2.2d',[lIndex])).AsString);
          while (Trim(lField) <> '') do
          begin
            ADivertedFlowProportions[LCount, lIndex] := StrToFloat(lField);
            lIndex := lIndex + 1;
            if (lIndex <= 10) then
              lField := Trim(LDataset.DataSet.FieldByName(Format('DivProp%2.2d',[lIndex])).AsString)
            else
              lField := '';
          end;
          LDataSet.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadDivType3Data (AFeatureID               : integer;
                                                     var AElevationsCount     : integer;
                                                     var AFlowsCount          : integer;
                                                     var AReservoir           : TReservoirData;
                                                     var AReservoirElevations : TReservoirElevationsArray): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadDivType3Data';
var
  LDataSet     : TAbstractModelDataset;
  LIndex       : integer;
  lField       : string;
  lReservoirNr : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirLevelsSQL(AFeatureID));
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount > 0) then
        begin
          AElevationsCount := LDataset.DataSet.FieldByName('ResLevelsCount').AsInteger;
          AFlowsCount      := LDataset.DataSet.FieldByName('RefFlowsCount').AsInteger;
          lReservoirNr     := LDataset.DataSet.FieldByName('NodeNumber').AsInteger;
          AReservoir       := TYieldModelDataObject(FAppModules.Model.ModelData).
                                CastNetworkElementData.CastReservoirList.
                                  CastReservoirByIdentifier[lReservoirNr];
          lIndex := 1;
          lField := Trim(LDataset.DataSet.FieldByName(Format('DivLevel%2.2d',[lIndex])).AsString);
          while (Trim(lField) <> '') do
          begin
            AReservoirElevations[lIndex] := StrToFloat(lField);
            lIndex := lIndex + 1;
            if (lIndex <= 10) then
              lField := Trim(LDataset.DataSet.FieldByName(Format('DivLevel%2.2d',[lIndex])).AsString)
            else
              lField := '';
          end;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Physical Flow Constraints                                                  *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadPhysicalFlowConstraints
                                              (AChannelList : TChannelList;
                                               AFeatureList : TPhysicalFlowConstraintList): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadPhysicalFlowConstraints';
var
  lDataSet                     : TAbstractModelDataset;
  lChannelNr                   : integer;
  lChannel                     : TGeneralFlowChannel;
  lPhysicalFlowConstraint      : TPhysicalFlowConstraint;
  lNrOfPoints                  : integer;
  lFeatureID                   : integer;
  lFeatureName                 : string;
  lUpstreamReservoirNr         : integer;
  lDownstreamReservoirNr       : integer;
  lStructureType               : integer;
  lElevationOfSill             : double;
  lMaximumGateHeight           : double;
  lDischargeCoefficient        : double;
  lStructureLength             : double;
  lWaterLevelAtADownstreamNode : double;
  lReferenceElevation          : double;  
  lIndex                       : integer;
  LRow,
  lCol                         : integer;
  LGroupNumber                 : integer;
  LSubGroupNumber              : integer;
  LArray1                      : TOneDimensionDoubleArray;
  LArray2                      : TOneDimensionDoubleArray;
  LArray3                      : TOneDimensionDoubleArray;
  LArray4                      : TOneDimensionDoubleArray;
  LTwoDimensionArray           : TTwoDimensionDoubleArray;
  lField : TAbstractFieldProperty;
  LFieldProperty : TAbstractFieldProperty;

begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFieldProperty := FAppModules.FieldProperties.FieldProperty('ConstraintElevationDifferences');
        lField := FAppModules.FieldProperties.FieldProperty('ConstraintMonthlyAverageDivertedFlow');

        LDataSet.SetSQL(FSQLAgent.GetPhysicalFlowConstraintsSQL);
        LDataset.DataSet.Open;
        while (NOT LDataset.DataSet.EOF) do
        begin
          //lFeatureType         := 6;
          lUpstreamReservoirNr := LDataset.DataSet.FieldByName('UpStreamReservoirNumber').AsInteger;
          lDownstreamReservoirNr := LDataset.DataSet.FieldByName('DownStreamReservoirNumber').AsInteger;

          lFeatureID                   := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          lFeatureName                 := Trim(LDataset.DataSet.FieldByName('FeatureName').AsString);
          lStructureType               := LDataset.DataSet.FieldByName('StructureType').AsInteger;
          lElevationOfSill             := LDataset.DataSet.FieldByName('SillElevation').AsFloat;
          lMaximumGateHeight           := LDataset.DataSet.FieldByName('GateHeight').AsFloat;
          lDischargeCoefficient        := LDataset.DataSet.FieldByName('DischargeCoefficient').AsFloat;
          lStructureLength             := LDataset.DataSet.FieldByName('ControlStructureLength').AsFloat;
          lWaterLevelAtADownstreamNode := LDataset.DataSet.FieldByName('WaterLevelAtDownstreamNode').AsFloat;
          lReferenceElevation          := LDataset.DataSet.FieldByName('ReferenceElevation').AsFloat;
          lNrOfPoints                  := LDataset.DataSet.FieldByName('PointsElevationNumber').AsInteger;
          lChannelNr                   := LDataset.DataSet.FieldByName('ConstraintsChannelNumber').AsInteger;
          lChannel                     := AChannelList.CastChannelByChannelNumber[lChannelNr];

          if (Assigned(lChannel)) then
          begin
            lPhysicalFlowConstraint := AFeatureList.NewPhysicalFlowConstraint;
            lPhysicalFlowConstraint.Initialise;
            lPhysicalFlowConstraint.Populate(lFeatureID, lFeatureName, lChannel.ChannelNumber, lUpstreamReservoirNr,
                                             lDownstreamReservoirNr, lStructureType, lElevationOfSill,
                                             lMaximumGateHeight, lDischargeCoefficient, lStructureLength,lNrOfPoints,
                                             lWaterLevelAtADownstreamNode,lReferenceElevation);
             lChannel.PhysicalFlowConstraint := lPhysicalFlowConstraint
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;

        SetLength(LArray1,LFieldProperty.ArrayLength);
        SetLength(LArray2,LFieldProperty.ArrayLength);
        SetLength(LArray3,LFieldProperty.ArrayLength);
        SetLength(LArray4,LFieldProperty.ArrayLength);
        SetLength(LTwoDimensionArray,lField.ArrayLength,lField.ArrayLength(1));
        try
          for lIndex := 0 to AFeatureList.PhysicalFlowConstraintCount-1 do
          begin
            lPhysicalFlowConstraint := AFeatureList.CastPhysicalFlowConstraintByIndex(lIndex);
            if not (lPhysicalFlowConstraint.StructureType in [4,5,7,8,9,10,11,12,13,14]) then Continue;


            LDataset.DataSet.Close;
            LDataSet.SetSQL(FSQLAgent.GetPhysicalFlowConstraintValuesSQL(lPhysicalFlowConstraint.FeatureID));
            LDataset.DataSet.Open;
            if (LDataset.DataSet.Bof and LDataset.DataSet.EOF) then Continue;

            for LCol := 0 to LFieldProperty.ArrayHigh do
            begin
              LArray1[LCol] := NullFloat;
              LArray2[LCol] := NullFloat;
              LArray3[LCol] := NullFloat;
              LArray4[LCol] := NullFloat;
              for LRow := 0 to lField.ArrayHighDimTwo do
                LTwoDimensionArray[LRow,LCol] := NullFloat;
            end;

            LGroupNumber := LDataset.DataSet.FieldByName('GroupNumber').AsInteger;
            while (not LDataset.DataSet.EOF) do
            begin
              LSubGroupNumber := LDataset.DataSet.FieldByName('SubGroupNumber').AsInteger;
              case LGroupNumber of
                pfcgDischargeCurve:
                  begin
                    for LCol := 1 to 10 do
                    begin
                      if LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).IsNull then Continue;
                      if(LSubGroupNumber = pfcstElevation) then
                        LArray1[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstDischarge) then
                        LArray2[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                    end;
                  end;
                pfcgKFactors:
                  begin
                    for LCol := 1 to 10 do
                    begin
                      if LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).IsNull then Continue;
                      if(LSubGroupNumber = pfcstChannelNumber) then
                        LArray1[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstKFactor) then
                        LArray2[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                    end;
                  end;
                pfcgSandAquifer:
                  begin
                    for LCol := 1 to 10 do
                    begin
                      if LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).IsNull then Continue;
                      if(LSubGroupNumber = pfcstHeadDifference) then
                        LArray1[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstAquiferFlow) then
                        LArray2[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstDownStreamNodeInflow) then
                        LArray3[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstRiverDepth) then
                        LArray4[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                    end;
                  end;
                pfcgSubmergedOutlet:
                  begin
                    for LCol := 1 to 10 do
                    begin
                      if LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).IsNull then Continue;
                      if(LSubGroupNumber = pfcstElevationDifference) then
                        LArray1[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstMonthlyAverageInflow) then
                        LArray2[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstMonthlyAverageDivertedFlow) then
                      begin
                        LRow := LDataset.DataSet.FieldByName('LineNumber').AsInteger;
                        LTwoDimensionArray[LRow,LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      end;
                    end;
                  end;
                pfcgPumpStation:
                  begin
                    for LCol := 1 to 10 do
                    begin
                      if LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).IsNull then Continue;
                      if(LSubGroupNumber = pfcstPumpingHead) then
                        LArray1[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      if(LSubGroupNumber = pfcstPumpingDischarge) then
                        LArray2[LCol] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                    end;
                  end;
              end;//case
              LDataset.DataSet.Next;
            end;
            LDataset.DataSet.Close;

            lChannelNr := lPhysicalFlowConstraint.ChannelNumber;
            lFeatureID := lPhysicalFlowConstraint.FeatureID;
            case LGroupNumber of
              pfcgDischargeCurve  : lPhysicalFlowConstraint.CastDischargeCurve.Populate(
                                    lFeatureID,lChannelNr,LArray1,LArray2);
              pfcgKFactors        : lPhysicalFlowConstraint.CastKFactors.Populate(
                                    lFeatureID,lChannelNr,LArray1,LArray2);
              pfcgSandAquifer     : lPhysicalFlowConstraint.CastSandAquifer.Populate(
                                    lFeatureID,lChannelNr,LArray1,LArray2,LArray3,LArray4);
              pfcgSubmergedOutlet : lPhysicalFlowConstraint.CastSubmergedOutlet.Populate(
                                    lFeatureID,lChannelNr,LArray1,LArray2,LTwoDimensionArray);
              pfcgPumpStation     : lPhysicalFlowConstraint.CastPumpStation.Populate(
                                    lFeatureID,lChannelNr,LArray1,LArray2);
            end;//case
          end;
        finally
          Finalize(LArray1);
          Finalize(LArray2);
          Finalize(LArray3);
          Finalize(LArray4);
          Finalize(LTwoDimensionArray);
        end;
      end;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


{******************************************************************************}
{* IFR Features                                                               *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadIFRFeatures (AChannelList : TChannelList;
                                                    AFeatureList : TIFRFeatureList): boolean; {F04}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadIFRFeatures';
var
  lDataSet               : TAbstractModelDataset;
  lChannelNr             : integer;
  lChannel               : TGeneralFlowChannel;
  lIndex                 : integer;
  lMonth                 : integer;
  lIFRFeature            : TIFRFeature;
  lNodeStr               : string;
  lResult                : Boolean;
  LRecordIdentifier      : integer;
  lFeatureName           : string;
  lFeatureType           : integer;
  lFeatureSubType        : integer;
  lLagMonths             : integer;
  lInflowOption          : integer;
  LIFRSiteID             : integer;
  LCalculationOption     : double;
  LExceedenceStr         : string;
  LExceedenceData,
  lReferenceNodeNumbers  : TStringList;
  lInflows               : TIFRArray;
  lReleases              : TIFRArray;
  LAnnualInflow,
  LExceedenceArray       : TExceedencePercentagesArray;
  LIFRVariables,
  LAnnualInflowProperty,
  LExceedenceProperty,
  LIFRReleaseVariables   : TAbstractFieldProperty;
  LReferenceFlowType     : TIFRFeatureReferenceFlowType;
  LIFRFeatureExists      : integer;
  LIFRLoss : integer;
  LMonthlyLoss : string;
begin
  Result := FALSE;
  try
    lResult := TRUE;

    LIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables');
    if not Assigned(LIFRVariables) then
      raise Exception.Create('Field (IFRVariables) not found in field properties');
    SetLength(lInflows,LIFRVariables.ArrayLength, LIFRVariables.ArrayLength(1));

    LIFRReleaseVariables := FAppModules.FieldProperties.FieldProperty('IFRReleaseVariables');
    if not Assigned(LIFRReleaseVariables) then
      raise Exception.Create('Field (IFRReleaseVariables) not found in field properties');
    SetLength(lReleases,LIFRReleaseVariables.ArrayLength, LIFRReleaseVariables.ArrayLength(1));

    LExceedenceProperty := FAppModules.FieldProperties.FieldProperty('ExceedencePercentage');
    if not Assigned(LExceedenceProperty) then
      raise Exception.Create('Field (IFRReleaseVariables) not found in field properties');
    SetLength(LExceedenceArray,LExceedenceProperty.ArrayLength);

    LAnnualInflowProperty := FAppModules.FieldProperties.FieldProperty('AnnualInflow');
    if not Assigned(LAnnualInflowProperty) then
      raise Exception.Create('Field (AnnualInflow) not found in field properties');
    SetLength(LAnnualInflow,LAnnualInflowProperty.ArrayLength);


    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lReferenceNodeNumbers := TStringList.Create;
        LExceedenceData       := TStringList.Create;
        try
          lInflowOption := 1;
          LDataset.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetIFRReferenceSQL);
          LDataset.DataSet.Open;
          if NOT LDataset.DataSet.EOF then
            lInflowOption := LDataset.DataSet.FieldByName('InflowOption').AsInteger;
          AFeatureList.PopulateInflowOption(lInflowOption);

          LDataset.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetIFRFeaturesSQL);
          LDataset.DataSet.Open;
          while ((NOT LDataset.DataSet.EOF) AND lResult) do
          begin
            lReferenceNodeNumbers.Clear;
            lFeatureType       := 7;
            lFeatureSubType    := 0;
            LRecordIdentifier  := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
            lLagMonths         := LDataset.DataSet.FieldByName('LagInMonthsCount').AsInteger;
            lChannelNr         := LDataset.DataSet.FieldByName('IFRChannelNumber').AsInteger;
            lFeatureName       := Trim(LDataset.DataSet.FieldByName('FeatureName').AsString);
            LIFRSiteID         := LDataset.DataSet.FieldByName('IFRSiteID').AsInteger;
            LExceedenceStr     := Trim(LDataset.DataSet.FieldByName('ExceedencePerc').AsString);
            LReferenceFlowType := TIFRFeatureReferenceFlowType(LDataset.DataSet.FieldByName('ReferenceFlowType').AsInteger);
            LIFRFeatureExists  := LDataset.DataSet.FieldByName('IFRStatusIndicator').AsInteger;
            LIFRLoss           := LDataset.DataSet.FieldByName('IFRLoss').AsInteger;
            LMonthlyLoss       := LDataset.DataSet.FieldByName('MonthlyIFRLoss').AsString;
            LCalculationOption   := NullFloat;
            if not LDataset.DataSet.FieldByName('CalculationOption').IsNull then
              LCalculationOption   := LDataset.DataSet.FieldByName('CalculationOption').AsFloat;
            lChannel        := AChannelList.CastChannelByChannelNumber[lChannelNr];

            if (Assigned(lChannel)) then
            begin
              lNodeStr := Trim(LDataset.DataSet.FieldByName('RefNodeNumbers').AsString);
              lReferenceNodeNumbers.Clear;
              lReferenceNodeNumbers.CommaText := lNodeStr;
            end
            else
              lResult := FALSE;

            if (lResult) then
            begin
              for lIndex := LIFRVariables.ArrayLow to LIFRVariables.ArrayHigh do
              begin
                for lMonth := 1 to 12 do
                begin
                  lInflows[lIndex, lMonth]  := NullFloat;
                  lReleases[lIndex, lMonth] := NullFloat;
                end;
                if (LIndex <= 10) then
                  LAnnualInflow[LIndex] := NullFloat;
              end;

              lResult := LoadInflowIFRData(LRecordIdentifier{lFeatureID}, lInflows, lReleases,LAnnualInflow);

            end;
            if (lResult) then
            begin
              LExceedenceData.CommaText := LExceedenceStr;
              for LIndex := Low(LExceedenceArray) to High(LExceedenceArray) do
                LExceedenceArray[LIndex] := NullFloat;
              for lIndex := 0 to LExceedenceData.Count-1 do
                LExceedenceArray[lIndex+1] := StrToFloatDef(LExceedenceData.Strings[lIndex],NullFloat);

              if (LReferenceFlowType = ifrtMonthly) then
                lIFRFeature := AFeatureList.NewMonthlyIFRFeature
              else
                lIFRFeature := AFeatureList.NewAnnualIFRFeature;

              lIFRFeature.Initialise;
              lResult     := lIFRFeature.Populate
                             (LRecordIdentifier, lFeatureName, lChannel.ChannelNumber, lFeatureType,
                             lFeatureSubType, lLagMonths, LCalculationOption,LIFRSiteID,LReferenceFlowType,
                             LExceedenceArray,
                             lReferenceNodeNumbers,lInflows, lReleases,LAnnualInflow,LIFRFeatureExists,LIFRLoss,LMonthlyLoss);

              if (lResult) then
                lChannel.IFRFeature := lIFRFeature
              else
                AFeatureList.DeleteIFRFeatureWithID(LRecordIdentifier);
            end;
            LDataset.DataSet.Next;
          end;
        finally
          FreeAndNil(LExceedenceData);
          FreeAndNil(lReferenceNodeNumbers);
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadInflowIFRData (AFeatureID    : integer;
                                                      var AInflows  : TIFRArray;
                                                      var AReleases : TIFRArray;
                                                      var AAnnualInflow : TExceedencePercentagesArray): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadInflowIFRData';
var
  lDataSet       : TAbstractModelDataset;
  lIndex         : integer;
  lMonth         : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMonthlyInflowIFRPairsSQL(AFeatureID));
        LDataset.DataSet.Open;
        Result := TRUE;
        lIndex := 0;
        while (NOT LDataset.DataSet.EOF) do
        begin
          lIndex := lIndex + 1;
          for lMonth := 1 to 12 do
          begin
            AInflows[lIndex, lMonth]  := LDataset.DataSet.FieldByName(Format('InflowVar%2.2d',[lMonth])).AsFloat;
            AReleases[lIndex, lMonth] := LDataset.DataSet.FieldByName(Format('ReleaseVar%2.2d',[lMonth])).AsFloat;
          end;
          
          if not (LDataset.DataSet.FieldByName('AnnualInflow').IsNull) and (LIndex <= 10) then
            AAnnualInflow[LIndex-1] := LDataset.DataSet.FieldByName('AnnualInflow').AsFloat;

          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Irrigation Areas                                                           *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadIrrigationAreas
                                           (AChannelList : TChannelList;
                                            AFeatureList : TIrrigationAreaList): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadIrrigationAreas';
var
  lAreaDataSet           : TAbstractModelDataset;
  lDataSet               : TAbstractModelDataset;
  lIrrigationArea        : TIrrigationArea;
  lDiversionChannel      : TGeneralFlowChannel;
  lReturnFlowChannel     : TGeneralFlowChannel;
  lConsumptiveChannel    : TGeneralFlowChannel;
  lIrrigationNodeNumber  : integer;
  lDiversionChannelNr    : integer;
  lConsumptiveChannelNr  : integer;
  lReturnFlowChannelNr   : integer;
  lResult                : boolean;
  lNewDiversion          : boolean;
  lNewReturnFlow         : boolean;
  lNewConsumptive        : boolean;
  lFeatureID             : integer;
  lFeatureName           : string;
  lFeatureType           : integer;
  lFeatureSubType        : integer;
  lIrrigationPolicy      : integer;
  lMonth                 : integer;
  lMonthlyDiversionFlows : TIrrMonthlyDoublesArray;
  lMonthlyReturnFlows    : TIrrMonthlyDoublesArray;
  LMonthlyDiversionFlowsField,
  LMonthlyReturnFlowsField: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LMonthlyDiversionFlowsField := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
    if not Assigned(LMonthlyDiversionFlowsField) then
      raise Exception.Create('Field (DiversionFlow) not found in field properties');
    SetLength(lMonthlyDiversionFlows,LMonthlyDiversionFlowsField.ArrayLength);

    LMonthlyReturnFlowsField := FAppModules.FieldProperties.FieldProperty('ReturnFlow');
    if not Assigned(LMonthlyReturnFlowsField) then
      raise Exception.Create('Field (ReturnFlow) not found in field properties');
    SetLength(lMonthlyReturnFlows,LMonthlyReturnFlowsField.ArrayLength);

    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lAreaDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lAreaDataSet.SetSQL(FSQLAgent.GetIrrigationAreasSQL);
        lAreaDataSet.DataSet.Open;
        while ((NOT LAreaDataset.DataSet.EOF) AND LResult) do
        begin
          lConsumptiveChannel   := nil;
          lReturnFlowChannel    := nil;
          lNewDiversion         := FALSE;
          lNewReturnFlow        := FALSE;
          lNewConsumptive       := FALSE;
          lFeatureType          := 8;
          lFeatureSubType       := 0;
          lFeatureName          := Trim(lAreaDataSet.DataSet.FieldByName('AreaName').AsString);
          lFeatureID            := lAreaDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger;
          lIrrigationPolicy     := lAreaDataSet.DataSet.FieldByName('RelaxationDemand').AsInteger;
          lDiversionChannelNr   := lAreaDataSet.DataSet.FieldByName('DiversionChannelNumber').AsInteger;
          lConsumptiveChannelNr := lAreaDataSet.DataSet.FieldByName('ConsumptiveChannelNumber').AsInteger;
          lReturnFlowChannelNr  := lAreaDataSet.DataSet.FieldByName('ReturnFlowChannelNumber').AsInteger;
          lIrrigationNodeNumber := lAreaDataSet.DataSet.FieldByName('IrrigationNodeNumber').AsInteger;

          lDiversionChannel     := AChannelList.CastChannelByChannelNumber[lDiversionChannelNr];
          if (NOT Assigned(lDiversionChannel)) then
          begin
            lDiversionChannel := AChannelList.NewGeneralFlowChannel;
            lDiversionChannel.Initialise;
            lNewDiversion     := TRUE;
            LDataSet.SetSQL(FSQLAgent.GetChannelByNumberSQL(lDiversionChannelNr));
            LDataSet.DataSet.Open;
            if ((lDataSet.DataSet.RecordCount = 0) OR
                (NOT lDiversionChannel.PopulateGeneralFlowChannel
                      (lDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                       lDiversionChannelNr,
                       lDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                       lDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                       lDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                       LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                       lDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                       Trim(lDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                       lDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('FirmYieldCalc').AsString),
                       Trim(LDataSet.DataSet.FieldByName('FlowOutput').AsString)))) then
              lResult := FALSE;
            LDataSet.DataSet.Close;
          end;

          if (lResult AND Assigned(lDiversionChannel)) then
          begin
            lConsumptiveChannel := AChannelList.CastChannelByChannelNumber[lConsumptiveChannelNr];
            if (NOT Assigned(lConsumptiveChannel)) then
            begin
              lConsumptiveChannel := AChannelList.NewGeneralFlowChannel;
              lConsumptiveChannel.Initialise;
              lNewConsumptive     := TRUE;
              LDataSet.SetSQL(FSQLAgent.GetChannelByNumberSQL(lConsumptiveChannelNr));
              LDataSet.DataSet.Open;
              if ((lDataSet.DataSet.RecordCount = 0) OR
                  (NOT lConsumptiveChannel.PopulateGeneralFlowChannel
                        (lDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                         lConsumptiveChannelNr,
                         lDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                         lDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                         lDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                         LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                         lDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                         Trim(lDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                         lDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('FirmYieldCalc').AsString),
                         Trim(LDataSet.DataSet.FieldByName('FlowOutput').AsString)))) then
                lResult := FALSE;
              LDataSet.DataSet.Close;
            end;
          end;

          if (lResult AND Assigned(lConsumptiveChannel)) then
          begin
            lReturnFlowChannel := AChannelList.CastChannelByChannelNumber[lReturnFlowChannelNr];
            if (NOT Assigned(lReturnFlowChannel)) then
            begin
              lReturnFlowChannel := AChannelList.NewGeneralFlowChannel;
              lReturnFlowChannel.Initialise;
              lNewReturnFlow     := TRUE;
              LDataSet.SetSQL(FSQLAgent.GetChannelByNumberSQL(lReturnFlowChannelNr));
              LDataSet.DataSet.Open;
              if ((lDataSet.DataSet.RecordCount = 0) OR
                  (NOT lReturnFlowChannel.PopulateGeneralFlowChannel
                        (lDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                         lReturnFlowChannelNr,
                         lDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                         lDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                         lDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                         LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                         lDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                         Trim(lDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                         lDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('FirmYieldCalc').AsString),
                         Trim(LDataSet.DataSet.FieldByName('FlowOutput').AsString)))) then
                lResult := FALSE;
            end;
          end;

          if (lResult AND Assigned(lReturnFlowChannel)) then
          begin
            for lMonth := LMonthlyDiversionFlowsField.ArrayLow to LMonthlyDiversionFlowsField.ArrayHigh do
              lMonthlyDiversionFlows[lMonth] := NullFloat;
            for lMonth := LMonthlyReturnFlowsField.ArrayLow to LMonthlyReturnFlowsField.ArrayHigh do
              lMonthlyReturnFlows[lMonth]    := NullFloat;
            lResult := LoadMonthlyDiversionFlowData(lFeatureID, lMonthlyDiversionFlows) AND
                       LoadMonthlyReturnFlowData(lFeatureID, lMonthlyReturnFlows);
          end;
          if (lResult) then
          begin
            lIrrigationArea := AFeatureList.NewIrrigationArea;
            lIrrigationArea.Initialise;
            lResult := lIrrigationArea.Populate
                         (lFeatureID, lFeatureName,lIrrigationNodeNumber, lConsumptiveChannel.ChannelNumber,
                          lFeatureType,lFeatureSubType, lMonthlyDiversionFlows, lMonthlyReturnFlows,
                          lDiversionChannel.ChannelNumber, lReturnFlowChannel.ChannelNumber, lIrrigationPolicy);
            if (lResult) then
            begin
              lConsumptiveChannel.IrrigationArea := lIrrigationArea;
            end
            else
            begin
              AFeatureList.DeleteIrrigationAreaWithID(lFeatureID);
              if (LNewDiversion) then
                AChannelList.DeleteGeneralFlowChannelWithID(lDiversionChannel.ChannelID);
              if (lNewConsumptive) then
                AChannelList.DeleteGeneralFlowChannelWithID(lConsumptiveChannel.ChannelID);
              if (lNewReturnFlow) then
                AChannelList.DeleteGeneralFlowChannelWithID(lReturnFlowChannel.ChannelID);
            end;
          end;
          LAreaDataset.DataSet.Next;
        end;
        LAreaDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
      lAreaDataSet.Free;
    end;
    Result := LResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadMonthlyDiversionFlowData
                                     (AFeatureID                 : integer;
                                      var AMonthlyDiversionFlows : TIrrMonthlyDoublesArray): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadMonthlyDiversionFlowData';
var
  LDataSet : TAbstractModelDataset;
  LIndex   : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMonthlyDiversionFlowsSQL(AFeatureID));
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 12 do
            AMonthlyDiversionFlows[LIndex] := LDataset.DataSet.FieldByName(Format('DFlow%2.2d',[LIndex])).AsFloat;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadMonthlyReturnFlowData
                                     (AFeatureID              : integer;
                                      var AMonthlyReturnFlows : TIrrMonthlyDoublesArray): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadMonthlyReturnFlowData';
var
  LDataSet : TAbstractModelDataset;
  LIndex   : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMonthlyReturnFlowsSQL(AFeatureID));
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount > 0) then
        begin
          for LIndex := 1 to 12 do
            AMonthlyReturnFlows[LIndex] :=LDataset.DataSet.FieldByName(Format('RFlow%2.2d',[LIndex])).AsFloat;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Power Plants                                                               *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadPowerPlants
                                      (AChannelList : TChannelList;
                                       AFeatureList : TPowerPlantList): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadPowerPlants';
var
  lPlantDataSet                  : TAbstractModelDataset;
  lDataSet                       : TAbstractModelDataset;
  lPowerPlant                    : TPowerPlant;
  lPowerChannel                  : TGeneralFlowChannel;
  lSpillChannel                  : TGeneralFlowChannel;
  lNewPower                      : Boolean;
  lNewSpill                      : Boolean;
  lPowerChannelNr                : integer;
  lSpillChannelNr                : integer;
  lResult                        : Boolean;
  lFeatureID                     : integer;
  lFeatureName                   : string;
  lFeatureType                   : integer;
  lFeatureSubType                : integer;
  lPowerPlantStatus              : Boolean;
  lMaximumGeneratorCapacity      : double;
  lMaximumTurbineCapacity        : double;
  lHeadLoss                      : double;
  lDesignHead                    : double;
  lCombinedEfficiency            : double;
  lMaximumNetHead                : double;
  lMinimumNetHead                : double;
  lNetHeadEfficiencyCount        : integer;
  lTailwaterElevationsCount      : integer;
  lTailwaterType                 : integer;
  lIndex                         : integer;
  lEfficiencyFactors             : TEfficiencyFactorsArray;
  lNetHeadFactors                : TNetHeadFactorsArray;
  lDischarges                    : TDischargesArray;
  lTailwaterElevations           : TElevationsArray;
  lMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
  lMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray;
  LEfficiencyFactorsField,
  LNetHeadFactorsField,
  LDischargesField,
  LTailwaterElevationsField,
  LMinimumMonthlyPowerGenerationField,
  LMinimumMonthlyPowerReleasefield: TAbstractFieldProperty;
  lChannelNrs     : TStringList;
  lChannelNr      : integer;
  lNrOfPlants     : integer;
begin
  Result := FALSE;
  try
    LEfficiencyFactorsField := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
    if not Assigned(LEfficiencyFactorsField) then
      raise Exception.Create('Field (EfficiencyFactor) not found in field properties');
    SetLength(lEfficiencyFactors,LEfficiencyFactorsField.ArrayLength);

    LNetHeadFactorsField := FAppModules.FieldProperties.FieldProperty('NetHeadFactors');
    if not Assigned(LNetHeadFactorsField) then
      raise Exception.Create('Field (NetHeadFactors) not found in field properties');
    SetLength(lNetHeadFactors,LNetHeadFactorsField.ArrayLength);

    LDischargesField := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
    if not Assigned(LDischargesField) then
      raise Exception.Create('Field (DownStreamLevel) not found in field properties');
    SetLength(lDischarges,LDischargesField.ArrayLength);

    LTailwaterElevationsField := FAppModules.FieldProperties.FieldProperty('TailWaterElevation');
    if not Assigned(LTailwaterElevationsField) then
      raise Exception.Create('Field (TailWaterElevation) not found in field properties');
    SetLength(lTailwaterElevations,LTailwaterElevationsField.ArrayLength);

    LMinimumMonthlyPowerGenerationField := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
    if not Assigned(LMinimumMonthlyPowerGenerationField) then
      raise Exception.Create('Field (MinEnergyGenerated) not found in field properties');
    SetLength(lMinimumMonthlyPowerGeneration,LMinimumMonthlyPowerGenerationField.ArrayLength);

    LMinimumMonthlyPowerReleasefield := FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease');
    if not Assigned(LMinimumMonthlyPowerReleasefield) then
      raise Exception.Create('Field (MinPowerChannelRelease) not found in field properties');
    SetLength(lMinimumMonthlyPowerRelease,LMinimumMonthlyPowerReleasefield.ArrayLength);

    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LPlantDataSet);
    try
      if (Assigned(LPlantDataSet) AND Assigned(lDataSet)) then
      begin
        try
          lChannelNrs   := TStringList.Create;

          LPlantDataSet.SetSQL(FSQLAgent.GetPowerPlantDataSQL);
          LPlantDataset.DataSet.Open;
          while ((NOT LPlantDataset.DataSet.EOF) AND lResult) do
          begin
            lSpillChannel   := nil;
            LNewPower       := FALSE;
            LNewSpill       := FALSE;
            lFeatureType    := 9;
            lFeatureSubType := 0;

            lPowerChannelNr           := LPlantDataSet.DataSet.FieldByName('PowerChannelNumber').AsInteger;
            lSpillChannelNr           := LPlantDataSet.DataSet.FieldByName('SpillChannelNumber').AsInteger;
            lFeatureID                := LPlantDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger;
            lFeatureName              := Trim(LPlantDataSet.DataSet.FieldByName('PowerPlantName').AsString);
            lMaximumGeneratorCapacity := LPlantDataSet.DataSet.FieldByName('MaxCapGenerator').AsFloat;
            lMaximumTurbineCapacity   := LPlantDataSet.DataSet.FieldByName('MaxCapTurbine').AsFloat;
            lCombinedEfficiency       := LPlantDataSet.DataSet.FieldByName('Efficiency').AsFloat;
            lPowerPlantStatus         := LPlantDataSet.DataSet.FieldByName('PowerPlantStatus').AsInteger = 1;
            lHeadLoss                 := LPlantDataSet.DataSet.FieldByName('HeadLoss').AsFloat;
            lDesignHead               := LPlantDataSet.DataSet.FieldByName('DesignHead').AsFloat;
            lMaximumNetHead           := LPlantDataSet.DataSet.FieldByName('MaxNetHead').AsFloat;
            lMinimumNetHead           := LPlantDataSet.DataSet.FieldByName('MinNetHead').AsFloat;
            lTailWaterType            := LPlantDataSet.DataSet.FieldByName('TailWaterTypeCode').AsInteger;
            lNetHeadEfficiencyCount   := LPlantDataSet.DataSet.FieldByName('PointsCount').AsInteger;
            lTailwaterElevationsCount := LPlantDataSet.DataSet.FieldByName('TailWaterCount').AsInteger;
            lNrOfPlants               := LPlantDataSet.DataSet.FieldByName('DownStreamPowerChannelCount').AsInteger;

            lChannelNrs.Clear;
            lIndex := 1;
            while (lIndex <= lNrOfPlants) do
            begin
              lChannelNr := LPlantDataSet.DataSet.FieldByName(Format('Channel%2.2d',[lIndex])).AsInteger;
              LChannelNrs.Add(IntToStr(lChannelNr));
              lIndex := lIndex + 1;
            end;

            lPowerChannel   := AChannelList.CastChannelByChannelNumber[lPowerChannelNr];
            if (NOT Assigned(lPowerChannel)) then
            begin
              lPowerChannel := AChannelList.NewGeneralFlowChannel;
              lPowerChannel.Initialise;
              LNewPower     := TRUE;
              LDataSet.SetSQL(FSQLAgent.GetChannelByNumberSQL(lPowerChannelNr));
              LDataSet.DataSet.Open;
              if ((lDataSet.DataSet.RecordCount = 0) OR
                  (NOT lPowerChannel.PopulateGeneralFlowChannel
                        (LDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                         lPowerChannelNr,
                         LDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                         lDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                         LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                         LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                         LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                         lDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                         Trim(LDataSet.DataSet.FieldByName('FirmYieldCalc').AsString),
                         Trim(LDataSet.DataSet.FieldByName('FlowOutput').AsString)))) then
                lResult := FALSE;
              LDataSet.DataSet.Close;
            end;

            if (lResult AND Assigned(lPowerChannel)) then
            begin
              lSpillChannel   := AChannelList.CastChannelByChannelNumber[lSpillChannelNr];
              if (NOT Assigned(lSpillChannel)) then
              begin
                lSpillChannel := AChannelList.NewGeneralFlowChannel;
                lSpillChannel.Initialise;
                LNewSpill     := TRUE;
                LDataSet.SetSQL(FSQLAgent.GetChannelByNumberSQL(lSpillChannelNr));
                LDataSet.DataSet.Open;
                if ((lDataSet.DataSet.RecordCount = 0) OR
                    (NOT lSpillChannel.PopulateGeneralFlowChannel
                          (LDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                           lSpillChannelNr,
                           LDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                           lDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                           Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                           LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                           LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                           LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                           Trim(LDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                           lDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                           Trim(LDataSet.DataSet.FieldByName('FirmYieldCalc').AsString),
                           Trim(LDataSet.DataSet.FieldByName('FlowOutput').AsString)))) then
                  lResult := FALSE;
                LDataSet.DataSet.Close;
              end;
            end;

            if (lResult AND Assigned(lSpillChannel)) then
            begin
              for lIndex := LEfficiencyFactorsField.ArrayLow to LEfficiencyFactorsField.ArrayHigh do
                lEfficiencyFactors[lIndex] := NullFloat;
              for lIndex := LNetHeadFactorsField.ArrayLow to LNetHeadFactorsField.ArrayHigh do
                lNetHeadFactors[lIndex] := NullFloat;
              for lIndex := LDischargesField.ArrayLow to LDischargesField.ArrayHigh do
                lDischarges[lIndex] := NullFloat;
              for lIndex := LTailwaterElevationsField.ArrayLow to LTailwaterElevationsField.ArrayHigh do
                lTailwaterElevations[lIndex] := NullFloat;
              for lIndex := LMinimumMonthlyPowerGenerationField.ArrayLow to LMinimumMonthlyPowerGenerationField.ArrayHigh do
                lMinimumMonthlyPowerGeneration[lIndex] := NullFloat;
              for lIndex := LMinimumMonthlyPowerReleasefield.ArrayLow to LMinimumMonthlyPowerReleasefield.ArrayHigh do
               lMinimumMonthlyPowerRelease[lIndex]    := NullFloat;

              lResult := LoadPowerPlantDetails
                           (lFeatureID, lNetHeadEfficiencyCount, lTailwaterElevationsCount,
                            lEfficiencyFactors, lNetHeadFactors, lDischarges, lTailwaterElevations,
                            lTailwaterType) AND
                         LoadPowerDemand
                           (lPowerChannelNr,lMinimumMonthlyPowerGeneration,
                            lMinimumMonthlyPowerRelease);
            end;
            if (lResult) then
            begin
              lPowerPlant := AFeatureList.NewPowerPlant;
              lResult := lPowerPlant.Populate
                           (lFeatureID, lFeatureName, lPowerChannel.ChannelNumber, lFeatureType,
                            lFeatureSubType, lSpillChannel.ChannelNumber, lPowerPlantStatus,
                            lMaximumGeneratorCapacity, lMaximumTurbineCapacity,
                            lHeadLoss, lDesignHead, lCombinedEfficiency,
                            lMaximumNetHead, lMinimumNetHead, lNetHeadEfficiencyCount,
                            lTailwaterElevationsCount, lTailwaterType, lEfficiencyFactors,
                            lNetHeadFactors, lDischarges, lTailwaterElevations,
                            lMinimumMonthlyPowerGeneration, lMinimumMonthlyPowerRelease,
                            lChannelNrs);
              if (lResult) then
              begin
                lPowerChannel.PowerPlant := lPowerPlant;
              end
              else
              begin
                AFeatureList.DeletePowerPlantWithID(lFeatureID);
                if (lNewPower) then
                  AChannelList.DeleteGeneralFlowChannelWithID(lPowerChannel.ChannelID);
                if (lNewSpill) then
                  AChannelList.DeleteGeneralFlowChannelWithID(lSpillChannel.ChannelID);
              end;
            end;
            LPlantDataset.DataSet.Next;
          end;
        finally
          FreeAndNil(lChannelNrs);
        end;
        LPlantDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
      lPlantDataSet.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadPowerPlantDetails
                                   (AFeatureID                : integer;
                                    ANetHeadEfficiencyCount   : integer;
                                    ATailwaterElevationsCount : integer;
                                    var AEfficiencyFactors    : TEfficiencyFactorsArray;
                                    var ANetHeadFactors       : TNetHeadFactorsArray;
                                    var ADischarges           : TDischargesArray;
                                    var ATailwaterElevations  : TElevationsArray;
                                    var ATailwaterType        : integer): boolean; {F07}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadPowerPlantDetails';
var
  LDataSet : TAbstractModelDataset;
  lCode    : integer;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetPowerPlantDetailsSQL(AFeatureID));
        LDataset.DataSet.Open;
        while (NOT LDataset.DataSet.EOF) do
        begin
          lCode  := LDataset.DataSet.FieldByName('FactorCode').AsInteger;
          case lCode of
          1 :
            begin
              for lIndex := 1 to ANetHeadEfficiencyCount do
                AEfficiencyFactors[lIndex] := LDataset.DataSet.FieldByName(Format('Factor%2.2d',[lIndex])).AsFloat;
            end;
          2 :
            begin
              for lIndex := 1 to ANetHeadEfficiencyCount do
                ANetHeadFactors[lIndex] := LDataset.DataSet.FieldByName(Format('Factor%2.2d',[lIndex])).AsFloat;
            end;
          3 :
            begin
              for lIndex := 1 to ATailwaterElevationsCount do
                ADischarges[lIndex] := LDataset.DataSet.FieldByName(Format('Factor%2.2d',[lIndex])).AsFloat;
            end;
          4 :
            begin
              for lIndex := 1 to ATailwaterElevationsCount do
                ATailwaterElevations[lIndex] := LDataset.DataSet.FieldByName(Format('Factor%2.2d',[lIndex])).AsFloat;
            end;
          else
          end;
          lDataSet.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadPowerDemand
                                     (APowerChannelNr                    : integer;
                                      var AMinimumMonthlyPowerGeneration : TPowMonthlyDoublesArray;
                                      var AMinimumMonthlyPowerRelease    : TPowMonthlyDoublesArray): boolean; {F08}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadPowerDemand';
var
  LDataSet : TAbstractModelDataset;
  lCode    : integer;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetPowerDemandSQL(APowerChannelNr));
        LDataset.DataSet.Open;
        while (NOT LDataset.DataSet.EOF) do
        begin
          lCode  := LDataset.DataSet.FieldByName('PowerCode').AsInteger;
          case lCode of
          1 :
            begin
              for lIndex := 1 to 12 do
                AMinimumMonthlyPowerGeneration[lIndex]
                  := LDataset.DataSet.FieldByName(Format('MinPower%2.2d',[lIndex])).AsFloat;
            end;
          2 :
            begin
              for lIndex := 1 to 12 do
                AMinimumMonthlyPowerRelease[lIndex]
                  := LDataset.DataSet.FieldByName(Format('MinPower%2.2d',[lIndex])).AsFloat;
            end;
          else
          end;
          lDataSet.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Specified Inflow                                                           *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadSpecifiedInflowData
                                        (AChannelList : TChannelList;
                                         AFeatureList : TSpecifiedInflowFeatureList): boolean; {F03}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadSpecifiedInflowData';
var
  lDataSet            : TAbstractModelDataset;
  lSpecifiedInflow    : TSpecifiedInflowFeature;
  lChannel            : TGeneralFlowChannel;
  lChannelNr          : integer;
  lResult             : boolean;
  lNewChannel         : Boolean;
  lFeatureID          : integer;
  lFeatureName        : string;
  lFeatureType        : integer;
  lFeatureSubType     : integer;
  lInflowFileName     : string;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetSpecifiedInflowDataSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND lResult) do
        begin
          lFeatureType        := 10;
          lFeatureSubType     := 0;
          lNewChannel         := FALSE;
          lChannelNr   := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          lFeatureID   := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          lFeatureName := Trim(LDataSet.DataSet.FieldByName('FeatureName').AsString);
          lInflowFileName := Trim(LDataSet.DataSet.FieldByName('InflowFileName').AsString);
          lChannel     := AChannelList.CastChannelByChannelNumber[lChannelNr];
          if (NOT Assigned(lChannel)) then
          begin
            lChannel := AChannelList.NewGeneralFlowChannel;
            lChannel.Initialise;
            LNewChannel := TRUE;
            if (NOT lChannel.PopulateGeneralFlowChannel
                      (LDataSet.DataSet.FieldByName('RecordIdentifier').AsInteger,
                       lChannelNr,
                       LDataSet.DataSet.FieldByName('ChannelType').AsInteger,
                       lDataSet.DataSet.FieldByName('ChannelSubType').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString),
                       LDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger,
                       LDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger,
                       LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger,
                       Trim(LDataSet.DataSet.FieldByName('SummaryOutput').AsString),
                       lDataSet.DataSet.FieldByName('ChannelAreaID').AsInteger,
                       Trim(lDataSet.DataSet.FieldByName('FlowOutput').AsString),
                       Trim(lDataSet.DataSet.FieldByName('FirmYieldCalc').AsString))) then
              lResult := FALSE;
          end;

          if (lResult) then
          begin
            lSpecifiedInflow := AFeatureList.NewSpecifiedInflowFeature;
            lResult := lSpecifiedInflow.Populate
                         (lFeatureID, lFeatureName, lInflowFileName, lChannel.ChannelNumber, lFeatureType,
                          lFeatureSubType);
            if (lResult) then
            begin
              lChannel.SpecifiedInflowFeature := lSpecifiedInflow;
            end
            else
            begin
              if (lNewChannel) then
                AChannelList.DeleteGeneralFlowChannelWithID(lChannel.ChannelID);
              AFeatureList.DeleteSpecifiedInflowFeatureWithID(lFeatureID);
            end;
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := LResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Water Demand Configuration                                                 *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadWaterDemandConfiguration (AWaterDemandConfiguration : TWaterDemandConfiguration): boolean; {F16}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadWaterDemandConfiguration';
var
  lDataSet               : TAbstractModelDataset;
  lResult                : Boolean;
  lIndex                 : integer;
  lWaterDemandCategory   : TWaterDemandCategory;
  lCategoryID            : integer;
  lCategoryDescr         : string;
  lRecurrenceInterval    : TWaterDemandPortion;
  lRiskCriteriaCount     : TAbstractFieldProperty;
  lProportion            : TWaterUseOutputProportion;
  LChannelNumber         : integer;
  LWaterUsePortion       : TWaterDemandPortion;
  LImplementReconciliation : integer;
begin
   Result := FALSE;
  try
    lResult := TRUE;
    LImplementReconciliation := 0;
    lRiskCriteriaCount := FAppModules.FieldProperties.FieldProperty('RecurrenceInterval');
    if not Assigned(LRiskCriteriaCount) then
      raise Exception.Create('Field (RecurrenceInterval) not found in field properties');
    SetLength(lRecurrenceInterval, lRiskCriteriaCount.ArrayLength);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetWaterDemandCategoriesSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND lResult) do
        begin
          lCategoryID     := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          lCategoryDescr  := Trim(LDataset.DataSet.FieldByName('CategoryDescription').AsString);
          for lIndex := lRiskCriteriaCount.ArrayLow to lRiskCriteriaCount.ArrayHigh do
          begin
            if (Trim(LDataset.DataSet.FieldByName(Format('CriteriaPortion%2.2d',[lIndex])).AsString) <> '') then
              lRecurrenceInterval[lIndex] := LDataset.DataSet.FieldByName(Format('CriteriaPortion%2.2d',[lIndex])).AsFloat
            else
              lRecurrenceInterval[lIndex] := NullFloat;
          end;
          lWaterDemandCategory := AWaterDemandConfiguration.NewWaterDemandCategory;
          lWaterDemandCategory.Initialise;
          lResult := lWaterDemandCategory.Populate
                           (lCategoryID, lCategoryDescr, lRecurrenceInterval );
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetWaterDemandRiskCriteriaSQL);
        LDataset.DataSet.Open;
        for lIndex := lRiskCriteriaCount.ArrayLow to lRiskCriteriaCount.ArrayHigh do
          lRecurrenceInterval[lIndex] := NullFloat;
        if (NOT LDataset.DataSet.EOF) then
        begin
          for lIndex := lRiskCriteriaCount.ArrayLow to lRiskCriteriaCount.ArrayHigh do
          begin
            if (Trim(LDataset.DataSet.FieldByName(Format('Interval%2.2d',[lIndex])).AsString) <> '') then
              lRecurrenceInterval[lIndex] := LDataset.DataSet.FieldByName(Format('Interval%2.2d',[lIndex])).AsFloat
          end;
          AWaterDemandConfiguration.Populate(lRecurrenceInterval);
        end;
        // Load WaterUseOutputProportion
        LDataSet.ClearSQL;
        LDataSet.SetSQL(FSQLAgent.GetWaterUseProportionsSQL);
        LDataset.DataSet.Open;
        SetLength(LWaterUsePortion, lRiskCriteriaCount.ArrayLength);
        while ((NOT LDataset.DataSet.EOF) AND lResult) do
        begin
          LChannelNumber := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          for lIndex := lRiskCriteriaCount.ArrayLow to lRiskCriteriaCount.ArrayHigh do
          begin
            if (Trim(LDataset.DataSet.FieldByName(Format('Category%2.2d', [lIndex])).AsString) <> '') then
              LWaterUsePortion[lIndex] := LDataset.DataSet.FieldByName(Format('Category%2.2d', [lIndex])).AsFloat
            else
              LWaterUsePortion[lIndex] := NullFloat;
          end;
          lProportion := AWaterDemandConfiguration.NewWaterUseOutputProportion;
          lProportion.Initialise;
          LResult := lProportion.Populate(LChannelNumber, LWaterUsePortion);
          LDataset.DataSet.Next;
        end;

        if lResult then
        begin
          LDataset.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.GetWaterDemandFileCreateSQL);
          LDataset.DataSet.Open;
          if (lDataSet.DataSet.RecordCount > 0) then
          begin
            LImplementReconciliation := LDataset.DataSet.FieldByName('ImplementFile').AsInteger;
          end;
          lResult := AWaterDemandConfiguration.InitialiseFileCreate(LImplementReconciliation);
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;
    TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
      WaterDemandConfiguration.CreateYieldWaterUseOutputProportion;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Water Demand Features                                                      *}
{******************************************************************************}

function TNetworkFeaturesLoadAgent.LoadWaterDemandFeatures (AChannelList : TChannelList;
                                                            AFeatureList : TWaterDemandFeatureList): boolean; {F16}
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadWaterDemandFeatures';
var
  lDataSet               : TAbstractModelDataset;
  lChannelNr             : integer;
  lChannel               : TGeneralFlowChannel;
  lIndex                 : integer;
  lWaterDemandFeature    : TWaterDemandFeature;
  lResult                : Boolean;
  lFeatureID             : integer;
  lFeatureName           : string;
  lFeatureType           : integer;
  lFeatureSubType        : integer;
  lCategoryID            : integer;
  lScenarioPortions      : TWaterDemandPortion;
  LScenarioCount         : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;

    LScenarioCount := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    if not Assigned(LScenarioCount) then
      raise Exception.Create('Field (ScenarioPortion) not found in field properties');
    SetLength(lScenarioPortions, LScenarioCount.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetWaterDemandCountsSQL);
        LDataset.DataSet.Open;
        if (NOT LDataset.DataSet.EOF) then
          AFeatureList.PopulateScenarioCount(LDataset.DataSet.FieldByName('ScenarioCount').AsInteger)
        else
          AFeatureList.PopulateScenarioCount(0);

        LDataset.DataSet.Close;
        LDataSet.SetSQL(FSQLAgent.GetWaterDemandFeaturesSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND lResult) do
        begin
          lFeatureType    := 13;
          lFeatureSubType := 0;
          lFeatureID      := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          lCategoryID     := LDataset.DataSet.FieldByName('CategoryID').AsInteger;
          lChannelNr      := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          lFeatureName    := Trim(LDataset.DataSet.FieldByName('FeatureName').AsString);
          for lIndex := LScenarioCount.ArrayLow to LScenarioCount.ArrayHigh do
          begin
            if (Trim(LDataset.DataSet.FieldByName(Format('ScenarioPortion%2.2d',[lIndex])).AsString) <> '') then
              lScenarioPortions[lIndex]  := LDataset.DataSet.FieldByName(Format('ScenarioPortion%2.2d',[lIndex])).AsFloat
            else
              lScenarioPortions[lIndex]  := NullFloat;
          end;
          lChannel        := AChannelList.CastChannelByChannelNumber[lChannelNr];

          if (Assigned(lChannel)) then
          begin
            lWaterDemandFeature := AFeatureList.NewWaterDemandFeature;
            lWaterDemandFeature.Initialise;
            lResult     := lWaterDemandFeature.Populate
                             (lFeatureID, lFeatureName, lChannel.ChannelNumber, lFeatureType,
                              lFeatureSubType, lCategoryID,lScenarioPortions);
            if (lResult) then
              lChannel.WaterDemandFeature := lWaterDemandFeature
            else
              AFeatureList.DeleteWaterDemandFeatureWithID(lFeatureID);
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadChannelArea(AChannelArea: TChannelAreaList): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadChannelArea';
var
  lDataSet               : TAbstractModelDataset;
  lResult                : Boolean;
  lChannelArea           : TChannelArea;
  lChannelAreaID         : integer;
  lChannelAreaName       : string;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetChannelAreaSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND lResult) do
        begin
          lChannelAreaID   := LDataset.DataSet.FieldByName('AreaID').AsInteger;
          lChannelAreaName := Trim(LDataset.DataSet.FieldByName('AreaName').AsString);
          lChannelArea := AChannelArea.NewChannelArea;
          lChannelArea.Initialise;
          lResult := lChannelArea.Populate(lChannelAreaID, lChannelAreaName);
          LDataset.DataSet.Next;
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{
function TNetworkFeaturesLoadAgent.LoadIrrigationBlockList(AChannelList: TChannelList; AFeature: TIrrigationBlockList): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadIrrigationBlockList';
var
  LSQLAgent : TIrrigationBlockSQLAgent;
  lDataSet,
  lOtherDataSet          : TAbstractModelDataset;
  lChannel               : IGeneralFlowChannel;
  lDiversionChannelNr    : integer;
  lReturnFlowChannelNr   : integer;
  lResult                : boolean;
  //lNewDiversion          : boolean;
  //lNewReturnFlow         : boolean;
  //lNewConsumptive        : boolean;

  lUpperZoneReturnFlow           : Double;
  lLowerZoneReturnFlow           : Double;
  lReturnFlowLoss                : Double;
  lUpperZoneSoilMoistureCapacity : Double;
  lLowerZoneSoilMoistureCapacity : Double;
  lUpperZoneSoilMoistureTarget   : Double;
  lInitialSoilMoistureStorage    : Double;

  lBlockNumber                   : Integer;
  lBlockName                     : WideString;
  lDescription                   : WideString;
  lMaxWaterAllocation            : Double;
  lFileName                      : WideString;
  lNodeNumber                    : Integer;
  lCanalTransportLoss            : Double;
  lEfficiencyFactor              : Double;
  lReturnFlowFactor              : Double;
  lNumberOfCropTypes             : Integer;
  lRainAboveRainFactorSpecValue  : Double;
  lRainBelowRainFactor           : Double;
  lRainCatchmentScalingFactor    : Double;
  lAllocatedIrrigationArea       : Double;
  lBlockIdentifier               : Integer;
  lCropWaterUseType              : Integer;
  lDroughtApplicable             : Integer;
  LCount,
  LIndex                         : Integer;
  LFieldName                     : string;
  lRainfallFactor,
  lPanEvaporation,
  lAPanConvFactor                : TMonthlyDoubleArray;
  lMonthlyWaterUse               : TMonthlyDoubleArray;
  //lWaterUsageFactorList          : TObjectList;
  LIrrigationBlock               : TIrrigationBlock;
  LWaterUsage                    : TWaterUsage;

  LWaterUsageBlockIdentifier       : integer;
  LWaterUsageIdentifier            : integer;
  LWaterUsagePercAreaUnderCropType : double;
  LWaterUsageCropName              : string;

  //tmpStr                           : WideString;
  LDiversionChannelMaxDemand       : TDiversionChannelMaxDemand;
  LChannelNumber                   : integer;
  LMaxDemands                      : TMonthlyDoubleArray;
  LInUse                           : boolean;

  //_______________________________Irrigation block type______________________________________________________________________
  LIrrigationBlockType : integer;
  LCurtailIrrigationAbstraction : integer;
  LCanalSeepageLoss : double;
  LCanalTransmissionLoss : double;
  LUpperSoilOutflow : double;
  LMultiplicationFactor : double;
  LMaxUpperZoneMoisture : double;
  LMinUpperZoneMoisture : double;
  LCropTypesCount : integer;
  LIrrigationSupplyCapacity : double;
  LAllocatedAreaPointsCount : integer;
  LMethodIrrigatedAreas : integer;
  LMaxWaterAllocationCount : integer;
  LMethodMaxWaterAllocation : integer;
  LReturnFlowVolumePointsCount : integer;
  LMethodReturnFlowVolume : integer;
  LSupplyCapacityPointsCount : integer;
  LMethodSupplyCapacity : integer;
  LIrrigationEfficienciesPointsCount : integer;
  LMethodIrrigationEfficiencies : integer;
  LReturnFlowFactorsCount : integer;
  LMethodReturnFlowFactors : integer;

  LIrrigatedAreasBreakPointYear : string;
  LIrrigatedArea : string;
  LMaximumWaterAllocationBreakPointYear : string;
  LMaximumWaterAllocation : string;
  LMaximumWaterAllocationGrowth : string;
  LReturnFlowVolumeBreakpointYear : string;
  LReturnFlowVolume : string;
  LSupplyCapacityBreakpointYear : string;
  LSupplyCapacity : string;
  LIrrigationEfficiencyBreakpointYear : string;
  LIrrigationEfficiency : string;
  LReturnFlowFactorBreakpointYear : string;
  LReturnFlowFactors : string;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LSQLAgent  := TIrrigationBlockSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lOtherDataSet);
    try
      LMultiplicationFactor := 0;
      if Assigned(lDataSet) then
      begin
        lDataSet.SetSQL(LSQLAgent.GetIrrigationBlockSQL);
        lDataSet.DataSet.Open;
        while ((not lDataSet.DataSet.EOF) AND LResult) do
        begin
          LIrrigationBlock              := AFeature.NewIrrigationBlock;

          lUpperZoneReturnFlow           := lDataSet.DataSet.FieldByName('UpperZoneReturnFlow').AsFloat;
          lLowerZoneReturnFlow           := lDataSet.DataSet.FieldByName('LowerZoneReturnFlow').AsFloat;
          lReturnFlowLoss                := lDataSet.DataSet.FieldByName('ReturnFlowLoss').AsFloat;
          lUpperZoneSoilMoistureCapacity := lDataSet.DataSet.FieldByName('UpperZoneSoilMoistureCapacity').AsFloat;
          lLowerZoneSoilMoistureCapacity := lDataSet.DataSet.FieldByName('LowerZoneSoilMoistureCapacity').AsFloat;
          lUpperZoneSoilMoistureTarget   := lDataSet.DataSet.FieldByName('UpperZoneSoilMoistureTarget').AsFloat;
          lInitialSoilMoistureStorage    := lDataSet.DataSet.FieldByName('InitialSoilMoistureStorage').AsFloat;

          lBlockNumber                   := lDataSet.DataSet.FieldByName('BlockNumber').AsInteger;
          lBlockName                     := Trim(lDataSet.DataSet.FieldByName('BlockName').AsString);
          lDescription                   := Trim(lDataSet.DataSet.FieldByName('Description').AsString);
          lMaxWaterAllocation            := lDataSet.DataSet.FieldByName('MaxWaterAllocation').AsFloat;
          lFileName                      := Trim(lDataSet.DataSet.FieldByName('FileName').AsString);
          lNodeNumber                    := lDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
          lCanalTransportLoss            := lDataSet.DataSet.FieldByName('CanalTransportLoss').AsFloat;
          lEfficiencyFactor              := lDataSet.DataSet.FieldByName('EfficiencyFactor').AsFloat;
          lReturnFlowFactor              := lDataSet.DataSet.FieldByName('ReturnFlowFactor').AsFloat;
          lRainAboveRainFactorSpecValue  := lDataSet.DataSet.FieldByName('RainAboveRainFactorSpecValue').AsFloat;
          lRainBelowRainFactor           := lDataSet.DataSet.FieldByName('RainBelowRainFactor').AsFloat;
          lRainCatchmentScalingFactor    := lDataSet.DataSet.FieldByName('RainCatchmentScalingFactor').AsFloat;
          lAllocatedIrrigationArea       := lDataSet.DataSet.FieldByName('AllocatedIrrigationArea').AsFloat;
          lBlockIdentifier               := lDataSet.DataSet.FieldByName('Identifier').AsInteger;
          lCropWaterUseType              := lDataSet.DataSet.FieldByName('CropWaterUseType').AsInteger;
          lDroughtApplicable             := lDataSet.DataSet.FieldByName('DroughtApplicable').AsInteger;

          //MVM: APan Convertion
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockAPanConvFactorSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName                            := 'Factor0'+IntToStr(LCount)
                else
                  LFieldName                            := 'Factor'+IntToStr(LCount);
                lAPanConvFactor[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;

          //Pan Evaporation
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockPanEvaporationSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName                            := 'Evaporation0'+IntToStr(LCount)
                else
                  LFieldName                            := 'Evaporation'+IntToStr(LCount);
                lPanEvaporation[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;

          //Rainfall Factor
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockRainfallFactorSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName                            := 'Factor0'+IntToStr(LCount)
                else
                  LFieldName                            := 'Factor'+IntToStr(LCount);
                lRainfallFactor[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;

          //Water use
          lNumberOfCropTypes := 0;
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockWaterUsageFactorSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              LWaterUsage := LIrrigationBlock.CastIrrigationBlockWaterUsageByIndex(lNumberOfCropTypes);
              if not Assigned(LWaterUsage) then
                LWaterUsage := LIrrigationBlock.NewWaterUse;
              if Assigned(LWaterUsage) then
              begin
                LWaterUsageBlockIdentifier       := lOtherDataSet.DataSet.FieldByName('BlockIdentifier').AsInteger;
                LWaterUsageIdentifier            := lOtherDataSet.DataSet.FieldByName('Identifier').AsInteger;
                LWaterUsagePercAreaUnderCropType := lOtherDataSet.DataSet.FieldByName('PercAreaUnderCropType').AsFloat;
                LWaterUsageCropName              := Trim(lOtherDataSet.DataSet.FieldByName('CropName').AsString);
                for LCount := 1 to 12 do
                begin
                  if LCount < 10 then
                    LFieldName  := 'Factor0'+IntToStr(LCount)
                  else
                    LFieldName  := 'Factor'+IntToStr(LCount);
                  lMonthlyWaterUse[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                end;
                LWaterUsage.Populate(LWaterUsageIdentifier,LWaterUsageBlockIdentifier,LWaterUsageCropName,LWaterUsagePercAreaUnderCropType,lMonthlyWaterUse);
              end;
              Inc(lNumberOfCropTypes);
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;


          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetDiversionChannelsMaxDemandSQL(lBlockNumber));
            lOtherDataSet.DataSet.Open;
            if not lOtherDataSet.DataSet.EOF then
            begin
              LDiversionChannelMaxDemand       := LIrrigationBlock.DiversionChannelMaxDemand;
              LChannelNumber                   := lOtherDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              LInUse                           := lOtherDataSet.DataSet.FieldByName('InUse').AsInteger = 1;
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName  := 'Value0'+IntToStr(LCount)
                else
                  LFieldName  := 'Value'+IntToStr(LCount);
                LMaxDemands[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              LDiversionChannelMaxDemand.PopulateMaxDemandValues(lBlockNumber,LChannelNumber,LMaxDemands,LInUse);
            end;
            lOtherDataSet.DataSet.Close;
          end;

          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockDetailSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            if not lOtherDataSet.DataSet.EOF then
            begin
              LIrrigationBlockType                := lOtherDataSet.DataSet.FieldByName('IrrigationBlockType').AsInteger;
              LCurtailIrrigationAbstraction  := lOtherDataSet.DataSet.FieldByName('CurtailIrrigationAbstraction').AsInteger;
              LCanalSeepageLoss              := lOtherDataSet.DataSet.FieldByName('CanalSeepageLoss').AsFloat;
              LCanalTransmissionLoss         := lOtherDataSet.DataSet.FieldByName('CanalTransmissionLoss').AsFloat;
              LUpperSoilOutflow              := lOtherDataSet.DataSet.FieldByName('UpperSoilOutflow').AsFloat;
              LMultiplicationFactor          := lOtherDataSet.DataSet.FieldByName('MultiplicationFactor').AsFloat;
              LMaxUpperZoneMoisture          := lOtherDataSet.DataSet.FieldByName('MaxUpperZoneMoisture').AsFloat;
              LMinUpperZoneMoisture          := lOtherDataSet.DataSet.FieldByName('MinUpperZoneMoisture').AsFloat;
              LCropTypesCount                := lOtherDataSet.DataSet.FieldByName('CropTypesCount').AsInteger;
              LIrrigationSupplyCapacity      := lOtherDataSet.DataSet.FieldByName('IrrigationSupplyCapacity').AsFloat;
              LAllocatedAreaPointsCount      := lOtherDataSet.DataSet.FieldByName('AllocatedAreaPointsCount').AsInteger;
              LMethodIrrigatedAreas          := lOtherDataSet.DataSet.FieldByName('MethodIrrigatedAreas').AsInteger;
              LMaxWaterAllocationCount       := lOtherDataSet.DataSet.FieldByName('MaxWaterAllocationCount').AsInteger;
              LMethodMaxWaterAllocation      := lOtherDataSet.DataSet.FieldByName('MethodMaxWaterAllocation').AsInteger;
              LReturnFlowVolumePointsCount   := lOtherDataSet.DataSet.FieldByName('ReturnFlowVolumePointsCount').AsInteger;
              LMethodReturnFlowVolume        := lOtherDataSet.DataSet.FieldByName('MethodReturnFlowVolume').AsInteger;
              LSupplyCapacityPointsCount     := lOtherDataSet.DataSet.FieldByName('SupplyCapacityPointsCount').AsInteger;
              LMethodSupplyCapacity          := lOtherDataSet.DataSet.FieldByName('MethodSupplyCapacity').AsInteger;
              LIrrigationEfficienciesPointsCount  := lOtherDataSet.DataSet.FieldByName('IrrigationEfficienciesPointsCount').AsInteger;
              LMethodIrrigationEfficiencies  := lOtherDataSet.DataSet.FieldByName('MethodIrrigationEfficiencies').AsInteger;
              LReturnFlowFactorsCount        := lOtherDataSet.DataSet.FieldByName('ReturnFlowFactorsCount').AsInteger;
              LMethodReturnFlowFactors       := lOtherDataSet.DataSet.FieldByName('MethodReturnFlowFactors').AsInteger;

              LIrrigatedAreasBreakPointYear         := lOtherDataSet.DataSet.FieldByName('IABreakpointYear').AsString;
              LIrrigatedArea                        := lOtherDataSet.DataSet.FieldByName('IABreakpointArea').AsString;
              LMaximumWaterAllocationBreakPointYear := lOtherDataSet.DataSet.FieldByName('MWABreakpointYear').AsString;
              LMaximumWaterAllocation               := lOtherDataSet.DataSet.FieldByName('MWAWaterAllocation').AsString;
              LMaximumWaterAllocationGrowth         := lOtherDataSet.DataSet.FieldByName('MWAWaterAllocationGrowth').AsString;
              LReturnFlowVolumeBreakpointYear       := lOtherDataSet.DataSet.FieldByName('RFVBreakpointYear').AsString;
              LReturnFlowVolume                     := lOtherDataSet.DataSet.FieldByName('RFVFlowVolume').AsString;
              LSupplyCapacityBreakpointYear         := lOtherDataSet.DataSet.FieldByName('SCBreakpointYear').AsString;
              LSupplyCapacity                       := lOtherDataSet.DataSet.FieldByName('SCSupplyCapacity').AsString;
              LIrrigationEfficiencyBreakpointYear   := lOtherDataSet.DataSet.FieldByName('IEBreakpointYear').AsString;
              LIrrigationEfficiency                 := lOtherDataSet.DataSet.FieldByName('IEIrrigationEfficiency').AsString;
              LReturnFlowFactorBreakpointYear       := lOtherDataSet.DataSet.FieldByName('RFFBreakpointYear').AsString;
              LReturnFlowFactors                    := lOtherDataSet.DataSet.FieldByName('RFFReturnFlowFactor').AsString;

              lResult := LIrrigationBlock.PopulateType4Details(
                                          LIrrigationBlockType,
                                          LCurtailIrrigationAbstraction,
                                          LCanalSeepageLoss,
                                          LCanalTransmissionLoss,
                                          LUpperSoilOutflow,
                                          LMultiplicationFactor,
                                          LMaxUpperZoneMoisture,
                                          LMinUpperZoneMoisture,
                                          LCropTypesCount,
                                          LIrrigationSupplyCapacity,
                                          LAllocatedAreaPointsCount,
                                          LMethodIrrigatedAreas,
                                          LMaxWaterAllocationCount,
                                          LMethodMaxWaterAllocation,
                                          LReturnFlowVolumePointsCount,
                                          LMethodReturnFlowVolume,
                                          LSupplyCapacityPointsCount,
                                          LMethodSupplyCapacity,
                                          LIrrigationEfficienciesPointsCount,
                                          LMethodIrrigationEfficiencies,
                                          LReturnFlowFactorsCount,
                                          LMethodReturnFlowFactors,

                                          LIrrigatedAreasBreakPointYear,
                                          LIrrigatedArea,
                                          LMaximumWaterAllocationBreakPointYear,
                                          LMaximumWaterAllocation,
                                          LMaximumWaterAllocationGrowth,
                                          LReturnFlowVolumeBreakpointYear,
                                          LReturnFlowVolume,
                                          LSupplyCapacityBreakpointYear,
                                          LSupplyCapacity,
                                          LIrrigationEfficiencyBreakpointYear,
                                          LIrrigationEfficiency,
                                          LReturnFlowFactorBreakpointYear,
                                          LReturnFlowFactors);
            end;
            lOtherDataSet.DataSet.Close;
          end;


          if (lResult) then
          begin
            lDiversionChannelNr    := 0;
            lReturnFlowChannelNr   := 0;

            for lIndex := 0 to AChannelList.ChannelCount-1 do
            begin
              lChannel := AChannelList.ChannelByIndex[LIndex];
              if (lChannel.ChannelType = 14) and (lChannel.DownStreamNodeNumber = lBlockNumber)then
                lDiversionChannelNr := lChannel.ChannelNumber;
              if (lChannel.ChannelType = 15) and (lChannel.UpStreamNodeNumber = lBlockNumber)then
                lReturnFlowChannelNr := lChannel.ChannelNumber;
              if(lDiversionChannelNr  > 0) and (lReturnFlowChannelNr > 0) then
                Break;
            end;

            lResult := LIrrigationBlock.Populate( lUpperZoneReturnFlow,
                                                  lLowerZoneReturnFlow,
                                                  lReturnFlowLoss,
                                                  LMultiplicationFactor,
                                                  lUpperZoneSoilMoistureCapacity,
                                                  lLowerZoneSoilMoistureCapacity,
                                                  lUpperZoneSoilMoistureTarget,
                                                  lInitialSoilMoistureStorage,

                                                  lRainfallFactor,
                                                  lPanEvaporation,
                                                  lAPanConvFactor,

                                                  lBlockNumber,
                                                  lBlockName,
                                                  lDescription,
                                                  lMaxWaterAllocation,
                                                  lFileName,
                                                  lNodeNumber,

                                                  lCanalTransportLoss,
                                                  lEfficiencyFactor,
                                                  lReturnFlowFactor,
                                                  lNumberOfCropTypes,
                                                  lRainAboveRainFactorSpecValue,
                                                  lRainBelowRainFactor,
                                                  lRainCatchmentScalingFactor,
                                                  lAllocatedIrrigationArea,
                                                  lBlockIdentifier,
                                                  lDiversionChannelNr ,
                                                  lReturnFlowChannelNr,
                                                  lDroughtApplicable,
                                                  lCropWaterUseType
                                                  );
          end;
          lDataSet.DataSet.Next;
        end;
        lDataSet.DataSet.Close;
      end;
    finally
      FreeAndnil(LSQLAgent);
      lDataSet.Free;
      lOtherDataSet.Free;
    end;
    Result := LResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

}

function TNetworkFeaturesLoadAgent.LoadIrrigationBlockList(AChannelList: TChannelList; AFeature: TIrrigationBlockList): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadIrrigationBlockList';
var
  LSQLAgent : TIrrigationBlockSQLAgent;
  lDataSet,
  lOtherDataSet : TAbstractModelDataset;
  lChannel               : IGeneralFlowChannel;
  lDiversionChannelNr    : integer;
  lReturnFlowChannelNr   : integer;
  lResult                : boolean;
  //lNewDiversion          : boolean;
  //lNewReturnFlow         : boolean;
  //lNewConsumptive        : boolean;

  lUpperZoneReturnFlow           : Double;
  lLowerZoneReturnFlow           : Double;
  lReturnFlowLoss                : Double;
  lUpperZoneSoilMoistureCapacity : Double;
  lLowerZoneSoilMoistureCapacity : Double;
  lUpperZoneSoilMoistureTarget   : Double;
  lInitialSoilMoistureStorage    : Double;

  lBlockNumber                   : Integer;
  lBlockName                     : WideString;
  lDescription                   : WideString;
  lMaxWaterAllocation            : Double;
  lFileName                      : WideString;
  lNodeNumber                    : Integer;
  lCanalTransportLoss            : Double;
  lEfficiencyFactor              : Double;
  lReturnFlowFactor              : Double;
  lNumberOfCropTypes             : Integer;
  lRainAboveRainFactorSpecValue  : Double;
  lRainBelowRainFactor           : Double;
  lRainCatchmentScalingFactor    : Double;
  lAllocatedIrrigationArea       : Double;
  lBlockIdentifier               : Integer;
  lCropWaterUseType              : Integer;
  lDroughtApplicable             : Integer;
  LCount,
  LIndex                         : Integer;
  LFieldName                     : string;
  lRainfallFactor,
  lPanEvaporation,
  lAPanConvFactor                : TMonthlyDoubleArray;
  lMonthlyWaterUse               : TMonthlyDoubleArray;
  //lWaterUsageFactorList          : TObjectList;
  LIrrigationBlock               : TIrrigationBlock;
  LWaterUsage                    : TWaterUsage;

  LWaterUsageBlockIdentifier       : integer;
  LWaterUsageIdentifier            : integer;
  LWaterUsagePercAreaUnderCropType : double;
  LWaterUsageCropName              : string;

  //tmpStr                           : WideString;
  LDiversionChannelMaxDemand       : TDiversionChannelMaxDemand;
  LChannelNumber                   : integer;
  LMaxDemands                      : TMonthlyDoubleArray;
  LInUse                           : boolean;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    LSQLAgent  := TIrrigationBlockSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lOtherDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.SetSQL(LSQLAgent.GetIrrigationBlockSQL);
        lDataSet.DataSet.Open;
        while ((not lDataSet.DataSet.EOF) AND LResult) do
        begin
          LIrrigationBlock      := AFeature.NewIrrigationBlock;

          lUpperZoneReturnFlow           := lDataSet.DataSet.FieldByName('UpperZoneReturnFlow').AsFloat;
          lLowerZoneReturnFlow           := lDataSet.DataSet.FieldByName('LowerZoneReturnFlow').AsFloat;
          lReturnFlowLoss                := lDataSet.DataSet.FieldByName('ReturnFlowLoss').AsFloat;
          lUpperZoneSoilMoistureCapacity := lDataSet.DataSet.FieldByName('UpperZoneSoilMoistureCapacity').AsFloat;
          lLowerZoneSoilMoistureCapacity := lDataSet.DataSet.FieldByName('LowerZoneSoilMoistureCapacity').AsFloat;
          lUpperZoneSoilMoistureTarget   := lDataSet.DataSet.FieldByName('UpperZoneSoilMoistureTarget').AsFloat;
          lInitialSoilMoistureStorage    := lDataSet.DataSet.FieldByName('InitialSoilMoistureStorage').AsFloat;

          lBlockNumber                   := lDataSet.DataSet.FieldByName('BlockNumber').AsInteger;
          lBlockName                     := Trim(lDataSet.DataSet.FieldByName('BlockName').AsString);
          lDescription                   := Trim(lDataSet.DataSet.FieldByName('Description').AsString);
          lMaxWaterAllocation            := lDataSet.DataSet.FieldByName('MaxWaterAllocation').AsFloat;
          lFileName                      := Trim(lDataSet.DataSet.FieldByName('FileName').AsString);
          lNodeNumber                    := lDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
          lCanalTransportLoss            := lDataSet.DataSet.FieldByName('CanalTransportLoss').AsFloat;
          lEfficiencyFactor              := lDataSet.DataSet.FieldByName('EfficiencyFactor').AsFloat;
          lReturnFlowFactor              := lDataSet.DataSet.FieldByName('ReturnFlowFactor').AsFloat;
          lRainAboveRainFactorSpecValue  := lDataSet.DataSet.FieldByName('RainAboveRainFactorSpecValue').AsFloat;
          lRainBelowRainFactor           := lDataSet.DataSet.FieldByName('RainBelowRainFactor').AsFloat;
          lRainCatchmentScalingFactor    := lDataSet.DataSet.FieldByName('RainCatchmentScalingFactor').AsFloat;
          lAllocatedIrrigationArea       := lDataSet.DataSet.FieldByName('AllocatedIrrigationArea').AsFloat;
          lBlockIdentifier               := lDataSet.DataSet.FieldByName('Identifier').AsInteger;
          lCropWaterUseType              := lDataSet.DataSet.FieldByName('CropWaterUseType').AsInteger;
          lDroughtApplicable             := lDataSet.DataSet.FieldByName('DroughtApplicable').AsInteger;

          //MVM: APan Convertion
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockAPanConvFactorSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName                            := 'Factor0'+IntToStr(LCount)
                else
                  LFieldName                            := 'Factor'+IntToStr(LCount);
                lAPanConvFactor[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;

          //Pan Evaporation
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockPanEvaporationSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName                            := 'Evaporation0'+IntToStr(LCount)
                else
                  LFieldName                            := 'Evaporation'+IntToStr(LCount);
                lPanEvaporation[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;

          //Rainfall Factor
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockRainfallFactorSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName                            := 'Factor0'+IntToStr(LCount)
                else
                  LFieldName                            := 'Factor'+IntToStr(LCount);
                lRainfallFactor[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;

          //Water use
          lNumberOfCropTypes := 0;
          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetIrrigationBlockWaterUsageFactorSQL(lBlockIdentifier));
            lOtherDataSet.DataSet.Open;
            while not lOtherDataSet.DataSet.EOF do
            begin
              LWaterUsage := LIrrigationBlock.CastIrrigationBlockWaterUsageByIndex(lNumberOfCropTypes);
              if not Assigned(LWaterUsage) then
                LWaterUsage := LIrrigationBlock.NewWaterUse;
              if Assigned(LWaterUsage) then
              begin
                LWaterUsageBlockIdentifier       := lOtherDataSet.DataSet.FieldByName('BlockIdentifier').AsInteger;
                LWaterUsageIdentifier            := lOtherDataSet.DataSet.FieldByName('Identifier').AsInteger;
                LWaterUsagePercAreaUnderCropType := lOtherDataSet.DataSet.FieldByName('PercAreaUnderCropType').AsFloat;
                LWaterUsageCropName              := Trim(lOtherDataSet.DataSet.FieldByName('CropName').AsString);
                for LCount := 1 to 12 do
                begin
                  if LCount < 10 then
                    LFieldName  := 'Factor0'+IntToStr(LCount)
                  else
                    LFieldName  := 'Factor'+IntToStr(LCount);
                  lMonthlyWaterUse[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                end;
                LWaterUsage.Populate(LWaterUsageIdentifier,LWaterUsageBlockIdentifier,LWaterUsageCropName,LWaterUsagePercAreaUnderCropType,lMonthlyWaterUse);
              end;
              Inc(lNumberOfCropTypes);
              lOtherDataSet.DataSet.Next;
            end;
            lOtherDataSet.DataSet.Close;
          end;


          if Assigned(lOtherDataSet) then
          begin
            lOtherDataSet.SetSQL(LSQLAgent.GetDiversionChannelsMaxDemandSQL(lBlockNumber));
            lOtherDataSet.DataSet.Open;
            if not lOtherDataSet.DataSet.EOF then
            begin
              LDiversionChannelMaxDemand       := LIrrigationBlock.DiversionChannelMaxDemand;
              LChannelNumber                   := lOtherDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
              LInUse                           := lOtherDataSet.DataSet.FieldByName('InUse').AsInteger = 1;
              for LCount := 1 to 12 do
              begin
                if LCount < 10 then
                  LFieldName  := 'Value0'+IntToStr(LCount)
                else
                  LFieldName  := 'Value'+IntToStr(LCount);
                LMaxDemands[LCount]  := lOtherDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              end;
              LDiversionChannelMaxDemand.PopulateMaxDemandValues(lBlockNumber,LChannelNumber,LMaxDemands,LInUse);
            end;
          end;



          if (lResult) then
          begin
            lDiversionChannelNr    := 0;
            lReturnFlowChannelNr   := 0;

            for lIndex := 0 to AChannelList.ChannelCount-1 do
            begin
              lChannel := AChannelList.ChannelByIndex[LIndex];
              if (lChannel.ChannelType = 14) and (lChannel.DownStreamNodeNumber = lBlockNumber)then
                lDiversionChannelNr := lChannel.ChannelNumber;
              if (lChannel.ChannelType = 15) and (lChannel.UpStreamNodeNumber = lBlockNumber)then
                lReturnFlowChannelNr := lChannel.ChannelNumber;
              if(lDiversionChannelNr  > 0) and (lReturnFlowChannelNr > 0) then
                Break;
            end;

            lResult := LIrrigationBlock.Populate( lUpperZoneReturnFlow,
                                                  lLowerZoneReturnFlow,
                                                  lReturnFlowLoss,
                                                  //LMultiplicationFactor,
                                                  0,
                                                  lUpperZoneSoilMoistureCapacity,
                                                  lLowerZoneSoilMoistureCapacity,
                                                  lUpperZoneSoilMoistureTarget,
                                                  lInitialSoilMoistureStorage,

                                                  lRainfallFactor,
                                                  lPanEvaporation,
                                                  lAPanConvFactor,

                                                  lBlockNumber,
                                                  lBlockName,
                                                  lDescription,
                                                  lMaxWaterAllocation,
                                                  lFileName,
                                                  lNodeNumber,

                                                  lCanalTransportLoss,
                                                  lEfficiencyFactor,
                                                  lReturnFlowFactor,
                                                  lNumberOfCropTypes,
                                                  lRainAboveRainFactorSpecValue,
                                                  lRainBelowRainFactor,
                                                  lRainCatchmentScalingFactor,
                                                  lAllocatedIrrigationArea,
                                                  lBlockIdentifier,
                                                  lDiversionChannelNr ,
                                                  lReturnFlowChannelNr,
                                                  lDroughtApplicable,
                                                  lCropWaterUseType
                                                  );
          end;
          lDataSet.DataSet.Next;
        end;
        lDataSet.DataSet.Close;
      end;
    finally
      FreeAndnil(LSQLAgent);
      lDataSet.Free;
      lOtherDataSet.Free;
    end;
    Result := LResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TNetworkFeaturesLoadAgent.LoadWetlands(AChannelList: TChannelList; AFeature: TWetlandList): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadWetlands';
var
  LWetlandSQLAgent : TWetlandSQLAgent;
  lDataSet          : TAbstractModelDataset;
  lChannel          : IGeneralFlowChannel;
  lInflowChannelNr,
  lOutflowChannelNr : Integer;

  LStorageVolume,
  LInflowProportion,
  LOutflowProportion,
  LUpstreamThreshold  : Double;

  LIndex,
  LIdentifier,
  LNodeNumber : Integer;
  LName       : WideString;
  LWetland    : TWetland;
begin
  Result := False;
  try
    LWetlandSQLAgent          := TWetlandSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.SetSQL(LWetlandSQLAgent.GetWetlandSQL);
        lDataSet.DataSet.Open;
        while (not lDataSet.DataSet.EOF) do
        begin
          LWetland    := AFeature.NewWetland;

          LIdentifier         := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNodeNumber         := LDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
          LName               := Trim(LDataSet.DataSet.FieldByName('WetlandName').AsString);
          LStorageVolume      := LDataSet.DataSet.FieldByName('Storage').AsFloat;
          LInflowProportion   := LDataSet.DataSet.FieldByName('InflowProportion').AsFloat;
          LOutflowProportion  := LDataSet.DataSet.FieldByName('OutflowProportion').AsFloat;
          LUpstreamThreshold  := LDataSet.DataSet.FieldByName('UpstreamThreshold').AsFloat;

          lInflowChannelNr  := 0;
          lOutflowChannelNr := 0;

          for lIndex := 0 to AChannelList.ChannelCount-1 do
          begin
            lChannel := AChannelList.ChannelByIndex[LIndex];
            if (lChannel.ChannelType = 16) and (lChannel.DownStreamNodeNumber = lNodeNumber)then
              lInflowChannelNr := lChannel.ChannelNumber;
            if (lChannel.ChannelType = 17) and (lChannel.UpStreamNodeNumber = lNodeNumber)then
              lOutflowChannelNr := lChannel.ChannelNumber;
            if(lInflowChannelNr  > 0) and (lOutflowChannelNr > 0) then
              Break;
          end;

          LWetland.Populate(LIdentifier, LNodeNumber, LName,
                            LUpstreamThreshold, LInflowProportion,
                            LStorageVolume, LOutflowProportion,
                            LInflowChannelNr, LOutflowChannelNr);

          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      FreeAndNil(LWetlandSQLAgent);
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadYMDemandCentres(AChannelList: TChannelList; AFeature: TYMDemandCentreList): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadYMDemandCentres';
var
  LYMDemandCentreSQLAgent   : TYMDemandCentreSQLAgent;
  lDataSet,
  lReturnFlowDataSet        : TAbstractModelDataset;
  lConsumptiveChannelNr,
  lReclaimationChannelNr  : Integer;

  LName,
  LDescription              : WideString;

  LIdentifier,
  LNodeNumber,
  LNodeNumberRef            : Integer;

  LAveReturnFlowFactor,
  LAveMonthlyEvaporation,
  LRoutingConstant,
  LRainScalingFactor,
  LStdDeviationFactor,
  LTotalFlowLost            : Double;

  LEvapoTranspiration       : TMonthlyDoubleArray;

  LYMDemandCentre           : TYMDemandCentre;
  LReturnFlow               : TYMDemandCentreReturnFlowFeature;
  LReturnFlowDemandCentreID,
  LReturnFlowIdentifier,
  LReturnFlowChannelNr      : Integer;
  LReturnFlowTotalReturnFlow,
  LReturnFlowFlowDiversion  : Double;
  LCount                    : Integer;
  LFieldName                : WideString;
begin
  Result := False;
  try
    LYMDemandCentreSQLAgent   := TYMDemandCentreSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), lDataSet);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), lReturnFlowDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.SetSQL(LYMDemandCentreSQLAgent.GetYMDemandCentreSQL);
        lDataSet.DataSet.Open;
        while (not lDataSet.DataSet.EOF) do
        begin
          LYMDemandCentre    := AFeature.NewYMDemandCentre;

          LIdentifier             := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNodeNumber             := LDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
          LName                   := Trim(LDataSet.DataSet.FieldByName('CentreName').AsString);
          LDescription            := Trim(LDataSet.DataSet.FieldByName('CentreDescription').AsString);
          LNodeNumberRef          := LDataSet.DataSet.FieldByName('NodeRefNr').AsInteger;
          LAveReturnFlowFactor    := LDataSet.DataSet.FieldByName('AveReturnFlowFactor').AsFloat;
          LAveMonthlyEvaporation  := LDataSet.DataSet.FieldByName('AveEvaporation').AsFloat;
          LRoutingConstant        := LDataSet.DataSet.FieldByName('RoutingConstant').AsFloat;
          LRainScalingFactor      := LDataSet.DataSet.FieldByName('RainfallScalingFactor').AsFloat;
          LStdDeviationFactor     := LDataSet.DataSet.FieldByName('StdDeviationFactor').AsFloat;
          LTotalFlowLost          := LDataSet.DataSet.FieldByName('TotalFlowLost').AsFloat;
          lConsumptiveChannelNr   := LDataSet.DataSet.FieldByName('ConsumptiveChannelNr').AsInteger;
          lReclaimationChannelNr  := LDataSet.DataSet.FieldByName('ReclaimationChannelNr').AsInteger;

          for LCount := 1 to 12 do
          begin
            if LCount < 10 then
              LFieldName  := 'EvapoTranspiration0'+IntToStr(LCount)
            else
              LFieldName  := 'EvapoTranspiration'+IntToStr(LCount);
            LEvapoTranspiration[LCount]  := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
          end;

          LYMDemandCentre.Populate(LIdentifier,LNodeNumber,LName,LNodeNumberRef,LAveReturnFlowFactor,
                                  LAveMonthlyEvaporation,LStdDeviationFactor,LRoutingConstant,
                                  LRainScalingFactor,LTotalFlowLost, LDescription,LEvapoTranspiration,
                                  lConsumptiveChannelNr,lReclaimationChannelNr);

          if lReturnFlowDataSet <> nil then
          begin
            lReturnFlowDataSet.SetSQL(LYMDemandCentreSQLAgent.GetReturnFlowFeatureSQL(LIdentifier));
            lReturnFlowDataSet.DataSet.Open;
            while (not lReturnFlowDataSet.DataSet.EOF) do
            begin
              LReturnFlow := LYMDemandCentre.CastReturnFlowList.NewReturnFlowFeature;

              LReturnFlowDemandCentreID   := lReturnFlowDataSet.DataSet.FieldByName('DemandCentreID').AsInteger;
              LReturnFlowIdentifier       := lReturnFlowDataSet.DataSet.FieldByName('Identifier').AsInteger;
              LReturnFlowChannelNr        := lReturnFlowDataSet.DataSet.FieldByName('ChannelNr').AsInteger;
              LReturnFlowTotalReturnFlow  := lReturnFlowDataSet.DataSet.FieldByName('TotalReturnFlow').AsFloat;
              LReturnFlowFlowDiversion    := lReturnFlowDataSet.DataSet.FieldByName('FlowDiversion').AsFloat;

              LReturnFlow.Populate( LReturnFlowIdentifier,
                                    LReturnFlowChannelNr,
                                    LReturnFlowDemandCentreID,
                                    LReturnFlowTotalReturnFlow,
                                    LReturnFlowFlowDiversion);

              lReturnFlowDataSet.DataSet.Next;
            end;
            lReturnFlowDataSet.DataSet.Close;
          end;
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      FreeAndNil(LYMDemandCentreSQLAgent);
      LDataSet.Free;
    end;
    Result := True; 
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadStreamFlowReduction(AFeature: TStreamFlowReductionList): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadStreamFlowReduction';
var
  LSQLAgent            : TStreamFlowReductionSQLAgent;
  lDataSet             : TAbstractModelDataset;
  LStreamFlowReduction : TStreamFlowReduction;

  LIdentifier,
  LInflowNodeNumber : Integer;
  LCoveredArea  : Double;

  LUnitRunoffFileName,
  LSoilMoistureFileName,
  LSFRName,
  LSFRDescr:WideString;
begin
  Result := False;
  try
    LSQLAgent  := TStreamFlowReductionSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataSet.SetSQL(LSQLAgent.GetStreamFlowReductionSQL);
        lDataSet.DataSet.Open;
        while (not lDataSet.DataSet.EOF) do
        begin
          LStreamFlowReduction  := AFeature.NewStreamFlowReduction;
          LIdentifier           := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LInflowNodeNumber     := LDataSet.DataSet.FieldByName('InflowNodeNumber').AsInteger;
          LCoveredArea          := LDataSet.DataSet.FieldByName('CoveredArea').AsFloat;
          LUnitRunoffFileName   := Trim(LDataSet.DataSet.FieldByName('UnitRunoffFileName').AsString);
          LSoilMoistureFileName := Trim(LDataSet.DataSet.FieldByName('SoilMoistureFileName').AsString);
          LSFRName              := Trim(LDataSet.DataSet.FieldByName('SFRName').AsString);
          LSFRDescr             := Trim(LDataSet.DataSet.FieldByName('SFRDescr').AsString);

          LStreamFlowReduction.Populate(LIdentifier, LInflowNodeNumber, LCoveredArea,
                                        LUnitRunoffFileName, LSoilMoistureFileName, LSFRName, LSFRDescr);

          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      FreeAndNil(LSQLAgent);
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadCurtailmentAndDrought(ACurtailmentAndDrought: TCurtailmentAndDrought): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadCurtailmentAndDrought';
var
  LDataSet    : TAbstractModelDataset;
  LResult     : Boolean;
  LFactors    : TStartMonthArray;
  LIndex      : integer;
  LCurtailmentPeriodCount : integer;
  LIncludeInSummary : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetCurtailmentSQL);
        LDataset.DataSet.Open;

        LCurtailmentPeriodCount := LDataSet.DataSet.FieldByName('CurtailmentPeriodCount').AsInteger;
        LIncludeInSummary := LDataSet.DataSet.FieldByName('InUse').AsInteger;
        for LIndex := 1 to 10 do
        begin
          if LDataSet.DataSet.FieldByName(Format('Month%2.2d',[LIndex])).IsNull then
            LFactors[LIndex] := NullInteger
          else
            LFactors[LIndex] := LDataSet.DataSet.FieldByName(Format('Month%2.2d',[LIndex])).AsInteger;
        end;
        ACurtailmentAndDrought.Populate(LFactors,LCurtailmentPeriodCount,LIncludeInSummary)
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;
    if LResult then
     LoadCurtailmentChannel(ACurtailmentAndDrought);
     
     LoadDroughtRestriction(ACurtailmentAndDrought)
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadCurtailmentChannel(ACurtailmentAndDrought: TCurtailmentAndDrought): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadCurtailmentChannel';
var
  LDataSet            : TAbstractModelDataset;
  LResult             : Boolean;
  LCurtailmentChannel : TCurtailedChannel;
  LIdentifier         : integer;
  LChannelNumber      : integer;
  LIndex              : integer;
  LAllocationFactors  : TAllocationFactorsArray;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetCurtailmentChannelSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND LResult) do
        begin
          LIdentifier    := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LChannelNumber := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;

          for LIndex := 1 to 10 do
          begin
            LAllocationFactors[LIndex] := LDataset.DataSet.FieldByName(Format('Factor%2.2d',[LIndex])).AsFloat;
          end;

          LCurtailmentChannel := ACurtailmentAndDrought.NewCurtailedChannel;
          LCurtailmentChannel.Initialise;
          LResult := LCurtailmentChannel.Populate(LIdentifier,LChannelNumber,LAllocationFactors);
          LDataset.DataSet.Next;
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadDroughtRestriction(ACurtailmentAndDrought: TCurtailmentAndDrought): boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadDroughtRestriction';
var
  LDataSet             : TAbstractModelDataset;
  LDroughtDataset      : TAbstractModelDataset;
  LResult              : Boolean;
  LDroughtRestriction  : TDroughtRestriction;
  LIdentifier          : integer;
  LReservoirStr        : string;
  LChannelStr          : string;
  LName                : string;
  LIndex               : integer;
  LFactors             : TAllocationFactorsArray;
  LVolumes             : TStorageVolumesArray;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDroughtDataset);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDroughtRestrictionSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND LResult) do
        begin
          LChannelStr   := '';
          LReservoirStr := '';

          LIdentifier   := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LName         := Trim(LDataset.DataSet.FieldByName('Name').AsString);
          LReservoirStr := Trim(LDataset.DataSet.FieldByName('ReservoirNumbers').AsString);
          LChannelStr   := Trim(LDataset.DataSet.FieldByName('ChannelNumbers').AsString);

          LDroughtRestriction := ACurtailmentAndDrought.NewDroughtRestriction;
          LDroughtRestriction.Initialise;
          LDroughtRestriction.Populate(LIdentifier,LName,LReservoirStr,LChannelStr);

          if Assigned(LDroughtDataset) then
          begin
            LDroughtDataset.DataSet.Close;
            LDroughtDataset.SetSQL(FSQLAgent.GetDroughtRestrictionFactorsSQL(LIdentifier));
            LDroughtDataset.DataSet.Open;
            if Not LDroughtDataset.DataSet.Eof then
            begin
              for LIndex := 1 to 10 do
              begin
                LFactors[LIndex] := LDroughtDataset.DataSet.FieldByName(Format('Factor%2.2d',[LIndex])).AsFloat;
              end;
              LDroughtRestriction.PopulateAllocationFactors(LFactors);
            end;

            LDroughtDataset.DataSet.Close;
            LDroughtDataset.SetSQL(FSQLAgent.GetDroughtRestrictionVolumeSQL(LIdentifier));
            LDroughtDataset.DataSet.Open;
            if Not LDroughtDataset.DataSet.Eof then
            begin
              for LIndex := 1 to 10 do
              begin
                LVolumes[LIndex] := LDroughtDataset.DataSet.FieldByName(Format('Volume%2.2d',[LIndex])).AsFloat;
              end;
              LDroughtRestriction.PopulateStorageVolumes(LVolumes);
            end;
            LDroughtDataset.DataSet.Close;
          end;

          LDataset.DataSet.Next;
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
      LDroughtDataset.DataSet.Close;
      LDroughtDataset.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadGroundWaters(AFeature: TGroundWaterList): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadGroundWaters';
var
  LIdentifier                             : Integer;
  LNodeNumber                             : Integer;
  LDescription                            : WideString;
  LName                                   : WideString;

  LDataSet,
  LOtherDataSet                           : TAbstractModelDataset;
 // LChannel                                : IGeneralFlowChannel;
  //LFieldName                              : string;
  LIndex                                  : Integer;

  LBaseflowNodeNr       : Integer;
  LAbstractionNodeNr    : Integer;
  LCollectionNodeNr     : Integer;


  LAquiferStorativity                     : Double;
  LAquiferStaticWaterLevel                : Double;
  LUnsaturatedStorageCapacity             : Double;
  LInitialUnsaturatedStorage              : Double;
  LMaximumDischargeRate                   : Double;
  LMovingAverageRecharge                  : Double;
  LPitmanSoilMoistureCapacity             : Double;
  LPitmanSoilMoistureStorageCapacity      : Double;
  LPitmansoilMoistureFlowState            : Double;
  LPitmanSoilMoistureFlowEquation         : Double;
  LPitmanMaximumGroundwaterFlow           : Double;
  LPitmanSoilMoistureRechargeEquation     : Double;
  LPitmanGroundwaterFlow                  : Double;
  LMaximumRateOfGroundwaterBaseFlow       : Double;
  LPowerHeadDifferenceBaseFlowEquation    : Double;
  LMaximumHydrologicalGradient            : Double;
  LAquiferTransmissivity                  : Double;
  LBoreHoleDistanceToRiver                : Double;
  LMaximumGroundwaterAbstraction          : Double;
  LParameterK2                            : Double;
  LParameterK3                            : Double;
  LGroundWaterEvaporationArea             : Double;
  LMonthlyWaterEvaporation                : TMonthlyDoubleArray;
  LMonthlyWaterUsageFactors               : TMonthlyDoubleArray;
  LGroundWater                            : TGroundWater;
  LGroundWaterSQLAgent                    : TGroundWaterSQLAgent;
begin
  Result := False;
  try
    LGroundWaterSQLAgent := TGroundWaterSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LOtherDataSet);

    LPitmanSoilMoistureCapacity          := 0.0;
    LPitmanSoilMoistureStorageCapacity   := 0.0;
    LPitmansoilMoistureFlowState         := 0.0;
    LPitmanSoilMoistureFlowEquation      := 0.0;
    LPitmanMaximumGroundwaterFlow        := 0.0;
    LPitmanSoilMoistureRechargeEquation  := 0.0;
    LPitmanGroundwaterFlow               := 0.0;

    LBaseflowNodeNr := 0;
    LAbstractionNodeNr := 0;
    LCollectionNodeNr := 0;

    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(LGroundWaterSQLAgent.GetGroundWaterSQL);
        LDataSet.DataSet.Open;
        while (not LDataSet.DataSet.EOF) do
        begin
          LGroundWater                         := AFeature.NewGroundWater;

          LIdentifier                          := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LNodeNumber                          := LDataSet.DataSet.FieldByName('AquiferNodeNumber').AsInteger;
          LDescription                         := Trim(LDataSet.DataSet.FieldByName('GroundWaterDescription').AsString);
          LName                                := Trim(LDataSet.DataSet.FieldByName('GroundWaterName').AsString);
          LAquiferStorativity                  := LDataSet.DataSet.FieldByName('AquiferStorativity').AsFloat;
          LAquiferStaticWaterLevel             := LDataSet.DataSet.FieldByName('AquiferStaticWaterlevel').AsFloat;
          LUnsaturatedStorageCapacity          := LDataSet.DataSet.FieldByName('UnsaturatedStorageCapacity').AsFloat;
          LInitialUnsaturatedStorage           := LDataSet.DataSet.FieldByName('InitialUnsaturatedStorage').AsFloat;
          LMaximumDischargeRate                := LDataSet.DataSet.FieldByName('MaximumAquiferRecharge').AsFloat;
          LMovingAverageRecharge               := LDataSet.DataSet.FieldByName('MovingAverageRecharge').AsFloat;
          LMaximumRateOfGroundwaterBaseFlow    := LDataSet.DataSet.FieldByName('MaximumBaseFlowRate').AsFloat;
          LPowerHeadDifferenceBaseFlowEquation := LDataSet.DataSet.FieldByName('HeadBaseFlowPower').AsFloat;
          LMaximumHydrologicalGradient         := LDataSet.DataSet.FieldByName('MaximumHydrologicalGradient').AsFloat;
          LAquiferTransmissivity               := LDataSet.DataSet.FieldByName('AquiferTransmissivity').AsFloat;
          LBoreHoleDistanceToRiver             := LDataSet.DataSet.FieldByName('BoreHoleDistanceToRiver').AsFloat;
          LMaximumGroundwaterAbstraction       := LDataSet.DataSet.FieldByName('MaximumWaterAbstraction').AsFloat;
          LParameterK2                         := LDataSet.DataSet.FieldByName('ParameterK2').AsFloat;
          LParameterK3                         := LDataSet.DataSet.FieldByName('ParameterK3').AsFloat;
          LGroundWaterEvaporationArea          := LDataSet.DataSet.FieldByName('WaterEvaporationArea').AsFloat;

          LGroundWater.Populate(LIdentifier,LNodeNumber,LDescription,LName,LAquiferStorativity,
                                LAquiferStaticWaterLevel,LUnsaturatedStorageCapacity,
                                LInitialUnsaturatedStorage,LMaximumDischargeRate,LMovingAverageRecharge,
                                LMaximumRateOfGroundwaterBaseFlow,LPowerHeadDifferenceBaseFlowEquation,
                                LMaximumHydrologicalGradient,LAquiferTransmissivity,
                                LBoreHoleDistanceToRiver,LMaximumGroundwaterAbstraction,
                                LParameterK2,LParameterK3,LGroundWaterEvaporationArea);

          //GroundWater Pitman
          if Assigned(LOtherDataSet) then
          begin
            LOtherDataSet.SetSQL(LGroundWaterSQLAgent.GetGroundWaterPitmanSQL(LIdentifier));
            LOtherDataSet.DataSet.Open;
            LPitmanSoilMoistureCapacity          := LOtherDataSet.DataSet.FieldByName('SoilMoistureCapacity').AsFloat;
            LPitmanSoilMoistureStorageCapacity   := LOtherDataSet.DataSet.FieldByName('SoilMoistureStorageCapacity').AsFloat;
            LPitmansoilMoistureFlowState         := LOtherDataSet.DataSet.FieldByName('SoilMoistureFlowState').AsFloat;
            LPitmanSoilMoistureFlowEquation      := LOtherDataSet.DataSet.FieldByName('SoilMoistureFlowEquation').AsFloat;
            LPitmanMaximumGroundwaterFlow        := LOtherDataSet.DataSet.FieldByName('MaximumGroundWaterFlow').AsFloat;
            LPitmanSoilMoistureRechargeEquation  := LOtherDataSet.DataSet.FieldByName('SoilMoistureRechargeEquation').AsFloat;
            LPitmanGroundwaterFlow               := LOtherDataSet.DataSet.FieldByName('GroundWaterFlow').AsFloat;
            LOtherDataSet.DataSet.Close;
          end;

          LGroundWater.PopulateGroundWaterPitman(LPitmanSoilMoistureCapacity,LPitmanSoilMoistureStorageCapacity,
                                                 LPitmansoilMoistureFlowState,LPitmanSoilMoistureFlowEquation,
                                                 LPitmanMaximumGroundwaterFlow,LPitmanSoilMoistureRechargeEquation,
                                                 LPitmanGroundwaterFlow);
          //GroundWater Evaporation
          if Assigned(LOtherDataSet) then
          begin
            LOtherDataSet.SetSQL(LGroundWaterSQLAgent.GetGroundWaterEvaporationSQL(LIdentifier));
            LOtherDataSet.DataSet.Open;
            for LIndex := 1 to 12 do
            begin
              LMonthlyWaterEvaporation[LIndex] := LOtherDataSet.DataSet.FieldByName(Format('Evaporation%2.2d',[LIndex])).AsFloat;
            end;
            LOtherDataSet.DataSet.Close;
          end;

          LGroundWater.PopulateGroundWaterEvaporation(LMonthlyWaterEvaporation);

          //GroundWater Usage Factor
          if Assigned(LOtherDataSet) then
          begin
            LOtherDataSet.SetSQL(LGroundWaterSQLAgent.GetGroundWaterUsageFactorSQL(LIdentifier));
            LOtherDataSet.DataSet.Open;
            for LIndex := 1 to 12 do
            begin
              LMonthlyWaterUsageFactors[LIndex]  := LOtherDataSet.DataSet.FieldByName(Format('Factor%2.2d',[LIndex])).AsFloat;;
            end;
            LOtherDataSet.DataSet.Close;
          end;
          LGroundWater.PopulateGroundWaterFactors(LMonthlyWaterUsageFactors);

          LoadGroundWaterSubCatchment(LGroundWater);

          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
      LOtherDataSet.Free;
      LGroundWaterSQLAgent.Free;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TNetworkFeaturesLoadAgent.LoadGroundWaterSubCatchment(AGroundWater: TGroundWater): Boolean;
const OPNAME = 'TNetworkFeaturesLoadAgent.LoadGroundWaterSubCatchment';
var
  LDataSet                               : TAbstractModelDataset;
  LRefNodeNumber                         : integer;
  LAbstractionNodeNumber                 : integer;
  LCollectionNodeNumber                  : integer;
  LBaseFlowNodeNumber                    : integer;
  LAquiferInflowChannelNr                : integer;
  LAquferExcessChannelNr                 : integer;
  LGroundWaterBaseFlowChannelNr          : integer;
  LRegulationFromAquiferChannelNr        : integer;
  LRegulationFromBaseFlowChannelNr       : integer;
  LGroundWaterBaseFlowRemainderChannelNr : integer;
  LGroundWaterAbstractionChannelNr       : integer;
  LOutflowToNetworkChannelNr             : integer;
  LDownstreamChannelNr                   : integer;
  LUpstreamChannelNr                     : integer;
  LSurfaceRunOffChannelNr                : integer;
  LGroundWaterSQLAgent                   : TGroundWaterSQLAgent;
begin
  Result := False;
  try
    if(AGroundWater = nil) then Exit;


    LGroundWaterSQLAgent := TGroundWaterSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      lDataSet.SetSQL(LGroundWaterSQLAgent.GetGroundWaterSubCatchmentSQL(AGroundWater.Identifier));
      lDataset.DataSet.Open;
      LRefNodeNumber                         := LDataset.DataSet.FieldByName('RefNodeNumber').AsInteger;
      LAbstractionNodeNumber                 := LDataset.DataSet.FieldByName('AbstractionNodeNumber').AsInteger;
      LCollectionNodeNumber                  := LDataset.DataSet.FieldByName('CollectionNodeNumber').AsInteger;
      LBaseFlowNodeNumber                    := LDataset.DataSet.FieldByName('BaseFlowNodeNumber').AsInteger;
      LAquiferInflowChannelNr                := LDataset.DataSet.FieldByName('AquiferInflowChannelNr').AsInteger;
      LAquferExcessChannelNr                 := LDataset.DataSet.FieldByName('AquferExcessChannelNr').AsInteger;
      LGroundWaterBaseFlowChannelNr          := LDataset.DataSet.FieldByName('GroundWaterBaseFlowChannelNr').AsInteger;
      LRegulationFromAquiferChannelNr        := LDataset.DataSet.FieldByName('RegulationFromAquiferChannelNr').AsInteger;
      LRegulationFromBaseFlowChannelNr       := LDataset.DataSet.FieldByName('RegulationFromBaseFlowChannelNr').AsInteger;
      LGroundWaterAbstractionChannelNr       := LDataset.DataSet.FieldByName('GroundWaterAbstractionChannelNr').AsInteger;
      LOutflowToNetworkChannelNr             := LDataset.DataSet.FieldByName('OutflowToNetworkChannelNr').AsInteger;
      LDownstreamChannelNr                   := LDataset.DataSet.FieldByName('DownstreamChannelNr').AsInteger;
      LUpstreamChannelNr                     := LDataset.DataSet.FieldByName('UpstreamChannelNr').AsInteger;
      LSurfaceRunOffChannelNr                := LDataset.DataSet.FieldByName('SurfaceRunOffChannelNr').AsInteger;
      LGroundWaterBaseFlowRemainderChannelNr := LDataset.DataSet.FieldByName('BaseFlowRemainderChannelNr').AsInteger; 

      AGroundWater.PopulateGroundWaterSubCatchment(LRefNodeNumber,LAquiferInflowChannelNr,LUpstreamChannelNr,LDownstreamChannelNr,LAquferExcessChannelNr,
                                                   LGroundWaterBaseFlowChannelNr,LRegulationFromAquiferChannelNr,LRegulationFromBaseFlowChannelNr,
                                                   LGroundWaterBaseFlowRemainderChannelNr,LSurfaceRunOffChannelNr,
                                                   LGroundWaterAbstractionChannelNr,LOutflowToNetworkChannelNr,
                                                   LBaseFlowNodeNumber,LAbstractionNodeNumber,LCollectionNodeNumber);
      lDataset.DataSet.Close;
      Result := True;
    finally
      lDataset.Free;
      LGroundWaterSQLAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
