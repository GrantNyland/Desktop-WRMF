//
//
//  UNIT      : Contains TChannelDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/08/29
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UChannelDataLoadAgent;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject,
  UChannelData,
  UMasterControlFeatures,
  UMinimumFlowConstraints,
  UMinMaxFlowConstraints,
  ULossFeatures,
  UPumpingFeatures,
  USpecifiedDemandFeatures,
  UChannelDataSQLAgent;

type
  TChannelDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TChannelDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadMasterControlFeatures (AChannelList : TChannelList;
                                        AFeatureList : TMasterControlFeatureList) : boolean;

    function LoadMinimumFlowConstraints (AChannelList : TChannelList;
                                         AFeatureList : TMinimumFlowConstraintList): boolean;

    function LoadSpecifiedDemandFeatures (AChannelList : TChannelList;
                                          AFeatureList : TSpecifiedDemandFeatureList): boolean;

    function LoadLossChannelValues(AFeatureID        : integer;
                                   var AWaterLoss    : TLossMonthlyDoublesArray;
                                   var ADivertedFlow : TLossMonthlyDoublesArray): boolean;

    function LoadMinMaxFlowConstraints (AChannelList : TChannelList;
                                        AFeatureList : TMinMaxFlowConstraintList;
                                        APumpingList : TPumpingFeatureList): boolean;
    function LoadMinMaxFlows (AFeatureID   : integer;
                              AMinMaxFlows : TMinMaxMonthlyDoublesArray): boolean;
    function LoadMinMaxDistribution(AFeatureID : integer;AMinMaxFlows : TMinMaxMonthlyDoublesArray;
                                    AMinMaxFlowConstraint : TMinMaxFlowConstraint): boolean;
    function PopulateGeneralFlowChannelData(ADataSet : TAbstractModelDataset;
                                            AChannel : TGeneralFlowChannel): boolean;
    function LoadLossFeatures (AChannelList : TChannelList;
                               AFeatureList : TLossFeatureList): boolean;

  public
    procedure LoadChannelDetailsContextData(AContextData: TStringList;ARecordIdentifier, AChannelNumber,
      AChannelType: string);
    procedure LoadChannelContextData (AContextData   : TStringList;
                                      AChannelID     : string;
                                      AChannelNumber : string);
    procedure LoadWaterSupplyDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
    procedure LoadContextData_FeatureIDSubIDFieldNameID (AContextData : TStringList;
                                                         AFeatureID   : string;
                                                         ASubID       : string;
                                                         AFieldNameID : string);
    procedure LoadContextData_FeatureID (AContextData : TStringList;
                                         AFeatureID   : string);
    procedure LoadContextData_FeatureIDFieldNameID (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
    function ConstructData (AChannelList                : TChannelList;
                            AMasterControlFeatureList   : TMasterControlFeatureList;
                            AMinimumFlowConstraintList  : TMinimumFlowConstraintList;
                            ASpecifiedDemandFeatureList : TSpecifiedDemandFeatureList;
                            AMinMaxFlowConstraintList   : TMinMaxFlowConstraintList;
                            APumpingFeatureList         : TPumpingFeatureList;
                            ALossFeatureList            : TLossFeatureList): boolean;

    function LoadOutputChannelData (AChannelList : TChannelList): boolean;
    function LoadGeneralFlowChannels (AChannelList : TChannelList): boolean;

  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UChannelPlanningLoadAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations, DB;

procedure TChannelDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TChannelDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TChannelDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TChannelDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChannelDataLoadAgent.ConstructData (
                                  AChannelList : TChannelList;
                                  AMasterControlFeatureList   : TMasterControlFeatureList;
                                  AMinimumFlowConstraintList  : TMinimumFlowConstraintList;
                                  ASpecifiedDemandFeatureList : TSpecifiedDemandFeatureList;
                                  AMinMaxFlowConstraintList   : TMinMaxFlowConstraintList;
                                  APumpingFeatureList         : TPumpingFeatureList;
                                  ALossFeatureList            : TLossFeatureList) : boolean;
const OPNAME = 'TChannelDataLoadAgent.ConstructData';
var
  lPlanningLoadAgent : TChannelPlanningLoadAgent;
begin
  Result := True;
  try
    LoadGeneralFlowChannels(AChannelList);
    LoadMasterControlFeatures(AChannelList, AMasterControlFeatureList);
    LoadMinimumFlowConstraints(AChannelList, AMinimumFlowConstraintList);
    LoadSpecifiedDemandFeatures(AChannelList, ASpecifiedDemandFeatureList);
    LoadMinMaxFlowConstraints(AChannelList, AMinMaxFlowConstraintList, APumpingFeatureList);
    LoadLossFeatures(AChannelList, ALossFeatureList);

    lPlanningLoadAgent := TChannelPlanningLoadAgent.Create(FAppModules);
    try
      lPlanningLoadAgent.LoadChannelTimeControl(AChannelList);
      lPlanningLoadAgent.LoadChannelSwitchControl(AChannelList);
    finally
      lPlanningLoadAgent.Free;
    end;
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

{******************************************************************************}
{* GeneralFlowChannel                                                         *}
{******************************************************************************}

function TChannelDataLoadAgent.PopulateGeneralFlowChannelData
                                            (ADataSet : TAbstractModelDataset;
                                             AChannel : TGeneralFlowChannel): boolean;
const OPNAME = 'TChannelDataLoadAgent.PopulateGeneralFlowChannelData';
var
  LRecordIdentifier         : integer;
  LChannelNumber            : integer;
  LChannelType              : integer;
  LChannelSubType           : integer;
  LUpStreamNodeNumber       : integer;
  LDownStreamDownNodeNumber : integer;
  LPenaltyNumber            : integer;
  LChannelName              : string;
  LSummaryOutput            : string;
  LFlowOutput,
  LFirmYield                : string;
  LChannelAreaId            : integer;
begin
  Result := False;
  try
    if Assigned(ADataSet) and Assigned(AChannel) then
    begin
      LRecordIdentifier         := ADataSet.DataSet.FieldByName('RecordIdentifier').AsInteger;
      LChannelName              := Trim(ADataSet.DataSet.FieldByName('ChannelName').AsString);
      LChannelNumber            := ADataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
      LChannelType              := ADataSet.DataSet.FieldByName('ChannelType').AsInteger;
      LChannelSubType           := ADataSet.DataSet.FieldByName('ChannelSubType').AsInteger;
      LPenaltyNumber            := ADataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
      LUpStreamNodeNumber       := ADataSet.DataSet.FieldByName('UpNodeNumber').AsInteger;
      LDownStreamDownNodeNumber := ADataSet.DataSet.FieldByName('DownNodeNumber').AsInteger;
      LSummaryOutput            := Trim(ADataSet.DataSet.FieldByName('SummaryOutput').AsString);
      LChannelAreaId            := ADataSet.DataSet.FieldByName('ChannelAreaID').AsInteger;
      LFirmYield                := Trim(ADataSet.DataSet.FieldByName('FirmYieldCalc').AsString);
      LFlowOutput               := Trim(ADataSet.DataSet.FieldByName('FlowOutput').AsString);
      Result :=  AChannel.PopulateGeneralFlowChannel
                   (LRecordIdentifier, LChannelNumber, LChannelType,
                    LChannelSubType, LChannelName, LUpStreamNodeNumber,
                    LDownStreamDownNodeNumber, LPenaltyNumber, LSummaryOutput,
                    LChannelAreaId,LFlowOutput, LFirmYield);
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataLoadAgent.LoadGeneralFlowChannels (AChannelList : TChannelList): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadGeneralFlowChannels';
var
  LDataSet     : TAbstractModelDataset;
  lChannel     : TGeneralFlowChannel;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetGeneralFlowChannelDataSQL);
        LDataset.DataSet.Open;
        Result := True;

        while (NOT LDataset.DataSet.EOF) do
        begin
          lChannel := AChannelList.NewGeneralFlowChannel;
          if not PopulateGeneralFlowChannelData(LDataset, lChannel) then
          begin
            AChannelList.DeleteGeneralFlowChannelWithID(lChannel.ChannelID);
          end;
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
{* MasterControlFeature                                                       *}
{******************************************************************************}

function TChannelDataLoadAgent.LoadMasterControlFeatures
                                       (AChannelList : TChannelList;
                                        AFeatureList : TMasterControlFeatureList) : boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadMasterControlFeatures';
var
  LDemandCentreDataSet : TAbstractModelDataset;
  LDataSet             : TAbstractModelDataset;
  LIndex               : integer;
  LMasterControlType   : string;
  LFactors             : TMasterMonthlyDoublesArray;
  lChannel             : TGeneralFlowChannel;
  lResult              : Boolean;
  lNewChannel          : Boolean;
  lChannelNr           : integer;
  lFeature             : TMasterControlFeature;
  lFeatureType         : integer;
  lFeatureSubType      : integer;
  lFeatureName         : string;
  lFeatureID           : integer;
  LMonthlyFactors      : TAbstractFieldProperty;
  lDemandCentreID      : integer;
  LDemandCentreType    : string;
  LAnnualDemand        : double;
  LMinimumDemand       : double;
  LIncludeInOutput     : boolean;
begin
  Result := FALSE;
  try
    LMonthlyFactors := FAppModules.FieldProperties.FieldProperty('MinEnergyDemand');
    if not Assigned(LMonthlyFactors) then
      raise Exception.Create('Field (MinEnergyDemand) not found in field properties');
    SetLength(LFactors,LMonthlyFactors.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDemandCentreDataSet);
    lResult := TRUE;
    try
      if Assigned(LDataSet) AND Assigned(LDemandCentreDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMasterControlChannelDataSQL);
        LDataset.DataSet.Open;
        while (lResult AND (NOT LDataset.DataSet.EOF)) do
        begin
          lFeatureType       := 12;
          lFeatureSubType    := 0;
          lNewChannel        := FALSE;
          lDemandCentreID    := 0;
          LDemandCentreType  := '';
          LAnnualDemand      := 0.0;
          LMinimumDemand     := 0.0;
          LIncludeInOutput   := FALSE;
          lChannelNr         := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          lFeatureID         := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          lFeatureName       := Trim(LDataset.DataSet.FieldByName('FeatureName').AsString);
          LMasterControlType := Trim(LDataset.DataSet.FieldByName('MasterControlType').AsString);
          for LIndex := LMonthlyFactors.ArrayLow to LMonthlyFactors.ArrayHigh do
            LFactors[LIndex] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).AsFloat;

          if (FAppModules.Model.ModelName = CPlanning) then
          begin
            LDemandCentreDataSet.DataSet.Close;
            LDemandCentreDataSet.SetSQL(FSQLAgent.GetDemandChannelCentreSQL(lChannelNr));
            LDemandCentreDataSet.DataSet.Open;
            if (LDemandCentreDataSet.DataSet.RecordCount > 0) then
            begin
              lDemandCentreID    := LDemandCentreDataSet.DataSet.FieldByName('DemandCentreID').AsInteger;
              LDemandCentreType  := Trim(LDemandCentreDataSet.DataSet.FieldByName('DemandCentreType').AsString);
              LAnnualDemand      := LDemandCentreDataSet.DataSet.FieldByName('AnnualDemand').AsFloat;
              LMinimumDemand     := LDemandCentreDataSet.DataSet.FieldByName('MinimumDemand').AsFloat;
              if(Trim(LDemandCentreDataSet.DataSet.FieldByName('IncludeInOutput').AsString) = '1') then
                LIncludeInOutput := True
              else
                LIncludeInOutput := False;
            end;
          end;

          lChannel := AChannelList.CastChannelByChannelNumber[lChannelNr];
          if (NOT Assigned(lChannel)) then
          begin
            lChannel    := AChannelList.NewGeneralFlowChannel;
            lChannel.Initialise;
            lNewChannel := TRUE;
            lResult     := PopulateGeneralFlowChannelData(LDataset, LChannel);
          end;

          if (lResult) then
          begin
            lFeature := AFeatureList.NewMasterControlFeature;
            lFeature.Initialise;
            lResult := lFeature.Populate(LFeatureID, lFeatureName, lFeatureType, lFeatureSubType,
                                         lChannel.ChannelNumber, LMasterControlType, LFactors,
                                         lDemandCentreID, LDemandCentreType, LAnnualDemand, LMinimumDemand, LIncludeInOutput);
            if (lResult) then
            begin
              lChannel.MasterControlFeature := lFeature;
            end
            else
            begin
              AFeatureList.DeleteMasterControlFeatureWithID(lFeatureID);
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
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* SpecifiedDemandFeature                                                     *}
{******************************************************************************}

function TChannelDataLoadAgent.LoadSpecifiedDemandFeatures
                                         (AChannelList : TChannelList;
                                          AFeatureList : TSpecifiedDemandFeatureList): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadSpecifiedDemandFeatures';
var
  LDataSet                 : TAbstractModelDataset;
  LCatchmentRefNumber      : integer;
  LStochasticIndicator     : string;
  LSpecifiedDemandFileName : string;
  lChannel                 : TGeneralFlowChannel;
  lResult                  : Boolean;
  lNewChannel              : Boolean;
  lChannelNr               : integer;
  lFeature                 : TSpecifiedDemandFeature;
  lFeatureType             : integer;
  lFeatureSubType          : integer;
  lFeatureName             : string;
  lFeatureID               : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    lResult := TRUE;
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetSpecifiedDemandChannelDataSQL);
        LDataset.DataSet.Open;
        while (lResult AND (NOT LDataset.DataSet.EOF)) do
        begin
          lFeatureType    := 4;
          lFeatureSubType := 0;
          lNewChannel  := FALSE;
          lChannelNr               := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          lFeatureID               := LDataset.DataSet.FieldByName('DemandID').AsInteger;
          LCatchmentRefNumber      := LDataset.DataSet.FieldByName('GaugeNumber').AsInteger;
          LStochasticIndicator     := Trim(LDataset.DataSet.FieldByName('Stochastic').AsString);
          LSpecifiedDemandFileName := Trim(LDataset.DataSet.FieldByName('Fullname').AsString);
          lFeatureName             := Trim(LDataset.DataSet.FieldByName('Featurename').AsString);
          lChannel                 := AChannelList.CastChannelByChannelNumber[lChannelNr];
          if (NOT Assigned(lChannel)) then
          begin
            lChannel    := AChannelList.NewGeneralFlowChannel;
            lChannel.Initialise;
            lNewChannel := TRUE;
            lResult     := PopulateGeneralFlowChannelData(LDataset, LChannel);
          end;
          if (lResult) then
          begin
            lFeature := AFeatureList.NewSpecifiedDemandFeature;
            lFeature.Initialise;
            lResult := lFeature.Populate(LFeatureID, lFeatureName, lChannel.ChannelNumber,
                         lFeatureType, lFeatureSubType, LCatchmentRefNumber,
                         LStochasticIndicator, LSpecifiedDemandFileName);
            if (lResult) then
            begin
              lChannel.SpecifiedDemandFeature := lFeature;
            end
            else
            begin
              AFeatureList.DeleteSpecifiedDemandFeatureWithID(lFeatureID);
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
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


{******************************************************************************}
{* MinMaxFlowConstraints                                                      *}
{******************************************************************************}

function TChannelDataLoadAgent.LoadMinMaxFlowConstraints
                                       (AChannelList : TChannelList;
                                        AFeatureList : TMinMaxFlowConstraintList;
                                        APumpingList : TPumpingFeatureList): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadMinMaxFlowConstraints';
var
  LDataSet         : TAbstractModelDataset;
  LFeatureID       : integer;
  LChannel         : TGeneralFlowChannel;
  lResult          : Boolean;
  lNewChannel      : Boolean;
  lChannelNr       : integer;
  lConstraint      : TMinMaxFlowConstraint;
  lPumping         : TPumpingFeature;
  lFeatureType     : integer;
  lFeatureSubType  : integer;
  lFeatureName     : string;
  lPumpingHead     : double;
  lEfficiency      : double;
  lPumpFeatureID   : integer;
  lPumpFeatureName : string;

  LMonthlyFlows    : TMinMaxMonthlyDoublesArray;
  LMinMaxFlowField : TAbstractFieldProperty;
  lArcCount        : integer;
  lMonth           : integer;

begin
  Result := FALSE;
  try
    LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    if not Assigned(LMinMaxFlowField) then
      raise Exception.Create('Field (FlowConstraints) not found in field properties');
    SetLength(LMonthlyFlows, LMinMaxFlowField.ArrayLength, LMinMaxFlowField.ArrayLength(1));

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    lResult := TRUE;
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMinMaxChannelDataSQL);
        LDataset.DataSet.Open;
        while (lResult AND (NOT LDataset.DataSet.EOF)) do
        begin
          lFeatureType     := 3;
          lFeatureSubType  := 0;
          lNewChannel      := FALSE;
          lChannelNr       := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          lFeatureID       := LDataset.DataSet.FieldByName('MinMaxID').AsInteger;
          lFeatureName     := Trim(LDataset.DataSet.FieldByName('MinMaxChannelName').AsString);
          lPumpFeatureID   := LDataset.DataSet.FieldByName('PumpID').AsInteger;
          lPumpFeatureName := Trim(LDataset.DataSet.FieldByName('FeatureName').AsString);
          LPumpingHead     := LDataset.DataSet.FieldByName('PumpingHead').AsFloat;
          LEfficiency      := LDataset.DataSet.FieldByName('PumpingEfficiency').AsFloat;
          lChannel         := AChannelList.CastChannelByChannelNumber[lChannelNr];
          if (NOT Assigned(lChannel)) then
          begin
            lChannel    := AChannelList.NewGeneralFlowChannel;
            lChannel.Initialise;
            lNewChannel := TRUE;
            lResult     := PopulateGeneralFlowChannelData(LDataset, LChannel);
          end;
          if (lResult) then
          begin
            lConstraint := AFeatureList.NewMinMaxFlowConstraint;
            lConstraint.Initialise;
            for lArcCount := LMinMaxFlowField.ArrayLow to LMinMaxFlowField.ArrayHigh do
              for lMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
                LMonthlyFlows[lArcCount, lMonth] := NullFloat;
            lResult := LoadMinMaxFlows(lFeatureID, lMonthlyFlows);
            if (lResult) then
            begin
              lConstraint.Populate(lFeatureID, lFeatureName, lChannel.ChannelNumber, lFeatureType, lFeatureSubType, lMonthlyFlows);
              if (FAppModules.Model.ModelName = CPlanning) then
                LoadMinMaxDistribution(lFeatureID,lMonthlyFlows,lConstraint);
              lChannel.MinMaxFlowConstraint := lConstraint;
              if (LChannel.ChannelType = 9) then
              begin
                lPumping := APumpingList.NewPumpingFeature;
                lPumping.Initialise;
                lPumping.Populate(lPumpFeatureID, lPumpFeatureName, lChannel.ChannelNumber, 11, 0, lPumpingHead, lEfficiency);
                LChannel.PumpingFeature := lPumping;
              end;
            end
            else
            begin
              AFeatureList.DeleteMinMaxFlowConstraintWithID(lFeatureID);
              if (lNewChannel) then
                AChannelList.DeleteGeneralFlowChannelWithID(LChannel.ChannelID);
            end;
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

function TChannelDataLoadAgent.LoadMinMaxFlows (AFeatureID   : integer;
                                                AMinMaxFlows : TMinMaxMonthlyDoublesArray): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadMinMaxFlows';
var
  LDataSet        : TAbstractModelDataset;
  LMonth          : integer;
  LSubID          : integer;
  LMinMaxFlowField : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMinMaxChannelValuesSQL(AFeatureID));
        LDataset.DataSet.Open;
        while (NOT LDataset.DataSet.EOF) do
        begin
          LSubID := LDataset.DataSet.FieldByName('SubIdentifier').AsInteger;
          for LMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
            AMinMaxFlows[lSubID, lMonth] := LDataset.DataSet.FieldByName(Format('MFlow%2.2d',[lMonth])).AsFloat;
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

function TChannelDataLoadAgent.LoadMinMaxDistribution(AFeatureID : integer;AMinMaxFlows : TMinMaxMonthlyDoublesArray;
                                                      AMinMaxFlowConstraint : TMinMaxFlowConstraint): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadMinMaxDistribution';
var
  LDataSet        : TAbstractModelDataset;
  LMonth          : integer;
  LSubID          : integer;
  LMinMaxFlowField : TAbstractFieldProperty;
  LRowCount,
  LArcCount,
  LIndex : integer;
  LMonthDays : TMonthDaysArray;
  LFieldProperty : TAbstractFieldProperty;
  LMonthlyDemand,
  LFactorsTotal : double;
  LDistribution : TMinMaxMonthlyDoublesArray;
begin
  Result := FALSE;
  try
    LMinMaxFlowField := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
    if (LMinMaxFlowField = nil) then
      raise Exception.Create('Field (FlowConstraints) not found in field properties');
    SetLength(LDistribution, LMinMaxFlowField.ArrayLength, LMinMaxFlowField.ArrayLength(1));
    for lArcCount := LMinMaxFlowField.ArrayLow  to LMinMaxFlowField.ArrayHigh do
      for lMonth := LMinMaxFlowField.ArrayLowDimTwo  to LMinMaxFlowField.ArrayHighDimTwo do
        LDistribution[lArcCount, lMonth] := NullFloat;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) and Assigned(AMinMaxFlowConstraint) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetMinMaxChannelDistributionValuesSQL(AFeatureID));
        LDataset.DataSet.Open;
        if LDataset.DataSet.EOF then
        begin
          if AMinMaxFlowConstraint.Channel <> nil then
          begin
            //LDataSet.SetSQL(FSQLAgent.GetChannelPenaltyStructureDataByIDSQL(AFeatureID));
            //LDataset.DataSet.Open;
            //if not (LDataset.DataSet.FieldByName('ArcCount').IsNull) and (LDataset.DataSet.FieldByName('ArcCount').AsInteger > 0) then
            //begin
              LFieldProperty := FAppModules.FieldProperties.FieldProperty('Days');
              if (LFieldProperty = nil) then
                raise Exception.Create('Field (Days) not found in field properties');
              SetLength(LMonthDays,LFieldProperty.ArrayLength);
              LMonthDays := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthsDaysArray;
              LRowCount  := 0;
              //for LArcCount := 1 to LDataset.DataSet.FieldByName('ArcCount').AsInteger do
              for LArcCount := 1 to High(AMinMaxFlows) do
              begin
                if(AMinMaxFlows[LArcCount,MinMonths] = NullFloat) then
                  Break;
                LFactorsTotal := 0.0;
                for LIndex := MinMonths to MaxMonths do
                begin
                  LDistribution[LArcCount,LIndex] := (LMonthDays[LIndex]* 24*60*60*AMinMaxFlows[LArcCount,LIndex])/1000000;
                  LFactorsTotal := LFactorsTotal + LDistribution[LArcCount,LIndex];
                end;
                for LIndex := MinMonths to MaxMonths do
                begin
                  LMonthlyDemand := (LFactorsTotal*LMonthDays[LIndex])/365.25;
                  if LMonthlyDemand <= 0 then
                    LMonthlyDemand := 1;
                  if LDistribution[LArcCount,LIndex] <= 0 then
                    LDistribution[LArcCount,LIndex] := 1;
                  LDistribution[LArcCount,LIndex] := LDistribution[LArcCount,LIndex]/LMonthlyDemand;
                end;
                LRowCount  := LRowCount + 1;
              end;
              AMinMaxFlowConstraint.PopulateDistributionFactors(LDistribution);
              FSQLAgent.AddMinMaxChannelDistributionBlockValues(AFeatureID, LRowCount,LDistribution);
            //end;
          end;
        end
        else
        begin
          while (not LDataset.DataSet.EOF) do
          begin
            LSubID := LDataset.DataSet.FieldByName('SubIdentifier').AsInteger;
            for LMonth := LMinMaxFlowField.ArrayLowDimTwo to LMinMaxFlowField.ArrayHighDimTwo do
              LDistribution[LSubID, lMonth] := LDataset.DataSet.FieldByName(Format ('Distribution%2.2d',[LMonth])).AsFloat;  //AMinMaxFlows[lSubID, lMonth];
            LDataset.DataSet.Next;
          end;
          AMinMaxFlowConstraint.PopulateDistributionFactors(LDistribution);
        end;

        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
      Finalize(LDistribution);
    end;
  Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


{******************************************************************************}
{* MinimumFlowConstraints                                                     *}
{******************************************************************************}

function TChannelDataLoadAgent.LoadMinimumFlowConstraints
                                        (AChannelList : TChannelList;
                                         AFeatureList : TMinimumFlowConstraintList): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadMinimumFlowConstraints';
var
  LDataSet         : TAbstractModelDataset;
  LIndex           : integer;
  LFeatureID       : integer;
  LChannel         : TGeneralFlowChannel;
  lResult          : Boolean;
  lNewChannel      : Boolean;
  lChannelNr       : integer;
  lConstraint      : TMinimumFlowConstraint;
  lFeatureType     : integer;
  lFeatureSubType  : integer;
  lFeatureName     : string;
  LMonthlyFlows    : TMinMonthlyDoublesArray;
  LMonthFlowsField : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LMonthFlowsField := FAppModules.FieldProperties.FieldProperty('MinFlowDemand');
    if not Assigned(LMonthFlowsField) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LMonthlyFlows,LMonthFlowsField.ArrayLength);

    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    if Assigned(LDataSet) then
    begin
      LDataSet.SetSQL(FSQLAgent.GetMinimumFlowChannelDataSQL);
      LDataset.DataSet.Open;
      while (lResult AND (NOT LDataset.DataSet.EOF)) do
      begin
        lFeatureType    := 1;
        lFeatureSubType := 0;
        lNewChannel := FALSE;
        if LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          lChannelNr   := NullInteger
        else
          lChannelNr   := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
        if LDataSet.DataSet.FieldByName('MinFlowRecordIdentifier').IsNull then
          lFeatureID   := NullInteger
        else
          lFeatureID   := LDataset.DataSet.FieldByName('MinFlowRecordIdentifier').AsInteger;
        lFeatureName := Trim(LDataset.DataSet.FieldByName('MinFlowChannelName').AsString);
        for LIndex := LMonthFlowsField.ArrayLow to LMonthFlowsField.ArrayHigh do
          LMonthlyFlows[LIndex] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).AsFloat;
        lChannel    := AChannelList.CastChannelByChannelNumber[lChannelNr];
        if (lChannel = nil) then
        begin
          lChannel    := AChannelList.NewGeneralFlowChannel;
          lChannel.Initialise;
          lNewChannel := TRUE;
          lResult     := PopulateGeneralFlowChannelData(LDataset, LChannel);
        end;
        if (lResult) then
        begin
          lConstraint := AFeatureList.NewMinimumFlowConstraint;
          lConstraint.Initialise;
          lResult := lConstraint.Populate(LFeatureID, lFeatureName, lChannel.ChannelNumber,
                       lFeatureType, lFeatureSubType, LMonthlyFlows);
          if (lResult) then
          begin
            lChannel.MinimumFlowConstraint := lConstraint;
          end
          else
          begin
            AFeatureList.DeleteMinimumFlowConstraintWithID(lFeatureID);
            if (lNewChannel) then
              AChannelList.DeleteGeneralFlowChannelWithID(LChannel.ChannelID);
          end;
        end;
        LDataset.DataSet.Next;
      end;
      LDataset.DataSet.Close;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


{******************************************************************************}
{* LossFeatures                                                               *}
{******************************************************************************}

function TChannelDataLoadAgent.LoadLossFeatures (AChannelList : TChannelList;
                                                 AFeatureList : TLossFeatureList): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadLossFeatures';
var
  LDataSet        : TAbstractModelDataset;
  LIndex          : integer;
  LFeatureID      : integer;
  LRefNode        : integer;
  lChannel        : TGeneralFlowChannel;
  lResult         : Boolean;
  lNewChannel     : Boolean;
  lChannelNr      : integer;
  lFeature        : TLossFeature;
  lFeatureType    : integer;
  lFeatureSubType : integer;
  lFeatureName    : string;
  LWaterLoss      : TLossMonthlyDoublesArray;
  LDivertedFlow   : TLossMonthlyDoublesArray;
  LMonthlyWaterLoss,
  LMonthlyDivertedFlow: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    LMonthlyWaterLoss := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthlyWaterLoss) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LWaterLoss,LMonthlyWaterLoss.ArrayLength);

    LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthlyDivertedFlow) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(LDivertedFlow,LMonthlyDivertedFlow.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    lResult := TRUE;
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetLossChannelDataSQL);
        LDataset.DataSet.Open;
        while (lResult AND (NOT LDataset.DataSet.EOF)) do
        begin
          lFeatureType    := 2;
          lNewChannel  := FALSE;
          lChannelNr   := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          lFeatureID   := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LRefNode     := LDataset.DataSet.FieldByName('Reference').AsInteger;
          lFeatureName := Trim(LDataset.DataSet.FieldByName('FeatureName').AsString);
          lFeatureSubType := LDataset.DataSet.FieldByName('LossType').AsInteger;
          lChannel     := AChannelList.CastChannelByChannelNumber[lChannelNr];
          if (NOT Assigned(lChannel)) then
          begin
            lChannel    := AChannelList.NewGeneralFlowChannel;
            lChannel.Initialise;
            lNewChannel := TRUE;
            lResult     := PopulateGeneralFlowChannelData(LDataset, LChannel);
          end;
          if (lResult) then
          begin
            for lIndex := LMonthlyWaterLoss.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
              lWaterLoss[lIndex] := NullFloat;
            for lIndex := LMonthlyDivertedFlow.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
              lDivertedFlow[lIndex] := NullFloat;
            lResult := LoadLossChannelValues(lFeatureID, LWaterLoss, LDivertedFlow);
          end;
          if (lResult) then
          begin
            lFeature := AFeatureList.NewLossFeature;
            lFeature.Initialise;
            lResult := lFeature.Populate(LFeatureID, lFeatureName, lChannel.ChannelNumber,
                         lFeatureType, lFeatureSubType, LRefNode, LWaterLoss,lDivertedFlow);
            if (lResult) then
            begin
              lChannel.LossFeature := lFeature;
            end
            else
            begin
              AFeatureList.DeleteLossFeatureWithID(lFeatureID);
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
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelDataLoadAgent.LoadLossChannelValues(AFeatureID        : integer;
                                                     var AWaterLoss    : TLossMonthlyDoublesArray;
                                                     var ADivertedFlow : TLossMonthlyDoublesArray): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadLossChannelValues';
var
  LDataSet      : TAbstractModelDataset;
  LIndex        : integer;
  LSubID        : integer;
  LMonthlyWaterLoss,
  LMonthlyDivertedFlow: TAbstractFieldProperty;
begin
  Result := False;
  try
    LMonthlyWaterLoss := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthlyWaterLoss) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(AWaterLoss,LMonthlyWaterLoss.ArrayLength);

    LMonthlyDivertedFlow := FAppModules.FieldProperties.FieldProperty('Month');
    if not Assigned(LMonthlyDivertedFlow) then
      raise Exception.Create('Field (Month) not found in field properties');
    SetLength(ADivertedFlow,LMonthlyDivertedFlow.ArrayLength);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetLossChannelValuesSQL(AFeatureID));
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.Eof) do
        begin
          LSubID := LDataset.DataSet.FieldByName('SubIdentifier').AsInteger;
          if LSubID = 0 then
          begin
            for LIndex := LMonthlyWaterLoss.ArrayLow to LMonthlyWaterLoss.ArrayHigh do
              AWaterLoss[LIndex] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).AsFloat;
          end
          else
          begin
            for LIndex := LMonthlyDivertedFlow.ArrayLow to LMonthlyDivertedFlow.ArrayHigh do
              ADivertedFlow[LIndex] := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LIndex])).AsFloat;
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

{******************************************************************************}
{* Other                                                                      *}
{******************************************************************************}

function TChannelDataLoadAgent.LoadOutputChannelData (AChannelList : TChannelList): boolean;
const OPNAME = 'TChannelDataLoadAgent.LoadOutputChannelData';
var
  LDataSet              : TAbstractModelDataset;
  LChannelNumber        : integer;
  LChannel              : TGeneralFlowChannel;
  LChannelName,
  LRequiresFirmYieldAnalysis,
  LSummaryOutputRequired: string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(AChannelList) and Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetChannelsSpecifiedForSummarySQL);
        LDataset.DataSet.Open;
        Result := True;
        while (not LDataset.DataSet.Eof) do
        begin
          LChannelNumber := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          LChannelName := Trim(LDataset.DataSet.FieldByName('ChannelName').AsString);
          LRequiresFirmYieldAnalysis := Trim(LDataset.DataSet.FieldByName('FirmYieldCalc').AsString);
          LChannel := AChannelList.CastChannelByChannelNumber[LChannelNumber];
          if Assigned(LChannel) then
          begin
            LSummaryOutputRequired := 'Y';
            LChannel.PopulateOutputChannelData(LSummaryOutputRequired,LRequiresFirmYieldAnalysis);
          end;
          Result := True;
          LDataset.DataSet.Next;
        end;
      end;
      LDataset.DataSet.Close;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TChannelDataLoadAgent.LoadChannelDetailsContextData(AContextData: TStringList;
  ARecordIdentifier, AChannelNumber,AChannelType: string);
const OPNAME = 'TChannelDataLoadAgent.LoadChannelDetailsContextData';
begin
  try
    FSQLAgent.LoadChannelDetailsContextData(AContextData, ARecordIdentifier, AChannelNumber, AChannelType);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataLoadAgent.LoadChannelContextData (AContextData   : TStringList;
                                                        AChannelID     : string;
                                                        AChannelNumber : string);
const OPNAME = 'TChannelDataLoadAgent.LoadChannelContextData';
begin
  try
    FSQLAgent.LoadChannelContextData(AContextData, AChannelID, AChannelNumber);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataLoadAgent.LoadWaterSupplyDataContextData(AContextData: TStringList; AFieldNameIdentifier: string);
const OPNAME = 'TChannelDataLoadAgent.LoadWaterSupplyDataContextData';
begin
  try
    FSQLAgent.LoadWaterSupplyDataContextData(AContextData, AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataLoadAgent.LoadContextData_FeatureIDSubIDFieldNameID
                                                     (AContextData : TStringList;
                                                      AFeatureID   : string;
                                                      ASubID       : string;
                                                      AFieldNameID : string);
const OPNAME = 'TChannelDataLoadAgent.LoadContextData_FeatureIDSubIDFieldNameID';
begin
  try
    FSQLAgent.LoadContextData_FeatureIDSubIDFieldNameID
     (AContextData, AFeatureID, ASubID, AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataLoadAgent.LoadContextData_FeatureID
                                                   (AContextData : TStringList;
                                                    AFeatureID   : string);
const OPNAME = 'TChannelDataLoadAgent.LoadContextData_FeatureID';
begin
  try
    FSQLAgent.LoadContextData_FeatureID(AContextData, AFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelDataLoadAgent.LoadContextData_FeatureIDFieldNameID
                                                   (AContextData : TStringList;
                                                    AFeatureID   : string;
                                                    AFieldNameID : string);
const OPNAME = 'TChannelDataLoadAgent.LoadContextData_FeatureIDFieldNameID';
begin
  try
    FSQLAgent.LoadContextData_FeatureIDFieldNameID(AContextData, AFeatureID, AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
