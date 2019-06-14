//
//
//  UNIT      : Contains TDataFileObjects Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDataFileObjects;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UParamObject,
  UPathsObject,
  UFileNames,
  UWRSM2000Object,
  UHydrologyFilesObject,
  URunParametersObject,
  UReservoirObject,
  UPowerChannelObject,
  UChannelDescriptionObject,
  UZoneDefinitionsObject,
  UFlowConstraintsObject,
  UReservoirHydrologyFilesObject,
  UNetworkVisualiserModelDataObject,
  UReservoirInitialLevelsObject,
  UIrrigationAreasObject,
  UChannelMinMaxObject,
  UPowerDemandObject,
  UMinimumPowerDemandsObject,
  UMinFlowChannelObject,
  UDivChannelDemandObject,
  UReleaseStructureObject,
  UDemandReconciliationObject,
  UAbstractObject,
  UMonthlyDamLevelsObject,
  UIrrigationBlockObject,
  UWetlandObject,
  UCurtailmentAndDroughtFileObject,
  UYMDemandCentreObject,
  USFRFileObject,
  UMineFileObject,
  UGroundWaterFileObject,
  UDDTSDailyDataObject;
type

  TDataFileObjects = class(TAbstractObject)
  protected

    //Network Visualiser link data
    FNetworkVisualiserModelDataObject: TNetworkVisualiserModelDataObject;
    FReservoirHydrologyFilesObject : TReservoirHydrologyFilesObject;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateReservoirs(AAllReservoirsAndNodes: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateAllReservoirs(AAllReservoirsAndNodes: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateReservoirInitialLevels(AAllReservoirsAndNodes: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateReservoirPhysicalCharacteristics(AAllReservoirsAndNodes: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateCurtailementsAndDroughtRestrictions(AAllChannels,AAllReservoirsAndNodes: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateTargetSystemYield(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;

    function ValidateChannels(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateAllChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateMasterChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidatePowerChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateIrrigationChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateDiversionChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateMinFlowChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateLossChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateMultiPurposeChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidatePumpingChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateInflowChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateDemandChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateGeneralChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateSummaryChannel(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidatePenaltyStructures(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;

  public
    //File F01.dat
    FRunParametersObject: TRunParametersObject;

    //File F02.dat
    FReservoirObject: TReservoirObject;

    //File F03.dat
    FChannelDescrObject: TChannelDescrObject;

    //File F04.dat
    FFlowConstraintsObject: TFlowConstraintsObject;

    //File F05.dat
    FZoneDefinitionsObject: TZoneDefinitionsObject;

    //File F06.dat
    FReservoirInitialLevelValuesObject: TReservoirInitialLevelValuesObject;

    //File F07.dat
    FPowerChannelObject : TPowerChannelsObject;

    //File F08.dat
    FMinimumPowerDemandsObject : TMinimumPowerDemandsObject;

    //File F09.dat
    FIrrigationAreaObject: TIrrigationAreaObject;

    //File F10.dat
//    F10Object : TStringList;
    FDivChannelDemandObject :TDivChannelDemandObject;

    //File F11.dat
    FMinFlowChannelObject : TMinFlowAndLossChannelObject;

    //File F12.dat
    FChannelMinMaxObject : TChannelMinMaxObject;

    //File F13.dat
    FPowerDemandObject : TPowerDemandObject;

    //File F14.dat
    F14Object : TStringList;
    FReleaseControlStructureObject:TReleaseControlStructureObject;


    //File F15.dat
    FCurtailmentAndDroughtFileObject: TCurtailmentAndDroughtFileObject;

    //File F16.dat
    FDemandReconciliationDataObject:TDemandReconciliationDataObject;

    //File F17.dat
    FIrrigationBlockObject: TIrrigationBlockObject;

    //File F18.dat
    FWetlandObject : TWetlandObject;

    //File F19.dat
    FYMDemandCentreObject : TYMDemandCentreObject;

    //File F20.dat
    FSFRFileObject : TSFRFileObject;

    //File F21.dat
    FMineListFileObject:TMineListFileObject;
    FMineSubCatchmentListObject:TMineSubCatchmentListFileObject;

    //File F22.dat
    FGroundWaterListFileObject: TGroundWaterListFileObject;

    //File Param.dat
    FParamObject : TParamObject;

    //File AltParam.dat
    FAltParamObject : TStringList;

    //File Wyrm.dat
    FPathsObject : TPathsObject;

    //File DWS.NET
    FWRSM2000Object: TWRSM2000Object;

    //Demand Files
    FDemandFilesObject: THydrologyFilesObject;

    //Hydrology Files
    FHydrologyFilesObject: THydrologyFilesObject;

    //Output *YLD.OUT
    FOutputYieldObject : TStringList;

    //Output *DAT.OUT
    FOutputDataObject : TStringList;

    //Output *DBG.OUT
    FOutputDebugObject : TStringList;

    //Output *PLT.OUT
    FOutputPlottingObject : TStringList;

    //Output *HYD.OUT
    FOutputHydroPowerObject : TStringList;

    // Monthly Dam Levels *DWL.TXT
    FMonthlyDamLevelsObject : TMonthlyDamLevelsObject;

    //Model DDTS data
    FDDTSDailyDataObject    : TDDTSDailyDataObject;
    property NetworkVisualiserModelDataObject: TNetworkVisualiserModelDataObject read FNetworkVisualiserModelDataObject;
    property ReservoirHydrologyFilesObject: TReservoirHydrologyFilesObject read FReservoirHydrologyFilesObject;

    procedure AddNodesWithoutInflow;
    procedure UpdateNetworkElementsType;
    procedure UpdateSpecifiedInflowChanelsFileNames;
    procedure CreateNaturalInflowChannels;

    function Initialise: boolean;override;
    procedure Reset;virtual;
    function ValidateFileData(AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ValidateDDTSFileData(AAppModules:TAppModules;AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
  end;

implementation


{ TDataFileObjects }
uses
  VoaimsCom_TLB,
  UErrorHandlingOperations;

procedure TDataFileObjects.CreateMemberObjects;
const OPNAME = 'TDataFileObjects.CreateMemberObjects';
begin
  try

    //File F01.dat
    FRunParametersObject:= TRunParametersObject.Create;

    //File F02.dat
    FReservoirObject := TReservoirObject.Create;

    //File F03.dat
    FChannelDescrObject := TChannelDescrObject.Create;

    //File F04.dat
    FFlowConstraintsObject := TFlowConstraintsObject.Create;

    //File F05.dat
    FZoneDefinitionsObject := TZoneDefinitionsObject.Create;

    //File F06.dat
    FReservoirInitialLevelValuesObject := TReservoirInitialLevelValuesObject.Create;

    //File F07.dat
    FPowerChannelObject := TPowerChannelsObject.Create;

    //File F08.dat
    FMinimumPowerDemandsObject := TMinimumPowerDemandsObject.Create;

    //File F09.dat
    FIrrigationAreaObject := TIrrigationAreaObject.Create;

    //File F10.dat
//    F10Object := TStringList.Create;
    FDivChannelDemandObject := TDivChannelDemandObject.Create;

    //File F11.dat
    FMinFlowChannelObject := TMinFlowAndLossChannelObject.Create;

    //File F12.dat
    FChannelMinMaxObject := TChannelMinMaxObject.Create;

    //File F13.dat
    FPowerDemandObject := TPowerDemandObject.Create;

    //File F14.dat
    F14Object := TStringList.Create;
    FReleaseControlStructureObject :=TReleaseControlStructureObject.Create;

    //File F15.dat
    FCurtailmentAndDroughtFileObject := TCurtailmentAndDroughtFileObject.Create;

    //File F16.dat
    FDemandReconciliationDataObject := TDemandReconciliationDataObject.Create;

    //File F17.dat
    FIrrigationBlockObject:= TIrrigationBlockObject.Create;

    //File F18.dat
    FWetlandObject := TWetlandObject.Create;

    //File F19.dat
    FYMDemandCentreObject := TYMDemandCentreObject.Create;

    //File F20.dat
    FSFRFileObject := TSFRFileObject.Create;

    //File F21.dat
    FMineListFileObject := TMineListFileObject.Create;
    FMineSubCatchmentListObject := TMineSubCatchmentListFileObject.Create;
    
    //File F22.dat
    FGroundWaterListFileObject := TGroundWaterListFileObject.Create;

    //File Param.dat
    FParamObject := TParamObject.Create;

    //File AltParam.dat
    FAltParamObject := TStringList.Create;

    //File Wyrm.dat
    FPathsObject := TPathsObject.Create;

    //File DWS.NET
    FWRSM2000Object := TWRSM2000Object.Create;

    //Hydrology File
    FDemandFilesObject:= THydrologyFilesObject.Create;

    //Hydrology File
    FHydrologyFilesObject:= THydrologyFilesObject.Create;

    //Network Visualiser link data
    FNetworkVisualiserModelDataObject := TNetworkVisualiserModelDataObject.Create;

    //Reservoir Hydrology File names link data
    FReservoirHydrologyFilesObject := TReservoirHydrologyFilesObject.Create;

    //Output *YLD.OUT
    FOutputYieldObject := TStringList.Create;

    //Output *DAT.OUT
    FOutputDataObject := TStringList.Create;

    //Output *DBG.OUT
    FOutputDebugObject := TStringList.Create;

    //Output *PLT.OUT
    FOutputPlottingObject := TStringList.Create;

    //Output *HYD.OUT
    FOutputHydroPowerObject := TStringList.Create;

    // Monthly Dam Levels *DWL.TXT
    FMonthlyDamLevelsObject := TMonthlyDamLevelsObject.Create;

    FDDTSDailyDataObject    := TDDTSDailyDataObject.Create;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TDataFileObjects.DestroyMemberObjects;
const OPNAME = 'TDataFileObjects.DestroyMemberObjects';
begin
  try

    //File F01.dat
    FRunParametersObject.Free;

    //File F02.dat
    FReservoirObject.Free;

    //File F03.dat
    FChannelDescrObject.Free;

    //File F04.dat
    FFlowConstraintsObject.Free;

    //File F05.dat
    FZoneDefinitionsObject.Free;

    //File F06.dat
    FReservoirInitialLevelValuesObject.Free;

    //File F07.dat
    FPowerChannelObject.Free;

    //File F08.dat
    FMinimumPowerDemandsObject.Free;

    //File F09.dat
    FIrrigationAreaObject.Free;

    //File F10.dat
//    F10Object.Free;
    FDivChannelDemandObject.Free;

    //File F11.dat
    FMinFlowChannelObject.Free;

    //File F12.dat
    FChannelMinMaxObject.Free;

    //File F13.dat
    FPowerDemandObject.Free;

    //File F14.dat
    F14Object.Free;
    FReleaseControlStructureObject.Free;

    //File F15.dat
    FCurtailmentAndDroughtFileObject.Free;

    //File F16.dat
    FDemandReconciliationDataObject.Free;

    //File F17.dat
    FIrrigationBlockObject.Free;

    //File F18.dat
    FWetlandObject.Free;

    //File F19.dat
    FYMDemandCentreObject.Free;

    //File F20.dat
    FSFRFileObject.Free;

    //File F21.dat
    FMineListFileObject.Free;
    FMineSubCatchmentListObject.Free;

    //File F22.dat
    FGroundWaterListFileObject.Free;

    //File Param.dat
    FParamObject.Free;

    //File Param.dat
    FAltParamObject.Free;

    //File Wyrm.dat
    FPathsObject.Free;

    //File DWS.NET
    FWRSM2000Object.Free;

    //Files hydrology
    FDemandFilesObject.Free;

    //Files hydrology
    FHydrologyFilesObject.Free;

    //Network Visualiser link data
    FNetworkVisualiserModelDataObject.Free;

    //Reservoir Hydrology File names link data
    FReservoirHydrologyFilesObject.Free;

    //Output *YLD.OUT
    FOutputYieldObject.Free;

    //Output *DAT.OUT
    FOutputDataObject.Free;

    //Output *DBG.OUT
    FOutputDebugObject.Free;

    //Output *PLT.OUT
    FOutputPlottingObject.Free;

    //Output *HYD.OUT
    FOutputHydroPowerObject.Free;

    // Monthly Dam Levels *DWL.TXT
    FMonthlyDamLevelsObject.Free;

    FDDTSDailyDataObject.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.Initialise: boolean;
const OPNAME = 'TDataFileObjects.Initialise';
begin
  Result := False;
  try
    Result := FChannelDescrObject.Initialise and
              FFlowConstraintsObject.Initialise and
              FReservoirObject.Initialise and
              FRunParametersObject.Initialise and
              FZoneDefinitionsObject.Initialise and
              FWRSM2000Object.Initialise and
              FDemandFilesObject.Initialise and
              FHydrologyFilesObject.Initialise and
              FReservoirInitialLevelValuesObject.Initialise and
              FPowerChannelObject.Initialise and
              FMinimumPowerDemandsObject.Initialise and
              FIrrigationAreaObject.Initialise and
              FDivChannelDemandObject.Initialise and
              FChannelMinMaxObject.Initialise and
              FPowerDemandObject.Initialise and
              FReleaseControlStructureObject.Initialise and
              FMinFlowChannelObject.Initialise and
              FNetworkVisualiserModelDataObject.Initialise and
              FReservoirHydrologyFilesObject.Initialise and
              FPathsObject.Initialise and
              FParamObject.Initialise and
              FDemandReconciliationDataObject.Initialise and
              FIrrigationBlockObject.Initialise and
              FWetlandObject.Initialise and
              FYMDemandCentreObject.Initialise and
              FMonthlyDamLevelsObject.Initialise and
              FSFRFileObject.Initialise and
              FMineListFileObject.Initialise and
              FCurtailmentAndDroughtFileObject.Initialise and
              FGroundWaterListFileObject.Initialise and
              FMineSubCatchmentListObject.Initialise and
              FDDTSDailyDataObject.Initialise;

    //F10Object.Clear;
    F14Object.Clear;

    //Output *YLD.OUT
    FOutputYieldObject.Clear;

    //Output *DAT.OUT
    FOutputDataObject.Clear;

    //Output *DBG.OUT
    FOutputDebugObject.Clear;

    //Output *PLT.OUT
    FOutputPlottingObject.Clear;

    //Output *HYD.OUT
    FOutputHydroPowerObject.Clear;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataFileObjects.Reset;
const OPNAME = 'TDataFileObjects.Reset';
begin
  try
    FChannelDescrObject.Reset;
    FFlowConstraintsObject.Reset;
    FReservoirObject.Reset;
    FRunParametersObject.Reset;
    FZoneDefinitionsObject.Reset;
    FWRSM2000Object.Reset;
    FDemandFilesObject.Reset;
    FHydrologyFilesObject.Reset;
    FReservoirInitialLevelValuesObject.Reset;
    FPowerChannelObject.Reset;
    FMinimumPowerDemandsObject.Reset;
    FIrrigationAreaObject.Reset;
    FDivChannelDemandObject.Reset;
    FChannelMinMaxObject.Reset;
    FPowerDemandObject.Reset;
    FReleaseControlStructureObject.Reset;
    FMinFlowChannelObject.Reset;
    FNetworkVisualiserModelDataObject.Reset;
    FReservoirHydrologyFilesObject.Reset;
    FPathsObject.Reset;
    FParamObject.Reset;
    FDemandReconciliationDataObject.Reset;
    FIrrigationBlockObject.Reset;
    FWetlandObject.Reset;
    FYMDemandCentreObject.Reset;
    FMonthlyDamLevelsObject.Reset;
    FSFRFileObject.Reset;
    FMineListFileObject.Reset;
    FMineSubCatchmentListObject.Reset;
    FCurtailmentAndDroughtFileObject.Reset;
    FGroundWaterListFileObject.Reset;
    FDDTSDailyDataObject.Reset;

    //AltParamObject.Clear;
    FAltParamObject.Clear;

    //F10Object.Clear;
    F14Object.Clear;

    //Output *YLD.OUT
    FOutputYieldObject.Clear;

    //Output *DAT.OUT
    FOutputDataObject.Clear;

    //Output *DBG.OUT
    FOutputDebugObject.Clear;

    //Output *PLT.OUT
    FOutputPlottingObject.Clear;

    //Output *HYD.OUT
    FOutputHydroPowerObject.Clear;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataFileObjects.AddNodesWithoutInflow;
const OPNAME = 'TDataFileObjects.AddNodesWithoutInflow';
var
  LCount: integer;
  LMasterChannel       :  TMasterChannelObject;
  LPowerChannel        :  TPowerChannelObject;
  LIrrigationChannel   :  TIrrigationChannelObject;
  LDiversionChannel    :  TDiversionChannelObject;
  LMinFlowChannel      :  TMinFlowChannelObject;
  LLossChannel         :  TLossChannelObject;
  LMultiPurposeChannel :  TMultiPurposeChannelObject;
  LPumpingChannel      :  TPumpingChannelObject;
  LInflowChannel       :  TInflowChannelObject;
  LDemandChannel       :  TDemandChannelObject;
  LGeneralChannel      :  TGeneralChannelObject;
  LIrrigationBlock     :  TIrrigationBlock;
  LMine                :  TMineFileObject;
  LDemandCentre        :  TYMDemandCentre;
  LGroundWater         :  TGroundWaterObject;
begin
  try
    for LCount := 0 to FChannelDescrObject.FMasterChannelList.Count -1 do
    begin
      LMasterChannel := TMasterChannelObject(FChannelDescrObject.FMasterChannelList.Items[LCount]);
      if LMasterChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMasterChannel.FUpNodeNumber.FData);
      if LMasterChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMasterChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FPowerChannelList.Count -1 do
    begin
      LPowerChannel := TPowerChannelObject(FChannelDescrObject.FPowerChannelList.Items[LCount]);
      if LPowerChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LPowerChannel.FUpNodeNumber.FData);
      if LPowerChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LPowerChannel.FDownNodeNumber.FData);

      if LPowerChannel.FSpillUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LPowerChannel.FSpillUpNodeNumber.FData);
      if LPowerChannel.FSpillDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LPowerChannel.FSpillDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FIrrigationChannelList.Count -1 do
    begin
      LIrrigationChannel := TIrrigationChannelObject(FChannelDescrObject.FIrrigationChannelList.Items[LCount]);
      if LIrrigationChannel.FUpstreamNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LIrrigationChannel.FUpstreamNodeNumber.FData);
      if LIrrigationChannel.FDownStreamNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LIrrigationChannel.FDownStreamNodeNumber.FData);

      if LIrrigationChannel.FIrrigationNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LIrrigationChannel.FIrrigationNodeNumber.FData, ntIrrigationNode);
    end;

    for LCount := 0 to FChannelDescrObject.FDiversionChannelList.Count -1 do
    begin
      LDiversionChannel := TDiversionChannelObject(FChannelDescrObject.FDiversionChannelList.Items[LCount]);
      if LDiversionChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LDiversionChannel.FUpNodeNumber.FData);
      if LDiversionChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LDiversionChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FMinFlowChannelList.Count -1 do
    begin
      LMinFlowChannel := TMinFlowChannelObject(FChannelDescrObject.FMinFlowChannelList.Items[LCount]);
      if LMinFlowChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMinFlowChannel.FUpNodeNumber.FData);
      if LMinFlowChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMinFlowChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FLossChannelList.Count -1 do
    begin
      LLossChannel := TLossChannelObject(FChannelDescrObject.FLossChannelList.Items[LCount]);
      if LLossChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LLossChannel.FUpNodeNumber.FData);
      if LLossChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LLossChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FMultiPurposeChannelList.Count -1 do
    begin
      LMultiPurposeChannel := TMultiPurposeChannelObject(FChannelDescrObject.FMultiPurposeChannelList.Items[LCount]);
      if LMultiPurposeChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMultiPurposeChannel.FUpNodeNumber.FData);
      if LMultiPurposeChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMultiPurposeChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FPumpingChannelList.Count -1 do
    begin
      LPumpingChannel := TPumpingChannelObject(FChannelDescrObject.FPumpingChannelList.Items[LCount]);
      if LPumpingChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LPumpingChannel.FUpNodeNumber.FData);
      if LPumpingChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LPumpingChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FInflowChannelList.Count -1 do
    begin
      LInflowChannel := TInflowChannelObject(FChannelDescrObject.FInflowChannelList.Items[LCount]);
      if LInflowChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LInflowChannel.FUpNodeNumber.FData);
      if LInflowChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LInflowChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FDemandChannelList.Count -1 do
    begin
      LDemandChannel := TDemandChannelObject(FChannelDescrObject.FDemandChannelList.Items[LCount]);
      if LDemandChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LDemandChannel.FUpNodeNumber.FData);
      if LDemandChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LDemandChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FChannelDescrObject.FGeneralChannelList.Count -1 do
    begin
      LGeneralChannel := TGeneralChannelObject(FChannelDescrObject.FGeneralChannelList.Items[LCount]);
      if LGeneralChannel.FUpNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LGeneralChannel.FUpNodeNumber.FData);
      if LGeneralChannel.FDownNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LGeneralChannel.FDownNodeNumber.FData);
    end;

    for LCount := 0 to FMineListFileObject.MineFileObjectCount -1 do
    begin
      LMine := FMineListFileObject.MineFileObjectByIndex[LCount];
      if LMine.NodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LMine.NodeNumber.FData,ntMineNode);
    end;

    for LCount := 0 to FIrrigationBlockObject.IrrigationBlockCount -1 do
    begin
      LIrrigationBlock := FIrrigationBlockObject.IrrigationBlockObjectByIndex[LCount];
      if LIrrigationBlock.BlockNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LIrrigationBlock.BlockNodeNumber.FData,ntIrrigationNode);
    end;

    for LCount := 0 to FYMDemandCentreObject.DemandCentreCount -1 do
    begin
      LDemandCentre := FYMDemandCentreObject.DemandCentreObjectByIndex[LCount];
      if LDemandCentre.NodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LDemandCentre.NodeNumber.FData,ntDemandCentreNode);
    end;

    for LCount := 0 to FChannelDescrObject.FGroundWaterList.Count -1 do
    begin
       LGroundWater := TGroundWaterObject(FChannelDescrObject.FGroundWaterList.Items[LCount]);
      if LGroundWater.FBaseflowNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LGroundWater.FBaseflowNodeNumber.FData,ntBaseFlowNode);
      if LGroundWater.FAbstractionNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LGroundWater.FAbstractionNodeNumber.FData,ntAbstractionNode);
      if LGroundWater.FCollectionNodeNumber.FInitalised then
        FReservoirObject.AddNodeWithoutInflow(LGroundWater.FCollectionNodeNumber.FData,ntCollectionNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataFileObjects.UpdateNetworkElementsType;
const OPNAME = 'TDataFileObjects.UpdateNetworkElementsType';
var
  LCount,
  LIndex: integer;
  LMine : TMineFileObject;
  LReservoir : TReservoir;
  LUndegroundMine : TMineUndegroundFileObject;
  LMinMaxChannel  : TMultiPurposeChannelObject;
begin
  for LCount := 0 to FMineListFileObject.MineFileObjectCount-1 do
  begin
    LMine := FMineListFileObject.MineFileObjectByIndex[LCount];
    if(LMine <> nil) and (LMine.PCDChannelNumber.FInitalised) then
    begin
      LMinMaxChannel := FChannelDescrObject.FindMultiPurposeChannel(LMine.PCDChannelNumber.FData);
      if(LMinMaxChannel <> nil) then
      begin
        LMinMaxChannel.FChannelType := ctMineToPCDChannel;
        if(LMinMaxChannel.FDownNodeNumber.FInitalised) then
        begin
          LReservoir := FReservoirObject.ReservoirByReservoirNumber(LMinMaxChannel.FDownNodeNumber.FData);
          if(LReservoir <> nil) and (LReservoir.FNodeType.FData = ntReservoir) then
          begin
            LReservoir.FNodeType.FData := ntMinePolutionControlDam;
            LReservoir.FNodeType.FInitalised := True;
          end;
        end;
      end;

      LMinMaxChannel := FChannelDescrObject.FindMultiPurposeChannel(LMine.RiverChannelNumber.FData);
      if(LMinMaxChannel <> nil) then
      begin
        LMinMaxChannel.FChannelType := ctMineToRiverDChannel;
      end;
    end;

    for LIndex := 0 to LMine.UndegroundCount-1 do
    begin
      LUndegroundMine := LMine.UndegroundByIndex[LIndex];
      if(LUndegroundMine <> nil) then
      begin
        LMinMaxChannel := FChannelDescrObject.FindMultiPurposeChannel(LUndegroundMine.ChannelNumberToUGDam.FData);
        if(LMinMaxChannel <> nil) then
        begin
          LMinMaxChannel.FChannelType := ctMineToUndergroundChannel;

          if(LMinMaxChannel.FDownNodeNumber.FInitalised) then
          begin
            LReservoir := FReservoirObject.ReservoirByReservoirNumber(LMinMaxChannel.FDownNodeNumber.FData);
            if(LReservoir <> nil) and (LReservoir.FNodeType.FData = ntReservoir) then 
            begin
              LReservoir.FNodeType.FData := ntMineUndergroundDam;
              LReservoir.FNodeType.FInitalised := True;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TDataFileObjects.ValidateFileData(AAppModules:TAppModules;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateFileData';
var
  LReservoirContainer: TStringList;
  LChannelNumber : integer;
  LAllChannelContainer : TStringList;
  LReservoir : TReservoir;
  LIndex: integer;
  LReservoirNumber: integer;
  LResult: boolean;
begin
  Result := True;
  try
    LReservoirContainer  := TStringList.Create;
    LAllChannelContainer := TStringList.Create;
    try
      LReservoirContainer.Sorted      := True;
      LReservoirContainer.Duplicates  := dupAccept;
      LAllChannelContainer.Sorted     := True;
      LAllChannelContainer.Duplicates := dupAccept;

      for LIndex := 0 to FReservoirObject.FReservoirs.Count -1 do
      begin
        LReservoir       := TReservoir(FReservoirObject.FReservoirs[LIndex]);
        LReservoirNumber := LReservoir.FNodeNo.FData;
        LReservoirContainer.AddObject(IntToStr(LReservoirNumber),LReservoir);
      end;
      for LIndex := 0 to FChannelDescrObject.FMasterChannelList.Count -1 do
      begin
        LChannelNumber := TMasterChannelObject(FChannelDescrObject.FMasterChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMasterChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FPowerChannelList.Count -1 do
      begin
        LChannelNumber := TPowerChannelObject(FChannelDescrObject.FPowerChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FPowerChannelList[LIndex]);
        LChannelNumber := TPowerChannelObject(FChannelDescrObject.FPowerChannelList[LIndex]).FSpillChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FPowerChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FIrrigationChannelList.Count -1 do
      begin
        LChannelNumber := TIrrigationChannelObject(FChannelDescrObject.FIrrigationChannelList[LIndex]).FDiversionChannelNumber .FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FIrrigationChannelList[LIndex]);
        LChannelNumber := TIrrigationChannelObject(FChannelDescrObject.FIrrigationChannelList[LIndex]).FReturnChannelNumber .FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FIrrigationChannelList[LIndex]);
        LChannelNumber := TIrrigationChannelObject(FChannelDescrObject.FIrrigationChannelList[LIndex]).FConsumptiveChannelNumber .FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FIrrigationChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FDiversionChannelList.Count -1 do
      begin
        LChannelNumber := TDiversionChannelObject(FChannelDescrObject.FDiversionChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FDiversionChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FMinFlowChannelList.Count -1 do
      begin
        LChannelNumber := TMinFlowChannelObject(FChannelDescrObject.FMinFlowChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMinFlowChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FLossChannelList.Count -1 do
      begin
        LChannelNumber := TLossChannelObject(FChannelDescrObject.FLossChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FLossChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FMultiPurposeChannelList.Count -1 do
      begin
        LChannelNumber := TMultiPurposeChannelObject(FChannelDescrObject.FMultiPurposeChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMultiPurposeChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FPumpingChannelList.Count -1 do
      begin
        LChannelNumber := TPumpingChannelObject(FChannelDescrObject.FPumpingChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FPumpingChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FInflowChannelList.Count -1 do
      begin
        LChannelNumber := TInflowChannelObject(FChannelDescrObject.FInflowChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FInflowChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FDemandChannelList.Count -1 do
      begin
        LChannelNumber := TDemandChannelObject(FChannelDescrObject.FDemandChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FDemandChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FGeneralChannelList.Count -1 do
      begin
        LChannelNumber := TGeneralChannelObject(FChannelDescrObject.FGeneralChannelList[LIndex]).FChannelNumber.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FGeneralChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FReturnFLowChannelList.Count -1 do
      begin
        LChannelNumber := TReturnFlowChannelObject(FChannelDescrObject.FReturnFLowChannelList[LIndex]).FChannelNr.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FReturnFLowChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FReclaimationChannelList.Count -1 do
      begin
        LChannelNumber := TReclaimationChannelObject(FChannelDescrObject.FReclaimationChannelList[LIndex]).FChannelNr.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FReclaimationChannelList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FIrrigationBlockList.Count -1 do
      begin
        LChannelNumber := TIrrigationBlockChannelObject(FChannelDescrObject.FIrrigationBlockList[LIndex]).FAbstractChannelNr.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FIrrigationBlockList[LIndex]);
        LChannelNumber := TIrrigationBlockChannelObject(FChannelDescrObject.FIrrigationBlockList[LIndex]).FReturnFlowChannelNr.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FIrrigationBlockList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FWetlandList.Count -1 do
      begin
        LChannelNumber := TWetlandChannelObject(FChannelDescrObject.FWetlandList[LIndex]).FInflowChannelNr.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FWetlandList[LIndex]);
      end;

      for LIndex := 0 to FChannelDescrObject.FWetlandList.Count -1 do
      begin
        LChannelNumber := TWetlandChannelObject(FChannelDescrObject.FWetlandList[LIndex]).FOutflowChannelNr.FData;
        LAllChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FWetlandList[LIndex]);
      end; 

      LResult := ValidateReservoirs(LReservoirContainer,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then Result := False;
      LResult := ValidateChannels(LAllChannelContainer,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then Result := False;
      LResult := ValidateCurtailementsAndDroughtRestrictions(LAllChannelContainer,LReservoirContainer,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then Result := False;

      LResult := ValidateTargetSystemYield(LAllChannelContainer,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then Result := False;

    finally
      LReservoirContainer.Free;
      LAllChannelContainer.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateReservoirs(AAllReservoirsAndNodes: TStrings;AAppModules:TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateReservoirs';
var
  LResult: boolean;
begin
  Result := True;
  try
      LResult := ValidateAllReservoirs(AAllReservoirsAndNodes,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then Result := False;
      if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
      LResult := ValidateReservoirPhysicalCharacteristics(AAllReservoirsAndNodes,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then Result := False;
      if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
      LResult := ValidateReservoirInitialLevels(AAllReservoirsAndNodes,AAppModules,AProgressUpdateFuntion);
      if (not LResult) then  Result := False;
      if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateAllReservoirs(AAllReservoirsAndNodes: TStrings;AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateAllReservoirs';
var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
begin
  Result := True;
  try
    //Check for duplicate reservoir numbers
    for LIndex := 0 to AAllReservoirsAndNodes.Count -2 do
    begin
      if(AAllReservoirsAndNodes[LIndex] = AAllReservoirsAndNodes[LIndex+1]) then
      begin
        Result := False;
        LMessage := AAppModules.Language.GetString('TDataFileObjects.strDuplicateReservoirNumber');
        LMessage := Format(LMessage,[AAllReservoirsAndNodes[LIndex]]);
        if Assigned(AProgressUpdateFuntion) then
          AProgressUpdateFuntion(LMessage,ptError,LStop);
        if AAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateReservoirInitialLevels(AAllReservoirsAndNodes: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateReservoirInitialLevels';
var
  LPos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LReservoir : TReservoir;
  LReservoirInitialLevel:TReservoirInitialLevelValues;
begin
  Result := True;
  try
    //Check for reservoir numbers not in F05 and F06
    for LIndex := 0 to AAllReservoirsAndNodes.Count -1 do
    begin
      LReservoir := TReservoir(AAllReservoirsAndNodes.Objects[LIndex]);
      if(LReservoir.FNodeType.FData in ReservoirsSet) then
      begin
        //Check for reservoir number in F02 with no initial water levels in F06
        LReservoirInitialLevel := FReservoirInitialLevelValuesObject.GetReservoirInitialLevel(LReservoir.FNodeNo.FData);
        if not Assigned(LReservoirInitialLevel) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strReservoirNoInitialLevel');
          LMessage := Format(LMessage,[LReservoir.FNodeNo.FData]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;
    end;

    //Check for initial water levels in F06 with no reservoir number in F02
    for LIndex := 0 to FReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines.Count -1 do
    begin
      LReservoirInitialLevel := TReservoirInitialLevelValues(FReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines[LIndex]);
      if(LReservoirInitialLevel.FReservoirNodeNumber.FData = 0) then Continue;
      LPos  := AAllReservoirsAndNodes.IndexOf(IntToStr(LReservoirInitialLevel.FReservoirNodeNumber.FData));
      LReservoir := nil;
      if(LPos >= 0) then
        LReservoir := TReservoir(AAllReservoirsAndNodes.Objects[LPos]);
      if (LPos < 0) or(Assigned(LReservoir) and (not
         (LReservoir.FNodeType.FData in ReservoirsSet))) then
      begin
        Result := False;
        LMessage := AAppModules.Language.GetString('TDataFileObjects.strInitialLevelNoReservoir');
        LMessage := Format(LMessage,[LReservoirInitialLevel.FReservoirNodeNumber.FData]);
        if Assigned(AProgressUpdateFuntion) then
          AProgressUpdateFuntion(LMessage,ptError,LStop);
        if AAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateReservoirPhysicalCharacteristics(AAllReservoirsAndNodes: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateReservoirPhysicalCharacteristics';
var
  LPos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LNode      :TNodes;
  LReservoir : TReservoir;
  LReservoirLevels:TReservoirLevels;
begin
  Result := True;
  try
    for LIndex := 0 to AAllReservoirsAndNodes.Count -1 do
    begin
      LReservoir := TReservoir(AAllReservoirsAndNodes.Objects[LIndex]);
      if(LReservoir.FNodeType.FData in ReservoirsSet) then
      begin
        //Check for reservoir number in F02 with no physical characteristics in F05
        LNode := FZoneDefinitionsObject.GetNode(LReservoir.FNodeNo.FData);
        if not Assigned(LNode) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strReservoirNoPhysical');
          LMessage := Format(LMessage,[LReservoir.FNodeNo.FData]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;

        //Check for reservoir number in F02 with no month end levels in F05
        If(FZoneDefinitionsObject.FReservoirLevelsCount.FData > 3) then
        begin
          LReservoirLevels := FZoneDefinitionsObject.GetReservoirLevel(LReservoir.FNodeNo.FData);
          if not Assigned(LReservoirLevels) then
          begin
            Result := False;
            LMessage := AAppModules.Language.GetString('TDataFileObjects.strReservoirNoLevel');
            LMessage := Format(LMessage,[LReservoir.FNodeNo.FData]);
            if Assigned(AProgressUpdateFuntion) then
              AProgressUpdateFuntion(LMessage,ptError,LStop);
            if AAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;
      end;
    end;

    //Check for physical characteristics in F05 with no reservoir number in F02
    for LIndex := 1 to FZoneDefinitionsObject.FNodes.Count -1 do
    begin
      LNode := TNodes(FZoneDefinitionsObject.FNodes[LIndex]);
      LPos  := AAllReservoirsAndNodes.IndexOf(IntToStr(LNode.FNodeNumberStorage.FData));
      LReservoir := nil;
      if(LPos >= 0) then
        LReservoir := TReservoir(AAllReservoirsAndNodes.Objects[LPos]);
      if (LPos < 0) or(Assigned(LReservoir) and (not
         (LReservoir.FNodeType.FData in ReservoirsSet))) then
      begin
        Result := False;
        LMessage := AAppModules.Language.GetString('TDataFileObjects.strPhysicalNoReservoir');
        LMessage := Format(LMessage,[LNode.FNodeNumberStorage.FData]);
        if Assigned(AProgressUpdateFuntion) then
           AProgressUpdateFuntion(LMessage,ptError,LStop);
        if AAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;

    //Check for month end levels in F05 with no reservoir number in F02
    for LIndex := 1 to FZoneDefinitionsObject.FReservoirLevels.Count -1 do
    begin
      LReservoirLevels := TReservoirLevels(FZoneDefinitionsObject.FReservoirLevels[LIndex]);
      LPos  := AAllReservoirsAndNodes.IndexOf(IntToStr(LReservoirLevels.FReservoirIdentifier.FData));
      LReservoir := nil;
      if(LPos >= 0) then
        LReservoir := TReservoir(AAllReservoirsAndNodes.Objects[LPos]);
      if (LPos < 0) or(Assigned(LReservoir) and
         (not (LReservoir.FNodeType.FData in ReservoirsSet))) then
      begin
        Result := False;
        LMessage := AAppModules.Language.GetString('TDataFileObjects.strLevelNoReservoir');
        LMessage := Format(LMessage,[LReservoirLevels.FReservoirIdentifier.FData]);
        if Assigned(AProgressUpdateFuntion) then
          AProgressUpdateFuntion(LMessage,ptError,LStop);
        if AAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateAllChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateAllChannel';
var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
begin
  Result := True;
  try
    for LIndex := 0 to AAllChannels.Count -2 do
    begin
      if(StrToInt(AAllChannels[LIndex]) = 0) then Continue;
      if(AAllChannels[LIndex] = AAllChannels[LIndex+1]) and (AAllChannels[LIndex] <> '0') then
      begin
        Result := False;
        LMessage := AAppModules.Language.GetString('TDataFileObjects.strDuplicateChannelNumber');
        LMessage := Format(LMessage,[AAllChannels[LIndex]]);
        if Assigned(AProgressUpdateFuntion) then
          AProgressUpdateFuntion(LMessage,ptError,LStop);
        if AAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateSummaryChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateSummaryChannel';
var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LSummaryChannelContainer      : TStringList;
begin
  Result := True;
  try
    LSummaryChannelContainer      := TStringList.Create;
    try

      LSummaryChannelContainer.Sorted := True;
      LSummaryChannelContainer.Duplicates := dupAccept;
      for LIndex := 0 to FChannelDescrObject.FSummaryChannelList.Count -1 do
      begin
        LChannelNumber := TSummaryChannelObject(FChannelDescrObject.FSummaryChannelList[LIndex]).FChannelNumber.FData;
        LSummaryChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FSummaryChannelList[LIndex]);
      end;

      for LIndex := 0 to LSummaryChannelContainer.Count -2 do
      begin
        if(LSummaryChannelContainer.Strings[LIndex] = LSummaryChannelContainer.Strings[LIndex+1]) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDuplicateOutputChannelNumber');
          LMessage := Format(LMessage,[LSummaryChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

      for LIndex := 0 to LSummaryChannelContainer.Count -1 do
      begin
        if(StrToInt(LSummaryChannelContainer[LIndex]) = 0) then Continue;
        if(AAllChannels.IndexOf(LSummaryChannelContainer.Strings[LIndex]) < 0) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strOutputChannelNotFound');
          LMessage := Format(LMessage,[LSummaryChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;
    finally
      LSummaryChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateMasterChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateMasterChannel';
var
  LPos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LMasterChannelContainer : TStringList;
  LMasterChannel          : TMasterChannelObject;
  LPowerDemand            : TPowerDemand;
begin
  Result := True;
  try
    LMasterChannelContainer      := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FMasterChannelList.Count -1 do
      begin
        LChannelNumber := TMasterChannelObject(FChannelDescrObject.FMasterChannelList[LIndex]).FChannelNumber.FData;
        LMasterChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMasterChannelList[LIndex]);
      end;

      for LIndex := 0 to LMasterChannelContainer.Count -1 do
      begin
        LMasterChannel := TMasterChannelObject(LMasterChannelContainer.Objects[LIndex]);
        if(LMasterChannel.FChannelNumber.FData = 0) then Continue;
        LPowerDemand   := FPowerDemandObject.GetPowerDemand(LMasterChannel.FChannelNumber.FData);
        if not Assigned(LPowerDemand) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strMasterControlNoDetails');
          LMessage := Format(LMessage,[LMasterChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

      for LIndex := 0 to FPowerDemandObject.FPowerDemandsLines.Count -1 do
      begin
        LPowerDemand   := TPowerDemand(FPowerDemandObject.FPowerDemandsLines[LIndex]);
        if(LPowerDemand.FChannelNumber.FData = 0) then Continue;
        LPos           := LMasterChannelContainer.IndexOf(IntToStr(LPowerDemand.FChannelNumber.FData));
        if(LPos < 0) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoMasterControl');
          LMessage := Format(LMessage,[IntToStr(LPowerDemand.FChannelNumber.FData)]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;
    finally
      LMasterChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidatePenaltyStructures(AAllChannels: TStrings; AAppModules: TAppModules;
                                                      AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidatePenaltyStructures';
var
  LCount               : integer;
  LMessage             : string;
  LStop                : boolean;
  LGeneralChannel      : TGeneralChannelObject;
  LPenaltyChannel      : TPenaltyChannelObject;
begin
  Result := True;
  try
    for LCount := 0 to FChannelDescrObject.FGeneralChannelList.Count -1 do
    begin
      LGeneralChannel := TGeneralChannelObject(FChannelDescrObject.FGeneralChannelList.Items[LCount]);
      if LGeneralChannel.FPenaltyStructType.FInitalised then
      begin
        LPenaltyChannel := FChannelDescrObject.FindPenaltyStructure(LGeneralChannel.FPenaltyStructType.FData);
        if LPenaltyChannel <> nil then
        begin
          if LPenaltyChannel.FArcCount.FData <> 1 then
          begin
            Result := False;
            LMessage := AAppModules.Language.GetString('TDataFileObjects.strInvalidArcCountNumber');
            LMessage := Format(LMessage,[IntToStr(LGeneralChannel.FChannelNumber.FData)]);
            if Assigned(AProgressUpdateFuntion) then
              AProgressUpdateFuntion(LMessage,ptError,LStop);
            if AAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateDDTSFileData(AAppModules: TAppModules; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateDDTSFileData';
begin
  Result := False;
  try
    Result := FDDTSDailyDataObject.ValidateDDTSFileData(AAppModules,AProgressUpdateFuntion);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateDemandChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateDemandChannel';
{var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LDemandChannel    : TDemandChannelObject;
  LDemandChannelContainer       : TStringList;}
begin
  Result := True;
  try
    {LDemandChannelContainer       := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FDemandChannelList.Count -1 do
      begin
        LChannelNumber := TDemandChannelObject(FChannelDescrObject.FDemandChannelList[LIndex]).FChannelNumber.FData;
        LDemandChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FDemandChannelList[LIndex]);
      end;

    finally
      LDemandChannelContainer.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateDiversionChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateDiversionChannel';
var
  Lpos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LDiversionChannelContainer    : TStringList;
  LDiversionData12              : TDiversionChannelData12;
  LDiversionData3               : TDiversionChannelData3;
  LDiversionChannel             : TDiversionChannelObject;
begin
  Result := True;
  try
    LDiversionChannelContainer    := TStringList.Create;
    try

      for LIndex := 0 to FChannelDescrObject.FDiversionChannelList.Count -1 do
      begin
        LChannelNumber := TDiversionChannelObject(FChannelDescrObject.FDiversionChannelList[LIndex]).FChannelNumber.FData;
        LDiversionChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FDiversionChannelList[LIndex]);
      end;

      for LIndex := 0 to LDiversionChannelContainer.Count -1 do
      begin
        LDiversionChannel := TDiversionChannelObject(LDiversionChannelContainer.Objects[LIndex]);
        if(LDiversionChannel.FChannelNumber.FData = 0) then Continue;
        LDiversionData12    := FDivChannelDemandObject.GetDiversionChanell12(LDiversionChannel.FChannelNumber.FData);
        LDiversionData3     := FDivChannelDemandObject.GetDiversionChanell3(LDiversionChannel.FChannelNumber.FData);
        if not Assigned(LDiversionData3)and not Assigned(LDiversionData12) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDiversionChannelNoDetails');
          LMessage := Format(LMessage,[LDiversionChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;
      for LIndex := 0 to FDivChannelDemandObject.DiversionChannelCount -1 do
      begin
        LDiversionData12    := FDivChannelDemandObject.DiversionChannelData12[LIndex];
        LDiversionData3     := FDivChannelDemandObject.DiversionChannelData3[LIndex];

        if Assigned(LDiversionData3) then
        begin
          if(LDiversionData3.FHeading.FDivChannelNumber.FData = 0) then Continue;

          LPos            := LDiversionChannelContainer.IndexOf(IntToStr(LDiversionData3.FHeading.FDivChannelNumber.FData));
          if(LPos < 0) then
          begin
            Result := False;
            LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoDiversionChannel');
            LMessage := Format(LMessage,[IntToStr(LDiversionData3.FHeading.FDivChannelNumber.FData)]);
            if Assigned(AProgressUpdateFuntion) then
              AProgressUpdateFuntion(LMessage,ptError,LStop);
            if AAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;

        if Assigned(LDiversionData12) then
        begin
          if(LDiversionData12.FHeading.FDivChannelNumber.FData = 0) then Continue;

          LPos            := LDiversionChannelContainer.IndexOf(IntToStr(LDiversionData12.FHeading.FDivChannelNumber.FData));
          if(LPos < 0) then
          begin
            Result := False;
            LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoDiversionChannel');
            LMessage := Format(LMessage,[IntToStr(LDiversionData12.FHeading.FDivChannelNumber.FData)]);
            if Assigned(AProgressUpdateFuntion) then
              AProgressUpdateFuntion(LMessage,ptError,LStop);
            if AAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;
      end;
    finally
      LDiversionChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateGeneralChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateGeneralChannel';
{var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LGeneralChannel   : TGeneralChannelObject;
  LGeneralChannelContainer      : TStringList;}
begin
  Result := True;
  try
    {LGeneralChannelContainer      := TStringList.Create;
    try

      for LIndex := 0 to FChannelDescrObject.FGeneralChannelList.Count -1 do
      begin
        LChannelNumber := TGeneralChannelObject(FChannelDescrObject.FGeneralChannelList[LIndex]).FChannelNumber.FData;
        LGeneralChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FGeneralChannelList[LIndex]);
      end;
    finally
      LGeneralChannelContainer.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateInflowChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateInflowChannel';
{var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LInflowChannel    : TInflowChannelObject;
  LInflowChannelContainer       : TStringList;}
begin
  Result := True;
  try
    {LInflowChannelContainer       := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FInflowChannelList.Count -1 do
      begin
        LChannelNumber := TInflowChannelObject(FChannelDescrObject.FInflowChannelList[LIndex]).FChannelNumber.FData;
        LInflowChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FInflowChannelList[LIndex]);
      end;
    finally
      LInflowChannelContainer.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateIrrigationChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateIrrigationChannel';
var
  LPos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LIrrigationArea   : TIrrigationArea;
  LIrrigationChannel: TIrrigationChannelObject;
  LIrrigationChannelContainer   : TStringList;
begin
  Result := True;
  try
    LIrrigationChannelContainer   := TStringList.Create;
    try

      for LIndex := 0 to FChannelDescrObject.FIrrigationChannelList.Count -1 do
      begin
        LChannelNumber := TIrrigationChannelObject(FChannelDescrObject.FIrrigationChannelList[LIndex]).FIrrigationNodeNumber .FData;
        LIrrigationChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FIrrigationChannelList[LIndex]);
      end;

      for LIndex := 0 to LIrrigationChannelContainer.Count -1 do
      begin
        LIrrigationChannel := TIrrigationChannelObject(LIrrigationChannelContainer.Objects[LIndex]);
        if(LIrrigationChannel.FIrrigationNodeNumber.FData = 0) then Continue;
        LIrrigationArea    := FIrrigationAreaObject.GetIrrigationArea(LIrrigationChannel.FIrrigationNodeNumber.FData);
        if not Assigned(LIrrigationArea) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strIrrigationChannelNoDetails');
          LMessage := Format(LMessage,[LIrrigationChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

      for LIndex := 0 to FIrrigationAreaObject.FIrrigationAreasLines.Count -1 do
      begin
        LIrrigationArea := TIrrigationArea(FIrrigationAreaObject.FIrrigationAreasLines[LIndex]);
        if(LIrrigationArea.FIrrigationNodeNumber.FData = 0) then Continue;
        LPos            := LIrrigationChannelContainer.IndexOf(IntToStr(LIrrigationArea.FIrrigationNodeNumber.FData));
        if(LPos < 0) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoIrrigationChannel');
          LMessage := Format(LMessage,[IntToStr(LIrrigationArea.FIrrigationNodeNumber.FData)]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;
    finally
      LIrrigationChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateLossChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateLossChannel';
var
  LPos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LLossData         : TLossChannel;
  LLossChannel      : TLossChannelObject;
  LLossChannelContainer : TStringList;
begin
  Result := True;
  try
    LLossChannelContainer   := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FLossChannelList.Count -1 do
      begin
        LChannelNumber := TLossChannelObject(FChannelDescrObject.FLossChannelList[LIndex]).FChannelNumber.FData;
        LLossChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FLossChannelList[LIndex]);
      end;

      for LIndex := 0 to LLossChannelContainer.Count -1 do
      begin
        LLossChannel   := TLossChannelObject(LLossChannelContainer.Objects[LIndex]);
        if(LLossChannel.FChannelNumber.FData = 0) then Continue;
        LLossData      := FMinFlowChannelObject.GetLossChannel(LLossChannel.FChannelNumber.FData);
        if not Assigned(LLossData) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strLossChannelNoDetails');
          LMessage := Format(LMessage,[LLossChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

      for LIndex := 0 to FMinFlowChannelObject.FLossChannelsLines.Count -1 do
      begin
        LLossData    := TLossChannel(FMinFlowChannelObject.FLossChannelsLines[LIndex]);
        if(LLossData.FLossChannelNumber.FData = 0) then Continue;
        LPos         := LLossChannelContainer.IndexOf(IntToStr(LLossData.FLossChannelNumber.FData));
        if(LPos < 0) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoLossChannel');
          LMessage := Format(LMessage,[IntToStr(LLossData.FLossChannelNumber.FData)]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;
    finally
      LLossChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateMinFlowChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateMinFlowChannel';
var
  LPos,
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LMinFlowData      : TMinFlowChannel;
  LMinFlowChannel   : TMinFlowChannelObject;
  LMinFlowChannelContainer : TStringList;
begin
  Result := True;
  try
    LMinFlowChannelContainer      := TStringList.Create;
    try

      for LIndex := 0 to FChannelDescrObject.FMinFlowChannelList.Count -1 do
      begin
        LChannelNumber := TMinFlowChannelObject(FChannelDescrObject.FMinFlowChannelList[LIndex]).FChannelNumber.FData;
        LMinFlowChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMinFlowChannelList[LIndex]);
      end;

      for LIndex := 0 to LMinFlowChannelContainer.Count -1 do
      begin
        LMinFlowChannel   := TMinFlowChannelObject(LMinFlowChannelContainer.Objects[LIndex]);
        if(LMinFlowChannel.FChannelNumber.FData = 0) then Continue;
        LMinFlowData      := FMinFlowChannelObject.GetMinFlowChannel(LMinFlowChannel.FChannelNumber.FData);
        if not Assigned(LMinFlowData) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strMinFlowChannelNoDetails');
          LMessage := Format(LMessage,[LMinFlowChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

      for LIndex := 0 to FMinFlowChannelObject.FMinFlowChannelsLines.Count -1 do
      begin
        LMinFlowData    := TMinFlowChannel(FMinFlowChannelObject.FMinFlowChannelsLines[LIndex]);
        if(LMinFlowData.FMinFlowChannelNumber.FData = 0) then Continue;
        LPos            := LMinFlowChannelContainer.IndexOf(IntToStr(LMinFlowData.FMinFlowChannelNumber.FData));
        if(LPos < 0) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoMinFlowChannel');
          LMessage := Format(LMessage,[IntToStr(LMinFlowData.FMinFlowChannelNumber.FData)]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

    finally
      LMinFlowChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateMultiPurposeChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateMultiPurposeChannel';
var
  LChannelNr,
  LIndex: integer;
  LMessage: string;
  LFound,
  LStop: boolean;
  LChannelNumber: integer;
  LChannelMinMax    : TChannelMinMax;
  LMinMaxChannel    : TMultiPurposeChannelObject;
  LMultiPurposeChannelContainer : TStringList;
begin
  Result := True;
  try
    LMultiPurposeChannelContainer := TStringList.Create;
    try

      for LIndex := 0 to FChannelDescrObject.FMultiPurposeChannelList.Count -1 do
      begin
        LChannelNumber := TMultiPurposeChannelObject(FChannelDescrObject.FMultiPurposeChannelList[LIndex]).FChannelNumber.FData;
        LMultiPurposeChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FMultiPurposeChannelList[LIndex]);
      end;

      for LIndex := 0 to LMultiPurposeChannelContainer.Count -1 do
      begin
        LMinMaxChannel   := TMultiPurposeChannelObject(LMultiPurposeChannelContainer.Objects[LIndex]);
        if(LMinMaxChannel.FChannelNumber.FData = 0) then Continue;
        LChannelMinMax   := FChannelMinMaxObject.GetMinMaxChannel(LMinMaxChannel.FChannelNumber.FData);
        if not Assigned(LChannelMinMax) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strMinMaxChannelNoDetails');
          LMessage := Format(LMessage,[LMultiPurposeChannelContainer[LIndex]]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

      for LIndex := 0 to FChannelMinMaxObject.FChannelMinMaxContainer.Count -1 do
      begin
        LChannelMinMax := TChannelMinMax(FChannelMinMaxObject.FChannelMinMaxContainer[LIndex]);
        if(LChannelMinMax.FChannelMinMaxNumber.FData = 0) then Continue;
        LChannelNr := LChannelMinMax.FChannelMinMaxNumber.FData;
        LFound     := False;
        if(FChannelDescrObject.FindMultiPurposeChannel(LChannelNr) <> nil) then
          LFound     := True;

        if(FChannelDescrObject.FindPumpingChannel(LChannelNr) <> nil) then
          LFound     := True;

        if not LFound then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strDetailsNoMinMaxChannel');
          LMessage := Format(LMessage,[IntToStr(LChannelMinMax.FChannelMinMaxNumber.FData)]);
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptError,LStop);
          if AAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;
      end;

    finally
      LMultiPurposeChannelContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidatePowerChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidatePowerChannel';
{var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LPowerChannel    : TPowerChannelObject;
  LPowerChannelContainer        : TStringList; }
begin
  Result := True;
  try
    {LPowerChannelContainer        := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FPowerChannelList.Count -1 do
      begin
        LChannelNumber := TPowerChannelObject(FChannelDescrObject.FPowerChannelList[LIndex]).FChannelNumber.FData;
        LPowerChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FPowerChannelList[LIndex]);
      end;
    finally
      LPowerChannelContainer.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidatePumpingChannel(AAllChannels: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidatePumpingChannel';
{var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
  LChannelNumber: integer;
  LPumpingChannel   : TPumpingChannelObject;
  LPumpingChannelContainer  : TStringList;}
begin


  Result := True;
  try
    {LPumpingChannelContainer        := TStringList.Create;
    try
      for LIndex := 0 to FChannelDescrObject.FPumpingChannelList.Count -1 do
      begin
        LChannelNumber := TPumpingChannelObject(FChannelDescrObject.FPumpingChannelList[LIndex]).FChannelNumber.FData;
        LPumpingChannelContainer.AddObject(IntToStr(LChannelNumber),FChannelDescrObject.FPumpingChannelList[LIndex]);
      end;
    finally
      LPumpingChannelContainer.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateChannels(AAllChannels: TStrings;AAppModules:TAppModules;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateChannels';
var
  LResult: boolean;
begin
  Result := True;
  try

    LResult := ValidatePenaltyStructures(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateAllChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateMasterChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidatePowerChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateIrrigationChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateDiversionChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateMinFlowChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateLossChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateMultiPurposeChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidatePumpingChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateInflowChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateDemandChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateGeneralChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
    LResult := ValidateSummaryChannel(AAllChannels,AAppModules,AProgressUpdateFuntion);
    if (not LResult) then  Result := False;
    if ((not Result) and AAppModules.GlobalData.StopOnFirstErr) then Exit ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateCurtailementsAndDroughtRestrictions(AAllChannels, AAllReservoirsAndNodes: TStrings; AAppModules: TAppModules;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateCurtailementsAndDroughtRestrictions';
var
  LCurtailedChannelNumber    : string;
  LCurtailedChannel          : TCurtailedChannelFileObject;
  LDroughtRestriction        : TDroughtRestrictionFileObject;
  LCount                     : integer;
  LIndex                     : integer;
  LMessage                   : string;
  LStop                      : boolean;
  LRestrictedChannels        : TStringList;
  LRestrictedReservoirs      : TStringList;
begin
  Result := True;
  try
    for LIndex := 0 to FCurtailmentAndDroughtFileObject.CurtailedChannelCount -1 do
    begin
      LCurtailedChannel       := FCurtailmentAndDroughtFileObject.CurtailedChannelByIndex[LIndex];
      LCurtailedChannelNumber := IntToStr(LCurtailedChannel.FChannelNumber.FData);
      if(AAllChannels.IndexOf(LCurtailedChannelNumber) < 0) then
      begin
        Result := False;
        LMessage := AAppModules.Language.GetString('TDataFileObjects.strCurtailedChannelNumberNoExist');
        LMessage := Format(LMessage,[LCurtailedChannelNumber]);
        if Assigned(AProgressUpdateFuntion) then
          AProgressUpdateFuntion(LMessage,ptError,LStop);
        if AAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;

    LRestrictedChannels        := TStringList.Create;
    LRestrictedReservoirs      := TStringList.Create;
    try
      for LIndex := 0 to FCurtailmentAndDroughtFileObject.DroughtRestrictionCount -1 do
      begin
        LDroughtRestriction             := FCurtailmentAndDroughtFileObject.DroughtRestrictionByIndex[LIndex];
        LRestrictedChannels.CommaText   := LDroughtRestriction.FChannelNumbers.FData;
        for LCount := 0 to LRestrictedChannels.Count-1 do
        begin
          if(AAllChannels.IndexOf(LRestrictedChannels[LCount]) < 0) then
          begin
            Result := False;
            LMessage := AAppModules.Language.GetString('TDataFileObjects.strRestrictedChannelNumberNoExist');
            LMessage := Format(LMessage,[LRestrictedChannels[LCount]]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if AAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;

        LRestrictedReservoirs.CommaText := LDroughtRestriction.FReservoirNumbers.FData;
        for LCount := 0 to LRestrictedReservoirs.Count-1 do
        begin
          if(AAllReservoirsAndNodes.IndexOf(LRestrictedReservoirs[LCount]) < 0) then
          begin
            Result := False;
            LMessage := AAppModules.Language.GetString('TDataFileObjects.strRestrictedReservoirNumberNoExist');
            LMessage := Format(LMessage,[LRestrictedReservoirs[LCount]]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            if AAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
        end;
      end;
    finally
      LRestrictedChannels.Free;
      LRestrictedReservoirs.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataFileObjects.ValidateTargetSystemYield(AAllChannels: TStrings;
                                           AAppModules: TAppModules;
                                           AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataFileObjects.ValidateTargetSystemYield';
var
  LNrOfLoadCases    : integer;
  LCount            : integer;
  LMessage          : string;
  LStop             : boolean;
  LTargetYield      : double;
  LMaximunYield     : double;
begin
  Result := True;
  try
    LNrOfLoadCases := FRunParametersObject.FNoLoadCases.FData;
    if LNrOfLoadCases > 0 then
    begin
      for LCount := 1 to LNrOfLoadCases do
      begin
        LTargetYield  := FRunParametersObject.FTargetYield[LCount].FData;
        LMaximunYield := FRunParametersObject.FMaxYield[LCount].FData;
        if(LTargetYield > LMaximunYield) then
        begin
          Result := False;
          LMessage := AAppModules.Language.GetString('TDataFileObjects.strTargetSystemYield');
          if Assigned(AProgressUpdateFuntion) then
            AProgressUpdateFuntion(LMessage,ptWarning,LStop);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataFileObjects.UpdateSpecifiedInflowChanelsFileNames;
const OPNAME = 'TDataFileObjects.ValidateTargetSystemYield';
var
  LCount          : integer;
  LInflowChannel  : TInflowChannelObject;
  LReservoir      : TReservoir;
  LCatchRef       : integer;
  LGauge          : TGaugeStochastics;
  LInflowFileName : string;
begin
  try
    for LCount := 0 to FChannelDescrObject.FInflowChannelList.Count -1 do
    begin
      LInflowChannel := TInflowChannelObject(FChannelDescrObject.FInflowChannelList.Items[LCount]);
      LReservoir     := nil;
      if LInflowChannel.FDownNodeNumber.FInitalised  and (LInflowChannel.FDownNodeNumber.FData > 0)then
        LReservoir :=  FReservoirObject.ReservoirByReservoirNumber(LInflowChannel.FDownNodeNumber.FData);
      if LInflowChannel.FUpNodeNumber.FInitalised and (LInflowChannel.FUpNodeNumber.FData > 0)then
        LReservoir :=  FReservoirObject.ReservoirByReservoirNumber(LInflowChannel.FUpNodeNumber.FData);
      if(LReservoir <> nil) then
      begin
        if LReservoir.FCatchRef.FInitalised then
        begin
          LCatchRef := LReservoir.FCatchRef.FData;
          LGauge := FParamObject.GaugeStochasticsContainer.GaugeStochasticsByIndex[LCatchRef-1];
          if(LGauge <> nil) then
          begin
            if LGauge.GaugePathName.FInitalised then
            begin
              if FRunParametersObject.FHistStoch.FInitalised then
                LInflowFileName := Trim(LGauge.GaugePathName.FData) + FRunParametersObject.FHistStoch.FData + '.inf'
              else
                LInflowFileName := Trim(LGauge.GaugePathName.FData) + '.inf';
              if FileExists(LInflowFileName) then
              begin
                 LInflowChannel.FInflowFileName.FData := ExtractFilename(LInflowFileName);
                 LInflowChannel.FInflowFileName.FInitalised := True;
                 LInflowChannel.FInflowFileName.FLength := Length(LInflowChannel.FInflowFileName.FData);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataFileObjects.CreateNaturalInflowChannels;
const OPNAME = 'TDataFileObjects.CreateNaturalInflowChannels';
var
  LCount                       : integer;
  LNaturalInflowChannel        : TNaturalInflowChannelObject;
  LReservoir                   : TReservoir;
  LNaturalInflowChannelNo      : integer;
begin
  try
    if FChannelDescrObject = nil then Exit;
    for LCount := 0 to FReservoirObject.FReservoirs.Count-1 do
    begin
      LReservoir :=  TReservoir(FReservoirObject.FReservoirs[LCount]);
      if LReservoir <> nil then
      begin

        if LReservoir.FNaturalInflowChannel.FInitalised then
        begin

          LNaturalInflowChannelNo := LReservoir.FNaturalInflowChannel.FData;

          LNaturalInflowChannel := FChannelDescrObject.AddNaturalInflowChannels;
          LNaturalInflowChannel.FChannelNumber.FData  := LNaturalInflowChannelNo;
          LNaturalInflowChannel.FChannelNumber.FInitalised := True;

          LNaturalInflowChannel.FChannelName.FData := 'Channel '+ IntToStr(LNaturalInflowChannelNo);
          LNaturalInflowChannel.FChannelName.FInitalised := True;
          LNaturalInflowChannel.FChannelName.FLength := Length(LNaturalInflowChannel.FChannelName.FData);

          LNaturalInflowChannel.FDownNodeNumber.FData := LReservoir.FNodeNo.FData;
          LNaturalInflowChannel.FDownNodeNumber.FInitalised := True;

          if  FChannelDescrObject.FInflowPenaltyNo.FInitalised then
          begin
            LNaturalInflowChannel.FPenaltyStructType.FData := FChannelDescrObject.FInflowPenaltyNo.FData;
            LNaturalInflowChannel.FPenaltyStructType.FInitalised := True;
          end;
          FChannelDescrObject.FNaturalInflowChannelCount.FData := FChannelDescrObject.FNaturalInflowChannelCount.FData +1;
          FChannelDescrObject.FNaturalInflowChannelCount.FInitalised := True;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


