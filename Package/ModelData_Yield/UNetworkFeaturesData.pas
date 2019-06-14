{******************************************************************************}
{*  UNIT      : Contains the class TNetworkFeaturesData.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UNetworkFeaturesData;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB,
  UMinimumFlowConstraints,
  ULossFeatures,
  USpecifiedDemandFeatures,
  UMinMaxFlowConstraints,
  UPumpingFeatures,
  UDiversionFeatures,
  UPhysicalFlowConstraints,
  UIFRFeatures,
  UIFRDataObject,
  UIrrigationAreas,
  USpecifiedInflowFeatures,
  UPowerPlants,
  UMasterControlFeatures,
  UChannelAreas,
  UWaterDemandFeatures,
  UIrrigationBlock,
  UWetland,
  UYMDemandCentre,
  UStreamFlowReduction,
  UMiningData,
  UCurtailmentAndDrought,
  UGroundWater;

type

  TNetworkFeaturesData = class(TAbstractAppObject, INetworkFeaturesData)
  private
  protected
    FMinimumFlowConstraintList     : TMinimumFlowConstraintList;
    FLossFeatureList               : TLossFeatureList;
    FSpecifiedDemandFeatureList    : TSpecifiedDemandFeatureList;
    FMinMaxFlowConstraintList      : TMinMaxFlowConstraintList;
    FPumpingFeatureList            : TPumpingFeatureList;
    FDiversionFeatureList          : TDiversionFeatureList;
    FPhysicalFlowConstraintList    : TPhysicalFlowConstraintList;
    FIFRFeatureList                : TIFRFeatureList;
    FIrrigationAreaList            : TIrrigationAreaList;
    FPowerPlantList                : TPowerPlantList;
    FSpecifiedInflowFeatureList    : TSpecifiedInflowFeatureList;
    FMasterControlFeatureList      : TMasterControlFeatureList;
    FWaterDemandFeatureList        : TWaterDemandFeatureList;
    FWaterDemandConfiguration      : TWaterDemandConfiguration;
    FChannelAreaList               : TChannelAreaList;
    FIrrigationBlockList           : TIrrigationBlockList;
    FWetlandList                   : TWetlandList;
    FYMDemandCentreList            : TYMDemandCentreList;
    FStreamFlowReductionList       : TStreamFlowReductionList;
    FIFRSiteList                   : TIFRSiteDataList;
    FMineList                      : TMineList;
    FCurtailmentAndDrought         : TCurtailmentAndDrought;
    FGroundWaterList               : TGroundWaterList;
    FMineSubCatchmentList          : TMineSubCatchmentList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    function Get_MinimumFlowConstraintList  : IMinimumFlowConstraintList; safecall;
    function Get_LossFeatureList            : ILossFeatureList; safecall;
    function Get_SpecifiedDemandFeatureList : ISpecifiedDemandFeatureList; safecall;
    function Get_MinMaxFlowConstraintList   : IMinMaxFlowConstraintList; safecall;
    function Get_PumpingFeatureList         : IPumpingFeatureList; safecall;
    function Get_DiversionFeatureList       : IDiversionFeatureList; safecall;
    function Get_PhysicalFlowConstraintList : IPhysicalFlowConstraintList; safecall;
    function Get_IFRFeatureList             : IIFRFeatureList; safecall;
    function Get_IrrigationAreaList         : IIrrigationAreaList; safecall;
    function Get_PowerPlantList             : IPowerPlantList; safecall;
    function Get_SpecifiedInflowFeatureList : ISpecifiedInflowFeatureList; safecall;
    function Get_MasterControlFeatureList   : IMasterControlFeatureList; safecall;
    function Get_WaterDemandFeatureList     : IWaterDemandFeatureList; safecall;
    function Get_WaterDemandConfiguration   : IWaterDemandConfiguration; safecall;
    function Get_ChannelAreaList            : IChannelAreaList; safecall;
    function Get_IrrigationBlockList        : IIrrigationBlockList; safecall;
    function Get_WetlandList                : IWetlandList; safecall;
    function Get_YMDemandCentreList         : IYMDemandCentreList; safecall;
    function Get_StreamFlowReductionList    : IStreamFlowReductionList; safecall;
    function Get_IFRSiteList                : TIFRSiteDataList; safecall;
    function Get_MineList                   : IMineList; safecall;
    function Get_CurtailmentAndDrought      : ICurtailmentAndDrought; safecall;
    function Get_GroundWaterList            : IGroundWaterList; safecall;
    function Get_MineSubCatchmentList       : IMineSubCatchmentList; safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property MinimumFlowConstraintList: IMinimumFlowConstraintList read Get_MinimumFlowConstraintList;
    property LossFeatureList: ILossFeatureList read Get_LossFeatureList;
    property SpecifiedDemandFeatureList: ISpecifiedDemandFeatureList read Get_SpecifiedDemandFeatureList;
    property MinMaxFlowConstraintList: IMinMaxFlowConstraintList read Get_MinMaxFlowConstraintList;
    property PumpingFeatureList: IPumpingFeatureList read Get_PumpingFeatureList;
    property DiversionFeatureList: IDiversionFeatureList read Get_DiversionFeatureList;
    property PhysicalFlowConstraintList: IPhysicalFlowConstraintList read Get_PhysicalFlowConstraintList;
    property IFRFeatureList: IIFRFeatureList read Get_IFRFeatureList;
    property IrrigationAreaList: IIrrigationAreaList read Get_IrrigationAreaList;
    property PowerPlantList: IPowerPlantList read Get_PowerPlantList;
    property MasterControlFeatureList: IMasterControlFeatureList read Get_MasterControlFeatureList;
    property SpecifiedInflowFeatureList: ISpecifiedInflowFeatureList read Get_SpecifiedInflowFeatureList;
    property WaterDemandFeatureList: IWaterDemandFeatureList read Get_WaterDemandFeatureList;
    property WaterDemandConfiguration: IWaterDemandConfiguration read Get_WaterDemandConfiguration;
    property ChannelAreaList : IChannelAreaList read Get_ChannelAreaList;
    property IrrigationBlockList : IIrrigationBlockList read Get_IrrigationBlockList;
    property WetlandList : IWetlandList read Get_WetlandList;
    property YMDemandCentreList: IYMDemandCentreList read Get_YMDemandCentreList;
    property IFRSiteList       : TIFRSiteDataList  read Get_IFRSiteList;
    property MineList          : IMineList  read Get_MineList;
    property CurtailmentAndDrought: ICurtailmentAndDrought read Get_CurtailmentAndDrought;
    property MineSubCatchmentList: IMineSubCatchmentList read Get_MineSubCatchmentList;


    property CastMinimumFlowConstraintList  : TMinimumFlowConstraintList  read FMinimumFlowConstraintList;
    property CastLossFeatureList            : TLossFeatureList            read FLossFeatureList;
    property CastSpecifiedDemandFeatureList : TSpecifiedDemandFeatureList read FSpecifiedDemandFeatureList;
    property CastMinMaxFlowConstraintList   : TMinMaxFlowConstraintList   read FMinMaxFlowConstraintList;
    property CastPumpingFeatureList         : TPumpingFeatureList         read FPumpingFeatureList;
    property CastDiversionFeatureList       : TDiversionFeatureList       read FDiversionFeatureList;
    property CastPhysicalFlowConstraintList : TPhysicalFlowConstraintList read FPhysicalFlowConstraintList;
    property CastIFRFeatureList             : TIFRFeatureList             read FIFRFeatureList;
    property CastIrrigationAreaList         : TIrrigationAreaList         read FIrrigationAreaList;
    property CastIrrigationBlockList        : TIrrigationBlockList        read FIrrigationBlockList;
    property CastPowerPlantList             : TPowerPlantList             read FPowerPlantList;
    property CastSpecifiedInflowDataList    : TSpecifiedInflowFeatureList read FSpecifiedInflowFeatureList;
    property CastMasterControlFeatureList   : TMasterControlFeatureList   read FMasterControlFeatureList;
    property CastWaterDemandFeatureList     : TWaterDemandFeatureList     read FWaterDemandFeatureList;
    property CastWaterDemandConfiguration   : TWaterDemandConfiguration   read FWaterDemandConfiguration;
    property CastChannelAreaList            : TChannelAreaList            read FChannelAreaList;
    property CastWetlandList                : TWetlandList                read FWetlandList;
    property CastYMDemandCentreList         : TYMDemandCentreList         read FYMDemandCentreList;
    property CastStreamFlowReductionList    : TStreamFlowReductionList    read FStreamFlowReductionList;
    property CastIFRSiteList                : TIFRSiteDataList            read FIFRSiteList;
    property CastMineList                   : TMineList                   read FMineList;
    property CastCurtailmentAndDrought      : TCurtailmentAndDrought      read FCurtailmentAndDrought;
    property CastGroundWaterList            : TGroundWaterList            read FGroundWaterList;
    property CastMineSubCatchmentList       : TMineSubCatchmentList       read FMineSubCatchmentList;
 end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{ TNetworkFeaturesData                                                         }
{******************************************************************************}

procedure TNetworkFeaturesData.CreateMemberObjects;
const OPNAME = 'TNetworkFeaturesData.CreateMemberObjects';
begin
  inherited;
  try
    FMinimumFlowConstraintList     := TMinimumFlowConstraintList.Create(FAppModules);
    FLossFeatureList               := TLossFeatureList.Create(FAppModules);
    FSpecifiedDemandFeatureList    := TSpecifiedDemandFeatureList.Create(FAppModules);
    FMinMaxFlowConstraintList      := TMinMaxFlowConstraintList.Create(FAppModules);
    FPumpingFeatureList            := TPumpingFeatureList.Create(FAppModules);
    FDiversionFeatureList          := TDiversionFeatureList.Create(FAppModules);
    FPhysicalFlowConstraintList    := TPhysicalFlowConstraintList.Create(FAppModules);
    FIFRFeatureList                := TIFRFeatureList.Create(FAppModules);
    FIrrigationAreaList            := TIrrigationAreaList.Create(FAppModules);
    FPowerPlantList                := TPowerPlantList.Create(FAppModules);
    FSpecifiedInflowFeatureList    := TSpecifiedInflowFeatureList.Create(FAppModules);
    FMasterControlFeatureList      := TMasterControlFeatureList.Create(FAppModules);
    FWaterDemandFeatureList        := TWaterDemandFeatureList.Create(FAppModules);
    FWaterDemandConfiguration      := TWaterDemandConfiguration.Create(FAppModules);
    FChannelAreaList               := TChannelAreaList.Create(FAppModules);
    FIrrigationBlockList           := TIrrigationBlockList.Create(FAppModules);
    FWetlandList                   := TWetlandList.Create(FAppModules);
    FYMDemandCentreList            := TYMDemandCentreList.Create(FAppModules);
    FStreamFlowReductionList       := TStreamFlowReductionList.Create(FAppModules);
    FIFRSiteList                   := TIFRSiteDataList.Create(FAppModules);
    FMineList                      := TMineList.Create(FAppModules);
    FCurtailmentAndDrought         := TCurtailmentAndDrought.Create(FAppModules);
    FGroundWaterList               := TGroundWaterList.Create(FAppModules);
    FMineSubCatchmentList          := TMineSubCatchmentList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkFeaturesData.DestroyMemberObjects;
const OPNAME = 'TNetworkFeaturesData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FMinimumFlowConstraintList);
    FreeAndNil(FLossFeatureList);
    FreeAndNil(FSpecifiedDemandFeatureList);
    FreeAndNil(FMinMaxFlowConstraintList);
    FreeAndNil(FPumpingFeatureList);
    FreeAndNil(FDiversionFeatureList);
    FreeAndNil(FPhysicalFlowConstraintList);
    FreeAndNil(FIFRFeatureList);
    FreeAndNil(FIrrigationAreaList);
    FreeAndNil(FPowerPlantList);
    FreeAndNil(FSpecifiedInflowFeatureList);
    FreeAndNil(FMasterControlFeatureList);
    FreeAndNil(FWaterDemandFeatureList);
    FreeAndNil(FWaterDemandConfiguration);
    FreeAndNil(FChannelAreaList);
    FreeAndNil(FIrrigationBlockList);
    FreeAndNil(FWetlandList);
    FreeAndNil(FYMDemandCentreList);
    FreeAndNil(FStreamFlowReductionList);
    FreeAndNil(FIFRSiteList);
    FreeAndNil(FMineList);
    FreeAndNil(FCurtailmentAndDrought);
    FreeAndNil(FGroundWaterList);
    FreeAndNil(FMineSubCatchmentList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData._AddRef: Integer;
const OPNAME = 'TNetworkFeaturesData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData._Release: Integer;
const OPNAME = 'TNetworkFeaturesData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Initialise: boolean;
const OPNAME = 'TNetworkFeaturesData.Initialise';
begin
  Result := FALSE;
  try
    Result := FMinimumFlowConstraintList.Initialise  AND
              FLossFeatureList.Initialise            AND
              FSpecifiedDemandFeatureList.Initialise AND
              FMinMaxFlowConstraintList.Initialise   AND
              FPumpingFeatureList.Initialise         AND
              FDiversionFeatureList.Initialise       AND
              FPhysicalFlowConstraintList.Initialise AND
              FIFRFeatureList.Initialise             AND
              FIrrigationAreaList.Initialise         AND
              FPowerPlantList.Initialise             AND
              FSpecifiedInflowFeatureList.Initialise AND
              FMasterControlFeatureList.Initialise   AND
              FWaterDemandFeatureList.Initialise     AND
              FWaterDemandConfiguration.Initialise   AND
              FChannelAreaList.Initialise            AND
              FWetlandList.Initialise                AND
              FYMDemandCentreList.Initialise         AND
              FIrrigationBlockList.Initialise        AND
              FStreamFlowReductionList.Initialise    AND
              FIFRSiteList.Initialise                AND
              FMineList.Initialise                   AND
              FCurtailmentAndDrought.Initialise      AND
              FGroundWaterList.Initialise            AND
              FMineSubCatchmentList.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkFeaturesData.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FMinimumFlowConstraintList.LanguageHasChanged  AND
              FLossFeatureList.LanguageHasChanged            AND
              FSpecifiedDemandFeatureList.LanguageHasChanged AND
              FMinMaxFlowConstraintList.LanguageHasChanged   AND
              FPumpingFeatureList.LanguageHasChanged         AND
              FDiversionFeatureList.LanguageHasChanged       AND
              FPhysicalFlowConstraintList.LanguageHasChanged AND
              FIFRFeatureList.LanguageHasChanged             AND
              FIrrigationAreaList.LanguageHasChanged         AND
              FPowerPlantList.LanguageHasChanged             AND
              FSpecifiedInflowFeatureList.LanguageHasChanged AND
              FMasterControlFeatureList.LanguageHasChanged   AND
              FWaterDemandFeatureList.LanguageHasChanged     AND
              FWaterDemandConfiguration.LanguageHasChanged   AND
              FChannelAreaList.LanguageHasChanged            AND
              FWetLandList.LanguageHasChanged                AND
              FYMDemandCentreList.LanguageHasChanged         AND
              FIrrigationBlockList.LanguageHasChanged        AND
              FStreamFlowReductionList.LanguageHasChanged    AND
              FIFRSiteList.LanguageHasChanged                AND
              FMineList.LanguageHasChanged                   AND
              FCurtailmentAndDrought.LanguageHasChanged      AND
              FGroundWaterList.LanguageHasChanged            AND
              FMineSubCatchmentList.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesData.SaveState: boolean;
const OPNAME = 'TNetworkFeaturesData.SaveState';
begin
  Result := False;
  try
    Result := FMinimumFlowConstraintList.SaveState  AND
              FLossFeatureList.SaveState            AND
              FSpecifiedDemandFeatureList.SaveState AND
              FMinMaxFlowConstraintList.SaveState   AND
              FPumpingFeatureList.SaveState         AND
              FDiversionFeatureList.SaveState       AND
              FPhysicalFlowConstraintList.SaveState AND
              FIFRFeatureList.SaveState             AND
              FIrrigationAreaList.SaveState         AND
              FPowerPlantList.SaveState             AND
              FSpecifiedInflowFeatureList.SaveState AND
              FMasterControlFeatureList.SaveState   AND
              FWaterDemandFeatureList.SaveState     AND
              FWaterDemandConfiguration.SaveState   AND
              FChannelAreaList.SaveState            AND
              FWetlandList.SaveState                AND
              FYMDemandCentreList.SaveState         AND
              FIrrigationBlockList.SaveState        AND
              FStreamFlowReductionList.SaveState    AND
              FIFRSiteList.SaveState                AND
              FMineList.SaveState                   AND
              FCurtailmentAndDrought.SaveState      AND
              FGroundWaterList.SaveState            AND
              FMineSubCatchmentList.SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesData.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TNetworkFeaturesData.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := FMinimumFlowConstraintList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)  AND
              FLossFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)            AND
              FSpecifiedDemandFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) AND
              FMinMaxFlowConstraintList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)   AND
              FPumpingFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)         AND
              FDiversionFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)       AND
              FPhysicalFlowConstraintList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) AND
              FIFRFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)             AND
              FIrrigationAreaList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)         AND
              FPowerPlantList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)             AND
              FSpecifiedInflowFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) AND
              FMasterControlFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)   AND
              FWaterDemandFeatureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)     AND
              FWaterDemandConfiguration.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)   AND
              FChannelAreaList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)            AND
              FWetlandList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)                AND
              FYMDemandCentreList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)         AND
              FIrrigationBlockList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)        AND
              FStreamFlowReductionList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)    AND
              FIFRSiteList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)                AND
              FMineList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)                   AND
              FCurtailmentAndDrought.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue)      AND
              FGroundWaterList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
              FMineSubCatchmentList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesData.StudyHasChanged: boolean;
const OPNAME = 'TNetworkFeaturesData.StudyHasChanged';
begin
  Result := False;
  try
    Result := FMinimumFlowConstraintList.StudyHasChanged  AND
              FLossFeatureList.StudyHasChanged            AND
              FSpecifiedDemandFeatureList.StudyHasChanged AND
              FMinMaxFlowConstraintList.StudyHasChanged   AND
              FPumpingFeatureList.StudyHasChanged         AND
              FDiversionFeatureList.StudyHasChanged       AND
              FPhysicalFlowConstraintList.StudyHasChanged AND
              FIFRFeatureList.StudyHasChanged             AND
              FIrrigationAreaList.StudyHasChanged         AND
              FPowerPlantList.StudyHasChanged             AND
              FSpecifiedInflowFeatureList.StudyHasChanged AND
              FMasterControlFeatureList.StudyHasChanged   AND
              FWaterDemandFeatureList.StudyHasChanged     AND
              FWaterDemandConfiguration.StudyHasChanged   AND
              FChannelAreaList.StudyHasChanged            AND
              FWetlandList.StudyHasChanged                AND
              FYMDemandCentreList.StudyHasChanged         AND
              FIrrigationBlockList.StudyHasChanged        AND
              FStreamFlowReductionList.StudyHasChanged    AND
              FIFRSiteList.StudyHasChanged                AND
              FMineList.StudyHasChanged                   AND
              FCurtailmentAndDrought.StudyHasChanged      AND
              FGroundWaterList.StudyHasChanged            AND
              FMineSubCatchmentList.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkFeaturesData.Get_MinimumFlowConstraintList: IMinimumFlowConstraintList;
const OPNAME = 'TNetworkFeaturesData.Get_MinimumFlowConstraintList';
begin
  Result := nil;
  try
    Result := FMinimumFlowConstraintList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_LossFeatureList: ILossFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_LossFeatureList';
begin
  Result := nil;
  try
    Result := FLossFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_SpecifiedDemandFeatureList: ISpecifiedDemandFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_SpecifiedDemandFeatureList';
begin
  Result := nil;
  try
    Result := FSpecifiedDemandFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_MinMaxFlowConstraintList: IMinMaxFlowConstraintList;
const OPNAME = 'TNetworkFeaturesData.Get_MinMaxFlowConstraintList';
begin
  Result := nil;
  try
    Result := FMinMaxFlowConstraintList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_PumpingFeatureList: IPumpingFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_PumpingFeatureList';
begin
  Result := nil;
  try
    Result := FPumpingFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_DiversionFeatureList: IDiversionFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_DiversionFeatureList';
begin
  Result := nil;
  try
    Result := FDiversionFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_PhysicalFlowConstraintList : IPhysicalFlowConstraintList;
const OPNAME = 'TNetworkFeaturesData.Get_PhysicalFlowConstraintList';
begin
  Result := nil;
  try
    Result := FPhysicalFlowConstraintList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_IFRFeatureList : IIFRFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_IFRFeatureList';
begin
  Result := nil;
  try
    Result := FIFRFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_IrrigationAreaList : IIrrigationAreaList;
const OPNAME = 'TNetworkFeaturesData.Get_IrrigationAreaList';
begin
  Result := nil;
  try
    Result := FIrrigationAreaList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_PowerPlantList: IPowerPlantList;
const OPNAME = 'TNetworkFeaturesData.Get_PowerPlantList';
begin
  Result := nil;
  try
    Result := FPowerPlantList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_SpecifiedInflowFeatureList : ISpecifiedInflowFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_SpecifiedInflowFeatureList';
begin
  Result := nil;
  try
    Result := FSpecifiedInflowFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_MasterControlFeatureList: IMasterControlFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_MasterControlFeatureList';
begin
  Result := nil;
  try
    Result := FMasterControlFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_WaterDemandFeatureList: IWaterDemandFeatureList;
const OPNAME = 'TNetworkFeaturesData.Get_WaterDemandFeatureList';
begin
  Result := nil;
  try
    Result := FWaterDemandFeatureList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_WaterDemandConfiguration: IWaterDemandConfiguration;
const OPNAME = 'TNetworkFeaturesData.Get_WaterDemandConfiguration';
begin
  Result := nil;
  try
    Result := FWaterDemandConfiguration;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Validate(var AErrors: WideString;  const AContext: WideString): WordBool;
const OPNAME = 'TNetworkFeaturesData.Validate';
begin
  Result := True;
  try
    if not FMinimumFlowConstraintList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FLossFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FSpecifiedDemandFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FMinMaxFlowConstraintList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FPumpingFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FDiversionFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FPhysicalFlowConstraintList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FIFRFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FIrrigationAreaList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FIrrigationBlockList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FPowerPlantList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FSpecifiedInflowFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FMasterControlFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FWetlandList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FYMDemandCentreList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FStreamFlowReductionList.Validate(AErrors,AContext) then
      Result := False;
    if not FIFRSiteList.Validate(AErrors,AContext) then
      Result := False;
    if not FMineList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if ((FAppModules.StudyArea.ModelVersion = '6.1') or (FAppModules.StudyArea.ModelVersion = '6.2')) then
      if not FWaterDemandFeatureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if ((FAppModules.StudyArea.ModelVersion = '6.1') or (FAppModules.StudyArea.ModelVersion = '6.2')) then
      if not FWaterDemandConfiguration.Validate(AErrors,AContext) then
        Result := False;
    if (FAppModules.StudyArea.ModelVersion = '7') then
      if not FCurtailmentAndDrought.Validate(AErrors,AContext) then
        Result := False;
    if (FAppModules.StudyArea.ModelVersion = '7') then
      if not FGroundWaterList.Validate(AErrors,AContext) then
        Result := False;
    if (FAppModules.StudyArea.ModelVersion = '7') then
      if not FMineSubCatchmentList.Validate(AErrors,AContext) then
        Result := False;

  except on E: Exception do HandleError(E, OPNAME); end;

end;

function TNetworkFeaturesData.Get_ChannelAreaList: IChannelAreaList;
const OPNAME = 'TNetworkFeaturesData.Get_ChannelAreaList';
begin
  Result := nil;
  try
    Result := FChannelAreaList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_IrrigationBlockList: IIrrigationBlockList;
const OPNAME = 'TNetworkFeaturesData.Get_IrrigationBlockList';
begin
  Result := nil;
  try
    Result := FIrrigationBlockList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_WetlandList: IWetlandList;
const OPNAME = 'TNetworkFeaturesData.Get_WetlandList';
begin
  Result := nil;
  try
    Result := FWetlandList;
  except on E:Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_YMDemandCentreList: IYMDemandCentreList; 
const OPNAME = 'TNetworkFeaturesData.Get_YMDemandCentreList';
begin
  Result := nil;
  try
    Result := FYMDemandCentreList;
  except on E:Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_StreamFlowReductionList: IStreamFlowReductionList;
const OPNAME = 'TNetworkFeaturesData.Get_StreamFlowReductionList';
begin
  Result := nil;
  try
    Result := FStreamFlowReductionList;
  except on E:Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_IFRSiteList: TIFRSiteDataList;
const OPNAME = 'TNetworkFeaturesData.Get_IFRSiteList';
begin
  Result := nil;
  try
    Result := FIFRSiteList;
  except on E:Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_MineList: IMineList;
const OPNAME = 'TNetworkFeaturesData.Get_MineList';
begin
  Result := nil;
  try
    Result := FMineList;
  except on E:Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_CurtailmentAndDrought: ICurtailmentAndDrought;
const OPNAME = 'TNetworkFeaturesData.Get_CurtailmentAndDrought';
begin
  Result := nil;
  try
    Result := FCurtailmentAndDrought;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_GroundWaterList: IGroundWaterList;
const OPNAME = 'TNetworkFeaturesData.Get_GroundWaterList';
begin
  Result := nil;
  try
    Result := FGroundWaterList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkFeaturesData.Get_MineSubCatchmentList: IMineSubCatchmentList;
const OPNAME = 'TNetworkFeaturesData.Get_MineSubCatchmentList';
begin
  Result := nil;
  try
    Result := FMineSubCatchmentList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.


