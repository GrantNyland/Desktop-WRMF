(******************************************************************************)
(*  Contains : Class TIrrigationModule.
(******************************************************************************)
unit UIrrigationModule;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UModule,
  UAbstractObject,
  HydrologyCom_TLB;

type
  TIrrigationCrop = class(TAbstractObject, IIrrigationCrop)
  protected
    FCropNo            : Integer;
    FCropName          : String;
    FCropPercentage    : Double;
    FMonthlyCropFactor : TMonthlyDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_CropNo: Integer; safecall;
    procedure Set_CropNo(Value: Integer); safecall;
    function Get_CropName: WideString; safecall;
    procedure Set_CropName(const Value: WideString); safecall;
    function Get_CropPercentage: Double; safecall;
    procedure Set_CropPercentage(Value: Double); safecall;
    function Get_CropFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_CropFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Populate (ACropNo                   : Integer;
                       const ACropName           : WideString;
                       ACropPercentage           : Double;
                       const AMonthlyCropFactors : WideString): WordBool; safecall;
    property CropNo: Integer read Get_CropNo write Set_CropNo;
    property CropName: WideString read Get_CropName write Set_CropName;
    property CropPercentage: Double read Get_CropPercentage write Set_CropPercentage;
    property CropFactorByMonth[AMonthIndex: Integer]: Double read Get_CropFactorByMonth write Set_CropFactorByMonth;
  end;

  TIrrigationModule = class(TNetworkModule, IIrrigationModule)
  protected
    FIrrigationName                    : WideString;
    FVersionNo                         : Integer;
    FModelType                         : Integer;
    FLastUsedModelType                 : Integer;
    FMAP                               : Double;
    FRainfallFileName                  : WideString;
    FMaxAnnualIrrigationAllocation     : Double;
    FAbstractionRouteNo                : Integer;
    FReturnFlowRouteNo                 : Integer;
    FReturnFlowPercentage              : Double;
    FAreaInterpolationType             : Integer;
    FMaxWaterAllocation                : Double;
    FWaterAllocationNoOfPoints         : Integer;
    FWaterAllocationInterpolationType  : Integer;
    FRunOffModuleNo                    : Integer;
    FTransferCanalSeepage              : Double;
    FProduceNetReturnFlows             : Integer;
    FTransferCanalFlowLossProportion   : Double;
    FTransferCanalSaltLossProportion   : Double;
    FIrrigationEfficiencyFactor        : Double;
    FReturnFlowFactor                  : Double;
    FLowerZoneReturnFlowProportion     : Double;
    FUpperZoneReturnFlowProportion     : Double;
    FSaltConcentrationFactor           : Double;
    FSaltLossProportion                : Double;
    FSaltLoad1                         : Double;
    FSaltLoad2                         : Double;
    FInitialSaltLoadUpperZone          : Double;
    FInitialSaltLoadLowerZone          : Double;
    FSoilMoistureCapacityUpperZone     : Double;
    FSoilMoistureCapacityLowerZone     : Double;
    FTargetSoilMoistureStorage         : Double;
    FInitialSoilMoistureStorage        : Double;
    FEffectiveRainfallFactor1          : Double;
    FEffectiveRainfallFactor2          : Double;
    FGrowthInterpolationType           : Integer;
    FReturnFlowInterpolationType       : Integer;
    FEfficiencyInterpolationType       : Integer;
    FMonthlyPIndexFactor               : TMonthlyDoubleArray;
    FMonthlyRainfallFactor             : TMonthlyDoubleArray;
    FMonthlyCropFactor                 : TMonthlyDoubleArray;
    FMonthlyAPanFactor                 : TMonthlyDoubleArray;
    FIrrigationCrops                   : TObjectList;
    FAreaData                          : TObjectList;
    FAllocationGrowthData              : TObjectList;
    FEfficiencyData                    : TObjectList;
    FReturnFlowData                    : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_IrrigationName: WideString; safecall;
    procedure Set_IrrigationName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_ModelType: Integer; safecall;
    procedure Set_ModelType(Value: Integer); safecall;
    function Get_LastUsedModelType: Integer; safecall;
    procedure Set_LastUsedModelType(Value: Integer); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_MaxAnnualIrrigationAllocation: Double; safecall;
    procedure Set_MaxAnnualIrrigationAllocation(Value: Double); safecall;
    function Get_AbstractionRouteNo: Integer; safecall;
    procedure Set_AbstractionRouteNo(Value: Integer); safecall;
    function Get_ReturnFlowRouteNo: Integer; safecall;
    procedure Set_ReturnFlowRouteNo(Value: Integer); safecall;
    function Get_ReturnFlowPercentage: Double; safecall;
    procedure Set_ReturnFlowPercentage(Value: Double); safecall;
    function Get_AreaInterpolationType: Integer; safecall;
    procedure Set_AreaInterpolationType(Value: Integer); safecall;
    function Get_MaxWaterAllocation: Double; safecall;
    procedure Set_MaxWaterAllocation(Value: Double); safecall;
    function Get_WaterAllocationNoOfPoints: Integer; safecall;
    procedure Set_WaterAllocationNoOfPoints(Value: Integer); safecall;
    function Get_WaterAllocationInterpolationType: Integer; safecall;
    procedure Set_WaterAllocationInterpolationType(Value: Integer); safecall;
    function Get_RunOffModuleNo: Integer; safecall;
    procedure Set_RunOffModuleNo(Value: Integer); safecall;
    function Get_TransferCanalSeepage: Double; safecall;
    procedure Set_TransferCanalSeepage(Value: Double); safecall;
    function Get_ProduceNetReturnFlows: Integer; safecall;
    procedure Set_ProduceNetReturnFlows(Value: Integer); safecall;
    function Get_TransferCanalFlowLossProportion: Double; safecall;
    procedure Set_TransferCanalFlowLossProportion(Value: Double); safecall;
    function Get_TransferCanalSaltLossProportion: Double; safecall;
    procedure Set_TransferCanalSaltLossProportion(Value: Double); safecall;
    function Get_IrrigationEfficiencyFactor: Double; safecall;
    procedure Set_IrrigationEfficiencyFactor(Value: Double); safecall;
    function Get_ReturnFlowFactor: Double; safecall;
    procedure Set_ReturnFlowFactor(Value: Double); safecall;
    function Get_LowerZoneReturnFlowProportion: Double; safecall;
    procedure Set_LowerZoneReturnFlowProportion(Value: Double); safecall;
    function Get_UpperZoneReturnFlowProportion: Double; safecall;
    procedure Set_UpperZoneReturnFlowProportion(Value: Double); safecall;
    function Get_SaltConcentrationFactor: Double; safecall;
    procedure Set_SaltConcentrationFactor(Value: Double); safecall;
    function Get_SaltLossProportion: Double; safecall;
    procedure Set_SaltLossProportion(Value: Double); safecall;
    function Get_SaltLoad1: Double; safecall;
    procedure Set_SaltLoad1(Value: Double); safecall;
    function Get_SaltLoad2: Double; safecall;
    procedure Set_SaltLoad2(Value: Double); safecall;
    function Get_InitialSaltLoadUpperZone: Double; safecall;
    procedure Set_InitialSaltLoadUpperZone(Value: Double); safecall;
    function Get_InitialSaltLoadLowerZone: Double; safecall;
    procedure Set_InitialSaltLoadLowerZone(Value: Double); safecall;
    function Get_SoilMoistureCapacityUpperZone: Double; safecall;
    procedure Set_SoilMoistureCapacityUpperZone(Value: Double); safecall;
    function Get_SoilMoistureCapacityLowerZone: Double; safecall;
    procedure Set_SoilMoistureCapacityLowerZone(Value: Double); safecall;
    function Get_TargetSoilMoistureStorage: Double; safecall;
    procedure Set_TargetSoilMoistureStorage(Value: Double); safecall;
    function Get_InitialSoilMoistureStorage: Double; safecall;
    procedure Set_InitialSoilMoistureStorage(Value: Double); safecall;
    function Get_EffectiveRainfallFactor1: Double; safecall;
    procedure Set_EffectiveRainfallFactor1(Value: Double); safecall;
    function Get_EffectiveRainfallFactor2: Double; safecall;
    procedure Set_EffectiveRainfallFactor2(Value: Double); safecall;
    function Get_GrowthInterpolationType: Integer; safecall;
    procedure Set_GrowthInterpolationType(Value: Integer); safecall;
    function Get_ReturnFlowInterpolationType: Integer; safecall;
    procedure Set_ReturnFlowInterpolationType(Value: Integer); safecall;
    function Get_EfficiencyInterpolationType: Integer; safecall;
    procedure Set_EfficiencyInterpolationType(Value: Integer); safecall;
    function Populate (ANetworkID                        : Integer;
                       AModuleID                         : Integer;
                       const AModuleType                 : WideString;
                       AModuleNumber                     : Integer;
                       ANetworkSequence                  : Integer;
                       const AActive                     : WideString;
                       AVersionNo                        : Integer;
                       const AIrrigationName             : WideString;
                       AModelType                        : Integer;
                       ALastUsedModelType                : Integer;
                       AMAP                              : Double;
                       const ARainfallFileName           : WideString;
                       AMaxAnnualIrrigationAllocation    : Double;
                       AAbstractionRouteNo               : Integer;
                       AReturnFlowRouteNo                : Integer;
                       AReturnFlowPercentage             : Double;
                       AAreaInterpolationType            : Integer;
                       AMaxWaterAllocation               : Double;
                       AWaterAllocationNoOfPoints        : Integer;
                       AWaterAllocationInterpolationType : Integer;
                       ARunOffModuleNo                   : Integer;
                       ATransferCanalSeepage             : Double;
                       AProduceNetReturnFlows            : Integer;
                       ATransferCanalFlowLossProportion  : Double;
                       ATransferCanalSaltLossProportion  : Double;
                       AIrrigationEfficiencyFactor       : Double;
                       AReturnFlowFactor                 : Double;
                       AUpperZoneReturnFlowProportion    : Double;
                       ALowerZoneReturnFlowProportion    : Double;
                       ASaltConcentrationFactor          : Double;
                       ASaltLossProportion               : Double;
                       ASaltLoad1                        : Double;
                       ASaltLoad2                        : Double;
                       AInitialSaltLoadUpperZone         : Double;
                       AInitialSaltLoadLowerZone         : Double;
                       ASoilMoistureCapacityUpperZone    : Double;
                       ASoilMoistureCapacityLowerZone    : Double;
                       ATargetSoilMoistureStorage        : Double;
                       AInitialSoilMoistureStorage       : Double;
                       AEffectiveRainfallFactor1         : Double;
                       AEffectiveRainfallFactor2         : Double;
                       AGrowthInterpolationType          : Integer;
                       AReturnFlowInterpolationType      : Integer;
                       AEfficiencyInterpolationType      : Integer;
                       ALongitude                        : Double;
                       ALatitude                         : Double): WordBool; safecall;
    function Get_PIndexFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_PIndexFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_RainfallFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_RainfallFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_CropFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_CropFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_APanFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_APanFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function AddIrrigationCrop: TIrrigationCrop;
    function Get_NoOfIrrigationCrops: Integer; safecall;
    function Get_IrrigationCropByCropNo(ACropNo: Integer): IIrrigationCrop; safecall;
    function Get_IrrigationCropByIndex(AIndex: Integer): IIrrigationCrop; safecall;
    function AddAreaData(AYear: Integer; AArea: Double): WordBool; safecall;
    function Get_NoOfAreaDataPoints: Integer; safecall;
    function Get_AreaYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_AreaYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_AreaValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AreaValueByIndex(AIndex: Integer; Value: Double); safecall;
    function AddAllocationGrowthData(AYear: Integer; AGrowth: Double): WordBool; safecall;
    function Get_NoOfAllocationGrowthPoints: Integer; safecall;
    function Get_AllocationGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_AllocationGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_AllocationGrowthValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AllocationGrowthValueByIndex(AIndex: Integer; Value: Double); safecall;
    function AddEfficiencyData(AYear: Integer; AEfficiency: Double): WordBool; safecall;
    function Get_NoOfEfficiencyDataPoints: Integer; safecall;
    function Get_EfficiencyYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_EfficiencyYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_EfficiencyValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_EfficiencyValueByIndex(AIndex: Integer; Value: Double); safecall;
    function AddReturnFlowData(AYear: Integer; AValue: Double): WordBool; safecall;
    function Get_NoOfReturnFlowDataPoints: Integer; safecall;
    function Get_ReturnFlowYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_ReturnFlowYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_ReturnFlowValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReturnFlowValueByIndex(AIndex: Integer; Value: Double); safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    function UpdateWQTData (ARootNode : IXMLNode): Boolean;
    function UpdateFactorsData (ARootNode : IXMLNode): Boolean;
    function UpdateAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateAllocationGrowthData (ARootNode : IXMLNode): Boolean;
    function UpdateReturnFlowData (ARootNode : IXMLNode): Boolean;
    function UpdateEfficiencyData (ARootNode : IXMLNode): Boolean;
    function UpdateCropsData (ARootNode : IXMLNode): Boolean;
    procedure ClearAreaData;
    procedure ClearAllocationGrowthData;
    procedure ClearCropsData;
    procedure ClearEfficiencyData;
    procedure ClearReturnFlowData;

    property IrrigationName: WideString read Get_IrrigationName write Set_IrrigationName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property ModelType: Integer read Get_ModelType write Set_ModelType;
    property LastUsedModelType: Integer read Get_LastUsedModelType write Set_LastUsedModelType;
    property MAP: Double read Get_MAP write Set_MAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property MaxAnnualIrrigationAllocation: Double read Get_MaxAnnualIrrigationAllocation write Set_MaxAnnualIrrigationAllocation;
    property AbstractionRouteNo: Integer read Get_AbstractionRouteNo write Set_AbstractionRouteNo;
    property ReturnFlowRouteNo: Integer read Get_ReturnFlowRouteNo write Set_ReturnFlowRouteNo;
    property ReturnFlowPercentage: Double read Get_ReturnFlowPercentage write Set_ReturnFlowPercentage;
    property AreaInterpolationType: Integer read Get_AreaInterpolationType write Set_AreaInterpolationType;
    property MaxWaterAllocation: Double read Get_MaxWaterAllocation write Set_MaxWaterAllocation;
    property WaterAllocationNoOfPoints: Integer read Get_WaterAllocationNoOfPoints write Set_WaterAllocationNoOfPoints;
    property WaterAllocationInterpolationType: Integer read Get_WaterAllocationInterpolationType write Set_WaterAllocationInterpolationType;
    property RunOffModuleNo: Integer read Get_RunOffModuleNo write Set_RunOffModuleNo;
    property TransferCanalSeepage: Double read Get_TransferCanalSeepage write Set_TransferCanalSeepage;
    property ProduceNetReturnFlows: Integer read Get_ProduceNetReturnFlows write Set_ProduceNetReturnFlows;
    property TransferCanalFlowLossProportion: Double read Get_TransferCanalFlowLossProportion write Set_TransferCanalFlowLossProportion;
    property TransferCanalSaltLossProportion: Double read Get_TransferCanalSaltLossProportion write Set_TransferCanalSaltLossProportion;
    property IrrigationEfficiencyFactor: Double read Get_IrrigationEfficiencyFactor write Set_IrrigationEfficiencyFactor;
    property ReturnFlowFactor: Double read Get_ReturnFlowFactor write Set_ReturnFlowFactor;
    property LowerZoneReturnFlowProportion: Double read Get_LowerZoneReturnFlowProportion write Set_LowerZoneReturnFlowProportion;
    property UpperZoneReturnFlowProportion: Double read Get_UpperZoneReturnFlowProportion write Set_UpperZoneReturnFlowProportion;
    property SaltConcentrationFactor: Double read Get_SaltConcentrationFactor write Set_SaltConcentrationFactor;
    property SaltLossProportion: Double read Get_SaltLossProportion write Set_SaltLossProportion;
    property SaltLoad1: Double read Get_SaltLoad1 write Set_SaltLoad1;
    property SaltLoad2: Double read Get_SaltLoad2 write Set_SaltLoad2;
    property InitialSaltLoadUpperZone: Double read Get_InitialSaltLoadUpperZone write Set_InitialSaltLoadUpperZone;
    property InitialSaltLoadLowerZone: Double read Get_InitialSaltLoadLowerZone write Set_InitialSaltLoadLowerZone;
    property SoilMoistureCapacityUpperZone: Double read Get_SoilMoistureCapacityUpperZone write Set_SoilMoistureCapacityUpperZone;
    property SoilMoistureCapacityLowerZone: Double read Get_SoilMoistureCapacityLowerZone write Set_SoilMoistureCapacityLowerZone;
    property TargetSoilMoistureStorage: Double read Get_TargetSoilMoistureStorage write Set_TargetSoilMoistureStorage;
    property InitialSoilMoistureStorage: Double read Get_InitialSoilMoistureStorage write Set_InitialSoilMoistureStorage;
    property EffectiveRainfallFactor1: Double read Get_EffectiveRainfallFactor1 write Set_EffectiveRainfallFactor1;
    property EffectiveRainfallFactor2: Double read Get_EffectiveRainfallFactor2 write Set_EffectiveRainfallFactor2;
    property GrowthInterpolationType: Integer read Get_GrowthInterpolationType write Set_GrowthInterpolationType;
    property ReturnFlowInterpolationType: Integer read Get_ReturnFlowInterpolationType write Set_ReturnFlowInterpolationType;
    property EfficiencyInterpolationType: Integer read Get_EfficiencyInterpolationType write Set_EfficiencyInterpolationType;
    property PIndexFactorByMonth[AMonthIndex: Integer]: Double read Get_PIndexFactorByMonth write Set_PIndexFactorByMonth;
    property RainfallFactorByMonth[AMonthIndex: Integer]: Double read Get_RainfallFactorByMonth write Set_RainfallFactorByMonth;
    property CropFactorByMonth[AMonthIndex: Integer]: Double read Get_CropFactorByMonth write Set_CropFactorByMonth;
    property APanFactorByMonth[AMonthIndex: Integer]: Double read Get_APanFactorByMonth write Set_APanFactorByMonth;
    property NoOfIrrigationCrops: Integer read Get_NoOfIrrigationCrops;
    property IrrigationCropByCropNo[ACropNo: Integer]: IIrrigationCrop read Get_IrrigationCropByCropNo;
    property IrrigationCropByIndex[AIndex: Integer]: IIrrigationCrop read Get_IrrigationCropByIndex;
    property NoOfAreaDataPoints: Integer read Get_NoOfAreaDataPoints;
    property AreaYearByIndex[AIndex: Integer]: Integer read Get_AreaYearByIndex write Set_AreaYearByIndex;
    property AreaValueByIndex[AIndex: Integer]: Double read Get_AreaValueByIndex write Set_AreaValueByIndex;
    property NoOfAllocationGrowthPoints: Integer read Get_NoOfAllocationGrowthPoints;
    property AllocationGrowthYearByIndex[AIndex: Integer]: Integer read Get_AllocationGrowthYearByIndex write Set_AllocationGrowthYearByIndex;
    property AllocationGrowthValueByIndex[AIndex: Integer]: Double read Get_AllocationGrowthValueByIndex write Set_AllocationGrowthValueByIndex;
    property NoOfEfficiencyDataPoints: Integer read Get_NoOfEfficiencyDataPoints;
    property EfficiencyYearByIndex[AIndex: Integer]: Integer read Get_EfficiencyYearByIndex write Set_EfficiencyYearByIndex;
    property EfficiencyValueByIndex[AIndex: Integer]: Double read Get_EfficiencyValueByIndex write Set_EfficiencyValueByIndex;
    property NoOfReturnFlowDataPoints: Integer read Get_NoOfReturnFlowDataPoints;
    property ReturnFlowYearByIndex[AIndex: Integer]: Integer read Get_ReturnFlowYearByIndex write Set_ReturnFlowYearByIndex;
    property ReturnFlowValueByIndex[AIndex: Integer]: Double read Get_ReturnFlowValueByIndex write Set_ReturnFlowValueByIndex;
  end;

  TIrrigationModuleAgent = class(TModuleAgent, IIrrigationModuleAgent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_IrrigationModuleCount: Integer; safecall;
    function FindIrrigationModuleByID(AModuleID: Integer): TIrrigationModule;
    function FindIrrigationModuleByNumber(AModuleNumber: Integer): TIrrigationModule;
    function Get_IrrigationModuleByID(AModuleID: Integer): IIrrigationModule; safecall;
    function Get_IrrigationModuleByNumber(AModuleNumber: Integer): IIrrigationModule; safecall;
    function Get_IrrigationModuleByIndex(AIndex: Integer): IIrrigationModule; safecall;
    function AddIrrigationModule : TIrrigationModule;
    function LoadIrrigationModules (ANetworkID: Integer) : Boolean;
    function CreateNewIrrigationModule (ANetworkID: Integer): IIrrigationModule; safecall;
    function RemoveIrrigationModule(AModuleNumber: Integer): WordBool; safecall;
    property IrrigationModuleCount: Integer read Get_IrrigationModuleCount;
    property IrrigationModuleByID[AID: Integer]: IIrrigationModule read Get_IrrigationModuleByID;
    property IrrigationModuleByIndex[AIndex: Integer]: IIrrigationModule read Get_IrrigationModuleByIndex;
    property IrrigationModuleByNumber[AModuleNumber: Integer]: IIrrigationModule read Get_IrrigationModuleByNumber;
  end;


implementation

uses

  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UFlowRoute,
  UModuleDBManager,
  UIrrigationDBManager,
  UErrorHandlingOperations;

{ TIrrigationCrop *************************************************************}

function TIrrigationCrop._AddRef: Integer;
const OPNAME = 'TIrrigationCrop._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationCrop._Release: Integer;
const OPNAME = 'TIrrigationCrop._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationCrop.Get_CropNo: Integer;
const OPNAME = 'TIrrigationCrop.Get_CropNo';
begin
  Result := 0;
  try
    Result := FCropNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationCrop.Set_CropNo(Value: Integer);
const OPNAME = 'TIrrigationCrop.Set_CropNo';
begin
  try
    FCropNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationCrop.Get_CropName: WideString;
const OPNAME = 'TIrrigationCrop.Get_CropName';
begin
  Result := '';
  try
    Result := FCropName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationCrop.Set_CropName(const Value: WideString);
const OPNAME = 'TIrrigationCrop.Set_CropName';
begin
  try
    FCropName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationCrop.Get_CropPercentage: Double;
const OPNAME = 'TIrrigationCrop.Get_CropPercentage';
begin
  Result := 0.0;
  try
    Result := FCropPercentage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationCrop.Set_CropPercentage(Value: Double);
const OPNAME = 'TIrrigationCrop.Set_CropPercentage';
begin
  try
    FCropPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationCrop.Get_CropFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TIrrigationCrop.Get_CropFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyCropFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationCrop.Set_CropFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationCrop.Set_CropFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyCropFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationCrop.Populate (ACropNo                   : Integer;
                                   const ACropName           : WideString;
                                   ACropPercentage           : Double;
                                   const AMonthlyCropFactors : WideString): WordBool;
const OPNAME = 'TIrrigationCrop.Populate';
var
  LTempList : TStringList;
  LIndex    : Integer;
begin
  Result := FALSE;
  try
    FCropNo         := ACropNo;
    FCropName       := ACropName;
    FCropPercentage := ACropPercentage;
    LTempList := TStringList.Create;
    try
      LTempList.CommaText := AMonthlyCropFactors;
      for LIndex := 1 to 12 do
        FMonthlyCropFactor[LIndex] := StrToFloat(LTempList.Strings[LIndex-1]);
    finally
      LTempList.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TIrrigationModule ***********************************************************}

function TIrrigationModule._AddRef: Integer;
const OPNAME = 'TIrrigationModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule._Release: Integer;
const OPNAME = 'TIrrigationModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.CreateMemberObjects;
const OPNAME = 'TIrrigationModule.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIrrigationCrops      := TObjectList.Create(FALSE);
    FAreaData             := TObjectList.Create(FALSE);
    FAllocationGrowthData := TObjectList.Create(FALSE);
    FEfficiencyData       := TObjectList.Create(FALSE);
    FReturnFlowData       := TObjectList.Create(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.DestroyMemberObjects;
const OPNAME = 'TIrrigationModule.DestroyMemberObjects';
begin
  try
    FIrrigationCrops.Free;
    FAreaData.Free;
    FAllocationGrowthData.Free;
    FEfficiencyData.Free;
    FReturnFlowData.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_IrrigationName: WideString; 
const OPNAME = 'TIrrigationModule.Get_IrrigationName';
begin
  Result := '';
  try
    Result := FIrrigationName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_IrrigationName(const Value: WideString);
const OPNAME = 'TIrrigationModule.Set_IrrigationName';
begin
  try
    FIrrigationName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_VersionNo: Integer;
const OPNAME = 'TIrrigationModule.Get_VersionNo';
begin
  Result := 0;
  try
    Result := FVersionNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_VersionNo(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_VersionNo';
begin
  try
    FVersionNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ModelType: Integer;
const OPNAME = 'TIrrigationModule.Get_ModelType';
begin
  Result := 0;
  try
    Result := FModelType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ModelType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_ModelType';
begin
  try
    FModelType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_LastUsedModelType: Integer;
const OPNAME = 'TIrrigationModule.Get_LastUsedModelType';
begin
  Result := 0;
  try
    Result := FLastUsedModelType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_LastUsedModelType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_LastUsedModelType';
begin
  try
    FLastUsedModelType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_MAP: Double;
const OPNAME = 'TIrrigationModule.Get_MAP';
begin
  Result := 0.0;
  try
    Result := FMAP;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_MAP(Value: Double);
const OPNAME = 'TIrrigationModule.Set_MAP';
begin
  try
    FMAP := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_RainfallFileName: WideString;
const OPNAME = 'TIrrigationModule.Get_RainfallFileName';
begin
  Result := '';
  try
    Result := FRainfallFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_RainfallFileName(const Value: WideString);
const OPNAME = 'TIrrigationModule.Set_RainfallFileName';
begin
  try
    FRainfallFileName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_MaxAnnualIrrigationAllocation: Double;
const OPNAME = 'TIrrigationModule.Get_MaxAnnualIrrigationAllocation';
begin
  Result := 0.0;
  try
    Result := FMaxAnnualIrrigationAllocation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_MaxAnnualIrrigationAllocation(Value: Double);
const OPNAME = 'TIrrigationModule.Set_MaxAnnualIrrigationAllocation';
begin
  try
    FMaxAnnualIrrigationAllocation := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_AbstractionRouteNo: Integer;
const OPNAME = 'TIrrigationModule.Get_AbstractionRouteNo';
begin
  Result := 0;
  try
    Result := FAbstractionRouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_AbstractionRouteNo(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_AbstractionRouteNo';
begin
  try
    FAbstractionRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ReturnFlowRouteNo: Integer;
const OPNAME = 'TIrrigationModule.Get_ReturnFlowRouteNo';
begin
  Result := 0;
  try
    Result := FReturnFlowRouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ReturnFlowRouteNo(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_ReturnFlowRouteNo';
begin
  try
    FReturnFlowRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ReturnFlowPercentage: Double;
const OPNAME = 'TIrrigationModule.Get_ReturnFlowPercentage';
begin
  Result := 0.0;
  try
    Result := FReturnFlowPercentage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ReturnFlowPercentage(Value: Double);
const OPNAME = 'TIrrigationModule.Set_ReturnFlowPercentage';
begin
  try
    FReturnFlowPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_AreaInterpolationType: Integer;
const OPNAME = 'TIrrigationModule.Get_AreaInterpolationType';
begin
  Result := 0;
  try
    Result := FAreaInterpolationType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_AreaInterpolationType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_AreaInterpolationType';
begin
  try
    FAreaInterpolationType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_MaxWaterAllocation: Double;
const OPNAME = 'TIrrigationModule.Get_MaxWaterAllocation';
begin
  Result := 0.0;
  try
    Result := FMaxWaterAllocation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_MaxWaterAllocation(Value: Double);
const OPNAME = 'TIrrigationModule.Set_MaxWaterAllocation';
begin
  try
    FMaxWaterAllocation := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_WaterAllocationNoOfPoints: Integer;
const OPNAME = 'TIrrigationModule.Get_WaterAllocationNoOfPoints';
begin
  Result := 0;
  try
    Result := FWaterAllocationNoOfPoints;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_WaterAllocationNoOfPoints(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_WaterAllocationNoOfPoints';
begin
  try
    FWaterAllocationNoOfPoints := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_WaterAllocationInterpolationType: Integer;
const OPNAME = 'TIrrigationModule.Get_WaterAllocationInterpolationType';
begin
  Result := 0;
  try
    Result := FWaterAllocationInterpolationType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_WaterAllocationInterpolationType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_WaterAllocationInterpolationType';
begin
  try
    FWaterAllocationInterpolationType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_RunOffModuleNo: Integer;
const OPNAME = 'TIrrigationModule.Get_RunOffModuleNo';
begin
  Result := 0;
  try
    Result := FRunOffModuleNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_RunOffModuleNo(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_RunOffModuleNo';
begin
  try
    FRunOffModuleNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_TransferCanalSeepage: Double;
const OPNAME = 'TIrrigationModule.Get_TransferCanalSeepage';
begin
  Result := 0.0;
  try
    Result := FTransferCanalSeepage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_TransferCanalSeepage(Value: Double);
const OPNAME = 'TIrrigationModule.Set_TransferCanalSeepage';
begin
  try
    FTransferCanalSeepage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ProduceNetReturnFlows: Integer;
const OPNAME = 'TIrrigationModule.Get_ProduceNetReturnFlows';
begin
  Result := 0;
  try
    Result := FProduceNetReturnFlows;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ProduceNetReturnFlows(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_ProduceNetReturnFlows';
begin
  try
    FProduceNetReturnFlows := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_TransferCanalFlowLossProportion: Double;
const OPNAME = 'TIrrigationModule.Get_TransferCanalFlowLossProportion';
begin
  Result := 0.0;
  try
    Result := FTransferCanalFlowLossProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_TransferCanalFlowLossProportion(Value: Double);
const OPNAME = 'TIrrigationModule.Set_TransferCanalFlowLossProportion';
begin
  try
    FTransferCanalFlowLossProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_TransferCanalSaltLossProportion: Double;
const OPNAME = 'TIrrigationModule.Get_TransferCanalSaltLossProportion';
begin
  Result := 0.0;
  try
    Result := FTransferCanalSaltLossProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_TransferCanalSaltLossProportion(Value: Double);
const OPNAME = 'TIrrigationModule.Set_TransferCanalSaltLossProportion';
begin
  try
    FTransferCanalSaltLossProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_IrrigationEfficiencyFactor: Double;
const OPNAME = 'TIrrigationModule.Get_IrrigationEfficiencyFactor';
begin
  Result := 0.0;
  try
    Result := FIrrigationEfficiencyFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_IrrigationEfficiencyFactor(Value: Double);
const OPNAME = 'TIrrigationModule.Set_IrrigationEfficiencyFactor';
begin
  try
    FIrrigationEfficiencyFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ReturnFlowFactor: Double;
const OPNAME = 'TIrrigationModule.Get_ReturnFlowFactor';
begin
  Result := 0.0;
  try
   Result := FReturnFlowFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ReturnFlowFactor(Value: Double);
const OPNAME = 'TIrrigationModule.Set_ReturnFlowFactor';
begin
  try
    FReturnFlowFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_LowerZoneReturnFlowProportion: Double;
const OPNAME = 'TIrrigationModule.Get_LowerZoneReturnFlowProportion';
begin
  Result := 0.0;
  try
    Result := FLowerZoneReturnFlowProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_LowerZoneReturnFlowProportion(Value: Double);
const OPNAME = 'TIrrigationModule.Set_LowerZoneReturnFlowProportion';
begin
  try
    FLowerZoneReturnFlowProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_UpperZoneReturnFlowProportion: Double;
const OPNAME = 'TIrrigationModule.Get_UpperZoneReturnFlowProportion';
begin
  Result := 0.0;
  try
    Result := FUpperZoneReturnFlowProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_UpperZoneReturnFlowProportion(Value: Double);
const OPNAME = 'TIrrigationModule.Set_UpperZoneReturnFlowProportion';
begin
  try
    FUpperZoneReturnFlowProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_SaltConcentrationFactor: Double;
const OPNAME = 'TIrrigationModule.Get_SaltConcentrationFactor';
begin
  Result := 0.0;
  try
    Result := FSaltConcentrationFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_SaltConcentrationFactor(Value: Double);
const OPNAME = 'TIrrigationModule.Set_SaltConcentrationFactor';
begin
  try
    FSaltConcentrationFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_SaltLossProportion: Double;
const OPNAME = 'TIrrigationModule.Get_SaltLossProportion';
begin
  Result := 0.0;
  try
    Result := FSaltLossProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_SaltLossProportion(Value: Double);
const OPNAME = 'TIrrigationModule.Set_SaltLossProportion';
begin
  try
    FSaltLossProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_SaltLoad1: Double;
const OPNAME = 'TIrrigationModule.Get_SaltLoad1';
begin
  Result := 0.0;
  try
    Result := FSaltLoad1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_SaltLoad1(Value: Double);
const OPNAME = 'TIrrigationModule.Set_SaltLoad1';
begin
  try
    FSaltLoad1 := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_SaltLoad2: Double;
const OPNAME = 'TIrrigationModule.Get_SaltLoad2';
begin
  Result := 0.0;
  try
    Result := FSaltLoad2;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_SaltLoad2(Value: Double);
const OPNAME = 'TIrrigationModule.Set_SaltLoad2';
begin
  try
    FSaltLoad2 := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_InitialSaltLoadUpperZone: Double;
const OPNAME = 'TIrrigationModule.Get_InitialSaltLoadUpperZone';
begin
  Result := 0.0;
  try
    Result := FInitialSaltLoadUpperZone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_InitialSaltLoadUpperZone(Value: Double);
const OPNAME = 'TIrrigationModule.Set_InitialSaltLoadUpperZone';
begin
  try
    FInitialSaltLoadUpperZone := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_InitialSaltLoadLowerZone: Double;
const OPNAME = 'TIrrigationModule.Get_InitialSaltLoadLowerZone';
begin
  Result := 0.0;
  try
    Result := FInitialSaltLoadLowerZone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_InitialSaltLoadLowerZone(Value: Double);
const OPNAME = 'TIrrigationModule.Set_InitialSaltLoadLowerZone';
begin
  try
    FInitialSaltLoadLowerZone := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_SoilMoistureCapacityUpperZone: Double;
const OPNAME = 'TIrrigationModule.Get_SoilMoistureCapacityUpperZone';
begin
  Result := 0.0;
  try
    Result := FSoilMoistureCapacityUpperZone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_SoilMoistureCapacityUpperZone(Value: Double);
const OPNAME = 'TIrrigationModule.Set_SoilMoistureCapacityUpperZone';
begin
  try
    FSoilMoistureCapacityUpperZone := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_SoilMoistureCapacityLowerZone: Double;
const OPNAME = 'TIrrigationModule.Get_SoilMoistureCapacityLowerZone';
begin
  Result := 0.0;
  try
    Result := FSoilMoistureCapacityLowerZone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_SoilMoistureCapacityLowerZone(Value: Double);
const OPNAME = 'TIrrigationModule.Set_SoilMoistureCapacityLowerZone';
begin
  try
    FSoilMoistureCapacityLowerZone := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_TargetSoilMoistureStorage: Double;
const OPNAME = 'TIrrigationModule.Get_TargetSoilMoistureStorage';
begin
  Result := 0.0;
  try
    Result := FTargetSoilMoistureStorage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_TargetSoilMoistureStorage(Value: Double);
const OPNAME = 'TIrrigationModule.Set_TargetSoilMoistureStorage';
begin
  try
    FTargetSoilMoistureStorage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_InitialSoilMoistureStorage: Double;
const OPNAME = 'TIrrigationModule.Get_InitialSoilMoistureStorage';
begin
  Result := 0.0;
  try
    Result := FInitialSoilMoistureStorage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_InitialSoilMoistureStorage(Value: Double);
const OPNAME = 'TIrrigationModule.Set_InitialSoilMoistureStorage';
begin
  try
    FInitialSoilMoistureStorage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_EffectiveRainfallFactor1: Double;
const OPNAME = 'TIrrigationModule.Get_EffectiveRainfallFactor1';
begin
  Result := 0.0;
  try
    Result := FEffectiveRainfallFactor1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_EffectiveRainfallFactor1(Value: Double);
const OPNAME = 'TIrrigationModule.Set_EffectiveRainfallFactor1';
begin
  try
    FEffectiveRainfallFactor1 := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_EffectiveRainfallFactor2: Double;
const OPNAME = 'TIrrigationModule.Get_EffectiveRainfallFactor2';
begin
  Result := 0.0;
  try
    Result := FEffectiveRainfallFactor2;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_EffectiveRainfallFactor2(Value: Double);
const OPNAME = 'TIrrigationModule.Set_EffectiveRainfallFactor2';
begin
  try
    FEffectiveRainfallFactor2 := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_GrowthInterpolationType: Integer;
const OPNAME = 'TIrrigationModule.Get_GrowthInterpolationType';
begin
  Result := 0;
  try
    Result := FGrowthInterpolationType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_GrowthInterpolationType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_GrowthInterpolationType';
begin
  try
    FGrowthInterpolationType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ReturnFlowInterpolationType: Integer;
const OPNAME = 'TIrrigationModule.Get_ReturnFlowInterpolationType';
begin
  Result := 0;
  try
    Result := FReturnFlowInterpolationType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ReturnFlowInterpolationType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_ReturnFlowInterpolationType';
begin
  try
    FReturnFlowInterpolationType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_EfficiencyInterpolationType: Integer;
const OPNAME = 'TIrrigationModule.Get_EfficiencyInterpolationType';
begin
  Result := 0;
  try
    Result := FEfficiencyInterpolationType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_EfficiencyInterpolationType(Value: Integer);
const OPNAME = 'TIrrigationModule.Set_EfficiencyInterpolationType';
begin
  try
    FEfficiencyInterpolationType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Populate (ANetworkID                        : Integer;
                                     AModuleID                         : Integer;
                                     const AModuleType                 : WideString;
                                     AModuleNumber                     : Integer;
                                     ANetworkSequence                  : Integer;
                                     const AActive                     : WideString;
                                     AVersionNo                        : Integer;
                                     const AIrrigationName             : WideString;
                                     AModelType                        : Integer;
                                     ALastUsedModelType                : Integer;
                                     AMAP                              : Double;
                                     const ARainfallFileName           : WideString;
                                     AMaxAnnualIrrigationAllocation    : Double;
                                     AAbstractionRouteNo               : Integer;
                                     AReturnFlowRouteNo                : Integer;
                                     AReturnFlowPercentage             : Double;
                                     AAreaInterpolationType            : Integer;
                                     AMaxWaterAllocation               : Double;
                                     AWaterAllocationNoOfPoints        : Integer;
                                     AWaterAllocationInterpolationType : Integer;
                                     ARunOffModuleNo                   : Integer;
                                     ATransferCanalSeepage             : Double;
                                     AProduceNetReturnFlows            : Integer;
                                     ATransferCanalFlowLossProportion  : Double;
                                     ATransferCanalSaltLossProportion  : Double;
                                     AIrrigationEfficiencyFactor       : Double;
                                     AReturnFlowFactor                 : Double;
                                     AUpperZoneReturnFlowProportion    : Double;
                                     ALowerZoneReturnFlowProportion    : Double;
                                     ASaltConcentrationFactor          : Double;
                                     ASaltLossProportion               : Double;
                                     ASaltLoad1                        : Double;
                                     ASaltLoad2                        : Double;
                                     AInitialSaltLoadUpperZone         : Double;
                                     AInitialSaltLoadLowerZone         : Double;
                                     ASoilMoistureCapacityUpperZone    : Double;
                                     ASoilMoistureCapacityLowerZone    : Double;
                                     ATargetSoilMoistureStorage        : Double;
                                     AInitialSoilMoistureStorage       : Double;
                                     AEffectiveRainfallFactor1         : Double;
                                     AEffectiveRainfallFactor2         : Double;
                                     AGrowthInterpolationType          : Integer;
                                     AReturnFlowInterpolationType      : Integer;
                                     AEfficiencyInterpolationType      : Integer;
                                     ALongitude                        : Double;
                                     ALatitude                         : Double): WordBool;
const OPNAME = 'TIrrigationModule.Populate';
begin
  Result := FALSE;
  try
    FNetworkID                         := ANetworkID;
    FModuleID                          := AModuleID;
    FModuleType                        := AModuleType;
    FModuleNumber                      := AModuleNumber;
    FNetworkSequence                   := ANetworkSequence;
    FActive                            := AActive;
    FVersionNo                         := AVersionNo;
    FIrrigationName                    := AIrrigationName;
    FModelType                         := AModelType;
    FLastUsedModelType                 := ALastUsedModelType;
    FMAP                               := AMAP;
    FRainfallFileName                  := ARainfallFileName;
    FMaxAnnualIrrigationAllocation     := AMaxAnnualIrrigationAllocation;
    FAbstractionRouteNo                := AAbstractionRouteNo;
    FReturnFlowRouteNo                 := AReturnFlowRouteNo;
    FReturnFlowPercentage              := AReturnFlowPercentage;
    FAreaInterpolationType             := AAreaInterpolationType;
    FMaxWaterAllocation                := AMaxWaterAllocation;
    FWaterAllocationNoOfPoints         := AWaterAllocationNoOfPoints;
    FWaterAllocationInterpolationType  := AWaterAllocationInterpolationType;
    FRunOffModuleNo                    := ARunOffModuleNo;
    FTransferCanalSeepage              := ATransferCanalSeepage;
    FProduceNetReturnFlows             := AProduceNetReturnFlows;
    FTransferCanalFlowLossProportion   := ATransferCanalFlowLossProportion;
    FTransferCanalSaltLossProportion   := ATransferCanalSaltLossProportion;
    FIrrigationEfficiencyFactor        := AIrrigationEfficiencyFactor;
    FReturnFlowFactor                  := AReturnFlowFactor;
    FLowerZoneReturnFlowProportion     := ALowerZoneReturnFlowProportion;
    FUpperZoneReturnFlowProportion     := AUpperZoneReturnFlowProportion;
    FSaltConcentrationFactor           := ASaltConcentrationFactor;
    FSaltLossProportion                := ASaltLossProportion;
    FSaltLoad1                         := ASaltLoad1;
    FSaltLoad2                         := ASaltLoad2;
    FInitialSaltLoadUpperZone          := AInitialSaltLoadUpperZone;
    FInitialSaltLoadLowerZone          := AInitialSaltLoadLowerZone;
    FSoilMoistureCapacityUpperZone     := ASoilMoistureCapacityUpperZone;
    FSoilMoistureCapacityLowerZone     := ASoilMoistureCapacityLowerZone;
    FTargetSoilMoistureStorage         := ATargetSoilMoistureStorage;
    FInitialSoilMoistureStorage        := AInitialSoilMoistureStorage;
    FEffectiveRainfallFactor1          := AEffectiveRainfallFactor1;
    FEffectiveRainfallFactor2          := AEffectiveRainfallFactor2;
    FGrowthInterpolationType           := AGrowthInterpolationType;
    FReturnFlowInterpolationType       := AReturnFlowInterpolationType;
    FEfficiencyInterpolationType       := AEfficiencyInterpolationType;
    FLongitude                         := ALongitude;
    FLatitude                          := ALatitude;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_PIndexFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_PIndexFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyPIndexFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_PIndexFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_PIndexFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyPIndexFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_RainfallFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_RainfallFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyRainfallFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_RainfallFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_RainfallFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyRainfallFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_CropFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_CropFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyCropFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_CropFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_CropFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyCropFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_APanFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_APanFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyAPanFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_APanFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_APanFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyAPanFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.AddIrrigationCrop: TIrrigationCrop;
const OPNAME = 'TIrrigationModule.AddIrrigationCrop';
var
  LCrop : TIrrigationCrop;
begin
  Result := nil;
  try
    LCrop := TIrrigationCrop.Create;
    FIrrigationCrops.Add(LCrop);
    Result := LCrop;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_NoOfIrrigationCrops: Integer;
const OPNAME = 'TIrrigationModule.Get_NoOfIrrigationCrops';
begin
  Result := 0;
  try
    Result := FIrrigationCrops.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_IrrigationCropByCropNo(ACropNo: Integer): IIrrigationCrop;
const OPNAME = 'TIrrigationModule.Get_IrrigationCropByCropNo';
var
  LCrop  : TIrrigationCrop;
  LIndex : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FIrrigationCrops.Count)) do
    begin
      LCrop := TIrrigationCrop(FIrrigationCrops.Items[LIndex]);
      if (LCrop.FCropNo = ACropNo) then
        Result := LCrop
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_IrrigationCropByIndex(AIndex: Integer): IIrrigationCrop;
const OPNAME = 'TIrrigationModule.Get_IrrigationCropByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FIrrigationCrops.Count)) then
      Result := TIrrigationCrop(FIrrigationCrops.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.AddAreaData(AYear: Integer; AArea: Double): WordBool;
const OPNAME = 'TIrrigationModule.AddAreaData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AArea;
    FAreaData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_NoOfAreaDataPoints: Integer;
const OPNAME = 'TIrrigationModule.Get_NoOfAreaDataPoints';
begin
  Result := 0;
  try
    Result := FAreaData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_AreaYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TIrrigationModule.Get_AreaYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FAreaData.Count)) then
      Result := TYearValuePair(FAreaData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_AreaYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TIrrigationModule.Set_AreaYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FAreaData.Count)) then
      TYearValuePair(FAreaData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_AreaValueByIndex(AIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_AreaValueByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FAreaData.Count)) then
      Result := TYearValuePair(FAreaData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_AreaValueByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_AreaValueByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FAreaData.Count)) then
      TYearValuePair(FAreaData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.AddAllocationGrowthData(AYear: Integer; AGrowth: Double): WordBool;
const OPNAME = 'TIrrigationModule.AddAllocationGrowthData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AGrowth;
    FAllocationGrowthData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_NoOfAllocationGrowthPoints: Integer;
const OPNAME = 'TIrrigationModule.Get_NoOfAllocationGrowthPoints';
begin
  Result := 0;
  try
    Result := FAllocationGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_AllocationGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TIrrigationModule.Get_AllocationGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FAllocationGrowthData.Count)) then
      Result := TYearValuePair(FAllocationGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_AllocationGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TIrrigationModule.Set_AllocationGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FAllocationGrowthData.Count)) then
      TYearValuePair(FAllocationGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_AllocationGrowthValueByIndex (AIndex : Integer): Double;
const OPNAME = 'TIrrigationModule.Get_AllocationGrowthValueByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FAllocationGrowthData.Count)) then
      Result := TYearValuePair(FAllocationGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_AllocationGrowthValueByIndex(AIndex : Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_AllocationGrowthValueByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FAllocationGrowthData.Count)) then
      TYearValuePair(FAllocationGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.AddEfficiencyData(AYear: Integer; AEfficiency: Double): WordBool;
const OPNAME = 'TIrrigationModule.AddEfficiencyData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AEfficiency;
    FEfficiencyData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_NoOfEfficiencyDataPoints: Integer;
const OPNAME = 'TIrrigationModule.Get_NoOfEfficiencyDataPoints';
begin
  Result := 0;
  try
    Result := FEfficiencyData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_EfficiencyYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TIrrigationModule.Get_EfficiencyYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FEfficiencyData.Count)) then
      Result := TYearValuePair(FEfficiencyData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_EfficiencyYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TIrrigationModule.Set_EfficiencyYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FEfficiencyData.Count)) then
      TYearValuePair(FEfficiencyData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_EfficiencyValueByIndex(AIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_EfficiencyValueByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FEfficiencyData.Count)) then
      Result := TYearValuePair(FEfficiencyData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_EfficiencyValueByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_EfficiencyValueByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FEfficiencyData.Count)) then
      TYearValuePair(FEfficiencyData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.AddReturnFlowData(AYear: Integer; AValue: Double): WordBool;
const OPNAME = 'TIrrigationModule.AddReturnFlowData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AValue;
    FReturnFlowData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_NoOfReturnFlowDataPoints: Integer;
const OPNAME = 'TIrrigationModule.Get_NoOfReturnFlowDataPoints';
begin
  Result := 0;
  try
    Result := FReturnFlowData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ReturnFlowYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TIrrigationModule.Get_ReturnFlowYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FReturnFlowData.Count)) then
      Result := TYearValuePair(FReturnFlowData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ReturnFlowYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TIrrigationModule.Set_ReturnFlowYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FReturnFlowData.Count)) then
      TYearValuePair(FReturnFlowData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.Get_ReturnFlowValueByIndex(AIndex: Integer): Double;
const OPNAME = 'TIrrigationModule.Get_ReturnFlowValueByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FReturnFlowData.Count)) then
      Result := TYearValuePair(FReturnFlowData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModule.Set_ReturnFlowValueByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TIrrigationModule.Set_ReturnFlowValueByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FReturnFlowData.Count)) then
      TYearValuePair(FReturnFlowData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModule.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdatePropertiesData';
var
  LActive                 : String;
  LLatitude               : Double;
  LLongitude              : Double;
  LIrrigationName         : String;
  LVersionNo              : Integer;
  LMAP                    : Double;
  LModelType              : Integer;
  LLastUsedModelType      : Integer;
  LRainfallFileName       : String;
  LMaxAnnualIrrAllocation : Double;
  LAbstractionRouteNo     : Integer;
  LReturnFlowRouteNo      : Integer;
  LReturnFlowPercentage   : Double;
  LDataNode               : IXMLNode;
begin
  Result := TRUE;
  try
    LDataNode := ARootNode.ChildNodes['IrrigationProperties'];
    LActive                 := LDataNode.ChildNodes['Active'].Text;
    LLatitude               := StrToFloat(LDataNode.ChildNodes['Latitude'].Text);
    LLongitude              := StrToFloat(LDataNode.ChildNodes['Longitude'].Text);
    LIrrigationName         := LDataNode.ChildNodes['IrrigationName'].Text;
    LVersionNo              := StrToInt(LDataNode.ChildNodes['VersionNo'].Text);
    LMAP                    := StrToFloat(LDataNode.ChildNodes['MAP'].Text);
    LModelType              := StrToInt(LDataNode.ChildNodes['ModelType'].Text);
    LLastUsedModelType      := StrToInt(LDataNode.ChildNodes['LastUsedModelType'].Text);
    LRainfallFileName       := LDataNode.ChildNodes['RainfallFileName'].Text;
    LMaxAnnualIrrAllocation := StrToFloat(LDataNode.ChildNodes['MaxAnnualIrrAllocation'].Text);
    LAbstractionRouteNo     := StrToInt(LDataNode.ChildNodes['AbstractionRouteNo'].Text);
    LReturnFlowRouteNo      := StrToInt(LDataNode.ChildNodes['ReturnFlowRouteNo'].Text);
    LReturnFlowPercentage   := StrToFloat(LDataNode.ChildNodes['ReturnFlowPercentage'].Text);

    Result := GIrrigationDBManager.UpdatePropertiesDataInDB
                                     (FModuleID, LActive, LLatitude, LLongitude, LIrrigationName,
                                      LVersionNo, LMAP, LModelType, LLastUsedModelType, LRainfallFileName,
                                      LMaxAnnualIrrAllocation, LAbstractionRouteNo, LReturnFlowRouteNo,
                                      LReturnFlowPercentage);
    if (Result) then
    begin
      FLatitude                      := LLatitude;
      FLongitude                     := LLongitude;
      FActive                        := LActive;
      FIrrigationName                := LIrrigationName;
      FVersionNo                     := LVersionNo;
      FMAP                           := LMAP;
      FModelType                     := LModelType;
      FLastUsedModelType             := LLastUsedModelType;
      FRainfallFileName              := LRainfallFileName;
      FMaxAnnualIrrigationAllocation := LMaxAnnualIrrAllocation;
      FAbstractionRouteNo            := LAbstractionRouteNo;
      FReturnFlowRouteNo             := LReturnFlowRouteNo;
      FReturnFlowPercentage          := LReturnFlowPercentage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateWQTData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateWQTData';
var
  LDataNode                         : IXMLNode;
  LProduceNetReturnFlows            : Integer;
  LIrrigationEfficiencyFactor       : Double;
  LTransferCanalSaltLossProportion  : Double;
  LTransferCanalFlowLossProportion  : Double;
  LTransferCanalSeepage             : Double;
  LRunOffModuleNo                   : Integer;
  LWaterAllocationInterpolationType : Integer;
  LMaxWaterAllocation               : Double;
  LReturnFlowFactor                 : Double;
  LUpperZoneReturnFlowProportion    : Double;
  LLowerZoneReturnFlowProportion    : Double;
  LSaltConcentrationFactor          : Double;
  LLandSaltLossProportion           : Double;
  LSaltLoad1                        : Double;
  LSaltLoad2                        : Double;
  LInitialSaltLoadLowerZone         : Double;
  LInitialSaltLoadUpperZone         : Double;
  LSoilMoistureCapacityLowerZone    : Double;
  LSoilMoistureCapacityUpperZone    : Double;
  LInitialSoilMoisture              : Double;
  LTargetSoilMoisture               : Double;
  LEffectiveRainfallFactor1         : Double;
  LEffectiveRainfallFactor2         : Double;
begin
  Result := FALSE;
  try
    LDataNode := ARootNode.ChildNodes['IrrigationWQT'];
    LProduceNetReturnFlows              := StrToInt(LDataNode.ChildNodes['ProduceNetReturnFlows'].Text);
    LIrrigationEfficiencyFactor         := StrToFloat(LDataNode.ChildNodes['IrrigationEfficiencyFactor'].Text);
    LTransferCanalSaltLossProportion    := StrToFloat(LDataNode.ChildNodes['TransferCanalSaltLossProportion'].Text);
    LTransferCanalFlowLossProportion    := StrToFloat(LDataNode.ChildNodes['TransferCanalFlowLossProportion'].Text);
    LTransferCanalSeepage               := StrToFloat(LDataNode.ChildNodes['TransferCanalSeepage'].Text);
    LRunOffModuleNo                     := StrToInt(LDataNode.ChildNodes['RunOffModuleNo'].Text);
    LWaterAllocationInterpolationType   := StrToInt(LDataNode.ChildNodes['WaterAllocationInterpolationType'].Text);
    LMaxWaterAllocation                 := StrToFloat(LDataNode.ChildNodes['MaxWaterAllocation'].Text);
    LReturnFlowFactor                   := StrToFloat(LDataNode.ChildNodes['ReturnFlowFactor'].Text);
    LUpperZoneReturnFlowProportion      := StrToFloat(LDataNode.ChildNodes['UpperZoneReturnFlowProportion'].Text);
    LLowerZoneReturnFlowProportion      := StrToFloat(LDataNode.ChildNodes['LowerZoneReturnFlowProportion'].Text);
    LSaltConcentrationFactor            := StrToFloat(LDataNode.ChildNodes['SaltConcentrationFactor'].Text);
    LLandSaltLossProportion             := StrToFloat(LDataNode.ChildNodes['LandSaltLossProportion'].Text);
    LSaltLoad1                          := StrToFloat(LDataNode.ChildNodes['SaltLoad1'].Text);
    LSaltLoad2                          := StrToFloat(LDataNode.ChildNodes['SaltLoad2'].Text);
    LInitialSaltLoadLowerZone           := StrToFloat(LDataNode.ChildNodes['InitialSaltLoadLowerZone'].Text);
    LInitialSaltLoadUpperZone           := StrToFloat(LDataNode.ChildNodes['InitialSaltLoadUpperZone'].Text);
    LSoilMoistureCapacityLowerZone      := StrToFloat(LDataNode.ChildNodes['SoilMoistureStorageCapacityLowerZone'].Text);
    LSoilMoistureCapacityUpperZone      := StrToFloat(LDataNode.ChildNodes['SoilMoistureStorageCapacityUpperZone'].Text);
    LInitialSoilMoisture                := StrToFloat(LDataNode.ChildNodes['InitialSoilMoisture'].Text);
    LTargetSoilMoisture                 := StrToFloat(LDataNode.ChildNodes['TargetSoilMoisture'].Text);
    LEffectiveRainfallFactor1           := StrToFloat(LDataNode.ChildNodes['EffectiveRainfallFactor1'].Text);
    LEffectiveRainfallFactor2           := StrToFloat(LDataNode.ChildNodes['EffectiveRainfallFactor2'].Text);

    Result := GIrrigationDBManager.UpdateWQTDataInDB
                                     (FModuleID, LProduceNetReturnFlows, LIrrigationEfficiencyFactor,
                                      LTransferCanalSaltLossProportion, LTransferCanalFlowLossProportion,
                                      LTransferCanalSeepage, LRunOffModuleNo, LWaterAllocationInterpolationType,
                                      LMaxWaterAllocation, LReturnFlowFactor, LUpperZoneReturnFlowProportion,
                                      LLowerZoneReturnFlowProportion, LSaltConcentrationFactor, LLandSaltLossProportion,
                                      LSaltLoad1, LSaltLoad2, LInitialSaltLoadLowerZone, LInitialSaltLoadUpperZone,
                                      LSoilMoistureCapacityLowerZone, LSoilMoistureCapacityUpperZone,
                                      LInitialSoilMoisture, LTargetSoilMoisture, LEffectiveRainfallFactor1,
                                      LEffectiveRainfallFactor2);
    if (Result) then
    begin
      FProduceNetReturnFlows             := LProduceNetReturnFlows;
      FIrrigationEfficiencyFactor        := LIrrigationEfficiencyFactor;
      FTransferCanalSaltLossProportion   := LTransferCanalSaltLossProportion;
      FTransferCanalFlowLossProportion   := LTransferCanalFlowLossProportion;
      FTransferCanalSeepage              := LTransferCanalSeepage;
      FRunOffModuleNo                    := LRunOffModuleNo;
      FWaterAllocationInterpolationType  := LWaterAllocationInterpolationType;
      FMaxWaterAllocation                := LMaxWaterAllocation;
      FReturnFlowFactor                  := LReturnFlowFactor;
      FUpperZoneReturnFlowProportion     := LUpperZoneReturnFlowProportion;
      FLowerZoneReturnFlowProportion     := LLowerZoneReturnFlowProportion;
      FSaltConcentrationFactor           := LSaltConcentrationFactor;
      FSaltLossProportion                := LLandSaltLossProportion;
      FSaltLoad1                         := LSaltLoad1;
      FSaltLoad2                         := LSaltLoad2;
      FInitialSaltLoadLowerZone          := LInitialSaltLoadLowerZone;
      FInitialSaltLoadUpperZone          := LInitialSaltLoadUpperZone;
      FSoilMoistureCapacityLowerZone     := LSoilMoistureCapacityLowerZone;
      FSoilMoistureCapacityUpperZone     := LSoilMoistureCapacityUpperZone;
      FInitialSoilMoistureStorage        := LInitialSoilMoisture;
      FTargetSoilMoistureStorage         := LTargetSoilMoisture;
      FEffectiveRainfallFactor1          := LEffectiveRainfallFactor1;
      FEffectiveRainfallFactor2          := LEffectiveRainfallFactor2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateFactorsData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateFactorsData';
var
  LListNode       : IXMLNode;
  LDataNode       : IXMLNode;
  LIndex          : Integer;
  LMonth          : Integer;
  LMonthList      : TStringList;
  LPindexList     : TStringList;
  LRainList       : TStringList;
  LCropList       : TStringList;
  LAPanList       : TStringList;
begin
  Result := FALSE;
  try
    LListNode := ARootNode.ChildNodes['IrrigationFactors'];

    LMonthList  := TStringList.Create;
    LPindexList := TStringList.Create;
    LRainList   := TStringList.Create;
    LAPanList   := TStringList.Create;
    LCropList   := TStringList.Create;
    try
      LListNode := LListNode.ChildNodes['DataList'];
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode       := LListNode.ChildNodes.Get(LIndex-1);
        LMonthList.Add(LDataNode.ChildNodes['Month'].Text);
        LPindexList.Add(LDataNode.ChildNodes['PIndexFactor'].Text);
        LRainList.Add(LDataNode.ChildNodes['RainfallFactor'].Text);
        LCropList.Add(LDataNode.ChildNodes['CropFactor'].Text);
        LAPanList.Add(LDataNode.ChildNodes['APanFactor'].Text);
      end;

      Result := GIrrigationDBManager.UpdateFactorsDataInDB
                                       (FModuleID, LMonthList, LPindexList, LRainList, LCropList, LAPanList);

      if (Result) then
      begin
        for LIndex := 0 to LMonthList.Count - 1 do
        begin
          LMonth                         := StrToInt(LMonthList.Strings[LIndex]);
          FMonthlyPIndexFactor[LMonth]   := StrToFloat(LPindexList.Strings[LIndex]);
          FMonthlyRainfallFactor[LMonth] := StrToFloat(LRainList.Strings[LIndex]);
          FMonthlyCropFactor[LMonth]     := StrToFloat(LCropList.Strings[LIndex]);
          FMonthlyAPanFactor[LMonth]     := StrToFloat(LAPanList.Strings[LIndex]);
        end;
      end;
    finally
      LMonthList.Free;
      LPindexList.Free;
      LRainList.Free;
      LCropList.Free;
      LAPanList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateAreaData';
var
  LIrrigationAreaNode    : IXMLNode;
  LListNode              : IXMLNode;
  LDataNode              : IXMLNode;
  LIndex                 : Integer;
  LAreaInterpolationType : Integer;
  LYearList              : TStringList;
  LAreaList              : TStringList;
begin
  Result := FALSE;
  try
    LIrrigationAreaNode    := ARootNode.ChildNodes['IrrigationArea'];
    LAreaInterpolationType := StrToInt(LIrrigationAreaNode.ChildNodes['AreaInterpolationType'].Text);
    LListNode              := LIrrigationAreaNode.ChildNodes['DataList'];

    LYearList := TStringList.Create;
    LAreaList := TStringList.Create;
    try
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LAreaList.Add(LDataNode.ChildNodes['Area'].Text);
      end;

      Result := GIrrigationDBManager.UpdateAreaDataInDB(FModuleID, LAreaInterpolationType, LYearList, LAreaList);

      if (Result) then
      begin
        FAreaInterpolationType := LAreaInterpolationType;
        Result := GIrrigationDBManager.LoadIrrigationAreaDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LAreaList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateAllocationGrowthData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateAllocationGrowthData';
var
  LAllocationGrowthNode : IXMLNode;
  LListNode             : IXMLNode;
  LDataNode             : IXMLNode;
  LIndex                : Integer;
  LInterpolationType    : Integer;
  LYearList             : TStringList;
  LGrowthList           : TStringList;
begin
  Result := FALSE;
  try
    LAllocationGrowthNode := ARootNode.ChildNodes['IrrigationAllocationGrowth'];
    LInterpolationType    := StrToInt(LAllocationGrowthNode.ChildNodes['AllocationGrowthInterpolationType'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LAllocationGrowthNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GIrrigationDBManager.UpdateAllocationGrowthDataInDB(FModuleID, LInterpolationType,
                                                                    LYearList, LGrowthList);
      if (Result) then
      begin
        FGrowthInterpolationType := LInterpolationType;
        Result := GIrrigationDBManager.LoadIrrigationAllocationGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateReturnFlowData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateReturnFlowData';
var
  LReturnFlowNode    : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LInterpolationType : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
begin
  Result := FALSE;
  try
    LReturnFlowNode    := ARootNode.ChildNodes['IrrigationReturnFlow'];
    LInterpolationType := StrToInt(LReturnFlowNode.ChildNodes['ReturnFlowInterpolationType'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LReturnFlowNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text)
      end;

      Result := GIrrigationDBManager.UpdateReturnFlowDataInDB(FModuleID, LInterpolationType, LYearList, LGrowthList);

      if (Result) then
      begin
        FReturnFlowInterpolationType := LInterpolationType;
        Result := GIrrigationDBManager.LoadIrrigationReturnFlowFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateEfficiencyData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateEfficiencyData';
var
  LEfficiencyNode    : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LInterpolationType : Integer;
  LYearList          : TStringList;
  LEfficiencyList    : TStringList;
begin
  Result := FALSE;
  try
    LEfficiencyNode := ARootNode.ChildNodes['IrrigationEfficiency'];
    LInterpolationType := StrToInt(LEfficiencyNode.ChildNodes['EfficiencyInterpolationType'].Text);

    LYearList       := TStringList.Create;
    LEfficiencyList := TStringList.Create;
    try
      LListNode := LEfficiencyNode.ChildNodes['DataList'];

      Result := GIrrigationDBManager.DeleteEfficiencyFromDB(FModuleID);
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LEfficiencyList.Add(LDataNode.ChildNodes['Efficiency'].Text);
      end;

      Result := GIrrigationDBManager.UpdateEfficiencyDataInDB(FModuleID, LInterpolationType, LYearList, LEfficiencyList);
      if (Result) then
      begin
        FEfficiencyInterpolationType := LInterpolationType;
        Result := GIrrigationDBManager.LoadIrrigationEfficiencyFromDB(Self);
      end;
    finally
      LYearList.Free;
      LEfficiencyList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationModule.UpdateCropsData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TIrrigationModule.UpdateCropsData';
var
  LIrrigationCropsNode : IXMLNode;
  LDataListNode        : IXMLNode;
  LCropDataNode        : IXMLNode;
  LMonthlyListNode     : IXMLNode;
  LNode                : IXMLNode;
  LIndex               : Integer;
  LCount               : Integer;
  LCropFactors         : String;
  LCropNoList          : TStringList;
  LPercentageList      : TStringList;
  LFactorsList         : TStringList;
begin
  Result := FALSE;
  try
    LIrrigationCropsNode := ARootNode.ChildNodes['IrrigationCrops'];

    LCropNoList     := TStringList.Create;
    LPercentageList := TStringList.Create;
    LFactorsList    := TStringList.Create;
    try
      LDataListNode := LIrrigationCropsNode.ChildNodes['DataList'];

      for LIndex := 1 to LDataListNode.ChildNodes.Count do
      begin
        LCropDataNode    := LDataListNode.ChildNodes.Get(LIndex-1);
        LMonthlyListNode := LCropDataNode.ChildNodes['MonthlyCropFactors'];
        LCropNoList.Add(LCropDataNode.ChildNodes['CropNo'].Text);
        LPercentageList.Add(LCropDataNode.ChildNodes['CropPercentage'].Text);
        LCropFactors := '';
        for LCount := 1 to 12 do
        begin
          LNode        := LMonthlyListNode.ChildNodes.Get(LCount-1);
          LCropFactors := LCropFactors + ',' + LNode.ChildNodes['CropFactor'].Text;
        end;
        LCropFactors := Copy(LCropFactors, 2, Length(LCropFactors) - 1);
        LFactorsList.Add(LCropFactors);
      end;

      Result := GIrrigationDBManager.UpdateCropsDataInDB(FModuleID, LCropNoList, LPercentageList, LFactorsList);

      if (Result) then
        Result := GIrrigationDBManager.LoadIrrigationCropsFromDB(Self);
    finally
      LCropNoList.Free;
      LPercentageList.Free;
      LFactorsList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationModule.ClearAreaData;
const OPNAME = 'TIrrigationModule.ClearAreaData ';
begin
  try
    while (FAreaData.Count > 0) do
      FAreaData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationModule.ClearAllocationGrowthData;
const OPNAME = 'TIrrigationModule.ClearAllocationGrowthData ';
begin
  try
    while (FAllocationGrowthData.Count > 0) do
      FAllocationGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationModule.ClearCropsData;
const OPNAME = 'TIrrigationModule.ClearCropsData ';
begin
  try
    while (FIrrigationCrops.Count > 0) do
      FIrrigationCrops.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationModule.ClearEfficiencyData;
const OPNAME = 'TIrrigationModule.ClearEfficiencyData ';
begin
  try
    while (FEfficiencyData.Count > 0) do
      FEfficiencyData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationModule.ClearReturnFlowData;
const OPNAME = 'TIrrigationModule.ClearReturnFlowData ';
begin
  try
    while (FReturnFlowData.Count > 0) do
      FReturnFlowData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIrrigationModuleAgent *******************************************************}

function TIrrigationModuleAgent._AddRef: Integer;
const OPNAME = 'TIrrigationModuleAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent._Release: Integer;
const OPNAME = 'TIrrigationModuleAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModuleAgent.CreateMemberObjects;
const OPNAME = 'TIrrigationModuleAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GIrrigationDBManager := TIrrigationDBManager.Create;
    GIrrigationDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationModuleAgent.DestroyMemberObjects;
const OPNAME = 'TIrrigationModuleAgent.DestroyMemberObjects';
begin
  try
    GIrrigationDBManager.ModuleAgent := nil;
    FreeAndNil(GIrrigationDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.AddIrrigationModule : TIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.AddIrrigationModule';
var
  LIrrigationModule : TIrrigationModule;
begin
  Result := nil;
  try
    LIrrigationModule := TIrrigationModule.Create;
    FList.Add(LIrrigationModule);
    Result := LIrrigationModule;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.LoadIrrigationModules (ANetworkID: Integer) : Boolean;
const OPNAME = 'TIrrigationModuleAgent.LoadIrrigationModules';
begin
  Result := FALSE;
  try
    Result := GIrrigationDBManager.LoadIrrigationModulesFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.CreateNewIrrigationModule (ANetworkID: Integer): IIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.CreateNewIrrigationModule';
begin
  Result := nil;
  try
    Result := GIrrigationDBManager.CreateNewIrrigationModuleInDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.RemoveIrrigationModule(AModuleNumber: Integer): WordBool;
const OPNAME = 'TIrrigationModuleAgent.RemoveIrrigationModule';
var
  LIrrModule  : TIrrigationModule;
  LIndex      : Integer;
begin
  Result := FALSE;
  try
    LIrrModule := FindIrrigationModuleByNumber(AModuleNumber);
    if (LIrrModule <> nil) then
    begin
      if (GIrrigationDBManager.RemoveIrrigationModuleFromDB(LIrrModule.ModuleID)) then
      begin
        Result := TRUE;
        LIndex := FList.IndexOf(LIrrModule);
        FList.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.Get_IrrigationModuleCount: Integer;
const OPNAME = 'TIrrigationModuleAgent.Get_IrrigationModuleCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.FindIrrigationModuleByID (AModuleID: Integer): TIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.FindIrrigationModuleByID';
var
  LIrrigationModule : TIrrigationModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LIrrigationModule := TIrrigationModule(FList.Items[LIndex]);
      if (LIrrigationModule.ModuleID = AModuleID) then
        Result := LIrrigationModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.Get_IrrigationModuleByID(AModuleID: Integer): IIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.Get_IrrigationModuleByID';
var
  LIrrigationModule : TIrrigationModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LIrrigationModule := TIrrigationModule(FList.Items[LIndex]);
      if (LIrrigationModule.ModuleID = AModuleID) then
        Result := LIrrigationModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.FindIrrigationModuleByNumber (AModuleNumber : Integer): TIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.FindIrrigationModuleByNumber';
var
  LIrrigationModule : TIrrigationModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LIrrigationModule := TIrrigationModule(FList.Items[LIndex]);
      if (LIrrigationModule.ModuleNumber = AModuleNumber) then
        Result := LIrrigationModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.Get_IrrigationModuleByNumber (AModuleNumber : Integer): IIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.Get_IrrigationModuleByNumber';
var
  LIrrigationModule : TIrrigationModule;
  LIndex           : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LIrrigationModule := TIrrigationModule(FList.Items[LIndex]);
      if (LIrrigationModule.ModuleNumber = AModuleNumber) then
        Result := LIrrigationModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationModuleAgent.Get_IrrigationModuleByIndex(AIndex: Integer): IIrrigationModule;
const OPNAME = 'TIrrigationModuleAgent.Get_IrrigationModuleByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TIrrigationModule(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
