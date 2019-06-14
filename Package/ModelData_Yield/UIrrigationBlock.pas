{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationBlock                           *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrigationBlock;

interface

uses
  Classes,
  Contnrs, VCL.dialogs,
  UAbstractObject,
  UChannelData,
  VoaimsCom_TLB,
  UConstants;

type
  TDiversionChannelMaxDemand = class(TAbstractAppObject)
  protected
    FIrrigationBlockNumber : Integer;
    FChannelNumber   : integer;
    FMaxDemand       : TMonthlyDoubleArray;
    FInUse           : boolean;
    function Update_MaxDemands:boolean;
    function Get_MaxDemandByIndex(AIndex: integer): double; safecall;
    procedure Set_MaxDemandByIndex(AIndex: integer; AValue: Double); safecall;
    function Get_InUse: boolean; safecall;
    procedure Set_InUse(AValue: boolean); safecall;
    procedure InitialiseValues(AValue: double);
  public
    procedure PopulateMaxDemandValues(AIrrigationBlockNumber,AChannelNumber : integer; AMaxDemands: TMonthlyDoubleArray; AInUse : boolean);
    function Initialise: boolean;override;
    function StudyHasChanged: boolean; override;
    function Populated: boolean;
    property MaxDemandByIndex[AIndex: integer] : double read Get_MaxDemandByIndex write Set_MaxDemandByIndex;
    property InUse : boolean read Get_InUse write Set_InUse;
  end;

{******************************************************************************}
{ Water usage                                                                  }
{******************************************************************************}
  TWaterUsage = class(TAbstractAppObject, IWaterUsage)
  protected
    FBlockIdentifier       : Integer;
    FIdentifier            : Integer;
    FCropName              : WideString;
    FPercAreaUnderCropType : Double;
    FMonthlyWaterUse       : TMonthlyDoubleArray;

    function Get_BlockIdentifier: Integer; safecall;
    function Get_Identifier: Integer; safecall;
    function Get_PercAreaUnderCropType: Double; safecall;
    function Get_CropName: WideString; safecall;
    function Get_MonthlyWaterUse(AIndex: Integer): Double; safecall;

    procedure Set_BlockIdentifier(Value: Integer); safecall;
    procedure Set_Identifier(Value: Integer); safecall;
    procedure Set_PercAreaUnderCropType(Value: Double); safecall;
    procedure Set_CropName(const Value: WideString); safecall;
    procedure Set_MonthlyWaterUse(AIndex: Integer; Value: Double); safecall;

    function ValidateWaterUsePercAreaUnderCropType(AErrorMessages: TStrings): Boolean;
    function ValidateWaterUseCropName(AErrorMessages: TStrings): Boolean;
    function ValidateWaterUseMonthlyWaterUse(AErrorMessages : TStrings; AErrorColumns  : TStringList): boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ASource: TWaterUsage);
    function Initialise : Boolean; override;
    function PopulateIDs(AIdentifier: integer; ACropName: string): boolean;

    function Populate ( AIdentifier,
                        ABlockIdentifier  : Integer;
                        ACropName         : WideString;
                        APercArea         : Double;
                        AMonthWaterUse    : TMonthlyDoubleArray ): WordBool;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property PercAreaUnderCropType            : Double     read Get_PercAreaUnderCropType write Set_PercAreaUnderCropType;
    property CropName                         : WideString read Get_CropName         write Set_CropName;
    property Identifier                       : Integer    read Get_Identifier       write Set_Identifier;
    property BlockIdentifier                  : Integer    read Get_BlockIdentifier  write Set_BlockIdentifier;
    property MonthlyWaterUse[AIndex: Integer] : Double     read Get_MonthlyWaterUse  write Set_MonthlyWaterUse;
  end;

{******************************************************************************}
{* Irrigation Block                                                           *}
{******************************************************************************}

  TIrrigationBlock = class(TAbstractAppObject, IIrrigationBlock)
  protected
    FIdentifier                    : Integer;
    FBlockNodeNumber               : Integer;
    FBlockName                     : WideString;
    FBlockDescription              : WideString;
    FDiversionChannelNr            : Integer;
    FReturnFlowChannelNr           : Integer;
    //Soil properties
    FUpperZoneReturnFlow           : Double;
    FLowerZoneReturnFlow           : Double;
    FReturnFlowLoss                : Double;
    FUpperZoneSoilMoistureCapacity : Double;
    FLowerZoneSoilMoistureCapacity : Double;
    FUpperZoneSoilMoistureTarget   : Double;
    FInitialSoilMoistureStorage    : Double;

    FMultiplicationFactor          : Double;

    FAPanConvFactor                : TMonthlyDoubleArray;
    FPanEvaporation                : TMonthlyDoubleArray;
    FRainfallFactor                : TMonthlyDoubleArray;

    FWaterUsageFactor              : TObjectList;

    FMonthlyDiversionFlows         : TIrrMonthlyDoublesArray;
    FMonthlyReturnFlows            : TIrrMonthlyDoublesArray;
    FMaxWaterAllocation            : Double;
    FFileName                      : WideString;
    FHydrologyNodeNumber           : Integer;

    FCanalTransportLoss            : Double;
    FEfficiencyFactor              : Double;
    FReturnFlowFactor              : Double;

    FNumberOfCropTypes             : Integer;

    FRainAboveRainFactorSpecValue  : Double;
    FRainBelowRainFactor           : Double;
    FRainCatchmentScalingFactor    : Double;
    FAllocatedIrrigationArea       : Double;
    FDroughtApplicable             : Integer;
    FCropWaterUseType              : Integer;
    FDiversionChannelMaxDemand     : TDiversionChannelMaxDemand;

    //_______________________________NEW BEGIN______________________________________________________________________
    FIrrigationBlockType : integer;
    FCurtailIrrigationAbstraction : integer;
    FCanalSeepageLoss : double;
    FCanalTransmissionLoss : double;
    FUpperSoilOutflow : double;
    FMaxUpperZoneMoisture : double;
    FMinUpperZoneMoisture : double;
    FCropTypesCount : integer;
    FIrrigationSupplyCapacity : double;
    FAllocatedAreaPointsCount : integer;
    FMethodIrrigatedAreas : integer;
    FMaxWaterAllocationCount : integer;
    FMethodMaxWaterAllocation : integer;
    FReturnFlowVolumePointsCount : integer;
    FMethodReturnFlowVolume : integer;
    FSupplyCapacityPointsCount : integer;
    FMethodSupplyCapacity : integer;
    FIrrigationEfficienciesPointsCount : integer;
    FMethodIrrigationEfficiencies : integer;
    FReturnFlowFactorsCount : integer;
    FMethodReturnFlowFactors : integer;

    FIrrigatedAreasBreakPointYear : TStringList;
    FIrrigatedArea : TStringList;
    FMaximumWaterAllocationBreakPointYear : TStringList;
    FMaximumWaterAllocation : TStringList;
    FMaximumWaterAllocationGrowth : TStringList;
    FReturnFlowVolumeBreakpointYear : TStringList;
    FReturnFlowVolume : TStringList;
    FSupplyCapacityBreakpointYear : TStringList;
    FSupplyCapacity : TStringList;
    FIrrigationEfficiencyBreakpointYear : TStringList;
    FIrrigationEfficiency : TStringList;
    FReturnFlowFactorBreakpointYear : TStringList;
    FReturnFlowFactors : TStringList;

     FOldIrrigatedAreasBreakPointYearCommaText : string;
     FOldIrrigatedAreaCommaText : string;
     FOldMaximumWaterAllocationBreakPointYearCommaText : string;
     FOldMaximumWaterAllocationCommaText : string;
     FOldMaximumWaterAllocationGrowthCommaText : string;
     FOldReturnFlowVolumeBreakpointYearCommaText : string;
     FOldReturnFlowVolumeCommaText : string;
     FOldSupplyCapacityBreakpointYearCommaText : string;
     FOldSupplyCapacityCommaText : string;
     FOldIrrigationEfficiencyBreakpointYearCommaText : string;
     FOldIrrigationEfficiencyCommaText : string;
     FOldReturnFlowFactorBreakpointYearCommaText : string;
     FOldReturnFlowFactorsCommaText : string;

    //_______________________________NEW END________________________________________________________________________

    function Get_AllocatedIrrigationArea      : Double; safecall;
    function Get_Identifier                   : Integer; safecall;
    function Get_BlockName                    : WideString; safecall;
    function Get_BlockNodeNumber              : Integer; safecall;
    function Get_CanalTransportLoss           : Double; safecall;
    function Get_EfficiencyFactor             : Double; safecall;
    function Get_FileName                     : WideString; safecall;
    function Get_MaxWaterAllocation           : Double; safecall;
    function Get_HydrologyNodeNumber          : Integer; safecall;
    function Get_NumberOfCropTypes            : Integer; safecall;
    function Get_RainAboveRainFactorSpecValue : Double; safecall;
    function Get_RainBelowRainFactor          : Double; safecall;
    function Get_RainCatchmentScalingFactor   : Double; safecall;
    function Get_ReturnFlowFactor             : Double; safecall;
    function Get_BlockDescription             : WideString; safecall;
    function Get_DiversionChannel             : IGeneralFlowChannel; safecall;
    function Get_ReturnFlowChannel            : IGeneralFlowChannel; safecall;
    function Get_WaterUsageCount              : Integer; safecall;
    function Get_DroughtApplicable            : Integer; safecall;
    function Get_CropWaterUseType             : Integer; safecall;
    function Get_HydrologyNode                : IReservoirData; safecall;
    function Get_BlockNode                    : IReservoirData; safecall;

    procedure Set_BlockDescription(const Value: WideString); safecall;
    procedure Set_AllocatedIrrigationArea(Value: Double); safecall;
    procedure Set_BlockIdentifier( Value: Integer); safecall;
    procedure Set_BlockName(const Value: WideString); safecall;
    procedure Set_BlockNodeNumber( Value: Integer); safecall;
    procedure Set_CanalTransportLoss( Value: Double); safecall;
    procedure Set_EfficiencyFactor( Value: Double); safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    procedure Set_MaxWaterAllocation( Value: Double); safecall;
    procedure Set_HydrologyNodeNumber( Value: Integer); safecall;
    procedure Set_NumberOfCropTypes( Value: Integer); safecall;
    procedure Set_RainAboveRainFactorSpecValue( Value: Double); safecall;
    procedure Set_RainBelowRainFactor( Value: Double); safecall;
    procedure Set_RainCatchmentScalingFactor( Value: Double); safecall;
    procedure Set_ReturnFlowFactor( Value: Double); safecall;
    procedure Set_WaterUsageFactor(AIndex: Integer;  Value: TWaterUsage); safecall;
    procedure Set_DroughtApplicable( Value: Integer); safecall;
    procedure Set_CropWaterUseType( Value: Integer); safecall;

    //_______________________________NEW BEGIN______________________________________________________________________
    function  Get_IrrigationBlockType : integer; safecall;
    procedure Set_IrrigationBlockType(AValue : integer); safecall;
    function  Get_CurtailIrrigationAbstraction : integer; safecall;
    procedure Set_CurtailIrrigationAbstraction(AValue : integer); safecall;
    function  Get_CanalSeepageLoss : double; safecall;
    procedure Set_CanalSeepageLoss(AValue : double); safecall;
    function  Get_CanalTransmissionLoss : double; safecall;
    procedure Set_CanalTransmissionLoss(AValue : double); safecall;
    function  Get_UpperSoilOutflow : double; safecall;
    procedure Set_UpperSoilOutflow(AValue : double); safecall;
    function  Get_MaxUpperZoneMoisture : double; safecall;
    procedure Set_MaxUpperZoneMoisture(AValue : double); safecall;
    function  Get_MinUpperZoneMoisture : double; safecall;
    procedure Set_MinUpperZoneMoisture(AValue : double); safecall;
    function  Get_CropTypesCount : integer; safecall;
    procedure Set_CropTypesCount(AValue : integer); safecall;
    function  Get_IrrigationSupplyCapacity : double; safecall;
    procedure Set_IrrigationSupplyCapacity(AValue : double); safecall;
    function  Get_AllocatedAreaPointsCount : integer; safecall;
    procedure Set_AllocatedAreaPointsCount(AValue : integer); safecall;
    function  Get_MethodIrrigatedAreas : integer; safecall;
    procedure Set_MethodIrrigatedAreas(AValue : integer); safecall;
    function  Get_MaxWaterAllocationCount : integer; safecall;
    procedure Set_MaxWaterAllocationCount(AValue : integer); safecall;
    function  Get_MethodMaxWaterAllocation : integer; safecall;
    procedure Set_MethodMaxWaterAllocation(AValue : integer); safecall;
    function  Get_ReturnFlowVolumePointsCount : integer; safecall;
    procedure Set_ReturnFlowVolumePointsCount(AValue : integer); safecall;
    function  Get_MethodReturnFlowVolume : integer; safecall;
    procedure Set_MethodReturnFlowVolume(AValue : integer); safecall;
    function  Get_SupplyCapacityPointsCount : integer; safecall;
    procedure Set_SupplyCapacityPointsCount(AValue : integer); safecall;
    function  Get_MethodSupplyCapacity : integer; safecall;
    procedure Set_MethodSupplyCapacity(AValue : integer); safecall;
    function  Get_MethodIrrigationEfficiencies : integer; safecall;
    procedure Set_IrrigationEfficienciesPointsCount(AValue : integer); safecall;
    function  Get_IrrigationEfficienciesPointsCount : integer; safecall;
    procedure Set_MethodIrrigationEfficiencies(AValue : integer); safecall;
    function  Get_ReturnFlowFactorsCount : integer; safecall;
    procedure Set_ReturnFlowFactorsCount(AValue : integer); safecall;
    function  Get_MethodReturnFlowFactors : integer; safecall;
    procedure Set_MethodReturnFlowFactors(AValue : integer); safecall;
    function Get_IrrigatedAreasBreakPointCount    : integer; safecall;

    procedure Set_IrrigatedAreasBreakPointCount(Value: Integer); safecall;
    function Get_MaximumWaterAllocationBreakPointCount     : integer; safecall;
    procedure Set_MaximumWaterAllocationBreakPointCount(Value: Integer); safecall;
    function Get_ReturnFlowVolumeBreakPointsCount : integer; safecall;
    procedure Set_ReturnFlowVolumeBreakPointsCount(Value: Integer); safecall;
    function Get_SupplyCapacityBreakPointsCount : integer; safecall;
    procedure Set_SupplyCapacityBreakPointsCount(Value: Integer); safecall;
    function Get_IrrigationEfficiencyBreakPointsCount : integer; safecall;
    procedure Set_IrrigationEfficiencyBreakPointsCount(Value: Integer); safecall;
    function Get_ReturnFlowFactorBreakPointsCount : integer; safecall;
    procedure Set_ReturnFlowFactorBreakPointsCount(Value: Integer); safecall;
    function Get_MultiplicationFactor: Double; safecall;
    procedure Set_MultiplicationFactor(AValue: Double); safecall;

    function Get_IrrigatedAreasBreakPointYearByIndex(AIndex : integer) : integer; safecall;
    function Get_IrrigatedAreaByIndex(AIndex : integer) : double; safecall;
    function Get_MaximumWaterAllocationBreakPointYearByIndex(AIndex : integer) : integer; safecall;
    function Get_MaximumWaterAllocationByIndex(AIndex : integer) : double; safecall;
    function Get_MaximumWaterAllocationGrowthByIndex(AIndex : integer) : double; safecall;
    function Get_ReturnFlowVolumeBreakpointYearByIndex(AIndex : integer) : integer; safecall;
    function Get_ReturnFlowVolumeByIndex(AIndex : integer) : double; safecall;
    function Get_SupplyCapacityBreakpointYearByIndex(AIndex : integer) : integer; safecall;
    function Get_SupplyCapacityByIndex(AIndex : integer) : double; safecall;
    function Get_IrrigationEfficiencyBreakpointYearByIndex(AIndex : integer) : integer; safecall;
    function Get_IrrigationEfficiencyByIndex(AIndex : integer) : double; safecall;
    function Get_ReturnFlowFactorBreakpointYearByIndex(AIndex : integer) : integer; safecall;
    function Get_ReturnFlowFactorsByIndex(AIndex : integer) : double; safecall;

    procedure Set_IrrigatedAreasBreakPointYearByIndex(AIndex : integer;  AValue: integer); safecall;
    procedure Set_IrrigatedAreaByIndex(AIndex : integer;  AValue: double); safecall;
    procedure Set_MaximumWaterAllocationBreakPointYearByIndex(AIndex : integer;  AValue: integer); safecall;
    procedure Set_MaximumWaterAllocationByIndex(AIndex : integer;  AValue: double); safecall;
    procedure Set_MaximumWaterAllocationGrowthByIndex(AIndex : integer;  AValue: double); safecall;
    procedure Set_ReturnFlowVolumeBreakpointYearByIndex(AIndex : integer;  AValue: integer); safecall;
    procedure Set_ReturnFlowVolumeByIndex(AIndex : integer;  AValue: double); safecall;
    procedure Set_SupplyCapacityBreakpointYearByIndex(AIndex : integer;  AValue: integer); safecall;
    procedure Set_SupplyCapacityByIndex(AIndex : integer;  AValue: double); safecall;
    procedure Set_IrrigationEfficiencyBreakpointYearByIndex(AIndex : integer;  AValue: integer); safecall;
    procedure Set_IrrigationEfficiencyByIndex(AIndex : integer;  AValue: double); safecall;
    procedure Set_ReturnFlowFactorBreakpointYearByIndex(AIndex : integer;  AValue: integer); safecall;
    procedure Set_ReturnFlowFactorsByIndex(AIndex : integer;  AValue: double); safecall;


    function SaveIrrigatedAreasBreakPointYear: boolean;
    function SaveIrrigatedArea: boolean;
    function SaveMaximumWaterAllocationBreakPointYear: boolean;
    function SaveMaximumWaterAllocation: boolean;
    function SaveMaximumWaterAllocationGrowth: boolean;
    function SaveReturnFlowVolumeBreakpointYear: boolean;
    function SaveReturnFlowVolume: boolean;
    function SaveSupplyCapacityBreakpointYear: boolean;
    function SaveSupplyCapacity: boolean;
    function SaveIrrigationEfficiencyBreakpointYear: boolean;
    function SaveIrrigationEfficiency: boolean;
    function SaveReturnFlowFactorBreakpointYear: boolean;
    function SaveReturnFlowFactors: boolean;

    //_______________________________NEW END________________________________________________________________________


    // Evaporation/Climate
    function Get_APanConvFactor(AMonth: Integer): Double; safecall;
    function Get_PanEvaporation(AMonth: Integer): Double; safecall;
    function Get_RainfallFactor(AMonth: Integer): Double; safecall;

    procedure Set_APanConvFactor(AMonth: Integer;  Value: Double); safecall;
    procedure Set_PanEvaporation(AMonth: Integer;  Value: Double); safecall;
    procedure Set_RainfallFactor(AMonth: Integer;  Value: Double); safecall;

    //Soil Properties
    function Get_UpperZoneReturnFlow : Double; safecall;
    function Get_LowerZoneReturnFlow : Double; safecall;
    function Get_ReturnFlowLoss : Double; safecall;
    function Get_UpperZoneSoilMoistureCapacity : Double; safecall;
    function Get_LowerZoneSoilMoistureCapacity : Double; safecall;
    function Get_UpperZoneSoilMoistureTarget : Double; safecall;
    function Get_InitialSoilMoistureStorage : Double; safecall;

    procedure Set_UpperZoneReturnFlow( Value: Double); safecall;
    procedure Set_LowerZoneReturnFlow( Value: Double); safecall;
    procedure Set_ReturnFlowLoss( Value: Double); safecall;
    procedure Set_UpperZoneSoilMoistureCapacity( Value: Double); safecall;
    procedure Set_LowerZoneSoilMoistureCapacity( Value: Double); safecall;
    procedure Set_UpperZoneSoilMoistureTarget( Value: Double); safecall;
    procedure Set_InitialSoilMoistureStorage( Value: Double); safecall;


    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateAllocatedIrrigationArea(AErrorMessages : TStrings) : WordBool;
    function ValidateUpperZoneReturnFlow(AErrorMessages : TStrings) : WordBool;
    function ValidateLowerZoneReturnFlow(AErrorMessages : TStrings) : WordBool;
    function ValidateReturnFlowLoss(AErrorMessages : TStrings) : WordBool;
    function ValidateUpperZoneSoilMoistureCapacity(AErrorMessages : TStrings) : WordBool;
    function ValidateLowerZoneSoilMoistureCapacity(AErrorMessages : TStrings) : WordBool;
    function ValidateUpperZoneSoilMoistureTarget(AErrorMessages : TStrings) : WordBool;
    function ValidateInitialSoilMoistureStorage(AErrorMessages : TStrings) : WordBool;
    function ValidateName(AErrorMessages: TStrings): Boolean;
    function ValidateRainfallFactor(AErrorMessages : TStrings; AErrorColumns  : TStringList): boolean;
    function ValidatePanEvaporation(AErrorMessages : TStrings; AErrorColumns  : TStringList): boolean;
    function ValidateAPanConvFactor(AErrorMessages : TStrings; AErrorColumns  : TStringList): boolean;

    function ValidateBlockNodeNumber(AErrorMessages: TStrings): Boolean;
    function ValidateBlockDescription(AErrorMessages: TStrings): Boolean;
    function ValidateMaxWaterAllocation(AErrorMessages: TStrings): Boolean;
    function ValidateFileName(AErrorMessages: TStrings): Boolean;
    function ValidateHydrologyNodeNumber(AErrorMessages: TStrings): Boolean;
    function ValidateCanalTransportLoss(AErrorMessages: TStrings): Boolean;
    function ValidateEfficiencyFactor(AErrorMessages: TStrings): Boolean;
    function ValidateReturnFlowFactor(AErrorMessages: TStrings): Boolean;
    function ValidateNumberOfCropTypes(AErrorMessages: TStrings): Boolean;
    function ValidateDiversionUpstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateReturnFlowDownstreamNode(AErrorMessages : TStrings): WordBool;
    function ValidateRainAboveRainFactorSpecValue(AErrorMessages: TStrings): Boolean;
    function ValidateRainBelowRainFactor(AErrorMessages: TStrings): Boolean;
    function ValidateRainCatchmentScalingFactor(AErrorMessages: TStrings): Boolean;
    function ValidateDroughtApplicable(AErrorMessages: TStrings): Boolean;
    function ValidateCropWaterUseType(AErrorMessages: TStrings): Boolean;

    function ValidateCurtailIrrigationAbstraction(AErrorMessages: TStrings): Boolean;
    function ValidateCanalSeepageLoss(AErrorMessages: TStrings): Boolean;
    function ValidateCanalTransmissionLoss(AErrorMessages: TStrings): Boolean;
    function ValidateUpperSoilOutflow(AErrorMessages: TStrings): Boolean;
    function ValidateMaxUpperZoneMoisture(AErrorMessages: TStrings): Boolean;
    function ValidateMinUpperZoneMoisture(AErrorMessages: TStrings): Boolean;
    function ValidateCropTypesCount(AErrorMessages: TStrings): Boolean;
    function ValidateIrrigationSupplyCapacity(AErrorMessages: TStrings): Boolean;
    function ValidateAllocatedAreaPointsCount(AErrorMessages: TStrings): Boolean;
    function ValidateMethodIrrigatedAreas(AErrorMessages: TStrings): Boolean;
    function ValidateMaxWaterAllocationCount(AErrorMessages: TStrings): Boolean;
    function ValidateMethodMaxWaterAllocation(AErrorMessages: TStrings): Boolean;
    function ValidateReturnFlowVolumePointsCount(AErrorMessages: TStrings): Boolean;
    function ValidateMethodReturnFlowVolume(AErrorMessages: TStrings): Boolean;
    function ValidateSupplyCapacityPointsCount(AErrorMessages: TStrings): Boolean;
    function ValidateMethodSupplyCapacity(AErrorMessages: TStrings): Boolean;
    function ValidateIrrigationEfficienciesPointsCount(AErrorMessages: TStrings): Boolean;
    function ValidateMethodIrrigationEfficiencies(AErrorMessages: TStrings): Boolean;
    function ValidateReturnFlowFactorsCount(AErrorMessages: TStrings): Boolean;
    function ValidateMethodReturnFlowFactors(AErrorMessages: TStrings): Boolean;


    function AddMonthlyWaterUse(AFeature : TWaterUsage): Boolean;
    function CreateNewIrrigationBlockWaterUse             : TWaterUsage; safecall;
    function Get_WaterUsageFactorByIndex(AIndex: Integer) : IWaterUsage; safecall;
    function Get_WaterUsageFactorByID(AIrrigationBlockWaterUsageID: Integer) : IWaterUsage; safecall;
  public
    procedure Assign(ASource: TIrrigationBlock);
    function Initialise : Boolean; override;
    function PopulateIDs(ABlockID : Integer; ABlockName : string): boolean;
    function Populate ( AUpperZoneReturnFlow,
                        ALowerZoneReturnFlow,
                        AReturnFlowLoss,
                        AMultiplicationFactor,
                        AUpperZoneSoilMoistureCapacity,
                        ALowerZoneSoilMoistureCapacity,
                        AUpperZoneSoilMoistureTarget,
                        AInitialSoilMoistureStorage : Double;

                            // Evaporation/Climate
                        ARainfallFactor,
                        APanEvaporation,
                        AAPanConvFactor : TMonthlyDoubleArray;

                            // Irrigation
                        ABlockNodeNumber     : Integer;
                        ABlockName,
                        ADescription         : WideString;
                        AMaxWaterAllocation  : Double;
                        AFileName            : WideString;
                        AHydrologyNodeNumber          : Integer;

                        ACanalTransportLoss,
                        AEfficiencyFactor,
                        AReturnFlowFactor              : Double;
                        ANumberOfCropTypes             : Integer;
                        ARainAboveRainFactorSpecValue,
                        ARainBelowRainFactor,
                        ARainCatchmentScalingFactor,
                        AAllocatedIrrigationArea       : Double;
                        ABlockIdentifier               : Integer;
                        ADiversionChannelNr            : Integer;
                        AReturnFlowChannelNr           : Integer;
                        ADroughtApplicable             : Integer;
                        ACropWaterUseType              : Integer
                        ): WordBool;

    function PopulateType4Details(
                        AIrrigationBlockType : integer;
                        ACurtailIrrigationAbstraction : integer;
                        ACanalSeepageLoss : double;
                        ACanalTransmissionLoss : double;
                        AUpperSoilOutflow : double;
                        AMultiplicationFactor : double;
                        AMaxUpperZoneMoisture : double;
                        AMinUpperZoneMoisture : double;
                        ACropTypesCount : integer;
                        AIrrigationSupplyCapacity : double;
                        AAllocatedAreaPointsCount : integer;
                        AMethodIrrigatedAreas : integer;
                        AMaxWaterAllocationCount : integer;
                        AMethodMaxWaterAllocation : integer;
                        AReturnFlowVolumePointsCount : integer;
                        AMethodReturnFlowVolume : integer;
                        ASupplyCapacityPointsCount : integer;
                        AMethodSupplyCapacity : integer;
                        AIrrigationEfficienciesPointsCount,
                        AMethodIrrigationEfficiencies : integer;
                        AReturnFlowFactorsCount : integer;
                        AMethodReturnFlowFactors : integer;

             
                        AIrrigatedAreasBreakPointYearCommaText : string;
                        AIrrigatedAreaCommaText : string;
                        AMaximumWaterAllocationBreakPointYearCommaText : string;
                        AMaximumWaterAllocationCommaText : string;
                        AMaximumWaterAllocationGrowthCommaText : string;
                        AReturnFlowVolumeBreakpointYearCommaText : string;
                        AReturnFlowVolumeCommaText : string;
                        ASupplyCapacityBreakpointYearCommaText : string;
                        ASupplyCapacityCommaText : string;
                        AIrrigationEfficiencyBreakpointYearCommaText : string;
                        AIrrigationEfficiencyCommaText : string;
                        AReturnFlowFactorBreakpointYearCommaText : string;
                        AReturnFlowFactorsCommaText : string
                                        ): WordBool;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CreateWaterUse : IWaterUsage; safecall;
    function NewWaterUse : TWaterUsage; safecall;

    function DeleteWaterUseWithID(AIrrigationBlockWaterUsageID : Integer) : WordBool;
    function DeleteWaterUseWithIndex(AIndex : Integer) : WordBool;
    function RemoveWaterUse(AIrrigationBlockWaterUsageID : Integer) : WordBool; safecall;
    function WaterUsageFactorByName(const AName: WideString)               : IWaterUsage; safecall;
    function CastIrrigationBlockWaterUsageByID(AIrrigationBlockWaterUsageID: Integer)     : TWaterUsage;
    function CastIrrigationBlockWaterUsageByIndex(AIndex: Integer)     : TWaterUsage;

    //_______________________________NEW BEGIN______________________________________________________________________
    function Get_IrrigatedAreasBreakPointYearCommaText : string;
    function Get_IrrigatedAreaCommaText : string;
    function Get_MaximumWaterAllocationBreakPointYearCommaText : string;
    function Get_MaximumWaterAllocationCommaText : string;
    function Get_MaximumWaterAllocationGrowthCommaText : string;
    function Get_ReturnFlowVolumeBreakpointYearCommaText : string;
    function Get_ReturnFlowVolumeCommaText : string;
    function Get_SupplyCapacityBreakpointYearCommaText : string;
    function Get_SupplyCapacityCommaText : string;
    function Get_IrrigationEfficiencyBreakpointYearCommaText : string;
    function Get_IrrigationEfficiencyCommaText : string;
    function Get_ReturnFlowFactorBreakpointYearCommaText : string;
    function Get_ReturnFlowFactorsCommaText : string;
    //_______________________________NEW END________________________________________________________________________


    property WaterUsageFactorByIndex[AIndex : Integer]                     : IWaterUsage read Get_WaterUsageFactorByIndex;
    property WaterUsageFactorByID[AIrrigationBlockWaterUsageID : Integer]  : IWaterUsage read Get_WaterUsageFactorByID;

    // Soil Properties
    property UpperZoneReturnFlow           : Double read Get_UpperZoneReturnFlow write Set_UpperZoneReturnFlow;
    property LowerZoneReturnFlow           : Double read Get_LowerZoneReturnFlow write Set_LowerZoneReturnFlow;
    property ReturnFlowLoss                : Double read Get_ReturnFlowLoss write Set_ReturnFlowLoss;
    property UpperZoneSoilMoistureCapacity : Double read Get_UpperZoneSoilMoistureCapacity write Set_UpperZoneSoilMoistureCapacity;
    property LowerZoneSoilMoistureCapacity : Double read Get_LowerZoneSoilMoistureCapacity write Set_LowerZoneSoilMoistureCapacity;
    property UpperZoneSoilMoistureTarget   : Double read Get_UpperZoneSoilMoistureTarget write Set_UpperZoneSoilMoistureTarget;
    property InitialSoilMoistureStorage    : Double read Get_InitialSoilMoistureStorage write Set_InitialSoilMoistureStorage;

    // Evaporation/Climate
    property RainfallFactor[AMonth: Integer] : Double read Get_RainfallFactor write Set_RainfallFactor;
    property PanEvaporation[AMonth: Integer] : Double read Get_PanEvaporation write Set_PanEvaporation;
    property APanConvFactor[AMonth: Integer] : Double read Get_APanConvFactor write Set_APanConvFactor;

    // Irrigation
    property BlockNodeNumber               : Integer     read Get_BlockNodeNumber write Set_BlockNodeNumber;
    property BlockName                     : WideString  read Get_BlockName write Set_BlockName;
    property BlockDescription              : WideString  read Get_BlockDescription write Set_BlockDescription;
    property MaxWaterAllocation            : Double      read Get_MaxWaterAllocation write Set_MaxWaterAllocation;
    property FileName                      : WideString  read Get_FileName write Set_FileName;
    property HydrologyNodeNumber           : Integer     read Get_HydrologyNodeNumber write Set_HydrologyNodeNumber;

    property CanalTransportLoss            : Double  read Get_CanalTransportLoss write Set_CanalTransportLoss;
    property EfficiencyFactor              : Double  read Get_EfficiencyFactor write Set_EfficiencyFactor;
    property ReturnFlowFactor              : Double  read Get_ReturnFlowFactor write Set_ReturnFlowFactor;

    property NumberOfCropTypes             : Integer read Get_NumberOfCropTypes write Set_NumberOfCropTypes;

    property RainAboveRainFactorSpecValue  : Double  read Get_RainAboveRainFactorSpecValue write Set_RainAboveRainFactorSpecValue;
    property RainBelowRainFactor           : Double  read Get_RainBelowRainFactor write Set_RainBelowRainFactor;
    property RainCatchmentScalingFactor    : Double  read Get_RainCatchmentScalingFactor write Set_RainCatchmentScalingFactor;
    property AllocatedIrrigationArea       : Double  read Get_AllocatedIrrigationArea write Set_AllocatedIrrigationArea;
    property BlockIdentifier               : Integer read Get_Identifier;// write Set_BlockIdentifier;

    property WaterUsageCount               : Integer read Get_WaterUsageCount;
    property DiversionChannelNr            : Integer read FDiversionChannelNr;
    property ReturnFlowChannelNr           : Integer read FReturnFlowChannelNr;
    property DroughtApplicable             : Integer read Get_DroughtApplicable write Set_DroughtApplicable;
    property CropWaterUseType              : Integer read Get_CropWaterUseType  write Set_CropWaterUseType;

    property DiversionChannel              : IGeneralFlowChannel  read Get_DiversionChannel;
    property ReturnFlowChannel             : IGeneralFlowChannel  read Get_ReturnFlowChannel;
    property DiversionChannelMaxDemand     : TDiversionChannelMaxDemand read FDiversionChannelMaxDemand;

    //_______________________________NEW BEGIN______________________________________________________________________

    property IrrigationBlockType : integer read Get_IrrigationBlockType write Set_IrrigationBlockType;
    property CurtailIrrigationAbstraction : integer read Get_CurtailIrrigationAbstraction write Set_CurtailIrrigationAbstraction;
    property CanalSeepageLoss : double read Get_CanalSeepageLoss write Set_CanalSeepageLoss;
    property CanalTransmissionLoss : double read Get_CanalTransmissionLoss write Set_CanalTransmissionLoss;
    property UpperSoilOutflow : double read Get_UpperSoilOutflow write Set_UpperSoilOutflow;
    property MaxUpperZoneMoisture : double read Get_MaxUpperZoneMoisture write Set_MaxUpperZoneMoisture;
    property MinUpperZoneMoisture : double read Get_MinUpperZoneMoisture write Set_MinUpperZoneMoisture;
    property CropTypesCount : integer read Get_CropTypesCount write Set_CropTypesCount;
    property IrrigationSupplyCapacity : double read Get_IrrigationSupplyCapacity write Set_IrrigationSupplyCapacity;

    property AllocatedAreaPointsCount : integer read Get_AllocatedAreaPointsCount write Set_AllocatedAreaPointsCount;

    property MethodIrrigatedAreas : integer read Get_MethodIrrigatedAreas write Set_MethodIrrigatedAreas;
    property MaxWaterAllocationCount : integer read Get_MaxWaterAllocationCount write Set_MaxWaterAllocationCount;

    property MethodMaxWaterAllocation : integer read Get_MethodMaxWaterAllocation write Set_MethodMaxWaterAllocation;
    property ReturnFlowVolumePointsCount : integer read Get_ReturnFlowVolumePointsCount write Set_ReturnFlowVolumePointsCount;

    property MethodReturnFlowVolume : integer read Get_MethodReturnFlowVolume write Set_MethodReturnFlowVolume;
    property SupplyCapacityPointsCount : integer read Get_SupplyCapacityPointsCount write Set_SupplyCapacityPointsCount;
    property MethodSupplyCapacity : integer read Get_MethodSupplyCapacity write Set_MethodSupplyCapacity;
    property IrrigationEfficienciesPointsCount : integer read Get_IrrigationEfficienciesPointsCount write Set_IrrigationEfficienciesPointsCount;

    property MethodIrrigationEfficiencies : integer read Get_MethodIrrigationEfficiencies write Set_MethodIrrigationEfficiencies;

    property ReturnFlowFactorsCount : integer read Get_ReturnFlowFactorsCount write Set_ReturnFlowFactorsCount;

    property MethodReturnFlowFactors : integer read Get_MethodReturnFlowFactors write Set_MethodReturnFlowFactors;

    property IrrigatedAreasBreakPointCount             : integer read Get_IrrigatedAreasBreakPointCount        ;
    property MaximumWaterAllocationBreakPointCount     : integer read Get_MaximumWaterAllocationBreakPointCount;
    property ReturnFlowVolumeBreakPointsCount          : integer read Get_ReturnFlowVolumeBreakPointsCount     ;
    property SupplyCapacityBreakPointsCount            : integer read Get_SupplyCapacityBreakPointsCount       ;
    property IrrigationEfficiencyBreakPointsCount      : integer read Get_IrrigationEfficiencyBreakPointsCount ;
    property ReturnFlowFactorBreakPointsCount          : integer read Get_ReturnFlowFactorBreakPointsCount     ;




    property IrrigatedAreasBreakPointYearByIndex[AIndex : integer]         : integer read Get_IrrigatedAreasBreakPointYearByIndex         write Set_IrrigatedAreasBreakPointYearByIndex;
    property IrrigatedAreaByIndex[AIndex : integer]                        : double  read Get_IrrigatedAreaByIndex                        write Set_IrrigatedAreaByIndex;
    property MaximumWaterAllocationBreakPointYearByIndex[AIndex : integer] : integer read Get_MaximumWaterAllocationBreakPointYearByIndex write Set_MaximumWaterAllocationBreakPointYearByIndex;
    property MaximumWaterAllocationByIndex[AIndex : integer]               : double  read Get_MaximumWaterAllocationByIndex               write Set_MaximumWaterAllocationByIndex;
    property MaximumWaterAllocationGrowthByIndex[AIndex : integer]         : double  read Get_MaximumWaterAllocationGrowthByIndex         write Set_MaximumWaterAllocationGrowthByIndex;
    property ReturnFlowVolumeBreakpointYearByIndex[AIndex : integer]       : integer read Get_ReturnFlowVolumeBreakpointYearByIndex       write Set_ReturnFlowVolumeBreakpointYearByIndex;
    property ReturnFlowVolumeByIndex[AIndex : integer]                     : double  read Get_ReturnFlowVolumeByIndex                     write Set_ReturnFlowVolumeByIndex;
    property SupplyCapacityBreakpointYearByIndex[AIndex : integer]         : integer read Get_SupplyCapacityBreakpointYearByIndex         write Set_SupplyCapacityBreakpointYearByIndex;
    property SupplyCapacityByIndex[AIndex : integer]                       : double  read Get_SupplyCapacityByIndex                       write Set_SupplyCapacityByIndex;
    property IrrigationEfficiencyBreakpointYearByIndex[AIndex : integer]   : integer read Get_IrrigationEfficiencyBreakpointYearByIndex   write Set_IrrigationEfficiencyBreakpointYearByIndex;
    property IrrigationEfficiencyByIndex[AIndex : integer]                 : double  read Get_IrrigationEfficiencyByIndex                 write Set_IrrigationEfficiencyByIndex;
    property ReturnFlowFactorBreakpointYearByIndex[AIndex : integer]       : integer read Get_ReturnFlowFactorBreakpointYearByIndex       write Set_ReturnFlowFactorBreakpointYearByIndex;
    property ReturnFlowFactorsByIndex[AIndex : integer]                    : double  read Get_ReturnFlowFactorsByIndex                    write Set_ReturnFlowFactorsByIndex;
    property MultiplicationFactor                                          : Double  read Get_MultiplicationFactor                        write Set_MultiplicationFactor;


    //_______________________________NEW END________________________________________________________________________
  end;



  TIrrigationBlockList = class(TAbstractAppObject, IIrrigationBlockList)
  protected
    FIrrigationBlockList  : TObjectList;
    FIrrigationBlockCount : Integer;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function ValidateIrrigationBlockCount (AErrorMessages : TStrings) : Boolean;
    function AddIrrigationBlock(AFeature : TIrrigationBlock): boolean;

    function CreateNewIrrigationBlock(ABlockType : integer): TIrrigationBlock;

    function DeleteIrrigationBlockWithID(ABlockIdentifier : integer) : WordBool;
    function DeleteIrrigationBlockWithIndex(AIndex : integer) : WordBool;
    function Get_IrrigationBlockCount: Integer; safecall;
    function GetDiversionChannelID(ABlockIdentifier : integer): integer;
    function GetReturnFlowChannelID(ABlockIdentifier : integer): integer;

    function Get_IrrigationBlockByBlockNodeNumber(AIrrigationBlockNodeNumber: Integer): IIrrigationBlock; safecall;
    function Get_IrrigationBlockByID(ABlockIdentifier: integer): IIrrigationBlock; safecall;
    function Get_IrrigationBlockByIndex(AIndex: integer): IIrrigationBlock; safecall;
  public
    property IrrigationBlockCount : integer read Get_IrrigationBlockCount;
    function Initialise : boolean; override;

    function NewIrrigationBlock : TIrrigationBlock;
    function CreateIrrigationBlock(ABlockType : integer) : IIrrigationBlock; safecall;
    function CopyCreate(ABLockID : integer) : IIrrigationBlock; safecall;
    function RemoveIrrigationBlock (AIrrigationBlockNodeNr : integer) : WordBool; safecall;

    function CastIrrigationBlockByID(ABlockIdentifier: integer): TIrrigationBlock;
    function CastIrrigationBlockByIndex(AIndex : integer): TIrrigationBlock;
    function CastIrrigationBlockByBlockNodeNumber(ABlockNodeNumber: Integer): TIrrigationBlock;
    function IrrigationBlockByName(const AName: WideString)                 : IIrrigationBlock; safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    procedure GetChannelNumbers(ABlockIdentifier: integer;var ADiversionChannelNumber, AReturnFlowChannelNumber: integer);
    function GetIrrigationBlocksPerHydrologyNodeNumber(AHydrologyNodeNumber: integer; AIrrigationBlocksList: TObjectList): boolean; virtual;

    property IrrigationBlockByIndex[AIndex : Integer]                       : IIrrigationBlock read Get_IrrigationBlockByIndex;
    property IrrigationBlockByID[ABlockIdentifier : Integer]              : IIrrigationBlock read Get_IrrigationBlockByID;
    property IrrigationBlockByBlockNodeNumber[AIrrigationBlockNodeNumber : Integer] : IIrrigationBlock read Get_IrrigationBlockByBlockNodeNumber;
  end;


implementation

uses
  System.UITypes,
  SysUtils,
  Math,
  VCL.Controls,
  UUtilities,
  Windows,
  UMainMenuEventType,
  UIrrigationBlockSQLAgent,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UNetworkElementData,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;

{ TIrrigationBlock }

procedure TIrrigationBlock.CreateMemberObjects;
const OPNAME = 'TIrrigationBlock.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FWaterUsageFactor := TObjectList.Create(True);
    FDiversionChannelMaxDemand     := TDiversionChannelMaxDemand.Create(FAppModules);


    FIrrigatedAreasBreakPointYear         := TStringList.Create;
    FIrrigatedArea                        := TStringList.Create;
    FMaximumWaterAllocationBreakPointYear := TStringList.Create;
    FMaximumWaterAllocation               := TStringList.Create;
    FMaximumWaterAllocationGrowth         := TStringList.Create;
    FReturnFlowVolumeBreakpointYear       := TStringList.Create;
    FReturnFlowVolume                     := TStringList.Create;
    FSupplyCapacityBreakpointYear         := TStringList.Create;
    FSupplyCapacity                       := TStringList.Create;
    FIrrigationEfficiencyBreakpointYear   := TStringList.Create;
    FIrrigationEfficiency                 := TStringList.Create;
    FReturnFlowFactorBreakpointYear       := TStringList.Create;
    FReturnFlowFactors                    := TStringList.Create;

    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlock.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FWaterUsageFactor);
    FreeAndNil(FDiversionChannelMaxDemand);
    FreeAndNil(FIrrigatedAreasBreakPointYear);
    FreeAndNil(FIrrigatedArea);
    FreeAndNil(FMaximumWaterAllocationBreakPointYear);
    FreeAndNil(FMaximumWaterAllocation);
    FreeAndNil(FMaximumWaterAllocationGrowth);
    FreeAndNil(FReturnFlowVolumeBreakpointYear);
    FreeAndNil(FReturnFlowVolume);
    FreeAndNil(FSupplyCapacityBreakpointYear);
    FreeAndNil(FSupplyCapacity);
    FreeAndNil(FIrrigationEfficiencyBreakpointYear);
    FreeAndNil(FIrrigationEfficiency);
    FreeAndNil(FReturnFlowFactorBreakpointYear);
    FreeAndNil(FReturnFlowFactors);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Initialise: boolean;
const OPNAME = 'TIrrigationBlock.Initialise';
var
  LCount : Integer;
begin
  Result := inherited Initialise;
  try
    FUpperZoneReturnFlow           := 0;
    FLowerZoneReturnFlow           := 0;
    FReturnFlowLoss                := 0;
    FUpperZoneSoilMoistureCapacity := 0;
    FLowerZoneSoilMoistureCapacity := 0;
    FUpperZoneSoilMoistureTarget   := 0;
    FInitialSoilMoistureStorage    := 0;

    for LCount := MinMonths to MaxMonths do
    begin
      FRainfallFactor[LCount] := 1.0;
      FPanEvaporation[LCount] := 1.0;
      FAPanConvFactor[LCount] := 1.0;
    end;
        // Irrigation
    FBlockNodeNumber               := NullInteger;
    FBlockName                     := '';
    FBlockDescription              := '';
    FMaxWaterAllocation            := 0;
    FFileName                      := '';
    FHydrologyNodeNumber           := 0;

    FCanalTransportLoss            := 0;
    FEfficiencyFactor              := 0;
    FReturnFlowFactor              := 0;

    FNumberOfCropTypes             := 0;

    FRainAboveRainFactorSpecValue  := 0;
    FRainBelowRainFactor           := 0;
    FRainCatchmentScalingFactor    := 0;
    FAllocatedIrrigationArea       := 0;
    FIdentifier                    := NullInteger;

    FDiversionChannelNr            := 0;
    FReturnFlowChannelNr           := 0;
    FDroughtApplicable             := 0;
    FCropWaterUseType              := 1;

    FWaterUsageFactor.Clear;
    FDiversionChannelMaxDemand.Initialise;
    //FWaterUsageFactor[LCount].Initialise;
    //_______________________________NEW____________________________________________________________________________
    FIrrigationBlockType                := NullInteger;
    FCurtailIrrigationAbstraction  := NullInteger;
    FCanalSeepageLoss              := NullFloat;
    FCanalTransmissionLoss         := NullFloat;
    FUpperSoilOutflow              := NullFloat;
    FMaxUpperZoneMoisture          := NullFloat;
    FMinUpperZoneMoisture          := NullFloat;
    FCropTypesCount                := NullInteger;
    FIrrigationSupplyCapacity      := NullFloat;
    FAllocatedAreaPointsCount      := NullInteger;
    FMethodIrrigatedAreas          := NullInteger;
    FMaxWaterAllocationCount       := NullInteger;
    FMethodMaxWaterAllocation      := NullInteger;
    FReturnFlowVolumePointsCount   := NullInteger;
    FMethodReturnFlowVolume        := NullInteger;
    FSupplyCapacityPointsCount     := NullInteger;
    FMethodSupplyCapacity          := NullInteger;
    FIrrigationEfficienciesPointsCount := NullInteger;
    FMethodIrrigationEfficiencies  := NullInteger;
    FReturnFlowFactorsCount        := NullInteger;
    FMethodReturnFlowFactors       := NullInteger;

    FIrrigatedAreasBreakPointYear.Clear;
    FIrrigatedArea.Clear;
    FMaximumWaterAllocationBreakPointYear.Clear;
    FMaximumWaterAllocation.Clear;
    FMaximumWaterAllocationGrowth.Clear;
    FReturnFlowVolumeBreakpointYear.Clear;
    FReturnFlowVolume.Clear;
    FSupplyCapacityBreakpointYear.Clear;
    FSupplyCapacity.Clear;
    FIrrigationEfficiencyBreakpointYear.Clear;
    FIrrigationEfficiency.Clear;
    FReturnFlowFactorBreakpointYear.Clear;
    FReturnFlowFactors.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.PopulateIDs(ABlockID : Integer; ABlockName: string): boolean;
const OPNAME = 'TIrrigationBlock.PopulateIDs';
begin
  Result := False;
  try
    FIdentifier       := ABlockID;
    FBlockName        := ABlockName;
    FBlockDescription := ABlockName;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Populate(AUpperZoneReturnFlow,
                        ALowerZoneReturnFlow,
                        AReturnFlowLoss,
                        AMultiplicationFactor,
                        AUpperZoneSoilMoistureCapacity,
                        ALowerZoneSoilMoistureCapacity,
                        AUpperZoneSoilMoistureTarget,
                        AInitialSoilMoistureStorage : Double;

                            // Evaporation/Climate
                        ARainfallFactor,
                        APanEvaporation,
                        AAPanConvFactor      : TMonthlyDoubleArray;

                            // Irrigation
                        ABlockNodeNumber     : Integer;
                        ABlockName,
                        ADescription         : WideString;
                        AMaxWaterAllocation  : Double;
                        AFileName            : WideString;
                        AHydrologyNodeNumber          : Integer;

                        ACanalTransportLoss,
                        AEfficiencyFactor,
                        AReturnFlowFactor              : Double;
                        ANumberOfCropTypes             : Integer;
                        ARainAboveRainFactorSpecValue,
                        ARainBelowRainFactor,
                        ARainCatchmentScalingFactor,
                        AAllocatedIrrigationArea       : Double;
                        ABlockIdentifier               : Integer;
                        ADiversionChannelNr            : Integer;
                        AReturnFlowChannelNr           : Integer;
                        ADroughtApplicable             : Integer;
                        ACropWaterUseType              : Integer
                        ): WordBool;
const OPNAME = 'TIrrigationBlock.Populate';
var
  LCount : Integer;
begin
  Result := FALSE;
  try
    FUpperZoneReturnFlow           := AUpperZoneReturnFlow;
    FLowerZoneReturnFlow           := ALowerZoneReturnFlow;
    FReturnFlowLoss                := AReturnFlowLoss;
    FMultiplicationFactor          := AMultiplicationFactor;
    FUpperZoneSoilMoistureCapacity := AUpperZoneSoilMoistureCapacity;
    FLowerZoneSoilMoistureCapacity := ALowerZoneSoilMoistureCapacity;
    FUpperZoneSoilMoistureTarget   := AUpperZoneSoilMoistureTarget;
    FInitialSoilMoistureStorage    := AInitialSoilMoistureStorage;

    // Evaporation/Climate
    for LCount := MinMonths to MaxMonths do
    begin
      FRainfallFactor[LCount]      := ARainfallFactor[LCount];
      FPanEvaporation[LCount]      := APanEvaporation[LCount];
      FAPanConvFactor[LCount]      := AAPanConvFactor[LCount];

    end;

    // Irrigation
    FBlockNodeNumber               := ABlockNodeNumber;
    FBlockName                     := ABlockName;
    FBlockDescription              := ADescription;
    FMaxWaterAllocation            := AMaxWaterAllocation;
    FFileName                      := AFileName;
    FHydrologyNodeNumber           := AHydrologyNodeNumber;

    FCanalTransportLoss            := ACanalTransportLoss;
    FEfficiencyFactor              := AEfficiencyFactor;
    FReturnFlowFactor              := AReturnFlowFactor;
    FNumberOfCropTypes             := ANumberOfCropTypes;
    FRainAboveRainFactorSpecValue  := ARainAboveRainFactorSpecValue;
    FRainBelowRainFactor           := ARainBelowRainFactor;
    FRainCatchmentScalingFactor    := ARainCatchmentScalingFactor;
    FAllocatedIrrigationArea       := AAllocatedIrrigationArea;
    FIdentifier                    := ABlockIdentifier;

    FDiversionChannelNr            := ADiversionChannelNr;
    FReturnFlowChannelNr           := AReturnFlowChannelNr;
    FDroughtApplicable             := ADroughtApplicable;
    FCropWaterUseType              := ACropWaterUseType;
    FDiversionChannelMaxDemand.FIrrigationBlockNumber := ABlockNodeNumber;
    FDiversionChannelMaxDemand.FChannelNumber := ADiversionChannelNr;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.PopulateType4Details(
         AIrrigationBlockType, ACurtailIrrigationAbstraction: integer;
         ACanalSeepageLoss, ACanalTransmissionLoss, AUpperSoilOutflow, AMultiplicationFactor,AMaxUpperZoneMoisture,
         AMinUpperZoneMoisture: double; ACropTypesCount: integer; AIrrigationSupplyCapacity: double;
         AAllocatedAreaPointsCount, AMethodIrrigatedAreas, AMaxWaterAllocationCount, AMethodMaxWaterAllocation,
         AReturnFlowVolumePointsCount, AMethodReturnFlowVolume, ASupplyCapacityPointsCount, AMethodSupplyCapacity,
         AIrrigationEfficienciesPointsCount,AMethodIrrigationEfficiencies, AReturnFlowFactorsCount, AMethodReturnFlowFactors: integer;

        AIrrigatedAreasBreakPointYearCommaText : string;
        AIrrigatedAreaCommaText : string;
        AMaximumWaterAllocationBreakPointYearCommaText : string;
        AMaximumWaterAllocationCommaText : string;
        AMaximumWaterAllocationGrowthCommaText : string;
        AReturnFlowVolumeBreakpointYearCommaText : string;
        AReturnFlowVolumeCommaText : string;
        ASupplyCapacityBreakpointYearCommaText : string;
        ASupplyCapacityCommaText : string;
        AIrrigationEfficiencyBreakpointYearCommaText : string;
        AIrrigationEfficiencyCommaText : string;
        AReturnFlowFactorBreakpointYearCommaText : string;
        AReturnFlowFactorsCommaText : string ): WordBool;
const OPNAME = 'TIrrigationBlock.PopulateType4Details';
begin
  Result := FALSE;
  try
    FIrrigationBlockType                :=  AIrrigationBlockType;
    FCurtailIrrigationAbstraction  :=  ACurtailIrrigationAbstraction;
    FCanalSeepageLoss              :=  ACanalSeepageLoss;
    FCanalTransmissionLoss         :=  ACanalTransmissionLoss;
    FUpperSoilOutflow              :=  AUpperSoilOutflow;
    FMultiplicationFactor          :=  AMultiplicationFactor;
    FMaxUpperZoneMoisture          :=  AMaxUpperZoneMoisture;
    FMinUpperZoneMoisture          :=  AMinUpperZoneMoisture;
    FCropTypesCount                :=  ACropTypesCount;
    FIrrigationSupplyCapacity      :=  AIrrigationSupplyCapacity;
    FAllocatedAreaPointsCount      :=  AAllocatedAreaPointsCount;
    FMethodIrrigatedAreas          :=  AMethodIrrigatedAreas;
    FMaxWaterAllocationCount       :=  AMaxWaterAllocationCount;
    FMethodMaxWaterAllocation      :=  AMethodMaxWaterAllocation;
    FReturnFlowVolumePointsCount   :=  AReturnFlowVolumePointsCount;
    FMethodReturnFlowVolume        :=  AMethodReturnFlowVolume;
    FSupplyCapacityPointsCount     :=  ASupplyCapacityPointsCount;
    FMethodSupplyCapacity          :=  AMethodSupplyCapacity;
    FIrrigationEfficienciesPointsCount := AIrrigationEfficienciesPointsCount;
    FMethodIrrigationEfficiencies  :=  AMethodIrrigationEfficiencies;
    FReturnFlowFactorsCount        :=  AReturnFlowFactorsCount;
    FMethodReturnFlowFactors       :=  AMethodReturnFlowFactors;

    FIrrigatedAreasBreakPointYear.CommaText         := AIrrigatedAreasBreakPointYearCommaText;
    FIrrigatedArea.CommaText                        := AIrrigatedAreaCommaText;
    FMaximumWaterAllocationBreakPointYear.CommaText := AMaximumWaterAllocationBreakPointYearCommaText;
    FMaximumWaterAllocation.CommaText               := AMaximumWaterAllocationCommaText;
    FMaximumWaterAllocationGrowth.CommaText         := AMaximumWaterAllocationGrowthCommaText;
    FReturnFlowVolumeBreakpointYear.CommaText       := AReturnFlowVolumeBreakpointYearCommaText;
    FReturnFlowVolume.CommaText                     := AReturnFlowVolumeCommaText;
    FSupplyCapacityBreakpointYear.CommaText         := ASupplyCapacityBreakpointYearCommaText;
    FSupplyCapacity.CommaText                       := ASupplyCapacityCommaText;
    FIrrigationEfficiencyBreakpointYear.CommaText   := AIrrigationEfficiencyBreakpointYearCommaText;
    FIrrigationEfficiency.CommaText                 := AIrrigationEfficiencyCommaText;
    FReturnFlowFactorBreakpointYear.CommaText       := AReturnFlowFactorBreakpointYearCommaText;
    FReturnFlowFactors.CommaText                    := AReturnFlowFactorsCommaText;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TIrrigationBlock.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols : TStringList;
  lStopOnFirstError : Boolean;
begin
  Result := FALSE;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    :=  TStringList.Create;
    try
      if (AContext = 'IrrigationBlockName') then
        Result := ValidateName(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockAllocatedIrrigationArea') then
        Result := ValidateAllocatedIrrigationArea(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockUpperZoneReturnFlow') then
        Result := ValidateUpperZoneReturnFlow(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockLowerZoneReturnFlow') then
        Result := ValidateLowerZoneReturnFlow(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockReturnFlowLoss') then
        Result := ValidateReturnFlowLoss(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockUpperZoneSoilMoistureCapacity') then
        Result := ValidateUpperZoneSoilMoistureCapacity(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockLowerZoneSoilMoistureCapacity') then
        Result := ValidateLowerZoneSoilMoistureCapacity(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockUpperZoneSoilMoistureTarget') then
        Result := ValidateUpperZoneSoilMoistureTarget(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockInitialSoilMoistureStorage') then
        Result := ValidateInitialSoilMoistureStorage(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockBlockNumber') then
        Result := ValidateBlockNodeNumber(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockDescription') then
        Result := ValidateBlockDescription(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockMaxWaterAllocation') then
        Result := ValidateMaxWaterAllocation(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockFileName') then
        Result := ValidateFileName(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockNodeNumber') then
        Result := ValidateHydrologyNodeNumber(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockCanalTransportLoss') then
        Result := ValidateCanalTransportLoss(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockEfficiencyFactor') then
        Result := ValidateEfficiencyFactor(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockReturnFlowFactor') then
        Result := ValidateReturnFlowFactor(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockNumberOfCropTypes') then
        Result := ValidateNumberOfCropTypes(lErrorMessage)
      else
      if (AContext = 'DiversionUpstreamNode') then
        Result := ValidateDiversionUpstreamNode(lErrorMessage)
      else
      if (AContext = 'ReturnFlowDownstreamNode') then
        Result := ValidateReturnFlowDownstreamNode(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockRainAboveRainFactorSpecValue') then
        Result := ValidateRainAboveRainFactorSpecValue(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockRainBelowRainFactor') then
        Result := ValidateRainBelowRainFactor(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockRainCatchmentScalingFactor') then
        Result := ValidateRainCatchmentScalingFactor(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockDroughtApplicable') then
        Result := ValidateDroughtApplicable(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockCropWaterUseType') then
        Result := ValidateCropWaterUseType(lErrorMessage)
      else
      if (AContext = 'CanalTransmissionLoss') then
        Result := ValidateCanalTransmissionLoss(lErrorMessage)
      else
      if (AContext = 'SupplyCapacityPointsCount') then
        Result := ValidateSupplyCapacityPointsCount(lErrorMessage)
      else
      if (AContext = 'IrrigationEfficienciesPointsCount') then
        Result := ValidateIrrigationEfficienciesPointsCount(lErrorMessage)
      else

      if (AContext = 'IrrigationBlockAPanConvFactor') then
      begin
        Result := ValidateAPanConvFactor(LErrorMessage,LErrorCols);
        if (not Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMessage.Text
          else
            AErrors := AErrors + CTStringsSeparator + LErrorMessage.Text + CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
        end;
      end else
      if (AContext = 'IrrigationBlockRainfallFactor') then
      begin
        Result := ValidateRainfallFactor(LErrorMessage,LErrorCols);
        if (not Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMessage.Text
          else
            AErrors := AErrors + CTStringsSeparator + LErrorMessage.Text + CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
        end;
      end else
      if (AContext = 'IrrigationBlockPanEvaporation') then
      begin
        Result := ValidatePanEvaporation(LErrorMessage,LErrorCols);
        if (not Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMessage.Text
          else
            AErrors := AErrors + CTStringsSeparator + LErrorMessage.Text + CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
        end;
      end else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidateUpperZoneReturnFlow(lErrorMessage))          then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateLowerZoneReturnFlow(lErrorMessage))          then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateReturnFlowLoss(lErrorMessage))               then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateUpperZoneSoilMoistureCapacity(lErrorMessage))then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateLowerZoneSoilMoistureCapacity(lErrorMessage))then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateUpperZoneSoilMoistureTarget(lErrorMessage))  then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateInitialSoilMoistureStorage(lErrorMessage))   then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateBlockNodeNumber(lErrorMessage))                  then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateBlockDescription(lErrorMessage))                  then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateMaxWaterAllocation(lErrorMessage))           then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateFileName(lErrorMessage))                     then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateHydrologyNodeNumber(lErrorMessage))                   then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateNumberOfCropTypes(lErrorMessage))            then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateDiversionUpstreamNode(lErrorMessage))        then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateReturnFlowDownstreamNode(lErrorMessage))     then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateCanalTransportLoss(lErrorMessage))           then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateEfficiencyFactor(lErrorMessage))             then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateRainAboveRainFactorSpecValue(lErrorMessage)) then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateRainBelowRainFactor(lErrorMessage))          then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateRainCatchmentScalingFactor(lErrorMessage))   then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateReturnFlowFactor(lErrorMessage))             then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateDroughtApplicable(lErrorMessage))             then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
          if (not ValidateCropWaterUseType(lErrorMessage))             then Result := False;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateName';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockName', FBlockName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FBlockName + ':'+lMessage);
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateRainfallFactor(AErrorMessages : TStrings;
                                                                AErrorColumns  : TStringList): boolean;
const OPNAME = 'TIrrigationBlock.ValidateRainfallFactor';
var
  LFieldProperty : TAbstractFieldProperty;
  LMessage : string;
  LIndex : integer;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockRainfallFactor');
    if (LFieldProperty <> nil) then
    begin
      AErrorColumns.Clear;
      for lIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        lMessage := '';
        if (FRainfallFactor[lIndex] <> NullFloat) then
        begin
          if (not FAppModules.FieldProperties.ValidateFieldProperty
             ('IrrigationBlockRainfallFactor', FloatToStr(FRainfallFactor[LIndex]),
             lMessage, LIndex)) then
          begin
            AErrorMessages.Add('ERROR:' +LMessage);
            AErrorColumns.Add(IntToStr(LIndex));
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.ValidateAPanConvFactor(AErrorMessages: TStrings; AErrorColumns: TStringList): boolean;
const OPNAME = 'TIrrigationBlock.ValidateAPanConvFactor';
var
  LFieldProperty : TAbstractFieldProperty;
  LMessage : string;
  LIndex : integer;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockAPanConvFactor');
    if (LFieldProperty <> nil) then
    begin
      AErrorColumns.Clear;
      for lIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        lMessage := '';
        if (FAPanConvFactor[lIndex] <> NullFloat) then
        begin
          if (not FAppModules.FieldProperties.ValidateFieldProperty
             ('IrrigationBlockAPanConvFactor', FloatToStr(FAPanConvFactor[LIndex]),
             lMessage, LIndex)) then
          begin
            AErrorMessages.Add('ERROR:' +LMessage);
            AErrorColumns.Add(IntToStr(LIndex));
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.ValidatePanEvaporation(AErrorMessages: TStrings; AErrorColumns: TStringList): boolean;
const OPNAME = 'TIrrigationBlock.ValidatePanEvaporation';
var
  LFieldProperty : TAbstractFieldProperty;
  LMessage : string;
  LIndex : integer;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockPanEvaporation');
    if (LFieldProperty <> nil) then
    begin
      AErrorColumns.Clear;
      for lIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        lMessage := '';
        if (FPanEvaporation[lIndex] <> NullFloat) then
        begin
          if (not FAppModules.FieldProperties.ValidateFieldProperty(
                          'IrrigationBlockPanEvaporation',
                          FloatToStr(FPanEvaporation[LIndex]),
                          lMessage,
                          LIndex)) then
          begin
            AErrorMessages.Add('ERROR:' +LMessage);
            AErrorColumns.Add(IntToStr(LIndex));
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock._AddRef: Integer;
const OPNAME = 'TIrrigationBlock._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock._Release: Integer;
const OPNAME = 'TIrrigationBlock._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_AllocatedIrrigationArea: Double;
const OPNAME = 'TIrrigationBlock.Get_AllocatedIrrigationArea';
begin
  try
    Result := FAllocatedIrrigationArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_APanConvFactor(AMonth: Integer): Double;
const OPNAME = 'TIrrigationBlock.Get_APanConvFactor';
begin
  try
    Result := FAPanConvFactor[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_Identifier: Integer;
const OPNAME = 'TIrrigationBlock.Get_Identifier';
begin
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_BlockName: WideString;
const OPNAME = 'TIrrigationBlock.Get_BlockName';
begin
  try
    Result := FBlockName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_BlockNodeNumber: Integer;
const OPNAME = 'TIrrigationBlock.Get_BlockNodeNumber';
begin
  try
    Result := FBlockNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_CanalTransportLoss: Double;
const OPNAME = 'TIrrigationBlock.Get_CanalTransportLoss';
begin
  try
    Result := FCanalTransportLoss;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_EfficiencyFactor: Double;
const OPNAME = 'TIrrigationBlock.Get_EfficiencyFactor';
begin
  try
    Result := FEfficiencyFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_FileName: WideString;
const OPNAME = 'TIrrigationBlock.Get_FileName';
var
  LPath: string;
begin
  Result := '';
  try
    if (Trim(FFileName) <> '') then
    begin
      LPath := Trim(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath);
      Result := LPath + ExtractFileName(FFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_MaxWaterAllocation: Double;
const OPNAME = 'TIrrigationBlock.Get_MaxWaterAllocation';
begin
  try
    Result := FMaxWaterAllocation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_HydrologyNodeNumber: Integer;
const OPNAME = 'TIrrigationBlock.Get_HydrologyNodeNumber';
begin
  Result := NullInteger;
  try
    Result := FHydrologyNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_NumberOfCropTypes: Integer;
const OPNAME = 'TIrrigationBlock.Get_NumberOfCropTypes';
begin
  Result := 0;
  try
    if(FCropWaterUseType = 2) then
      Result := 1
    else
      Result := FNumberOfCropTypes;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_PanEvaporation(AMonth: Integer): Double;
const OPNAME = 'TIrrigationBlock.Get_PanEvaporation';
begin
  try
    Result := FPanEvaporation[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_RainAboveRainFactorSpecValue: Double;
const OPNAME = 'TIrrigationBlock.Get_RainAboveRainFactorSpecValue';
begin
  Result := NullFloat;
  try
    Result := FRainAboveRainFactorSpecValue;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_RainBelowRainFactor: Double;
const OPNAME = 'TIrrigationBlock.Get_RainBelowRainFactor';
begin
  Result := NullFloat;
  try
    Result := FRainBelowRainFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_RainCatchmentScalingFactor: Double;
const OPNAME = 'TIrrigationBlock.Get_RainCatchmentScalingFactor';
begin
  try
    Result := FRainCatchmentScalingFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_RainfallFactor(AMonth: Integer): Double;
const OPNAME = 'TIrrigationBlock.Get_RainfallFactor';
begin
  try
    Result := FRainfallFactor[AMonth];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowFactor: Double;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactor';
begin
  try
    Result := FReturnFlowFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowFactorBreakPointsCount: integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorBreakPointsCount';
begin
  Result := 0;
  try
    Result := FReturnFlowFactorBreakpointYear.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowFactorBreakpointYearByIndex(AIndex: integer): integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorBreakpointYearByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowFactorBreakpointYear.Count) then
      Result := StrToIntDef(FReturnFlowFactorBreakpointYear[AIndex],NullInteger);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TIrrigationBlock.Get_WaterUsageFactor( AIndex: Integer): TWaterUsage;
const OPNAME = 'TIrrigationBlock.Get_WaterUsageFactor';
begin
  if AIndex in [MinNoOfCrops..MaxNoOfCrops] then
    Result := FWaterUsageFactor[AIndex];
end;}

procedure TIrrigationBlock.Set_AllocatedIrrigationArea(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_AllocatedIrrigationArea';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FAllocatedIrrigationArea <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FAllocatedIrrigationArea);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockAllocatedIrrigationArea', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FAllocatedIrrigationArea := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockAllocatedIrrigationArea',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_APanConvFactor(AMonth: Integer; Value: Double);
const OPNAME = 'TIrrigationBlock.Set_APanConvFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FAPanConvFactor[AMonth] <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FAPanConvFactor[AMonth]);
        LLoadAgent.LoadContextData_Factor(LContextData, FloatToStr(FIdentifier),
                                          IntToStr(AMonth) );
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockAPanConvFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FAPanConvFactor[AMonth] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockBlockIdentifier',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_BlockIdentifier(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_BlockIdentifier';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FIdentifier <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FIdentifier);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockBlockIdentifier', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FIdentifier := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockBlockIdentifier',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_BlockName(const Value: WideString);
const OPNAME = 'TIrrigationBlock.Set_BlockName';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FBlockName <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FBlockName;
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockName', Value, LOldValue, LContextData ) then
        begin
          FBlockName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockName',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_BlockNodeNumber(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_BlockNodeNumber';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FBlockNodeNumber <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FBlockNodeNumber);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockBlockNumber', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FBlockNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockBlockNumber',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_CanalTransportLoss(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_CanalTransportLoss';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FCanalTransportLoss <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FCanalTransportLoss);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockCanalTransportLoss', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FCanalTransportLoss := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockCanalTransportLoss',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_EfficiencyFactor(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_EfficiencyFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FEfficiencyFactor <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FEfficiencyFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockEfficiencyFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FEfficiencyFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockEfficiencyFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_FileName(const Value: WideString);
const OPNAME = 'TIrrigationBlock.Set_FileName';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LDemandFileName,
  LMesg,
  LOldValue: string;
  LFileName: TAbstractModelFileName;
  LNewFileIndex: integer;
  LOldFileName : String;

begin
  try
    LDemandFileName    := ExtractFileName(Value);
    if (FFileName <> LDemandFileName) then
    begin
      LContextData := TStringList.Create;
      LLoadAgent   := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockFileName', LDemandFileName, FFileName, LContextData ) then
        begin
          if(LDemandFileName <> '') then
          begin
            LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.Count+1;
            LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.FindFile(LDemandFileName);
            if (LFileName = nil) then
              TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddDemandFileName(LNewFileIndex,LDemandFileName,False,0.0,
                FileLastWriteDate(LDemandFileName));
            LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.FindFile(LDemandFileName);
            if (LFileName <> nil) and (not LFileName.SavedInDB) then
            begin
              LMesg := FAppModules.Language.GetString('SelectedFile.ImportFile');
              if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
              begin
                LOldFileName := Value;
                CopyFile(PChar(LOldFileName), PChar(LFileName.FileName),False);
                FAppModules.Model.ProcessEvent(CmeImportDemandFile,LFileName);
              end;
            end;
          end;
          LOldValue := FFileName;
          FFileName := LDemandFileName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockFileName',FFileName, LDemandFileName);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end; 
end;

procedure TIrrigationBlock.Set_MaxWaterAllocation(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_MaxWaterAllocation';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FMaxWaterAllocation <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FMaxWaterAllocation);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockMaxWaterAllocation', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FMaxWaterAllocation := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockMaxWaterAllocation',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_HydrologyNodeNumber(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_HydrologyNodeNumber';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FHydrologyNodeNumber <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FHydrologyNodeNumber);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockNodeNumber', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FHydrologyNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockNodeNumber',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_NumberOfCropTypes(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_NumberOfCropTypes';
var
  LLoadAgent    : TIrrigationBlockSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
  LWaterUse     : TWaterUsage;
  LDif,
  LCnt          : Integer;
begin
  try
    if (FNumberOfCropTypes <> Value) and (Value <= MaxNoOfCrops) then
    begin
      if(FCropWaterUseType = 2) and (Value < 1) then Exit;

      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FNumberOfCropTypes);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        begin
          FNumberOfCropTypes := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockNumberOfCropTypes',LOldValue,IntToStr(Value));

          LDif := FNumberOfCropTypes - FWaterUsageFactor.Count;
          if FNumberOfCropTypes < FWaterUsageFactor.Count then
          begin
            for LCnt := FWaterUsageFactor.Count - 1 downto FNumberOfCropTypes do
            begin
              LWaterUse := TWaterUsage(FWaterUsageFactor.Items[LCnt]);
              if Assigned(LWaterUse) then
                RemoveWaterUse(LWaterUse.Identifier);
            end;
          end;
          if FNumberOfCropTypes > FWaterUsageFactor.Count then
          begin
            for LCnt := 0 to LDif - 1 do
              CreateWaterUse;
          end;
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_PanEvaporation(AMonth: Integer; Value: Double);
const OPNAME = 'TIrrigationBlock.Set_PanEvaporation';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FPanEvaporation[AMonth] <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FPanEvaporation[AMonth]);
        LLoadAgent.LoadContextData_Factor(LContextData, FloatToStr(FIdentifier), IntToStr(AMonth) );
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockPanEvaporation', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FPanEvaporation[AMonth] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockBlockIdentifier',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_RainAboveRainFactorSpecValue(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_RainAboveRainFactorSpecValue';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FRainAboveRainFactorSpecValue <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FRainAboveRainFactorSpecValue);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockRainAboveRainFactorSpecValue', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FRainAboveRainFactorSpecValue := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockRainAboveRainFactorSpecValue',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_RainBelowRainFactor(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_RainBelowRainFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FRainBelowRainFactor <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FRainBelowRainFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockRainBelowRainFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FRainBelowRainFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockRainBelowRainFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_RainCatchmentScalingFactor(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_RainCatchmentScalingFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FRainCatchmentScalingFactor <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FRainCatchmentScalingFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockRainCatchmentScalingFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FRainCatchmentScalingFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockRainCatchmentScalingFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_RainfallFactor(AMonth: Integer; Value: Double);
const OPNAME = 'TIrrigationBlock.Set_RainfallFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FRainfallFactor[AMonth] <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FRainfallFactor[AMonth]);
        LLoadAgent.LoadContextData_Factor(LContextData, FloatToStr(FIdentifier), IntToStr(AMonth) );
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockRainfallFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FRainfallFactor[AMonth] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockBlockIdentifier',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowFactor(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FReturnFlowFactor <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FReturnFlowFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockReturnFlowFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FReturnFlowFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockReturnFlowFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowFactorBreakPointsCount(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowFactorBreakPointsCount';
begin
  try
    if(FReturnFlowFactorBreakpointYear.Count = Value) and (FReturnFlowFactors.Count = Value) then Exit;
    FOldReturnFlowFactorBreakpointYearCommaText := FReturnFlowFactorBreakpointYear.CommaText;
    FOldReturnFlowFactorsCommaText := FReturnFlowFactors.CommaText;
    if(Value < FReturnFlowFactorBreakpointYear.Count) then
    begin
      while(FReturnFlowFactorBreakpointYear.Count > Value) do
      begin
        FReturnFlowFactorBreakpointYear.Delete(FReturnFlowFactorBreakpointYear.Count-1);
      end;
    end;
    if(Value < FReturnFlowFactors.Count) then
    begin
      while(FReturnFlowFactors.Count > Value) do
      begin
        FReturnFlowFactors.Delete(FReturnFlowFactors.Count-1);
      end;
    end;
    if(Value > FReturnFlowFactorBreakpointYear.Count) then
    begin
      while(FReturnFlowFactorBreakpointYear.Count < Value) do
      begin
        FReturnFlowFactorBreakpointYear.Add('1900');
      end;
    end;
    if(Value > FReturnFlowFactors.Count) then
    begin
      while(FReturnFlowFactors.Count < Value) do
      begin
        FReturnFlowFactors.Add('0.0000');
      end;
    end;
    SaveReturnFlowFactorBreakpointYear;
    SaveReturnFlowFactors;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowFactorBreakpointYearByIndex(AIndex, AValue: integer);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowFactorBreakpointYearByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowFactorBreakpointYear.Count) then
    begin
      FOldReturnFlowFactorBreakpointYearCommaText := FReturnFlowFactorBreakpointYear.CommaText;
      FReturnFlowFactorBreakpointYear[AIndex] := IntToStr(AValue);
      SaveReturnFlowFactorBreakpointYear;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_WaterUsageFactor(AIndex: Integer; Value: TWaterUsage);
const OPNAME = 'TIrrigationBlock.Set_WaterUsageFactor';
begin   
  if AIndex in [MinNoOfCrops..MaxNoOfCrops] then
    FWaterUsageFactor[AIndex] := Value;
end;

function TIrrigationBlock.Get_BlockDescription: WideString;
const OPNAME = 'TIrrigationBlock.Get_BlockDescription';
begin
  try
    Result := FBlockDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_BlockDescription(const Value: WideString);
const OPNAME = 'TIrrigationBlock.Set_BlockDescription';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FBlockDescription <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FBlockDescription;
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockDescription', Value, LOldValue, LContextData ) then
        begin
          FBlockDescription := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockDescription',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateUpperZoneReturnFlow(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateUpperZoneReturnFlow';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockUpperZoneReturnFlow', FloatToStr(FUpperZoneReturnFlow), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FUpperZoneReturnFlow)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateInitialSoilMoistureStorage(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateInitialSoilMoistureStorage';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockInitialSoilMoistureStorage', FloatToStr(FInitialSoilMoistureStorage), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FInitialSoilMoistureStorage)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateLowerZoneReturnFlow(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateLowerZoneReturnFlow';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockLowerZoneReturnFlow', FloatToStr(FLowerZoneReturnFlow), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FLowerZoneReturnFlow)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateLowerZoneSoilMoistureCapacity(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateLowerZoneSoilMoistureCapacity';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockLowerZoneSoilMoistureCapacity', FloatToStr(FLowerZoneSoilMoistureCapacity), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FLowerZoneSoilMoistureCapacity)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateReturnFlowLoss(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateReturnFlowLoss';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockReturnFlowLoss', FloatToStr(FReturnFlowLoss), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FReturnFlowLoss)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateUpperZoneSoilMoistureCapacity(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateUpperZoneSoilMoistureCapacity';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockUpperZoneSoilMoistureCapacity', FloatToStr(FUpperZoneSoilMoistureCapacity), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FUpperZoneSoilMoistureCapacity)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateUpperZoneSoilMoistureTarget(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateUpperZoneSoilMoistureTarget';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockUpperZoneSoilMoistureTarget', FloatToStr(FUpperZoneSoilMoistureTarget), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FUpperZoneSoilMoistureTarget)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateBlockNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateBlockNodeNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockBlockNumber', IntToStr(FBlockNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FBlockNodeNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateCanalTransportLoss(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateCanalTransportLoss';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockCanalTransportLoss', FloatToStr(FCanalTransportLoss), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FCanalTransportLoss)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateBlockDescription(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateBlockDescription';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockDescription', FBlockDescription, lMessage)) then
      AErrorMessages.Add('WARNING:' +FBlockDescription + ':'+lMessage);
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateEfficiencyFactor(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateEfficiencyFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockEfficiencyFactor', FloatToStr(FEfficiencyFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FEfficiencyFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateFileName(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateFileName';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockFileName', FFileName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FFileName + ':'+lMessage)
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMaxWaterAllocation(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateMaxWaterAllocation';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockMaxWaterAllocation', FloatToStr(FMaxWaterAllocation), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxWaterAllocation)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateHydrologyNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateHydrologyNodeNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockNodeNumber', FloatToStr(FHydrologyNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FHydrologyNodeNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateReturnFlowFactor(AErrorMessages: TStrings): Boolean;
const OPNAME='TIrrigationBlock.ValidateReturnFlowFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockReturnFlowFactor', FloatToStr(FReturnFlowFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FReturnFlowFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_WaterUsageCount: integer;
const OPNAME = 'TIrrigationBlock.Get_WaterUsageCount';
begin
  Result := 0;
  try
    if Assigned(FWaterUsageFactor) then
    begin
      Result := FWaterUsageFactor.Count;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_WaterUsageFactorByID(AIrrigationBlockWaterUsageID: integer): IWaterUsage;
const OPNAME = 'TIrrigationBlock.Get_WaterUsageFactorByID';
var
  lIndex            : integer;
  lIrrigationBlockWaterUse  : TWaterUsage;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FWaterUsageFactor.Count)) do
    begin
      lIrrigationBlockWaterUse := TWaterUsage(FWaterUsageFactor.Items[lIndex]);
      if (lIrrigationBlockWaterUse.FIdentifier = AIrrigationBlockWaterUsageID) then
        Result := lIrrigationBlockWaterUse
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_WaterUsageFactorByIndex(AIndex: integer): IWaterUsage;
const OPNAME = 'TIrrigationBlock.Get_WaterUsageFactorByIndex';
var
  lIrrigationBlockWaterUse : TWaterUsage;
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FWaterUsageFactor.Count)) then
    begin
      lIrrigationBlockWaterUse := TWaterUsage(FWaterUsageFactor.Items[AIndex]);
      Result := lIrrigationBlockWaterUse;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.WaterUsageFactorByName(const AName: WideString): IWaterUsage;
const OPNAME = 'TIrrigationBlock.WaterUsageFactorByName';
var
  lIndex            : integer;
  lIrrigationBlockWaterUse  : TWaterUsage;
begin
  Result := nil;
  try
    for lIndex := 0 to FWaterUsageFactor.Count - 1 do
    begin
      lIrrigationBlockWaterUse := TWaterUsage(FWaterUsageFactor.Items[lIndex]);
      if UpperCase(lIrrigationBlockWaterUse.FCropName) = UpperCase(AName) then
      begin
        Result := IWaterUsage(lIrrigationBlockWaterUse);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.CastIrrigationBlockWaterUsageByID(AIrrigationBlockWaterUsageID: Integer): TWaterUsage;
const OPNAME = 'TIrrigationBlock.CastIrrigationBlockWaterUsageByID';
var
 lIndex                   : Integer;
 lIrrigationBlockWaterUse : TWaterUsage;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FWaterUsageFactor.Count)) do
    begin
      lIrrigationBlockWaterUse := TWaterUsage(FWaterUsageFactor.Items[lIndex]);
      if (lIrrigationBlockWaterUse.FIdentifier = AIrrigationBlockWaterUsageID) then
        Result := lIrrigationBlockWaterUse
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.CastIrrigationBlockWaterUsageByIndex(AIndex: Integer): TWaterUsage;
const OPNAME = 'TIrrigationBlock.CastIrrigationBlockWaterUsageByIndex';
var
  lIrrigationBlockWaterUse : TWaterUsage;
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FWaterUsageFactor.Count)) then
    begin
      lIrrigationBlockWaterUse := TWaterUsage(FWaterUsageFactor.Items[AIndex]);
      Result := lIrrigationBlockWaterUse;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.CreateWaterUse: IWaterUsage;
const OPNAME = 'TIrrigationBlock.CreateWaterUse';
begin
  Result := nil;
  try
    if FWaterUsageFactor.Count < MaxNoOfCrops then
      Result := CreateNewIrrigationBlockWaterUse
    else
      ShowMessage(Format(FAppModules.Language.GetString('Message.IrrBlockCropTypesLimit'),[MaxNoOfCrops]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.CreateNewIrrigationBlockWaterUse: TWaterUsage;
const OPNAME = 'TIrrigationBlock.CreateNewIrrigationBlockWaterUse';
var
  lLoadAgent          : TIrrigationBlockSQLAgent;
  lWaterUse           : TWaterUsage;
begin
  Result := nil;
  try
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      lWaterUse := TWaterUsage.Create(FAppModules);
      lWaterUse.Initialise;
      lWaterUse.FBlockIdentifier := Self.FIdentifier;
      if LLoadAgent.InsertIrrigationBlockWaterUsageFactor(lWaterUse) then
      begin
        FWaterUsageFactor.Add(lWaterUse);
        Result := lWaterUse;
      end
      else
        FreeAndNil(lWaterUse);
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.NewWaterUse: TWaterUsage;
const OPNAME = 'TIrrigationBlock.NewWaterUse';
begin
  Result := nil;
  try
    Result := TWaterUsage.Create(FAppModules);
    Result.Initialise;
    FWaterUsageFactor.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.AddMonthlyWaterUse(AFeature: TWaterUsage): Boolean;
const OPNAME = 'TIrrigationBlock.AddMonthlyWaterUse';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      if FWaterUsageFactor.Count < MaxNoOfCrops then
      begin
        FWaterUsageFactor.Add(AFeature);
        Result := True;
      end else
        ShowMessage(Format(FAppModules.Language.GetString('Message.IrrBlockCropTypesLimit'),[MaxNoOfCrops]));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.DeleteWaterUseWithID(AIrrigationBlockWaterUsageID: integer): WordBool;
const OPNAME = 'TIrrigationBlock.DeleteWaterUseWithID';
var
  lIrrigationBlockWaterUse : TWaterUsage;
  lIndex            : Integer;
begin
  Result := False;
  try
    lIrrigationBlockWaterUse := CastIrrigationBlockWaterUsageByID(AIrrigationBlockWaterUsageID);
    if (lIrrigationBlockWaterUse <> nil) then
    begin
      lIndex := FWaterUsageFactor.IndexOf(lIrrigationBlockWaterUse);
      Result := DeleteWaterUseWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.DeleteWaterUseWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TIrrigationBlock.DeleteWaterUseWithIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) then
    begin
      FWaterUsageFactor.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.RemoveWaterUse(AIrrigationBlockWaterUsageID: integer): WordBool;
const OPNAME = 'TIrrigationBlock.RemoveWaterUse';
var
  lLoadAgent : TIrrigationBlockSQLAgent;
begin
  Result := False;
  try
    if (AIrrigationBlockWaterUsageID > 0) then
    begin
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteIrrigationBlockMonthlyWaterUse(Self.FIdentifier, AIrrigationBlockWaterUsageID) then
        begin
          DeleteWaterUseWithID(AIrrigationBlockWaterUsageID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateNumberOfCropTypes(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateNumberOfCropTypes';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockNumberOfCropTypes', IntToStr(FNumberOfCropTypes), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FNumberOfCropTypes)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_DiversionChannel: IGeneralFlowChannel;
const OPNAME = 'TIrrigationBlock.Get_DiversionChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FDiversionChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowChannel: IGeneralFlowChannel;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FReturnFlowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateDiversionUpstreamNode(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateDiversionUpstreamNode';
var
  lMessage          : WideString;
  lDiversionChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FDiversionChannelNr <> 0) then
    begin
      lDiversionChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelList.ChannelByChannelNumber[FDiversionChannelNr];
      if (lDiversionChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FBlockName]));
        Result := False;
      end
      else
      begin
        if (NOT lDiversionChannel.Validate(lMessage,'UpNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FBlockName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.ValidateReturnFlowDownstreamNode(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TIrrigationBlock.ValidateReturnFlowDownstreamNode';
var
  lMessage           : WideString;
  lReturnFlowChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FReturnFlowChannelNr <> 0) then
    begin
      lReturnFlowChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                              ChannelList.ChannelByChannelNumber[FReturnFlowChannelNr];
      if (NOT lReturnFlowChannel.Validate(lMessage,'DownNodeNumber')) then
      begin
        Result := False;
        AErrorMessages.Add('ERROR:' +FBlockName+ ':'+lMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.ValidateAllocatedIrrigationArea(AErrorMessages: TStrings): WordBool;
const OPNAME='TIrrigationBlock.ValidateAllocatedIrrigationArea';
var
  LNode : IReservoirData;
  LParamReference : IParamReference;
  lMessage1 : string;
  lMessage : WideString;
begin
  Result := False;
  try
    lMessage := '';
    if(not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockAllocatedIrrigationArea',
      FloatToStr(FAllocatedIrrigationArea), lMessage1)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FAllocatedIrrigationArea)+ ':'+lMessage1)
    else
    begin
      LNode := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FHydrologyNodeNumber];
      if(LNode <> nil) and (LNode.ReservoirConfigurationData.CatchmentRef <> 0) then
      begin
        LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                           ReferenceDataByCatchNumber[LNode.ReservoirConfigurationData.CatchmentRef];
        if(LParamReference <> nil) then
        begin
          LParamReference.Validate(lMessage,'CatchmentAreaParam');
          if(lMessage <> '') then
           AErrorMessages.Add(lMessage);
        end;
      end;
    end;
    Result := lMessage = '';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateRainAboveRainFactorSpecValue(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateRainAboveRainFactorSpecValue';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockRainAboveRainFactorSpecValue', FloatToStr(FRainAboveRainFactorSpecValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FRainAboveRainFactorSpecValue)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateRainBelowRainFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateRainBelowRainFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockRainBelowRainFactor', FloatToStr(FRainBelowRainFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FRainBelowRainFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateRainCatchmentScalingFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateRainCatchmentScalingFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockRainCatchmentScalingFactor', FloatToStr(FRainCatchmentScalingFactor), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FRainCatchmentScalingFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateCropWaterUseType(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateCropWaterUseType';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockCropWaterUseType', IntToStr(FCropWaterUseType), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FCropWaterUseType)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateDroughtApplicable( AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateDroughtApplicable';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockDroughtApplicable', IntToStr(FDroughtApplicable), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FDroughtApplicable)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateCurtailIrrigationAbstraction(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateCurtailIrrigationAbstraction';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CurtailIrrigationAbstraction', IntToStr(FCurtailIrrigationAbstraction), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FCurtailIrrigationAbstraction)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateCanalSeepageLoss(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateCanalSeepageLoss';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CanalSeepageLoss', FloatToStr(FCanalSeepageLoss), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FCanalSeepageLoss)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateCanalTransmissionLoss(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateCanalTransmissionLoss';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CanalTransmissionLoss', FloatToStr(FCanalTransmissionLoss), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FCanalTransmissionLoss)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateUpperSoilOutflow(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateUpperSoilOutflow';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('UpperSoilOutflow', FloatToStr(FUpperSoilOutflow), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FUpperSoilOutflow)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIrrigationBlock.ValidateMaxUpperZoneMoisture(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMaxUpperZoneMoisture';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxUpperZoneMoisture', FloatToStr(FMaxUpperZoneMoisture), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxUpperZoneMoisture)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMinUpperZoneMoisture(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMinUpperZoneMoisture';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinUpperZoneMoisture', FloatToStr(FMinUpperZoneMoisture), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinUpperZoneMoisture)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateCropTypesCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateCropTypesCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CropTypesCount', IntToStr(FCropTypesCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FCropTypesCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateIrrigationSupplyCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateIrrigationSupplyCapacity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationSupplyCapacity', FloatToStr(FIrrigationSupplyCapacity), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FIrrigationSupplyCapacity)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateAllocatedAreaPointsCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateAllocatedAreaPointsCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AllocatedAreaPointsCount', IntToStr(FAllocatedAreaPointsCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FAllocatedAreaPointsCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMethodIrrigatedAreas(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMethodIrrigatedAreas';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MethodIrrigatedAreas', IntToStr(FMethodIrrigatedAreas), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMethodIrrigatedAreas)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMaxWaterAllocationCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMaxWaterAllocationCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxWaterAllocationCount', IntToStr(FMaxWaterAllocationCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMaxWaterAllocationCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMethodMaxWaterAllocation(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMethodMaxWaterAllocation';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MethodMaxWaterAllocation', IntToStr(FMethodMaxWaterAllocation), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMethodMaxWaterAllocation)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateReturnFlowVolumePointsCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateReturnFlowVolumePointsCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ReturnFlowVolumePointsCount', IntToStr(FReturnFlowVolumePointsCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReturnFlowVolumePointsCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMethodReturnFlowVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMethodReturnFlowVolume';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MethodReturnFlowVolume', IntToStr(FMethodReturnFlowVolume), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMethodReturnFlowVolume)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateSupplyCapacityPointsCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateSupplyCapacityPointsCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SupplyCapacityPointsCount', IntToStr(FSupplyCapacityPointsCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FSupplyCapacityPointsCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIrrigationBlock.ValidateMethodSupplyCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMethodSupplyCapacity';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MethodSupplyCapacity', IntToStr(FMethodSupplyCapacity), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMethodSupplyCapacity)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateIrrigationEfficienciesPointsCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateIrrigationEfficienciesPointsCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationEfficienciesPointsCount', IntToStr(FIrrigationEfficienciesPointsCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FIrrigationEfficienciesPointsCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



function TIrrigationBlock.ValidateMethodIrrigationEfficiencies(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMethodIrrigationEfficiencies';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MethodIrrigationEfficiencies', IntToStr(FMethodIrrigationEfficiencies), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMethodIrrigationEfficiencies)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateReturnFlowFactorsCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateReturnFlowFactorsCount';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ReturnFlowFactorsCount', IntToStr(FReturnFlowFactorsCount), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReturnFlowFactorsCount)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.ValidateMethodReturnFlowFactors(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlock.ValidateMethodReturnFlowFactors';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MethodReturnFlowFactors', IntToStr(FMethodReturnFlowFactors), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FMethodReturnFlowFactors)+ ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIrrigationBlock.Get_CropWaterUseType: Integer;
const OPNAME = 'TIrrigationBlock.Get_CropWaterUseType';
begin
  Result := NullInteger;
  try
    Result := FCropWaterUseType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_DroughtApplicable: Integer;
const OPNAME = 'TIrrigationBlock.Get_DroughtApplicable';
begin
  Result := NullInteger;
  try
    Result := FDroughtApplicable;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_CropWaterUseType(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_CropWaterUseType';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
  LWaterUsage : TWaterUsage;
begin
  try
    if FCropWaterUseType <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FCropWaterUseType);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockCropWaterUseType', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FCropWaterUseType := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockCropWaterUseType',LOldValue,FloatToStr(Value));
          if(FCropWaterUseType = 2) then
          begin
            if(FWaterUsageFactor.Count = 0) then
              Set_NumberOfCropTypes(1);
          end;
          if(FWaterUsageFactor.Count > 0) then
          begin
            LWaterUsage := TWaterUsage(FWaterUsageFactor.Items[0]);
            if(FCropWaterUseType = 2) then
            begin
              LWaterUsage.Set_PercAreaUnderCropType(100.0);
              LWaterUsage.Set_CropName(FAppModules.Language.GetString('IrrigationBlock.AllCropTypes'));
            end
            else
            begin
              LWaterUsage.Set_PercAreaUnderCropType(0.0);
              LWaterUsage.Set_CropName(FAppModules.Language.GetString('IrrigationBlock.CropName') + IntToStr(BlockIdentifier)+ '-' + IntToStr(LWaterUsage.FIdentifier));
            end;
          end;
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_DroughtApplicable(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_DroughtApplicable';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FDroughtApplicable <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FDroughtApplicable);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockDroughtApplicable', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FDroughtApplicable := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockDroughtApplicable',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_BlockNode: IReservoirData;
const OPNAME = 'TIrrigationBlock.Get_BlockNode';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FBlockNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_HydrologyNode: IReservoirData;
const OPNAME = 'TIrrigationBlock.Get_HydrologyNode';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FHydrologyNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Assign(ASource: TIrrigationBlock);
const OPNAME = 'TIrrigationBlock.Assign';
var
  LIndex : integer;
  LMonth : integer;
begin
  try
    UpperZoneReturnFlow           := ASource.UpperZoneReturnFlow;
    LowerZoneReturnFlow           := ASource.LowerZoneReturnFlow;
    ReturnFlowLoss                := ASource.ReturnFlowLoss;
    UpperZoneSoilMoistureCapacity := ASource.UpperZoneSoilMoistureCapacity;
    LowerZoneSoilMoistureCapacity := ASource.LowerZoneSoilMoistureCapacity;
    UpperZoneSoilMoistureTarget   := ASource.UpperZoneSoilMoistureTarget;
    InitialSoilMoistureStorage    := ASource.InitialSoilMoistureStorage;
    for LIndex := MinMonths to MaxMonths do
    begin
      RainfallFactor[LIndex] := ASource.RainfallFactor[LIndex];
      PanEvaporation[LIndex] := ASource.PanEvaporation[LIndex];
      APanConvFactor[LIndex] := ASource.APanConvFactor[LIndex];
    end;
    BlockName                     := 'Copy of '+ASource.BlockName;
    BlockDescription              := 'Copy of '+ASource.BlockDescription;
    MaxWaterAllocation            := ASource.MaxWaterAllocation;
    FileName                      := ASource.FileName;
    HydrologyNodeNumber           := ASource.HydrologyNodeNumber;
    CanalTransportLoss            := ASource.CanalTransportLoss;
    EfficiencyFactor              := ASource.EfficiencyFactor;
    ReturnFlowFactor              := ASource.ReturnFlowFactor;
    NumberOfCropTypes             := ASource.NumberOfCropTypes;
    RainAboveRainFactorSpecValue  := ASource.RainAboveRainFactorSpecValue;
    RainBelowRainFactor           := ASource.RainBelowRainFactor;
    RainCatchmentScalingFactor    := ASource.RainCatchmentScalingFactor;
    AllocatedIrrigationArea       := ASource.AllocatedIrrigationArea;
    DroughtApplicable             := ASource.DroughtApplicable;
    CropWaterUseType              := ASource.CropWaterUseType;
    for LIndex := 0 to NumberOfCropTypes do
    begin
      if ASource.WaterUsageFactorByIndex[LIndex] <> nil then
      begin
        TWaterUsage(FWaterUsageFactor.Items[LIndex]).PercAreaUnderCropType := ASource.WaterUsageFactorByIndex[LIndex].PercAreaUnderCropType;
        TWaterUsage(FWaterUsageFactor.Items[LIndex]).CropName := ASource.WaterUsageFactorByIndex[LIndex].CropName;
        for LMonth := 1 to 12 do
          if ASource.WaterUsageFactorByIndex[LIndex].MonthlyWaterUse[LMonth] <> NullFloat then
            TWaterUsage(FWaterUsageFactor.Items[LIndex]).MonthlyWaterUse[LMonth]  := ASource.WaterUsageFactorByIndex[LIndex].MonthlyWaterUse[LMonth];
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIrrigationBlockList }

function TIrrigationBlockList._AddRef: Integer;
const OPNAME = 'TIrrigationBlockList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList._Release: Integer;
const OPNAME = 'TIrrigationBlockList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.CreateIrrigationBlock(ABlockType : integer): IIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.CreateIrrigationBlock';
begin
  Result := nil;
  try
    Result := CreateNewIrrigationBlock(ABlockType);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlockList.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIrrigationBlockList := TObjectList.Create(True);
    Initialise;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlockList.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlockList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FIrrigationBlockList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.Get_IrrigationBlockCount: Integer;
const OPNAME = 'TIrrigationBlockList.Get_IrrigationBlockCount';
begin
  Result := 0;
  try
    Result := FIrrigationBlockList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.Get_IrrigationBlockByID(ABlockIdentifier: integer): IIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.Get_IrrigationBlockByID';
var
  lIndex            : integer;
  lIrrigationBlock  : TIrrigationBlock;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationBlockList.Count)) do
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[lIndex]);
      if (lIrrigationBlock.FIdentifier = ABlockIdentifier) then
        Result := lIrrigationBlock
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.Get_IrrigationBlockByIndex(AIndex: integer): IIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.Get_IrrigationBlockByIndex';
var
  lIrrigationBlock : TIrrigationBlock;
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FIrrigationBlockList.Count)) then
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[AIndex]);
      Result := lIrrigationBlock;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.Initialise: boolean;
const OPNAME = 'TIrrigationBlockList.Initialise';
begin
  Result := inherited Initialise;
  try
    FIrrigationBlockCount := 0;
    FIrrigationBlockList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.RemoveIrrigationBlock(AIrrigationBlockNodeNr: integer): WordBool;
const OPNAME = 'TIrrigationBlockList.RemoveIrrigationBlock';
var
  LLoadAgent                : TIrrigationBlockSQLAgent;
  LNetworkElementData       : TNetworkElementData;
  LDiversionChannelNumber   : integer;
  LReturnFlowChannelNumber  : integer;
  LIrrigationBlock          : TIrrigationBlock;
  LBlockIdentifier          : Integer;
begin
  Result              := False;
  try
    LIrrigationBlock  := CastIrrigationBlockByBlockNodeNumber(AIrrigationBlockNodeNr);
    if (LIrrigationBlock = nil) then  Exit;

    LNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).
                           CastNetworkElementData;
    LBlockIdentifier  := LIrrigationBlock.BlockIdentifier;

    LDiversionChannelNumber  := GetDiversionChannelID(LBlockIdentifier);
    if(LDiversionChannelNumber >= 0) then
      LNetworkElementData.ChannelList.RemoveChannelWithID(LDiversionChannelNumber);

    LReturnFlowChannelNumber := GetReturnFlowChannelID(LBlockIdentifier);
    if(LReturnFlowChannelNumber >= 0) then
      LNetworkElementData.ChannelList.RemoveChannelWithID(LReturnFlowChannelNumber);

    LNetworkElementData.CastReservoirList.DeleteIrrigationNode(AIrrigationBlockNodeNr);

    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.DeleteIrrigationBlock(AIrrigationBlockNodeNr) then
      begin
        DeleteIrrigationBlockWithID(LBlockIdentifier);
        Result := True;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.CreateNewIrrigationBlock(ABlockType : integer): TIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.CreateNewIrrigationBlock';
var

  lLoadAgent          : TIrrigationBlockSQLAgent;
  lDiversionChannel   : IGeneralFlowChannel;
  lReturnFlowChannel  : IGeneralFlowChannel;
  lIrrigationBlock    : TIrrigationBlock;
  LBlockNode          : IReservoirData;
  lNetworkElementData : TNetworkElementData;
begin
  Result := nil;
  try
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      lNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;
      lIrrigationBlock    := TIrrigationBlock.Create(FAppModules);
      lDiversionChannel   := lNetworkElementData.CastChannelList.CreateNewChannel;
      lReturnFlowChannel  := lNetworkElementData.CastChannelList.CreateNewChannel;
      LBlockNode          := lNetworkElementData.CastReservoirList.CreateIrrigationNode;
      try
        lIrrigationBlock.Initialise;
        lIrrigationBlock.FBlockNodeNumber                := LBlockNode.ReservoirConfigurationData.ReservoirIdentifier;
        lIrrigationBlock.FDiversionChannelNr             := lDiversionChannel.ChannelNumber;
        lIrrigationBlock.FReturnFlowChannelNr            := lReturnFlowChannel.ChannelNumber;
        lIrrigationBlock.FIrrigationBlockType            := ABlockType;
        lIrrigationBlock.FCropWaterUseType               := ABlockType;
        if LLoadAgent.InsertIrrigationBlock(lIrrigationBlock) then
        begin
          lDiversionChannel.ChannelType                    := 14;
          lDiversionChannel.DownStreamNodeNumber           := lIrrigationBlock.FBlockNodeNumber;
          lReturnFlowChannel.UpStreamNodeNumber            := lIrrigationBlock.FBlockNodeNumber;
          lReturnFlowChannel.ChannelType                   := 15;
          FIrrigationBlockList.Add(lIrrigationBlock);
          Result := lIrrigationBlock;
        end
        else
        begin
          FreeAndNil(lIrrigationBlock);
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lDiversionChannel.ChannelID);
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lReturnFlowChannel.ChannelID);
        end;
      except
        FreeAndNil(lIrrigationBlock);
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lDiversionChannel.ChannelID);
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lReturnFlowChannel.ChannelID);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.DeleteIrrigationBlockWithID(ABlockIdentifier: integer): WordBool;
const OPNAME = 'TIrrigationBlockList.DeleteIrrigationBlockWithID';
var
  lIrrigationBlock  : TIrrigationBlock;
  lIndex            : Integer;
begin
  Result := False;
  try
    lIrrigationBlock := CastIrrigationBlockByID(ABlockIdentifier);
    if (lIrrigationBlock <> nil) then
    begin
      lIndex := FIrrigationBlockList.IndexOf(lIrrigationBlock);
      Result := DeleteIrrigationBlockWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TIrrigationBlockList.CastIrrigationBlockByID(ABlockIdentifier: integer): TIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.CastIrrigationBlockByID';
var
 lIndex           : integer;
 lIrrigationBlock : TIrrigationBlock;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationBlockList.Count)) do
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[lIndex]);
      if (lIrrigationBlock.FIdentifier = ABlockIdentifier) then
        Result := lIrrigationBlock
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.DeleteIrrigationBlockWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TIrrigationBlockList.DeleteIrrigationBlockWithIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) then
    begin
      FIrrigationBlockList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.IrrigationBlockByName(const AName: WideString): IIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.IrrigationBlockByName';
var
  lIndex            : integer;
  lIrrigationBlock  : TIrrigationBlock;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationBlockList.Count)) do
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[lIndex]);
      if (lIrrigationBlock.FBlockName = AName) then
        Result := lIrrigationBlock
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TIrrigationBlockList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    for LIndex := 0 to FIrrigationBlockList.Count -1 do
    begin
      if not TIrrigationBlock(FIrrigationBlockList[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_InitialSoilMoistureStorage: Double;
const OPNAME = 'TIrrigationBlock.Get_InitialSoilMoistureStorage';
begin
  try
    Result := FInitialSoilMoistureStorage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_LowerZoneReturnFlow: Double;
const OPNAME = 'TIrrigationBlock.Get_LowerZoneReturnFlow';
begin
  try
    Result := FLowerZoneReturnFlow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_LowerZoneSoilMoistureCapacity: Double;
const OPNAME = 'TIrrigationBlock.Get_LowerZoneSoilMoistureCapacity';
begin
  try
    Result := FLowerZoneSoilMoistureCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowLoss: Double;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowLoss';
begin
  try
    Result := FReturnFlowLoss;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_UpperZoneReturnFlow: Double;
const OPNAME = 'TIrrigationBlock.Get_UpperZoneReturnFlow';
begin
  try
    Result := FUpperZoneReturnFlow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_UpperZoneSoilMoistureCapacity: Double;
const OPNAME = 'TIrrigationBlock.Get_UpperZoneSoilMoistureCapacity';
begin
  try
    Result := FUpperZoneSoilMoistureCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_UpperZoneSoilMoistureTarget: Double;
const OPNAME = 'TIrrigationBlock.Get_UpperZoneSoilMoistureTarget';
begin
  try
    Result := FUpperZoneSoilMoistureTarget;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.ValidateIrrigationBlockCount(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TIrrigationBlockList.ValidateIrrigationBlockCount';
var
  lMessage : string;
  lValue   : integer;
begin
  Result := False;
  try
    lMessage := '';
    lValue   := IrrigationBlockCount;
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockCount', IntToStr(lValue), lMessage)) then
      AErrorMessages.Add('ERROR:' +lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_InitialSoilMoistureStorage(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_InitialSoilMoistureStorage';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FInitialSoilMoistureStorage <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FInitialSoilMoistureStorage);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockInitialSoilMoistureStorage', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FInitialSoilMoistureStorage := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockInitialSoilMoistureStorage',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TIrrigationBlock.Set_LowerZoneReturnFlow(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_LowerZoneReturnFlow';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FLowerZoneReturnFlow <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FLowerZoneReturnFlow);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockLowerZoneReturnFlow', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FLowerZoneReturnFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockLowerZoneReturnFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_LowerZoneSoilMoistureCapacity(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_LowerZoneSoilMoistureCapacity';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FLowerZoneSoilMoistureCapacity <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FLowerZoneSoilMoistureCapacity);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockLowerZoneSoilMoistureCapacity', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FLowerZoneSoilMoistureCapacity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockLowerZoneSoilMoistureCapacity',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowLoss(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowLoss';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FReturnFlowLoss <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FReturnFlowLoss);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockReturnFlowLoss', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FReturnFlowLoss := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockReturnFlowLoss',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_UpperZoneReturnFlow(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_UpperZoneReturnFlow';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FUpperZoneReturnFlow <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FUpperZoneReturnFlow);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockUpperZoneReturnFlow', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FUpperZoneReturnFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockUpperZoneReturnFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_UpperZoneSoilMoistureCapacity(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_UpperZoneSoilMoistureCapacity';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FUpperZoneSoilMoistureCapacity <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FUpperZoneSoilMoistureCapacity);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockUpperZoneSoilMoistureCapacity', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FUpperZoneSoilMoistureCapacity := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockUpperZoneSoilMoistureCapacity',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_UpperZoneSoilMoistureTarget(Value: Double);
const OPNAME = 'TIrrigationBlock.Set_UpperZoneSoilMoistureTarget';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;

begin
  try
    if FUpperZoneSoilMoistureTarget <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FUpperZoneSoilMoistureTarget);
        LLoadAgent.LoadContextData_FeatureID(LContextData, FloatToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockUpperZoneSoilMoistureTarget', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FUpperZoneSoilMoistureTarget := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockUpperZoneSoilMoistureTarget',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.AddIrrigationBlock(AFeature: TIrrigationBlock): boolean;
const OPNAME = 'TIrrigationBlockList.AddIrrigationBlock';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FIrrigationBlockList.Add(AFeature);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.CastIrrigationBlockByIndex(AIndex: integer): TIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.CastIrrigationBlockByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FIrrigationBlockList.Count) then
      Result := TIrrigationBlock(FIrrigationBlockList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlockList.GetChannelNumbers(ABlockIdentifier: integer; var ADiversionChannelNumber, AReturnFlowChannelNumber: integer);
const OPNAME = 'TIrrigationBlockList.GetChannelNumbers';
var
 lIrrigationBlock : TIrrigationBlock;
begin
  try
    ADiversionChannelNumber   := NullInteger;
    AReturnFlowChannelNumber  := NullInteger;
    lIrrigationBlock           := CastIrrigationBlockByID(ABlockIdentifier);
    if (lIrrigationBlock <> nil) then
    begin
      ADiversionChannelNumber   := lIrrigationBlock.DiversionChannel.ChannelNumber;
      AReturnFlowChannelNumber  := lIrrigationBlock.ReturnFlowChannel.ChannelNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.NewIrrigationBlock: TIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.NewIrrigationBlock';
begin
  Result := nil;
  try
    Result := TIrrigationBlock.Create(FAppModules);
    FIrrigationBlockList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.GetDiversionChannelID(ABlockIdentifier: integer): integer;
const OPNAME = 'TIrrigationBlockList.GetDiversionChannelID';
var
  LIrrigationBlock : TIrrigationBlock;
  LChannel         : IGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LIrrigationBlock := CastIrrigationBlockByID(ABlockIdentifier);
    if(LIrrigationBlock <> nil) then
    begin
      LChannel := LIrrigationBlock.DiversionChannel;
      if(LChannel <> nil) then
         Result := LChannel.ChannelID;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.GetReturnFlowChannelID(ABlockIdentifier: integer): integer;
const OPNAME = 'TIrrigationBlockList.GetReturnFlowChannelID';
var
  LIrrigationBlock :TIrrigationBlock;
  LChannel: IGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LIrrigationBlock := CastIrrigationBlockByID(ABlockIdentifier);
    if(LIrrigationBlock <> nil) then
    begin
      LChannel := LIrrigationBlock.ReturnFlowChannel;
      if(LChannel <> nil) then
         Result := LChannel.ChannelID;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.Get_IrrigationBlockByBlockNodeNumber(AIrrigationBlockNodeNumber: Integer): IIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.Get_IrrigationBlockByBlockNodeNumber';
var
  lIndex            : Integer;
  lIrrigationBlock  : TIrrigationBlock;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationBlockList.Count)) do
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[lIndex]);
      if (lIrrigationBlock.FBlockNodeNumber = AIrrigationBlockNodeNumber) then
        Result := lIrrigationBlock
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber(ABlockNodeNumber: Integer): TIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber';
var
  lIndex            : Integer;
  lIrrigationBlock  : TIrrigationBlock;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FIrrigationBlockList.Count)) do
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[lIndex]);
      if (lIrrigationBlock.FBlockNodeNumber = ABlockNodeNumber) then
        Result := lIrrigationBlock
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.GetIrrigationBlocksPerHydrologyNodeNumber(AHydrologyNodeNumber: integer; AIrrigationBlocksList: TObjectList): boolean;
const OPNAME = 'TIrrigationBlockList.GetIrrigationBlocksPerHydrologyNodeNumber';
var
  lIndex            : Integer;
  lIrrigationBlock  : TIrrigationBlock;
begin
  Result := False;
  try
    if(AIrrigationBlocksList = nil) then Exit;
    AIrrigationBlocksList.Clear;
    for lIndex := 0 to FIrrigationBlockList.Count-1 do
    begin
      lIrrigationBlock := TIrrigationBlock(FIrrigationBlockList.Items[lIndex]);
      if (lIrrigationBlock.FHydrologyNodeNumber = AHydrologyNodeNumber) then
        AIrrigationBlocksList.Add(lIrrigationBlock);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlockList.CopyCreate(ABLockID: integer): IIrrigationBlock;
const OPNAME = 'TIrrigationBlockList.CopyCreate';
var
  LDestIrrigationBlock : TIrrigationBlock;
  LSourceIrrigationBlock : TIrrigationBlock;
begin
  Result := nil;
  try
    LSourceIrrigationBlock := CastIrrigationBlockByBlockNodeNumber(ABLockID);
    if LSourceIrrigationBlock <> nil then
    begin
      LDestIrrigationBlock := CreateNewIrrigationBlock(LSourceIrrigationBlock.IrrigationBlockType);
      if LDestIrrigationBlock <> nil then
      begin
        LDestIrrigationBlock.Assign(LSourceIrrigationBlock);
        Result := LDestIrrigationBlock;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIrrigationBlock.Get_IrrigationBlockType : integer;
const OPNAME = 'TIrrigationBlock.Get_IrrigationBlockType';
begin
  Result := NullInteger;
  try
    Result := FIrrigationBlockType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_IrrigationBlockType(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_IrrigationBlockType';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FIrrigationBlockType then
    begin

      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FIrrigationBlockType);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationType', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FIrrigationBlockType := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationType',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_CurtailIrrigationAbstraction : integer;
const OPNAME = 'TIrrigationBlock.Get_CurtailIrrigationAbstraction';
begin
  Result := NullInteger;
  try
    Result := FCurtailIrrigationAbstraction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_CurtailIrrigationAbstraction(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_CurtailIrrigationAbstraction';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FCurtailIrrigationAbstraction then
    begin

      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FCurtailIrrigationAbstraction);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('CurtailIrrigationAbstraction', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FCurtailIrrigationAbstraction := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CurtailIrrigationAbstraction',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_CanalSeepageLoss : double;
const OPNAME = 'TIrrigationBlock.Get_CanalSeepageLoss';
begin
  Result := NullFloat;
  try

    Result := FCanalSeepageLoss;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_CanalSeepageLoss(AValue : double);
const OPNAME = 'TIrrigationBlock.Set_CanalSeepageLoss';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FCanalSeepageLoss then
    begin

      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FCanalSeepageLoss);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('CanalSeepageLoss', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FCanalSeepageLoss := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CanalSeepageLoss',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_CanalTransmissionLoss : double;
const OPNAME = 'TIrrigationBlock.Get_CanalTransmissionLoss';
begin
  Result := NullFloat;
  try
    Result := FCanalTransmissionLoss;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_CanalTransmissionLoss(AValue : double);
const OPNAME = 'TIrrigationBlock.Set_CanalTransmissionLoss';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FCanalTransmissionLoss then
    begin

      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FCanalTransmissionLoss);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('CanalTransmissionLoss', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FCanalTransmissionLoss := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CanalTransmissionLoss',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_UpperSoilOutflow : double;
const OPNAME = 'TIrrigationBlock.Get_UpperSoilOutflow';
begin
  Result := NullFloat;
  try
    Result := FUpperSoilOutflow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_UpperSoilOutflow(AValue : double);
const OPNAME = 'TIrrigationBlock.Set_UpperSoilOutflow';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FUpperSoilOutflow then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FUpperSoilOutflow);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('UpperSoilOutflow', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FUpperSoilOutflow := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UpperSoilOutflow',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationBreakPointCount: integer;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationBreakPointCount';
begin
  Result := 0;
  try
    Result := FMaximumWaterAllocationBreakPointYear.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationBreakPointYearByIndex(AIndex: integer): integer;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationBreakPointYearByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= 0) and (AIndex < FMaximumWaterAllocationBreakPointYear.Count) then
      Result := StrToIntDef(FMaximumWaterAllocationBreakPointYear[AIndex],NullInteger);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FMaximumWaterAllocation.Count) then
      Result := StrToFloatDef(FMaximumWaterAllocation[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationGrowthByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationGrowthByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FMaximumWaterAllocationGrowth.Count) then
      Result := StrToFloatDef(FMaximumWaterAllocationGrowth[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_MaxUpperZoneMoisture : double;
const OPNAME = 'TIrrigationBlock.Get_MaxUpperZoneMoisture';
begin
  Result := NullFloat;
  try
    Result := FMaxUpperZoneMoisture;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MaximumWaterAllocationBreakPointCount(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_MaximumWaterAllocationBreakPointCount';
begin
  try
    if(FMaximumWaterAllocationBreakPointYear.Count = Value) and (FMaximumWaterAllocation.Count = Value) then Exit;
    FOldMaximumWaterAllocationBreakPointYearCommaText := FMaximumWaterAllocationBreakPointYear.CommaText;
    FOldMaximumWaterAllocationCommaText := FMaximumWaterAllocation.CommaText;
    FOldMaximumWaterAllocationGrowthCommaText := FMaximumWaterAllocationGrowth.CommaText;

    if(Value < FMaximumWaterAllocationBreakPointYear.Count) then
    begin
      while(FMaximumWaterAllocationBreakPointYear.Count > Value) do
      begin
        FMaximumWaterAllocationBreakPointYear.Delete(FMaximumWaterAllocationBreakPointYear.Count-1);
      end;
    end;
    if(Value < FMaximumWaterAllocation.Count) then
    begin
      while(FMaximumWaterAllocation.Count > Value) do
      begin
        FMaximumWaterAllocation.Delete(FMaximumWaterAllocation.Count-1);
      end;
    end;
    if(Value < FMaximumWaterAllocationGrowth.Count) then
    begin
      while(FMaximumWaterAllocationGrowth.Count > Value) do
      begin
        FMaximumWaterAllocationGrowth.Delete(FMaximumWaterAllocationGrowth.Count-1);
      end;
    end;
    if(Value > FMaximumWaterAllocationBreakPointYear.Count) then
    begin
      while(FMaximumWaterAllocationBreakPointYear.Count < Value) do
      begin
        FMaximumWaterAllocationBreakPointYear.Add('1900');
      end;
    end;
    if(Value > FMaximumWaterAllocation.Count) then
    begin
      while(FMaximumWaterAllocation.Count < Value) do
      begin
        FMaximumWaterAllocation.Add('0.0000');
      end;
    end;
    if(Value > FMaximumWaterAllocationGrowth.Count) then
    begin
      while(FMaximumWaterAllocationGrowth.Count < Value) do
      begin
        FMaximumWaterAllocationGrowth.Add('0.0000');
      end;
    end;
    SaveMaximumWaterAllocationBreakPointYear;
    SaveMaximumWaterAllocation;
    SaveMaximumWaterAllocationGrowth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_MaximumWaterAllocationBreakPointYearByIndex(AIndex, AValue: integer);
const OPNAME = 'TIrrigationBlock.Set_MaximumWaterAllocationBreakPointYearByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FMaximumWaterAllocationBreakPointYear.Count) then
    begin
      FOldMaximumWaterAllocationBreakPointYearCommaText := FMaximumWaterAllocationBreakPointYear.CommaText;
      FMaximumWaterAllocationBreakPointYear[AIndex] := IntToStr(AValue);
      SaveMaximumWaterAllocationBreakPointYear;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_MaximumWaterAllocationByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_MaximumWaterAllocationByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FMaximumWaterAllocation.Count) then
    begin
      FOldMaximumWaterAllocationCommaText := FMaximumWaterAllocation.CommaText;
      FMaximumWaterAllocation[AIndex] := FormatFloat('##0.0000',AValue);
      SaveMaximumWaterAllocation;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_MaximumWaterAllocationGrowthByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_MaximumWaterAllocationGrowthByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FMaximumWaterAllocationGrowth.Count) then
    begin
      FOldMaximumWaterAllocationGrowthCommaText := FMaximumWaterAllocationGrowth.CommaText;
      FMaximumWaterAllocationGrowth[AIndex] := FormatFloat('##0.0000',AValue);
      SaveMaximumWaterAllocationGrowth;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_MaxUpperZoneMoisture(AValue : double);
const OPNAME = 'TIrrigationBlock.Set_MaxUpperZoneMoisture';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMaxUpperZoneMoisture then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FMaxUpperZoneMoisture);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MaxUpperZoneMoisture', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FMaxUpperZoneMoisture := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxUpperZoneMoisture',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MinUpperZoneMoisture : double;
const OPNAME = 'TIrrigationBlock.Get_MinUpperZoneMoisture';
begin
  Result := NullFloat;
  try
    Result := FMinUpperZoneMoisture;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MultiplicationFactor: Double;
const OPNAME = 'TIrrigationBlock.Get_MultiplicationFactor';
begin
  Result := 0;
  try
    Result := FMultiplicationFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_MinUpperZoneMoisture(AValue : double);
const OPNAME = 'TIrrigationBlock.Set_MinUpperZoneMoisture';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMinUpperZoneMoisture then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FMinUpperZoneMoisture);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MinUpperZoneMoisture', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FMinUpperZoneMoisture := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinUpperZoneMoisture',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MultiplicationFactor(AValue: Double);
const OPNAME = 'TIrrigationBlock.Set_MultiplicationFactor';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMultiplicationFactor then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FMultiplicationFactor);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrBlockMultiplicationFactor', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FMultiplicationFactor := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrBlockMultiplicationFactor',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_CropTypesCount : integer;
const OPNAME = 'TIrrigationBlock.Get_CropTypesCount';
begin
  Result := NullInteger;
  try
    Result := FCropTypesCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_CropTypesCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_CropTypesCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FCropTypesCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FCropTypesCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('CropTypesCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FCropTypesCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CropTypesCount',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigatedAreaByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_IrrigatedAreaByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FIrrigatedArea.Count) then
      Result := StrToFloatDef(FIrrigatedArea[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_IrrigatedAreasBreakPointCount: integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorBreakPointsCount';
begin
  Result := 0;
  try
    Result := FIrrigatedAreasBreakPointYear.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_IrrigatedAreasBreakPointYearByIndex(AIndex: integer): integer;
const OPNAME = 'TIrrigationBlock.Get_IrrigatedAreasBreakPointYearByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= 0) and (AIndex < FIrrigatedAreasBreakPointYear.Count) then
      Result := StrToIntDef(FIrrigatedAreasBreakPointYear[AIndex],NullInteger);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_IrrigationEfficiencyBreakPointsCount: integer;
const OPNAME = 'TIrrigationBlock.Get_IrrigationEfficiencyBreakPointsCount';
begin
  Result := 0;
  try
    Result := FIrrigationEfficiencyBreakpointYear.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_IrrigationEfficiencyBreakpointYearByIndex(AIndex: integer): integer;
const OPNAME = 'TIrrigationBlock.Get_IrrigationEfficiencyBreakpointYearByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= 0) and (AIndex < FIrrigationEfficiencyBreakpointYear.Count) then
      Result := StrToIntDef(FIrrigationEfficiencyBreakpointYear[AIndex],NullInteger);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigationEfficiencyByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_IrrigationEfficiencyByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FIrrigationEfficiency.Count) then
      Result := StrToFloatDef(FIrrigationEfficiency[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigationSupplyCapacity : double;
const OPNAME = 'TIrrigationBlock.Get_IrrigationSupplyCapacity';
begin
  Result := NullFloat;
  try
    Result := FIrrigationSupplyCapacity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_IrrigatedAreaByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_IrrigatedAreaByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FIrrigatedArea.Count) then
    begin
      FOldIrrigatedAreaCommaText := FIrrigatedArea.CommaText;
      FIrrigatedArea[AIndex] := FormatFloat('##0.0000',AValue);
      SaveIrrigatedArea;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_IrrigatedAreasBreakPointCount(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_IrrigatedAreasBreakPointCount';
begin
  try

    if(FIrrigatedAreasBreakPointYear.Count = Value) and (FIrrigatedArea.Count = Value) then Exit;
    FOldIrrigatedAreasBreakPointYearCommaText :=   FIrrigatedAreasBreakPointYear.CommaText;
    FOldIrrigatedAreaCommaText := FIrrigatedArea.CommaText;
    if(Value < FIrrigatedAreasBreakPointYear.Count) then
    begin
      while(FIrrigatedAreasBreakPointYear.Count > Value) do
      begin
        FIrrigatedAreasBreakPointYear.Delete(FIrrigatedAreasBreakPointYear.Count-1);
      end;
    end;
    if(Value < FIrrigatedArea.Count) then
    begin
      while(FIrrigatedArea.Count > Value) do
      begin
        FIrrigatedArea.Delete(FIrrigatedArea.Count-1);
      end;
    end;
    if(Value > FIrrigatedAreasBreakPointYear.Count) then
    begin
      while(FIrrigatedAreasBreakPointYear.Count < Value) do
      begin
        FIrrigatedAreasBreakPointYear.Add('1900');
      end;
    end;
    if(Value > FIrrigatedArea.Count) then
    begin
      while(FIrrigatedArea.Count < Value) do
      begin
        FIrrigatedArea.Add('0.0000');
      end;
    end;

    SaveIrrigatedAreasBreakPointYear;
    SaveIrrigatedArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_IrrigatedAreasBreakPointYearByIndex(AIndex, AValue: integer);
const OPNAME = 'TIrrigationBlock.Set_IrrigatedAreasBreakPointYearByIndex';
begin
  try

    if(AIndex >= 0) and (AIndex < FIrrigatedAreasBreakPointYear.Count) then
    begin
      FOldIrrigatedAreasBreakPointYearCommaText :=   FIrrigatedAreasBreakPointYear.CommaText;
      FIrrigatedAreasBreakPointYear[AIndex] := IntToStr(AValue);
      SaveIrrigatedAreasBreakPointYear;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_IrrigationEfficiencyBreakPointsCount(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_IrrigationEfficiencyBreakPointsCount';
begin
  try
    if(FIrrigationEfficiencyBreakpointYear.Count = Value) and (FIrrigationEfficiency.Count = Value) then Exit;
    FOldIrrigationEfficiencyBreakpointYearCommaText := FIrrigationEfficiencyBreakpointYear.CommaText;
    FOldIrrigationEfficiencyCommaText := FIrrigationEfficiency.CommaText;
    if(Value < FIrrigationEfficiencyBreakpointYear.Count) then
    begin
      while(FIrrigationEfficiencyBreakpointYear.Count > Value) do
      begin
        FIrrigationEfficiencyBreakpointYear.Delete(FIrrigationEfficiencyBreakpointYear.Count-1);
      end;
    end;
    if(Value < FIrrigationEfficiency.Count) then
    begin
      while(FIrrigationEfficiency.Count > Value) do
      begin
        FIrrigationEfficiency.Delete(FIrrigationEfficiency.Count-1);
      end;
    end;
    if(Value > FIrrigationEfficiencyBreakpointYear.Count) then
    begin
      while(FIrrigationEfficiencyBreakpointYear.Count < Value) do
      begin
        FIrrigationEfficiencyBreakpointYear.Add('1900');
      end;
    end;
    if(Value > FIrrigationEfficiency.Count) then
    begin
      while(FIrrigationEfficiency.Count < Value) do
      begin
        FIrrigationEfficiency.Add('0.0000');
      end;
    end;
    SaveIrrigationEfficiencyBreakpointYear;
    SaveIrrigationEfficiency;
    Set_IrrigationEfficienciesPointsCount(Value);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_IrrigationEfficiencyBreakpointYearByIndex(AIndex, AValue: integer);
const OPNAME = 'TIrrigationBlock.Set_IrrigationEfficiencyBreakpointYearByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FIrrigationEfficiencyBreakpointYear.Count) then
    begin
      FOldIrrigationEfficiencyBreakpointYearCommaText := FIrrigationEfficiencyBreakpointYear.CommaText;
      FIrrigationEfficiencyBreakpointYear[AIndex] := IntToStr(AValue);
      SaveIrrigationEfficiencyBreakpointYear;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_IrrigationEfficiencyByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_IrrigationEfficiencyByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FIrrigationEfficiency.Count) then
    begin
      FOldIrrigationEfficiencyCommaText := FIrrigationEfficiency.CommaText;
      FIrrigationEfficiency[AIndex] := FormatFloat('##0.0000',AValue);
      SaveIrrigationEfficiency;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_IrrigationSupplyCapacity(AValue : double);
const OPNAME = 'TIrrigationBlock.Set_IrrigationSupplyCapacity';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FIrrigationSupplyCapacity then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FIrrigationSupplyCapacity);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationSupplyCapacity', FloatToStr(AValue), LOldValue, LContextData ) then
        begin
          FIrrigationSupplyCapacity := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationSupplyCapacity',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigatedAreaCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_IrrigatedAreaCommaText';
begin
  Result := '';
  try
    Result := FIrrigatedArea.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigatedAreasBreakPointYearCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_IrrigatedAreasBreakPointYearCommaText';
begin
  Result := '';
  try
    Result := FIrrigatedAreasBreakPointYear.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigationEfficiencyBreakpointYearCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_IrrigationEfficiencyBreakpointYearCommaText';
begin
  Result := '';
  try
    Result := FIrrigationEfficiencyBreakpointYear.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigationEfficiencyCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_IrrigationEfficiencyCommaText';
begin
  Result := '';
  try
    Result := FIrrigationEfficiency.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationBreakPointYearCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationBreakPointYearCommaText';
begin
  Result := '';
  try
    Result := FMaximumWaterAllocationBreakPointYear.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationCommaText';
begin
  Result := '';
  try
    Result := FMaximumWaterAllocation.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MaximumWaterAllocationGrowthCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_IrrigatedAreaCommaText';
begin
  Result := '';
  try
    Result := FReturnFlowFactorBreakpointYear.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowFactorBreakpointYearCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorBreakpointYearCommaText';
begin
  Result := '';
  try
    Result := FIrrigatedArea.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowFactorsCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorsCommaText';
begin
  Result := '';
  try
    Result := FReturnFlowFactors.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowVolumeBreakpointYearCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowVolumeBreakpointYearCommaText';
begin
  Result := '';
  try
    Result := FReturnFlowVolumeBreakpointYear.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowVolumeCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowVolumeCommaText';
begin
  Result := '';
  try
    Result := FReturnFlowVolume.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_SupplyCapacityBreakpointYearCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_SupplyCapacityBreakpointYearCommaText';
begin
  Result := '';
  try
    Result := FSupplyCapacityBreakpointYear.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_SupplyCapacityCommaText: string;
const OPNAME = 'TIrrigationBlock.Get_SupplyCapacityCommaText';
begin
  Result := '';
  try
    Result := FSupplyCapacity.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_AllocatedAreaPointsCount : integer;
const OPNAME = 'TIrrigationBlock.Get_AllocatedAreaPointsCount';
begin
  Result := NullInteger;
  try
    Result := FAllocatedAreaPointsCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveIrrigatedArea: boolean;
const OPNAME = 'TIrrigationBlock.SaveIrrigatedArea';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointArea', FIrrigatedArea.CommaText,
      FOldIrrigatedAreaCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointArea',FOldIrrigatedAreaCommaText,
        FIrrigatedArea.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveIrrigatedAreasBreakPointYear: boolean;
const OPNAME = 'TIrrigationBlock.SaveIrrigatedAreasBreakPointYear';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointYearsDefined', FIrrigatedAreasBreakPointYear.CommaText,
      FOldIrrigatedAreasBreakPointYearCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointYearsDefined',FOldIrrigatedAreasBreakPointYearCommaText,
        FIrrigatedAreasBreakPointYear.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveIrrigationEfficiency: boolean;
const OPNAME = 'TIrrigationBlock.SaveIrrigationEfficiency';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointIrrigationEfficiencies', FIrrigationEfficiency.CommaText,
      FOldIrrigationEfficiencyCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointIrrigationEfficiencies',FOldIrrigationEfficiencyCommaText,
        FIrrigationEfficiency.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveIrrigationEfficiencyBreakpointYear: boolean;
const OPNAME = 'TIrrigationBlock.SaveIrrigationEfficiencyBreakpointYear';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointYearsIrrigationEfficiencies', FIrrigationEfficiencyBreakpointYear.CommaText,
      FOldIrrigationEfficiencyBreakpointYearCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointYearsIrrigationEfficiencies',FOldIrrigationEfficiencyBreakpointYearCommaText,
        FIrrigationEfficiencyBreakpointYear.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveMaximumWaterAllocation: boolean;
const OPNAME = 'TIrrigationBlock.SaveMaximumWaterAllocation';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointMaxWaterAllocation', FMaximumWaterAllocation.CommaText,
      FOldMaximumWaterAllocationCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointMaxWaterAllocation',FOldMaximumWaterAllocationCommaText,
        FMaximumWaterAllocation.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveMaximumWaterAllocationBreakPointYear: boolean;
const OPNAME = 'TIrrigationBlock.SaveMaximumWaterAllocationBreakPointYear';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointYearsMaxWaterAllocation', FMaximumWaterAllocationBreakPointYear.CommaText,
      FOldMaximumWaterAllocationBreakPointYearCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointYearsMaxWaterAllocation',FOldMaximumWaterAllocationBreakPointYearCommaText,
        FMaximumWaterAllocationBreakPointYear.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveMaximumWaterAllocationGrowth: boolean;
const OPNAME = 'TIrrigationBlock.SaveMaximumWaterAllocationGrowth';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointMaxWaterAllocationGrowth', FMaximumWaterAllocationGrowth.CommaText,
      FOldMaximumWaterAllocationGrowthCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointMaxWaterAllocationGrowth',FOldMaximumWaterAllocationGrowthCommaText,
        FMaximumWaterAllocationGrowth.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveReturnFlowFactorBreakpointYear: boolean;
const OPNAME = 'TIrrigationBlock.SaveReturnFlowFactorBreakpointYear';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointYearsReturnFlowFactor', FReturnFlowFactorBreakpointYear.CommaText,
      FOldReturnFlowFactorBreakpointYearCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointYearsReturnFlowFactor',FOldReturnFlowFactorBreakpointYearCommaText,
        FReturnFlowFactorBreakpointYear.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveReturnFlowFactors: boolean;
const OPNAME = 'TIrrigationBlock.SaveReturnFlowFactors';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointReturnFlowFactor', FReturnFlowFactors.CommaText,
      FOldReturnFlowFactorsCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointReturnFlowFactor',FOldReturnFlowFactorsCommaText,
        FReturnFlowFactors.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveReturnFlowVolume: boolean;
const OPNAME = 'TIrrigationBlock.SaveReturnFlowVolume';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointReturnFlowVolume', FReturnFlowVolume.CommaText,
      FOldReturnFlowVolumeCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointReturnFlowVolume',FOldReturnFlowVolumeCommaText,
        FReturnFlowVolume.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveReturnFlowVolumeBreakpointYear: boolean;
const OPNAME = 'TIrrigationBlock.SaveReturnFlowVolumeBreakpointYear';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointYearsReturnFlowVolume', FReturnFlowVolumeBreakpointYear.CommaText,
      FOldReturnFlowVolumeBreakpointYearCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointYearsReturnFlowVolume',FOldReturnFlowVolumeBreakpointYearCommaText,
        FReturnFlowVolumeBreakpointYear.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveSupplyCapacity: boolean;
const OPNAME = 'TIrrigationBlock.SaveSupplyCapacity';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointSupplyCapacity', FSupplyCapacity.CommaText,
      FOldSupplyCapacityCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointSupplyCapacity',FOldSupplyCapacityCommaText,
        FSupplyCapacity.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.SaveSupplyCapacityBreakpointYear: boolean;
const OPNAME = 'TIrrigationBlock.SaveSupplyCapacityBreakpointYear';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
begin
  Result := False;
  try
    LContextData := TStringList.Create;
    LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
      if FAppModules.FieldProperties.UpdateFieldValue('BreakpointYearsSupplyCapacity', FSupplyCapacityBreakpointYear.CommaText,
      FOldSupplyCapacityBreakpointYearCommaText, LContextData ) then
      begin
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'BreakpointYearsSupplyCapacity',FOldSupplyCapacityBreakpointYearCommaText,
        FSupplyCapacityBreakpointYear.CommaText);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_AllocatedAreaPointsCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_AllocatedAreaPointsCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FAllocatedAreaPointsCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FAllocatedAreaPointsCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('AllocatedAreaPointsCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FAllocatedAreaPointsCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocatedAreaPointsCount',LOldValue,IntToStr(AValue));
          Set_IrrigatedAreasBreakPointCount(AValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MethodIrrigatedAreas : integer;
const OPNAME = 'TIrrigationBlock.Get_MethodIrrigatedAreas';
begin
  Result := NullInteger;
  try
    Result := FMethodIrrigatedAreas;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MethodIrrigatedAreas(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MethodIrrigatedAreas';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMethodIrrigatedAreas then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMethodIrrigatedAreas);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MethodIrrigatedAreas', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMethodIrrigatedAreas := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MethodIrrigatedAreas',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MaxWaterAllocationCount : integer;
const OPNAME = 'TIrrigationBlock.Get_MaxWaterAllocationCount';
begin
  Result := NullInteger;
  try
    Result := FMaxWaterAllocationCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MaxWaterAllocationCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MaxWaterAllocationCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMaxWaterAllocationCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMaxWaterAllocationCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MaxWaterAllocationCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMaxWaterAllocationCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxWaterAllocationCount',LOldValue,IntToStr(AValue));
          Set_MaximumWaterAllocationBreakPointCount(AValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MethodMaxWaterAllocation : integer;
const OPNAME = 'TIrrigationBlock.Get_MethodMaxWaterAllocation';
begin
  Result := NullInteger;
  try
    Result := FMethodMaxWaterAllocation;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MethodMaxWaterAllocation(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MethodMaxWaterAllocation';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMethodMaxWaterAllocation then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMethodMaxWaterAllocation);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MethodMaxWaterAllocation', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMethodMaxWaterAllocation := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MethodMaxWaterAllocation',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowVolumeBreakPointsCount: integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowVolumeBreakPointsCount';
begin
  Result := 0;
  try
    Result := FReturnFlowVolumeBreakpointYear.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowVolumeBreakpointYearByIndex(AIndex: integer): integer;
const OPNAME = 'TIrrigationBlock.Get_MaximumWaterAllocationGrowthByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowVolumeBreakpointYear.Count) then
      Result := StrToIntDef(FReturnFlowVolumeBreakpointYear[AIndex],NullInteger);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowVolumeByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowVolumeByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowVolume.Count) then
      Result := StrToFloatDef(FReturnFlowVolume[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_ReturnFlowVolumePointsCount : integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowVolumePointsCount';
begin
  Result := NullInteger;
  try
    Result := FReturnFlowVolumePointsCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_ReturnFlowVolumeBreakPointsCount(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowVolumeBreakPointsCount';
begin
  try
    if(FReturnFlowVolumeBreakpointYear.Count = Value) and (FReturnFlowVolume.Count = Value) then Exit;
    FOldReturnFlowVolumeBreakpointYearCommaText := FReturnFlowVolumeBreakpointYear.CommaText;
    FOldReturnFlowVolumeCommaText := FReturnFlowVolume.CommaText;
    if(Value < FReturnFlowVolumeBreakpointYear.Count) then
    begin
      while(FReturnFlowVolumeBreakpointYear.Count > Value) do
      begin
        FReturnFlowVolumeBreakpointYear.Delete(FReturnFlowVolumeBreakpointYear.Count-1);
      end;
    end;
    if(Value < FReturnFlowVolume.Count) then
    begin
      while(FReturnFlowVolume.Count > Value) do
      begin
        FReturnFlowVolume.Delete(FReturnFlowVolume.Count-1);
      end;
    end;
    if(Value > FReturnFlowVolumeBreakpointYear.Count) then
    begin
      while(FReturnFlowVolumeBreakpointYear.Count < Value) do
      begin
        FReturnFlowVolumeBreakpointYear.Add('1900');
      end;
    end;
    if(Value > FReturnFlowVolume.Count) then
    begin
      while(FReturnFlowVolume.Count < Value) do
      begin
        FReturnFlowVolume.Add('0.0000');
      end;
    end;
    SaveReturnFlowVolumeBreakpointYear;
    SaveReturnFlowVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowVolumeBreakpointYearByIndex(AIndex, AValue: integer);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowVolumeBreakpointYearByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowVolumeBreakpointYear.Count) then
    begin
      FOldReturnFlowVolumeBreakpointYearCommaText := FReturnFlowVolumeBreakpointYear.CommaText;
      FReturnFlowVolumeBreakpointYear[AIndex] := IntToStr(AValue);
      SaveReturnFlowVolumeBreakpointYear;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowVolumeByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowVolumeByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowVolume.Count) then
    begin
      FOldReturnFlowVolumeCommaText := FReturnFlowVolume.CommaText;
      FReturnFlowVolume[AIndex] := FormatFloat('##0.0000',AValue);
      SaveReturnFlowVolume;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowVolumePointsCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowVolumePointsCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FReturnFlowVolumePointsCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FReturnFlowVolumePointsCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('ReturnFlowVolumePointsCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FReturnFlowVolumePointsCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReturnFlowVolumePointsCount',LOldValue,IntToStr(AValue));
          Set_ReturnFlowVolumeBreakPointsCount(AValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MethodReturnFlowVolume : integer;
const OPNAME = 'TIrrigationBlock.Get_MethodReturnFlowVolume';
begin
  Result := NullInteger;
  try
    Result := FMethodReturnFlowVolume;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MethodReturnFlowVolume(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MethodReturnFlowVolume';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMethodReturnFlowVolume then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMethodReturnFlowVolume);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MethodReturnFlowVolume', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMethodReturnFlowVolume := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MethodReturnFlowVolume',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_SupplyCapacityBreakPointsCount: integer;
const OPNAME = 'TIrrigationBlock.Get_SupplyCapacityBreakPointsCount';
begin
  Result := 0;
  try
    Result := FSupplyCapacityBreakpointYear.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_SupplyCapacityBreakpointYearByIndex(AIndex: integer): integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowVolumeByIndex';
begin
  Result := NullInteger;
  try
    if(AIndex >= 0) and (AIndex < FSupplyCapacityBreakpointYear.Count) then
      Result := StrToIntDef(FSupplyCapacityBreakpointYear[AIndex],NullInteger);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationBlock.Get_SupplyCapacityByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_SupplyCapacityByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FSupplyCapacity.Count) then
      Result := StrToFloatDef(FSupplyCapacity[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_SupplyCapacityPointsCount : integer;
const OPNAME = 'TIrrigationBlock.Get_SupplyCapacityPointsCount';
begin
  Result := NullInteger;
  try
    Result := FSupplyCapacityPointsCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_SupplyCapacityBreakPointsCount(Value: Integer);
const OPNAME = 'TIrrigationBlock.Set_SupplyCapacityBreakPointsCount';
begin
  try
    if(FSupplyCapacityBreakpointYear.Count = Value) and (FSupplyCapacity.Count = Value) then Exit;
    FOldSupplyCapacityBreakpointYearCommaText := FSupplyCapacityBreakpointYear.CommaText;
    FOldSupplyCapacityCommaText := FSupplyCapacity.CommaText;
    if(Value < FSupplyCapacityBreakpointYear.Count) then
    begin
      while(FSupplyCapacityBreakpointYear.Count > Value) do
      begin
        FSupplyCapacityBreakpointYear.Delete(FSupplyCapacityBreakpointYear.Count-1);
      end;
    end;
    if(Value < FSupplyCapacity.Count) then
    begin
      while(FSupplyCapacity.Count > Value) do
      begin
        FSupplyCapacity.Delete(FSupplyCapacity.Count-1);
      end;
    end;
    if(Value > FSupplyCapacityBreakpointYear.Count) then
    begin
      while(FSupplyCapacityBreakpointYear.Count < Value) do
      begin
        FSupplyCapacityBreakpointYear.Add('1900');
      end;
    end;
    if(Value > FSupplyCapacity.Count) then
    begin
      while(FSupplyCapacity.Count < Value) do
      begin
        FSupplyCapacity.Add('0.0000');
      end;
    end;
    SaveSupplyCapacityBreakpointYear;
    SaveSupplyCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_SupplyCapacityBreakpointYearByIndex(AIndex, AValue: integer);
const OPNAME = 'TIrrigationBlock.Set_SupplyCapacityBreakpointYearByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FSupplyCapacityBreakpointYear.Count) then
    begin
      FOldSupplyCapacityBreakpointYearCommaText := FSupplyCapacityBreakpointYear.CommaText;
      FSupplyCapacityBreakpointYear[AIndex] := IntToStr(AValue);
      SaveSupplyCapacityBreakpointYear;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_SupplyCapacityByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_SupplyCapacityByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FSupplyCapacity.Count) then
    begin
      FOldSupplyCapacityCommaText := FSupplyCapacity.CommaText;
      FSupplyCapacity[AIndex] := FormatFloat('##0.0000',AValue);
      SaveSupplyCapacity;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_SupplyCapacityPointsCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_SupplyCapacityPointsCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FSupplyCapacityPointsCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FSupplyCapacityPointsCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('SupplyCapacityPointsCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FSupplyCapacityPointsCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupplyCapacityPointsCount',LOldValue,IntToStr(AValue));
          Set_SupplyCapacityBreakPointsCount(AValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MethodSupplyCapacity : integer;
const OPNAME = 'TIrrigationBlock.Get_MethodSupplyCapacity';
begin
  Result := NullInteger;
  try
    Result := FMethodSupplyCapacity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MethodSupplyCapacity(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MethodSupplyCapacity';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMethodSupplyCapacity then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMethodSupplyCapacity);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MethodSupplyCapacity', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMethodSupplyCapacity := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MethodSupplyCapacity',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_IrrigationEfficienciesPointsCount : integer;
const OPNAME = 'TIrrigationBlock.Get_IrrigationEfficienciesPointsCount';
begin
  Result := NullInteger;
  try
    Result := FIrrigationEfficienciesPointsCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_IrrigationEfficienciesPointsCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_IrrigationEfficienciesPointsCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FIrrigationEfficienciesPointsCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FIrrigationEfficienciesPointsCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationEfficienciesPointsCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FIrrigationEfficienciesPointsCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationEfficienciesPointsCount',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MethodIrrigationEfficiencies : integer;
const OPNAME = 'TIrrigationBlock.Get_MethodIrrigationEfficiencies';
begin
  Result := NullInteger;
  try
    Result := FMethodIrrigationEfficiencies;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MethodIrrigationEfficiencies(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MethodIrrigationEfficiencies';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMethodIrrigationEfficiencies then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMethodIrrigationEfficiencies);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MethodIrrigationEfficiencies', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMethodIrrigationEfficiencies := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MethodIrrigationEfficiencies',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowFactorsByIndex(AIndex: integer): double;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorBreakpointYearByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowFactors.Count) then
      Result := StrToFloatDef(FReturnFlowFactors[AIndex],NullFloat);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_ReturnFlowFactorsCount : integer;
const OPNAME = 'TIrrigationBlock.Get_ReturnFlowFactorsCount';
begin
  Result := NullInteger;
  try
    Result := FReturnFlowFactorsCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_ReturnFlowFactorsByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowFactorsByIndex';
begin
  try
    if(AIndex >= 0) and (AIndex < FReturnFlowFactors.Count) then
    begin
      FOldReturnFlowFactorsCommaText := FReturnFlowFactors.CommaText;
      FReturnFlowFactors[AIndex] := FormatFloat('##0.0000',AValue);
      SaveReturnFlowFactors;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIrrigationBlock.Set_ReturnFlowFactorsCount(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_ReturnFlowFactorsCount';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FReturnFlowFactorsCount then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FReturnFlowFactorsCount);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('ReturnFlowFactorsCount', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FReturnFlowFactorsCount := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReturnFlowFactorsCount',LOldValue,IntToStr(AValue));
          Set_ReturnFlowFactorBreakPointsCount(AValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlock.Get_MethodReturnFlowFactors : integer;
const OPNAME = 'TIrrigationBlock.Get_MethodReturnFlowFactors';
begin
  Result := NullInteger;
  try
    Result := FMethodReturnFlowFactors;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlock.Set_MethodReturnFlowFactors(AValue : integer);
const OPNAME = 'TIrrigationBlock.Set_MethodReturnFlowFactors';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if AValue <> FMethodReturnFlowFactors then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FMethodReturnFlowFactors);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('MethodReturnFlowFactors', IntToStr(AValue), LOldValue, LContextData ) then
        begin
          FMethodReturnFlowFactors := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MethodReturnFlowFactors',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{ TWaterUsage }

function TWaterUsage._AddRef: Integer;
const OPNAME = 'TWaterUsage._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage._Release: Integer;
const OPNAME = 'TWaterUsage._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUsage.CreateMemberObjects;
const OPNAME = 'TWaterUsage.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUsage.DestroyMemberObjects;
const OPNAME = 'TWaterUsage.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage.Get_BlockIdentifier: Integer;
const OPNAME = 'TWaterUsage.Get_BlockIdentifier';
begin
  Result := FBlockIdentifier;
end;

function TWaterUsage.Get_CropName: WideString;
const OPNAME = 'TWaterUsage.Get_CropName';
begin
  Result := FCropName;
end;

function TWaterUsage.Get_Identifier: Integer;
const OPNAME = 'TWaterUsage.Get_Identifier';
begin
  Result := FIdentifier;
end;

function TWaterUsage.Get_MonthlyWaterUse(AIndex: Integer): Double;
const OPNAME = 'TWaterUsage.Get_MonthlyWaterUse';
begin
  Result := FMonthlyWaterUse[AIndex];
end;

function TWaterUsage.Get_PercAreaUnderCropType: Double;
const OPNAME = 'TWaterUsage.Get_PercAreaUnderCropType';
begin
  Result := FPercAreaUnderCropType;
end;

function TWaterUsage.Initialise: Boolean;
const OPNAME = 'TWaterUsage.Initialise';
var
  LCount : Integer;
begin
  Result := inherited Initialise;
  try
    FBlockIdentifier      := 1;
    FIdentifier           := 1;
    FCropName             := '';
    FPercAreaUnderCropType:= 0;
    for LCount := MinMonths to MaxMonths do
      FMonthlyWaterUse[LCount] := 0.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage.Populate(AIdentifier,
                              ABlockIdentifier  : Integer;
                              ACropName         : WideString;
                              APercArea         : Double;
                              AMonthWaterUse    : TMonthlyDoubleArray): WordBool;
const OPNAME = 'TWaterUsage.Populate';
var
  LCount : Integer;
begin
  Result := False;
  try
    FIdentifier            := AIdentifier;
    FBlockIdentifier       := ABlockIdentifier;
    FCropName              := ACropName;
    FPercAreaUnderCropType := APercArea;
    for LCount := MinMonths to MaxMonths do
      FMonthlyWaterUse[LCount] := AMonthWaterUse[LCount];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage.PopulateIDs(AIdentifier: integer; ACropName: string): boolean;
const OPNAME = 'TWaterUsage.PopulateIDs';
begin
  Result := False;
  try
    FIdentifier            := AIdentifier;
    FCropName              := ACropName;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUsage.Set_BlockIdentifier(Value: Integer);
const OPNAME = 'TWaterUsage.Set_BlockIdentifier';
begin
  FBlockIdentifier := Value;
end;

procedure TWaterUsage.Set_CropName(const Value: WideString);
const OPNAME = 'TWaterUsage.Set_CropName';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FCropName <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FCropName;
        LLoadAgent.LoadContextData_FeatureIDWaterUse(
                                      LContextData,
                                      IntToStr(FBlockIdentifier),
                                      IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockWaterUsageCropName',Value, LOldValue, LContextData ) then
        begin
          FCropName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockWaterUsageCropName',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUsage.Set_Identifier(Value: Integer);
const OPNAME = 'TWaterUsage.Set_Identifier';
begin
  FIdentifier := Value;
end;

procedure TWaterUsage.Set_MonthlyWaterUse(AIndex: Integer; Value: Double);
const OPNAME = 'TWaterUsage.Set_MonthlyWaterUse';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FMonthlyWaterUse[AIndex] <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FMonthlyWaterUse[AIndex]);
        LLoadAgent.LoadContextData_FactorWaterUse(
                      LContextData,
                      IntToStr(FBlockIdentifier),
                      IntToStr(FIdentifier),
                      IntToStr(AIndex) );
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockWaterUsageFactor', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FMonthlyWaterUse[AIndex] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockWaterUsageFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUsage.Set_PercAreaUnderCropType(Value: Double);
const OPNAME = 'TWaterUsage.Set_PercAreaUnderCropType';
var
  LLoadAgent : TIrrigationBlockSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    if FPercAreaUnderCropType <> Value then
    begin
      LContextData := TStringList.Create;
      LLoadAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FPercAreaUnderCropType);
        LLoadAgent.LoadContextData_FeatureIDWaterUse(
                                      LContextData,
                                      IntToStr(FBlockIdentifier),
                                      IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('IrrigationBlockPercAreaUnderCropType', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FPercAreaUnderCropType := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationBlockPercAreaUnderCropType',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TWaterUsage.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'IrrigationBlockPercAreaUnderCropType') then
        Result := ValidateWaterUsePercAreaUnderCropType(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockWaterUsageCropName') then
        Result := ValidateWaterUseCropName(lErrorMessage)
      else
      if (AContext = 'IrrigationBlockWaterUsageFactor') then
      begin
        Result := ValidateWaterUseMonthlyWaterUse(LErrorMessage,LErrorCols);
        if (not Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorMessage.Text
          else
            AErrors := AErrors + CTStringsSeparator + LErrorMessage.Text + CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
        end;
      end
      else
      begin
        Result := True;
        if (not ValidateWaterUsePercAreaUnderCropType(lErrorMessage)) then Result := False else
        if (not ValidateWaterUseCropName(lErrorMessage))              then Result := False;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage.ValidateWaterUseCropName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWaterUsage.ValidateWaterUseCropName';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockWaterUsageCropName', FCropName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FCropName + ':'+lMessage);
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUsage.ValidateWaterUseMonthlyWaterUse(AErrorMessages: TStrings; AErrorColumns: TStringList): boolean;
const OPNAME = 'TWaterUsage.ValidateWaterUseMonthlyWaterUse';
var
  LFieldProperty : TAbstractFieldProperty;
  LMessage : string;
  LIndex : integer;
begin
  Result := False;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageFactor');
    if (LFieldProperty <> nil) then
    begin
      AErrorColumns.Clear;
      for lIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
      begin
        lMessage := '';
        if (FMonthlyWaterUse[lIndex] <> NullFloat) then
        begin
          if (not FAppModules.FieldProperties.ValidateFieldProperty
             ('IrrigationBlockWaterUsageFactor', FloatToStr(FMonthlyWaterUse[LIndex]),
             lMessage, LIndex)) then
          begin
            AErrorMessages.Add('ERROR:' +LMessage);
            AErrorColumns.Add(IntToStr(LIndex));
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUsage.ValidateWaterUsePercAreaUnderCropType(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWaterUsage.ValidateWaterUsePercAreaUnderCropType';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockPercAreaUnderCropType', FloatToStr(FPercAreaUnderCropType), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FPercAreaUnderCropType) + ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUsage.Assign(ASource: TWaterUsage);
const OPNAME = 'TWaterUsage.Assign';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDiversionChannelMaxDemand }

function TDiversionChannelMaxDemand.Get_MaxDemandByIndex(AIndex: integer): double;
const OPNAME = 'TDiversionChannelMaxDemand.Get_MaxDemandByIndex';
begin
  Result := NullFloat;
  try
    if(AIndex >= Low(FMaxDemand)) and (AIndex <= High(FMaxDemand)) then
      Result := FMaxDemand[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionChannelMaxDemand.Initialise: boolean;
const OPNAME = 'TDiversionChannelMaxDemand.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FChannelNumber := NullInteger;
    FInUse         := False;
    for LIndex := Low(FMaxDemand) to High(FMaxDemand) do
      FMaxDemand[LIndex] := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionChannelMaxDemand.Populated: boolean;
const OPNAME = 'TDiversionChannelMaxDemand.Populated';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := Low(FMaxDemand) to High(FMaxDemand) do
    begin
      if(FMaxDemand[LIndex] >= 0) then
      begin
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionChannelMaxDemand.PopulateMaxDemandValues(AIrrigationBlockNumber,AChannelNumber : integer;
          AMaxDemands: TMonthlyDoubleArray; AInUse : boolean);
const OPNAME = 'TDiversionChannelMaxDemand.PopulateMaxDemandValues';
var
  LIndex : integer;
begin
  try
    FIrrigationBlockNumber := AIrrigationBlockNumber;
    FChannelNumber := AChannelNumber;
    FInUse         := AInUse;
    for LIndex := Low(FMaxDemand) to High(FMaxDemand) do
    begin
      FMaxDemand[LIndex] := AMaxDemands[LIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionChannelMaxDemand.StudyHasChanged: boolean;
const OPNAME = 'TDiversionChannelMaxDemand.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionChannelMaxDemand.Set_MaxDemandByIndex(AIndex: integer;  AValue: Double);
const OPNAME = 'TDiversionChannelMaxDemand.Set_MaxDemandByIndex';
var
  LOldValue : double;
begin
  try
    if(AIndex >= Low(FMaxDemand)) and (AIndex <= High(FMaxDemand)) then
    begin
      if Not Populated then
      begin
        InitialiseValues(AValue);
        FInUse := True;
      end;
      
      LOldValue := FMaxDemand[AIndex];
      FMaxDemand[AIndex] := AValue;
      if Update_MaxDemands then
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'OUTPUTDATASELECTION','','')
      else
        FMaxDemand[AIndex] := LOldValue;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionChannelMaxDemand.Get_InUse: boolean;
const OPNAME = 'TDiversionChannelMaxDemand.Get_InUse';
begin
  Result := False;
  try
    Result := FInUse;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionChannelMaxDemand.Set_InUse(AValue: boolean);
const OPNAME = 'TDiversionChannelMaxDemand.Set_InUse';
var
  LOldValue : boolean;
begin
  try
    LOldValue := FInUse;
    FInUse := AValue;
    if Not Populated then
      InitialiseValues(0.0);
    if Update_MaxDemands then
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'OUTPUTDATASELECTION','','')
    else
      FInUse := LOldValue;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionChannelMaxDemand.Update_MaxDemands: boolean;
const OPNAME = 'TDiversionChannelMaxDemand.Update_MaxDemands';
var
  LAgent : TIrrigationBlockSQLAgent;
begin
  Result := False;
  try
    LAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      Result := LAgent.SaveDiversionChannelMaxDemand(FIrrigationBlockNumber,FChannelNumber,FMaxDemand,FInUse);
    finally
      LAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionChannelMaxDemand.InitialiseValues(AValue: double);
const OPNAME = 'TDiversionChannelMaxDemand.InitialiseValues';
var
  LIndex : integer;
begin
  try
    FInUse := True;
    for LIndex := Low(FMaxDemand) to High(FMaxDemand) do
    begin
      FMaxDemand[LIndex] := AValue;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.




