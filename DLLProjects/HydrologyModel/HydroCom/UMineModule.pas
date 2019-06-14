(******************************************************************************)
(*  Contains : Class TMineModule.
(******************************************************************************)
unit UMineModule;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UModule,
  UAbstractObject,
  HydrologyCom_TLB;

type
  TLoadFlowRefPair = class(TObject)
  protected
    FLoad    : Double;
    FFlowRef : Double;
  public
    property Load    : Double read FLoad    write FLoad;
    property FlowRef : Double read FFlowRef write FFlowRef;
  end;

  TSlurryDump = class(TAbstractObject, ISlurryDump)
  protected
    FModuleID              : Integer;
    FSectionNo             : Integer;
    FSectionName           : String;
    FArea                  : Double;
    FRunOffFactor          : Double;
    FSeepProportion        : Double;
    FPCDFullSupplyVolume   : Double;
    FPCDFullSupplyArea     : Double;
    FPCDInitialVolume      : Double;
    FInterpolationOption   : Integer;
    FStdDev                : Double;
    FAreaGrowthData        : TObjectList;
    FMonthlyRechargeFactor : TMonthlyDoubleArray;
    FQvsSLD                : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ModuleID: Integer; safecall;
    procedure Set_ModuleID(Value: Integer); safecall;
    function Get_SectionNo: Integer; safecall;
    procedure Set_SectionNo(Value: Integer); safecall;
    function Get_SectionName: WideString; safecall;
    procedure Set_SectionName(const Value: WideString); safecall;
    function Get_Area: Double; safecall;
    procedure Set_Area(Value: Double); safecall;
    function Get_RunOffFactor: Double; safecall;
    procedure Set_RunOffFactor(Value: Double); safecall;
    function Get_SeepProportion: Double; safecall;
    procedure Set_SeepProportion(Value: Double); safecall;
    function Get_PCDFullSupplyVolume: Double; safecall;
    procedure Set_PCDFullSupplyVolume(Value: Double); safecall;
    function Get_PCDFullSupplyArea: Double; safecall;
    procedure Set_PCDFullSupplyArea(Value: Double); safecall;
    function Get_PCDInitialVolume: Double; safecall;
    procedure Set_PCDInitialVolume(Value: Double); safecall;
    function Get_InterpolationOption: Integer; safecall;
    procedure Set_InterpolationOption(Value: Integer); safecall;
    function Get_StdDev: Double; safecall;
    procedure Set_StdDev(Value: Double); safecall;
    function Populate (AModuleID             : Integer;
                       ASectionNo            : Integer;
                       const ASectionName    : WideString;
                       AArea                 : Double;
                       ARunOffFactor         : Double;
                       ASeepProportion       : Double;
                       APCDFullSupplyVolume  : Double;
                       APCDFullSupplyArea    : Double;
                       APCDInitialVolume     : Double;
                       AInterpolatrionOption : Integer;
                       AStdDev               : Double): WordBool; safecall;
    function AddAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_NoOfAreaGrowthPoints: Integer; safecall;
    function Get_AreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_AreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_AreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_RechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_RechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function Get_QvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_QvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_QvsSLDNoOfPoints: Integer; safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    function UpdateGrowthData (ARootNode : IXMLNode): Boolean;
    function UpdateMonthlyData (ARootNode : IXMLNode): Boolean;
    function UpdateQSLDData (ARootNode : IXMLNode) : Boolean;
    procedure ClearAreaGrowthData;
    procedure ClearQvsSLDData;

    property ModuleID: Integer read Get_ModuleID write Set_ModuleID;
    property SectionNo: Integer read Get_SectionNo write Set_SectionNo;
    property SectionName: WideString read Get_SectionName write Set_SectionName;
    property Area: Double read Get_Area write Set_Area;
    property RunOffFactor: Double read Get_RunOffFactor write Set_RunOffFactor;
    property SeepProportion: Double read Get_SeepProportion write Set_SeepProportion;
    property PCDFullSupplyVolume: Double read Get_PCDFullSupplyVolume write Set_PCDFullSupplyVolume;
    property PCDFullSupplyArea: Double read Get_PCDFullSupplyArea write Set_PCDFullSupplyArea;
    property PCDInitialVolume: Double read Get_PCDInitialVolume write Set_PCDInitialVolume;
    property InterpolationOption: Integer read Get_InterpolationOption write Set_InterpolationOption;
    property StdDev: Double read Get_StdDev write Set_StdDev;
    property NoOfAreaGrowthPoints: Integer read Get_NoOfAreaGrowthPoints;
    property AreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_AreaGrowthYearByIndex write Set_AreaGrowthYearByIndex;
    property AreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_AreaGrowthFactorByIndex write Set_AreaGrowthFactorByIndex;
    property RechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_RechargeFactorByMonth write Set_RechargeFactorByMonth;
    property QvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_QvsSLDFlowRefByIndex write Set_QvsSLDFlowRefByIndex;
    property QvsSLDLoadByIndex[AIndex: Integer]: Double read Get_QvsSLDLoadByIndex write Set_QvsSLDLoadByIndex;
    property QvsSLDNoOfPoints: Integer read Get_QvsSLDNoOfPoints;
  end;
  
  TUndergroundSection = class(TAbstractObject, IUndergroundSection)
  protected
    FModuleID                                  : Integer;
    FSectionNo                                 : Integer;
    FSectionName                               : WideString;
    FOutFlowRouteNo                            : Integer;
    FUpstreamCatchmentArea                     : Double;
    FBoardAndPillarArea                        : Double;
    FHighExtractionArea                        : Double;
    FSurfaceRunOffFactor                       : Double;
    FBoardAndPillarInterpolationOption         : Integer;
    FHighExtractionInterpolationOption         : Integer;
    FUndergroundStdDev                         : Double;
    FBoardAndPillarGrowthData                  : TObjectList;
    FHighExtractionGrowthData                  : TObjectList;
    FMonthlyUndergroundWaterRechargePortion    : TMonthlyDoubleArray;
    FMonthlyBoardAndPillarRechargeFactor       : TMonthlyDoubleArray;
    FMonthlyHighExtractionRechargeFactor       : TMonthlyDoubleArray;
    FQvsSLD                                    : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ModuleID: Integer; safecall;
    procedure Set_ModuleID(Value: Integer); safecall;
    function Get_SectionNo: Integer; safecall;
    procedure Set_SectionNo(Value: Integer); safecall;
    function Get_SectionName: WideString; safecall;
    procedure Set_SectionName(const Value: WideString); safecall;
    function Get_OutFlowRouteNo: Integer; safecall;
    procedure Set_OutFlowRouteNo(Value: Integer); safecall;
    function Get_UpstreamCatchmentArea: Double; safecall;
    procedure Set_UpstreamCatchmentArea(Value: Double); safecall;
    function Get_BoardAndPillarArea: Double; safecall;
    procedure Set_BoardAndPillarArea(Value: Double); safecall;
    function Get_HighExtractionArea: Double; safecall;
    procedure Set_HighExtractionArea(Value: Double); safecall;
    function Get_SurfaceRunOffFactor: Double; safecall;
    procedure Set_SurfaceRunOffFactor(Value: Double); safecall;
    function Get_BoardAndPillarInterpolationOption: Integer; safecall;
    procedure Set_BoardAndPillarInterpolationOption(Value: Integer); safecall;
    function Get_HighExtractionInterpolationOption: Integer; safecall;
    procedure Set_HighExtractionInterpolationOption(Value: Integer); safecall;
    function Get_UndergroundStdDev: Double; safecall;
    procedure Set_UndergroundStdDev(Value: Double); safecall;
    function Populate (AModuleID                          : Integer;
                       ASectionNo                         : Integer;
                       const ASectionName                 : WideString;
                       AOutFlowRouteNo                    : Integer;
                       AUpstreamCatchmentArea             : Double;
                       ABoardAndPillarArea                : Double;
                       AHighExtractionArea                : Double;
                       ASurfaceRunOffFactor               : Double;
                       ABoardAndPillarInterpolationOption : Integer;
                       AHighExtractionInterpolationOption : Integer;
                       AUndergroundStdDev                 : Double): WordBool; safecall;
    function AddBoardAndPillarGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddHighExtractionGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_NoOfBoardAndPillarGrowthPoints: Integer; safecall;
    function Get_NoOfHighExtractionGrowthPoints: Integer; safecall;
    function Get_BoardAndPillarGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_BoardAndPillarGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_BoardAndPillarGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_BoardAndPillarGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_HighExtractionGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_HighExtractionGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_HighExtractionGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_HighExtractionGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_UndergroundWaterRechargePortionByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_UndergroundWaterRechargePortionByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_BoardAndPillarRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_BoardAndPillarRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_HighExtractionRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_HighExtractionRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function Get_QvsSLDNoOfPoints: Integer; safecall;
    function Get_QvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_QvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    function UpdateBoardPillarData (ARootNode : IXMLNode): Boolean;
    function UpdateHighExtractionData (ARootNode : IXMLNode): Boolean;
    function UpdateMonthlyData (ARootNode : IXMLNode): Boolean;
    function UpdateQSLDData (ARootNode : IXMLNode) : Boolean;
    procedure ClearBoardAndPillarGrowthData;
    procedure ClearHighExtractionGrowthData;
    procedure ClearQvsSLDData;

    property ModuleID: Integer read Get_ModuleID write Set_ModuleID;
    property SectionNo: Integer read Get_SectionNo write Set_SectionNo;
    property SectionName: WideString read Get_SectionName write Set_SectionName;
    property OutFlowRouteNo: Integer read Get_OutFlowRouteNo write Set_OutFlowRouteNo;
    property UpstreamCatchmentArea: Double read Get_UpstreamCatchmentArea write Set_UpstreamCatchmentArea;
    property BoardAndPillarArea: Double read Get_BoardAndPillarArea write Set_BoardAndPillarArea;
    property HighExtractionArea: Double read Get_HighExtractionArea write Set_HighExtractionArea;
    property SurfaceRunOffFactor: Double read Get_SurfaceRunOffFactor write Set_SurfaceRunOffFactor;
    property BoardAndPillarInterpolationOption: Integer read Get_BoardAndPillarInterpolationOption write Set_BoardAndPillarInterpolationOption;
    property HighExtractionInterpolationOption: Integer read Get_HighExtractionInterpolationOption write Set_HighExtractionInterpolationOption;
    property UndergroundStdDev: Double read Get_UndergroundStdDev write Set_UndergroundStdDev;
    property NoOfBoardAndPillarGrowthPoints: Integer read Get_NoOfBoardAndPillarGrowthPoints;
    property NoOfHighExtractionGrowthPoints: Integer read Get_NoOfHighExtractionGrowthPoints;
    property BoardAndPillarGrowthYearByIndex[AIndex: Integer]: Integer read Get_BoardAndPillarGrowthYearByIndex write Set_BoardAndPillarGrowthYearByIndex;
    property BoardAndPillarGrowthFactorByIndex[AIndex: Integer]: Double read Get_BoardAndPillarGrowthFactorByIndex write Set_BoardAndPillarGrowthFactorByIndex;
    property HighExtractionGrowthYearByIndex[AIndex: Integer]: Integer read Get_HighExtractionGrowthYearByIndex write Set_HighExtractionGrowthYearByIndex;
    property HighExtractionGrowthFactorByIndex[AIndex: Integer]: Double read Get_HighExtractionGrowthFactorByIndex write Set_HighExtractionGrowthFactorByIndex;
    property UndergroundWaterRechargePortionByMonth[AMonthIndex: Integer]: Double read Get_UndergroundWaterRechargePortionByMonth write Set_UndergroundWaterRechargePortionByMonth;
    property BoardAndPillarRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_BoardAndPillarRechargeFactorByMonth write Set_BoardAndPillarRechargeFactorByMonth;
    property HighExtractionRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_HighExtractionRechargeFactorByMonth write Set_HighExtractionRechargeFactorByMonth;
    property QvsSLDNoOfPoints: Integer read Get_QvsSLDNoOfPoints;
    property QvsSLDLoadByIndex[AIndex: Integer]: Double read Get_QvsSLDLoadByIndex write Set_QvsSLDLoadByIndex;
    property QvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_QvsSLDFlowRefByIndex write Set_QvsSLDFlowRefByIndex;
  end;

  TOpencastPit = class(TAbstractObject, IOpencastPit)
  protected
    FModuleID                                    : Integer;
    FSectionNo                                   : Integer;
    FSectionName                                 : String;
    FCoalReserveArea                             : Double;
    FWorkingsArea                                : Double;
    FCommissionYear                              : Integer;
    FCommissionMonth                             : Integer;
    FDecommissionYear                            : Integer;
    FDecommissionMonth                           : Integer;
    FDisturbedArea                               : Double;
    FRehabilitatedArea                           : Double;
    FPitEvaporationArea                          : Double;
    FWorkingsAreaInterpolationOption             : Integer;
    FDisturbedAreaInterpolationOption            : Integer;
    FRehabilitatedAreaInterpolationOption        : Integer;
    FPitEvaporationAreaInterpolationOption       : Integer;
    FDisturbedAreaRunOffFactor                   : Double;
    FDisturbedWorkingsAreaRunOffFactor           : Double;
    FWashOffParameter                            : Double;
    FSulphateBuildUpRate                         : Double;
    FInitialSaltMass                             : Double;
    FInspoilsStorageDecantVolume                 : Double;
    FInspoilsStorageSeepageVolume                : Double;
    FInspoilsStorageInitialVolume                : Double;
    FInspoilsDecantInterpolationOption           : Integer;
    FInspoilsSeepageInterpolationOption          : Integer;
    FMaxSeepageRate                              : Double;
    FSeepageEquationExponent                     : Double;
    FPCDFullSurfaceArea                          : Double;
    FPCDCapacity                                 : Double;
    FPCDInitialVolume                            : Double;
    FInspoilsDamConcentration                    : Double;
    FStdDevWorkingsArea                          : Double;
    FStdDevSeepageDecant                         : Double;
    FWorkingsAreaGrowthData                      : TObjectList;
    FDisturbedAreaGrowthData                     : TObjectList;
    FRehabilitatedAreaGrowthData                 : TObjectList;
    FPitEvaporationGrowthData                    : TObjectList;
    FInspoilsDecantGrowthData                    : TObjectList;
    FInspoilsSeepageGrowthData                   : TObjectList;
    FMonthlyDisturbedAreaRechargeFactor          : TMonthlyDoubleArray;
    FMonthlyDisturbedWorkingsAreaRechargeFactor  : TMonthlyDoubleArray;
    FWorkingsAreaQvsSLD                          : TObjectList;
    FSeepageDecantQvsSLD                         : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ModuleID: Integer; safecall;
    procedure Set_ModuleID(Value: Integer); safecall;
    function Get_SectionNo: Integer; safecall;
    procedure Set_SectionNo(Value: Integer); safecall;
    function Get_SectionName: WideString; safecall;
    procedure Set_SectionName(const Value: WideString); safecall;
    function Get_CoalReserveArea: Double; safecall;
    procedure Set_CoalReserveArea(Value: Double); safecall;
    function Get_WorkingsArea: Double; safecall;
    procedure Set_WorkingsArea(Value: Double); safecall;
    function Get_CommissionYear: Integer; safecall;
    procedure Set_CommissionYear(Value: Integer); safecall;
    function Get_CommissionMonth: Integer; safecall;
    procedure Set_CommissionMonth(Value: Integer); safecall;
    function Get_DecommissionYear: Integer; safecall;
    procedure Set_DecommissionYear(Value: Integer); safecall;
    function Get_DecommissionMonth: Integer; safecall;
    procedure Set_DecommissionMonth(Value: Integer); safecall;
    function Get_DisturbedArea: Double; safecall;
    procedure Set_DisturbedArea(Value: Double); safecall;
    function Get_RehabilitatedArea: Double; safecall;
    procedure Set_RehabilitatedArea(Value: Double); safecall;
    function Get_PitEvaporationArea: Double; safecall;
    procedure Set_PitEvaporationArea(Value: Double); safecall;
    function Get_WorkingsAreaInterpolationOption: Integer; safecall;
    procedure Set_WorkingsAreaInterpolationOption(Value: Integer); safecall;
    function Get_DisturbedAreaInterpolationOption: Integer; safecall;
    procedure Set_DisturbedAreaInterpolationOption(Value: Integer); safecall;
    function Get_RehabilitatedAreaInterpolationOption: Integer; safecall;
    procedure Set_RehabilitatedAreaInterpolationOption(Value: Integer); safecall;
    function Get_PitEvaporationAreaInterpolationOption: Integer; safecall;
    procedure Set_PitEvaporationAreaInterpolationOption(Value: Integer); safecall;
    function Get_DisturbedAreaRunOffFactor: Double; safecall;
    procedure Set_DisturbedAreaRunOffFactor(Value: Double); safecall;
    function Get_DisturbedWorkingsAreaRunOffFactor: Double; safecall;
    procedure Set_DisturbedWorkingsAreaRunOffFactor(Value: Double); safecall;
    function Get_WashOffParameter: Double; safecall;
    procedure Set_WashOffParameter(Value: Double); safecall;
    function Get_SulphateBuildUpRate: Double; safecall;
    procedure Set_SulphateBuildUpRate(Value: Double); safecall;
    function Get_InitialSaltMass: Double; safecall;
    procedure Set_InitialSaltMass(Value: Double); safecall;
    function Get_InspoilsStorageDecantVolume: Double; safecall;
    procedure Set_InspoilsStorageDecantVolume(Value: Double); safecall;
    function Get_InspoilsStorageSeepageVolume: Double; safecall;
    procedure Set_InspoilsStorageSeepageVolume(Value: Double); safecall;
    function Get_InspoilsStorageInitialVolume: Double; safecall;
    procedure Set_InspoilsStorageInitialVolume(Value: Double); safecall;
    function Get_InspoilsDecantInterpolationOption: Integer; safecall;
    procedure Set_InspoilsDecantInterpolationOption(Value: Integer); safecall;
    function Get_InspoilsSeepageInterpolationOption: Integer; safecall;
    procedure Set_InspoilsSeepageInterpolationOption(Value: Integer); safecall;
    function Get_MaxSeepageRate: Double; safecall;
    procedure Set_MaxSeepageRate(Value: Double); safecall;
    function Get_SeepageEquationExponent: Double; safecall;
    procedure Set_SeepageEquationExponent(Value: Double); safecall;
    function Get_PCDFullSurfaceArea: Double; safecall;
    procedure Set_PCDFullSurfaceArea(Value: Double); safecall;
    function Get_PCDCapacity: Double; safecall;
    procedure Set_PCDCapacity(Value: Double); safecall;
    function Get_PCDInitialVolume: Double; safecall;
    procedure Set_PCDInitialVolume(Value: Double); safecall;
    function Get_InspoilsDamConcentration: Double; safecall;
    procedure Set_InspoilsDamConcentration(Value: Double); safecall;
    function Get_StdDevWorkingsArea: Double; safecall;
    procedure Set_StdDevWorkingsArea(Value: Double); safecall;
    function Get_StdDevSeepageDecant: Double; safecall;
    procedure Set_StdDevSeepageDecant(Value: Double); safecall;
    function Populate(AModuleID                             : Integer;
                      ASectionNo                            : Integer;
                      const ASectionName                    : WideString;
                      ACoalReserveArea                      : Double;
                      AWorkingsArea                         : Double;
                      ACommissionYear                       : Integer;
                      ACommissionMonth                      : Integer;
                      ADecommissionYear                     : Integer;
                      ADecommissionMonth                    : Integer;
                      ADisturbedArea                        : Double;
                      ARehabilitatedArea                    : Double;
                      APitEvaporationArea                   : Double;
                      AWorkingsAreaInterpolationOption      : Integer;
                      ADisturbedAreaInterpolationOption     : Integer;
                      ARehabilitatedAreaInterpolationOption : Integer;
                      APitEvaporationAreaInterpolationOption : Integer;
                      ADisturbedAreaRunOffFactor            : Double;
                      ADisturbedWorkingsAreaRunOffFactor    : Double;
                      AWashOffParameter                     : Double;
                      ASulphateBuildUpRate                  : Double;
                      AInitialSaltMass                      : Double;
                      AInspoilsStorageDecantVolume          : Double;
                      AInspoilsStorageSeepageVolume         : Double;
                      AInspoilsStorageInitialVolume         : Double;
                      AInspoilsDecantInterpolationOption    : Integer;
                      AInspoilsSeepageInterpolationOption   : Integer;
                      AMaxSeepageRate                       : Double;
                      ASeepageEquationExponent              : Double;
                      APCDFullSurfaceArea                   : Double;
                      APCDCapacity                          : Double;
                      APCDInitialVolume                     : Double;
                      AInspoilsDamConcentration             : Double;
                      AStdDevWorkingsArea                   : Double;
                      AStdDevSeepageDecant                  : Double): WordBool; safecall;
    function AddWorkingsAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddDisturbedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddRehabilitatedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddPitEvaporationGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddInspoilsDecantGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddInspoilsSeepageGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_WorkingsAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_WorkingsAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_WorkingsAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_WorkingsAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfWorkingsAreaGrowthPoints: Integer; safecall;
    function Get_DisturbedAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_DisturbedAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_DisturbedAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DisturbedAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfDisturbedAreaGrowthPoints: Integer; safecall;
    function Get_RehabilitatedAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_RehabilitatedAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_RehabilitatedAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_RehabilitatedAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfRehabilitatedAreaGrowthPoints: Integer; safecall;
    function Get_PitEvaporationGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_PitEvaporationGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_PitEvaporationGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PitEvaporationGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfPitEvaporationGrowthPoints: Integer; safecall;
    function Get_InspoilsDecantGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_InspoilsDecantGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_InspoilsDecantGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_InspoilsDecantGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfInspoilsDecantGrowthPoints: Integer; safecall;
    function Get_InspoilsSeepageGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_InspoilsSeepageGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_InspoilsSeepageGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_InspoilsSeepageGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfInspoilsSeepageGrowthPoints: Integer; safecall;
    function Get_DisturbedAreaRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_DisturbedAreaRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_DisturbedWorkingsAreaRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_DisturbedWorkingsAreaRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_WorkingsAreaQvsSLDNoOfPoints: Integer; safecall;
    function Get_WorkingsAreaQvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_WorkingsAreaQvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_WorkingsAreaQvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_WorkingsAreaQvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_SeepageDecantQvsSLDNoOfPoints: Integer; safecall;
    function Get_SeepageDecantQvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_SeepageDecantQvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_SeepageDecantQvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_SeepageDecantQvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function AddWorkingsAreaLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function AddSeepageDecantLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    function UpdateWorkingsAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateDisturbedAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateRehabilitatedAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateEvaporationAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateInspoilsDecantData (ARootNode : IXMLNode): Boolean;
    function UpdateInspoilsSeepageData (ARootNode : IXMLNode): Boolean;
    function UpdateRechargeFactorsData (ARootNode : IXMLNode): Boolean;
    function UpdateWorkingsQSLDData (ARootNode : IXMLNode): Boolean;
    function UpdateSeepDecantQSLDData (ARootNode : IXMLNode): Boolean;
    procedure ClearWorkingsAreaGrowthData;
    procedure ClearDisturbedAreaGrowthData;
    procedure ClearRehabilitatedAreaGrowthData;
    procedure ClearPitEvaporationGrowthData;
    procedure ClearInspoilsDecantGrowthData;
    procedure ClearInspoilsSeepageGrowthData;
    procedure ClearWorkingsAreaQvsSLD;
    procedure ClearSeepageDecantQvsSLD;

    property ModuleID: Integer read Get_ModuleID write Set_ModuleID;
    property SectionNo: Integer read Get_SectionNo write Set_SectionNo;
    property SectionName: WideString read Get_SectionName write Set_SectionName;
    property CoalReserveArea: Double read Get_CoalReserveArea write Set_CoalReserveArea;
    property WorkingsArea: Double read Get_WorkingsArea write Set_WorkingsArea;
    property CommissionYear: Integer read Get_CommissionYear write Set_CommissionYear;
    property CommissionMonth: Integer read Get_CommissionMonth write Set_CommissionMonth;
    property DecommissionYear: Integer read Get_DecommissionYear write Set_DecommissionYear;
    property DecommissionMonth: Integer read Get_DecommissionMonth write Set_DecommissionMonth;
    property DisturbedArea: Double read Get_DisturbedArea write Set_DisturbedArea;
    property RehabilitatedArea: Double read Get_RehabilitatedArea write Set_RehabilitatedArea;
    property PitEvaporationArea: Double read Get_PitEvaporationArea write Set_PitEvaporationArea;
    property WorkingsAreaInterpolationOption: Integer read Get_WorkingsAreaInterpolationOption write Set_WorkingsAreaInterpolationOption;
    property DisturbedAreaInterpolationOption: Integer read Get_DisturbedAreaInterpolationOption write Set_DisturbedAreaInterpolationOption;
    property RehabilitatedAreaInterpolationOption: Integer read Get_RehabilitatedAreaInterpolationOption write Set_RehabilitatedAreaInterpolationOption;
    property PitEvaporationAreaInterpolationOption: Integer read Get_PitEvaporationAreaInterpolationOption write Set_PitEvaporationAreaInterpolationOption;
    property DisturbedAreaRunOffFactor: Double read Get_DisturbedAreaRunOffFactor write Set_DisturbedAreaRunOffFactor;
    property DisturbedWorkingsAreaRunOffFactor: Double read Get_DisturbedWorkingsAreaRunOffFactor write Set_DisturbedWorkingsAreaRunOffFactor;
    property WashOffParameter: Double read Get_WashOffParameter write Set_WashOffParameter;
    property SulphateBuildUpRate: Double read Get_SulphateBuildUpRate write Set_SulphateBuildUpRate;
    property InitialSaltMass: Double read Get_InitialSaltMass write Set_InitialSaltMass;
    property InspoilsStorageDecantVolume: Double read Get_InspoilsStorageDecantVolume write Set_InspoilsStorageDecantVolume;
    property InspoilsStorageSeepageVolume: Double read Get_InspoilsStorageSeepageVolume write Set_InspoilsStorageSeepageVolume;
    property InspoilsStorageInitialVolume: Double read Get_InspoilsStorageInitialVolume write Set_InspoilsStorageInitialVolume;
    property InspoilsDecantInterpolationOption: Integer read Get_InspoilsDecantInterpolationOption write Set_InspoilsDecantInterpolationOption;
    property InspoilsSeepageInterpolationOption: Integer read Get_InspoilsSeepageInterpolationOption write Set_InspoilsSeepageInterpolationOption;
    property MaxSeepageRate: Double read Get_MaxSeepageRate write Set_MaxSeepageRate;
    property SeepageEquationExponent: Double read Get_SeepageEquationExponent write Set_SeepageEquationExponent;
    property PCDFullSurfaceArea: Double read Get_PCDFullSurfaceArea write Set_PCDFullSurfaceArea;
    property PCDCapacity: Double read Get_PCDCapacity write Set_PCDCapacity;
    property PCDInitialVolume: Double read Get_PCDInitialVolume write Set_PCDInitialVolume;
    property InspoilsDamConcentration: Double read Get_InspoilsDamConcentration write Set_InspoilsDamConcentration;
    property StdDevWorkingsArea: Double read Get_StdDevWorkingsArea write Set_StdDevWorkingsArea;
    property StdDevSeepageDecant: Double read Get_StdDevSeepageDecant write Set_StdDevSeepageDecant;
    property WorkingsAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_WorkingsAreaGrowthYearByIndex write Set_WorkingsAreaGrowthYearByIndex;
    property WorkingsAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_WorkingsAreaGrowthFactorByIndex write Set_WorkingsAreaGrowthFactorByIndex;
    property NoOfWorkingsAreaGrowthPoints: Integer read Get_NoOfWorkingsAreaGrowthPoints;
    property DisturbedAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_DisturbedAreaGrowthYearByIndex write Set_DisturbedAreaGrowthYearByIndex;
    property DisturbedAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_DisturbedAreaGrowthFactorByIndex write Set_DisturbedAreaGrowthFactorByIndex;
    property NoOfDisturbedAreaGrowthPoints: Integer read Get_NoOfDisturbedAreaGrowthPoints;
    property RehabilitatedAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_RehabilitatedAreaGrowthYearByIndex write Set_RehabilitatedAreaGrowthYearByIndex;
    property RehabilitatedAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_RehabilitatedAreaGrowthFactorByIndex write Set_RehabilitatedAreaGrowthFactorByIndex;
    property NoOfRehabilitatedAreaGrowthPoints: Integer read Get_NoOfRehabilitatedAreaGrowthPoints;
    property PitEvaporationGrowthYearByIndex[AIndex: Integer]: Integer read Get_PitEvaporationGrowthYearByIndex write Set_PitEvaporationGrowthYearByIndex;
    property PitEvaporationGrowthFactorByIndex[AIndex: Integer]: Double read Get_PitEvaporationGrowthFactorByIndex write Set_PitEvaporationGrowthFactorByIndex;
    property NoOfPitEvaporationGrowthPoints: Integer read Get_NoOfPitEvaporationGrowthPoints;
    property InspoilsDecantGrowthYearByIndex[AIndex: Integer]: Integer read Get_InspoilsDecantGrowthYearByIndex write Set_InspoilsDecantGrowthYearByIndex;
    property InspoilsDecantGrowthFactorByIndex[AIndex: Integer]: Double read Get_InspoilsDecantGrowthFactorByIndex write Set_InspoilsDecantGrowthFactorByIndex;
    property NoOfInspoilsDecantGrowthPoints: Integer read Get_NoOfInspoilsDecantGrowthPoints;
    property InspoilsSeepageGrowthYearByIndex[AIndex: Integer]: Integer read Get_InspoilsSeepageGrowthYearByIndex write Set_InspoilsSeepageGrowthYearByIndex;
    property InspoilsSeepageGrowthFactorByIndex[AIndex: Integer]: Double read Get_InspoilsSeepageGrowthFactorByIndex write Set_InspoilsSeepageGrowthFactorByIndex;
    property NoOfInspoilsSeepageGrowthPoints: Integer read Get_NoOfInspoilsSeepageGrowthPoints;
    property DisturbedAreaRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_DisturbedAreaRechargeFactorByMonth write Set_DisturbedAreaRechargeFactorByMonth;
    property DisturbedWorkingsAreaRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_DisturbedWorkingsAreaRechargeFactorByMonth write Set_DisturbedWorkingsAreaRechargeFactorByMonth;
    property WorkingsAreaQvsSLDNoOfPoints: Integer read Get_WorkingsAreaQvsSLDNoOfPoints;
    property WorkingsAreaQvsSLDLoadByIndex[AIndex: Integer]: Double read Get_WorkingsAreaQvsSLDLoadByIndex write Set_WorkingsAreaQvsSLDLoadByIndex;
    property WorkingsAreaQvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_WorkingsAreaQvsSLDFlowRefByIndex write Set_WorkingsAreaQvsSLDFlowRefByIndex;
    property SeepageDecantQvsSLDNoOfPoints: Integer read Get_SeepageDecantQvsSLDNoOfPoints;
    property SeepageDecantQvsSLDLoadByIndex[AIndex: Integer]: Double read Get_SeepageDecantQvsSLDLoadByIndex write Set_SeepageDecantQvsSLDLoadByIndex;
    property SeepageDecantQvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_SeepageDecantQvsSLDFlowRefByIndex write Set_SeepageDecantQvsSLDFlowRefByIndex;
  end;
  
  TMineModule = class(TNetworkModule, IMineModule)
  protected
    FMineName                         : String;
    FVersionNo                        : Integer;
    FRunOffModuleNo                   : Integer;
    FOutflowRouteNoToRiver            : Integer;
    FOutflowRouteNoToPCD              : Integer;
    FRainfallFileName                 : String;
    FMAP                              : Double;
    FPlantArea                        : Double;
    FPlantAreaRunOffFactor            : Double;
    FSaltBuildUpRate                  : Double;
    FSaltWashOffFactor                : Double;
    FInitialSaltStore                 : Double;
    FPlantAreaGrowthInterpolationType : Integer;
    FPlantAreaGrowthData              : TObjectList;
    FOpencastPits                     : TObjectList;
    FUndergroundSections              : TObjectList;
    FSlurryDumps                      : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_MineName: WideString; safecall;
    procedure Set_MineName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_RunOffModuleNo: Integer; safecall;
    procedure Set_RunOffModuleNo(Value: Integer); safecall;
    function Get_OutflowRouteNoToRiver: Integer; safecall;
    procedure Set_OutflowRouteNoToRiver(Value: Integer); safecall;
    function Get_OutflowRouteNoToPCD: Integer; safecall;
    procedure Set_OutflowRouteNoToPCD(Value: Integer); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_PlantArea: Double; safecall;
    procedure Set_PlantArea(Value: Double); safecall;
    function Get_PlantAreaRunOffFactor: Double; safecall;
    procedure Set_PlantAreaRunOffFactor(Value: Double); safecall;
    function Get_SaltBuildUpRate: Double; safecall;
    procedure Set_SaltBuildUpRate(Value: Double); safecall;
    function Get_SaltWashOffFactor: Double; safecall;
    procedure Set_SaltWashOffFactor(Value: Double); safecall;
    function Get_InitialSaltStore: Double; safecall;
    procedure Set_InitialSaltStore(Value: Double); safecall;
    function Get_PlantAreaGrowthInterpolationType: Integer; safecall;
    procedure Set_PlantAreaGrowthInterpolationType(Value: Integer); safecall;
    function Get_PlantAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_PlantAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_PlantAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PlantAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function AddPlantAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Populate (ANetworkID                        : Integer;
                       AModuleID                         : Integer;
                       const AModuleType                 : WideString;
                       AModuleNumber                     : Integer;
                       ANetworkSequence                  : Integer;
                       const AActive                     : WideString;
                       const AMineName                   : WideString;
                       AVersionNo                        : Integer;
                       ARunOffModuleNo                   : Integer;
                       AOutflowRouteNoToRiver            : Integer;
                       AOutflowRouteNoToPCD              : Integer;
                       const ARainfallFileName           : WideString;
                       AMAP                              : Double;
                       APlantArea                        : Double;
                       APlantAreaRunOffFactor            : Double;
                       ASaltBuildUpRate                  : Double;
                       ASaltWashOffFactor                : Double;
                       AInitialSaltStore                 : Double;
                       APlantAreaGrowthInterpolationType : Integer;
                       ALongitude                        : Double;
                       ALatitude                         : Double): WordBool; safecall;
    function AddOpencastPit: TOpencastPit;
    function CreateNewOpencastPit : IOpencastPit;
    function RemoveOpencastPit (ASectionNo : Integer) : WordBool;
    function Get_NoOfOpencastPits: Integer; safecall;
    function FindOpencastPitBySectionNo(ASectionNo: Integer): TOpencastPit;
    function Get_OpencastPitBySectionNo(ASectionNo: Integer): IOpencastPit; safecall;
    function Get_OpencastPitByIndex(AIndex: Integer): IOpencastPit; safecall;
    function Get_NoOfPlantAreaGrowthPoints: Integer; safecall;
    function CreateNewUndergroundSection : IUndergroundSection;
    function RemoveUndergroundSection (ASectionNo : Integer) : WordBool;
    function AddUndergroundSection: TUndergroundSection;
    function Get_NoOfUndergroundSections: Integer; safecall;
    function FindUndergroundSectionBySectionNo(ASectionNo: Integer): TUndergroundSection;
    function Get_UndergroundSectionBySectionNo(ASectionNo: Integer): IUndergroundSection; safecall;
    function Get_UndergroundSectionByIndex(AIndex: Integer): IUndergroundSection; safecall;
    function AddSlurryDump: TSlurryDump;
    function CreateNewSlurryDump : ISlurryDump;
    function RemoveSlurryDump (ASectionNo : Integer) : WordBool;
    function Get_NoOfSlurryDumps: Integer; safecall;
    function FindSlurryDumpSectionBySectionNo(ASectionNo: Integer): TSlurryDump;
    function Get_SlurryDumpBySectionNo(ASectionNo: Integer): ISlurryDump; safecall;
    function Get_SlurryDumpByIndex(AIndex: Integer): ISlurryDump; safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): WordBool;
    function UpdatePlantAreaData (ARootNode : IXMLNode): WordBool;
    function UpdateSections (ARootNode : IXMLNode) : WordBool;
    procedure ClearPlantAreaGrowthData;

    property MineName: WideString read Get_MineName write Set_MineName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property RunOffModuleNo: Integer read Get_RunOffModuleNo write Set_RunOffModuleNo;
    property OutflowRouteNoToRiver: Integer read Get_OutflowRouteNoToRiver write Set_OutflowRouteNoToRiver;
    property OutflowRouteNoToPCD: Integer read Get_OutflowRouteNoToPCD write Set_OutflowRouteNoToPCD;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property MAP: Double read Get_MAP write Set_MAP;
    property PlantArea: Double read Get_PlantArea write Set_PlantArea;
    property PlantAreaRunOffFactor: Double read Get_PlantAreaRunOffFactor write Set_PlantAreaRunOffFactor;
    property SaltBuildUpRate: Double read Get_SaltBuildUpRate write Set_SaltBuildUpRate;
    property SaltWashOffFactor: Double read Get_SaltWashOffFactor write Set_SaltWashOffFactor;
    property InitialSaltStore: Double read Get_InitialSaltStore write Set_InitialSaltStore;
    property PlantAreaGrowthInterpolationType: Integer read Get_PlantAreaGrowthInterpolationType write Set_PlantAreaGrowthInterpolationType;
    property NoOfOpencastPits: Integer read Get_NoOfOpencastPits;
    property OpencastPitBySectionNo[ASectionNo: Integer]: IOpencastPit read Get_OpencastPitBySectionNo;
    property OpencastPitByIndex[AIndex: Integer]: IOpencastPit read Get_OpencastPitByIndex;
    property NoOfPlantAreaGrowthPoints: Integer read Get_NoOfPlantAreaGrowthPoints;
    property NoOfUndergroundSections: Integer read Get_NoOfUndergroundSections;
    property UndergroundSectionBySectionNo[ASectionNo: Integer]: IUndergroundSection read Get_UndergroundSectionBySectionNo;
    property UndergroundSectionByIndex[AIndex: Integer]: IUndergroundSection read Get_UndergroundSectionByIndex;
    property NoOfSlurryDumps: Integer read Get_NoOfSlurryDumps;
    property SlurryDumpBySectionNo[ASectionNo: Integer]: ISlurryDump read Get_SlurryDumpBySectionNo;
    property SlurryDumpByIndex[AIndex: Integer]: ISlurryDump read Get_SlurryDumpByIndex;
  end;

  TMineModuleAgent = class(TModuleAgent, IMineModuleAgent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_MineModuleCount: Integer; safecall;
    function FindMineModuleByID(AModuleID: Integer): TMineModule;
    function FindMineModuleByNumber(AModuleNumber: Integer): TMineModule;
    function Get_MineModuleByID(AModuleID: Integer): IMineModule; safecall;
    function Get_MineModuleByNumber(AModuleNumber: Integer): IMineModule; safecall;
    function Get_MineModuleByIndex(AIndex: Integer): IMineModule; safecall;
    function AddMineModule: TMineModule;
    function CreateNewMineModule (ANetworkID: Integer): IMineModule; safecall;
    function RemoveMineModule(AModuleNumber: Integer): WordBool; safecall;
    function LoadMineModules (ANetworkID : Integer) : Boolean;
    property MineModuleCount: Integer read Get_MineModuleCount;
    property MineModuleByID[AModuleID: Integer]: IMineModule read Get_MineModuleByID;
    property MineModuleByIndex[AIndex: Integer]: IMineModule read Get_MineModuleByIndex;
    property MineModuleByNumber[AModuleNumber: Integer]: IMineModule read Get_MineModuleByNumber;
  end;


implementation

uses

  SysUtils,
  Windows,
  VCL.Forms,
  Math,

  UModuleDBManager,
  UMineDBManager,
  UErrorHandlingOperations;

{ TSlurryDump *****************************************************************}

function TSlurryDump._AddRef: Integer;
const OPNAME = 'TSlurryDump._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump._Release: Integer;
const OPNAME = 'TSlurryDump._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.CreateMemberObjects;
const OPNAME = 'TSlurryDump.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FAreaGrowthData := TObjectList.Create;
    FQvsSLD         := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.DestroyMemberObjects;
const OPNAME = 'TSlurryDump.DestroyMemberObjects';
begin
  try
    FAreaGrowthData.Free;
    FQvsSLD.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_SectionNo: Integer;
const OPNAME = 'TSlurryDump.Get_SectionNo';
begin
  Result := 0;
  try
    Result := FSectionNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_SectionNo(Value: Integer);
const OPNAME = 'TSlurryDump.Set_SectionNo';
begin
  try
    FSectionNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_ModuleID: Integer;
const OPNAME = 'TSlurryDump.Get_ModuleID';
begin
  Result := 0;
  try
    Result := FModuleID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_ModuleID(Value: Integer);
const OPNAME = 'TSlurryDump.Set_ModuleID';
begin
  try
    FModuleID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_SectionName: WideString;
const OPNAME = 'TSlurryDump.Get_SectionName';
begin
  Result := '';
  try
    Result := FSectionName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_SectionName(const Value: WideString);
const OPNAME = 'TSlurryDump.Set_SectionName';
begin
  try
    FSectionName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_Area: Double;
const OPNAME = 'TSlurryDump.Get_Area';
begin
  Result := 0.0;
  try
    Result := FArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_Area(Value: Double);
const OPNAME = 'TSlurryDump.Set_Area';
begin
  try
    FArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_RunOffFactor: Double;
const OPNAME = 'TSlurryDump.Get_RunOffFactor';
begin
  Result := 0.0;
  try
    Result := FRunOffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_RunOffFactor(Value: Double);
const OPNAME = 'TSlurryDump.Set_RunOffFactor';
begin
  try
    FRunOffFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_SeepProportion: Double;
const OPNAME = 'TSlurryDump.Get_SeepProportion';
begin
  Result := 0.0;
  try
    Result := FSeepProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_SeepProportion(Value: Double);
const OPNAME = 'TSlurryDump.Set_SeepProportion';
begin
  try
    FSeepProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_PCDFullSupplyVolume: Double;
const OPNAME = 'TSlurryDump.Get_PCDFullSupplyVolume';
begin
  Result := 0.0;
  try
    Result := FPCDFullSupplyVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_PCDFullSupplyVolume(Value: Double);
const OPNAME = 'TSlurryDump.Set_PCDFullSupplyVolume';
begin
  try
    FPCDFullSupplyVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_PCDFullSupplyArea: Double;
const OPNAME = 'TSlurryDump.Get_PCDFullSupplyArea';
begin
  Result := 0.0;
  try
    Result := FPCDFullSupplyArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_PCDFullSupplyArea(Value: Double);
const OPNAME = 'TSlurryDump.Set_PCDFullSupplyArea';
begin
  try
    FPCDFullSupplyArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_PCDInitialVolume: Double;
const OPNAME = 'TSlurryDump.Get_PCDInitialVolume';
begin
  Result := 0.0;
  try
    Result := FPCDInitialVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_PCDInitialVolume(Value: Double);
const OPNAME = 'TSlurryDump.Set_PCDInitialVolume';
begin
  try
    FPCDInitialVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_InterpolationOption: Integer;
const OPNAME = 'TSlurryDump.Get_InterpolationOption';
begin
  Result := 0;
  try
    Result := FInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_InterpolationOption(Value: Integer);
const OPNAME = 'TSlurryDump.Set_InterpolationOption';
begin
  try
    FInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_StdDev: Double;
const OPNAME = 'TSlurryDump.Get_StdDev';
begin
  Result := 0.0;
  try
    Result := FStdDev;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_StdDev(Value: Double);
const OPNAME = 'TSlurryDump.Set_StdDev';
begin
  try
    FStdDev := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Populate (AModuleID             : Integer;
                               ASectionNo            : Integer;
                               const ASectionName    : WideString;
                               AArea                 : Double;
                               ARunOffFactor         : Double;
                               ASeepProportion       : Double;
                               APCDFullSupplyVolume  : Double;
                               APCDFullSupplyArea    : Double;
                               APCDInitialVolume     : Double;
                               AInterpolatrionOption : Integer;
                               AStdDev               : Double): WordBool;
const OPNAME = 'TSlurryDump.Populate';
begin
  Result := FALSE;
  try
    FModuleID            := AModuleID;
    FSectionNo           := ASectionNo;
    FSectionName         := ASectionName;
    FArea                := AArea;
    FRunOffFactor        := ARunOffFactor;
    FSeepProportion      := ASeepProportion;
    FPCDFullSupplyVolume := APCDFullSupplyVolume;
    FPCDFullSupplyArea   := APCDFullSupplyArea;
    FPCDInitialVolume    := APCDInitialVolume;
    FInterpolationOption := AInterpolatrionOption;
    FStdDev              := AStdDev;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.AddAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TSlurryDump.AddAreaGrowthData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AGrowthFactor;
    FAreaGrowthData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_NoOfAreaGrowthPoints: Integer;
const OPNAME = 'TSlurryDump.Get_NoOfAreaGrowthPoints';
begin
  Result := 0;
  try
    Result := FAreaGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_AreaGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TSlurryDump.Get_AreaGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FAreaGrowthData.Count)) then
      Result := TYearValuePair(FAreaGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_AreaGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TSlurryDump.Set_AreaGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FAreaGrowthData.Count)) then
      TYearValuePair(FAreaGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_AreaGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TSlurryDump.Get_AreaGrowthFactorByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FAreaGrowthData.Count)) then
      Result := TYearValuePair(FAreaGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_AreaGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TSlurryDump.Set_AreaGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FAreaGrowthData.Count)) then
      TYearValuePair(FAreaGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_RechargeFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TSlurryDump.Get_RechargeFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyRechargeFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_RechargeFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TSlurryDump.Set_RechargeFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyRechargeFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool;
const OPNAME = 'TSlurryDump.AddLoadFlowRefPair';
var
  LPair : TLoadFlowRefPair;
begin
  Result := FALSE;
  try
    LPair := TLoadFlowRefPair.Create;
    LPair.Load := ALoad;
    LPair.FlowRef := AFlowRef;
    FQvsSLD.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_QvsSLDFlowRefByIndex(AIndex: Integer): Double;
const OPNAME = 'TSlurryDump.Get_QvsSLDFlowRefByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FQvsSLD.Items[AIndex]).FlowRef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_QvsSLDFlowRefByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TSlurryDump.Set_QvsSLDFlowRefByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      TLoadFlowRefPair(FQvsSLD.Items[AIndex]).FlowRef := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_QvsSLDLoadByIndex(AIndex: Integer): Double;
const OPNAME = 'TSlurryDump.Get_QvsSLDLoadByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FQvsSLD.Items[AIndex]).Load;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_QvsSLDLoadByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TSlurryDump.Set_QvsSLDLoadByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      TLoadFlowRefPair(FQvsSLD.Items[AIndex]).Load := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_QvsSLDNoOfPoints: Integer;
const OPNAME = 'TSlurryDump.Get_QvsSLDNoOfPoints';
begin
  Result := 0;
  try
    Result := FQvsSLD.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TSlurryDump.UpdatePropertiesData';
var
  LDataNode                   : IXMLNode;
  LSectionName                : String;
  LSlurryDumpArea             : Double;
  LSlurryDumpRunOffFactor     : Double;
  LSlurrySeepProportion       : Double;
  LSlurryPCDFullSupplyVolume  : Double;
  LSlurryPCDFullSupplyArea    : Double;
  LSlurryPCDInitialVolume     : Double;
begin
  Result := FALSE;
  try
    LDataNode := ARootNode.ChildNodes['MineSlurryDumpProperties'];
    LSectionName                := LDataNode.ChildNodes['SectionName'].Text;
    LSlurryDumpArea             := StrToFloat(LDataNode.ChildNodes['SlurryDumpArea'].Text);
    LSlurryDumpRunOffFactor     := StrToFloat(LDataNode.ChildNodes['SlurryDumpRunOffFactor'].Text);
    LSlurrySeepProportion       := StrToFloat(LDataNode.ChildNodes['SlurrySeepProportion'].Text);
    LSlurryPCDFullSupplyVolume  := StrToFloat(LDataNode.ChildNodes['SlurryPCDFullSupplyVolume'].Text);
    LSlurryPCDFullSupplyArea    := StrToFloat(LDataNode.ChildNodes['SlurryPCDFullSupplyArea'].Text);
    LSlurryPCDInitialVolume     := StrToFloat(LDataNode.ChildNodes['SlurryPCDInitialVolume'].Text);

    if (GMineDBManager.UpdateSlurryDumpPropertiesDataInDB
                        (FModuleID, FSectionNo, LSectionName, LSlurryDumpArea, LSlurryDumpRunOffFactor,
                         LSlurrySeepProportion, LSlurryPCDFullSupplyVolume, LSlurryPCDFullSupplyArea,
                         LSlurryPCDInitialVolume)) then
    begin
      FSectionName         := LSectionName;
      FArea                := LSlurryDumpArea;
      FRunOffFactor        := LSlurryDumpRunOffFactor;
      FSeepProportion      := LSlurrySeepProportion;
      FPCDFullSupplyVolume := LSlurryPCDFullSupplyVolume;
      FPCDFullSupplyArea   := LSlurryPCDFullSupplyArea;
      FPCDInitialVolume    := LSlurryPCDInitialVolume;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSlurryDump.UpdateGrowthData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TSlurryDump.UpdateGrowthData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LInterpolationType : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineSlurryDumpArea'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['SlurryDumpInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateSlurryDumpGrowthDataInDB(FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);

      if (Result) then
      begin
        FInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSlurryDump.UpdateMonthlyData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TSlurryDump.UpdateMonthlyData';
var
  LSectionNode : IXMLNode;
  LListNode    : IXMLNode;
  LDataNode    : IXMLNode;
  LIndex       : Integer;
  LMonth       : Integer;
  LFactor      : Double;
  LMonthList   : TStringList;
  LFactorList  : TStringList;
begin
  Result := FALSE;
  try
    LSectionNode := ARootNode.ChildNodes['MineSlurryDumpRechargeFactors'];

    LMonthList  := TStringList.Create;
    LFactorList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode  := LListNode.ChildNodes.Get(LIndex-1);
        LMonthList.Add(LDataNode.ChildNodes['Month'].Text);
        LFactorList.Add(LDataNode.ChildNodes['SlurryRechargeFactor'].Text);
      end;

      Result := GMineDBManager.UpdateSlurryDumpMonthlyDataInDB
                                 (FModuleID, FSectionNo, LMonthList, LFactorList);
      if (Result) then
      begin
        for LIndex := 0 to LMonthList.Count - 1 do
        begin
          LMonth  := StrToInt(LMonthList.Strings[LIndex]);
          LFactor := StrToFloat(LFactorList.Strings[LIndex]);
          FMonthlyRechargeFactor[LMonth] := LFactor;
        end;
      end;
    finally
      LMonthList.Free;
      LFactorList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSlurryDump.UpdateQSLDData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TSlurryDump.UpdateQSLDData';
var
  LSectionNode : IXMLNode;
  LListNode    : IXMLNode;
  LDataNode    : IXMLNode;
  LIndex       : Integer;
  LStdDev      : Double;
  LFlowRefList : TStringList;
  LLoadList    : TStringList;
begin
  Result := TRUE;
  try
    LSectionNode := ARootNode.ChildNodes['MineSlurryDumpQSLD'];
    LStdDev      := StrToFloat(LSectionNode.ChildNodes['StdDevSlurryDump'].Text);

    LFlowRefList := TStringList.Create;
    LLoadList    := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LFlowRefList.Add(LDataNode.ChildNodes['Flow'].Text);
        LLoadList.Add(LDataNode.ChildNodes['Load'].Text);
      end;

      Result := GMineDBManager.UpdateSlurryDumpQSLDDataInDB(FModuleID, FSectionNo, LStdDev, LFlowRefList, LLoadList);

      if (Result) then
      begin
        FStdDev := LStdDev;
        Result := GMineDBManager.LoadQvsSLDDataFromDB(Self);
      end;
    finally
      LFlowRefList.Free;
      LLoadList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSlurryDump.ClearAreaGrowthData;
const OPNAME = 'TSlurryDump.ClearAreaGrowthData';
begin
  try
    while (FAreaGrowthData.Count > 0) do
      FAreaGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSlurryDump.ClearQvsSLDData;
const OPNAME = 'TSlurryDump.ClearQvsSLDData';
begin
  try
    while (FQvsSLD.Count > 0) do
      FQvsSLD.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TUndergroundSection *********************************************************}

function TUndergroundSection._AddRef: Integer;
const OPNAME = 'TUndergroundSection._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection._Release: Integer;
const OPNAME = 'TUndergroundSection._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.CreateMemberObjects;
const OPNAME = 'TUndergroundSection.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FBoardAndPillarGrowthData     := TObjectList.Create;
    FHighExtractionGrowthData     := TObjectList.Create;
    FQvsSLD                       := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.DestroyMemberObjects;
const OPNAME = 'TUndergroundSection.DestroyMemberObjects';
begin
  try
    FBoardAndPillarGrowthData.Free;
    FHighExtractionGrowthData.Free;
    FQvsSLD.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_ModuleID: Integer;
const OPNAME = 'TUndergroundSection.Get_ModuleID';
begin
  Result := 0;
  try
    Result := FModuleID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_ModuleID(Value: Integer);
const OPNAME = 'TUndergroundSection.Set_ModuleID';
begin
  try
    FModuleID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_SectionNo: Integer;
const OPNAME = 'TUndergroundSection.Get_SectionNo';
begin
  Result := 0;
  try
    Result := FSectionNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_SectionNo(Value: Integer);
const OPNAME = 'TUndergroundSection.Set_SectionNo';
begin
  try
    FSectionNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_SectionName: WideString;
const OPNAME = 'TUndergroundSection.Get_SectionName';
begin
  Result := '';
  try
    Result := FSectionName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_SectionName(const Value: WideString);
const OPNAME = 'TUndergroundSection.Set_SectionName';
begin
  try
    FSectionName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_OutFlowRouteNo: Integer;
const OPNAME = 'TUndergroundSection.Get_OutFlowRouteNo';
begin
  Result := 0;
  try
    Result := FOutFlowRouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_OutFlowRouteNo(Value: Integer);
const OPNAME = 'TUndergroundSection.Set_OutFlowRouteNo';
begin
  try
    FOutFlowRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_UpstreamCatchmentArea: Double;
const OPNAME = 'TUndergroundSection.Get_UpstreamCatchmentArea';
begin
  Result := 0.0;
  try
    Result := FUpstreamCatchmentArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_UpstreamCatchmentArea(Value: Double);
const OPNAME = 'TUndergroundSection.Set_UpstreamCatchmentArea';
begin
  try
    FUpstreamCatchmentArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_BoardAndPillarArea: Double;
const OPNAME = 'TUndergroundSection.Get_BoardAndPillarArea';
begin
  Result := 0.0;
  try
    Result := FBoardAndPillarArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_BoardAndPillarArea(Value: Double);
const OPNAME = 'TUndergroundSection.Set_BoardAndPillarArea';
begin
  try
    FBoardAndPillarArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_HighExtractionArea: Double;
const OPNAME = 'TUndergroundSection.Get_HighExtractionArea';
begin
  Result := 0.0;
  try
    Result := FHighExtractionArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_HighExtractionArea(Value: Double);
const OPNAME = 'TUndergroundSection.Set_HighExtractionArea';
begin
  try
    FHighExtractionArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_SurfaceRunOffFactor: Double;
const OPNAME = 'TUndergroundSection.Get_SurfaceRunOffFactor';
begin
  Result := 0.0;
  try
    Result := FSurfaceRunOffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_SurfaceRunOffFactor(Value: Double);
const OPNAME = 'TUndergroundSection.Set_SurfaceRunOffFactor';
begin
  try
    FSurfaceRunOffFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_BoardAndPillarInterpolationOption: Integer;
const OPNAME = 'TUndergroundSection.Get_BoardAndPillarInterpolationOption';
begin
  Result := 0;
  try
    Result := FBoardAndPillarInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_BoardAndPillarInterpolationOption(Value: Integer);
const OPNAME = 'TUndergroundSection.Set_BoardAndPillarInterpolationOption';
begin
  try
    FBoardAndPillarInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_HighExtractionInterpolationOption: Integer;
const OPNAME = 'TUndergroundSection.Get_HighExtractionInterpolationOption';
begin
  Result := 0;
  try
    Result := FHighExtractionInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_HighExtractionInterpolationOption(Value: Integer);
const OPNAME = 'TUndergroundSection.Set_HighExtractionInterpolationOption';
begin
  try
    FHighExtractionInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_UndergroundStdDev: Double;
const OPNAME = 'TUndergroundSection.Get_UndergroundStdDev';
begin
  Result := 0.0;
  try
    Result := FUndergroundStdDev;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_UndergroundStdDev(Value: Double);
const OPNAME = 'TUndergroundSection.Set_UndergroundStdDev';
begin
  try
    FUndergroundStdDev := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Populate (AModuleID                          : Integer;
                                       ASectionNo                         : Integer;
                                       const ASectionName                 : WideString;
                                       AOutFlowRouteNo                    : Integer;
                                       AUpstreamCatchmentArea             : Double;
                                       ABoardAndPillarArea                : Double;
                                       AHighExtractionArea                : Double;
                                       ASurfaceRunOffFactor               : Double;
                                       ABoardAndPillarInterpolationOption : Integer;
                                       AHighExtractionInterpolationOption : Integer;
                                       AUndergroundStdDev                 : Double): WordBool;
const OPNAME = 'TUndergroundSection.Populate';
begin
  Result := FALSE;
  try
    FModuleID                          := AModuleID;
    FSectionNo                         := ASectionNo;
    FSectionName                       := ASectionName;
    FOutFlowRouteNo                    := AOutFlowRouteNo;
    FUpstreamCatchmentArea             := AUpstreamCatchmentArea;
    FBoardAndPillarArea                := ABoardAndPillarArea;
    FHighExtractionArea                := AHighExtractionArea;
    FSurfaceRunOffFactor               := ASurfaceRunOffFactor;
    FBoardAndPillarInterpolationOption := ABoardAndPillarInterpolationOption;
    FHighExtractionInterpolationOption := AHighExtractionInterpolationOption;
    FUndergroundStdDev                 := AUndergroundStdDev;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.AddBoardAndPillarGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TUndergroundSection.AddBoardAndPillarGrowthData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AGrowthFactor;
    FBoardAndPillarGrowthData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.AddHighExtractionGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TUndergroundSection.AddHighExtractionGrowthData';
var
  LPair : TYearValuePair;
begin
  Result := FALSE;
  try
    LPair := TYearValuePair.Create;
    LPair.Year := AYear;
    LPair.Value := AGrowthFactor;
    FHighExtractionGrowthData.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_NoOfBoardAndPillarGrowthPoints: Integer;
const OPNAME = 'TUndergroundSection.Get_NoOfBoardAndPillarGrowthPoints';
begin
  Result := 0;
  try
    Result := FBoardAndPillarGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_NoOfHighExtractionGrowthPoints: Integer;
const OPNAME = 'TUndergroundSection.Get_NoOfHighExtractionGrowthPoints';
begin
  Result := 0;
  try
    Result := FHighExtractionGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_BoardAndPillarGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TUndergroundSection.Get_BoardAndPillarGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FBoardAndPillarGrowthData.Count)) then
      Result := TYearValuePair(FBoardAndPillarGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_BoardAndPillarGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TUndergroundSection.Set_BoardAndPillarGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FBoardAndPillarGrowthData.Count)) then
      TYearValuePair(FBoardAndPillarGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_BoardAndPillarGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_BoardAndPillarGrowthFactorByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FBoardAndPillarGrowthData.Count)) then
      Result := TYearValuePair(FBoardAndPillarGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_BoardAndPillarGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_BoardAndPillarGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FBoardAndPillarGrowthData.Count)) then
      TYearValuePair(FBoardAndPillarGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_HighExtractionGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TUndergroundSection.Get_HighExtractionGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FHighExtractionGrowthData.Count)) then
      Result := TYearValuePair(FHighExtractionGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_HighExtractionGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TUndergroundSection.Set_HighExtractionGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FHighExtractionGrowthData.Count)) then
      TYearValuePair(FHighExtractionGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_HighExtractionGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_HighExtractionGrowthFactorByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FHighExtractionGrowthData.Count)) then
      Result := TYearValuePair(FHighExtractionGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_HighExtractionGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_HighExtractionGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FHighExtractionGrowthData.Count)) then
      TYearValuePair(FHighExtractionGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_UndergroundWaterRechargePortionByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_UndergroundWaterRechargePortionByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyUndergroundWaterRechargePortion[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_UndergroundWaterRechargePortionByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_UndergroundWaterRechargePortionByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyUndergroundWaterRechargePortion[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_BoardAndPillarRechargeFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_BoardAndPillarRechargeFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyBoardAndPillarRechargeFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_BoardAndPillarRechargeFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_BoardAndPillarRechargeFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyBoardAndPillarRechargeFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_HighExtractionRechargeFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_HighExtractionRechargeFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyHighExtractionRechargeFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_HighExtractionRechargeFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_HighExtractionRechargeFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyHighExtractionRechargeFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool;
const OPNAME = 'TUndergroundSection.AddLoadFlowRefPair';
var
  LPair : TLoadFlowRefPair;
begin
  Result := FALSE;
  try
    LPair := TLoadFlowRefPair.Create;
    LPair.Load := ALoad;
    LPair.FlowRef := AFlowRef;
    FQvsSLD.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_QvsSLDNoOfPoints: Integer;
const OPNAME = 'TUndergroundSection.Get_QvsSLDNoOfPoints';
begin
  Result := 0;
  try
    Result := FQvsSLD.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_QvsSLDLoadByIndex(AIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_QvsSLDLoadByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FQvsSLD.Items[AIndex]).Load;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_QvsSLDLoadByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_QvsSLDLoadByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      TLoadFlowRefPair(FQvsSLD.Items[AIndex]).Load := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.Get_QvsSLDFlowRefByIndex(AIndex: Integer): Double;
const OPNAME = 'TUndergroundSection.Get_QvsSLDFlowRefByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FQvsSLD.Items[AIndex]).FlowRef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUndergroundSection.Set_QvsSLDFlowRefByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TUndergroundSection.Set_QvsSLDFlowRefByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FQvsSLD.Count)) then
      TLoadFlowRefPair(FQvsSLD.Items[AIndex]).FlowRef := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUndergroundSection.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TUndergroundSection.UpdatePropertiesData';
var
  LDataNode                  : IXMLNode;
  LSectionName               : String;
  LUndergroundOutflowRouteNo : Integer;
  LUpstreamCatchmentArea     : Double;
  LBoardAndPillarArea        : Double;
  LHighExtractionArea        : Double;
  LSurfaceRunOffFactor       : Double;
begin
  Result := TRUE;
  try
    LDataNode := ARootNode.ChildNodes['MineUndergroundProperties'];
    LSectionName               := LDataNode.ChildNodes['SectionName'].Text;
    LUndergroundOutflowRouteNo := StrToInt(LDataNode.ChildNodes['UndergroundOutflowRouteNo'].Text);
    LUpstreamCatchmentArea     := StrToFloat(LDataNode.ChildNodes['UpstreamCatchmentArea'].Text);
    LBoardAndPillarArea        := StrToFloat(LDataNode.ChildNodes['BoardAndPillarArea'].Text);
    LHighExtractionArea        := StrToFloat(LDataNode.ChildNodes['HighExtractionArea'].Text);
    LSurfaceRunOffFactor       := StrToFloat(LDataNode.ChildNodes['SurfaceRunOffFactor'].Text);

    Result := GMineDBManager.UpdateUndergroundSectionPropertiesDataInDB
                              (FModuleID, FSectionNo, LSectionName, LUndergroundOutflowRouteNo,
                               LUpstreamCatchmentArea, LBoardAndPillarArea, LHighExtractionArea, LSurfaceRunOffFactor);

    if (Result) then
    begin
      FSectionName                       := LSectionName;
      FOutFlowRouteNo                    := LUndergroundOutflowRouteNo;
      FUpstreamCatchmentArea             := LUpstreamCatchmentArea;
      FBoardAndPillarArea                := LBoardAndPillarArea;
      FHighExtractionArea                := LHighExtractionArea;
      FSurfaceRunOffFactor               := LSurfaceRunOffFactor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUndergroundSection.UpdateBoardPillarData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TUndergroundSection.UpdateBoardPillarData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LInterpolationType : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
begin
  Result := TRUE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineUndergroundBoardPillar'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['BoardAndPillarInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateUndergroundSectionBoardPillarDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
      if (Result) then
      begin
        FBoardAndPillarInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadBoardPillarGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUndergroundSection.UpdateHighExtractionData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TUndergroundSection.UpdateHighExtractionData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LInterpolationType : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineUndergroundHighExtraction'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['HighExtractionInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateUndergroundSectionHighExtractionDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
      if (Result) then
      begin
        FHighExtractionInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadHighExtractionGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUndergroundSection.UpdateMonthlyData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TUndergroundSection.UpdateMonthlyData';
var
  LSectionNode  : IXMLNode;
  LListNode     : IXMLNode;
  LDataNode     : IXMLNode;
  LIndex        : Integer;
  LMonth        : Integer;
  LMonthList    : TStringList;
  LBPFactorList : TStringList;
  LHEFactorList : TStringList;
begin
  Result := FALSE;
  try
    LSectionNode := ARootNode.ChildNodes['MineUndergroundRechargeFactors'];

    LMonthList    := TStringList.Create;
    LBPFactorList := TStringList.Create;
    LHEFactorList := TStringList.Create;
    try
      LListNode    := LSectionNode.ChildNodes['DataList'];
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode  := LListNode.ChildNodes.Get(LIndex-1);
        LMonthList.Add(LDataNode.ChildNodes['Month'].Text);
        LBPFactorList.Add(LDataNode.ChildNodes['BoardAndPillarRechargeFactor'].Text);
        LHEFactorList.Add(LDataNode.ChildNodes['HighExtractionRechargeFactor'].Text);
      end;

      Result := GMineDBManager.UpdateUndergroundSectionMonthlyDataInDB
                                 (FModuleID, FSectionNo, LMonthList, LBPFactorList, LHEFactorList);
      if (Result) then
      begin
        for LIndex := 0 to LMonthList.Count - 1 do
        begin
          LMonth := StrToInt(LMonthList.Strings[LIndex]);
          FMonthlyBoardAndPillarRechargeFactor[LMonth] := StrToFloat(LBPFactorList.Strings[LIndex]);
          FMonthlyHighExtractionRechargeFactor[LMonth] := StrToFloat(LHEFactorList.Strings[LIndex])
        end;
      end;
    finally
      LMonthList.Free;
      LBPFactorList.Free;
      LHEFactorList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUndergroundSection.UpdateQSLDData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TUndergroundSection.UpdateQSLDData';
var
  LSectionNode : IXMLNode;
  LListNode    : IXMLNode;
  LDataNode    : IXMLNode;
  LIndex       : Integer;
  LStdDev      : Double;
  LFlowRefList : TStringList;
  LLoadList    : TStringList;
begin
  Result := TRUE;
  try
    LSectionNode := ARootNode.ChildNodes['MineUndergroundQSLD'];
    LStdDev      := StrToFloat(LSectionNode.ChildNodes['StdDevUnderground'].Text);

    LFlowRefList := TStringList.Create;
    LLoadList    := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LFlowRefList.Add(LDataNode.ChildNodes['Flow'].Text);
        LLoadList.Add(LDataNode.ChildNodes['Load'].Text);
      end;

      Result := GMineDBManager.UpdateUndergroundSectionQSLDDataInDB
                                 (FModuleID, FSectionNo, LStdDev, LFlowRefList, LLoadList);

      if (Result) then
      begin
        FUndergroundStdDev := LStdDev;
        Result := GMineDBManager.LoadQvsSLDDataFromDB(Self);
      end;
    finally
      LFlowRefList.Free;
      LLoadList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUndergroundSection.ClearBoardAndPillarGrowthData;
const OPNAME = 'TUndergroundSection.ClearBoardAndPillarGrowthData';
begin
  try
    while (FBoardAndPillarGrowthData.Count > 0) do
      FBoardAndPillarGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUndergroundSection.ClearHighExtractionGrowthData;
const OPNAME = 'TUndergroundSection.ClearHighExtractionGrowthData';
begin
  try
    while (FHighExtractionGrowthData.Count > 0) do
      FHighExtractionGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUndergroundSection.ClearQvsSLDData;
const OPNAME = 'TUndergroundSection.ClearQvsSLDData';
begin
  try
    while (FQvsSLD.Count > 0) do
      FQvsSLD.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOpencastPit ****************************************************************}

function TOpencastPit._AddRef: Integer;
const OPNAME = 'TOpencastPit._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit._Release: Integer;
const OPNAME = 'TOpencastPit._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.CreateMemberObjects;
const OPNAME = 'TOpencastPit.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FWorkingsAreaGrowthData      := TObjectList.Create(FALSE);
    FDisturbedAreaGrowthData     := TObjectList.Create(FALSE);
    FRehabilitatedAreaGrowthData := TObjectList.Create(FALSE);
    FPitEvaporationGrowthData    := TObjectList.Create(FALSE);
    FInspoilsDecantGrowthData    := TObjectList.Create(FALSE);
    FInspoilsSeepageGrowthData   := TObjectList.Create(FALSE);
    FWorkingsAreaQvsSLD          := TObjectList.Create(FALSE);
    FSeepageDecantQvsSLD         := TObjectList.Create(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.DestroyMemberObjects;
const OPNAME = 'TOpencastPit.DestroyMemberObjects';
begin
  try
    FWorkingsAreaGrowthData.Free;
    FDisturbedAreaGrowthData.Free;
    FRehabilitatedAreaGrowthData.Free;
    FPitEvaporationGrowthData.Free;
    FInspoilsDecantGrowthData.Free;
    FInspoilsSeepageGrowthData.Free;
    FWorkingsAreaQvsSLD.Free;
    FSeepageDecantQvsSLD.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_ModuleID: Integer;
const OPNAME = 'TOpencastPit.Get_ModuleID';
begin
  Result := 0;
  try
    Result := FModuleID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_ModuleID(Value: Integer);
const OPNAME = 'TOpencastPit.Set_ModuleID';
begin
  try
    FModuleID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SectionNo: Integer;
const OPNAME = 'TOpencastPit.Get_SectionNo';
begin
  Result := 0;
  try
    Result := FSectionNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_SectionNo(Value: Integer);
const OPNAME = 'TOpencastPit.Set_SectionNo';
begin
  try
    FSectionNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SectionName: WideString;
const OPNAME = 'TOpencastPit.Get_SectionName';
begin
  Result := '';
  try
    Result := FSectionName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_SectionName(const Value: WideString);
const OPNAME = 'TOpencastPit.Set_SectionName';
begin
  try
    FSectionName := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_CoalReserveArea: Double;
const OPNAME = 'TOpencastPit.Get_CoalReserveArea';
begin
  Result := 0.0;
  try
    Result := FCoalReserveArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_CoalReserveArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_CoalReserveArea';
begin
  try
    FCoalReserveArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsArea: Double;
const OPNAME = 'TOpencastPit.Get_WorkingsArea';
begin
  Result := 0.0;
  try
    Result := FWorkingsArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WorkingsArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_WorkingsArea';
begin
  try
    FWorkingsArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_CommissionYear: Integer;
const OPNAME = 'TOpencastPit.Get_CommissionYear';
begin
  Result := 0;
  try
    Result := FCommissionYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_CommissionYear(Value: Integer);
const OPNAME = 'TOpencastPit.Set_CommissionYear';
begin
  try
    FCommissionYear := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_CommissionMonth: Integer;
const OPNAME = 'TOpencastPit.Get_CommissionMonth';
begin
  Result := 0;
  try
    Result := FCommissionMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_CommissionMonth(Value: Integer);
const OPNAME = 'TOpencastPit.Set_CommissionMonth';
begin
  try
    FCommissionMonth := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DecommissionYear: Integer;
const OPNAME = 'TOpencastPit.Get_DecommissionYear';
begin
  Result := 0;
  try
    Result := FDecommissionYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DecommissionYear(Value: Integer);
const OPNAME = 'TOpencastPit.Set_DecommissionYear';
begin
  try
    FDecommissionYear := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DecommissionMonth: Integer;
const OPNAME = 'TOpencastPit.Get_DecommissionMonth';
begin
  Result := 0;
  try
    Result := FDecommissionMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DecommissionMonth(Value: Integer);
const OPNAME = 'TOpencastPit.Set_DecommissionMonth';
begin
  try
    FDecommissionMonth := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedArea: Double;
const OPNAME = 'TOpencastPit.Get_DisturbedArea';
begin
  Result := 0.0;
  try
    Result := FDisturbedArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_DisturbedArea';
begin
  try
    FDisturbedArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_RehabilitatedArea: Double;
const OPNAME = 'TOpencastPit.Get_RehabilitatedArea';
begin
  Result := 0.0;
  try
    Result := FRehabilitatedArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_RehabilitatedArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_RehabilitatedArea';
begin
  try
    FRehabilitatedArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PitEvaporationArea: Double;
const OPNAME = 'TOpencastPit.Get_PitEvaporationArea';
begin
  Result := 0.0;
  try
    Result := FPitEvaporationArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PitEvaporationArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_PitEvaporationArea';
begin
  try
    FPitEvaporationArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsAreaInterpolationOption: Integer;
const OPNAME = 'TOpencastPit.Get_WorkingsAreaInterpolationOption';
begin
  Result := 0;
  try
    Result := FWorkingsAreaInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WorkingsAreaInterpolationOption(Value: Integer);
const OPNAME = 'TOpencastPit.Set_WorkingsAreaInterpolationOption';
begin
  try
    FWorkingsAreaInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedAreaInterpolationOption: Integer;
const OPNAME = 'TOpencastPit.Get_DisturbedAreaInterpolationOption';
begin
  Result := 0;
  try
    Result := FDisturbedAreaInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedAreaInterpolationOption(Value: Integer);
const OPNAME = 'TOpencastPit.Set_DisturbedAreaInterpolationOption';
begin
  try
    FDisturbedAreaInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_RehabilitatedAreaInterpolationOption: Integer;
const OPNAME = 'TOpencastPit.Get_RehabilitatedAreaInterpolationOption';
begin
  Result := 0;
  try
    Result := FRehabilitatedAreaInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_RehabilitatedAreaInterpolationOption(Value: Integer);
const OPNAME = 'TOpencastPit.Set_RehabilitatedAreaInterpolationOption';
begin
  try
    FRehabilitatedAreaInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PitEvaporationAreaInterpolationOption: Integer;
const OPNAME = 'TOpencastPit.Get_PitEvaporationAreaInterpolationOption';
begin
  Result := 0;
  try
    Result :=  FPitEvaporationAreaInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PitEvaporationAreaInterpolationOption(Value: Integer);
const OPNAME = 'TOpencastPit.Set_PitEvaporationAreaInterpolationOption';
begin
  try
    FPitEvaporationAreaInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedAreaRunOffFactor: Double;
const OPNAME = 'TOpencastPit.Get_DisturbedAreaRunOffFactor';
begin
  Result := 0.0;
  try
    Result := FDisturbedAreaRunOffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedAreaRunOffFactor(Value: Double);
const OPNAME = 'TOpencastPit.Set_DisturbedAreaRunOffFactor';
begin
  try
    FDisturbedAreaRunOffFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedWorkingsAreaRunOffFactor: Double;
const OPNAME = 'TOpencastPit.Get_DisturbedWorkingsAreaRunOffFactor';
begin
  Result := 0.0;
  try
    Result := FDisturbedWorkingsAreaRunOffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedWorkingsAreaRunOffFactor(Value: Double);
const OPNAME = 'TOpencastPit.Set_DisturbedWorkingsAreaRunOffFactor';
begin
  try
    FDisturbedWorkingsAreaRunOffFactor := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WashOffParameter: Double;
const OPNAME = 'TOpencastPit.Get_WashOffParameter';
begin
  Result := 0.0;
  try
    Result := FWashOffParameter;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WashOffParameter(Value: Double);
const OPNAME = 'TOpencastPit.Set_WashOffParameter';
begin
  FWashOffParameter := Value;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SulphateBuildUpRate: Double;
const OPNAME = 'TOpencastPit.Get_SulphateBuildUpRate';
begin
  Result := 0.0;
  try
    Result := FSulphateBuildUpRate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_SulphateBuildUpRate(Value: Double);
const OPNAME = 'TOpencastPit.Set_SulphateBuildUpRate';
begin
  try
    FSulphateBuildUpRate := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InitialSaltMass: Double;
const OPNAME = 'TOpencastPit.Get_InitialSaltMass';
begin
  Result := 0.0;
  try
    Result := FInitialSaltMass;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InitialSaltMass(Value: Double);
const OPNAME = 'TOpencastPit.Set_InitialSaltMass';
begin
  try
    FInitialSaltMass := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsStorageDecantVolume: Double;
const OPNAME = 'TOpencastPit.Get_InspoilsStorageDecantVolume';
begin
  Result := 0.0;
  try
    Result := FInspoilsStorageDecantVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsStorageDecantVolume(Value: Double);
const OPNAME = 'TOpencastPit.Set_InspoilsStorageDecantVolume';
begin
  try
    FInspoilsStorageDecantVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsStorageSeepageVolume: Double;
const OPNAME = 'TOpencastPit.Get_InspoilsStorageSeepageVolume';
begin
  Result := 0.0;
  try
    Result := FInspoilsStorageSeepageVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsStorageSeepageVolume(Value: Double);
const OPNAME = 'TOpencastPit.Set_InspoilsStorageSeepageVolume';
begin
  try
    FInspoilsStorageSeepageVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsStorageInitialVolume: Double;
const OPNAME = 'TOpencastPit.Get_InspoilsStorageInitialVolume';
begin
  Result := 0.0;
  try
    Result := FInspoilsStorageInitialVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsStorageInitialVolume(Value: Double);
const OPNAME = 'TOpencastPit.Set_InspoilsStorageInitialVolume';
begin
  try
    FInspoilsStorageInitialVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsDecantInterpolationOption: Integer;
const OPNAME = 'TOpencastPit.Get_InspoilsDecantInterpolationOption';
begin
  Result := 0;
  try
    Result := FInspoilsDecantInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsDecantInterpolationOption(Value: Integer);
const OPNAME = 'TOpencastPit.Set_InspoilsDecantInterpolationOption';
begin
  try
    FInspoilsDecantInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsSeepageInterpolationOption: Integer;
const OPNAME = 'TOpencastPit.Get_InspoilsSeepageInterpolationOption';
begin
  Result := 0;
  try
    Result := FInspoilsSeepageInterpolationOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsSeepageInterpolationOption(Value: Integer);
const OPNAME = 'TOpencastPit.Set_InspoilsSeepageInterpolationOption';
begin
  try
    FInspoilsSeepageInterpolationOption := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_MaxSeepageRate: Double;
const OPNAME = 'TOpencastPit.Get_MaxSeepageRate';
begin
  Result := 0.0;
  try
    Result := FMaxSeepageRate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_MaxSeepageRate(Value: Double);
const OPNAME = 'TOpencastPit.Set_MaxSeepageRate';
begin
  try
    FMaxSeepageRate := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SeepageEquationExponent: Double;
const OPNAME = 'TOpencastPit.Get_SeepageEquationExponent';
begin
  Result := 0.0;
  try
    Result := FSeepageEquationExponent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_SeepageEquationExponent(Value: Double);
const OPNAME = 'TOpencastPit.Set_SeepageEquationExponent';
begin
  try
    FSeepageEquationExponent := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PCDFullSurfaceArea: Double;
const OPNAME = 'TOpencastPit.Get_PCDFullSurfaceArea';
begin
  Result := 0.0;
  try
    Result := FPCDFullSurfaceArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PCDFullSurfaceArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_PCDFullSurfaceArea';
begin
  try
    FPCDFullSurfaceArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PCDCapacity: Double;
const OPNAME = 'TOpencastPit.Get_PCDCapacity';
begin
  Result := 0.0;
  try
    Result := FPCDCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PCDCapacity(Value: Double);
const OPNAME = 'TOpencastPit.Set_PCDCapacity';
begin
  try
    FPCDCapacity := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PCDInitialVolume: Double;
const OPNAME = 'TOpencastPit.Get_PCDInitialVolume';
begin
  Result := 0.0;
  try
    Result := FPCDInitialVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PCDInitialVolume(Value: Double);
const OPNAME = 'TOpencastPit.Set_PCDInitialVolume';
begin
  try
    PCDInitialVolume := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsDamConcentration: Double;
const OPNAME = 'TOpencastPit.Get_InspoilsDamConcentration';
begin
  Result := 0.0;
  try
    Result := FInspoilsDamConcentration;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsDamConcentration(Value: Double);
const OPNAME = 'TOpencastPit.Set_InspoilsDamConcentration';
begin
  try
    FInspoilsDamConcentration := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_StdDevWorkingsArea: Double;
const OPNAME = 'TOpencastPit.Get_StdDevWorkingsArea';
begin
  Result := 0.0;
  try
    Result := FStdDevWorkingsArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_StdDevWorkingsArea(Value: Double);
const OPNAME = 'TOpencastPit.Set_StdDevWorkingsArea';
begin
  try
    FStdDevWorkingsArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_StdDevSeepageDecant: Double;
const OPNAME = 'TOpencastPit.Get_StdDevSeepageDecant';
begin
  Result := 0.0;
  try
    Result := FStdDevSeepageDecant;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_StdDevSeepageDecant(Value: Double);
const OPNAME = 'TOpencastPit.Set_StdDevSeepageDecant';
begin
  try
    FStdDevSeepageDecant := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Populate (AModuleID                             : Integer;
                                ASectionNo                            : Integer;
                                const ASectionName                    : WideString;
                                ACoalReserveArea                      : Double;
                                AWorkingsArea                         : Double;
                                ACommissionYear                       : Integer;
                                ACommissionMonth                      : Integer;
                                ADecommissionYear                     : Integer;
                                ADecommissionMonth                    : Integer;
                                ADisturbedArea                        : Double;
                                ARehabilitatedArea                    : Double;
                                APitEvaporationArea                   : Double;
                                AWorkingsAreaInterpolationOption      : Integer;
                                ADisturbedAreaInterpolationOption     : Integer;
                                ARehabilitatedAreaInterpolationOption : Integer;
                                APitEvaporationAreaInterpolationOption : Integer;
                                ADisturbedAreaRunOffFactor            : Double;
                                ADisturbedWorkingsAreaRunOffFactor    : Double;
                                AWashOffParameter                     : Double;
                                ASulphateBuildUpRate                  : Double;
                                AInitialSaltMass                      : Double;
                                AInspoilsStorageDecantVolume          : Double;
                                AInspoilsStorageSeepageVolume         : Double;
                                AInspoilsStorageInitialVolume         : Double;
                                AInspoilsDecantInterpolationOption    : Integer;
                                AInspoilsSeepageInterpolationOption   : Integer;
                                AMaxSeepageRate                       : Double;
                                ASeepageEquationExponent              : Double;
                                APCDFullSurfaceArea                   : Double;
                                APCDCapacity                          : Double;
                                APCDInitialVolume                     : Double;
                                AInspoilsDamConcentration             : Double;
                                AStdDevWorkingsArea                   : Double;
                                AStdDevSeepageDecant                  : Double): WordBool;
const OPNAME = 'TOpencastPit.Populate';
begin
  Result := FALSE;
  try
    FModuleID                              := AModuleID;
    FSectionNo                             := ASectionNo;
    FSectionName                           := ASectionName;
    FCoalReserveArea                       := ACoalReserveArea;
    FWorkingsArea                          := AWorkingsArea;
    FCommissionYear                        := ACommissionYear;
    FCommissionMonth                       := ACommissionMonth;
    FDecommissionYear                      := ADecommissionYear;
    FDecommissionMonth                     := ADecommissionMonth;
    FDisturbedArea                         := ADisturbedArea;
    FRehabilitatedArea                     := ARehabilitatedArea;
    FPitEvaporationArea                    := APitEvaporationArea;
    FWorkingsAreaInterpolationOption       := AWorkingsAreaInterpolationOption;
    FDisturbedAreaInterpolationOption      := ADisturbedAreaInterpolationOption;
    FRehabilitatedAreaInterpolationOption  := ARehabilitatedAreaInterpolationOption;
    FPitEvaporationAreaInterpolationOption := APitEvaporationAreaInterpolationOption;
    FDisturbedAreaRunOffFactor             := ADisturbedAreaRunOffFactor;
    FDisturbedWorkingsAreaRunOffFactor     := ADisturbedWorkingsAreaRunOffFactor;
    FWashOffParameter                      := AWashOffParameter;
    FSulphateBuildUpRate                   := ASulphateBuildUpRate;
    FInitialSaltMass                       := AInitialSaltMass;
    FInspoilsStorageDecantVolume           := AInspoilsStorageDecantVolume;
    FInspoilsStorageSeepageVolume          := AInspoilsStorageSeepageVolume;
    FInspoilsStorageInitialVolume          := AInspoilsStorageInitialVolume;
    FInspoilsDecantInterpolationOption     := AInspoilsDecantInterpolationOption;
    FInspoilsSeepageInterpolationOption    := AInspoilsSeepageInterpolationOption;
    FMaxSeepageRate                        := AMaxSeepageRate;
    FSeepageEquationExponent               := ASeepageEquationExponent;
    FPCDFullSurfaceArea                    := APCDFullSurfaceArea;
    FPCDCapacity                           := APCDCapacity;
    FPCDInitialVolume                      := APCDInitialVolume;
    FInspoilsDamConcentration              := AInspoilsDamConcentration;
    FStdDevWorkingsArea                    := AStdDevWorkingsArea;
    FStdDevSeepageDecant                   := AStdDevSeepageDecant;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddWorkingsAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TOpencastPit.AddWorkingsAreaGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FWorkingsAreaGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsAreaGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TOpencastPit.Get_WorkingsAreaGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaGrowthData.Count)) then
      Result := TYearValuePair(FWorkingsAreaGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WorkingsAreaGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TOpencastPit.Set_WorkingsAreaGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaGrowthData.Count)) then
      TYearValuePair(FWorkingsAreaGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsAreaGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_WorkingsAreaGrowthFactorByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaGrowthData.Count)) then
      Result := TYearValuePair(FWorkingsAreaGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WorkingsAreaGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_WorkingsAreaGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaGrowthData.Count)) then
      TYearValuePair(FWorkingsAreaGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_NoOfWorkingsAreaGrowthPoints: Integer;
const OPNAME = 'TOpencastPit.Get_NoOfWorkingsAreaGrowthPoints';
begin
  Result := 0;
  try
    Result := FWorkingsAreaGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddDisturbedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TOpencastPit.AddDisturbedAreaGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FDisturbedAreaGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedAreaGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TOpencastPit.Get_DisturbedAreaGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FDisturbedAreaGrowthData.Count)) then
      Result := TYearValuePair(FDisturbedAreaGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedAreaGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TOpencastPit.Set_DisturbedAreaGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FDisturbedAreaGrowthData.Count)) then
      TYearValuePair(FDisturbedAreaGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedAreaGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_DisturbedAreaGrowthFactorByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FDisturbedAreaGrowthData.Count)) then
      Result := TYearValuePair(FDisturbedAreaGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedAreaGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_DisturbedAreaGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FDisturbedAreaGrowthData.Count)) then
      TYearValuePair(FDisturbedAreaGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_NoOfDisturbedAreaGrowthPoints: Integer;
const OPNAME = 'TOpencastPit.Get_NoOfDisturbedAreaGrowthPoints';
begin
  Result := 0;
  try
    Result := FDisturbedAreaGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddRehabilitatedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TOpencastPit.AddRehabilitatedAreaGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FRehabilitatedAreaGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_RehabilitatedAreaGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TOpencastPit.Get_RehabilitatedAreaGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FRehabilitatedAreaGrowthData.Count)) then
      Result := TYearValuePair(FRehabilitatedAreaGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_RehabilitatedAreaGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TOpencastPit.Set_RehabilitatedAreaGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FRehabilitatedAreaGrowthData.Count)) then
      TYearValuePair(FRehabilitatedAreaGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_RehabilitatedAreaGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_RehabilitatedAreaGrowthFactorByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FRehabilitatedAreaGrowthData.Count)) then
      Result := TYearValuePair(FRehabilitatedAreaGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_RehabilitatedAreaGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_RehabilitatedAreaGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FRehabilitatedAreaGrowthData.Count)) then
      TYearValuePair(FRehabilitatedAreaGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_NoOfRehabilitatedAreaGrowthPoints: Integer;
const OPNAME = 'TOpencastPit.Get_NoOfRehabilitatedAreaGrowthPoints';
begin
  Result := 0;
  try
    Result := FRehabilitatedAreaGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddPitEvaporationGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TOpencastPit.AddPitEvaporationGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FPitEvaporationGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PitEvaporationGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TOpencastPit.Get_PitEvaporationGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FPitEvaporationGrowthData.Count)) then
      Result := TYearValuePair(FPitEvaporationGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PitEvaporationGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TOpencastPit.Set_PitEvaporationGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FPitEvaporationGrowthData.Count)) then
      TYearValuePair(FPitEvaporationGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_PitEvaporationGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_PitEvaporationGrowthFactorByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FPitEvaporationGrowthData.Count)) then
      Result := TYearValuePair(FPitEvaporationGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_PitEvaporationGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_PitEvaporationGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FPitEvaporationGrowthData.Count)) then
      TYearValuePair(FPitEvaporationGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_NoOfPitEvaporationGrowthPoints: Integer;
const OPNAME = 'TOpencastPit.Get_NoOfPitEvaporationGrowthPoints';
begin
  Result := 0;
  try
    Result := FPitEvaporationGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddInspoilsDecantGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TOpencastPit.AddInspoilsDecantGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FInspoilsDecantGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsDecantGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TOpencastPit.Get_InspoilsDecantGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsDecantGrowthData.Count)) then
      Result := TYearValuePair(FInspoilsDecantGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsDecantGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TOpencastPit.Set_InspoilsDecantGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsDecantGrowthData.Count)) then
      TYearValuePair(FInspoilsDecantGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsDecantGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_InspoilsDecantGrowthFactorByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsDecantGrowthData.Count)) then
      Result := TYearValuePair(FInspoilsDecantGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsDecantGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_InspoilsDecantGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsDecantGrowthData.Count)) then
      TYearValuePair(FInspoilsDecantGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_NoOfInspoilsDecantGrowthPoints: Integer;
const OPNAME = 'TOpencastPit.Get_NoOfInspoilsDecantGrowthPoints';
begin
  Result := 0;
  try
    Result := FInspoilsDecantGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddInspoilsSeepageGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TOpencastPit.AddInspoilsSeepageGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FInspoilsSeepageGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsSeepageGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TOpencastPit.Get_InspoilsSeepageGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsSeepageGrowthData.Count)) then
      Result := TYearValuePair(FInspoilsSeepageGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsSeepageGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TOpencastPit.Set_InspoilsSeepageGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsSeepageGrowthData.Count)) then
      TYearValuePair(FInspoilsSeepageGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_InspoilsSeepageGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_InspoilsSeepageGrowthFactorByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsSeepageGrowthData.Count)) then
      Result := TYearValuePair(FInspoilsSeepageGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_InspoilsSeepageGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_InspoilsSeepageGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FInspoilsSeepageGrowthData.Count)) then
      TYearValuePair(FInspoilsSeepageGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_NoOfInspoilsSeepageGrowthPoints: Integer;
const OPNAME = 'TOpencastPit.Get_NoOfInspoilsSeepageGrowthPoints';
begin
  Result := 0;
  try
    Result := FInspoilsSeepageGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedAreaRechargeFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_DisturbedAreaRechargeFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyDisturbedAreaRechargeFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedAreaRechargeFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_DisturbedAreaRechargeFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyDisturbedAreaRechargeFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_DisturbedWorkingsAreaRechargeFactorByMonth(AMonthIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_DisturbedWorkingsAreaRechargeFactorByMonth';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyDisturbedWorkingsAreaRechargeFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_DisturbedWorkingsAreaRechargeFactorByMonth(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_DisturbedWorkingsAreaRechargeFactorByMonth';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyDisturbedWorkingsAreaRechargeFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsAreaQvsSLDNoOfPoints: Integer;
const OPNAME = 'TOpencastPit.Get_WorkingsAreaQvsSLDNoOfPoints';
begin
  Result := 0;
  try
    Result := FWorkingsAreaQvsSLD.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsAreaQvsSLDLoadByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_WorkingsAreaQvsSLDLoadByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FWorkingsAreaQvsSLD.Items[AIndex]).Load;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WorkingsAreaQvsSLDLoadByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_WorkingsAreaQvsSLDLoadByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaQvsSLD.Count)) then
      TLoadFlowRefPair(FWorkingsAreaQvsSLD.Items[AIndex]).Load := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_WorkingsAreaQvsSLDFlowRefByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_WorkingsAreaQvsSLDFlowRefByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FWorkingsAreaQvsSLD.Items[AIndex]).FlowRef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_WorkingsAreaQvsSLDFlowRefByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_WorkingsAreaQvsSLDFlowRefByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FWorkingsAreaQvsSLD.Count)) then
      TLoadFlowRefPair(FWorkingsAreaQvsSLD.Items[AIndex]).FlowRef := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SeepageDecantQvsSLDNoOfPoints: Integer;
const OPNAME = 'TOpencastPit.Get_SeepageDecantQvsSLDNoOfPoints';
begin
  Result := 0;
  try
    Result := FSeepageDecantQvsSLD.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SeepageDecantQvsSLDLoadByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_SeepageDecantQvsSLDLoadByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FSeepageDecantQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FSeepageDecantQvsSLD.Items[AIndex]).Load;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_SeepageDecantQvsSLDLoadByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_SeepageDecantQvsSLDLoadByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FSeepageDecantQvsSLD.Count)) then
      TLoadFlowRefPair(FSeepageDecantQvsSLD.Items[AIndex]).Load := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.Get_SeepageDecantQvsSLDFlowRefByIndex(AIndex: Integer): Double;
const OPNAME = 'TOpencastPit.Get_SeepageDecantQvsSLDFlowRefByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FSeepageDecantQvsSLD.Count)) then
      Result := TLoadFlowRefPair(FSeepageDecantQvsSLD.Items[AIndex]).FlowRef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpencastPit.Set_SeepageDecantQvsSLDFlowRefByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TOpencastPit.Set_SeepageDecantQvsSLDFlowRefByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FSeepageDecantQvsSLD.Count)) then
      TLoadFlowRefPair(FSeepageDecantQvsSLD.Items[AIndex]).FlowRef := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddWorkingsAreaLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool;
const OPNAME = 'TOpencastPit.AddWorkingsAreaLoadFlowRefPair';
var
  LPair : TLoadFlowRefPair;
begin
  Result := FALSE;
  try
    LPair := TLoadFlowRefPair.Create;
    LPair.Load := ALoad;
    LPair.FlowRef := AFlowRef;
    FWorkingsAreaQvsSLD.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.AddSeepageDecantLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool;
const OPNAME = 'TOpencastPit.AddSeepageDecantLoadFlowRefPair';
var
  LPair : TLoadFlowRefPair;
begin
  Result := FALSE;
  try
    LPair := TLoadFlowRefPair.Create;
    LPair.Load := ALoad;
    LPair.FlowRef := AFlowRef;
    FSeepageDecantQvsSLD.Add(LPair);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpencastPit.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdatePropertiesData';
var
  LDataNode                         : IXMLNode;
  LSectionName                      : String;
  LCoalReserveArea                  : Double;
  LWorkingArea                      : Double;
  LCommissionYear                   : Integer;
  LCommissionMonth                  : Integer;
  LDeCommissionYear                 : Integer;
  LDeCommissionMonth                : Integer;
  LDisturbedArea                    : Double;
  LRehabilitatedArea                : Double;
  LEvaporationArea                  : Double;
  LDisturbedAreaRunOffFactor        : Double;
  LDisturbedWorkingAreaRunOffFactor : Double;
  LWashOffParameter                 : Double;
  LSulphateBuildUpRate              : Double;
  LInitialSaltMass                  : Double;
  LInspoilsStorageDecant            : Double;
  LInspoilsStorageSeepage           : Double;
  LSeepageEquationExponent          : Double;
  LMaxSeepageRate                   : Double;
  LInspoilsStorageInitialVolume     : Double;
  LInspoilsDamConcentration         : Double;
  LPCDInitialVolume                 : Double;
  LPCDCapacity                      : Double;
  LPCDFullSurfaceArea               : Double;
begin
  Result := TRUE;
  try
    LDataNode := ARootNode.ChildNodes['MineOpencastPitProperties'];
    LSectionName                      := LDataNode.ChildNodes['SectionName'].Text;
    LCoalReserveArea                  := StrToFloat(LDataNode.ChildNodes['CoalReserveArea'].Text);
    LWorkingArea                      := StrToFloat(LDataNode.ChildNodes['WorkingArea'].Text);
    LCommissionYear                   := StrToInt(LDataNode.ChildNodes['CommissionYear'].Text);
    LCommissionMonth                  := StrToInt(LDataNode.ChildNodes['CommissionMonth'].Text);
    LDeCommissionYear                 := StrToInt(LDataNode.ChildNodes['DeCommissionYear'].Text);
    LDeCommissionMonth                := StrToInt(LDataNode.ChildNodes['DeCommissionMonth'].Text);
    LDisturbedArea                    := StrToFloat(LDataNode.ChildNodes['DisturbedArea'].Text);
    LRehabilitatedArea                := StrToFloat(LDataNode.ChildNodes['RehabilitatedArea'].Text);
    LEvaporationArea                  := StrToFloat(LDataNode.ChildNodes['EvaporationArea'].Text);
    LDisturbedAreaRunOffFactor        := StrToFloat(LDataNode.ChildNodes['DisturbedAreaRunOffFactor'].Text);
    LDisturbedWorkingAreaRunOffFactor := StrToFloat(LDataNode.ChildNodes['DisturbedWorkingAreaRunOffFactor'].Text);
    LWashOffParameter                 := StrToFloat(LDataNode.ChildNodes['WashOffParameter'].Text);
    LSulphateBuildUpRate              := StrToFloat(LDataNode.ChildNodes['SulphateBuildUpRate'].Text);
    LInitialSaltMass                  := StrToFloat(LDataNode.ChildNodes['InitialSaltMass'].Text);
    LInspoilsStorageDecant            := StrToFloat(LDataNode.ChildNodes['InspoilsStorageDecant'].Text);
    LInspoilsStorageSeepage           := StrToFloat(LDataNode.ChildNodes['InspoilsStorageSeepage'].Text);
    LSeepageEquationExponent          := StrToFloat(LDataNode.ChildNodes['SeepageEquationExponent'].Text);
    LMaxSeepageRate                   := StrToFloat(LDataNode.ChildNodes['MaxSeepageRate'].Text);
    LInspoilsStorageInitialVolume     := StrToFloat(LDataNode.ChildNodes['InspoilsStorageInitialVolume'].Text);
    LInspoilsDamConcentration         := StrToFloat(LDataNode.ChildNodes['InspoilsDamConcentration'].Text);
    LPCDInitialVolume                 := StrToFloat(LDataNode.ChildNodes['PCDInitialVolume'].Text);
    LPCDCapacity                      := StrToFloat(LDataNode.ChildNodes['PCDCapacity'].Text);
    LPCDFullSurfaceArea               := StrToFloat(LDataNode.ChildNodes['PCDFullSurfaceArea'].Text);

    Result := GMineDBManager.UpdateOpencastPitPropertiesDataInDB
                              (FModuleID, FSectionNo, LSectionName, LCoalReserveArea, LWorkingArea, LCommissionYear,
                               LCommissionMonth, LDeCommissionYear, LDeCommissionMonth, LDisturbedArea, LRehabilitatedArea,
                               LEvaporationArea, LDisturbedAreaRunOffFactor, LDisturbedWorkingAreaRunOffFactor, LWashOffParameter,
                               LSulphateBuildUpRate, LInitialSaltMass, LInspoilsStorageDecant, LInspoilsStorageSeepage,
                               LSeepageEquationExponent, LMaxSeepageRate, LInspoilsStorageInitialVolume, LInspoilsDamConcentration,
                               LPCDInitialVolume, LPCDCapacity, LPCDFullSurfaceArea);
    if (Result) then
    begin
      FSectionName                       := LSectionName;
      FCoalReserveArea                   := LCoalReserveArea;
      FWorkingsArea                      := LWorkingArea;
      FCommissionYear                    := LCommissionYear;
      FCommissionMonth                   := LCommissionMonth;
      FDeCommissionYear                  := LDeCommissionYear;
      FDeCommissionMonth                 := LDeCommissionMonth;
      FDisturbedArea                     := LDisturbedArea;
      FRehabilitatedArea                 := LRehabilitatedArea;
      FPitEvaporationArea                := LEvaporationArea;
      FDisturbedAreaRunOffFactor         := LDisturbedAreaRunOffFactor;
      FDisturbedWorkingsAreaRunOffFactor := LDisturbedWorkingAreaRunOffFactor;
      FWashOffParameter                  := LWashOffParameter;
      FSulphateBuildUpRate               := LSulphateBuildUpRate;
      FInitialSaltMass                   := LInitialSaltMass;
      FInspoilsStorageDecantVolume       := LInspoilsStorageDecant;
      FInspoilsStorageSeepageVolume      := LInspoilsStorageSeepage;
      FSeepageEquationExponent           := LSeepageEquationExponent;
      FMaxSeepageRate                    := LMaxSeepageRate;
      FInspoilsStorageInitialVolume      := LInspoilsStorageInitialVolume;
      FInspoilsDamConcentration          := LInspoilsDamConcentration;
      FPCDInitialVolume                  := LPCDInitialVolume;
      FPCDCapacity                       := LPCDCapacity;
      FPCDFullSurfaceArea                := LPCDFullSurfaceArea;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateWorkingsAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateWorkingsAreaData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := TRUE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineOpencastPitWorkings'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['WorkingAreaInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitWorkingsAreaDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);

      if (Result) then
      begin
        FWorkingsAreaInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadWorkingAreaGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateDisturbedAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateDisturbedAreaData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineOpencastPitDisturbed'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['DisturbedAreaInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitDisturbedAreaDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
                                 
      if (Result) then
      begin
        FDisturbedAreaInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadDisturbedAreaGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateRehabilitatedAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateRehabilitatedAreaData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineOpencastPitRehabilitated'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['RehabilitatedAreaInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitRehabilitatedAreaDataInDB
                                (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
      if (Result) then
      begin
        FRehabilitatedAreaInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadRehabilitatedAreaGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateEvaporationAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateEvaporationAreaData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineOpencastPitEvaporation'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['EvaporationAreaInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitEvaporationAreaDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
      if (Result) then
      begin
        FPitEvaporationAreaInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadPitEvaporationAreaGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateInspoilsDecantData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateInspoilsDecantData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineOpencastPitDecant'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['InspoilsDecantInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitInspoilsDecantDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
      if (Result) then
      begin
        FInspoilsDecantInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadInspoilsDecantGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateInspoilsSeepageData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateInspoilsSeepageData';
var
  LSectionNode       : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := FALSE;
  try
    LSectionNode       := ARootNode.ChildNodes['MineOpencastPitSeepage'];
    LInterpolationType := StrToInt(LSectionNode.ChildNodes['InspoilsSeepageInterpolationOption'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['GrowthDataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Growth'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitInspoilsSeepageDataInDB
                                 (FModuleID, FSectionNo, LInterpolationType, LYearList, LGrowthList);
      if (Result) then
      begin
        FInspoilsSeepageInterpolationOption := LInterpolationType;
        Result := GMineDBManager.LoadInspoilsSeepageGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateRechargeFactorsData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateRechargeFactorsData';
var
  LSectionNode  : IXMLNode;
  LListNode     : IXMLNode;
  LDataNode     : IXMLNode;
  LIndex        : Integer;
  LMonth        : Integer;
  LMonthList    : TStringList;
  LDFactorList  : TStringList;
  LDWFactorList : TStringList;
begin
  Result := FALSE;
  try
    LSectionNode := ARootNode.ChildNodes['MineOpencastPitRechargeFactors'];

    LMonthList    := TStringList.Create;
    LDFactorList  := TStringList.Create;
    LDWFactorList := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode  := LListNode.ChildNodes.Get(LIndex-1);
        LMonthList.Add(LDataNode.ChildNodes['Month'].Text);
        LDFactorList.Add(LDataNode.ChildNodes['DisturbedAreaRechargeFactor'].Text);
        LDWFactorList.Add(LDataNode.ChildNodes['DisturbedWorkingsAreaRechargeFactor'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitMonthlyDataInDB(FModuleID, FSectionNo, LMonthList, LDFactorList, LDWFactorList);

      if (Result) then
      begin
        for LIndex := 0 to LMonthList.Count - 1 do
        begin
          LMonth := StrToInt(LMonthList.Strings[LIndex]);
          FMonthlyDisturbedAreaRechargeFactor[LMonth]         := StrToFloat(LDFactorList.Strings[LIndex]);
          FMonthlyDisturbedWorkingsAreaRechargeFactor[LMonth] := StrToFloat(LDWFactorList.Strings[LIndex]);
        end;
      end;
    finally
      LMonthList.Free;
      LDFactorList.Free;
      LDWFactorList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateWorkingsQSLDData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateWorkingsQSLDData';
var
  LSectionNode : IXMLNode;
  LListNode    : IXMLNode;
  LDataNode    : IXMLNode;
  LIndex       : Integer;
  LLoadList    : TStringList;
  LFlowRefList : TStringList;
  LStdDev      : Double;
begin
  Result := FALSE;
  try
    LSectionNode := ARootNode.ChildNodes['MineOpencastPitWorkingsQSLD'];
    LStdDev      := StrToFloat(LSectionNode.ChildNodes['StdDevWorkingsArea'].Text);

    LFlowRefList := TStringList.Create;
    LLoadList    := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LFlowRefList.Add(LDataNode.ChildNodes['Flow'].Text);
        LLoadList.Add(LDataNode.ChildNodes['Load'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitWorkingsQSLDDataInDB
                                 (FModuleID, FSectionNo, LStdDev, LFlowRefList, LLoadList);

      if (Result) then
      begin
        FStdDevWorkingsArea := LStdDev;
        Result := GMineDBManager.LoadWorkingsQvsSLDDataFromDB(Self);
      end;
    finally
      LFlowRefList.Free;
      LLoadList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOpencastPit.UpdateSeepDecantQSLDData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TOpencastPit.UpdateSeepDecantQSLDData';
var
  LSectionNode : IXMLNode;
  LListNode    : IXMLNode;
  LDataNode    : IXMLNode;
  LIndex       : Integer;
  LLoadList    : TStringList;
  LFlowRefList : TStringList;
  LStdDev      : Double;
begin
  Result := TRUE;
  try
    LSectionNode := ARootNode.ChildNodes['MineOpencastPitSeepDecantQSLD'];
    LStdDev      := StrToFloat(LSectionNode.ChildNodes['StdDevSeepageDecant'].Text);

    LFlowRefList := TStringList.Create;
    LLoadList    := TStringList.Create;
    try
      LListNode := LSectionNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LFlowRefList.Add(LDataNode.ChildNodes['Flow'].Text);
        LLoadList.Add(LDataNode.ChildNodes['Load'].Text);
      end;

      Result := GMineDBManager.UpdateOpencastPitSeepDecantQSLDDataInDB
                                 (FModuleID, FSectionNo, LStdDev, LFlowRefList, LLoadList);
      if (Result) then
      begin
        FStdDevSeepageDecant := LStdDev;
        Result := GMineDBManager.LoadSeepDecantQvsSLDDataFromDB(Self);
      end;
    finally
      LFlowRefList.Free;
      LLoadList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearWorkingsAreaGrowthData;
const OPNAME = 'TOpencastPit.ClearWorkingsAreaGrowthData';
begin
  try
    while (FWorkingsAreaGrowthData.Count > 0) do
      FWorkingsAreaGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearDisturbedAreaGrowthData;
const OPNAME = 'TOpencastPit.ClearDisturbedAreaGrowthData';
begin
  try
    while (FDisturbedAreaGrowthData.Count > 0) do
      FDisturbedAreaGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearRehabilitatedAreaGrowthData;
const OPNAME = 'TOpencastPit.ClearRehabilitatedAreaGrowthData';
begin
  try
    while (FRehabilitatedAreaGrowthData.Count > 0) do
      FRehabilitatedAreaGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearPitEvaporationGrowthData;
const OPNAME = 'TOpencastPit.ClearPitEvaporationGrowthData';
begin
  try
    while (FPitEvaporationGrowthData.Count > 0) do
      FPitEvaporationGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearInspoilsDecantGrowthData;
const OPNAME = 'TOpencastPit.ClearInspoilsDecantGrowthData';
begin
  try
    while (FInspoilsDecantGrowthData.Count > 0) do
      FInspoilsDecantGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearInspoilsSeepageGrowthData;
const OPNAME = 'TOpencastPit.ClearInspoilsSeepageGrowthData';
begin
  try
    while (FInspoilsSeepageGrowthData.Count > 0) do
      FInspoilsSeepageGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearWorkingsAreaQvsSLD;
const OPNAME = 'TOpencastPit.ClearWorkingsAreaQvsSLD';
begin
  try
    while (FWorkingsAreaQvsSLD.Count > 0) do
      FWorkingsAreaQvsSLD.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpencastPit.ClearSeepageDecantQvsSLD;
const OPNAME = 'TOpencastPit.ClearSeepageDecantQvsSLD';
begin
  try
    while (FSeepageDecantQvsSLD.Count > 0) do
      FSeepageDecantQvsSLD.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMineModule **************************************************************}

function TMineModule._AddRef: Integer;
const OPNAME = 'TMineModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule._Release: Integer;
const OPNAME = 'TMineModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineModule.CreateMemberObjects;
const OPNAME = 'TMineModule.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPlantAreaGrowthData := TObjectList.Create(FALSE);
    FOpencastPits        := TObjectList.Create(FALSE);
    FUndergroundSections := TObjectList.Create(FALSE);
    FSlurryDumps         := TObjectList.Create(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineModule.DestroyMemberObjects;
const OPNAME = 'TMineModule.DestroyMemberObjects';
begin
  try
    FPlantAreaGrowthData.Free;
    FOpencastPits.Free;
    FUndergroundSections.Free;
    FSlurryDumps.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_MineName: WideString;
const OPNAME = 'TMineModule.Get_MineName';
begin
  Result := '';
  try
    Result := FMineName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_MineName(const Value: WideString);
const OPNAME = 'TMineModule.Set_MineName';
begin
  try
    FMineName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_VersionNo: Integer;
const OPNAME = 'TMineModule.Get_VersionNo';
begin
  Result := 0;
  try
    Result := FVersionNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_VersionNo(Value: Integer);
const OPNAME = 'TMineModule.Set_VersionNo';
begin
  try
    FVersionNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_RunOffModuleNo: Integer;
const OPNAME = 'TMineModule.Get_RunOffModuleNo';
begin
  Result := 0;
  try
    Result := FRunOffModuleNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_RunOffModuleNo(Value: Integer);
const OPNAME = 'TMineModule.Set_RunOffModuleNo';
begin
  try
    FRunOffModuleNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_OutflowRouteNoToRiver: Integer;
const OPNAME = 'TMineModule.Get_OutflowRouteNoToRiver';
begin
  Result := 0;
  try
    Result := FOutflowRouteNoToRiver;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_OutflowRouteNoToRiver(Value: Integer);
const OPNAME = 'TMineModule.Set_OutflowRouteNoToRiver';
begin
  try
    FOutflowRouteNoToRiver := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_OutflowRouteNoToPCD: Integer;
const OPNAME = 'TMineModule.Get_OutflowRouteNoToPCD';
begin
  Result := 0;
  try
    Result := FOutflowRouteNoToPCD;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_OutflowRouteNoToPCD(Value: Integer);
const OPNAME = 'TMineModule.Set_OutflowRouteNoToPCD';
begin
  try
    FOutflowRouteNoToPCD := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_RainfallFileName: WideString;
const OPNAME = 'TMineModule.Get_RainfallFileName';
begin
  Result := '';
  try
    Result := FRainfallFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_RainfallFileName(const Value: WideString);
const OPNAME = 'TMineModule.Set_RainfallFileName';
begin
  try
    FRainfallFileName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_MAP: Double;
const OPNAME = 'TMineModule.Get_MAP';
begin
  Result := 0.0;
  try
    Result := FMAP;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_MAP(Value: Double);
const OPNAME = 'TMineModule.Set_MAP';
begin
  try
    FMAP := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_PlantArea: Double;
const OPNAME = 'TMineModule.Get_PlantArea';
begin
  Result := 0.0;
  try
    Result := FPlantArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_PlantArea(Value: Double);
const OPNAME = 'TMineModule.Set_PlantArea';
begin
  try
    FPlantArea := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_PlantAreaRunOffFactor: Double;
const OPNAME = 'TMineModule.Get_PlantAreaRunOffFactor';
begin
  Result := 0.0;
  try
    Result := FPlantAreaRunOffFactor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_PlantAreaRunOffFactor(Value: Double);
const OPNAME = 'TMineModule.Set_PlantAreaRunOffFactor';
begin
  try
    FPlantAreaRunOffFactor := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_SaltBuildUpRate: Double;
const OPNAME = 'TMineModule.Get_SaltBuildUpRate';
begin
  Result := 0.0;
  try
    Result := FSaltBuildUpRate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_SaltBuildUpRate(Value: Double);
const OPNAME = 'TMineModule.Set_SaltBuildUpRate';
begin
  try
    FSaltBuildUpRate := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_SaltWashOffFactor: Double;
const OPNAME = 'TMineModule.Get_SaltWashOffFactor';
begin
  Result := 0.0;
  try
    Result := FSaltWashOffFactor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_SaltWashOffFactor(Value: Double);
const OPNAME = 'TMineModule.Set_SaltWashOffFactor';
begin
  try
    FSaltWashOffFactor := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_InitialSaltStore: Double;
const OPNAME = 'TMineModule.Get_InitialSaltStore';
begin
  Result := 0.0;
  try
    Result := FInitialSaltStore;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_InitialSaltStore(Value: Double);
const OPNAME = 'TMineModule.Set_InitialSaltStore';
begin
  try
    FInitialSaltStore := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_PlantAreaGrowthInterpolationType: Integer;
const OPNAME = 'TMineModule.Get_PlantAreaGrowthInterpolationType';
begin
  Result := 0;
  try
    Result := FPlantAreaGrowthInterpolationType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_PlantAreaGrowthInterpolationType(Value: Integer);
const OPNAME = 'TMineModule.Set_PlantAreaGrowthInterpolationType';
begin
  try
    FPlantAreaGrowthInterpolationType := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_NoOfPlantAreaGrowthPoints: Integer;
const OPNAME = 'TMineModule.Get_NoOfPlantAreaGrowthPoints';
begin
  Result := 0;
  try
    Result := FPlantAreaGrowthData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_PlantAreaGrowthYearByIndex(AIndex: Integer): Integer;
const OPNAME = 'TMineModule.Get_PlantAreaGrowthYearByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FPlantAreaGrowthData.Count)) then
      Result := TYearValuePair(FPlantAreaGrowthData.Items[AIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_PlantAreaGrowthYearByIndex(AIndex: Integer; Value: Integer);
const OPNAME = 'TMineModule.Set_PlantAreaGrowthYearByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FPlantAreaGrowthData.Count)) then
      TYearValuePair(FPlantAreaGrowthData.Items[AIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Get_PlantAreaGrowthFactorByIndex(AIndex: Integer): Double;
const OPNAME = 'TMineModule.Get_PlantAreaGrowthFactorByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FPlantAreaGrowthData.Count)) then
      Result := TYearValuePair(FPlantAreaGrowthData.Items[AIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineModule.Set_PlantAreaGrowthFactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TMineModule.Set_PlantAreaGrowthFactorByIndex';
begin
  try
    if ((AIndex >= 0) AND (AIndex < FPlantAreaGrowthData.Count)) then
      TYearValuePair(FPlantAreaGrowthData.Items[AIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.AddPlantAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool;
const OPNAME = 'TMineModule.AddPlantAreaGrowthData';
var
  LData : TYearValuePair;
begin
  Result := FALSE;
  try
    LData := TYearValuePair.Create;
    LData.Year := AYear;
    LData.Value := AGrowthFactor;
    FPlantAreaGrowthData.Add(LData);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.Populate (ANetworkID                        : Integer;
                               AModuleID                         : Integer;
                               const AModuleType                 : WideString;
                               AModuleNumber                     : Integer;
                               ANetworkSequence                  : Integer;
                               const AActive                     : WideString;
                               const AMineName                   : WideString;
                               AVersionNo                        : Integer;
                               ARunOffModuleNo                   : Integer;
                               AOutflowRouteNoToRiver            : Integer;
                               AOutflowRouteNoToPCD              : Integer;
                               const ARainfallFileName           : WideString;
                               AMAP                              : Double;
                               APlantArea                        : Double;
                               APlantAreaRunOffFactor            : Double;
                               ASaltBuildUpRate                  : Double;
                               ASaltWashOffFactor                : Double;
                               AInitialSaltStore                 : Double;
                               APlantAreaGrowthInterpolationType : Integer;
                               ALongitude                        : Double;
                               ALatitude                         : Double): WordBool;
const OPNAME = 'TMineModule.Populate';
begin
  Result := FALSE;
  try
    FNetworkID                        := ANetworkID;
    FModuleID                         := AModuleID;
    FModuleType                       := AModuleType;
    FModuleNumber                     := AModuleNumber;
    FNetworkSequence                  := ANetworkSequence;
    FActive                           := AActive;
    FMineName                         := AMineName;
    FVersionNo                        := AVersionNo;
    FRunOffModuleNo                   := ARunOffModuleNo;
    FOutflowRouteNoToRiver            := AOutflowRouteNoToRiver;
    FOutflowRouteNoToPCD              := AOutflowRouteNoToPCD;
    FRainfallFileName                 := ARainfallFileName;
    FMAP                              := AMAP;
    FPlantArea                        := APlantArea;
    FPlantAreaRunOffFactor            := APlantAreaRunOffFactor;
    FSaltBuildUpRate                  := ASaltBuildUpRate;
    FSaltWashOffFactor                := ASaltWashOffFactor;
    FInitialSaltStore                 := AInitialSaltStore;
    FPlantAreaGrowthInterpolationType := APlantAreaGrowthInterpolationType;
    FLongitude                        := ALongitude;
    FLatitude                         := ALatitude;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.AddOpencastPit: TOpencastPit;
const OPNAME = 'TMineModule.AddOpencastPit';
var
  LOpencastPit : TOpencastPit;
begin
  Result := nil;
  try
    LOpencastPit := TOpencastPit.Create;
    FOpencastPits.Add(LOpencastPit);
    Result := LOpencastPit;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_NoOfOpencastPits: Integer;
const OPNAME = 'TMineModule.Get_NoOfOpencastPits';
begin
  Result := 0;
  try
    Result := FOpencastPits.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.FindOpencastPitBySectionNo(ASectionNo: Integer): TOpencastPit;
const OPNAME = 'TMineModule.FindOpencastPitBySectionNo';
var
  LOpencastPit : TOpencastPit;
  LIndex       : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FOpencastPits.Count)) do
    begin
      LOpencastPit := TOpencastPit(FOpencastPits.Items[LIndex]);
      if (LOpencastPit.FSectionNo = ASectionNo) then
        Result := LOpencastPit
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_OpencastPitBySectionNo(ASectionNo: Integer): IOpencastPit;
const OPNAME = 'TMineModule.Get_OpencastPitBySectionNo';
begin
  Result := nil;
  try
    Result := FindOpencastPitBySectionNo(ASectionNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_OpencastPitByIndex(AIndex: Integer): IOpencastPit;
const OPNAME = 'TMineModule.Get_NoOfOpencastPits';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FOpencastPits.Count)) then
      Result := TOpencastPit(FOpencastPits.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.AddUndergroundSection: TUndergroundSection;
const OPNAME = 'TMineModule.AddUndergroundSection';
var
  LUndergroundSection : TUndergroundSection;
begin
  Result := nil;
  try
    LUndergroundSection := TUndergroundSection.Create;
    FUndergroundSections.Add(LUndergroundSection);
    Result := LUndergroundSection;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.CreateNewUndergroundSection : IUndergroundSection;
const OPNAME = 'TMineModule.CreateNewUndergroundSection';
begin
  Result := nil;
  try
    Result := GMineDBManager.CreateNewUndergroundSectionInDB(Self);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.RemoveUndergroundSection (ASectionNo : Integer) : WordBool;
const OPNAME = 'TMineModule.RemoveUndergroundSection';
var
  LUnderground : TUndergroundSection;
  LIndex       : Integer;
begin
  Result := FALSE;
  try
    LUnderground := FindUndergroundSectionBySectionNo(ASectionNo);
    if (LUnderground <> nil) then
    begin
      if (GMineDBManager.RemoveUndergroundSectionFromDB(FModuleID, ASectionNo)) then
      begin
        LIndex := FUndergroundSections.IndexOf(LUnderground);
        FUndergroundSections.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_NoOfUndergroundSections: Integer;
const OPNAME = 'TMineModule.Get_NoOfUndergroundSections';
begin
  Result := 0;
  try
    Result := FUndergroundSections.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.FindUndergroundSectionBySectionNo (ASectionNo: Integer): TUndergroundSection;
const OPNAME = 'TMineModule.FindUndergroundSectionBySectionNo';
var
  LUndergroundSection : TUndergroundSection;
  LIndex              : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FUndergroundSections.Count)) do
    begin
      LUndergroundSection := TUndergroundSection(FUndergroundSections.Items[LIndex]);
      if (LUndergroundSection.FSectionNo = ASectionNo) then
        Result := LUndergroundSection
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_UndergroundSectionBySectionNo (ASectionNo: Integer): IUndergroundSection;
const OPNAME = 'TMineModule.Get_UndergroundSectionBySectionNo';
begin
  Result := nil;
  try
    Result := FindUndergroundSectionBySectionNo(ASectionNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_UndergroundSectionByIndex (AIndex: Integer): IUndergroundSection;
const OPNAME = 'TMineModule.Get_UndergroundSectionByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FUndergroundSections.Count)) then
      Result := TUndergroundSection(FUndergroundSections.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.AddSlurryDump: TSlurryDump;
const OPNAME = 'TMineModule.AddSlurryDump';
var
  LSlurryDump : TSlurryDump;
begin
  Result := nil;
  try
    LSlurryDump := TSlurryDump.Create;
    FSlurryDumps.Add(LSlurryDump);
    Result := LSlurryDump;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.CreateNewSlurryDump : ISlurryDump;
const OPNAME = 'TMineModule.CreateNewSlurryDump';
begin
  Result := nil;
  try
    Result := GMineDBManager.CreateNewSlurryDumpInDB(Self);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.RemoveSlurryDump (ASectionNo : Integer) : WordBool;
const OPNAME = 'TMineModule.RemoveSlurryDump';
var
  LSlurryDump  : TSlurryDump;
  LIndex       : Integer;
begin
  Result := FALSE;
  try
    LSlurryDump := FindSlurryDumpSectionBySectionNo(ASectionNo);
    if (LSlurryDump <> nil) then
    begin
      if (GMineDBManager.RemoveSlurryDumpFromDB(FModuleID, ASectionNo)) then
      begin
        LIndex := FSlurryDumps.IndexOf(LSlurryDump);
        FSlurryDumps.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_NoOfSlurryDumps: Integer;
const OPNAME = 'TMineModule.Get_NoOfSlurryDumps';
begin
  Result := 0;
  try
    Result := FSlurryDumps.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.FindSlurryDumpSectionBySectionNo (ASectionNo: Integer): TSlurryDump;
const OPNAME = 'TMineModule.FindSlurryDumpSectionBySectionNo';
var
  LSlurryDump : TSlurryDump;
  LIndex      : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FSlurryDumps.Count)) do
    begin
      LSlurryDump := TSlurryDump(FSlurryDumps.Items[LIndex]);
      if (LSlurryDump.FSectionNo = ASectionNo) then
        Result := LSlurryDump
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_SlurryDumpBySectionNo(ASectionNo: Integer): ISlurryDump;
const OPNAME = 'TMineModule.Get_SlurryDumpBySectionNo';
begin
  Result := nil;
  try
    Result := FindSlurryDumpSectionBySectionNo(ASectionNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.Get_SlurryDumpByIndex(AIndex: Integer): ISlurryDump;
const OPNAME = 'TMineModule.Get_SlurryDumpByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FSlurryDumps.Count)) then
      Result := TSlurryDump(FSlurryDumps.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.UpdatePropertiesData (ARootNode : IXMLNode): WordBool;
const OPNAME = 'TMineModule.UpdatePropertiesData';
var
  LActive                : String;
  LLatitude              : Double;
  LLongitude             : Double;
  LMineName              : String;
  LVersionNo             : Integer;
  LMAP                   : Double;
  LRainfallFileName      : String;
  LRunOffModuleNo        : Integer;
  LOutflowRouteNoToRiver : Integer;
  LOutflowRouteToPCD     : Integer;
  LPlantArea             : Double;
  LPlantAreaRunOffFactor : Double;
  LSaltBuildUpRate       : Double;
  LSaltWashOffFactor     : Double;
  LInitialSaltStore      : Double;
  LDataNode              : IXMLNode;
begin
  Result := TRUE;
  try
    LDataNode := ARootNode.ChildNodes['MineProperties'];
    LActive                := LDataNode.ChildNodes['Active'].Text;
    LLatitude              := StrToFloat(LDataNode.ChildNodes['Latitude'].Text);
    LLongitude             := StrToFloat(LDataNode.ChildNodes['Longitude'].Text);
    LMineName              := LDataNode.ChildNodes['MineName'].Text;
    LVersionNo             := StrToInt(LDataNode.ChildNodes['VersionNo'].Text);
    LMAP                   := StrToFloat(LDataNode.ChildNodes['MAP'].Text);
    LRainfallFileName      := LDataNode.ChildNodes['RainfallFileName'].Text;
    LRunOffModuleNo        := StrToInt(LDataNode.ChildNodes['RunOffModuleNo'].Text);
    LOutflowRouteNoToRiver := StrToInt(LDataNode.ChildNodes['OutflowRouteNoToRiver'].Text);
    LOutflowRouteToPCD     := StrToInt(LDataNode.ChildNodes['OutflowRouteNoToPCD'].Text);
    LPlantArea             := StrToFloat(LDataNode.ChildNodes['PlantArea'].Text);
    LPlantAreaRunOffFactor := StrToFloat(LDataNode.ChildNodes['PlantAreaRunOffFactor'].Text);
    LSaltBuildUpRate       := StrToFloat(LDataNode.ChildNodes['SaltBuildUpRate'].Text);
    LSaltWashOffFactor     := StrToFloat(LDataNode.ChildNodes['SaltWashOffFactor'].Text);
    LInitialSaltStore      := StrToFloat(LDataNode.ChildNodes['InitialSaltStore'].Text);

    Result := GMineDBManager.UpdateMinePropertiesDataInDB
                               (FModuleID, LActive, LLatitude, LLongitude, LMineName, LVersionNo,
                                LMAP, LRainfallFileName, LRunOffModuleNo, LOutflowRouteNoToRiver,
                                LOutflowRouteToPCD, LPlantArea, LPlantAreaRunOffFactor, LSaltBuildUpRate,
                                LSaltWashOffFactor, LInitialSaltStore);

    if (Result) then
    begin
      FLatitude              := LLatitude;
      FLongitude             := LLongitude;
      FActive                := LActive;
      FMineName              := LMineName;
      FVersionNo             := LVersionNo;
      FMAP                   := LMAP;
      FRainfallFileName      := LRainfallFileName;
      FRunOffModuleNo        := LRunOffModuleNo;
      FOutflowRouteNoToRiver := LOutflowRouteNoToRiver;
      FOutflowRouteNoToPCD   := LOutflowRouteToPCD;
      FPlantArea             := LPlantArea;
      FPlantAreaRunOffFactor := LPlantAreaRunOffFactor;
      FSaltBuildUpRate       := LSaltBuildUpRate;
      FSaltWashOffFactor     := LSaltWashOffFactor;
      FInitialSaltStore      := LInitialSaltStore;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.UpdatePlantAreaData (ARootNode : IXMLNode): WordBool;
const OPNAME = 'TMineModule.UpdatePlantAreaData';
var
  LMinePlantAreaNode : IXMLNode;
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LYearList          : TStringList;
  LGrowthList        : TStringList;
  LInterpolationType : Integer;
begin
  Result := TRUE;
  try
    LMinePlantAreaNode := ARootNode.ChildNodes['MinePlantArea'];
    LInterpolationType := StrToInt(LMinePlantAreaNode.ChildNodes['PlantAreaInterpolationType'].Text);

    LYearList   := TStringList.Create;
    LGrowthList := TStringList.Create;
    try
      LListNode := LMinePlantAreaNode.ChildNodes['DataList'];

      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex-1);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LGrowthList.Add(LDataNode.ChildNodes['Factor'].Text);
      end;

      Result := GMineDBManager.UpdateMinePlantAreaDataInDB(FModuleID, LInterpolationType, LYearList, LGrowthList);

      if (Result) then
      begin
        FPlantAreaGrowthInterpolationType := LInterpolationType;
        Result := GMineDBManager.LoadMinePlantAreaGrowthDataFromDB(Self);
      end;
    finally
      LYearList.Free;
      LGrowthList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TMineModule.ClearPlantAreaGrowthData;
const OPNAME = 'TMineModule.ClearPlantAreaGrowthData';
begin
  try
    while (FPlantAreaGrowthData.Count > 0) do
      FPlantAreaGrowthData.Delete(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.UpdateSections (ARootNode : IXMLNode) : WordBool;
const OPNAME = 'TMineModule.UpdateSections';
var
  LSectionNode : IXMLNode;
  LAction      : String;
  LSectionType : String;
  LSectionNo   : Integer;
  LOpencastPit : IOpencastPit;
  LUnderground : IUndergroundSection;
  LSlurryDump  : ISlurryDump;
begin
  Result := FALSE;
  try
    LSectionNode := ARootNode.ChildNodes['MineSections'];
    LAction      := LSectionNode.ChildNodes['Action'].Text;
    LSectionType := LSectionNode.ChildNodes['SectionType'].Text;
    LSectionNo   := StrToInt(LSectionNode.ChildNodes['SectionNo'].Text);
    if (LAction = 'Add') then
    begin
      if (LSectionType = 'Opencast') then
        LOpencastPit := CreateNewOpencastPit
      else if (LSectionType = 'Underground') then
        LUnderground := CreateNewUndergroundSection
      else if (LSectionType = 'SlurryDump') then
        LSlurryDump := CreateNewSlurryDump;
    end
    else
    if (LAction = 'Remove') then
    begin
      if (LSectionType = 'Opencast') then
        RemoveOpencastPit(LSectionNo)
      else if (LSectionType = 'Underground') then
        RemoveUndergroundSection(LSectionNo)
      else if (LSectionType = 'SlurryDump') then
        RemoveSlurryDump(LSectionNo);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineModule.CreateNewOpencastPit : IOpencastPit;
const OPNAME = 'TMineModule.CreateNewOpencastPit';
begin
  Result := nil;
  try
    Result := GMineDBManager.CreateNewOpencastPitInDB(Self);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModule.RemoveOpencastPit (ASectionNo : Integer) : WordBool;
const OPNAME = 'TMineModule.RemoveOpencastPit';
var
  LOpencastPit  : TOpencastPit;
  LIndex        : Integer;
begin
  Result := FALSE;
  try
    LOpencastPit := FindOpencastPitBySectionNo(ASectionNo);
    if (LOpencastPit <> nil) then
    begin
      if (GMineDBManager.RemoveOpencastPitFromDB(FModuleID, LOpencastPit.SectionNo)) then
      begin
        LIndex := FOpencastPits.IndexOf(LOpencastPit);
        FOpencastPits.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TMineModuleAgent *******************************************************}

function TMineModuleAgent._AddRef: Integer;
const OPNAME = 'TMineModuleAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent._Release: Integer;
const OPNAME = 'TMineModuleAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineModuleAgent.CreateMemberObjects;
const OPNAME = 'TMineModuleAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GMineDBManager := TMineDBManager.Create;
    GMineDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineModuleAgent.DestroyMemberObjects;
const OPNAME = 'TMineModuleAgent.DestroyMemberObjects';
begin
  try
    GMineDBManager.ModuleAgent := nil;
    FreeAndNil(GMineDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.AddMineModule : TMineModule;
const OPNAME = 'TMineModuleAgent.AddMineModule';
var
  LMineModule : TMineModule;
begin
  Result := nil;
  try
    LMineModule := TMineModule.Create;
    FList.Add(LMineModule);
    Result := LMineModule;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.CreateNewMineModule (ANetworkID: Integer): IMineModule;
const OPNAME = 'TMineModuleAgent.CreateNewMineModule';
begin
  Result := nil;
  try
    Result := GMineDBManager.CreateNewMineModuleInDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.RemoveMineModule(AModuleNumber: Integer): WordBool;
const OPNAME = 'TMineModuleAgent.RemoveMineModule';
var
  LMineModule : TMineModule;
  LIndex      : Integer;
begin
  Result := FALSE;
  try
    LMineModule := FindMineModuleByNumber(AModuleNumber);
    if (LMineModule <> nil) then
    begin
      if (GMineDBManager.RemoveMineModuleFromDB(LMineModule.ModuleID)) then
      begin
        Result := TRUE;
        LIndex := FList.IndexOf(LMineModule);
        FList.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.Get_MineModuleCount: Integer;
const OPNAME = 'TMineModuleAgent.Get_MineModuleCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.FindMineModuleByID (AModuleID: Integer): TMineModule;
const OPNAME = 'TMineModuleAgent.FindMineModuleByID';
var
  LMineModule : TMineModule;
  LIndex      : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LMineModule := TMineModule(FList.Items[LIndex]);
      if (LMineModule.ModuleID = AModuleID) then
        Result := LMineModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.Get_MineModuleByID (AModuleID: Integer): IMineModule;
const OPNAME = 'TMineModuleAgent.Get_MineModuleByID';
var
  LMineModule : TMineModule;
  LIndex      : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LMineModule := TMineModule(FList.Items[LIndex]);
      if (LMineModule.ModuleID = AModuleID) then
        Result := LMineModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.FindMineModuleByNumber (AModuleNumber : Integer): TMineModule;
const OPNAME = 'TMineModuleAgent.FindMineModuleByNumber';
var
  LMineModule : TMineModule;
  LIndex      : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LMineModule := TMineModule(FList.Items[LIndex]);
      if (LMineModule.ModuleNumber = AModuleNumber) then
        Result := LMineModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.Get_MineModuleByNumber (AModuleNumber : Integer): IMineModule;
const OPNAME = 'TMineModuleAgent.Get_MineModuleByNumber';
var
  LMineModule : TMineModule;
  LIndex      : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LMineModule := TMineModule(FList.Items[LIndex]);
      if (LMineModule.ModuleNumber = AModuleNumber) then
        Result := LMineModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.Get_MineModuleByIndex(AIndex: Integer): IMineModule;
const OPNAME = 'TMineModuleAgent.Get_MineModuleByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TMineModule(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineModuleAgent.LoadMineModules (ANetworkID : Integer) : Boolean;
const OPNAME = 'TMineModuleAgent.LoadMineModules';
begin
  Result := FALSE;
  try
    Result := GMineDBManager.LoadMineModulesFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
