(******************************************************************************)
(*  Contains : Class TRunOffModule.
(******************************************************************************)
unit URunOffModule;


interface

uses
  Classes,
  Contnrs,
  XMLIntf,

  UModule,
  UAbstractObject,
  HydrologyCom_TLB;

type

  TRunOffSamiModel = class(TAbstractObject, IRunOffSamiModel)
  protected
    FAquiferThickness               : double;
    FStorativity                    : double;
    FInitialAquiferStorage          : double;
    FStaticWaterLevel               : double;
    FUnsaturatedStorage             : double;
    FInitialUnsaturatedZoneStorage  : double;
    FPerculationPower               : double;
    FGPOW                           : double;
    FMaxDischarge                   : double;
    FInteractionCurvePower          : double;
    FHGSL                           : double;
    FHGGW                           : double;
    FMaxHydrologicalGradient        : double;
    FTransmissivity                 : double;
    FBoreholeDistanceToRiver        : double;
    FGroundWaterEvaporationArea     : double;
    FK2                             : double;
    FK3                             : double;
    FInterflowLag                   : double;
    FRechargeAveragedNoMonths       : double;
    FUseAbstractions                : integer;
    FPOW                            : double;
    FSL                             : integer;
    FST                             : integer;
    FFT                             : double;
    FGW                             : double;
    FZMIN                           : integer;
    FZMAX                           : integer;
    FPI                             : double;
    FTL                             : double;
    FGL                             : double;
    FR                              : double;
    FFF                             : double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_AquiferThickness: Double; safecall;
    procedure Set_AquiferThickness(Value: Double); safecall;
    function Get_Storativity: Double; safecall;
    procedure Set_Storativity(Value: Double); safecall;
    function Get_InitialAquiferStorage: Double; safecall;
    procedure Set_InitialAquiferStorage(Value: Double); safecall;
    function Get_StaticWaterLevel: Double; safecall;
    procedure Set_StaticWaterLevel(Value: Double); safecall;
    function Get_UnsaturatedStorage: Double; safecall;
    procedure Set_UnsaturatedStorage(Value: Double); safecall;
    function Get_InitialUnsaturatedZoneStorage: Double; safecall;
    procedure Set_InitialUnsaturatedZoneStorage(Value: Double); safecall;
    function Get_PerculationPower: Double; safecall;
    procedure Set_PerculationPower(Value: Double); safecall;
    function Get_GPOW: Double; safecall;
    procedure Set_GPOW(Value: Double); safecall;
    function Get_MaxDischarge: Double; safecall;
    procedure Set_MaxDischarge(Value: Double); safecall;
    function Get_InteractionCurvePower: Double; safecall;
    procedure Set_InteractionCurvePower(Value: Double); safecall;
    function Get_HGSL: Double; safecall;
    procedure Set_HGSL(Value: Double); safecall;
    function Get_HGGW: Double; safecall;
    procedure Set_HGGW(Value: Double); safecall;
    function Get_MaxHydrologicalGradient: Double; safecall;
    procedure Set_MaxHydrologicalGradient(Value: Double); safecall;
    function Get_Transmissivity: Double; safecall;
    procedure Set_Transmissivity(Value: Double); safecall;
    function Get_BoreholeDistanceToRiver: Double; safecall;
    procedure Set_BoreholeDistanceToRiver(Value: Double); safecall;
    function Get_GroundWaterEvaporationArea: Double; safecall;
    procedure Set_GroundWaterEvaporationArea(Value: Double); safecall;
    function Get_K2: Double; safecall;
    procedure Set_K2(Value: Double); safecall;
    function Get_K3: Double; safecall;
    procedure Set_K3(Value: Double); safecall;
    function Get_InterflowLag: Double; safecall;
    procedure Set_InterflowLag(Value: Double); safecall;
    function Get_RechargeAveragedNoMonths: Double; safecall;
    procedure Set_RechargeAveragedNoMonths(Value: Double); safecall;
    function Get_UseAbstractions: Integer; safecall;
    procedure Set_UseAbstractions(Value: Integer); safecall;
    function Get_POW: Double; safecall;
    procedure Set_POW(Value: Double); safecall;
    function Get_SL: Integer; safecall;
    procedure Set_SL(Value: Integer); safecall;
    function Get_ST: Integer; safecall;
    procedure Set_ST(Value: Integer); safecall;
    function Get_FT: Double; safecall;
    procedure Set_FT(Value: Double); safecall;
    function Get_GW: Double; safecall;
    procedure Set_GW(Value: Double); safecall;
    function Get_ZMIN: Integer; safecall;
    procedure Set_ZMIN(Value: Integer); safecall;
    function Get_ZMAX: Integer; safecall;
    procedure Set_ZMAX(Value: Integer); safecall;
    function Get_PI: Double; safecall;
    procedure Set_PI(Value: Double); safecall;
    function Get_TL: Double; safecall;
    procedure Set_TL(Value: Double); safecall;
    function Get_GL: Double; safecall;
    procedure Set_GL(Value: Double); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_FF: Double; safecall;
    procedure Set_FF(Value: Double); safecall;
    procedure Populate (AAquiferThickness              : Double;
                        AStorativity                   : Double;
                        AInitialAquiferStorage         : Double;
                        AStaticWaterLevel              : Double;
                        AUnsaturatedStorage            : Double;
                        AInitialUnsaturatedZoneStorage : Double;
                        APerculationPower              : Double;
                        AGPOW                          : Double;
                        AMaxDischarge                  : Double;
                        AInteractionCurvePower         : Double;
                        AHGSL                          : Double;
                        AHGGW                          : Double;
                        AMaxHydrologicalGradient       : Double;
                        ATransmissivity                : Double;
                        ABoreholeDistanceToRiver       : Double;
                        AGroundWaterEvaporationArea    : Double;
                        AK2                            : Double;
                        AK3                            : Double;
                        AInterflowLag                  : Double;
                        ARechargeAveragedNoMonths      : Double;
                        AUseAbstractions               : Integer;
                        APOW                           : Double;
                        ASL                            : Integer;
                        AST                            : Integer;
                        AFT                            : Double;
                        AGW                            : Double;
                        AZMIN                          : Integer;
                        AZMAX                          : Integer;
                        API                            : Double;
                        ATL                            : Double;
                        AGL                            : Double;
                        AR                             : Double;
                        AFF                            : Double); safecall;
    property AquiferThickness: Double read Get_AquiferThickness write Set_AquiferThickness;
    property Storativity: Double read Get_Storativity write Set_Storativity;
    property InitialAquiferStorage: Double read Get_InitialAquiferStorage write Set_InitialAquiferStorage;
    property StaticWaterLevel: Double read Get_StaticWaterLevel write Set_StaticWaterLevel;
    property UnsaturatedStorage: Double read Get_UnsaturatedStorage write Set_UnsaturatedStorage;
    property InitialUnsaturatedZoneStorage: Double read Get_InitialUnsaturatedZoneStorage write Set_InitialUnsaturatedZoneStorage;
    property PerculationPower: Double read Get_PerculationPower write Set_PerculationPower;
    property GPOW: Double read Get_GPOW write Set_GPOW;
    property MaxDischarge: Double read Get_MaxDischarge write Set_MaxDischarge;
    property InteractionCurvePower: Double read Get_InteractionCurvePower write Set_InteractionCurvePower;
    property HGSL: Double read Get_HGSL write Set_HGSL;
    property HGGW: Double read Get_HGGW write Set_HGGW;
    property MaxHydrologicalGradient: Double read Get_MaxHydrologicalGradient write Set_MaxHydrologicalGradient;
    property Transmissivity: Double read Get_Transmissivity write Set_Transmissivity;
    property BoreholeDistanceToRiver: Double read Get_BoreholeDistanceToRiver write Set_BoreholeDistanceToRiver;
    property GroundWaterEvaporationArea: Double read Get_GroundWaterEvaporationArea write Set_GroundWaterEvaporationArea;
    property K2: Double read Get_K2 write Set_K2;
    property K3: Double read Get_K3 write Set_K3;
    property InterflowLag: Double read Get_InterflowLag write Set_InterflowLag;
    property RechargeAveragedNoMonths: Double read Get_RechargeAveragedNoMonths write Set_RechargeAveragedNoMonths;
    property UseAbstractions: Integer read Get_UseAbstractions write Set_UseAbstractions;
    property POW: Double read Get_POW write Set_POW;
    property SL: Integer read Get_SL write Set_SL;
    property ST: Integer read Get_ST write Set_ST;
    property FT: Double read Get_FT write Set_FT;
    property GW: Double read Get_GW write Set_GW;
    property ZMIN: Integer read Get_ZMIN write Set_ZMIN;
    property ZMAX: Integer read Get_ZMAX write Set_ZMAX;
    property PI: Double read Get_PI write Set_PI;
    property TL: Double read Get_TL write Set_TL;
    property GL: Double read Get_GL write Set_GL;
    property R: Double read Get_R write Set_R;
    property FF: Double read Get_FF write Set_FF;
  end;

  TRunOffHughesModel = class(TAbstractObject, IRunOffHughesModel)
  protected
    FInflowRouteNo                  : Integer;
    FInfluenceROMNo                 : Integer;
    FGroundWaterModel               : Integer;
    FHGSL                           : Double;
    FGPOW                           : Double;
    FTLGMax                         : Double;
    FHGGW                           : Double;
    FPOW                            : Double;
    FSL                             : Integer;
    FST                             : Integer;
    FFT                             : Double;
    FGW                             : Double;
    FZMIN                           : Integer;
    FZMAX                           : Integer;
    FPI                             : Double;
    FTL                             : Double;
    FGL                             : Double;
    FR                              : Double;
    FFF                             : Double;
    FUseNoOfReaches                 : Integer;
    FDrainageDensity                : Double;
    FNoOfReaches                    : Integer;
    FRiparianAreaWidthPercentage    : Double;
    FRiparianStripFactor            : Double;
    FRestWaterLevel                 : Double;
    FTransmissivity                 : Double;
    FStorativity                    : Double;
    FGroundWaterSlope               : Double;
    FAnnualUpperZoneAbstraction     : Double;
    FMonthlyUpperZoneAbstraction    : TMonthlyDoubleArray;
    FAnnualRiparianZoneAbstraction  : Double;
    FMonthlyRiparianZoneAbstraction : TMonthlyDoubleArray;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_InflowRouteNo: Integer; safecall;
    procedure Set_InflowRouteNo(Value: Integer); safecall;
    function Get_InfluenceROMNo: Integer; safecall;
    procedure Set_InfluenceROMNo(Value: Integer); safecall;
    function Get_GroundWaterModel: Integer; safecall;
    procedure Set_GroundWaterModel(Value: Integer); safecall;
    function Get_HGSL: Double; safecall;
    procedure Set_HGSL(Value: Double); safecall;
    function Get_GPOW: Double; safecall;
    procedure Set_GPOW(Value: Double); safecall;
    function Get_TLGMax: Double; safecall;
    procedure Set_TLGMax(Value: Double); safecall;
    function Get_HGGW: Double; safecall;
    procedure Set_HGGW(Value: Double); safecall;
    function Get_POW: Double; safecall;
    procedure Set_POW(Value: Double); safecall;
    function Get_SL: Integer; safecall;
    procedure Set_SL(Value: Integer); safecall;
    function Get_ST: Integer; safecall;
    procedure Set_ST(Value: Integer); safecall;
    function Get_FT: Double; safecall;
    procedure Set_FT(Value: Double); safecall;
    function Get_GW: Double; safecall;
    procedure Set_GW(Value: Double); safecall;
    function Get_ZMIN: Integer; safecall;
    procedure Set_ZMIN(Value: Integer); safecall;
    function Get_ZMAX: Integer; safecall;
    procedure Set_ZMAX(Value: Integer); safecall;
    function Get_PI: Double; safecall;
    procedure Set_PI(Value: Double); safecall;
    function Get_TL: Double; safecall;
    procedure Set_TL(Value: Double); safecall;
    function Get_GL: Double; safecall;
    procedure Set_GL(Value: Double); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_FF: Double; safecall;
    procedure Set_FF(Value: Double); safecall;
    function Get_UseNoOfReaches: Integer; safecall;
    procedure Set_UseNoOfReaches(Value: Integer); safecall;
    function Get_DrainageDensity: Double; safecall;
    procedure Set_DrainageDensity(Value: Double); safecall;
    function Get_NoOfReaches: Integer; safecall;
    procedure Set_NoOfReaches(Value: Integer); safecall;
    function Get_RiparianAreaWidthPercentage: Double; safecall;
    procedure Set_RiparianAreaWidthPercentage(Value: Double); safecall;
    function Get_RiparianStripFactor: Double; safecall;
    procedure Set_RiparianStripFactor(Value: Double); safecall;
    function Get_RestWaterLevel: Double; safecall;
    procedure Set_RestWaterLevel(Value: Double); safecall;
    function Get_Transmissivity: Double; safecall;
    procedure Set_Transmissivity(Value: Double); safecall;
    function Get_Storativity: Double; safecall;
    procedure Set_Storativity(Value: Double); safecall;
    function Get_GroundWaterSlope: Double; safecall;
    procedure Set_GroundWaterSlope(Value: Double); safecall;
    function Get_AnnualUpperZoneAbstraction: Double; safecall;
    procedure Set_AnnualUpperZoneAbstraction(Value: Double); safecall;
    function Get_MonthlyUpperZoneAbstraction(AMonthIndex: Integer): Double; safecall;
    procedure Set_MonthlyUpperZoneAbstraction(AMonthIndex: Integer; Value: Double); safecall;
    function Get_AnnualRiparianZoneAbstraction: Double; safecall;
    procedure Set_AnnualRiparianZoneAbstraction(Value: Double); safecall;
    function Get_MonthlyRiparianZoneAbstraction(AMonthIndex: Integer): Double; safecall;
    procedure Set_MonthlyRiparianZoneAbstraction(AMonthIndex: Integer; Value: Double); safecall;
    function Populate(AInflowRouteNo               : Integer;  AInfluenceROMNo      : Integer;
                      AGroundWaterModel            : Integer;  AHGSL                : Double;
                      AGPOW                        : Double;   ATLGMax              : Double;
                      AHGGW                        : Double;   APOW                 : Double;
                      ASL                          : Integer;  AST                  : Integer;
                      AFT                          : Double;   AGW                  : Double;
                      AZMIN                        : Integer;  AZMAX                : Integer;
                      API                          : Double;   ATL                  : Double;
                      AGL                          : Double;   AR                   : Double;
                      AFF                          : Double;   AUseNoOfReaches      : Integer;
                      ADrainageDensity             : Double;   ANoOfReaches         : Integer;
                      ARiparianAreaWidthPercentage : Double;   ARiparianStripFactor : Double;
                      ARestWaterLevel              : Double;   ATransmissivity      : Double;
                      AStorativity                 : Double;   AGroundWaterSlope    : Double;
                      AAnnualUpperZoneAbstraction           : Double;
                      const AMonthlyUpperZoneAbstraction    : WideString;
                      AAnnualRiparianZoneAbstraction        : Double;
                      const AMonthlyRiparionZoneAbstraction : WideString): WordBool; safecall;
    property InflowRouteNo: Integer read Get_InflowRouteNo write Set_InflowRouteNo;
    property InfluenceROMNo: Integer read Get_InfluenceROMNo write Set_InfluenceROMNo;
    property GroundWaterModel: Integer read Get_GroundWaterModel write Set_GroundWaterModel;
    property HGSL: Double read Get_HGSL write Set_HGSL;
    property GPOW: Double read Get_GPOW write Set_GPOW;
    property TLGMax: Double read Get_TLGMax write Set_TLGMax;
    property HGGW: Double read Get_HGGW write Set_HGGW;
    property POW: Double read Get_POW write Set_POW;
    property SL: Integer read Get_SL write Set_SL;
    property ST: Integer read Get_ST write Set_ST;
    property FT: Double read Get_FT write Set_FT;
    property GW: Double read Get_GW write Set_GW;
    property ZMIN: Integer read Get_ZMIN write Set_ZMIN;
    property ZMAX: Integer read Get_ZMAX write Set_ZMAX;
    property PI: Double read Get_PI write Set_PI;
    property TL: Double read Get_TL write Set_TL;
    property GL: Double read Get_GL write Set_GL;
    property R: Double read Get_R write Set_R;
    property FF: Double read Get_FF write Set_FF;
    property UseNoOfReaches: Integer read Get_UseNoOfReaches write Set_UseNoOfReaches;
    property DrainageDensity: Double read Get_DrainageDensity write Set_DrainageDensity;
    property NoOfReaches: Integer read Get_NoOfReaches write Set_NoOfReaches;
    property RiparianAreaWidthPercentage: Double read Get_RiparianAreaWidthPercentage write Set_RiparianAreaWidthPercentage;
    property RiparianStripFactor: Double read Get_RiparianStripFactor write Set_RiparianStripFactor;
    property RestWaterLevel: Double read Get_RestWaterLevel write Set_RestWaterLevel;
    property Transmissivity: Double read Get_Transmissivity write Set_Transmissivity;
    property Storativity: Double read Get_Storativity write Set_Storativity;
    property GroundWaterSlope: Double read Get_GroundWaterSlope write Set_GroundWaterSlope;
    property AnnualUpperZoneAbstraction: Double read Get_AnnualUpperZoneAbstraction write Set_AnnualUpperZoneAbstraction;
    property MonthlyUpperZoneAbstraction[AMonthIndex: Integer]: Double read Get_MonthlyUpperZoneAbstraction write Set_MonthlyUpperZoneAbstraction;
    property AnnualRiparianZoneAbstraction: Double read Get_AnnualRiparianZoneAbstraction write Set_AnnualRiparianZoneAbstraction;
    property MonthlyRiparianZoneAbstraction[AMonthIndex: Integer]: Double read Get_MonthlyRiparianZoneAbstraction write Set_MonthlyRiparianZoneAbstraction;
  end;

  TRunOffPitmanModel = class(TAbstractObject, IRunOffPitmanModel)
  protected
    FPOW            : Double;
    FSL             : Integer;
    FST             : Integer;
    FFT             : Double;
    FGW             : Double;
    FZMIN           : Integer;
    FZMAX           : Integer;
    FPI             : Double;
    FTL             : Double;
    FGL             : Double;
    FR              : Double;
    FFF             : Double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_POW: Double; safecall;
    procedure Set_POW(Value: Double); safecall;
    function Get_SL: Integer; safecall;
    procedure Set_SL(Value: Integer); safecall;
    function Get_ST: Integer; safecall;
    procedure Set_ST(Value: Integer); safecall;
    function Get_FT: Double; safecall;
    procedure Set_FT(Value: Double); safecall;
    function Get_GW: Double; safecall;
    procedure Set_GW(Value: Double); safecall;
    function Get_ZMIN: Integer; safecall;
    procedure Set_ZMIN(Value: Integer); safecall;
    function Get_ZMAX: Integer; safecall;
    procedure Set_ZMAX(Value: Integer); safecall;
    function Get_PI: Double; safecall;
    procedure Set_PI(Value: Double); safecall;
    function Get_TL: Double; safecall;
    procedure Set_TL(Value: Double); safecall;
    function Get_GL: Double; safecall;
    procedure Set_GL(Value: Double); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_FF: Double; safecall;
    procedure Set_FF(Value: Double); safecall;
    function Populate(APOW  : Double;
                      ASL   : Integer;
                      AST   : Integer;
                      AFT   : Double;
                      AGW   : Double;
                      AZMIN : Integer;
                      AZMAX : Integer;
                      API   : Double;
                      ATL   : Double;
                      AGL   : Double;
                      AR    : Double;
                      AFF   : Double): WordBool; safecall;
    property POW  : Double  read Get_POW  write Set_POW;
    property SL   : Integer read Get_SL   write Set_SL;
    property ST   : Integer read Get_ST   write Set_ST;
    property FT   : Double  read Get_FT   write Set_FT;
    property GW   : Double  read Get_GW   write Set_GW;
    property ZMIN : Integer read Get_ZMIN write Set_ZMIN;
    property ZMAX : Integer read Get_ZMAX write Set_ZMAX;
    property PI   : Double  read Get_PI   write Set_PI;
    property TL   : Double  read Get_TL   write Set_TL;
    property GL   : Double  read Get_GL   write Set_GL;
    property R    : Double  read Get_R    write Set_R;
    property FF   : Double  read Get_FF   write Set_FF;
  end;

  TRunOffAfforestation = class(TAbstractObject, IRunOffAfforestation)
  protected
    FAlgorithm                : Integer;
    FPineAreaPercentage       : Double;
    FPineRotationPeriod       : Integer;
    FEucalyptusAreaPercentage : Double;
    FEucalyptusRotationPeriod : Integer;
    FWattleAreaPercentage     : Double;
    FWattleRotationPeriod     : Integer;
    FOptimalAreaPercentage    : Double;
    FSFRReductionMAR          : Double;
    FSFRReductionLowFlows     : Double;
    FYearlyData               : TList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_PineAreaPercentage: Double; safecall;
    procedure Set_PineAreaPercentage(Value: Double); safecall;
    function Get_PineRotationPeriod: Integer; safecall;
    procedure Set_PineRotationPeriod(Value: Integer); safecall;
    function Get_EucalyptusAreaPercentage: Double; safecall;
    procedure Set_EucalyptusAreaPercentage(Value: Double); safecall;
    function Get_EucalyptusRotationPeriod: Integer; safecall;
    procedure Set_EucalyptusRotationPeriod(Value: Integer); safecall;
    function Get_WattleAreaPercentage: Double; safecall;
    procedure Set_WattleAreaPercentage(Value: Double); safecall;
    function Get_WattleRotationPeriod: Integer; safecall;
    procedure Set_WattleRotationPeriod(Value: Integer); safecall;
    function Get_OptimalAreaPercentage: Double; safecall;
    procedure Set_OptimalAreaPercentage(Value: Double); safecall;
    function Get_SFRReductionMAR: Double; safecall;
    procedure Set_SFRReductionMAR(Value: Double); safecall;
    function Get_SFRReductionLowFlows: Double; safecall;
    procedure Set_SFRReductionLowFlows(Value: Double); safecall;
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_Area(AYearIndex: Integer): Double; safecall;
    procedure Set_Area(AYearIndex: Integer; Value: Double); safecall;
    function Populate(AAlgorithm                : Integer;
                      APineAreaPercentage       : Double;
                      APineRotationPeriod       : Integer;
                      AEucalyptusAreaPercentage : Double;
                      AEucalyptusRotationPeriod : Integer;
                      AWattleAreaPercentage     : Double;
                      AWattleRotationPeriod     : Integer;
                      AOptimalAreaPercentage    : Double;
                      ASFRReductionMAR          : Double;
                      ASFRReductionLowFlows     : Double): WordBool; safecall;
    procedure AddAreaData(AYear: Integer;
                          AArea: Double); safecall;
    property Algorithm: Integer read Get_Algorithm write Set_Algorithm;
    property PineAreaPercentage: Double read Get_PineAreaPercentage write Set_PineAreaPercentage;
    property PineRotationPeriod: Integer read Get_PineRotationPeriod write Set_PineRotationPeriod;
    property EucalyptusAreaPercentage: Double read Get_EucalyptusAreaPercentage write Set_EucalyptusAreaPercentage;
    property EucalyptusRotationPeriod: Integer read Get_EucalyptusRotationPeriod write Set_EucalyptusRotationPeriod;
    property WattleAreaPercentage: Double read Get_WattleAreaPercentage write Set_WattleAreaPercentage;
    property WattleRotationPeriod: Integer read Get_WattleRotationPeriod write Set_WattleRotationPeriod;
    property OptimalAreaPercentage: Double read Get_OptimalAreaPercentage write Set_OptimalAreaPercentage;
    property SFRReductionMAR: Double read Get_SFRReductionMAR write Set_SFRReductionMAR;
    property SFRReductionLowFlows: Double read Get_SFRReductionLowFlows write Set_SFRReductionLowFlows;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property Area[AYearIndex: Integer]: Double read Get_Area write Set_Area;
  end;

  TRunOffAlienVegetation = class(TAbstractObject, IRunOffAlienVegetation)
  protected
    FAlgorithm                : Integer;
    FRiparianVegetationArea   : Double;
    FTallTreeAreaPercentage   : Double;
    FTallTreeAge              : Double;
    FMediumTreeAreaPercentage : Double;
    FMediumTreeAge            : Double;
    FTallSchrubAreaPercentage : Double;
    FTallSchrubAge            : Double;
    FOptimalAreaPercentage    : Double;
    FYearlyData               : TList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_RiparianVegetationArea: Double; safecall;
    procedure Set_RiparianVegetationArea(Value: Double); safecall;
    function Get_TallTreeAreaPercentage: Double; safecall;
    procedure Set_TallTreeAreaPercentage(Value: Double); safecall;
    function Get_TallTreeAge: Double; safecall;
    procedure Set_TallTreeAge(Value: Double); safecall;
    function Get_MediumTreeAreaPercentage: Double; safecall;
    procedure Set_MediumTreeAreaPercentage(Value: Double); safecall;
    function Get_MediumTreeAge: Double; safecall;
    procedure Set_MediumTreeAge(Value: Double); safecall;
    function Get_TallSchrubAreaPercentage: Double; safecall;
    procedure Set_TallSchrubAreaPercentage(Value: Double); safecall;
    function Get_TallSchrubAge: Double; safecall;
    procedure Set_TallSchrubAge(Value: Double); safecall;
    function Get_OptimalAreaPercentage: Double; safecall;
    procedure Set_OptimalAreaPercentage(Value: Double); safecall;
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_Area(AYearIndex: Integer): Double; safecall;
    procedure Set_Area(AYearIndex: Integer; Value: Double); safecall;
    function Populate(AAlgorithm                : Integer;
                      ARiparianVegetationArea   : Double;
                      ATallTreeAreaPercentage   : Double;
                      ATallTreeAge              : Double;
                      AMediumTreeAreaPercentage : Double;
                      AMediumTreeAge            : Double;
                      ATallSchrubAreaPercentage : Double;
                      ATallSchrubAge            : Double;
                      AOptimalAreaPercentage    : Double): WordBool; safecall;
    procedure AddAreaData(AYear: Integer; AArea: Double); safecall;
    property Algorithm: Integer read Get_Algorithm write Set_Algorithm;
    property RiparianVegetationArea: Double read Get_RiparianVegetationArea write Set_RiparianVegetationArea;
    property TallTreeAreaPercentage: Double read Get_TallTreeAreaPercentage write Set_TallTreeAreaPercentage;
    property TallTreeAge: Double read Get_TallTreeAge write Set_TallTreeAge;
    property MediumTreeAreaPercentage: Double read Get_MediumTreeAreaPercentage write Set_MediumTreeAreaPercentage;
    property MediumTreeAge: Double read Get_MediumTreeAge write Set_MediumTreeAge;
    property TallSchrubAreaPercentage: Double read Get_TallSchrubAreaPercentage write Set_TallSchrubAreaPercentage;
    property TallSchrubAge: Double read Get_TallSchrubAge write Set_TallSchrubAge;
    property OptimalAreaPercentage: Double read Get_OptimalAreaPercentage write Set_OptimalAreaPercentage;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property Area[AYearIndex: Integer]: Double read Get_Area write Set_Area;
  end;

  TRunOffPavedArea = class(TAbstractObject, IRunOffPavedArea)
  protected
    FYearlyData  : TList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_Proportion(AYearIndex: Integer): Double; safecall;
    procedure Set_Proportion(AYearIndex: Integer; Value: Double); safecall;
    procedure AddAreaData(AYear: Integer; AProportion: Double); safecall;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property Proportion[AYearIndex: Integer]: Double read Get_Proportion write Set_Proportion;
  end;

  TRunOffGroundWaterAbstraction = class(TAbstractObject, IRunOffGroundWaterAbstraction)
  protected
    FYearlyData  : TList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_AbstractionValue(AYearIndex: Integer): double; safecall;
    procedure Set_AbstractionValue(AYearIndex: Integer; Value: double); safecall;
    procedure AddAbstractionData(AYear: Integer; AValue: Double); safecall;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property AbstractionValue[AYearIndex: Integer]: double read Get_AbstractionValue write Set_AbstractionValue;
  end;

  TRunOffOutflowRoute = class(TAbstractObject, IRunOffOutflowRoute)
  protected
    FRouteNo           : Integer;
    FOutflowProportion : Double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_OutflowPercentage: Double; safecall;
    procedure Set_OutflowPercentage(Value: Double); safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property OutflowPercentage: Double read Get_OutflowPercentage write Set_OutflowPercentage;
  end;
  
  TRunOffModule = class(TNetworkModule, IRunOffModule)
  protected
    FRunOffName              : String;
    FVersionNo               : Integer;
    FCatchmentArea           : Double;
    FCatchmentMAP            : Double;
    FRainfallFileName        : String;
    FProduceNaturalisedFlows : Integer;
    FAPanFactor              : TMonthlyDoubleArray;
    FPitmanModel             : TRunOffPitmanModel;
    FHughesModel             : TRunOffHughesModel;
    FSamiModel               : TRunOffSamiModel;
    FAfforestation           : TRunOffAfforestation;
    FAlienVegetation         : TRunOffAlienVegetation;
    FPavedArea               : TRunOffPavedArea;
    FOutflowRoutes           : TList;
    FSlaveModuleNumbers      : TStringList;
    FGroundWaterAbstraction  : TRunOffGroundWaterAbstraction;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_RunOffName: WideString; safecall;
    procedure Set_RunOffName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_CatchmentArea: Double; safecall;
    procedure Set_CatchmentArea(Value: Double); safecall;
    function Get_CatchmentMAP: Double; safecall;
    procedure Set_CatchmentMAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_ProduceNaturalisedFlows: Integer; safecall;
    procedure Set_ProduceNaturalisedFlows(Value: Integer); safecall;
    function Get_APanFactor(AMonthIndex: Integer): Double; safecall;
    procedure Set_APanFactor(AMonthIndex: Integer; Value: Double); safecall;
    function Get_PitmanModel: IRunOffPitmanModel; safecall;
    function Get_HughesModel: IRunOffHughesModel; safecall;
    function Get_SamiModel: IRunOffSamiModel; safecall;
    function Get_Afforestation: IRunOffAfforestation; safecall;
    function Get_AlienVegetation: IRunOffAlienVegetation; safecall;
    function Get_PavedArea: IRunOffPavedArea; safecall;
    function Populate(ANetworkID                 : Integer;
                      AModuleID                  : Integer;
                      const AModuleType          : WideString;
                      AModuleNumber              : Integer;
                      ANetworkSequence           : Integer;
                      const AActive              : WideString;
                      const ARunOffName          : WideString;
                      AVersionNo                 : Integer;
                      ACatchmentArea             : Double;
                      ACatchmentMAP              : Double;
                      const ARainfallFileName    : WideString;
                      AProduceNaturalisedInflows : Integer;
                      ALongitude                 : Double;
                      ALatitude                  : Double;
                      const AAPanFactors         : WideString): WordBool; safecall;
    procedure AddOutflowRoute(ARouteNo: Integer; AOutflowPercentage: Double); safecall;
    function CreateNewOutflowRoute(ARouteNo : Integer; AOutflowPerc: Double) : Boolean;
    function RemoveOutflowRoute(ARouteNo : Integer) : Boolean;
    function DeleteOutflowRoute(ARouteNo : Integer) : Boolean;
    function Get_NoOfOutflowRoutes: Integer; safecall;
    function Get_OutflowRouteByIndex(AIndex: Integer): IRunOffOutflowRoute; safecall;
    function Get_OutflowRouteByRouteNo(ARouteNo: Integer): IRunOffOutflowRoute; safecall;
    function FindOutFlowRouteByRouteNo(ARouteNo: Integer): TRunOffOutflowRoute;
    function Get_NoOfSlaves: Integer; safecall;
    function SlaveModuleNoByIndex(AIndex: Integer): Integer; safecall;
    procedure AddSlaveModuleNo(ASlaveModuleNo: Integer); safecall;
    function Get_GroundWaterAbstraction: IRunOffGroundWaterAbstraction; safecall;
    function UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
    function UpdateSlavesData (ARootNode : IXMLNode): Boolean;
    function UpdatePitmanData (ARootNode : IXMLNode): Boolean;
    function UpdateAfforestationData (ARootNode : IXMLNode): Boolean;
    function UpdateAlienVegetationData (ARootNode : IXMLNode): Boolean;
    function UpdatePavedAreaData (ARootNode : IXMLNode): Boolean;
    function UpdateGroundWaterAbstractionData (ARootNode : IXMLNode): Boolean;
    function UpdateOutflowRoutesData (ARootNode : IXMLNode): Boolean;
    function UpdateHughesData (ARootNode : IXMLNode): Boolean;
    function UpdateSamiData (ARootNode : IXMLNode): Boolean;
    procedure ClearSlaveModuleNumbers;
    procedure ClearAfforestationAreaData;
    procedure ClearAlienVegetationAreaData;
    procedure ClearPavedAreaData;
    procedure ClearGroundwaterAbstractionData;
    procedure ClearOutFlowRoutes;
    property RunOffName: WideString read Get_RunOffName write Set_RunOffName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property CatchmentArea: Double read Get_CatchmentArea write Set_CatchmentArea;
    property CatchmentMAP: Double read Get_CatchmentMAP write Set_CatchmentMAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property ProduceNaturalisedFlows: Integer read Get_ProduceNaturalisedFlows write Set_ProduceNaturalisedFlows;
    property APanFactor[AMonthIndex: Integer]: Double read Get_APanFactor write Set_APanFactor;
    property PitmanModel: IRunOffPitmanModel read Get_PitmanModel;
    property HughesModel: IRunOffHughesModel read Get_HughesModel;
    property SamiModel: IRunOffSamiModel read Get_SamiModel;
    property Afforestation: IRunOffAfforestation read Get_Afforestation;
    property AlienVegetation: IRunOffAlienVegetation read Get_AlienVegetation;
    property PavedArea: IRunOffPavedArea read Get_PavedArea;
    property GroundwaterAbstraction : TRunOffGroundWaterAbstraction read FGroundWaterAbstraction;
    property NoOfOutflowRoutes: Integer read Get_NoOfOutflowRoutes;
    property OutflowRouteByRouteNo[ARouteNo: Integer]: IRunOffOutflowRoute read Get_OutflowRouteByRouteNo;
    property OutflowRouteByIndex[AIndex: Integer]: IRunOffOutflowRoute read Get_OutflowRouteByIndex;
  end;

  TRunOffModuleAgent = class(TModuleAgent, IRunOffModuleAgent)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function FindRunOffModuleByID(AModuleID: Integer): TRunOffModule;
    function FindRunOffModuleByNumber(AModuleNumber: Integer): TRunOffModule;
    function FindRunOffModuleByIndex(AIndex: Integer): TRunOffModule;
    function Get_RunOffModuleCount: Integer; safecall;
    function Get_RunOffModuleByID(AModuleID: Integer): IRunOffModule; safecall;
    function Get_RunOffModuleByNumber(AModuleNumber: Integer): IRunOffModule; safecall;
    function Get_RunOffModuleByIndex(AIndex: Integer): IRunOffModule; safecall;
    function LoadRunOffModules(ANetworkID: Integer): Boolean;
    function AddRunOffModule: TRunOffModule;
    function CreateNewRunOffModule (ANetworkID: Integer): IRunOffModule; safecall;
    function RemoveRunOffModule(AModuleNumber: Integer): WordBool; safecall;
    property RunOffModuleCount: Integer read Get_RunOffModuleCount;
    property RunOffModuleByID[AID: Integer]: IRunOffModule read Get_RunOffModuleByID;
    property RunOffModuleByNumber[AModuleNumber: Integer]: IRunOffModule read Get_RunOffModuleByNumber;
    property RunOffModuleByIndex[AIndex: Integer]: IRunOffModule read Get_RunOffModuleByIndex;
  end;


implementation

uses

  System.Types,
  SysUtils,
  Windows,
  VCL.Forms,
  Math,
  
  UModuleDBManager,
  URunOffDBManager,
  UErrorHandlingOperations;

{ TRunOffSamiModel ************************************************************}

function TRunOffSamiModel._AddRef: Integer;
const OPNAME = 'TRunOffSamiModel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffSamiModel._Release: Integer;
const OPNAME = 'TRunOffSamiModel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffSamiModel.Get_AquiferThickness: Double;
const OPNAME = 'TRunOffSamiModel.Get_AquiferThickness';
begin
  Result := 0.0;
  try
    Result := FAquiferThickness;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_AquiferThickness(Value: Double);
const OPNAME = 'TRunOffSamiModel.Get_AquiferThickness';
begin
  try
    FAquiferThickness := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_Storativity: Double;
const OPNAME = 'TRunOffSamiModel.Get_Storativity';
begin
  Result := 0.0;
  try
    Result := FStorativity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_Storativity(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_Storativity';
begin
  try
    FStorativity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_InitialAquiferStorage: Double;
const OPNAME = 'TRunOffSamiModel.Get_InitialAquiferStorage';
begin
  Result := 0.0;
  try
    Result := FInitialAquiferStorage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_InitialAquiferStorage(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_InitialAquiferStorage';
begin
  try
    FInitialAquiferStorage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_StaticWaterLevel: Double;
const OPNAME = 'TRunOffSamiModel.Get_StaticWaterLevel';
begin
  Result := 0.0;
  try
    Result := FStaticWaterLevel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_StaticWaterLevel(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_StaticWaterLevel';
begin
  try
    FStaticWaterLevel := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_UnsaturatedStorage: Double;
const OPNAME = 'TRunOffSamiModel.Get_UnsaturatedStorage';
begin
  Result := 0.0;
  try
    Result := FUnsaturatedStorage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_UnsaturatedStorage(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_UnsaturatedStorage';
begin
  try
    FUnsaturatedStorage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_InitialUnsaturatedZoneStorage: Double;
const OPNAME = 'TRunOffSamiModel.Get_InitialUnsaturatedZoneStorage';
begin
  Result := 0.0;
  try
    Result := FInitialUnsaturatedZoneStorage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_InitialUnsaturatedZoneStorage(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_InitialUnsaturatedZoneStorage';
begin
  try
    FInitialUnsaturatedZoneStorage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_PerculationPower: Double;
const OPNAME = 'TRunOffSamiModel.Get_PerculationPower';
begin
  Result := 0.0;
  try
    Result := FPerculationPower;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_PerculationPower(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_PerculationPower';
begin
  try
    FPerculationPower := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_GPOW: Double;
const OPNAME = 'TRunOffSamiModel.Get_GPOW';
begin
  Result := 0.0;
  try
    Result := FGPOW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_GPOW(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_GPOW';
begin
  try
    FGPOW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_MaxDischarge: Double;
const OPNAME = 'TRunOffSamiModel.Get_MaxDischarge';
begin
  Result := 0.0;
  try
    Result := FMaxDischarge;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_MaxDischarge(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_MaxDischarge';
begin
  try
    FMaxDischarge := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_InteractionCurvePower: Double;
const OPNAME = 'TRunOffSamiModel.Get_InteractionCurvePower';
begin
  Result := 0.0;
  try
    Result := FInteractionCurvePower;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_InteractionCurvePower(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_InteractionCurvePower';
begin
  try
    FInteractionCurvePower := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_HGSL: Double;
const OPNAME = 'TRunOffSamiModel.Get_HGSL';
begin
  Result := 0.0;
  try
    Result := FHGSL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_HGSL(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_HGSL';
begin
  try
    FHGSL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_HGGW: Double;
const OPNAME = 'TRunOffSamiModel.Get_HGGW';
begin
  Result := 0.0;
  try
    Result := FHGGW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_HGGW(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_HGGW';
begin
  try
    FHGGW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_MaxHydrologicalGradient: Double;
const OPNAME = 'TRunOffSamiModel.Get_MaxHydrologicalGradient';
begin
  Result := 0.0;
  try
    Result := FMaxHydrologicalGradient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_MaxHydrologicalGradient(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_MaxHydrologicalGradient';
begin
  try
    FMaxHydrologicalGradient := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_Transmissivity: Double;
const OPNAME = 'TRunOffSamiModel.Get_Transmissivity';
begin
  Result := 0.0;
  try
    Result := FTransmissivity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_Transmissivity(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_Transmissivity';
begin
  try
    FTransmissivity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_BoreholeDistanceToRiver: Double;
const OPNAME = 'TRunOffSamiModel.Get_BoreholeDistanceToRiver';
begin
  Result := 0.0;
  try
    Result := FBoreholeDistanceToRiver;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_BoreholeDistanceToRiver(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_BoreholeDistanceToRiver';
begin
  try
    FBoreholeDistanceToRiver := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_GroundWaterEvaporationArea: Double;
const OPNAME = 'TRunOffSamiModel.Get_GroundWaterEvaporationArea';
begin
  Result := 0.0;
  try
    Result := FGroundWaterEvaporationArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_GroundWaterEvaporationArea(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_GroundWaterEvaporationArea';
begin
  try
    FGroundWaterEvaporationArea := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_K2: Double;
const OPNAME = 'TRunOffSamiModel.Get_K2';
begin
  Result := 0.0;
  try
    Result := FK2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_K2(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_K2';
begin
  try
    FK2 := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_K3: Double;
const OPNAME = 'TRunOffSamiModel.Get_K3';
begin
  Result := 0.0;
  try
    Result := FK3;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_K3(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_K3';
begin
  try
    FK3 := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_InterflowLag: Double;
const OPNAME = 'TRunOffSamiModel.Get_InterflowLag';
begin
  Result := 0.0;
  try
    Result := FInterflowLag;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_InterflowLag(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_InterflowLag';
begin
  try
    FInterflowLag := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_RechargeAveragedNoMonths: Double;
const OPNAME = 'TRunOffSamiModel.Get_RechargeAveragedNoMonths';
begin
  Result := 0.0;
  try
    Result := FRechargeAveragedNoMonths;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_RechargeAveragedNoMonths(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_RechargeAveragedNoMonths';
begin
  try
    FRechargeAveragedNoMonths := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_UseAbstractions: Integer;
const OPNAME = 'TRunOffSamiModel.Get_UseAbstractions';
begin
  Result := 0;
  try
    Result := FUseAbstractions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_UseAbstractions(Value: Integer);
const OPNAME = 'TRunOffSamiModel.Set_UseAbstractions';
begin
  try
    FUseAbstractions := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_POW: Double;
const OPNAME = 'TRunOffSamiModel.Get_POW';
begin
  Result := 0.0;
  try
    Result := FPOW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_POW(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_POW';
begin
  try
    FPOW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_SL: Integer;
const OPNAME = 'TRunOffSamiModel.Get_SL';
begin
  Result := 0;
  try
    Result := FSL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_SL(Value: Integer);
const OPNAME = 'TRunOffSamiModel.Set_SL';
begin
  try
    FSL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_ST: Integer;
const OPNAME = 'TRunOffSamiModel.Get_ST';
begin
  Result := 0;
  try
    Result := FST;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_ST(Value: Integer);
const OPNAME = 'TRunOffSamiModel.Set_ST';
begin
  try
    FST := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_FT: Double;
const OPNAME = 'TRunOffSamiModel.Get_FT';
begin
  Result := 0.0;
  try
    Result := FFT;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_FT(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_FT';
begin
  try
    FFT := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_GW: Double;
const OPNAME = 'TRunOffSamiModel.Get_GW';
begin
  Result := 0.0;
  try
    Result := FGW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_GW(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_GW';
begin
  try
    FGW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_ZMIN: Integer;
const OPNAME = 'TRunOffSamiModel.Get_ZMIN';
begin
  Result := 0;
  try
    Result := FZMIN;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_ZMIN(Value: Integer);
const OPNAME = 'TRunOffSamiModel.Set_ZMIN';
begin
  try
    FZMIN := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_ZMAX: Integer;
const OPNAME = 'TRunOffSamiModel.Get_ZMAX';
begin
  Result := 0;
  try
    Result := FZMAX;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_ZMAX(Value: Integer);
const OPNAME = 'TRunOffSamiModel.Set_ZMAX';
begin
  try
    FZMAX := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_PI: Double;
const OPNAME = 'TRunOffSamiModel.Get_PI';
begin
  Result := 0.0;
  try
    Result := FPI;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_PI(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_PI';
begin
  try
    FPI := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_TL: Double;
const OPNAME = 'TRunOffSamiModel.Get_TL';
begin
  Result := 0.0;
  try
    Result := FTL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_TL(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_TL';
begin
  try
    FTL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_GL: Double;
const OPNAME = 'TRunOffSamiModel.Get_GL';
begin
  Result := 0.0;
  try
    Result := FGL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_GL(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_GL';
begin
  try
    FGL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_R: Double;
const OPNAME = 'TRunOffSamiModel.Get_R';
begin
  Result := 0.0;
  try
    Result := FR;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_R(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_R';
begin
  try
    FR := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiModel.Get_FF: Double;
const OPNAME = 'TRunOffSamiModel.Get_FF';
begin
  Result := 0.0;
  try
    Result := FFF;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Set_FF(Value: Double);
const OPNAME = 'TRunOffSamiModel.Set_FF';
begin
  try
    FFF := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiModel.Populate(AAquiferThickness              : Double;
                                    AStorativity                   : Double;
                                    AInitialAquiferStorage         : Double;
                                    AStaticWaterLevel              : Double;
                                    AUnsaturatedStorage            : Double;
                                    AInitialUnsaturatedZoneStorage : Double;
                                    APerculationPower              : Double;
                                    AGPOW                          : Double;
                                    AMaxDischarge                  : Double;
                                    AInteractionCurvePower         : Double;
                                    AHGSL                          : Double;
                                    AHGGW                          : Double;
                                    AMaxHydrologicalGradient       : Double;
                                    ATransmissivity                : Double;
                                    ABoreholeDistanceToRiver       : Double;
                                    AGroundWaterEvaporationArea    : Double;
                                    AK2                            : Double;
                                    AK3                            : Double;
                                    AInterflowLag                  : Double;
                                    ARechargeAveragedNoMonths      : Double;
                                    AUseAbstractions               : Integer;
                                    APOW                           : Double;
                                    ASL                            : Integer;
                                    AST                            : Integer;
                                    AFT                            : Double;
                                    AGW                            : Double;
                                    AZMIN                          : Integer;
                                    AZMAX                          : Integer;
                                    API                            : Double;
                                    ATL                            : Double;
                                    AGL                            : Double;
                                    AR                             : Double;
                                    AFF                            : Double);
const OPNAME = 'TRunOffSamiModel.Populate';
begin
  try
    FAquiferThickness               := AAquiferThickness;
    FStorativity                    := AStorativity;
    FInitialAquiferStorage          := AInitialAquiferStorage;
    FStaticWaterLevel               := AStaticWaterLevel;
    FUnsaturatedStorage             := AUnsaturatedStorage;
    FInitialUnsaturatedZoneStorage  := AInitialUnsaturatedZoneStorage;
    FPerculationPower               := APerculationPower;
    FGPOW                           := AGPOW;
    FMaxDischarge                   := AMaxDischarge;
    FInteractionCurvePower          := AInteractionCurvePower;
    FHGSL                           := AHGSL;
    FHGGW                           := AHGGW;
    FMaxHydrologicalGradient        := AMaxHydrologicalGradient;
    FTransmissivity                 := ATransmissivity;
    FBoreholeDistanceToRiver        := ABoreholeDistanceToRiver;
    FGroundWaterEvaporationArea     := AGroundWaterEvaporationArea;
    FK2                             := AK2;
    FK3                             := AK3;
    FInterflowLag                   := AInterflowLag;
    FRechargeAveragedNoMonths       := ARechargeAveragedNoMonths;
    FUseAbstractions                := AUseAbstractions;
    FPOW                            := APOW;
    FSL                             := ASL;
    FST                             := AST;
    FFT                             := AFT;
    FGW                             := AGW;
    FZMIN                           := AZMIN;
    FZMAX                           := AZMAX;
    FPI                             := API;
    FTL                             := ATL;
    FGL                             := AGL;
    FR                              := AR;
    FFF                             := AFF;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRunOffHughesModel **********************************************************}

function TRunOffHughesModel._AddRef: Integer;
const OPNAME = 'TRunOffHughesModel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffHughesModel._Release: Integer;
const OPNAME = 'TRunOffHughesModel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffHughesModel.Get_InflowRouteNo: Integer;
const OPNAME = 'TRunOffHughesModel.Get_InflowRouteNo';
begin
  Result := 0;
  try
    Result := FInflowRouteNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_InflowRouteNo(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_InflowRouteNo';
begin
  try
    FInflowRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_InfluenceROMNo: Integer;
const OPNAME = 'TRunOffHughesModel.Get_InfluenceROMNo';
begin
  Result := 0;
  try
    Result := FInfluenceROMNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_InfluenceROMNo(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_InfluenceROMNo';
begin
  try
    FInfluenceROMNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_GroundWaterModel: Integer;
const OPNAME = 'TRunOffHughesModel.Get_GroundWaterModel';
begin
  Result := 0;
  try
    Result := FGroundWaterModel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_GroundWaterModel(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_GroundWaterModel';
begin
  try
    FGroundWaterModel := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_HGSL: Double;
const OPNAME = 'TRunOffHughesModel.Get_HGSL';
begin
  Result := 0.0;
  try
    Result := FHGSL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_HGSL(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_HGSL';
begin
  try
    FHGSL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_GPOW: Double;
const OPNAME = 'TRunOffHughesModel.Get_GPOW';
begin
  Result := 0.0;
  try
    Result := FGPOW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_GPOW(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_GPOW';
begin
  try
    FGPOW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_TLGMax: Double;
const OPNAME = 'TRunOffHughesModel.Get_TLGMax';
begin
  Result := 0.0;
  try
    Result := FTLGMax;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_TLGMax(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_TLGMax';
begin
  try
    FTLGMax := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_HGGW: Double;
const OPNAME = 'TRunOffHughesModel.Get_HGGW';
begin
  Result := 0.0;
  try
    Result := FHGGW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_HGGW(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_HGGW';
begin
  try
    FHGGW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_POW: Double;
const OPNAME = 'TRunOffHughesModel.Get_POW';
begin
  Result := 0.0;
  try
    Result := FPOW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_POW(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_POW';
begin
  try
    FPOW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_SL: Integer;
const OPNAME = 'TRunOffHughesModel.Get_SL';
begin
  Result := 0;
  try
    Result := FSL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_SL(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_SL';
begin
  try
    FSL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_ST: Integer;
const OPNAME = 'TRunOffHughesModel.Get_ST';
begin
  Result := 0;
  try
    Result := FST;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_ST(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_ST';
begin
  try
    FST := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_FT: Double;
const OPNAME = 'TRunOffHughesModel.Get_FT';
begin
  Result := 0.0;
  try
    Result := FFT;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_FT(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_FT';
begin
  try
    FFT := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_GW: Double;
const OPNAME = 'TRunOffHughesModel.Get_GW';
begin
  Result := 0.0;
  try
    Result := FGW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_GW(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_GW';
begin
  try
    FGW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_ZMIN: Integer;
const OPNAME = 'TRunOffHughesModel.Get_ZMIN';
begin
  Result := 0;
  try
    Result := FZMIN;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_ZMIN(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_ZMIN';
begin
  try
    FZMIN := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_ZMAX: Integer;
const OPNAME = 'TRunOffHughesModel.Get_ZMAX';
begin
  Result := 0;
  try
    Result := FZMAX;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_ZMAX(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_ZMAX';
begin
  try
    ZMAX := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_PI: Double;
const OPNAME = 'TRunOffHughesModel.Get_PI';
begin
  Result := 0.0;
  try
    Result := FPI;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_PI(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_PI';
begin
  try
    FPI := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_TL: Double;
const OPNAME = 'TRunOffHughesModel.Get_TL';
begin
  Result := 0.0;
  try
    Result := FTL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_TL(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_TL';
begin
  try
    FTL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_GL: Double;
const OPNAME = 'TRunOffHughesModel.Get_GL';
begin
  Result := 0.0;
  try
    Result := FGL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_GL(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_GL';
begin
  try
    FGL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_R: Double;
const OPNAME = 'TRunOffHughesModel.Get_R';
begin
  Result := 0.0;
  try
    Result := FR;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_R(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_R';
begin
  try
    FR := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_FF: Double;
const OPNAME = 'TRunOffHughesModel.Get_FF';
begin
  Result := 0.0;
  try
    Result := FFF;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_FF(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_FF';
begin
  try
    FFF := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_UseNoOfReaches: Integer;
const OPNAME = 'TRunOffHughesModel.Get_UseNoOfReaches';
begin
  Result := 0;
  try
    Result := FUseNoOfReaches;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_UseNoOfReaches(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_UseNoOfReaches';
begin
  try
    FUseNoOfReaches := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_DrainageDensity: Double;
const OPNAME = 'TRunOffHughesModel.Get_DrainageDensity';
begin
  Result := 0.0;
  try
    Result := FDrainageDensity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_DrainageDensity(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_DrainageDensity';
begin
  try
    FDrainageDensity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_NoOfReaches: Integer;
const OPNAME = 'TRunOffHughesModel.Get_NoOfReaches';
begin
  Result := 0;
  try
    Result := FNoOfReaches;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_NoOfReaches(Value: Integer);
const OPNAME = 'TRunOffHughesModel.Set_NoOfReaches';
begin
  try
    FNoOfReaches := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_RiparianAreaWidthPercentage: Double;
const OPNAME = 'TRunOffHughesModel.Get_RiparianAreaWidthPercentage';
begin
  Result := 0.0;
  try
    Result := FRiparianAreaWidthPercentage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_RiparianAreaWidthPercentage(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_RiparianAreaWidthPercentage';
begin
  try
    FRiparianAreaWidthPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_RiparianStripFactor: Double;
const OPNAME = 'TRunOffHughesModel.Get_RiparianStripFactor';
begin
  Result := 0.0;
  try
    Result := FRiparianStripFactor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_RiparianStripFactor(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_RiparianStripFactor';
begin
  try
    FRiparianStripFactor := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_RestWaterLevel: Double;
const OPNAME = 'TRunOffHughesModel.Get_RestWaterLevel';
begin
  Result := 0.0;
  try
    Result := FRestWaterLevel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_RestWaterLevel(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_RestWaterLevel';
begin
  try
    FRestWaterLevel := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_Transmissivity: Double;
const OPNAME = 'TRunOffHughesModel.Get_Transmissivity';
begin
  Result := 0.0;
  try
    Result := FTransmissivity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_Transmissivity(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_Transmissivity';
begin
  try
    FTransmissivity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_Storativity: Double;
const OPNAME = 'TRunOffHughesModel.Get_Storativity';
begin
  Result := 0.0;
  try
    Result := FStorativity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_Storativity(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_Storativity';
begin
  try
    FStorativity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_GroundWaterSlope: Double;
const OPNAME = 'TRunOffHughesModel.Get_GroundWaterSlope';
begin
  Result := 0.0;
  try
    Result := FGroundWaterSlope;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_GroundWaterSlope(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_GroundWaterSlope';
begin
  try
    FGroundWaterSlope := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_AnnualUpperZoneAbstraction: Double;
const OPNAME = 'TRunOffHughesModel.Get_AnnualUpperZoneAbstraction';
begin
  Result := 0.0;
  try
    Result := FAnnualUpperZoneAbstraction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_AnnualUpperZoneAbstraction(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_AnnualUpperZoneAbstraction';
begin
  try
    FAnnualUpperZoneAbstraction := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_MonthlyUpperZoneAbstraction(AMonthIndex: Integer): Double;
const OPNAME = 'TRunOffHughesModel.Get_MonthlyUpperZoneAbstraction';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyUpperZoneAbstraction[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_MonthlyUpperZoneAbstraction(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_MonthlyUpperZoneAbstraction';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyUpperZoneAbstraction[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_AnnualRiparianZoneAbstraction: Double;
const OPNAME = 'TRunOffHughesModel.Get_AnnualRiparianZoneAbstraction';
begin
  Result := 0.0;
  try
    Result := FAnnualRiparianZoneAbstraction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_AnnualRiparianZoneAbstraction(Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_AnnualRiparianZoneAbstraction';
begin
  try
    FAnnualRiparianZoneAbstraction := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Get_MonthlyRiparianZoneAbstraction(AMonthIndex: Integer): Double;
const OPNAME = 'TRunOffHughesModel.Get_MonthlyRiparianZoneAbstraction';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FMonthlyRiparianZoneAbstraction[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesModel.Set_MonthlyRiparianZoneAbstraction(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TRunOffHughesModel.Set_MonthlyRiparianZoneAbstraction';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FMonthlyRiparianZoneAbstraction[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesModel.Populate(AInflowRouteNo               : Integer;  AInfluenceROMNo      : Integer;
                                     AGroundWaterModel            : Integer;  AHGSL                : Double;
                                     AGPOW                        : Double;   ATLGMax              : Double;
                                     AHGGW                        : Double;   APOW                 : Double;
                                     ASL                          : Integer;  AST                  : Integer;
                                     AFT                          : Double;   AGW                  : Double;
                                     AZMIN                        : Integer;  AZMAX                : Integer;
                                     API                          : Double;   ATL                  : Double;
                                     AGL                          : Double;   AR                   : Double;
                                     AFF                          : Double;   AUseNoOfReaches      : Integer;
                                     ADrainageDensity             : Double;   ANoOfReaches         : Integer;
                                     ARiparianAreaWidthPercentage : Double;   ARiparianStripFactor : Double;
                                     ARestWaterLevel              : Double;   ATransmissivity      : Double;
                                     AStorativity                 : Double;   AGroundWaterSlope    : Double;
                                     AAnnualUpperZoneAbstraction           : Double;
                                     const AMonthlyUpperZoneAbstraction    : WideString;
                                     AAnnualRiparianZoneAbstraction        : Double;
                                     const AMonthlyRiparionZoneAbstraction : WideString): WordBool;
const OPNAME = 'TRunOffHughesModel.Populate';
var
  LTempList : TStringList;
  LIndex    : Integer;
begin
  Result := FALSE;
  try
    FInflowRouteNo                  := AInflowRouteNo;
    FInfluenceROMNo                 := AInfluenceROMNo;
    FGroundWaterModel               := AGroundWaterModel;
    FHGSL                           := AHGSL;
    FGPOW                           := AGPOW;
    FTLGMax                         := ATLGMax;
    FHGGW                           := AHGGW;
    FPOW                            := APOW;
    FSL                             := ASL;
    FST                             := AST;
    FFT                             := AFT;
    FGW                             := AGW;
    FZMIN                           := AZMIN;
    FZMAX                           := AZMAX;
    FPI                             := API;
    FTL                             := ATL;
    FGL                             := AGL;
    FR                              := AR;
    FFF                             := AFF;
    FUseNoOfReaches                 := AUseNoOfReaches;
    FDrainageDensity                := ADrainageDensity;
    FNoOfReaches                    := ANoOfReaches;
    FRiparianAreaWidthPercentage    := ARiparianAreaWidthPercentage;
    FRiparianStripFactor            := ARiparianStripFactor;
    FRestWaterLevel                 := ARestWaterLevel;
    FTransmissivity                 := ATransmissivity;
    FStorativity                    := AStorativity;
    FGroundWaterSlope               := AGroundWaterSlope;
    FAnnualUpperZoneAbstraction     := AAnnualUpperZoneAbstraction;
    FAnnualRiparianZoneAbstraction  := AAnnualRiparianZoneAbstraction;

    LTempList := TStringList.Create;
    try
      LTempList.CommaText := AMonthlyUpperZoneAbstraction;
      for LIndex := 1 to 12 do
        FMonthlyUpperZoneAbstraction[LIndex] := StrToFloat(LTempList.Strings[LIndex-1]);
      LTempList.Clear;
      LTempList.CommaText := AMonthlyRiparionZoneAbstraction;
      for LIndex := 1 to 12 do
        FMonthlyRiparianZoneAbstraction[LIndex] := StrToFloat(LTempList.Strings[LIndex-1]);
      Result := TRUE;
    finally
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRunOffPitmanModel **********************************************************}

function TRunOffPitmanModel._AddRef: Integer;
const OPNAME = 'TRunOffPitmanModel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffPitmanModel._Release: Integer;
const OPNAME = 'TRunOffPitmanModel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffPitmanModel.Get_POW: Double;
const OPNAME = 'TRunOffPitmanModel.Get_POW';
begin
  Result := 0.0;
  try
    Result := FPOW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_POW(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_POW';
begin
  try
    FPOW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_SL: Integer;
const OPNAME = 'TRunOffPitmanModel.Get_SL';
begin
  Result := 0;
  try
    Result := FSL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_SL(Value: Integer);
const OPNAME = 'TRunOffPitmanModel.Set_SL';
begin
  try
    FSL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_ST: Integer;
const OPNAME = 'TRunOffPitmanModel.Get_ST';
begin
  Result := 0;
  try
    Result := FST;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_ST(Value: Integer);
const OPNAME = 'TRunOffPitmanModel.Set_ST';
begin
  try
    FST := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_FT: Double;
const OPNAME = 'TRunOffPitmanModel.Get_FT';
begin
  Result := 0.0;
  try
    Result := FFT;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_FT(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_FT';
begin
  try
    FFT := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_GW: Double;
const OPNAME = 'TRunOffPitmanModel.Get_GW';
begin
  Result := 0.0;
  try
    Result := FGW;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_GW(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_GW';
begin
  try
    FGW := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_ZMIN: Integer;
const OPNAME = 'TRunOffPitmanModel.Get_ZMIN';
begin
  Result := 0;
  try
    Result := FZMIN;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_ZMIN(Value: Integer);
const OPNAME = 'TRunOffPitmanModel.Set_ZMIN';
begin
  try
    FZMIN := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_ZMAX: Integer;
const OPNAME = 'TRunOffPitmanModel.Get_ZMAX';
begin
  Result := 0;
  try
    Result := FZMAX;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_ZMAX(Value: Integer);
const OPNAME = 'TRunOffPitmanModel.Set_ZMAX';
begin
  try
    FZMAX := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_PI: Double;
const OPNAME = 'TRunOffPitmanModel.Get_PI';
begin
  Result := 0.0;
  try
    Result := FPI;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_PI(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_PI';
begin
  try
    FPI := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_TL: Double;
const OPNAME = 'TRunOffPitmanModel.Get_TL';
begin
  Result := 0.0;
  try
    Result := FTL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_TL(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_TL';
begin
  try
    FTL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_GL: Double;
const OPNAME = 'TRunOffPitmanModel.Get_GL';
begin
  Result := 0.0;
  try
    Result := FGL;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_GL(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_GL';
begin
  try
    FGL := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_R: Double;
const OPNAME = 'TRunOffPitmanModel.Get_R';
begin
  Result := 0.0;
  try
    Result := FR;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_R(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_R';
begin
  try
    FR := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Get_FF: Double;
const OPNAME = 'TRunOffPitmanModel.Get_FF';
begin
  Result := 0.0;
  try
    Result := FFF;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPitmanModel.Set_FF(Value: Double);
const OPNAME = 'TRunOffPitmanModel.Set_FF';
begin
  try
    FFF := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPitmanModel.Populate(APOW  : Double;
                                     ASL   : Integer;
                                     AST   : Integer;
                                     AFT   : Double;
                                     AGW   : Double;
                                     AZMIN : Integer;
                                     AZMAX : Integer;
                                     API   : Double;
                                     ATL   : Double;
                                     AGL   : Double;
                                     AR    : Double;
                                     AFF   : Double): WordBool;
const OPNAME = 'TRunOffPitmanModel.Populate';
begin
  Result := FALSE;
  try
    FPOW  := APOW;
    FSL   := ASL;
    FST   := AST;
    FFT   := AFT;
    FGW   := AGW;
    FZMIN := AZMIN;
    FZMAX := AZMAX;
    FPI   := API;
    FTL   := ATL;
    FGL   := AGL;
    FR    := AR;
    FFF   := AFF;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRunOffAfforestation ********************************************************}

function TRunOffAfforestation._AddRef: Integer;
const OPNAME = 'TRunOffAfforestation._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAfforestation._Release: Integer;
const OPNAME = 'TRunOffAfforestation._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAfforestation.CreateMemberObjects;
const OPNAME = 'TRunOffAfforestation.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FYearlyData := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAfforestation.DestroyMemberObjects;
const OPNAME = 'TRunOffAfforestation.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    for LCount := FYearlyData.Count - 1 downto 0 do
      TYearValuePair(FYearlyData.Items[LCount]).Free;
    FYearlyData.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAfforestation.Get_Algorithm: Integer;
const OPNAME = 'TRunOffAfforestation.Get_Algorithm';
begin
  Result := 0;
  try
    Result := FAlgorithm;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_Algorithm(Value: Integer);
const OPNAME = 'TRunOffAfforestation.Set_Algorithm';
begin
  try
    FAlgorithm := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_PineAreaPercentage: Double;
const OPNAME = 'TRunOffAfforestation.Get_PineAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FPineAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_PineAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_PineAreaPercentage';
begin
  try
    FPineAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_PineRotationPeriod: Integer;
const OPNAME = 'TRunOffAfforestation.Get_PineRotationPeriod';
begin
  Result := 0;
  try
    Result := FPineRotationPeriod;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_PineRotationPeriod(Value: Integer);
const OPNAME = 'TRunOffAfforestation.Set_PineRotationPeriod';
begin
  try
    FPineRotationPeriod := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_EucalyptusAreaPercentage: Double;
const OPNAME = 'TRunOffAfforestation.Get_EucalyptusAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FEucalyptusAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_EucalyptusAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_EucalyptusAreaPercentage';
begin
  try
    FEucalyptusAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_EucalyptusRotationPeriod: Integer;
const OPNAME = 'TRunOffAfforestation.Get_EucalyptusRotationPeriod';
begin
  Result := 0;
  try
    Result := FEucalyptusRotationPeriod;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_EucalyptusRotationPeriod(Value: Integer);
const OPNAME = 'TRunOffAfforestation.Set_EucalyptusRotationPeriod';
begin
  try
    FEucalyptusRotationPeriod := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_WattleAreaPercentage: Double;
const OPNAME = 'TRunOffAfforestation.Get_WattleAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FWattleAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_WattleAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_WattleAreaPercentage';
begin
  try
    FWattleAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_WattleRotationPeriod: Integer;
const OPNAME = 'TRunOffAfforestation.Get_WattleRotationPeriod';
begin
  Result := 0;
  try
    Result := FWattleRotationPeriod;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_WattleRotationPeriod(Value: Integer);
const OPNAME = 'TRunOffAfforestation.Set_WattleRotationPeriod';
begin
  try
    FWattleRotationPeriod := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_OptimalAreaPercentage: Double;
const OPNAME = 'TRunOffAfforestation.Get_OptimalAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FOptimalAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_OptimalAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_OptimalAreaPercentage';
begin
  try
    FOptimalAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_SFRReductionMAR: Double;
const OPNAME = 'TRunOffAfforestation.Get_SFRReductionMAR';
begin
  Result := 0.0;
  try
    Result := FSFRReductionMAR;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_SFRReductionMAR(Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_SFRReductionMAR';
begin
  try
    FSFRReductionMAR := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_SFRReductionLowFlows: Double;
const OPNAME = 'TRunOffAfforestation.Get_SFRReductionLowFlows';
begin
  Result := 0.0;
  try
    Result := FSFRReductionLowFlows;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_SFRReductionLowFlows(Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_SFRReductionLowFlows';
begin
  try
    FSFRReductionLowFlows := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_NumberOfYears: Integer;
const OPNAME = 'TRunOffAfforestation.Get_NumberOfYears';
begin
  Result := 0;
  try
    Result := FYearlyData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_Year(AYearIndex: Integer): Integer;
const OPNAME = 'TRunOffAfforestation.Get_Year';
begin
  Result := 0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_Year(AYearIndex: Integer; Value: Integer);
const OPNAME = 'TRunOffAfforestation.Set_Year';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Get_Area(AYearIndex: Integer): Double;
const OPNAME = 'TRunOffAfforestation.Get_Area';
begin
  Result := 0.0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.Set_Area(AYearIndex: Integer; Value: Double);
const OPNAME = 'TRunOffAfforestation.Set_Area';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestation.Populate(AAlgorithm                : Integer;
                                       APineAreaPercentage       : Double;
                                       APineRotationPeriod       : Integer;
                                       AEucalyptusAreaPercentage : Double;
                                       AEucalyptusRotationPeriod : Integer;
                                       AWattleAreaPercentage     : Double;
                                       AWattleRotationPeriod     : Integer;
                                       AOptimalAreaPercentage    : Double;
                                       ASFRReductionMAR          : Double;
                                       ASFRReductionLowFlows     : Double): WordBool;
const OPNAME = 'TRunOffAfforestation.Populate';
begin
  Result := FALSE;
  try
    FAlgorithm                := AAlgorithm;
    FPineAreaPercentage       := APineAreaPercentage;
    FPineRotationPeriod       := APineRotationPeriod;
    FEucalyptusAreaPercentage := AEucalyptusAreaPercentage;
    FEucalyptusRotationPeriod := AEucalyptusRotationPeriod;
    FWattleAreaPercentage     := AWattleAreaPercentage;
    FWattleRotationPeriod     := AWattleRotationPeriod;
    FOptimalAreaPercentage    := AOptimalAreaPercentage;
    FSFRReductionMAR          := ASFRReductionMAR;
    FSFRReductionLowFlows     := ASFRReductionLowFlows;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestation.AddAreaData(AYear: Integer;
                                           AArea: Double);
const OPNAME = 'TRunOffAfforestation.AddAreaData';
var
  LYearAreaPair : TYearValuePair;
begin
  try
    LYearAreaPair := TYearValuePair.Create;
    LYearAreaPair.Year  := AYear;
    LYearAreaPair.Value := AArea;
    FYearlyData.Add(LYearAreaPair);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TRunOffAlienVegetation ******************************************************}

function TRunOffAlienVegetation._AddRef: Integer;
const OPNAME = 'TRunOffAlienVegetation._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation._Release: Integer;
const OPNAME = 'TRunOffAlienVegetation._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.CreateMemberObjects;
const OPNAME = 'TRunOffAlienVegetation.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FYearlyData := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.DestroyMemberObjects;
const OPNAME = 'TRunOffAlienVegetation.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    for LCount := FYearlyData.Count - 1 downto 0 do
      TYearValuePair(FYearlyData.Items[LCount]).Free;
    FYearlyData.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_Algorithm: Integer;
const OPNAME = 'TRunOffAlienVegetation.Get_Algorithm';
begin
  Result := 0;
  try
    Result := FAlgorithm;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_Algorithm(Value: Integer);
const OPNAME = 'TRunOffAlienVegetation.Set_Algorithm';
begin
  try
    FAlgorithm := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_RiparianVegetationArea: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_RiparianVegetationArea';
begin
  Result := 0.0;
  try
    Result := FRiparianVegetationArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_RiparianVegetationArea(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_RiparianVegetationArea';
begin
  try
    FRiparianVegetationArea := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_TallTreeAreaPercentage: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_TallTreeAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FTallTreeAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_TallTreeAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_TallTreeAreaPercentage';
begin
  try
    FTallTreeAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_TallTreeAge: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_TallTreeAge';
begin
  Result := 0.0;
  try
    Result := FTallTreeAge;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_TallTreeAge(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_TallTreeAge';
begin
  try
    FTallTreeAge := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_MediumTreeAreaPercentage: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_MediumTreeAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FMediumTreeAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_MediumTreeAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_MediumTreeAreaPercentage';
begin
  try
    FMediumTreeAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_MediumTreeAge: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_MediumTreeAge';
begin
  Result := 0.0;
  try
    Result := FMediumTreeAge;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_MediumTreeAge(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_MediumTreeAge';
begin
  try
    FMediumTreeAge := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_TallSchrubAreaPercentage: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_TallSchrubAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FTallSchrubAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_TallSchrubAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_TallSchrubAreaPercentage';
begin
  try
    FTallSchrubAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_TallSchrubAge: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_TallSchrubAge';
begin
  Result := 0.0;
  try
    Result := FTallSchrubAge;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_TallSchrubAge(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_TallSchrubAge';
begin
  try
    FTallSchrubAge := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_OptimalAreaPercentage: Double;
const OPNAME = 'TRunOffAlienVegetation.Get_OptimalAreaPercentage';
begin
  Result := 0.0;
  try
    Result := FOptimalAreaPercentage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_OptimalAreaPercentage(Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_OptimalAreaPercentage';
begin
  try
    FOptimalAreaPercentage := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_NumberOfYears: Integer;
const OPNAME = 'TRunOffAlienVegetation.Get_NumberOfYears';
begin
  Result := 0;
  try
    Result := FYearlyData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_Year(AYearIndex: Integer): Integer;
const OPNAME = 'TRunOffAlienVegetation.Get_Year';
begin
  Result := 0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_Year(AYearIndex: Integer; Value: Integer);
const OPNAME = 'TRunOffAlienVegetation.Set_Year';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Get_Area(AYearIndex: Integer): Double;
const OPNAME = 'TRunOffAlienVegetation.Get_Area';
begin
  Result := 0.0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.Set_Area(AYearIndex: Integer; Value: Double);
const OPNAME = 'TRunOffAlienVegetation.Set_Area';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffAlienVegetation.Populate(AAlgorithm                : Integer;
                                         ARiparianVegetationArea   : Double;
                                         ATallTreeAreaPercentage   : Double;
                                         ATallTreeAge              : Double;
                                         AMediumTreeAreaPercentage : Double;
                                         AMediumTreeAge            : Double;
                                         ATallSchrubAreaPercentage : Double;
                                         ATallSchrubAge            : Double;
                                         AOptimalAreaPercentage    : Double): WordBool;
const OPNAME = 'TRunOffAlienVegetation.Populate';
begin
  Result := FALSE;
  try
    FAlgorithm                := AAlgorithm;
    FRiparianVegetationArea   := ARiparianVegetationArea;
    FTallTreeAreaPercentage   := ATallTreeAreaPercentage;
    FTallTreeAge              := ATallTreeAge;
    FMediumTreeAreaPercentage := AMediumTreeAreaPercentage;
    FMediumTreeAge            := AMediumTreeAge;
    FTallSchrubAreaPercentage := ATallSchrubAreaPercentage;
    FTallSchrubAge            := ATallSchrubAge;
    FOptimalAreaPercentage    := AOptimalAreaPercentage;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffAlienVegetation.AddAreaData(AYear: Integer; AArea: Double);
const OPNAME = 'TRunOffAlienVegetation.AddAreaData';
var
  LYearAreaPair : TYearValuePair;
begin
  try
    LYearAreaPair := TYearValuePair.Create;
    LYearAreaPair.Year  := AYear;
    LYearAreaPair.Value := AArea;
    FYearlyData.Add(LYearAreaPair);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TRunOffPavedArea ************************************************************}

function TRunOffPavedArea._AddRef: Integer;
const OPNAME = 'TRunOffPavedArea._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffPavedArea._Release: Integer;
const OPNAME = 'TRunOffPavedArea._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffPavedArea.CreateMemberObjects;
const OPNAME = 'TRunOffPavedArea.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FYearlyData := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffPavedArea.DestroyMemberObjects;
const OPNAME = 'TRunOffPavedArea.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    for LCount := FYearlyData.Count - 1 downto 0 do
      TYearValuePair(FYearlyData.Items[LCount]).Free;
    FYearlyData.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffPavedArea.Get_NumberOfYears: Integer;
const OPNAME = 'TRunOffPavedArea.Get_NumberOfYears';
begin
  Result := 0;
  try
    Result := FYearlyData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffPavedArea.Get_Year(AYearIndex: Integer): Integer;
const OPNAME = 'TRunOffPavedArea.Get_Year';
begin
  Result := 0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffPavedArea.Set_Year(AYearIndex: Integer; Value: Integer);
const OPNAME = 'TRunOffPavedArea.Set_Year';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffPavedArea.Get_Proportion(AYearIndex: Integer): Double;
const OPNAME = 'TRunOffPavedArea.Get_Proportion';
begin
  Result := 0.0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffPavedArea.Set_Proportion(AYearIndex: Integer; Value: Double);
const OPNAME = 'TRunOffPavedArea.Set_Proportion';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffPavedArea.AddAreaData(AYear: Integer; AProportion: Double);
const OPNAME = 'TRunOffPavedArea.AddAreaData';
var
  LYearProportionPair : TYearValuePair;
begin
  try
    LYearProportionPair := TYearValuePair.Create;
    LYearProportionPair.Year  := AYear;
    LYearProportionPair.Value := AProportion;
    FYearlyData.Add(LYearProportionPair);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TRunOffGroundwaterAbstraction ***********************************************}

function TRunOffGroundwaterAbstraction._AddRef: Integer;
const OPNAME = 'TRunOffGroundwaterAbstraction._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffGroundwaterAbstraction._Release: Integer;
const OPNAME = 'TRunOffGroundwaterAbstraction._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffGroundwaterAbstraction.CreateMemberObjects;
const OPNAME = 'TRunOffGroundwaterAbstraction.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FYearlyData := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffGroundwaterAbstraction.DestroyMemberObjects;
const OPNAME = 'TRunOffGroundwaterAbstraction.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    for LCount := FYearlyData.Count - 1 downto 0 do
      TYearValuePair(FYearlyData.Items[LCount]).Free;
    FYearlyData.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffGroundwaterAbstraction.Get_NumberOfYears: Integer;
const OPNAME = 'TRunOffGroundwaterAbstraction.Get_NumberOfYears';
begin
  Result := 0;
  try
    Result := FYearlyData.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffGroundwaterAbstraction.Get_Year(AYearIndex: Integer): Integer;
const OPNAME = 'TRunOffGroundwaterAbstraction.Get_Year';
begin
  Result := 0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Year;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffGroundwaterAbstraction.Set_Year(AYearIndex: Integer; Value: Integer);
const OPNAME = 'TRunOffGroundwaterAbstraction.Set_Year';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Year := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffGroundwaterAbstraction.Get_AbstractionValue(AYearIndex: Integer): double;
const OPNAME = 'TRunOffGroundwaterAbstraction.Get_AbstractionValue';
begin
  Result := 0.0;
  try
    if (AYearIndex <= FYearlyData.Count) then
      Result := TYearValuePair(FYearlyData.Items[AYearIndex]).Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffGroundwaterAbstraction.Set_AbstractionValue(AYearIndex: Integer; Value: double);
const OPNAME = 'TRunOffGroundwaterAbstraction.Set_AbstractionValue';
begin
  try
    if (AYearIndex <= FYearlyData.Count) then
      TYearValuePair(FYearlyData.Items[AYearIndex]).Value := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffGroundwaterAbstraction.AddAbstractionData(AYear: Integer; AValue: Double);
const OPNAME = 'TRunOffGroundwaterAbstraction.AddAbstractionData';
var
  LYearValuePair : TYearValuePair;
begin
  try
    LYearValuePair := TYearValuePair.Create;
    LYearValuePair.Year  := AYear;
    LYearValuePair.Value := AValue;
    FYearlyData.Add(LYearValuePair);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TRunOffOutflowRoute *********************************************************}

function TRunOffOutflowRoute._AddRef: Integer;
const OPNAME = 'TRunOffOutflowRoute._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffOutflowRoute._Release: Integer;
const OPNAME = 'TRunOffOutflowRoute._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffOutflowRoute.Get_RouteNo: Integer;
const OPNAME = 'TRunOffOutflowRoute.Get_RouteNo';
begin
  Result := 0;
  try
    Result := FRouteNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffOutflowRoute.Set_RouteNo(Value: Integer);
const OPNAME = 'TRunOffOutflowRoute.Set_RouteNo';
begin
  try
    FRouteNo := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffOutflowRoute.Get_OutflowPercentage: Double;
const OPNAME = 'TRunOffOutflowRoute.Get_OutflowPercentage';
begin
  Result := 0.0;
  try
    Result := FOutflowProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffOutflowRoute.Set_OutflowPercentage(Value: Double);
const OPNAME = 'TRunOffOutflowRoute.Set_OutflowPercentage';
begin
  try
    FOutflowProportion := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TRunOffModule ***************************************************************}

function TRunOffModule._AddRef: Integer;
const OPNAME = 'TRunOffModule._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule._Release: Integer;
const OPNAME = 'TRunOffModule._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffModule.CreateMemberObjects;
const OPNAME = 'TRunOffModule.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPitmanModel            := TRunOffPitmanModel.Create;
    FHughesModel            := TRunOffHughesModel.Create;
    FSamiModel              := TRunOffSamiModel.Create;
    FOutflowRoutes          := TList.Create;
    FSlaveModuleNumbers     := TStringList.Create;
    FAfforestation          := TRunOffAfforestation.Create;
    FAlienVegetation        := TRunOffAlienVegetation.Create;
    FPavedArea              := TRunOffPavedArea.Create;
    FGroundWaterAbstraction := TRunOffGroundWaterAbstraction.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffModule.DestroyMemberObjects;
const OPNAME = 'TRunOffModule.DestroyMemberObjects';
var
  LCount : Integer;
begin
  try
    FPitmanModel.Free;
    FHughesModel.Free;
    FAfforestation.Free;
    FAlienVegetation.Free;
    FPavedArea.Free;
    for LCount := FOutflowRoutes.Count - 1 downto 0 do
      TRunOffOutflowRoute(FOutflowRoutes.Items[LCount]).Free;
    FOutflowRoutes.Free;
    FSlaveModuleNumbers.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule.Get_RunOffName: WideString;
const OPNAME = 'TRunOffModule.Get_RunOffName';
begin
  Result := '';
  try
    Result := FRunOffName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_RunOffName(const Value: WideString);
const OPNAME = 'TRunOffModule.Set_RunOffName';
begin
  try
    FRunOffName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_VersionNo: Integer;
const OPNAME = 'TRunOffModule.Get_VersionNo';
begin
  Result := 0;
  try
    Result := FVersionNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_VersionNo(Value: Integer);
const OPNAME = 'TRunOffModule.Set_VersionNo';
begin
  try
    FVersionNo := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_CatchmentArea: Double;
const OPNAME = 'TRunOffModule.Get_CatchmentArea';
begin
  Result := 0.0;
  try
    Result := FCatchmentArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_CatchmentArea(Value: Double);
const OPNAME = 'TRunOffModule.Set_CatchmentArea';
begin
  try
    FCatchmentArea := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_CatchmentMAP: Double;
const OPNAME = 'TRunOffModule.Get_CatchmentMAP';
begin
  Result := 0;
  try
    Result := FCatchmentMAP;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_CatchmentMAP(Value: Double);
const OPNAME = 'TRunOffModule.Set_CatchmentMAP';
begin
  try
    FCatchmentMAP := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_RainfallFileName: WideString;
const OPNAME = 'TRunOffModule.Get_RainfallFileName';
begin
  Result := '';
  try
    Result := FRainfallFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_RainfallFileName(const Value: WideString);
const OPNAME = 'TRunOffModule.Set_RainfallFileName';
begin
  try
    FRainfallFileName := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_ProduceNaturalisedFlows: Integer;
const OPNAME = 'TRunOffModule.Get_ProduceNaturalisedFlows';
begin
  Result := 0;
  try
    Result := FProduceNaturalisedFlows;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_ProduceNaturalisedFlows(Value: Integer);
const OPNAME = 'TRunOffModule.Set_ProduceNaturalisedFlows';
begin
  try
    FProduceNaturalisedFlows := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_APanFactor(AMonthIndex: Integer): Double;
const OPNAME = 'TRunOffModule.Get_APanFactor';
begin
  Result := 0.0;
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      Result := FAPanFactor[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.Set_APanFactor(AMonthIndex: Integer; Value: Double);
const OPNAME = 'TRunOffModule.Set_APanFactor';
begin
  try
    if ((AMonthIndex > 0) AND (AMonthIndex <= 12)) then
      FAPanFactor[AMonthIndex] := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_PitmanModel: IRunOffPitmanModel;
const OPNAME = 'TRunOffModule.Get_PitmanModel';
begin
  Result := nil;
  try
    Result := FPitmanModel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_HughesModel: IRunOffHughesModel;
const OPNAME = 'TRunOffModule.Get_HughesModel';
begin
  Result := nil;
  try
    Result := FHughesModel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_SamiModel: IRunOffSamiModel;
const OPNAME = 'TRunOffModule.Get_SamiModel';
begin
  Result := nil;
  try
    Result := FSamiModel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_Afforestation: IRunOffAfforestation;
const OPNAME = 'TRunOffModule.Get_Afforestation';
begin
  Result := nil;
  try
    Result := FAfforestation;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_AlienVegetation: IRunOffAlienVegetation;
const OPNAME = 'TRunOffModule.Get_AlienVegetation';
begin
  Result := nil;
  try
    Result := FAlienVegetation;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_PavedArea: IRunOffPavedArea;
const OPNAME = 'TRunOffModule.Get_PavedArea';
begin
  Result := nil;
  try
    Result := FPavedArea;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Populate(ANetworkID                 : Integer;
                                AModuleID                  : Integer;
                                const AModuleType          : WideString;
                                AModuleNumber              : Integer;
                                ANetworkSequence           : Integer;
                                const AActive              : WideString;
                                const ARunOffName          : WideString;
                                AVersionNo                 : Integer;
                                ACatchmentArea             : Double;
                                ACatchmentMAP              : Double;
                                const ARainfallFileName    : WideString;
                                AProduceNaturalisedInflows : Integer;
                                ALongitude                 : Double;
                                ALatitude                  : Double;
                                const AAPanFactors         : WideString): WordBool;
const OPNAME = 'TRunOffModule.Populate';
var
  LIndex    : Integer;
  LTempList : TStringList;
begin
  Result := FALSE;
  try
    FNetworkID               := ANetworkID;
    FModuleID                := AModuleID;
    FModuleType              := AModuleType;
    FModuleNumber            := AModuleNumber;
    FNetworkSequence         := ANetworkSequence;
    FActive                  := AActive;
    FRunOffName              := ARunOffName;
    FVersionNo               := AVersionNo;
    FCatchmentArea           := ACatchmentArea;
    FCatchmentMAP            := ACatchmentMAP;
    FRainfallFileName        := ARainfallFileName;
    FProduceNaturalisedFlows := AProduceNaturalisedInflows;
    FLongitude               := ALongitude;
    FLatitude                := ALatitude;
    LTempList := TStringList.Create;
    try
      LTempList.CommaText := AAPanFactors;
      for LIndex := 1 to 12 do
        FAPanFactor[LIndex] := StrToFloat(LTempList.Strings[LIndex-1]);
    finally
      LTempList.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

// Calls AddOutflowRoute to create the instance and add it to the FOutFlowRoutes list.
function TRunOffModule.CreateNewOutflowRoute(ARouteNo : Integer; AOutflowPerc: Double) : Boolean;
const OPNAME = 'TRunOffModule.CreateNewOutflowRoute';
begin
  Result := FALSE;
  try
    Result := GRunOffDBManager.InsertRunOffOutflowRouteInDB(FModuleID, ARouteNo, AOutflowPerc);
    if (Result) then
      AddOutflowRoute(ARouteNo, AOutflowPerc);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Removes the instance of a TRunOffOutflowRoute from the FOutFlowRoutes list and destroys the instance.
function TRunOffModule.RemoveOutflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TRunOffModule.RemoveOutflowRoute';
var
  LOutflowRoute : TRunOffOutFlowRoute;
begin
  Result := FALSE;
  try
    LOutflowRoute := FindOutFlowRouteByRouteNo(ARouteNo);
    if (LOutflowRoute <> nil) then
    begin
      FOutFlowRoutes.Remove(LOutflowRoute);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Deletes the entry for the outflow route in the database.
// Calls RemoveOutflowRoute to remove the instance from the FOutFlowRoutes list.
function TRunOffModule.DeleteOutflowRoute(ARouteNo : Integer) : Boolean;
const OPNAME = 'TRunOffModule.DeleteOutflowRoute';
begin
  Result := FALSE;
  try
    Result := GRunOffDBManager.DeleteRunOffOutflowRouteFromDB(FModuleID, ARouteNo);
    if (Result) then
      Result := RemoveOutflowRoute(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// Creates an instance of a TRunOffOutflowRoute that already exists in the database and adds it to the FOutFlowRoutes list
procedure TRunOffModule.AddOutflowRoute(ARouteNo: Integer; AOutflowPercentage: Double);
const OPNAME = 'TRunOffModule.AddOutflowRoute';
var
  LOutflowRoute : TRunOffOutflowRoute;
begin
  try
    LOutflowRoute := TRunOffOutflowRoute.Create;
    LOutflowRoute.RouteNo := ARouteNo;
    LOutflowRoute.OutflowPercentage := AOutflowPercentage;
    FOutflowRoutes.Add(LOutflowRoute);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_NoOfOutflowRoutes: Integer;
const OPNAME = 'TRunOffModule.Get_NoOfOutflowRoutes';
begin
  Result := 0;
  try
    Result := FOutflowRoutes.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_OutflowRouteByIndex(AIndex: Integer): IRunOffOutflowRoute;
const OPNAME = 'TRunOffModule.Get_OutflowRouteByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FOutflowRoutes.Count)) then
      Result := TRunOffOutflowRoute(FOutflowRoutes.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.Get_OutflowRouteByRouteNo(ARouteNo: Integer): IRunOffOutflowRoute;
const OPNAME = 'TRunOffModule.Get_OutflowRouteByRouteNo';
begin
  Result := nil;
  try
    Result := FindOutFlowRouteByRouteNo(ARouteNo);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule.FindOutFlowRouteByRouteNo(ARouteNo: Integer): TRunOffOutflowRoute;
const OPNAME = 'TRunOffModule.FindOutFlowRouteByRouteNo';
var
  LOutflowRoute : TRunOffOutflowRoute;
  LIndex        : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FOutflowRoutes.Count)) do
    begin
      LOutflowRoute := TRunOffOutflowRoute(FOutflowRoutes.Items[LIndex]);
      if (LOutflowRoute.RouteNo = ARouteNo) then
        Result := LOutflowRoute
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule.Get_NoOfSlaves: Integer;
const OPNAME = 'TRunOffModule.Get_NoOfSlaves';
begin
  Result := 0;
  try
    Result := FSlaveModuleNumbers.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule.SlaveModuleNoByIndex (AIndex : Integer): Integer;
const OPNAME = 'TRunOffModule.SlaveModuleNoByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FSlaveModuleNumbers.Count)) then
      Result := StrToInt(FSlaveModuleNumbers.Strings[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffModule.AddSlaveModuleNo(ASlaveModuleNo: Integer);
const OPNAME = 'TRunOffModule.AddSlaveModuleNo';
begin
  try
    FSlaveModuleNumbers.Add(IntToStr(ASlaveModuleNo));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule.Get_GroundWaterAbstraction: IRunOffGroundWaterAbstraction;
const OPNAME = 'TRunOffModule.Get_GroundWaterAbstraction';
begin
  Result := nil;
  try
    Result := FGroundWaterAbstraction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModule.UpdatePropertiesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdatePropertiesData';
var
  LActive              : String;
  LLatitude            : Double;
  LLongitude           : Double;
  LRunOffName          : String;
  LVersionNo           : Integer;
  LCatchmentMAP        : Double;
  LCatchmentArea       : Double;
  LRainfallFileName    : String;
  LProduceNatFlows     : Integer;
  LPropertiesNode      : IXMLNode;
  LAPanDataListNode    : IXMLNode;
  LDataNode            : IXMLNode;
  LIndex               : Integer;
  LMonth               : Integer;
  LAPanFactor          : TMonthlyDoubleArray;
begin
  Result := FALSE;
  try
    LPropertiesNode := ARootNode.ChildNodes['RunOffProperties'];
    LActive              := LPropertiesNode.ChildNodes['Active'].Text;
    LLatitude            := StrToFloat(LPropertiesNode.ChildNodes['Latitude'].Text);
    LLongitude           := StrToFloat(LPropertiesNode.ChildNodes['Longitude'].Text);
    LRunOffName          := LPropertiesNode.ChildNodes['RunOffName'].Text;
    LVersionNo           := StrToInt(LPropertiesNode.ChildNodes['VersionNo'].Text);
    LCatchmentArea       := StrToFloat(LPropertiesNode.ChildNodes['CatchmentArea'].Text);
    LCatchmentMAP        := StrToFloat(LPropertiesNode.ChildNodes['CatchmentMAP'].Text);
    LRainfallFileName    := LPropertiesNode.ChildNodes['RainfallFileName'].Text;
    LProduceNatFlows     := StrToInt(LPropertiesNode.ChildNodes['ProduceNaturalisedFlows'].Text);

    LAPanDataListNode := LPropertiesNode.ChildNodes['APanDataList'];
    for LIndex := 1 to LAPanDataListNode.ChildNodes.Count do
    begin
      LDataNode           := LAPanDataListNode.ChildNodes.Get(LIndex-1);
      LMonth              := StrToInt(LDataNode.ChildNodes['Month'].Text);
      LAPanFactor[LMonth] := StrToFloat(LDataNode.ChildNodes['APanFactor'].Text);
    end;

    if (GRunOffDBManager.UpdatePropertiesDataInDB
                          (FModuleID, LActive, LLatitude, LLongitude, LRunOffName, LVersionNo, LCatchmentMAP,
                           LCatchmentArea, LRainfallFileName, LProduceNatFlows, LAPanFactor)) then
    begin
      FLatitude                := LLatitude;
      FLongitude               := LLongitude;
      FActive                  := LActive;
      FRunOffName              := LRunOffName;
      FVersionNo               := LVersionNo;
      FCatchmentArea           := LCatchmentArea;
      FCatchmentMAP            := LCatchmentMAP;
      FRainfallFileName        := LRainfallFileName;
      FProduceNaturalisedFlows := LProduceNatFlows;
      for LIndex := 1 to 12 do
        FAPanFactor[LIndex] := LAPanFactor[LIndex];
      Result := TRUE;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateSlavesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateSlavesData';
var
  LListNode      : IXMLNode;
  LDataNode      : IXMLNode;
  LIndex         : Integer;
  LSlaveNoList   : TStringList;
begin
  Result := TRUE;
  try
    LListNode := ARootNode.ChildNodes['RunOffSlaves'];

    LSlaveNoList := TStringList.Create;
    try
      LListNode := LListNode.ChildNodes['DataList'];
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode      := LListNode.ChildNodes.Get(LIndex-1);
        LSlaveNoList.Add(LDataNode.ChildNodes['SlaveModuleNo'].Text);
      end;

      Result := GRunOffDBManager.UpdateSlavesDataInDB(FModuleID, LSlaveNoList);

      if (Result) then
        Result := GRunOffDBManager.LoadRunOffSlavesFromDB(Self);
    finally
      LSlaveNoList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdatePitmanData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdatePitmanData';
var
  LPitmanPOW           : Double;
  LPitmanSL            : Integer;
  LPitmanST            : Integer;
  LPitmanFT            : Double;
  LPitmanGW            : Double;
  LPitmanZMIN          : Integer;
  LPitmanZMAX          : Integer;
  LPitmanPI            : Double;
  LPitmanTL            : Double;
  LPitmanGL            : Double;
  LPitmanR             : Double;
  LPitmanFF            : Double;
  LRunOffPitmanNode    : IXMLNode;
begin
  Result := FALSE;
  try
    LRunOffPitmanNode    := ARootNode.ChildNodes['RunOffPitman'];
    LPitmanPOW           := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanPOW'].Text);
    LPitmanSL            := StrToInt(LRunOffPitmanNode.ChildNodes['PitmanSL'].Text);
    LPitmanST            := StrToInt(LRunOffPitmanNode.ChildNodes['PitmanST'].Text);
    LPitmanFT            := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanFT'].Text);
    LPitmanGW            := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanGW'].Text);
    LPitmanZMIN          := StrToInt(LRunOffPitmanNode.ChildNodes['PitmanZMIN'].Text);
    LPitmanZMAX          := StrToInt(LRunOffPitmanNode.ChildNodes['PitmanZMAX'].Text);
    LPitmanPI            := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanPI'].Text);
    LPitmanTL            := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanTL'].Text);
    LPitmanGL            := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanGL'].Text);
    LPitmanR             := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanR'].Text);
    LPitmanFF            := StrToFloat(LRunOffPitmanNode.ChildNodes['PitmanFF'].Text);

    if (GRunOffDBManager.UpdatePitmanDataInDB
                                 (FModuleID, LPitmanPOW, LPitmanSL, LPitmanST, LPitmanFT, LPitmanGW,
                                  LPitmanZMIN, LPitmanZMAX, LPitmanPI, LPitmanTL, LPitmanGL, LPitmanR,
                                  LPitmanFF)) then
    begin
      FPitmanModel.FPOW  := LPitmanPOW;
      FPitmanModel.FSL   := LPitmanSL;
      FPitmanModel.FST   := LPitmanST;
      FPitmanModel.FFT   := LPitmanFT;
      FPitmanModel.FGW   := LPitmanGW;
      FPitmanModel.FZMIN := LPitmanZMIN;
      FPitmanModel.FZMAX := LPitmanZMAX;
      FPitmanModel.FPI   := LPitmanPI;
      FPitmanModel.FTL   := LPitmanTL;
      FPitmanModel.FGL   := LPitmanGL;
      FPitmanModel.FR    := LPitmanR;
      FPitmanModel.FFF   := LPitmanFF;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateAfforestationData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateAfforestationData';
var
  LAfforestationAlgorithm    : Integer;
  LPineAreaPercentage        : Double;
  LPineRotationPeriod        : Integer;
  LEucalyptusAreaPercentage  : Double;
  LEucalyptusRotationPeriod  : Integer;
  LWattleAreaPercentage      : Double;
  LWattleRotationPeriod      : Integer;
  LOptimalAreaPercentage     : Double;
  LSFRReductionMAR           : Double;
  LSFRReductionLowFlows      : Double;
  LAfforestationNode         : IXMLNode;
  LListNode                  : IXMLNode;
  LDataNode                  : IXMLNode;
  LIndex                     : Integer;
  LYearList                  : TStringList;
  LAreaList                  : TStringList;
begin
  Result := FALSE;
  try
    LAfforestationNode        := ARootNode.ChildNodes['RunOffAfforestation'];
    LAfforestationAlgorithm   := StrToInt(LAfforestationNode.ChildNodes['AfforestationAlgorithm'].Text);
    LPineAreaPercentage       := StrToFloat(LAfforestationNode.ChildNodes['PineAreaPercentage'].Text);
    LPineRotationPeriod       := StrToInt(LAfforestationNode.ChildNodes['PineRotationPeriod'].Text);
    LEucalyptusAreaPercentage := StrToFloat(LAfforestationNode.ChildNodes['EucalyptusAreaPercentage'].Text);
    LEucalyptusRotationPeriod := StrToInt(LAfforestationNode.ChildNodes['EucalyptusRotationPeriod'].Text);
    LWattleAreaPercentage     := StrToFloat(LAfforestationNode.ChildNodes['WattleAreaPercentage'].Text);
    LWattleRotationPeriod     := StrToInt(LAfforestationNode.ChildNodes['WattleRotationPeriod'].Text);
    LOptimalAreaPercentage    := StrToFloat(LAfforestationNode.ChildNodes['OptimalAreaPercentage'].Text);
    LSFRReductionMAR          := StrToFloat(LAfforestationNode.ChildNodes['SFRReductionMAR'].Text);
    LSFRReductionLowFlows     := StrToFloat(LAfforestationNode.ChildNodes['SFRReductionLowFlows'].Text);

    LYearList := TStringList.Create;
    LAreaList := TStringList.Create;
    try
      LListNode := LAfforestationNode.ChildNodes['DataList'];
      for LIndex := 0 to LListNode.ChildNodes.Count - 1 do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LAreaList.Add(LDataNode.ChildNodes['Area'].Text);
      end;

      Result := GRunOffDBManager.UpdateAfforestationDataInDB
                                  (FModuleID, LAfforestationAlgorithm, LPineAreaPercentage, LPineRotationPeriod,
                                   LEucalyptusAreaPercentage, LEucalyptusRotationPeriod, LWattleAreaPercentage,
                                   LWattleRotationPeriod, LOptimalAreaPercentage, LSFRReductionMAR,
                                   LSFRReductionLowFlows, LYearList, LAreaList);
      if (Result) then
      begin
        FAfforestation.FAlgorithm                := LAfforestationAlgorithm;
        FAfforestation.FPineAreaPercentage       := LPineAreaPercentage;
        FAfforestation.FPineRotationPeriod       := LPineRotationPeriod;
        FAfforestation.FEucalyptusAreaPercentage := LEucalyptusAreaPercentage;
        FAfforestation.FEucalyptusRotationPeriod := LEucalyptusRotationPeriod;
        FAfforestation.FWattleAreaPercentage     := LWattleAreaPercentage;
        FAfforestation.FWattleRotationPeriod     := LWattleRotationPeriod;
        FAfforestation.FOptimalAreaPercentage    := LOptimalAreaPercentage;
        FAfforestation.FSFRReductionMAR          := LSFRReductionMAR;
        FAfforestation.FSFRReductionLowFlows     := LSFRReductionLowFlows;

        Result := GRunOffDBManager.LoadAfforestationFromDB(Self);
      end;
    finally
      LYearList.Free;
      LAreaList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateAlienVegetationData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateAlienVegetationData';
var
  LAlienVegetationAlgorithm  : Integer;
  LRiparianVegetationArea    : Double;
  LTallTreeAreaPercentage    : Double;
  LTallTreeAge               : Double;
  LMediumTreeAreaPercentage  : Double;
  LMediumTreeAge             : Double;
  LTallSchrubAreaPercentage  : Double;
  LTallSchrubAge             : Double;
  LOptimalAreaPercentage     : Double;
  LAlienVegetationNode       : IXMLNode;
  LListNode                  : IXMLNode;
  LDataNode                  : IXMLNode;
  LIndex                     : Integer;
  LYearList                  : TStringList;
  LAreaList                  : TStringList;
begin
  Result := FALSE;
  try
    LAlienVegetationNode       := ARootNode.ChildNodes['RunOffAlienVegetation'];
    LAlienVegetationAlgorithm  := StrToInt(LAlienVegetationNode.ChildNodes['AlienVegetationAlgorithm'].Text);
    LRiparianVegetationArea    := StrToFloat(LAlienVegetationNode.ChildNodes['RiparianVegetationArea'].Text);
    LTallTreeAreaPercentage    := StrToFloat(LAlienVegetationNode.ChildNodes['TallTreeAreaPercentage'].Text);
    LTallTreeAge               := StrToFloat(LAlienVegetationNode.ChildNodes['TallTreeAge'].Text);
    LMediumTreeAreaPercentage  := StrToFloat(LAlienVegetationNode.ChildNodes['MediumTreeAreaPercentage'].Text);
    LMediumTreeAge             := StrToFloat(LAlienVegetationNode.ChildNodes['MediumTreeAge'].Text);
    LTallSchrubAreaPercentage  := StrToFloat(LAlienVegetationNode.ChildNodes['TallSchrubAreaPercentage'].Text);
    LTallSchrubAge             := StrToFloat(LAlienVegetationNode.ChildNodes['TallSchrubAge'].Text);
    LOptimalAreaPercentage     := StrToFloat(LAlienVegetationNode.ChildNodes['OptimalAreaPercentage'].Text);

    LYearList := TStringList.Create;
    LAreaList := TStringList.Create;
    try
      LListNode := LAlienVegetationNode.ChildNodes['DataList'];
      for LIndex := 0 to LListNode.ChildNodes.Count - 1 do
      begin
        LDataNode := LListNode.ChildNodes.Get(LIndex);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LAreaList.Add(LDataNode.ChildNodes['Area'].Text);
      end;

      Result := GRunOffDBManager.UpdateAlienVegetationDataInDB
                                  (FModuleID, LAlienVegetationAlgorithm, LRiparianVegetationArea,
                                   LTallTreeAreaPercentage, LTallTreeAge, LMediumTreeAreaPercentage,
                                   LMediumTreeAge, LTallSchrubAreaPercentage, LTallSchrubAge,
                                   LOptimalAreaPercentage, LYearList, LAreaList);

      if (Result) then
      begin
        FAlienVegetation.FAlgorithm                := LAlienVegetationAlgorithm;
        FAlienVegetation.FRiparianVegetationArea   := LRiparianVegetationArea;
        FAlienVegetation.FTallTreeAreaPercentage   := LTallTreeAreaPercentage;
        FAlienVegetation.FTallTreeAge              := LTallTreeAge;
        FAlienVegetation.FMediumTreeAreaPercentage := LMediumTreeAreaPercentage;
        FAlienVegetation.FMediumTreeAge            := LMediumTreeAge;
        FAlienVegetation.FTallSchrubAreaPercentage := LTallSchrubAreaPercentage;
        FAlienVegetation.FTallSchrubAge            := LTallSchrubAge;
        FAlienVegetation.FOptimalAreaPercentage    := LOptimalAreaPercentage;

        Result := GRunOffDBManager.LoadAlienVegetationFromDB(Self);
      end;
    finally
      LYearList.Free;
      LAreaList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdatePavedAreaData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdatePavedAreaData';
var
  LPavedAreaNode  : IXMLNode;
  LListNode       : IXMLNode;
  LDataNode       : IXMLNode;
  LIndex          : Integer;
  LYearList       : TStringList;
  LProportionList : TStringList;
begin
  Result := FALSE;
  try
    LPavedAreaNode := ARootNode.ChildNodes['RunOffPavedArea'];

    LYearList := TStringList.Create;
    LProportionList := TStringList.Create;
    try
      LListNode      := LPavedAreaNode.ChildNodes['DataList'];
      for LIndex := 0 to LListNode.ChildNodes.Count - 1 do
      begin
        LDataNode   := LListNode.ChildNodes.Get(LIndex);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LProportionList.Add(LDataNode.ChildNodes['Proportion'].Text);
      end;

      Result := GRunOffDBManager.UpdatePavedAreaDataInDB(FModuleID, LYearList, LProportionList);

      if (Result) then
        Result := GRunOffDBManager.LoadPavedAreaFromDB(Self);
    finally
      LYearList.Free;
      LProportionList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateGroundWaterAbstractionData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateGroundWaterAbstractionData';
var
  LGroundWaterAbstractionNode : IXMLNode;
  LListNode                   : IXMLNode;
  LDataNode                   : IXMLNode;
  LIndex                      : Integer;
  LYearList                   : TStringList;
  LAbstractionList            : TStringList;
begin
  Result := FALSE;
  try
    LGroundWaterAbstractionNode := ARootNode.ChildNodes['RunOffGroundWaterAbstraction'];

    LYearList        := TStringList.Create;
    LAbstractionList := TStringList.Create;
    try
      LListNode      := LGroundWaterAbstractionNode.ChildNodes['DataList'];
      for LIndex := 0 to LListNode.ChildNodes.Count - 1 do
      begin
        LDataNode    := LListNode.ChildNodes.Get(LIndex);
        LYearList.Add(LDataNode.ChildNodes['Year'].Text);
        LAbstractionList.Add(LDataNode.ChildNodes['Abstraction'].Text);
      end;

      Result := GRunOffDBManager.UpdateGroundwaterAbstractionDataInDB(FModuleID, LYearList, LAbstractionList);

      if (Result) then
        Result := GRunOffDBManager.LoadGroundWaterAbstractionFromDB(Self);
    finally
      LYearList.Free;
      LAbstractionList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateOutflowRoutesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateOutflowRoutesData';
var
  LListNode          : IXMLNode;
  LDataNode          : IXMLNode;
  LIndex             : Integer;
  LOutflowRouteNo    : String;
  LOutflowPercentage : String;
  LRouteNoList       : TStringList;
  LPercentageList    : TStringList;
begin
  Result := FALSE;
  try
    LListNode := ARootNode.ChildNodes['RunOffOutflowRoutes'];

    LRouteNoList    := TStringList.Create;
    LPercentageList := TStringList.Create;
    try
      LListNode := LListNode.ChildNodes['DataList'];
      for LIndex := 1 to LListNode.ChildNodes.Count do
      begin
        LDataNode          := LListNode.ChildNodes.Get(LIndex-1);
        LRouteNoList.Add(LDataNode.ChildNodes['RouteNo'].Text);
        LPercentageList.Add(LDataNode.ChildNodes['OutflowPercentage'].Text);
      end;

      Result := GRunOffDBManager.UpdateOutflowRoutesDataInDB(FModuleID, LRouteNoList, LPercentageList);

      if (Result) then
      begin
        for LIndex := 0 to LRouteNoList.Count - 1 do
        begin
          LOutflowRouteNo    := LRouteNoList.Strings[LIndex];
          LOutflowPercentage := LPercentageList.Strings[LIndex];
          OutflowRouteByRouteNo[StrToInt(LOutflowRouteNo)].OutflowPercentage := StrToFloat(LOutflowPercentage);
        end;
      end;

    finally
      LRouteNoList.Free;
      LPercentageList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateHughesData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateHughesData';
var
  LInflowRouteNo                 : Integer;
  LInfluenceROMNo                : Integer;
  LGroundWaterModel              : Integer;
  LHughesHGSL                    : Double;
  LHughesGPOW                    : Double;
  LHughesTLGMax                  : Double;
  LHughesHGGW                    : Double;
  LHughesPOW                     : Double;
  LHughesSL                      : Integer;
  LHughesST                      : Integer;
  LHughesFT                      : Double;
  LHughesGW                      : Double;
  LHughesZMIN                    : Integer;
  LHughesZMAX                    : Integer;
  LHughesPI                      : Double;
  LHughesTL                      : Double;
  LHughesGL                      : Double;
  LHughesR                       : Double;
  LHughesFF                      : Double;
  LUseNoOfReaches                : Integer;
  LDrainageDensity               : Double;
  LNumberOfReaches               : Integer;
  LRiparianAreaWidthPercentage   : Double;
  LRiparianStripFactor           : Double;
  LRestWaterlevel                : Double;
  LTransmissivity                : Double;
  LStorativity                   : Double;
  LGroundwaterSlope              : Double;
  LAnnualUpperZoneAbstraction    : Double;
  LAnnualRiparianZoneAbstraction : Double;
  LAbstractionDataNode           : IXMLNode;
  LDataNode                      : IXMLNode;
  LIndex                         : Integer;
  LMonth                         : Integer;
  LUpperZoneDemand               : TMonthlyDoubleArray;
  LRiparianZoneDemand            : TMonthlyDoubleArray;
  //LFieldName                     : String;
  LHughesNode                    : IXMLNode;
begin
  Result := TRUE;
  try
    LHughesNode                    := ARootNode.ChildNodes['RunOffHughes'];
    LInflowRouteNo                 := StrToInt(LHughesNode.ChildNodes['InflowRouteNo'].Text);
    LInfluenceROMNo                := StrToInt(LHughesNode.ChildNodes['InfluenceROMNo'].Text);
    LGroundWaterModel              := StrToInt(LHughesNode.ChildNodes['GroundWaterModel'].Text);
    LHughesHGSL                    := StrToFloat(LHughesNode.ChildNodes['HughesHGSL'].Text);
    LHughesGPOW                    := StrToFloat(LHughesNode.ChildNodes['HughesGPOW'].Text);
    LHughesTLGMax                  := StrToFloat(LHughesNode.ChildNodes['HughesTLGMax'].Text);
    LHughesHGGW                    := StrToFloat(LHughesNode.ChildNodes['HughesHGGW'].Text);
    LHughesPOW                     := StrToFloat(LHughesNode.ChildNodes['HughesPOW'].Text);
    LHughesSL                      := StrToInt(LHughesNode.ChildNodes['HughesSL'].Text);
    LHughesST                      := StrToInt(LHughesNode.ChildNodes['HughesST'].Text);
    LHughesFT                      := StrToFloat(LHughesNode.ChildNodes['HughesFT'].Text);
    LHughesGW                      := StrToFloat(LHughesNode.ChildNodes['HughesGW'].Text);
    LHughesZMIN                    := StrToInt(LHughesNode.ChildNodes['HughesZMIN'].Text);
    LHughesZMAX                    := StrToInt(LHughesNode.ChildNodes['HughesZMAX'].Text);
    LHughesPI                      := StrToFloat(LHughesNode.ChildNodes['HughesPI'].Text);
    LHughesTL                      := StrToFloat(LHughesNode.ChildNodes['HughesTL'].Text);
    LHughesGL                      := StrToFloat(LHughesNode.ChildNodes['HughesGL'].Text);
    LHughesR                       := StrToFloat(LHughesNode.ChildNodes['HughesR'].Text);
    LHughesFF                      := StrToFloat(LHughesNode.ChildNodes['HughesFF'].Text);
    LUseNoOfReaches                := StrToInt(LHughesNode.ChildNodes['UseNoOfReaches'].Text);
    LDrainageDensity               := StrToFloat(LHughesNode.ChildNodes['DrainageDensity'].Text);
    LNumberOfReaches               := StrToInt(LHughesNode.ChildNodes['NumberOfReaches'].Text);
    LRiparianAreaWidthPercentage   := StrToFloat(LHughesNode.ChildNodes['RiparianAreaWidthPercentage'].Text);
    LRiparianStripFactor           := StrToFloat(LHughesNode.ChildNodes['RiparianStripFactor'].Text);
    LRestWaterlevel                := StrToFloat(LHughesNode.ChildNodes['RestWaterlevel'].Text);
    LTransmissivity                := StrToFloat(LHughesNode.ChildNodes['Transmissivity'].Text);
    LStorativity                   := StrToFloat(LHughesNode.ChildNodes['Storativity'].Text);
    LGroundwaterSlope              := StrToFloat(LHughesNode.ChildNodes['GroundwaterSlope'].Text);
    LAnnualUpperZoneAbstraction    := StrToFloat(LHughesNode.ChildNodes['AnnualUpperZoneAbstraction'].Text);
    LAnnualRiparianZoneAbstraction := StrToFloat(LHughesNode.ChildNodes['AnnualRiparianZoneAbstraction'].Text);

    LAbstractionDataNode := LHughesNode.ChildNodes['DataList'];
    for LIndex := 1 to LAbstractionDataNode.ChildNodes.Count do
    begin
      LDataNode        := LAbstractionDataNode.ChildNodes.Get(LIndex-1);
      LMonth           := StrToInt(LDataNode.ChildNodes['Month'].Text);
      LUpperZoneDemand[LMonth]   := StrToFloat(LDataNode.ChildNodes['UpperZone'].Text);
      LRiparianZoneDemand[LMonth]:= StrToFloat(LDataNode.ChildNodes['RiparianZone'].Text);
    end;

    Result := GRunOffDBManager.UpdateHughesDataInDB
                                (FModuleID, LInflowRouteNo, LInfluenceROMNo, LGroundWaterModel,
                                 LHughesHGSL, LHughesGPOW, LHughesTLGMax, LHughesHGGW, LHughesPOW,
                                 LHughesSL, LHughesST, LHughesFT, LHughesGW, LHughesZMIN, LHughesZMAX,
                                 LHughesPI, LHughesTL, LHughesGL, LHughesR, LHughesFF, LUseNoOfReaches,
                                 LDrainageDensity, LNumberOfReaches, LRiparianAreaWidthPercentage,
                                 LRiparianStripFactor, LRestWaterlevel, LTransmissivity, LStorativity,
                                 LGroundwaterSlope, LAnnualUpperZoneAbstraction, LAnnualRiparianZoneAbstraction,
                                 LUpperZoneDemand, LRiparianZoneDemand);

    if (Result) then
    begin
      FHughesModel.FInflowRouteNo                := LInflowRouteNo;
      FHughesModel.FInfluenceROMNo               := LInfluenceROMNo;
      FHughesModel.FGroundWaterModel             := LGroundWaterModel;
      FHughesModel.FHGSL                         := LHughesHGSL;
      FHughesModel.FGPOW                         := LHughesGPOW;
      FHughesModel.FTLGMax                       := LHughesTLGMax;
      FHughesModel.FHGGW                         := LHughesHGGW;
      FHughesModel.FPOW                          := LHughesPOW;
      FHughesModel.FSL                           := LHughesSL;
      FHughesModel.FST                           := LHughesST;
      FHughesModel.FFT                           := LHughesFT;
      FHughesModel.FGW                           := LHughesGW;
      FHughesModel.FZMIN                         := LHughesZMIN;
      FHughesModel.FZMAX                         := LHughesZMAX;
      FHughesModel.FPI                           := LHughesPI;
      FHughesModel.FTL                           := LHughesTL;
      FHughesModel.FGL                           := LHughesGL;
      FHughesModel.FR                            := LHughesR;
      FHughesModel.FFF                           := LHughesFF;
      FHughesModel.UseNoOfReaches                := LUseNoOfReaches;
      FHughesModel.DrainageDensity               := LDrainageDensity;
      FHughesModel.NoOfReaches                   := LNumberOfReaches;
      FHughesModel.RiparianAreaWidthPercentage   := LRiparianAreaWidthPercentage;
      FHughesModel.RiparianStripFactor           := LRiparianStripFactor;
      FHughesModel.RestWaterLevel                := LRestWaterlevel;
      FHughesModel.Transmissivity                := LTransmissivity;
      FHughesModel.Storativity                   := LStorativity;
      FHughesModel.GroundWaterSlope              := LGroundwaterSlope;
      FHughesModel.AnnualUpperZoneAbstraction    := LAnnualUpperZoneAbstraction;
      FHughesModel.AnnualRiparianZoneAbstraction := LAnnualRiparianZoneAbstraction;
      for LIndex := 1 to 12 do
      begin
        FHughesModel.FMonthlyUpperZoneAbstraction[LIndex] := LUpperZoneDemand[LIndex];
        FHughesModel.FMonthlyRiparianZoneAbstraction[LIndex] := LRiparianZoneDemand[LIndex];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffModule.UpdateSamiData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TRunOffModule.UpdateSamiData';
var
  LSamiNode                      : IXMLNode;
  LAquiferThickness              : Double;
  LStorativity                   : Double;
  LInitialAquiferStorage         : Double;
  LStaticWaterLevel              : Double;
  LUnsaturatedStorage            : Double;
  LInitialUnsaturatedZoneStorage : Double;
  LPerculationPower              : Double;
  LMaxDischarge                  : Double;
  LInteractionCurvePower         : Double;
  LMaxHydrologicalGradient       : Double;
  LTransmissivity                : Double;
  LBoreholeDistanceToRiver       : Double;
  LGroundWaterEvaporationArea    : Double;
  LInterflowLag                  : Double;
  LRechargeAveragedNoMonths      : Double;
  LUseAbstractions               : Integer;
  LSamiGPOW                      : Double;
  LSamiHGSL                      : Double;
  LSamiHGGW                      : Double;
  LSamiK2                        : Double;
  LSamiK3                        : Double;
  LSamiPOW                       : Double;
  LSamiSL                        : Integer;
  LSamiST                        : Integer;
  LSamiFT                        : Double;
  LSamiGW                        : Double;
  LSamiZMIN                      : Integer;
  LSamiZMAX                      : Integer;
  LSamiPI                        : Double;
  LSamiTL                        : Double;
  LSamiGL                        : Double;
  LSamiR                         : Double;
  LSamiFF                        : Double;
begin
  Result := TRUE;
  try
    LSamiNode                      := ARootNode.ChildNodes['RunOffSami'];
    LAquiferThickness              := StrToFloat(LSamiNode.ChildNodes['AquiferThickness'].Text);
    LStorativity                   := StrToFloat(LSamiNode.ChildNodes['Storativity'].Text);
    LInitialAquiferStorage         := StrToFloat(LSamiNode.ChildNodes['InitialAquiferStorage'].Text);
    LStaticWaterLevel              := StrToFloat(LSamiNode.ChildNodes['StaticWaterLevel'].Text);
    LUnsaturatedStorage            := StrToFloat(LSamiNode.ChildNodes['UnsaturatedStorage'].Text);
    LInitialUnsaturatedZoneStorage := StrToFloat(LSamiNode.ChildNodes['InitialUnsaturatedZoneStorage'].Text);
    LPerculationPower              := StrToFloat(LSamiNode.ChildNodes['PerculationPower'].Text);
    LMaxDischarge                  := StrToFloat(LSamiNode.ChildNodes['MaxDischarge'].Text);
    LInteractionCurvePower         := StrToFloat(LSamiNode.ChildNodes['InteractionCurvePower'].Text);
    LMaxHydrologicalGradient       := StrToFloat(LSamiNode.ChildNodes['MaxHydrologicalGradient'].Text);
    LTransmissivity                := StrToFloat(LSamiNode.ChildNodes['Transmissivity'].Text);
    LBoreholeDistanceToRiver       := StrToFloat(LSamiNode.ChildNodes['BoreholeDistanceToRiver'].Text);
    LGroundWaterEvaporationArea    := StrToFloat(LSamiNode.ChildNodes['GroundWaterEvaporationArea'].Text);
    LInterflowLag                  := StrToFloat(LSamiNode.ChildNodes['InterflowLag'].Text);
    LRechargeAveragedNoMonths      := StrToFloat(LSamiNode.ChildNodes['RechargeAveragedNoMonths'].Text);
    LUseAbstractions               := StrToInt(LSamiNode.ChildNodes['UseAbstractions'].Text);
    LSamiGPOW                      := StrToFloat(LSamiNode.ChildNodes['SamiGPOW'].Text);
    LSamiHGSL                      := StrToFloat(LSamiNode.ChildNodes['SamiHGSL'].Text);
    LSamiHGGW                      := StrToFloat(LSamiNode.ChildNodes['SamiHGGW'].Text);
    LSamiK2                        := StrToFloat(LSamiNode.ChildNodes['SamiK2'].Text);
    LSamiK3                        := StrToFloat(LSamiNode.ChildNodes['SamiK3'].Text);
    LSamiPOW                       := StrToFloat(LSamiNode.ChildNodes['SamiPOW'].Text);
    LSamiSL                        := StrToInt(LSamiNode.ChildNodes['SamiSL'].Text);
    LSamiST                        := StrToInt(LSamiNode.ChildNodes['SamiST'].Text);
    LSamiFT                        := StrToFloat(LSamiNode.ChildNodes['SamiFT'].Text);
    LSamiGW                        := StrToFloat(LSamiNode.ChildNodes['SamiGW'].Text);
    LSamiZMIN                      := StrToInt(LSamiNode.ChildNodes['SamiZMIN'].Text);
    LSamiZMAX                      := StrToInt(LSamiNode.ChildNodes['SamiZMAX'].Text);
    LSamiPI                        := StrToFloat(LSamiNode.ChildNodes['SamiPI'].Text);
    LSamiTL                        := StrToFloat(LSamiNode.ChildNodes['SamiTL'].Text);
    LSamiGL                        := StrToFloat(LSamiNode.ChildNodes['SamiGL'].Text);
    LSamiR                         := StrToFloat(LSamiNode.ChildNodes['SamiR'].Text);
    LSamiFF                        := StrToFloat(LSamiNode.ChildNodes['SamiFF'].Text);

    Result := GRunOffDBManager.UpdateSamiDataInDB
                                (FModuleID, LAquiferThickness, LStorativity, LInitialAquiferStorage,
                                 LStaticWaterLevel, LUnsaturatedStorage, LInitialUnsaturatedZoneStorage,
                                 LPerculationPower, LMaxDischarge, LInteractionCurvePower, LMaxHydrologicalGradient,
                                 LTransmissivity, LBoreholeDistanceToRiver, LGroundWaterEvaporationArea,
                                 LInterflowLag, LRechargeAveragedNoMonths, LUseAbstractions, LSamiGPOW,
                                 LSamiHGSL, LSamiHGGW, LSamiK2, LSamiK3, LSamiPOW, LSamiSL, LSamiST, LSamiFT,
                                 LSamiGW, LSamiZMIN, LSamiZMAX, LSamiPI, LSamiTL, LSamiGL, LSamiR, LSamiFF);
    if (Result) then
    begin
      FSamiModel.AquiferThickness              := LAquiferThickness;
      FSamiModel.Storativity                   := LStorativity;
      FSamiModel.InitialAquiferStorage         := LInitialAquiferStorage;
      FSamiModel.StaticWaterLevel              := LStaticWaterLevel;
      FSamiModel.UnsaturatedStorage            := LUnsaturatedStorage;
      FSamiModel.InitialUnsaturatedZoneStorage := LInitialUnsaturatedZoneStorage;
      FSamiModel.PerculationPower              := LPerculationPower;
      FSamiModel.MaxDischarge                  := LMaxDischarge;
      FSamiModel.InteractionCurvePower         := LInteractionCurvePower;
      FSamiModel.MaxHydrologicalGradient       := LMaxHydrologicalGradient;
      FSamiModel.Transmissivity                := LTransmissivity;
      FSamiModel.BoreholeDistanceToRiver       := LBoreholeDistanceToRiver;
      FSamiModel.GroundWaterEvaporationArea    := LGroundWaterEvaporationArea;
      FSamiModel.InterflowLag                  := LInterflowLag;
      FSamiModel.RechargeAveragedNoMonths      := LRechargeAveragedNoMonths;
      FSamiModel.UseAbstractions               := LUseAbstractions;
      FSamiModel.FGPOW                         := LSamiGPOW;
      FSamiModel.FHGSL                         := LSamiHGSL;
      FSamiModel.FHGGW                         := LSamiHGGW;
      FSamiModel.FK2                           := LSamiK2;
      FSamiModel.FK3                           := LSamiK3;
      FSamiModel.FPOW                          := LSamiPOW;
      FSamiModel.FSL                           := LSamiSL;
      FSamiModel.FST                           := LSamiST;
      FSamiModel.FFT                           := LSamiFT;
      FSamiModel.FGW                           := LSamiGW;
      FSamiModel.FZMIN                         := LSamiZMIN;
      FSamiModel.FZMAX                         := LSamiZMAX;
      FSamiModel.FPI                           := LSamiPI;
      FSamiModel.FTL                           := LSamiTL;
      FSamiModel.FGL                           := LSamiGL;
      FSamiModel.FR                            := LSamiR;
      FSamiModel.FFF                           := LSamiFF;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.ClearSlaveModuleNumbers;
const OPNAME = 'TRunOffModule.ClearSlaveModuleNumbers';
begin
  try
    FSlaveModuleNumbers.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.ClearAfforestationAreaData;
const OPNAME = 'TRunOffModule.ClearAfforestationAreaData';
begin
  try
    FAfforestation.FYearlyData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.ClearAlienVegetationAreaData;
const OPNAME = 'TRunOffModule.ClearAlienVegetationAreaData';
begin
  try
    FAlienVegetation.FYearlyData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.ClearPavedAreaData;
const OPNAME = 'TRunOffModule.ClearPavedAreaData';
begin
  try
    FPavedArea.FYearlyData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.ClearGroundwaterAbstractionData;
const OPNAME = 'TRunOffModule.ClearGroundwaterAbstractionData';
begin
  try
    FGroundWaterAbstraction.FYearlyData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffModule.ClearOutFlowRoutes;
const OPNAME = 'TRunOffModule.ClearOutFlowRoutes';
var
  LOutflowRoute : TRunOffOutflowRoute;
begin
  try
    while (FOutflowRoutes.Count > 0) do
    begin
      LOutflowRoute := TRunOffOutflowRoute(FOutflowRoutes.Items[0]);
      FOutflowRoutes.Remove(LOutflowRoute);
      LOutflowRoute.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRunOffModuleAgent **********************************************************}

function TRunOffModuleAgent._AddRef: Integer;
const OPNAME = 'TRunOffModuleAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent._Release: Integer;
const OPNAME = 'TRunOffModuleAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffModuleAgent.CreateMemberObjects;
const OPNAME = 'TRunOffModuleAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    GRunOffDBManager := TRunOffDBManager.Create;
    GRunOffDBManager.ModuleAgent := Self;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunOffModuleAgent.DestroyMemberObjects;
const OPNAME = 'TRunOffModuleAgent.DestroyMemberObjects';
begin
  try
    GRunOffDBManager.ModuleAgent := nil;
    FreeAndNil(GRunOffDBManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.AddRunOffModule : TRunOffModule;
const OPNAME = 'TRunOffModuleAgent.AddRunOffModule';
var
  LRunOffModule : TRunOffModule;
begin
  Result := nil;
  try
    LRunOffModule := TRunOffModule.Create;
    FList.Add(LRunOffModule);
    Result := LRunOffModule;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.CreateNewRunOffModule (ANetworkID: Integer): IRunOffModule;
const OPNAME = 'TRunOffModuleAgent.CreateNewRunOffModule';
begin
  Result := nil;
  try
    Result := GRunOffDBManager.CreateNewRunOffModuleInDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.RemoveRunOffModule(AModuleNumber : Integer) : WordBool;
const OPNAME = 'TRunOffModuleAgent.RemoveRunOffModule';
var
  LRunOffModule : TRunOffModule;
  LIndex        : Integer;
begin
  Result := FALSE;
  try
    LRunOffModule := FindRunOffModuleByNumber(AModuleNumber);
    if (LRunOffModule <> nil) then
    begin
      if (GRunOffDBManager.RemoveRunOffModuleFromDB(LRunOffModule.ModuleID)) then
      begin
        Result := TRUE;
        LIndex := FList.IndexOf(LRunOffModule);
        FList.Delete(LIndex);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.Get_RunOffModuleCount: Integer;
const OPNAME = 'TRunOffModuleAgent.Get_RunOffModuleCount';
begin
  Result := 0;
  try
    Result := FList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.FindRunOffModuleByID (AModuleID: Integer): TRunOffModule;
const OPNAME = 'TRunOffModuleAgent.FindRunOffModuleByID';
var
  LRunOffModule : TRunOffModule;
  LIndex        : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LRunOffModule := TRunOffModule(FList.Items[LIndex]);
      if (LRunOffModule.ModuleID = AModuleID) then
        Result := LRunOffModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.Get_RunOffModuleByID(AModuleID: Integer): IRunOffModule;
const OPNAME = 'TRunOffModuleAgent.Get_RunOffModuleByID';
var
  LRunOffModule : TRunOffModule;
  LIndex        : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LRunOffModule := TRunOffModule(FList.Items[LIndex]);
      if (LRunOffModule.ModuleID = AModuleID) then
        Result := LRunOffModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.FindRunOffModuleByNumber (AModuleNumber : Integer): TRunOffModule;
const OPNAME = 'TRunOffModuleAgent.FindRunOffModuleByNumber';
var
  LRunOffModule : TRunOffModule;
  LIndex        : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LRunOffModule := TRunOffModule(FList.Items[LIndex]);
      if (LRunOffModule.ModuleNumber = AModuleNumber) then
        Result := LRunOffModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.FindRunOffModuleByIndex(AIndex: Integer): TRunOffModule;
const OPNAME = 'TRunOffModuleAgent.FindRunOffModuleByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TRunOffModule(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.Get_RunOffModuleByNumber (AModuleNumber : Integer): IRunOffModule;
const OPNAME = 'TRunOffModuleAgent.Get_RunOffModuleByNumber';
var
  LRunOffModule : TRunOffModule;
  LIndex        : Integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FList.Count)) do
    begin
      LRunOffModule := TRunOffModule(FList.Items[LIndex]);
      if (LRunOffModule.ModuleNumber = AModuleNumber) then
        Result := LRunOffModule
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.Get_RunOffModuleByIndex(AIndex: Integer): IRunOffModule;
const OPNAME = 'TRunOffModuleAgent.Get_RunOffModuleByIndex';
begin
  Result := nil;
  try
    if (AIndex < FList.Count) then
      Result := TRunOffModule(FList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffModuleAgent.LoadRunOffModules (ANetworkID : Integer) : Boolean;
const OPNAME = 'TRunOffModuleAgent.LoadRunOffModules';
begin
  Result := FALSE;
  try
    Result := GRunOffDBManager.LoadRunOffModulesFromDB(ANetworkID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
