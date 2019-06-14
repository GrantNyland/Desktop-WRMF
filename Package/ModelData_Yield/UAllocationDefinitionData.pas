{******************************************************************************}
{*  UNIT      : Contains TAllocationDefinition Class
{*  AUTHOR    : Sam Dhlamini(Cornastone)
{*  DATE      : 04/01/2006
{*  COPYRIGHT : Copyright © 2004 DWAF
{******************************************************************************}

unit UAllocationDefinitionData;

interface
uses
  SysUtils,
  VCL.Controls,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UYieldModelDataObject;

type

  TTree = class(TObject)
  protected
    FParent   : TTree;
    FText     : string;
    FChildren : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNode (AParent : TTree;
                      AText   : string) : TTree;
    function FindNode (AText : string) : TTree;
  end;

  TUserCategory = class(TAbstractAppObject, IUserCategory)
  protected
    FAllocDefID     : integer;
    FUserCategoryID : integer;
    FDescription    : WideString;
    FDistribution   : array of double;

    function Get_CategoryID : integer; safecall;
    function Get_Description : WideString; safecall;
    procedure Set_Description(const AValue : WideString); safecall;
    function Get_DistributionByIndex (AIndex : integer) : double; safecall;
    procedure Set_DistributionByIndex (AIndex : integer; AValue : double); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateUserCategoryName ( AErrorMessages : TStrings ) : WordBool;
    function ValidateDistribution ( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
  public
    function Populate (AAllocDefID     : integer;
                       ACategoryID     : integer;
                       ADescription    : string;
                       ANrOfRIs        : integer;
                       ADistribution   : array of double) : Boolean;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;

    property AllocDefID : integer read FAllocDefID;
    property CategoryID : integer read Get_CategoryID;
    property Description : WideString read Get_Description write Set_Description;
    property DistributionByIndex[AIndex : Integer] : double read Get_DistributionByIndex write Set_DistributionByIndex;
  end;

  TAllocationLevel = class(TAbstractAppObject, IAllocationLevel)
  protected
    FAllocDefID        : integer;
    FAllocationLevelID : integer;
    FDescription       : WideString;
    FCurtailment       : array of double;

    function Get_Description : WideString; safecall;
    procedure Set_Description(const AValue : WideString); safecall;
    function Get_CurtailmentByIndex (AIndex : integer) : double; safecall;
    procedure Set_CurtailmentByIndex (AIndex : integer;
                                     AValue : double); safecall;
    function Get_AllocationLevelID : integer; safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateAllocLevelName ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocLevelCurtailment( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;

  public
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function Populate (AAllocDefID     : integer;
                       AAllocLevelID : integer;
                       ADescription  : string;
                       ANrOfRIs      : integer;
                       ACurtailment  : array of double) : Boolean;

    property AllocDefID : integer read FAllocDefID;
    property AllocationLevelID : integer read Get_AllocationLevelID;
    property Description : WideString read Get_Description write Set_Description;
    property CurtailmentByIndex[AIndex : Integer] : double read Get_CurtailmentByIndex write Set_CurtailmentByIndex;
  end;

  TCoefficient = class(TAbstractAppObject, ICoefficient)
  protected
    FAllocDefID   : integer;
    FSubSystemID  : integer;
    FStartPercNr  : integer;
    FCurveSetNr   : integer;
    FLoadCaseNr   : integer;
    FTargetDraft  : double;
    FCoefficientA : double;
    FCoefficientB : double;
    FCoefficientC : double;
    FCoefficientD : double;
    FRisk         : double;

    function Get_TargetDraft  : double; safecall;
    procedure Set_TargetDraft( AValue : double );safecall;
    function Get_CoefficientA : double; safecall;
    procedure Set_CoefficientA(AValue : double ); safecall;
    function Get_CoefficientB : double; safecall;
    procedure Set_CoefficientB(AValue : double ); safecall;
    function Get_CoefficientC : double; safecall;
    procedure Set_CoefficientC(AValue : double ); safecall;
    function Get_CoefficientD : double; safecall;
    procedure Set_CoefficientD(AValue : double ); safecall;
    function Get_Risk : double; safecall;
    procedure Set_Risk(AValue : double ); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateLoadCase ( AErrorMessages : TStrings ) : WordBool;
    function ValidateCoefficientA ( AErrorMessages : TStrings ) : WordBool;
    function ValidateCoefficientB ( AErrorMessages : TStrings ) : WordBool;
    function ValidateCoefficientC ( AErrorMessages : TStrings ) : WordBool;
    function ValidateCoefficientD ( AErrorMessages : TStrings ) : WordBool;
    function ValidateRiskProportion ( AErrorMessages : TStrings ) : WordBool;
  public
    function Populate (AAllocDefID     : integer;
                       ASubSystemID    : integer;
                       AStartPercNr    : integer;
                       ACurveSetNr     : integer;
                       ALoadCaseNr     : integer;
                       ATargetDraft    : double;
                       ACoeffA         : double;
                       ACoeffB         : double;
                       ACoeffC         : double;
                       ACoeffD         : double;
                       ARisk           : double) : Boolean;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;

    property AllocDefID   : integer read FAllocDefID;
    property SubSystemID  : integer read FSubSystemID;
    property StartPercNr  : integer read FStartPercNr;
    property CurveSetNr   : integer read FCurveSetNr;
    property LoadCaseNr   : integer read FLoadCaseNr;
    property TargetDraft  : double  read Get_TargetDraft  write Set_TargetDraft;
    property CoefficientA : double  read Get_CoefficientA write Set_CoefficientA;
    property CoefficientB : double  read Get_CoefficientB write Set_CoefficientB;
    property CoefficientC : double  read Get_CoefficientC write Set_CoefficientC;
    property CoefficientD : double  read Get_CoefficientD write Set_CoefficientD;
    property Risk         : double  read Get_Risk         write Set_Risk;
  end;

  TCoefficientArray = array of array of array of TCoefficient;

  TSubSystem = class(TAbstractAppObject, ISubSystem)
  protected
    FAllocDefID                : integer;
    FSubSystemID               : integer;
    FName                      : string;
    FOrder                     : integer;
    FStartMonth                : integer;
    FStartYear                 : integer;
    FEndMonth                  : integer;
    FEndYear                   : integer;
    FSubtractID                : integer;
    FSupportID                 : integer;
    FSupportChannelNr          : integer;
    FShortTermYield            : double;
    FLongTermYield             : double;
    FLowestStreamFlow          : double;
    FFirmYield                 : Boolean;
    FReservoirNrs              : TStringList;
    FChannelNrs                : array of integer;
    FSupportCalcType           : integer;
    FCoefficients              : TCoefficientArray;

    function Get_ReservoirNrs : WideString; safecall;
    procedure Set_ReservoirNrs (const AValue : WideString); safecall;
    function Get_SubSystemID : integer; safecall;
    function Get_Name : WideString; safecall;
    procedure Set_Name(const AValue : WideString); safecall;
    function Get_Order : integer; safecall;
    procedure Set_Order(AValue : integer); safecall;
    function Get_StartMonth : integer; safecall;
    procedure Set_StartMonth(AValue : integer); safecall;
    function Get_StartYear : integer; safecall;
    procedure Set_StartYear(AValue : integer); safecall;
    function Get_EndMonth : integer; safecall;
    procedure Set_EndMonth(AValue : integer); safecall;
    function Get_EndYear : integer; safecall;
    procedure Set_EndYear(AValue : integer); safecall;
    function Get_SubtractID : integer; safecall;
    procedure Set_SubtractID(AValue : integer); safecall;
    function Get_SupportID : integer; safecall;
    procedure Set_SupportID(AValue : integer); safecall;
    function Get_SupportChannelNr : integer; safecall;
    procedure Set_SupportChannelNr (AValue : integer); safecall;
    function Get_ShortTermYield : double; safecall;
    procedure Set_ShortTermYield(AValue : double); safecall;
    function Get_LongTermYield : double; safecall;
    procedure Set_LongTermYield(AValue : double); safecall;
    function Get_LowestStreamFlow : double; safecall;
    procedure Set_LowestStreamFlow(AValue : double); safecall;
    function Get_FirmYield : Wordbool; safecall;
    procedure Set_FirmYield(AValue : Wordbool); safecall;
    function Get_SupportCalcType : integer; safecall;
    procedure Set_SupportCalcType(AValue : integer); safecall;
    function Get_CoefficientByPercCurveCase (APercIndex  : integer;
                                             ACurveIndex : integer;
                                             ACaseIndex  : integer) : ICoefficient; safecall;
    procedure Set_CoefficientByPercCurveCase (APercIndex  : integer;
                                              ACurveIndex : integer;
                                              ACaseIndex  : integer;
                                              const ACoefficient : ICoefficient ); safecall;
    function Get_RoutingChannelNrByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_RoutingChannelNrByIndex(AIndex: Integer; AValue: Integer); safecall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateSubSystemReservoirNrs ( AErrorMessages : TStrings ) : WordBool;
    function ValidateRoutingChannelNrs (AErrorMessages : TStrings;
                                        AErrorColumns  : TStringList) : WordBool;
    function ValidateSubSystemName ( AErrorMessages : TStrings ) : WordBool;
    function ValidateSubSystemStartMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateSubSystemStartYear (AErrorMessages : TStrings) : WordBool;
    function ValidateSubSystemEndMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateSubSystemEndYear (AErrorMessages : TStrings) : WordBool;
    function ValidateSubtractedSubSystemID ( AErrorMessages : TStrings ) : WordBool;
    function ValidateSupportingSubSystemID ( AErrorMessages : TStrings ) : WordBool;
    function ValidateShortTermYield ( AErrorMessages : TStrings ) : WordBool;
    function ValidateLongTermYield ( AErrorMessages : TStrings ) : WordBool;
    function ValidateLowestStreamFlow ( AErrorMessages : TStrings ) : WordBool;
    function ValidateFirmYield ( AErrorMessages : TStrings ) : WordBool;
    function ValidateSupportCalcType ( AErrorMessages : TStrings ) : WordBool;
  public
    function Initialise : boolean; override;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString): WordBool; safecall;
    function Populate (AAllocDefID         : integer;
                       ASubSystemID        : integer;
                       AName               : string;
                       AOrder              : integer;
                       AStartYear          : integer;
                       AStartMonth         : integer;
                       AEndYear            : integer;
                       AEndMonth           : integer;
                       ASubtractID         : integer;
                       ASupportID          : integer;
                       ASupportChannelNr   : integer;
                       AShortTermYield     : double;
                       ALongTermYield      : double;
                       ALowestStreamFlow   : double;
                       AFirmYield          : Boolean;
                       AReservoirNrs       : string;
                       AChannelNrs         : array of integer;
                       ASupportCalcType    : integer) : Boolean;
    procedure SetCoefficientDimensions (ANrOfStartPercs : integer;
                                        ANrOfCurveSets  : integer;
                                        ANrOfLoadCases  : integer);
    function NewCoefficient ( APercNr : integer; ACurveNr : integer; ACaseNr : integer) : TCoefficient;
    function CreateCoefficient ( APercNr : integer; ACurveNr : integer; ACaseNr : integer ) : TCoefficient;
    function DeleteCoefficient ( APercNr : integer; ACurveNr : integer; ACaseNr : integer ) : WordBool;
    function RemoveCoefficient ( APercNr : integer; ACurveNr : integer; ACaseNr : integer ) : WordBool;
    function CastCoefficientByPercCurveCase (APercIndex  : integer;
                                             ACurveIndex : integer;
                                             ACaseIndex  : integer) : TCoefficient;

    property AllocDefID       : integer     read FAllocDefID;
    property SubSystemID      : integer     read Get_SubSystemID;
    property Name             : WideString  read Get_Name             write Set_Name;
    property Order            : integer     read Get_Order            write Set_Order;
    property StartMonth       : integer     read Get_StartMonth       write Set_StartMonth;
    property StartYear        : integer     read Get_StartYear        write Set_StartYear;
    property EndMonth         : integer     read Get_EndMonth         write Set_EndMonth;
    property EndYear          : integer     read Get_EndYear          write Set_EndYear;
    property SubtractID       : integer     read Get_SubtractID       write Set_SubtractID;
    property SupportID        : integer     read Get_SupportID        write Set_SupportID;
    property ShortTermYield   : double      read Get_ShortTermYield   write Set_ShortTermYield;
    property LongTermYield    : double      read Get_LongTermYield    write Set_LongTermYield;
    property LowestStreamFlow : double      read Get_LowestStreamFlow write Set_LowestStreamFlow;
    property FirmYield        : Wordbool    read Get_FirmYield        write Set_FirmYield;
    property ReservoirNrs     : WideString  read Get_ReservoirNrs   write Set_ReservoirNrs;
    property RoutingChannelNrByIndex[AIndex: Integer]: Integer read Get_RoutingChannelNrByIndex write Set_RoutingChannelNrByIndex;
    property SupportCalcType  : integer read Get_SupportCalcType  write Set_SupportCalcType;
    property CoefficientByPercCurveCase[APercIndex  : integer;
                                        ACurveIndex : integer;
                                        ACaseIndex  : integer] : ICoefficient read Get_CoefficientByPercCurveCase
                                                                              write Set_CoefficientByPercCurveCase;
  end;

  TSupportChannel = class(TAbstractAppObject, ISupportChannel)
  protected
    FAllocDefID          : integer;
    FSupportChannelID    : integer;
    FChannelNumber       : integer;
    FNrOfCntrlSubSystems : integer;
    FSubSystemIDs        : array of integer;
    FSubSystemFactors    : array of double;

    function Get_SupportChannelID: Integer; safecall;
    function Get_ChannelNumber: integer; safecall;
    procedure Set_ChannelNumber (AValue : integer); safecall;
    function Get_NrOfCntrlSubSystems: integer; safecall;
    procedure Set_NrOfCntrlSubSystems ( AValue : integer ); safecall;
    function Get_SubSystemIDByIndex (AIndex : integer) : integer; safecall;
    procedure Set_SubSystemIDByIndex (AIndex : integer;
                                     ANumber : integer); safecall;
    function Get_SubSystemFactorByIndex (AIndex : integer) : double; safecall;
    procedure Set_SubSystemFactorByIndex (AIndex  : integer;
                                         AValue : double); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateSupportChannelNr (AErrorMessages : TStrings) : WordBool;
    function ValidateCntrlSubSystems (AErrorMessages : TStrings;
                                      AErrorColumns  : TStringList) : WordBool;
  public
    function Initialise : boolean; override;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function Populate (AAllocDefID       : integer;
                       ASupportChannelID : integer;
                       AChannelNumber    : integer;
                       ANrOfSubSystems   : integer;
                       ASubSystemIDs     : array of integer;
                       AFactors          : array of double) : Boolean;
    function NewControllingSubSystem : integer; safecall;
    function RemoveControllingSubSystem(AIndex: Integer): WordBool; safecall;

    property AllocDefID          : integer read FAllocDefID;
    property SupportChannelID    : integer read FSupportChannelID;
    property ChannelNumber       : integer read Get_ChannelNumber write Set_ChannelNumber;
    property NrOfCntrlSubSystems : integer read Get_NrOfCntrlSubSystems write Set_NrOfCntrlSubSystems;
    property SubSystemIDByIndex[AIndex : integer] : integer read Get_SubSystemIDByIndex write Set_SubSystemIDByIndex;
    property SubSystemFactorByIndex[AIndex : integer] : double read Get_SubSystemFactorByIndex write Set_SubSystemFactorByIndex;
  end;

  TSupportSubSystem = class(TAbstractAppObject, ISupportSubSystem)
  protected
    FAllocDefID         : integer;
    FDemandDefID        : integer;
    FSupportSubSystemID : integer;
    FSubSystemID        : integer;
    FSupportChannelNrs  : Array [1..5] of integer;

    function Get_SupportSubSystemID : integer; safecall;
    function Get_SubSystemID : integer; safecall;
    procedure Set_SubSystemID (AValue : integer); safecall;
    function Get_SupportChannelNrByIndex (AIndex : integer) : integer; safecall;
    procedure Set_SupportChannelNrByIndex (AIndex  : integer;
                                           AValue : integer); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise : boolean; override;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;

    function Populate (AAllocDefID         : integer;
                       ADemandDefID        : integer;
                       ASupportID          : integer;
                       ASubSystemID        : integer;
                       AChannelNrs         : array of integer) : boolean;

    property SupportSubSystemID : integer read Get_SupportSubSystemID;
    property SubSystemID : integer read Get_SubSystemID write Set_SubSystemID;
    property SupportChannelNrByIndex[AIndex : integer] : integer read Get_SupportChannelNrByIndex write Set_SupportChannelNrByIndex;
  end;

  TDemandDefinition = class(TAbstractAppObject, IDemandDefinition)
  protected
    FAllocDefID            : integer;
    FDemandDefID           : integer;
    FOrder                 : integer;
    FName                  : string;
    FParentSubSystemID     : integer;
    FGrowthType            : integer;
    FTargetDemand          : double;
    FDemandCentreID        : integer;
    FUserCategoryID        : integer;
    FSupportArc1           : integer;
    FSupportArc2           : integer;
    FNrOfSupportSubSystems : integer;
    FSupportSubSystems     : TObjectList;

    function Get_DemandDefID : integer; safecall;
    function Get_Name : WideString; safecall;
    procedure Set_Name(const AValue : WideString); safecall;
    function Get_Order : integer; safecall;
    procedure Set_Order(AValue : integer); safecall;
    function Get_ParentSubSystemID : integer; safecall;
    procedure Set_ParentSubSystemID(AValue : integer); safecall;
    function Get_GrowthType : integer; safecall;
    procedure Set_GrowthType(AValue : integer); safecall;
    function Get_TargetDemand : double; safecall;
    procedure Set_TargetDemand(AValue : double); safecall;
    function Get_DemandCentreID : integer; safecall;
    procedure Set_DemandCentreID(AValue : integer); safecall;
    function Get_UserCategoryID : integer; safecall;
    procedure Set_UserCategoryID(AValue : integer); safecall;
    function Get_SupportArc1 : integer; safecall;
    procedure Set_SupportArc1(AValue : integer); safecall;
    function Get_SupportArc2 : integer; safecall;
    procedure Set_SupportArc2(AValue : integer); safecall;
    function Get_NrOfSupportSubSystems : integer; safecall;
    procedure Set_NrOfSupportSubSystems ( AValue : integer ); safecall;
    function Get_SupportSubSystemByIndex (AIndex : integer) : ISupportSubSystem; safecall;
    function Get_SupportSubSystemByID (AID : integer) : ISupportSubSystem; safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateDemandCentreID (AErrorMessages : TStrings) : WordBool;
    function ValidateResidentInSubSystem (AErrorMessages : TStrings) : WordBool;
    function ValidateGrowthType (AErrorMessages : TStrings) : WordBool;
    function ValidateTargetDemand (AErrorMessages : TStrings) : WordBool;
    function ValidateUserCategory (AErrorMessages : TStrings) : WordBool;
    function ValidateSupportArc1 (AErrorMessages : TStrings) : WordBool;
    function ValidateSupportArc2 (AErrorMessages : TStrings) : WordBool;
    function ValidateSupportSubSystems (AErrorMessages : TStrings;
                                        AErrorColumns  : TStringList) : WordBool;
  public
    function CastSupportSubSystemByIndex (AIndex : integer) : TSupportSubSystem;
    function CastSupportSubSystemByID (AID : integer) : TSupportSubSystem;
    function Initialise : boolean; override;
    function Validate (var AErrors    : WideString;
                       const AContext : WideString) : WordBool; safecall;
    function Populate (AAllocDefID        : integer;
                       ADemandDefID       : integer;
                       AName              : string;
                       AParentSubSystemID : integer;
                       AOrder             : integer;
                       AGrowthType        : integer;
                       ATargetDemand      : double;
                       ADemandCentreID    : integer;
                       AUserCategoryID    : integer;
                       ASupportArc1       : integer;
                       ASupportArc2       : integer) : boolean;
    function NewSupportSubSystem : ISupportSubSystem; safecall;
    function CreateSupportSubSystem : TSupportSubSystem;
    function DeleteSupportSubSystem (AIndex : integer) : WordBool;
    function RemoveSupportSubSystem (AIndex : integer) : WordBool; safecall;

    property AllocDefID            : integer  read FAllocDefID;
    property DemandDefID           : integer read Get_DemandDefID;
    property Name                   : WideString  read Get_Name          write Set_Name;
    property Order                  : integer read Get_Order             write Set_Order;
    property ParentSubSystemID      : integer read Get_ParentSubSystemID write Set_ParentSubSystemID;
    property GrowthType             : integer read Get_GrowthType        write Set_GrowthType;
    property TargetDemand           : double  read Get_TargetDemand      write Set_TargetDemand;
    property DemandCentreID         : integer read Get_DemandCentreID    write Set_DemandCentreID;
    property UserCategoryID         : integer read Get_UserCategoryID    write Set_UserCategoryID;
    property SupportArc1            : integer read Get_SupportArc1       write Set_SupportArc1;
    property SupportArc2            : integer read Get_SupportArc2       write Set_SupportArc2;
    property NrOfSupportSubSystems  : integer read Get_NrOfSupportSubSystems write Set_NrOfSupportSubSystems;
    property SupportSubSystemByID[AID : integer] : ISupportSubSystem read Get_SupportSubSystemByID;
    property SupportSubSystemByIndex[AIndex : integer] : ISupportSubSystem read Get_SupportSubSystemByIndex;
  end;

  TFixedPosition = class(TAbstractAppObject, IFixedPosition)
  protected
    FAllocDefID          : integer;
    FFixedPositionID     : integer;
    FFixedPositionNr     : integer;
    FFixedPosSubSystemID : integer;

    function Get_FixedPositionID : integer; safecall;
    function Get_FixedPositionNr : integer; safecall;
    procedure Set_FixedPositionNr ( AValue : integer ); safecall;
    function Get_FixedPosSubSystemID : integer; safecall;
    procedure Set_FixedPosSubSystemID ( AValue : integer ); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Populate ( AAllocDefID          : integer;
                        AFixedPositionID     : integer;
                        AFixedPositionNr     : integer;
                        AFixedPosSubSystemID : integer ) : boolean;

    property AllocDefID          : integer read FAllocDefID;
    property FixedPositionID     : integer read FFixedPositionID;
    property FixedPositionNr     : integer read Get_FixedPositionNr write Set_FixedPositionNr;
    property FixedPosSubSystemID : integer read Get_FixedPosSubSystemID write Set_FixedPosSubSystemID;
  end;

  TSpecificOrder = class(TAbstractAppObject, ISpecificOrder)
  protected
    FAllocDefID        : integer;
    FSpecificOrderID   : integer;
    FBeforeSubSystemID : integer;
    FAfterSubSystemID  : integer;

    function Get_SpecificOrderID: Integer; safecall;
    function Get_BeforeSubSystemID : integer; safecall;
    procedure Set_BeforeSubSystemID ( Avalue : integer ); safecall;
    function Get_AfterSubSystemID : integer; safecall;
    procedure Set_AfterSubSystemID ( AValue : integer ); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Populate ( AAllocDefID          : integer;
                        ASpecificOrderID   : integer;
                        ABeforeSubSystemID : integer;
                        AAfterSubSystemID  : integer ) : boolean;
    property AllocDefID        : integer read FAllocDefID;
    property SpecificOrderID   : integer read FSpecificOrderID;
    property BeforeSubSystemID : integer read Get_BeforeSubSystemID write Set_BeforeSubSystemID;
    property AfterSubSystemID  : integer read Get_AfterSubSystemID write Set_AfterSubSystemID;
  end;

  TAllocationDefinition = class(TAbstractAppObject, IAllocationDefinition)
  protected
    FAllocDefID               : integer;
    FName                     : string;
    FSwitchDefID              : integer;
    FAllocDefFileName         : string;
    FStartYear                : integer;
    FStartMonth               : integer;
    FNrOfCategories           : integer;
    FNrOfReliabilityClasses   : integer;
    FNrOfAllocationLevels     : integer;
    FRecurrenceIntervals      : array of integer;
    FRecurrenceIntervalLabels : array of string;
    FCategoryList             : TObjectList;
    FAllocationLevelList      : TObjectList;

    FPeriodLength             : integer;
    FNrOfLoadCases            : integer;
    FNrOfStartingPercentages  : integer;
    FNrOfSubSystems           : integer;
    FNrOfCurveSets            : integer;
    FDecisionCurveSet         : array [1..12] of integer;
    FStartingPercentages      : array of double;

    FSubSystemList            : TObjectList;

    FSupportStrategy          : integer;
    FBalancingOption          : integer;
    FNrInFixedPosition        : integer;
    FFixedPositionList        : TObjectList;
    FNrInSpecificOrder        : integer;
    FSpecificOrderList        : TObjectList;

    FNrOfSupportChannels      : integer;
    FSupportChannelList       : TObjectList;

    FNrOfDemandDefinitions    : integer;
    FDemandDefinitionList     : TObjectList;

    function Get_AllocationDefinitionID : integer; safecall;
    function Get_Name : WideString; safecall;
    procedure Set_Name (const AValue : WideString); safecall;
    function Get_AllocDefFileName: WideString; safecall;
    procedure Set_AllocDefFileName(const Value: WideString); safecall;

    function Get_StartYear : integer; safecall;
    procedure Set_StartYear (AValue : integer); safecall;
    function Get_StartMonth : integer; safecall;
    procedure Set_StartMonth (AValue : integer); safecall;

    function Get_NrOfCategories : integer; safecall;
    function Get_NrOfReliabilityClasses : integer; safecall;
    procedure Set_NrOfReliabilityClasses(AValue: integer); safecall;
    function Get_PeriodLength : integer; safecall;
    procedure Set_PeriodLength(AValue : integer); safecall;
    function Get_NrOfLoadCases : integer; safecall;
    procedure Set_NrOfLoadCases(AValue : integer); safecall;
    function Get_NrOfStartingPercentages : integer; safecall;
    procedure Set_NrOfStartingPercentages(AValue : integer); safecall;
    function Get_NrOfSubSystems : integer; safecall;
    function Get_NrOfCurveSets : integer; safecall;
    procedure Set_NrOfCurveSets(AValue : integer); safecall;
    function Get_RecurrenceIntervalByIndex(AIndex : integer) : integer; safecall;
    procedure Set_RecurrenceIntervalByIndex(AIndex : integer; AValue : integer); safecall;
    function Get_RILabelByIndex(AIndex : integer) : WideString; safecall;
    procedure Set_RILabelByIndex(AIndex : integer; const AValue : WideString); safecall;

    function Get_CategoryByID (AID : integer) : IUserCategory; safecall;
    function Get_CategoryByIndex (AIndex : integer) : IUserCategory; safecall;
    function Get_SubSystemByIndex (AIndex : integer) : ISubSystem; safecall;
    function Get_SubSystemByID (AID : integer) : ISubSystem; safecall;
    function Get_AllocationLevelByIndex (AIndex : integer) : IAllocationLevel; safecall;
    function Get_DecisionCurveSetByMonth (AMonth : integer) : integer; safecall;
    procedure Set_DecisionCurveSetByMonth (AMonth : integer;
                                          AValue : integer); safecall;
    function Get_StartingPercentageByIndex (AIndex : integer) : double; safecall;
    procedure Set_StartingPercentageByIndex (AIndex : integer;
                                                      AValue : double); safecall;

    function Get_NrInFixedPosition : integer; safecall;
    function Get_FixedPositionByIndex (AIndex : integer) : IFixedPosition; safecall;
    function Get_FixedPositionByID (AID : Integer): IFixedPosition; safecall;
    function Get_NrInSpecificOrder : integer; safecall;
    function Get_SpecificOrderByIndex (AIndex : integer) : ISpecificOrder; safecall;
    function Get_SpecificOrderByID (AID : integer) : ISpecificOrder; safecall;
    function Get_SupportChannelByIndex (AIndex : integer) : ISupportChannel; safecall;
    function Get_SupportChannelByID (AID : integer) : ISupportChannel; safecall;
    function Get_DemandDefinitionByIndex (AIndex : integer) : IDemandDefinition; safecall;
    function Get_DemandDefinitionByID (AID : integer) : IDemandDefinition; safecall;
    function Get_NrOfAllocationLevels: Integer; safecall;
    function Get_NrOfSupportChannels : Integer; safecall;
    function Get_NrOfDemandDefinitions: Integer; safecall;
    function Get_SupportStrategy: Integer; safecall;
    procedure Set_SupportStrategy(Value: Integer); safecall;
    function Get_BalancingOption: Integer; safecall;
    procedure Set_BalancingOption(Value: Integer); safecall;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ResetCoefficientDimensions (ANrOfStartStoragePercs : integer;
                                          ANrOfCurveSets         : integer;
                                          ANrOfLoadCases         : integer);
    function ValidateAllocDefName ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefFileName ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefStartYear (AErrorMessages : TStrings) : WordBool;
    function ValidateAllocDefStartMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateAllocDefNrOfReliabilityClasses ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefPeriodLength ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefNrOfLoadCases ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefNrOfStartStoragePercs ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefNrOfCurveSets ( AErrorMessages : TStrings ) : WordBool;
    function ValidateAllocDefRIValue ( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
    function ValidateMonthCurveSet ( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
    function ValidateAllocDefRILabel (AErrorMessages : TStrings;
                                      AErrorColumns  : TStringList) : WordBool;
    function ValidateSupportStrategy (AErrorMessages : TStrings) : WordBool;
    function ValidateBalancingOption (AErrorMessages : TStrings) : WordBool;
    function ValidateFixedPosition (AErrorMessages : TStrings;
                                    AErrorColumns  : TStringList) : WordBool;
    function ValidateSpecificOrder (AErrorMessages : TStrings;
                                    AErrorColumns  : TStringList) : WordBool;
    function ValidateCyclicOrder (AErrorMessages : TStrings;
                                  AErrorIndex    : TStringList) : WordBool;

  public
    function Validate (var AErrors    : WideString;
                       const AContext : WideString): WordBool; safecall;
    function Initialise : boolean; override;
    function Populate (AAllocDefID               : integer;
                       AName                     : string;
                       ASwitchDefID              : integer;
                       AAllocDefFileName         : string;
                       AStartYear                : integer;
                       AStartMonth               : integer;
                       AEndYear                  : integer;
                       AEndMonth                 : integer;
                       ANrOfReliabilityClasses   : integer;
                       ANrOfLoadCases            : integer;
                       ANrOfStartingPercentages  : integer;
                       ANrOfCurveSets            : integer;
                       APeriodLength             : integer;
                       ASupportStrategy          : integer;
                       ABalancingOption          : integer;
                       ARecurrenceIntervals      : array of integer;
                       ARecurrenceIntervalLabels : array of string;
                       ADecisionCurveSet         : array of integer;
                       AStartingPercentages      : array of double) : Boolean;

    function NewUserCategory : IUserCategory; safecall;
    function CreateCategory : TUserCategory;
    function DeleteCategoryByID (AID : integer) : Boolean;
    function DeleteCategoryByIndex (AIndex : integer) : Boolean;
    function RemoveUserCategory (AID : integer) : WordBool; safecall;

    function NewAllocationLevel : IAllocationLevel; safecall;
    function CreateAllocationLevel : TAllocationLevel;
    function DeleteAllocationLevelByID (AID : integer) : Boolean;
    function DeleteAllocationLevelByIndex (AIndex : integer) : Boolean;
    function RemoveAllocationLevel (AID : integer) : WordBool; safecall;

    function NewSubSystem  : ISubSystem; safecall;
    function CreateSubSystem : TSubSystem;
    function DeleteSubSystemByID (AID : integer) : Boolean;
    function DeleteSubSystemByIndex (AIndex : integer) : Boolean;
    function RemoveSubSystem (AID : integer) : WordBool; safecall;

    function NewFixedPosition: IFixedPosition; safecall;
    function CreateFixedPosition : TFixedPosition;
    function DeleteFixedPositionByIndex (AIndex : integer) : WordBool;
    function DeleteFixedPositionByID (AID : integer) : WordBool;
    function RemoveFixedPosition(AID: Integer): WordBool; safecall;

    function NewSpecificOrder: ISpecificOrder; safecall;
    function CreateSpecificOrder : TSpecificOrder;
    function DeleteSpecificOrderByIndex (AIndex : integer) : Boolean;
    function DeleteSpecificOrderByID (AID : integer) : Boolean;
    function RemoveSpecificOrder(AID : Integer): WordBool; safecall;

    function NewSupportChannel : ISupportChannel; safecall;
    function CreateSupportChannel : TSupportChannel;
    function DeleteSupportChannelByIndex (AIndex : integer) : Boolean;
    function DeleteSupportChannelByID (AID : integer) : Boolean;
    function RemoveSupportChannel (AID : integer) : WordBool; safecall;

    function NewDemandDefinition : IDemandDefinition; safecall;
    function CreateDemandDefinition : TDemandDefinition;
    function DeleteDemandDefinitionByID (AID : integer) : Boolean;
    function DeleteDemandDefinitionByIndex (AIndex : integer) : Boolean;
    function RemoveDemandDefinition (AID : integer) : WordBool; safecall;

    function CastCategoryByID (AID : integer) : TUserCategory;
    function CastCategoryByIndex (AIndex : integer) : TUserCategory;
    function CastSubSystemByIndex (AIndex : integer) : TSubSystem;
    function CastSubSystemByID (AID : integer) : TSubSystem;
    function CastAllocationLevelByID (AID : integer) : TAllocationLevel;
    function CastAllocationLevelByIndex (AIndex : integer) : TAllocationLevel;
    function CastSupportChannelByIndex (AIndex : integer) : TSupportChannel;
    function CastSupportChannelByID (AID : integer) : TSupportChannel;
    function CastDemandDefinitionByIndex (AIndex : integer) : TDemandDefinition;
    function CastDemandDefinitionByID (AID : integer) : TDemandDefinition;
    function CastFixedPositionByIndex ( AIndex : integer ) : TFixedPosition;
    function CastFixedPositionByID ( AID : integer ) : TFixedPosition;
    function CastSpecificOrderByIndex ( AIndex : integer ) : TSpecificOrder;
    function CastSpecificOrderByID (AID : integer) : TSpecificOrder;
    property AllocationDefinitionID : integer    read Get_AllocationDefinitionID;
    property Name                   : WideString read Get_Name                   write Set_Name;
    property AllocDefFileName       : WideString read Get_AllocDefFileName       write Set_AllocDefFileName;
    property StartYear              : integer    read Get_StartYear              write Set_StartYear;
    property StartMonth             : integer    read Get_StartMonth             write Set_StartMonth;
    property NrOfCategories         : integer    read Get_NrOfCategories;
    property NrOfReliabilityClasses : integer    read Get_NrOfReliabilityClasses write Set_NrOfReliabilityClasses;
    property NrOfAllocationLevels   : integer    read Get_NrOfAllocationLevels;

    property PeriodLength           : integer    read Get_PeriodLength           write Set_PeriodLength;
    property NrOfLoadCases          : integer    read Get_NrOfLoadCases          write Set_NrOfLoadCases;
    property NrOfStartingPercentages: integer    read Get_NrOfStartingPercentages write Set_NrOfStartingPercentages;
    property NrOfSubSystems         : integer    read Get_NrOfSubSystems;
    property NrOfCurveSets          : integer    read Get_NrOfCurveSets          write Set_NrOfCurveSets;
    property RecurrenceIntervalByIndex[AIndex : Integer] : integer read Get_RecurrenceIntervalByIndex write Set_RecurrenceIntervalByIndex;
    property RILabelByIndex[AIndex : Integer] : WideString  read Get_RILabelByIndex write Set_RILabelByIndex;


    property CategoryByID[AID : integer]                 : IUserCategory read Get_CategoryByID;
    property CategoryByIndex[AIndex : integer]           : IUserCategory read Get_CategoryByIndex;
    property AllocationLevelByIndex[AIndex : integer]    : IAllocationLevel read Get_AllocationLevelByIndex;
    property SubSystemByIndex[AIndex : integer]          : ISubSystem read Get_SubSystemByIndex;
    property SubSystemByID[AID : integer]                : ISubSystem read Get_SubSystemByID;
    property DecisionCurveSetByMonth[AMonth : integer]   : integer read  Get_DecisionCurveSetByMonth
                                                                   write Set_DecisionCurveSetByMonth;
    property StartingPercentageByIndex[AIndex : integer] : double  read  Get_StartingPercentageByIndex
                                                                   write Set_StartingPercentageByIndex;

    property NrInFixedPosition : integer read Get_NrInFixedPosition;
    property FixedPositionByIndex[AIndex : integer] : IFixedPosition read Get_FixedPositionByIndex;
    property FixedPositionByID[AID : integer] : IFixedPosition read Get_FixedPositionByID;
    property NrInSpecificOrder : integer read Get_NrInSpecificOrder;
    property SpecificOrderByIndex[AIndex : integer] : ISpecificOrder read Get_SpecificOrderByIndex;
    property SpecificOrderByID[AID : integer] : ISpecificOrder read Get_SpecificOrderByID;

    property NrOfSupportChannels      : integer read Get_NrOfSupportChannels;
    property SupportChannelByIndex[AIndex : integer] : ISupportChannel read Get_SupportChannelByIndex;
    property NrOfDemandDefinitions : integer read Get_NrOfDemandDefinitions;
    property DemandDefinitionByIndex[AIndex : integer] : IDemandDefinition read Get_DemandDefinitionByIndex;
    property DemandDefinitionByID[AID : integer] : IDemandDefinition read Get_DemandDefinitionByID;

  end;

  TAllocationDefinitionsList = class(TAbstractAppObject, IAllocationDefinitionsList)
  protected
    FAllocationDefinitionsList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function AddAllocationDefinition (AAllocDef : TAllocationDefinition): boolean;
  public
    function Initialise: Boolean; override;

    function NewAllocationDefinition : IAllocationDefinition; safecall;
    function CreateAllocationDefinition : TAllocationDefinition;
    function DeleteAllocationDefinitionWithID (AID : integer) : WordBool;
    function DeleteAllocationDefinitionWithIndex (AIndex : integer) : WordBool;
    function CastAllocationDefinitionByIndex (AIndex : integer): TAllocationDefinition;
    function CastAllocationDefinitionByID (AID : integer): TAllocationDefinition;

    function RemoveAllocationDefinitionWithID (AID : integer) : WordBool; safecall;
    function Get_AllocationDefinitionCount: integer; safecall;
    function Get_AllocationDefinitionByID (AID: integer): IAllocationDefinition; safecall;
    function Get_AllocationDefinitionByIndex (AIndex: integer): IAllocationDefinition; safecall;

    property AllocationDefinitionCount: integer read Get_AllocationDefinitionCount;
    property AllocationDefinitionByIndex[AIndex : integer]: IAllocationDefinition read Get_AllocationDefinitionByIndex;
    property AllocationDefinitionByID[AID : integer]: IAllocationDefinition read Get_AllocationDefinitionByID;
  end;

    procedure AddErrors (var AErrors : WideString;
                         AErrorList  : TStringList;
                         AErrorCols  : TStringList);

implementation

uses
  System.Types,
  System.UITypes,
  Math,
  UConstants,
  VCL.ComCtrls,
  VCL.Dialogs,
  UAllocationDefLoadAgent,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

procedure AddErrors (var AErrors : WideString;
                                           AErrorList  : TStringList;
                                           AErrorCols  : TStringList);
const OPNAME = 'UAllocationDefinitionData.AddErrors';
begin
  try
    if (AErrorCols.Count = 0) then
      AErrors := AErrors + AErrorList.Text
    else
      AErrors := AErrors + CTStringsSeparator + AErrorList.Text +
                           CTStringsSeparator + AErrorCols.Text + CTStringsSeparator;
    AErrorList.Clear;
    AErrorCols.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TUserCategory                                                              *}
{******************************************************************************}

procedure TUserCategory.CreateMemberObjects;
const OPNAME = 'TUserCategory.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserCategory.DestroyMemberObjects;
const OPNAME = 'TUserCategory.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TUserCategory._AddRef: Integer;
const OPNAME = 'TUserCategory._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUserCategory._Release: Integer;
const OPNAME = 'TUserCategory._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUserCategory.Populate (AAllocDefID     : integer;
                                 ACategoryID     : integer;
                                 ADescription    : string;
                                 ANrOfRIs        : integer;
                                 ADistribution   : array of double) : Boolean;
const OPNAME = 'TUserCategory.Populate';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FAllocDefID     := AAllocDefID;
    FUserCategoryID := ACategoryID;
    FDescription    := ADescription;
    SetLength(FDistribution, ANrOfRIs + 1);
    for lIndex := 1 to ANrOfRIs do
      FDistribution[lIndex] := ADistribution[lIndex];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUserCategory.Validate (var AErrors    : WideString;
                                 const AContext : WideString) : WordBool;
const OPNAME = 'TUserCategory.Validate';
var
  lErrorList : TStringList;
  lErrorCols : TStringList;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      Result := True;
      if AContext = 'UserCategoryName' then
        Result := ValidateUserCategoryName(lErrorList)
      else
      if (AContext = 'Distribution') then
      begin
        Result := ValidateDistribution(lErrorList, lErrorCols);
        if (NOT Result) then
          AddErrors(AErrors, lErrorList, lErrorCols);
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUserCategory.Get_CategoryID : integer;
const OPNAME = 'TUserCategory.Get_CategoryID';
begin
  Result := 0;
  try
    Result := FUserCategoryID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TUserCategory.Get_Description: WideString;
const OPNAME = 'TUserCategory.Get_Description';
begin
  Result := '';
  try
    Result := FDescription;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TUserCategory.Get_DistributionByIndex(AIndex: integer): double;
const OPNAME = 'TUserCategory.Get_DistributionByIndex';
var
  LAllocDef    : IAllocationDefinition;
begin
  try
    if (FAllocDefID > 0) then
    begin
      LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      if ( AIndex >= 1 ) and ( LAllocDef <> nil) AND ( AIndex <= LAllocDef.NrOfReliabilityClasses ) then
      Result := FDistribution[AIndex];
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserCategory.Set_Description(const AValue: WideString);
const OPNAME = 'TUserCategory.Set_Description';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_UserCategory(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FUserCategoryID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'UserCategoryName', AValue, FDescription, LContextData) then
        begin
          LOldValue := FDescription;
          FDescription := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UserCategoryName',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUserCategory.Set_DistributionByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TUserCategory.Set_DistributionByIndex';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LAllocDef    : IAllocationDefinition;
begin
  try
    if (FAllocDefID > 0) then
    begin
      LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      if ( AIndex >= 1 ) and ( LAllocDef <> nil) and ( AIndex <= LAllocDef.NrOfReliabilityClasses ) then
      begin
        LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_UserCategoryFieldNameID(LContextData, IntToStr(FAllocDefID),
                                                    IntToStr(FUserCategoryID), IntToStr(AIndex) );
            if FAppModules.FieldProperties.UpdateFieldValue(
               'Distribution', FloatToStr(AValue), FloatToStr(FDistribution[AIndex]), LContextData) then
            begin
              LOldValue := FloatToStr(FDistribution[AIndex]);
              FDistribution[AIndex] := AValue;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'Distribution',LOldValue,FloatToStr(AValue));
            end;
          finally
            FreeAndNil ( LContextData );
          end;
        finally
          FreeAndNil ( LLoadAgent );
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TUserCategory.ValidateUserCategoryName ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TUserCategory.ValidateUserCategoryName';
var
  LMessage   : string;
  lALlocDef  : IAllocationDefinition;
  lOtherCat  : IUserCategory;
  lIndex     : integer;
  lDuplicate : boolean;
begin
  Result := True;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('UserCategoryName', FDescription, LMessage)) then
      AErrorMessages.Add ('WARNING:'+FDescription + ':' + LMessage)
    else
    begin
      lALlocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      if (lALlocDef <> nil) then
      begin
        lDuplicate := FALSE;
        lIndex     := 0;
        while ((NOT lDuplicate) AND (lIndex < lALlocDef.NrOfCategories)) do
        begin
          lOtherCat := lALlocDef.CategoryByIndex[lIndex];
          if (lOtherCat.CategoryID <> Self.CategoryID) AND (lOtherCat.Description = Self.Description) then
            lDuplicate := TRUE
          else
            lIndex := lIndex + 1;
        end;
        if (lDuplicate) then
        begin
          AErrorMessages.Add('WARNING:'+'Duplicate User Category name');
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TUserCategory.ValidateDistribution ( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
const OPNAME = 'TUserCategory.ValidateDistribution';
var
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LIndex            : integer;
  LAllocDef         : IAllocationDefinition;
  lTotal            : double;
begin
  Result := False;
  try
    if (FAllocDefID > 0) then
    begin
      LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      AErrorColumns.Clear;
      LResult := True;
      LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      lTotal := 0;
      for LIndex := 1 to LAllocDef.NrOfReliabilityClasses do
      begin
        lMessage := '';
        lTotal   := lTotal + FDistribution[LIndex];
        if ( not FAppModules.FieldProperties.ValidateFieldProperty
           ('Distribution', FloatToStr ( FDistribution [ LIndex ] ),
           LMessage, LIndex)) then
        begin
          LResult := False;
          AErrorMessages.Add ( 'ERROR:'+ LMessage );
          AErrorColumns.Add ( IntToStr ( LIndex ) );
          if ( LStopOnFirstError ) then
            Break;
        end;
      end;
      if (LResult OR (NOT LStopOnFirstError)) AND (lTotal <> 1) then
      begin
        lResult := FALSE;
        lMessage := 'Distribution must sum to 1.00';
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(LAllocDef.NrOfReliabilityClasses + 1));
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TAllocationLevel                                                           *}
{******************************************************************************}

procedure TAllocationLevel.CreateMemberObjects;
const OPNAME = 'TAllocationLevel.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationLevel.DestroyMemberObjects;
const OPNAME = 'TAllocationLevel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationLevel.Get_AllocationLevelID: integer;
const OPNAME = 'TAllocationLevel.Get_AllocationLevelID';
begin
  Result := 0;
  try
    Result := FAllocationLevelID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationLevel.Get_CurtailmentByIndex(AIndex: integer): double;
const OPNAME = 'TAllocationLevel.Get_CurtailmentByIndex';
var
  LAllocDef    : IAllocationDefinition;
begin
  Result := 0;
  try
    if ( FAllocDefID > 0 ) then
    begin
      LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      if ( AIndex >= 1 ) and ( LAllocDef <> nil ) and ( AIndex <= LAllocDef.NrOfReliabilityClasses ) then
        Result := FCurtailment[AIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationLevel.Get_Description: WideString;
const OPNAME = 'TAllocationLevel.Get_Description';
begin
  Result := '';
  try
    Result := FDescription;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TAllocationLevel.Set_CurtailmentByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TAllocationLevel.Set_CurtailmentByIndex';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LAllocDef    : IAllocationDefinition;
begin
  try
    if ( FAllocDefID > 0 ) then
    begin
      LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      if ( LAllocDef <> nil ) and ( AIndex >= 1 ) and ( AIndex <= LAllocDef.NrOfReliabilityClasses ) then
      begin
        LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_AllocationLevelFieldNameID ( LContextData, IntToStr(FAllocDefID),
                                                                    IntToStr(FAllocationLevelID), IntToStr(AIndex) );
            if FAppModules.FieldProperties.UpdateFieldValue(
               'Curtailment', FloatToStr(AValue), FloatToStr(FCurtailment[AIndex]), LContextData) then
            begin
              LOldValue := FloatToStr(FCurtailment[AIndex]);
              FCurtailment[AIndex] := AValue;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'Curtailment',LOldValue,FloatToStr(AValue));
            end;
          finally
            FreeAndNil ( LContextData );
          end;
        finally
          FreeAndNil ( LLoadAgent );
        end;
      end;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationLevel.Set_Description(const AValue: WideString);
const OPNAME = 'TAllocationLevel.Set_Description';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocationLevel(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FAllocationLevelID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AllocLevelName', AValue, FDescription, LContextData) then
        begin
          LOldValue := FDescription;
          FDescription := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocLevelName',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationLevel._AddRef: Integer;
const OPNAME = 'TAllocationLevel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationLevel._Release: Integer;
const OPNAME = 'TAllocationLevel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationLevel.Populate (AAllocDefID     : integer;
                                    AAllocLevelID  : integer;
                                    ADescription   : string;
                                    ANrOfRIs       : integer;
                                    ACurtailment   : array of double) : Boolean;
const OPNAME = 'TAllocationLevel.Populate';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FAllocDefID        := AAllocDefID;
    FAllocationLevelID := AAllocLevelID;
    FDescription       := ADescription;
    SetLength(FCurtailment, ANrOfRIs  + 1);
    for lIndex := 1 to ANrOfRIs do
      FCurtailment[lIndex] := ACurtailment[lIndex];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationLevel.Validate (var AErrors    : WideString;
                                    const AContext : WideString) : WordBool;
const OPNAME = 'TAllocationLevel.Validate';
var
  lErrorList : TStringList;
  lErrorCols : TStringList;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      Result := True;
      if AContext = 'AllocLevelName' then
        Result := ValidateAllocLevelName(lErrorList)
      else
      if (AContext = 'Curtailment') then
      begin
        Result := ValidateAllocLevelCurtailment(lErrorList, lErrorCols);
        if (NOT Result) then
          AddErrors(AErrors, lErrorList, lErrorCols);
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil ( LErrorList );
      FreeAndNil ( lErrorCols );
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationLevel.ValidateAllocLevelName ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TAllocationLevel.ValidateAllocLevelName';
var
  lMessage    : string;
  lALlocDef   : IAllocationDefinition;
  lOtherLevel : IAllocationLevel;
  lIndex      : integer;
  lDuplicate  : boolean;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('AllocLevelName', FDescription, lMessage)) then
      AErrorMessages.Add('WARNING:'+FDescription + ':' + lMessage)
    else
    begin
      lALlocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      if (lALlocDef <> nil) then
      begin
        lDuplicate := FALSE;
        lIndex     := 0;
        while ((NOT lDuplicate) AND (lIndex < lALlocDef.NrOfAllocationLevels)) do
        begin
          lOtherLevel := lALlocDef.AllocationLevelByIndex[lIndex];
          if (lOtherLevel.AllocationLevelID <> Self.AllocationLevelID) AND
             (lOtherLevel.Description = Self.Description) then
            lDuplicate := TRUE
          else
            lIndex := lIndex + 1;
        end;
        if (lDuplicate) then
        begin
          AErrorMessages.Add('WARNING:'+'Duplicate allocation Level description');
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationLevel.ValidateAllocLevelCurtailment( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
const OPNAME = 'TAllocationLevel.ValidateAllocLevelCurtailment';
var
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LIndex            : integer;
  LAllocDef    : IAllocationDefinition;
begin
  Result := False;
  try
    if (FAllocDefID > 0) then
    begin
      LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      AErrorColumns.Clear;
      LResult := True;
      LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LIndex := 1 to LAllocDef.NrOfReliabilityClasses do
      begin
        lMessage := '';
        if ( not FAppModules.FieldProperties.ValidateFieldProperty
           ('Curtailment', FloatToStr ( FCurtailment [ LIndex ] ),
           LMessage, LIndex)) then
        begin
          LResult := False;
          AErrorMessages.Add ('ERROR:'+ LMessage );
          AErrorColumns.Add ( IntToStr ( LIndex ) );
          if ( LStopOnFirstError ) then
            Break;
        end;
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TAllocationDefinition                                                      *}
{******************************************************************************}

procedure TAllocationDefinition.CreateMemberObjects;
const OPNAME = 'TAllocationDefinition.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
      FCategoryList         := TObjectList.Create;
      FAllocationLevelList  := TObjectList.Create;
      FSubSystemList        := TObjectList.Create;
      FSpecificOrderList    := TObjectList.Create;
      FFixedPositionList    := TObjectList.Create;
      FSupportChannelList   := TObjectList.Create;
      FDemandDefinitionList := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.DestroyMemberObjects;
const OPNAME = 'TAllocationDefinition.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil ( FCategoryList );
    FreeAndNil ( FAllocationLevelList );
    FreeAndNil ( FSubSystemList );
    FreeAndNil ( FSpecificOrderList );
    FreeAndNil ( FFixedPositionList );
    FreeAndNil ( FSupportChannelList );
    FreeAndNil ( FDemandDefinitionList );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Initialise : boolean;
const OPNAME = 'TAllocationDefinition.Initialise';
var
  lIndex : integer;
begin
  Result := inherited Initialise;
  try
    FName                    := '';
    FStartYear               := 0;
    FStartMonth              := 0;
    FAllocDefFileName        := '';
    FAllocDefFileName        := '';
    FNrOfCategories          := 0;
    FNrOfReliabilityClasses  := 0;
    FNrOfAllocationLevels    := 0;
    FNrOfSubSystems          := 0;
    FNrOfCurveSets           := 0;
    FNrInFixedPosition       := 0;
    FNrInSpecificOrder       := 0;
    FNrOfLoadCases           := 0;
    FNrOfStartingPercentages := 0;
    FNrOfSupportChannels     := 0;
    FNrOfDemandDefinitions   := 0;

    for lIndex := 1 to 12 do
      FDecisionCurveSet[lIndex] := 0;
    FCategoryList.Clear;
    FAllocationLevelList.Clear;
    FSubSystemList.Clear;
    FFixedPositionList.Clear;
    FSpecificOrderList.Clear;
    FSupportChannelList.Clear;
    FDemandDefinitionList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Populate (AAllocDefID               : integer;
                                         AName                     : string;
                                         ASwitchDefID              : integer;
                                         AAllocDefFileName         : string;
                                         AStartYear                : integer;
                                         AStartMonth               : integer;
                                         AEndYear                  : integer;
                                         AEndMonth                 : integer;
                                         ANrOfReliabilityClasses   : integer;
                                         ANrOfLoadCases            : integer;
                                         ANrOfStartingPercentages  : integer;
                                         ANrOfCurveSets            : integer;
                                         APeriodLength             : integer;
                                         ASupportStrategy          : integer;
                                         ABalancingOption          : integer;
                                         ARecurrenceIntervals      : array of integer;
                                         ARecurrenceIntervalLabels : array of string;
                                         ADecisionCurveSet         : array of integer;
                                         AStartingPercentages      : array of double) : Boolean;
const OPNAME = 'TAllocationDefinition.Populate';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FAllocDefID               := AAllocDefID;
    FName                     := AName;
    FSwitchDefID              := ASwitchDefID;
    FAllocDefFileName         := AAllocDefFileName;
    FStartYear                := AStartYear;
    FStartMonth               := AStartMonth;
    FNrOfReliabilityClasses   := ANrOfReliabilityClasses;
    FNrOfLoadCases            := ANrOfLoadCases;
    FNrOfStartingPercentages  := ANrOfStartingPercentages;
    FNrOfCurveSets            := ANrOfCurveSets;
    FPeriodLength             := APeriodLength;
    FSupportStrategy          := ASupportStrategy;
    FBalancingOption          := ABalancingOption;

    SetLength(FRecurrenceIntervals,      FNrOfReliabilityClasses  + 1);
    SetLength(FRecurrenceIntervalLabels, FNrOfReliabilityClasses  + 1);
    SetLength(FStartingPercentages,      FNrOfStartingPercentages + 1);

    for lIndex := 1 to FNrOfReliabilityClasses do
    begin
      FRecurrenceIntervals[lIndex]      := ARecurrenceIntervals[lIndex];
      FRecurrenceIntervalLabels[lIndex] := ARecurrenceIntervalLabels[lIndex]
    end;
    for lIndex := 1 to 12 do
      FDecisionCurveSet[lIndex] := ADecisionCurveSet[lIndex];
    for lIndex := 1 to FNrOfStartingPercentages do
      FStartingPercentages[lIndex] := AStartingPercentages[lIndex];
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.ResetCoefficientDimensions (ANrOfStartStoragePercs : integer;
                                                            ANrOfCurveSets         : integer;
                                                            ANrOfLoadCases         : integer);
const OPNAME = 'TAllocationDefinition.ResetCoefficientDimensions';
var
  lSubSystem   : TSubSystem;
  lPercIdx     : integer;
  lCurveIdx    : integer;
  lCaseIdx     : integer;
  lCount       : integer;
begin
  try
    for lCount := 0 to FNrOfSubSystems - 1 do
    begin
      lSubSystem := CastSubSystemByIndex(lCount);
      if (lSubSystem <> nil) then
      begin
        for lPercIdx := 1 to FNrOfStartingPercentages do
        begin
          for lCurveIdx := 1 to FNrOfCurveSets do
          begin
            for lCaseIdx := 1 to FNrOfLoadCases  do
            begin
              if (lPercIdx > ANrOfStartStoragePercs) OR
                 (lCurveIdx > ANrOfCurveSets) OR
                 (lCaseIdx > ANrOfLoadCases) then
                lSubSystem.RemoveCoefficient(lPercIdx, lCurveIdx, lCaseIdx);
            end;
          end;
        end;
        lSubSystem.SetCoefficientDimensions(ANrOfStartStoragePercs, ANrOfCurveSets, ANrOfLoadCases);
        for lPercIdx := 1 to ANrOfStartStoragePercs do
        begin
          for lCurveIdx := 1 to ANrOfCurveSets do
          begin
            for lCaseIdx := 1 to ANrOfLoadCases do
            begin
              if (lSubSystem.CastCoefficientByPercCurveCase(lPercIdx, lCurveIdx, lCaseIdx) = nil) then
                lSubSystem.NewCoefficient(lPercIdx, lCurveIdx, lCaseIdx);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewUserCategory : IUserCategory;
const OPNAME = 'TAllocationDefinition.NewUserCategory';
var
  lCategory       : TUserCategory;
  lUserCategoryID : integer;
  lLoadAgent      : TAllocationDefLoadAgent;
begin
  Result := nil;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertUserCategory ( FAllocDefID ,lUserCategoryID ) ) then
      begin
        lCategory := CreateCategory;
        lCategory.Initialise;
        lCategory.FAllocDefID := FAllocDefID;
        lCategory.FUserCategoryID := lUserCategoryID;
        lCategory.Description := 'New Category ' + IntToStr(lUserCategoryID);
        Result := lCategory;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TAllocationDefinition.CreateCategory : TUserCategory;
const OPNAME = 'TAllocationDefinition.CreateCategory';
var
  lCategory : TUserCategory;
  lIndex    : integer;
begin
  Result := nil;
  try
    lCategory := TUserCategory.Create(FAppModules);
    FCategoryList.Add(lCategory);
    SetLength(lCategory.FDistribution, FNrOfReliabilityClasses + 1);
    for lIndex := 1 to FNrOfReliabilityClasses do
      lCategory.FDistribution[lIndex] := 0.0;
    FNrOfCategories := FNrOfCategories + 1;
    Result := lCategory;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveUserCategory (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.RemoveUserCategory';
var
  lLoadAgent : TAllocationDefLoadAgent;
  lCategory  : TUserCategory;
begin
  Result := False;
  try
    lCategory := CastCategoryByID(AID);
    if ( lCategory <> nil ) then
    begin
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if ( lLoadAgent.DeleteCategoryByID (AID, lCategory.FAllocDefID ) ) then
        begin
          DeleteCategoryByID ( AID );
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveAllocationLevel (AID : integer) : WordBool; safecall;
const OPNAME = 'TAllocationDefinition.RemoveAllocationLevel';
var
  lLoadAgent       : TAllocationDefLoadAgent;
  lAllocationLevel : TAllocationLevel;
begin
  Result := False;
  try
    lAllocationLevel := CastAllocationLevelByID(AID);
    if ( lAllocationLevel <> nil ) then
    begin
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if ( lLoadAgent.DeleteAllocationLevelByID (AID, lAllocationLevel.AllocDefID ) ) then
        begin
          DeleteAllocationLevelByID ( AID );
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveSubSystem (AID : integer) : WordBool; safecall;
const OPNAME = 'TAllocationDefinition.RemoveSubSystem';
var
  lLoadAgent : TAllocationDefLoadAgent;
  lSubSystem : TSubSystem;
  lOrder     : integer;
  lCount     : integer;
begin
  Result := False;
  try
    lSubSystem := CastSubSystemByID(AID);
    if (lSubSystem <> nil) then
    begin
      lOrder := lSubSystem.Order;
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteSubSystemByID(lSubSystem.SubSystemID, FAllocDefID)) then
        begin
          DeleteSubSystemByID(AID);
          for lCount := 0 to FNrOfSubSystems - 1 do
          begin
            lSubSystem := CastSubSystemByIndex(lCount);
            if (lSubSystem.Order > lOrder) then
              lSubSystem.Order := lSubSystem.Order - 1;
          end;
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteCategoryByID(AID : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteCategoryByID';
var
  lIndex    : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FCategoryList.Count)) do
    begin
      if (TUserCategory(FCategoryList.Items[lIndex]).FUserCategoryID = AID) then
      begin
        Result := TRUE;
        FCategoryList.Delete(lIndex);
        FNrOfCategories := FNrOfCategories - 1;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteCategoryByIndex(AIndex : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteCategoryByIndex';
begin
  Result := False;
  try
    if (FCategoryList.Count > AIndex) then
    begin
      FCategoryList.Delete(AIndex);
      FNrOfCategories := FNrOfCategories - 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewAllocationLevel : IAllocationLevel;
const OPNAME = 'TAllocationDefinition.NewAllocationLevel';
var
  lAllocLevel     : TAllocationLevel;
  lAllocLevelID   : integer;
  lLoadAgent      : TAllocationDefLoadAgent;
begin
  Result := nil;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertAllocationLevel ( FAllocDefID ,lAllocLevelID ) ) then
      begin
        lAllocLevel := CreateAllocationLevel;
        lAllocLevel.Initialise;
        lAllocLevel.FAllocDefID := FAllocDefID;
        lAllocLevel.FAllocationLevelID := lAllocLevelID;
        lAllocLevel.Description := 'New Level ' + IntToStr(lAllocLevelID);
        Result := lAllocLevel;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewSubSystem  : ISubSystem; safecall;
const OPNAME = 'TAllocationDefinition.NewSubSystem';
var
  lSubSystem   : TSubSystem;
  lSubSystemID : integer;
  lLoadAgent   : TAllocationDefLoadAgent;
  lPercIdx     : integer;
  lCurveIdx    : integer;
  lCaseIdx     : integer;
begin
  Result := nil;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertSubSystem(FAllocDefID, lSubSystemID)) then
      begin
        lSubSystem := CreateSubSystem;
        lSubSystem.FAllocDefID  := FAllocDefID;
        lSubSystem.FSubSystemID := lSubSystemID;
        lSubSystem.Order        := FNrOfSubSystems;
        lSubSystem.Name         := 'Sub-System ' + IntToStr(lSubSystemID);
        lSubSystem.SetCoefficientDimensions(FNrOfStartingPercentages, FNrOfCurveSets, FNrOfLoadCases);

        for lPercIdx := 1 to FNrOfStartingPercentages do
          for lCurveIdx := 1 to FNrOfCurveSets do
            for lCaseIdx := 1 to FNrOfLoadCases do
              lSubSystem.NewCoefficient(lPercIdx, lCurveIdx, lCaseIdx);
        Result := lSubSystem;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CreateSubSystem : TSubSystem;
const OPNAME = 'TAllocationDefinition.CreateSubSystem';
var
  lSubSystem  : TSubSystem;
begin
  Result := nil;
  try
    lSubSystem := TSubSystem.Create(FAppModules);
    lSubSystem.Initialise;
    FSubSystemList.Add(lSubSystem);
    FNrOfSubSystems := FNrOfSubSystems + 1;
    Result := lSubSystem;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CreateAllocationLevel : TAllocationLevel;
const OPNAME = 'TAllocationDefinition.CreateAllocationLevel';
var
  lAllocLevel : TAllocationLevel;
  lIndex      : integer;
begin
  Result := nil;
  try
    lAllocLevel := TAllocationLevel.Create(FAppModules);
    FAllocationLevelList.Add(lAllocLevel);
    SetLength(lAllocLevel.FCurtailment, FNrOfReliabilityClasses + 1);
    for lIndex := 1 to FNrOfReliabilityClasses do
      lAllocLevel.FCurtailment[lIndex] := 0.0;
    FNrOfAllocationLevels := FNrOfAllocationLevels + 1;
    Result := lAllocLevel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteAllocationLevelByID (AID : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteAllocationLevelByID';
var
  lIndex    : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FAllocationLevelList.Count)) do
    begin
      if (TAllocationLevel(FAllocationLevelList.Items[lIndex]).FAllocationLevelID = AID) then
      begin
        Result := TRUE;
        FAllocationLevelList.Delete(lIndex);
        FNrOfAllocationLevels := FNrOfAllocationLevels - 1;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteAllocationLevelByIndex (AIndex : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteAllocationLevelByIndex';
begin
  Result := False;
  try
    if (FAllocationLevelList.Count > AIndex) then
    begin
      FAllocationLevelList.Delete(AIndex);
      FNrOfAllocationLevels := FNrOfAllocationLevels - 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteSubSystemByID (AID : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteSubSystemByID';
var
  lSubSystem   : TSubSystem;
  lPercIdx     : integer;
  lCurveIdx    : integer;
  lCaseIdx     : integer;
  lIndex       : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FSubSystemList.Count)) do
    begin
      lSubSystem := CastSubSystemByIndex(lIndex);
      if (lSubSystem.FSubSystemID = AID) then
      begin
        for lPercIdx := 1 to FNrOfStartingPercentages do
        begin
          for lCurveIdx := 1 to FNrOfCurveSets do
          begin
            for lCaseIdx := 1 to FNrOfLoadCases  do
            begin
              lSubSystem.DeleteCoefficient(lPercIdx, lCurveIdx, lCaseIdx)
            end;
          end;
        end;
        FSubSystemList.Delete(lIndex);
        FNrOfSubSystems := FNrOfSubSystems - 1;
        Result := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteSubSystemByIndex (AIndex : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteSubSystemByIndex';
var
  lSubSystem   : TSubSystem;
  lPercIdx     : integer;
  lCurveIdx    : integer;
  lCaseIdx     : integer;
begin
  Result := FALSE;
  try
    if (FSubSystemList.Count > AIndex) then
    begin
      lSubSystem := CastSubSystemByIndex(AIndex);
      if (lSubSystem <> nil) then
      begin
        for lPercIdx := 1 to FNrOfStartingPercentages do
        begin
          for lCurveIdx := 1 to FNrOfCurveSets do
          begin
            for lCaseIdx := 1 to FNrOfLoadCases  do
            begin
              lSubSystem.DeleteCoefficient(lPercIdx, lCurveIdx, lCaseIdx)
            end;
          end;
        end;
        FSubSystemList.Delete(AIndex);
        FNrOfSubSystems := FNrOfSubSystems - 1;
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewSupportChannel : ISupportChannel; safecall;
const OPNAME = 'TAllocationDefinition.NewSupportChannel';
var
  LSupportChannel   : TSupportChannel;
  LLoadAgent        : TAllocationDefLoadAgent;
  lSupportChannelID : integer;
begin
  Result := nil;
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.InsertSupportChannel (FAllocDefID, lSupportChannelID)) then
      begin
        LSupportChannel := CreateSupportChannel;
        LSupportChannel.Initialise;
        LSupportChannel.FAllocDefID := FAllocDefID;
        LSupportChannel.FSupportChannelID := lSupportChannelID;
        Result := LSupportChannel;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CreateSupportChannel : TSupportChannel;
const OPNAME = 'TAllocationDefinition.CreateSupportChannel';
var
  lSupportChannel : TSupportChannel;
begin
  Result := nil;
  try
    lSupportChannel := TSupportChannel.Create(FAppModules);
    lSupportChannel.Initialise;
    FSupportChannelList.Add(lSupportChannel);
    FNrOfSupportChannels := FNrOfSupportChannels + 1;
    Result := lSupportChannel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveSupportChannel (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.RemoveSupportChannel';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LSupportChannel  : TSupportChannel;
begin
  Result := False;
  try
    LSupportChannel := CastSupportChannelByID(AID);
    if ( LSupportChannel <> nil ) then
    begin
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteSupportChannelByID(AID, LSupportChannel.AllocDefID)) then
        begin
          DeleteSupportChannelByID(AID);
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteSupportChannelByIndex(AIndex : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteSupportChannelByIndex';
begin
  Result := FALSE;
  try
    if (FSupportChannelList.Count > AIndex) then
    begin
      FSupportChannelList.Delete(AIndex);
      FNrOfSupportChannels := FNrOfSupportChannels - 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteSupportChannelByID (AID : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteSupportChannelByID';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FSupportChannelList.Count) do
    begin
      if (TSupportChannel(FSupportChannelList.Items[lIndex]).FSupportChannelID = AID) then
      begin
        FSupportChannelList.Delete(lIndex);
        FNrOfSupportChannels := FNrOfSupportChannels - 1;
        Result := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewDemandDefinition : IDemandDefinition;
const OPNAME = 'TAllocationDefinition.NewDemandDefinition';
var
  LDemandDefinition : TDemandDefinition;
  LDemandDefID      : integer;
  LLoadAgent        : TAllocationDefLoadAgent;
begin
  Result := nil;
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.InsertDemandDefinition(FAllocDefID, LDemandDefID)) then
      begin
        LDemandDefinition := CreateDemandDefinition;
        LDemandDefinition.Initialise;
        LDemandDefinition.FAllocDefID := FAllocDefID;
        LDemandDefinition.FDemandDefID := LDemandDefID;
        LDemandDefinition.FName := 'Undefined Demand Centre ' + IntToStr(LDemandDefID);
        Result := LDemandDefinition;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TAllocationDefinition.CreateDemandDefinition: TDemandDefinition;
const OPNAME = 'TAllocationDefinition.CreateDemandDefinition';
var
  lDemandDefinition : TDemandDefinition;
begin
  Result := nil;
  try
    lDemandDefinition := TDemandDefinition.Create ( FAppModules );
    lDemandDefinition.Initialise;
    FDemandDefinitionList.Add(lDemandDefinition);
    FNrOfDemandDefinitions := FNrOfDemandDefinitions + 1;
    Result := lDemandDefinition;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveDemandDefinition (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.RemoveDemandDefinition';
var
  LLoadAgent        : TAllocationDefLoadAgent;
  LDemandDefinition : TDemandDefinition;
  lOrder            : integer;
  lCount            : integer;
  lParentID         : integer;
begin
  Result := False;
  try
    LDemandDefinition := CastDemandDefinitionByID(AID);
    if (LDemandDefinition <> nil) then
    begin
      lOrder     := LDemandDefinition.Order;
      lParentID  := LDemandDefinition.ParentSubSystemID;
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteDemandDefinitionByID(AID, FAllocDefID) ) then
        begin
          DeleteDemandDefinitionByID(AID);
          for lCount := 0 to FNrOfDemandDefinitions - 1 do
          begin
            LDemandDefinition := CastDemandDefinitionByIndex(lCount);
            if (LDemandDefinition.ParentSubSystemID = lParentID) AND (LDemandDefinition.Order > lOrder) then
              LDemandDefinition.Order := LDemandDefinition.Order - 1;
          end;
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TAllocationDefinition.DeleteDemandDefinitionByIndex(AIndex: integer): Boolean;
const OPNAME = 'TAllocationDefinition.DeleteDemandDefinitionByIndex';
begin
  Result := FALSE;
  try
    if (FDemandDefinitionList.Count > AIndex) then
    begin
      FDemandDefinitionList.Delete(AIndex);
      FNrOfDemandDefinitions := FNrOfDemandDefinitions - 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteDemandDefinitionByID (AID : integer): Boolean;
const OPNAME = 'TAllocationDefinition.DeleteDemandDefinitionByID';
var
  lIndex            : integer;
  lDemandDefinition : TDemandDefinition;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FDemandDefinitionList.Count) do
    begin
      lDemandDefinition := CastDemandDefinitionByIndex(lIndex);
      if (lDemandDefinition.FDemandDefID = AID) then
      begin
        FDemandDefinitionList.Delete(lIndex);
        FNrOfDemandDefinitions := FNrOfDemandDefinitions - 1;
        Result := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewFixedPosition : IFixedPosition;
const OPNAME = 'TAllocationDefinition.NewFixedPosition';
var
  LFixedPosition   : TFixedPosition;
  LLoadAgent       : TAllocationDefLoadAgent;
  lFixedPositionID : integer;
begin
  Result := nil;
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if ( LLoadAgent.InsertFixedPosition ( FAllocDefID, lFixedPositionID ) ) then
      begin
        LFixedPosition := CreateFixedPosition;
        LFixedPosition.Initialise;
        LFixedPosition.FAllocDefID := FAllocDefID;
        LFixedPosition.FFixedPositionID := lFixedPositionID;
        Result := LFixedPosition;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CreateFixedPosition : TFixedPosition;
const OPNAME = 'TAllocationDefinition.CreateFixedPosition';
var
  LFixedPosition : TFixedPosition;
begin
  Result := nil;
  try
    LFixedPosition := TFixedPosition.Create(FAppModules);
    LFixedPosition.Initialise;
    FFixedPositionList.Add(LFixedPosition);
    LFixedPosition.FAllocDefID := FAllocDefID;
    FNrInFixedPosition := FNrInFixedPosition + 1;
    Result := LFixedPosition;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveFixedPosition (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.RemoveFixedPosition';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LFixedPosition  : TFixedPosition;
begin
  Result := False;
  try
    LFixedPosition := CastFixedPositionByID(AID);
    if ( LFixedPosition <> nil ) then
    begin
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteFixedPosition(FAllocDefID, AID)) then
        begin
          DeleteFixedPositionByID(AID);
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteFixedPositionByIndex (AIndex : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.DeleteFixedPositionByIndex';
begin
  Result := FALSE;
  try
    if (FFixedPositionList.Count > AIndex) then
    begin
      FFixedPositionList.Delete(AIndex);
      FNrInFixedPosition := FNrInFixedPosition - 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteFixedPositionByID (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.DeleteFixedPositionByID';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FFixedPositionList.Count) do
    begin
      if (TFixedPosition(FFixedPositionList.Items[lIndex]).FFixedPositionID = AID) then
      begin
        FFixedPositionList.Delete(lIndex);
        FNrInFixedPosition := FNrInFixedPosition - 1;
        Result := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.NewSpecificOrder : ISpecificOrder;
const OPNAME = 'TAllocationDefinition.NewSpecificOrder';
var
  LSpecificOrder     : TSpecificOrder;
  LLoadAgent         : TAllocationDefLoadAgent;
  lSpecificOrderID   : integer;
begin
  Result := nil;
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if ( LLoadAgent.InsertSpecificOrder (FAllocDefID, lSpecificOrderID) ) then
      begin
        LSpecificOrder := CreateSpecificOrder;
        LSpecificOrder.Initialise;
        LSpecificOrder.FAllocDefID := FAllocDefID;
        LSpecificOrder.FSpecificOrderID := lSpecificOrderID;
        Result := LSpecificOrder;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CreateSpecificOrder : TSpecificOrder;
const OPNAME = 'TAllocationDefinition.CreateSpecificOrder';
begin
  Result := nil;
  try
    Result :=  TSpecificOrder.Create ( FAppModules );
    FSpecificOrderList.Add(Result);
    Result.FAllocDefID := FAllocDefID;
    FNrInSpecificOrder := FNrInSpecificOrder + 1;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.RemoveSpecificOrder (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinition.RemoveSpecificOrder';
var
  LLoadAgent      : TAllocationDefLoadAgent;
  lSpecificOrder  : TSpecificOrder;
begin
  Result := False;
  try
    lSpecificOrder := CastSpecificOrderByID(AID);
    if (lSpecificOrder <> nil) then
    begin
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteSpecificOrder(FAllocDefID, AID)) then
        begin
          DeleteSpecificOrderByID(AID);
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteSpecificOrderByIndex (AIndex : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteSpecificOrderByIndex';
begin
  Result := False;
  try
    if (FSpecificOrderList.Count > AIndex) then
    begin
      FSpecificOrderList.Delete(AIndex);
      FNrInSpecificOrder := FNrInSpecificOrder - 1;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.DeleteSpecificOrderByID (AID : integer) : Boolean;
const OPNAME = 'TAllocationDefinition.DeleteSpecificOrderByID';
var
  lIndex : integer;
begin
  Result := False;
  try
    lIndex := 0;
    while (NOT Result) AND (lIndex < FSpecificOrderList.Count) do
    begin
      if (TSpecificOrder(FSpecificOrderList.Items[lIndex]).FSpecificOrderID = AID) then
      begin
        FSpecificOrderList.Delete(lIndex);
        FNrInSpecificOrder := FNrInSpecificOrder - 1;
        Result := True;
      end
      else
        lIndex := lIndex + 1;  
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_AllocationDefinitionID : integer;
const OPNAME = 'TAllocationDefinition.Get_AllocationDefinitionID';
begin
  Result := 0;
  try
    Result := FAllocDefID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_Name : WideString;
const OPNAME = 'TAllocationDefinition.Get_Name';
begin
  Result := '';
  try
    Result := FName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_Name (const AValue : WideString);
const OPNAME = 'TAllocationDefinition.Set_Name';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AllocDefName', AValue, FName, LContextData) then
        begin
          LOldValue := FName;
          FName := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocDefName',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_AllocDefFileName: WideString;
const OPNAME = 'TAllocationDefinition.Get_AllocDefFileName';
begin
  Result := '';
  try
    Result := FAllocDefFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_AllocDefFileName(const Value: WideString);
const OPNAME = 'TAllocationDefinition.Set_AllocDefFileName';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AllocDefFileName', Value, FAllocDefFileName, LContextData) then
        begin
          LOldValue := FAllocDefFileName;
          FAllocDefFileName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocDefFileName',LOldValue,Value);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME) end;
end;

function TAllocationDefinition.Get_StartYear : integer;
const OPNAME = 'TAllocationDefinition.Get_StartYear';
begin
  Result := 0;
  try
    Result := FStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_StartYear (AValue : integer);
const OPNAME = 'TAllocationDefinition.Set_StartYear';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        //LLoadAgent.LoadContextData_FamilyFileInDamfile(LContextData,IntToStr(FSwitchDefID));
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AllocDefStartYear', IntToStr(AValue), IntToStr(FStartYear), LContextData) then
        begin
          LOldValue := IntToStr(FStartYear);
          FStartYear := AValue;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocDefStartYear',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_StartMonth : integer;
const OPNAME = 'TAllocationDefinition.Get_StartMonth';
begin
  Result := 0;
  try
    Result := FStartMonth;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_StartMonth (AValue : integer);
const OPNAME = 'TAllocationDefinition.Set_StartMonth';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        //LLoadAgent.LoadContextData_FamilyFileInDamfile(LContextData,IntToStr(FSwitchDefID));
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AllocDefStartMonth', IntToStr(AValue), IntToStr(FStartMonth), LContextData) then
        begin
          LOldValue := IntToStr(FStartMonth);
          FStartMonth := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AllocDefStartMonth',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfCategories: integer;
const OPNAME = 'TAllocationDefinition.Get_NrOfCategories';
begin
  Result := 0;
  try
    Result := FNrOfCategories;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfCurveSets : integer;
const OPNAME = 'TAllocationDefinition.Get_NrOfCurveSets';
begin
  Result := 0;
  try
    Result := FNrOfCurveSets;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfLoadCases : integer;
const OPNAME = 'TAllocationDefinition.Get_NrOfLoadCases';
begin
  Result := 0;
  try
    Result := FNrOfLoadCases;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfReliabilityClasses : integer;
const OPNAME = 'TAllocationDefinition.Get_NrOfReliabilityClasses';
begin
  try
    Result := FNrOfReliabilityClasses;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfStartingPercentages : integer;
const OPNAME = 'TAllocationDefinition.Get_NrOfStartingPercentages';
begin
  try
    Result := FNrOfStartingPercentages;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfSubSystems : integer;
const OPNAME = 'TAllocationDefinition.Get_NrOfSubSystems';
begin
  Result := 0;
  try
    Result := FNrOfSubSystems;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_PeriodLength : integer;
const OPNAME = 'TAllocationDefinition.Get_PeriodLength';
begin
  Result := 0; 
  try
    Result := FPeriodLength
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_RecurrenceIntervalByIndex(AIndex : integer): integer;
const OPNAME = 'TAllocationDefinition.Get_RecurrenceIntervalByIndex';
begin
  try
    Result := FRecurrenceIntervals[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_RILabelByIndex(AIndex : integer): WideString;
const OPNAME = 'TAllocationDefinition.Get_RILabelByIndex';
begin
  Result := '';
  try
    Result := FRecurrenceIntervalLabels[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_NrOfCurveSets(AValue: integer);
const OPNAME = 'TAllocationDefinition.Set_NrOfCurveSets';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LIndex       : integer;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'NrOfCurveSets', IntToStr(AValue), IntToStr(FNrOfCurveSets), LContextData) then
        begin
          ResetCoefficientDimensions(FNrOfStartingPercentages, AValue, FNrOfLoadCases);
          LOldValue := IntToStr(FNrOfCurveSets);
          FNrOfCurveSets := AValue;
          if (LOldValue = '0') AND (AValue <> 0) then
          begin
            for lIndex := 1 to 12 do
              Set_DecisionCurveSetByMonth(lIndex, 1);
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NrOfCurveSets',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_NrOfLoadCases(AValue: integer);
const OPNAME = 'TAllocationDefinition.Set_NrOfLoadCases';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if (FNrOfLoadCases <> AValue) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
          if FAppModules.FieldProperties.UpdateFieldValue(
             'NrOfLoadCases', IntToStr(AValue), IntToStr(FNrOfLoadCases), LContextData) then
          begin
            if (FNrOfLoadCases > AValue) then
              if (MessageDlg(FAppModules.Language.GetString('Message.subsystemsLoadCaseChange'),
                  mtConfirmation,mbYesNoCancel,0) <> mrYes) then
                Exit;
            ResetCoefficientDimensions(FNrOfStartingPercentages, FNrOfCurveSets, AValue);
            LOldValue := IntToStr(FNrOfLoadCases);
            FNrOfLoadCases := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'NrOfLoadCases',LOldValue,IntToStr(AValue));
          end;
        finally
          FreeAndNil(LContextData);
        end;
      finally
        FreeAndNil(LLoadAgent);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_NrOfReliabilityClasses(AValue: integer);
const OPNAME = 'TAllocationDefinition.Set_NrOfReliabilityClasses';
var
  lCount      : integer;
  lIndex      : integer;
  lCategory   : TUserCategory;
  lAllocLevel : TAllocationLevel;

  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'NrOfReliabilityClasses', IntToStr(AValue), IntToStr(FNrOfReliabilityClasses), LContextData) then
        begin
          LOldValue := IntToStr(FNrOfReliabilityClasses);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NrOfReliabilityClasses',LOldValue,IntToStr(AValue));

          SetLength(FRecurrenceIntervals, AValue + 1);
          SetLength(FRecurrenceIntervalLabels, AValue + 1);
          for lIndex := FNrOfReliabilityClasses + 1 to AValue do
          begin
            FRecurrenceIntervals[lIndex] := 0;
            FRecurrenceIntervalLabels[lIndex] := '';
          end;
          for lCount := 0 to FNrOfCategories - 1 do
          begin
            lCategory := TUserCategory(FCategoryList.Items[lCount]);
            SetLength(lCategory.FDistribution, AValue + 1);
            for lIndex := FNrOfReliabilityClasses + 1 to AValue do
              lCategory.FDistribution[lIndex] := 0.0;
          end;
          for lCount := 0 to FNrOfAllocationLevels - 1 do
          begin
            lAllocLevel := TAllocationLevel(FAllocationLevelList.Items[lCount]);
            SetLength(lAllocLevel.FCurtailment, AValue + 1);
            for lIndex := FNrOfReliabilityClasses + 1 to AValue do
              lAllocLevel.FCurtailment[lIndex] := 0.0;
          end;
          FNrOfReliabilityClasses := AValue;
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_NrOfStartingPercentages(AValue: integer);
const OPNAME = 'TAllocationDefinition.Set_NrOfStartingPercentages';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
  lIndex       : integer;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'NrOfStartStoragePercs', IntToStr(AValue), IntToStr(FNrOfStartingPercentages), LContextData) then
        begin
          SetLength(FStartingPercentages, AValue + 1);
          for lIndex := FNrOfStartingPercentages + 1 to AValue do
            FStartingPercentages[lIndex] := 0.0;

          ResetCoefficientDimensions(AValue, FNrOfCurveSets, FNrOfLoadCases);
          LOldValue := IntToStr(FNrOfStartingPercentages);
          FNrOfStartingPercentages := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NrOfStartStoragePercs',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_PeriodLength(AValue: integer);
const OPNAME = 'TAllocationDefinition.Set_PeriodLength';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PeriodLength', IntToStr(AValue), IntToStr(FPeriodLength), LContextData) then
        begin
          LOldValue := IntToStr(FPeriodLength);
          FPeriodLength := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PeriodLength',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_RecurrenceIntervalByIndex(AIndex: integer; AValue: integer);
const OPNAME = 'TAllocationDefinition.Set_RecurrenceIntervalByIndex';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefIDFieldNameID(LContextData, IntToStr(FAllocDefID), IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'RIValue', IntToStr(AValue), IntToStr(FRecurrenceIntervals[AIndex]), LContextData) then
        begin
          LOldValue := IntToStr(FRecurrenceIntervals[AIndex]);
          FRecurrenceIntervals[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RIValue',LOldValue,IntToStr(AValue));

          if (Trim(FRecurrenceIntervalLabels[AIndex]) = '') then
            Set_RILabelByIndex(AIndex, '1/' + IntToStr(FRecurrenceIntervals[AIndex]));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_RILabelByIndex(AIndex: integer; const AValue: WideString);
const OPNAME = 'TAllocationDefinition.Set_RILabelByIndex ';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefIDFieldNameID(LContextData, IntToStr(FAllocDefID), IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'RILabel', AValue, FRecurrenceIntervalLabels[AIndex], LContextData) then
        begin
          LOldValue := IntToStr(FRecurrenceIntervals[AIndex]);
          FRecurrenceIntervalLabels[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RILabel',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastCategoryByIndex (AIndex : integer) : TUserCategory;
const OPNAME = 'TAllocationDefinition.CastCategoryByIndex';
begin
  Result := nil;
  try
    Result := TUserCategory(FCategoryList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_CategoryByIndex (AIndex : integer) : IUserCategory;
const OPNAME = 'TAllocationDefinition.Get_CategoryByIndex';
begin
  Result := nil;
  try
    Result := CastCategoryByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastCategoryByID (AID : integer) : TUserCategory;
const OPNAME = 'TAllocationDefinition.CastCategoryByID';
var
  lIndex    : integer;
  lCategory : TUserCategory;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FCategoryList.Count)) do
    begin
      lCategory := TUserCategory(FCategoryList.Items[lIndex]);
      if (lCategory.FUserCategoryID = AID) then
        Result := lCategory
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_CategoryByID (AID : integer) : IUserCategory;
const OPNAME = 'TAllocationDefinition.Get_CategoryByID';
begin
  Result := nil;
  try
    Result := CastCategoryByID(AID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastAllocationLevelByID (AID : integer) : TAllocationLevel;
const OPNAME = 'TAllocationDefinition.CastAllocationLevelByID';
var
  lIndex      : integer;
  lAllocLevel : TAllocationLevel;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FAllocationLevelList.Count)) do
    begin
      lAllocLevel := TAllocationLevel(FAllocationLevelList.Items[lIndex]);
      if (lAllocLevel.FAllocationLevelID = AID) then
        Result := lAllocLevel
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastAllocationLevelByIndex (AIndex : integer) : TAllocationLevel;
const OPNAME = 'TAllocationDefinition.CastAllocationLevelByIndex';
begin
  Result := nil;
  try
    Result := TAllocationLevel(FAllocationLevelList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_AllocationLevelByIndex (AIndex : integer) : IAllocationLevel;
const OPNAME = 'TAllocationDefinition.Get_AllocationLevelByIndex';
begin
  Result := nil;
  try
    Result := CastAllocationLevelByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastSubSystemByIndex (AIndex : integer) : TSubSystem;
const OPNAME = 'TAllocationDefinition.CastSubSystemByIndex';
begin
  Result := nil;
  try
    Result := TSubSystem(FSubSystemList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SubSystemByIndex (AIndex : integer) : ISubSystem;
const OPNAME = 'TAllocationDefinition.Get_SubSystemByIndex';
begin
  Result := nil;
  try
    Result := CastSubSystemByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastSubSystemByID (AID : integer) : TSubSystem;
const OPNAME = 'TAllocationDefinition.CastSubSystemByID';
var
  lIndex     : integer;
  lSubSystem : TSubSystem;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSubSystemList.Count)) do
    begin
      lSubSystem := TSubSystem(FSubSystemList.Items[lIndex]); 
      if (lSubSystem.FSubSystemID = AID) then
        Result := lSubSystem
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SubSystemByID (AID : integer) : ISubSystem;
const OPNAME = 'TAllocationDefinition.Get_SubSystemByID';
begin
  Result := nil;
  try
    Result := CastSubSystemByID(AID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_DecisionCurveSetByMonth (AMonth : integer) : integer;
const OPNAME = 'TAllocationDefinition.Get_DecisionCurveSetByMonth';
begin
  Result := 0;
  try
    Result := FDecisionCurveSet[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_DecisionCurveSetByMonth (AMonth : integer;
                                                             AValue : integer);
const OPNAME = 'TAllocationDefinition.Set_DecisionCurveSetByMonth';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefIDFieldNameID(LContextData, IntToStr(FAllocDefID), IntToStr(AMonth));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MonthCurveSet', IntToStr(AValue), IntToStr(FDecisionCurveSet[AMonth]), LContextData) then
        begin
          LOldValue := IntToStr(FDecisionCurveSet[AMonth]);
          FDecisionCurveSet[AMonth] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthCurveSet',LOldValue,IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_StartingPercentageByIndex (AIndex : integer) : double;
const OPNAME = 'TAllocationDefinition.Get_StartingPercentageByIndex';
begin
  Result := 0.0;
  try
    Result := FStartingPercentages[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAllocationDefinition.Set_StartingPercentageByIndex (AIndex : integer;
                                                              AValue : double);
const OPNAME = 'TAllocationDefinition.Set_StartingPercentageByIndex';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefIDFieldNameID(LContextData, IntToStr(FAllocDefID), IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'StartStoragePerc', FloatToStr(AValue), FloatToStr(FStartingPercentages[AIndex]), LContextData) then
        begin
          LOldValue := FloatToStr(FStartingPercentages[AIndex]);
          FStartingPercentages[AIndex] := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartStoragePerc',LOldValue,FloatToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrInFixedPosition : integer;
const OPNAME = 'TAllocationDefinition.Get_NrInFixedPosition';
begin
  Result := 0;
  try
    Result := FNrInFixedPosition;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfAllocationLevels: Integer; safecall;
const OPNAME = 'TAllocationDefinition.Get_NrOfAllocationLevels';
begin
  Result := 0;
  try
    Result := FNrOfAllocationLevels;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfSupportChannels: Integer; safecall;
const OPNAME = 'TAllocationDefinition.Get_NrOfSupportChannels';
begin
  Result := 0;
  try
    Result := FNrOfSupportChannels;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrOfDemandDefinitions: Integer; safecall;
const OPNAME = 'TAllocationDefinition.Get_NrOfDemandDefinitions';
begin
  Result := 0;
  try
    Result := FNrOfDemandDefinitions;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_NrInSpecificOrder : integer;
const OPNAME = 'TAllocationDefinition.Get_NrInSpecificOrder';
begin
  Result := 0;
  try
    Result := FNrInSpecificOrder;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_FixedPositionByIndex (AIndex: integer): IFixedPosition;
const OPNAME = 'TAllocationDefinition.Get_FixedPositionByIndex';
begin
  Result := nil;
  try
    Result := TFixedPosition( FFixedPositionList.Items [ AIndex ]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_FixedPositionByID (AID: integer): IFixedPosition;
const OPNAME = 'TAllocationDefinition.Get_FixedPositionByID';
var
  lIndex    : integer;
  lFixedPos : TFixedPosition;
begin
  Result := nil;
  try
    lIndex := 0;
    while (Result = nil) AND (lIndex < FFixedPositionList.Count) do
    begin
      lFixedPos := TFixedPosition(FFixedPositionList.Items[lIndex]);
      if (lFixedPos.FFixedPositionID = AID) then
        Result := lFixedPos
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SpecificOrderByIndex (AIndex: integer): ISpecificOrder;
const OPNAME = 'TAllocationDefinition.Get_SpecificOrderByIndex';
begin
  Result := nil;
  try
    Result := TSpecificOrder(FSpecificOrderList.Items [ AIndex ]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SpecificOrderByID (AID: integer): ISpecificOrder;
const OPNAME = 'TAllocationDefinition.Get_SpecificOrderByID';
var
  lIndex    : integer;
  lFixedPos : TSpecificOrder;
begin
  Result := nil;
  try
    lIndex := 0;
    while (Result = nil) AND (lIndex < FSpecificOrderList.Count) do
    begin
      lFixedPos := TSpecificOrder(FSpecificOrderList.Items[lIndex]);
      if (lFixedPos.FSpecificOrderID = AID) then
        Result := lFixedPos
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastSupportChannelByIndex (AIndex : integer) : TSupportChannel;
const OPNAME = 'TAllocationDefinition.CastSupportChannelByIndex';
begin
  Result := nil;
  try
    Result := TSupportChannel(FSupportChannelList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SupportChannelByIndex (AIndex : integer) : ISupportChannel;
const OPNAME = 'TAllocationDefinition.Get_SupportChannelByIndex';
begin
  Result := nil;
  try
    Result := CastSupportChannelByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastSupportChannelByID (AID : integer) : TSupportChannel;
const OPNAME = 'TAllocationDefinition.CastSupportChannelByID';
var
  lIndex   : integer;
  lChannel : TSupportChannel;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSupportChannelList.Count)) do
    begin
      lChannel := TSupportChannel(FSupportChannelList.Items[lIndex]);
      if (lChannel.SupportChannelID = AID) then
        Result := lChannel
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SupportChannelByID (AID : integer) : ISupportChannel;
const OPNAME = 'TAllocationDefinition.Get_SupportChannelByID';
begin
  Result := nil;
  try
    Result := CastSupportChannelByID(AID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastDemandDefinitionByIndex(AIndex: integer): TDemandDefinition;
const OPNAME = 'TAllocationDefinition.CastDemandDefinitionByIndex';
begin
  Result := nil;
  try
    Result := TDemandDefinition(FDemandDefinitionList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastDemandDefinitionByID (AID : integer) : TDemandDefinition;
const OPNAME = 'TAllocationDefinition.CastDemandDefinitionByID';
var
  lIndex   : integer;
  lCentre  : TDemandDefinition;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FDemandDefinitionList.Count)) do
    begin
      lCentre := TDemandDefinition(FDemandDefinitionList.Items[lIndex]);
      if (lCentre.DemandDefID = AID) then
        Result := lCentre
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_DemandDefinitionByID (AID : integer): IDemandDefinition;
const OPNAME = 'TAllocationDefinition.Get_DemandDefinitionByID';
begin
  Result := nil;
  try
    Result := CastDemandDefinitionByID(AID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastFixedPositionByIndex ( AIndex : integer ) : TFixedPosition;
const OPNAME = 'TAllocationDefinition.CastFixedPositionByIndex';
begin
  Result := nil;
  try
    Result := TFixedPosition(FFixedPositionList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastFixedPositionByID (AID : integer) : TFixedPosition;
const OPNAME = 'TAllocationDefinition.CastFixedPositionByID';
var
  lIndex         : integer;
  lFixedPosition : TFixedPosition;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FFixedPositionList.Count)) do
    begin
      lFixedPosition := TFixedPosition(FFixedPositionList.Items[lIndex]);
      if (lFixedPosition.FixedPositionID = AID) then
        Result := lFixedPosition
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastSpecificOrderByIndex ( AIndex : integer ) : TSpecificOrder;
const OPNAME = 'TAllocationDefinition.CastSpecificOrderByIndex';
begin
  Result := nil;
  try
    Result := TSpecificOrder(FSpecificOrderList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.CastSpecificOrderByID (AID : integer) : TSpecificOrder;
const OPNAME = 'TAllocationDefinition.CastSpecificOrderByID';
var
  lIndex         : integer;
  lSpecificOrder : TSpecificOrder;
begin
  Result := nil;
  try
    lIndex := 0;
    while (Result = nil) AND (lIndex < FSpecificOrderList.Count) do
    begin
      lSpecificOrder := TSpecificOrder(FSpecificOrderList.Items[lIndex]);
      if (lSpecificOrder.FSpecificOrderID = AID) then
        Result := lSpecificOrder
      else
        lIndex := lIndex + 1;
    end;      
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_DemandDefinitionByIndex(AIndex: integer): IDemandDefinition;
const OPNAME = 'TAllocationDefinition.Get_DemandDefinitionByIndex';
begin
  Result := nil;
  try
    Result := CastDemandDefinitionByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Get_SupportStrategy: Integer;
const OPNAME = 'TAllocationDefinition.Get_SupportStrategy';
begin
  Result := 0;
  try
    Result := FSupportStrategy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAllocationDefinition.Set_SupportStrategy(Value: Integer);
const OPNAME = 'TAllocationDefinition.Set_SupportStrategy';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportStrategy', IntToStr(Value), IntToStr(FSupportStrategy), LContextData) then
        begin
          LOldValue := IntToStr(FSupportStrategy);
          FSupportStrategy := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupportStrategy',LOldValue,IntToStr(Value));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.Get_BalancingOption: Integer;
const OPNAME = 'TAllocationDefinition.Get_BalancingOption';
begin
  Result := 0;
  try
    Result := FBalancingOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAllocationDefinition.Set_BalancingOption(Value: Integer);
const OPNAME = 'TAllocationDefinition.Set_BalancingOption';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_AllocDefID(LContextData, IntToStr(FAllocDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'BalancingOption', IntToStr(Value), IntToStr(FBalancingOption), LContextData) then
        begin
          LOldValue := IntToStr(FBalancingOption);
          FBalancingOption := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'BalancingOption',LOldValue,IntToStr(Value));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.Validate (var AErrors    : WideString;
                                         const AContext : WideString) : WordBool;
const OPNAME = 'TAllocationDefinition.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
  lErrorCols        : TStringList;
  LIndex            : integer;
  lColFlag          : Boolean;
begin
  Result := FALSE;
  try
    LErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      lColFlag := FALSE;
      if (AContext = 'AllocDefName') then
        Result := ValidateAllocDefName(lErrorList)
      else
      if (AContext = 'AllocDefFileName') then
        Result := ValidateAllocDefFileName(lErrorList)
      else
      if (AContext = 'AllocDefStartYear') then
        Result := ValidateAllocDefStartYear(lErrorList)
      else
      if (AContext = 'AllocDefStartMonth') then
        Result := ValidateAllocDefStartMonth(lErrorList)
      else
      if (AContext = 'NrOfReliabilityClasses') then
        Result := ValidateAllocDefNrOfReliabilityClasses(lErrorList)
      else
      if (AContext = 'PeriodLength') then
        Result := ValidateAllocDefPeriodLength(lErrorList)
      else
      if (AContext = 'NrOfLoadCases') then
        Result := ValidateAllocDefNrOfLoadCases(lErrorList)
      else
      if (AContext = 'NrOfStartStoragePercs') then
        Result := ValidateAllocDefNrOfStartStoragePercs(lErrorList)
      else
      if (AContext = 'NrOfCurveSets') then
        Result := ValidateAllocDefNrOfCurveSets(lErrorList)
      else
      if (AContext = 'SupportStrategy') then
        Result := ValidateSupportStrategy(lErrorList)
      else
      if (AContext = 'BalancingOption') then
        Result := ValidateBalancingOption(lErrorList)
      else
      if (AContext = 'MonthCurveSet') then
      begin
        Result := ValidateMonthCurveSet(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end
      else
      if (AContext = 'RIValue') then
      begin
        Result := ValidateAllocDefRIValue(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end
      else
      if (AContext = 'RILabel') then
      begin
        Result := ValidateAllocDefRILabel(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end
      else
      if (AContext = 'FixedPosition') then
      begin
        Result := ValidateFixedPosition(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end
      else
      if (AContext = 'SpecificOrder') then
      begin
        Result := ValidateSpecificOrder(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateAllocDefName(lErrorList)) then
          Result := False;
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefStartYear(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefStartMonth(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefNrOfReliabilityClasses(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefPeriodLength(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefNrOfLoadCases(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefNrOfStartStoragePercs(lErrorList)) then
            Result := False;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefNrOfCurveSets(lErrorList)) then
            Result := False;
        end
        else
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSupportStrategy(lErrorList)) then
            Result := FALSE;
        end
        else
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateBalancingOption(lErrorList)) then
            Result := FALSE;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefRIValue ( lErrorList, lErrorCols)) then
            Result := False;
        end
        else
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateAllocDefRILabel(lErrorList, lErrorCols)) then
            Result := FALSE;
        end
        else
        if (Result or (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateMonthCurveSet ( lErrorList, lErrorCols)) then
            Result := False;
        end
        else
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateFixedPosition(lErrorList, lErrorCols)) then
            Result := FALSE;
        end
        else
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateSpecificOrder(lErrorList, lErrorCols)) then
            Result := FALSE;
        end
        else
        begin
          for  LIndex := 0 to FCategoryList.Count -1 do
          begin
            if (NOT CastCategoryByIndex ( LIndex ).Validate ( AErrors, AContext)) then
              Result := False;
            if ( (NOT Result ) and LStopOnFirstError ) then
              Break;
          end;
          for  LIndex := 0 to FAllocationLevelList.Count -1 do
          begin
            if (NOT CastAllocationLevelByIndex ( LIndex ).Validate ( AErrors, AContext)) then
              Result := False;
            if ( (NOT Result ) and LStopOnFirstError ) then
              Break;
          end;
          for  LIndex := 0 to FDemandDefinitionList.Count -1 do
          begin
            if (NOT CastDemandDefinitionByIndex ( LIndex ).Validate ( AErrors, AContext )) then
              Result := False;
            if ( (NOT Result ) and LStopOnFirstError ) then
              Break;
          end;
          for  LIndex := 0 to FSupportChannelList.Count -1 do
          begin
            if (NOT CastSupportChannelByIndex ( LIndex ).Validate ( AErrors, AContext )) then
              Result := False;
            if ( (NOT Result ) and LStopOnFirstError ) then
              Break;
          end;
          for  LIndex := 0 to FSubSystemList.Count -1 do
          begin
            if (NOT CastSubSystemByIndex ( LIndex ).Validate ( AErrors, AContext )) then
              Result := False;
            if ( (NOT Result ) and LStopOnFirstError ) then
              Break;
          end;
        end;
      end;
      if (lColFlag AND (NOT Result)) then
      begin
        if (lErrorCols.Count = 0) then
          AErrors := AErrors + lErrorList.Text
        else
          AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                               CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
        lErrorList.Clear;
        lErrorCols.Clear;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil (lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefName (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefName';
var
  lAllocDefList : TAllocationDefinitionsList;
  lMessage      : string;
  lUnique       : Boolean;
  lIndex        : integer;
  LAllocDef     : TAllocationDefinition;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty( 'AllocDefName', FName, lMessage )) then
      AErrorMessages.Add ( 'WARNING:'+FName + ':' + lMessage )
    else
    begin
      lAllocDefList := TPlanningModelDataObject(FAppModules.Model.ModelData).
                         CastAllocationDefinitionsList;
      lUnique := True;
      lIndex  := 0;
      while ( lUnique and ( lIndex < LAllocDefList.AllocationDefinitionCount )) do
      begin
        LAllocDef := LAllocDefList.CastAllocationDefinitionByIndex(LIndex);
        if ( ( FAllocDefID <> LAllocDef.FAllocDefID ) and
            ( UpperCase ( Trim ( FName )) = UpperCase ( Trim ( LAllocDef.Name )))) then
        begin
          lMessage := FAppModules.Language.GetString ( 'ContextValidation.DuplicateAllocDefName' );
          AErrorMessages.Add ('WARNING:'+ Format ( lMessage, [ FName ] ));
          //lUnique := False;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TAllocationDefinition.ValidateAllocDefFileName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefFileName';
var
  lAllocDefList : TAllocationDefinitionsList;
  lMessage      : string;
  lUnique       : Boolean;
  lIndex        : integer;
  LAllocDef     : TAllocationDefinition;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('AllocDefFileName', FName, lMessage)) then
      AErrorMessages.Add('WARNING:'+FName + ':' + lMessage)
    else
    begin
      lAllocDefList := TPlanningModelDataObject(FAppModules.Model.ModelData).
                         CastAllocationDefinitionsList;
      lUnique := True;
      lIndex  := 0;
      while (lUnique and (lIndex < LAllocDefList.AllocationDefinitionCount)) do
      begin
        LAllocDef := LAllocDefList.CastAllocationDefinitionByIndex(LIndex);
        if ((FAllocDefID <> LAllocDef.FAllocDefID) and
            (UpperCase(Trim(FAllocDefFileName)) = UpperCase(Trim(LAllocDef.FAllocDefFileName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateAllocDefFileName');
          AErrorMessages.Add('WARNING:'+Format(lMessage, [FAllocDefFileName]));
          //lUnique := False;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinition.ValidateAllocDefStartYear (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefStartYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('AllocDefStartYear', IntToStr(FStartYear), LMessage);
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+IntToStr(FStartYear) + ':' +  LMessage)
    else
    if (FStartYear = 0) then
    begin
      lMessage := FAppModules.Language.GetString ('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefStartMonth (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefStartMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('AllocDefStartMonth', IntToStr(FStartYear), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FStartMonth) + ':' +  LMessage)
    else
    if (FStartMonth = 0) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefNrOfReliabilityClasses ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefNrOfReliabilityClasses';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'NrOfReliabilityClasses', IntToStr ( FNrOfReliabilityClasses ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ( 'ERROR:'+ IntToStr ( FNrOfReliabilityClasses ) + ':' +  LMessage );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.ValidateAllocDefPeriodLength ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefPeriodLength';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'PeriodLength', IntToStr ( FPeriodLength ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ( 'ERROR:'+IntToStr ( FPeriodLength ) + ':' +  LMessage );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.ValidateSupportStrategy (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateSupportStrategy';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SupportStrategy', IntToStr(FSupportStrategy), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FSupportStrategy) + ':' +  LMessage);
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TAllocationDefinition.ValidateBalancingOption (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateBalancingOption';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('BalancingOption', IntToStr(FBalancingOption), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FBalancingOption) + ':' +  LMessage);
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefNrOfLoadCases(AErrorMessages : TStrings): WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefNrOfLoadCases';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('NrOfLoadCases', IntToStr(FNrOfLoadCases), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FNrOfLoadCases)+':'+LMessage);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefNrOfStartStoragePercs ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefNrOfStartStoragePercs';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'NrOfStartStoragePercs', IntToStr ( FNrOfStartingPercentages ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ IntToStr ( FNrOfStartingPercentages ) + ':' +  LMessage );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.ValidateAllocDefNrOfCurveSets ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefNrOfCurveSets';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'NrOfCurveSets', IntToStr ( FNrOfCurveSets ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ IntToStr ( FNrOfCurveSets ) + ':' +  LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAllocationDefinition.ValidateMonthCurveSet ( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateMonthCurveSet';
var
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LIndex            : integer;
begin
  Result := False;
  try
    AErrorColumns.Clear;
    LResult := True;
    LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 1 to 12 do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('MonthCurveSet', IntToStr ( FDecisionCurveSet [ LIndex ] ),
         LMessage, LIndex)) then
      begin
        LResult := False;
        AErrorMessages.Add ( 'ERROR:'+LMessage );
        AErrorColumns.Add ( IntToStr ( LIndex ));
        if ( LStopOnFirstError ) then
          Break;
      end
      else
      begin
        if (FDecisionCurveSet[lIndex] > FNrOfCurveSets) then
        begin
          LResult := False;
          LMessage := FAppModules.Language.GetString ( 'ContextValidation.InvalidDecisionCurveSet' );
          AErrorMessages.Add ( 'ERROR:'+LMessage );
          AErrorColumns.Add ( IntToStr ( LIndex ));
          if ( LStopOnFirstError ) then
            Break;
        end
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefRIValue ( AErrorMessages : TStrings; AErrorColumns  : TStringList ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefRIValue';
var
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LIndex            : integer;
begin
  Result := False;
  try
    AErrorColumns.Clear;
    LResult := True;
    LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 1 to FNrOfReliabilityClasses do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('RIValue', FloatToStr ( FRecurrenceIntervals [ LIndex ] ),
         LMessage, LIndex)) then
      begin
        LResult := False;
        AErrorMessages.Add ( 'ERROR:'+LMessage );
        AErrorColumns.Add ( IntToStr ( LIndex ));
        if ( LStopOnFirstError ) then
          Break;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateAllocDefRILabel (AErrorMessages : TStrings;
                                                        AErrorColumns  : TStringList ) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateAllocDefRILabel';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lIndex            : integer;
begin
  Result := FALSE;
  try
    AErrorColumns.Clear;
    lResult := True;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := 1 to FNrOfReliabilityClasses do
    begin
      lMessage := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('RILabel', FRecurrenceIntervalLabels[LIndex],
         LMessage, LIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex));
        if (lStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateFixedPosition (AErrorMessages : TStrings;
                                                      AErrorColumns  : TStringList) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateFixedPosition';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lIndex            : integer;
  lFixedPosition    : TFixedPosition;
  lOther            : TFixedPosition;
  lSubSystemID      : integer;
  lSubSystem        : TSubSystem;
  lLoop             : integer;
  lDuplicate        : Boolean;
begin
  Result := FALSE;
  try
    AErrorColumns.Clear;
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := 0 to FNrInFixedPosition - 1 do
    begin
      lMessage := '';
      lFixedPosition := CastFixedPositionByIndex(lIndex);
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('FixedPositionNr', IntToStr(lFixedPosition.FFixedPositionNr), lMessage)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex+1) + ',0');
        if (lStopOnFirstError) then
          Break;
      end
      else
      if (lFixedPosition.FFixedPositionNr > FNrOfSubSystems) then
      begin
        lResult  := FALSE;
        lMessage := FAppModules.Language.GetString ( 'ContextValidation.PositionNumberError' );
        AErrorMessages.Add( 'ERROR:'+Format ( lMessage, [ IntToStr(FNrOfSubSystems) ] ));
        AErrorColumns.Add(IntToStr(lIndex+1) + ',0');
        if (lStopOnFirstError) then
          Break;
      end
      else
      begin
        lDuplicate := FALSE;
        lLoop      := 0;
        while ((NOT lDuplicate) AND (lLoop < lIndex)) do
        begin
          lOther := CastFixedPositionByIndex(lLoop);
          if (lFixedPosition.FFixedPositionNr = lOther.FFixedPositionNr) then
            lDuplicate := TRUE
          else
            lLoop := lLoop + 1;
        end;
        if (lDuplicate) then
        begin
          lResult := FALSE;
          lMessage := 'Duplicate position numbers.';
          AErrorMessages.Add('ERROR:'+lMessage);
          AErrorColumns.Add(IntToStr(lIndex+1) + ',0');
        end;
      end;
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('FixedPosSubSystemID', IntToStr(lFixedPosition.FFixedPosSubSystemID), lMessage)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex+1) + ',1');
        if (lStopOnFirstError) then
          Break;
      end
      else
      begin
        lSubSystemID := lFixedPosition.FFixedPosSubSystemID;
        lSubSystem   := CastSubSystemByID(lSubSystemID);
        if (lSubSystem = nil) then
        begin
          lResult  := FALSE;
          if (lSubSystemID = 0) then
            lMessage := FAppModules.Language.GetString ( 'ContextValidation.UnspecifiedSubSystem' )
          else
            lMessage := FAppModules.Language.GetString ( 'ContextValidation.SubSystemNotExist');
          AErrorMessages.Add('ERROR:'+ Format ( lMessage, [ IntToStr( lSubSystemID ) ] ));
          AErrorColumns.Add (IntToStr(lIndex+1) + ',1');
          if (lStopOnFirstError) then
            Break;
        end
        else
        begin
          lDuplicate := FALSE;
          lLoop      := 0;
          while ((NOT lDuplicate) AND (lLoop < lIndex)) do
          begin
            lOther := CastFixedPositionByIndex(lLoop);
            if (lSubSystemID = lOther.FFixedPosSubSystemID) then
              lDuplicate := TRUE
            else
              lLoop := lLoop + 1;
          end;
          if (lDuplicate) then
          begin
            lResult := FALSE;
            lMessage := 'Duplicate sub-systems.';
            AErrorMessages.Add('ERROR:'+lMessage);
            AErrorColumns.Add(IntToStr(lIndex+1) + ',1');
          end;
        end;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateSpecificOrder (AErrorMessages : TStrings;
                                                      AErrorColumns  : TStringList) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateSpecificOrder';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lLocalResult      : Boolean;
  lIndex            : integer;
  lSpecificOrder    : TSpecificOrder;
  lSubSystem        : TSubSystem;
  lBeforeID         : integer;
  lAfterID          : integer;
  lOther            : TSpecificOrder;
  lDuplicate        : boolean;
  lCount            : integer;
begin
  Result := FALSE;
  try
    AErrorColumns.Clear;
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for lIndex := 0 to FNrInSpecificOrder - 1 do
    begin
      lLocalResult := TRUE;
      lMessage := '';
      lSpecificOrder := CastSpecificOrderByIndex(lIndex);
      lBeforeID      := lSpecificOrder.FBeforeSubSystemID;
      lAfterID       := lSpecificOrder.FAfterSubSystemID;
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('BeforeSubSystemID', IntToStr(lBeforeID), lMessage)) then
      begin
        lLocalResult := FALSE;
        lResult      := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex+1) + ',0');
        if (lStopOnFirstError) then
          Break;
      end
      else
      begin
        lSubSystem   := CastSubSystemByID(lBeforeID);
        if (lSubSystem = nil) then
        begin
          lLocalResult  := FALSE;
          lResult       := FALSE;
          if (lBeforeID = 0) then
            lMessage := FAppModules.Language.GetString('ContextValidation.UnspecifiedSubSystem')
          else
            lMessage := FAppModules.Language.GetString('ContextValidation.SubSystemNotExist');
          AErrorMessages.Add('ERROR:'+Format(lMessage, [IntToStr(lBeforeID)]));
          AErrorColumns.Add (IntToStr(lIndex+1) + ',0');
          if (lStopOnFirstError) then
            Break;
        end
      end;
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
         ('AfterSubSystemID', IntToStr(lAfterID), lMessage)) then
      begin
        lLocalResult := FALSE;
        lResult      := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex+1) + ',1');
        if (lStopOnFirstError) then
          Break;
      end
      else
      begin
        lSubSystem   := CastSubSystemByID(lAfterID);
        if (lSubSystem = nil) then
        begin
          lLocalResult := FALSE;
          lResult      := FALSE;
          if (lAfterID = 0) then
            lMessage :=  FAppModules.Language.GetString('ContextValidation.UnspecifiedSubSystem')
          else
            lMessage := FAppModules.Language.GetString('ContextValidation.SubSystemNotExist');
          AErrorMessages.Add('ERROR:'+Format(lMessage, [IntToStr(lAfterID)]));
          AErrorColumns.Add (IntToStr(lIndex+1) + ',1');
          if (lStopOnFirstError) then
            Break;
        end
      end;
      if (lLocalResult) AND (lBeforeID = lAfterID) then
      begin
        lLocalResult := FALSE;
        lResult      := FALSE;
        lMessage := 'Before and After Sub-systems may not be the same.';
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add(IntToStr(lIndex+1) + ',1');
        if (lStopOnFirstError) then
          Break;
      end;
      if (lLocalResult) then
      begin
        lDuplicate := FALSE;
        lCount     := lIndex - 1;
        while ((NOT lDuplicate) AND (lCount >= 0)) do
        begin
          lOther := CastSpecificOrderByIndex(lCount);
          if (lBeforeID = lOther.FBeforeSubSystemID) AND (lAfterID = lOther.FAfterSubSystemID) then
            lDuplicate := TRUE
          else
            lCount := lCount - 1;
        end;
        if (lDuplicate) then
        begin
          lResult      := FALSE;
          lMessage     := 'Duplicate specific order.';
          AErrorMessages.Add('ERROR:'+lMessage);
          AErrorColumns.Add(IntToStr(lIndex+1) + ',1');
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    if (lResult) then
      lResult := ValidateCyclicOrder(AErrorMessages, AErrorColumns);
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAllocationDefinition.ValidateCyclicOrder (AErrorMessages : TStrings;
                                                    AErrorIndex    : TStringList) : WordBool;
const OPNAME = 'TAllocationDefinition.ValidateCyclicOrder';
var
  lIndex         : integer;
  lCount         : integer;
  lFound         : boolean;
  lTree          : TTree;
  lParent        : TTree;
  lNode          : TTree;
  lCyclic        : boolean;
  lSpecificOrder : TSpecificOrder;
  lBeforeID      : string;
  lAfterID       : string;
  lMessage       : string;
  lFirstID       : string;
  lLastID        : string;
  lCommaPos      : integer;
  lErrorStr      : string;
begin
  Result := FALSE;
  try
    lCyclic := FALSE;
    lIndex  := 0;
    lTree   := TTree.Create;
    try
      while (NOT lCyclic) AND (lIndex < FNrInSpecificOrder) do
      begin
        AErrorIndex.Clear;
        lSpecificOrder := CastSpecificOrderByIndex(lIndex);
        lBeforeID      := IntToStr(lSpecificOrder.FBeforeSubSystemID);
        lAfterID       := IntToStr(lSpecificOrder.FAfterSubSystemID);

        lParent := lTree.FindNode(lBeforeID);
        if (lParent = nil) then
          lParent := lTree.AddNode(lParent, lBeforeID);
        lNode := lTree.AddNode(lParent, lAfterID);
        while (NOT lCyclic) AND (lParent <> nil) do
        begin
          AErrorIndex.Insert(0, lParent.FText + ',' + lNode.FText);
          if (lParent.FText = lAfterID) then
            lCyclic := TRUE
          else
          begin
            lNode   := lParent;
            lParent := lParent.FParent;
          end;  
        end;
        lIndex := lIndex + 1;
      end;
      if (lCyclic) then
      begin
        lMessage := 'Cyclic sequence.';
        AErrorMessages.Clear;
        for lIndex := 0 to AErrorIndex.Count - 1 do
        begin
          lErrorStr := AErrorIndex.Strings[lIndex];
          lCommaPos := Pos(',', lErrorStr);
          lFirstID  := Copy(lErrorStr, 1, lCommaPos - 1);
          lLastID   := Copy(lErrorStr, lCommaPos + 1, Length(lErrorStr) - lCommaPos);
          lCount    := 0;
          lFound    := FALSE;
          while (NOT lFound) AND (lCount < FNrInSpecificOrder) do
          begin
            lSpecificOrder := CastSpecificOrderByIndex(lCount);
            lBeforeID      := IntToStr(lSpecificOrder.FBeforeSubSystemID);
            lAfterID       := IntToStr(lSpecificOrder.FAfterSubSystemID);
            if (lBeforeID = lFirstID) AND (lAfterID = lLastID) then
            begin
              lFound := TRUE;
              lErrorStr := IntToStr(lCount + 1) + ',1';
              AErrorIndex.Strings[lIndex] := lErrorStr;
              AErrorMessages.Add('ERROR:'+lMessage);
            end
            else
              lCount := lCount + 1;
          end;
        end;

      end;
      Result := NOT lCyclic;
    finally
      FreeAndNil(lTree);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* TSubSystem                                                                 *}
{******************************************************************************}

procedure TSubSystem.CreateMemberObjects;
const OPNAME = 'TSubSystem.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReservoirNrs := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.SetCoefficientDimensions (ANrOfStartPercs : integer;
                                               ANrOfCurveSets  : integer;
                                               ANrOfLoadCases  : integer);
const OPNAME = 'TSubSystem.SetCoefficientDimensions';
begin
  try
    SetLength(FCoefficients, ANrOfStartPercs + 1,
                             ANrOfCurveSets + 1,
                             ANrOfLoadCases + 1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Initialise : boolean;
const OPNAME = 'TSubSystem.Initialise';
var
  lIndex : integer;
begin
  Result := False;
  try
    FName             := FAppModules.Language.GetString('PlanningGUI.NewSubSystem');
    FStartMonth       := 0;
    FStartYear        := 0;
    FEndMonth         := 0;
    FEndYear          := 0;
    FSubtractID       := 0;
    FSupportID        := 0;
    FSupportChannelNr := 0;
    FShortTermYield   := 0;
    FLongTermYield    := 0;
    FLowestStreamFlow := 0;
    FFirmYield        := FALSE;
    FSupportCalcType  := 0;
    FReservoirNrs.Clear;
    SetLength(FChannelNrs, 5 + 1);
    for lIndex := 1 to 5 do
      FChannelNrs[lIndex] := 0;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Validate (var AErrors    : WideString;
                              const AContext : WideString) : WordBool;
const OPNAME = 'TSubSystem.Validate';
var
  lErrorList : TStringList;
  lErrorCols : TStringList;
  lColFlag   : boolean;
begin
  Result := False;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      lColFlag := FALSE;
      if (AContext = 'SubSystemReservoirNrs') then
        Result := ValidateSubSystemReservoirNrs(lErrorList)
      else
      if (AContext = 'RoutingChannelNr') then
      begin
        Result := ValidateRoutingChannelNrs(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end
      else
      if (AContext = 'SubSystemName') then
        Result := ValidateSubSystemName(lErrorList)
      else
      if (AContext = 'SubSystemStartMonth') then
        Result := ValidateSubSystemStartMonth(lErrorList)
      else
      if (AContext = 'SubSystemStartYear') then
        Result := ValidateSubSystemStartYear(lErrorList)
      else
      if (AContext = 'SubSystemEndMonth') then
        Result := ValidateSubSystemEndMonth(lErrorList)
      else
      if (AContext = 'SubSystemEndYear') then
        Result := ValidateSubSystemEndYear(lErrorList)
      else
      if (AContext = 'SubtractedSubSystemID') then
        Result := ValidateSubtractedSubSystemID(lErrorList)
      else
      if (AContext = 'SupportingSubSystemID') then
        Result := ValidateSupportingSubSystemID(lErrorList)
      else
      if (AContext = 'ShortTermYield') then
        Result := ValidateShortTermYield(lErrorList)
      else
      if (AContext = 'LongTermYield') then
        Result := ValidateLongTermYield(lErrorList)
      else
      if (AContext = 'LowestStreamFlow') then
        Result := ValidateLowestStreamFlow(lErrorList)
      else
      if (AContext = 'FirmYield') then
        Result := ValidateFirmYield(lErrorList)
      else
      if (AContext = 'SupportCalcType') then
        Result := ValidateSupportCalcType(lErrorList);
      if (lColFlag AND (NOT Result)) then
        AddErrors(AErrors, lErrorList, lErrorCols);
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubSystem.ValidateSubSystemReservoirNrs ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubSystemReservoirNrs';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'SubSystemReservoirNrs', FReservoirNrs.CommaText, LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FReservoirNrs.CommaText + ':' + LMessage )
    else
    begin
      if (FReservoirNrs.Count > 20) then
      begin
        Result := FALSE;
        AErrorMessages.Add('ERROR:'+'Sub-system may contain maximum of 20 reservoirs');
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateRoutingChannelNrs (AErrorMessages : TStrings;
                                               AErrorColumns  : TStringList) : WordBool;
const OPNAME = 'TSubSystem.ValidateRoutingChannelNrs';
var
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LIndex            : integer;
  lDuplicate        : Boolean;
  lLoop             : integer;
begin
  Result := False;
  try
    AErrorColumns.Clear;
    LResult := True;
    LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 1 to 5 do
    begin
      if (FChannelNrs[lIndex] <> 0) then
      begin
        lMessage := '';
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty
           ('RoutingChannelNr', IntToStr(FChannelNrs[LIndex]), LMessage, LIndex)) then
        begin
          LResult := False;
          AErrorMessages.Add ('ERROR:'+ LMessage );
          AErrorColumns.Add ( IntToStr ( LIndex ));
          if ( LStopOnFirstError) then
            Break;
        end
        else
        begin
          lDuplicate := FALSE;
          lLoop := 1;
          while ((NOT lDuplicate) AND (lLoop < LIndex)) do
          begin
            if (FChannelNrs[lIndex] = FChannelNrs[lLoop]) then
              lDuplicate := TRUE
            else
              lLoop := lLoop + 1;
          end;
          if (lDuplicate) then
          begin
            lResult := FALSE;
            lMessage := FAppModules.Language.GetString ('ContextValidation.DuplicateChannel');
            AErrorMessages.Add('ERROR:'+Format(lMessage, [IntToStr(FChannelNrs[lIndex])]));
            AErrorColumns.Add(IntToStr(lIndex));
          end;
        end;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSubSystemName ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubSystemName';
var
  LMessage : string;
begin
  Result := True;
  try
    LMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty( 'SubSystemName', FName, LMessage )) then
      AErrorMessages.Add ('WARNING:'+ FName + ':' + LMessage )

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSubSystemStartYear (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubSystemStartYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SubSystemStartYear', IntToStr(FStartYear), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FStartYear) + ':' + LMessage)
    else
    if (FStartYear = 0) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSubSystemStartMonth (AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubSystemStartMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SubSystemStartMonth', IntToStr(FStartMonth), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FStartMonth) + ':' + LMessage)
    else
    if (FStartMonth = 0) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSubSystemEndYear (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubSystemEndYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SubSystemEndYear', IntToStr(FEndYear), LMessage);
    if (NOT Result )then
      AErrorMessages.Add('ERROR:'+IntToStr(FEndYear) + ':' + LMessage)
    else
    if (FEndYear = 0) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end
    else
    if (FEndYear < FStartYear) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidStartEndDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSubSystemEndMonth (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubSystemEndMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SubSystemEndMonth', IntToStr(FEndMonth), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FEndMonth) + ':' + LMessage)
    else
    if (FEndMonth = 0) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end
    else
    if (FEndYear = FStartYear) AND (FEndMonth < FStartMonth) then
    begin
      lMessage := FAppModules.Language.GetString('ContextValidation.InvalidStartEndDate');
      AErrorMessages.Add('ERROR:'+lMessage);
      Result := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSubtractedSubSystemID ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateSubtractedSubSystemID';
var
  LMessage : string;
  lOtherSystem : ISubSystem;
  lAllocDef    : IAllocationDefinition;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'SubtractedSubSystemID', IntToStr ( FSubtractID ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ( 'ERROR:'+IntToStr ( FSubtractID ) + ':' + LMessage )
    else
    if (FSubtractID <> 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
      lOtherSystem := lAllocDef.SubSystemByID[FSubtractID];
      if (lOtherSystem = nil) then
      begin
        Result := False;
        LMessage := FAppModules.Language.GetString ( 'ContextValidation.InvalidSubtractID' );
        AErrorMessages.Add ( 'ERROR:'+LMessage );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSupportingSubSystemID ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateSupportingSubSystemID';
var
  LMessage : string;
  lOtherSystem : ISubSystem;
  lAllocDef    : IAllocationDefinition;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'SupportingSubSystemID', IntToStr ( FSupportID ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ( 'ERROR:'+IntToStr ( FSupportID ) + ':' + LMessage )
    else
    if (FSupportID <> 0) then
    begin
      lAllocDef := ( FAppModules.Model.ModelData as IPlanningModelData ).
                     AllocationDefinitionsList.AllocationDefinitionByID [ FAllocDefID ];
      lOtherSystem := lAllocDef.SubSystemByID [ FSupportID ];
      if ( lOtherSystem = nil) then
      begin
        Result := False;
        LMessage := FAppModules.Language.GetString ( 'ContextValidation.InvalidSupportingID' );
        AErrorMessages.Add ('ERROR:'+ LMessage );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateShortTermYield ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateShortTermYield';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'ShortTermYield', FloatToStr ( FShortTermYield ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FShortTermYield ) + ':' + LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateLongTermYield ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateLongTermYield';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'LongTermYield', FloatToStr ( FLongTermYield ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FLongTermYield ) + ':' + LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateLowestStreamFlow ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateLowestStreamFlow';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'LowestStreamFlow', FloatToStr ( FLowestStreamFlow ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FLowestStreamFlow ) + ':' + LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateFirmYield ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateFirmYield';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if FFirmYield then
      Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'FirmYield', 'Y', LMessage )
    else
      Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'FirmYield', 'N', LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.ValidateSupportCalcType ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TSubSystem.ValidateSupportCalcType';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'SupportCalcType', FloatToStr ( FSupportCalcType ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FSupportCalcType ) + ':' + LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.NewCoefficient ( APercNr  : integer;
                                     ACurveNr : integer;
                                     ACaseNr  : integer ) : TCoefficient;
const OPNAME = 'TSubSystem.NewCoefficient';
var
  LCoefficient : TCoefficient;
  lLoadAgent : TAllocationDefLoadAgent;
begin
  Result := nil;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertCoefficient ( FAllocDefID ,FSubSystemID, APercNr,
                                         ACurveNr, ACaseNr )) then
      begin
        LCoefficient := CreateCoefficient( APercNr, ACurveNr, ACaseNr );
        LCoefficient.Populate(FAllocDefID, FSubSystemID, APercNr, ACurveNr, ACaseNr,
                              0, 0, 0, 0, 0, 0);
        Result := LCoefficient;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.CreateCoefficient (APercNr  : integer;
                                       ACurveNr : integer;
                                       ACaseNr  : integer) : TCoefficient;
const OPNAME = 'TSubSystem.CreateCoefficient';
begin
  Result := nil;
  try
    FCoefficients[APercNr, ACurveNr, ACaseNr] := TCoefficient.Create(FAppModules);
    Result := FCoefficients[APercNr, ACurveNr, ACaseNr];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.RemoveCoefficient (APercNr  : integer;
                                       ACurveNr : integer;
                                       ACaseNr  : integer) : WordBool;
const OPNAME = 'TSubSystem.RemoveCoefficient';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LCoefficient : TCoefficient;
begin
  Result := False;
  try
    LCoefficient := CastCoefficientByPercCurveCase(APercNr, ACurveNr, ACaseNr);
    if (LCoefficient <> nil) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (LLoadAgent.DeleteCoeficient(FAllocDefID,FSubSystemID,APercNr,ACurveNr,ACaseNr)) then
        begin
          DeleteCoefficient(APercNr,ACurveNr,ACaseNr);
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubSystem.DeleteCoefficient (APercNr  : integer;
                                       ACurveNr : integer;
                                       ACaseNr  : integer) : WordBool;
const OPNAME = 'TSubSystem.DeleteCoefficient';
var
  LCoeficient : TCoefficient;
begin
  Result := False;
  try
    LCoeficient := CastCoefficientByPercCurveCase(APercNr, ACurveNr, ACaseNr);
    if (LCoeficient <> nil) then
    begin
      FCoefficients[APercNr, ACurveNr, ACaseNr] := nil;
      FreeAndNil(LCoeficient);
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Populate (AAllocDefID       : integer;
                              ASubSystemID      : integer;
                              AName             : string;
                              AOrder            : integer;
                              AStartYear        : integer;
                              AStartMonth       : integer;
                              AEndYear          : integer;
                              AEndMonth         : integer;
                              ASubtractID       : integer;
                              ASupportID        : integer;
                              ASupportChannelNr : integer;
                              AShortTermYield   : double;
                              ALongTermYield    : double;
                              ALowestStreamFlow : double;
                              AFirmYield        : Boolean;
                              AReservoirNrs     : string;
                              AChannelNrs       : array of integer;
                              ASupportCalcType  : integer) : Boolean;
const OPNAME = 'TSubSystem.Populate';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FAllocDefID       := AAllocDefID;
    FSubSystemID      := ASubSystemID;
    FName             := AName;
    FOrder            := AOrder;
    FStartYear        := AStartYear;
    FStartMonth       := AStartMonth;
    FEndYear          := AEndYear;
    FEndMonth         := AEndMonth;
    FSubtractID       := ASubtractID;
    FSupportID        := ASupportID;
    FSupportChannelNr := ASupportChannelNr;
    FShortTermYield   := AShortTermYield;
    FLongTermYield    := ALongTermYield;
    FLowestStreamFlow := ALowestStreamFlow;
    FFirmYield        := AFirmYield;
    FSupportCalcType  := ASupportCalcType;
    FReservoirNrs.CommaText := AReservoirNrs;
    for lIndex := 1 to 5 do
      FChannelNrs[lIndex] := AChannelNrs[lIndex];
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.DestroyMemberObjects;
const OPNAME = 'TSubSystem.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReservoirNrs);
    Finalize(FCoefficients);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_RoutingChannelNrByIndex (AIndex : integer): integer;
const OPNAME = 'TSubSystem.Get_RoutingChannelNrByIndex';
begin
  try
    if (AIndex >= 1) AND (AIndex <= 5) then
      Result := FChannelNrs[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_RoutingChannelNrByIndex (AIndex : integer;
                                                  AValue : integer);
const OPNAME = 'TSubSystem.Set_RoutingChannelNrByIndex';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if (AIndex >= 1) AND (AIndex <= 5) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SubSystemFieldNameID(LContextData, IntToStr(FAllocDefID),
                                                          IntToStr(FSubSystemID), IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
             'RoutingChannelNr', IntToStr(AValue), IntToStr(FChannelNrs[AIndex]), LContextData) then
          begin
            LOldValue := IntToStr(FChannelNrs[AIndex]);
            FChannelNrs[AIndex] := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RoutingChannelNr',LOldValue,IntToStr(AValue));
          end;
        finally
          FreeAndNil ( LContextData );
        end;
      finally
        FreeAndNil ( LLoadAgent );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_CoefficientByPercCurveCase ( APercIndex  : integer;
                                                      ACurveIndex : integer;
                                                      ACaseIndex  : integer; const ACoefficient : ICoefficient);
const OPNAME = 'TSubSystem.Set_CoefficientByPercCurveCase';
begin
  try
  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.CastCoefficientByPercCurveCase (APercIndex  : integer;
                                                    ACurveIndex : integer;
                                                    ACaseIndex  : integer): TCoefficient;
const OPNAME = 'TSubSystem.CastCoefficientByPercCurveCase';
begin
  Result := nil;
  try
    Result := FCoefficients[APercIndex, ACurveIndex, ACaseIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_CoefficientByPercCurveCase(APercIndex, ACurveIndex, ACaseIndex: integer): ICoefficient;
const OPNAME = 'TSubSystem.Get_CoefficientByPercCurveCase';
begin
  Result := nil;
  try
    Result := CastCoefficientByPercCurveCase(APercIndex, ACurveIndex, ACaseIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_ReservoirNrs: WideString;
const OPNAME = 'TSubSystem.Get_ReservoirNrs';
begin
  try
    Result := FReservoirNrs.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_EndMonth: integer;
const OPNAME = 'TSubSystem.Get_EndMonth';
begin
  Result := 0;
  try
    Result := FEndMonth
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_EndYear: integer;
const OPNAME = 'TSubSystem.Get_EndYear';
begin
  Result := 0;
  try
    Result := FEndYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_Name: WideString;
const OPNAME = 'TSubSystem.Get_Name';
begin
  Result := '';
  try
    Result := FName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_Order: integer;
const OPNAME = 'TSubSystem.Get_Order';
begin
  Result := 0;
  try
    Result := FOrder;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubSystem.Get_StartMonth: integer;
const OPNAME = 'TSubSystem.Get_StartMonth';
begin
  Result := 0;
  try
    Result := FStartMonth;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_StartYear: integer;
const OPNAME = 'TSubSystem.Get_StartYear';
begin
  Result := 0;
  try
    Result := FStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_SubSystemID: integer;
const OPNAME = 'TSubSystem.Get_SubSystemID';
begin
  Result := 0;
  try
    Result := FSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_SubtractID: integer;
const OPNAME = 'TSubSystem.Get_SubtractID';
begin
  Result := 0;
  try
    Result := FSubtractID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_SubtractID(AValue: integer);
const OPNAME = 'TSubSystem.Set_SubtractID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubtractedSubSystemID', IntToStr ( AValue ), IntToStr ( FSubtractID ), LContextData) then
        begin
          LOldValue := IntToStr ( FSubtractID );
          FSubtractID := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubtractedSubSystemID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_SupportID: integer;
const OPNAME = 'TSubSystem.Get_SupportID';
begin
  Result := 0;
  try
    Result := FSupportID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_SupportID(AValue: integer);
const OPNAME = 'TSubSystem.Set_SupportID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportingSubSystemID', IntToStr ( AValue ), IntToStr ( FSupportID ), LContextData) then
        begin
          LOldValue := IntToStr ( FSupportID );
          FSupportID := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupportingSubSystemID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_SupportChannelNr: integer;
const OPNAME = 'TSubSystem.Get_SupportChannelNr';
begin
  Result := 0;
  try
    Result := FSupportChannelNr;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_SupportChannelNr(AValue: integer);
const OPNAME = 'TSubSystem.Set_SupportChannelNr';
var
  lLoadAgent   : TAllocationDefLoadAgent;
  lContextData : TStringList;
  lOldValue    : string;
begin
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      lContextData := TStringList.Create;
      try
        lLoadAgent.LoadContextData_SubSystem(lContextData, IntToStr(FAllocDefID), IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportingChannelNr', IntToStr(AValue), IntToStr(FSupportChannelNr), LContextData) then
        begin
          lOldValue  := IntToStr(FSupportChannelNr);
          FSupportChannelNr := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'SupportingChannelNr', lOldValue, IntToStr(AValue));
        end;
      finally
        FreeAndNil(lContextData);
      end;
    finally
      FreeAndNil(lLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_SupportCalcType: integer;
const OPNAME = 'TSubSystem.Get_SupportCalcType';
begin
  Result := 0;
  try
    Result := FSupportCalcType;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_SupportCalcType(AValue: integer);
const OPNAME = 'TSubSystem.Set_SupportCalcType';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportCalcType', IntToStr ( AValue ), IntToStr ( FSupportCalcType ), LContextData) then
        begin
          LOldValue := IntToStr ( FSupportCalcType );
          FSupportCalcType := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupportCalcType',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_ShortTermYield: double;
const OPNAME = 'TSubSystem.Get_ShortTermYield';
begin
  Result := 0;
  try
    Result := FShortTermYield;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_ShortTermYield(AValue: double);
const OPNAME = 'TSubSystem.Set_ShortTermYield';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ShortTermYield', FloatToStr ( AValue ), FloatToStr ( FShortTermYield ), LContextData) then
        begin
          LOldValue := FloatToStr ( FShortTermYield );
          FShortTermYield := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ShortTermYield',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TSubSystem.Get_LongTermYield: double;
const OPNAME = 'TSubSystem.Get_LongTermYield';
begin
  Result := 0;
  try
    Result := FLongTermYield;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_LongTermYield(AValue: double);
const OPNAME = 'TSubSystem.Set_LongTermYield';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'LongTermYield', FloatToStr ( AValue ), FloatToStr ( FLongTermYield ), LContextData) then
        begin
          LOldValue := FloatToStr ( FLongTermYield );
          FLongTermYield := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LongTermYield',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_LowestStreamFlow: double;
const OPNAME = 'TSubSystem.Get_LowestStreamFlow';
begin
  Result := 0;
  try
    Result := FLowestStreamFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_LowestStreamFlow(AValue: double);
const OPNAME = 'TSubSystem.Set_LowestStreamFlow';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'LowestStreamFlow', FloatToStr ( AValue ), FloatToStr ( FLowestStreamFlow ), LContextData) then
        begin
          LOldValue := FloatToStr ( FLowestStreamFlow );
          FLowestStreamFlow := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LowestStreamFlow',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem.Get_FirmYield: Wordbool;
const OPNAME = 'TSubSystem.Get_FirmYield';
begin
  Result := False;
  try
    Result := FFirmYield;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_FirmYield(AValue: Wordbool);
const OPNAME = 'TSubSystem.Set_FirmYield';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LNewFirmYield   : string;
  LOldFirmYield   : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if AValue then
          LNewFirmYield := 'Y'
        else
          LNewFirmYield := 'N';

        if not FFirmYield then
          LOldFirmYield := 'N'
        else
          LOldFirmYield := 'Y';
        if FAppModules.FieldProperties.UpdateFieldValue(
           'FirmYield', LNewFirmYield, LOldFirmYield, LContextData) then
        begin
          FFirmYield := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FirmYield',LOldFirmYield,LNewFirmYield );
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_ReservoirNrs(const AValue: WideString);
const OPNAME = 'TSubSystem.Set_ReservoirNrs';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemReservoirNrs', AValue, FReservoirNrs.CommaText, LContextData) then
        begin
          LOldValue := FReservoirNrs.CommaText;
          FReservoirNrs.Clear;
          if (Trim(AValue) <> '') then
          FReservoirNrs.CommaText := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubSystemReservoirNrs',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_EndMonth(AValue : integer);
const OPNAME = 'TSubSystem.Set_EndMonth';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemEndMonth', IntToStr ( AValue ), IntToStr ( FEndMonth ), LContextData) then
        begin
          LOldValue := IntToStr ( FEndMonth );
          FEndMonth := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubSystemEndMonth',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_EndYear(AValue: integer);
const OPNAME = 'TSubSystem.Set_EndYear';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemEndYear', IntToStr ( AValue ), IntToStr ( FEndYear ), LContextData) then
        begin
          LOldValue := IntToStr ( FEndYear );
          FEndYear := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubSystemEndYear',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_Name(const AValue: WideString);
const OPNAME = 'TSubSystem.Set_Name';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemName', AValue, FName, LContextData) then
        begin
          LOldValue := FName;
          FName := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubSystemName',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_Order (AValue : integer);
const OPNAME = 'TSubSystem.Set_Order';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemOrder', IntToStr(AValue), IntToStr(FOrder), LContextData) then
        begin
          LOldValue := IntToStr(FOrder);
          FOrder := AValue;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit, 'SubSystemOrder', LOldValue, IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubSystem.Set_StartMonth(AValue: integer);
const OPNAME = 'TSubSystem.Set_StartMonth';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemStartMonth', IntToStr ( AValue ), IntToStr ( FStartMonth ), LContextData) then
        begin
          LOldValue := IntToStr ( FStartMonth );
          FStartMonth := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubSystemStartMonth',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSubSystem.Set_StartYear(AValue: integer);
const OPNAME = 'TSubSystem.Set_StartYear';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SubSystem(LContextData, IntToStr(FAllocDefID),
                                             IntToStr(FSubSystemID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SubSystemStartYear', IntToStr ( AValue ), IntToStr ( FStartYear ), LContextData) then
        begin
          LOldValue := IntToStr ( FStartYear );
          FStartYear := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SubSystemStartYear',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSubSystem._AddRef: Integer;
const OPNAME = 'TSubSystem._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSubSystem._Release: Integer;
const OPNAME = 'TSubSystem._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TCoefficient                                                               *}
{******************************************************************************}

function TCoefficient._AddRef: Integer;
const OPNAME = 'TCoefficient._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCoefficient._Release: Integer;
const OPNAME = 'TCoefficient._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCoefficient.Get_CoefficientA: double;
const OPNAME = 'TCoefficient.Get_CoefficientA';
begin
  Result := 0.0; 
  try
    Result := FCoefficientA
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Get_CoefficientB: double;
const OPNAME = 'TCoefficient.Get_CoefficientB';
begin
  Result := 0.0;
  try
    Result := FCoefficientB;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Get_CoefficientC: double;
const OPNAME = 'TCoefficient.Get_CoefficientC';
begin
  Result := 0.0;
  try
    Result := FCoefficientC;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Get_CoefficientD: double;
const OPNAME = 'TCoefficient.Get_CoefficientD';
begin
  Result := 0.0;
  try
    Result := FCoefficientD;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Get_Risk: double;
const OPNAME = 'TCoefficient.Get_Risk';
begin
  Result := 0.0;
  try
    Result := FRisk
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Get_TargetDraft : double;
const OPNAME = 'TCoefficient.Get_TargetDraft';
begin
  Result := 0.0;
  try
    Result := FTargetDraft;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCoefficient.Set_TargetDraft(AValue: double);
const OPNAME = 'TCoefficient.Set_TargetDraft';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Coefficient ( LContextData, IntToStr(FAllocDefID),
                                                 IntToStr(FSubSystemID), IntToStr(FStartPercNr),
                                                 IntToStr(FCurveSetNr), IntToStr(FLoadCaseNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'LoadCase', FloatToStr ( AValue ), FloatToStr ( FTargetDraft ), LContextData) then
        begin
          LOldValue := FloatToStr ( FTargetDraft );
          FTargetDraft := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'LoadCase',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TCoefficient.Set_CoefficientA(AValue: double);
const OPNAME = 'TCoefficient.Set_CoefficientA';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Coefficient ( LContextData, IntToStr(FAllocDefID),
                                                 IntToStr(FSubSystemID), IntToStr(FStartPercNr),
                                                 IntToStr(FCurveSetNr), IntToStr(FLoadCaseNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'CoefficientA', FloatToStr ( AValue ), FloatToStr ( FCoefficientA ), LContextData) then
        begin
          LOldValue := FloatToStr ( FCoefficientA );
          FCoefficientA := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CoefficientA',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCoefficient.Set_CoefficientB(AValue: double);
const OPNAME = 'TCoefficient.Set_CoefficientB';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Coefficient ( LContextData, IntToStr(FAllocDefID),
                                                 IntToStr(FSubSystemID), IntToStr(FStartPercNr),
                                                 IntToStr(FCurveSetNr), IntToStr(FLoadCaseNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'CoefficientB', FloatToStr ( AValue ), FloatToStr ( FCoefficientB ), LContextData) then
        begin
          LOldValue := FloatToStr ( FCoefficientB );
          FCoefficientB := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CoefficientB',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCoefficient.Set_CoefficientC(AValue: double);
const OPNAME = 'TCoefficient.Set_CoefficientC';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Coefficient ( LContextData, IntToStr(FAllocDefID),
                                                 IntToStr(FSubSystemID), IntToStr(FStartPercNr),
                                                 IntToStr(FCurveSetNr), IntToStr(FLoadCaseNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'CoefficientC', FloatToStr ( AValue ), FloatToStr ( FCoefficientC ), LContextData) then
        begin
          LOldValue := FloatToStr ( FCoefficientC );
          FCoefficientC := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CoefficientC',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCoefficient.Set_CoefficientD(AValue: double);
const OPNAME = 'TCoefficient.Set_CoefficientD';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Coefficient ( LContextData, IntToStr(FAllocDefID),
                                                 IntToStr(FSubSystemID), IntToStr(FStartPercNr),
                                                 IntToStr(FCurveSetNr), IntToStr(FLoadCaseNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'CoefficientD', FloatToStr ( AValue ), FloatToStr ( FCoefficientD ), LContextData) then
        begin
          LOldValue := FloatToStr ( FCoefficientD );
          FCoefficientD := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CoefficientD',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCoefficient.Set_Risk(AValue: double);
const OPNAME = 'TCoefficient.Set_Risk';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_Coefficient ( LContextData, IntToStr(FAllocDefID),
                                                 IntToStr(FSubSystemID), IntToStr(FStartPercNr),
                                                 IntToStr(FCurveSetNr), IntToStr(FLoadCaseNr));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'RiskProportion', FloatToStr ( AValue ), FloatToStr ( FRisk ), LContextData) then
        begin
          LOldValue := FloatToStr ( FRisk );
          FRisk := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RiskProportion',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Validate (var AErrors    : WideString;
                                        const AContext : WideString) : WordBool;
const OPNAME = 'TCoefficient.Validate';
var
  LErrorList : TStringList;
begin
  Result := False;
  try
    LErrorList := TStringList.Create;
    try
      Result := TRUE;
      if (AContext = 'LoadCase') then
        Result := ValidateLoadCase(lErrorList)
      else
      if (AContext = 'CoefficientA') then
        Result := ValidateCoefficientA(lErrorList)
      else
      if (AContext = 'CoefficientB') then
        Result := ValidateCoefficientB(lErrorList)
      else
      if (AContext = 'CoefficientC') then
        Result := ValidateCoefficientC(lErrorList)
      else
      if (AContext = 'CoefficientD') then
        Result := ValidateCoefficientD(lErrorList)
      else
      if (AContext = 'RiskProportion') then
        Result := ValidateRiskProportion(lErrorList);
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficient.ValidateLoadCase ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TCoefficient.ValidateLoadCase';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'LoadCase', FloatToStr ( FTargetDraft ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FTargetDraft ) + ':' + LMessage )
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.ValidateCoefficientA ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TCoefficient.ValidateCoefficientA';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'CoefficientA', FloatToStr ( FCoefficientA ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FCoefficientA ) + ':' + LMessage )
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.ValidateCoefficientB ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TCoefficient.ValidateCoefficientB';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'CoefficientB', FloatToStr ( FCoefficientB ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FCoefficientB ) + ':' + LMessage )
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.ValidateCoefficientC ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TCoefficient.ValidateCoefficientC';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'CoefficientC', FloatToStr ( FCoefficientC ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FCoefficientC ) + ':' + LMessage )
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.ValidateCoefficientD ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TCoefficient.ValidateCoefficientD';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'CoefficientD', FloatToStr ( FCoefficientD ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FCoefficientD ) + ':' + LMessage )
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.ValidateRiskProportion ( AErrorMessages : TStrings ) : WordBool;
const OPNAME = 'TCoefficient.ValidateRiskProportion';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ( 'RiskProportion', FloatToStr ( FRisk ), LMessage );
    if (NOT Result) then
      AErrorMessages.Add ('ERROR:'+ FloatToStr ( FRisk ) + ':' + LMessage )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCoefficient.Populate (AAllocDefID     : integer;
                                ASubSystemID    : integer;
                                AStartPercNr    : integer;
                                ACurveSetNr     : integer;
                                ALoadCaseNr     : integer;
                                ATargetDraft    : double;
                                ACoeffA         : double;
                                ACoeffB         : double;
                                ACoeffC         : double;
                                ACoeffD         : double;
                                ARisk           : double) : Boolean;
const OPNAME = 'TCoefficient.Populate';
begin
  Result := False;
  try
    FAllocDefID   := AAllocDefID;
    FSubSystemID  := ASubSystemID;
    FStartPercNr  := AStartPercNr;
    FCurveSetNr   := ACurveSetNr;
    FLoadCaseNr   := ALoadCaseNr;
    FTargetDraft  := ATargetDraft;
    FCoefficientA := ACoeffA;
    FCoefficientB := ACoeffB;
    FCoefficientC := ACoeffC;
    FCoefficientD := ACoeffD;
    FRisk         := ARisk;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCoefficient.CreateMemberObjects;
const OPNAME = 'TCoefficient.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCoefficient.DestroyMemberObjects;
const OPNAME = 'TCoefficient.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TSupportChannel                                                            *}
{******************************************************************************}

procedure TSupportChannel.CreateMemberObjects;
const OPNAME = 'TSupportChannel.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportChannel.DestroyMemberObjects;
const OPNAME = 'TSupportChannel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TSupportChannel._AddRef: Integer;
const OPNAME = 'TSupportChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSupportChannel._Release: Integer;
const OPNAME = 'TSupportChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSupportChannel.Initialise : boolean;
const OPNAME = 'TSupportChannel.Initialise';
begin
  Result := False;
  try
    FChannelNumber := 0;
    FNrOfCntrlSubSystems := 0;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.Validate (var AErrors    : WideString;
                                   const AContext : WideString) : WordBool;
const OPNAME = 'TSupportChannel.Validate';
var
  lErrorList  : TStringList;
  lErrorCols  : TStringList;
  lColFlag    : Boolean;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      lColFlag := FALSE;
      if (AContext = 'SupportChannelNr') then
        Result := ValidateSupportChannelNr(lErrorList)
      else
      if (AContext = 'CntrlSubSystems') then
      begin
        Result := ValidateCntrlSubSystems(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end;
      if (lColFlag AND (NOT Result)) then
        AddErrors(AErrors, lErrorList, lErrorCols);
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupportChannel.ValidateSupportChannelNr (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TSupportChannel.ValidateSupportChannelNr';
var
  lMessage   : string;
  lChannel   : IGeneralFlowChannel;
  lAllocDef  : IAllocationDefinition;
  lOther     : ISupportChannel;
  lIndex     : integer;
  lDuplicate : boolean;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('SupportChannelNr', IntToStr(FChannelNumber), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+lMessage)
    else
    begin
      lChannel := (FAppModules.Model.ModelData as IYieldModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumber];
      if (lChannel = nil) then
      begin
        Result   := FALSE;
        if (FChannelNumber = 0) then
          lMessage := FAppModules.Language.GetString('ContextValidation.UnspecifiedChannel')
        else
          lMessage := FAppModules.Language.GetString('ContextValidation.ChannelNotExist');
        AErrorMessages.Add('ERROR:'+Format ( lMessage, [ IntToStr(FChannelNumber) ] ));
      end
      else
      begin
        lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                        AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
        if (lAllocDef <> nil) then
        begin
          lDuplicate := FALSE;
          lIndex     := 0;
          while ((NOT lDuplicate) AND (lIndex < lAllocDef.NrOfSupportChannels)) do
          begin
            lOther := lAllocDef.SupportChannelByIndex[lIndex];
            if (Self.SupportChannelID <> lOther.SupportChannelID) AND
               (Self.ChannelNumber = lOther.ChannelNumber) then
              lDuplicate := TRUE
            else
              lIndex := lIndex + 1;
          end;
          if (lDuplicate) then
          begin
            Result := FALSE;
            lMessage := 'Duplicate support channel.';
            AErrorMessages.Add('ERROR:'+lMessage);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.ValidateCntrlSubSystems (AErrorMessages : TStrings;
                                                  AErrorColumns  : TStringList ) : WordBool;
const OPNAME = 'TSupportChannel.ValidateCntrlSubSystems';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lLocalResult      : Boolean;
  lIndex            : integer;
  lSubSysID         : integer;
  lSubSystem        : ISubSystem;
  lAllocDef         : IAllocationDefinition;
  lCount            : integer;
  lDuplicate        : Boolean;
begin
  Result := FALSE;
  try
    AErrorColumns.Clear;
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lAllocDef         := (FAppModules.Model.ModelData as IPlanningModelData).
                           AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
    for lIndex := 1 to NrOfCntrlSubSystems do
    begin
      lLocalResult := TRUE;
      lMessage  := '';
      lSubSysID := FSubSystemIDs[lIndex];
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
               ('CntrlSubSystemID', IntToStr(lSubSysID), lMessage, lIndex)) then
      begin
        lResult      := FALSE;
        lLocalResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add (IntToStr(lIndex) + ',1');
        if (lStopOnFirstError) then
          Break;
      end
      else
      begin
        lSubSystem := lAllocDef.SubSystemByID[lSubSysID];
        if (lSubSystem = nil) then
        begin
          lLocalResult := FALSE;
          lResult      := FALSE;
          if (lSubSysID = 0) then
            lMessage := FAppModules.Language.GetString('ContextValidation.UnspecifiedSubSystem')
          else
            lMessage := FAppModules.Language.GetString('ContextValidation.SubSystemNotExist');
          AErrorMessages.Add('ERROR:'+ Format ( lMessage, [ IntToStr ( lSubSysID ) ] )  );
          AErrorColumns.Add (IntToStr(lIndex) + ',1');
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
               ('CntrlFactor', FloatToStr(FSubSystemFactors[lIndex]), lMessage, lIndex)) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add (IntToStr(lIndex) + ',2');
        if (lStopOnFirstError) then
          Break;
      end;
      if (lLocalResult) then
      begin
        lDuplicate := FALSE;
        lCount     := lIndex - 1;
        while (NOT lDuplicate) AND (lCount >= 0) do
        begin
          if (FSubSystemIDs[lCount] = lSubSysID) then
            lDuplicate := TRUE
          else
            lCount := lCount - 1;
        end;
        if (lDuplicate) then
        begin
          lResult      := FALSE;
          AErrorMessages.Add('ERROR:'+'Duplicate controlling sub-system.');
          AErrorColumns.Add (IntToStr(lIndex) + ',1');
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupportChannel.Populate (AAllocDefID       : integer;
                                   ASupportChannelID : integer;
                                   AChannelNumber    : integer;
                                   ANrOfSubSystems   : integer;
                                   ASubSystemIDs     : array of integer;
                                   AFactors          : array of double) : Boolean;
const OPNAME = 'TSupportChannel.Populate';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FAllocDefID          := AAllocDefID;
    FSupportChannelID    := ASupportChannelID;
    FChannelNumber       := AChannelNumber;
    FNrOfCntrlSubSystems := ANrOfSubSystems;
    SetLength(FSubSystemIDs, ANrOfSubSystems + 1);
    SetLength(FSubSystemFactors, ANrOfSubSystems + 1);
    for lIndex := 1 to FNrOfCntrlSubSystems do
    begin
      FSubSystemIDs[lIndex] := ASubSystemIDs[lIndex];
      FSubSystemFactors[lIndex] := AFactors[lIndex];
    end;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.NewControllingSubSystem : integer;
const OPNAME = 'TSupportChannel.NewControllingSubSystem';
begin
  Result := 0;
  try
    NrOfCntrlSubSystems := FNrOfCntrlSubSystems + 1;
    SetLength(FSubSystemIDs, FNrOfCntrlSubSystems + 1);
    SetLength(FSubSystemFactors, FNrOfCntrlSubSystems + 1);
    SubSystemIDByIndex[FNrOfCntrlSubSystems] := 0;
    SubSystemFactorByIndex[FNrOfCntrlSubSystems] := 0.0;
    Result := FNrOfCntrlSubSystems;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.RemoveControllingSubSystem (AIndex: integer) : WordBool;
const OPNAME = 'TSupportChannel.RemoveControllingSubSystem';
var
  lCount : integer;
begin
  Result := FALSE;
  try
    for lCount := AIndex to FNrOfCntrlSubSystems - 1 do
    begin
      SubSystemIDByIndex[lCount] := FSubSystemIDs[lCount+1];
      SubSystemFactorByIndex[lCount] := FSubSystemFactors[lCount+1];
    end;
    SubSystemIDByIndex[FNrOfCntrlSubSystems] := 0;
    SubSystemFactorByIndex[FNrOfCntrlSubSystems] := 0.0;
    NrOfCntrlSubSystems := FNrOfCntrlSubSystems - 1;
    SetLength(FSubSystemIDs, FNrOfCntrlSubSystems + 1);
    SetLength(FSubSystemFactors, FNrOfCntrlSubSystems + 1);
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.Get_SubSystemFactorByIndex(AIndex: integer): double;
const OPNAME = 'TSupportChannel.Get_SubSystemFactorByIndex';
begin
  try
    if ( AIndex >= 1 ) and ( AIndex <= FNrOfCntrlSubSystems) then 
      Result := FSubSystemFactors[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TSupportChannel.Get_SubSystemIDByIndex(AIndex: integer): integer;
const OPNAME = 'TSupportChannel.Get_SubSystemIDByIndex';
begin
  try
  if ( AIndex >= 1 ) and ( AIndex <= FNrOfCntrlSubSystems) then
    Result := FSubSystemIDs[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportChannel.Set_SubSystemFactorByIndex (AIndex  : integer;
                                                     AValue : double);
const OPNAME = 'TSupportChannel.Set_SubSystemFactorByIndex';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if ( AIndex >= 1 ) and ( AIndex <= FNrOfCntrlSubSystems) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SupportChannelFieldNameID ( LContextData, IntToStr ( FAllocDefID ),
                                                                  IntToStr ( FSupportChannelID ), IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
             'CntrlFactor', FloatToStr( AValue ), FloatToStr ( FSubSystemFactors [ AIndex ] ), LContextData) then
          begin
            LOldValue := FloatToStr ( FSubSystemFactors [ AIndex ] );
            FSubSystemFactors [ AIndex ] := AValue;
            FAppModules.Model.StudyDataHasChanged ( sdccEdit,'CntrlFactor', LOldValue, FloatToStr ( AValue ));
          end;
        finally
          FreeAndNil ( LContextData );
        end;
      finally
        FreeAndNil ( LLoadAgent );
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.Get_SupportChannelID: integer;
const OPNAME = 'TSupportChannel.Get_SupportChannelID';
begin
  Result := 0;
  try
    Result := FSupportChannelID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.Get_ChannelNumber: integer;
const OPNAME = 'TSupportChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportChannel.Get_NrOfCntrlSubSystems: integer;
const OPNAME = 'TSupportChannel.Get_NrOfCntrlSubSystems';
begin
  Result := 0;
  try
    Result := FNrOfCntrlSubSystems;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportChannel.Set_NrOfCntrlSubSystems( AValue : integer );
const OPNAME = 'TSupportChannel.Set_NrOfCntrlSubSystems';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SupportChannel ( LContextData, IntToStr ( FAllocDefID ),
                                                    IntToStr ( FSupportChannelID ));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'NrOfCntrlSubSystems', IntToStr ( AValue ), IntToStr( FNrOfCntrlSubSystems ), LContextData) then
        begin
          LOldValue := IntToStr( FNrOfCntrlSubSystems );
          FNrOfCntrlSubSystems := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NrOfCntrlSubSystems',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TSupportChannel.Set_ChannelNumber ( AValue : integer );
const OPNAME = 'TSupportChannel.Set_ChannelNumber';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SupportChannel ( LContextData, IntToStr ( FAllocDefID ),
                                                    IntToStr ( FSupportChannelID ));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportChannelNr', IntToStr ( AValue ), IntToStr( FChannelNumber ), LContextData) then
        begin
          LOldValue := IntToStr( FChannelNumber );
          FChannelNumber := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupportChannelNr',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportChannel.Set_SubSystemIDByIndex (AIndex, ANumber: integer);
const OPNAME = 'TSupportChannel.Set_SubSystemIDByIndex';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if ( AIndex >= 1 ) and ( AIndex <= FNrOfCntrlSubSystems) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SupportChannelFieldNameID ( LContextData, IntToStr ( FAllocDefID ),
                                                                  IntToStr ( FSupportChannelID ), IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
             'CntrlSubSystemID', IntToStr(ANumber), IntToStr ( FSubSystemIDs [ AIndex ] ), LContextData) then
          begin
            LOldValue := IntToStr ( FSubSystemIDs [ AIndex ] );
            FSubSystemIDs [ AIndex ] := ANumber;
            FAppModules.Model.StudyDataHasChanged ( sdccEdit,'CntrlSubSystemID', LOldValue, IntToStr ( ANumber ));
          end;
        finally
          FreeAndNil ( LContextData );
        end;
      finally
        FreeAndNil ( LLoadAgent );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TSupportSubSystem                                                          *}
{******************************************************************************}

procedure TSupportSubSystem.CreateMemberObjects;
const OPNAME = 'TSupportSubSystem.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportSubSystem.DestroyMemberObjects;
const OPNAME = 'TSupportSubSystem.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TSupportSubSystem._AddRef: Integer;
const OPNAME = 'TSupportSubSystem._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSupportSubSystem._Release: Integer;
const OPNAME = 'TSupportSubSystem._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSupportSubSystem.Initialise : boolean;
const OPNAME = 'TSupportSubSystem.Initialise';
var
  lIndex : integer;
begin
  Result := False;
  try
    FSupportSubSystemID := 0;
    FSubSystemID        := 0;
    for lIndex := 1 to 5 do
      FSupportChannelNrs[lIndex] := 0;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportSubSystem.Validate (var AErrors    : WideString;
                                        const AContext : WideString) : WordBool;
const OPNAME = 'TSupportSubSystem.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSupportSubSystem.Populate (AAllocDefID  : integer;
                                     ADemandDefID : integer;
                                     ASupportID   : integer;
                                     ASubSystemID : integer;
                                     AChannelNrs  : array of integer) : boolean;
const OPNAME = 'TSupportSubSystem.Populate';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FAllocDefID         := AAllocDefID;
    FDemandDefID        := ADemandDefID;
    FSupportSubSystemID := ASupportID;
    FSubSystemID        := ASubSystemID;
    for lIndex := 1 to 5 do
      FSupportChannelNrs[lIndex] := AChannelNrs[lIndex];
    Result := TRUE;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportSubSystem.Get_SupportSubSystemID: integer;
const OPNAME = 'TSupportSubSystem.Get_SupportSubSystemID';
begin
  Result := 0;
  try
    Result := FSupportSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportSubSystem.Get_SubSystemID: integer;
const OPNAME = 'TSupportSubSystem.Get_SubSystemID';
begin
  Result := 0;
  try
    Result := FSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportSubSystem.Set_SubSystemID(AValue: integer);
const OPNAME = 'TSupportSubSystem.Set_SubSystemID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SupportSubSystem ( LContextData, IntToStr ( FAllocDefID ),
                                                      IntToStr ( FDemandDefID ),
                                                      IntToStr ( FSupportSubSystemID ));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupSubSystemID', IntToStr ( AValue ), IntToStr( FSubSystemID ), LContextData) then
        begin
          LOldValue := IntToStr( FSubSystemID );
          FSubSystemID := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupSubSystemID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSupportSubSystem.Get_SupportChannelNrByIndex(AIndex: integer): integer;
const OPNAME = 'TSupportSubSystem.Get_SupportChannelNrByIndex';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := 0;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty ( 'SupSubSysChannelNr' );
    if ( AIndex >= LFieldProperty.ArrayLow ) and ( AIndex <= LFieldProperty.ArrayHigh) then
      Result := FSupportChannelNrs [ AIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSupportSubSystem.Set_SupportChannelNrByIndex(AIndex, AValue: integer);
const OPNAME = 'TSupportSubSystem.Set_SupportChannelNrByIndex';
var
  LFieldProperty : TAbstractFieldProperty;
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty ( 'SupSubSysChannelNr' );
    if ( AIndex >= LFieldProperty.ArrayLow ) and ( AIndex <= LFieldProperty.ArrayHigh) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SupportSubSystemFieldNameID ( LContextData, IntToStr ( FAllocDefID ),
                                                        IntToStr ( FDemandDefID ),
                                                        IntToStr ( FSupportSubSystemID ), IntToStr ( AIndex ));
          if FAppModules.FieldProperties.UpdateFieldValue(
             'SupSubSysChannelNr', IntToStr ( AValue ), IntToStr( FSupportChannelNrs [ AIndex ] ), LContextData) then
          begin
            LOldValue := IntToStr( FSubSystemID );
            FSupportChannelNrs [ AIndex ] := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'SupSubSysChannelNr',LOldValue,IntToStr ( AValue ));
          end;
        finally
          FreeAndNil ( LContextData );
        end;
      finally
        FreeAndNil ( LLoadAgent );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TDemandDefinition                                                          *}
{******************************************************************************}

procedure TDemandDefinition.CreateMemberObjects;
const OPNAME = 'TDemandDefinition.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSupportSubSystems := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.DestroyMemberObjects;
const OPNAME = 'TDemandDefinition.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition._AddRef: Integer;
const OPNAME = 'TDemandDefinition._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandDefinition._Release: Integer;
const OPNAME = 'TDemandDefinition._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDemandDefinition.Initialise : boolean;
const OPNAME = 'TDemandDefinition.Initialise';
begin
  Result := False;
  try
    FDemandDefID         := 0;
    FName                := '';
    FOrder               := 0;
    FParentSubSystemID   := 0;
    FGrowthType          := 0;
    FTargetDemand        := 0.0;
    FDemandCentreID      := 0;
    FUserCategoryID      := 0;
    FSupportArc1         := 0;
    FSupportArc2         := 0;
    FSupportSubSystems.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Validate (var AErrors    : WideString;
                                  const AContext : WideString) : WordBool;
const OPNAME = 'TDemandDefinition.Validate';
var
  lErrorList : TStringList;
  lErrorCols : TStringList;
  lColFlag   : Boolean;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      lColFlag := FALSE;
      if (AContext = 'DDDemandCentreID') then
        Result := ValidateDemandCentreID(lErrorList)
      else
      if (AContext = 'ParentSubSystemID') then
        Result := ValidateResidentInSubSystem(lErrorList)
      else
      if (AContext = 'GrowthType') then
        Result := ValidateGrowthType(lErrorList)
      else
      if (AContext = 'TargetDemand') then
        Result := ValidateTargetDemand(lErrorList)
      else
      if (AContext = 'UserCategory') then
        Result := ValidateUserCategory(lErrorList)
      else
      if (AContext = 'SupportArc1') then
        Result := ValidateSupportArc1(lErrorList)
      else
      if (AContext = 'SupportArc2') then
        Result := ValidateSupportArc2(lErrorList)
      else
      if (AContext = 'SupportSubSystems') then
      begin
        Result := ValidateSupportSubSystems(lErrorList, lErrorCols);
        lColFlag := TRUE;
      end;
      if (lColFlag AND (NOT Result)) then
        AddErrors(AErrors, lErrorList, lErrorCols);
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(lErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateDemandCentreID (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateDemandCentreID';
var
  lMessage          : string;
  lDemandCentre     : IMasterControlFeature;
{  lAllocDefList     : TAllocationDefinitionsList;
  lUnique           : Boolean;
  lIndex            : integer;
  lAllocDef         : TAllocationDefinition;
  lDemandDefinition : TDemandDefinition;}
begin
  Result := False;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                 ('DDDemandCentreID', IntToStr(FDemandCentreID), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+FName + ':' + lMessage)
    else
    begin
      lDemandCentre := (FAppModules.Model.ModelData as IYieldModelData).
                         NetworkFeaturesData.MasterControlFeatureList.DemandCentreByID[FDemandCentreID];
      if (lDemandCentre = nil) then
      begin
        Result := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.DemandCentreDoesNotExist');
        AErrorMessages.Add('ERROR:'+Format(lMessage, [FName]));
      end;
{
      lAllocDefList := TPlanningModelDataObject(FAppModules.Model.ModelData).
                         CastAllocationDefinitionsList;
      lAllocDef := lAllocDefList.CastAllocationDefinitionByID(FAllocDefID);
      lUnique   := True;
      lIndex    := 0;
      while (lUnique AND (lIndex < lAllocDef.NrOfDemandDefinitions)) do
      begin
        lDemandDefinition := lAllocDef.CastDemandDefinitionByIndex(lIndex);
        if ((FDemandDefID <> lDemandDefinition.FDemandDefID) AND
            (UpperCase(Trim(FName)) = UpperCase(Trim(lDemandDefinition.FName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateDemandDefinitionName');
          AErrorMessages.Add(Format(lMessage, [FName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      Result := lUnique;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateResidentInSubSystem (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateResidentInSubSystem';
var
  lMessage      : string;
  lSubSystem    : TSubSystem;
  lAllocDefList : TAllocationDefinitionsList;
  lAllocDef     : TAllocationDefinition;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('ParentSubSystemID', IntToStr(FParentSubSystemID), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FParentSubSystemID) + ':' +  lMessage)
    else
    begin
      lAllocDefList := TPlanningModelDataObject(FAppModules.Model.ModelData).
                         CastAllocationDefinitionsList;
      lAllocDef  := lAllocDefList.CastAllocationDefinitionByID(FAllocDefID);
      lSubSystem := lAllocDef.CastSubSystemByID(FParentSubSystemID);
      if (lSubSystem = nil) then
      begin
        Result   := FALSE;
        if (FParentSubSystemID = 0) then
          lMessage := FAppModules.Language.GetString('ContextValidation.UnspecifiedSubSystem')
        else
          lMessage := FAppModules.Language.GetString('ContextValidation.SubSystemNotExist');
        AErrorMessages.Add ('ERROR:'+ Format ( lMessage, [ IntToStr ( FParentSubSystemID ) ] ));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateGrowthType (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateGrowthType';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('GrowthType', IntToStr(FGrowthType), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+LMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateTargetDemand (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateTargetDemand';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('TargetDemand', FloatToStr(FTargetDemand), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+LMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateUserCategory (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateUserCategory';
var
  lMessage      : string;
  lUserCategory : TUserCategory;
  lAllocDefList : TAllocationDefinitionsList;
  lAllocDef     : TAllocationDefinition;
begin
  Result := FALSE;
  try
    lMessage := '';
    Result   := FAppModules.FieldProperties.ValidateFieldProperty
                  ('DCUserCategoryID', IntToStr(FUserCategoryID), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FUserCategoryID) + ':' +  lMessage)
    else
    begin
      lAllocDefList := TPlanningModelDataObject(FAppModules.Model.ModelData).
                         CastAllocationDefinitionsList;
      lAllocDef  := lAllocDefList.CastAllocationDefinitionByID(FAllocDefID);
      lUserCategory := lAllocDef.CastCategoryByID(FUserCategoryID);
      if (lUserCategory = nil) then
      begin
        Result   := FALSE;
        if (FUserCategoryID = 0) then
          lMessage := FAppModules.Language.GetString ('ContextValidation.UnspecifiedUserCategory')
        else
          lMessage := FAppModules.Language.GetString ('ContextValidation.UserCategoryNotExist');
        AErrorMessages.Add('ERROR:'+ Format ( lMessage, [ IntToStr ( FUserCategoryID ) ] ));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateSupportArc1 (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateSupportArc1';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SupportArc1', IntToStr(FSupportArc1), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+LMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateSupportArc2 (AErrorMessages : TStrings) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateSupportArc2';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('SupportArc2', IntToStr(FSupportArc1), LMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:'+LMessage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.ValidateSupportSubSystems (AErrorMessages : TStrings;
                                                  AErrorColumns  : TStringList) : WordBool;
const OPNAME = 'TDemandDefinition.ValidateSupportSubSystems';
var
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lLocalResult      : boolean;
  lIndex            : integer;
  lCount            : integer;
  lLoop             : integer;
  lDuplicate        : boolean;
  lSubSysID         : integer;
  lSubSystem        : ISubSystem;
  lAllocDef         : IAllocationDefinition;
  lSupSubSystem     : TSupportSubSystem;
  lChannelNr        : integer;
  lChannel          : IGeneralFlowChannel;
  lOther            : TSupportSubSystem;
begin
  Result := FALSE;
  try
    AErrorColumns.Clear;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lAllocDef         := (FAppModules.Model.ModelData as IPlanningModelData).
                           AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
    lResult := TRUE;
    lIndex  := 0;
    while (lResult OR (NOT lStopOnFirstError)) AND (lIndex < NrOfSupportSubSystems) do
    begin
      lMessage  := '';
      lLocalResult  := TRUE;
      lSupSubSystem := CastSupportSubSystemByIndex(lIndex);
      lSubSysID     := lSupSubSystem.FSubSystemID;
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
               ('SupSubSystemID', IntToStr(lSubSysID), lMessage, lIndex)) then
      begin
        lResult := FALSE;
        lLocalResult := FALSE;
        AErrorMessages.Add('ERROR:'+lMessage);
        AErrorColumns.Add (IntToStr(lIndex+1) + ',0');
        if (lStopOnFirstError) then
          Break;
      end
      else
      begin
        lSubSystem := lAllocDef.SubSystemByID[lSubSysID];
        if (lSubSystem = nil) then
        begin
          lResult := FALSE;
          lLocalResult := FALSE;
          if (lSubSysID = 0) then
            lMessage := FAppModules.Language.GetString ('ContextValidation.UnspecifiedSubSystem')
          else
            lMessage := FAppModules.Language.GetString ('ContextValidation.SubSystemNotExist');
          AErrorMessages.Add( 'ERROR:'+Format ( lMessage, [ IntToStr ( lSubSysID ) ] ));
          AErrorColumns.Add (IntToStr(lIndex+1) + ',0');
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      if (lLocalResult) then
      begin
        lDuplicate := FALSE;
        lCount     := lIndex - 1;
        while (NOT lDuplicate) AND (lCount >= 0) do
        begin
          lOther := CastSupportSubSystemByIndex(lCount);
          if (lSupSubSystem.SupportSubSystemID <> lOther.SupportSubSystemID) AND
             (lSupSubSystem.SubSystemID = lOther.SubSystemID) then
            lDuplicate := TRUE
          else
            lCount := lCount - 1;
        end;
        if (lDuplicate) then
        begin
          lResult := FALSE;
          lMessage := 'Duplicate supporting sub-system.';
          AErrorMessages.Add('ERROR:'+lMessage);
          AErrorColumns.Add (IntToStr(lIndex+1) + ',0');
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      for lCount := 1 to 5 do
      begin
        lChannelNr := lSupSubSystem.FSupportChannelNrs[lCount];
        if (lChannelNr <> 0) then
        begin
          if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                   ('SupSubSysChannelNr', IntToStr(lChannelNr), lMessage, lCount)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:'+lMessage);
            AErrorColumns.Add (IntToStr(lIndex+1) + ',' + IntToStr(lCount));
            if (lStopOnFirstError) then
              Break;
          end
          else
          begin
            lChannel := (FAppModules.Model.ModelData as IYieldModelData).
                          NetworkElementData.ChannelList.ChannelByChannelNumber[lChannelNr];
            if (lChannel = nil) then
            begin
              lResult   := FALSE;
              if (lChannelNr = 0) then
                lMessage := FAppModules.Language.GetString ('ContextValidation.UnspecifiedChannel')
              else
                lMessage := FAppModules.Language.GetString ('ContextValidation.ChannelNotExist');
              AErrorMessages.Add('ERROR:'+ Format ( lMessage, [ IntToStr ( lChannelNr ) ] ));
              AErrorColumns.Add (IntToStr(lIndex+1) + ',' + IntToStr(lCount));
            end
            else
            begin
              lDuplicate := FALSE;
              lLoop := lCount - 1;
              while ((NOT lDuplicate) AND (lLoop >= 1)) do
              begin
                if (lChannelNr = lSupSubSystem.FSupportChannelNrs[lLoop]) then
                  lDuplicate := TRUE
                else
                  lLoop := lLoop - 1;
              end;
              if (lDuplicate) then
              begin
                lResult   := FALSE;
                lMessage := FAppModules.Language.GetString ('ContextValidation.DuplicateChannel');
                AErrorMessages.Add( 'ERROR:'+Format ( lMessage, [ IntToStr ( lChannelNr ) ] ));
                AErrorColumns.Add (IntToStr(lIndex+1) + ',' + IntToStr(lCount));
              end;
            end;
          end;
        end;
      end;
      lIndex := lIndex + 1;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.Populate (AAllocDefID        : integer;
                                 ADemandDefID       : integer;
                                 AName              : string;
                                 AParentSubSystemID : integer;
                                 AOrder             : integer;
                                 AGrowthType        : integer;
                                 ATargetDemand      : double;
                                 ADemandCentreID    : integer;
                                 AUserCategoryID    : integer;
                                 ASupportArc1       : integer;
                                 ASupportArc2       : integer) : boolean;
const OPNAME = 'TDemandDefinition.Populate';
begin
  Result := FALSE;
  try
    FAllocDefID        := AAllocDefID;
    FDemandDefID       := ADemandDefID;
    FName              := AName;
    FParentSubSystemID := AParentSubSystemID;
    FOrder             := AOrder;
    FGrowthType        := AGrowthType;
    FTargetDemand      := ATargetDemand;
    FDemandCentreID    := ADemandCentreID;
    FUserCategoryID    := AUserCategoryID;
    FSupportArc1       := ASupportArc1;
    FSupportArc2       := ASupportArc2;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.NewSupportSubSystem : ISupportSubSystem;
const OPNAME = 'TDemandDefinition.NewSupportSubSystem';
var
  LSupportSubSystem : TSupportSubSystem;
  LSupportSubSystemID : integer;
  lLoadAgent : TAllocationDefLoadAgent;
begin
  Result := nil;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertSupportSubSystem ( FAllocDefID ,FDemandDefID, LSupportSubSystemID )) then
      begin
        LSupportSubSystem := CreateSupportSubSystem;
        LSupportSubSystem.Initialise;
        LSupportSubSystem.FAllocDefID         := FAllocDefID;
        LSupportSubSystem.FDemandDefID        := FDemandDefID;
        LSupportSubSystem.FSupportSubSystemID := LSupportSubSystemID;
        Result := LSupportSubSystem;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.CreateSupportSubSystem : TSupportSubSystem;
const OPNAME = 'TDemandDefinition.CreateSupportSubSystem';
var
  lSupSubSys : TSupportSubSystem;
begin
  Result := nil;
  try
    lSupSubSys := TSupportSubSystem.Create ( FAppModules );
    FSupportSubSystems.Add ( lSupSubSys );
    FNrOfSupportSubSystems := FNrOfSupportSubSystems + 1;
    Result := lSupSubSys;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.RemoveSupportSubSystem (AIndex : integer) : WordBool;
const OPNAME = 'TDemandDefinition.RemoveSupportSubSystem';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LSupportSubSystem  : TSupportSubSystem;
begin
  Result := False;
  try
    LSupportSubSystem := CastSupportSubSystemByIndex ( AIndex );
    if ( LSupportSubSystem <> nil) then
    begin
      LLoadAgent := TAllocationDefLoadAgent.Create ( FAppModules );
      try
        if ( LLoadAgent.DeleteSupportSubSystemByID ( LSupportSubSystem.FSupportSubSystemID, FAllocDefID, FDemandDefID )) then
        begin
          DeleteSupportSubSystem ( AIndex );
          Result := True;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.DeleteSupportSubSystem(AIndex : integer) : WordBool;
const OPNAME = 'TDemandDefinition.DeleteSupportSubSystem';
begin
  Result := FALSE;
  try
    if (FSupportSubSystems.Count > AIndex) then
    begin
      FSupportSubSystems.Delete(AIndex);
      FNrOfSupportSubSystems := FNrOfSupportSubSystems - 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.CastSupportSubSystemByIndex(AIndex: integer): TSupportSubSystem;
const OPNAME = 'TDemandDefinition.CastSupportSubSystemByIndex';
begin
  Result := nil;
  try
    Result := TSupportSubSystem(FSupportSubSystems.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.CastSupportSubSystemByID(AID: integer): TSupportSubSystem;
const OPNAME = 'TDemandDefinition.CastSupportSubSystemByID';
var
  lIndex     : integer;
  lSupSubSys : TSupportSubSystem;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSupportSubSystems.Count)) do
    begin
      lSupSubSys := TSupportSubSystem(FSupportSubSystems.Items[lIndex]);
      if (lSupSubSys.SubSystemID = AID) then
        Result := lSupSubSys
      else
        lIndex := lIndex + 1;;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_SupportSubSystemByIndex(AIndex: integer): ISupportSubSystem;
const OPNAME = 'TDemandDefinition.Get_SupportSubSystemByIndex';
begin
  Result := nil;
  try
    Result := CastSupportSubSystemByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_SupportSubSystemByID(AID: integer): ISupportSubSystem;
const OPNAME = 'TDemandDefinition.Get_SupportSubSystemByID';
begin
  Result := nil;
  try
    Result := CastSupportSubSystemByID(AID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_DemandDefID : integer;
const OPNAME = 'TDemandDefinition.Get_DemandDefID';
begin
  Result := 0;
  try
    Result := FDemandDefID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_Name : WideString;
const OPNAME = 'TDemandDefinition.Get_Name';
begin
  Result := '';
  try
    Result := FName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_Order: integer;
const OPNAME = 'TDemandDefinition.Get_Order';
begin
  Result := 0;
  try
    Result := FOrder;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDemandDefinition.Set_Name(const AValue : WideString);
const OPNAME = 'TDemandDefinition.Set_Name';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DemandDefName', AValue, FName, LContextData) then
        begin
          LOldValue := FName;
          FName := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DemandDefName',LOldValue,AValue);
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_Order (AValue : integer);
const OPNAME = 'TDemandDefinition.Set_Order';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DemandDefOrder', IntToStr(AValue), IntToStr(FOrder), LContextData) then
        begin
          LOldValue := IntToStr(FOrder);
          FOrder := AValue;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit, 'DemandDefOrder', LOldValue, IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDefinition.Get_ParentSubSystemID : integer;
const OPNAME = 'TDemandDefinition.Get_ParentSubSystemID';
begin
  Result := 0;
  try
    Result := FParentSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_ParentSubSystemID(AValue : integer);
const OPNAME = 'TDemandDefinition.Set_ParentSubSystemID';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  lAllocDef    : IAllocationDefinition;
  lOldResID    : integer;
  lCount       : integer;
  lOldOrder    : integer;
  lNewOrder    : integer;
  lOther       : IDemandDefinition;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ParentSubSystemID', IntToStr(AValue), IntToStr(FParentSubSystemID), LContextData) then
        begin
          lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                       AllocationDefinitionsList.AllocationDefinitionByID[FAllocDefID];
          lOldResID := FParentSubSystemID;
          lOldOrder := FOrder;
          lNewOrder := 0;
          for lCount := 0 to lAllocDef.NrOfDemandDefinitions - 1 do
          begin
            lOther := lAllocDef.DemandDefinitionByIndex[lCount];
            if (lOther.DemandDefID <> FDemandDefID) then
            begin
              if (lOther.ParentSubSystemID = lOldResID) AND (lOther.Order > lOldOrder) then
                lOther.Order := lOther.Order - 1
              else
              if (lOther.ParentSubSystemID = AValue) then
                lNewOrder := lNewOrder + 1;
            end;
          end;
          FParentSubSystemID := AValue;
          Self.Order         := lNewOrder + 1;
          lCount := 0;
          while (lCount < FSupportSubSystems.Count) do
          begin
            if (TSupportSubSystem(FSupportSubSystems.Items[lCount]).FSubSystemID = FParentSubSystemID) then
              RemoveSupportSubSystem(lCount)
            else
              lCount := lCount + 1;
          end;
          FAppModules.Model.StudyDataHasChanged
            (sdccEdit,'ParentSubSystemID', IntToStr(lOldResID), IntToStr(AValue));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_GrowthType : integer;
const OPNAME = 'TDemandDefinition.Get_GrowthType';
begin
  Result := 0;
  try
    Result := FGrowthType;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_GrowthType ( AValue : integer );
const OPNAME = 'TDemandDefinition.Set_GrowthType';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'GrowthType', IntToStr ( AValue ), IntToStr ( FGrowthType ), LContextData) then
        begin
          LOldValue := IntToStr ( FGrowthType );
          FGrowthType := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'GrowthType',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_TargetDemand : double;
const OPNAME = 'TDemandDefinition.Get_TargetDemand';
begin
  Result := 0.0;
  try
    Result := FTargetDemand;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_TargetDemand(AValue : double);
const OPNAME = 'TDemandDefinition.Set_TargetDemand';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'TargetDemand', FloatToStr ( AValue ), FloatToStr ( FTargetDemand ), LContextData) then
        begin
          LOldValue := FloatToStr ( FTargetDemand );
          FTargetDemand := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'TargetDemand',LOldValue,FloatToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_DemandCentreID : integer;
const OPNAME = 'TDemandDefinition.Get_DemandCentreID';
begin
  Result := 0;
  try
    Result := FDemandCentreID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_DemandCentreID(AValue : integer);
const OPNAME = 'TDemandDefinition.Set_DemandCentreID';
var
  LLoadAgent   : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DDDemandCentreID', IntToStr(AValue), IntToStr(FDemandCentreID), LContextData) then
        begin
          LOldValue := IntToStr(FDemandCentreID);
          FDemandCentreID := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'DDDemandCentreID', LOldValue, IntToStr(AValue));
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_UserCategoryID : integer;
const OPNAME = 'TDemandDefinition.Get_UserCategoryID';
begin
  Result := 0;
  try
    Result := FUserCategoryID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_UserCategoryID(AValue : integer);
const OPNAME = 'TDemandDefinition.Set_UserCategoryID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DCUserCategoryID', IntToStr ( AValue ), IntToStr ( FUserCategoryID ), LContextData) then
        begin
          LOldValue := IntToStr ( FUserCategoryID );
          FUserCategoryID := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'DCUserCategoryID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_SupportArc1 : integer;
const OPNAME = 'TDemandDefinition.Get_SupportArc1';
begin
  Result := 0;
  try
    Result := FSupportArc1;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_SupportArc1(AValue : integer);
const OPNAME = 'TDemandDefinition.Set_SupportArc1';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportArc1', IntToStr ( AValue ), IntToStr ( FSupportArc1 ), LContextData) then
        begin
          LOldValue := IntToStr ( FSupportArc1 );
          FSupportArc1 := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'SupportArc1',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_SupportArc2 : integer;
const OPNAME = 'TDemandDefinition.Get_SupportArc2';
begin
  Result := 0;
  try
    Result := FSupportArc2;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDemandDefinition.Get_NrOfSupportSubSystems : integer;
const OPNAME = 'TDemandDefinition.Get_NrOfSupportSubSystems';
begin
  Result := 0;
  try
    Result := FNrOfSupportSubSystems;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDemandDefinition.Set_NrOfSupportSubSystems ( AValue : integer ); safecall;
const OPNAME = 'TDemandDefinition.Set_NrOfSupportSubSystems';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'NrOfSupportSubSystems', IntToStr ( AValue ), IntToStr ( FNrOfSupportSubSystems ), LContextData) then
        begin
          LOldValue := IntToStr ( FNrOfSupportSubSystems );
          FNrOfSupportSubSystems := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'NrOfSupportSubSystems',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDemandDefinition.Set_SupportArc2(AValue : integer);
const OPNAME = 'TDemandDefinition.Set_SupportArc2';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DemandDefinition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FDemandDefID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SupportArc2', IntToStr ( AValue ), IntToStr ( FSupportArc2 ), LContextData) then
        begin
          LOldValue := IntToStr ( FSupportArc2 );
          FSupportArc2 := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'SupportArc2',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TAllocationDefinitionsList                                                 *}
{******************************************************************************}

procedure TAllocationDefinitionsList.CreateMemberObjects;
const OPNAME = 'TAllocationDefinitionsList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FAllocationDefinitionsList := TObjectList.Create(TRUE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAllocationDefinitionsList.DestroyMemberObjects;
const OPNAME = 'TAllocationDefinitionsList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FAllocationDefinitionsList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList._AddRef: Integer;
const OPNAME = 'TAllocationDefinitionsList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList._Release: Integer;
const OPNAME = 'TAllocationDefinitionsList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.AddAllocationDefinition (AAllocDef : TAllocationDefinition): boolean;
const OPNAME = 'TAllocationDefinitionsList.AddAllocationDefinition';
begin
  Result := False;
  try
    if (AAllocDef <> nil) then
    begin
      FAllocationDefinitionsList.Add(AAllocDef);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.Get_AllocationDefinitionByID (AID : integer): IAllocationDefinition;
const OPNAME = 'TAllocationDefinitionsList.Get_AllocationDefinitionByID';
begin
  Result := nil;
  try
    Result := CastAllocationDefinitionByID(AID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.CastAllocationDefinitionByID (AID : integer): TAllocationDefinition;
const OPNAME = 'TAllocationDefinitionsList.CastAllocationDefinitionByID';
var
 LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FAllocationDefinitionsList.Count -1 do
      if (TAllocationDefinition(FAllocationDefinitionsList[LIndex]).AllocationDefinitionID = AID) then
      begin
        Result := TAllocationDefinition(FAllocationDefinitionsList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.Get_AllocationDefinitionByIndex (AIndex: integer): IAllocationDefinition;
const OPNAME = 'TAllocationDefinitionsList.Get_AllocationDefinitionByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FAllocationDefinitionsList.Count) then
      Result := TAllocationDefinition(FAllocationDefinitionsList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.CastAllocationDefinitionByIndex(AIndex: integer): TAllocationDefinition;
const OPNAME = 'TAllocationDefinitionsList.CastAllocationDefinitionByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) AND (AIndex < FAllocationDefinitionsList.Count) then
      Result := TAllocationDefinition(FAllocationDefinitionsList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.Get_AllocationDefinitionCount: integer;
const OPNAME = 'TAllocationDefinitionsList.Get_AllocationDefinitionCount';
begin
  Result := 0;
  try
    Result := FAllocationDefinitionsList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.Initialise: boolean;
const OPNAME = 'TAllocationDefinitionsList.Initialise';
begin
  Result := inherited Initialise;
  try
    FAllocationDefinitionsList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.CreateAllocationDefinition : TAllocationDefinition;
const OPNAME = 'TAllocationDefinitionsList.CreateAllocationDefinition';
var
  lAllocDef : TAllocationDefinition;
begin
  Result := nil;
  try
    lAllocDef := TAllocationDefinition.Create(FAppModules);
    AddAllocationDefinition(lAllocDef);
    Result := lAllocDef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.NewAllocationDefinition : IAllocationDefinition;
const OPNAME = 'TAllocationDefinitionsList.NewAllocationDefinition';
var
  lAllocDef   : TAllocationDefinition;
  lAllocDefID : integer;
  lLoadAgent  : TAllocationDefLoadAgent;
  lTempName   : string;
begin
  Result := nil;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertAllocationDefinition(lAllocDefID)) then
      begin
        lAllocDef := CreateAllocationDefinition;
        lAllocDef.Initialise;
        lAllocDef.FAllocDefID := lAllocDefID;
        lTempName := FAppModules.Language.GetString ('ContextValidation.AllocationDefinition');
        lAllocDef.Name := Format(lTempName, [IntToStr(lAllocDefID)]);
        lAllocDef.NrOfReliabilityClasses := 4;
        lAllocDef.RecurrenceIntervalByIndex[1] := 200;
        lAllocDef.RecurrenceIntervalByIndex[2] := 100;
        lAllocDef.RecurrenceIntervalByIndex[3] := 50;
        lAllocDef.RecurrenceIntervalByIndex[4] := 20;
        Result := lAllocDef;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.RemoveAllocationDefinitionWithID (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinitionsList.RemoveAllocationDefinitionWithID';
var
  lLoadAgent : TAllocationDefLoadAgent;
  lAllocDef  : TAllocationDefinition;
begin
  Result := False;
  try
    lAllocDef := CastAllocationDefinitionByID(AID);
    if (lAllocDef <> nil) then
    begin
      lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
      try
        if (lLoadAgent.DeleteAllocationDefinition(AID)) then
        begin
          DeleteAllocationDefinitionWithID(AID);
          Result := TRUE;
        end;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.DeleteAllocationDefinitionWithID (AID : integer) : WordBool;
const OPNAME = 'TAllocationDefinitionsList.DeleteAllocationDefinitionWithID';
var
  LIndex: integer;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FAllocationDefinitionsList.Count -1 do
    begin
      if (TAllocationDefinition(FAllocationDefinitionsList.Items[lIndex]).AllocationDefinitionID = AID) then
      begin
        FAllocationDefinitionsList.Remove(FAllocationDefinitionsList.Items[lIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAllocationDefinitionsList.DeleteAllocationDefinitionWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TAllocationDefinitionsList.DeleteAllocationDefinitionWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) and (AIndex < FAllocationDefinitionsList.Count) then
    begin
      FAllocationDefinitionsList.Remove(FAllocationDefinitionsList.Items[AIndex]);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TFixedPosition                                                             *}
{******************************************************************************}

function TFixedPosition._AddRef: Integer;
const OPNAME = 'TFixedPosition._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFixedPosition._Release: Integer;
const OPNAME = 'TFixedPosition._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFixedPosition.CreateMemberObjects;
const OPNAME = 'TFixedPosition.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TFixedPosition.DestroyMemberObjects;
const OPNAME = 'TFixedPosition.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFixedPosition.Get_FixedPosSubSystemID: integer;
const OPNAME = 'TFixedPosition.Get_FixedPosSubSystemID';
begin
  Result := 0;
  try
    Result := FFixedPosSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFixedPosition.Get_FixedPositionID : integer;
const OPNAME = 'TFixedPosition.Get_FixedPositionID';
begin
  Result := 0;
  try
    Result := FFixedPositionID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFixedPosition.Get_FixedPositionNr : integer;
const OPNAME = 'TFixedPosition.Get_FixedPositionNr';
begin
  Result := 0;
  try
    Result := FFixedPositionNr;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFixedPosition.Set_FixedPositionNr ( AValue : integer );
const OPNAME = 'TFixedPosition.Set_FixedPositionNr';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FixedPosition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FFixedPositionID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'FixedPositionNr', IntToStr ( AValue ), IntToStr ( FFixedPositionNr ), LContextData) then
        begin
          LOldValue := IntToStr ( FFixedPositionNr );
          FFixedPositionNr := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'FixedPositionNr',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TFixedPosition.Set_FixedPosSubSystemID(AValue: integer);
const OPNAME = 'TFixedPosition.Set_FixedPosSubSystemID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FixedPosition(LContextData, IntToStr(FAllocDefID),
                                               IntToStr(FFixedPositionID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'FixedPosSubSystemID', IntToStr ( AValue ), IntToStr ( FFixedPosSubSystemID ), LContextData) then
        begin
          LOldValue := IntToStr ( FFixedPosSubSystemID );
          FFixedPosSubSystemID := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'FixedPosSubSystemID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFixedPosition.Populate ( AAllocDefID          : integer;
                                   AFixedPositionID     : integer;
                                   AFixedPositionNr     : integer;
                                   AFixedPosSubSystemID : integer ) : boolean;
const OPNAME = 'TFixedPosition.Populate';
begin
  Result := False;
  try
    FAllocDefID          := AAllocDefID;
    FFixedPositionID     := AFixedPositionID;
    FFixedPositionNr     := AFixedPositionNr;
    FFixedPosSubSystemID := AFixedPosSubSystemID;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TSpecificOrder                                                             *}
{******************************************************************************}

function TSpecificOrder._AddRef: Integer;
const OPNAME = 'TSpecificOrder._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecificOrder._Release: Integer;
const OPNAME = 'TSpecificOrder._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecificOrder.CreateMemberObjects;
const OPNAME = 'TSpecificOrder.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSpecificOrder.DestroyMemberObjects;
const OPNAME = 'TSpecificOrder.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSpecificOrder.Get_AfterSubSystemID : integer;
const OPNAME = 'TSpecificOrder.Get_AfterSubSystemID';
begin
  Result := 0;
  try
    REsult := FAfterSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSpecificOrder.Get_SpecificOrderID : integer;
const OPNAME = 'TSpecificOrder.Get_SpecificOrderID';
begin
  Result := 0;
  try
    Result := FSpecificOrderID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSpecificOrder.Get_BeforeSubSystemID : integer;
const OPNAME = 'TSpecificOrder.Get_BeforeSubSystemID';
begin
  Result := 0;
  try
    Result := FBeforeSubSystemID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSpecificOrder.Set_AfterSubSystemID(AValue: integer);
const OPNAME = 'TSpecificOrder.Set_AfterSubSystemID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SpecificOrder ( LContextData, IntToStr(FAllocDefID),
                                                   IntToStr(FSpecificOrderID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AfterSubSystemID', IntToStr ( AValue ), IntToStr ( FBeforeSubSystemID ), LContextData) then
        begin
          LOldValue := IntToStr ( FAfterSubSystemID );
          FAfterSubSystemID := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'AfterSubSystemID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSpecificOrder.Set_BeforeSubSystemID(Avalue: integer);
const OPNAME = 'TSpecificOrder.Set_BeforeSubSystemID';
var
  LLoadAgent : TAllocationDefLoadAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_SpecificOrder ( LContextData, IntToStr(FAllocDefID),
                                                   IntToStr(FSpecificOrderID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'BeforeSubSystemID', IntToStr ( AValue ), IntToStr ( FBeforeSubSystemID ), LContextData) then
        begin
          LOldValue := IntToStr ( FBeforeSubSystemID );
          FBeforeSubSystemID := AValue;
          FAppModules.Model.StudyDataHasChanged ( sdccEdit,'BeforeSubSystemID',LOldValue,IntToStr ( AValue ));
        end;
      finally
        FreeAndNil ( LContextData );
      end;
    finally
      FreeAndNil ( LLoadAgent );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSpecificOrder.Populate ( AAllocDefID        : integer;
                                   ASpecificOrderID   : integer;
                                   ABeforeSubSystemID : integer;
                                   AAfterSubSystemID  : integer ) : boolean;
const OPNAME = 'TSpecificOrder.Populate';
begin
  Result := False;
  try
    FAllocDefID        := AAllocDefID;
    FSpecificOrderID   := ASpecificOrderID;
    FBeforeSubSystemID := ABeforeSubSystemID;
    FAfterSubSystemID  := AAfterSubSystemID;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TTree }

constructor TTree.Create;
const OPNAME = 'TTree.Create';
begin
  try
    FText     := '';
    FParent   := nil;
    FChildren := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

destructor TTree.Destroy;
const OPNAME = 'TTree.Destroy';
begin
  try
    inherited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTree.AddNode(AParent: TTree; AText: string): TTree;
const OPNAME = 'TTree.AddNode';
var
  lNode  : TTree;
begin
  Result := nil;
  try
    lNode := TTree.Create;
    lNode.FText   := AText;
    lNode.FParent := AParent;
    if (AParent = nil) then
      Self.FChildren.Add(lNode)
    else  
      AParent.FChildren.Add(lNode);
    Result := lNode;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTree.FindNode(AText: string): TTree;
const OPNAME = 'TTree.FindNode';
var
  lIndex : integer;
begin
  Result := nil;
  try
    if (Self.FText = AText) then
      Result := Self
    else
    begin
      lIndex := 0;
      while ((Result = nil) AND (lIndex < FChildren.Count)) do
      begin
        Result := TTree(FChildren[lIndex]).FindNode(AText);
        lIndex := lIndex + 1;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
