unit UMiningData;
{******************************************************************************}
{*  UNIT      : Contains the class TMineList                                  *}
{*  AUTHOR    : Simo Simelane                                                 *}
{*  DATE      : 2007/03/15                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

interface
uses
  Classes,
  Contnrs,
  UConstants,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TOpenCast = class (TAbstractAppObject,IOpenCast)
  protected
    FMineIdentifier              : integer;
    FIdentifier                  : integer;
    FPitName                     : WideString;
    FCoalReserveArea             : double;
    FWorkingsArea                : double;
    FDisturbedWorkingsArea       : double;
    FDisturbedArea               : double;
    FWaterSurfaceEvapArea        : double;
    FDisturbedAreaRunoff         : double;
    FDisturbedWorkingsAreaRunoff : double;
    FDecantVolume                : double;
    FSeepageVolume               : double;
    FAnalysisStartVolume         : double;
    FMaximumSeepageRate          : double;
    FSeepageExponent             : double;
    FPCDSurfaceArea              : double;
    FPCDStorageCapacity          : double;
    FPCDAnalysisStartVolume      : double;
    FDisturbedRechargeFactors    : TMonthlyDoubleArray;
    FWorkingAreaRechargeFactors  : TMonthlyDoubleArray;

    function ValidatePitName(AErrorMessages: TStrings)                     : Boolean;
    function ValidateCoalReserveArea(AErrorMessages: TStrings)             : Boolean;
    function ValidateWorkingsArea(AErrorMessages: TStrings)                : Boolean;
    function ValidateDisturbedWorkingsArea(AErrorMessages: TStrings)       : Boolean;
    function ValidateDisturbedArea(AErrorMessages: TStrings)               : Boolean;
    function ValidateWaterSurfaceEvapArea(AErrorMessages: TStrings)        : Boolean;
    function ValidateDisturbedAreaRunoff(AErrorMessages: TStrings)         : Boolean;
    function ValidateDisturbedWorkingsAreaRunoff(AErrorMessages: TStrings) : Boolean;
    function ValidateDecantVolume(AErrorMessages: TStrings)                : Boolean;
    function ValidateSeepageVolume(AErrorMessages: TStrings)               : Boolean;
    function ValidateAnalysisStartVolume(AErrorMessages: TStrings)         : Boolean;
    function ValidateMaximumSeepageRate(AErrorMessages: TStrings)          : Boolean;
    function ValidateSeepageExponent(AErrorMessages: TStrings)             : Boolean;
    function ValidatePCDSurfaceArea(AErrorMessages: TStrings)              : Boolean;
    function ValidatePCDStorageCapacity(AErrorMessages: TStrings)          : Boolean;
    function ValidatePCDAnalysisStartVolume(AErrorMessages: TStrings)      : Boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ASource: TOpenCast);virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetIdentifier : integer;
    function Get_Identifier : integer; safecall;
    function Get_PitName : WideString; safecall;
    procedure Set_PitName (const Value : WideString); safecall;
    function Get_CoalReserveArea : double; safecall;
    procedure Set_CoalReserveArea (Value : double); safecall;
    function Get_WorkingsArea : double; safecall;
    procedure Set_WorkingsArea (Value : double); safecall;
    function Get_DisturbedWorkingsArea : double; safecall;
    procedure Set_DisturbedWorkingsArea (Value :double); safecall;
    function Get_DisturbedArea : double; safecall;
    procedure Set_DisturbedArea (Value : double); safecall;
    function Get_WaterSurfaceEvapArea : double; safecall;
    procedure Set_WaterSurfaceEvapArea (Value : double); safecall;
    function Get_DisturbedAreaRunoff : double; safecall;
    procedure Set_DisturbedAreaRunoff (Value : double); safecall;
    function Get_DisturbedWorkingsAreaRunoff  : double; safecall;
    procedure Set_DisturbedWorkingsAreaRunoff (Value : double); safecall;
    function Get_DecantVolume : double; safecall;
    procedure Set_DecantVolume (Value : double); safecall;
    function Get_SeepageVolume : double; safecall;
    procedure Set_SeepageVolume (Value : double); safecall;
    function Get_AnalysisStartVolume : double; safecall;
    procedure Set_AnalysisStartVolume (Value : double); safecall;
    function Get_MaximumSeepageRate : double; safecall;
    procedure Set_MaximumSeepageRate (Value : double); safecall;
    function Get_SeepageExponent : double; safecall;
    procedure Set_SeepageExponent (Value : double); safecall;
    function Get_PCDSurfaceArea : double; safecall;
    procedure Set_PCDSurfaceArea (Value : double); safecall;
    function Get_PCDStorageCapacity : double; safecall;
    procedure Set_PCDStorageCapacity (Value : double); safecall;
    function Get_PCDAnalysisStartVolume : double; safecall;
    procedure Set_PCDAnalysisStartVolume (Value : double); safecall;
    function Get_DisturbedRechargeFactor(AIndex: integer): double; safecall;
    procedure Set_DisturbedRechargeFactor(AIndex : integer; Value : double); safecall;
    function Get_WorkingAreaRechargeFactor(AIndex: integer): double; safecall;
    procedure Set_WorkingAreaRechargeFactor(AIndex : integer; Value : double); safecall;

    procedure Populate(AMineIdentifier              : integer;
                       AIdentifier                  : integer;
                       APitName                     : WideString;
                       ACoalReserveArea             : double;
                       AWorkingsArea                : double;
                       ADisturbedWorkingsArea       : double;
                       ADisturbedArea               : double;
                       AWaterSurfaceEvapArea        : double;
                       ADisturbedAreaRunoff         : double;
                       ADisturbedWorkingsAreaRunoff : double;
                       ADecantVolume                : double;
                       ASeepageVolume               : double;
                       AAnalysisStartVolume         : double;
                       AMaximumSeepageRate          : double;
                       ASeepageExponent             : double;
                       APCDSurfaceArea              : double;
                       APCDStorageCapacity          : double;
                       APCDAnalysisStartVolume      : double);
    procedure PopulateDisturbedRechargeFactors(ADisturbedRechargeFactors: TMonthlyDoubleArray);
    procedure PopulateWorkingAreaRechargeFactors(AWorkingAreaRechargeFactors: TMonthlyDoubleArray);
    property MineIdentifier              : integer read GetIdentifier;
    property Identifier                  : integer read Get_Identifier;
    property PitName                     : WideString  read Get_PitName               write Set_PitName;
    property CoalReserveArea             : double  read Get_CoalReserveArea       write Set_CoalReserveArea;
    property WorkingsArea                : double  read Get_WorkingsArea          write Set_WorkingsArea;
    property DisturbedWorkingsArea       : double  read Get_DisturbedWorkingsArea write Set_DisturbedWorkingsArea;
    property DisturbedArea               : double  read Get_DisturbedArea         write Set_DisturbedArea;
    property WaterSurfaceEvapArea        : double  read Get_WaterSurfaceEvapArea  write Set_WaterSurfaceEvapArea;
    property DisturbedAreaRunoff         : double  read Get_DisturbedAreaRunoff   write Set_DisturbedAreaRunoff;
    property DecantVolume                : double  read Get_DecantVolume          write Set_DecantVolume;
    property SeepageVolume               : double  read Get_SeepageVolume         write Set_SeepageVolume;
    property AnalysisStartVolume         : double  read Get_AnalysisStartVolume   write Set_AnalysisStartVolume;
    property MaximumSeepageRate          : double  read Get_MaximumSeepageRate    write Set_MaximumSeepageRate;
    property SeepageExponent             : double  read Get_SeepageExponent       write Set_SeepageExponent;
    property PCDSurfaceArea              : double  read Get_PCDSurfaceArea        write Set_PCDSurfaceArea;
    property PCDStorageCapacity          : double  read Get_PCDStorageCapacity    write Set_PCDStorageCapacity;
    property PCDAnalysisStartVolume      : double  read Get_PCDAnalysisStartVolume write Set_PCDAnalysisStartVolume;
    property DisturbedWorkingsAreaRunoff : double  read Get_DisturbedWorkingsAreaRunoff write Set_DisturbedWorkingsAreaRunoff;
    property DisturbedRechargeFactor[AIndex: Integer]: Double read Get_DisturbedRechargeFactor write Set_DisturbedRechargeFactor;
    property WorkingAreaRechargeFactor[AIndex: Integer]: Double read Get_WorkingAreaRechargeFactor write Set_WorkingAreaRechargeFactor;
  end;

  TUnderground       = class (TAbstractAppObject,IUnderground)
  protected
    FMineIdentifier                   : integer;
    FIdentifier                       : integer;
    FUndergroundSectionName           : WideString;
    FChannelNumberToUGDam             : integer;
    FUpstreamCatchmentArea            : double;
    FBoardPillarCatchmentArea         : double;
    FHighExtractionCatchmentArea      : double;
    FHighExtractionAreaRunoffFactor   : double;
    FUpstreamRunoffPortions           : TMonthlyDoubleArray;
    FBoardAndPilarRechargeFactors     : TMonthlyDoubleArray;
    FHighExtractionRechargeFactors    : TMonthlyDoubleArray;

    function ValidateUndergroundSectionName(AErrorMessages: TStrings)  : Boolean;
    function ValidateChannelNumberToUGDam(AErrorMessages: TStrings)  : Boolean;
    function ValidateUpstreamCatchmentArea(AErrorMessages: TStrings)  : Boolean;
    function ValidateBoardPillarCatchmentArea(AErrorMessages: TStrings)  : Boolean;
    function ValidateHighExtractionCatchmentArea(AErrorMessages: TStrings)  : Boolean;
    function ValidateHighExtractionAreaRunoffFactor(AErrorMessages: TStrings)  : Boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ASource : TUnderground);virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetMineIdentifier : integer;
    function Get_Identifier : integer; safecall;
    function Get_UndergroundSectionName : WideString; safecall;
    procedure Set_UndergroundSectionName (const Value : WideString); safecall;
    function Get_ChannelNumberToUGDam : integer; safecall;
    procedure Set_ChannelNumberToUGDam (Value : integer); safecall;
    function Get_UpstreamCatchmentArea : double; safecall;
    procedure Set_UpstreamCatchmentArea (Value : double); safecall;
    function Get_BoardPillarCatchmentArea : double; safecall;
    procedure Set_BoardPillarCatchmentArea (Value : Double); safecall;
    function Get_HighExtractionCatchmentArea : double; safecall;
    procedure Set_HighExtractionCatchmentArea (Value : Double); safecall;
    function Get_HighExtractionAreaRunoffFactor : double; safecall;
    procedure Set_HighExtractionAreaRunoffFactor (Value : Double); safecall;

    function Get_UpstreamRunoffPortion(AIndex: integer): double; safecall;
    procedure Set_UpstreamRunoffPortion(AIndex : integer; Value : double); safecall;
    function Get_BoardAndPilarRechargeFactor(AIndex: integer): double; safecall;
    procedure Set_BoardAndPilarRechargeFactor(AIndex : integer; Value : double); safecall;
    function Get_HighExtractionRechargeFactor(AIndex: integer): double; safecall;
    procedure Set_HighExtractionRechargeFactor(AIndex : integer; Value : double); safecall;
    function Get_ChannelToUnderGroundDam   : IGeneralFlowChannel; safecall;
    function Get_UndergroundDam    : IReservoirData; safecall;
    function Get_UndergroundDamNodeNumber: integer; safecall;
    function Get_ChannelToUnderGroundDamChannellNumber: integer; safecall;

    procedure Populate(AMineIdentifier : integer;AIdentifier : integer;
                       AUndergroundSectionName           : WideString;
                       AChannelNumberToUGDam             : integer;
                       AUpstreamCatchmentArea            : double;
                       ABoardPillarCatchmentArea         : double;
                       AHighExtractionCatchmentArea      : double;
                       AHighExtractionAreaRunoffFactor   : double);
    procedure PopulateUpstreamRunoffPortions(AUpstreamRunoffPortions: TMonthlyDoubleArray);
    procedure PopulateBoardAndPilarRechargeFactors(ABoardAndPilarRechargeFactors: TMonthlyDoubleArray);
    procedure PopulateHighExtractionRechargeFactors(AHighExtractionRechargeFactors: TMonthlyDoubleArray);

    property MineIdentifier : integer read GetMineIdentifier;
    property Identifier : integer read Get_Identifier;
    property UndergroundSectionName : WideString         read Get_UndergroundSectionName         write Set_UndergroundSectionName;
    property ChannelNumberToUGDam : integer          read Get_ChannelNumberToUGDam           write Set_ChannelNumberToUGDam;
    property UpstreamCatchmentArea : double          read Get_UpstreamCatchmentArea          write Set_UpstreamCatchmentArea;
    property BoardPillarCatchmentArea : double       read Get_BoardPillarCatchmentArea       write Set_BoardPillarCatchmentArea;
    property HighExtractionCatchmentArea : double    read Get_HighExtractionCatchmentArea    write Set_HighExtractionCatchmentArea;
    property HighExtractionAreaRunoffFactor : double read Get_HighExtractionAreaRunoffFactor write Set_HighExtractionAreaRunoffFactor;
    property UpstreamRunoffPortion[AIndex: Integer]: Double read Get_UpstreamRunoffPortion   write Set_UpstreamRunoffPortion;
    property BoardAndPilarRechargeFactor[AIndex: Integer]: Double read Get_BoardAndPilarRechargeFactor   write Set_BoardAndPilarRechargeFactor;
    property HighExtractionRechargeFactor[AIndex: Integer]: Double read Get_HighExtractionRechargeFactor write Set_HighExtractionRechargeFactor;
    property ChannelToUnderGroundDam  : IGeneralFlowChannel read Get_ChannelToUnderGroundDam;
    property UndergroundDam  : IReservoirData read Get_UndergroundDam;
    property UndergroundDamNodeNumber: integer read Get_UndergroundDamNodeNumber;
    property ChannelToUnderGroundDamChannellNumber: integer read Get_ChannelToUnderGroundDamChannellNumber;
  end;

  TSlurryDump = class(TAbstractAppObject,ISlurryDump)
  protected
    FMineIdentifier            : integer;
    FIdentifier                : integer;
    FDumpName                  : WideString;
    FDumpSurfaceArea           : double;
    FRunoffFactorToPCD         : double;
    FSeepageSplitFactor        : double;
    FPCDStorageCapacity        : double;
    FPCDSurfaceArea            : double;
    FPCDAnalysisStartVolume    : double;
    FRechargeFactor            : TMonthlyDoubleArray;

    function ValidateDumpName(AErrorMessages: TStrings)  : Boolean;
    function ValidateDumpSurfaceArea(AErrorMessages: TStrings)  : Boolean;
    function ValidateRunoffFactorToPCD(AErrorMessages: TStrings)  : Boolean;
    function ValidateSeepageSplitFactor(AErrorMessages: TStrings)  : Boolean;
    function ValidatePCDStorageCapacity(AErrorMessages: TStrings)  : Boolean;
    function ValidatePCDSurfaceArea(AErrorMessages: TStrings)  : Boolean;
    function ValidatePCDAnalysisStartVolume(AErrorMessages: TStrings)  : Boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ASource: TSlurryDump); virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetMineIdentifier : integer;
    function Get_Identifier : integer; safecall;
    function Get_DumpName : WideString; safecall;
    procedure Set_DumpName (const Value : WideString); safecall;
    function Get_DumpSurfaceArea : double; safecall;
    procedure Set_DumpSurfaceArea (Value : double); safecall;
    function Get_RunoffFactorToPCD : double; safecall;
    procedure Set_RunoffFactorToPCD (Value : double); safecall;
    function Get_SeepageSplitFactor : double; safecall;
    procedure Set_SeepageSplitFactor (Value : double); safecall;
    function Get_PCDStorageCapacity : double; safecall;
    procedure Set_PCDStorageCapacity (Value : double); safecall;
    function Get_PCDSurfaceArea : double; safecall;
    procedure Set_PCDSurfaceArea (Value : double); safecall;
    function Get_PCDAnalysisStartVolume : double; safecall;
    procedure Set_PCDAnalysisStartVolume (Value : double); safecall;

    function Get_RechargeFactor(AIndex: integer): double; safecall;
    procedure Set_RechargeFactor(AIndex : integer; Value : double); safecall;

    procedure Populate(AMineIdentifier: integer; AIdentifier: integer; ADumpName : WideString;
                       ADumpSurfaceArea: double; ARunoffFactorToPCD: double; ASeepageSplitFactor: double;
                       APCDStorageCapacity: double;APCDSurfaceArea : double; APCDAnalysisStartVolume : double);
    procedure PopulateRechargeFactors(ARechargeFactors   : TMonthlyDoubleArray);

    property MineIdentifier            : integer     read GetMineIdentifier;
    property Identifier                : integer     read Get_Identifier;
    property DumpName                  : WideString  read Get_DumpName               write Set_DumpName;
    property DumpSurfaceArea           : double      read Get_DumpSurfaceArea        Write Set_DumpSurfaceArea;
    property RunoffFactorToPCD         : double      read Get_RunoffFactorToPCD      write Set_RunoffFactorToPCD;
    property SeepageSplitFactor        : double      read Get_SeepageSplitFactor     write Set_SeepageSplitFactor;
    property PCDStorageCapacity        : double      read Get_PCDStorageCapacity     write Set_PCDStorageCapacity;
    property PCDSurfaceArea            : double      read Get_PCDSurfaceArea         write Set_PCDSurfaceArea;
    property PCDAnalysisStartVolume    : double      read Get_PCDAnalysisStartVolume write Set_PCDAnalysisStartVolume;
    property RechargeFactor[AIndex: Integer]: double read Get_RechargeFactor         write Set_RechargeFactor;
  end;

  TMineSubCatchment = class(TAbstractAppObject, IMineSubCatchment)
  protected
    FIdentifier                   : Integer;
    FCatchmentReferenceNr         : Integer;
    FCatchmentRefName             : string;
    FProportionAntecedentFlows    : Double;
    FGroundwaterFlowVolume        : Double;
    FAntecedentRunoffDecayFactor  : Double;
    FCatchmentRefUsed             : boolean;
    FMinimunGroundwaterFlowVolume : TMonthlyDoubleArray;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ASource : TMineSubCatchment);virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    procedure Populate(AIdentifier                  : integer;
                       ACatchmentReferenceNr        : integer;
                       ACatchmentRefName            : string;
                       AProportionAntecedentFlow    : double;
                       AGroundwaterFlowVolume       : double;
                       AAntecedentRunoffDecayFactor : double;
                       ACatchmentRefUsed            : boolean);
    procedure PopulateSome(AIdentifier,ACatchmentReferenceNr: integer;
                           ACatchmentRefUsed: boolean;ACatchmentRefName: string);
    procedure PopulateMinimunGroundwaterFlowVolume(AGroundwaterFlowVolume: TMonthlyDoubleArray);
    function Get_Identifier: Integer; safecall;
    function Get_CatchmentReferenceNr: Integer; safecall;
    procedure Set_CatchmentReferenceNr(Value: Integer); safecall;
    function Get_CatchmentRefName: WideString; safecall;
    procedure Set_CatchmentRefName(const Value: WideString); safecall;
    function Get_CatchmentRefUsed: WordBool; safecall;
    procedure Set_CatchmentRefUsed(Value: WordBool); safecall;
    function Get_MinimunGroundwaterFlowVolume(AIndex: Integer): Double; safecall;
    procedure Set_MinimunGroundwaterFlowVolume(AIndex: Integer; Value: Double); safecall;
    function Get_ProportionAntecedentFlows: Double; safecall;
    procedure Set_ProportionAntecedentFlows(Value: Double); safecall;
    function Get_GroundwaterFlowVolume: Double; safecall;
    procedure Set_GroundwaterFlowVolume(Value: Double); safecall;
    function Get_AntecedentRunoffDecayFactor: Double; safecall;
    procedure Set_AntecedentRunoffDecayFactor(Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property Identifier: Integer read Get_Identifier;
    property CatchmentReferenceNr: Integer read Get_CatchmentReferenceNr write Set_CatchmentReferenceNr;
    property CatchmentRefName: WideString read Get_CatchmentRefName write Set_CatchmentRefName;
    property CatchmentRefUsed: WordBool read Get_CatchmentRefUsed write Set_CatchmentRefUsed;
    property MinimunGroundwaterFlowVolume[AIndex: Integer]: Double read Get_MinimunGroundwaterFlowVolume write Set_MinimunGroundwaterFlowVolume;
    property ProportionAntecedentFlows: Double read Get_ProportionAntecedentFlows write Set_ProportionAntecedentFlows;
    property GroundwaterFlowVolume: Double read Get_GroundwaterFlowVolume write Set_GroundwaterFlowVolume;
    property AntecedentRunoffDecayFactor: Double read Get_AntecedentRunoffDecayFactor write Set_AntecedentRunoffDecayFactor;
  end;

  TMineSubCatchmentList = class(TAbstractAppObject,IMineSubCatchmentList)
  protected
    FMineSubCatchmentContainer : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function  CheckMineSubCatchmentExist(ACatchmentRefNr: integer): boolean;
  public
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function NewMineSubCatchment: TMineSubCatchment;
    function CreateMineSubCatchment(ACatchmentRefNr: Integer): IMineSubCatchment; safecall;
    procedure ValidateMineCatchmentRefInUse(ACatchmentRefNr: Integer);

    function Get_MineSubCatchmentCount: Integer; safecall;
    function Get_MineSubCatchmentByRefNodeNr(ARefNodeNr: Integer): IMineSubCatchment; safecall;
    function Get_MineSubCatchmentByIdentifier(AIdentifier: Integer): IMineSubCatchment; safecall;
    function Get_MineSubCatchmentByIndex(AIndex: Integer): IMineSubCatchment; safecall;

    function Get_CastMineSubCatchmentByIndex(AIndex: Integer): TMineSubCatchment;
    function Get_CastMineSubCatchmentByIdentifier(AIdentifier: Integer): TMineSubCatchment;
    function Get_CastMineSubCatchmentByRefNodeNr(ARefNodeNr: Integer): TMineSubCatchment;
    function GetCatchmentRefNameByRefNumber(ARefNum : integer): string;

    property MineSubCatchmentCount: Integer read Get_MineSubCatchmentCount;
    property MineSubCatchmentByRefNodeNr[ARefNodeNr: Integer]: IMineSubCatchment read Get_MineSubCatchmentByRefNodeNr;
    property MineSubCatchmentByIdentifier[AIdentifier: Integer]: IMineSubCatchment read Get_MineSubCatchmentByIdentifier;
    property MineSubCatchmentByIndex[AIndex: Integer]: IMineSubCatchment read Get_MineSubCatchmentByIndex;

    property CastMineSubCatchmentByIndex[AIndex: Integer]: TMineSubCatchment read Get_CastMineSubCatchmentByIndex;
    property CastMineSubCatchmentByRefNodeNr[ARefNodeNr: Integer]: TMineSubCatchment read Get_CastMineSubCatchmentByRefNodeNr;

 end;

  TMine = class(TAbstractAppObject,IMine)
  protected
    FOpenCastObjectContainer      : TObjectList;
    FUnderGroundObjectContainer   : TObjectList;
    FSlurryDumpObjectContainer    : TObjectList;

    FIdentifier                   : Integer;
    FNodeNumber                   : integer;
    FMineName                     : WideString;
    FRiverChannelNumber           : integer;
    FPCDChannelNumber             : integer;
    FHydrologyNodeNumber          : integer;
    FBeneficiationPlantArea       : double;
    FBeneficiationRunoffFactor    : double;
    FPanEvaporations              : TMonthlyDoubleArray;
    FLakeEvaporations             : TMonthlyDoubleArray;

    function ValidateMineName(AErrorMessages: TStrings)  : Boolean;
    function ValidateRiverChannelNumber(AErrorMessages: TStrings)  : Boolean;
    function ValidatePCDChannelNumber(AErrorMessages: TStrings)  : Boolean;
    function ValidateHydrologyNodeNumber(AErrorMessages: TStrings)  : Boolean;
    function ValidateBeneficiationPlantArea(AErrorMessages: TStrings)  : Boolean;
    function ValidateBeneficiationRunoffFactor(AErrorMessages: TStrings)  : Boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateMineSubCatchment(ACatchmentRef:Integer);
  public
    function Get_CatchmentRefNrByReservoirID(AReservoirID: Integer): Integer; safecall;
    procedure Assign(ASource : TMine);virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    procedure Populate(AIdentifier                  : integer;
                       ANodeNumber                  : integer;
                       AMineName                    : WideString;
                       ARiverChannelNumber          : integer;
                       APCDChannelNumber            : integer;
                       AHydrologyNodeNumber         : integer;
                       ABeneficiationPlantArea      : double;
                       ABeneficiationRunoffFactor   : double);
    procedure PopulatePanEvaporations(APanEvaporations: TMonthlyDoubleArray);
    procedure PopulateLakeEvaporations(ALakeEvaporations: TMonthlyDoubleArray);
    function NewOpenCast    : TOpenCast;
    function NewUnderground : TUnderground;
    function NewSlurryDump  : TSlurryDump;
    function CreateOpenCast : IOpenCast; safecall;
    function RemoveOpenCast(AOpenCastID : integer) : WordBool; safecall;
    function CreateUnderGround : IUnderground; safecall;
    function RemoveUnderGround(AUndergroundID : integer) : WordBool; safecall;
    function CreateSlurryDump : ISlurryDump; safecall;
    function RemoveSlurryDump(ASlurryDumpID : integer) : WordBool; safecall;
    function CreatePolutionControlDam    : IReservoirData;
    function RemovePolutionControlDam    : WordBool;

    function Get_Identifier : integer; safecall;
    function Get_NodeNumber : integer; safecall;
    function Get_MineName : WideString; safecall;
    procedure Set_MineName (const Value : WideString); safecall;
    function Get_RiverChannelNumber : integer; safecall;
    //procedure Set_RiverChannelNumber (Value : integer); safecall;
    function Get_PCDChannelNumber : integer; safecall;
    procedure Set_PCDChannelNumber (Value : integer); safecall;
    function Get_HydrologyNodeNumber : integer; safecall;
    procedure Set_HydrologyNodeNumber (Value : integer); safecall;
    function Get_BeneficiationPlantArea : double; safecall;
    procedure Set_BeneficiationPlantArea (Value : double); safecall;
    function Get_BeneficiationRunoffFactor : double; safecall;
    procedure Set_BeneficiationRunoffFactor (Value : double); safecall;
    function Get_PanEvaporation(AIndex: integer): double; safecall;
    procedure Set_PanEvaporation(AIndex : integer; Value : double); safecall;
    function Get_LakeEvaporation(AIndex: integer): double; safecall;
    procedure Set_LakeEvaporation(AIndex : integer; Value : double); safecall;
    function Get_PCDNodelNumber : integer; safecall;

    function Get_OpenCastCount: integer; safecall;
    function Get_UndergroundCount: integer; safecall;
    function Get_SlurryDumpCount: integer; safecall;

    function Get_RiverChannel          : IGeneralFlowChannel; safecall;
    function Get_PCDChannel            : IGeneralFlowChannel; safecall;
    function Get_HydrologyNode         : IReservoirData; safecall;
    function Get_PolutionControlDam    : IReservoirData; safecall;
    function Get_RiverNode             : IReservoirData; safecall;
    function Get_MineNode              : IReservoirData; safecall;

    function Get_OpenCastByIndex(AIndex: integer)  : IOpenCast;  safecall;
    function Get_OpenCastByIdentifier(AIdentifier: integer)  : IOpenCast; safecall;
    function Get_UnderGroundByIndex(AIndex: integer): IUnderground;  safecall;
    function Get_UnderGroundByIdentifier(AIdentifier: integer): IUnderground; safecall;
    function Get_SlurryDumpByIndex(AIndex: integer): ISlurryDump; safecall;
    function Get_SlurryDumpByIdentifier(AIdentifier: integer): ISlurryDump; safecall;

    function Get_CastOpenCastByIndex(AIndex: integer)  : TOpenCast;
    function Get_CastUnderGroundByIndex(AIndex: integer): TUnderground;
    function Get_CastSlurryDumpByIndex(AIndex: integer): TSlurryDump;
    function Get_CastOpenCastByID(AIdentifier: integer)  : TOpenCast;
    function Get_CastUnderGroundByID(AIdentifier: integer): TUnderground;
    function Get_CastSlurryDumpByID(AIdentifier: integer): TSlurryDump;

    function Get_OpenCastIdentifierByIndex(AIndex: integer): integer;
    function Get_UnderGroundIdentifierByIndex(AIndex: integer): integer;
    function Get_SlurryDumpIdentifierByIndex(AIndex: integer): integer;

    function Get_PolutionControlDamExists: WordBool; safecall;
    procedure Set_PolutionControlDamExists(Value: WordBool); safecall;

    property Identifier          : integer read Get_Identifier;
    property NodeNumber          : integer read Get_NodeNumber;
    property MineName            : WideString  read Get_MineName          write Set_MineName;
    property RiverChannelNumber  : integer read Get_RiverChannelNumber;
    property PCDChannelNumber    : integer read Get_PCDChannelNumber;
    property HydrologyNodeNumber : integer read Get_HydrologyNodeNumber   write Set_HydrologyNodeNumber;
    property PCDNodelNumber      : integer read Get_PCDNodelNumber;
    property BeneficiationPlantArea : double read Get_BeneficiationPlantArea write Set_BeneficiationPlantArea;
    property BeneficiationRunoffFactor : double  read Get_BeneficiationRunoffFactor write Set_BeneficiationRunoffFactor;
    property PanEvaporation[AIndex: Integer]  : Double read Get_PanEvaporation write Set_PanEvaporation;
    property LakeEvaporation[AIndex: Integer] : Double read Get_LakeEvaporation write Set_LakeEvaporation;
    property OpenCastCount     : integer read Get_OpenCastCount;
    property UndergroundCount  : integer read Get_UndergroundCount;
    property SlurryDumpCount   : integer read Get_SlurryDumpCount ;

    property OpenCastByIndex[AIndex: integer] :IOpenCast read Get_OpenCastByIndex;
    property OpenCastByIdentifier[AIdentifier: integer]   :IOpenCast read Get_OpenCastByIndex;
    property UnderGroundByIndex[AIndex: integer] :IUnderground read Get_UnderGroundByIndex;
    property SlurryDumpByIndex[AIndex: integer] :ISlurryDump read Get_SlurryDumpByIndex;

    property RiverChannel       : IGeneralFlowChannel read Get_RiverChannel;
    property PCDChannel         : IGeneralFlowChannel read Get_PCDChannel;
    property HydrologyNode      : IReservoirData      read Get_HydrologyNode;
    property RiverNode          : IReservoirData      read Get_RiverNode;
    property PolutionControlDam : IReservoirData      read Get_PolutionControlDam;
    property MineNode           : IReservoirData      read Get_MineNode;

    property CastOpenCastByIndex[AIndex: integer]     : TOpenCast    read Get_CastOpenCastByIndex;
    property CastUnderGroundByIndex[AIndex: integer]  : TUnderground read Get_CastUnderGroundByIndex;
    property CastSlurryDumpByIndex[AIndex: integer]   : TSlurryDump  read Get_CastSlurryDumpByIndex;
    property CastOpenCastByID[AIdentifier: integer]   : TOpenCast    read Get_CastOpenCastByID;
    property CastUnderGroundByID[AIdentifier: integer]: TUnderground read Get_CastUnderGroundByID;
    property CastSlurryDumpByID[AIdentifier: integer] : TSlurryDump  read Get_CastSlurryDumpByID;

    property OpenCastIdentifierByIndex[AIndex: integer]    : integer read Get_OpenCastIdentifierByIndex;
    property UnderGroundIdentifierByIndex[AIndex: integer] : integer read Get_UnderGroundIdentifierByIndex;
    property SlurryDumpIdentifierByIndex[AIndex: integer]  : integer read Get_SlurryDumpIdentifierByIndex;
    property CatchmentRefNrByReservoirID[AReservoirID: Integer]: Integer read Get_CatchmentRefNrByReservoirID;
    property PolutionControlDamExists: WordBool read Get_PolutionControlDamExists write Set_PolutionControlDamExists;
  end;

  TMineList = class(TAbstractAppObject,IMineList)
  protected
    FMinesObjectContainer    : TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_MineCount : integer; safecall;
    function Get_MineByIdentifier(AMineID: integer): IMine; safecall;
    function Get_MineByIndex(AIndex: integer): IMine; safecall;
    function Get_MineByNodeNumber(ANodeNumber: Integer): IMine; safecall;
    function Get_MineByPCDNumber(APCDNumber: Integer): IMine; safecall;

    function Get_CastMineByID(AMineID: Integer): TMine;
    function Get_CastMineByIndex(AIndex : Integer): TMine;
    function Get_CastMineByNodeNumber(ANodeNumber : Integer): TMine;
    function Get_CastMineByPCDNumber(APCDNumber : Integer): TMine;
    function Get_ChannelToUnderGroundDamByDamNumber(AReservoirNumber: integer):IGeneralFlowChannel;safecall;
  public
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Initialise : boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function NewMine : TMine;
    function CreateMine : IMine; safecall;
    function CopyCreate(AMineNumber : integer) : IMine; safecall;
    function RemoveMine(AMineNumber : integer) : WordBool; safecall;
    function GetNodeNumberFromIdentifier(AMineID: Integer): integer;

    property MineCount : Integer read Get_MineCount;
    property MineByIndex[AIndex: Integer]:            IMine read Get_MineByIndex;
    property MineByID[AMineID: Integer]:              IMine read Get_MineByIdentifier;
    property MineByNodeNumber[AMineNumber: Integer]:  IMine read Get_MineByNodeNumber;
    property MineByPCDNumber[AMineNumber: Integer]:   IMine read Get_MineByPCDNumber;
    property ChannelToUnderGroundDamByDamNumber[AReservoirNumber: Integer]:   IGeneralFlowChannel read Get_ChannelToUnderGroundDamByDamNumber;

    property CastMineByIndex[AIndex: Integer]:            TMine read Get_CastMineByIndex;
    property CastMineByID[AMineID: Integer]:              TMine read Get_CastMineByID;
    property CastMineByNodeNumber[AMineNumber: Integer]:  TMine read Get_CastMineByNodeNumber;
  end;

implementation

uses
  System.Types,
  SysUtils,
  USystemModelManager,
  UMineSQLAgent,
  UYieldModelDataObject,
  UReservoirData,
  UParameterData,
  UPlanningMineData,
  UPlanningMineSQLAgent,
  UErrorHandlingOperations,
  UPlanningModelDataObject,
  Math, UNetworkFeaturesData, UNetworkElementData;

{OpenCast}

procedure TOpenCast.CreateMemberObjects;
const OPNAME = 'TOpenCast.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.DestroyMemberObjects;
const OPNAME = 'TOpenCast.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast._AddRef: Integer;
const OPNAME = 'TOpenCast._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast._Release: Integer;
const OPNAME = 'TOpenCast._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Initialise: boolean;
const OPNAME = 'TOpenCast.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FMineIdentifier              := 0;
    FIdentifier                  := 0;
    FPitName                     := '';
    FCoalReserveArea             := 0.0;
    FWorkingsArea                := 0.0;
    FDisturbedWorkingsArea       := 0.0;
    FDisturbedArea               := 0.0;
    FWaterSurfaceEvapArea        := 0.0;
    FDisturbedAreaRunoff         := 0.0;
    FDisturbedWorkingsAreaRunoff := 0.0;
    FDecantVolume                := 0.0;
    FSeepageVolume               := 0.0;
    FAnalysisStartVolume         := 0.0;
    FMaximumSeepageRate          := 0.0;
    FSeepageExponent             := 0.0;
    FPCDSurfaceArea              := 0.0;
    FPCDStorageCapacity          := 0.0;
    FPCDAnalysisStartVolume      := 0.0;
    for LIndex := Low(FDisturbedRechargeFactors) to High(FDisturbedRechargeFactors) do
      FDisturbedRechargeFactors[LIndex] := 0.0;
    for LIndex := Low(FWorkingAreaRechargeFactors) to High(FWorkingAreaRechargeFactors) do
      FWorkingAreaRechargeFactors[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TOpenCast.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'PitName') then
        Result := ValidatePitName(lErrorMessage)
      else
      if (AContext = 'CoalReserveArea') then
        Result := ValidateCoalReserveArea(lErrorMessage)
      else
      if (AContext = 'WorkingsArea') then
        Result := ValidateWorkingsArea(lErrorMessage)
      else
      if (AContext = 'DisturbedWorkingsArea') then
        Result := ValidateDisturbedWorkingsArea(lErrorMessage)
      else
      if (AContext = 'DisturbedArea') then
        Result := ValidateDisturbedArea(lErrorMessage)
      else
      if (AContext = 'WaterSurfaceEvapArea') then
        Result := ValidateWaterSurfaceEvapArea(lErrorMessage)
      else
      if (AContext = 'DisturbedAreaRunOff') then
        Result := ValidateDisturbedAreaRunOff(lErrorMessage)
      else
      if (AContext = 'DisturbedWorkingsAreaRunOff') then
        Result := ValidateDisturbedWorkingsAreaRunOff(lErrorMessage)
      else
      if (AContext = 'DecantVolume') then
        Result := ValidateDecantVolume(lErrorMessage)
      else
      if (AContext = 'SeepageVolume') then
        Result := ValidateSeepageVolume(lErrorMessage)
      else
      if (AContext = 'AnalysisStartVolume') then
        Result := ValidateAnalysisStartVolume(lErrorMessage)
      else
      if (AContext = 'MaximumSeepageRate') then
        Result := ValidateMaximumSeepageRate(lErrorMessage)
      else
      if (AContext = 'SeepageExponent') then
        Result := ValidateSeepageExponent(lErrorMessage)
      else
      if (AContext = 'OpenCastPCDSurfaceArea') then
        Result := ValidatePCDSurfaceArea(lErrorMessage)
      else
      if (AContext = 'OpenCastPCDStorageCapacity') then
        Result := ValidatePCDStorageCapacity(lErrorMessage)
      else
      if (AContext = 'OpenCastPCDAnalysisStartVolume') then
        Result := ValidatePCDAnalysisStartVolume(lErrorMessage)
      else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidatePitName(lErrorMessage))           then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateCoalReserveArea(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateWorkingsArea(lErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateDisturbedWorkingsArea(lErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateDisturbedArea(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateWaterSurfaceEvapArea(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateDisturbedAreaRunoff(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateDisturbedWorkingsAreaRunoff(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateDecantVolume(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateSeepageVolume(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateAnalysisStartVolume(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateMaximumSeepageRate(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateSeepageExponent(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePCDSurfaceArea(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePCDStorageCapacity(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePCDAnalysisStartVolume(lErrorMessage))  then Result := False;
        end;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Populate(AMineIdentifier:Integer;AIdentifier: integer;APitName : WideString; ACoalReserveArea,
                             AWorkingsArea,ADisturbedWorkingsArea, ADisturbedArea,
                             AWaterSurfaceEvapArea,ADisturbedAreaRunoff,
                             ADisturbedWorkingsAreaRunoff, ADecantVolume,
                             ASeepageVolume,AAnalysisStartVolume, AMaximumSeepageRate,
                             ASeepageExponent,APCDSurfaceArea, APCDStorageCapacity,
                             APCDAnalysisStartVolume : double);
const OPNAME = 'TOpenCast.Populate';
begin
  try
    FMineIdentifier              := AMineIdentifier;
    FIdentifier                  := AIdentifier;
    FPitName                     := APitName;
    FCoalReserveArea             := ACoalReserveArea;
    FWorkingsArea                := AWorkingsArea;
    FDisturbedWorkingsArea       := ADisturbedWorkingsArea;
    FDisturbedArea               := ADisturbedArea;
    FWaterSurfaceEvapArea        := AWaterSurfaceEvapArea;
    FDisturbedAreaRunoff         := ADisturbedAreaRunoff;
    FDisturbedWorkingsAreaRunoff := ADisturbedWorkingsAreaRunoff;
    FDecantVolume                := ADecantVolume;
    FSeepageVolume               := ASeepageVolume;
    FAnalysisStartVolume         := AAnalysisStartVolume;
    FMaximumSeepageRate          := AMaximumSeepageRate;
    FSeepageExponent             := ASeepageExponent;
    FPCDSurfaceArea              := APCDSurfaceArea;
    FPCDStorageCapacity          := APCDStorageCapacity;
    FPCDAnalysisStartVolume      := APCDAnalysisStartVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.PopulateDisturbedRechargeFactors(ADisturbedRechargeFactors: TMonthlyDoubleArray);
const OPNAME = 'TOpenCast.PopulateDisturbedRechargeFactors';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FDisturbedRechargeFactors) to High(FDisturbedRechargeFactors) do
        FDisturbedRechargeFactors[LIndex] := ADisturbedRechargeFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.PopulateWorkingAreaRechargeFactors(AWorkingAreaRechargeFactors: TMonthlyDoubleArray);
const OPNAME = 'TOpenCast.PopulateWorkingAreaRechargeFactors';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FWorkingAreaRechargeFactors) to High(FWorkingAreaRechargeFactors) do
        FWorkingAreaRechargeFactors[LIndex] := AWorkingAreaRechargeFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.GetIdentifier: integer;
const OPNAME = 'TOpenCast.GetIdentifier';
begin
  Result := 0;
  try
    Result := FMineIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_Identifier: integer;
const OPNAME = 'TOpenCast.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_AnalysisStartVolume: double;
const OPNAME = 'TOpenCast.Get_AnalysisStartVolume';
begin
  Result := NullFloat;
  try
    Result := FAnalysisStartVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_CoalReserveArea: double;
const OPNAME = 'TOpenCast.Get_CoalReserveArea';
begin
  Result := NullFloat;
  try
    Result := FCoalReserveArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_DecantVolume: double;
const OPNAME = 'TOpenCast.Get_DecantVolume';
begin
  Result := NullFloat;
  try
    Result := FDecantVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_DisturbedArea: double;
const OPNAME = 'TOpenCast.Get_DisturbedArea';
begin
  Result := NullFloat;
  try
    Result := FDisturbedArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_DisturbedAreaRunoff: double;
const OPNAME = 'TOpenCast.Get_DisturbedAreaRunoff';
begin
  Result := NullFloat;
  try
    Result := FDisturbedAreaRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_DisturbedWorkingsArea: double;
const OPNAME = 'TOpenCast.Get_DisturbedWorkingsArea';
begin
  Result := NullFloat;
  try
    Result := FDisturbedWorkingsArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_DisturbedRechargeFactor(AIndex: integer): double;
const OPNAME = 'TOpenCast.Get_DisturbedRechargeFactor';
begin
  Result := NullFloat;
  try
    Result := FDisturbedRechargeFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_WorkingAreaRechargeFactor(AIndex: integer): double;
const OPNAME = 'TOpenCast.Get_WorkingAreaRechargeFactor';
begin
  Result := NullFloat;
  try
    Result := FWorkingAreaRechargeFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_PCDStorageCapacity: double;
const OPNAME = 'TOpenCast.Get_PCDStorageCapacity';
begin
  Result := NullFloat;
  try
    Result := FPCDStorageCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_PCDSurfaceArea: double;
const OPNAME = 'TOpenCast.Get_PCDSurfaceArea';
begin
  Result := NullFloat;
  try
    Result := FPCDSurfaceArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_PCDAnalysisStartVolume: double;
const OPNAME = 'TOpenCast.Get_PCDAnalysisStartVolume';
begin
  Result := NullFloat;
  try
    Result := FPCDAnalysisStartVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_PitName: WideString;
const OPNAME = 'TOpenCast.Get_PitName';
begin
  Result := NullString;
  try
    Result := FPitName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_SeepageExponent: double;
const OPNAME = 'TOpenCast.Get_SeepageExponent';
begin
  Result := NullFloat;
  try
    Result := FSeepageExponent;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_MaximumSeepageRate: double;
const OPNAME = 'TOpenCast.Get_MaximumSeepageRate';
begin
  Result := NullFloat;
  try
    Result := FMaximumSeepageRate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_SeepageVolume: double;
const OPNAME = 'TOpenCast.Get_SeepageVolume';
begin
  Result := NullFloat;
  try
    Result := FSeepageVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_WaterSurfaceEvapArea: double;
const OPNAME = 'TOpenCast.Get_WaterSurfaceEvapArea';
begin
  Result := NullFloat;
  try
    Result := FWaterSurfaceEvapArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_WorkingsArea: double;
const OPNAME = 'TOpenCast.Get_WorkingsArea';
begin
  Result := NullFloat;
  try
    Result := FWorkingsArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.Get_DisturbedWorkingsAreaRunoff: double;
const OPNAME = 'TOpenCast.Get_DisturbedWorkingsAreaRunoff';
begin
  Result := NullFloat;
  try
    Result := FDisturbedWorkingsAreaRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_AnalysisStartVolume(Value: double);
const OPNAME = 'TOpenCast.Set_AnalysisStartVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
     begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'AnalysisStartVolume', FloatToStr(Value), FloatToStr(FAnalysisStartVolume), LContextData) then
          begin
            LOldValue            := FAnalysisStartVolume;
            FAnalysisStartVolume := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AnalysisStartVolume',FloatToStr(LOldValue),FloatToStr(FAnalysisStartVolume));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_CoalReserveArea(Value: double);
const OPNAME = 'TOpenCast.Set_CoalReserveArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'CoalReserveArea', FloatToStr(Value), FloatToStr(FCoalReserveArea), LContextData) then
          begin
            LOldValue        := FCoalReserveArea;
            FCoalReserveArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'CoalReserveArea',FloatToStr(LOldValue),FloatToStr(FCoalReserveArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_DecantVolume(Value: double);
const OPNAME = 'TOpenCast.Set_DecantVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DecantVolume', FloatToStr(Value), FloatToStr(FDecantVolume), LContextData) then
          begin
            LOldValue     := FDecantVolume;
            FDecantVolume := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DecantVolume',FloatToStr(LOldValue),FloatToStr(FDecantVolume));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_DisturbedArea(Value: double);
const OPNAME = 'TOpenCast.Set_DisturbedArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DisturbedArea', FloatToStr(Value), FloatToStr(FDisturbedArea), LContextData) then
          begin
            LOldValue         := FDisturbedArea;
            FDisturbedArea    := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DisturbedArea',FloatToStr(LOldValue),FloatToStr(FDisturbedArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_DisturbedAreaRunoff(Value: double);
const OPNAME = 'TOpenCast.Set_DisturbedAreaRunoff';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DisturbedAreaRunOff', FloatToStr(Value), FloatToStr(FDisturbedAreaRunoff), LContextData) then
          begin
            LOldValue             := FDisturbedAreaRunoff;
            FDisturbedAreaRunoff  := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DisturbedAreaRunOff',FloatToStr(LOldValue),FloatToStr(FDisturbedAreaRunoff));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_DisturbedWorkingsArea(Value: double);
const OPNAME = 'TOpenCast.Set_DisturbedWorkingsArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DisturbedWorkingsArea', FloatToStr(Value), FloatToStr(FDisturbedWorkingsArea), LContextData) then
          begin
            LOldValue                 := FDisturbedWorkingsArea;
            FDisturbedWorkingsArea    := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DisturbedWorkingsArea',FloatToStr(LOldValue),FloatToStr(FDisturbedWorkingsArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_DisturbedRechargeFactor(AIndex: integer;Value: double);
const OPNAME = 'TOpenCast.Set_DisturbedRechargeFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_RechargeFactor(LContextData, FMineIdentifier,1,FIdentifier,1,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'RechargeFactors', FloatToStr(Value), FloatToStr(FDisturbedRechargeFactors[AIndex]), LContextData) then
          begin
            LPrevValue := FDisturbedRechargeFactors[AIndex];
            FDisturbedRechargeFactors[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RechargeFactors',FloatToStr(LPrevValue),FloatToStr(FDisturbedRechargeFactors[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpenCast.Set_WorkingAreaRechargeFactor(AIndex: integer;Value: double);
const OPNAME = 'TOpenCast.Set_WorkingAreaRechargeFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_RechargeFactor(LContextData,FMineIdentifier,1,FIdentifier,2,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'RechargeFactors', FloatToStr(Value), FloatToStr(FWorkingAreaRechargeFactors[AIndex]), LContextData) then
          begin
            LPrevValue := FWorkingAreaRechargeFactors[AIndex];
            FWorkingAreaRechargeFactors[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RechargeFactors',FloatToStr(LPrevValue),FloatToStr(FWorkingAreaRechargeFactors[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOpenCast.Set_PCDStorageCapacity(Value: double);
const OPNAME = 'TOpenCast.Set_PCDStorageCapacity';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'OpenCastPCDStorageCapacity', FloatToStr(Value), FloatToStr(FPCDStorageCapacity), LContextData) then
          begin
            LOldValue           := FPCDStorageCapacity;
            FPCDStorageCapacity := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'OpenCastPCDStorageCapacity',FloatToStr(LOldValue),FloatToStr(FPCDStorageCapacity));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_PCDSurfaceArea(Value: double);
const OPNAME = 'TOpenCast.Set_PCDSurfaceArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'OpenCastPCDSurfaceArea', FloatToStr(Value), FloatToStr(FPCDSurfaceArea), LContextData) then
          begin
            LOldValue        := FPCDSurfaceArea;
            FPCDSurfaceArea  := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'OpenCastPCDSurfaceArea',FloatToStr(LOldValue),FloatToStr(FPCDSurfaceArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_PCDAnalysisStartVolume(Value: double);
const OPNAME = 'TOpenCast.Set_PCDAnalysisStartVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'OpenCastPCDAnalysisStartVolume', FloatToStr(Value), FloatToStr(FPCDAnalysisStartVolume), LContextData) then
          begin
            LOldValue               := FPCDAnalysisStartVolume;
            FPCDAnalysisStartVolume := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'OpenCastPCDAnalysisStartVolume',FloatToStr(LOldValue),FloatToStr(FPCDAnalysisStartVolume));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_PitName(const Value: WideString);
const OPNAME = 'TOpenCast.Set_PitName';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'PitName', Value, FPitName, LContextData) then
          begin
            LOldValue        := FPitName;
            FPitName         := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'PitName',LOldValue,FPitName);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_SeepageExponent(Value: double);
const OPNAME = 'TOpenCast.Set_SeepageExponent';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'SeepageExponent', FloatToStr(Value), FloatToStr(FSeepageExponent), LContextData) then
          begin
            LOldValue         := FSeepageExponent;
            FSeepageExponent  := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'SeepageExponent',FloatToStr(LOldValue),FloatToStr(FSeepageExponent));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_MaximumSeepageRate(Value: double);
const OPNAME = 'TOpenCast.Set_MaximumSeepageRate';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaximumSeepageRate', FloatToStr(Value), FloatToStr(FMaximumSeepageRate), LContextData) then
          begin
            LOldValue            := FMaximumSeepageRate;
            FMaximumSeepageRate  := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaximumSeepageRate',FloatToStr(LOldValue),FloatToStr(FMaximumSeepageRate));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_SeepageVolume(Value: double);
const OPNAME = 'TOpenCast.Set_SeepageVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'SeepageVolume', FloatToStr(Value), FloatToStr(FSeepageVolume), LContextData) then
          begin
            LOldValue     := FSeepageVolume;
            FSeepageVolume := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'SeepageVolume',FloatToStr(LOldValue),FloatToStr(FSeepageVolume));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_WaterSurfaceEvapArea(Value: double);
const OPNAME = 'TOpenCast.Set_WaterSurfaceEvapArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'WaterSurfaceEvapArea', FloatToStr(Value), FloatToStr(FWaterSurfaceEvapArea), LContextData) then
          begin
            LOldValue              := FWaterSurfaceEvapArea;
            FWaterSurfaceEvapArea  := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WaterSurfaceEvapArea',FloatToStr(LOldValue),FloatToStr(FWaterSurfaceEvapArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_WorkingsArea(Value: double);
const OPNAME = 'TOpenCast.Set_WorkingsArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'WorkingsArea', FloatToStr(Value), FloatToStr(FWorkingsArea), LContextData) then
          begin
            LOldValue        := FWorkingsArea;
            FWorkingsArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WorkingsArea',FloatToStr(LOldValue),FloatToStr(FWorkingsArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOpenCast.Set_DisturbedWorkingsAreaRunoff(Value: double);
const OPNAME = 'TOpenCast.Set_DisturbedWorkingsAreaRunoff';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DisturbedWorkingsAreaRunOff', FloatToStr(Value), FloatToStr(FDisturbedWorkingsAreaRunoff), LContextData) then
          begin
            LOldValue                     := FDisturbedWorkingsAreaRunoff;
            FDisturbedWorkingsAreaRunoff  := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DisturbedWorkingsAreaRunOff',FloatToStr(LOldValue),FloatToStr(FDisturbedWorkingsAreaRunoff));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{Underground}

procedure TUnderground.CreateMemberObjects;
const OPNAME = 'TUnderground.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.DestroyMemberObjects;
const OPNAME = 'TUnderground.DestroyMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground._AddRef: Integer;
const OPNAME = 'TUnderground._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground._Release: Integer;
const OPNAME = 'TUnderground._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TUnderground.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'UndergroundSectionName') then
        Result := ValidateUndergroundSectionName(lErrorMessage)
      else
      if (AContext = 'ChannelNumberToUGDam') then
        Result := ValidateChannelNumberToUGDam(lErrorMessage)
      else
      if (AContext = 'UpstreamCatchmentArea') then
        Result := ValidateUpstreamCatchmentArea(lErrorMessage)
      else
      if (AContext = 'BoardPillarCatchmentArea') then
        Result := ValidateBoardPillarCatchmentArea(lErrorMessage)
      else
      if (AContext = 'HighExtractionCatchmentArea') then
        Result := ValidateHighExtractionCatchmentArea(lErrorMessage)
      else
      if (AContext = 'HighExtractionAreaRunoffFactor') then
        Result := ValidateHighExtractionAreaRunoffFactor(lErrorMessage)
      else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidateUndergroundSectionName(lErrorMessage))  then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateChannelNumberToUGDam(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateUpstreamCatchmentArea(lErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateBoardPillarCatchmentArea(lErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateHighExtractionCatchmentArea(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateHighExtractionAreaRunoffFactor(lErrorMessage))   then Result := False;
        end;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Initialise: boolean;
const OPNAME = 'TUnderground.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FIdentifier                       := 0;
    FUndergroundSectionName           := '';
    FChannelNumberToUGDam             := 0;
    FUpstreamCatchmentArea            := 0.0;
    FBoardPillarCatchmentArea         := 0.0;
    FHighExtractionCatchmentArea      := 0.0;
    FHighExtractionAreaRunoffFactor   := 0.0;
    for LIndex := Low(FUpstreamRunoffPortions) to High(FUpstreamRunoffPortions) do
      FUpstreamRunoffPortions[LIndex] := 0.0;
    for LIndex := Low(FBoardAndPilarRechargeFactors) to High(FBoardAndPilarRechargeFactors) do
      FBoardAndPilarRechargeFactors[LIndex] := 0.0;
    for LIndex := Low(FHighExtractionRechargeFactors) to High(FHighExtractionRechargeFactors) do
      FHighExtractionRechargeFactors[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Populate(AMineIdentifier: integer; AIdentifier: integer;AUndergroundSectionName: WideString;
                                AChannelNumberToUGDam : integer; AUpstreamCatchmentArea,
                                ABoardPillarCatchmentArea, AHighExtractionCatchmentArea,
                                AHighExtractionAreaRunoffFactor : double);
const OPNAME = 'TUnderground.Populate';
begin
  try
    FMineIdentifier                 := AMineIdentifier;
    FIdentifier                     := AIdentifier;
    FUndergroundSectionName         := AUndergroundSectionName;
    FChannelNumberToUGDam           := AChannelNumberToUGDam;
    FUpstreamCatchmentArea          := AUpstreamCatchmentArea;
    FBoardPillarCatchmentArea       := ABoardPillarCatchmentArea;
    FHighExtractionCatchmentArea    := AHighExtractionCatchmentArea;
    FHighExtractionAreaRunoffFactor := AHighExtractionAreaRunoffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.PopulateBoardAndPilarRechargeFactors(ABoardAndPilarRechargeFactors: TMonthlyDoubleArray);
var
  LIndex: integer;
const OPNAME = 'TUnderground.PopulateBoardAndPilarRechargeFactors';
begin
  try
    for LIndex := Low(FBoardAndPilarRechargeFactors) to High(FBoardAndPilarRechargeFactors) do
        FBoardAndPilarRechargeFactors[LIndex] := ABoardAndPilarRechargeFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.PopulateHighExtractionRechargeFactors(AHighExtractionRechargeFactors: TMonthlyDoubleArray);
const OPNAME = 'TUnderground.PopulateHighExtractionRechargeFactors';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FHighExtractionRechargeFactors) to High(FHighExtractionRechargeFactors) do
        FHighExtractionRechargeFactors[LIndex] := AHighExtractionRechargeFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.PopulateUpstreamRunoffPortions(AUpstreamRunoffPortions: TMonthlyDoubleArray);
const OPNAME = 'TUnderground.PopulateUpstreamRunoffPortions';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FUpstreamRunoffPortions) to High(FUpstreamRunoffPortions) do
        FUpstreamRunoffPortions[LIndex] := AUpstreamRunoffPortions[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.GetMineIdentifier: integer;
const OPNAME = 'TUnderground.GetMineIdentifier';
begin
  Result := 0;
  try
    Result := FMineIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_Identifier: integer;
const OPNAME = 'TUnderground.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_BoardPillarCatchmentArea: double;
const OPNAME = 'TUnderground.Get_BoardPillarCatchmentArea';
begin
  Result := NullFloat;
  try
    Result := FBoardPillarCatchmentArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_UpstreamCatchmentArea: double;
const OPNAME = 'TUnderground.Get_UpstreamCatchmentArea';
begin
  Result := NullFloat;
  try
    Result := FUpstreamCatchmentArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_ChannelNumberToUGDam : integer;
const OPNAME = 'TUnderground.Get_ChannelNumberToUGDam';
begin
  Result := NullInteger;
  try
    Result := FChannelNumberToUGDam;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_HighExtractionCatchmentArea: double;
const OPNAME = 'TUnderground.Get_HighExtractionCatchmentArea';
begin
  Result := NullFloat;
  try
    Result := FHighExtractionCatchmentArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_HighExtractionAreaRunoffFactor: double;
const OPNAME = 'TUnderground.Get_HighExtractionAreaRunoffFactor';
begin
  Result := NullFloat;
  try
    Result := FHighExtractionAreaRunoffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_BoardAndPilarRechargeFactor(AIndex: integer): double;
const OPNAME = 'TUnderground.Get_BoardAndPilarRechargeFactor';
begin
  Result := NullFloat;
  try
    Result := FBoardAndPilarRechargeFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_HighExtractionRechargeFactor(AIndex: integer): double;
const OPNAME = 'TUnderground.Get_HighExtractionRechargeFactor';
begin
  Result := NullFloat;
  try
    Result := FHighExtractionRechargeFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_UpstreamRunoffPortion(AIndex: integer): double;
const OPNAME = 'TUnderground.Get_UpstreamRunoffPortion';
begin
  Result := NullFloat;
  try
    Result := FUpstreamRunoffPortions[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_UndergroundSectionName: WideString;
const OPNAME = 'TUnderground.Get_UndergroundSectionName';
begin
  Result := NullString;
  try
    Result := FUndergroundSectionName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_BoardPillarCatchmentArea(Value: Double);
const OPNAME = 'TUnderground.Set_BoardPillarCatchmentArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_Underground(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'BoardPillarCatchmentArea', FloatToStr(Value), FloatToStr(FBoardPillarCatchmentArea), LContextData) then
          begin
            LOldValue                 := FBoardPillarCatchmentArea;
            FBoardPillarCatchmentArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'BoardPillarCatchmentArea',FloatToStr(LOldValue),FloatToStr(FBoardPillarCatchmentArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_UpstreamCatchmentArea(Value: double);
const OPNAME = 'TUnderground.Set_UpstreamCatchmentArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_Underground(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'UpstreamCatchmentArea', FloatToStr(Value), FloatToStr(FUpstreamCatchmentArea), LContextData) then
          begin
            LOldValue               := FUpstreamCatchmentArea;
            FUpstreamCatchmentArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'UpstreamCatchmentArea',FloatToStr(LOldValue),FloatToStr(FUpstreamCatchmentArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_ChannelNumberToUGDam(Value: integer);
const OPNAME = 'TUnderground.Set_ChannelNumberToUGDam';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_Underground(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ChannelNumberToUGDam', IntToStr(Value), IntToStr(FChannelNumberToUGDam), LContextData) then
          begin
            LOldValue             := FChannelNumberToUGDam;
            FChannelNumberToUGDam := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelNumberToUGDam',IntToStr(LOldValue),IntToStr(FChannelNumberToUGDam));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_HighExtractionCatchmentArea(Value: Double);
const OPNAME = 'TUnderground.Set_HighExtractionCatchmentArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_Underground(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'HighExtractionCatchmentArea', FloatToStr(Value), FloatToStr(FHighExtractionCatchmentArea), LContextData) then
          begin
            LOldValue                    := FHighExtractionCatchmentArea;
            FHighExtractionCatchmentArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'HighExtractionCatchmentArea',FloatToStr(LOldValue),FloatToStr(FHighExtractionCatchmentArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_HighExtractionAreaRunoffFactor(Value: Double);
const OPNAME = 'TUnderground.Set_HighExtractionAreaRunoffFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_Underground(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'HighExtractionAreaRunoffFactor', FloatToStr(Value), FloatToStr(FHighExtractionAreaRunoffFactor), LContextData) then
          begin
            LOldValue                       := FHighExtractionAreaRunoffFactor;
            FHighExtractionAreaRunoffFactor := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'HighExtractionAreaRunoffFactor',FloatToStr(LOldValue),FloatToStr(FHighExtractionAreaRunoffFactor));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_UndergroundSectionName(const Value: WideString);
const OPNAME = 'TUnderground.Set_UndergroundSectionName';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_Underground(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'UndergroundSectionName', Value, FUndergroundSectionName, LContextData) then
          begin
            LOldValue               := FUndergroundSectionName;
            FUndergroundSectionName := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'UndergroundSectionName',LOldValue,FUndergroundSectionName);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TUnderground.Set_BoardAndPilarRechargeFactor(AIndex: integer;Value: double);
const OPNAME = 'TUnderground.Set_BoardAndPilarRechargeFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_RechargeFactor(LContextData, FMineIdentifier,2,FIdentifier,3,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'RechargeFactors', FloatToStr(Value), FloatToStr(FBoardAndPilarRechargeFactors[AIndex]), LContextData) then
          begin
            LPrevValue := FBoardAndPilarRechargeFactors[AIndex];
            FBoardAndPilarRechargeFactors[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RechargeFactors',FloatToStr(LPrevValue),FloatToStr(FBoardAndPilarRechargeFactors[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUnderground.Set_HighExtractionRechargeFactor(AIndex: integer;Value: double);
const OPNAME = 'TUnderground.Set_HighExtractionRechargeFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_RechargeFactor(LContextData, FMineIdentifier,2,FIdentifier,4,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'RechargeFactors', FloatToStr(Value), FloatToStr(FHighExtractionRechargeFactors[AIndex]), LContextData) then
          begin
            LPrevValue := FHighExtractionRechargeFactors[AIndex];
            FHighExtractionRechargeFactors[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RechargeFactors',FloatToStr(LPrevValue),FloatToStr(FHighExtractionRechargeFactors[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUnderground.Set_UpstreamRunoffPortion(AIndex: integer; Value: double);
const OPNAME = 'TUnderground.Set_UpstreamRunoffPortion';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_UGUpstreamRunoff(LContextData, FMineIdentifier,FIdentifier,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'MineUGUpstreamRunoff', FloatToStr(Value), FloatToStr(FUpstreamRunoffPortions[AIndex]), LContextData) then
          begin
            LPrevValue := FUpstreamRunoffPortions[AIndex];
            FUpstreamRunoffPortions[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MineUGUpstreamRunoff',FloatToStr(LPrevValue),FloatToStr(FUpstreamRunoffPortions[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUnderground.Get_ChannelToUnderGroundDam: IGeneralFlowChannel;
const OPNAME = 'TUnderground.Get_ChannelToUnderGroundDam';
begin
  Result := nil;
  try

    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumberToUGDam];

    if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumberToUGDam];

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_UndergroundDam: IReservoirData;
const OPNAME = 'TUnderground.Get_UndergroundDam';
var
  LUndergroundChannel : IGeneralFlowChannel;
begin
  Result := nil;
  try
    LUndergroundChannel := ChannelToUnderGroundDam;
    if(LUndergroundChannel <> nil) then
      Result := LUndergroundChannel.DownStreamNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_ChannelToUnderGroundDamChannellNumber: integer;
const OPNAME = 'TUnderground.Get_ChannelToUnderGroundDamChannellNumber';
var
  LUndergroundChannel : IGeneralFlowChannel;
begin
  Result := 0;
  try
    LUndergroundChannel := ChannelToUnderGroundDam;
    if(LUndergroundChannel <> nil) then
      Result := LUndergroundChannel.ChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.Get_UndergroundDamNodeNumber: integer;
const OPNAME = 'TUnderground.Get_UndergroundDamNodeNumber';
var
  LUndergroundDam : IReservoirData;
begin
  Result := 0;
  try
     LUndergroundDam:= UndergroundDam;
    if(LUndergroundDam <> nil) then
      Result := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{SlurryDump}

procedure TSlurryDump.CreateMemberObjects;
const OPNAME = 'TSlurryDump.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.DestroyMemberObjects;
const OPNAME = 'TSlurryDump.DestroyMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

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

function TSlurryDump.Initialise: boolean;
const OPNAME = 'TSlurryDump.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FMineIdentifier         := 0;
    FIdentifier             := 0;
    FDumpName               := '';
    FDumpSurfaceArea        := 0.0;
    FRunoffFactorToPCD      := 0.0;
    FSeepageSplitFactor     := 0.0;
    FPCDStorageCapacity     := 0.0;
    FPCDSurfaceArea         := 0.0;
    FPCDAnalysisStartVolume := 0.0;
    for LIndex := Low(FRechargeFactor) to High(FRechargeFactor) do
      FRechargeFactor[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Populate(AMineIdentifier : integer;AIdentifier : integer; ADumpName : WideString;
                               ADumpSurfaceArea, ARunoffFactorToPCD, ASeepageSplitFactor,
                               APCDStorageCapacity, APCDSurfaceArea,
                               APCDAnalysisStartVolume : double);
const OPNAME = 'TSlurryDump.Populate';
begin
  try
    FMineIdentifier          := AMineIdentifier;
    FIdentifier              := AIdentifier;
    FDumpName                := ADumpName;
    FDumpSurfaceArea         := ADumpSurfaceArea;
    FRunoffFactorToPCD       := ARunoffFactorToPCD;
    FSeepageSplitFactor      := ASeepageSplitFactor;
    FPCDStorageCapacity      := APCDStorageCapacity;
    FPCDSurfaceArea          := APCDSurfaceArea;
    FPCDAnalysisStartVolume  := APCDAnalysisStartVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.PopulateRechargeFactors(ARechargeFactors: TMonthlyDoubleArray);
const OPNAME = 'TSlurryDump.PopulateRechargeFactors';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FRechargeFactor) to High(FRechargeFactor) do
        FRechargeFactor[LIndex] := ARechargeFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TSlurryDump.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TSlurryDump.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'DumpName') then
        Result := ValidateDumpName(lErrorMessage)
      else
      if (AContext = 'DumpSurfaceArea') then
        Result := ValidateDumpSurfaceArea(lErrorMessage)
      else
      if (AContext = 'RunoffFactorToPCD') then
        Result := ValidateRunoffFactorToPCD(lErrorMessage)
      else
      if (AContext = 'SeepageSplitFactor') then
        Result := ValidateSeepageSplitFactor(lErrorMessage)
      else
      if (AContext = 'DumpPCDStorageCapacity') then
        Result := ValidatePCDStorageCapacity(lErrorMessage)
      else
      if (AContext = 'DumpPCDSurfaceArea') then
        Result := ValidatePCDSurfaceArea(lErrorMessage)
      else
      if (AContext = 'DumpPCDAnalysisStartVolume') then
        Result := ValidatePCDAnalysisStartVolume(lErrorMessage)
      else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidateDumpName(lErrorMessage))  then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateDumpSurfaceArea(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateRunoffFactorToPCD(lErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateSeepageSplitFactor(lErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePCDStorageCapacity(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePCDSurfaceArea(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePCDAnalysisStartVolume(lErrorMessage))   then Result := False;
        end;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.GetMineIdentifier: integer;
const OPNAME = 'TSlurryDump.GetMineIdentifier';
begin
  Result := 0;
  try
    Result := FMineIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_Identifier: integer;
const OPNAME = 'TSlurryDump.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_DumpName: WideString;
const OPNAME = 'TSlurryDump.Get_DumpName';
begin
  Result := NullString;
  try
    Result := FDumpName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_RechargeFactor(AIndex: integer): double;
const OPNAME = 'TSlurryDump.Get_RechargeFactor';
begin
  Result := NullFloat;
  try
    Result := FRechargeFactor[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_PCDAnalysisStartVolume: double;
const OPNAME = 'TSlurryDump.Get_PCDAnalysisStartVolume';
begin
  Result := NullFloat;
  try
    Result := FPCDAnalysisStartVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_PCDStorageCapacity: double;
const OPNAME = 'TSlurryDump.Get_PCDStorageCapacity';
begin
  Result := NullFloat;
  try
    Result := FPCDStorageCapacity;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_PCDSurfaceArea: double;
const OPNAME = 'TSlurryDump.Get_PCDSurfaceArea';
begin
  Result := NullFloat;
  try
    Result := FPCDSurfaceArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_RunoffFactorToPCD: double;
const OPNAME = 'TSlurryDump.Get_RunoffFactorToPCD';
begin
  Result := NullFloat;
  try
    Result := FRunoffFactorToPCD;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_SeepageSplitFactor: double;
const OPNAME = 'TSlurryDump.Get_SeepageSplitFactor';
begin
  Result := NullFloat;
  try
    Result := FSeepageSplitFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.Get_DumpSurfaceArea: double;
const OPNAME = 'TSlurryDump.Get_DumpSurfaceArea';
begin
  Result := NullFloat;
  try
    Result := FDumpSurfaceArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TSlurryDump.Set_DumpName(const Value: WideString);
const OPNAME = 'TSlurryDump.Set_DumpName';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DumpName', Value, FDumpName, LContextData) then
          begin
            LOldValue := FDumpName;
            FDumpName := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DumpName',LOldValue,FDumpName);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_RechargeFactor(AIndex: integer;Value: double);
const OPNAME = 'TSlurryDump.Set_RechargeFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_RechargeFactor(LContextData, FMineIdentifier,3,FIdentifier,5,IntToStr(AIndex));
          if FAppModules.FieldProperties.UpdateFieldValue(
            'RechargeFactors', FloatToStr(Value), FloatToStr(FRechargeFactor[AIndex]), LContextData) then
          begin
            LPrevValue := FRechargeFactor[AIndex];
            FRechargeFactor[AIndex] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RechargeFactors',FloatToStr(LPrevValue),FloatToStr(FRechargeFactor[AIndex]));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSlurryDump.Set_PCDAnalysisStartVolume(Value: double);
const OPNAME = 'TSlurryDump.Set_PCDAnalysisStartVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DumpPCDAnalysisStartVolume', FloatToStr(Value), FloatToStr(FPCDAnalysisStartVolume), LContextData) then
          begin
            LOldValue               := FPCDAnalysisStartVolume;
            FPCDAnalysisStartVolume := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DumpPCDAnalysisStartVolume',FloatToStr(LOldValue),FloatToStr(FPCDAnalysisStartVolume));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_PCDStorageCapacity(Value: double);
const OPNAME = 'TSlurryDump.Set_PCDStorageCapacity';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DumpPCDStorageCapacity', FloatToStr(Value), FloatToStr(FPCDStorageCapacity), LContextData) then
          begin
            LOldValue           := FPCDStorageCapacity;
            FPCDStorageCapacity := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DumpPCDStorageCapacity',FloatToStr(LOldValue),FloatToStr(FPCDStorageCapacity));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_PCDSurfaceArea(Value: double);
const OPNAME = 'TSlurryDump.Set_PCDSurfaceArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DumpPCDSurfaceArea', FloatToStr(Value), FloatToStr(FPCDSurfaceArea), LContextData) then
          begin
            LOldValue       := FPCDSurfaceArea;
            FPCDSurfaceArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DumpPCDSurfaceArea',FloatToStr(LOldValue),FloatToStr(FPCDSurfaceArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_RunoffFactorToPCD(Value: double);
const OPNAME = 'TSlurryDump.Set_RunoffFactorToPCD';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'RunoffFactorToPCD', FloatToStr(Value), FloatToStr(FRunoffFactorToPCD), LContextData) then
          begin
            LOldValue          := FRunoffFactorToPCD;
            FRunoffFactorToPCD := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RunoffFactorToPCD',FloatToStr(LOldValue),FloatToStr(FRunoffFactorToPCD));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_SeepageSplitFactor(Value: double);
const OPNAME = 'TSlurryDump.Set_SeepageSplitFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'SeepageSplitFactor', FloatToStr(Value), FloatToStr(FSeepageSplitFactor), LContextData) then
          begin
            LOldValue           := FSeepageSplitFactor;
            FSeepageSplitFactor := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'SeepageSplitFactor',FloatToStr(LOldValue),FloatToStr(FSeepageSplitFactor));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSlurryDump.Set_DumpSurfaceArea(Value: double);
const OPNAME = 'TSlurryDump.Set_DumpSurfaceArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DumpSurfaceArea', FloatToStr(Value), FloatToStr(FDumpSurfaceArea), LContextData) then
          begin
            LOldValue        := FDumpSurfaceArea;
            FDumpSurfaceArea := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DumpSurfaceArea',FloatToStr(LOldValue),FloatToStr(FDumpSurfaceArea));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TMine }

procedure TMine.CreateMemberObjects;
const OPNAME = 'TMine.CreateMemberObjects';
begin
  inherited;
  try
    FOpenCastObjectContainer    := TObjectList.Create(True);
    FUnderGroundObjectContainer := TObjectList.Create(True);
    FSlurryDumpObjectContainer  := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.DestroyMemberObjects;
const OPNAME = 'TMine.DestroyMemberObjects';
begin
  try
    inherited;
    FOpenCastObjectContainer.Clear;
    FUnderGroundObjectContainer.Clear;
    FSlurryDumpObjectContainer.Clear;
    FOpenCastObjectContainer.Free;
    FUnderGroundObjectContainer.Free;
    FSlurryDumpObjectContainer.Free;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine._AddRef: Integer;
const OPNAME = 'TMine._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine._Release: Integer;
const OPNAME = 'TMine._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Initialise: boolean;
const OPNAME = 'TMine.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FOpenCastObjectContainer.Clear;
    FUnderGroundObjectContainer.Clear;
    FSlurryDumpObjectContainer.Clear;

    FIdentifier                  := 0;
    FNodeNumber                  := 0;
    FMineName                    := '';
    FRiverChannelNumber          := 0;
    FPCDChannelNumber            := 0;
    FHydrologyNodeNumber         := 0;
    FBeneficiationPlantArea      := 0.0;
    FBeneficiationRunoffFactor   := 0.0;
    for LIndex := Low(FPanEvaporations) to High(FPanEvaporations) do
      FPanEvaporations[LIndex] := 0.0;
    for LIndex := Low(FLakeEvaporations) to High(FLakeEvaporations) do
      FLakeEvaporations[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TMine.Validate';
var
  LIndex : integer;
begin
  Result := True;
  try
    for LIndex := 0 to FOpenCastObjectContainer.Count-1 do
    begin
      if not TOpenCast(FOpenCastObjectContainer.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
    end;
    for LIndex := 0 to FUnderGroundObjectContainer.Count-1 do
    begin
      if not TUnderground(FUnderGroundObjectContainer.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
    end;
    for LIndex := 0 to FSlurryDumpObjectContainer.Count-1 do
    begin
      if not TSlurryDump(FSlurryDumpObjectContainer.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_Identifier: integer;
const OPNAME = 'TMine.Get_Identifier';
begin
  Result := NullInteger;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_NodeNumber: integer;
const OPNAME = 'TMine.Get_NodeNumber';
begin
  Result := NullInteger;
  try
    Result := FNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_BeneficiationRunoffFactor: double;
const OPNAME = 'TMine.Get_BeneficiationRunoffFactor';
begin
  Result := NullFloat;
  try
    Result := FBeneficiationRunoffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_BeneficiationRunoffFactor(Value: double);
const OPNAME = 'TMine.Set_BeneficiationRunoffFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'BeneficiationRunOffFactor', FloatToStr(Value), FloatToStr(FBeneficiationRunoffFactor), LContextData) then
        begin
          LOldValue := FBeneficiationRunoffFactor;
          FBeneficiationRunoffFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'BeneficiationRunOffFactor',FloatToStr(LOldValue),FloatToStr(FBeneficiationRunoffFactor));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_PCDChannelNumber: integer;
const OPNAME = 'TMine.Get_PCDChannelNumber';
begin
  Result := NullInteger;
  try
    Result := FPCDChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_PCDChannelNumber(Value: integer);
const OPNAME = 'TMine.Set_PCDChannelNumber';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'PCDChannelNumber', IntToStr(Value), IntToStr(FPCDChannelNumber), LContextData) then
        begin
          LOldValue := FPCDChannelNumber;
          FPCDChannelNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PCDChannelNumber',IntToStr(LOldValue),IntToStr(FPCDChannelNumber));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_RiverChannelNumber: integer;
const OPNAME = 'TMine.Get_RiverChannelNumber';
begin
  Result := NullInteger;
  try
    Result := FRiverChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{procedure TMine.Set_RiverChannelNumber(Value: integer);
const OPNAME = 'TMine.Set_RiverChannelNumber';
begin
  try
    FRiverChannelNumber := Value;
    //DSR  complete
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TMine.Get_BeneficiationPlantArea: double;
const OPNAME = 'TMine.Get_BeneficiationPlantArea';
begin
  Result := NullFloat;
  try
    Result := FBeneficiationPlantArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_BeneficiationPlantArea(Value: double);
const OPNAME = 'TMine.Set_BeneficiationPlantArea';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'BeneficiationPlantArea', FloatToStr(Value), FloatToStr(FBeneficiationPlantArea), LContextData) then
        begin
          LOldValue := FBeneficiationPlantArea;
          FBeneficiationPlantArea := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'BeneficiationPlantArea',FloatToStr(LOldValue),FloatToStr(FBeneficiationPlantArea));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_MineName: WideString;
const OPNAME = 'TMine.Get_MineName';
begin
  Result := NullString;
  try
    Result := FMineName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_MineName(const Value: WideString);
const OPNAME = 'TMine.Set_MineName';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MineName', Value, FMineName, LContextData) then
        begin
          LOldValue := FMineName;
          FMineName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MineName',LOldValue,Value);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_HydrologyNodeNumber: integer;
const OPNAME = 'TMine.Get_HydrologyNodeNumber';
begin
  Result := NullInteger;
  try
    Result := FHydrologyNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_HydrologyNodeNumber(Value: integer);
const OPNAME = 'TMine.Set_HydrologyNodeNumber';
var
  LOldValue             : integer;
  LLoadAgent            : TMineSQLAgent;
  LContextData          : TStringList;
  LCatchmentRef         : integer;
  LReservoirData        : IReservoirData;
  LReservoirDataList    : IReservoirDataList;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'HydrologyNodeNumber', IntToStr(Value), IntToStr(FHydrologyNodeNumber), LContextData) then
        begin
          LOldValue := FHydrologyNodeNumber;
          FHydrologyNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'HydrologyNodeNumber',IntToStr(LOldValue),IntToStr(FHydrologyNodeNumber));

          LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                                .ReservoirList;
          LReservoirData := LReservoirDataList.ReservoirOrNodeByIdentifier[FHydrologyNodeNumber];
          if LReservoirData <> nil then
          begin
            LCatchmentRef := LReservoirData.ReservoirConfigurationData.CatchmentRef;
            if LCatchmentRef > 0 then
              CreateMineSubCatchment(LCatchmentRef);
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_PanEvaporation(AIndex: integer): double;
const OPNAME = 'TMine.Get_PanEvaporation';
begin
  Result := NullFloat;
  try
    Result := FPanEvaporations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_PanEvaporation(AIndex: integer; Value: double);
const OPNAME = 'TMine.Set_PanEvaporation';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineEvaporation(LContextData,FIdentifier,IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'MinePanEvaporationFactors', FloatToStr(Value), FloatToStr(FPanEvaporations[AIndex]), LContextData) then
        begin
          LPrevValue := FPanEvaporations[AIndex];
          FPanEvaporations[AIndex] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinePanEvaporationFactors',FloatToStr(LPrevValue),FloatToStr(FPanEvaporations[AIndex]));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMine.Get_LakeEvaporation(AIndex: integer): double;
const OPNAME = 'TMine.Get_LakeEvaporation';
begin
  Result := NullFloat;
  try
     Result := FLakeEvaporations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_LakeEvaporation(AIndex: integer; Value: double);
const OPNAME = 'TMine.Set_LakeEvaporation';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineEvaporation(LContextData,FIdentifier,IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'MineLakeEvaporationFactors', FloatToStr(Value), FloatToStr(FLakeEvaporations[AIndex]), LContextData) then
        begin
          LPrevValue := FLakeEvaporations[AIndex];
          FLakeEvaporations[AIndex] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MineLakeEvaporationFactors',FloatToStr(LPrevValue),FloatToStr(FLakeEvaporations[AIndex]));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMine.Get_OpenCastCount: integer;
const OPNAME = 'TMine.Get_OpenCastCount';
begin
  Result :=  0;
  try
    Result :=  FOpenCastObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_UndergroundCount: integer;
const OPNAME = 'TMine.Get_UndergroundCount';
begin
  Result := 0;
  try
    Result := FUnderGroundObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_SlurryDumpCount: integer;
const OPNAME = 'TMine.Get_SlurryDumpCount';
begin
  Result := 0;
  try
    Result := FSlurryDumpObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_CastOpenCastByIndex(AIndex: integer): TOpenCast;
const OPNAME = 'TMine.Get_CastOpenCastByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FOpenCastObjectContainer.Count) then
      Result := TOpenCast(FOpenCastObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_CastOpenCastByID(AIdentifier: integer): TOpenCast;
const OPNAME = 'TMine.Get_CastOpenCastByID';
var
  LIndex    : Integer;
  LOpenCast : TOpenCast;
begin
  Result := nil;
  try
    for LIndex := 0 to  FOpenCastObjectContainer.Count-1 do
    begin
      LOpenCast := TOpenCast(FOpenCastObjectContainer.Items[LIndex]);
      if (LOpenCast.Identifier = AIdentifier) then
      begin
        Result := LOpenCast;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_CastUnderGroundByIndex(AIndex: integer): TUnderground;
const OPNAME = 'TMine.Get_CastUnderGroundByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FUnderGroundObjectContainer.Count) then
      Result := TUnderground(FUnderGroundObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_CastUnderGroundByID(AIdentifier: integer): TUnderground;
const OPNAME = 'TMine.Get_CastUnderGroundByID';
var
  LIndex    : Integer;
  LUGMining : TUnderground;
begin
  Result := nil;
  try
    for LIndex := 0 to FUnderGroundObjectContainer.Count-1 do
    begin
      LUGMining := TUnderground(FUnderGroundObjectContainer.Items[LIndex]);
      if (LUGMining.Identifier = AIdentifier) then
      begin
        Result := LUGMining;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_CastSlurryDumpByIndex(AIndex: integer): TSlurryDump;
const OPNAME = 'TMine.Get_CastSlurryDumpByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FSlurryDumpObjectContainer.Count) then
      Result := TSlurryDump(FSlurryDumpObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_CastSlurryDumpByID(AIdentifier: integer): TSlurryDump;
const OPNAME = 'TMine.Get_CastSlurryDumpByID';
var
  LIndex      : Integer;
  LSlurryDump : TSlurryDump;
begin
  Result := nil;
  try
    for LIndex := 0 to FSlurryDumpObjectContainer.Count-1 do
    begin
      LSlurryDump := TSlurryDump(FSlurryDumpObjectContainer.Items[LIndex]);
      if (LSlurryDump.Identifier = AIdentifier) then
      begin
        Result := LSlurryDump;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_UnderGroundByIdentifier(AIdentifier: integer): IUnderground;
const OPNAME = 'TMine.Get_UnderGroundByIdentifier';
begin
  Result := nil;
  try
    Result := Get_CastUnderGroundByID(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_UnderGroundByIndex(AIndex: integer): IUnderground;
const OPNAME = 'TMine.Get_UnderGroundByIndex';
begin
  Result := nil;
  try
    Result := Get_CastUnderGroundByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_SlurryDumpByIdentifier(AIdentifier: integer): ISlurryDump;
const OPNAME = 'TMine.Get_SlurryDumpByIdentifier';
begin
  Result := nil;
  try
    Result := Get_CastSlurryDumpByID(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_SlurryDumpByIndex(AIndex: integer): ISlurryDump;
const OPNAME = 'TMine.Get_SlurryDumpByIndex';
begin
  Result := nil;
  try
    Result := Get_CastSlurryDumpByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_OpenCastByIndex(AIndex: integer): IOpenCast;
const OPNAME = 'TMine.Get_OpenCastByIndex';
begin
  Result := nil;
  try
    Result := Get_CastOpenCastByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_OpenCastByIdentifier(AIdentifier: integer): IOpenCast;
const OPNAME = 'TMine.Get_OpenCastByIdentifier';
begin
  Result := nil;
  try
    Result := Get_CastOpenCastByID(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.NewOpenCast: TOpenCast;
const OPNAME = 'TMine.NewOpenCast';
begin
  Result := Nil;
  try
    Result := TOpenCast.Create(FAppModules);
    FOpenCastObjectContainer.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.NewUnderground: TUnderground;
const OPNAME = 'TMine.NewUnderground';
begin
  Result := Nil;
  try
    Result := TUnderground.Create(FAppModules);
    FUnderGroundObjectContainer.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.NewSlurryDump: TSlurryDump;
const OPNAME = 'TMine.NewSlurryDump';
begin
  Result := Nil;
  try
    Result := TSlurryDump.Create(FAppModules);
    FSlurryDumpObjectContainer.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.CreateOpenCast: IOpenCast;
const OPNAME = 'TMine.CreateOpenCast';
var
  LSQLAgent   : TMineSQLAgent;
  LOpencast   : TOpenCast;
begin
  Result := Nil;
  try
    if (FOpenCastObjectContainer.Count >= 10) then
      Exit;

    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LOpencast := NewOpenCast;
      LOpencast.FMineIdentifier := FIdentifier;
      LOpencast.FIdentifier     := LSQLAgent.GetMaxOpenCastID+1;;
      LOpencast.FPitName        := FMineName + ' - Opencast Pit '+IntToStr(LOpencast.FIdentifier);
      if LSQLAgent.AddOpenCast(LOpencast) then
        Result := LOpencast
      else
        FOpenCastObjectContainer.Remove(LOpencast);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.CreateUnderGround: IUnderground;
const OPNAME = 'TMine.CreateUnderGround';
var
  LIdentifier      : integer;
  LSQLAgent        : TMineSQLAgent;
  LUnderground     : TUnderground;
  LMineNode,
  LUndergroundDam  : IReservoirData;
  LMinMaxFeature   : IMinMaxFlowConstraint;
  LChannelToUndergroundDam : IGeneralFlowChannel;
  LContinue                : boolean;
begin
  Result := Nil;
  try
    if (FUnderGroundObjectContainer.Count >= 10) then
      Exit;

    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LUndergroundDam  := nil;
      LMinMaxFeature   := nil;
      LChannelToUndergroundDam := nil;

      LMineNode := MineNode;
      LContinue := (LMineNode <> nil);

      //Create UndergroundDam
      if LContinue  then
      begin
        LUndergroundDam := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateReservoir(ntMineUndergroundDam);
        LContinue := (LUndergroundDam <> nil)
      end;

      //Create channel to UndergroundDam
      if LContinue  then
      begin
        LIdentifier := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
        LUndergroundDam.ReservoirConfigurationData.ReservoirName := IntToStr(LIdentifier) + ' - '+ FMineName +' U/G Dam' ;
        LChannelToUndergroundDam := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
        LContinue := (LChannelToUndergroundDam <> nil);
      end;

      //Create channel to PCD min-max feature
      if LContinue  then
      begin
        LMinMaxFeature :=  (FAppModules.Model as IYieldModel).DoCreateMinMaxFlowFeature(LChannelToUndergroundDam.ChannelNumber);
        LChannelToUndergroundDam.ChannelType          := ctMineToUndergroundChannel;
        //hannelToUndergroundDam.UpStreamNodeNumber := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        LChannelToUndergroundDam.UpStreamNodeNumber := 0;
        LChannelToUndergroundDam.DownStreamNodeNumber := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
        LChannelToUndergroundDam.ChannelName          := IntToStr(LChannelToUndergroundDam.ChannelNumber)+ ' - '+ FMineName + ' To '+
                                                          LUndergroundDam.ReservoirConfigurationData.ReservoirName;
        LContinue := (LMinMaxFeature <> nil);
      end;

      //Create channel to underground section
      if LContinue  then
      begin
        LUnderground := NewUnderground;
        LUnderground.FMineIdentifier := FIdentifier;
        LUnderground.FIdentifier     := LSQLAgent.GetMaxUndergroundID+1;
        LUnderground.FChannelNumberToUGDam     := LChannelToUndergroundDam.ChannelNumber;
        LUnderground.FUndergroundSectionName   := FMineName + ' - Underground Section '+IntToStr(LUnderground.FIdentifier);
        LContinue :=  LSQLAgent.AddUnderground(LUnderground);
        if LContinue then
          Result := LUnderground
        else
          FUndergroundObjectContainer.Remove(LUnderground);
      end;

      if not LContinue then
      begin
        if (LUndergroundDam  <> nil) then
        begin
          LIdentifier      := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
          LUndergroundDam  := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LIdentifier);
        end;

        if (LChannelToUndergroundDam  <> nil) then
        begin
          LIdentifier := LChannelToUndergroundDam.ChannelNumber;
          LChannelToUndergroundDam := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.CreateSlurryDump: ISlurryDump;
const OPNAME = 'TMine.CreateSlurryDump';
var
  LSQLAgent    : TMineSQLAgent;
  LSlurryDump : TSlurryDump;
begin
  Result := Nil;
  try
    if (FSlurryDumpObjectContainer.Count >= 10) then
      Exit;

    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LSlurryDump := NewSlurryDump;
      LSlurryDump.FMineIdentifier := FIdentifier;
      LSlurryDump.FIdentifier     := LSQLAgent.GetMaxSlurryDumpID+1;;
      LSlurryDump.FDumpName       := FMineName + ' - Slurry Dump '+IntToStr(LSlurryDump.FIdentifier);
      if LSQLAgent.AddSlurryDump(LSlurryDump) then
        Result := LSlurryDump
      else
        FSlurryDumpObjectContainer.Remove(LSlurryDump);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.RemoveOpenCast(AOpenCastID: integer): WordBool;
const OPNAME = 'TMine.RemoveOpenCast';
var
  LOpenCast: TOpenCast;
  LSQLAgent : TMineSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LOpenCast := Get_CastOpenCastByID(AOpenCastID);
      if(LOpenCast <> nil) then
      begin
        if LSQLAgent.DeleteOpenCast(LOpenCast) then
        begin
          FOpenCastObjectContainer.Remove(LOpenCast);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.RemoveUnderGround(AUndergroundID: integer): WordBool;
const OPNAME = 'TMine.RemoveUnderGround';
var
  LUnderground  : TUnderground;
  LSQLAgent     : TMineSQLAgent;
  LUndergroundDamNodeNumber: integer;
  LChannelToUnderGroundDamChannellNumber: integer;
begin
  Result := False;
  try
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LUnderground := Get_CastUnderGroundByID(AUndergroundID);
      if(LUnderground <> nil) then
      begin
        LUndergroundDamNodeNumber := LUnderground.UndergroundDamNodeNumber;
        if(LUndergroundDamNodeNumber <> 0) then
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LUndergroundDamNodeNumber);
        LChannelToUnderGroundDamChannellNumber := LUnderground.ChannelToUnderGroundDamChannellNumber;
        (FAppModules.Model as IYieldModel).DoConvertChannel(LChannelToUnderGroundDamChannellNumber);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LChannelToUnderGroundDamChannellNumber);

        if LSQLAgent.DeleteUnderground(LUnderground) then
        begin
          FUnderGroundObjectContainer.Remove(LUnderground);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.RemoveSlurryDump(ASlurryDumpID: integer): WordBool;
const OPNAME = 'TMine.RemoveSlurryDump';
var
  LSlurryDump   : TSlurryDump;
  LSQLAgent     : TMineSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LSlurryDump := Get_CastSlurryDumpByID(ASlurryDumpID);
      if(LSlurryDump <> nil) then
      begin
        if LSQLAgent.DeleteSlurryDump(LSlurryDump) then
        begin
          FSlurryDumpObjectContainer.Remove(LSlurryDump);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_HydrologyNode: IReservoirData;
const OPNAME = 'TMine.Get_HydrologyNode';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FHydrologyNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_MineNode: IReservoirData;
const OPNAME = 'TMine.Get_MineNode';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FNodeNumber];
   if FAppModules.StudyArea.ModelCode = CPlanning then
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FNodeNumber];

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_PCDChannel: IGeneralFlowChannel;
const OPNAME = 'TMine.Get_PCDChannel';
begin
  Result := nil;
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[FPCDChannelNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_RiverChannel: IGeneralFlowChannel;
const OPNAME = 'TMine.Get_RiverChannel';
begin
  Result := nil;
  try

    Result := (FAppModules.Model.ModelData as IYieldModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[FRiverChannelNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_PolutionControlDam: IReservoirData;
const OPNAME = 'TMine.Get_PolutionControlDam';
var
  LPCDChannel : IGeneralFlowChannel;
begin
  Result := nil;
  try
    LPCDChannel := PCDChannel;
    if(LPCDChannel <> nil) then
      Result := LPCDChannel.DownStreamNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_RiverNode: IReservoirData;
const OPNAME = 'TMine.Get_RiverNode';
var
  LRiverChannel : IGeneralFlowChannel;
begin
  Result := nil;
  try
    LRiverChannel := RiverChannel;
    if(LRiverChannel <> nil) then
      Result := LRiverChannel.DownStreamNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_PCDNodelNumber: integer;
const OPNAME = 'TMine.Get_PCDNodelNumber';
var
  LReservoirData : IReservoirData;
begin
  Result := NullInteger;
  try
    LReservoirData := PolutionControlDam;
    if(LReservoirData <> nil) then
      Result := LReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Populate(AIdentifier, ANodeNumber: integer; AMineName: WideString;
          ARiverChannelNumber, APCDChannelNumber, AHydrologyNodeNumber: integer;
          ABeneficiationPlantArea, ABeneficiationRunoffFactor: double);
const OPNAME = 'TMine.Populate';
begin
  try
    FIdentifier                  := AIdentifier;
    FNodeNumber                  := ANodeNumber;
    FMineName                    := AMineName;
    FRiverChannelNumber          := ARiverChannelNumber;
    FPCDChannelNumber            := APCDChannelNumber;
    FHydrologyNodeNumber         := AHydrologyNodeNumber;
    FBeneficiationPlantArea      := ABeneficiationPlantArea;
    FBeneficiationRunoffFactor   := ABeneficiationRunoffFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.PopulateLakeEvaporations(ALakeEvaporations: TMonthlyDoubleArray);
const OPNAME = 'TMine.PopulateLakeEvaporations';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FLakeEvaporations) to High(FLakeEvaporations) do
        FLakeEvaporations[LIndex] := ALakeEvaporations[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.PopulatePanEvaporations(APanEvaporations: TMonthlyDoubleArray);
const OPNAME = 'TMine.PopulatePanEvaporations';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FPanEvaporations) to High(FPanEvaporations) do
        FPanEvaporations[LIndex] := APanEvaporations[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.ValidateBeneficiationPlantArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TMine.ValidateBeneficiationPlantArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('BeneficiationPlantArea', FloatToStr(FBeneficiationPlantArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FBeneficiationPlantArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.ValidateBeneficiationRunoffFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TMine.ValidateBeneficiationRunoffFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('BeneficiationRunOffFactor', FloatToStr(FBeneficiationRunoffFactor), lMessage)) then
      AErrorMessages.Add(FloatToStr(FBeneficiationRunoffFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.ValidateHydrologyNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TMine.ValidateHydrologyNodeNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('HydrologyNodeNumber', IntToStr(FHydrologyNodeNumber), lMessage)) then
      AErrorMessages.Add(IntToStr(FHydrologyNodeNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.ValidateMineName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TMine.ValidateMineName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MineName', FMineName, lMessage)) then
      AErrorMessages.Add(FMineName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.ValidatePCDChannelNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TMine.ValidatePCDChannelNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PCDChannelNumber', IntToStr(FPCDChannelNumber), lMessage)) then
      AErrorMessages.Add(IntToStr(FPCDChannelNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.ValidateRiverChannelNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TMine.ValidateRiverChannelNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('RiverChannelNumber', IntToStr(FRiverChannelNumber), lMessage)) then
      AErrorMessages.Add(IntToStr(FRiverChannelNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_OpenCastIdentifierByIndex(AIndex: integer): integer;
const OPNAME = 'TMine.Get_OpenCastIdentifierByIndex';
var
  LOpenCast : IOpenCast;
begin
  Result := 0;
  try
    LOpenCast := OpenCastByIndex[AIndex];
    if(LOpenCast <> nil) then
      Result := LOpenCast.Identifier;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_SlurryDumpIdentifierByIndex(AIndex: integer): integer;
const OPNAME = 'TMine.Get_SlurryDumpIdentifierByIndex';
var
  LSlurryDump : ISlurryDump;
begin
  Result := 0;
  try
    LSlurryDump := SlurryDumpByIndex[AIndex];
    if(LSlurryDump <> nil) then
      Result := LSlurryDump.Identifier;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_UnderGroundIdentifierByIndex(AIndex: integer): integer;
const OPNAME = 'TMine.Get_UnderGroundIdentifierByIndex';
var
  LUnderGround : IUnderGround;
begin
  Result := 0;
  try
    LUnderGround := UnderGroundByIndex[AIndex];
    if(LUnderGround <> nil) then
      Result := LUnderGround.Identifier;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Assign(ASource: TMine);
const OPNAME = 'TMine.Assign';
var
  LIndex : integer;
  LCopyOpenCast : IOpenCast;
  LDestOpenCast : TOpenCast;
  LUnderGroundCopy : IUnderground;
  LDestUnderGround : TUnderground;
  LSlurryDumpCopy : ISlurryDump;
  LDestSlurryDump : TSlurryDump;
  LChannelList : IChannelList;
  LChnnelPenalty : IChannelPenalty;
  LDestRiverChannel : IGeneralFlowChannel;
  LSourceRiverChannel : IGeneralFlowChannel;
  LDestPCDChannel : IGeneralFlowChannel;
  LSourcePCDChannel : IGeneralFlowChannel;
  LDestPolutionControlDam : TReservoirData;
  LSourcePolutionControlDam : TReservoirData;
begin
  try
    MineName := 'Copy of '+ASource.MineName;
    HydrologyNodeNumber := ASource.HydrologyNodeNumber;
    BeneficiationPlantArea := ASource.BeneficiationPlantArea;
    BeneficiationRunoffFactor := ASource.BeneficiationRunoffFactor;
    for LIndex := 1 to 12 do
      if ASource.PanEvaporation[LIndex] <> NullFloat then
        PanEvaporation[LIndex]  := ASource.PanEvaporation[LIndex];
    for LIndex := 1 to 12 do
      if ASource.LakeEvaporation[LIndex] <> NullFloat then
        LakeEvaporation[LIndex]  := ASource.LakeEvaporation[LIndex];
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    if LChannelList <> nil then
    begin
      LDestRiverChannel := LChannelList.ChannelByChannelNumber[RiverChannelNumber];
      LSourceRiverChannel := LChannelList.ChannelByChannelNumber[ASource.RiverChannelNumber];
      if (LDestRiverChannel <> nil) and (LSourceRiverChannel <> nil) then
      begin
        LDestRiverChannel.ChannelName := 'Copy of '+ LSourceRiverChannel.ChannelName;
        LDestRiverChannel.ChannelType := LSourceRiverChannel.ChannelType;
        LDestRiverChannel.ChannelSubType := LSourceRiverChannel.ChannelSubType;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceRiverChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestRiverChannel.ChannelPenalty := LChnnelPenalty;
      end;
      LDestPCDChannel := LChannelList.ChannelByChannelNumber[PCDChannelNumber];
      LSourcePCDChannel := LChannelList.ChannelByChannelNumber[ASource.PCDChannelNumber];
      if (LDestRiverChannel <> nil) and (LSourcePCDChannel <> nil) then
      begin
        LDestPCDChannel.ChannelName := 'Copy of '+ LSourcePCDChannel.ChannelName;
        LDestPCDChannel.ChannelType := LSourcePCDChannel.ChannelType;
        LDestPCDChannel.ChannelSubType := LSourcePCDChannel.ChannelSubType;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourcePCDChannel.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestPCDChannel.ChannelPenalty := LChnnelPenalty;
      end;
    end;

    if (ASource.PolutionControlDam <> nil) and (PolutionControlDam <> nil) then
    begin
      LSourcePolutionControlDam := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier];
      LDestPolutionControlDam := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[PolutionControlDam.ReservoirConfigurationData.ReservoirIdentifier];
      if (LDestPolutionControlDam <> nil) and (LSourcePolutionControlDam <> nil) then
        LDestPolutionControlDam.Assign(LSourcePolutionControlDam);
    end;

    for LIndex := 0 to ASource.OpenCastCount-1 do
    begin
      if ASource.OpenCastByIndex[LIndex] <> nil then
      begin
        LCopyOpenCast := CreateOpenCast;
        LDestOpenCast := CastOpenCastByID[LCopyOpenCast.Identifier];
        if LDestOpenCast <> nil then
          LDestOpenCast.Assign(ASource.CastOpenCastByIndex[LIndex]);
      end;
    end;

    for LIndex := 0 to ASource.UndergroundCount-1 do
    begin
      if ASource.UnderGroundByIndex[LIndex] <> nil then
      begin
        LUnderGroundCopy := CreateUnderGround;
        LDestUnderGround := CastUnderGroundByID[LUnderGroundCopy.Identifier];
        if LDestUnderGround <> nil then
          LDestUnderGround.Assign(ASource.CastUnderGroundByIndex[LIndex]);
      end;
    end;
    for LIndex := 0 to ASource.SlurryDumpCount-1 do
    begin
      if ASource.SlurryDumpByIndex[LIndex] <> nil then
      begin
        LSlurryDumpCopy := CreateSlurryDump;
        LDestSlurryDump := CastSlurryDumpByID[LSlurryDumpCopy.Identifier];
        if LDestSlurryDump <> nil then
          LDestSlurryDump.Assign(ASource.CastSlurryDumpByIndex[LIndex]);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMine.Get_CatchmentRefNrByReservoirID(AReservoirID: integer): integer;
const OPNAME = 'TMine.Get_CatchmentRefNrByReservoirID';
var
  LIndex : integer;
  LReservoirData        : IReservoirData;
  LReservoirDataList    : IReservoirDataList;
  LMineSubCatchmentList : TMineSubCatchmentList;
begin
  Result := 0;
  try
    LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                          .ReservoirList;
    if LReservoirDataList <> nil then
    begin
      for LIndex := 0 to LReservoirDataList.ReservoirAndNodesCount-1 do
      begin
        LReservoirData := LReservoirDataList.ReservoirOrNodeByIndex[LIndex];
        if LReservoirData <> nil then
        begin
          if LReservoirData.ReservoirConfigurationData.ReservoirIdentifier = AReservoirID then
          begin
            if LReservoirData.ReservoirConfigurationData.CatchmentRef <> 0 then
            begin
              LMineSubCatchmentList := TMineSubCatchmentList.Create(FAppModules);
              if LMineSubCatchmentList <> nil then
              begin
                Result := LReservoirData.ReservoirConfigurationData.CatchmentRef;
                break;
              end
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.CreateMineSubCatchment(ACatchmentRef: Integer);
const OPNAME = 'TMine.CreateMineSubCatchment';
var
  LMineSubCatchmentList : IMineSubCatchmentList;
begin
  try
    LMineSubCatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkFeaturesData.MineSubCatchmentList;
    if LMineSubCatchmentList <> nil then
       LMineSubCatchmentList.CreateMineSubCatchment(ACatchmentRef)
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.Get_PolutionControlDamExists: WordBool;
const OPNAME = 'TMine.Get_PolutionControlDamExists';
begin
  Result := False;
  try
    Result := (PolutionControlDam <> nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMine.Set_PolutionControlDamExists(Value: WordBool);
const OPNAME = 'TMine.Set_PolutionControlDamExists';
begin
  try
    if Value and Get_PolutionControlDamExists then Exit;
    if (not Value) and (not Get_PolutionControlDamExists) then Exit;
    if Value then
      CreatePolutionControlDam
    else
      RemovePolutionControlDam;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.CreatePolutionControlDam: IReservoirData;
const OPNAME = 'TMine.CreatePolutionControlDam';
var
  LPCDam         : IReservoirData;
  LPCDChannel    : IGeneralFlowChannel;
  LContinue      : boolean;
  LMineNodeNumber: integer;
  LMineNode      : IReservoirData;
  LMinMaxFeature : IMinMaxFlowConstraint;
  LIdentifier    : integer;
begin
  try
      //Create PCD
      LMineNode       := MineNode;
      LMineNodeNumber := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
      LPCDam := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateReservoir(ntMinePolutionControlDam);
      LContinue := (LPCDam <> nil);

      //Create channel to PCD
      if LContinue  then
      begin
        LPCDam.ReservoirConfigurationData.ReservoirName := 'Mine '+ IntToStr(LMineNodeNumber) +' Central PCD ';
        LPCDChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
        LContinue := (LPCDChannel <> nil);
      end;

      //Create channel to PCD min-max feature
      if LContinue  then
      begin
        LPCDChannel.UpStreamNodeNumber   := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        //LPCDChannel.UpStreamNodeNumber   := 0;
        LPCDChannel.DownStreamNodeNumber := LPCDam.ReservoirConfigurationData.ReservoirIdentifier;
        LMinMaxFeature :=  (FAppModules.Model as IYieldModel).DoCreateMinMaxFlowFeature(LPCDChannel.ChannelNumber);
        LPCDChannel.ChannelType := ctMineToPCDChannel;
        LPCDChannel.ChannelName := IntToStr(LPCDChannel.ChannelNumber)+ ' - Mine '+ IntToStr(LMineNodeNumber) +' To Central PCD';
        Set_PCDChannelNumber(LPCDChannel.ChannelNumber);
        LContinue := (LMinMaxFeature <> nil);
      end;
      if LContinue then
      begin
        TSystemModelManager(FAppModules.Model).CreateTabSheetTreeViewElement(
                                        'PCDDam',
                                        'FeaturesHeading,Mine,Mine',
                                        LPCDam.ReservoirConfigurationData.ReservoirIdentifier,
                                        Self.NodeNumber,
                                        LPCDam.ReservoirConfigurationData.ReservoirName,
                                        'RESERVOIR',
                                        'PCDDAM',
                                        False,
                                        tvsnAll)
      end
      else
      begin
        if(LPCDam <> nil) then
        begin
          LIdentifier := LPCDam.ReservoirConfigurationData.ReservoirIdentifier;
          LPCDam := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LIdentifier);
        end;

        if(LPCDChannel <> nil) then
        begin
          LIdentifier := LPCDChannel.ChannelNumber;
          LPCDChannel := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
          Set_PCDChannelNumber(LPCDChannel.ChannelNumber);
        end;
      end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMine.RemovePolutionControlDam: WordBool;
const OPNAME = 'TMine.RemovePolutionControlDam';
var
  LPCDNodeNumber : integer;
begin
  Result := False;
  try
    if PolutionControlDamExists then
    begin
      LPCDNodeNumber := Self.PCDNodelNumber;
      (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(Self.PCDNodelNumber);
      (FAppModules.Model as IYieldModel).DoConvertChannel(Self.FPCDChannelNumber);
      (FAppModules.Model.ModelData  as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(Self.FPCDChannelNumber);
      Set_PCDChannelNumber(0);
      TSystemModelManager(FAppModules.Model).DeleteTabSheetTreeViewElement(
                                      'PCDDam',
                                      'FeaturesHeading,Mine,Mine',
                                      LPCDNodeNumber,
                                      Self.NodeNumber);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TMineList }

procedure TMineList.CreateMemberObjects;
const OPNAME = 'TMineList.CreateMemberObjects';
begin
  inherited;
  try
    FMinesObjectContainer := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineList.DestroyMemberObjects;
const OPNAME = 'TMineList.DestroyMemberObjects';
begin
  try
    inherited;
    FMinesObjectContainer.Clear;
    FMinesObjectContainer.Free;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList._AddRef: Integer;
const OPNAME = 'TMineList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList._Release: Integer;
const OPNAME = 'TMineList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Initialise: boolean;
const OPNAME = 'TMineList.Initialise';
begin
  Result := inherited Initialise;
  try
    FMinesObjectContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TMineList.Validate';
var
  LIndex    : Integer;
  LMine  : TMine;
begin
  Result := True;
  try
    for LIndex := 0 to FMinesObjectContainer.Count-1 do
    begin
      LMine := TMine(FMinesObjectContainer.Items[LIndex]);
      if not LMine.Validate(AErrors,AContext) then
        Result := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_MineCount: integer;
const OPNAME = 'TMineList.Get_MineCount';
begin
  Result := 0;
  try
    Result := FMinesObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_CastMineByID(AMineID: Integer): TMine;
const OPNAME = 'TMineList.Get_CastMineByID';
var
  LIndex    : Integer;
  LMine  : TMine;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMinesObjectContainer.Count)) do
    begin
      LMine := TMine(FMinesObjectContainer.Items[LIndex]);
      if (LMine.Identifier = AMineID) then
        Result := LMine
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_CastMineByIndex(AIndex: Integer): TMine;
const OPNAME = 'TMineList.Get_CastMineByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMinesObjectContainer.Count) then
      Result := TMine(FMinesObjectContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_CastMineByNodeNumber(ANodeNumber: Integer): TMine;
const OPNAME = 'TMineList.Get_CastMineByNodeNumber';
var
  LIndex    : Integer;
  LMine  : TMine;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMinesObjectContainer.Count)) do
    begin
      LMine := TMine(FMinesObjectContainer.Items[LIndex]);
      if (LMine.NodeNumber = ANodeNumber) then
        Result := LMine
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_MineByIdentifier(AMineID: integer): IMine;
const OPNAME = 'TMineList.Get_MineByIdentifier';
begin
  Result := nil;
  try
    Result := Get_CastMineByID(AMineID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_MineByIndex(AIndex : integer): IMine;
const OPNAME = 'TMineList.Get_MineByIndex';
begin
  Result := nil;
  try
    Result := Get_CastMineByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_MineByNodeNumber(ANodeNumber : integer): IMine;
const OPNAME = 'TMineList.Get_MineByNodeNumber';
begin
  Result := nil;
  try
    Result := Get_CastMineByNodeNumber(ANodeNumber);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_MineByPCDNumber(APCDNumber: Integer): IMine;
const OPNAME = 'TMineList.Get_MineByPCDNumber';
begin
  Result := nil;
  try
    Result := Get_CastMineByPCDNumber(APCDNumber);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.Get_CastMineByPCDNumber(APCDNumber: Integer): TMine;
const OPNAME = 'TMineList.Get_CastMineByPCDNumber';
var
  LIndex : Integer;
  LMine  : TMine;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMinesObjectContainer.Count)) do
    begin
      LMine := TMine(FMinesObjectContainer.Items[LIndex]);
      if (LMine.PCDNodelNumber = APCDNumber) then
        Result := LMine
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.CreateMine: IMine;
const OPNAME = 'TMineList.CreateMine';
var
  LSQLAgent      : TMineSQLAgent;
  LMine          : TMine;
  LPlanningMine  : TPlanningMine;
  LMineNode      : IReservoirData;
  LMineToRiver   : IGeneralFlowChannel;
  LMinMaxFeature : IMinMaxFlowConstraint;
  LContinue      : boolean;
  LMineNodeNumber,
  LNewMineID,
  LIdentifier    : integer;
begin
  Result := nil;
  try
    if(FMinesObjectContainer.Count >= 100) then
      Exit;
    if FAppModules.Model.ModelName = CYield then
    begin

      LSQLAgent := TMineSQLAgent.Create(FAppModules);
      try
        LMine           := nil;
        LMineNode       := nil;
        LMineToRiver    := nil;
        LMinMaxFeature  := nil;
        LContinue       := True;
        LMineNodeNumber := 0;
        LNewMineID      := LSQLAgent.GetMaxMineID + 1;

        //Create mine node
        if LContinue  then
        begin
          LMineNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntMineNode);
          LContinue := (LMineNode <> nil);
        end;


        //Create channel to River
        if LContinue  then
        begin
          LMineToRiver := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
          LContinue := (LMineToRiver <> nil);
        end;

        //Create channel to River min-max feature
        if LContinue  then
        begin
          LMineToRiver.UpStreamNodeNumber   := 0; //LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
          LMinMaxFeature :=  (FAppModules.Model as IYieldModel).DoCreateMinMaxFlowFeature(LMineToRiver.ChannelNumber);
          LMineToRiver.ChannelType := ctMineToRiverDChannel;
          LMineToRiver.ChannelName := IntToStr(LMineToRiver.ChannelNumber)+ ' - Mine '+ IntToStr(LMineNodeNumber) +' To River';
          LContinue := (LMinMaxFeature <> nil);
        end;

        //Create mine
        if LContinue  then
        begin
          LMine := NewMine;
          LContinue := (LMine <> nil)
        end;

        //Populate mine/channels
        if LContinue  then
        begin
          LMine.FIdentifier                := LNewMineID;
          LMine.FRiverChannelNumber        := LMineToRiver.ChannelNumber;
          LMine.FNodeNumber                := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
          LMine.FPCDChannelNumber          := 0;
          LMine.FMineName                  := 'Mine '+IntToStr(LMine.FNodeNumber);
          LContinue := LSQLAgent.AddMine(LMine);
        end;

        if not LContinue  then
        begin
          if(LMine <> nil) then
          begin
            LSQLAgent.DeleteMine(LMine);
            FMinesObjectContainer.Remove(LMine)
          end;

          if(LMineNode <> nil) then
          begin
            LIdentifier := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
            LMineNode := nil;
            (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LIdentifier);
          end;

          if(LMineToRiver <> nil) then
          begin
            LIdentifier := LMineToRiver.ChannelNumber;
            LMineToRiver := nil;
            (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
            (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
          end;
        end
        else
          Result := LMine;
      finally
        LSQLAgent.Free;
      end;
    end;
    if FAppModules.Model.ModelName = CPlanning then
    begin
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LPlanningMine           := nil;
      LMineNode       := nil;
      LMineToRiver    := nil;
      LMinMaxFeature  := nil;
      LContinue       := True;
      LMineNodeNumber := 0;
      LNewMineID      := LSQLAgent.GetMaxMineID + 1;

      //Create mine node
      if LContinue  then
      begin
        LMineNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntMineNode);
        LContinue := (LMineNode <> nil);
      end;


      //Create channel to River
      if LContinue  then
      begin
        LMineToRiver := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
        LContinue := (LMineToRiver <> nil);
      end;

      //Create channel to River min-max feature
      if LContinue  then
      begin
        LMineToRiver.UpStreamNodeNumber   := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        LMinMaxFeature :=  (FAppModules.Model as IYieldModel).DoCreateMinMaxFlowFeature(LMineToRiver.ChannelNumber);
        LMineToRiver.ChannelType := ctMineToRiverDChannel;
        LMineToRiver.ChannelName := IntToStr(LMineToRiver.ChannelNumber)+ ' - Mine '+ IntToStr(LMineNodeNumber) +' To River';
        LContinue := (LMinMaxFeature <> nil);
      end;

      //Create mine
      if LContinue  then
      begin
        LPlanningMine := TPlanningMine(NewMine);
        LContinue := (LPlanningMine <> nil)
      end;

      //Populate mine/channels
      if LContinue  then
      begin
        LPlanningMine.FIdentifier                := LNewMineID;
        LPlanningMine.FRiverChannelNumber        := LMineToRiver.ChannelNumber;
        LPlanningMine.FNodeNumber                := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        LPlanningMine.FPCDChannelNumber          := 0;
        LPlanningMine.FMineName                  := 'Mine '+IntToStr(LPlanningMine.FNodeNumber);
        LPlanningMine.Populate(0,'',0.0,0.0,0.0,0.0);
        LContinue := LSQLAgent.AddMine(LPlanningMine);
      end;

      if not LContinue  then
      begin
        if(LPlanningMine <> nil) then
        begin
          LSQLAgent.DeleteMine(LPlanningMine);
          FMinesObjectContainer.Remove(LPlanningMine)
        end;

        if(LMineNode <> nil) then
        begin
          LIdentifier := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
          LMineNode := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LIdentifier);
        end;

        if(LMineToRiver <> nil) then
        begin
          LIdentifier := LMineToRiver.ChannelNumber;
          LMineToRiver := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;
      end
      else
        Result := LPlanningMine;
    finally
      LSQLAgent.Free;
    end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.NewMine: TMine;
const OPNAME = 'TMineList.NewMine';
begin
  Result := nil;
  try
    if FAppModules.Model.ModelName = CPlanning then
    begin
      Result := TPlanningMine.Create(FAppModules);
      Result.Initialise;
      FMinesObjectContainer.Add(Result);
    end;
    if FAppModules.Model.ModelName = CYield then
    begin
      Result := TMine.Create(FAppModules);
      Result.Initialise;
      FMinesObjectContainer.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.RemoveMine(AMineNumber: integer): WordBool;
const OPNAME = 'TMineList.RemoveMine';
var
  LMine     : TMine;
  LSQLAgent : TMineSQLAgent;
  LIndex    : integer;
begin
  Result := False;
  try
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      LMine := Get_CastMineByNodeNumber(AMineNumber);

      if(LMine <> nil) then
      begin
        LMine.RemovePolutionControlDam;
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LMine.FNodeNumber);
        (FAppModules.Model as IYieldModel).DoConvertChannel(LMine.FRiverChannelNumber);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LMine.FRiverChannelNumber);

        for LIndex := LMine.OpenCastCount-1 downto 0 do
        begin
          LMine.RemoveOpenCast(LMine.OpenCastIdentifierByIndex[LIndex])
        end;
        for LIndex := LMine.UndergroundCount-1 downto 0 do
        begin
          LMine.RemoveUnderGround(LMine.UnderGroundIdentifierByIndex[LIndex])
        end;
        for LIndex := LMine.SlurryDumpCount-1 downto 0 do
        begin
          LMine.RemoveSlurryDump(LMine.SlurryDumpIdentifierByIndex[LIndex])
        end;

        LSQLAgent.DeleteMine(LMine);
        FMinesObjectContainer.Remove(LMine);
        Result := True;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.GetNodeNumberFromIdentifier(AMineID: Integer): integer;
const OPNAME = 'TMineList.GetNodeNumberFromIdentifier';
var
  LMine : TMine;
begin
  Result := NullInteger;
  try
    LMine := Get_CastMineByID(AMineID);
    if(LMine <> nil) then
      Result := LMine.NodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateAnalysisStartVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateAnalysisStartVolume';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AnalysisStartVolume', FloatToStr(FAnalysisStartVolume), lMessage)) then
      AErrorMessages.Add(FloatToStr(FAnalysisStartVolume)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateCoalReserveArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateCoalReserveArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CoalReserveArea', FloatToStr(FCoalReserveArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FCoalReserveArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateDecantVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateDecantVolume';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DecantVolume', FloatToStr(FDecantVolume), lMessage)) then
      AErrorMessages.Add(FloatToStr(FDecantVolume)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateDisturbedArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateDisturbedArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DisturbedArea', FloatToStr(FDisturbedArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FDisturbedArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateDisturbedAreaRunoff(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateDisturbedAreaRunoff';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DisturbedAreaRunOff', FloatToStr(FDisturbedAreaRunoff), lMessage)) then
      AErrorMessages.Add(FloatToStr(FDisturbedAreaRunoff)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateDisturbedWorkingsArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateDisturbedWorkingsArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DisturbedWorkingsArea', FloatToStr(FDisturbedWorkingsArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FDisturbedWorkingsArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateDisturbedWorkingsAreaRunoff(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateDisturbedWorkingsAreaRunoff';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DisturbedWorkingsAreaRunOff', FloatToStr(FDisturbedWorkingsAreaRunoff), lMessage)) then
      AErrorMessages.Add(FloatToStr(FDisturbedWorkingsAreaRunoff)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateMaximumSeepageRate(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateMaximumSeepageRate';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaximumSeepageRate', FloatToStr(FMaximumSeepageRate), lMessage)) then
      AErrorMessages.Add(FloatToStr(FMaximumSeepageRate)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidatePCDAnalysisStartVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidatePCDAnalysisStartVolume';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('OpenCastPCDAnalysisStartVolume', FloatToStr(FPCDAnalysisStartVolume), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPCDAnalysisStartVolume)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidatePCDStorageCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidatePCDStorageCapacity';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('OpenCastPCDStorageCapacity', FloatToStr(FPCDStorageCapacity), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPCDStorageCapacity)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidatePCDSurfaceArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidatePCDSurfaceArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('OpenCastPCDSurfaceArea', FloatToStr(FPCDSurfaceArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPCDSurfaceArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidatePitName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidatePitName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PitName', FPitName, lMessage)) then
      AErrorMessages.Add(FPitName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateSeepageExponent(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateSeepageExponent';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SeepageExponent', FloatToStr(FSeepageExponent), lMessage)) then
      AErrorMessages.Add(FloatToStr(FSeepageExponent)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateSeepageVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateSeepageVolume';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SeepageVolume', FloatToStr(FSeepageVolume), lMessage)) then
      AErrorMessages.Add(FloatToStr(FSeepageVolume)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateWaterSurfaceEvapArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateWaterSurfaceEvapArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('WaterSurfaceEvapArea', FloatToStr(FWaterSurfaceEvapArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FWaterSurfaceEvapArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOpenCast.ValidateWorkingsArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TOpenCast.ValidateWorkingsArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('WorkingsArea', FloatToStr(FWorkingsArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FWorkingsArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.ValidateBoardPillarCatchmentArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TUnderground.ValidateBoardPillarCatchmentArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('BoardPillarCatchmentArea', FloatToStr(FBoardPillarCatchmentArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FBoardPillarCatchmentArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.ValidateChannelNumberToUGDam(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TUnderground.ValidateChannelNumberToUGDam';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ChannelNumberToUGDam', IntToStr(FChannelNumberToUGDam), lMessage)) then
      AErrorMessages.Add(IntToStr(FChannelNumberToUGDam)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.ValidateHighExtractionAreaRunoffFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TUnderground.ValidateHighExtractionAreaRunoffFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('HighExtractionAreaRunoffFactor', FloatToStr(FHighExtractionAreaRunoffFactor), lMessage)) then
      AErrorMessages.Add(FloatToStr(FHighExtractionAreaRunoffFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.ValidateHighExtractionCatchmentArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TUnderground.ValidateHighExtractionCatchmentArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('HighExtractionCatchmentArea', FloatToStr(FHighExtractionCatchmentArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FHighExtractionCatchmentArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.ValidateUndergroundSectionName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TUnderground.ValidateUndergroundSectionName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('UndergroundSectionName', FUndergroundSectionName, lMessage)) then
      AErrorMessages.Add(FUndergroundSectionName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUnderground.ValidateUpstreamCatchmentArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TUnderground.ValidateUpstreamCatchmentArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('UpstreamCatchmentArea', FloatToStr(FUpstreamCatchmentArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FUpstreamCatchmentArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidateDumpName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidateDumpName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DumpName', FDumpName, lMessage)) then
      AErrorMessages.Add(FDumpName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidateDumpSurfaceArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidateDumpSurfaceArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DumpSurfaceArea', FloatToStr(FDumpSurfaceArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FDumpSurfaceArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidatePCDAnalysisStartVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidatePCDAnalysisStartVolume';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DumpPCDAnalysisStartVolume', FloatToStr(FPCDAnalysisStartVolume), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPCDAnalysisStartVolume)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidatePCDStorageCapacity(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidatePCDStorageCapacity';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DumpPCDStorageCapacity', FloatToStr(FPCDStorageCapacity), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPCDStorageCapacity)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidatePCDSurfaceArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidatePCDSurfaceArea';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DumpPCDSurfaceArea', FloatToStr(FPCDSurfaceArea), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPCDSurfaceArea)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidateRunoffFactorToPCD(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidateRunoffFactorToPCD';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('RunoffFactorToPCD', FloatToStr(FRunoffFactorToPCD), lMessage)) then
      AErrorMessages.Add(FloatToStr(FRunoffFactorToPCD)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSlurryDump.ValidateSeepageSplitFactor(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TSlurryDump.ValidateSeepageSplitFactor';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SeepageSplitFactor', FloatToStr(FSeepageSplitFactor), lMessage)) then
      AErrorMessages.Add(FloatToStr(FSeepageSplitFactor)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TMineList.Get_ChannelToUnderGroundDamByDamNumber(AReservoirNumber: integer): IGeneralFlowChannel;
const OPNAME = 'TMineList.Get_ChannelToUnderGroundDamByDamNumber';
var
  LIndex       : integer;
  LCount       : integer;
  LMine        : TMine;
  LUnderGround : TUnderground;
begin
  Result := nil;
  try
    for LIndex := 0 to MineCount-1 do
    begin

      LMine := CastMineByIndex[LIndex];

      for LCount := 0 to LMine.UndergroundCount-1 do
      begin
        LUnderGround := LMine.CastUnderGroundByIndex[LCount];
        if LUnderGround.UndergroundDam <> nil then
        begin
          if(LUnderGround.UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier = AReservoirNumber) then
          begin
            Result := LUnderGround.ChannelToUnderGroundDam;
            Break;
          end;
        end;
      end;
      if (Result <> nil) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineList.CopyCreate(AMineNumber: integer): IMine;
const OPNAME = 'TMineList.CopyCreate';
var
  LCopyMine : IMine;
  LDestMine : TMine;
  LSourceMine : TMine;
begin
  Result := nil;
  try
    LSourceMine := CastMineByNodeNumber[AMineNumber];
    if LSourceMine <> nil then
    begin
      LCopyMine := CreateMine;
      LDestMine := CastMineByID[LCopyMine.Identifier];
      if LDestMine <> nil then
      begin
        LDestMine.Assign(LSourceMine);
        Result := LDestMine;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOpenCast.Assign(ASource: TOpenCast);
const OPNAME = 'TOpenCast.Assign';
var
  LIndex : integer;
begin
  try
    PitName                     := 'Copy of '+ASource.PitName;
    CoalReserveArea             := ASource.CoalReserveArea;
    WorkingsArea                := ASource.WorkingsArea;
    DisturbedWorkingsArea       := ASource.DisturbedWorkingsArea;
    DisturbedArea               := ASource.DisturbedArea;
    WaterSurfaceEvapArea        := ASource.WaterSurfaceEvapArea;
    DisturbedAreaRunoff         := ASource.DisturbedAreaRunoff;
    DecantVolume                := ASource.DecantVolume;
    SeepageVolume               := ASource.SeepageVolume;
    AnalysisStartVolume         := ASource.AnalysisStartVolume;
    MaximumSeepageRate          := ASource.MaximumSeepageRate;
    SeepageExponent             := ASource.SeepageExponent;
    PCDSurfaceArea              := ASource.PCDSurfaceArea;
    PCDStorageCapacity          := ASource.PCDStorageCapacity;
    PCDAnalysisStartVolume      := ASource.PCDAnalysisStartVolume;
    DisturbedWorkingsAreaRunoff := ASource.DisturbedWorkingsAreaRunoff;
    for LIndex := 1 to 12 do
      if ASource.DisturbedRechargeFactor[LIndex] <> nullfloat then
        DisturbedRechargeFactor[LIndex] := ASource.DisturbedRechargeFactor[LIndex];
    for LIndex := 1 to 12 do
      if ASource.WorkingAreaRechargeFactor[LIndex] <> nullfloat then
        WorkingAreaRechargeFactor[LIndex]:= ASource.WorkingAreaRechargeFactor[LIndex];

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSlurryDump.Assign(ASource: TSlurryDump);
const OPNAME = 'TSlurryDump.Assign';
var
  LIndex : integer;
begin
  try
    DumpName                  := 'Copy of '+ASource.DumpName;
    DumpSurfaceArea           := ASource.DumpSurfaceArea;
    RunoffFactorToPCD         := ASource.RunoffFactorToPCD;
    SeepageSplitFactor        := ASource.SeepageSplitFactor;
    PCDStorageCapacity        := ASource.PCDStorageCapacity;
    PCDSurfaceArea            := ASource.PCDSurfaceArea;
    PCDAnalysisStartVolume    := ASource.PCDAnalysisStartVolume;
    for LIndex := 1 to 12 do
      if ASource.RechargeFactor[LIndex] <> NullFloat then
        RechargeFactor[LIndex] := ASource.RechargeFactor[LIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUnderground.Assign(ASource: TUnderground);
const OPNAME = 'TUnderground.Assign';
var
  LIndex : integer;
  LDestChannelToUnderGroundDam : IGeneralFlowChannel;
  LSourceChannelToUnderGroundDam : IGeneralFlowChannel;
  LChannelList : IChannelList;
  LChnnelPenalty : IChannelPenalty;
  LDestUndergroundDam : TReservoirData;
  LSourceUndergroundDam : TReservoirData;
begin
  try
    UndergroundSectionName := 'Copy of '+ASource.UndergroundSectionName;
    ChannelNumberToUGDam := ASource.ChannelNumberToUGDam;
    UpstreamCatchmentArea := ASource.UpstreamCatchmentArea;
    BoardPillarCatchmentArea := ASource.BoardPillarCatchmentArea;
    HighExtractionCatchmentArea := ASource.HighExtractionCatchmentArea;
    HighExtractionAreaRunoffFactor := ASource.HighExtractionAreaRunoffFactor;
    for LIndex := 1 to 12 do
      if ASource.UpstreamRunoffPortion[LIndex] <> NullFloat then
        UpstreamRunoffPortion[LIndex]:= ASource.UpstreamRunoffPortion[LIndex];
    for LIndex := 1 to 12 do
      if ASource.BoardAndPilarRechargeFactor[LIndex] <> NullFloat then
        BoardAndPilarRechargeFactor[LIndex] := ASource.BoardAndPilarRechargeFactor[LIndex];
    for LIndex := 1 to 12 do
      if ASource.HighExtractionRechargeFactor[LIndex] <> NullFloat then
        HighExtractionRechargeFactor[LIndex] := ASource.HighExtractionRechargeFactor[LIndex];
    LChannelList := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
    if LChannelList <> nil then
    begin
      LDestChannelToUnderGroundDam := LChannelList.ChannelByChannelNumber[FChannelNumberToUGDam];
      LSourceChannelToUnderGroundDam := LChannelList.ChannelByChannelNumber[ASource.ChannelNumberToUGDam];
      if (LDestChannelToUnderGroundDam <> nil) and (LSourceChannelToUnderGroundDam <> nil) then
      begin
        LDestChannelToUnderGroundDam.ChannelName := 'Copy of '+ LSourceChannelToUnderGroundDam.ChannelName;
        LDestChannelToUnderGroundDam.ChannelType := LSourceChannelToUnderGroundDam.ChannelType;
        LDestChannelToUnderGroundDam.ChannelSubType := LSourceChannelToUnderGroundDam.ChannelSubType;
        LChnnelPenalty := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceChannelToUnderGroundDam.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestChannelToUnderGroundDam.ChannelPenalty := LChnnelPenalty;
      end;
    end;
    LSourceUndergroundDam := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.UndergroundDamNodeNumber];
    LDestUndergroundDam := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[UndergroundDamNodeNumber];
    if (LDestUndergroundDam <> nil) and (LSourceUndergroundDam<> nil) then
      LDestUndergroundDam.Assign(LSourceUndergroundDam);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TMineSubCatchment }

procedure TMineSubCatchment.CreateMemberObjects;
const OPNAME = 'TMineSubCatchment.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.DestroyMemberObjects;
const OPNAME = 'TMineSubCatchment.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment._AddRef: Integer;
const OPNAME = 'TMineSubCatchment._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment._Release: Integer;
const OPNAME = 'TMineSubCatchment._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Initialise: boolean;
const OPNAME = 'TMineSubCatchment.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FIdentifier                  := 0;
    FCatchmentReferenceNr        := 0;
    FProportionAntecedentFlows   := 0.0;
    FGroundwaterFlowVolume       := 0.0;
    FAntecedentRunoffDecayFactor := 0.0;
    FCatchmentRefName            := '';
    FCatchmentRefUsed            := False;

    for LIndex := Low(FMinimunGroundwaterFlowVolume) to High(FMinimunGroundwaterFlowVolume) do
      FMinimunGroundwaterFlowVolume[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Assign(ASource: TMineSubCatchment);
const OPNAME = 'TMineSubCatchment.Assign';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Populate(AIdentifier,ACatchmentReferenceNr: integer;
                                     ACatchmentRefName: string;AProportionAntecedentFlow,
                                     AGroundwaterFlowVolume,AAntecedentRunoffDecayFactor: double;
                                     ACatchmentRefUsed: boolean);
const OPNAME = 'TMineSubCatchment.Populate';
begin
  try
    FIdentifier                  := AIdentifier;
    FCatchmentReferenceNr        := ACatchmentReferenceNr;
    FCatchmentRefName            := ACatchmentRefName;
    FProportionAntecedentFlows   := AProportionAntecedentFlow;
    FGroundwaterFlowVolume       := AGroundwaterFlowVolume;
    FAntecedentRunoffDecayFactor := AntecedentRunoffDecayFactor;
    FCatchmentRefUsed            := ACatchmentRefUsed;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.PopulateMinimunGroundwaterFlowVolume(AGroundwaterFlowVolume: TMonthlyDoubleArray);
const OPNAME = 'TMineSubCatchment.PopulateMinimunGroundwaterFlowVolume';
var
  LIndex: integer;
begin
  try
    for LIndex := Low(FMinimunGroundwaterFlowVolume) to High(FMinimunGroundwaterFlowVolume) do
        FMinimunGroundwaterFlowVolume[LIndex] := AGroundwaterFlowVolume[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Get_Identifier: Integer;
const OPNAME = 'TMineSubCatchment.Get_Identifier';
begin
  Result := NullInteger;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Get_CatchmentReferenceNr: Integer;
const OPNAME = 'TMineSubCatchment.Get_CatchmentReferenceNr';
begin
  Result := NullInteger;
  try
    Result := FCatchmentReferenceNr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_CatchmentReferenceNr(Value: Integer);
const OPNAME = 'TMineSubCatchment.Set_CatchmentReferenceNr';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'MineCatchmentReferenceNr', IntToStr(Value), IntToStr(FCatchmentReferenceNr), LContextData) then
        begin
          LOldValue := FCatchmentReferenceNr;
          FCatchmentReferenceNr := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MineCatchmentReferenceNr',IntToStr(LOldValue),IntToStr(FCatchmentReferenceNr));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Get_CatchmentRefName: WideString;
const OPNAME = 'TMineSubCatchment.Get_CatchmentRefName';
begin
  Result := '';
  try
    Result := FCatchmentRefName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_CatchmentRefName(const Value: WideString);
const OPNAME = 'TMineSubCatchment.Set_CatchmentRefName';
begin
  try

 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Get_CatchmentRefUsed: WordBool;
const OPNAME = 'TMineSubCatchment.Get_CatchmentRefUsed';
begin
  Result := False;
  try
    Result := FCatchmentRefUsed;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_CatchmentRefUsed(Value: WordBool);
const OPNAME = 'TMineSubCatchment.Set_CatchmentRefUsed';
var
  LOldValue    : string;
  LNewValue    : string;
  LContextData : TStringList;
  LLoadAgent   : TMineSQLAgent;
begin
  try
    if FCatchmentRefUsed then
      LOldValue := '1'
    else
      LOldValue := '0';

    if Value then
      LNewValue := '1'
    else
      LNewValue := '0';

    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'CatchmentRefInUse',LNewValue,LOldValue, LContextData) then
        begin
          FCatchmentRefUsed := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CatchmentRefInUse',LOldValue, LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;



function TMineSubCatchment.Get_MinimunGroundwaterFlowVolume(AIndex: Integer): Double;
const OPNAME = 'TMineSubCatchment.Get_MinimunGroundwaterFlowVolume';
begin
  Result := NullFloat;
  try
    Result := FMinimunGroundwaterFlowVolume[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_MinimunGroundwaterFlowVolume(AIndex: Integer;Value: Double);
const OPNAME = 'TMineSubCatchment.Set_MinimunGroundwaterFlowVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineSubCatchmentVolumeFactors(LContextData,FIdentifier,IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'MineSubCatchmentFlowVolume', FloatToStr(Value), FloatToStr(FMinimunGroundwaterFlowVolume[AIndex]), LContextData) then
        begin
          LPrevValue := FMinimunGroundwaterFlowVolume[AIndex];
          FMinimunGroundwaterFlowVolume[AIndex] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MineSubCatchmentFlowVolume',FloatToStr(LPrevValue),FloatToStr(FMinimunGroundwaterFlowVolume[AIndex]));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchment.Get_ProportionAntecedentFlows: Double;
const OPNAME = 'TMineSubCatchment.Get_ProportionAntecedentFlows';
begin
  Result := NullFloat;
  try
    Result := FProportionAntecedentFlows;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_ProportionAntecedentFlows(Value: Double);
const OPNAME = 'TMineSubCatchment.Set_ProportionAntecedentFlows';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'ProportionAntecedentFlow', FloatToStr(Value), FloatToStr(FProportionAntecedentFlows), LContextData) then
        begin
          LOldValue := FProportionAntecedentFlows;
          FProportionAntecedentFlows := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ProportionAntecedentFlow',FloatToStr(LOldValue),FloatToStr(FProportionAntecedentFlows));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Get_AntecedentRunoffDecayFactor: Double;
const OPNAME = 'TMineSubCatchment.Get_AntecedentRunoffDecayFactor';
begin
  Result := NullFloat;
  try
    Result := FAntecedentRunoffDecayFactor;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_AntecedentRunoffDecayFactor(Value: Double);
const OPNAME = 'TMineSubCatchment.Set_AntecedentRunoffDecayFactor';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'AntecedentRunoffDecayFactor', FloatToStr(Value), FloatToStr(FAntecedentRunoffDecayFactor), LContextData) then
        begin
          LOldValue := FAntecedentRunoffDecayFactor;
          FAntecedentRunoffDecayFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AntecedentRunoffDecayFactor',FloatToStr(LOldValue),FloatToStr(FAntecedentRunoffDecayFactor));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Get_GroundwaterFlowVolume: Double;
const OPNAME = 'TMineSubCatchment.Get_GroundwaterFlowVolume';
begin
  Result := NullFloat;
  try
    Result := FGroundwaterFlowVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.Set_GroundwaterFlowVolume(Value: Double);
const OPNAME = 'TMineSubCatchment.Set_GroundwaterFlowVolume';
var
  LLoadAgent   : TMineSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TMineSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_MineData(LContextData, FIdentifier);
        if FAppModules.FieldProperties.UpdateFieldValue(
           'GroundwaterFlowVolume', FloatToStr(Value), FloatToStr(FGroundwaterFlowVolume), LContextData) then
        begin
          LOldValue := FGroundwaterFlowVolume;
          FGroundwaterFlowVolume := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GroundwaterFlowVolume',FloatToStr(LOldValue),FloatToStr(FGroundwaterFlowVolume));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchment.PopulateSome(AIdentifier,ACatchmentReferenceNr: integer;
                                         ACatchmentRefUsed: boolean;ACatchmentRefName: string);
const OPNAME = 'TMineSubCatchment.PopulateSome';
begin
  try
    FIdentifier           := AIdentifier;
    FCatchmentReferenceNr := ACatchmentReferenceNr;
    FCatchmentRefUsed     := ACatchmentRefUsed;
    FCatchmentRefName     := ACatchmentRefName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchment.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TMineSubCatchment.Validate';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TMineSubCatchmentList }

function TMineSubCatchmentList._AddRef: Integer;
const OPNAME = 'TMineSubCatchmentList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList._Release: Integer;
const OPNAME = 'TMineSubCatchmentList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Initialise: boolean;
const OPNAME = 'TMineSubCatchmentList.Initialise';
begin
  Result := inherited Initialise;
  try
    FMineSubCatchmentContainer.Clear;
    Result := True;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchmentList.CreateMemberObjects;
const OPNAME = 'TMineSubCatchmentList.CreateMemberObjects';
begin
  inherited;
  try
    FMineSubCatchmentContainer := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMineSubCatchmentList.DestroyMemberObjects;
const OPNAME = 'TMineSubCatchmentList.DestroyMemberObjects';
begin
  try
    inherited;
    FMineSubCatchmentContainer.Clear;
    FMineSubCatchmentContainer.Free;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.CreateMineSubCatchment(ACatchmentRefNr: Integer): IMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.CreateMineSubCatchment';
var
  LMineSubCatchment : TMineSubCatchment;
  LSQLAgent         : TMineSQLAgent;
  LIdentifier       : integer;
  LCatchmentRefName : string;
  LCatchmentRefUsed : boolean;
begin
  Result := nil;
  try
    if CheckMineSubCatchmentExist(ACatchmentRefNr) then
    begin
      LMineSubCatchment := CastMineSubCatchmentByRefNodeNr[ACatchmentRefNr];
      if LMineSubCatchment <> nil then
        Result := LMineSubCatchment;
    end
    else
    begin
      LSQLAgent := TMineSQLAgent.Create(FAppModules);
      try
        LIdentifier := 0;
        LCatchmentRefName := GetCatchmentRefNameByRefNumber(ACatchmentRefNr);
        if (LSQLAgent.AddMineSubCatchment(LIdentifier,ACatchmentRefNr,LCatchmentRefName)) then
        begin
          LMineSubCatchment :=  NewMineSubCatchment;
          LMineSubCatchment.Initialise;
          LCatchmentRefUsed := True;
          LMineSubCatchment.PopulateSome(LIdentifier, ACatchmentRefNr,
                                         LCatchmentRefUsed,LCatchmentRefName);
          Result := LMineSubCatchment;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.NewMineSubCatchment: TMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.NewMineSubCatchment';
begin
  Result := nil;
  try
    Result := TMineSubCatchment.Create(FAppModules);;
    Result.Initialise;
    FMineSubCatchmentContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_MineSubCatchmentByIdentifier(AIdentifier: Integer): IMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.Get_MineSubCatchmentByIdentifier';
begin
  Result := nil;
  try
    Result := Get_CastMineSubCatchmentByIdentifier(AIdentifier);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_MineSubCatchmentByIndex(AIndex: Integer): IMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.Get_MineSubCatchmentByIndex';
begin
  Result := nil;
  try
    Result := Get_CastMineSubCatchmentByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_MineSubCatchmentByRefNodeNr(ARefNodeNr: Integer): IMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.Get_MineSubCatchmentByRefNodeNr';
var
  LIndex            : Integer;
  LMineSubCatchment : TMineSubCatchment;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMineSubCatchmentContainer.Count)) do
    begin
      LMineSubCatchment := TMineSubCatchment(FMineSubCatchmentContainer.Items[LIndex]);
      if (LMineSubCatchment.CatchmentReferenceNr = ARefNodeNr) then
        Result := LMineSubCatchment
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_MineSubCatchmentCount: Integer;
const OPNAME = 'TMineSubCatchmentList.Get_MineSubCatchmentCount';
begin
  Result := 0;
  try
    Result := FMineSubCatchmentContainer.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Validate(var AErrors: WideString;
                                        const AContext: WideString): WordBool;
const OPNAME = 'TMineSubCatchmentList.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_CastMineSubCatchmentByIndex(AIndex: Integer): TMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.Get_CastMineSubCatchmentByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMineSubCatchmentContainer.Count) then
      Result := TMineSubCatchment(FMineSubCatchmentContainer[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_CastMineSubCatchmentByIdentifier(AIdentifier: Integer): TMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.Get_CastMineSubCatchmentByIdentifier';
var
  LIndex            : Integer;
  LMineSubCatchment : TMineSubCatchment;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMineSubCatchmentContainer.Count)) do
    begin
      LMineSubCatchment := TMineSubCatchment(FMineSubCatchmentContainer.Items[LIndex]);
      if (LMineSubCatchment.Identifier = AIdentifier) then
        Result := LMineSubCatchment
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.CheckMineSubCatchmentExist(ACatchmentRefNr: integer): boolean;
const OPNAME = 'TMineSubCatchmentList.CheckMineSubCatchmentExist';
var
  LIndex            : integer;
  LMineSubCatchment : TMineSubCatchment;
begin
  Result := False;
  try
    for LIndex := 0 to FMineSubCatchmentContainer.Count-1 do
    begin
      LMineSubCatchment := TMineSubCatchment(FMineSubCatchmentContainer[LIndex]);
      if LMineSubCatchment <> nil then
      begin
        if LMineSubCatchment.CatchmentReferenceNr = ACatchmentRefNr then
        begin
          Result := True;
          break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.Get_CastMineSubCatchmentByRefNodeNr(ARefNodeNr: Integer): TMineSubCatchment;
const OPNAME = 'TMineSubCatchmentList.Get_CastMineSubCatchmentByRefNodeNr';
var
  LIndex            : Integer;
  LMineSubCatchment : TMineSubCatchment;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMineSubCatchmentContainer.Count)) do
    begin
      LMineSubCatchment := TMineSubCatchment(FMineSubCatchmentContainer.Items[LIndex]);
      if (LMineSubCatchment.CatchmentReferenceNr = ARefNodeNr) then
        Result := LMineSubCatchment
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSubCatchmentList.GetCatchmentRefNameByRefNumber(ARefNum: integer): string;
const OPNAME = 'TMineSubCatchmentList.GetCatchmentRefNameByRefNumber';
var
  LIndex           : integer;
  LCatchmentRef     : TParamReference;
  LCatchRefList     : TObjectList;
begin
  try
    LCatchRefList := TYieldModelDataObject(FAppModules.Model.ModelData).CastCastParameterData.AllReferenceData;
    if (LCatchRefList <> nil) then
    begin
      for LIndex := 0 to LCatchRefList.Count - 1 do
      begin
        LCatchmentRef  := TParamReference(lCatchRefList.Items[LIndex]);
        if LCatchmentRef <> nil then
        begin
          if LCatchmentRef.CatchReference = ARefNum then
          begin
            Result := ExtractFileName(lCatchmentRef.FileReference);
            Exit;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentList.ValidateMineCatchmentRefInUse(ACatchmentRefNr: Integer);
const OPNAME = 'TMineSubCatchmentList.ValidateMineCatchmentRefInUse';
var
  LIndex             : integer;
  LIndexA            : integer;
  LNodeRefNr         :integer;
//  LCatchmentRef      :integer;

  LReservoirData     : TReservoirData;
  LReservoirDataList : TReservoirDataList;

//  LMine              : TMine;
//  LMinelist          : TMineList;

  LMineStr           : TStringList;
  LReservoirDataStr  : TStringList;

  LMineSubCatchment     : TMineSubCatchment;
begin
  try
    LMineStr          := TStringList.Create;
    LReservoirDataStr := TStringList.Create;
    try
      if FMineSubCatchmentContainer.Count > 0 then
      begin
        for LIndex := 0 to FMineSubCatchmentContainer.Count -1 do
        begin
          LMineSubCatchment := CastMineSubCatchmentByIndex[LIndex];
          if LMineSubCatchment <> nil then
          begin
            if LMineSubCatchment.CatchmentReferenceNr > 0 then
              LMineStr.Add(IntToStr(LMineSubCatchment.CatchmentReferenceNr));
          end;
        end;
      end;

      LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
                          .CastReservoirList;
      if LReservoirDataList <> nil then
      begin
        for LIndexA := 0 to LReservoirDataList.ReservoirAndNodesCount-1 do
        begin
          LReservoirData := LReservoirDataList.CastReservoirOrNodeByIndex[LIndexA];
          if LReservoirData <> nil then
          begin
            if LReservoirData.ReservoirConfigurationData.CatchmentRef > 0 then
               LReservoirDataStr.Add(IntToStr(LReservoirData.ReservoirConfigurationData.CatchmentRef))
          end;
        end;
      end;

      if ((LMineStr.Count > 0) and (LReservoirDataStr.Count > 0)) then
      begin
        for LIndex := 0 to LMineStr.Count-1 do
        begin
          LNodeRefNr := StrToInt(LMineStr[LIndex]);
          if (LReservoirDataStr.IndexOf(LMineStr[LIndex]) < 0) then
          begin
            begin
              LMineSubCatchment := CastMineSubCatchmentByRefNodeNr[LNodeRefNr];
              if LMineSubCatchment <> nil then
              LMineSubCatchment.Set_CatchmentRefUsed(False);
            end;
          end
          else
          begin
            LMineSubCatchment := CastMineSubCatchmentByRefNodeNr[LNodeRefNr];
            if LMineSubCatchment <> nil then
              LMineSubCatchment.Set_CatchmentRefUsed(True);
          end;
        end;
      end;
    finally
      LMineStr.Free;
      LReservoirDataStr.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
