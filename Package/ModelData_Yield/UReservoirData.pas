//
//
//  UNIT      : Contains TReservoirData Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/02/25
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirData;

interface

uses
  Contnrs,
  Classes,
  UAbstractObject,
  VoaimsCom_TLB,
  UReservoirZoneElevationData,
  UReservoirPenaltyStructureData;

type
  TReservoirEvaporationsData = class(TAbstractAppObject,IReservoirEvaporationsData)
  protected
    FMonthlyEvaporations : array of double;
    FRecordIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CopyFrom (AResID  : integer; ASource : TReservoirEvaporationsData) : boolean;
  public
    procedure Assign(ASource : TReservoirEvaporationsData); virtual;
    procedure Set_MonthlyEvaporationsByIndex(AIndex: integer;ANewValue: double); safecall;
    function Get_MonthlyEvaporationsByIndex(AIndex: integer): double; safecall;
    function Get_RecordIdentifier: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function Initialise: boolean; override;
    function LoadMonthlyEvaporationsFromDataset(ADataSet: TAbstractModelDataset): boolean;
    procedure InitialiseNewEvaporations(ANewRecordId: integer ;ANodeType: TNodeType);
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property MonthlyEvaporationsByIndex[AIndex: Integer]: Double read Get_MonthlyEvaporationsByIndex write Set_MonthlyEvaporationsByIndex;
  end;

  TReservoirElevationsData = class(TAbstractAppObject,IReservoirElevationsData)
  protected
    FStartElevation: integer;
    FReservoirElevations: array of double;
    FRecordIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function RecalculateAreaWhenFull: WordBool;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CopyFrom (AResID  : integer;
                       ASource : TReservoirElevationsData) : boolean;
  public
    procedure Assign(ASource : TReservoirElevationsData); virtual;
    procedure Set_ReservoirElevationsByIndex(AIndex: integer;ANewValue: double); safecall;
    function Get_ReservoirElevationsByIndex(AIndex: integer): double; safecall;
    function Get_RecordIdentifier: integer; safecall;
    function Get_StartElevation: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function DeleteElevationValue(AIndex: integer): WordBool;safecall;
    function InsertElevationValue(AIndex: integer;AValue: double): WordBool;safecall;

    procedure InitialiseNewElevations(ANewRecordId: integer ;ANodeType: TNodeType); virtual;
    function Initialise: boolean; override;
    function LoadMonthlyElevationsFromDataset(ADataSet: TAbstractModelDataset): boolean; virtual;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property StartElevation: Integer read Get_StartElevation;
    property ReservoirElevationsByIndex[AIndex: Integer]: Double read Get_ReservoirElevationsByIndex write Set_ReservoirElevationsByIndex;
  end;

  TReservoirVolumesData = class(TAbstractAppObject,IReservoirVolumeData)
  protected
    FStartVolume: integer;
    FReservoirVolumes: array of double;
    FRecordIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CopyFrom (AResID  : integer;
                       ASource : TReservoirVolumesData) : boolean;
  public
    procedure Assign(ASource : TReservoirVolumesData); virtual;
    procedure Set_ReservoirVolumesByIndex(AIndex: integer;ANewValue: double); safecall;
    function Get_RecordIdentifier: integer; safecall;
    function Get_StartVolume: integer; safecall;
    function Get_ReservoirVolumesByIndex(AIndex: integer): double; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_MaxReservoirVolume: Double; safecall;

    function DeleteVolumeValue(AIndex: integer): WordBool;safecall;
    function InsertVolumeValue(AIndex: integer;AValue: double): WordBool;safecall;

    function Initialise: boolean; override;
    function LoadVolumesFromDataset(ADataSet: TAbstractModelDataset): boolean;
    procedure InitialiseNewVolumes(ANewRecordId: integer ;ANodeType: TNodeType);
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property StartVolume: Integer read Get_StartVolume;
    property ReservoirVolumesByIndex[AIndex: Integer]: Double read Get_ReservoirVolumesByIndex write Set_ReservoirVolumesByIndex;
    property MaxReservoirVolume: Double read Get_MaxReservoirVolume;
  end;

  TReservoirAreasData = class(TAbstractAppObject,IReservoirAreaData)
  protected
    FStartArea: integer;
    FReservoirAreas: array of double;
    FRecordIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function RecalculateAreaWhenFull: WordBool;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure UpdateMaxArea;
    function Get_MaxArea: double;
    function CopyFrom (AResID  : integer;
                       ASource : TReservoirAreasData) : boolean;
  public
    procedure Assign(ASource : TReservoirAreasData); virtual;
    procedure Set_ReservoirAreasByIndex(AIndex: integer;ANewValue: double); safecall;
    function Get_ReservoirAreasByIndex(AIndex: integer): double; safecall;
    function Get_StartArea: integer; safecall;
    function Get_RecordIdentifier: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function DeleteAreaValue(AIndex: integer): WordBool;safecall;
    function InsertAreaValue(AIndex: integer;AValue: double): WordBool;safecall;

    function Initialise: boolean; override;
    function LoadAreasFromDataset(ADataSet: TAbstractModelDataset): boolean;
    procedure InitialiseNewAreas(ANewRecordId: integer ;ANodeType: TNodeType);
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property StartArea: Integer read Get_StartArea;
    property ReservoirAreasByIndex[AIndex: Integer]: Double read Get_ReservoirAreasByIndex write Set_ReservoirAreasByIndex;
  end;

  TReservoirConfigurationData = class(TAbstractAppObject,IReservoirConfigurationData)
  protected
    FRecordIdentifier    : integer;
    FNodeType            : TNodeType;
    FReservoirName       : string;
    FReservoirIdentifier : integer;
    FPenaltyStruct       : integer;
    FPointsCount         : integer;
    FIncludeSummary      : string;
    FDrainageScale       : double;
    FMaxArea             : double;
    FUrbanRunOff         : double;
    FAreaWhenFull        : double;
    FVolumeWhenFull      : double;
    FMaxVolume           : double;
    FPriority            : double;
    FStatusIndicator     : integer;
    FAfforestationScale  : double;
    FIrrigationScale     : double;
    FCatchmentRef        : integer;
    FRainCoef            : double;
    FDamLevelsFileName   : string;
    FXCoord              : double;
    FYCoord              : double;
    FGroudID             : integer;
    FNaturalInflowChannel: integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function ValidateReservoirName (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateReservoirNumber (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateReservoirPenaltyStructure (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateReservoirPenaltyValue (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateReservoirPriority (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateCatchmentRefNumber (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateSumDrainageScale (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateSumAfforestationScale (AErrorMessages : TStrings) : boolean; virtual;
    function ValidateSumIrrigationScale(AErrorMessages : TStrings) : boolean; virtual;
    function ValidateSumUrbanRunOff(AErrorMessages : TStrings) : boolean; virtual;
    function ValidateAreawhenFull(AErrorMessages: TStrings): boolean; virtual;
    function ValidatePointsCount(AErrorMessages: TStrings): boolean; virtual;
    function ValidateRainCoef(AErrorMessages: TStrings): boolean; virtual;
    function ValidateXCoord(AErrorMessages: TStrings): boolean; virtual;
    function ValidateYCoord(AErrorMessages: TStrings): boolean; virtual;
    function ValidateDamLevelFileName(AErrorMessages: TStrings): boolean; virtual;
    function CopyFrom (AResID  : integer; ASource : TReservoirConfigurationData) : boolean;
  public
    procedure Assign(ASource : TReservoirConfigurationData); virtual;
    function Get_RecordIdentifier: integer;  safecall;
    function Get_NodeType: TNodeType; safecall;
    function Get_ReservoirName: WideString; safecall;
    function Get_ReservoirIdentifier: integer; safecall;
    function Get_PenaltyStructIdentifier: integer; safecall;
    function Get_PointsCount: integer; safecall;
    function Get_IncludeSummary : WideString; safecall;
    function Get_DrainageScale : double; safecall;
    function Get_UrbanRunOff : double; safecall;
    function Get_MaxArea: double; safecall;
    function Get_AreaWhenFull: double; safecall;
    function Get_VolumeWhenFull: double; safecall;
    function Get_MaxVolume: double; safecall;
    function Get_Priority: double; safecall;
    function Get_StatusIndicator: integer; safecall;
    function Get_AfforestationScale: double; safecall;
    function Get_IrrigationScale: double; safecall;
    function Get_CatchmentRef: integer; safecall;
    function Get_RainCoef: double; safecall;
    function Get_DamLevelsFileName: WideString; safecall;
    function Get_XCoord: Double; safecall;
    function Get_YCoord: Double; safecall;
    function Get_GroupID: integer; safecall;
    function Get_NaturalInflowChannel: Integer; safecall;

    procedure Set_ReservoirName(const Value: WideString); safecall;
    procedure Set_DrainageScale(Value: double); safecall;
    procedure Set_AfforestationScale(Value: double); safecall;
    procedure Set_IrrigationScale(Value: double); safecall;
    procedure Set_UrbanRunOff(Value: double); safecall;
    procedure Set_AreaWhenFull(Value: double); safecall;
    procedure Set_VolumeWhenFull(Value: double); safecall;
    procedure Set_CatchmentRef(Value: integer); safecall;
    procedure Set_PointsCount(Value: integer); virtual;
    procedure Set_IncludeSummary(const Value: WideString); safecall;
    procedure Set_PenaltyStructIdentifier(Value: integer); safecall;
    procedure Set_StatusIndicator(Value: integer); safecall;
    procedure Set_Priority(Value: double); safecall;
    procedure Set_RainCoef(Value: double); safecall;
    procedure Set_DamLevelsFileName(const Value: WideString); safecall;
    procedure Set_XCoord(Value: Double); safecall;
    procedure Set_YCoord(Value: Double); safecall;
    procedure Set_GroupID(Value: integer); safecall;

    procedure Set_NaturalInflowChannel(Value: Integer); safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function Initialise: boolean; override;
    function PopulateReservoirConfigurationData(ADataSet: TAbstractModelDataset): boolean;
    function PopulateNodeData(AReservoirIdentifier: integer; AReservoirName: string;ANodeType: TNodeType): boolean;
    function PopulateMaxArea(AMaxArea: double): boolean;
    function PopulateMaxVolume(AMaxVolume: double): boolean;
    procedure InitialiseNewConfigurationData(ANewRecordId, ANewReservoirNumber: integer;ANodeType: TNodeType);
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property RecordIdentifier: Integer read Get_RecordIdentifier;
    property NodeType: TNodeType read Get_NodeType;
    property PenaltyStructIdentifier: Integer read Get_PenaltyStructIdentifier write Set_PenaltyStructIdentifier;
    property PointsCount: Integer read Get_PointsCount;
    property StatusIndicator: Integer read Get_StatusIndicator write Set_StatusIndicator;
    property ReservoirName: WideString read Get_ReservoirName write Set_ReservoirName;
    property ReservoirIdentifier: Integer read Get_ReservoirIdentifier;
    property IncludeSummary: WideString read Get_IncludeSummary write Set_IncludeSummary;
    property DrainageScale: Double read Get_DrainageScale write Set_DrainageScale;
    property AfforestationScale: Double read Get_AfforestationScale write Set_AfforestationScale;
    property IrrigationScale: Double read Get_IrrigationScale write Set_IrrigationScale;
    property UrbanRunOff: Double read Get_UrbanRunOff write Set_UrbanRunOff;
    property CatchmentRef: Integer read Get_CatchmentRef write Set_CatchmentRef;
    property MaxArea: Double read Get_MaxArea;
    property AreaWhenFull: Double read Get_AreaWhenFull write Set_AreaWhenFull;
    property VolumeWhenFull: Double read Get_VolumeWhenFull write Set_VolumeWhenFull;
    property MaxVolume: Double read Get_MaxVolume;
    property Priority: Double read Get_Priority write Set_Priority;
    property RainCoef : Double read Get_RainCoef write Set_RainCoef;
    property DamLevelsFileName : WideString read Get_DamLevelsFileName write Set_DamLevelsFileName;
    property XCoord: Double read Get_XCoord write Set_XCoord;
    property YCoord: Double read Get_YCoord write Set_YCoord;
    property GroupID: Integer read Get_GroupID write Set_GroupID;
    property NaturalInflowChannel: Integer read Get_NaturalInflowChannel write Set_NaturalInflowChannel;
  end;

  TReservoirTimeControl = class(TAbstractAppObject,IReservoirTimeControl)
  protected
    FReservoirNumber           : integer;
    FReservoirStartYear        : integer;
    FReservoirStartMonth       : integer;
    FReservoirEndYear          : integer;
    FReservoirEndMonth         : integer;
    FReservoirEconomicLife     : integer;
    FReservoirCapitalCost      : Double;
    FReservoirOMCost           : Double;
    FReservoirCostSchedule     : WideString;
    FBaseNodeNumber            : integer;
    FReplacements              : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateStartYear (AErrorMessages : TStrings) : WordBool;
    function ValidateStartMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateEndYear (AErrorMessages : TStrings) : WordBool;
    function ValidateEndMonth (AErrorMessages : TStrings) : WordBool;
    function ValidateEconomicLife (AErrorMessages : TStrings) : WordBool;
    function ValidateCapitalCost (AErrorMessages : TStrings) : WordBool;
    function ValidateOMCost (AErrorMessages : TStrings) : WordBool;
    function ValidateYearsToConstruct (AErrorMessages : TStrings) : WordBool;
    function ValidateCostSchedule (AErrorMessages : TStrings): WordBool;
    function ValidateBaseNodeNumber (AErrorMessages : TStrings): WordBool;
    function ValidateReplacements(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;

  public
    procedure Assign(ASource : TReservoirTimeControl); virtual;
    function Initialise: boolean; override;
    function Populate (AReservoirNumber       : integer;
                       ABaseNodeNumber        : integer;
                       AReservoirStartYear    : integer;
                       AReservoirStartMonth   : integer;
                       AReservoirEndYear      : integer;
                       AReservoirEndMonth     : integer;
                       AReservoirEconomicLife : integer;
                       AReservoirCapitalCost  : double;
                       AReservoirOMCost       : double;
                       AReservoirCostSchedule     : string) : Boolean;

    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(Value: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(Value: Integer); safecall;
    function Get_EndMonth: Integer; safecall;
    procedure Set_EndMonth(Value: Integer); safecall;
    function Get_EconomicLife: Integer; safecall;
    procedure Set_EconomicLife(Value: Integer); safecall;
    function Get_CapitalCost: double; safecall;
    procedure Set_CapitalCost(Value: double); safecall;
    function Get_OMCost: double; safecall;
    procedure Set_OMCost(Value: double); safecall;
    function Get_YearsToConstruct: Integer; safecall;
    procedure Set_YearsToConstruct(Value: Integer); safecall;
    function Get_CostSchedule: WideString; safecall;
    procedure Set_CostSchedule(const Value: WideString); safecall;
    function Get_CostScheduleByIndex (AIndex : integer) : double; safecall;
    procedure Set_CostScheduleByIndex (AIndex : integer;
                                       AValue : double); safecall;
    function Get_BaseNodeNumber: Integer; safecall;
    procedure Set_BaseNodeNumber(AValue: Integer); safecall;
    function Get_ReservoirNumber: Integer; safecall;
    function Get_Replacements: WideString; safecall;

    procedure AddReplacement(AResNr: Integer); safecall;
    procedure DeleteReplacement(AResNr: Integer); safecall;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;
    property EndYear: Integer read Get_EndYear write Set_EndYear;
    property EndMonth: Integer read Get_EndMonth write Set_EndMonth;
    property EconomicLife: Integer read Get_EconomicLife write Set_EconomicLife;
    property CapitalCost: double read Get_CapitalCost write Set_CapitalCost;
    property OMCost: double read Get_OMCost write Set_OMCost;
    property YearsToConstruct: Integer read Get_YearsToConstruct write Set_YearsToConstruct;
    property CostSchedule: WideString read Get_CostSchedule write Set_CostSchedule;
    property CostScheduleByIndex[AIndex : integer] : double read Get_CostScheduleByIndex write Set_CostScheduleByIndex;
    property BaseNodeNumber: Integer read Get_BaseNodeNumber write Set_BaseNodeNumber;
    property ReservoirNumber: Integer read Get_ReservoirNumber;
  end;

  TReservoirData = class(TAbstractAppObject,IReservoirData)
  protected
    FReservoirEvaporationsData    : TReservoirEvaporationsData;
    FReservoirElevationsData      : TReservoirElevationsData;
    FReservoirVolumesData         : TReservoirVolumesData;
    FReservoirAreasData           : TReservoirAreasData;
    FReservoirConfigurationData   : TReservoirConfigurationData;
    FReservoirZoneElevationsData  : TReservoirZoneElevationData;
    FReservoirTimeControl         : TReservoirTimeControl;
    FDownStreamPowerChannels      : TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetReservoirConfigurationDataCast: TReservoirConfigurationData;
    //function GetReservoirPenaltyStructureCast: TReservoirPenaltyStructureData;
    function GetReservoirZoneElevationsDataCast: TReservoirZoneElevationData;
    function GetReservoirEvaporationsDataCast: TReservoirEvaporationsData;
    function GetReservoirElevationsDataCast: TReservoirElevationsData;
    function GetReservoirVolumesDataCast: TReservoirVolumesData;
    function GetReservoirAreasDataCast: TReservoirAreasData;
    //procedure Set_FullSupplyLevel(ANewValue: double);

    function ValidateStartingStorageLevel(AErrorMessages: TStrings): boolean; virtual;
    function ValidateFullStorageLevel(AErrorMessages: TStrings): boolean; virtual;
    function ValidateDeadStorageLevel(AErrorMessages: TStrings): boolean; virtual;
    function ValidateBottomOfReservoir(AErrorMessages: TStrings): boolean; virtual;
    function ValidateElevation (AErrorMessages : TStrings; AErrorColumns: TStringList) : Boolean; virtual;
    function ValidateVolume (AErrorMessages : TStrings; AErrorColumns: TStringList) : Boolean; virtual;
    function ValidateSurfaceArea (AErrorMessages : TStrings; AErrorColumns: TStringList) : Boolean; virtual;
    function ValidateMonthlyZoneElevation (AErrorMessages : TStrings;
                                           AErrorColumns  : TStringList) : Boolean;
    function ValidateEvaporation (AErrorMessages : TStrings;
                                  AErrorColumns  : TStringList) : Boolean;
  public
    procedure Assign(ASource : TReservoirData); virtual;
    function PopulateDownStreamPowerChannels (AChannels : TStrings): Boolean;
    function Get_ReservoirEvaporationsData :IReservoirEvaporationsData;  safecall;
    function Get_ReservoirElevationsData :IReservoirElevationsData;  safecall;
    function Get_ReservoirVolumesData :IReservoirVolumeData;  safecall;
    function Get_ReservoirAreasData :IReservoirAreaData;  safecall;
    function Get_ReservoirConfigurationData: IReservoirConfigurationData;  safecall;
    function Get_ReservoirZoneElevationsData: IReservoirZoneElevationsData;  safecall;
    function Get_ReservoirPenaltyStructureData: IReservoirPenalty;  safecall;
    function Get_TimeControl: IReservoirTimeControl; safecall;
    function DownStreamPowerChannels: WideString; safecall;
    function DownStreamPowerChannelCount: Integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Clone : IReservoirData; safecall;
    function DeletePhysicalCharacteristicsRow(AIndex: integer): WordBool;safecall;
    function InsertPhysicalCharacteristicsRow(AIndex: integer;AElevation,AVolume,AArea: double): WordBool;safecall;
    function CreateTimeControl : TReservoirTimeControl;
    function DeleteTimeControl : Boolean;
    function NewTimeControl: IReservoirTimeControl; safecall;
    function RemoveTimeControl : WordBool; safecall;

    function GetReservoirVolumeByElevation(AElevation:Double)       : Double; safecall;
    function GetReservoirElevationByVolume(AVolume:Double)          : Double; safecall;
    function GetReservoirElevationBySurfaceArea(AArea:Double)       : Double; safecall;
    function GetReservoirSurfaceAreaByElevation(AElevation:Double)  : Double; safecall;

    function GetReservoirSurfaceAreaByVolume(AVolume:Double)        : Double; safecall;
    function GetReservoirVolumeBySurfaceArea(AArea:Double)          : Double; safecall;
    function CreateNaturalInflowChannel: integer;

    procedure InitialiseNewReservoir(ANewRecordId, ANewReservoirNumber: integer;ANodeType: TNodeType);
    //procedure PopulateReservoirPenaltyStructureIdentifier(APenaltyStructIdentifier: integer); override;
    function Initialise: boolean; override;
    function PopulateReservoirConfigurationData(ADataSet: TAbstractModelDataset): boolean;
    function RecalculateAreaWhenFull: WordBool; safecall;
    function RecalculateVolumeWhenFull: WordBool;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property CastReservoirConfigurationData: TReservoirConfigurationData read GetReservoirConfigurationDataCast;
    //property CastReservoirPenaltyStructure: TReservoirPenaltyStructureData read GetReservoirPenaltyStructureCast;
    property CastReservoirZoneElevationsData: TReservoirZoneElevationData read GetReservoirZoneElevationsDataCast;
    property CastReservoirEvaporationsData: TReservoirEvaporationsData read GetReservoirEvaporationsDataCast;
    property CastReservoirElevationsData: TReservoirElevationsData read GetReservoirElevationsDataCast;
    property CastReservoirVolumesData: TReservoirVolumesData read GetReservoirVolumesDataCast;
    property CastReservoirAreasData: TReservoirAreasData read GetReservoirAreasDataCast;

    property ReservoirConfigurationData: IReservoirConfigurationData read Get_ReservoirConfigurationData;
    property ReservoirPenaltyStructureData: IReservoirPenalty read Get_ReservoirPenaltyStructureData;
    property ReservoirZoneElevationsData: IReservoirZoneElevationsData read Get_ReservoirZoneElevationsData;
    property ReservoirEvaporationsData: IReservoirEvaporationsData read Get_ReservoirEvaporationsData;
    property ReservoirElevationsData: IReservoirElevationsData read Get_ReservoirElevationsData;
    property ReservoirVolumesData: IReservoirVolumeData read Get_ReservoirVolumesData;
    property ReservoirAreasData: IReservoirAreaData read Get_ReservoirAreasData;
    property TimeControl : IReservoirTimeControl read Get_TimeControl;
  end;

  TReservoirDataList = class(TAbstractAppObject,IReservoirDataList)
  protected
    FReservoirData,
    FNodeWithInflowData,
    FNodeWithoutInflowData,
    FIrrigationNodeData,
    //FWetlandData,
    FReservoirAndNodesData: TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetIrrigationNodeByIdentifier( ANodeIdentifier: integer): TReservoirData;
    function GetIrrigationNodeByIndex(ANodeIndex: integer): TReservoirData;
    function GetIrrigationNodesCount: integer;

    function ReservoirHasNoValidCatchmentRef(AReservoir: TReservoirData): boolean;
    function GetReservoirByIdentifierCast(AReservoirIdentifier: integer): TReservoirData;
    function GetReservoirByIndexCast(AReservoirIndex: integer): TReservoirData;
    function GetReservoirByRecordIdentifier(ARecordIdentifier: integer): TReservoirData;
    function GetReservoirByNameCast(AReservoirName: string): TReservoirData;
    function GetNodeWithInflowByIdentifierCast(ANodeIdentifier: integer): TReservoirData;
    function GetNodeWithoutInflowByIdentifierCast(ANodeIdentifier: integer): TReservoirData;
    function GetReservoirOrNodeByIdentifierCast(AReservoirOrNodeIdentifier: Integer): TReservoirData;
    function GetReservoirOrNodeByRecordIdentifierCast(AReservoirOrNodeRecordIdentifier: Integer): TReservoirData;
    function GetReservoirOrNodeByIndexCast(AReservoirOrNodeIndex: Integer): TReservoirData;
    function GetNodeWithInflowByIndexCast(AIndex: Integer): TReservoirData;
    function GetCastReservoirByReservoirGroupID(AGroupID: integer): TReservoirData;

    function GetHighestResevoirOrNodeNumber: integer;
    function GetHighestResevoirOrNodeRecordIdentifier: integer;
    function CastCreateReservoir(ANodeType: TNodeType = ntReservoir) : TReservoirData;
  public
    function CopyCreate(AReservoirNumber : integer) : IReservoirData; safecall;
    function Get_ReservoirByIdentifier(AReservoirIdentifier: integer): IReservoirData; safecall;
    function Get_ReservoirByIndex(AReservoirIndex: integer): IReservoirData; safecall;
    function Get_NodeWithInflowByIdentifier(ANodeIdentifier: integer): IReservoirData; safecall;
    function Get_NodeWithInflowByIndex(ANodeIndex: integer): IReservoirData; safecall;
    function Get_NodeWithoutInflowByIdentifier(ANodeIdentifier: integer): IReservoirData; safecall;
    function Get_NodeWithoutInflowByIndex(ANodeIndex: integer): IReservoirData; safecall;
    function Get_IrrigationNodeByIdentifier(ANodeIdentifier: integer): IReservoirData; safecall;
    function Get_IrrigationNodeByIndex(ANodeIndex: integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByIdentifier(AReservoirOrNodeIdentifier: integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByIndex(AReservoirOrNodeIndex: integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByID(AIdentifier: Integer): IReservoirData; safecall;
    function Get_ReservoirOrNodeByName(const AName: WideString): IReservoirData; safecall;
    //function Get_WetlandByIndex(AWetlandByIndex: integer): IReservoirData; safecall;
    //function Get_WetlandByByID(AIdentifier: Integer): IReservoirData; safecall;
    function Get_ReservoirCount: integer;safecall;
    function Get_NodesWithInflowCount: integer; safecall;
    function Get_NodesWithoutInflowCount: integer; safecall;
    function Get_IrrigationNodesCount: integer; safecall;
    //function Get_WetlandsCount: integer; safecall;
    function Get_ReservoirAndNodesCount: integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function CreateReservoir(ANodeType: TNodeType) : IReservoirData; safecall;
    function DeleteReservoir(AReservoirNumber: integer) : WordBool; safecall;
    function CreateNodeWithInflow(ANodeType: TNodeType) : IReservoirData; safecall;
    function DeleteNodeWithInflow(ANodeNumber: integer) : WordBool; safecall;
    function CreateNodeWithoutInflow(ANodeType: TNodeType) : IReservoirData; safecall;
    function DeleteNodeWithoutInflow(ANodeNumber: integer) : WordBool; safecall;
    function CreateIrrigationNode : IReservoirData; safecall;
    function CreateNewIrrigationNode : IReservoirData;
    function DeleteIrrigationNode(ANodeNumber: integer) : boolean; safecall;
    function CreateReservoirsZone(AZoneLevel: integer; ACurrentReservoirNumber: integer = 0):IReservoirZoneElevationsData ; safecall;
    function DeleteReservoirsZone(AZoneLevel: integer;ADeleteAction: TDeleteAction): boolean ; safecall;
    function NodeHasInflow (ANodeNr : integer) : WordBool; safecall;
    function Initialise: boolean; override;
    procedure AddZeroNode;
    procedure AddReservoirOrNodeToList(AReservoirOrNode: TReservoirData);
    procedure RemoveReservoirOrNodeFromList(AReservoirOrNode: TReservoirData);

    //procedure AddInferedNode(ANodeNumber: integer);

    function ReservoirCountPerPenaltyStructure(APenaltyID:integer): integer; virtual;
    function PenaltyStructureAdded(APenaltyID: integer): boolean; virtual;
    function PenaltyStructureDeleted(APenaltyID: integer): boolean; virtual;

    function GetReservoirsAndNodesPerCatchmentRef(ACatchmentRefNumber: integer; AReservoirAndNodesList: TObjectList): boolean; virtual;
    function GetReservoirsAndNodesOutSideCatchmentRef(ACatchmentRefNumber: integer; AReservoirAndNodesList: TObjectList): boolean; virtual;
    function GetReservoirsPerPenaltyStructure(APenaltyStructureID: integer; AReservoirList: TObjectList): boolean; virtual;
    function GetReservoirsWithNoPenaltyStructure(AReservoirList: TObjectList): boolean; virtual;
    function GetReservoirsWithNoCatchmentRef(AReservoirList: TObjectList): boolean; virtual;
    function UpdateReservoirName(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateDrainageScalingFactor(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateAfforestationScalingFactor(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateIrrigationScalingFactor(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateAreaWhenFull(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateCatchmentReference(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateIncludeSummary(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdatePenaltyStructure(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateFullSupplyLevel(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateStatusIndicator(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateDeadStorageLevel(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateBottomOfReservoirLevel(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateInitialReservoirLevel(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateReservoirEvaporations(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateReservoirVolumes(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateReservoirElevations(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;
    function UpdateReservoirAreas(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;virtual;

    property CastReservoirByIdentifier[AReservoirIdentifier: integer]: TReservoirData read GetReservoirByIdentifierCast;
    property CastReservoirByIndex[AReservoirIndex: integer]: TReservoirData read GetReservoirByIndexCast;
    property CastReservoirByRecordIdentifier[ARecordIdentifier: integer]: TReservoirData read GetReservoirByRecordIdentifier;
    property CastReservoirByName[AReservoirName: string]: TReservoirData read GetReservoirByNameCast;
    property CastReservoirOrNodeByIndex[AReservoirOrNodeIndex: Integer]: TReservoirData read GetReservoirOrNodeByIndexCast;
    property CastReservoirOrNodeByIdentifier[AReservoirOrNodeIdentifier: Integer]: TReservoirData read GetReservoirOrNodeByIdentifierCast;
    property CastReservoirOrNodeByRecordIdentifier[AReservoirOrNodeRecordIdentifier: Integer]: TReservoirData read GetReservoirOrNodeByRecordIdentifierCast;
    property CastNodeWithInflowByIndex[AIndex: Integer]: TReservoirData read GetNodeWithInflowByIndexCast;
    property CastReservoirByReservoirGroupID[AIndex: integer]: TReservoirData read GetCastReservoirByReservoirGroupID;

    property ReservoirCount: Integer read Get_ReservoirCount;
    property ReservoirAndNodesCount: Integer read Get_ReservoirAndNodesCount;
    property NodesWithInflowCount: Integer read Get_NodesWithInflowCount;
    property NodesWithoutInflowCount: Integer read Get_NodesWithoutInflowCount;
    property IrrigationNodesCount: Integer read Get_IrrigationNodesCount;
    property ReservoirByIdentifier[AReservoirIdentifier: Integer]: IReservoirData read Get_ReservoirByIdentifier;
    property ReservoirByIndex[AReservoirIndex: Integer]: IReservoirData read Get_ReservoirByIndex;
    property NodeWithInflowByIdentifier[ANodeIdentifier: Integer]: IReservoirData read Get_NodeWithInflowByIdentifier;
    property NodeWithInflowByIndex[ANodeIndex: Integer]: IReservoirData read Get_NodeWithInflowByIndex;
    property NodeWithoutInflowByIdentifier[ANodeIdentifier: Integer]: IReservoirData read Get_NodeWithoutInflowByIdentifier;
    property NodeWithoutInflowByIndex[ANodeIndex: Integer]: IReservoirData read Get_NodeWithoutInflowByIndex;
    property IrrigationNodeByIdentifier[ANodeIdentifier: Integer]: IReservoirData read Get_IrrigationNodeByIdentifier;
    property IrrigationNodeByIndex[ANodeIndex: Integer]: IReservoirData read Get_IrrigationNodeByIndex;


    property ReservoirOrNodeByIdentifier[AReservoirOrNodeIdentifier: Integer]: IReservoirData read Get_ReservoirOrNodeByIdentifier;
    property ReservoirOrNodeByIndex[AReservoirOrNodeIndex: Integer]: IReservoirData read Get_ReservoirOrNodeByIndex;
    property ReservoirOrNodeByID[AIndentifier: Integer]: IReservoirData read Get_ReservoirOrNodeByID;
  end;
procedure AddErrors (var AErrors : WideString; AErrorList  : TStringList; AErrorCols  : TStringList);

implementation

uses
  System.Types,
  System.UITypes,
  Math,
  VCL.Controls,
  VCL.Dialogs,
  SysUtils,
  UUtilities,
  UConstants,
  UParameterData,
  UAbstractFileNamesObject,
  UMainMenuEventType,
  UYieldModelDataObject,
  UDDTSDataObject,
  UReservoirDataSQLAgent,
  UReservoirTimeControlSQLAgent,
  UErrorHandlingOperations, UFileNames, DB;

const
  CReservoirNameFieldIdentifier = 'ReservoirName';
  
procedure AddErrors (var AErrors : WideString;AErrorList  : TStringList;AErrorCols  : TStringList);
const OPNAME = 'UReservoirData.AddErrors';
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

 { TReservoirConfigurationData }

procedure TReservoirConfigurationData.CreateMemberObjects;
const OPNAME = 'TReservoirConfigurationData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirConfigurationData.DestroyMemberObjects;
const OPNAME = 'TReservoirConfigurationData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Initialise: boolean;
const OPNAME = 'TReservoirConfigurationData.Initialise';
begin
  Result := inherited Initialise;
  try
    FRecordIdentifier    := -1;
    FReservoirIdentifier := -1;
    FPointsCount         := -1;
    FStatusIndicator     := -1;
    FPriority            := 0.0;
    FNodeType            := ntUnknown;
    FReservoirName       := '';
    FPenaltyStruct       := -1;
    FIncludeSummary      := '';
    FDrainageScale       := 0.0;
    FMaxArea             := 0.0;
    FMaxVolume           := 0.0;
    FAfforestationScale  := 0.0;
    FIrrigationScale     := 0.0;
    FAreaWhenFull        := 0.0;
    FVolumeWhenFull      := 0.0;
    FRainCoef            := 0.0;
    FUrbanRunOff         := 0.0;
    FDamLevelsFileName   := '';
    FXCoord              := 0.0;
    FYCoord              := 0.0;
    FNaturalInflowChannel:= 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_ReservoirName(const Value: WideString);
const OPNAME = 'TReservoirConfigurationData.Set_ReservoirName';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirName', Value, FReservoirName, LContextData) then
        begin
          LOldValue := FReservoirName;
          FReservoirName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirName',LOldValue,Value);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_DrainageScale(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_DrainageScale';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    if(FNodeType in [ntMineUndergroundDam]) then
      Value := 0.0
    else
      Value := Value/100.0;
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DrainageScale',  FloatToStr(Value),FloatToStr(FDrainageScale), LContextData) then
        begin
          LPrevValue := FDrainageScale;
          FDrainageScale := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DrainageScale',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_AfforestationScale(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_AfforestationScale';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    if(FNodeType in [ntMineUndergroundDam,ntGroundWater]) then
      Value := 0.0
    else
      Value := Value/100.0;
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'AfforestationScale',  FloatToStr(Value),FloatToStr(FAfforestationScale), LContextData) then
        begin
          LPrevValue := FAfforestationScale;
          FAfforestationScale := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AfforestationScale',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_IrrigationScale(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_IrrigationScale';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    if(FNodeType in [ntMineUndergroundDam,ntGroundWater]) then
      Value := 0.0
    else
      Value := Value/100.0;
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'IrrigationScale',  FloatToStr(Value),FloatToStr(FIrrigationScale), LContextData) then
        begin
          LPrevValue := FIrrigationScale;
          FIrrigationScale := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IrrigationScale',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_UrbanRunOff(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_UrbanRunOff';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  Value := Value/100.0;
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'UrbanRunOff',  FloatToStr(Value),FloatToStr(FUrbanRunOff), LContextData) then
        begin
          LPrevValue := FUrbanRunOff;
          FUrbanRunOff := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UrbanRunOff',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;    

procedure TReservoirConfigurationData.Set_AreaWhenFull(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_AreaWhenFull';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'AreaFull',  FloatToStr(Value),FloatToStr(FAreaWhenFull), LContextData) then
        begin
          LPrevValue := FAreaWhenFull;
          FAreaWhenFull := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AreaFull',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_VolumeWhenFull (Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_VolumeWhenFull';
begin
  try
    FVolumeWhenFull := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_CatchmentRef(Value: integer);
const OPNAME = 'TReservoirConfigurationData.Set_CatchmentRef';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LIndex       : integer;
  LPrevValue   : integer;
  LParamReference: TParamReference;
  LMesg,
  LFileName,
  LFileNamePrefix: string;
  LFileNamesList: TStringList;
  LHydrologyFile: TAbstractModelFileName;
  LNewFileIndex: integer;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'CatchmentRef',  IntToStr(Value),FloatToStr(FCatchmentRef), LContextData) then
        begin
          LPrevValue := FCatchmentRef;
          FCatchmentRef := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CatchmentRef',IntToStr(LPrevValue),IntToStr(Value));
          LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).CastCastParameterData.CastReferenceDataByCatchNumber[Value];
          if(LParamReference <> nil) then
          begin
            LFileNamePrefix := LParamReference.FileReference;
            LFileNamesList  := TStringList.Create;
            try
              TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFilesFromPrefix(LFileNamePrefix,'',LFileNamesList);
              if(LFileNamesList.Count = 0) then
              begin
                GetHydrologyFileNames(LFileNamePrefix+'.*',LFileNamesList);
                if(LFileNamesList.Count > 0) then
                begin
                  LMesg := FAppModules.Language.GetString('HydrologyFile.ImportFiles');
                  if (MessageDlg(LMesg,mtConfirmation,mbOKCancel,0) = mrOk) then
                  begin
                    LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.Count;
                    for LIndex := 0 to LFileNamesList.Count -1 do
                    begin
                      LNewFileIndex := LNewFileIndex + 1;
                      LFileName := LFileNamesList[LIndex];
                      TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.
                        AddHydrologyFileName(LNewFileIndex,LFileName,False,0.0,FileLastWriteDate(LFileName));
                      LHydrologyFile := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);
                      FAppModules.Model.ProcessEvent(CmeImportHydrologyFile,LHydrologyFile);
                    end;
                  end;
                end
                else
                begin
                  LMesg := FAppModules.Language.GetString('HydrologyFile.NoImportFile');
                  ShowMessage(LMesg);
                end;
              end;
            finally
              LFileNamesList.Free;
            end;
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TReservoirConfigurationData.Set_IncludeSummary(const Value: WideString);
const OPNAME = 'TReservoirConfigurationData.Set_IncludeSummary';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: string;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'IncludeSummary', Value, FIncludeSummary, LContextData) then
        begin
          LPrevValue := FIncludeSummary;
          FIncludeSummary := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'IncludeSummary',LPrevValue,Value);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_PenaltyStructIdentifier(Value: integer);
const OPNAME = 'TReservoirConfigurationData.Set_PenaltyStructIdentifier';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: integer;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), '');
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PenaltyStruct', IntToStr(Value), IntToStr(FPenaltyStruct), LContextData) then
        begin
          LPrevValue := FPenaltyStruct;
          FPenaltyStruct := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PenaltyStruct',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_StatusIndicator(Value: integer);
const OPNAME = 'TReservoirConfigurationData.Set_StatusIndicator';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: integer;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadNodesDetailsContextData(LContextData,IntToStr(FReservoirIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'StatusIndicator', IntToStr(Value), IntToStr(FStatusIndicator), LContextData) then
        begin
          LPrevValue := FStatusIndicator;
          FStatusIndicator := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StatusIndicator',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_Priority(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_Priority';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadNodesDetailsContextData(LContextData,IntToStr(FReservoirIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirPriority',  FloatToStr(Value),FloatToStr(FPriority), LContextData) then
        begin
          LPrevValue := FPriority;
          FPriority := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirPriority',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_RainCoef(Value: double);
const OPNAME = 'TReservoirConfigurationData.Set_RainCoef';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'RainCoef',  FloatToStr(Value),FloatToStr(FRainCoef), LContextData) then
        begin
          LPrevValue := FRainCoef;
          FRainCoef  := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RainCoef',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.Get_AfforestationScale: double;
const OPNAME = 'TReservoirConfigurationData.Get_AfforestationScale';
begin
  Result := 0.0;
  try
    Result := FAfforestationScale* 100.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_AreaWhenFull: double;
const OPNAME = 'TReservoirConfigurationData.Get_AreaWhenFull';
begin
  Result := 0.0;
  try
    Result := FAreaWhenFull;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_VolumeWhenFull: double;
const OPNAME = 'TReservoirConfigurationData.Get_VolumeWhenFull';
begin
  Result := 0.0;
  try
    Result := FVolumeWhenFull;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_CatchmentRef: integer;
const OPNAME = 'TReservoirConfigurationData.Get_CatchmentRef';
begin
  Result := 0;
  try
    Result := FCatchmentRef;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_DrainageScale: double;
const OPNAME = 'TReservoirConfigurationData.Get_DrainageScale';
begin
  Result := 0.0;
  try
    Result := FDrainageScale * 100.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_UrbanRunOff: double;
const OPNAME = 'TReservoirConfigurationData.Get_UrbanRunOff';
begin
  Result := 0.0;
  try
    Result := FUrbanRunOff * 100.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirConfigurationData.Get_IncludeSummary: WideString;
const OPNAME = 'TReservoirConfigurationData.Get_IncludeSummary';
begin
  Result := '';
  try
    Result := FIncludeSummary;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_IrrigationScale: double;
const OPNAME = 'TReservoirConfigurationData.Get_IrrigationScale';
begin
  Result := 0.0;
  try
    Result := FIrrigationScale*100.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_MaxArea: double;
const OPNAME = 'TReservoirConfigurationData.Get_MaxArea';
begin
  Result := 0.0;
  try
    Result := FMaxArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_MaxVolume: double;
const OPNAME = 'TReservoirConfigurationData.Get_MaxVolume';
begin
  Result := 0.0;
  try
    Result := FMaxVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_PenaltyStructIdentifier: integer;
const OPNAME = 'TReservoirConfigurationData.Get_PenaltyStructIdentifier';
begin
  Result := 0;
  try
    Result := FPenaltyStruct;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_PointsCount: integer;
const OPNAME = 'TReservoirConfigurationData.Get_PointsCount';
begin
  Result := 0;
  try
    Result := FPointsCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_Priority: double;
const OPNAME = 'TReservoirConfigurationData.Get_Priority';
begin
  Result := 0.0;
  try
    Result := FPriority;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_RecordIdentifier: integer;
const OPNAME = 'TReservoirConfigurationData.Get_RecordIdentifier';
begin
  Result := 0;
  try
    Result := FRecordIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_ReservoirIdentifier: integer;
const OPNAME = 'TReservoirConfigurationData.Get_ReservoirIdentifier';
begin
  Result := 0;
  try
    Result := FReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_ReservoirName: WideString;
const OPNAME = 'TReservoirConfigurationData.Get_ReservoirName';
begin
  Result := '';
  try
    Result := FReservoirName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_NodeType: TNodeType;
const OPNAME = 'TReservoirConfigurationData.Get_NodeType';
begin
  Result := ntUnknown;
  try
    Result := FNodeType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_StatusIndicator: integer;
const OPNAME = 'TReservoirConfigurationData.Get_StatusIndicator';
begin
  Result := 0;
  try
    Result := FStatusIndicator;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_RainCoef: double;
const OPNAME = 'TReservoirConfigurationData.Get_RainCoef';
begin
  Result := 0.0;
  try
    Result := FRainCoef;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.PopulateReservoirConfigurationData(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirConfigurationData.PopulateReservoirConfigurationData';
begin
  Result := False;
  try
    FRecordIdentifier     := ADataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
    FReservoirName        := Trim(ADataset.DataSet.FieldByName('ReservoirName').AsString);
    FPenaltyStruct        := ADataset.DataSet.FieldByName('PenaltyStruct').AsInteger;
    FReservoirIdentifier  := ADataset.DataSet.FieldByName('ReservoirIdentifier').AsInteger;
    FIncludeSummary       := Trim(ADataset.DataSet.FieldByName('IncludeSummary').AsString);
    FPointsCount          := ADataset.DataSet.FieldByName('PointsCount').AsInteger;
    FStatusIndicator      := ADataset.DataSet.FieldByName('StatusIndicator').AsInteger;
    FDrainageScale        := ADataset.DataSet.FieldByName('DrainageScale').AsFloat;
    FAfforestationScale   := ADataset.DataSet.FieldByName('AfforestationScale').AsFloat;
    FIrrigationScale      := ADataset.DataSet.FieldByName('IrrigationScale').AsFloat;
    FUrbanRunOff          := ADataSet.DataSet.FieldByName('UrbanRunoff').AsFloat;
    FCatchmentRef         := ADataset.DataSet.FieldByName('CatchmentRef').AsInteger;
    FPriority             := ADataset.DataSet.FieldByName('ReservoirPriority').AsFloat;
    FRainCoef             := ADataSet.DataSet.FieldByName('RainCoef').AsFloat;
    FNodeType             := TNodeType(ADataset.DataSet.FieldByName('NodeType').AsInteger);
    FDamLevelsFileName    := Trim(ADataset.DataSet.FieldByName('DamLevelsFileName').AsString);
    FXCoord               := ADataSet.DataSet.FieldByName('XCoord').AsFloat;
    FYCoord               := ADataSet.DataSet.FieldByName('YCoord').AsFloat;
    FGroudID              := ADataSet.DataSet.FieldByName('GroupID').AsInteger;
    FNaturalInflowChannel := ADataSet.DataSet.FieldByName('NaturalInflowChannel').AsInteger;
    if (FNodeType in [ntReservoir,ntWetlandNode,ntMinePolutionControlDam,ntMineUndergroundDam]) then
      FAreaWhenFull := ADataset.DataSet.FieldByName('AreaFull').AsFloat;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirConfigurationData.PopulateNodeData(AReservoirIdentifier: integer; AReservoirName: string;
  ANodeType: TNodeType): boolean;
const OPNAME = 'TReservoirConfigurationData.PopulateNodeData';
begin
  Result := False;
  try
    FReservoirIdentifier := AReservoirIdentifier;
    FReservoirName       := AReservoirName;
    FNodeType            := ANodeType;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirConfigurationData.PopulateMaxArea(AMaxArea: double): boolean;
const OPNAME = 'TReservoirConfigurationData.PopulateMaxArea';
begin
  Result := False;
  try
    FMaxArea := AMaxArea;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirConfigurationData.PopulateMaxVolume(AMaxVolume: double): boolean;
const OPNAME = 'TReservoirConfigurationData.PopulateMaxVolume';
begin
  Result := False;
  try
    FMaxVolume := AMaxVolume;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirConfigurationData.ValidateCatchmentRefNumber(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateCatchmentRefNumber';
var
  lMessage : string;
  lMaxNum  : integer;
begin
  Result := False;
  try
    if not (FNodeType in ReservoirsAndNodeWithInflowSet)  then
      Result := True
    else
    begin
      lMessage := '';
      Result := FAppModules.FieldProperties.ValidateFieldProperty('CatchmentRef', IntToStr(FCatchmentRef),
                lMessage);
      if (NOT Result) then
        AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
      else
      begin
        lMaxNum := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceCount;
        if (FCatchmentRef > lMaxNum) then
        begin
          Result := FALSE;
          lMessage := FAppModules.language.GetString('ContextValidation.InvalidCatchmentRefNumber');
          AErrorMessages.Add('ERROR:' +Format(lMessage, [IntToStr(FCatchmentRef), IntToStr(lMaxNum)]));
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateSumDrainageScale(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateSumDrainageScale';
var
  LCount: integer;
  LReservoir:IReservoirData;
  LTotal: double;
  LReservoirList: TObjectList;
  LMessage : string;
begin
  Result := False;
  try
    if not (FNodeType in ReservoirsAndNodeWithInflowSet)  then
    begin
      Result := True;
      Exit;
    end;
    LReservoirList := TObjectList.Create(False);
    try
      LTotal := 0;
      TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
      CastReservoirList.GetReservoirsAndNodesPerCatchmentRef(
        Self.CatchmentRef,LReservoirList);
      for LCount := 0 to LReservoirList.Count -1 do
      begin
        LReservoir := TReservoirData(LReservoirList.Items[LCount]);
        if (FNodeType in [ntMineUndergroundDam,ntGroundWater])  then
        begin
          if (LReservoir.ReservoirConfigurationData.DrainageScale <> 0.0) then
          begin
            LMessage := FAppModules.Language.GetString('ContextValidation.MineDrainageScale');
            AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
          end;

          if (LReservoir.ReservoirConfigurationData.AfforestationScale <> 0.0) then
          begin
            LMessage := FAppModules.Language.GetString('ContextValidation.MineAfforestationScale');
            AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
          end;

          if (LReservoir.ReservoirConfigurationData.IrrigationScale <> 0.0) then
          begin
            LMessage := FAppModules.Language.GetString('ContextValidation.MineIrrigationScale');
            AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
          end;
        end
        else
        LReservoir := TReservoirData(LReservoirList.Items[LCount]);
        LTotal     := LTotal + LReservoir.ReservoirConfigurationData.DrainageScale;
      end;

      if not (FNodeType in [ntMineUndergroundDam,ntGroundWater])  then
      begin
        if not Result then
          Result := ((100.0 - LTotal) < 0.0001) and
                    ((100.0 - LTotal) > -0.0001);
        if not Result then
        begin
          LMessage := FAppModules.Language.GetString('ContextValidation.SumDrainageScale');
          AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
        end;
      end;
      Result := True;
    finally
      LReservoirList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateSumIrrigationScale(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateSumIrrigationScale';
var
  LCount: integer;
  LReservoir:IReservoirData;
  LTotal: double;
  LReservoirList: TObjectList;
  LMessage : string;
begin
  Result := False;
  try
    if not (FNodeType in ReservoirsAndNodeWithInflowSet)  then
    begin
      Result := True;
      Exit;
    end;

    LReservoirList := TObjectList.Create(False);
    try
      LTotal := 0;
      TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
      CastReservoirList.GetReservoirsAndNodesPerCatchmentRef(
      Self.CatchmentRef,LReservoirList);
      for LCount := 0 to LReservoirList.Count -1 do
      begin
        LReservoir := TReservoirData(LReservoirList.Items[LCount]);
        LTotal     := LTotal + LReservoir.ReservoirConfigurationData.IrrigationScale;
      end;
      Result := (LTotal < 0.0001) and
                (LTotal > -0.0001);
      if not Result then
        Result := ((100.0 - LTotal) < 0.0001) and
                  ((100.0 - LTotal) > -0.0001);
      if not Result then
      begin
        LMessage := FAppModules.Language.GetString('ContextValidation.SumIrrigationScale');
        AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
      end;
      Result := True;
    finally
      LReservoirList.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateSumUrbanRunOff(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateSumUrbanRunOff';
var
  LCount         : integer;
  LReservoir     :IReservoirData;
  LTotal         : double;
  LReservoirList : TObjectList;
  LMessage       : string;
begin
  Result := False;
  try
    if not (FNodeType in ReservoirsAndNodeWithInflowSet)  then
    begin
      Result := True;
      Exit;
    end;

    LReservoirList := TObjectList.Create(False);
    try
      LTotal := 0;
      TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
      CastReservoirList.GetReservoirsAndNodesPerCatchmentRef(
      Self.CatchmentRef,LReservoirList);
      for LCount := 0 to LReservoirList.Count -1 do
      begin
        LReservoir := TReservoirData(LReservoirList.Items[LCount]);
        LTotal     := LTotal + LReservoir.ReservoirConfigurationData.UrbanRunOff;
      end;
      Result := (LTotal < 0.0001) and
                (LTotal > -0.0001);
      if not Result then
        Result := ((100.0 - LTotal) < 0.0001) and
                  ((100.0 - LTotal) > -0.0001);
      if not Result then
      begin
        LMessage := FAppModules.Language.GetString('ContextValidation.UrbanRunOff');
        AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
      end;
      Result := True;
    finally
      LReservoirList.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateSumAfforestationScale(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateSumAfforestationScale';
var
  LCount: integer;
  LReservoir:IReservoirData;
  LTotal: double;
  LReservoirList: TObjectList;
  LMessage : string;
begin
  Result := False;
  try
    if not (FNodeType in ReservoirsAndNodeWithInflowSet)  then
    begin
      Result := True;
      Exit;
    end;

    LReservoirList := TObjectList.Create(False);
    try
      LTotal := 0;
      TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
      CastReservoirList.GetReservoirsAndNodesPerCatchmentRef(
        Self.CatchmentRef,LReservoirList);
      for LCount := 0 to LReservoirList.Count -1 do
      begin
        LReservoir := TReservoirData(LReservoirList.Items[LCount]);
        LTotal     := LTotal + LReservoir.ReservoirConfigurationData.AfforestationScale;
      end;
      Result := (LTotal < 0.0001) and
                (LTotal > -0.0001);
      if not Result then
        Result := ((100.0 - LTotal) < 0.0001) and
                  ((100.0 - LTotal) > -0.0001);
      if not Result then
      begin
        LMessage := FAppModules.Language.GetString('ContextValidation.SumAfforestationScale');
        AErrorMessages.Add('WARNING:' +Trim(FReservoirName)+ ':'+LMessage);
      end;
      Result := True;
    finally
      LReservoirList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateAreawhenFull(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateAreawhenFull';
var
  lMessage   : string;
  lReservoir : IReservoirData;
begin
  result := FALSE;
  try
    if not (FNodeType in ReservoirsSet)  then
    begin
      Result := True;
      Exit;
    end;

    Result := FAppModules.FieldProperties.ValidateFieldProperty('AreaFull', FloatToStr(FAreaWhenFull), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
    else
    begin
      if(FAppModules.Model.ModelName <> CDDTS) then
        lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FReservoirIdentifier]
      else
        lReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FReservoirIdentifier];

      //lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FReservoirIdentifier];
      if ((AreaWhenFull > FMaxArea) OR (AreaWhenFull < lReservoir.ReservoirAreasData.ReservoirAreasByIndex[FPointsCount]))then
      begin
        Result := FALSE;
        lMessage := FAppModules.language.GetString('ContextValidation.AreaWhenFull');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [Trim(lReservoir.ReservoirConfigurationData.ReservoirName)]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateReservoirName(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateReservoirName';
var
  lMessage       : string;
  //lUnique        : Boolean;
  //lIndex         : integer;
  //lReservoir     : IReservoirData;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirName', FReservoirName, lMessage)) then
      AErrorMessages.Add('WARNING:(' +IntToStr(FReservoirIdentifier)+ ')'+lMessage)
    else
    begin
     {   TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.GetReservoirList.ReservoirOrNodeByIndex
      for Lindex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.GetReservoirList.ReservoirCount -1 do
      begin
        TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.GetReservoirList.ReservoirOrNodeByIndex
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lReservoirList.ReservoirCount)) do
      begin
        lReservoir := lReservoirList.CastReservoirByIndex[lIndex];
        if ((FRecordIdentifier <> lReservoir.ReservoirConfigurationData.RecordIdentifier) AND
            (UpperCase(Trim(FReservoirName)) = UpperCase(Trim(lReservoir.ReservoirConfigurationData.ReservoirName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateReservoirName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [FReservoirName]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //Result := lUnique;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateReservoirPenaltyStructure(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateReservoirPenaltyStructure';
var
  lMessage     : string;
begin
  Result := FALSE;
  try
    if not (FNodeType in ReservoirsSet)  then
      Result := True
    else
    begin
      lMessage   := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
                ('PenaltyStruct', IntToStr(FPenaltyStruct), lMessage)) then
       AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
      else
        Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirConfigurationData.ValidateReservoirPenaltyValue(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateReservoirPenaltyValue';
var
  //LIndex            : integer;
  //LErrorMsg         : string;
  //LPenalty          : IReservoirPenalty;
  //LValue            : IReservoirPenaltyValue;
  //lStopOnFirstError : Boolean;
  lResult           : Boolean;
begin
  Result := False;
  try
    lResult := True;
    {lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
       ReservoirPenaltyStructureList.ReservoirPenaltyStructureByIdentifier[FPenaltyStruct];
    for LIndex := 0 to LPenalty.PenaltyValueCount - 1 do
    begin
      lValue := LPenalty.ReservoirPenaltyValueByIndex[lIndex];
      if not FAppModules.FieldProperties.ValidateFieldProperty(
          'ZonePenalty',FloatToStr(LValue.PenaltyValue),LErrorMsg) then
      begin
        lResult := FALSE;
        AErrorMessages.Add('ERROR:' +LErrorMsg);
        if (lStopOnFirstError) then
          Break;
      end;
    end;}
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.InitialiseNewConfigurationData(ANewRecordId, ANewReservoirNumber: integer;ANodeType: TNodeType);
const OPNAME = 'TReservoirConfigurationData.InitialiseNewConfigurationData';
begin
  try
    case ANodeType of
      ntReservoir         : FReservoirName       := 'Reservoir ' + IntToStr(ANewReservoirNumber);
      ntNodeWithInflow,
      ntNodeWithoutInflow : FReservoirName       := 'Node ' + IntToStr(ANewReservoirNumber);
      ntIrrigationNode    : FReservoirName       := 'Irrigation Node ' + IntToStr(ANewReservoirNumber);
      ntWetlandNode       : FReservoirName       := 'Wetland Node ' + IntToStr(ANewReservoirNumber);
      ntMineNode          : FReservoirName       := 'Mine Node ' + IntToStr(ANewReservoirNumber);
      ntDemandCentreNode  : FReservoirName       := 'Demand Centre Node ' + IntToStr(ANewReservoirNumber);
      ntGroundWater       : FReservoirName       := 'Aquifer Node ' + IntToStr(ANewReservoirNumber);
      ntBaseFlowNode      : FReservoirName       := 'BaseFlow Node ' + IntToStr(ANewReservoirNumber);
      ntAbstractionNode   : FReservoirName       := 'Abstraction Node ' + IntToStr(ANewReservoirNumber);
      ntCollectionNode    : FReservoirName       := 'Collection Node ' + IntToStr(ANewReservoirNumber);
    end;

    FRecordIdentifier    := ANewRecordId;
    FNodeType            := ANodeType;
    FReservoirIdentifier := ANewReservoirNumber;
    FPenaltyStruct       := 0;
    if(ANodeType in ReservoirsSet) then
      FPointsCount         := 3
    else
      FPointsCount         := 0;
    FIncludeSummary      := 'Y';
    FDrainageScale       := 0.0;
    FMaxArea             := 0.0;
    FAreaWhenFull        := 0.0;
    FVolumeWhenFull      := 0.0;
    FMaxVolume           := 0.0;
    FPriority            := 0.0;
    FUrbanRunOff         := 0.0;
    FAfforestationScale  := 0.0;
    FIrrigationScale     := 0.0;
    FCatchmentRef        := 0;
    FStatusIndicator     := 1;
    FRainCoef            := 0.0;
    FDamLevelsFileName   := '';
    FXCoord              := 0.0;
    FYCoord              := 0.0;
    FGroudID             := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidatePointsCount(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidatePointsCount';
var
  lMessage : string;
begin
  result := FALSE;
  try
    if not (FNodeType in ReservoirsSet)  then
    begin
      Result := True;
      Exit;
    end;

    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('PointsCount',
            IntToStr(FPointsCount), lMessage)) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateRainCoef(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateRainCoef';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if not (FNodeType in ReservoirsSet)  then
    begin
      Result := True;
      Exit;
    end;

    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('RainCoef',
            FloatToStr(FRainCoef), lMessage)) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateXCoord(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateXCoord';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('XCoord',
            FloatToStr(FXCoord), lMessage)) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateYCoord(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateYCoord';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('YCoord',
            FloatToStr(FYCoord), lMessage)) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_PointsCount(Value: integer);
const OPNAME = 'TReservoirConfigurationData.Set_PointsCount';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: integer;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), '');
        if FAppModules.FieldProperties.UpdateFieldValue(
             'PointsCount', IntToStr(Value), IntToStr(FPointsCount), LContextData) then
        begin
          LPrevValue := FPointsCount;
          FPointsCount := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PointsCount',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateReservoirNumber(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateReservoirNumber';
var
  lMessage : String;
begin
  result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirNodeNumber', IntToStr(FReservoirIdentifier),
              lMessage)) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.ValidateReservoirPriority(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateReservoirPriority';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    if not (FNodeType in ReservoirsSet)  then
      Result := True
    else
    begin
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirPriority', FloatToStr(FPriority),
              lMessage)) then
        AErrorMessages.Add('ERROR:' +Trim(FReservoirName)+ ':'+lMessage)
      else
        Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData._AddRef: Integer;
const OPNAME = 'TReservoirConfigurationData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData._Release: Integer;
const OPNAME = 'TReservoirConfigurationData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirConfigurationData.Validate';
var
  LStopOnFirstError : Boolean;
  LErrorMessages    : TStringList;
  //lMessage          : string;
begin
  Result := True;
  try
    LErrorMessages := TStringList.Create;
    try
      if(AContext = 'ReservoirName') then
        Result := ValidateReservoirName(LErrorMessages)
      else
      if(AContext = 'ReservoirNumber') then
        Result := ValidateReservoirNumber(LErrorMessages)
      else
      if(AContext = 'ReservoirPriority') then
        Result := ValidateReservoirPriority(LErrorMessages)
      else
      if(AContext = 'ReservoirPenaltyStructure') then
        Result := ValidateReservoirPenaltyStructure(LErrorMessages)
      else
      if(AContext = 'ReservoirPenaltyValue') then
        Result := ValidateReservoirPenaltyValue(LErrorMessages)
      else
      if(AContext = 'CatchmentRefNumber') then
        Result := ValidateCatchmentRefNumber(LErrorMessages)
      else
      if(AContext = 'SumDrainageScale') then
        Result := ValidateSumDrainageScale(LErrorMessages)
      else
      if(AContext = 'SumIrrigationScale') then
        Result := ValidateSumIrrigationScale(LErrorMessages)
      else
      if(AContext = 'SumAfforestationScale') then
        Result := ValidateSumAfforestationScale(LErrorMessages)
      else
      if(AContext = 'SumUrbanRunOff') then
        Result := ValidateSumUrbanRunOff(LErrorMessages)
      else
      if(AContext = 'AreaWhenFull') then
        Result := ValidateAreawhenFull(LErrorMessages)
      else
      if(AContext = 'PointsCount') then
        Result := ValidatePointsCount(LErrorMessages)
      else
      if(AContext = 'RainCoef') then
        Result := ValidateRainCoef(LErrorMessages)
      else
      if(AContext = 'XCoord') then
        Result := ValidateXCoord(LErrorMessages)
      else
      if(AContext = 'YCoord') then
        Result := ValidateYCoord(LErrorMessages)
      else
      if(AContext = 'DamLevelsFileName') then
        Result := ValidateDamLevelFileName(LErrorMessages)
      else
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if not  ValidateReservoirName(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateReservoirNumber(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateReservoirPriority(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;


        if not ValidateReservoirPenaltyStructure(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateReservoirPenaltyValue(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSumDrainageScale(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSumIrrigationScale(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSumAfforestationScale(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSumUrbanRunOff(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateAreawhenFull(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateCatchmentRefNumber(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidatePointsCount(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateRainCoef(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateXCoord(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateYCoord(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        {if not ValidateDamLevelFileName(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('IncludeSummary',IncludeSummary,lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(FReservoirName+ ':'+lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('ReservoirNodeNumber',IntToStr(FReservoirIdentifier),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('PointsCount',IntToStr(FPointsCount),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('AreaFull',FloatToStr(AreaWhenFull),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('RainCoef',FloatToStr(FPriority),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('CatchmentRef',IntTostr(FCatchmentRef),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('DrainageScale',FloatToStr(FDrainageScale),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('AfforestationScale',FloatToStr(FAfforestationScale),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('DamLevelsFileName',FDamLevelsFileName,lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;

        if not FAppModules.FieldProperties.ValidateFieldProperty('IrrigationScale',FloatToStr(FIrrigationScale),lMessage) then
        begin
          Result := False;
          LErrorMessages.Add(lMessage);
          if lStopOnFirstError then
          begin
            AErrors := AErrors + LErrorMessages.Text;
            Exit;
          end;
        end;}
      end;
      AErrors := AErrors + LErrorMessages.Text;
    finally
      LErrorMessages.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirConfigurationData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FRecordIdentifier)
    else
      Result := Result + ',Identifier=' + IntToStr(FRecordIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.Get_DamLevelsFileName: WideString;
const OPNAME = 'TReservoirConfigurationData.Get_DamLevelsFileName';
begin
  Result := '';
  try
    Result := FDamLevelsFileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_DamLevelsFileName(const Value: WideString);
const OPNAME = 'TReservoirConfigurationData.Set_DamLevelsFileName';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LMesg,
  LOldValue   : string;
  LFileName   : TAbstractModelFileName;
  LNewFileIndex : integer;
  LNewFileName  : string;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LNewFileName := UpperCase(Trim(ExtractFileName(Value)));
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'DamLevelsFileName', LNewFileName, FDamLevelsFileName, LContextData) then
        begin
          LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DamLevelsFileNames.Count+1;
          LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DamLevelsFileNames.FindFile(Value);
          if (LFileName = nil) then
          begin
            TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddDamLevelsFileName(LNewFileIndex,Value,False,0.0,
              FileLastWriteDate(Value));
            LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DamLevelsFileNames.FindFile(Value);
          end;
          LOldValue := FDamLevelsFileName;
          FDamLevelsFileName := LNewFileName;
          if (LFileName <> nil) and (not LFileName.SavedInDB) then
          begin
            LMesg := FAppModules.Language.GetString('SelectedFile.ImportFile');
            if (MessageDlg(LMesg,mtConfirmation,mbOKCancel,0) = mrOk) then
              FAppModules.Model.ProcessEvent(CmeImportDamLevelsFile,LFileName);
          end;
          if (LFileName <> nil) and (LFileName.SavedInDB) then
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamLevelsFileName',LOldValue,Value)
          else
            FDamLevelsFileName := LOldValue;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.ValidateDamLevelFileName(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirConfigurationData.ValidateDamLevelFileName';
var
  lMessage       : string;
  lValid         :  Boolean;
  //lResult        : Boolean;
  lFieldProperty : TAbstractFieldProperty;
  LDamLevelFile  : TAbstractModelFileName;
begin
  Result := FALSE;
  try
    if not (FNodeType in ReservoirsSet)  then
    begin
      Result := True;
      Exit;
    end;

    //lResult := TRUE;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('DamLevelsFileName');
    if (lFieldProperty <> nil) then
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                ('DamLevelsFileName', FDamLevelsFileName, lMessage);
      if (NOT lValid) then
      begin
        //lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.FileEmpty');
        AErrorMessages.Add('WARNING:' +Format(lMessage, [lFieldProperty.FieldMaximumValue]));
      end
      else
      begin
        if(FDamLevelsFileName <> '') then
        begin
          LDamLevelFile := TYieldModelDataObject(FAppModules.Model.ModelData).
                                  FileNamesObject.DamLevelsFileNames.FindFile(FDamLevelsFileName);
          if (LDamLevelFile = nil) then
          begin
            //lResult := FALSE;
            lMessage := FAppModules.language.GetString('ContextValidation.InvalidFileName');
            AErrorMessages.Add('WARNING:' +Format(lMessage, [lFieldProperty.FieldMaximumValue]));
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.CopyFrom(AResID: integer;
                                              ASource: TReservoirConfigurationData): boolean;
const OPNAME = 'TReservoirConfigurationData.CopyFrom';
begin
  Result := FALSE;
  try
    FNodeType           := ASource.NodeType;
    FPenaltyStruct      := ASource.PenaltyStructIdentifier;
    FPointsCount        := ASource.PointsCount;
    FIncludeSummary     := ASource.IncludeSummary;
    FDrainageScale      := ASource.DrainageScale;
    FMaxArea            := ASource.MaxArea;
    FAreaWhenFull       := ASource.AreaWhenFull;
    FVolumeWhenFull     := ASource.VolumeWhenFull;
    FMaxVolume          := ASource.MaxVolume;
    FPriority           := ASource.Priority;
    FStatusIndicator    := ASource.StatusIndicator;
    FAfforestationScale := ASource.AfforestationScale;
    FIrrigationScale    := ASource.IrrigationScale;
    FCatchmentRef       := ASource.CatchmentRef;
    FRainCoef           := ASource.FRainCoef;
    FDamLevelsFileName  := ASource.DamLevelsFileName;
    FXCoord              := ASource.XCoord;
    FYCoord              := ASource.YCoord;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirConfigurationData.Get_XCoord: Double;
const OPNAME = 'TReservoirConfigurationData.Get_XCoord';
begin
  Result := 0.0;
  try
    Result := FXCoord;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.Get_YCoord: Double;
const OPNAME = 'TReservoirConfigurationData.Get_YCoord';
begin
  Result := 0.0;
  try
    Result := FYCoord;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_XCoord(Value: Double);
const OPNAME = 'TReservoirConfigurationData.Set_XCoord';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'XCoord',  FloatToStr(Value),FloatToStr(FXCoord), LContextData) then
        begin
          LPrevValue := FXCoord;
          FXCoord  := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'XCoord',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_YCoord(Value: Double);
const OPNAME = 'TReservoirConfigurationData.Set_YCoord';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'YCoord',  FloatToStr(Value),FloatToStr(FYCoord), LContextData) then
        begin
          LPrevValue := FYCoord;
          FYCoord  := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'YCoord',FloatToStr(LPrevValue),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_GroupID(Value: integer);
const OPNAME = 'TReservoirConfigurationData.Set_GroupID';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirAreaGroupID', IntToStr(Value), IntToStr(FGroudID), LContextData) then
        begin
          LPrevValue := FGroudID;
          FGroudID := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirAreaGroupID',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirConfigurationData.Set_NaturalInflowChannel(Value: Integer);
const OPNAME = 'TReservoirConfigurationData.Set_NaturalInflowChannel';
var
  LLoadAgent   : TReservoirDataSQLAgent;
  LContextData : TStringList;
  LPrevValue   : integer;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadReservoirNamesContextData(LContextData,
          IntToStr(FRecordIdentifier), IntToStr(FReservoirIdentifier), IntToStr(FPenaltyStruct));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'NaturalInflowChannel', IntToStr(Value), IntToStr(FNaturalInflowChannel), LContextData) then
        begin
          LPrevValue := FNaturalInflowChannel;
          FNaturalInflowChannel := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NaturalInflowChannel',IntToStr(LPrevValue),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirConfigurationData.Assign(ASource: TReservoirConfigurationData);
const OPNAME = 'TReservoirConfigurationData.Assign';
var
  LResrvoir : TReservoirData;
begin
  try
    if (ASource <> nil) then
    begin
      ReservoirName           := 'Copy of '+ASource.FReservoirName;
      PenaltyStructIdentifier := ASource.FPenaltyStruct;
      IncludeSummary          := ASource.FIncludeSummary;
      DrainageScale           := ASource.FDrainageScale;
      AfforestationScale      := ASource.FAfforestationScale;
      IrrigationScale         := ASource.FIrrigationScale;
      UrbanRunOff             := ASource.FUrbanRunOff;
      CatchmentRef            := ASource.FCatchmentRef;
      AreaWhenFull            := ASource.FAreaWhenFull;
      RainCoef                := ASource.FRainCoef;
      DamLevelsFileName       := ASource.FDamLevelsFileName;
      XCoord                  := ASource.FXCoord;
      YCoord                  := ASource.FYCoord;
      VolumeWhenFull          := ASource.FVolumeWhenFull;
      Set_PointsCount(ASource.FPointsCount);
      FMaxArea                := ASource.FMaxArea;
      FMaxVolume              := ASource.FMaxVolume;

      LResrvoir               := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                                 CastReservoirList.CastReservoirOrNodeByRecordIdentifier[FRecordIdentifier];
      if(LResrvoir.FReservoirConfigurationData.NodeType in ReservoirsAndNodeWithInflowSet) then
      begin
        StatusIndicator         := ASource.FStatusIndicator;
        Priority                := ASource.FPriority;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirConfigurationData.Get_GroupID: integer;
const OPNAME = 'TReservoirConfigurationData.Get_GroupID';
begin
  Result := 0;
  try
    Result := FGroudID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirConfigurationData.Get_NaturalInflowChannel: Integer;
const OPNAME = 'TReservoirConfigurationData.Get_NaturalInflowChannel';
begin
  Result := 0;
  try
    Result := FNaturalInflowChannel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TReservoirData }

procedure TReservoirData.CreateMemberObjects;
const OPNAME = 'TReservoirData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReservoirConfigurationData  := TReservoirConfigurationData.Create(AppModules);
    FReservoirZoneElevationsData := TReservoirZoneElevationData.Create(AppModules);
    FReservoirEvaporationsData   := TReservoirEvaporationsData.Create(AppModules);
    FReservoirElevationsData     := TReservoirElevationsData.Create(AppModules);
    FReservoirVolumesData        := TReservoirVolumesData.Create(AppModules);
    FReservoirAreasData          := TReservoirAreasData.Create(AppModules);
    FDownStreamPowerChannels     := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.CreateNaturalInflowChannel: integer;
const OPNAME = 'TReservoirData.DestroyMemberObjects';
var
  LChannel : IGeneralFlowChannel;
  LChannelPenalty : IChannelPenalty;
  LPenaltyList      : IChannelPenaltyList;
  LConfigurationData : IReservoirConfigurationData;
begin
  Result := NullInteger;
  try
    LConfigurationData := ReservoirConfigurationData;
    if (LConfigurationData <> nil) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.CreateChannel;
       if TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelPenaltyList.ChannelPenaltyCount > 0 then
       LPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkElementData.ChannelPenaltyList;
       if (LPenaltyList <> nil) then
         LChannelPenalty :=  TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkElementData.ChannelPenaltyList.ChannelPenaltyByIdentifier[LPenaltyList.InflowPenaltyNo];
      if (LPenaltyList <> nil) then
      begin
        if (LChannel <> nil)and (LChannelPenalty <> nil) then
        begin
          LConfigurationData.NaturalInflowChannel := LChannel.ChannelNumber;
          LChannel.ChannelType := 36;
          LChannel.SummaryOutputRequired := 'N';
          LChannel.DownStreamNodeNumber := LConfigurationData.ReservoirIdentifier;
          LChannel.Set_ChannelPenalty(LChannelPenalty);
          Result := LChannel.ChannelNumber;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirData.DestroyMemberObjects;
const OPNAME = 'TReservoirData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReservoirConfigurationData);
    FreeAndNil(FReservoirZoneElevationsData);
    FreeAndNil(FReservoirEvaporationsData);
    FreeAndNil(FReservoirElevationsData);
    FreeAndNil(FReservoirVolumesData);
    FreeAndNil(FReservoirAreasData);
    FreeAndNil(FDownStreamPowerChannels);
    if (FReservoirTimeControl <> nil) then
      FreeAndNil(FReservoirTimeControl);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Initialise: boolean;
const OPNAME = 'TReservoirData.Initialise';
begin
  Result := inherited Initialise;
  try
    FReservoirConfigurationData.Initialise;
    FReservoirZoneElevationsData.Initialise;
    FReservoirEvaporationsData.Initialise;
    FReservoirElevationsData.Initialise;
    FReservoirVolumesData.Initialise;
    FReservoirAreasData.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.PopulateReservoirConfigurationData(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirData.PopulateReservoirConfigurationData';
begin
  Result := False;
  try
    Result := FReservoirConfigurationData.PopulateReservoirConfigurationData(ADataSet) and
              FReservoirZoneElevationsData.PopulateReservoirZoneElevationData(ADataSet)
               ;
    if Result then
      FReservoirZoneElevationsData.PopulateReservoirData(Self);
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirData.InitialiseNewReservoir(ANewRecordId, ANewReservoirNumber: integer;ANodeType: TNodeType);
const OPNAME = 'TReservoirData.InitialiseNewReservoir';
begin
  try
    Initialise;
    FReservoirConfigurationData.InitialiseNewConfigurationData(ANewRecordId,ANewReservoirNumber,ANodeType);
    FReservoirEvaporationsData.InitialiseNewEvaporations(ANewRecordId,ANodeType);
    FReservoirElevationsData.InitialiseNewElevations(ANewRecordId,ANodeType);
    FReservoirVolumesData.InitialiseNewVolumes(ANewRecordId,ANodeType);
    FReservoirAreasData.InitialiseNewAreas(ANewRecordId,ANodeType);
    FReservoirZoneElevationsData.InitialiseNewZoneElevation(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.GetReservoirConfigurationDataCast: TReservoirConfigurationData;
const OPNAME = 'TReservoirData.GetReservoirConfigurationDataCast';
begin
  Result := nil;
  try
    Result := FReservoirConfigurationData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetReservoirZoneElevationsDataCast: TReservoirZoneElevationData;
const OPNAME = 'TReservoirData.GetReservoirZoneElevationsDataCast';
begin
  Result := nil;
  try
    Result := FReservoirZoneElevationsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetReservoirEvaporationsDataCast: TReservoirEvaporationsData;
const OPNAME = 'TReservoirData.GetReservoirEvaporationsDataCast';
begin
  Result := nil;
  try
    Result := FReservoirEvaporationsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetReservoirElevationsDataCast: TReservoirElevationsData;
const OPNAME = 'TReservoirData.GetReservoirElevationsDataCast';
begin
  Result := nil;
  try
    Result := FReservoirElevationsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetReservoirVolumesDataCast: TReservoirVolumesData;
const OPNAME = 'TReservoirData.GetReservoirVolumesDataCast';
begin
  Result := nil;
  try
    Result := FReservoirVolumesData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetReservoirAreasDataCast: TReservoirAreasData;
const OPNAME = 'TReservoirData.GetReservoirAreasDataCast';
begin
  Result := nil;
  try
    Result := FReservoirAreasData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirAreasData: IReservoirAreaData;
const OPNAME = 'TReservoirData.Get_ReservoirAreasData';
begin
  Result := nil;
  try
    Result := FReservoirAreasData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirConfigurationData: IReservoirConfigurationData;
const OPNAME = 'TReservoirData.Get_ReservoirConfigurationData';
begin
  Result := nil;
  try
    Result := FReservoirConfigurationData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirElevationsData: IReservoirElevationsData;
const OPNAME = 'TReservoirData.Get_ReservoirElevationsData';
begin
  Result := nil;
  try
    Result := FReservoirElevationsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirEvaporationsData: IReservoirEvaporationsData;
const OPNAME = 'TReservoirData.Get_ReservoirEvaporationsData';
begin
  Result := nil;
  try
    Result := FReservoirEvaporationsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirVolumesData: IReservoirVolumeData;
const OPNAME = 'TReservoirData.Get_ReservoirVolumesData';
begin
  Result := nil;
  try
    Result := FReservoirVolumesData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirZoneElevationsData: IReservoirZoneElevationsData;
const OPNAME = 'TReservoirData.Get_ReservoirZoneElevationsData';
begin
  Result := nil;
  try
    Result := FReservoirZoneElevationsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateStartingStorageLevel(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirData.ValidateStartingStorageLevel';
var
  lMessage : string;
  lCount   : integer;
begin
  Result := FALSE;
  try
    Result := FAppModules.FieldProperties.ValidateFieldProperty
               ('ResInitialLevelsLev',
               FloatToStr(FReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1]) , lMessage, 1);
    if (NOT result) then
       AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage)
    else
    begin
      lCount := FReservoirConfigurationData.PointsCount;
      if lCount > 0 then
      begin
        if ((FReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1] > FReservoirElevationsData.FReservoirElevations[1]) OR
            (FReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1] < FReservoirElevationsData.FReservoirElevations[lCount])) then
        begin
          Result := FALSE;
          lMessage := FAppModules.language.GetString('ContextValidation.StartingStorage');
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.ValidateFullStorageLevel(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirData.ValidateFullStorageLevel';
var
  lMessage : string;
  lCount   : integer;
begin
  result := FALSE;
  try
    Result := FAppModules.FieldProperties.ValidateFieldProperty('FullSupplyLevel',
              FloatToStr(FReservoirZoneElevationsData.FullSupplyLevel.Elevation), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage)
    else
    begin
      lCount := ReservoirConfigurationData.PointsCount;
      if lCount > 0 then
      begin
        if ((FReservoirZoneElevationsData.FullSupplyLevel.Elevation < FReservoirZoneElevationsData.DeadStorageLevel.Elevation) OR
           (FReservoirZoneElevationsData.FullSupplyLevel.Elevation > FReservoirElevationsData.FReservoirElevations[1]) OR
           (FReservoirZoneElevationsData.FullSupplyLevel.Elevation < FReservoirElevationsData.FReservoirElevations[lCount])) then
        begin
          Result := FALSE;
          lMessage := FAppModules.language.GetString('ContextValidation.FullStorageLevel');
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage);
        end;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateDeadStorageLevel(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirData.ValidateDeadStorageLevel';
var
  lMessage : string;
  lCount   : integer;
begin
  result := FALSE;
  try
    Result := FAppModules.FieldProperties.ValidateFieldProperty('DeadStorageLevel',
              FloatToStr(FReservoirZoneElevationsData.DeadStorageLevel.Elevation), lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage)
    else
    begin
      lCount := FReservoirConfigurationData.PointsCount;
      if lCount > 0 then
      begin
        if ((FReservoirZoneElevationsData.DeadStorageLevel.Elevation > FReservoirElevationsData.FReservoirElevations[1]) OR
            (FReservoirZoneElevationsData.DeadStorageLevel.Elevation < FReservoirElevationsData.FReservoirElevations[lCount]) OR
            (FReservoirZoneElevationsData.DeadStorageLevel.Elevation < FReservoirZoneElevationsData.BottomOfReservoir.Elevation)) then
        begin
          Result := FALSE;
          lMessage := FAppModules.language.GetString('ContextValidation.DeadStorageLevel');
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+ lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateBottomOfReservoir(AErrorMessages: TStrings): boolean;
const OPNAME = 'TReservoirData.ValidateBottomOfReservoir';
var
  lMessage : string;
  lCount   : integer;
begin
  result := FALSE;
  try
    Result := FAppModules.FieldProperties.ValidateFieldProperty('BottomOfReservoir',
              FloatToStr(FReservoirZoneElevationsData.BottomOfReservoir.Elevation),lMessage);
    if (NOT result) then
      AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage)
    else
    begin
      lCount := ReservoirConfigurationData.PointsCount;
      if lCount > 0 then
      begin
        if ((FReservoirZoneElevationsData.BottomOfReservoir.Elevation > FReservoirElevationsData.FReservoirElevations[1]) OR
           (FReservoirZoneElevationsData.BottomOfReservoir.Elevation < FReservoirElevationsData.FReservoirElevations[lCount])) then
        begin
          Result := FALSE;
          lMessage := FAppModules.language.GetString('ContextValidation.BottomOfResevoir');
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage);
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateElevation(AErrorMessages: TStrings; AErrorColumns: TStringList): Boolean;
const OPNAME = 'TReservoirData.ValidateElevation';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lValid            : Boolean;
  LElevation        : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    LElevation := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    if Assigned(LElevation) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LElevation.ArrayLow to FReservoirConfigurationData.PointsCount do
      begin
        lMessage := '';
        if (Not FAppModules.FieldProperties.ValidateFieldProperty
           ('SurfaceElevation', FloatToStr(FReservoirElevationsData.FReservoirElevations[lIndex]),
           lMessage,lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LElevation.ArrayLow;
        while (lValid AND(lIndex < LElevation.ArrayHigh)) do
        begin
          if ((FReservoirElevationsData.FReservoirElevations[lIndex+1] <> NullFloat) AND
              (FReservoirElevationsData.FReservoirElevations[lIndex] < FReservoirElevationsData.FReservoirElevations[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.ElevationRangesNotInDescendingOrder');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Trim(FReservoirConfigurationData.ReservoirName)]));
            AErrorColumns.Add(IntToStr(lIndex));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
      Result := lResult;
    end;
     except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateVolume(AErrorMessages: TStrings;AErrorColumns: TStringList): Boolean;
const OPNAME = 'TReservoirData.ValidateVolume';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lValid            : Boolean;
  LVolume           : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    LVolume := FAppModules.FieldProperties.FieldProperty('Volume');
    if Assigned(LVolume) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LVolume.ArrayLow to FReservoirConfigurationData.PointsCount do
      begin
        lMessage := '';
        if (Not FAppModules.FieldProperties.ValidateFieldProperty
           ('Volume', FloatToStr(FReservoirVolumesData.FReservoirVolumes[lIndex]),
           lMessage,lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LVolume.ArrayLow;
        while (lValid AND(lIndex < LVolume.ArrayHigh)) do
        begin
          if ((FReservoirVolumesData.FReservoirVolumes[lIndex+1] <> NullFloat) AND
              (FReservoirVolumesData.FReservoirVolumes[lIndex] < FReservoirVolumesData.FReservoirVolumes[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.VolumeRangesNotInDescendingOrder');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Trim(FReservoirConfigurationData.ReservoirName)]));
            AErrorColumns.Add(IntToStr(lIndex));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateSurfaceArea(AErrorMessages: TStrings; AErrorColumns: TStringList): Boolean;
const OPNAME = 'TReservoirData.ValidateSurfaceArea';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  lValid            : Boolean;
  LSurfaceArea      : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    LSurfaceArea := FAppModules.FieldProperties.FieldProperty('Area');
    if Assigned(LSurfaceArea) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for lIndex := LSurfaceArea.ArrayLow to FReservoirConfigurationData.PointsCount do
      begin
        lMessage := '';
        if (Not FAppModules.FieldProperties.ValidateFieldProperty
           ('Area', FloatToStr(FReservoirAreasData.FReservoirAreas[lIndex]),
           lMessage,lIndex)) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+ ':'+lMessage);
          AErrorColumns.Add(IntToStr(lIndex));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
      if (lResult OR (NOT lStopOnFirstError)) then
      begin
        lValid := TRUE;
        lIndex := LSurfaceArea.ArrayLow;
        while (lValid AND(lIndex < LSurfaceArea.ArrayHigh)) do
        begin
          if ((FReservoirAreasData.FReservoirAreas[lIndex+1] <> NullFloat) AND
              (FReservoirAreasData.FReservoirAreas[lIndex] < FReservoirAreasData.FReservoirAreas[lIndex+1])) then
          begin
            lMessage := FAppModules.Language.GetString('ContextValidation.SurfaceAreaRangesNotInDescendingOrder');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [Trim(FReservoirConfigurationData.ReservoirName)]));
            AErrorColumns.Add(IntToStr(lIndex));
            lValid := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        if (NOT lValid) then
          lResult := FALSE;
      end;
      Result := lResult;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.DeletePhysicalCharacteristicsRow(AIndex: integer): wordbool;
const OPNAME = 'TReservoirData.DeletePhysicalCharacteristicsRow';
var
  LSQLAgent:TReservoirDataSQLAgent;
begin
  Result := false;
  try
    Result := FReservoirElevationsData.DeleteElevationValue(AIndex) and
              FReservoirVolumesData.DeleteVolumeValue(AIndex) and
              FReservoirAreasData.DeleteAreaValue(AIndex);
    if Result then
    begin
      FReservoirConfigurationData.Set_PointsCount(FReservoirConfigurationData.PointsCount -1);
      LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.UpdatePhysicalCharacteristicsRow(Self);
      finally
        LSQLAgent.Free;
      end;
      RecalculateAreaWhenFull;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.InsertPhysicalCharacteristicsRow(AIndex: integer;AElevation, AVolume, AArea: double): wordbool;
const OPNAME = 'TReservoirData.InsertPhysicalCharacteristicsRow';
var
  LSQLAgent:TReservoirDataSQLAgent;
begin
  Result := FALSE;
  try
    Result := FReservoirElevationsData.InsertElevationValue(AIndex,AElevation) and
              FReservoirVolumesData.InsertVolumeValue(AIndex,AVolume) and
              FReservoirAreasData.InsertAreaValue(AIndex,AArea);
    if Result then
    begin
      FReservoirConfigurationData.Set_PointsCount(FReservoirConfigurationData.PointsCount + 1);
      LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.UpdatePhysicalCharacteristicsRow(Self);
        if not Result then
        begin
         FReservoirElevationsData.DeleteElevationValue(AIndex);
         FReservoirVolumesData.DeleteVolumeValue(AIndex);
         FReservoirAreasData.DeleteAreaValue(AIndex);
         FReservoirConfigurationData.Set_PointsCount(FReservoirConfigurationData.PointsCount - 1);
        end;
      finally
        LSQLAgent.Free;
      end;
      RecalculateAreaWhenFull;
    end;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateMonthlyZoneElevation(AErrorMessages: TStrings;
                                                     AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TReservoirData.ValidateMonthlyZoneElevation';
var
  lIndex            : integer;
  LCount            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  LDiff,
  LCurrentLevel     : double;
  LMonthlyElevation : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    LMonthlyElevation := FAppModules.FieldProperties.FieldProperty('ReservoirLev');
    if Assigned(LMonthlyElevation) then
    begin
      lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
      for LCount := 0  to FReservoirZoneElevationsData.ReservoirDrawDownLevelsCount - 1 do
      begin
        for LIndex := LMonthlyElevation.ArrayLow to LMonthlyElevation.ArrayHigh do
        begin
          //Validate field properties
          lMessage := '';
          LCurrentLevel := ReservoirZoneElevationsData.DrawDownLevelByIndex[LCount].MonthlyElevationByIndex[LIndex];
          if (Not FAppModules.FieldProperties.ValidateFieldProperty('ReservoirLev', FloatToStr(LCurrentLevel),lMessage,lIndex)) then
          begin
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+':'+lMessage);
            AErrorColumns.Add(IntToStr(lIndex));
            if (lStopOnFirstError) then
              Break;
          end;
          //Validate <= Full storage level
          LDiff := Abs(LCurrentLevel - FReservoirZoneElevationsData.FullSupplyLevel.Elevation);
          if(LDiff > 0.00999) then
            if (LCurrentLevel > FReservoirZoneElevationsData.FullSupplyLevel.Elevation) then
            begin
              lMessage := FAppModules.language.GetString('ContextValidation.MonthlyZoneElevation');
              lResult := FALSE;
              AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+':'+lMessage);
              AErrorColumns.Add(IntToStr(lIndex));
              if (lStopOnFirstError) then
                Break;
            end;
          //Validate >= Dead storage level
          LDiff := Abs(LCurrentLevel - FReservoirZoneElevationsData.DeadStorageLevel.Elevation);
          if(LDiff > 0.00999) then
            if (LCurrentLevel < FReservoirZoneElevationsData.DeadStorageLevel.Elevation) then
            begin
              lMessage := FAppModules.language.GetString('ContextValidation.MonthlyZoneElevation');
              lResult := FALSE;
              AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+':'+lMessage);
              AErrorColumns.Add(IntToStr(lIndex));
              if (lStopOnFirstError) then
                Break;
            end;
          //Validate <= previous zone level
          if (LCount > 0) then
          begin
            LDiff := Abs(LCurrentLevel - ReservoirZoneElevationsData.DrawDownLevelByIndex[LCount-1].MonthlyElevationByIndex[LIndex]);
            if(LDiff > 0.00999) then
            begin
              if (LCurrentLevel > ReservoirZoneElevationsData.DrawDownLevelByIndex[LCount-1].MonthlyElevationByIndex[LIndex]) then
              begin
                lMessage := FAppModules.language.GetString('ContextValidation.MonthlyZoneElevationOrder');
                lResult := FALSE;
                AErrorMessages.Add('ERROR:' +Format(lMessage,[IntToStr(FReservoirConfigurationData.FReservoirIdentifier),
                                          FReservoirConfigurationData.ReservoirName]));
                AErrorColumns.Add(IntToStr(lIndex));
                if (lStopOnFirstError) then
                  Break;
              end;
            end;
          end;
        end;
        if not lResult and lStopOnFirstError then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.ValidateEvaporation(AErrorMessages: TStrings; AErrorColumns  : TStringList): Boolean;
const OPNAME = 'TReservoirData.ValidateEvaporation';
var
  lIndex            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lResult           : Boolean;
  LEvaporation      : TAbstractFieldProperty;
begin
  result := False;
  try
    lResult := True;
    if (FReservoirConfigurationData.FNodeType in [ntReservoir,ntWetlandNode,ntMinePolutionControlDam,ntMineUndergroundDam]) then
    begin
      LEvaporation := FAppModules.FieldProperties.FieldProperty('Evaporation');
      if Assigned(LEvaporation) then
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        lIndex := LEvaporation.ArrayLow;
        while (lResult AND (lIndex <= LEvaporation.ArrayHigh)) do
        begin
          if (ReservoirEvaporationsData.MonthlyEvaporationsByIndex[lIndex] = NullFloat) then
          begin
            lMessage := FAppModules.language.GetString('ContextValidation.EvaporationUnassigned');
            lResult := FALSE;
            AErrorMessages.Add('ERROR:' +Format(lMessage,[FReservoirConfigurationData.ReservoirName]));
          end
          else
            lIndex := lIndex + 1;
        end;
        if (lResult OR (NOT lStopOnFirstError)) then
        begin
          for lIndex := LEvaporation.ArrayLow to LEvaporation.ArrayHigh do
          begin
            lMessage := '';
            if (NOT FAppModules.FieldProperties.ValidateFieldProperty('Evaporation',
                FloatToStr(ReservoirEvaporationsData.MonthlyEvaporationsByIndex[lIndex]),
                lMessage,lIndex)) then
            begin
              lResult := False;
              AErrorMessages.Add('ERROR:' +Trim(FReservoirConfigurationData.ReservoirName)+':'+lMessage);
              AErrorColumns.Add(IntToStr(lIndex));
              if (lStopOnFirstError) then
                Break;
            end;
          end;
        end;
        Result := lResult;
      end;
    end
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData._AddRef: Integer;
const OPNAME = 'TReservoirData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData._Release: Integer;
const OPNAME = 'TReservoirData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_ReservoirPenaltyStructureData: IReservoirPenalty;
const OPNAME = 'TReservoirData.Get_ReservoirPenaltyStructureData';
begin
  Result := nil;
  try
    Result :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
               ReservoirPenaltyStructureList.ReservoirPenaltyByIdentifier[
               FReservoirConfigurationData.FPenaltyStruct];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Get_TimeControl: IReservoirTimeControl;
const OPNAME = 'TReservoirData.Get_TimeControl';
begin
  Result := nil;
  try
    Result := FReservoirTimeControl;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirData.Validate';
var
  LMessages         : TStringList;
  lErrorCols        : TStringList;
  LStopOnFirstError : boolean;
begin
  Result := True;
  try
    LMessages  := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      if(AContext = 'StartingStorageLevel') then
        Result := ValidateStartingStorageLevel(LMessages)
      else
      if(AContext = 'FullStorageLevel') then
        Result := ValidateFullStorageLevel(LMessages)
      else
      if(AContext = 'DeadStorageLevel') then
        Result := ValidateDeadStorageLevel(LMessages)
      else
      if(AContext = 'BottomOfReservoir') then
        Result := ValidateBottomOfReservoir(LMessages)
      else
      if(AContext = 'Elevation') then
      begin
        Result := ValidateElevation(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if(AContext = 'Volume') then
      begin
        Result := ValidateVolume(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if(AContext = 'SurfaceArea') then
      begin
        Result := ValidateSurfaceArea(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if(AContext = 'MonthlyZoneElevation') then
      begin
        Result := ValidateMonthlyZoneElevation(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      if(AContext = 'Evaporation') then
      begin
        Result := ValidateEvaporation(LMessages,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + LMessages.Text
          else
            AErrors := AErrors + CTStringsSeparator + LMessages.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          LMessages.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;

        if(FAppModules.Model.ModelName <> CDDTS) then
          if not FReservoirConfigurationData.Validate(AErrors,AContext) then
            Result := False;

        if (not Result) and lStopOnFirstError then
          Exit;
        if not ValidateStartingStorageLevel(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateFullStorageLevel(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateDeadStorageLevel(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateBottomOfReservoir(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateElevation (LMessages,lErrorCols) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateVolume (LMessages,lErrorCols) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateSurfaceArea (LMessages,lErrorCols) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMonthlyZoneElevation (LMessages,lErrorCols) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateEvaporation (LMessages,lErrorCols) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
      end;
      AErrors := AErrors + LMessages.Text;
    finally
      LMessages.Free;
      lErrorCols.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetKeyValues (const AParamField : WideString;
                                      const AFieldIndex : WideString) : WideString;
const OPNAME = 'TReservoirData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FReservoirConfigurationData.FRecordIdentifier)
    else
      Result := Result + ',Identifier=' + IntToStr(FReservoirConfigurationData.FRecordIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.RecalculateAreaWhenFull: WordBool;
const OPNAME = 'TReservoirData.RecalculateAreaWhenFull';
var
  LIndex             : integer;
  LMinLevel,
  LMaxLevel,
  LMinArea,
  LMaxArea,
  LFullSupplyLevel,
  LSlope,LYIntercept : double;
  LAreaWhenFull      : double;
  lVolumeWhenFull    : double;
  lMinVolume         : double;
  lMaxVolume         : double;
begin
  Result := False;
  try
    if (ReservoirConfigurationData.PointsCount < 0) then Exit;
    LAreaWhenFull   := -1.0;
    lVolumeWhenFull := -1.0;
    LFullSupplyLevel := ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
    for LIndex := 1 to ReservoirConfigurationData.PointsCount do
    begin
      if CompareDouble(LFullSupplyLevel, ReservoirElevationsData.ReservoirElevationsByIndex[LIndex],3) then
      begin
        LAreaWhenFull   := ReservoirAreasData.ReservoirAreasByIndex[LIndex];
        lVolumeWhenFull := ReservoirVolumesData.ReservoirVolumesByIndex[lIndex];
        Break;
      end;
    end;
    if (LAreaWhenFull >= 0.0) then
    begin
      if not CompareDouble(LAreaWhenFull,ReservoirConfigurationData.AreaWhenFull,3) then
      begin
        ReservoirConfigurationData.AreaWhenFull := LAreaWhenFull;
        ReservoirConfigurationData.VolumeWhenFull := lVolumeWhenFull;
      end;
    end
    else
    begin
      LMinLevel  := MinDouble;
      LMaxLevel  := MaxDouble;
      LMinArea   := MinDouble;
      LMaxArea   := MaxDouble;
      lMinVolume := MinDouble;
      lMaxVolume := MaxDouble;
      for LIndex := 1 to ReservoirConfigurationData.PointsCount do
      begin
        if (ReservoirElevationsData.ReservoirElevationsByIndex[LIndex] < LFullSupplyLevel) and
           (ReservoirElevationsData.ReservoirElevationsByIndex[LIndex] > LMinLevel) then
        begin
          LMinLevel  := ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
          LMinArea   := ReservoirAreasData.ReservoirAreasByIndex[LIndex];
          lMinVolume := ReservoirVolumesData.ReservoirVolumesByIndex[lIndex];
        end;

        if (ReservoirElevationsData.ReservoirElevationsByIndex[LIndex] > LFullSupplyLevel) and
           (ReservoirElevationsData.ReservoirElevationsByIndex[LIndex] < LMaxLevel) then
        begin
          LMaxLevel  := ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
          LMaxArea   := ReservoirAreasData.ReservoirAreasByIndex[LIndex];
          lMaxVolume := ReservoirVolumesData.ReservoirVolumesByIndex[lIndex];
        end;
      end;

      if (LMinLevel <> MinDouble) and (LMaxLevel <> MaxDouble) then
      begin
        if (LMaxLevel <> LMinLevel) then
        begin
          LSlope      := (LMaxArea - LMinArea)/(LMaxLevel - LMinLevel);
          LYIntercept := (LMinArea - (LSlope * LMinLevel));
          LAreaWhenFull := (LSlope*LFullSupplyLevel) + LYIntercept;
          if not CompareDouble(LAreaWhenFull,ReservoirConfigurationData.AreaWhenFull,3) then
            ReservoirConfigurationData.AreaWhenFull := LAreaWhenFull;

          LSlope          := (lMaxVolume - lMinVolume)/(LMaxLevel - LMinLevel);
          LYIntercept     := (lMinVolume - (LSlope * LMinLevel));
          lVolumeWhenFull := (LSlope * LFullSupplyLevel) + LYIntercept;
          if not CompareDouble(lVolumeWhenFull, ReservoirConfigurationData.VolumeWhenFull, 3) then
            ReservoirConfigurationData.VolumeWhenFull := lVolumeWhenFull;
        end;
      end;

    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.RecalculateVolumeWhenFull : WordBool;
const OPNAME = 'TReservoirData.RecalculateVolumeWhenFull';
var
  lIndex           : integer;
  lMinLevel        : double;
  lMaxLevel        : double;
  lFullSupplyLevel : double;
  lSlope           : double;
  lYIntercept      : double;
  lVolumeWhenFull  : double;
  lMinVolume       : double;
  lMaxVolume       : double;
begin
  Result := FALSE;
  try
    if (ReservoirConfigurationData.PointsCount < 0) then Exit;
    lVolumeWhenFull := -1.0;
    lFullSupplyLevel := ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
    for lIndex := 1 to ReservoirConfigurationData.PointsCount do
    begin
      if CompareDouble(lFullSupplyLevel, ReservoirElevationsData.ReservoirElevationsByIndex[lIndex], 3) then
      begin
        lVolumeWhenFull := ReservoirVolumesData.ReservoirVolumesByIndex[lIndex];
        Break;
      end;
    end;
    if (lVolumeWhenFull >= 0.0) then
    begin
      if NOT CompareDouble(lVolumeWhenFull, ReservoirConfigurationData.VolumeWhenFull, 3) then
        ReservoirConfigurationData.VolumeWhenFull := lVolumeWhenFull;
    end
    else
    begin
      lMinLevel  := MinDouble;
      lMaxLevel  := MaxDouble;
      lMinVolume := MinDouble;
      lMaxVolume := MaxDouble;
      for lIndex := 1 to ReservoirConfigurationData.PointsCount do
      begin
        if (ReservoirElevationsData.ReservoirElevationsByIndex[lIndex] < lFullSupplyLevel) AND
           (ReservoirElevationsData.ReservoirElevationsByIndex[lIndex] > lMinLevel) then
        begin
          lMinLevel  := ReservoirElevationsData.ReservoirElevationsByIndex[lIndex];
          lMinVolume := ReservoirVolumesData.ReservoirVolumesByIndex[lIndex];
        end;
        if (ReservoirElevationsData.ReservoirElevationsByIndex[lIndex] > lFullSupplyLevel) AND
           (ReservoirElevationsData.ReservoirElevationsByIndex[lIndex] < lMaxLevel) then
        begin
          lMaxLevel  := ReservoirElevationsData.ReservoirElevationsByIndex[lIndex];
          lMaxVolume := ReservoirVolumesData.ReservoirVolumesByIndex[lIndex];
        end;
      end;

      if (lMinLevel <> MinDouble) and (lMaxLevel <> MaxDouble) then
      begin
        if (lMaxLevel <> lMinLevel) then
        begin
          lSlope          := (lMaxVolume - lMinVolume)/(lMaxLevel - lMinLevel);
          lYIntercept     := (lMinVolume - (lSlope * lMinLevel));
          lVolumeWhenFull := (lSlope * lFullSupplyLevel) + lYIntercept;
          if NOT CompareDouble(lVolumeWhenFull, ReservoirConfigurationData.VolumeWhenFull, 3) then
            ReservoirConfigurationData.VolumeWhenFull := lVolumeWhenFull;
        end;
      end;

    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.CreateTimeControl: TReservoirTimeControl;
const OPNAME = 'TReservoirData.CreateTimeControl';
begin
  Result := nil;
  try
    FReservoirTimeControl := TReservoirTimeControl.Create(FAppModules);
    FReservoirTimeControl.FReservoirNumber := FReservoirConfigurationData.FReservoirIdentifier;
    FReservoirTimeControl.FBaseNodeNumber  := FReservoirConfigurationData.FReservoirIdentifier;
    Result := FReservoirTimeControl;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.DeleteTimeControl: Boolean;
const OPNAME = 'TReservoirData.DeleteTimeControl';
begin
  Result := FALSE;
  try
    FreeAndNil(FReservoirTimeControl);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.NewTimeControl: IReservoirTimeControl;
const OPNAME = 'TReservoirData.NewTimeControl';
var
  lLoadAgent   : TReservoirTimeControlSQLAgent;
begin
  Result := nil;
  try
    lLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      if (lLoadAgent.InsertReservoirTimeControl(FReservoirConfigurationData.FReservoirIdentifier)) then
      begin
        Result := CreateTimeControl;
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReservoirData.RemoveTimeControl : WordBool;
const OPNAME = 'TReservoirData.RemoveTimeControl';
var
  lLoadAgent : TReservoirTimeControlSQLAgent;
begin
  Result := False;
  try
    lLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      if (lLoadAgent.DeleteReservoirTimeControl(FReservoirConfigurationData.FReservoirIdentifier)) then
      begin
        DeleteTimeControl;
        Result := True;
      end;
     finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReservoirData.Clone : IReservoirData;
const OPNAME = 'TReservoirData.Clone';
var
  lClone   : TReservoirData;
  lCloneID : integer;
begin
  Result := nil;
  try
    lClone := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                CastReservoirList.CastCreateReservoir;
    lCloneID := lClone.ReservoirConfigurationData.RecordIdentifier;
    {Presley}
    lClone.FReservoirEvaporationsData.CopyFrom(lCloneID, Self.FReservoirEvaporationsData);
    lClone.FReservoirElevationsData.CopyFrom(lCloneID, Self.FReservoirElevationsData);
    lClone.FReservoirVolumesData.CopyFrom(lCloneID, Self.FReservoirVolumesData);
    lClone.FReservoirAreasData.CopyFrom(lCloneID, Self.FReservoirAreasData);
    lClone.FReservoirConfigurationData.CopyFrom(lCloneID, Self.FReservoirConfigurationData);
    lClone.FReservoirZoneElevationsData.CopyFrom(lCloneID, Self.FReservoirZoneElevationsData);
    Result := lClone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.DownStreamPowerChannels: WideString;
const OPNAME = 'TReservoirData.DownStreamPowerChannels';
begin
  Result := '';
  try
    FDownStreamPowerChannels.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.DownStreamPowerChannelCount: Integer;
const OPNAME = 'TReservoirData.DownStreamPowerChannelCount';
begin
  Result := 0;
  try
    FDownStreamPowerChannels.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.PopulateDownStreamPowerChannels(AChannels: TStrings): Boolean;
const OPNAME = 'TReservoirData.PopulateDownStreamPowerChannels';
begin
  Result := False;
  try
    if Assigned(AChannels) then
    begin
      FDownStreamPowerChannels.Assign(AChannels);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirData.GetReservoirElevationBySurfaceArea(AArea: Double): Double;
const OPNAME = 'TReservoirData.GetReservoirElevationBySurfaceArea';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to Self.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AArea <= Self.ReservoirAreasData.ReservoirAreasByIndex[LIndex]) and
         (AArea >= Self.ReservoirAreasData.ReservoirAreasByIndex[LIndex + 1]) then
      begin
        LVal1 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal2 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];
        LVal3 := Self.ReservoirAreasData.ReservoirAreasByIndex[LIndex];
        LVal4 := Self.ReservoirAreasData.ReservoirAreasByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := AArea*((LVal1-LVal2)/(LVal3-LVal4))+LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)));
        except
          Result := AArea; // Vertical ASYMTOTE  --- this might be incorrect because here we know x calculating Y
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.GetReservoirElevationByVolume(AVolume: Double): Double;
const OPNAME = 'TReservoirData.GetReservoirElevationByVolume';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to Self.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AVolume <= Self.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex]) and
         (AVolume >= Self.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex + 1]) then
      begin
        LVal1 := Self.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex];
        LVal2 := Self.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex+1];
        LVal3 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal4 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := (AVolume-(LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)))))/((LVal1-LVal2)/(LVal3 - LVal4));
        except
          Result := AVolume; // Vertical ASYMTOTE
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.GetReservoirSurfaceAreaByElevation(AElevation: Double): Double;
const OPNAME = 'TReservoirData.GetReservoirSurfaceAreaByElevation';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to Self.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AElevation <= Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex]) and
         (AElevation >= Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex + 1]) then
      begin
        LVal1 := Self.ReservoirAreasData.ReservoirAreasByIndex[LIndex];
        LVal2 := Self.ReservoirAreasData.ReservoirAreasByIndex[LIndex+1];
        LVal3 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal4 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := AElevation*((LVal1-LVal2)/(LVal3-LVal4))+LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)));
        except
          Result := AElevation; // Vertical ASYMTOTE  --- this might be incorrect because here we know x calculating Y
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirData.GetReservoirSurfaceAreaByVolume(AVolume: Double): Double;
const OPNAME = 'TReservoirData.GetReservoirSurfaceAreaByVolume';
begin
//  ShowMessage('Todo : '+OPNAME);
end;

function TReservoirData.GetReservoirVolumeBySurfaceArea(AArea: Double): Double;
const OPNAME = 'TReservoirData.GetReservoirVolumeBySurfaceArea';
begin
//  ShowMessage('Todo : '+OPNAME);
end;

function TReservoirData.GetReservoirVolumeByElevation(AElevation: Double): Double;
const OPNAME = 'TReservoirData.GetReservoirVolumeByElevation';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to Self.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AElevation <= Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex]) and
         (AElevation >= Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex + 1]) then
      begin
        LVal1 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal2 := Self.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];
        LVal3 := Self.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex];
        LVal4 := Self.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := (AElevation-(LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)))))/((LVal1-LVal2)/(LVal3 - LVal4));
        except
          Result := AElevation; // Vertical ASYMTOTE
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirData.Assign(ASource: TReservoirData);
const OPNAME = 'TReservoirData.Assign';
var
  LReservoirTimeControl : IReservoirTimeControl;
  LChannel : IGeneralFlowChannel;
  LChannelPenalty : IChannelPenalty;
  LPenaltyList      : IChannelPenaltyList;
begin
  try
    if (ASource <> nil) then
    begin
      FReservoirConfigurationData.Assign(ASource.FReservoirConfigurationData);
      if(ASource.FReservoirConfigurationData.NodeType in ReservoirsSet) then
      begin
        FReservoirAreasData.Assign(ASource.FReservoirAreasData);
        FReservoirElevationsData.Assign(ASource.FReservoirElevationsData);
        FReservoirEvaporationsData.Assign(ASource.FReservoirEvaporationsData);
        FReservoirVolumesData.Assign(ASource.FReservoirVolumesData);
        FReservoirZoneElevationsData.Assign(ASource.FReservoirZoneElevationsData);
        FDownStreamPowerChannels.Assign(ASource.FDownStreamPowerChannels);

        if ASource.FReservoirTimeControl <> nil then
        begin
          LReservoirTimeControl := NewTimeControl;
          if (LReservoirTimeControl <> nil) then
            FReservoirTimeControl.Assign(ASource.FReservoirTimeControl);
        end;
        if (ASource.FReservoirConfigurationData.FNaturalInflowChannel <> 0) then
        begin
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.CreateChannel;
          if TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelPenaltyList.ChannelPenaltyCount > 0 then
            LPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                    NetworkElementData.ChannelPenaltyList;
          if (LPenaltyList <> nil) then
             LChannelPenalty :=  TYieldModelDataObject(FAppModules.Model.ModelData).
                                    NetworkElementData.ChannelPenaltyList.ChannelPenaltyByIdentifier[LPenaltyList.InflowPenaltyNo];
          if (LPenaltyList <> nil) then
          begin
            if (LChannel <> nil)and (LChannelPenalty <> nil) then
            begin
              FReservoirConfigurationData.NaturalInflowChannel := LChannel.ChannelNumber;
              LChannel.ChannelType := 36;
              LChannel.SummaryOutputRequired := 'N';
              LChannel.DownStreamNodeNumber := FReservoirConfigurationData.ReservoirIdentifier;
              LChannel.Set_ChannelPenalty(LChannelPenalty);
            end;
          end;

        end;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirDataList }

procedure TReservoirDataList.CreateMemberObjects;
const OPNAME = 'TReservoirDataList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReservoirAndNodesData := TObjectList.Create(True);
    FReservoirData         := TObjectList.Create(False);
    FNodeWithInflowData    := TObjectList.Create(False);
    FNodeWithoutInflowData := TObjectList.Create(False);
    FIrrigationNodeData    := TObjectList.Create(False);
    //FWetlandData           := TObjectList.Create(False);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirDataList.DestroyMemberObjects;
const OPNAME = 'TReservoirDataList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReservoirAndNodesData);
    FreeAndNil(FReservoirData);
    FreeAndNil(FNodeWithInflowData);
    FreeAndNil(FNodeWithoutInflowData);
    FreeAndNil(FIrrigationNodeData);
    //FreeAndNil(FWetlandData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Initialise: boolean;
const OPNAME = 'TReservoirDataList.Initialise';
begin
  Result := inherited Initialise;
  try
    FReservoirData.Clear;
    FNodeWithInflowData.Clear;
    FNodeWithoutInflowData.Clear;
    FIrrigationNodeData.Clear;
    FReservoirAndNodesData.Clear;
    //FWetlandData.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.Get_NodesWithInflowCount: integer;
const OPNAME = 'TReservoirDataList.Get_NodesWithInflowCount';
begin
  Result := 0;
  try
    Result := FNodeWithInflowData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.Get_NodesWithoutInflowCount: integer;
const OPNAME = 'TReservoirDataList.Get_NodesWithoutInflowCount';
begin
  Result := 0;
  try
    Result := FNodeWithoutInflowData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.Get_ReservoirCount: integer;
const OPNAME = 'TReservoirDataList.Get_ReservoirCount';
begin
  Result := 0;
  try
    Result := FReservoirData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TReservoirDataList.Get_WetlandsCount: integer;
const OPNAME = 'TReservoirDataList.Get_WetlandsCount';
begin
  Result := 0;
  try
    Result := FWetlandData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TReservoirDataList.Get_ReservoirByIdentifier(AReservoirIdentifier: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_ReservoirByIdentifier';
var LReservoirIndex: integer;
begin
  Result := nil;
  try
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if (TReservoirData(FReservoirData[LReservoirIndex]).ReservoirConfigurationData.ReservoirIdentifier = AReservoirIdentifier) then
      begin
        Result := TReservoirData(FReservoirData[LReservoirIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_ReservoirByIndex(AReservoirIndex: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_ReservoirByIndex';
begin
  Result := nil;
  try
    if(AReservoirIndex >= 0) and (AReservoirIndex < FReservoirData.Count) then
      Result := TReservoirData(FReservoirData[AReservoirIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetReservoirByIdentifierCast(AReservoirIdentifier: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirByIdentifierCast';
var LReservoirIndex: integer;
begin
  Result := nil;
  try
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if (TReservoirData(FReservoirData[LReservoirIndex]).ReservoirConfigurationData.ReservoirIdentifier = AReservoirIdentifier) then
      begin
        Result := TReservoirData(FReservoirData[LReservoirIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetReservoirByIndexCast(AReservoirIndex: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirByIndexCast';
begin
  Result := nil;
  try
    if(AReservoirIndex >= 0) and (AReservoirIndex < FReservoirData.Count) then
      Result := TReservoirData(FReservoirData[AReservoirIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetReservoirByRecordIdentifier(ARecordIdentifier: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirByRecordIdentifier';
var LReservoirIndex: integer;
begin
  Result := nil;
  try
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if (TReservoirData(FReservoirData[LReservoirIndex]).ReservoirConfigurationData.RecordIdentifier = ARecordIdentifier) then
      begin
        Result := TReservoirData(FReservoirData[LReservoirIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirDataList.UpdateReservoirName(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateReservoirName';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.ReservoirName := ANewValue;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateDrainageScalingFactor(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateDrainageScalingFactor';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.DrainageScale := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateAfforestationScalingFactor(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateAfforestationScalingFactor';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.AfforestationScale := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateIrrigationScalingFactor(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateIrrigationScalingFactor';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.IrrigationScale := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateAreaWhenFull(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateAreaWhenFull';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.AreaWhenFull := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateCatchmentReference(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateCatchmentReference';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.CatchmentRef := StrToInt(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateIncludeSummary(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateIncludeSummary';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.IncludeSummary := ANewValue;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdatePenaltyStructure(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdatePenaltyStructure';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.PenaltyStructIdentifier := StrToInt(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateFullSupplyLevel(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateFullSupplyLevel';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirZoneElevationsData.FullSupplyLevel.Elevation := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateDeadStorageLevel(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateDeadStorageLevel';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirZoneElevationsData.DeadStorageLevel.Elevation := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateBottomOfReservoirLevel(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateBottomOfReservoirLevel';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirZoneElevationsData.BottomOfReservoir.Elevation := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateInitialReservoirLevel(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateInitialReservoirLevel';
var LReservoirIdentifier,
    LFieldNameIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['ReservoirNodeNumber']);
      LFieldNameIdentifier:= StrToInt(AContextData.Values['FieldNameIdentifier']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirZoneElevationsData.InitialLevelsData.UpdateInitialLevelValue(LFieldNameIdentifier, StrToFloat(ANewValue));
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateReservoirEvaporations(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateReservoirEvaporations';
var LRecordIdentifier,
    LFieldNameIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LRecordIdentifier := StrToInt(AContextData.Values['RecordIdentifier']);
      LFieldNameIdentifier:= StrToInt(AContextData.Values['FieldNameIdentifier']);
      CastReservoirByRecordIdentifier[LRecordIdentifier].ReservoirEvaporationsData.MonthlyEvaporationsByIndex[LFieldNameIdentifier] := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateReservoirVolumes(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateReservoirVolumes';
var LRecordIdentifier,
    LFieldNameIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LRecordIdentifier := StrToInt(AContextData.Values['RecordIdentifier']);
      LFieldNameIdentifier:= StrToInt(AContextData.Values['FieldNameIdentifier']);
      CastReservoirByRecordIdentifier[LRecordIdentifier].ReservoirVolumesData.ReservoirVolumesByIndex[LFieldNameIdentifier] := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateReservoirElevations(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateReservoirElevations';
var LRecordIdentifier,
    LFieldNameIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LRecordIdentifier := StrToInt(AContextData.Values['RecordIdentifier']);
      LFieldNameIdentifier:= StrToInt(AContextData.Values['FieldNameIdentifier']);
      CastReservoirByRecordIdentifier[LRecordIdentifier].ReservoirElevationsData.ReservoirElevationsByIndex[LFieldNameIdentifier] := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.UpdateReservoirAreas(AFieldName,
  ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateReservoirAreas';
var LRecordIdentifier,
    LFieldNameIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LRecordIdentifier := StrToInt(AContextData.Values['RecordIdentifier']);
      LFieldNameIdentifier:= StrToInt(AContextData.Values['FieldNameIdentifier']);
      CastReservoirByRecordIdentifier[LRecordIdentifier].ReservoirAreasData.ReservoirAreasByIndex[LFieldNameIdentifier] := StrToFloat(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.GetReservoirsAndNodesPerCatchmentRef(ACatchmentRefNumber: integer; AReservoirAndNodesList: TObjectList): boolean;
const OPNAME = 'TReservoirDataList.GetReservoirsAndNodesPerCatchmentRef';
var
  LReservoirOrNodeIndex: integer;
begin
  Result := False;
  try
    if not Assigned(AReservoirAndNodesList) then Exit;
    AReservoirAndNodesList.Clear;
    for LReservoirOrNodeIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if (ReservoirOrNodeByIndex[LReservoirOrNodeIndex].ReservoirConfigurationData.CatchmentRef = ACatchmentRefNumber) then
        AReservoirAndNodesList.Add(FReservoirAndNodesData[LReservoirOrNodeIndex]);
    end;
    Result := True;

  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.GetReservoirsAndNodesOutSideCatchmentRef(ACatchmentRefNumber: integer; AReservoirAndNodesList: TObjectList): boolean;
const OPNAME = 'TReservoirDataList.GetReservoirsAndNodesOutSideCatchmentRef';
var
  LReservoirOrNodeIndex  : integer;
  LReservoirOrNodeObject : IReservoirData;
begin
  Result := False;
  try
    if not Assigned(AReservoirAndNodesList) then Exit;
    AReservoirAndNodesList.Clear;
    for LReservoirOrNodeIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      LReservoirOrNodeObject := ReservoirOrNodeByIndex[LReservoirOrNodeIndex];
      if (LReservoirOrNodeObject.ReservoirConfigurationData.NodeType in NodeWithInflowAndReservoirSet) AND
         (LReservoirOrNodeObject.ReservoirConfigurationData.CatchmentRef <> ACatchmentRefNumber) then
        AReservoirAndNodesList.Add(FReservoirAndNodesData[LReservoirOrNodeIndex]);
    end;
    Result := True;

  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.GetReservoirsPerPenaltyStructure(APenaltyStructureID: integer; AReservoirList: TObjectList): boolean;
const OPNAME = 'TReservoirDataList.GetReservoirsPerPenaltyStructure';
var
  LReservoirIndex: integer;
begin
  Result := False;
  try
    if not Assigned(AReservoirList) then Exit;
    AReservoirList.Clear;
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if (ReservoirByIndex[LReservoirIndex].ReservoirConfigurationData.PenaltyStructIdentifier = APenaltyStructureID) then
        AReservoirList.Add(FReservoirData[LReservoirIndex]);
    end;
    Result := True;

  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.ReservoirHasNoValidCatchmentRef(AReservoir: TReservoirData): boolean;
const OPNAME = 'TReservoirDataList.ReservoirHasNoValidCatchmentRef';
var
  LParamReference: TParamReference;
begin
  Result := False;
  try
    LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).CastCastParameterData.CastReferenceDataByCatchNumber[
    AReservoir.ReservoirConfigurationData.CatchmentRef];
    Result := not Assigned(LParamReference);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.GetReservoirsWithNoPenaltyStructure(AReservoirList: TObjectList): boolean;
const OPNAME = 'TReservoirDataList.GetReservoirsWithNoPenaltyStructure';
var
  LReservoirIndex: integer;
begin
  Result := False;
  try
    if not Assigned(AReservoirList) then Exit;
    AReservoirList.Clear;
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if (ReservoirByIndex[LReservoirIndex].ReservoirConfigurationData.PenaltyStructIdentifier <= 0) then
        AReservoirList.Add(FReservoirData[LReservoirIndex]);
    end;
    Result := True;

  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.GetReservoirsWithNoCatchmentRef(AReservoirList: TObjectList): boolean;
const OPNAME = 'TReservoirDataList.GetReservoirsWithNoCatchmentRef';
var
  LReservoirIndex: integer;
begin
  Result := False;
  try
    if not Assigned(AReservoirList) then Exit;
    AReservoirList.Clear;
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if ReservoirHasNoValidCatchmentRef(CastReservoirByIndex[LReservoirIndex]) then
         AReservoirList.Add(FReservoirData[LReservoirIndex]);
    end;
    for LReservoirIndex := 0 to FNodeWithInflowData.Count - 1 do
    begin
      if ReservoirHasNoValidCatchmentRef(CastNodeWithInflowByIndex[LReservoirIndex]) then
         AReservoirList.Add(FNodeWithInflowData[LReservoirIndex]);
    end;
    Result := True;

  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataList.Get_NodeWithInflowByIdentifier(ANodeIdentifier: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_NodeWithInflowByIdentifier';
var
  LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FNodeWithInflowData.Count - 1 do
    begin
      if (TReservoirData(FNodeWithInflowData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = ANodeIdentifier) then
      begin
        Result := TReservoirData(FNodeWithInflowData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_NodeWithInflowByIndex(ANodeIndex: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_NodeWithInflowByIndex';
begin
  Result := nil;
  try
    if(ANodeIndex >= 0) and (ANodeIndex < FNodeWithInflowData.Count) then
      Result := TReservoirData(FNodeWithInflowData[ANodeIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_NodeWithoutInflowByIdentifier(ANodeIdentifier: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_NodeWithoutInflowByIdentifier';
var
  LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FNodeWithoutInflowData.Count - 1 do
    begin
      if (TReservoirData(FNodeWithoutInflowData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = ANodeIdentifier) then
      begin
        Result := TReservoirData(FNodeWithoutInflowData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_NodeWithoutInflowByIndex(ANodeIndex: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_NodeWithoutInflowByIndex';
begin
  Result := nil;
  try
    if(ANodeIndex >= 0) and (ANodeIndex < FNodeWithoutInflowData.Count) then
      Result := TReservoirData(FNodeWithoutInflowData[ANodeIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_ReservoirOrNodeByIdentifier(AReservoirOrNodeIdentifier: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_ReservoirOrNodeByIdentifier';
var LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if (TReservoirData(FReservoirAndNodesData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = AReservoirOrNodeIdentifier) then
      begin
        Result := TReservoirData(FReservoirAndNodesData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_ReservoirOrNodeByID(AIdentifier: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_ReservoirOrNodeByID';
var
  lIndex: integer;
begin
  Result := nil;
  try
    for lIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if (TReservoirData(FReservoirAndNodesData[lIndex]).ReservoirConfigurationData.RecordIdentifier = AIdentifier) then
      begin
        Result := TReservoirData(FReservoirAndNodesData[lIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TReservoirDataList.Get_WetlandByByID(AIdentifier: Integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_WetlandByByID';
var
  lIndex: integer;
begin
  Result := nil;
  try
    for lIndex := 0 to FWetlandData.Count - 1 do
    begin
      if (TReservoirData(FWetlandData[lIndex]).ReservoirConfigurationData.RecordIdentifier = AIdentifier) then
      begin
        Result := TReservoirData(FWetlandData[lIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TReservoirDataList.Get_ReservoirOrNodeByIndex(AReservoirOrNodeIndex: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_ReservoirOrNodeByIndex';
begin
  Result := nil;
  try
    if(AReservoirOrNodeIndex >= 0) and (AReservoirOrNodeIndex < FReservoirAndNodesData.Count) then
      Result := TReservoirData(FReservoirAndNodesData[AReservoirOrNodeIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TReservoirDataList.Get_WetlandByIndex( AWetlandByIndex: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_WetlandByIndex';
begin
  Result := nil;
  try
    if(AWetlandByIndex >= 0) and (AWetlandByIndex < FWetlandData.Count) then
      Result := TReservoirData(FWetlandData[AWetlandByIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TReservoirDataList.GetReservoirByNameCast(AReservoirName: string): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirByNameCast';
var LReservoirIndex: integer;
begin
  Result := nil;
  try
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
    begin
      if (TReservoirData(FReservoirData[LReservoirIndex]).ReservoirConfigurationData.ReservoirName = AReservoirName) then
      begin
        Result := TReservoirData(FReservoirData[LReservoirIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.UpdateStatusIndicator(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TReservoirDataList.UpdateStatusIndicator';
var LReservoirIdentifier: integer;
begin
  Result := False;
  try
    if FAppModules.FieldProperties.UpdateFieldValue(AFieldName, ANewValue, AOldValue, AContextData) then
    begin
      LReservoirIdentifier := StrToInt(AContextData.Values['NodeCount']);
      ReservoirByIdentifier[LReservoirIdentifier].ReservoirConfigurationData.StatusIndicator := StrToInt(ANewValue);
      FAppModules.Model.StudyDataHasChanged(sdccEdit,AFieldName,AOldValue,ANewValue);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirDataList.AddZeroNode;
const OPNAME = 'TReservoirDataList.AddZeroNode';
var
  LReservoirData: TReservoirData;
  LIReservoirData: IReservoirData;
begin
  try
    LIReservoirData := Get_ReservoirOrNodeByIdentifier(0);
    if (LIReservoirData = nil) then
    begin
      LReservoirData := TReservoirData.Create(FAppModules);
      LReservoirData.Initialise;

      LReservoirData.CastReservoirConfigurationData.PopulateNodeData(0,'0',ntNodeWithoutInflow);
      AddReservoirOrNodeToList(LReservoirData);
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

{procedure TReservoirDataList.AddInferedNode(ANodeNumber: integer);
const OPNAME = 'TReservoirDataList.AddInferedNode';
var
  LReservoirData: TReservoirData;
  LAbstractReservoirData: TAbstractReservoirData;
begin
  try
    LAbstractReservoirData := GetReservoirOrNodeByIdentifier(ANodeNumber);
    if not Assigned(LAbstractReservoirData) then
    begin
      LReservoirData := TReservoirData.Create(FAppModules);
      LReservoirData.Initialise;

      LReservoirData.CastReservoirConfigurationData.PopulateNodeData(ANodeNumber,IntToStr(ANodeNumber),ntNodeWithoutInflow);
      AddReservoirOrNodeToList(LReservoirData);
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TReservoirDataList.Get_ReservoirAndNodesCount: integer;
const OPNAME = 'TReservoirDataList.Get_ReservoirAndNodesCount';
begin
  Result := 0;
  try
    Result := FReservoirAndNodesData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.PenaltyStructureAdded(APenaltyID: integer): boolean;
const OPNAME = 'TReservoirDataList.PenaltyStructureAdded';
var
  LIndex: integer;
  LReservoirData: TReservoirData;
begin
  Result := False;
  try
    for LIndex := 0 to FReservoirAndNodesData.Count -1 do
    begin
      LReservoirData := TReservoirData(FReservoirAndNodesData.Items[LIndex]);
      if(LReservoirData.FReservoirConfigurationData.FPenaltyStruct >= APenaltyID) then
        LReservoirData.FReservoirConfigurationData.PenaltyStructIdentifier :=
        LReservoirData.FReservoirConfigurationData.PenaltyStructIdentifier +1;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.PenaltyStructureDeleted(APenaltyID: integer): boolean;
const OPNAME = 'TReservoirDataList.PenaltyStructureDeleted';
var
  LIndex: integer;
  LReservoirData: TReservoirData;
begin
  Result := False;
  try
    for LIndex := 0 to FReservoirAndNodesData.Count -1 do
    begin
      LReservoirData := TReservoirData(FReservoirAndNodesData.Items[LIndex]);
      if(LReservoirData.FReservoirConfigurationData.FPenaltyStruct = APenaltyID) then
        LReservoirData.FReservoirConfigurationData.PenaltyStructIdentifier := 0
      else
      if(LReservoirData.FReservoirConfigurationData.FPenaltyStruct > APenaltyID) then
        LReservoirData.FReservoirConfigurationData.PenaltyStructIdentifier :=
          LReservoirData.FReservoirConfigurationData.PenaltyStructIdentifier -1;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.ReservoirCountPerPenaltyStructure(APenaltyID: integer): integer;
const OPNAME = 'TReservoirDataList.ReservoirCountPerPenaltyStructure';
var
  LReservoirIndex: integer;
begin
  Result := 0;
  try
    for LReservoirIndex := 0 to FReservoirData.Count - 1 do
      if (ReservoirByIndex[LReservoirIndex].ReservoirConfigurationData.PenaltyStructIdentifier = APenaltyID) then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.GetHighestResevoirOrNodeNumber: integer;
const OPNAME = 'TReservoirDataList.GetHighestResevoirOrNodeNumber';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if(TReservoirData(FReservoirAndNodesData.Items[LIndex]).
         ReservoirConfigurationData.ReservoirIdentifier > Result) then
         Result := TReservoirData(FReservoirAndNodesData.Items[LIndex]).
                   ReservoirConfigurationData.ReservoirIdentifier
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.GetHighestResevoirOrNodeRecordIdentifier: integer;
const OPNAME = 'TReservoirDataList.GetHighestResevoirOrNodeRecordIdentifier';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if(TReservoirData(FReservoirAndNodesData.Items[LIndex]).
         ReservoirConfigurationData.RecordIdentifier > Result) then
         Result := TReservoirData(FReservoirAndNodesData.Items[LIndex]).
                   ReservoirConfigurationData.RecordIdentifier
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.CreateReservoir(ANodeType: TNodeType): IReservoirData;
const OPNAME = 'TReservoirDataList.CreateReservoir';
begin
  Result := nil;
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
    begin
      if(TYieldModelDataObject(FAppModules.Model.ModelData).
         NetworkElementData.ReservoirPenaltyStructureList.PenaltyCount = 0) then
      begin
        ShowMessage(FAppModules.Language.GetString('ReservoirPenalty.ZeroPenaltyCount'));
        Exit;
      end;

      if(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.
         CastHydrologyFileNames.FilesSavedInDatabaseCount = 0) then
      begin
        ShowMessage(FAppModules.Language.GetString('HydrologyFile.NoFileImported'));
      end;
    end;

    Result := CastCreateReservoir(ANodeType);
    if (Result <> nil) then
      if(FAppModules.Model.ModelName = CPlanning) then
        TReservoirData(Result).CreateNaturalInflowChannel;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.CastCreateReservoir(ANodeType: TNodeType = ntReservoir) : TReservoirData;
const OPNAME = 'TReservoirDataList.CastCreateReservoir';
var
  LReservoir : TReservoirData;
  LSQLAgent  : TReservoirDataSQLAgent;
begin
  Result := nil;
  try
    LReservoir := TReservoirData.Create(FAppModules);
    LReservoir.InitialiseNewReservoir(GetHighestResevoirOrNodeRecordIdentifier + 1,
                                      GetHighestResevoirOrNodeNumber + 1,ANodeType);

    LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      if(ANodeType = ntNodeWithInflow) then
      begin
        if LSQLAgent.CreateNodeWithInflow(LReservoir) then
        begin
          AddReservoirOrNodeToList(LReservoir);
          Result := LReservoir;
        end
        else
          LReservoir.Free;
      end
      else
      //if(ANodeType = ntReservoir) then
      begin
        if LSQLAgent.CreateReservoir(LReservoir,ReservoirCount+1,(ReservoirCount=0)) then
        begin
          AddReservoirOrNodeToList(LReservoir);
          Result := LReservoir;
        end
        else
          LReservoir.Free;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.DeleteReservoir(AReservoirNumber: integer): WordBool;
const OPNAME = 'TReservoirDataList.DeleteReservoir';
var
  LReservoir : TReservoirData;
  LSQLAgent  : TReservoirDataSQLAgent;
  LMsg       : string;
  LMesgDlgResult : word;
begin
  Result := False;
  try
    LReservoir := GetReservoirByIdentifierCast(AReservoirNumber);
    if not Assigned(LReservoir) then
      ShowMessage(FAppModules.Language.GetString('Message.TheReservoirNumber')+ IntToStr(AReservoirNumber)+FAppModules.Language.GetString('Message.DoesNotExist'))
    else
    begin
      LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        LMesgDlgResult := mrOk;
        if(FAppModules.Model.ModelName <>  CDDTS) then
        begin
          if(LReservoir.CastReservoirConfigurationData.FNodeType = ntReservoir) then
          begin
            LMsg :=FAppModules.Language.GetString('Message.ReservoirDeleteWarning');
            LMesgDlgResult := WRMFMessageDialog(LMsg,mtConfirmation,mbOKCancel,
                            ['Delete']);
            if (LMesgDlgResult = mrCancel) then Exit;
          end;
        end;
        if (LMesgDlgResult = mrOk) then
        begin
          if LSQLAgent.DeleteReservoir(LReservoir,ReservoirCount-1,(ReservoirCount=1)) then
          begin
            RemoveReservoirOrNodeFromList(LReservoir);
            Result := True;
          end;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.CreateNodeWithInflow(ANodeType: TNodeType): IReservoirData;
const OPNAME = 'TReservoirDataList.CreateNodeWithInflow';
begin
  Result := nil;
  try
    if(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.
       CastHydrologyFileNames.FilesSavedInDatabaseCount = 0) then
    begin
      ShowMessage(FAppModules.Language.GetString('HydrologyFile.NoFileImported'));
    end;

    Result := CastCreateReservoir(ntNodeWithInflow);
    if (Result <> nil) then
      if(FAppModules.Model.ModelName = CPlanning) then
        TReservoirData(Result).CreateNaturalInflowChannel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.DeleteNodeWithInflow(ANodeNumber: integer): WordBool;
const OPNAME = 'TReservoirDataList.DeleteNodeWithInflow';
var
  LNode      : TReservoirData;
  LSQLAgent  : TReservoirDataSQLAgent;
begin
  Result := False;
  try
    LNode := GetNodeWithInflowByIdentifierCast(ANodeNumber);
    if not Assigned(LNode) then
      ShowMessage(FAppModules.Language.GetString('Message.TheNodeNumber')+ IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.DoesNotExist'))
    else
    begin
      LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        if LSQLAgent.DeleteNodeWithInflow(LNode) then
        begin
          RemoveReservoirOrNodeFromList(LNode);
          Result := True;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.CreateNodeWithoutInflow(ANodeType: TNodeType): IReservoirData;
const OPNAME = 'TReservoirDataList.CreateNodeWithoutInflow';
var
  LNode:TReservoirData;
  LSQLAgent:TReservoirDataSQLAgent;
begin
  Result := nil;
  try
    LNode := TReservoirData.Create(FAppModules);
    LNode.InitialiseNewReservoir(GetHighestResevoirOrNodeRecordIdentifier + 1,
                                 GetHighestResevoirOrNodeNumber + 1,ANodeType);

    LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      if LSQLAgent.CreateNodeWithoutInflow(LNode) then
      begin
        AddReservoirOrNodeToList(LNode);
        Result := LNode;
      end
      else
        LNode.Free;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.DeleteNodeWithoutInflow(ANodeNumber: integer): WordBool;
const OPNAME = 'TReservoirDataList.DeleteNodeWithoutInflow';
var
  LNode      : TReservoirData;
  LSQLAgent  : TReservoirDataSQLAgent;
begin
  Result := False;
  try
    if(ANodeNumber = 0) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.TheZeroNode'));
      Exit;
    end;
    LNode := GetNodeWithoutInflowByIdentifierCast(ANodeNumber);
    if not Assigned(LNode) then
      ShowMessage(FAppModules.Language.GetString('Message.TheNodeNumber')+ IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.DoesNotExist'))
    else
    begin
      LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        if LSQLAgent.DeleteNodeWithoutInflow(LNode) then
        begin
          RemoveReservoirOrNodeFromList(LNode);
          Result := True;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirDataList.AddReservoirOrNodeToList(AReservoirOrNode: TReservoirData);
const OPNAME = 'TReservoirDataList.AddReservoirOrNodeToList';
begin
  try
    if Assigned(AReservoirOrNode) then
    begin
      FReservoirAndNodesData.Add(AReservoirOrNode);
      Case AReservoirOrNode.ReservoirConfigurationData.NodeType of
        ntReservoir,
        ntWetlandNode,
        ntGroundWater            : FReservoirData.Add(AReservoirOrNode);
        ntMinePolutionControlDam : FReservoirData.Add(AReservoirOrNode);
        ntMineUndergroundDam     : FReservoirData.Add(AReservoirOrNode);
        ntNodeWithInflow         : FNodeWithInflowData.Add(AReservoirOrNode);
        ntNodeWithoutInflow,
        ntMineNode,
        ntDemandCentreNode,
        ntBaseFlowNode,
        ntAbstractionNode,
        ntCollectionNode         : FNodeWithoutInflowData.Add(AReservoirOrNode);
        ntIrrigationNode         : FIrrigationNodeData.Add(AReservoirOrNode);
      else
        raise Exception.Create('Reservoir or node of unknown type used as parameter');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirDataList.RemoveReservoirOrNodeFromList(AReservoirOrNode: TReservoirData);
const OPNAME = 'TReservoirDataList.RemoveReservoirOrNodeFromList';
begin
  try
    if Assigned(AReservoirOrNode) then
    begin
      FReservoirData.Remove(AReservoirOrNode);
      FNodeWithInflowData.Remove(AReservoirOrNode);
      FNodeWithoutInflowData.Remove(AReservoirOrNode);
      FIrrigationNodeData.Remove(AReservoirOrNode);
      FReservoirAndNodesData.Remove(AReservoirOrNode);
      //FWetlandData.Remove(AReservoirOrNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.CreateNewIrrigationNode: IReservoirData;
const OPNAME = 'TReservoirDataList.CreateNewIrrigationNode';
var
  LNode     : TReservoirData;
  LSQLAgent : TReservoirDataSQLAgent;
begin
  Result := nil;
  try
    LNode := TReservoirData.Create(FAppModules);
    LNode.InitialiseNewReservoir(GetHighestResevoirOrNodeRecordIdentifier + 1,
                                 GetHighestResevoirOrNodeNumber + 1,ntIrrigationNode);

    LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      if LSQLAgent.CreateNodeWithoutInflow(LNode) then
      begin
        AddReservoirOrNodeToList(LNode);
        Result := LNode;
      end
      else
        LNode.Free;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.CreateIrrigationNode: IReservoirData;
const OPNAME = 'TReservoirDataList.CreateIrrigationNode';
var
  LNode : IReservoirData;
begin
  Result := nil;
  try
    lNode := CreateNewIrrigationNode;
    Result := lNode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.DeleteIrrigationNode(ANodeNumber: integer): boolean;
const OPNAME = 'TReservoirDataList.DeleteIrrigationNode';
var
  LNode:TReservoirData;
  LSQLAgent:TReservoirDataSQLAgent;
begin
  Result := False;
  try
    if(ANodeNumber = 0) then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.TheZeroNode'));
      Exit;
    end;
    LNode := GetIrrigationNodeByIdentifier(ANodeNumber);
    if not Assigned(LNode) then
      ShowMessage(FAppModules.Language.GetString('Message.TheNodeNumber')+ IntToStr(ANodeNumber)+FAppModules.Language.GetString('Message.DoesNotExist'))
    else
    begin
      LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
      try
        if LSQLAgent.DeleteNodeWithoutInflow(LNode) then
        begin
          RemoveReservoirOrNodeFromList(LNode);
          Result := True;
        end;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.GetIrrigationNodesCount: integer;
const OPNAME = 'TReservoirDataList.GetIrrigationNodesCount';
begin
  Result := 0;
  try
    Result := FIrrigationNodeData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.GetIrrigationNodeByIdentifier(ANodeIdentifier: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetIrrigationNodeByIdentifier';
var
  LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FIrrigationNodeData.Count - 1 do
    begin
      if (IrrigationNodeByIndex[LNodeIndex].ReservoirConfigurationData.ReservoirIdentifier = ANodeIdentifier) then
      begin
        Result := TReservoirData(FIrrigationNodeData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetIrrigationNodeByIndex(ANodeIndex: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetIrrigationNodeByIndex';
begin
  Result := nil;
  try
    if(ANodeIndex >= 0) and (ANodeIndex < FIrrigationNodeData.Count) then
      Result := TReservoirData(FIrrigationNodeData[ANodeIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.CreateReservoirsZone(AZoneLevel: integer; ACurrentReservoirNumber: integer): IReservoirZoneElevationsData;
const OPNAME = 'TReservoirDataList.CreateReservoirsZone';
var
  LCount: integer;
  LReservoirData:TReservoirData;
  LDrawDownElevation:TDrawDownElevation;
begin
  Result := nil;
  try
    for LCount := 0 to  ReservoirCount -1 do
    begin
      LReservoirData := CastReservoirByIndex[LCount];
      if(LReservoirData.FReservoirConfigurationData.ReservoirIdentifier = ACurrentReservoirNumber) then
      begin
        LDrawDownElevation := LReservoirData.FReservoirZoneElevationsData.CreateDrawDownLevel(AZoneLevel,LReservoirData,True);
        Result := LReservoirData.ReservoirZoneElevationsData;
      end
      else
        LDrawDownElevation := LReservoirData.FReservoirZoneElevationsData.CreateDrawDownLevel(AZoneLevel,LReservoirData,False);
      if not Assigned(LDrawDownElevation) then
        Exit;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.DeleteReservoirsZone(AZoneLevel: integer;ADeleteAction: TDeleteAction): boolean;
const OPNAME = 'TReservoirDataList.DeleteReservoirsZone';
var
  LCount: integer;
  LReservoirData:TReservoirData;
begin
  Result := False;
  try
    Result := True;
    for LCount := 0 to  ReservoirCount -1 do
    begin
      if not Result then
        Break;
      LReservoirData := CastReservoirByIndex[LCount];
      Result := Result and LReservoirData.FReservoirZoneElevationsData.DeleteDrawDownLevel(AZoneLevel,ADeleteAction);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList._AddRef: Integer;
const OPNAME = 'TReservoirDataList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList._Release: Integer;
const OPNAME = 'TReservoirDataList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_IrrigationNodeByIdentifier(ANodeIdentifier: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_IrrigationNodeByIdentifier';
var
  LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FIrrigationNodeData.Count - 1 do
    begin
      if (TReservoirData(FIrrigationNodeData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = ANodeIdentifier) then
      begin
        Result := TReservoirData(FIrrigationNodeData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_IrrigationNodeByIndex(ANodeIndex: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_IrrigationNodeByIndex';
begin
  Result := nil;
  try
    if(ANodeIndex >= 0) and (ANodeIndex < FIrrigationNodeData.Count) then
      Result := TReservoirData(FIrrigationNodeData[ANodeIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_IrrigationNodesCount: integer;
const OPNAME = 'TReservoirDataList.Get_IrrigationNodesCount';
begin
  Result := 0;
  try
    Result := FIrrigationNodeData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataList.GetNodeWithInflowByIdentifierCast(ANodeIdentifier: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetNodeWithInflowByIdentifierCast';
var
  LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FNodeWithInflowData.Count - 1 do
    begin
      if (TReservoirData(FNodeWithInflowData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = ANodeIdentifier) then
      begin
        Result := TReservoirData(FNodeWithInflowData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetNodeWithoutInflowByIdentifierCast(ANodeIdentifier: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetNodeWithoutInflowByIdentifierCast';
var
  LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FNodeWithoutInflowData.Count - 1 do
    begin
      if (TReservoirData(FNodeWithoutInflowData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = ANodeIdentifier) then
      begin
        Result := TReservoirData(FNodeWithoutInflowData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirDataList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to FReservoirAndNodesData.Count -1 do
    begin
      if not TReservoirData(FReservoirAndNodesData[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetReservoirOrNodeByIdentifierCast(AReservoirOrNodeIdentifier: Integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirOrNodeByIdentifierCast';
var LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if (TReservoirData(FReservoirAndNodesData[LNodeIndex]).ReservoirConfigurationData.ReservoirIdentifier = AReservoirOrNodeIdentifier) then
      begin
        Result := TReservoirData(FReservoirAndNodesData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetReservoirOrNodeByRecordIdentifierCast(AReservoirOrNodeRecordIdentifier: Integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirOrNodeByRecordIdentifierCast';
var LNodeIndex: integer;
begin
  Result := nil;
  try
    for LNodeIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if (TReservoirData(FReservoirAndNodesData[LNodeIndex]).ReservoirConfigurationData.RecordIdentifier = AReservoirOrNodeRecordIdentifier) then
      begin
        Result := TReservoirData(FReservoirAndNodesData[LNodeIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetReservoirOrNodeByIndexCast(AReservoirOrNodeIndex: Integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetReservoirOrNodeByIndexCast';
begin
  Result := nil;
  try
    if(AReservoirOrNodeIndex >= 0) and (AReservoirOrNodeIndex < FReservoirAndNodesData.Count) then
      Result := TReservoirData(FReservoirAndNodesData[AReservoirOrNodeIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetNodeWithInflowByIndexCast(AIndex: Integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetNodeWithInflowByIndexCast';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FNodeWithInflowData.Count) then
      Result := TReservoirData(FNodeWithInflowData[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.NodeHasInflow (ANodeNr : integer) : WordBool;
const OPNAME = 'TReservoirDataList.NodeHasInflow';
begin
  Result := FALSE;
  try
    if (NodeWithInflowByIdentifier[ANodeNr] <> nil) then
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.CopyCreate(AReservoirNumber: integer): IReservoirData;
const OPNAME = 'TReservoirDataList.CopyCreate';
var
  LSourceReservoir  : TReservoirData;
  LDestReservoir    : TReservoirData;
begin
  Result := nil;
  try
    LSourceReservoir := CastReservoirByIdentifier[AReservoirNumber];
    if (LSourceReservoir <> nil) then
    begin
      LDestReservoir := CastCreateReservoir(LSourceReservoir.ReservoirConfigurationData.NodeType);
      LDestReservoir.Assign(LSourceReservoir);
      Result := LDestReservoir;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.GetCastReservoirByReservoirGroupID(AGroupID: integer): TReservoirData;
const OPNAME = 'TReservoirDataList.GetCastReservoirByReservoirGroupID';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FReservoirAndNodesData.Count -1 do
      if(TReservoirData(FReservoirAndNodesData[LIndex]).FReservoirConfigurationData.
                        FGroudID = AGroupID) then
      begin
        Result := TReservoirData(FReservoirAndNodesData[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirDataList.Get_ReservoirOrNodeByName(const AName: WideString): IReservoirData;
const OPNAME = 'TReservoirDataList.Get_ReservoirOrNodeByName';
var
  lIndex: integer;
begin
  Result := nil;
  try
    for lIndex := 0 to FReservoirAndNodesData.Count - 1 do
    begin
      if (TReservoirData(FReservoirAndNodesData[lIndex]).ReservoirConfigurationData.ReservoirName = AName) then
      begin
        Result := TReservoirData(FReservoirAndNodesData[lIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TReservoirEvaporationsData }
function TReservoirEvaporationsData.Get_MonthlyEvaporationsByIndex(AIndex: integer): double;
const OPNAME = 'TReservoirEvaporationsData.Get_MonthlyEvaporationsByIndex';
begin
  Result := NullFloat;
  try
    Result := FMonthlyEvaporations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirEvaporationsData.LoadMonthlyEvaporationsFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirEvaporationsData.LoadMonthlyEvaporationsFromDataset';
begin
  Result := False;
  try
    if ADataset.DataSet.FieldByName('RecordIdentifier').IsNull then
      FRecordIdentifier        := NullInteger
    else
     FRecordIdentifier        := ADataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
    if ADataset.DataSet.FieldByName('Evapo01').IsNull then
      FMonthlyEvaporations[1]        := NullFloat
    else
      FMonthlyEvaporations[1]  := ADataset.DataSet.FieldByName('Evapo01').AsFloat;

    if ADataset.DataSet.FieldByName('Evapo02').IsNull then
      FMonthlyEvaporations[2]        := NullFloat
    else
      FMonthlyEvaporations[2]  := ADataset.DataSet.FieldByName('Evapo02').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo03').IsNull then
      FMonthlyEvaporations[3]        := NullFloat
    else
      FMonthlyEvaporations[3]  := ADataset.DataSet.FieldByName('Evapo03').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo04').IsNull then
      FMonthlyEvaporations[4]        := NullFloat
    else
      FMonthlyEvaporations[4]  := ADataset.DataSet.FieldByName('Evapo04').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo05').IsNull then
      FMonthlyEvaporations[5]        := NullFloat
    else
      FMonthlyEvaporations[5]  := ADataset.DataSet.FieldByName('Evapo05').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo06').IsNull then
      FMonthlyEvaporations[6]        := NullFloat
    else
      FMonthlyEvaporations[6]  := ADataset.DataSet.FieldByName('Evapo06').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo07').IsNull then
      FMonthlyEvaporations[7]        := NullFloat
    else
      FMonthlyEvaporations[7]  := ADataset.DataSet.FieldByName('Evapo07').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo08').IsNull then
      FMonthlyEvaporations[8]        := NullFloat
    else
      FMonthlyEvaporations[8]  := ADataset.DataSet.FieldByName('Evapo08').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo09').IsNull then
      FMonthlyEvaporations[9]        := NullFloat
    else
      FMonthlyEvaporations[9]  := ADataset.DataSet.FieldByName('Evapo09').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo10').IsNull then
      FMonthlyEvaporations[10]        := NullFloat
    else
      FMonthlyEvaporations[10] := ADataset.DataSet.FieldByName('Evapo10').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo11').IsNull then
      FMonthlyEvaporations[11]        := NullFloat
    else
      FMonthlyEvaporations[11] := ADataset.DataSet.FieldByName('Evapo11').AsFloat;
    if ADataset.DataSet.FieldByName('Evapo12').IsNull then
      FMonthlyEvaporations[12]        := NullFloat
    else
      FMonthlyEvaporations[12] := ADataset.DataSet.FieldByName('Evapo12').AsFloat;

    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirEvaporationsData.Initialise: boolean;
const OPNAME = 'TReservoirEvaporationsData.Initialise';
var
  LCount: integer;
begin
  Result := inherited Initialise;
  try
    FRecordIdentifier := NullInteger;
    for LCount := Low(FMonthlyEvaporations) to High(FMonthlyEvaporations) do
      FMonthlyEvaporations[LCount] := NullFloat;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirEvaporationsData.Set_MonthlyEvaporationsByIndex(AIndex: integer; ANewValue: double);
const OPNAME = 'TReservoirEvaporationsData.Set_MonthlyEvaporationsByIndex';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadEvaporationsContextData(LContextData,IntToStr(FRecordIdentifier),IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'Evaporation', FloatToStr(ANewValue), FloatToStr(FMonthlyEvaporations[AIndex]), LContextData) then
        begin
          LPrevValue := FMonthlyEvaporations[AIndex];
          FMonthlyEvaporations[AIndex] := ANewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Evaporation',FloatToStr(LPrevValue),FloatToStr(ANewValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationsData.Get_RecordIdentifier: integer;
const OPNAME = 'TReservoirEvaporationsData.Get_RecordIdentifier';
begin
  Result := NullInteger;
  try
    Result := FRecordIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirEvaporationsData.CreateMemberObjects;
const OPNAME = 'TReservoirEvaporationsData.CreateMemberObjects';
var
  LMonthlyEvaporations: TAbstractFieldProperty;
begin
  inherited;
  try
    LMonthlyEvaporations := FAppModules.FieldProperties.FieldProperty('Evaporation');
    if not Assigned(LMonthlyEvaporations) then
      raise Exception.Create('Field (Evaporation) not found in field properties');
      SetLength(FMonthlyEvaporations,LMonthlyEvaporations.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirEvaporationsData.DestroyMemberObjects;
const OPNAME = 'TReservoirEvaporationsData.DestroyMemberObjects';
begin
  try
    Finalize(FMonthlyEvaporations);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

procedure TReservoirEvaporationsData.InitialiseNewEvaporations(ANewRecordId: integer ;ANodeType: TNodeType);
const OPNAME = 'TReservoirEvaporationsData.InitialiseNewEvaporations';
var
  LIndex:  integer;
begin
  try
    FRecordIdentifier :=  ANewRecordId;
    for LIndex := Low(FMonthlyEvaporations) to High(FMonthlyEvaporations) do
      FMonthlyEvaporations[LIndex] := 0.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirEvaporationsData._AddRef: Integer;
const OPNAME = 'TReservoirEvaporationsData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirEvaporationsData._Release: Integer;
const OPNAME = 'TReservoirEvaporationsData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirEvaporationsData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirEvaporationsData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirEvaporationsData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirEvaporationsData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FRecordIdentifier)
    else
      Result := Result + ',Identifier=' + IntToStr(FRecordIdentifier);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationsData.CopyFrom (AResID  : integer;
                                              ASource : TReservoirEvaporationsData) : boolean;
const OPNAME = 'TReservoirEvaporationsData.CopyFrom';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FRecordIdentifier := AResID;
    for lIndex := Low(FMonthlyEvaporations) to High(FMonthlyEvaporations) do
      FMonthlyEvaporations[lIndex] := ASource.MonthlyEvaporationsByIndex[lIndex];
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationsData.Assign(ASource: TReservoirEvaporationsData);
const OPNAME = 'TReservoirEvaporationsData.Assign';
var
  LIndex : integer;
  LMonthlyEvaporations: TAbstractFieldProperty;
begin
  try
    if (ASource = nil) then Exit;
    LMonthlyEvaporations := FAppModules.FieldProperties.FieldProperty('Evaporation');
    for LIndex := LMonthlyEvaporations.ArrayLow to LMonthlyEvaporations.ArrayHigh do
      MonthlyEvaporationsByIndex[LIndex] := ASource.MonthlyEvaporationsByIndex[LIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirElevationsData }

procedure TReservoirElevationsData.CreateMemberObjects;
const OPNAME = 'TReservoirElevationsData.CreateMemberObjects';
var
  LReservoirElevations: TAbstractFieldProperty;
begin
  inherited;
  try
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    if not Assigned(LReservoirElevations) then
      raise Exception.Create('Field (SurfaceElevation) not found in field properties');
       SetLength(FReservoirElevations,LReservoirElevations.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.Initialise: boolean;
const OPNAME = 'TReservoirElevationsData.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FStartElevation := 3;
    for LIndex := Low(FReservoirElevations) to High(FReservoirElevations) do
      FReservoirElevations[LIndex] := NullFloat;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirElevationsData.Get_ReservoirElevationsByIndex(AIndex: integer): double;
const OPNAME = 'TReservoirElevationsData.Get_ReservoirElevationsByIndex';
begin
  Result := NullFloat;
  try
    Result := FReservoirElevations[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.LoadMonthlyElevationsFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirElevationsData.LoadMonthlyElevationsFromDataset';
var
  LIndex,
  LCount: integer;
  LReservoirElevations: TAbstractFieldProperty;
begin
  Result := False;
  try
    FStartElevation := 3;
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    for LIndex := LReservoirElevations.ArrayLow to LReservoirElevations.ArrayHigh do
    begin
      if ADataset.DataSet.FieldByName(Format('ReservoirElev%2.2d',[LIndex])).IsNull then
        Break;
      FStartElevation := LIndex;
    end;

    LCount := 1;

    FRecordIdentifier  := ADataset.DataSet.FieldByName('RecordIdentifier').AsInteger;

    for LIndex := FStartElevation downto 1 do
    begin
     if ADataset.DataSet.FieldByName(Format('ReservoirElev%2.2d',[LIndex])).IsNull then
       FReservoirElevations[LCount] := NullFloat
     else
       FReservoirElevations[LCount] := ADataset.DataSet.FieldByName(Format('ReservoirElev%2.2d',[LIndex])).AsFloat;
     LCount := LCount + 1;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirElevationsData.Set_ReservoirElevationsByIndex(AIndex: integer; ANewValue: double);
const OPNAME = 'TReservoirElevationsData.Set_ReservoirElevationsByIndex';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadElevationsContextData(LContextData,IntToStr(FRecordIdentifier),IntToStr(FStartElevation - AIndex + 1));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'SurfaceElevation', FloatToStr(ANewValue), FloatToStr(FReservoirElevations[AIndex]), LContextData) then
        begin
          LPrevValue :=  FReservoirElevations[AIndex];
          FReservoirElevations[AIndex] := ANewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SurfaceElevation',FloatToStr(LPrevValue),FloatToStr(ANewValue));
          RecalculateAreaWhenFull;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirElevationsData.Get_RecordIdentifier: integer;
const OPNAME = 'TReservoirElevationsData.Get_RecordIdentifier';
begin
  Result := NullInteger;
  try
    Result := FRecordIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.Get_StartElevation: integer;
const OPNAME = 'TReservoirElevationsData.Get_StartElevation';
begin
  Result := NullInteger;
  try
    Result := FStartElevation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirElevationsData.DestroyMemberObjects;
const OPNAME = 'TReservoirElevationsData.DestroyMemberObjects';
begin
  try
    Finalize(FReservoirElevations);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

procedure TReservoirElevationsData.InitialiseNewElevations(ANewRecordId: integer ;ANodeType: TNodeType);
const OPNAME = 'TReservoirElevationsData.InitialiseNewElevations';
var
  LIndex:  integer;
  LReservoirElevations: TAbstractFieldProperty;
begin
  try
    FRecordIdentifier :=  ANewRecordId;
    FStartElevation   := 3;
    for LIndex := Low(FReservoirElevations) to High(FReservoirElevations) do
      FReservoirElevations[LIndex] := NullFloat;

    if not (ANodeType in NodesSet) then
    begin
      LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
      for LIndex := LReservoirElevations.ArrayLow to FStartElevation do
        FReservoirElevations[LIndex] := 0.0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.DeleteElevationValue(AIndex: integer): WordBool;
const OPNAME = 'TReservoirElevationsData.DeleteElevationValue';
var
  LIndex:  integer;
  LReservoirElevations: TAbstractFieldProperty;
begin
  Result := False;
  try
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    if not Assigned(LReservoirElevations) then
      raise Exception.Create('Field (SurfaceElevation) not found in field properties');
    if (AIndex < LReservoirElevations.ArrayLow) or (AIndex > LReservoirElevations.ArrayHigh) then
      raise Exception.Create('Index('+IntToStr(AIndex)+') out of bounds');

    for LIndex := AIndex to FStartElevation - 1 do
      FReservoirElevations[LIndex] := FReservoirElevations[LIndex+1];
    FReservoirElevations[FStartElevation] := NullFloat;
    FStartElevation := FStartElevation -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.InsertElevationValue(AIndex: integer;AValue: double): WordBool;
const OPNAME = 'TReservoirElevationsData.InsertElevationValue';
var
  LIndex:  integer;
  LReservoirElevations: TAbstractFieldProperty;
begin
  Result := False;
  try
    if(FStartElevation = NullInteger) then
      FStartElevation := 3;

    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    if not Assigned(LReservoirElevations) then
      raise Exception.Create('Field (SurfaceElevation) not found in field properties');
    if (AIndex < LReservoirElevations.ArrayLow) or (AIndex > LReservoirElevations.ArrayHigh) then
      raise Exception.Create('Index('+IntToStr(AIndex)+') out of bounds');
    if((1 + FStartElevation) > LReservoirElevations.ArrayHigh) then
      raise Exception.Create('No more than('+IntToStr(LReservoirElevations.ArrayHigh)+') Elevations ca be added to the database');

    for LIndex := FStartElevation downto AIndex do
      FReservoirElevations[LIndex+1] := FReservoirElevations[LIndex];
    FReservoirElevations[AIndex] := AValue;
    FStartElevation := FStartElevation + 1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData._AddRef: Integer;
const OPNAME = 'TReservoirElevationsData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData._Release: Integer;
const OPNAME = 'TReservoirElevationsData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.CopyFrom(AResID: integer;ASource: TReservoirElevationsData): boolean;
const OPNAME = 'TReservoirElevationsData.CopyFrom';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    FRecordIdentifier := AResID;
    for lIndex := Low(FReservoirElevations) to High(FReservoirElevations) do
      FReservoirElevations[lIndex] := ASource.ReservoirElevationsByIndex[lIndex];
    FStartElevation := ASource.StartElevation;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirElevationsData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirElevationsData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirElevationsData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirElevationsData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirElevationsData.RecalculateAreaWhenFull: WordBool;
const OPNAME = 'TReservoirElevationsData.RecalculateAreaWhenFull';
var
  lReservoir     : IReservoirData;
begin
  Result := False;
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByID[FRecordIdentifier]
    else
      lReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirOrNodeByID[FRecordIdentifier];

  //  lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
  //                  NetworkElementData.ReservoirList.ReservoirOrNodeByID[FRecordIdentifier];
    if (lReservoir <> nil) then
    begin
      lReservoir.RecalculateAreaWhenFull;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirElevationsData.Assign(ASource: TReservoirElevationsData);
const OPNAME = 'TReservoirElevationsData.Assign';
var
  LIndex: integer;
  LReservoirElevations: TAbstractFieldProperty;
begin
  try
    if (ASource = nil) then Exit;
    FStartElevation := ASource.StartElevation;
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    for LIndex := LReservoirElevations.ArrayLow to FStartElevation do
        ReservoirElevationsByIndex[LIndex] := ASource.ReservoirElevationsByIndex[LIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirVolumesData }

procedure TReservoirVolumesData.CreateMemberObjects;
const OPNAME = 'TReservoirVolumesData.CreateMemberObjects';
var
  LReservoirVolumes: TAbstractFieldProperty;
begin
  inherited;
  try
    LReservoirVolumes := FAppModules.FieldProperties.FieldProperty('Volume');
    if not Assigned(LReservoirVolumes) then
      raise Exception.Create('Field (Volume) not found in field properties');
       SetLength(FReservoirVolumes,LReservoirVolumes.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.Get_ReservoirVolumesByIndex(AIndex: integer): double;
const OPNAME = 'TReservoirVolumesData.Get_ReservoirVolumesByIndex';
begin
  Result := NullFloat;
  try
    Result := FReservoirVolumes[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.LoadVolumesFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirVolumesData.LoadVolumesFromDataset';
var
  LIndex,
  LCount: integer;
begin
  Result := False;
  try
    FStartVolume := 3;
    for LIndex := 1 to 15 do
    begin
      if ADataset.DataSet.FieldByName(Format('Volume%2.2d',[LIndex])).IsNull then
        Break;
      FStartVolume := LIndex;
    end;

    LCount := 1;
    FRecordIdentifier  := ADataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
    for LIndex := FStartVolume downto 1 do
    begin
     if ADataset.DataSet.FieldByName(Format('Volume%2.2d',[LIndex])).IsNull then
       FReservoirVolumes[LCount] := NullFloat
     else
       FReservoirVolumes[LCount] := ADataset.DataSet.FieldByName(Format('Volume%2.2d',[LIndex])).AsFloat;
     LCount := LCount + 1;
    end;
    Result := True;

  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirVolumesData.Initialise: boolean;
const OPNAME = 'TReservoirVolumesData.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FStartVolume := 3;
    for LIndex := Low(FReservoirVolumes) to High(FReservoirVolumes) do
      FReservoirVolumes[LIndex] := NullFloat;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirVolumesData.Set_ReservoirVolumesByIndex(AIndex: integer; ANewValue: double);
const OPNAME = 'TReservoirVolumesData.Set_ReservoirVolumesByIndex';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadVolumesContextData(LContextData,IntToStr(FRecordIdentifier),IntToStr(FStartVolume - AIndex + 1));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'Volume', FloatToStr(ANewValue), FloatToStr(FReservoirVolumes[AIndex]), LContextData) then
        begin
          LPrevValue := FReservoirVolumes[AIndex];
          FReservoirVolumes[AIndex] := ANewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Volume',FloatToStr(LPrevValue),FloatToStr(ANewValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirVolumesData.Get_RecordIdentifier: integer;
const OPNAME = 'TReservoirVolumesData.Get_RecordIdentifier';
begin
  Result := NullInteger;
  try
    Result := FRecordIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.Get_StartVolume: integer;
const OPNAME = 'TReservoirVolumesData.Get_StartVolume';
begin
  Result := NullInteger;
  try
    Result := FStartVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirVolumesData.DestroyMemberObjects;
const OPNAME = 'TReservoirVolumesData.DestroyMemberObjects';
begin
  try
    Finalize(FReservoirVolumes);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

procedure TReservoirVolumesData.InitialiseNewVolumes(ANewRecordId: integer ;ANodeType: TNodeType);
const OPNAME = 'TReservoirVolumesData.InitialiseNewVolumes';
var
  LIndex            :  integer;
  LReservoirVolumes : TAbstractFieldProperty;
begin
  try
    FRecordIdentifier :=  ANewRecordId;
    FStartVolume      := 3;
    for LIndex := Low(FReservoirVolumes) to High(FReservoirVolumes) do
    FReservoirVolumes[LIndex] := NullFloat;

    if not (ANodeType in NodesSet) then
    begin
      LReservoirVolumes := FAppModules.FieldProperties.FieldProperty('Volume');
      for LIndex := LReservoirVolumes.ArrayLow to FStartVolume do
        FReservoirVolumes[LIndex] := 0.0;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.DeleteVolumeValue(AIndex: integer): WordBool;
const OPNAME = 'TReservoirVolumesData.DeleteVolumeValue';
var
  LIndex:  integer;
  LReservoirVolumes: TAbstractFieldProperty;
begin
  Result := False;
  try
    LReservoirVolumes := FAppModules.FieldProperties.FieldProperty('Volume');
    if not Assigned(LReservoirVolumes) then
      raise Exception.Create('Field (Volume) not found in field properties');
    if (AIndex < LReservoirVolumes.ArrayLow) or (AIndex > LReservoirVolumes.ArrayHigh) then
      raise Exception.Create('Index('+IntToStr(AIndex)+') out of bounds');

    for LIndex := AIndex to FStartVolume-1 do
      FReservoirVolumes[LIndex] := FReservoirVolumes[LIndex+1];
    FReservoirVolumes[FStartVolume] := NullFloat;
    FStartVolume := FStartVolume - 1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.InsertVolumeValue(AIndex: integer;AValue: double): WordBool;
const OPNAME = 'TReservoirVolumesData.InsertVolumeValue';
var
  LIndex:  integer;
  LReservoirVolumes: TAbstractFieldProperty;
begin
  Result := False;
  try
    if(FStartVolume = NullInteger) then
      FStartVolume := 3;

    LReservoirVolumes := FAppModules.FieldProperties.FieldProperty('Volume');
    if not Assigned(LReservoirVolumes) then
      raise Exception.Create('Field (Volume) not found in field properties');
    if (AIndex < LReservoirVolumes.ArrayLow) or (AIndex > LReservoirVolumes.ArrayHigh) then
      raise Exception.Create('Index('+IntToStr(AIndex)+') out of bounds');
    if((1 + FStartVolume) > LReservoirVolumes.ArrayHigh) then
      raise Exception.Create('No more than('+IntToStr(LReservoirVolumes.ArrayHigh)+') Volume ca be added to the database');

    for LIndex := FStartVolume downto AIndex do
      FReservoirVolumes[LIndex+1] := FReservoirVolumes[LIndex];
    FReservoirVolumes[AIndex] := AValue;
    FStartVolume := FStartVolume + 1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData._AddRef: Integer;
const OPNAME = 'TReservoirVolumesData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData._Release: Integer;
const OPNAME = 'TReservoirVolumesData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.CopyFrom(AResID: integer;
                                        ASource: TReservoirVolumesData): boolean;
const OPNAME = 'TReservoirVolumesData.CopyFrom';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    FRecordIdentifier := AResID;
    for lIndex := Low(FReservoirVolumes) to High(FReservoirVolumes) do
      FReservoirVolumes[lIndex] := ASource.ReservoirVolumesByIndex[lIndex];
    FStartVolume := ASource.StartVolume;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirVolumesData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirVolumesData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirVolumesData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirVolumesData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirVolumesData.Get_MaxReservoirVolume: Double;
const OPNAME = 'TReservoirVolumesData.Get_MaxReservoirVolume';
var
  LVolumes : TStringList;
  LIndex   : integer;
  LPos     : integer;
  LValue   : double;
begin
  Result := 0.0;
  try
    LVolumes := TStringList.Create;
    try
      LVolumes.Sorted := True;
      LVolumes.Sort;
      for LIndex := Low(FReservoirVolumes) to High(FReservoirVolumes) do
        LVolumes.Add(FormatFloat('00000000000000.000',FReservoirVolumes[LIndex]));

      LPos := LVolumes.Count;
      LValue :=  StrToFloat(LVolumes[LPos-2]);
      //Result := StrToFloat(FormatFloat('###0.00',LValue));
      Result := LValue;
    finally
      LVolumes.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumesData.Assign(ASource: TReservoirVolumesData);
const OPNAME = 'TReservoirVolumesData.Assign';
var
  LIndex: integer;
  LReservoirVolumes: TAbstractFieldProperty;
begin
  try
    if (ASource = nil) then Exit;
    FStartVolume := ASource.StartVolume;
    LReservoirVolumes := FAppModules.FieldProperties.FieldProperty('Volume');
    for LIndex := LReservoirVolumes.ArrayLow to FStartVolume do
      ReservoirVolumesByIndex[LIndex] := ASource.ReservoirVolumesByIndex[LIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirAreasData }

procedure TReservoirAreasData.CreateMemberObjects;
const OPNAME = 'TReservoirAreasData.CreateMemberObjects';
var
  LReservoirArea: TAbstractFieldProperty;
begin
  inherited;
  try
    LReservoirArea := FAppModules.FieldProperties.FieldProperty('Area');
    if not Assigned(LReservoirArea) then
      raise Exception.Create('Field (Area) not found in field properties');
      SetLength(FReservoirAreas,LReservoirArea.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.LoadAreasFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirAreasData.LoadAreasFromDataset';
var
  LIndex,
  LCount: integer;
begin
  Result := False;
  try
    FStartArea := 3;
    for LIndex := 1 to 15 do
    begin
      if ADataset.DataSet.FieldByName(Format('Area%2.2d',[LIndex])).IsNull then
        Break;
      FStartArea := LIndex;
    end;

    LCount := 1;
    FRecordIdentifier  := ADataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
    for LIndex := FStartArea downto 1 do
    begin
      if ADataset.DataSet.FieldByName(Format('Area%2.2d',[LIndex])).IsNull then
        FReservoirAreas[LCount] := NullFloat
      else
        FReservoirAreas[LCount] := ADataset.DataSet.FieldByName(Format('Area%2.2d',[LIndex])).AsFloat;
      LCount := LCount + 1;
    end;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirAreasData.Initialise: boolean;
const OPNAME = 'TReservoirAreasData.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FStartArea := 3;
    for LIndex := Low(FReservoirAreas) to High(FReservoirAreas) do
      FReservoirAreas[LIndex] := NullFloat;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirAreasData.Get_ReservoirAreasByIndex(AIndex: integer): double;
const OPNAME = 'TReservoirAreasData.Get_ReservoirAreasByIndex';
begin
  Result := NullFloat;
  try
    Result := FReservoirAreas[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirAreasData.Set_ReservoirAreasByIndex(AIndex: integer; ANewValue: double);
const OPNAME = 'TReservoirAreasData.Set_ReservoirAreasByIndex';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadAreasContextData(LContextData,IntToStr(FRecordIdentifier),IntToStr(FStartArea - AIndex + 1));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'Area', FloatToStr(ANewValue), FloatToStr(FReservoirAreas[AIndex]), LContextData) then
        begin
          LPrevValue := FReservoirAreas[AIndex];
          FReservoirAreas[AIndex] := ANewValue;
          UpdateMaxArea;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Area',FloatToStr(LPrevValue),FloatToStr(ANewValue));
          RecalculateAreaWhenFull;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreasData.Get_StartArea: integer;
const OPNAME = 'TReservoirAreasData.Get_StartArea';
begin
  Result := NullInteger;
  try
    Result := FStartArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.Get_RecordIdentifier: integer;
const OPNAME = 'TReservoirAreasData.Get_RecordIdentifier';
begin
  Result := NullInteger;
  try
    Result := FRecordIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirAreasData.DestroyMemberObjects;
const OPNAME = 'TReservoirAreasData.DestroyMemberObjects';
begin
  try
    Finalize(FReservoirAreas);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;
procedure TReservoirAreasData.InitialiseNewAreas(ANewRecordId: integer ;ANodeType: TNodeType);
const OPNAME = 'TReservoirAreasData.InitialiseNewAreas';
var
  LIndex          :  integer;
  LReservoirAreas : TAbstractFieldProperty;
begin
  try
    FRecordIdentifier :=  ANewRecordId;
    FStartArea        := 3;
    for LIndex := Low(FReservoirAreas) to High(FReservoirAreas) do
    FReservoirAreas[LIndex] := NullFloat;

    if not (ANodeType in NodesSet) then
    begin
      LReservoirAreas := FAppModules.FieldProperties.FieldProperty('Area');
      if not Assigned(LReservoirAreas) then
        raise Exception.Create('Field (Area) not found in field properties');
      for LIndex := LReservoirAreas.ArrayLow to LReservoirAreas.ArrayLow + 2 do
        FReservoirAreas[LIndex] := 0.0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.DeleteAreaValue(AIndex: integer): WordBool;
const OPNAME = 'TReservoirAreasData.DeleteAreaValue';
var
  LIndex:  integer;
  LReservoirArea: TAbstractFieldProperty;
begin
  Result := False;
  try
    LReservoirArea := FAppModules.FieldProperties.FieldProperty('Area');
    if not Assigned(LReservoirArea) then
      raise Exception.Create('Field (Area) not found in field properties');
    if (AIndex < LReservoirArea.ArrayLow) or (AIndex > LReservoirArea.ArrayHigh) then
      raise Exception.Create('Index('+IntToStr(AIndex)+') out of bounds');


    for LIndex := AIndex  to FStartArea-1 do
      FReservoirAreas[LIndex] := FReservoirAreas[LIndex+1];
    FReservoirAreas[FStartArea] := NullFloat;
    FStartArea := FStartArea - 1;
    UpdateMaxArea;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.InsertAreaValue(AIndex: integer; AValue: double): WordBool;
const OPNAME = 'TReservoirAreasData.InsertAreaValue';
var
  LIndex:  integer;
  LReservoirArea: TAbstractFieldProperty;
begin
  Result := False;
  try
    if(FStartArea = NullInteger) then
      FStartArea := 0;

    LReservoirArea := FAppModules.FieldProperties.FieldProperty('Area');
    if not Assigned(LReservoirArea) then
      raise Exception.Create('Field (Area) not found in field properties');
    if (AIndex < LReservoirArea.ArrayLow) or (AIndex > LReservoirArea.ArrayHigh) then
      raise Exception.Create('Index('+IntToStr(AIndex)+') out of bounds');
    if((1 + FStartArea) > LReservoirArea.ArrayHigh) then
      raise Exception.Create('No more than('+IntToStr(LReservoirArea.ArrayHigh)+') Area ca be added to the database');

    for LIndex := FStartArea downto AIndex do
      FReservoirAreas[LIndex+1] := FReservoirAreas[LIndex];
    FReservoirAreas[AIndex] := AValue;
    FStartArea := FStartArea + 1;
    UpdateMaxArea;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.Get_MaxArea: double;
const OPNAME = 'TReservoirAreasData.Get_MaxArea';
var
  LIndex: integer;
begin
  Result := 0.0;
  try
    for LIndex := 0 to FStartArea do
      if(FReservoirAreas[LIndex] > Result) then
         Result := FReservoirAreas[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.CopyFrom(AResID: integer;
                                      ASource: TReservoirAreasData): boolean;
const OPNAME = 'TReservoirAreasData.CopyFrom';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    FRecordIdentifier := AResID;
    for lIndex := Low(FReservoirAreas) to High(FReservoirAreas) do
      FReservoirAreas[lIndex] := ASource.ReservoirAreasByIndex[lIndex];
    FStartArea := ASource.StartArea;
    Result := TRUE;
 except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TReservoirAreasData.UpdateMaxArea;
const OPNAME = 'TReservoirAreasData.UpdateMaxArea';
var
  LMaxArea: double;
  LReservoir: TReservoirData;
begin
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.GetReservoirByRecordIdentifier(FRecordIdentifier)
    else
      lReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.GetReservoirByRecordIdentifier(FRecordIdentifier);

  // LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
   //              CastNetworkElementData.CastReservoirList.GetReservoirByRecordIdentifier(FRecordIdentifier);
   if Assigned(LReservoir) then
   begin
     LMaxArea := Get_MaxArea;
     LReservoir.CastReservoirConfigurationData.PopulateMaxArea(LMaxArea);
   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData._AddRef: Integer;
const OPNAME = 'TReservoirAreasData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData._Release: Integer;
const OPNAME = 'TReservoirAreasData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirAreasData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirAreasData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirAreasData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAreasData.RecalculateAreaWhenFull: WordBool;
const OPNAME = 'TReservoirAreasData.RecalculateAreaWhenFull';
var
  lReservoir     : IReservoirData;
begin
  Result := False;
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByID[FRecordIdentifier]
    else
      lReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirOrNodeByID[FRecordIdentifier];
    //lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
    //                NetworkElementData.ReservoirList.ReservoirOrNodeByID[FRecordIdentifier];
    if (lReservoir <> nil) then
    begin
      lReservoir.RecalculateAreaWhenFull;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAreasData.Assign(ASource: TReservoirAreasData);
const OPNAME = 'TReservoirAreasData.Assign';
var
  LIndex: integer;
  LReservoirArea: TAbstractFieldProperty;
begin
  try
    if (ASource = nil) then Exit;
    FStartArea := ASource.StartArea;
    LReservoirArea := FAppModules.FieldProperties.FieldProperty('Area');
    for LIndex := LReservoirArea.ArrayLow to FStartArea do
      ReservoirAreasByIndex[LIndex] := ASource.ReservoirAreasByIndex[LIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{* TReservoirTimeControl                                                      *}
{******************************************************************************}

procedure TReservoirTimeControl.CreateMemberObjects;
const OPNAME = 'TReservoirTimeControl.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReplacements          := TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.DestroyMemberObjects;
const OPNAME = 'TReservoirTimeControl.DestroyMemberObjects';
var
  lBaseNode : IReservoirData;
begin
  try
    if (FAppModules.Model <> nil) AND (FAppModules.Model.ModelData <> nil) AND
       (FBaseNodeNumber <> FReservoirNumber) AND (FBaseNodeNumber <> 0) then
    begin
      lBaseNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                     ReservoirList.ReservoirByIdentifier[FBaseNodeNumber];
      if (lBaseNode <> nil) then
      begin
        lBaseNode.TimeControl.DeleteReplacement(FReservoirNumber);
        lBaseNode := nil;
      end;
    end;
    FreeAndNil(FReplacements);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl._AddRef: Integer;
const OPNAME = 'TReservoirTimeControl._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl._Release: Integer;
const OPNAME = 'TReservoirTimeControl._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirTimeControl.Initialise: boolean;
const OPNAME = 'TReservoirTimeControl.Initialise';
begin
  Result := FALSE;
  try
    FReservoirNumber             := 0;
    FReservoirStartYear          := 0;
    FReservoirStartMonth         := 0;
    FReservoirEndYear            := 0;
    FReservoirEndMonth           := 0;
    FReservoirEconomicLife       := 0;
    FReservoirCapitalCost        := 0.0;
    FReservoirOMCost             := 0.0;
    FBaseNodeNumber              := 0;
    FReservoirCostSchedule       := '';
    FReplacements.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_StartYear: Integer;
const OPNAME = 'TReservoirTimeControl.Get_StartYear';
begin
  Result := 0;
  try
     Result := FReservoirStartYear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_StartYear(Value: Integer);
const OPNAME = 'TReservoirTimeControl.Set_StartYear';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirStartYear',  IntToStr(Value),IntToStr(FReservoirStartYear), LContextData) then
        begin
          FReservoirStartYear := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirStartYear', IntToStr(FReservoirStartYear),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_StartMonth: Integer;
const OPNAME = 'TReservoirTimeControl.Get_StartMonth';
begin
  Result := 0;
  try
    Result := FReservoirStartMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_StartMonth(Value: Integer);
const OPNAME = 'TReservoirTimeControl.Set_StartMonth';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirStartMonth',  IntToStr(Value),IntToStr(FReservoirStartMonth), LContextData) then
        begin
          FReservoirStartMonth := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirStartMonth', IntToStr(FReservoirStartMonth),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_EndYear: Integer;
const OPNAME = 'TReservoirTimeControl.Get_EndYear';
begin
  Result := 0;
  try
    Result := FReservoirEndYear
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_EndYear(Value: Integer);
const OPNAME = 'TReservoirTimeControl.Set_EndYear';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirEndYear',  IntToStr(Value),IntToStr(FReservoirEndYear), LContextData) then
        begin
          FReservoirEndYear := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirEndYear', IntToStr(FReservoirEndYear),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_EndMonth: Integer;
const OPNAME = 'TReservoirTimeControl.Get_EndMonth';
begin
  Result := Result;
  try
    Result := FReservoirEndMonth
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_EndMonth(Value: Integer);
const OPNAME = 'TReservoirTimeControl.Set_EndMonth';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirEndMonth',  IntToStr(Value),IntToStr(FReservoirEndMonth), LContextData) then
        begin
          FReservoirEndMonth := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirEndMonth', IntToStr(FReservoirEndMonth),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_EconomicLife: Integer;
const OPNAME = 'TReservoirTimeControl.Get_EconomicLife';
begin
  Result := 0;
  try
    Result := FReservoirEconomicLife;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_EconomicLife(Value: Integer);
const OPNAME = 'TReservoirTimeControl.Set_EconomicLife';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirEconomicLife',  IntToStr(Value),IntToStr(FReservoirEconomicLife), LContextData) then
        begin
          FReservoirEconomicLife := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirEconomicLife', IntToStr(FReservoirEconomicLife),IntToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirTimeControl.Get_CapitalCost: double;
const OPNAME = 'TReservoirTimeControl.Get_CapitalCost';
begin
  Result := 0.0;
  try
    Result := FReservoirCapitalCost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_CapitalCost(Value: double);
const OPNAME = 'TReservoirTimeControl.Set_CapitalCost';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirCapitalCost',  FloatToStr(Value),FloatToStr(FReservoirOMCost), LContextData) then
        begin
          FReservoirCapitalCost := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirCapitalCost', FloatToStr(FReservoirCapitalCost),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_OMCost: double;
const OPNAME = 'TReservoirTimeControl.Get_OMCost';
begin
  Result := 0;
  try
    Result := FReservoirOMCost;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_OMCost(Value: double);
const OPNAME = 'TReservoirTimeControl.Set_OMCost';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirOMCost',  FloatToStr(Value),FloatToStr(FReservoirOMCost), LContextData) then
        begin
          FReservoirOMCost := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirOMCost', FloatToStr(FReservoirOMCost),FloatToStr(Value));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_YearsToConstruct: Integer;
const OPNAME = 'TReservoirTimeControl.Get_YearsToConstruct';
begin
  Result := 0;
  try
    Result :=  StringsItemsCount(FReservoirCostSchedule);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_YearsToConstruct(Value: Integer);
const OPNAME = 'TReservoirTimeControl.Set_YearsToConstruct';
var
  LTempStr     : string;
begin
  try
    if(Value < 0) then Exit;
    LTempStr := FReservoirCostSchedule;
    if ChangeSizeCommatextString(Value,'0.0',LTempStr) then
      CostSchedule := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_CostSchedule: WideString;
const OPNAME = 'TReservoirTimeControl.Get_CostSchedule';
begin
  Result := '';
  try
     Result := UnCompressCommatextString(FReservoirCostSchedule);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_CostSchedule(const Value: WideString);
const OPNAME = 'TReservoirTimeControl.Set_CostSchedule';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LTempStr     : string;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LContextData.Clear;
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData,
                   IntToStr(FReservoirNumber));
        begin
          LTempStr := CompressCommatextString(Value);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ReservoirCostSchedule', Value, FReservoirCostSchedule, LContextData) then
          begin
            LOldValue := FReservoirCostSchedule;
            FReservoirCostSchedule := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirCostSchedule',LOldValue,Value);
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

function TReservoirTimeControl.Get_CostScheduleByIndex (AIndex : integer) : double;
const OPNAME = 'TReservoirTimeControl.Get_CostScheduleByIndex';
var
  LTempStr: string;
begin
  Result := NullFloat;
  try
    LTempStr := GetCompressedCommaTextIndexValue(AIndex,FReservoirCostSchedule);
    Result   := StrToFloatDef(LTempStr,NullFloat);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_CostScheduleByIndex (AIndex : integer;
                                                         AValue : double);
const OPNAME = 'TReservoirTimeControl.Set_CostScheduleByIndex';
var
  LTempStr : string;
begin
  try
    LTempStr := FReservoirCostSchedule;
    if UpdateCompressCommatextString(AIndex, FloatToStr(AValue), LTempStr) then
      CostSchedule := LTempStr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_BaseNodeNumber: Integer;
const OPNAME = 'TReservoirTimeControl.Get_BaseNodeNumber';
begin
  Result := 0;
  try
    Result := FBaseNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Set_BaseNodeNumber(AValue: Integer);
const OPNAME = 'TReservoirTimeControl.Set_BaseNodeNumber';
var
  LLoadAgent   : TReservoirTimeControlSQLAgent;
  LContextData : TStringList;
  LBaseNode : IReservoirData;
  LOldValue : integer;
begin
  try
    LLoadAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_ReservoirTimeControl(LContextData, IntToStr(FReservoirNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'BaseNodeNumber',  IntToStr(AValue),IntToStr(FBaseNodeNumber), LContextData) then
        begin
          LOldValue := FBaseNodeNumber;
          FBaseNodeNumber := AValue;
          LBaseNode := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                          ReservoirList.ReservoirByIdentifier[FBaseNodeNumber];
          if LBaseNode <> nil then
          begin
            if LBaseNode.TimeControl <> nil then
              LBaseNode.TimeControl.AddReplacement(FReservoirNumber);
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'BaseNodeNumber', IntToStr(LOldValue),IntToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirTimeControl.Get_ReservoirNumber: Integer;
const OPNAME = 'TReservoirTimeControl.Get_ReservoirNumber';
begin
  Result := 0;
  try
    Result := ReservoirNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TReservoirTimeControl.Validate';
var
  LErrorCols : TStringList;
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    LErrorList := TStringList.Create;
    LErrorCols := TStringList.Create;
    try
      if (AContext = 'ReservoirStartYear') then
        Result := ValidateStartYear(lErrorList)
      else
      if (AContext = 'ReservoirStartMonth') then
        Result := ValidateStartMonth(lErrorList)
      else
      if (AContext = 'ReservoirEndYear') then
        Result := ValidateEndYear(lErrorList)
      else
      if (AContext = 'ReservoirEndMonth') then
         Result := ValidateEndMonth(lErrorList)
      else
      if (AContext = 'ReservoirEconomicLife') then
        Result := ValidateEconomicLife(lErrorList)
      else
      if (AContext = 'ReservoirCapitalCost') then
        Result := ValidateCapitalCost(lErrorList)
      else
      if (AContext = 'ReservoirOMCost') then
        Result := ValidateOMCost(lErrorList)
      else
      if (AContext = 'ReservoirYearsToConstruct') then
        Result := ValidateYearsToConstruct(lErrorList)
      else
      if (AContext = 'ReservoirCostSchedule') then
         Result := ValidateCostSchedule(lErrorList)
      else
      if (AContext = 'BaseNodeNumber') then
        Result := ValidateBaseNodeNumber(lErrorList)
      else

      if (AContext = 'Replacements') then
      begin
        Result := ValidateReplacements(LErrorList,LErrorCols);
        if (not Result) then
          AddErrors(AErrors, LErrorList, LErrorCols);
      end
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateStartYear(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStartMonth(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEndYear(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEndMonth(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateEconomicLife(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCapitalCost(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateOMCost(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateYearsToConstruct(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCostSchedule(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
      FreeAndNil(LErrorCols);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControl.Populate(AReservoirNumber       : integer;
                       ABaseNodeNumber        : integer;
                       AReservoirStartYear    : integer;
                       AReservoirStartMonth   : integer;
                       AReservoirEndYear      : integer;
                       AReservoirEndMonth     : integer;
                       AReservoirEconomicLife : integer;
                       AReservoirCapitalCost  : double;
                       AReservoirOMCost       : double;
                       AReservoirCostSchedule     : string): Boolean;
const OPNAME = 'TReservoirTimeControl.Populate';
begin
  Result := FALSE;
  try
    FReservoirNumber                 := AReservoirNumber;
    FBaseNodeNumber                  := ABaseNodeNumber;
    FReservoirStartYear              := AReservoirStartYear;
    FReservoirStartMonth             := AReservoirStartMonth;
    FReservoirEndYear                := AReservoirEndYear;
    FReservoirEndMonth               := AReservoirEndMonth;
    FReservoirEconomicLife           := AReservoirEconomicLife;
    FReservoirCapitalCost            := AReservoirCapitalCost;
    FReservoirOMCost                 := AReservoirOMCost;
    FReservoirCostSchedule           := AReservoirCostSchedule;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.AddReplacement(AResNr: Integer);
const OPNAME = 'TReservoirTimeControl.AddReplacement';
begin
  try
    FReplacements.Add(IntToStr(AResNr));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.DeleteReplacement(AResNr: Integer);
const OPNAME = 'TReservoirTimeControl.DeleteReplacement';
var
  lIndex : integer;
begin
  try
    lIndex :=  FReplacements.IndexOf(IntToStr(AResNr));
    if (lIndex >= 0) then
      FReplacements.Delete(lIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.Get_Replacements: WideString;
const OPNAME = 'TReservoirTimeControl.Get_Replacements';
begin
  Result := '';
  try
    Result := FReplacements.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TReservoirTimeControl.ValidateStartYear(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateStartYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirStartYear',
            IntToStr(FReservoirStartYear), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReservoirStartYear)+ ':'+lMessage)
    else
    if (StartYear > EndYear) then
      AErrorMessages.Add('ERROR:' +FAppModules.Language.GetString('ContextValidation.InvalidDate'))
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateStartMonth(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateStartMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirStartMonth',
            IntToStr(FReservoirStartMonth), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReservoirStartMonth)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateEndYear(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateEndYear';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirEndYear',
            IntToStr(FReservoirEndYear), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReservoirEndYear)+ ':'+lMessage);
    if (EndYear < StartYear) then
      AErrorMessages.Add('ERROR:' +FAppModules.Language.GetString('ContextValidation.InvalidDate'))
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateEndMonth(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateEndMonth';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirEndMonth',
            IntToStr(FReservoirEndMonth), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReservoirEndMonth)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateEconomicLife(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateEconomicLife';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirEconomicLife',
            IntToStr(FReservoirEconomicLife), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FReservoirEconomicLife)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateCapitalCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateCapitalCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirCapitalCost',
            FloatToStr(FReservoirCapitalCost), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FReservoirCapitalCost)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateOMCost(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateOMCost';
var
  lMessage : string;
begin
  Result := FALSE;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirOMCost',
            FloatToStr(FReservoirOMCost), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FReservoirOMCost)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateYearsToConstruct(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateYearsToConstruct';
var
  lMessage : string;
  LYearsToConstruct: integer;
begin
  Result := FALSE;
  try
    lMessage := '';
    LYearsToConstruct := YearsToConstruct;
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirYearsToConstruct',
            IntToStr(LYearsToConstruct), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(LYearsToConstruct)+ ':'+lMessage)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateCostSchedule(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateCostSchedule';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ReservoirCostSchedule',
            FReservoirCostSchedule, LMessage)) then
      AErrorMessages.Add('ERROR:' +FReservoirCostSchedule + ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateBaseNodeNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateBaseNodeNumber';
var
  LMessage : string;
begin
  Result := FALSE;
  try
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('BaseNodeNumber',
            IntToStr(FBaseNodeNumber), LMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FBaseNodeNumber) + ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirTimeControl.ValidateReplacements(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
const OPNAME = 'TReservoirTimeControl.ValidateReplacements';
var
  LIndex : integer;
  LMessage : string;

  LCurReplacementStartDate : TDateTime;
  LCurReplacementEndDate : TDateTime;

  LPreReplacementStartDate : TDateTime;

  LDay : word;
  LReservoirStartMonth : word;
  LReservoirEndMonth : word;
  LReservoirStartYear,
  LReservoirEndYear : word;
  LReplacement : IReservoirData;
  LResult : WordBool;
begin
  Result := False;
  try
    LMessage := '';
    LResult := True;
    LDay := 1;
    LReservoirStartMonth := 0;
    LReservoirEndMonth := 0;
    LReservoirStartYear := 0;
    LReservoirEndYear := 0;
    LPreReplacementStartDate := 0;
    if FReservoirStartYear = 0 then
    begin
      LMessage := 'Invalid Base Reservoir Start Year.';
      AErrorMessages.Add('ERROR:'+LMessage);
      AErrorColumns.Add('1');
      LResult := False;
    end
    else
      LReservoirStartYear := FReservoirStartYear;
    if FReservoirStartMonth = 0 then
    begin
      LMessage := 'Invalid Base Reservoir Start Month.';
      AErrorMessages.Add('ERROR:'+LMessage);
      AErrorColumns.Add('2');
      LResult := False;
    end
    else
      LReservoirStartMonth := FReservoirStartMonth;
    if (LResult) and (LReservoirStartMonth <> 0) and (LReservoirStartYear <> 0)then
      LPreReplacementStartDate := Encodedate(LReservoirStartYear,LReservoirStartMonth,LDay);

    if FReservoirEndYear = 0 then
    begin
      LMessage := 'Invalid Base Reservoir End Year.';
      AErrorMessages.Add('ERROR:'+LMessage);
      AErrorColumns.Add('3');
      LResult := False;
    end
    else
      LReservoirEndYear := FReservoirEndYear;
    if FReservoirEndMonth = 0 then
    begin
      LMessage := 'Invalid Base Reservoir End Month.';
      AErrorMessages.Add('ERROR:'+LMessage);
      AErrorColumns.Add('4');
      LResult := False;
    end
    else
      LReservoirEndMonth := FReservoirEndMonth;

    for LIndex := 0 to FReplacements.Count-1 do
    begin
      LReplacement := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirByIdentifier[StrToInt(FReplacements.Strings[LIndex])];
      if (LReplacement <> nil) then
      begin
        if (LReplacement.TimeControl <> nil) then
        begin

          if LReplacement.TimeControl.StartYear = 0 then
          begin
            LMessage := 'Invalid Replacement Reservoir Start Year.';
            AErrorMessages.Add('ERROR:'+LMessage);
            AErrorColumns.Add(IntToStr(LIndex+1));
            LResult := False;
          end
          else
            LReservoirStartYear := LReplacement.TimeControl.StartYear;
          if LReplacement.TimeControl.StartMonth = 0 then
          begin
            LMessage := 'Invalid Replacement Reservoir Start Month.';
            AErrorMessages.Add('ERROR:'+LMessage);
            AErrorColumns.Add(IntToStr(LIndex+2));
            LResult := False;
          end
          else
            LReservoirStartMonth := LReplacement.TimeControl.StartMonth;
          if LReplacement.TimeControl.EndYear = 0 then
          begin
            LMessage := 'Invalid Replacement Reservoir End Year.';
            AErrorMessages.Add('ERROR:'+LMessage);
            AErrorColumns.Add(IntToStr(LIndex+3));
            LResult := False;
          end
          else
            LReservoirEndYear := LReplacement.TimeControl.EndYear;
          if LReplacement.TimeControl.EndMonth = 0 then
          begin
            LMessage := 'Invalid Replacement Reservoir End Month.';
            AErrorMessages.Add('ERROR:'+LMessage);
            AErrorColumns.Add(IntToStr(LIndex+4));
            LResult := False;
          end
          else
            LReservoirEndMonth := LReplacement.TimeControl.EndMonth;
          if LResult then
          begin
            LCurReplacementStartDate := Encodedate(LReservoirStartYear,LReservoirStartMonth,LDay);
            LCurReplacementEndDate  := Encodedate(LReservoirEndYear,LReservoirEndMonth,LDay);
            if LCurReplacementEndDate > LPreReplacementStartDate then
            begin
              LMessage := 'Time Overlap';
              AErrorMessages.Add('ERROR:'+LMessage);
              AErrorColumns.Add(IntToStr(LIndex+5));
              LResult := False;
              LPreReplacementStartDate := LCurReplacementStartDate;
            end;

          end;

        end;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirTimeControl.Assign(ASource: TReservoirTimeControl);
const OPNAME = 'TReservoirTimeControl.Assign';
begin
  try
    if (ASource <> nil) then
    begin
      StartYear        := ASource.StartYear;
      StartMonth       := ASource.StartMonth;
      EndYear          := ASource.EndYear;
      EndMonth         := ASource.EndMonth;
      EconomicLife     := ASource.EconomicLife;
      CapitalCost      := ASource.CapitalCost;
      OMCost           := ASource.OMCost;
      YearsToConstruct := ASource.YearsToConstruct;
      CostSchedule     := ASource.CostSchedule;
      CostSchedule     := ASource.CostSchedule;
      BaseNodeNumber   := ASource.BaseNodeNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
