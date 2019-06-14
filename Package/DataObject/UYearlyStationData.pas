{******************************************************************************}
{*  UNIT      : Contains TStationData and TYearlyData classes                 *}
{*  AUTHOR    : RH Steyn                                                      *}
{*  DATE      : 2003/09/18                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYearlyStationData;

interface

uses

  Classes,
  SysUtils,
  VCL.Controls,
  Contnrs,
  UAbstractObject,
  RainfallCom_TLB;

type
  TMonthlyRainfall         = array[1..12] of double;
  TMonthlyPatchSign        = array[1..12] of string;
  TMonthlyScaledDown       = array[1..12] of double;

  TYearlyData = class(TAbstractAppObject, IYearlyData)
  protected
    FYear                    : integer;
    FHydroYear               : string;
    FTotal                   : double;
    FMissingMonths           : integer;
    FUnreliableMonths        : integer;
    FMonthlyRainfall         : TMonthlyRainfall;
    FMonthlyPatchSign        : TMonthlyPatchSign;
    FMonthlyScaledDown       : TMonthlyScaledDown;
    function Get_Year: Integer; safecall;
    procedure Set_Year(Value: Integer); safecall;
    function Get_HydroYear: WideString; safecall;
    procedure Set_HydroYear(const Value: WideString); safecall;
    function Get_Total: Double; safecall;
    procedure Set_Total(Value: Double); safecall;
    function Get_MonthlyRainfall(AMonth: Integer): Double; safecall;
    procedure Set_MonthlyRainfall(AMonth: Integer; Value: Double); safecall;
    function Get_MonthlyPatchSign(AMonth: Integer): WideString; safecall;
    procedure Set_MonthlyPatchSign(AMonth: Integer; const Value: WideString); safecall;
    function Get_MissingMonths: Integer; safecall;
    procedure Set_MissingMonths(Value: Integer); safecall;
    function Get_HasMissingData: WordBool; safecall;
    function Get_UnreliableMonths: Integer; safecall;
    procedure Set_UnreliableMonths(Value: Integer); safecall;
    function Get_HasUnreliableData: WordBool; safecall;
    function Get_MonthlyScaledDown(AMonth: Integer): Double; safecall;
    procedure Set_MonthlyScaledDown(AMonth: Integer; Value: Double); safecall;


    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: boolean; override;

    property Year          : integer    read Get_Year          write Set_Year;
    property HydroYear     : WideString read Get_HydroYear     write Set_HydroYear;
    property Total         : double     read Get_Total         write Set_Total;
    property MissingMonths    : integer read Get_MissingMonths    write Set_MissingMonths;
    property UnreliableMonths : Integer read Get_UnreliableMonths write Set_UnreliableMonths;
    property HasMissingData    : WordBool   read Get_HasMissingData;
    property HasUnreliableData : WordBool   read Get_HasUnreliableData;
    property MonthlyRainfall[AMonth: Integer]: Double read Get_MonthlyRainfall write Set_MonthlyRainfall;
    property MonthlyPatchSign[AMonth: Integer]: WideString read Get_MonthlyPatchSign write Set_MonthlyPatchSign;
    property MonthlyScaledDown[AMonth: Integer]: Double read Get_MonthlyScaledDown write Set_MonthlyScaledDown;
  end;

  TPatchData = class;

  TRainfallDataSplit = class(TAbstractAppObject, IRainfallDataSplit)
  protected
    FHydroStartYear    : integer;
    FHydroEndYear      : integer;
    FNrOfMissingMonths : integer;
    FMonthMax          : array [1..12] of double;
    FMonthMin          : array [1..12] of double;
    FMonthCount        : array [1..12] of integer;
    FMonthTotal        : array [1..12] of double;
    FMonthMAP          : array [1..12] of double;
    FMonthStdDev       : array [1..12] of double;
    FMonthCV           : array [1..12] of double;
    FYearMin           : double;
    FYearMax           : double;
    FYearCount         : integer;
    FGrandTotal        : double;
    FMAP               : double;
    FStdDeviation      : double;
    FCV                : double;
    FXMonthMax         : array [1..12] of double;
    FXMonthMin         : array [1..12] of double;
    FXMonthCount       : array [1..12] of integer;
    FXMonthTotal       : array [1..12] of double;
    FXMonthMAP         : array [1..12] of double;
    FXMonthStdDev      : array [1..12] of double;
    FXMonthCV          : array [1..12] of double;
    FXYearMin          : double;
    FXYearMax          : double;
    FXYearCount        : integer;
    FXGrandTotal       : double;
    FXMAP              : double;
    FXStdDeviation     : double;
    FXCV               : double;
    FHighlightMonths   : TStringList;
    FHighlightYears    : TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_HydroStartYear: Integer; safecall;
    procedure Set_HydroStartYear(Value: Integer); safecall;
    function Get_HydroEndYear: Integer; safecall;
    procedure Set_HydroEndYear(Value: Integer); safecall;
    function Get_NrOfMissingMonths: Integer; safecall;
    procedure Set_NrOfMissingMonths(Value: Integer); safecall;
    function Get_GrandTotal: Double; safecall;
    procedure Set_GrandTotal(Value: Double); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_StdDeviation: Double; safecall;
    procedure Set_StdDeviation(Value: Double); safecall;
    function Get_CV: Double; safecall;
    procedure Set_CV(Value: Double); safecall;
    function Get_XGrandTotal: Double; safecall;
    procedure Set_XGrandTotal(Value: Double); safecall;
    function Get_XMAP: Double; safecall;
    procedure Set_XMAP(Value: Double); safecall;
    function Get_XStdDeviation: Double; safecall;
    procedure Set_XStdDeviation(Value: Double); safecall;
    function Get_XCV: Double; safecall;
    procedure Set_XCV(Value: Double); safecall;
    function Get_YearCount: Integer; safecall;
    procedure Set_YearCount(Value: Integer); safecall;
    function Get_YearMin: Double; safecall;
    procedure Set_YearMin(Value: Double); safecall;
    function Get_YearMax: Double; safecall;
    procedure Set_YearMax(Value: Double); safecall;
    function Get_XYearCount: Integer; safecall;
    procedure Set_XYearCount(Value: Integer); safecall;
    function Get_XYearMin: Double; safecall;
    procedure Set_XYearMin(Value: Double); safecall;
    function Get_XYearMax: Double; safecall;
    procedure Set_XYearMax(Value: Double); safecall;
    function Get_MonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_MonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_MonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_MonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMin(AMonth: Integer): Double; safecall;
    procedure Set_MonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMax(AMonth: Integer): Double; safecall;
    procedure Set_MonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_MonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_MonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_MonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_MonthCV(AMonth: Integer): Double; safecall;
    procedure Set_MonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_XMonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_XMonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_XMonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMin(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMax(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_XMonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCV(AMonth: Integer): Double; safecall;
    procedure Set_XMonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_HighlightMonths: WideString; safecall;
    function Get_HighlightYears: WideString; safecall;
    function ResetStats : Boolean;
  public
    function Initialise : boolean; override;
    property HydroStartYear: Integer                  read Get_HydroStartYear    write Set_HydroStartYear;
    property HydroEndYear: Integer                    read Get_HydroEndYear      write Set_HydroEndYear;
    property NrOfMissingMonths: Integer               read Get_NrOfMissingMonths write Set_NrOfMissingMonths;
    property GrandTotal: Double                       read Get_GrandTotal        write Set_GrandTotal;
    property MAP: Double                              read Get_MAP               write Set_MAP;
    property StdDeviation: Double                     read Get_StdDeviation      write Set_StdDeviation;
    property CV: Double                               read Get_CV                write Set_CV;
    property XGrandTotal: Double                      read Get_XGrandTotal       write Set_XGrandTotal;
    property XMAP: Double                             read Get_XMAP              write Set_XMAP;
    property XStdDeviation: Double                    read Get_XStdDeviation     write Set_XStdDeviation;
    property XCV: Double                              read Get_XCV               write Set_XCV;

    property YearCount   : Integer                    read Get_YearCount         write Set_YearCount;
    property YearMin     : Double                     read Get_YearMin           write Set_YearMin;
    property YearMax     : Double                     read Get_YearMax           write Set_YearMax;
    property XYearCount  : Integer                    read Get_XYearCount        write Set_XYearCount;
    property XYearMin    : Double                     read Get_XYearMin          write Set_XYearMin;
    property XYearMax    : Double                     read Get_XYearMax          write Set_XYearMax;
    property MonthCount[AMonth: Integer]   : Integer  read Get_MonthCount        write Set_MonthCount;
    property MonthTotal[AMonth: Integer]   : Double   read Get_MonthTotal        write Set_MonthTotal;
    property MonthMin[AMonth: Integer]     : Double   read Get_MonthMin          write Set_MonthMin;
    property MonthMax[AMonth: Integer]     : Double   read Get_MonthMax          write Set_MonthMax;
    property MonthMAP[AMonth: Integer]     : Double   read Get_MonthMAP          write Set_MonthMAP;
    property MonthStdDev[AMonth: Integer]  : Double   read Get_MonthStdDev       write Set_MonthStdDev;
    property MonthCV[AMonth: Integer]      : Double   read Get_MonthCV           write Set_MonthCV;
    property XMonthCount[AMonth: Integer]  : Integer  read Get_XMonthCount       write Set_XMonthCount;
    property XMonthTotal[AMonth: Integer]  : Double   read Get_XMonthTotal       write Set_XMonthTotal;
    property XMonthMin[AMonth: Integer]    : Double   read Get_XMonthMin         write Set_XMonthMin;
    property XMonthMax[AMonth: Integer]    : Double   read Get_XMonthMax         write Set_XMonthMax;
    property XMonthMAP[AMonth: Integer]    : Double   read Get_XMonthMAP         write Set_XMonthMAP;
    property XMonthStdDev[AMonth: Integer] : Double   read Get_XMonthStdDev      write Set_XMonthStdDev;
    property XMonthCV[AMonth: Integer]     : Double   read Get_XMonthCV          write Set_XMonthCV;
    property HighlightMonths               : WideString read Get_HighlightMonths;
    property HighlightYears                : WideString read Get_HighlightYears;
  end;

  TRainfallData = class(TAbstractAppObject, IRainfallData)
  protected
    FStationID         : integer;
    FStationNumber     : string;
    FStartYear         : integer;
    FEndYear           : integer;
    FYearlyData        : TObjectList;
    FHydroYearlyData   : TObjectList;
    FHydroStartYear    : integer;
    FHydroEndYear      : integer;
    FNrOfMissingMonths : integer;
    FMonthMax          : array [1..12] of double;
    FMonthMin          : array [1..12] of double;
    FMonthCount        : array [1..12] of integer;
    FMonthTotal        : array [1..12] of double;
    FMonthMAP          : array [1..12] of double;
    FMonthStdDev       : array [1..12] of double;
    FMonthCV           : array [1..12] of double;
    FYearMin           : double;
    FYearMax           : double;
    FYearCount         : integer;
    FGrandTotal        : double;
    FMAP               : double;
    FStdDeviation      : double;
    FCV                : double;
    FXMonthMax         : array [1..12] of double;
    FXMonthMin         : array [1..12] of double;
    FXMonthCount       : array [1..12] of integer;
    FXMonthTotal       : array [1..12] of double;
    FXMonthMAP         : array [1..12] of double;
    FXMonthStdDev      : array [1..12] of double;
    FXMonthCV          : array [1..12] of double;
    FXYearMin          : double;
    FXYearMax          : double;
    FXYearCount        : integer;
    FXGrandTotal       : double;
    FXMAP              : double;
    FXStdDeviation     : double;
    FXCV               : double;
    FHighlightMonths   : TStringList;
    FHighlightYears    : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure PopulateHighlights;
    procedure PopulateRepeats;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_HydroYearlyData: TObjectList;
    function Get_YearlyData: TObjectList;
    function Get_StationID: Integer; safecall;
    procedure Set_StationID(Value: Integer); safecall;
    function Get_StationNumber: WideString; safecall;
    procedure Set_StationNumber(const Value: WideString); safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(Value: Integer); safecall;
    function Get_HydroStartYear: Integer; safecall;
    procedure Set_HydroStartYear(Value: Integer); safecall;
    function Get_HydroEndYear: Integer; safecall;
    procedure Set_HydroEndYear(Value: Integer); safecall;
    function Get_GrandTotal: Double; safecall;
    procedure Set_GrandTotal(Value: Double); safecall;
    function Get_StdDeviation: Double; safecall;
    procedure Set_StdDeviation(Value: Double); safecall;
    function Get_CV: Double; safecall;
    procedure Set_CV(Value: Double); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_NrOfMissingMonths: Integer; safecall;
    procedure Set_NrOfMissingMonths(Value: Integer); safecall;

    function Get_XGrandTotal: Double; safecall;
    procedure Set_XGrandTotal(Value: Double); safecall;
    function Get_XMAP: Double; safecall;
    procedure Set_XMAP(Value: Double); safecall;
    function Get_XStdDeviation: Double; safecall;
    procedure Set_XStdDeviation(Value: Double); safecall;
    function Get_XCV: Double; safecall;
    procedure Set_XCV(Value: Double); safecall;

    function Get_YearCount: Integer; safecall;
    procedure Set_YearCount(Value: Integer); safecall;
    function Get_YearMin: Double; safecall;
    procedure Set_YearMin(Value: Double); safecall;
    function Get_YearMax: Double; safecall;
    procedure Set_YearMax(Value: Double); safecall;
    function Get_XYearCount: Integer; safecall;
    procedure Set_XYearCount(Value: Integer); safecall;
    function Get_XYearMin: Double; safecall;
    procedure Set_XYearMin(Value: Double); safecall;
    function Get_XYearMax: Double; safecall;
    procedure Set_XYearMax(Value: Double); safecall;
    function Get_MonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_MonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_MonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_MonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMin(AMonth: Integer): Double; safecall;
    procedure Set_MonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMax(AMonth: Integer): Double; safecall;
    procedure Set_MonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_MonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_MonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_MonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_MonthCV(AMonth: Integer): Double; safecall;
    procedure Set_MonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_XMonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_XMonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_XMonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMin(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMax(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_XMonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCV(AMonth: Integer): Double; safecall;
    procedure Set_XMonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_HighlightMonths: WideString; safecall;
    function Get_HighlightYears: WideString; safecall;
    function ResetStats : Boolean;
  public
    function Initialise : boolean; override;
    procedure CalculateStats;
    function HydroYearsCount: Integer; safecall;
    function HydroStartDate: TDateTime; safecall;
    function HydroEndDate: TDateTime; safecall;
    function AddYearlyData(AYearlyData : TYearlyData) : boolean;
    function CastHydroYearDataByYear(AYear: Integer): TYearlyData;
    function GetHydroYearDataByIndex(AIndex: Integer): IYearlyData; safecall;
    function GetHydroYearDataByYear(AYear: Integer): IYearlyData; safecall;
    function CastHydroYearDataByIndex(AIndex: Integer): TYearlyData;
    procedure GetBaseDataForYearAndMonth (AYear          : integer;
                                          AMonth         : integer;
                                          var ARainfall  : double;
                                          var APatchSign : WideString); safecall;
    function StreamOut (const ATitle : WideString) : WideString;

    property StationID         : Integer    read Get_StationID         write Set_StationID;
    property StationNumber     : WideString read Get_StationNumber     write Set_StationNumber;
    property StartYear         : Integer    read Get_StartYear         write Set_StartYear;
    property EndYear           : Integer    read Get_EndYear           write Set_EndYear;
    property HydroStartYear    : Integer    read Get_HydroStartYear    write Set_HydroStartYear;
    property HydroEndYear      : Integer    read Get_HydroEndYear      write Set_HydroEndYear;
    property GrandTotal        : Double     read Get_GrandTotal        write Set_GrandTotal;
    property StdDeviation      : Double     read Get_StdDeviation      write Set_StdDeviation;
    property CV                : Double     read Get_CV                write Set_CV;
    property MAP               : Double     read Get_MAP               write Set_MAP;
    property NrOfMissingMonths : Integer    read Get_NrOfMissingMonths write Set_NrOfMissingMonths;
    property XGrandTotal       : Double     read Get_XGrandTotal    write Set_XGrandTotal;
    property XMAP              : Double     read Get_XMAP           write Set_XMAP;
    property XStdDeviation     : Double     read Get_XStdDeviation  write Set_XStdDeviation;
    property XCV               : Double     read Get_XCV            write Set_XCV;

    property YearCount   : Integer                    read Get_YearCount         write Set_YearCount;
    property YearMin     : Double                     read Get_YearMin           write Set_YearMin;
    property YearMax     : Double                     read Get_YearMax           write Set_YearMax;
    property XYearCount  : Integer                    read Get_XYearCount        write Set_XYearCount;
    property XYearMin    : Double                     read Get_XYearMin          write Set_XYearMin;
    property XYearMax    : Double                     read Get_XYearMax          write Set_XYearMax;
    property MonthCount[AMonth: Integer]   : Integer  read Get_MonthCount        write Set_MonthCount;
    property MonthTotal[AMonth: Integer]   : Double   read Get_MonthTotal        write Set_MonthTotal;
    property MonthMin[AMonth: Integer]     : Double   read Get_MonthMin          write Set_MonthMin;
    property MonthMax[AMonth: Integer]     : Double   read Get_MonthMax          write Set_MonthMax;
    property MonthMAP[AMonth: Integer]     : Double   read Get_MonthMAP          write Set_MonthMAP;
    property MonthStdDev[AMonth: Integer]  : Double   read Get_MonthStdDev       write Set_MonthStdDev;
    property MonthCV[AMonth: Integer]      : Double   read Get_MonthCV           write Set_MonthCV;
    property XMonthCount[AMonth: Integer]  : Integer  read Get_XMonthCount       write Set_XMonthCount;
    property XMonthTotal[AMonth: Integer]  : Double   read Get_XMonthTotal       write Set_XMonthTotal;
    property XMonthMin[AMonth: Integer]    : Double   read Get_XMonthMin         write Set_XMonthMin;
    property XMonthMax[AMonth: Integer]    : Double   read Get_XMonthMax         write Set_XMonthMax;
    property XMonthMAP[AMonth: Integer]    : Double   read Get_XMonthMAP         write Set_XMonthMAP;
    property XMonthStdDev[AMonth: Integer] : Double   read Get_XMonthStdDev      write Set_XMonthStdDev;
    property XMonthCV[AMonth: Integer]     : Double   read Get_XMonthCV          write Set_XMonthCV;
    property HighlightMonths               : WideString read Get_HighlightMonths;
    property HighlightYears                : WideString read Get_HighlightYears;
    property YearlyData        : TObjectList read Get_YearlyData;
    property HydroYearlyData   : TObjectList read Get_HydroYearlyData;
  end;

  TStationData = class(TAbstractAppObject, IStationData)
  protected
    FRainfallData      : TRainfallData;
    FDataSplits        : TObjectList;
    FStationName       : string;
    FLatitude          : integer;
    FLongitude         : integer;
    FHeight            : integer;
    FStationType       : string;
    FWR90              : Boolean;
    FPatches           : TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateHydrologicalYears;
    procedure LoadRAWSplits;
    function GetMPData (AStartYear : integer;
                        AEndYear   : integer;
                        AMPData    : TStringList): boolean;
    function GetRawDataForCalendarYear (AYear           : integer;
                                        var ARainfall   : TMonthlyRainfall;
                                        var APatchSigns : TMonthlyPatchSign) : boolean;
    function Get_StationName: WideString; safecall;
    procedure Set_StationName(const Value: WideString); safecall;
    function Get_Latitude: Integer; safecall;
    procedure Set_Latitude(Value: Integer); safecall;
    function Get_Longitude: Integer; safecall;
    procedure Set_Longitude(Value: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(Value: Integer); safecall;
    function Get_StationType: WideString; safecall;
    procedure Set_StationType(const Value: WideString); safecall;
    function Get_IsInWR90: WordBool; safecall;
    procedure Set_IsInWR90(Value: WordBool); safecall;
    function Get_RainfallData: IRainfallData; safecall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CalculateSplitStats (ASplit : TRainfallDataSplit);
    procedure PopulateHighlights (ASplit : TRainfallDataSplit);
    procedure PopulateRepeats (ASplit : TRainfallDataSplit);
  public
    property Patches           : TObjectList read FPatches;
    function Initialise : boolean; override;
    procedure LoadMonthlyWRCData;
    procedure LoadMonthlyPatchData;
    procedure LoadMonthlyRAWData;
    function CastPatchWithID (APatchID: Integer) : TPatchData;
    function CastPatchWithIndex (AIndex : integer) : TPatchData;
    function CastPatchWithName (APatchName : string) : TPatchData;
    function CastSplitWithIndex (AIndex : Integer): TRainfallDataSplit;
    function CastSplitForYears (AStartYear : Integer;
                                AEndYear   : Integer): TRainfallDataSplit;
    function CreateAndPopulateSplit (AStartYear : integer;
                                     AEndYear   : integer) : boolean;
    function DeleteSplitForYears (AStartYear : Integer;
                                  AEndYear   : Integer) : boolean;
    function UpdateSplitForYears (AOldStartYear,ANewStartYear : Integer;
                                           AOldEndYear,ANewEndYear   : Integer) : boolean;

    function GetKeyValues (const AParamField : WideString;
                           const AFieldIndex : WideString) : WideString; safecall;
    function GetPatchWithID(APatchID: Integer) : IPatchData; safecall;
    function GetPatchWithIndex(AIndex: Integer): IPatchData; safecall;
    function DeletePatchWithID(APatchID: Integer): WordBool; safecall;
    function PatchCount: Integer; safecall;
    function GetSplitWithIndex(AIndex: Integer): IRainfallDataSplit; safecall;
    function GetSplitForYears(AStartYear: Integer; AEndYear: Integer): IRainfallDataSplit; safecall;
    function SplitCount: Integer; safecall;

    procedure SaveRAWFile (AStartYear       : integer;
                           AEndYear         : integer;
                           const ADirectory : WideString); safecall;
    procedure SaveMPFile (AStartYear       : integer;
                          AEndYear         : integer;
                          const ADirectory : WideString); safecall;
    procedure LoadMonthlyData; safecall;
    procedure RecalculateStatistics; safecall;
    procedure RepopulateHighlights; safecall;
    procedure LatLong (var ALat  : WideString;
                       var ALong : WideString); safecall;
    function IsPartOfZone: WordBool; safecall;
    function HasACreatedPatch: WordBool; safecall;
    function IsAPatchSource: WordBool; safecall;
    function MayBeDeleted: WordBool; safecall;
    function MayDeleteSplit (AStartYear : Integer;
                             AEndYear   : Integer): WordBool; safecall;

    function GetRAWData (AStartYear : integer;
                         AEndYear   : integer;
                         ARawData   : TStringList): boolean;
    function PopulateCommonBlockRAWData (AStartYear  : integer;
                                         AEndYear    : integer;
                                         AGaugeIndex : Integer;
                                         AAddIndex   : Integer;
                                         var ABEGY   : Integer;
                                         var AENDY   : Integer): WordBool; safecall;
    function RainfallData : IRainfallData; safecall;
    property CastRainfallData : TRainfallData read FRainfallData;
    property StationName : WideString read Get_StationName write Set_StationName;
    property Latitude    : Integer    read Get_Latitude    write Set_Latitude;
    property Longitude   : Integer    read Get_Longitude   write Set_Longitude;
    property Height      : Integer    read Get_Height      write Set_Height;
    property StationType : WideString read Get_StationType write Set_StationType;
    property IsInWR90    : WordBool   read Get_IsInWR90    write Set_IsInWR90;
  end;

  TPatchData = class(TAbstractAppObject, IPatchData)
  protected
    FRainfallData          : TRainfallData;
    FPatchID               : integer;
    FPatchTypeID           : integer;
    FPatchName             : string;
    FSources               : TStringList;
    FPatchMultiple         : boolean;
    FPatchStartYear        : integer;
    FPatchEndYear          : integer;
    FClassRInputFileName   : string;
    FClassROutputFileName  : string;
    FPatchRInputFileName   : string;
    FPatchRPrintFileName   : string;
    FPatchRPlotFileName    : string;
    FChangeDate            : TDateTime;
    FClassRDate            : TDateTime;
    FPatchRInputDate       : TDateTime;
    FPatchROutputDate      : TDateTime;
    FPatchRPrintDate       : TDateTime;
    FPatchRPlotDate        : TDateTime;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
{
    procedure GetCompressData (var AData  : WideString;
                               AFieldName : string);
    procedure SetCompressData (AData      : WideString;
                               AFieldName : string);
}
    procedure GetCompressData (var AData  : String;
                               AFieldName : string);
    procedure SetCompressData (AData      : String;
                               AFieldName : string);
    procedure SaveMonthlyPatchData (AStationID : integer;
                                    AValue     : TStringlist);
    procedure CreateHydrologicalYears;

    function GetMPData  (AStartYear : integer;
                         AEndYear   : integer;
                         AMPData  : TStringList): boolean;
    function GetPATData (AStartYear : integer;
                         AEndYear   : integer;
                         APATData   : TStringList): boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_PatchID: Integer; safecall;
    procedure Set_PatchID(Value: Integer); safecall;
    function Get_PatchTypeID: Integer; safecall;
    procedure Set_PatchTypeID(Value: Integer); safecall;
    function Get_PatchName: WideString; safecall;
    procedure Set_PatchName(const Value: WideString); safecall;
    function Get_PatchMultiple: WordBool; safecall;
    procedure Set_PatchMultiple(Value: WordBool); safecall;
    function Get_ClassRInputFileName: WideString; safecall;
    procedure Set_ClassRInputFileName(const Value: WideString); safecall;
    function Get_ClassROutputFileName: WideString; safecall;
    procedure Set_ClassROutputFileName(const Value: WideString); safecall;
    function Get_PatchRInputFileName: WideString; safecall;
    procedure Set_PatchRInputFileName(const Value: WideString); safecall;
    function Get_PatchRPrintFileName: WideString; safecall;
    procedure Set_PatchRPrintFileName(const Value: WideString); safecall;
    function Get_PatchRPlotFileName: WideString; safecall;
    procedure Set_PatchRPlotFileName(const Value: WideString); safecall;
    function Get_PatchStartYear: Integer; safecall;
    procedure Set_PatchStartYear(Value: Integer); safecall;
    function Get_PatchEndYear: Integer; safecall;
    procedure Set_PatchEndYear(Value: Integer); safecall;
    function Get_PatchRInputData : WideString; safecall;
    procedure Set_PatchRInputData (const Value : WideString); safecall;
    function Get_ClassRInputData: WideString; safecall;
    procedure Set_ClassRInputData(const Value: WideString); safecall;
    function Get_ClassROutputData: WideString; safecall;
    procedure Set_ClassROutputData(const Value: WideString); safecall;
    function Get_PatchROutputData : WideString; safecall;
    function Get_PatchRPrintData: WideString; safecall;
    procedure Set_PatchRPrintData(const Value: WideString); safecall;
    function Get_PatchRPlotData: WideString; safecall;
    procedure Set_PatchRPlotData(const Value: WideString); safecall;
    function Get_ChangeDate: TDateTime; safecall;
    procedure Set_ChangeDate(Value: TDateTime); safecall;
    function Get_ClassRDate: TDateTime; safecall;
    procedure Set_ClassRDate(Value: TDateTime); safecall;
    function Get_PatchRInputDate: TDateTime; safecall;
    procedure Set_PatchRInputDate(Value: TDateTime); safecall;
    function Get_PatchROutputDate: TDateTime; safecall;
    procedure Set_PatchROutputDate(Value: TDateTime); safecall;
    function Get_PatchRPrintDate: TDateTime; safecall;
    procedure Set_PatchRPrintDate(Value: TDateTime); safecall;
    function Get_PatchRPlotDate: TDateTime; safecall;
    procedure Set_PatchRPlotDate(Value: TDateTime); safecall;
    function Get_SourceInfo: WideString; safecall;
    procedure Set_SourceInfo(const Value: WideString); safecall;
    function Get_RainfallData: IRainfallData; safecall;

    function Get_PatchScaledDownStatus(AStationID : integer;
                                       APatchID   : integer;
                                       AYear      : integer;
                                       AMonth     : integer): WordBool;safecall;
    function Clear_PatchScaledDownStatus(AStationID, APatchID: integer): WordBool;
    function Set_PatchScaledDownStatus(AStationID : integer;
                                       APatchID   : integer;
                                       AYear      : integer;
                                       AMonth     : integer;
                                       AStatus    : WordBool): WordBool;safecall;
    function Save_PatchScaledDownStatus(AStationID : integer;
                                       APatchID   : integer;
                                       AYear      : integer;
                                       AMonth     : integer;
                                       AIdentifier: integer;
                                       AStatus    : WordBool): WordBool;safecall;

  public
    function Initialise : boolean; override;
    procedure AddSource (AStationID : integer;
                         APatchID   : integer;
                         ATarget    : boolean;
                         AStartYear : integer;
                         AEndYear   : integer);
    procedure RemoveSource (AStationID : integer);
    procedure Populate (APatchMultiple         : boolean;
                        AClassRInputFileName   : string;
                        AClassROutputFileName  : string;
                        APatchRInputFileName   : string;
                        APatchRPrintFileName   : string;
                        APatchRPlotFileName    : string;
                        APatchStartYear        : integer;
                        APatchEndYear          : integer;
                        AChangedDate           : TDateTime;
                        AClassRDate            : TDateTime;
                        APatchRInputDate       : TDateTime;
                        APatchROutputDate      : TDateTime;
                        APatchRPrintDate       : TDateTime;
                        APatchRPlotDate        : TDateTime);

    function SourcesCount: Integer; safecall;
    procedure DeleteMonthlyPatchData; safecall;
    procedure SavePatchROutputData (AStationID   : Integer;
                                    const AValue : WideString); safecall;
    procedure GetSourceInfoByIndex (AIndex         : integer;
                                    out AStationID : integer;
                                    out APatchID   : integer;
                                    out ATarget    : WideString;
                                    out AStartYear : integer;
                                    out AEndYear   : integer); safecall;
    procedure GetSourceInfoByStationID (AStationID     : integer;
                                        out APatchID   : integer;
                                        out ATarget    : WideString;
                                        out AStartYear : integer;
                                        out AEndYear   : integer); safecall;
    procedure SaveRAWFile (AStartYear       : integer;
                           AEndYear         : integer;
                           const ADirectory : WideString); safecall;
    procedure SaveMPFile (AStartYear       : integer;
                          AEndYear         : integer;
                          const ADirectory : WideString); safecall;
    procedure SavePATFile (AStartYear       : integer;
                           AEndYear         : integer;
                           const ADirectory : WideString); safecall;
    function GetKeyValues (const AParamField : WideString;
                           const AFieldIndex : WideString) : WideString; safecall;
    function GetRAWData (AStartYear : integer;
                         AEndYear   : integer;
                         ARawData   : TStringList): boolean;

    function PopulateCommonBlockRAWData (AStartYear  : integer;
                                         AEndYear    : integer;
                                         AGaugeIndex : Integer;
                                         AAddIndex   : Integer;
                                         var ABEGY   : Integer;
                                         var AENDY   : Integer): WordBool; safecall;
    function RainfallData : IRainfallData; safecall;
    property CastRainfallData     : TRainfallData read FRainfallData;
    property PatchID              : Integer    read Get_PatchID              write Set_PatchID;
    property PatchTypeID          : Integer    read Get_PatchTypeID          write Set_PatchTypeID;
    property PatchName            : WideString read Get_PatchName            write Set_PatchName;
    property PatchMultiple        : WordBool   read Get_PatchMultiple        write Set_PatchMultiple;
    property ClassRInputFileName  : WideString read Get_ClassRInputFileName  write Set_ClassRInputFileName;
    property ClassROutputFileName : WideString read Get_ClassROutputFileName write Set_ClassROutputFileName;
    property PatchRInputFileName  : WideString read Get_PatchRInputFileName  write Set_PatchRInputFileName;
    property PatchRPrintFileName  : WideString read Get_PatchRPrintFileName  write Set_PatchRPrintFileName;
    property PatchRPlotFileName   : WideString read Get_PatchRPlotFileName   write Set_PatchRPlotFileName;
    property PatchStartYear       : Integer    read Get_PatchStartYear       write Set_PatchStartYear;
    property PatchEndYear         : Integer    read Get_PatchEndYear         write Set_PatchEndYear;
    property ClassRInputData      : WideString read Get_ClassRInputData      write Set_ClassRInputData;
    property ClassROutputData     : WideString read Get_ClassROutputData     write Set_ClassROutputData;
    property PatchRInputData      : WideString read Get_PatchRInputData      write Set_PatchRInputData;
    property PatchROutputData     : WideString read Get_PatchROutputData;
    property PatchRPrintData      : WideString read Get_PatchRPrintData      write Set_PatchRPrintData;
    property PatchRPlotData       : WideString read Get_PatchRPlotData       write Set_PatchRPlotData;
    property ChangeDate           : TDateTime  read Get_ChangeDate           write Set_ChangeDate;
    property ClassRDate           : TDateTime  read Get_ClassRDate           write Set_ClassRDate;
    property PatchRInputDate      : TDateTime  read Get_PatchRInputDate      write Set_PatchRInputDate;
    property PatchROutputDate     : TDateTime  read Get_PatchROutputDate     write Set_PatchROutputDate;
    property PatchRPrintDate      : TDateTime  read Get_PatchRPrintDate      write Set_PatchRPrintDate;
    property PatchRPlotDate       : TDateTime  read Get_PatchRPlotDate       write Set_PatchRPlotDate;
    property SourceInfo           : WideString read Get_SourceInfo           write Set_SourceInfo;

  end;

implementation

uses
  DB,
  Math,
  DateUtils,
  System.UITypes,
  VCL.Dialogs,
  UConstants,
  UDataSetType,
  UDWADBComponents,
  //ZLib,
  //ZLibEx,
  System.ZLib,
  URainfallCommonBlocks,
  UErrorHandlingOperations;

{******************************************************************************}
{* TYearlyData                                                                *}
{******************************************************************************}

function TYearlyData._AddRef: Integer;
const OPNAME = 'TYearlyData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYearlyData._Release: Integer;
const OPNAME = 'TYearlyData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYearlyData.Initialise : boolean;
const OPNAME = 'TYearlyData.Initialise';
var
  lIndex : integer;
begin
  Result := inherited Initialise;
  try
    FYear  := 0;
    FTotal := 0;
    FMissingMonths := 0;
    FHydroYear := '';
    for lIndex := 1 to 12 do
    begin
      FMonthlyRainfall[lIndex]   := NullFloat;
      FMonthlyScaledDown[lIndex] := NullFloat;
      FMonthlyPatchSign[lIndex]  := '[';
    end;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_Year: Integer;
const OPNAME = 'TYearlyData.Get_Year';
begin
  Result := 0;
  try
    Result := FYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_Year(Value: Integer);
const OPNAME = 'TYearlyData.Set_Year';
begin
  try
    FYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_HydroYear: WideString;
const OPNAME = 'TYearlyData.Get_HydroYear';
begin
  Result := '';
  try
    Result := FHydroYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_HydroYear(const Value: WideString);
const OPNAME = 'TYearlyData.Set_HydroYear';
begin
  try
    FHydroYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_Total: Double;
const OPNAME = 'TYearlyData.Get_Total';
begin
  Result := 0;
  try
    Result := FTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_Total(Value: Double);
const OPNAME = 'TYearlyData.Set_Total';
begin
  try
    FTotal := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_MissingMonths: integer;
const OPNAME = 'TYearlyData.Get_MissingMonths';
begin
  Result := 0;
  try
    Result := FMissingMonths;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_MissingMonths(Value: integer);
const OPNAME = 'TYearlyData.Set_MissingMonths';
begin
  try
    FMissingMonths := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_MonthlyRainfall(AMonth: Integer): Double;
const OPNAME = 'TYearlyData.Get_MonthlyRainfall';
begin
  Result := 0;
  try
    Result := FMonthlyRainfall[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_MonthlyRainfall(AMonth: Integer; Value: double);
const OPNAME = 'TYearlyData.Set_MonthlyRainfall';
begin
  try
    FMonthlyRainfall[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_MonthlyPatchSign(AMonth: Integer): WideString;
const OPNAME = 'TYearlyData.Get_MonthlyPatchSign';
begin
  Result := '';
  try
    Result := FMonthlyPatchSign[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_MonthlyPatchSign(AMonth: Integer; const Value: WideString);
const OPNAME = 'TYearlyData.Set_MonthlyPatchSign';
begin
  try
    FMonthlyPatchSign[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_HasUnreliableData: WordBool;
const OPNAME = 'TYearlyData.Get_HasUnreliableData';
begin
  Result := False;
  try
    Result := FUnreliableMonths > 0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_UnreliableMonths: Integer;
const OPNAME = 'TYearlyData.Get_UnreliableMonths';
begin
  Result := 0;
  try
    Result := FUnreliableMonths;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_UnreliableMonths(Value: Integer);
const OPNAME = 'TYearlyData.Set_UnreliableMonths';
begin
  try
    FUnreliableMonths:= Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYearlyData.Get_HasMissingData : WordBool;
const OPNAME = 'TYearlyData.Get_HasMissingData';
begin
  Result := False;
  try
    Result := FMissingMonths > 0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TRainfallDataSplit                                                         *}
{******************************************************************************}

procedure TRainfallDataSplit.CreateMemberObjects;
const OPNAME = 'TRainfallDataSplit.CreateMemberObjects';
begin
  inherited;
  try
    FHighlightMonths := TStringList.Create;
    FHighlightYears  := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.DestroyMemberObjects;
const OPNAME = 'TRainfallDataSplit.DestroyMemberObjects';
begin
  try
    FreeAndNil(FHighlightMonths);
    FreeAndNil(FHighlightYears);
    inherited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit._AddRef: Integer;
const OPNAME = 'TRainfallDataSplit._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallDataSplit._Release: Integer;
const OPNAME = 'TRainfallDataSplit._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallDataSplit.Initialise : boolean;
const OPNAME = 'TRainfallDataSplit.Initialise';
begin
  Result := FALSE;
  try
    FHydroStartYear    := 0;
    FHydroEndYear      := 0;
    FNrOfMissingMonths := 0;
    Result := ResetStats;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallDataSplit.ResetStats : boolean;
const OPNAME = 'TRainfallDataSplit.ResetStats';
var
  lMonth : integer;
begin
  Result := FALSE;
  try
    FHighlightMonths.Clear;
    FHighlightYears.Clear;
    for lMonth := 1 to 12 do
    begin
      FMonthMax[lMonth]     := NullFloat;
      FMonthMin[lMonth]     := NullFloat;
      FMonthCount[lMonth]   := 0;
      FMonthTotal[lMonth]   := 0;
      FMonthMAP[lMonth]     := 0;
      FMonthStdDev[lMonth]  := 0;
      FMonthCV[lMonth]      := 0;
      FXMonthMax[lMonth]    := NullFloat;
      FXMonthMin[lMonth]    := NullFloat;
      FXMonthCount[lMonth]  := 0;
      FXMonthTotal[lMonth]  := 0;
      FXMonthMAP[lMonth]    := 0;
      FXMonthStdDev[lMonth] := 0;
      FXMonthCV[lMonth]     := 0;
    END;
    FYearMin           := NullFloat;
    FYearMax           := NullFloat;
    FYearCount         := 0;
    FGrandTotal        := 0;
    FMAP               := 0;
    FStdDeviation      := 0;
    FCV                := 0;
    FXYearMin          := NullFloat;
    FXYearMax          := NullFloat;
    FXYearCount        := 0;
    FXGrandTotal       := 0;
    FXMAP              := 0;
    FXStdDeviation     := 0;
    FXCV               := 0;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_HydroStartYear: Integer;
const OPNAME = 'TRainfallDataSplit.Get_HydroStartYear';
begin
  Result := 0;
  try
    Result := FHydroStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_HydroStartYear(Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_HydroStartYear';
begin
  try
    FHydroStartYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_HydroEndYear: Integer;
const OPNAME = 'TRainfallDataSplit.Get_HydroEndYear';
begin
  Result := 0;
  try
    Result := FHydroEndYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_HydroEndYear(Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_HydroEndYear';
begin
  try
    FHydroEndYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_NrOfMissingMonths: Integer;
const OPNAME = 'TRainfallDataSplit.Get_NrOfMissingMonths';
begin
  Result := 0;
  try
    Result := FNrOfMissingMonths;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_NrOfMissingMonths(Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_NrOfMissingMonths';
begin
  try
    FNrOfMissingMonths := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_GrandTotal: double;
const OPNAME = 'TRainfallDataSplit.Get_GrandTotal';
begin
  Result := 0.0;
  try
    Result := FGrandTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_GrandTotal(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_GrandTotal';
begin
  try
    FGrandTotal := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MAP: double;
const OPNAME = 'TRainfallDataSplit.Get_MAP';
begin
  Result := 0.0;
  try
    Result := FMAP;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_MAP(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_MAP';
begin
  try
    FMAP := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_StdDeviation: double;
const OPNAME = 'TRainfallDataSplit.Get_StdDeviation';
begin
  Result := 0.0;
  try
    Result := FStdDeviation;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_StdDeviation(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_StdDeviation';
begin
  try
    FStdDeviation := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_CV: double;
const OPNAME = 'TRainfallDataSplit.Get_CV';
begin
  Result := 0.0;
  try
    Result := FCV;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_CV(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_CV';
begin
  try
    FCV := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XGrandTotal: double;
const OPNAME = 'TRainfallDataSplit.Get_XGrandTotal';
begin
  Result := 0.0;
  try
    Result := FXGrandTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XGrandTotal(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_XGrandTotal';
begin
  try
    FXGrandTotal := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMAP: double;
const OPNAME = 'TRainfallDataSplit.Get_XMAP';
begin
  Result := 0.0;
  try
    Result := FXMAP;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMAP(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_XMAP';
begin
  try
    FXMAP := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XStdDeviation: double;
const OPNAME = 'TRainfallDataSplit.Get_XStdDeviation';
begin
  Result := 0.0;
  try
    Result := FXStdDeviation;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XStdDeviation(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_XStdDeviation';
begin
  try
    FXStdDeviation := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XCV: double;
const OPNAME = 'TRainfallDataSplit.Get_XCV';
begin
  Result := 0.0;
  try
    Result := FXCV;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XCV(Value: double);
const OPNAME = 'TRainfallDataSplit.Set_XCV';
begin
  try
    FXCV := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_YearCount: Integer;
const OPNAME = 'TRainfallDataSplit.Get_YearCount';
begin
  Result := 0;
  try
    Result := FYearCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_YearCount(Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_YearCount';
begin
  try
  FYearCount := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_YearMin: Double;
const OPNAME = 'TRainfallDataSplit.Get_YearMin';
begin
  Result := 0.0;
  try
    Result := FYearMin;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallDataSplit.Set_YearMin(Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_YearMin';
begin
  try
    FYearMin := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_YearMax: Double;
const OPNAME = 'TRainfallDataSplit.Get_YearMax';
begin
  Result := 0.0;
  try
    Result := FYearMax;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_YearMax(Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_YearMax';
begin
  try
    FYearMax := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XYearCount: Integer;
const OPNAME = 'TRainfallDataSplit.Get_XYearCount';
begin
  Result := 0;
  try
    Result := FXYearCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XYearCount(Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_XYearCount';
begin
  try
    FXYearCount := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XYearMin: Double;
const OPNAME = 'TRainfallDataSplit.Get_XYearMin';
begin
  Result := 0.0;
  try
    Result := FXYearMin;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XYearMin(Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XYearMin';
begin
  try
    FXYearMin := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XYearMax: Double;
const OPNAME = 'TRainfallDataSplit.Get_XYearMax';
begin
  Result := 0.0;
  try
    Result := FXYearMax;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XYearMax(Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XYearMax';
begin
  try
    FXYearMax := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthCount(AMonth: Integer): Integer;
const OPNAME = 'TRainfallDataSplit.Get_MonthCount';
begin
  Result := 0;
  try
    Result := FMonthCount[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallDataSplit.Set_MonthCount(AMonth: Integer; Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_MonthCount';
begin
  try
    FMonthCount[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthTotal(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_MonthTotal';
begin
  Result := 0.0;
  try
    Result := FMonthTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_MonthTotal(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_MonthTotal';
begin
  try
    FMonthTotal[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthMin(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_MonthMin';
begin
  Result := NullFloat;
  try
    Result := FMonthMin[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_MonthMin(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_MonthMin';
begin
  try
    FMonthMin[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthMax(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_MonthMax';
begin
  Result := NullFloat;
  try
    Result := FMonthMax[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_MonthMax(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_MonthMax';
begin
  try
    FMonthMax[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthMAP(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_MonthMAP';
begin
  Result := 0.0;
  try
    Result := FMonthMAP[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_MonthMAP(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_MonthMAP';
begin
  try
    FMonthMAP[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthStdDev(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_MonthStdDev';
begin
  Result := 0.0;
  try
    Result := FMonthStdDev[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_MonthStdDev(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_MonthStdDev';
begin
  try
    FMonthStdDev[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_MonthCV(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_MonthCV';
begin
  Result := 0.0;
  try
    Result := FMonthCV[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallDataSplit.Set_MonthCV(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_MonthCV';
begin
  try
    FMonthCV[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthCount(AMonth: Integer): Integer;
const OPNAME = 'TRainfallDataSplit.Get_XMonthCount';
begin
  Result := 0;
  try
    Result := FXMonthCount[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthCount(AMonth: Integer; Value: Integer);
const OPNAME = 'TRainfallDataSplit.Set_XMonthCount';
begin
  try
    FXMonthCount[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthTotal(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_XMonthTotal';
begin
  Result := 0.0;
  try
    Result := FXMonthTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthTotal(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XMonthTotal';
begin
  try
    FXMonthTotal[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthMin(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_XMonthMin';
begin
  Result := NullFloat;
  try
    Result := FXMonthMin[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthMin(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XMonthMin';
begin
  try
    FXMonthMin[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthMax(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_XMonthMax';
begin
  Result := NullFloat;
  try
    Result := FXMonthMax[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthMax(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XMonthMax';
begin
  try
    FXMonthMax[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthMAP(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_XMonthMAP';
begin
  Result := 0.0;
  try
    Result := FXMonthMAP[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthMAP(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XMonthMAP';
begin
  try
    FXMonthMAP[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthStdDev(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_XMonthStdDev';
begin
  Result := 0.0;
  try
    Result := FXMonthStdDev[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthStdDev(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XMonthStdDev';
begin
  try
    FXMonthStdDev[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_XMonthCV(AMonth: Integer): Double;
const OPNAME = 'TRainfallDataSplit.Get_XMonthCV';
begin
  Result := 0.0;
  try
    Result := FXMonthCV[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallDataSplit.Set_XMonthCV(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallDataSplit.Set_XMonthCV';
begin
  try
    FXMonthCV[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_HighlightMonths : WideString;
const OPNAME = 'TRainfallDataSplit.Get_HighlightMonths';
begin
  Result := '';
  try
    Result := FHighlightMonths.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallDataSplit.Get_HighlightYears : WideString;
const OPNAME = 'TRainfallDataSplit.Get_HighlightYears';
begin
  Result := '';
  try
    Result := FHighlightYears.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TRainfallData                                                              *}
{******************************************************************************}

procedure TRainfallData.CreateMemberObjects;
const OPNAME = 'TRainfallData.CreateMemberObjects';
begin
  inherited;
  try
    FYearlyData      := TObjectList.Create;
    FHydroYearlyData := TObjectList.Create;
    FHighlightMonths := TStringList.Create;
    FHighlightYears  := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.DestroyMemberObjects;
const OPNAME = 'TRainfallData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FHydroYearlyData);
    FreeAndNil(FYearlyData);
    FreeAndNil(FHighlightMonths);
    FreeAndNil(FHighlightYears);
    inherited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData._AddRef: Integer;
const OPNAME = 'TRainfallData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallData._Release: Integer;
const OPNAME = 'TRainfallData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallData.Initialise : boolean;
const OPNAME = 'TRainfallData.Initialise';
begin
  Result := FALSE;
  try
    FStationNumber     := '';
    FStationID         := 0;
    FStartYear         := 0;
    FEndYear           := 0;
    FHydroStartYear    := 0;
    FHydroEndYear      := 0;
    FNrOfMissingMonths := 0;
    FHydroYearlyData.Clear;
    FYearlyData.Clear;
    Result := ResetStats;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallData.ResetStats : boolean;
const OPNAME = 'TRainfallData.ResetStats';
var
  lMonth : integer;
begin
  Result := FALSE;
  try
    FHighlightMonths.Clear;
    FHighlightYears.Clear;
    for lMonth := 1 to 12 do
    begin
      FMonthMax[lMonth]     := NullFloat;
      FMonthMin[lMonth]     := NullFloat;
      FMonthCount[lMonth]   := 0;
      FMonthTotal[lMonth]   := 0;
      FMonthMAP[lMonth]     := 0;
      FMonthStdDev[lMonth]  := 0;
      FMonthCV[lMonth]      := 0;
      FXMonthMax[lMonth]    := NullFloat;
      FXMonthMin[lMonth]    := NullFloat;
      FXMonthCount[lMonth]  := 0;
      FXMonthTotal[lMonth]  := 0;
      FXMonthMAP[lMonth]    := 0;
      FXMonthStdDev[lMonth] := 0;
      FXMonthCV[lMonth]     := 0;
    END;
    FYearMin           := NullFloat;
    FYearMax           := NullFloat;
    FYearCount         := 0;
    FGrandTotal        := 0;
    FMAP               := 0;
    FStdDeviation      := 0;
    FCV                := 0;
    FXYearMin          := NullFloat;
    FXYearMax          := NullFloat;
    FXYearCount        := 0;
    FXGrandTotal       := 0;
    FXMAP              := 0;
    FXStdDeviation     := 0;
    FXCV               := 0;

    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_StationID: Integer;
const OPNAME = 'TRainfallData.Get_StationID';
begin
  Result := 0;
  try
    Result := FStationID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_HydroYearlyData: TObjectList;
const OPNAME = 'TRainfallData.Get_HydroYearlyData';
begin
  Result := nil;
  try
    Result := FHydroYearlyData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_YearlyData: TObjectList;
const OPNAME = 'TRainfallData.Get_YearlyData';
begin
  Result := nil;
  try
    Result := FYearlyData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_StationID(Value: Integer);
const OPNAME = 'TRainfallData.Set_StationID';
begin
  try
    FStationID := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_StationNumber: WideString;
const OPNAME = 'TRainfallData.Get_StationNumber';
begin
  Result := '';
  try
    Result := FStationNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_StationNumber(const Value: WideString);
const OPNAME = 'TRainfallData.Set_StationNumber';
begin
  try
    FStationNumber := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_StartYear: Integer;
const OPNAME = 'TRainfallData.Get_StartYear';
begin
  Result := 0;
  try
    Result := FStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_StartYear(Value: Integer);
const OPNAME = 'TRainfallData.Set_StartYear';
begin
  try
    FStartYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_EndYear: Integer;
const OPNAME = 'TRainfallData.Get_EndYear';
begin
  Result := 0;
  try
    Result := FEndYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_EndYear(Value: Integer);
const OPNAME = 'TRainfallData.Set_EndYear';
begin
  try
    FEndYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_HydroStartYear: Integer;
const OPNAME = 'TRainfallData.Get_HydroStartYear';
begin
  Result := 0;
  try
    Result := FHydroStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_HydroStartYear(Value: Integer);
const OPNAME = 'TRainfallData.Set_HydroStartYear';
begin
  try
    FHydroStartYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_HydroEndYear: Integer;
const OPNAME = 'TRainfallData.Get_HydroEndYear';
begin
  Result := 0;
  try
    Result := FHydroEndYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_HydroEndYear(Value: Integer);
const OPNAME = 'TRainfallData.Set_HydroEndYear';
begin
  try
    FHydroEndYear := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_GrandTotal: Double;
const OPNAME = 'TRainfallData.Get_GrandTotal';
begin
  Result := 0;
  try
    Result := FGrandTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_GrandTotal(Value: Double);
const OPNAME = 'TRainfallData.Set_GrandTotal';
begin
  try
    FGrandTotal := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_StdDeviation: Double;
const OPNAME = 'TRainfallData.Get_StdDeviation';
begin
  Result := 0;
  try
    Result := FStdDeviation;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_StdDeviation(Value: Double);
const OPNAME = 'TRainfallData.Set_StdDeviation';
begin
  try
    FStdDeviation := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_CV: Double;
const OPNAME = 'TRainfallData.Get_CV';
begin
  Result := 0;
  try
    Result := FCV;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_CV(Value: Double);
const OPNAME = 'TRainfallData.Set_CV';
begin
  try
    FCV := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MAP: Double;
const OPNAME = 'TRainfallData.Get_MAP';
begin
  Result := 0;
  try
    Result := FMAP;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_MAP(Value: Double);
const OPNAME = 'TRainfallData.Set_MAP';
begin
  try
    FMAP := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XGrandTotal: double;
const OPNAME = 'TRainfallData.Get_XGrandTotal';
begin
  Result := 0.0;
  try
    Result := FXGrandTotal;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XGrandTotal(Value: double);
const OPNAME = 'TRainfallData.Set_XGrandTotal';
begin
  try
    FXGrandTotal := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMAP: double;
const OPNAME = 'TRainfallData.Get_XMAP';
begin
  Result := 0.0;
  try
    Result := FXMAP;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMAP(Value: double);
const OPNAME = 'TRainfallData.Set_XMAP';
begin
  try
    FXMAP := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XStdDeviation: double;
const OPNAME = 'TRainfallData.Get_XStdDeviation';
begin
  Result := 0.0;
  try
    Result := FXStdDeviation;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XStdDeviation(Value: double);
const OPNAME = 'TRainfallData.Set_XStdDeviation';
begin
  try
    FXStdDeviation := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XCV: double;
const OPNAME = 'TRainfallData.Get_XCV';
begin
  Result := 0.0;
  try
    Result := FXCV;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XCV(Value: double);
const OPNAME = 'TRainfallData.Set_XCV';
begin
  try
    FXCV := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TRainfallData.Get_YearCount: Integer;
const OPNAME = 'TRainfallData.Get_YearCount';
begin
  Result := 0;
  try
    Result := FYearCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_YearCount(Value: Integer);
const OPNAME = 'TRainfallData.Set_YearCount';
begin
  try
  FYearCount := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_YearMin: Double;
const OPNAME = 'TRainfallData.Get_YearMin';
begin
  Result := 0.0;
  try
    Result := FYearMin;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallData.Set_YearMin(Value: Double);
const OPNAME = 'TRainfallData.Set_YearMin';
begin
  try
    FYearMin := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_YearMax: Double;
const OPNAME = 'TRainfallData.Get_YearMax';
begin
  Result := 0.0;
  try
    Result := FYearMax;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_YearMax(Value: Double);
const OPNAME = 'TRainfallData.Set_YearMax';
begin
  try
    FYearMax := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XYearCount: Integer;
const OPNAME = 'TRainfallData.Get_XYearCount';
begin
  Result := 0;
  try
    Result := FXYearCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XYearCount(Value: Integer);
const OPNAME = 'TRainfallData.Set_XYearCount';
begin
  try
    FXYearCount := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XYearMin: Double;
const OPNAME = 'TRainfallData.Get_XYearMin';
begin
  Result := 0.0;
  try
    Result := FXYearMin;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XYearMin(Value: Double);
const OPNAME = 'TRainfallData.Set_XYearMin';
begin
  try
    FXYearMin := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XYearMax: Double;
const OPNAME = 'TRainfallData.Get_XYearMax';
begin
  Result := 0.0;
  try
    Result := FXYearMax;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XYearMax(Value: Double);
const OPNAME = 'TRainfallData.Set_XYearMax';
begin
  try
    FXYearMax := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthCount(AMonth: Integer): Integer;
const OPNAME = 'TRainfallData.Get_MonthCount';
begin
  Result := 0;
  try
    Result := FMonthCount[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallData.Set_MonthCount(AMonth: Integer; Value: Integer);
const OPNAME = 'TRainfallData.Set_MonthCount';
begin
  try
    FMonthCount[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthTotal(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_MonthTotal';
begin
  Result := 0.0;
  try
    Result := FMonthTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_MonthTotal(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_MonthTotal';
begin
  try
    FMonthTotal[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthMin(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_MonthMin';
begin
  Result := NullFloat;
  try
    Result := FMonthMin[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_MonthMin(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_MonthMin';
begin
  try
    FMonthMin[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthMax(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_MonthMax';
begin
  Result := NullFloat;
  try
    Result := FMonthMax[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_MonthMax(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_MonthMax';
begin
  try
    FMonthMax[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthMAP(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_MonthMAP';
begin
  Result := 0.0;
  try
    Result := FMonthMAP[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_MonthMAP(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_MonthMAP';
begin
  try
    FMonthMAP[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthStdDev(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_MonthStdDev';
begin
  Result := 0.0;
  try
    Result := FMonthStdDev[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_MonthStdDev(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_MonthStdDev';
begin
  try
    FMonthStdDev[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_MonthCV(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_MonthCV';
begin
  Result := 0.0;
  try
    Result := FMonthCV[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRainfallData.Set_MonthCV(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_MonthCV';
begin
  try
    FMonthCV[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthCount(AMonth: Integer): Integer;
const OPNAME = 'TRainfallData.Get_XMonthCount';
begin
  Result := 0;
  try
    Result := FXMonthCount[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthCount(AMonth: Integer; Value: Integer);
const OPNAME = 'TRainfallData.Set_XMonthCount';
begin
  try
    FXMonthCount[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthTotal(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_XMonthTotal';
begin
  Result := 0.0;
  try
    Result := FXMonthTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthTotal(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_XMonthTotal';
begin
  try
    FXMonthTotal[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthMin(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_XMonthMin';
begin
  Result := NullFloat;
  try
    Result := FXMonthMin[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthMin(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_XMonthMin';
begin
  try
    FXMonthMin[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthMax(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_XMonthMax';
begin
  Result := NullFloat;
  try
    Result := FXMonthMax[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthMax(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_XMonthMax';
begin
  try
    FXMonthMax[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthMAP(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_XMonthMAP';
begin
  Result := 0.0;
  try
    Result := FXMonthMAP[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthMAP(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_XMonthMAP';
begin
  try
    FXMonthMAP[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthStdDev(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_XMonthStdDev';
begin
  Result := 0.0;
  try
    Result := FXMonthStdDev[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthStdDev(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_XMonthStdDev';
begin
  try
    FXMonthStdDev[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_XMonthCV(AMonth: Integer): Double;
const OPNAME = 'TRainfallData.Get_XMonthCV';
begin
  Result := 0.0;
  try
    Result := FXMonthCV[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_XMonthCV(AMonth: Integer; Value: Double);
const OPNAME = 'TRainfallData.Set_XMonthCV';
begin
  try
    FXMonthCV[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_NrOfMissingMonths: Integer;
const OPNAME = 'TRainfallData.Get_NrOfMissingMonths';
begin
  Result := 0;
  try
    Result := FNrOfMissingMonths;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.Set_NrOfMissingMonths(Value: Integer);
const OPNAME = 'TRainfallData.Set_NrOfMissingMonths';
begin
  try
    FNrOfMissingMonths := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_HighlightMonths : WideString;
const OPNAME = 'TRainfallData.Get_HighlightMonths';
begin
  Result := '';
  try
    Result := FHighlightMonths.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.Get_HighlightYears : WideString;
const OPNAME = 'TRainfallData.Get_HighlightYears';
begin
  Result := '';
  try
    Result := FHighlightYears.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.HydroYearsCount : integer;
const OPNAME = 'TRainfallData.HydroYearsCount';
begin
  Result := 0;
  try
    Result := FHydroYearlyData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.HydroStartDate : TDateTime;
const OPNAME = 'TRainfallData.HydroStartDate';
var
  lYear  : integer;
  lMonth : integer;
begin
  Result := 0;
  try
    lYear := FHydroStartYear;
    if (lYear > 0) then
    begin
      lMonth:= (FAppModules.Model.ModelData as IRainfallModelData).HydroStartMonth;
      Result := EncodeDate(lYear, lMonth, 1);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.HydroEndDate : TDateTime;
const OPNAME = 'TRainfallData.HydroEndDate';
var
  lYear  : integer;
  lMonth : integer;
begin
  Result := 0;
  try
    lYear := FHydroEndYear;
    if (lYear > 0) then
    begin
      lMonth := (FAppModules.Model.ModelData as IRainfallModelData).HydroStartMonth;
      if (lMonth = 1) then
        lMonth := 12
      else
        lMonth := lMonth - 1;
      Result := EncodeDate(lYear+1, lMonth, 30);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.CalculateStats;
const OPNAME = 'TRainfallData.CalculateStats';
var
  lYear             : integer;
  lMonth            : integer;
  lYearlyData       : TYearlyData;
  lMissingMonths    : integer;
  lUnreliableMonths : integer;
  lRainfallObj      : IRainfallModelData;
  lRAWFlags         : string;
  lFlag             : string;
  lValue            : double;
  lTotal            : double;
  lXTotal           : double;
  lMonthTotal       : array [1..12] of double;
  lXMonthTotal      : array [1..12] of double;
begin
  try
    ResetStats;
    lTotal            := 0;
    lRainfallObj      := (FAppModules.Model.ModelData as IRainfallModelData);
    lRAWFlags         := lRainfallObj.RAWFlags;
    // Calculate totals, counts, min and max
    for lYear := 0 to FHydroYearlyData.Count - 1 do
    begin
      lYearlyData       := TYearlyData(FHydroYearlyData.Items[lYear]);
      lYearlyData.Total := 0;
      lMissingMonths    := 0;
      lUnreliableMonths := 0;
      lYearlyData.UnreliableMonths := 0;
      lYearlyData.MissingMonths    := 0;
      for lMonth := 1 to 12 do
      begin
        lFlag  := lYearlyData.MonthlyPatchSign[lMonth];
        lValue := lYearlyData.MonthlyRainfall[lMonth];
        if (lYearlyData.FMonthlyRainfall[lMonth] = NullFloat) then
          lMissingMonths := lMissingMonths + 1
        else
        begin
          FMonthTotal[lMonth] := FMonthTotal[lMonth] + lValue;
          FMonthCount[lMonth] := FMonthCount[lMonth] + 1;
          if ((FMonthMax[lMonth] = NullFloat) OR
              (lValue > FMonthMax[lMonth])) then
            FMonthMax[lMonth] := lValue;
          if ((FMonthMin[lMonth] = NullFloat) OR
              (lValue < FMonthMin[lMonth])) then
            FMonthMin[lMonth] := lValue;
          lYearlyData.Total := lYearlyData.Total + lValue;

          if (Pos(lFlag, lRAWFlags) > 0) then {unreliable}
            lUnreliableMonths := lUnreliableMonths + 1
          else
          begin
            FXMonthTotal[lMonth] := FXMonthTotal[lMonth] + lValue;
            FXMonthCount[lMonth] := FXMonthCount[lMonth] + 1;
            if ((FXMonthMax[lMonth] = NullFloat) OR
                (lValue > FXMonthMax[lMonth])) then
              FXMonthMax[lMonth] := lValue;
            if ((FXMonthMin[lMonth] = NullFloat) OR
                (lValue < FXMonthMin[lMonth])) then
              FXMonthMin[lMonth] := lValue;
          end;
        end;
      end;
      lYearlyData.MissingMonths    := lMissingMonths;
      lYearlyData.UnreliableMonths := lUnreliableMonths;
      lTotal := lTotal + lMissingMonths;
      if (NOT lYearlyData.HasMissingData) then
      begin
        FGrandTotal := FGrandTotal + lYearlyData.Total;
        FYearCount  := FYearCount + 1;
        if ((FYearMax = NullFloat) OR (lYearlyData.Total > FYearMax)) then
          FYearMax := lYearlyData.Total;
        if ((FYearMin = NullFloat) OR (lYearlyData.Total < FYearMin)) then
          FYearMin := lYearlyData.Total;
        if (NOT lYearlyData.HasUnreliableData) then
        begin
          FXGrandTotal := FXGrandTotal + lYearlyData.Total;
          FXYearCount  := FXYearCount + 1;
          if ((FXYearMax = NullFloat) OR (lYearlyData.Total > FXYearMax)) then
            FXYearMax := lYearlyData.Total;
          if ((FXYearMin = NullFloat) OR (lYearlyData.Total < FXYearMin)) then
            FXYearMin := lYearlyData.Total;
        end;
      end;
    end;
    FNrOfMissingMonths := Trunc(lTotal);
    // Calculate MAP = mean average rainfall
    if (FYearCount > 0) then
      FMAP  := FGrandTotal / FYearCount;
    if (FXYearCount > 0) then
      FXMAP := FXGrandTotal / FXYearCount;
    for lMonth := 1 to 12 do
    begin
      if (FMonthCount[lMonth] > 0) then
        FMonthMAP[lMonth] := FMonthTotal[lMonth]/FMonthCount[lMonth];
      if (FXMonthCount[lMonth] > 0) then
        FXMonthMAP[lMonth] := FXMonthTotal[lMonth]/FXMonthCount[lMonth];
    end;
    // Calculate standard deviation and CV
    lTotal := 0;
    lXTotal := 0;
    for lMonth := 1 to 12 do
    begin
      lMonthTotal[lMonth] := 0;
      lXMonthTotal[lMonth] := 0;
    end;
    for lYear := 0 to FHydroYearlyData.Count - 1 do
    begin
      lYearlyData := TYearlyData(FHydroYearlyData.Items[lYear]);
      if (NOT lYearlyData.HasMissingData) then
      begin
        lTotal := lTotal + Power(lYearlyData.Total - FMAP, 2);
        if (NOT lYearlyData.HasUnreliableData) then
          lXTotal := lXTotal + Power(lYearlyData.Total - FXMAP, 2);
      end;
      for lMonth := 1 to 12 do
      begin
        lValue := lYearlyData.MonthlyRainfall[lMonth];
        lFlag  := lYearlyData.MonthlyPatchSign[lMonth];
        if (lValue <> NullFloat) then
        begin
          lMonthTotal[lMonth] := lMonthTotal[lMonth] + Power((lValue - FMonthMAP[lMonth]), 2);
          if (Pos(lFlag, lRAWFlags) <= 0) then {not unreliable}
            lXMonthTotal[lMonth] := lXMonthTotal[lMonth] + Power((lValue - FXMonthMAP[lMonth]), 2);
        end;
      end;
    end;
    if (FYearCount > 1) then
      FStdDeviation := Sqrt(lTotal / (FYearCount - 1));
    if (FXYearCount > 1) then
      FXStdDeviation := Sqrt(lXTotal / (FXYearCount - 1));
    if (FMAP > 0) then
      FCV := FStdDeviation / FMAP * 100;
    if (FXMAP > 0) then
      FXCV := FXStdDeviation / FXMAP * 100;
    for lMonth := 1 to 12 do
    begin
      if (FMonthCount[lMonth] > 1) then
      begin
        FMonthStdDev[lMonth] := Sqrt(lMonthTotal[lMonth] / (FMonthCount[lMonth] - 1));
        if (FMonthMAP[lMonth] > 0) then
          FMonthCV[lMonth] := FMonthStdDev[lMonth] / FMonthMAP[lMonth] * 100;
      end;
      if (FXMonthCount[lMonth] > 1) then
      begin
        FXMonthStdDev[lMonth] := Sqrt(lXMonthTotal[lMonth] / (FXMonthCount[lMonth] - 1));
        if (FXMonthMAP[lMonth] > 0) then
          FXMonthCV[lMonth] := FXMonthStdDev[lMonth] / FXMonthMAP[lMonth] * 100;
      end;
    end;
    PopulateHighlights;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.PopulateRepeats;
const OPNAME = 'TRainfallData.PopulateRepeats';
var
  lYearIdx1    : integer;
  lYearIdx2    : integer;
  lMonthIdx1   : integer;
  lMonthIdx2   : integer;
  lYearlyData1 : IYearlyData;
  lYearlyData2 : IYearlyData;
  lValue1      : double;
  lValue2      : double;
  lRepeat      : boolean;
  lCount       : integer;
  lDateStr     : string;
  lMonth       : integer;
  lYear        : integer;
begin
  try
    lYearIdx1  := 0;
    lMonthIdx1 := 1;
    while (lYearIdx1 < HydroYearsCount) AND (lMonthIdx1 <= 12) do
    begin
      lYearlyData1 := GetHydroYearDataByIndex(lYearIdx1);
      lValue1    := lYearlyData1.MonthlyRainfall[lMonthIdx1];
      if ((lValue1 = NullFloat) OR (lValue1 = 0)) then
      begin
        lMonthIdx1 := lMonthIdx1 + 1;
        if (lMonthIdx1 > 12) then
        begin
          lMonthIdx1 := 1;
          lYearIdx1  := lYearIdx1 + 1;
        end;
      end
      else
      begin
        lYear      := lYearlyData1.Year;
        if (lMonthIdx1 <= 3) then
          lMonth   := lMonthIdx1 + 9
        else
        begin
          lMonth   := lMonthIdx1 - 3;
          lYear    := lYear + 1;
        end;
        lDateStr   := Format('%4d%2d', [lYear, lMonth]);;
        FHighlightMonths.Add(lDateStr);
        lCount     := 1;
        lRepeat    := (lValue1 <> NullFloat) AND (lValue1 <> 0);
        lYearIdx2  := lYearIdx1;
        lMonthIdx2 := lMonthIdx1;
        while (lRepeat) do
        begin
          lMonthIdx2 := lMonthIdx2 + 1;
          if (lMonthIdx2 > 12) then
          begin
            lMonthIdx2 := 1;
            lYearIdx2  := lYearIdx2 + 1;
          end;
          if (lYearIdx2 < HydroYearsCount) then
          begin
            lYearlyData2 := GetHydroYearDataByIndex(lYearIdx2);
            if ((lYearlyData2.Year >= HydroStartYear) AND (lYearlyData2.Year <= HydroEndYear)) then
            begin
              lValue2 := lYearlyData2.MonthlyRainfall[lMonthIdx2];
              if (lValue1 = lValue2) then
              begin
                lCount   := lCount + 1;
                lYear    := lYearlyData2.Year;
                if (lMonthIdx2 <= 3) then
                  lMonth := lMonthIdx2 + 9
                else
                begin
                  lMonth := lMonthIdx2 - 3;
                  lYear  := lYear + 1;
                end;
                lDateStr := Format('%4d%2d', [lYear, lMonth]);
                FHighlightMonths.Add(lDateStr);
              end
              else
                lRepeat := FALSE;
            end;
          end
          else
            lRepeat := FALSE;
        end;
        if (lCount <= 3) then
        begin
          while (lCount >= 1) do
          begin
            FHighlightMonths.Delete(FHighlightMonths.Count - 1);
            lCount := lCount - 1;
          end;
        end;
        lMonthIdx1 := lMonthIdx2;
        lYearIdx1  := lYearIdx2;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.PopulateHighlights;
const OPNAME = 'TRainfallData.PopulateHighlights';
var
  lMonth         : integer;
  lYear          : integer;
  lRainfallObj   : IRainfallModelData;
  lWetMonths     : string;
  lYearMAP       : double;
  lMonthMAP      : double;
  lYearIdx       : integer;
  lMonthIdx      : integer;
  lYearlyData    : IYearlyData;
  lValue         : double;
  lHighlight     : boolean;
  lHighlightYear : boolean;
  lDateStr       : string;
begin
  try
    FHighlightMonths.Clear;
    FHighlightYears.Clear;

    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lWetMonths   := lRainfallObj.WetSeasonMonths;
    if (lRainfallObj.HighlightRepeatingValues) then
      PopulateRepeats;

    if (lRainfallObj.IncludeUnreliableData) then
      lYearMAP := FMAP
    else
      lYearMAP := FXMAP;
    for lYearIdx := 0 to HydroYearsCount - 1 do
    begin
      lYearlyData    := GetHydroYearDataByIndex(lYearIdx);
      lHighlightYear := FALSE;
      { Annual value greater than twice the annual average }
      if (NOT lHighlightYear) AND (lRainfallObj.HighlightAnnualGreaterThanAverage) then
        lHighlightYear := (lYearlyData.Total > (lYearMAP * 2));
      { Annual value less than the annual average }
      if (NOT lHighlightYear) AND (lRainfallObj.HighlightAnnualLessThanAverage) then
        lHighlightYear := (lYearlyData.Total < (lYearMAP * 0.5));
      if (lHighlightYear) then
        FHighlightYears.Add(IntToStr(lYearlyData.Year));

      for lMonthIdx := 1 to 12 do
      begin
        if (lMonthIdx <= 3) then
        begin
          lMonth := lMonthIdx + 9;
          lYear  := lYearlyData.Year;
        end
        else
        begin
          lMonth := lMonthIdx - 3;
          lYear  := lYearlyData.Year + 1;
        end;
        lDateStr := Format('%4d%2d', [lYear, lMonth]);
        if (FHighlightMonths.IndexOf(lDateStr) < 0) then
        begin
          lHighlight := FALSE;
          lValue     := lYearlyData.MonthlyRainfall[lMonthIdx];

          { Zeros in wet months}
          if (NOT lHighlight) AND (lRainfallObj.HighlightWetSeasonZeros) then
          begin
            if (lValue = 0) then
            begin
              if (Pos(IntToStr(lMonth), lWetMonths) > 0) then
                lHighlight := TRUE;
            end;
          end;
          { Monthly value greater than user defined proportion of monthly average }
          if (NOT lHighlight) AND (lRainfallObj.HighlightMonthlyGreaterThanProportion) then
          begin
            if (lRainfallObj.IncludeUnreliableData) then
              lMonthMAP := FMonthMAP[lMonthIdx]
            else
              lMonthMAP := FXMonthMAP[lMonthIdx];
            lHighlight := (lValue > (lMonthMAP * lRainfallObj.MonthlyGreaterThanProportionValue));
          end;
          { Monthly value greater than user defined value }
          if (NOT lHighlight) AND (lRainfallObj.HighlightMonthlyGreaterThanAbsolute) then
            lHighlight := (lValue > lRainfallObj.MonthlyGreaterThanAbsoluteValue);
          { Values rounded to 100mm }
          if (NOT lHighlight) AND (lRainfallObj.HighlightRoundedValues) then
          begin
            lHighlight := (lValue <> 0) AND ((Trunc(lValue * 10) mod 1000) = 0);
          end;

          if (lHighlight) then
            FHighlightMonths.Add(lDateStr);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.CastHydroYearDataByIndex (AIndex : integer) : TYearlyData;
const OPNAME = 'TRainfallData.CastHydroYearDataByIndex';
begin
  Result := nil;
  try
    if (AIndex < FHydroYearlyData.Count) then
      Result := TYearlyData(FHydroYearlyData.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.GetHydroYearDataByIndex (AIndex : integer) : IYearlyData;
const OPNAME = 'TRainfallData.GetHydroYearDataByIndex';
begin
  Result := nil;
  try
    Result :=  CastHydroYearDataByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.CastHydroYearDataByYear(AYear: Integer): TYearlyData;
const OPNAME = 'TRainfallData.CastHydroYearDataByYear';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FYearlyData.Count - 1 do
    begin
      if TYearlyData(FYearlyData.Items[lIndex]).Year = AYear then
      begin
        Result := TYearlyData(FYearlyData.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.GetHydroYearDataByYear(AYear: Integer): IYearlyData;
const OPNAME = 'TRainfallData.GetHydroYearDataByYear';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FYearlyData.Count - 1 do
    begin
      if TYearlyData(FYearlyData.Items[lIndex]).Year = AYear then
      begin
        Result := TYearlyData(FYearlyData.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallData.AddYearlyData(AYearlyData: TYearlyData): boolean;
const OPNAME = 'TRainfallData.AddYearlyData';
begin
  Result := False;
  try
    FHydroYearlyData.Add(AYearlyData);
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallData.GetBaseDataForYearAndMonth (AYear          : integer;
                                                    AMonth         : integer;
                                                    var ARainfall  : double;
                                                    var APatchSign : WideString);
const OPNAME = 'TRainfallData.GetBaseDataForYearAndMonth';
var
  lYearlyData  : TYearlyData;
  lIndex       : integer;
  lFound       : boolean;
begin
  try
    ARainfall  := NullFloat;
    APatchSign := '';
    lIndex     := 0;
    lFound     := FALSE;
    while ((NOT lFound) AND (lIndex < FYearlyData.Count)) do
    begin
      lYearlyData := TYearlyData(FYearlyData.Items[lIndex]);
      if (AYear = lYearlyData.Year) then
      begin
        ARainfall  := lYearlyData.MonthlyRainfall[AMonth];
        APatchSign := lYearlyData.MonthlyPatchSign[AMonth];
        lFound     := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TRainfallData.StreamOut (const ATitle : WideString) : WideString;
const OPNAME = 'TRainfallData.StreamOut';
var
  lMonthIdx         : integer;
  lIndex            : integer;
  lHydroCount       : integer;
  lMax              : array [1..12] of double;
  lMin              : array [1..12] of double;
  lMonthTotal       : array [1..12] of double;
  lMonthCount       : array [1..12] of integer;
  lMonthAvg         : array [1..12] of double;
  lMonthStdDev      : array [1..12] of double;
  lYearlyData       : IYearlyData;
  lYearMin          : double;
  lYearMax          : double;
  lYearCount        : integer;
  lTempStr          : string;
begin
  Result := '';
  try
    Result := ATitle + #13#10;
    lTempStr := FAppModules.Language.GetString('Rainfall.MonthString');
    Result := Result + lTempStr + #13#10;

    lHydroCount := HydroYearsCount;
    for lMonthIdx := 1 to 12 do
    begin
      lMonthTotal[lMonthIdx] := 0;
      lMonthCount[lMonthIdx] := 0;
      lMax[lMonthIdx] := NullFloat;
      lMin[lMonthIdx] := NullFloat;
    end;
    lYearCount := 0;
    lYearMin   := NullFloat;
    lYearMax   := NullFloat;
    // Calculate totals and counts
    for lIndex := 0 to lHydroCount - 1 do
    begin
      lYearlyData := GetHydroYearDataByIndex(lIndex);
      lTempStr := lYearlyData.HydroYear;
      for lMonthIdx := 1 to 12 do
      begin
        if (lYearlyData.MonthlyRainfall[lMonthIdx] = NullFloat) then
        begin
          lTempStr := lTempStr + ',';
        end
        else
        begin
          lTempStr := lTempStr + ',' + Format('%6.1f', [lYearlyData.MonthlyRainfall[lMonthIdx]]);
          lMonthTotal[lMonthIdx] := lMonthTotal[lMonthIdx] + lYearlyData.MonthlyRainfall[lMonthIdx];
          lMonthCount[lMonthIdx] := lMonthCount[lMonthIdx] + 1;
          if ((lMax[lMonthIdx] = NullFloat) OR
              (lYearlyData.MonthlyRainfall[lMonthIdx] > lMax[lMonthIdx])) then
            lMax[lMonthIdx] := lYearlyData.MonthlyRainfall[lMonthIdx];
          if ((lMin[lMonthIdx] = NullFloat) OR
              (lYearlyData.MonthlyRainfall[lMonthIdx] < lMin[lMonthIdx])) then
            lMin[lMonthIdx] := lYearlyData.MonthlyRainfall[lMonthIdx];
        end;
        lTempStr := lTempStr + ',' + lYearlyData.MonthlyPatchSign[lMonthIdx];
      end;
      if (lYearlyData.HasMissingData) then
        lTempStr := lTempStr + ',' + Format('%6.1f', [lYearlyData.Total]) + ',+'
      else
      begin
        lTempStr := lTempStr + ',' + Format('%6.1f', [lYearlyData.Total]);
        lYearCount := lYearCount + 1;
        if ((lYearMax = NullFloat) OR (lYearlyData.Total > lYearMax)) then
          lYearMax := lYearlyData.Total;
        if ((lYearMin = NullFloat) OR (lYearlyData.Total < lYearMin)) then
          lYearMin := lYearlyData.Total;
      end;
      Result:= Result + lTempStr + #13#10;
    end;

    // Calculate Avg
    for lMonthIdx := 1 to 12 do
    begin
      if (lMonthCount[lMonthIdx] > 0) then
        lMonthAvg[lMonthIdx] := lMonthTotal[lMonthIdx]/lMonthCount[lMonthIdx]
      else
        lMonthAvg[lMonthIdx] := 0;
    end;
    // Calculate StdDev
    for lMonthIdx := 1 to 12 do
    begin
      lMonthTotal[lMonthIdx] := 0;
    end;
    for lIndex := 0 to lHydroCount - 1 do
    begin
      lYearlyData := GetHydroYearDataByIndex(lIndex);
      for lMonthIdx := 1 to 12 do
      begin
        if (lYearlyData.MonthlyRainfall[lMonthIdx] <> NullFloat) then
        begin
          lMonthTotal[lMonthIdx] := lMonthTotal[lMonthIdx] + Power((lYearlyData.MonthlyRainfall[lMonthIdx] - lMonthAvg[lMonthIdx]), 2);
        end;
      end;
    end;
    for lMonthIdx := 1 to 12 do
    begin
      if (lMonthCount[lMonthIdx] > 1) then
        lMonthStdDev[lMonthIdx] := Sqrt(lMonthTotal[lMonthIdx] / (lMonthCount[lMonthIdx] - 1))
      else
        lMonthStdDev[lMonthIdx] := 0;
    end;

    lTempStr := 'Avg.';
    for lMonthIdx := 1 to 12 do
      lTempStr := lTempStr + ',' + Format('%6.1f', [lMonthAvg[lMonthIdx]]) + ',';
    lTempStr := lTempStr + ',' + Format('%6.1f', [FMAP]);
    Result := Result + lTempStr + #13#10;

    lTempStr := FAppModules.Language.GetString('Rainfall.StdDev');
    for lMonthIdx := 1 to 12 do
      lTempStr := lTempStr + ',' + Format('%6.1f', [lMonthStdDev[lMonthIdx]]) + ',';
    lTempStr := lTempStr + ',' + Format('%6.1f', [FStdDeviation]);
    Result := Result + lTempStr + #13#10;

    lTempStr := FAppModules.Language.GetString('Rainfall.Max');
    for lMonthIdx := 1 to 12 do
    begin
      if (lMax[lMonthIdx] <> NullFloat) then
        lTempStr := lTempStr + ',' + Format('%6.1f', [lMax[lMonthIdx]]) + ','
      else
        lTempStr := lTempStr + ',,';
    end;
    lTempStr := lTempStr + ',' + Format('%6.1f', [lYearMax]);
    Result := Result + lTempStr + #13#10;

    lTempStr := FAppModules.Language.GetString('Rainfall.Min');
    for lMonthIdx := 1 to 12 do
    begin
      if (lMin[lMonthIdx] <> NullFloat) then
        lTempStr := lTempStr + ',' + Format('%6.1f', [lMin[lMonthIdx]]) + ','
      else
        lTempStr := lTempStr + ',,';
    end;
    lTempStr := lTempStr + ',' + Format('%6.1f', [lYearMin]);
    Result := Result + lTempStr + #13#10;

    lTempStr := FAppModules.Language.GetString('Rainfall.Cnt');
    for lMonthIdx := 1 to 12 do
      lTempStr := lTempStr + ',' + Format('%6d', [lMonthCount[lMonthIdx]]) + ',';
    lTempStr := lTempStr + ',' + Format('%6d', [lYearCount]);
    Result := Result + lTempStr + #13#10;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
  
{******************************************************************************}
{* TStationData                                                               *}
{******************************************************************************}

procedure TStationData.CreateMemberObjects;
const OPNAME = 'TStationData.CreateMemberObjects';
begin
  inherited;
  try
    FRainfallData := TRainfallData.Create(FAppModules);
    FPatches      := TObjectList.Create;
    FDataSplits   := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.DestroyMemberObjects;
const OPNAME = 'TStationData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDataSplits);
    FreeAndNil(FPatches);
    FreeAndNil(FRainfallData);
    inherited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData._AddRef: Integer;
const OPNAME = 'TStationData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStationData._Release: Integer;
const OPNAME = 'TStationData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStationData.Initialise : boolean;
const OPNAME = 'TStationData.Initialise';
begin
  Result := FALSE;
  try
    FStationName       := '';
    FLatitude          := 0;
    FLongitude         := 0;
    FHeight            := 0;
    FStationType       := '';
    FWR90              := FALSE;
    FPatches.Clear;
    FDataSplits.Clear;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_StationName: WideString;
const OPNAME = 'TStationData.Get_StationName';
begin
  Result := '';
  try
    Result := FStationName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.Set_StationName(const Value: WideString);
const OPNAME = 'TStationData.Set_StationName';
begin
  try
    FStationName := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_Latitude: Integer;
const OPNAME = 'TStationData.Get_Latitude';
begin
  Result := 0;
  try
    Result := FLatitude;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.Set_Latitude(Value: Integer);
const OPNAME = 'TStationData.Set_Latitude';
begin
  try
    FLatitude := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_Longitude: Integer;
const OPNAME = 'TStationData.Get_Longitude';
begin
  Result := 0;
  try
    Result := FLongitude;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.Set_Longitude(Value: Integer);
const OPNAME = 'TStationData.Set_Longitude';
begin
  try
    FLongitude := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_Height: Integer;
const OPNAME = 'TStationData.Get_Height';
begin
  Result := 0;
  try
    Result := FHeight;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.Set_Height(Value: Integer);
const OPNAME = 'TStationData.Set_Height';
begin
  try
    FHeight := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_StationType: WideString;
const OPNAME = 'TStationData.Get_StationType';
begin
  Result := '';
  try
    Result := FStationType;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.Set_StationType(const Value: WideString);
const OPNAME = 'TStationData.Set_StationType';
begin
  try
    FStationType := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_IsInWR90: WordBool;
const OPNAME = 'TStationData.Get_IsInWR90';
begin
  Result := FALSE;
  try
    Result := FWR90;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.Set_IsInWR90(Value: WordBool);
const OPNAME = 'TStationData.Set_IsInWR90';
begin
  try
    FWR90 := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.Get_RainfallData : IRainfallData;
const OPNAME = 'TStationData.Get_RainfallData';
begin
  Result := nil;
  try
    Result := FRainfallData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.CastPatchWithID (APatchID : integer) : TPatchData;
const OPNAME = 'TStationData.CastPatchWithID';
var
  lPatch : TPatchData;
  lIndex : integer;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPatches.Count)) do
    begin
      lPatch := TPatchData(FPatches.Items[lIndex]);
      if (APatchID = lPatch.PatchID) then
        Result := lPatch
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.CastPatchWithName (APatchName : string) : TPatchData;
const OPNAME = 'TStationData.CastPatchWithName';
var
  lPatch : TPatchData;
  lIndex : integer;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FPatches.Count)) do
    begin
      lPatch := TPatchData(FPatches.Items[lIndex]);
      if (APatchName = lPatch.PatchName) then
        Result := lPatch
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.GetPatchWithID (APatchID : integer) : IPatchData;
const OPNAME = 'TStationData.GetPatchWithID';
begin
  Result := nil;
  try
    Result := CastPatchWithID(APatchID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.GetPatchWithIndex (AIndex : integer) : IPatchData;
const OPNAME = 'TStationData.GetPatchWithIndex';
begin
  Result := nil;
  try
    Result := CastPatchWithIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.DeletePatchWithID (APatchID : integer) : WordBool;
const OPNAME = 'TStationData.DeletePatchWithID';
var
  lPatch      : TPatchData;
  lIndex      : integer;
  lDataset    : TAbstractModelDataset;
  lSQL        : string;
  lStationID  : integer;
  lSearchKey  : string;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FPatches.Count)) do
    begin
      lPatch := TPatchData(FPatches.Items[lIndex]);
      if (APatchID = lPatch.PatchID) then
      begin
        lStationID := FRainfallData.StationID;
        FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
        try
          if (Assigned(lDataset)) then
          begin
            lDataset.DataSet.Close;
            lSQL := 'DELETE FROM RainfallCatchmentSource ' +
                    ' WHERE StationID = ' + IntToStr(lStationID) +
                    ' AND SourcePatchID = ' + IntToStr(APatchID);
            lDataset.SetSQL(lSQL);
            lDataset.ExecSQL;

            lDataset.DataSet.Close;
            lSQL := 'DELETE FROM RainfallPatchSource ' +
                    ' WHERE PatchID = ' + IntToStr(APatchID) +
                    ' AND SourceStationID = ' + IntToStr(lStationID);
            lDataset.SetSQL(lSQL);
            lDataset.ExecSQL;

            lDataset.DataSet.Close;
            lSQL := 'DELETE FROM RainfallMonthlyPatchData ' +
                    ' WHERE PatchID = ' + IntToStr(APatchID) +
                    ' AND StationID = ' + IntToStr(lStationID);
            lDataset.SetSQL(lSQL);
            lDataset.ExecSQL;

            lDataset.DataSet.Close;
            lSQL := 'DELETE FROM RainfallScaledDownPatchValues ' +
                    ' WHERE PatchID = ' + IntToStr(APatchID) +
                    ' AND StationID = ' + IntToStr(lStationID);
            lDataset.SetSQL(lSQL);
            lDataset.ExecSQL;

            lSearchKey := 'PatchID=' + IntToStr(APatchID) + '%';
            // Delete ChangeParameter
            lDataSet.DataSet.Close;
            lSQL := 'DELETE * FROM ChangeParameter WHERE KeyValues LIKE ' + QuotedStr(lSearchKey);
            lDataSet.DataSet.Close;
            lDataSet.SetSQL(lSQL);
            lDataSet.ExecSQL;

            // Delete MetaDataItem
            lDataSet.DataSet.Close;
            lSQL := 'DELETE * FROM MetaDataItem WHERE KeyValues LIKE ' + QuotedStr(lSearchKey);
            lDataSet.DataSet.Close;
            lDataSet.SetSQL(lSQL);
            lDataSet.ExecSQL;
          end;
        finally
          if Assigned(lDataset) then
            FreeAndNil(lDataset);
        end;
        FPatches.Delete(lIndex);
        Result := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.GetSplitWithIndex (AIndex : Integer): IRainfallDataSplit;
const OPNAME = 'TStationData.GetSplitWithIndex';
begin
  Result := nil;
  try
    Result := CastSplitWithIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.GetSplitForYears (AStartYear : Integer;
                                        AEndYear   : Integer): IRainfallDataSplit;
const OPNAME = 'TStationData.GetSplitForYears';
begin
  try
    Result := CastSplitForYears(AStartYear, AEndYear);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.SplitCount : Integer;
const OPNAME = 'TStationData.SplitCount';
begin
  Result := 0;
  try
    Result := FDataSplits.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.LatLong (var ALat  : WideString;
                                var ALong : WideString);
const OPNAME = 'TStationData.LatLong';
var
  lLatDeg  : integer;
  lLongDeg : integer;
  lLatMin  : integer;
  lLongMin : integer;
  lPosLat  : integer;
begin
  try
    ALat  := '';
    ALong := '';
    lPosLat  := Abs(FLatitude);
    lLatDeg  := lPosLat div 60;
    lLatMin  := lPosLat - lLatDeg * 60;
    lLongDeg := FLongitude div 60;
    lLongMin := FLongitude - lLongDeg * 60;
    if (FLatitude < 0) then
      ALat  := Format('-%2d', [lLatDeg]) + Chr(176) + Format('%2d', [lLatMin]) + Chr(39)
    else
      ALat  := Format('%2d', [lLatDeg]) + Chr(176) + Format('%2d', [lLatMin]) + Chr(39);
    ALong := Format('%2d', [lLongDeg]) + Chr(176) + Format('%2d', [lLongMin]) + Chr(39);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TStationData.IsPartOfZone : WordBool;
const OPNAME = 'TStationData.IsPartOfZone';
var
  LDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallCatchmentSource ' +
                'WHERE Model = '        + QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
                ' AND StationID = '     + IntToStr(FRainfallData.StationID);

        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount > 0) then
          Result := TRUE;
        LDataset.DataSet.Close;
      end;
    finally
      if Assigned(lDataset) then
        FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TStationData.HasACreatedPatch : WordBool;
const OPNAME = 'TStationData.HasACreatedPatch';
var
  lIndex : integer;
  lPatch : TPatchData;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FPatches.Count)) do
    begin
      lPatch := TPatchData(FPatches.Items[lIndex]);
      if (lPatch.FPatchTypeID <> 1) then {WRC patch}
        Result := TRUE
      else
        lIndex := lIndex + 1;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TStationData.MayBeDeleted : WordBool;
const OPNAME = 'TStationData.MayBeDeleted';
begin
  Result := FALSE;
  try
    Result := (NOT IsPartOfZone);
    if Result then
      Result := (NOT HasACreatedPatch);
    if Result then
      Result := (NOT IsAPatchSource);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TStationData.MayDeleteSplit (AStartYear : Integer;
                                      AEndYear   : Integer): WordBool;
const OPNAME = 'TStationData.MayDeleteSplit';
var
  LDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchSource ' +
                ' WHERE SourceStationID = '   + IntToStr(FRainfallData.StationID) +
                ' AND HydroStartYear = ' + IntToStr(AStartYear) +
                ' AND HydroEndYear = ' + IntToStr(AEndYear);
        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if (LDataset.DataSet.Eof) then
        begin
          LDataset.DataSet.Close;
          lSQL := 'SELECT * FROM RainfallCatchmentSource ' +
                  ' WHERE StationID = '   + IntToStr(FRainfallData.StationID) +
                  ' AND HydroStartYear = ' + IntToStr(AStartYear) +
                  ' AND HydroEndYear = ' + IntToStr(AEndYear);
          LDataset.SetSQL(lSQL);
          LDataset.DataSet.Open;
          if (LDataset.DataSet.Eof) then
            Result := TRUE;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      if Assigned(lDataset) then
        FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TStationData.IsAPatchSource : WOrdBool;
const OPNAME = 'TStationData.IsAPatchSource';
var
  LDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        lSQL := 'SELECT PatchID FROM RainfallPatchR ' +
                ' WHERE StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND SubArea = '         + QuotedStr(FAppModules.StudyArea.SubAreaCode);
        lSQL := 'SELECT * FROM RainfallPatchSource ' +
                ' WHERE PatchID IN (' + lSQL + ') ' +
                ' AND SourceStationID = '   + IntToStr(FRainfallData.StationID);

        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if (LDataset.DataSet.RecordCount > 0) then
          Result := TRUE;
        LDataset.DataSet.Close;
      end;
    finally
      if Assigned(lDataset) then
        FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TStationData.LoadMonthlyData;
const OPNAME = 'TStationData.LoadMonthlyData';
begin
  try
    FPatches.Clear;
    LoadMonthlyRAWData;
    LoadRAWSplits;
    if (FRainfallData.StationID < 100000) then
      LoadMonthlyWRCData;
    LoadMonthlyPatchData;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TStationData.LoadMonthlyRawData;
const OPNAME = 'TStationData.LoadMonthlyRawData';
var
  lDataset          : TAbstractModelDataset;
  lSQL              : string;
  lYearlyData       : TYearlyData;
  lYear             : integer;
  lExpectedYear     : integer;
  lMonth            : integer;
  lAllNull          : boolean;
  lIndex            : integer;
  lValue            : double;
begin
  try
    FRainfallData.FYearlyData.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.DataSet.Close;
        if (FRainfallData.StationID < 100000) then
          lSQL := 'SELECT * FROM RainfallMonthlyRAWData WHERE StationID = ' +
                  IntToStr(FRainfallData.StationID) +
                  ' ORDER BY Year'
        else
          lSQL := 'SELECT * FROM RainfallUserMonthlyData WHERE StationID = ' +
                  IntToStr(FRainfallData.StationID) +
                  ' ORDER BY Year';
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        lDataset.DataSet.First;
        lExpectedYear := 0;
        while (NOT lDataset.DataSet.EOF) do
        begin
          lYear := lDataset.DataSet.FieldByName('Year').AsInteger;
          if (lExpectedYear = 0) then
          begin
            lExpectedYear := lYear - 1;
            FRainfallData.FStartYear := lYear;
          end;
          FRainfallData.FEndYear := lYear;

          lExpectedYear := lExpectedYear + 1;
          lYearlyData   := TYearlyData.Create(FAppModules);
          lYearlyData.Initialise;
          FRainfallData.FYearlyData.Add(lYearlyData);
          lYearlyData.Year := lExpectedYear;

          if (lYear = lExpectedYear) then
          begin
            for lMonth := 1 to 12 do
            begin
              if (lDataset.DataSet.FieldByName(Format('Value%2.2d',[lMonth])).IsNull) then
                lYearlyData.FMonthlyRainfall[lMonth] := NullFloat
              else
              begin
                lValue := lDataset.DataSet.FieldByName(Format('Value%2.2d',[lMonth])).AsFloat;
                lValue := (Round(lValue * 10)) / 10;
                lYearlyData.FMonthlyRainfall[lMonth] := lValue;
              end;
              lYearlyData.FMonthlyPatchSign[lMonth] := Trim(lDataset.DataSet .FieldByName(Format('Flag%2.2d',[lMonth])).AsString);
            end;
            lDataset.DataSet.Next;
          end;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
    if (FRainfallData.FYearlyData.Count > 0) then
    begin
      lIndex := FRainfallData.FYearlyData.Count - 1;
      lAllNull := TRUE;
      while (lAllNull) and ( lIndex >= 0 ) do
      begin
        lYearlyData := TYearlyData(FRainfallData.FYearlyData.Items[lIndex]);
        lMonth := 1;
        while (lAllNull AND (lMonth <= 12)) do
        begin
          if (lYearlyData.FMonthlyRainfall[lMonth] <> NullFloat) then
            lAllNull := FALSE
          else
            lMonth := lMonth + 1;
        end;
        if (lAllNull) then
        begin
          FRainfallData.FYearlyData.Delete(lIndex);
          lIndex := FRainfallData.FYearlyData.Count - 1;
        end;
      end;
    end;
    CreateHydrologicalYears;
    FRainfallData.CalculateStats;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.CreateHydrologicalYears;
const OPNAME = 'TStationData.CreateHydrologicalYears';
var
  lMonth           : integer;
  lHeadNULL        : boolean;
  lTailNULL        : boolean;
  lPrevYear        : TYearlyData;
  lThisYear        : TYearlyData;
  lIndex           : integer;
  lHydroYear       : TYearlyData;
  lHydroStartMonth : integer;
  lHydroMonth      : integer;
  lChangeValue     : double;
  lChangeSign      : string;
  lFieldIndex      : string;
  lDataField       : string;
  lSignField       : string;
  lKeyValues       : string;
begin
  try
    lHydroStartMonth   := (FAppModules.Model.ModelData as IRainfallModelData).HydroStartMonth;

    FRainfallData.FHydroYearlyData.Clear;
    FRainfallData.FHydroStartYear := 0;
    FRainfallData.FHydroEndYear   := 0;
    lDataField := 'MonthlyRAWData';
    lSignField := 'MonthlyRAWSign';
    if (FRainfallData.FYearlyData.Count > 0) then
    begin
      lPrevYear := nil;
      lThisYear := nil;
      lIndex := 0;
      while (lIndex < FRainfallData.FYearlyData.Count) do
      begin
        lThisYear := TYearlyData(FRainfallData.FYearlyData.Items[lIndex]);
        if (lIndex = 0) then
        begin
          lMonth := 1;
          lHeadNULL := TRUE;
          { Check if rainfall for all months before start of hydrological year is NULL }
          while (lHeadNULL AND (lMonth < lHydroStartMonth)) do
          begin
            if (lThisYear.FMonthlyRainfall[lMonth] <> NullFloat) then
              lHeadNULL := FALSE
            else
            begin
              lFieldIndex := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
              lKeyValues  := GetKeyValues(lDataField, lFieldIndex);
              if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) OR
                 (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
                lHeadNULL := FALSE
              else
                lMonth := lMonth + 1;
            end;
          end;
          if (NOT lHeadNULL) then
          begin
            lHydroYear := TYearlyData.Create(FAppModules);
            lHydroYear.Initialise;
            FRainfallData.FHydroYearlyData.Add(lHydroYear);
            lHydroYear.Year := lThisYear.Year - 1;
            lHydroYear.HydroYear := IntToStr(lHydroYear.Year) + '/' + Copy(IntToStr(lThisYear.Year), 3, 2);
            for lMonth := lHydroStartMonth to 12 do
            begin
              lHydroMonth  := lMonth - (lHydroStartMonth - 1);
              lFieldIndex  := IntToStr(lThisYear.Year-1) + ',' + IntToStr(lMonth);
              lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
              if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                             (lDataField, lKeyValues,
                                              FloatToStr(NullFloat),
                                              IntToStr(lMonth)));
                lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
              end;
              if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeSign := FAppModules.Changes.GetParameterValue
                                             (lSignField, lKeyValues, '', IntToStr(lMonth));
                lHydroYear.FMonthlyPatchSign[lHydroMonth] := lChangeSign;
              end;
            end;
            for lMonth := 1 to lHydroStartMonth - 1 do
            begin
              lHydroMonth := lMonth + (12 - lHydroStartMonth + 1);
              lFieldIndex := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
              lKeyValues  := GetKeyValues(lDataField, lFieldIndex);
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lThisYear.FMonthlyRainfall[lMonth];
              lHydroYear.FMonthlyPatchSign[lHydroMonth] := lThisYear.FMonthlyPatchSign[lMonth];

{              if(lThisYear.FMonthlyPatchSign[lMonth] = '*') then
              begin
                lHydroYear.FMonthlyScaledDown[lMonth] := lThisYear.FMonthlyRainfall[lMonth];
              end;
 }
              if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                             (lDataField, lKeyValues,
                                              FloatToStr(lThisYear.FMonthlyRainfall[lMonth]),
                                              IntToStr(lMonth)));
                lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
              end;
              if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeSign := FAppModules.Changes.GetParameterValue
                                             (lSignField, lKeyValues,
                                              lThisYear.FMonthlyPatchSign[lMonth],
                                              IntToStr(lMonth));
                lHydroYear.FMonthlyPatchSign[lHydroMonth] := lChangeSign;
              end;
            end;
          end;
        end
        else
        begin
          lHydroYear := TYearlyData.Create(FAppModules);
          lHydroYear.Initialise;
          FRainfallData.FHydroYearlyData.Add(lHydroYear);
          lHydroYear.Year := lThisYear.Year - 1;
          lHydroYear.HydroYear := IntToStr(lHydroYear.Year) + '/' + Copy(IntToStr(lThisYear.Year), 3, 2);
          for lMonth := lHydroStartMonth to 12 do
          begin
            lHydroMonth  := lMonth - (lHydroStartMonth - 1);
            lFieldIndex  := IntToStr(lThisYear.Year-1) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            lHydroYear.FMonthlyRainfall[lHydroMonth]  := lPrevYear.FMonthlyRainfall[lMonth];
            lHydroYear.FMonthlyPatchSign[lHydroMonth] := lPrevYear.FMonthlyPatchSign[lMonth];

{            if(lPrevYear.FMonthlyPatchSign[lMonth] = '*') then
              lHydroYear.FMonthlyScaledDown[lMonth] := lPrevYear.FMonthlyRainfall[lMonth];
 }
            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                           (lDataField, lKeyValues,
                                            FloatToStr(lPrevYear.FMonthlyRainfall[lMonth]),
                                            IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                           (lSignField, lKeyValues,
                                            lPrevYear.FMonthlyPatchSign[lMonth],
                                            IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;
          end;
          for lMonth := 1 to lHydroStartMonth - 1 do
          begin
            lHydroMonth  := lMonth + (12 - lHydroStartMonth + 1);
            lFieldIndex  := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            lHydroYear.FMonthlyRainfall[lHydroMonth]  := lThisYear.FMonthlyRainfall[lMonth];
            lHydroYear.FMonthlyPatchSign[lHydroMonth] := lThisYear.FMonthlyPatchSign[lMonth];

{            if (lThisYear.FMonthlyPatchSign[lMonth] = '*') then
               lHydroYear.FMonthlyScaledDown[lMonth] :=  lThisYear.FMonthlyRainfall[lMonth];
 }

            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                           (lDataField, lKeyValues,
                                            FloatToStr(lThisYear.FMonthlyRainfall[lMonth]),
                                            IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                           (lSignField, lKeyValues,
                                            lThisYear.FMonthlyPatchSign[lMonth],
                                            IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;
          end;
        end;
        lPrevYear := lThisYear;
        lIndex    := lIndex + 1;
      end;
      if (lPrevYear <> nil) then
      begin
        lMonth := lHydroStartMonth;
        lTailNULL := TRUE;
        while (lTailNULL AND (lMonth <= 12)) do
        begin
          if (lPrevYear.FMonthlyRainfall[lMonth] <> NullFloat) then
            lTailNULL := FALSE
          else
          begin
            lFieldIndex  := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) OR
               (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
              lTailNULL := FALSE
            else
              lMonth := lMonth + 1;
          end;
        end;
        if (NOT lTailNULL) then
        begin
          lHydroYear := TYearlyData.Create(FAppModules);
          lHydroYear.Initialise;
          FRainfallData.FHydroYearlyData.Add(lHydroYear);
          lHydroYear.Year := lThisYear.Year;
          lHydroYear.HydroYear := IntToStr(lHydroYear.Year) + '/' + Copy(IntToStr(lThisYear.Year+1), 3, 2);
          for lMonth := lHydroStartMonth to 12 do
          begin
            lHydroMonth  := lMonth - (lHydroStartMonth - 1);
            lFieldIndex  := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            lHydroYear.FMonthlyRainfall[lHydroMonth]  := lPrevYear.FMonthlyRainfall[lMonth];
            lHydroYear.FMonthlyPatchSign[lHydroMonth] := lPrevYear.FMonthlyPatchSign[lMonth];

{            if (lPrevYear.FMonthlyPatchSign[lMonth] = '*') then
               lHydroYear.FMonthlyScaledDown[lMonth] :=  lPrevYear.FMonthlyRainfall[lMonth];
 }

            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                         (lDataField, lKeyValues,
                                          FloatToStr(lPrevYear.FMonthlyRainfall[lMonth]),
                                          IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                         (lSignField, lKeyValues,
                                          lPrevYear.FMonthlyPatchSign[lMonth],
                                          IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;
          end;
          for lMonth := 1 to lHydroStartMonth - 1 do
          begin
            lHydroMonth := lMonth + (12 - lHydroStartMonth + 1);
            lFieldIndex := IntToStr(lThisYear.Year+1) + ',' + IntToStr(lMonth);
            lKeyValues  := GetKeyValues(lDataField, lFieldIndex);
            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                         (lDataField, lKeyValues,
                                          FloatToStr(NullFloat),
                                          IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                         (lSignField, lKeyValues, '', IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth] := lChangeSign;
            end;
          end;
        end;
      end;
      if (FRainfallData.FHydroYearlyData.Count > 0) then
      begin
        lHydroYear      := TYearlyData(FRainfallData.FHydroYearlyData.Items[0]);
        FRainfallData.FHydroStartYear := lHydroYear.Year;
        lHydroYear      := TYearlyData(FRainfallData.FHydroYearlyData.Items[FRainfallData.FHydroYearlyData.Count - 1]);
        FRainfallData.FHydroEndYear   := lHydroYear.Year;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStationData.LoadRAWSplits;
const OPNAME = 'TStationData.LoadRAWSplits';
var
  lDataset    : TAbstractModelDataset;
  lSQL        : string;
  lStartYear  : integer;
  lEndYear    : integer;
begin
  try
    FDataSplits.Clear;
    CreateAndPopulateSplit(0, 0);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallRAWSplits WHERE StationID = ' +
                IntToStr(FRainfallData.StationID) +
                ' ORDER BY HydroStartYear';
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        lDataset.DataSet.First;
        while (NOT lDataset.DataSet.EOF) do
        begin
          lStartYear := lDataset.DataSet.FieldByName('HydroStartYear').AsInteger;
          lEndYear   := lDataset.DataSet.FieldByName('HydroEndYear').AsInteger;
          CreateAndPopulateSplit(lStartYear, lEndYear);
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStationData.CreateAndPopulateSplit (AStartYear : integer;
                                              AEndYear   : integer) : boolean;
const OPNAME = 'TStationData.CreateAndPopulateSplit';
var
  lSplit         : TRainfallDataSplit;
begin
  Result := FALSE;
  try
    if (AStartYear = 0) AND (AEndYear = 0) then
    begin
      lSplit := TRainfallDataSplit.Create(FAppModules);
      lSplit.Initialise;
      lSplit.HydroStartYear    := FRainfallData.HydroStartYear;
      lSplit.HydroEndYear      := FRainfallData.HydroEndYear;
      CalculateSplitStats(lSplit);
{      lSplit.NrOfMissingMonths := FRainfallData.NrOfMissingMonths;
      lSplit.FGrandTotal       := FRainfallData.GrandTotal;
      lSplit.MAP               := FRainfallData.MAP;
      lSplit.StdDeviation      := FRainfallData.StdDeviation;
      lSplit.CV                := FRainfallData.CV;}
      FDataSplits.Add(lSplit);
      Result := TRUE;
    end
    else
    if (AStartYear >= FRainfallData.HydroStartYear) AND
       (AEndYear   <= FRainfallData.HydroEndYear) then
    begin
      lSplit := TRainfallDataSplit.Create(FAppModules);
      lSplit.Initialise;
      lSplit.HydroStartYear := AStartYear;
      lSplit.HydroEndYear   := AEndYear;
      CalculateSplitStats(lSplit);
      FDataSplits.Add(lSplit);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStationData.PopulateRepeats (ASplit : TRainfallDataSplit);
const OPNAME = 'TStationData.PopulateRepeats';
var
  lYearIdx1    : integer;
  lYearIdx2    : integer;
  lMonthIdx1   : integer;
  lMonthIdx2   : integer;
  lYearlyData1 : IYearlyData;
  lYearlyData2 : IYearlyData;
  lValue1      : double;
  lValue2      : double;
  lRepeat      : boolean;
  lCount       : integer;
  lDateStr     : string;
  lMonth       : integer;
  lYear        : integer;
begin
  try
    lYearIdx1  := 0;
    lMonthIdx1 := 1;
    while (lYearIdx1 < FRainfallData.HydroYearsCount) AND (lMonthIdx1 <= 12) do
    begin
      lYearlyData1 := FRainfallData.GetHydroYearDataByIndex(lYearIdx1);
      if ((lYearlyData1.Year >= ASplit.HydroStartYear) AND (lYearlyData1.Year <= ASplit.HydroEndYear)) then
      begin
        lValue1    := lYearlyData1.MonthlyRainfall[lMonthIdx1];
        if ((lValue1 = NullFloat) OR (lValue1 = 0)) then
        begin
          lMonthIdx1 := lMonthIdx1 + 1;
          if (lMonthIdx1 > 12) then
          begin
            lMonthIdx1 := 1;
            lYearIdx1  := lYearIdx1 + 1;
          end;
        end
        else
        begin
          lYear      := lYearlyData1.Year;
          if (lMonthIdx1 <= 3) then
            lMonth   := lMonthIdx1 + 9
          else
          begin
            lMonth   := lMonthIdx1 - 3;
            lYear    := lYear + 1;
          end;
          lDateStr   := Format('%4d%2d', [lYear, lMonth]);;
          ASplit.FHighlightMonths.Add(lDateStr);
          lCount     := 1;
          lRepeat    := (lValue1 <> NullFloat) AND (lValue1 <> 0);
          lYearIdx2  := lYearIdx1;
          lMonthIdx2 := lMonthIdx1;
          while (lRepeat) do
          begin
            lMonthIdx2 := lMonthIdx2 + 1;
            if (lMonthIdx2 > 12) then
            begin
              lMonthIdx2 := 1;
              lYearIdx2  := lYearIdx2 + 1;
            end;
            lYearlyData2 := FRainfallData.GetHydroYearDataByIndex(lYearIdx2);
            if ((lYearlyData2 <> nil) AND (lYearlyData2.Year >= ASplit.HydroStartYear) AND (lYearlyData2.Year <= ASplit.HydroEndYear)) then
            begin
              lValue2 := lYearlyData2.MonthlyRainfall[lMonthIdx2];
              if (lValue1 = lValue2) then
              begin
                lCount   := lCount + 1;
                lYear    := lYearlyData2.Year;
                if (lMonthIdx2 <= 3) then
                  lMonth := lMonthIdx2 + 9
                else
                begin
                  lMonth := lMonthIdx2 - 3;
                  lYear  := lYear + 1;
                end;
                lDateStr := Format('%4d%2d', [lYear, lMonth]);
                ASplit.FHighlightMonths.Add(lDateStr);
              end
              else
                lRepeat := FALSE;
            end
            else
              lRepeat := FALSE;
          end;
          if (lCount <= 3) then
          begin
            while (lCount >= 1) do
            begin
              ASplit.FHighlightMonths.Delete(ASplit.FHighlightMonths.Count - 1);
              lCount := lCount - 1;
            end;
          end;
          lMonthIdx1 := lMonthIdx2;
          lYearIdx1  := lYearIdx2;
        end;
      end
      else
      begin
        lYearIdx1  := lYearIdx1 + 1;
        lMonthIdx1 := 1;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.PopulateHighlights (ASplit : TRainfallDataSplit);
const OPNAME = 'TStationData.PopulateHighlights';
var
  lMonth         : integer;
  lYear          : integer;
  lRainfallObj   : IRainfallModelData;
  lWetMonths     : string;
  lYearMAP       : double;
  lMonthMAP      : double;
  lYearIdx       : integer;
  lMonthIdx      : integer;
  lYearlyData    : IYearlyData;
  lValue         : double;
  lHighlight     : boolean;
  lHighlightYear : boolean;
  lDateStr       : string;
begin
  try
    ASplit.FHighlightMonths.Clear;
    ASplit.FHighlightYears.Clear;

    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lWetMonths   := lRainfallObj.WetSeasonMonths;
    if (lRainfallObj.HighlightRepeatingValues) then
      PopulateRepeats(ASplit);

    if (lRainfallObj.IncludeUnreliableData) then
      lYearMAP := ASplit.FMAP
    else
      lYearMAP := ASplit.FXMAP;
    for lYearIdx := 0 to FRainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData    := FRainfallData.GetHydroYearDataByIndex(lYearIdx);
      if ((lYearlyData.Year >= ASplit.HydroStartYear) AND (lYearlyData.Year <= ASplit.HydroEndYear)) then
      begin
        lHighlightYear := FALSE;
        { Annual value greater than twice the annual average }
        if (NOT lHighlightYear) AND (lRainfallObj.HighlightAnnualGreaterThanAverage) then
          lHighlightYear := (lYearlyData.Total > (lYearMAP * 2));
        { Annual value less than the annual average }
        if (NOT lHighlightYear) AND (lRainfallObj.HighlightAnnualLessThanAverage) then
          lHighlightYear := (lYearlyData.Total < (lYearMAP * 0.5));
        if (lHighlightYear) then
          ASplit.FHighlightYears.Add(IntToStr(lYearlyData.Year));

        for lMonthIdx := 1 to 12 do
        begin
          if (lMonthIdx <= 3) then
          begin
            lMonth := lMonthIdx + 9;
            lYear  := lYearlyData.Year;
          end
          else
          begin
            lMonth := lMonthIdx - 3;
            lYear  := lYearlyData.Year + 1;
          end;
          lDateStr := Format('%4d%2d', [lYear, lMonth]);
          if (ASplit.FHighlightMonths.IndexOf(lDateStr) < 0) then
          begin
            lHighlight := FALSE;
            lValue     := lYearlyData.MonthlyRainfall[lMonthIdx];

            { Zeros in wet months}
            if (NOT lHighlight) AND (lRainfallObj.HighlightWetSeasonZeros) then
            begin
              if (lValue = 0) then
              begin
                if (Pos(IntToStr(lMonth), lWetMonths) > 0) then
                  lHighlight := TRUE;
              end;
            end;
            { Monthly value greater than user defined proportion of monthly average }
            if (NOT lHighlight) AND (lRainfallObj.HighlightMonthlyGreaterThanProportion) then
            begin
              if (lRainfallObj.IncludeUnreliableData) then
                lMonthMAP := ASplit.FMonthMAP[lMonthIdx]
              else
                lMonthMAP := ASplit.FXMonthMAP[lMonthIdx];
              lHighlight := (lValue > (lMonthMAP * lRainfallObj.MonthlyGreaterThanProportionValue));
            end;
            { Monthly value greater than user defined value }
            if (NOT lHighlight) AND (lRainfallObj.HighlightMonthlyGreaterThanAbsolute) then
              lHighlight := (lValue > lRainfallObj.MonthlyGreaterThanAbsoluteValue);
            { Values rounded to 100mm }
            if (NOT lHighlight) AND (lRainfallObj.HighlightRoundedValues) then
            begin
              lHighlight := (lValue <> 0) AND ((Trunc(lValue * 10) mod 1000) = 0);
            end;

            if (lHighlight) then
              ASplit.FHighlightMonths.Add(lDateStr);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.CalculateSplitStats (ASplit : TRainfallDataSplit);
const OPNAME = 'TStationData.CalculateSplitStats';
var
  lYear          : integer;
  lMonth         : integer;
  lYearlyData    : TYearlyData;
  lRainfallObj   : IRainfallModelData;
  lRAWFlags      : string;
  lFlag          : string;
  lValue         : double;
  lTotal         : double;
  lXTotal        : double;
  lMonthTotal    : array [1..12] of double;
  lXMonthTotal   : array [1..12] of double;
begin
  try
    ASplit.ResetStats;
    lTotal := 0;
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRAWFlags    := lRainfallObj.RAWFlags;
    // Calculate counts, total min and max
    for lYear := 0 to FRainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData := FRainfallData.CastHydroYearDataByIndex(lYear);
      if (lYearlyData.Year >= ASplit.HydroStartYear) AND (lYearlyData.Year <= ASplit.HydroEndYear) then
      begin
        for lMonth := 1 to 12 do
        begin
          lFlag  := lYearlyData.MonthlyPatchSign[lMonth];
          lValue := lYearlyData.MonthlyRainfall[lMonth];
          if (lValue <> NullFloat) then
          begin
            ASplit.MonthTotal[lMonth] := ASplit.MonthTotal[lMonth] + lValue;
            ASplit.MonthCount[lMonth] := ASplit.MonthCount[lMonth] + 1;
            if ((ASplit.MonthMax[lMonth] = NullFloat) OR
                (lValue > ASplit.MonthMax[lMonth])) then
              ASplit.MonthMax[lMonth] := lValue;
            if ((ASplit.MonthMin[lMonth] = NullFloat) OR
                (lValue < ASplit.MonthMin[lMonth])) then
              ASplit.MonthMin[lMonth] := lValue;

            if (Pos(lFlag, lRAWFlags) <= 0) then
            begin
              ASplit.XMonthTotal[lMonth] := ASplit.XMonthTotal[lMonth] + lValue;
              ASplit.XMonthCount[lMonth] := ASplit.XMonthCount[lMonth] + 1;
              if ((ASplit.XMonthMax[lMonth] = NullFloat) OR
                  (lValue > ASplit.XMonthMax[lMonth])) then
                ASplit.XMonthMax[lMonth] := lValue;
              if ((ASplit.XMonthMin[lMonth] = NullFloat) OR
                  (lValue < ASplit.XMonthMin[lMonth])) then
                ASplit.XMonthMin[lMonth] := lValue;
            end;
          end;
        end;
        if (NOT lYearlyData.HasMissingData) then
        begin
          ASplit.GrandTotal := ASplit.GrandTotal + lYearlyData.Total;
          ASplit.YearCount  := ASplit.YearCount + 1;
          if ((ASplit.YearMax = NullFloat) OR (lYearlyData.Total > ASplit.YearMax)) then
            ASplit.YearMax := lYearlyData.Total;
          if ((ASplit.YearMin = NullFloat) OR (lYearlyData.Total < ASplit.YearMin)) then
            ASplit.YearMin := lYearlyData.Total;
          if (NOT lYearlyData.HasUnreliableData) then
          begin
            ASplit.XGrandTotal := ASplit.XGrandTotal + lYearlyData.Total;
            ASplit.XYearCount  := ASplit.XYearCount + 1;
            if ((ASplit.XYearMax = NullFloat) OR (lYearlyData.Total > ASplit.XYearMax)) then
              ASplit.XYearMax := lYearlyData.Total;
            if ((ASplit.XYearMin = NullFloat) OR (lYearlyData.Total < ASplit.XYearMin)) then
              ASplit.XYearMin := lYearlyData.Total;
          end;
        end;
        lTotal := lTotal + lYearlyData.MissingMonths;
      end;
    end;
    ASplit.NrOfMissingMonths := Trunc(lTotal);
    // Calculate MAP = mean average rainfall
    if (ASplit.FYearCount > 0) then
      ASplit.MAP := ASplit.GrandTotal / ASplit.YearCount;
    if (ASplit.FXYearCount > 0) then
      ASplit.XMAP := ASplit.XGrandTotal / ASplit.XYearCount;
    for lMonth := 1 to 12 do
    begin
      if (ASplit.MonthCount[lMonth] > 0) then
        ASplit.MonthMAP[lMonth] := ASplit.MonthTotal[lMonth]/ASplit.MonthCount[lMonth];
      if (ASplit.XMonthCount[lMonth] > 0) then
        ASplit.XMonthMAP[lMonth] := ASplit.XMonthTotal[lMonth]/ASplit.XMonthCount[lMonth];
    end;
    // Calculate standard deviation
    lTotal  := 0;
    lXTotal := 0;
    for lMonth := 1 to 12 do
    begin
      lMonthTotal[lMonth] := 0;
      lXMonthTotal[lMonth] := 0;
    end;
    for lYear := 0 to FRainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData := FRainfallData.CastHydroYearDataByIndex(lYear);
      if (lYearlyData.Year >= ASplit.HydroStartYear) AND (lYearlyData.Year <= ASplit.HydroEndYear) then
      begin
        if (NOT lYearlyData.HasMissingData) then
        begin
          lTotal := lTotal + Power(lYearlyData.Total - ASplit.MAP, 2);
          if (NOT lYearlyData.HasUnreliableData) then
            lXTotal := lXTotal + Power(lYearlyData.Total - ASplit.XMAP, 2);
        end;
        for lMonth := 1 to 12 do
        begin
          lValue := lYearlyData.MonthlyRainfall[lMonth];
          lFlag  := lYearlyData.MonthlyPatchSign[lMonth];
          if (lValue <> NullFloat) then
          begin
            lMonthTotal[lMonth] := lMonthTotal[lMonth] + Power((lValue - ASplit.MonthMAP[lMonth]), 2);
            if (Pos(lFlag, lRAWFlags) <= 0) then {not unreliable}
              lXMonthTotal[lMonth] := lXMonthTotal[lMonth] + Power((lValue - ASplit.XMonthMAP[lMonth]), 2);
          end;
        end;
      end;
    end;

    if (ASplit.YearCount > 1) then
      ASplit.StdDeviation := Sqrt(lTotal / (ASplit.YearCount - 1));
    if (ASplit.MAP > 0) then
      ASplit.CV := ASplit.StdDeviation / ASplit.MAP * 100;
    if (ASplit.XYearCount > 1) then
      ASplit.XStdDeviation := Sqrt(lXTotal / (ASplit.XYearCount - 1));
    if (ASplit.XMAP > 0) then
      ASplit.XCV := ASplit.XStdDeviation / ASplit.XMAP * 100;
    for lMonth := 1 to 12 do
    begin
      if (ASplit.MonthCount[lMonth] > 1) then
      begin
        ASplit.MonthStdDev[lMonth] := Sqrt(lMonthTotal[lMonth] / (ASplit.MonthCount[lMonth] - 1));
        if (ASplit.MonthMAP[lMonth] > 0) then
          ASplit.MonthCV[lMonth] := ASplit.MonthStdDev[lMonth] / ASplit.MonthMAP[lMonth] * 100;
      end;
      if (ASplit.XMonthCount[lMonth] > 1) then
      begin
        ASplit.XMonthStdDev[lMonth] := Sqrt(lXMonthTotal[lMonth] / (ASplit.XMonthCount[lMonth] - 1));
        if (ASplit.XMonthMAP[lMonth] > 0) then
          ASplit.XMonthCV[lMonth] := ASplit.XMonthStdDev[lMonth] / ASplit.XMonthMAP[lMonth] * 100;
      end;
    end;
    PopulateHighlights(ASplit);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStationData.GetRawDataForCalendarYear (AYear           : integer;
                                                 var ARainfall   : TMonthlyRainfall;
                                                 var APatchSigns : TMonthlyPatchSign) : boolean;
const OPNAME = 'TStationData.GetRawDataForCalendarYear';
var
  lYearlyData  : TYearlyData;
  lResultData  : TYearlyData;
  lIndex       : integer;
begin
  Result := FALSE;
  try
    for lIndex :=1 to 12 do
    begin
      ARainfall[lIndex]   := NullFloat;
      APatchSigns[lIndex] := '';
    end;
    lResultData := nil;
    lIndex := 0;
    while ((lResultData = nil) AND (lIndex < FRainfallData.FYearlyData.Count)) do
    begin
      lYearlyData := TYearlyData(FRainfallData.FYearlyData.Items[lIndex]);
      if (AYear = lYearlyData.Year) then
        lResultData := lYearlyData
      else
        lIndex := lIndex + 1;
    end;
    if (lResultData <> nil) then
    begin
      for lIndex := 1 to 12 do
      begin
        ARainfall[lIndex ]  := lResultData.FMonthlyRainfall[lIndex];
        APatchSigns[lIndex] := lResultData.FMonthlyPatchSign[lIndex];
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.LoadMonthlyWRCData;
const OPNAME = 'TStationData.LoadMonthlyWRCData';
var
  lDatasetA         : TAbstractModelDataset;
  lDatasetB         : TAbstractModelDataset;
  lSQL              : string;
  lPatchData        : TPatchData;
  lPatchID          : integer;
  lYearlyData       : TYearlyData;
  lRainfall         : TMonthlyRainfall;
  lPatchSign        : TMonthlyPatchSign;
  lYear             : integer;
  lExpectedYear     : integer;
  lMonth            : integer;
  lYearIndex        : integer;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetA);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetB);
    try
      if (Assigned(lDatasetA) AND Assigned(lDatasetB)) then
      begin
        // Load patches
        lDatasetA.DataSet.Close;
        lSQL := ' SELECT * FROM RainfallPatchWRC WHERE StationID = ' + IntToStr(FRainfallData.FStationID) +
                ' ORDER BY PatchID';
        lDatasetA.SetSQL(lSQL);
        lDatasetA.DataSet.Open;
        lDatasetA.DataSet.First;
        while (NOT lDatasetA.DataSet.EOF) do
        begin
          lPatchID   := lDatasetA.DataSet.FieldByName('PatchID').AsInteger;
          lPatchData := TPatchData.Create(FAppModules);
          lPatchData.Initialise;
          FPatches.Add(lPatchData);
          lPatchData.PatchID       := lPatchID;
          lPatchData.RainfallData.StationNumber := FRainfallData.FStationNumber;
          lPatchData.RainfallData.StationID     := FRainfallData.FStationID;
          lPatchData.PatchTypeID   := lDatasetA.DataSet.FieldByName('PatchTypeID').AsInteger;
          lPatchData.PatchName     := Trim(lDatasetA.DataSet.FieldByName('Description').AsString);

          lDatasetB.DataSet.Close;
          lSQL := 'SELECT * FROM RainfallMonthlyWRCData WHERE StationID = ' +
                  IntToStr(FRainfallData.FStationID) +
                  ' AND PatchID = ' + IntToStr(lPatchID) +
                  ' ORDER BY Year';
          lDatasetB.SetSQL(lSQL);
          lDatasetB.DataSet.Open;
          lDatasetB.DataSet.First;
          lExpectedYear := 0;
          while (NOT lDatasetB.DataSet.EOF) do
          begin
            lYear := lDatasetB.DataSet.FieldByName('Year').AsInteger;
            if (lExpectedYear = 0) then
            begin
              if (FRainfallData.FStartYear > 0) then
              begin
                for lYearIndex := FRainfallData.FStartYear to lYear-1 do
                begin
                  lYearlyData := TYearlyData.Create(FAppModules);
                  lYearlyData.Initialise;
                  lPatchData.FRainfallData.FYearlyData.Add(lYearlyData);
                  lYearlyData.Year := lYearIndex;
                  if (GetRawDataForCalendarYear(lYearIndex, lRainfall, lPatchSign)) then
                  begin
                    for lMonth := 1 to 12 do
                    begin
                      lYearlyData.FMonthlyRainfall[lMonth]  := lRainfall[lMonth];
                      lYearlyData.FMonthlyPatchSign[lMonth] := lPatchSign[lMonth];

//                      if  (lPatchSign[lMonth] = '*') then
//                       lYearlyData.FMonthlyScaledDown[lMonth] := lRainfall[lMonth];
                    end;
                  end;
                end;
              end;
              if ((FRainfallData.FStartYear > 0) AND (FRainfallData.FStartYear < lYear)) then
                lPatchData.RainfallData.StartYear := FRainfallData.FStartYear
              else
                lPatchData.RainfallData.StartYear := lYear;
              lExpectedYear := lYear - 1;
            end;
            lPatchData.RainfallData.EndYear := lYear;

            lExpectedYear    := lExpectedYear + 1;
            lYearlyData      := TYearlyData.Create(FAppModules);
            lYearlyData.Initialise;
            lPatchData.FRainfallData.FYearlyData.Add(lYearlyData);
            lYearlyData.Year := lExpectedYear;

            if (lYear = lExpectedYear) then
            begin
              for lMonth := 1 to 12 do
              begin
                if (lDatasetB.DataSet.FieldByName(Format('Value%2.2d',[lMonth])).IsNull) then
                  lYearlyData.FMonthlyRainfall[lMonth] := NullFloat
                else
                  lYearlyData.FMonthlyRainfall[lMonth] := lDatasetB.DataSet.FieldByName(Format('Value%2.2d',[lMonth])).AsFloat;
                lYearlyData.FMonthlyPatchSign[lMonth] := Trim(lDatasetB.DataSet.FieldByName(Format('Flag%2.2d',[lMonth])).AsString);
              end;
              lDatasetB.DataSet.Next;
            end
            else
            begin
              if (GetRawDataForCalendarYear(lExpectedYear, lRainfall, lPatchSign)) then
              begin
                for lMonth := 1 to 12 do
                begin
                  lYearlyData.FMonthlyRainfall[lMonth]  := lRainfall[lMonth];
                  lYearlyData.FMonthlyPatchSign[lMonth] := lPatchSign[lMonth]
                end;
              end;
            end;
          end;
          if ((lPatchData.FRainfallData.FYearlyData.Count > 0) AND (FRainfallData.FEndYear > lPatchData.RainfallData.EndYear)) then
          begin
            for lYearIndex := lPatchData.RainfallData.EndYear + 1 to FRainfallData.FEndYear do
            begin
              lYearlyData := TYearlyData.Create(FAppModules);
              lYearlyData.Initialise;
              lPatchData.FRainfallData.FYearlyData.Add(lYearlyData);
              lYearlyData.Year := lYearIndex;
              if (GetRawDataForCalendarYear(lYearIndex, lRainfall, lPatchSign)) then
              begin
                for lMonth := 1 to 12 do
                begin
                  lYearlyData.FMonthlyRainfall[lMonth]  := lRainfall[lMonth];
                  lYearlyData.FMonthlyPatchSign[lMonth] := lPatchSign[lMonth];

//                  if (lPatchSign[lMonth] = '*') then
//                    lYearlyData.FMonthlyScaledDown[lMonth] :=  lRainfall[lMonth];
                end;
              end;
            end;
            lPatchData.RainfallData.EndYear := FRainfallData.FEndYear;
          end;
          lDatasetA.DataSet.Next;
          lPatchData.CreateHydrologicalYears;
          lPatchData.FRainfallData.CalculateStats;
        end;
      end;
    finally
      lDatasetA.Free;
      lDatasetB.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.LoadMonthlyPatchData;
const OPNAME = 'TStationData.LoadMonthlyPatchData';
var
  lDatasetA         : TAbstractModelDataset;
  lDatasetB         : TAbstractModelDataset;
  lDatasetC         : TAbstractModelDataset;
  lSQL              : string;
  lPatch            : TPatchData;
  lPatchID          : integer;
  lSourceStationID  : integer;
  lSourcePatchID    : integer;
  lPatchMultiple    : boolean;
  lTargetStation    : boolean;
  lSourceList       : TStringList;
  lYearlyData       : TYearlyData;
  lRainfall         : TMonthlyRainfall;
  lPatchSign        : TMonthlyPatchSign;
  lYear             : integer;
  lExpectedYear     : integer;
  lMonth            : integer;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetA);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetB);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetC);

    lSourceList := TStringList.Create;
    try
      if (Assigned(lDatasetA) AND Assigned(lDatasetB)) then
      begin
        lDatasetA.DataSet.Close;
        lSQL := 'SELECT RainfallPatchR.* FROM RainfallPatchR, RainfallPatchSource ' +
                ' WHERE RainfallPatchR.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND RainfallPatchR.SubArea = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
                ' AND RainfallPatchSource.SourceStationID = ' + IntToStr(FRainfallData.FStationID) +
                ' AND RainfallPatchSource.PatchID = RainfallPatchR.PatchID';
        lDatasetA.SetSQL(lSQL);
        lDatasetA.DataSet.Open;
        while (NOT lDatasetA.DataSet.Eof) do
        begin
          lPatchID := lDatasetA.DataSet.FieldByName('PatchID').AsInteger;
          lPatchMultiple := (UpperCase(Trim(lDatasetA.DataSet.FieldByName('PatchMultipleStations').AsString)) = 'Y');
          lDatasetB.DataSet.Close;
          lSQL := 'SELECT * FROM RainfallPatchSource ' +
                  ' WHERE PatchID = ' + IntToStr(lPatchID);
          lDatasetB.SetSQL(lSQL);
          lDatasetB.DataSet.Open;
          lSourceList.Clear;
          lTargetStation := FALSE;
          while (NOT lDatasetB.DataSet.EOF) do
          begin
            lSourceStationID  := lDatasetB.DataSet.FieldByName('SourceStationID').AsInteger;
            lSourcePatchID    := lDatasetB.DataSet.FieldByName('SourcePatchID').AsInteger;
            lSourceList.Add(IntToStr(lSourceStationID) + ',' +
                            IntToStr(lSourcePatchID) + ',' +
                            Trim(UpperCase(lDatasetB.DataSet.FieldByName('TargetStation').AsString)) + ',' +
                            Trim(lDatasetB.DataSet.FieldByName('HydroStartYear').AsString) + ',' +
                            Trim(lDatasetB.DataSet.FieldByName('HydroEndYear').AsString) );
            if (FRainfallData.FStationID = lSourceStationID) then
              lTargetStation := Trim(UpperCase(lDatasetB.DataSet.FieldByName('TargetStation').AsString)) = 'Y';
            lDatasetB.DataSet.Next;
          end;
          if (lPatchMultiple OR lTargetStation) then
          begin
            lPatch := TPatchData.Create(FAppModules);
            lPatch.Initialise;
            FPatches.Add(lPatch);
            lPatch.PatchID       := lPatchID;
            lPatch.RainfallData.StationID     := FRainfallData.FStationID;
            lPatch.RainfallData.StationNumber := FRainfallData.FStationNumber;
            lPatch.PatchTypeID   := lDatasetA.DataSet.FieldByName('PatchTypeID').AsInteger;
            lPatch.PatchName     := Trim(lDatasetA.DataSet.FieldByName('Description').AsString);
            lPatch.SourceInfo    := lSourceList.CommaText;
            lPatch.Populate(lPatchMultiple,
                            Trim(lDatasetA.DataSet.FieldByName('ClassRInputFileName').AsString),
                            Trim(lDatasetA.DataSet.FieldByName('ClassROutputFileName').AsString),
                            Trim(lDatasetA.DataSet.FieldByName('PatchRInputFileName').AsString),
                            Trim(lDatasetA.DataSet.FieldByName('PatchRPrintedFileName').AsString),
                            Trim(lDatasetA.DataSet.FieldByName('PatchRPlottedFileName').AsString),
                            lDatasetA.DataSet.FieldByName('PatchStartYear').AsInteger,
                            lDatasetA.DataSet.FieldByName('PatchEndYear').AsInteger,
                            lDatasetA.DataSet.FieldByName('PatchChangeDate').AsDateTime,
                            lDatasetA.DataSet.FieldByName('ClassRDate').AsDateTime,
                            lDatasetA.DataSet.FieldByName('PatchRInputDate').AsDateTime,
                            lDatasetA.DataSet.FieldByName('PatchROutputDate').AsDateTime,
                            lDatasetA.DataSet.FieldByName('PatchRPrintedDate').AsDateTime,
                            lDatasetA.DataSet.FieldByName('PatchRPlottedDate').AsDateTime);

            lDatasetB.DataSet.Close;
            lSQL := 'SELECT * FROM RainfallMonthlyPatchData WHERE StationID = ' +
                    IntToStr(FRainfallData.FStationID) +
                    ' AND PatchID = ' + IntToStr(lPatchID) +
                    ' ORDER BY Year';
            lDatasetB.SetSQL(lSQL);
            lDatasetB.DataSet.Open;
            lDatasetB.DataSet.First;
            lExpectedYear := 0;
            while (NOT lDatasetB.DataSet.EOF) do
            begin
              lYear := lDatasetB.DataSet.FieldByName('Year').AsInteger;
              if (lExpectedYear = 0) then
              begin
                lExpectedYear := lYear - 1;
                lPatch.RainfallData.StartYear := lYear;
              end;
              lPatch.RainfallData.EndYear := lYear;

              lExpectedYear    := lExpectedYear + 1;
              lYearlyData      := TYearlyData.Create(FAppModules);
              lYearlyData.Initialise;
              lPatch.FRainfallData.FYearlyData.Add(lYearlyData);
              lYearlyData.Year := lExpectedYear;

              if (lYear = lExpectedYear) then
              begin
                for lMonth := 1 to 12 do
                begin
                  if (lDatasetB.DataSet.FieldByName(Format('Value%2.2d',[lMonth])).IsNull) then
                    lYearlyData.FMonthlyRainfall[lMonth] := NullFloat
                  else
                    lYearlyData.FMonthlyRainfall[lMonth] := lDatasetB.DataSet.FieldByName(Format('Value%2.2d',[lMonth])).AsFloat;
                  lYearlyData.FMonthlyPatchSign[lMonth]  := Trim(lDatasetB.DataSet.FieldByName(Format('Flag%2.2d',[lMonth])).AsString);
                end;
                lDatasetB.DataSet.Next;
              end
              else
              begin
                if (GetRawDataForCalendarYear(lExpectedYear, lRainfall, lPatchSign)) then
                begin
                  for lMonth := 1 to 12 do
                  begin
                    lYearlyData.FMonthlyRainfall[lMonth]  := lRainfall[lMonth];
                    lYearlyData.FMonthlyPatchSign[lMonth] := lPatchSign[lMonth];
                  end;
                end;
              end;
            end;
            lPatch.CreateHydrologicalYears;
            lPatch.FRainfallData.CalculateStats;
          end;
          lDatasetA.DataSet.Next;
        end;
      end;
    finally
      lDatasetA.Free;
      lDatasetB.Free;
      FreeAndNil(lSourceList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.RecalculateStatistics;
const OPNAME = 'TStationData.RecalculateStatistics';
var
  lIndex : integer;
  lSplit : TRainfallDataSplit;
  lPatch : TPatchData;
begin
  try
    FRainfallData.CalculateStats;
    for lIndex := 0 to FDataSplits.Count - 1 do
    begin
      lSplit := CastSplitWithIndex(lIndex);
      CalculateSplitStats(lSplit);
    end;
    for lIndex := 0 to FPatches.Count - 1 do
    begin
      lPatch := CastPatchWithIndex(lIndex);
      lPatch.FRainfallData.CalculateStats;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.RepopulateHighlights;
const OPNAME = 'TStationData.RepopulateHighlights';
var
  lIndex : integer;
  lSplit : TRainfallDataSplit;
  lPatch : TPatchData;
begin
  try
    FRainfallData.PopulateHighlights;
    for lIndex := 0 to FDataSplits.Count - 1 do
    begin
      lSplit := CastSplitWithIndex(lIndex);
      PopulateHighlights(lSplit);
    end;
    for lIndex := 0 to FPatches.Count - 1 do
    begin
      lPatch := CastPatchWithIndex(lIndex);
      lPatch.FRainfallData.PopulateHighlights;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.PatchCount : integer;
const OPNAME = 'TStationData.PatchCount';
begin
  Result := 0;
  try
    Result := FPatches.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.CastPatchWithIndex (AIndex : integer) : TPatchData;
const OPNAME = 'TStationData.CastPatchWithIndex';
begin
  Result := nil;
  try
    if (AIndex < FPatches.Count) then
      Result := TPatchData(FPatches.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.CastSplitWithIndex (AIndex : Integer): TRainfallDataSplit;
const OPNAME = 'TStationData.CastSplitWithIndex';
begin
  Result := nil;
  try
    if (AIndex < FDataSplits.Count) then
      Result := TRainfallDataSplit(FDataSplits.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.CastSplitForYears (AStartYear : Integer;
                                         AEndYear   : Integer): TRainfallDataSplit;
const OPNAME = 'TStationData.CastSplitForYears';
var
  lSplit : TRainfallDataSplit;
  lIndex : integer;
begin
  Result := nil;
  try
    lIndex := 0;
    if (AStartYear = 0) AND (AEndYear = 0) AND (FDataSplits.Count > 0) then
      Result := TRainfallDataSplit(FDataSplits.Items[0])
    else
    begin
      while ((Result = nil) AND (lIndex < FDataSplits.Count)) do
      begin
        lSplit := TRainfallDataSplit(FDataSplits.Items[lIndex]);
        if (AStartYear = lSplit.HydroStartYear) AND (AEndYear = lSplit.HydroEndYear) then
          Result := lSplit
        else
          lIndex := lIndex + 1;
      end;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.DeleteSplitForYears (AStartYear : Integer;
                                           AEndYear   : Integer) : boolean;
const OPNAME = 'TStationData.DeleteSplitForYears';
var
  lSplit : TRainfallDataSplit;
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := 0;
    while ((NOT Result) AND (lIndex < FDataSplits.Count)) do
    begin
      lSplit := TRainfallDataSplit(FDataSplits.Items[lIndex]);
      if (AStartYear = lSplit.HydroStartYear) AND (AEndYear = lSplit.HydroEndYear) then
      begin
        FDataSplits.Delete(lIndex);
        Result := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.UpdateSplitForYears (AOldStartYear,ANewStartYear : Integer;
                                           AOldEndYear,ANewEndYear   : Integer) : boolean;
const OPNAME = 'TStationData.UpdateSplitForYears';
var
  LSplit : TRainfallDataSplit;
  LIndex : integer;
begin
  Result := False;
  try
    LIndex := 0;
    while ((not Result) and (LIndex < FDataSplits.Count)) do
    begin
      LSplit := TRainfallDataSplit(FDataSplits.Items[LIndex]);
      if (AOldStartYear = LSplit.HydroStartYear) AND (AOldEndYear = LSplit.HydroEndYear) then
      begin
        LSplit.HydroStartYear := ANewStartYear;
        LSplit.HydroEndYear := ANewEndYear;
        Result := True;
      end
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TStationData.GetKeyValues (const AParamField : WideString;
                                    const AFieldIndex : WideString): WideString;
const OPNAME = 'TStationData.GetKeyValues';
var
  lYear : integer;
begin
  Result := '';
  try
    lYear := StrToInt(Copy(AFieldIndex, 1, 4));
    if (AParamField = 'MonthlyRAWData') OR
       (AParamField = 'MonthlyRAWSign') then
      Result := 'StationID=' + IntToStr(FRainfallData.FStationID) +
                ',Year='     + IntToStr(lYear);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStationData.SaveRAWFile (AStartYear       : integer;
                                    AEndYear         : integer;
                                    const ADirectory : WideString);
const OPNAME = 'TStationData.SaveRAWFile';
var
  lRawData      : TStringList;
  lRAWFile      : TFileStream;
  lStationName  : string;
begin
  try
    lRawData := TStringList.Create;
    try
      if (GetRAWData(AStartYear, AEndYear, lRawData)) then
      begin
        try
          lStationName := FRainfallData.StationNumber;
          while (Pos(' ', lStationName) > 0) do
            Delete(lStationName, Pos(' ', lStationName), 1);
          lStationName:= Copy(lStationName, 1, 8);

          lRAWFile := TFileStream.Create(ADirectory + lStationName + '.RAW', fmCreate );
          lRawData.SaveToStream(lRAWFile);
        finally
          FreeAndNil(lRAWFile);
        end;
      end;
    finally
      FreeAndNil(lRawData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStationData.GetRAWData (AStartYear : integer;
                                  AEndYear   : integer;
                                  ARawData   : TStringList): boolean;
const OPNAME = 'TStationData.GetRawData';
var
  lIndex            : integer;
  lYearIndex        : integer;
  lRawLine          : string;
  lValue            : integer;
  lFlag             : string;
  lStationName      : string;
  lStartYear        : integer;
  lEndYear          : integer;
  lYearlyData       : IYearlyData;
  lRainfallObj      : IRainfallModelData;
  lRawFlags         : TStringList;
  lWarning          : string;
  lYear             : integer;
  lMonth            : integer;
  lGrandTotal       : integer;
  lMAP              : integer;
  lYearCount        : integer;
begin
  Result := FALSE;
  try
    ARawData.Clear;
    lWarning := '';
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRawFlags    := TStringList.Create;
    try
      lRawFlags.CommaText := lRainfallObj.RAWFlags;
      lStationName := FRainfallData.StationNumber;
      while (Pos(' ', lStationName) > 0) do
        Delete(lStationName, Pos(' ', lStationName), 1);
      lStationName:= Copy(lStationName, 1, 8);

      if (AStartYear = 0) AND (AEndYear = 0) then
      begin
        lStartYear := FRainfallData.HydroStartYear;
        lEndYear   := FRainfallData.HydroEndYear;
      end
      else
      begin
        lStartYear := AStartYear;
        lEndYear   := AEndYear;
      end;
      if (lStartYear < 1900) then
        lStartYear := 1900;
      lGrandTotal := 0;
      lMAP        := 0;
      lYearCount  := 0;
      for lYearIndex := 0 to FRainfallData.HydroYearsCount - 1 do
      begin
        lYearlyData := FRainfallData.GetHydroYearDataByIndex(lYearIndex);
        // New version of PatchR seems to fail if RAW files contains data before 1900
        if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
        begin
          lYearCount := lYearCount + 1;
          lRawLine := Format('%8s%5d ', [lStationName, lYearlyData.Year]);
          for lIndex := 1 to 12 do
          begin
            if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
              lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
            else
              lValue := 0;
            lFlag    := Trim(lYearlyData.MonthlyPatchSign[lIndex]);
            if (lRawFlags.IndexOf(lFlag) >= 0) then
              lFlag := '+'
            else
              lFlag := '';
            lRawLine := lRawLine + Format('%5d%1s', [lValue, lFlag]);

            if (lValue >= 999999) then
            begin
              lYear := lYearlyData.Year;
              if (lIndex < 4) then
                lMonth := lIndex + 9
              else
              begin
                lMonth := lIndex - 3;
                lYear  := lYear + 1;
              end;
              lWarning := lWarning + #13#10 + IntToStr(lYear) + ' ' + FormatSettings.ShortMonthNames[lMonth];
            end;
          end;
          lGrandTotal := lGrandTotal + Round(lYearlyData.Total * 10);
          ARawData.Add(lRawLine);
        end;
      end;
    finally
      FreeAndNil(lRawFlags);
    end;
    if (lYearCount > 0) then
      lMAP := Trunc(lGrandTotal/lYearCount);
    ARawData.Insert(0, Format(FAppModules.Language.GetString('Rainfall.FormatedString'),
                    [lStationName, lStartYear, lEndYear, lMAP,' L']));

    if (lWarning <> '') then
    begin
      lWarning := FAppModules.Language.GetString('Rainfall.WarningString') +
                  FRainfallData.StationNumber + FAppModules.Language.GetString('Rainfall.On') + lWarning +
                  '.' + #13#10 + FAppModules.Language.GetString('Rainfall.NotReadingCorrect') +
                  FAppModules.Language.GetString('Rainfall.Exceed4Characters');
      MessageDlg(lWarning, mtWarning, [mbOK], 0);
    end;

    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.PopulateCommonBlockRAWData (AStartYear  : integer;
                                                  AEndYear    : integer;
                                                  AGaugeIndex : Integer;
                                                  AAddIndex   : Integer;
                                                  var ABEGY   : Integer;
                                                  var AENDY   : Integer): WordBool;
const OPNAME = 'TStationData.PopulateCommonBlockRAWData';
var
  lIndex            : integer;
  LYear             : integer;
  lValue            : integer;
  lFlag             : string;
  lStationName      : string;
  lStartYear        : integer;
  lEndYear          : integer;
  lYearlyData       : IYearlyData;
  lRainfallObj      : IRainfallModelData;
  lRawFlags         : TStringList;
  LREFY             : Integer;
  LYearIndex        : Integer;
  LWarning          : string;
  LMonth            : Integer;
  LYearNo           : Integer;
begin
  Result := FALSE;
  try
    LWarning := '';
    LREFY := 1899;
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRawFlags    := TStringList.Create;
    try
      lRawFlags.CommaText := lRainfallObj.RAWFlags;
      lStationName := FRainfallData.StationNumber;
      while (Pos(' ', lStationName) > 0) do
        Delete(lStationName, Pos(' ', lStationName), 1);
      lStationName:= Copy(lStationName, 1, 8);
      for lIndex := 1 to 8 do
//        GFCB.RCOM02.LABELN[AGaugeIndex, LIndex] := lStationName[LIndex];
        GFCB.RCOM02.NGAUGE[AGaugeIndex, LIndex] := lStationName[LIndex];
      for lIndex := 3 to 8 do
        GFCB.RCOM02.LABELN[AGaugeIndex, LIndex] := lStationName[LIndex];
      if (AStartYear = 0) AND (AEndYear = 0) then
      begin
        lStartYear := FRainfallData.HydroStartYear;
        lEndYear   := FRainfallData.HydroEndYear;
      end
      else
      begin
        lStartYear := AStartYear;
        lEndYear   := AEndYear;
      end;
      if (lStartYear < 1900) then
        lStartYear := 1900;
      ABEGY := MIN(MAX(LREFY, lStartYear), ABEGY);
      AENDY := MAX(lEndYear, AENDY);

      for lYear := 0 to FRainfallData.HydroYearsCount - 1 do
      begin
        lYearlyData := FRainfallData.GetHydroYearDataByIndex(lYear);
        LYearIndex  := lYearlyData.Year - LREFY + AAddIndex;
        // New version of PatchR seems to fail if RAW files contains data before 1900
        if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
        begin
          for lIndex := 1 to 12 do
          begin
            if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
              lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
            else
              lValue := 0;

            if (lValue >= 9999) then
            begin
              LYearNo := lYearlyData.Year;
              if (lIndex < 4) then
                lMonth := lIndex + 9
              else
              begin
                lMonth := lIndex - 3;
                LYearNo  := LYearNo + 1;
              end;
              lWarning := lWarning + #13#10 + IntToStr(LYearNo) + ' ' + FormatSettings.ShortMonthNames[lMonth];
            end;

            lFlag    := Trim(lYearlyData.MonthlyPatchSign[lIndex]);
            if (lRawFlags.IndexOf(lFlag) >= 0) then
              lFlag := '+'
            else
              lFlag := ' ';
            GFCB.RCOM03.IDATA[lIndex, AGaugeIndex, LYearIndex] := lValue;
            GFCB.RCOM04.JCODE[lIndex, AGaugeIndex, LYearIndex] := lFlag[1];
          end;
        end;
      end;
    finally
      FreeAndNil(lRawFlags);
    end;
    if (LWarning <> '') then
    begin
      lWarning := FAppModules.Language.GetString('Rainfall.WarningString') +
                  FRainfallData.StationNumber + FAppModules.Language.GetString('Rainfall.On') + lWarning +
                  '.' + #13#10 + FAppModules.Language.GetString('Rainfall.NotReadingCorrect') +
                  FAppModules.Language.GetString('Rainfall.Exceed4Characters');
      MessageDlg(lWarning, mtWarning, [mbOK], 0);
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStationData.SaveMPFile (AStartYear       : integer;
                                   AEndYear         : integer;
                                   const ADirectory : WideString);
const OPNAME = 'TStationData.SaveMPFile';
var
  lMPData      : TStringList;
  lMPFile      : TFileStream;
  lStationName : string;
begin
  try
    lMPData  := TStringList.Create;
    try
      if (GetMPData(AStartYear, AEndYear, lMPData)) then
      begin
        try
          lStationName := RainfallData.StationNumber;
          while (Pos(' ', lStationName) > 0) do
            Delete(lStationName, Pos(' ', lStationName), 1);
          lStationName:= Copy(lStationName, 1, 8);

          lMPFile  := TFileStream.Create(ADirectory + lStationName + '.MP', fmCreate );
          lMPData.SaveToStream(lMPFile);
        finally
          FreeAndNil(lMPFile);
        end;
      end;
    finally
      FreeAndNil(lMPData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStationData.GetMPData (AStartYear : integer;
                                 AEndYear   : integer;
                                 AMPData    : TStringList): boolean;
const OPNAME = 'TStationData.GetMPData';
var
  lIndex            : integer;
  lYearIndex        : integer;
  lBlockNo          : string;
  lPositionInBlock  : string;
  lMPLine           : string;
  lValue            : integer;
  lStationName      : string;
  lYearlyData       : IYearlyData;
  lStationNumber    : string;
  lGrandTotal       : integer;
  lMAP              : integer;
  lYearCount        : integer;
  lStartYear        : integer;
  lEndYear          : integer;
begin
  Result := FALSE;
  try
    AMPData.Clear;
    lStationNumber := RainfallData.StationNumber;
    lStationName   := lStationNumber;
    while (Pos(' ', lStationName) > 0) do
      Delete(lStationName, Pos(' ', lStationName), 1);
    lStationName := Copy(lStationName, 1, 8);
    lBlockNo         := Copy(lStationNumber, 1, 4);
    lPositionInBlock := Copy(lStationNumber, 5, 3);
    while CharInSet(lPositionInBlock[Length(lPositionInBlock) - 1] , [ 'A'..'Z' ]) do
      delete(lPositionInBlock, Length(lPositionInBlock) - 1, 1);

    if (AStartYear = 0) AND (AEndYear = 0) then
    begin
      lStartYear := FRainfallData.HydroStartYear;
      lEndYear   := FRainfallData.HydroEndYear;
    end
    else
    begin
      lStartYear := AStartYear;
      lEndYear   := AEndYear;
    end;

    lGrandTotal := 0;
    lMAP        := 0;
    lYearCount  := 0;
    for lYearIndex := 0 to RainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData := RainfallData.GetHydroYearDataByIndex(lYearIndex);
      if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
      begin
        lYearCount := lYearCount + 1;
        lMPLine  := Format('%7s%6d ', [lBlockNo + lPositionInBlock, lYearlyData.Year]);
        for lIndex := 1 to 12 do
        begin
          if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
            lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
          else
            lValue := 0;
          lMPLine  := lMPLine + Format('%4d ', [lValue]);
        end;
        lGrandTotal := lGrandTotal + Round(lYearlyData.Total * 10);
        AMPData.Add(lMPLine);
      end;
    end;
    if (lYearCount > 0) then
      lMAP := Trunc(lGrandTotal/lYearCount);
    AMPData.Insert(0, Format( ' %4s  %3s%5d%5d%7d%1s',
                    [lBlockNo, lPositionInBlock, lStartYear, lEndYear, lMAP, '.' ] )  );
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStationData.RainfallData : IRainfallData;
const OPNAME = 'TStationData.RainfallData';
begin
  Result := nil;
  try
    Result := FRainfallData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{******************************************************************************}
{* TPatchData                                                                 *}
{******************************************************************************}

procedure TPatchData.CreateMemberObjects;
const OPNAME = 'TPatchData.CreateMemberObjects';
begin
  inherited;
  try
    FRainfallData := TRainfallData.Create(FAppModules);
    FSources      := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.DestroyMemberObjects;
const OPNAME = 'TPatchData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSources);
    FreeAndNil(FRainfallData);
    inherited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData._AddRef: Integer;
const OPNAME = 'TPatchData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPatchData._Release: Integer;
const OPNAME = 'TPatchData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPatchData.Initialise : boolean;
const OPNAME = 'TPatchData.Initialise';
begin
  Result := FALSE;
  try
    inherited Initialise;
    FPatchID              := 0;
    FPatchTypeID          := 0;
    FPatchName            := '';
    FPatchMultiple        := FALSE;
    FChangeDate           := 0;
    FClassRDate           := 0;
    FPatchRInputDate      := 0;
    FPatchROutputDate     := 0;
    FPatchRPrintDate      := 0;
    FPatchRPlotDate       := 0;
    FClassRInputFileName  := '';
    FClassROutputFileName := '';
    FPatchRInputFileName  := '';
    FPatchRPrintFileName  := '';
    FPatchRPlotFileName   := '';
    FSources.Clear;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchID: Integer;
const OPNAME = 'TPatchData.Get_PatchID';
begin
  Result := 0;
  try
    Result := FPatchID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchID(Value: Integer);
const OPNAME = 'TPatchData.Set_PatchID';
begin
  try
    FPatchID := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchTypeID: Integer;
const OPNAME = 'TPatchData.Get_PatchTypeID';
begin
  Result := 0;
  try
    Result := FPatchTypeID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchTypeID(Value: Integer);
const OPNAME = 'TPatchData.Set_PatchTypeID';
begin
  try
    FPatchTypeID := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchName: WideString;
const OPNAME = 'TPatchData.Get_PatchName';
begin
  Result := '';
  try
    Result := FPatchName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchName(const Value: WideString);
const OPNAME = 'TPatchData.Set_PatchName';
begin
  try
    FPatchName := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchMultiple: WordBool;
const OPNAME = 'TPatchData.Get_PatchMultiple';
begin
  Result := FALSE;
  try
    Result := FPatchMultiple;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchMultiple (Value : WordBool);
const OPNAME = 'TPatchData.Set_PatchMultiple';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        if (Value) then
          lSQL := 'UPDATE RainfallPatchR SET PatchMultipleStations = ' + QuotedStr('Y')
        else
          lSQL := 'UPDATE RainfallPatchR SET PatchMultipleStations = ' + QuotedStr('N');
        lSQL := lSQL + ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FPatchMultiple := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_ClassRInputFileName: WideString;
const OPNAME = 'TPatchData.Get_ClassRInputFileName';
begin
  Result := '';
  try
    Result := FClassRInputFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_ClassRInputFileName (const Value: WideString);
const OPNAME = 'TPatchData.Set_ClassRInputFileName';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET ClassRInputFileName = ' + QuotedStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FClassRInputFileName := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_ClassROutputFileName: WideString;
const OPNAME = 'TPatchData.Get_ClassROutputFileName';
begin
  Result := '';
  try
    Result := FClassROutputFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_ClassROutputFileName(const Value: WideString);
const OPNAME = 'TPatchData.Set_ClassROutputFileName';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET ClassROutputFileName = ' + QuotedStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FClassROutputFileName := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchRInputFileName: WideString;
const OPNAME = 'TPatchData.Get_PatchRInputFileName';
begin
  Result := '';
  try
    Result := FPatchRInputFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRInputFileName(const Value: WideString);
const OPNAME = 'TPatchData.Set_PatchRInputFileName';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET PatchRInputFileName = ' + QuotedStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FPatchRInputFileName := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchRPrintFileName: WideString;
const OPNAME = 'TPatchData.Get_PatchRPrintFileName';
begin
  Result := '';
  try
    Result := FPatchRPrintFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRPrintFileName(const Value: WideString);
const OPNAME = 'TPatchData.Set_PatchRPrintFileName';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET PatchRPrintedFileName = ' + QuotedStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FPatchRPrintFileName := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchRPlotFileName: WideString;
const OPNAME = 'TPatchData.Get_PatchRPlotFileName';
begin
  Result := '';
  try
    Result := FPatchRPlotFileName;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRPlotFileName(const Value: WideString);
const OPNAME = 'TPatchData.Set_PatchRPlotFileName';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET PatchRPlottedFileName = ' + QuotedStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FPatchRPlotFileName := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchStartYear: Integer;
const OPNAME = 'TPatchData.Get_PatchStartYear';
begin
  Result := 0;
  try
    Result := FPatchStartYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchStartYear(Value: Integer);
const OPNAME = 'TPatchData.Set_PatchStartYear';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET PatchStartYear = ' + IntToStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FPatchStartYear := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchEndYear: Integer;
const OPNAME = 'TPatchData.Get_PatchEndYear';
begin
  Result := 0;
  try
    Result := FPatchEndYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchEndYear(Value: Integer);
const OPNAME = 'TPatchData.Set_PatchEndYear';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR ' +
                'SET PatchEndYear = ' + IntToStr(Value) +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FPatchEndYear := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_ClassRInputData: WideString;
const OPNAME = 'TPatchData.Get_ClassRInputData';
var
//  lData : WideString;
  LData : String;
begin
  Result := '';
  try
    GetCompressData(lData, 'ClassRInput');
    Result := lData;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.Set_ClassRInputData(const Value: WideString);
const OPNAME = 'TPatchData.Set_ClassRInputData';
begin
  try
    SetCompressData(Value, 'ClassRInput');
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPatchData.Get_ClassROutputData: WideString;
const OPNAME = 'TPatchData.Get_ClassROutputData';
var
//  lData : WideString;
  LData : String;
begin
  Result := '';
  try
    GetCompressData(lData, 'ClassROutput');	// RianaRain
    Result := lData;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.Set_ClassROutputData(const Value: WideString);
const OPNAME = 'TPatchData.Set_ClassROutputData';
begin
  try
    SetCompressData(Value, 'ClassROutput');	//RianaRain
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPatchData.Get_PatchRPrintData: WideString;
const OPNAME = 'TPatchData.Get_PatchRPrintData';
var
//  lData : WideString;
  LData : String;
begin
  Result := '';
  try
    GetCompressData(lData, 'PatchRPrinted');
    Result := lData;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.Set_PatchRPrintData(const Value: WideString);
const OPNAME = 'TPatchData.Set_PatchRPrintData';
begin
  try
    SetCompressData(Value, 'PatchRPrinted');
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPatchData.Get_PatchRPlotData: WideString;
const OPNAME = 'TPatchData.Get_PatchRPlotData';
var
//  lData : WideString;
  LData : String;
begin
  Result := '';
  try
    GetCompressData(lData, 'PatchRPlotted');
    Result := lData;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.Set_PatchRPlotData(const Value: WideString);
const OPNAME = 'TPatchData.Set_PatchRPlotData';
begin
  try
    SetCompressData(Value, 'PatchRPlotted');
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPatchData.Get_ChangeDate: TDateTime;
const OPNAME = 'TPatchData.Get_ChangeDate';
begin
  try
    Result := FChangeDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_ChangeDate(Value: TDateTime);
const OPNAME = 'TPatchData.Set_ChangeDate';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR SET PatchChangeDate = :ADate ' +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['ADate'], [DateTimeToStr(Value)]);
        lDataset.ExecSQL;
        FChangeDate := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_ClassRDate: TDateTime;
const OPNAME = 'TPatchData.Get_ClassRDate';
begin
  try
    Result := FClassRDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_ClassRDate(Value: TDateTime);
const OPNAME = 'TPatchData.Set_ClassRDate';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR SET ClassRDate = :ADate ' +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['ADate'], [DateTimeToStr(Value)]);
        lDataset.ExecSQL;
        FClassRDate := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchRInputDate: TDateTime;
const OPNAME = 'TPatchData.Get_PatchRInputDate';
begin
  try
    Result := FPatchRInputDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRInputDate(Value: TDateTime);
const OPNAME = 'TPatchData.Set_PatchRInputDate';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR SET PatchRInputDate = :ADate ' +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['ADate'], [DateTimeToStr(Value)]);
        lDataset.ExecSQL;
        FPatchRInputDate := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchROutputDate: TDateTime;
const OPNAME = 'TPatchData.Get_PatchROutputDate';
begin
  try
    Result := FPatchROutputDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchROutputDate(Value: TDateTime);
const OPNAME = 'TPatchData.Set_PatchROutputDate';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR SET PatchROutputDate = :ADate ' +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['ADate'], [DateTimeToStr(Value)]);
        lDataset.ExecSQL;
        FPatchROutputDate := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchRPrintDate: TDateTime;
const OPNAME = 'TPatchData.Get_PatchRPrintDate';
begin
  try
    Result := FPatchRPrintDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRPrintDate(Value: TDateTime);
const OPNAME = 'TPatchData.Set_PatchRPrintDate';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR SET PatchRPrintedDate = :ADate ' +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['ADate'], [DateTimeToStr(Value)]);
        lDataset.ExecSQL;
        FPatchRPrintDate := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchRPlotDate: TDateTime;
const OPNAME = 'TPatchData.Get_PatchRPlotDate';
begin
  try
    Result := FPatchRPlotDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRPlotDate(Value: TDateTime);
const OPNAME = 'TPatchData.Set_PatchRPlotDate';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallPatchR SET PatchRPlottedDate = :ADate ' +
                ' WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['ADate'], [DateTimeToStr(Value)]);
        lDataset.ExecSQL;
        FPatchRPlotDate := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Set_PatchRInputData (const Value : WideString);
const OPNAME = 'TPatchData.Set_PatchRInputData';
begin
  try
    SetCompressData(Value, 'PatchRInput');
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPatchData.Get_PatchRInputData : WideString;
const OPNAME = 'TPatchData.Get_PatchRInputData';
var
//  lData : WideString;
  LData : String;
begin
  Result := '';
  try
    GetCompressData(lData, 'PatchRInput');
    Result := lData;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.AddSource (AStationID : integer;
                                APatchID   : integer;
                                ATarget    : boolean;
                                AStartYear : integer;
                                AEndYear   : integer);
const OPNAME = 'TPatchData.AddSource';
var
  lTempStr : string;
begin
  try
    lTempStr := IntToStr(AStationID) + ',' + IntToStr(APatchID);
    if (ATarget) then
      lTempStr := lTempStr + ',Y,'
    else
      lTempStr := lTempStr + ',N,';
    lTempStr := lTempStr + IntToStr(AStartYear) + ',' + IntToStr(AEndYear);
    FSources.Add(lTempStr);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.RemoveSource (AStationID : integer);
const OPNAME = 'TPatchData.RemoveSource';
var
  lSourceStr : string;
  lIndex     : integer;
  lDone      : boolean;
begin
  try
    lSourceStr := IntToStr(AStationID);
    lDone      := FALSE;
    lIndex     := 0;
    while ((NOT lDone) AND (lIndex < FSources.Count)) do
    begin
      if (Pos(lSourceStr, FSources.Strings[lIndex]) > 0) then
      begin
        FSources.Delete(lIndex);
        lDone := TRUE;
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.GetSourceInfoByIndex (AIndex         : integer;
                                           out AStationID : integer;
                                           out APatchID   : integer;
                                           out ATarget    : WideString;
                                           out AStartYear : integer;
                                           out AEndYear   : integer);
const OPNAME = 'TPatchData.GetSourceInfoByIndex';
var
  lSourceStr : string;
  lPos       : integer;
begin
  try
    AStationID := 0;
    APatchID   := 0;
    if (AIndex < FSources.Count) then
    begin
      lSourceStr := FSources.Strings[AIndex];
      lPos       := Pos(',', lSourceStr);
      AStationID := StrToInt(Copy(lSourceStr, 1, lPos - 1));
      lSourceStr := Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos);
      lPos       := Pos(',', lSourceStr);
      APatchID   := StrToInt(Copy(lSourceStr, 1, lPos - 1));
      lSourceStr := Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos);
      lPos       := Pos(',', lSourceStr);
      ATarget    := Copy(lSourceStr, 1, lPos - 1);
      lSourceStr := Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos);
      lPos       := Pos(',', lSourceStr);
      AStartYear := StrToInt(Copy(lSourceStr, 1, lPos - 1));
      AEndYear   := StrToInt(Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.GetSourceInfoByStationID (AStationID     : integer;
                                               out APatchID   : integer;
                                               out ATarget    : WideString;
                                               out AStartYear : integer;
                                               out AEndYear   : integer);
const OPNAME = 'TPatchData.GetSourceInfoByStationID';
var
  lSourceStr : string;
  lPos       : integer;
  lIndex     : integer;
  lStationID : integer;
  lFound     : boolean;
begin
  try
    APatchID   := 0;
    lIndex     := 0;
    lFound     := FALSE;
    while ((lIndex < FSources.Count) AND (NOT lFound)) do
    begin
      lSourceStr := FSources.Strings[lIndex];
      lPos       := Pos(',', lSourceStr);
      lStationID := StrToInt(Copy(lSourceStr, 1, lPos - 1));
      if (lStationID = AStationID) then
      begin
        lFound := TRUE;
        lSourceStr := Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos);
        lPos       := Pos(',', lSourceStr);
        APatchID   := StrToInt(Copy(lSourceStr, 1, lPos - 1));
        lSourceStr := Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos);
        lPos       := Pos(',', lSourceStr);
        ATarget    := Copy(lSourceStr, 1, lPos - 1);
        lSourceStr := Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos);
        lPos       := Pos(',', lSourceStr);
        AStartYear := StrToInt(Copy(lSourceStr, 1, lPos - 1));
        AEndYear   := StrToInt(Copy(lSourceStr, lPos + 1, Length(lSourceStr) - lPos));
      end
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.Populate (APatchMultiple         : boolean;
                               AClassRInputFileName   : string;
                               AClassROutputFileName  : string;
                               APatchRInputFileName   : string;
                               APatchRPrintFileName   : string;
                               APatchRPlotFileName    : string;
                               APatchStartYear        : integer;
                               APatchEndYear          : integer;
                               AChangedDate           : TDateTime;
                               AClassRDate            : TDateTime;
                               APatchRInputDate       : TDateTime;
                               APatchROutputDate      : TDateTime;
                               APatchRPrintDate       : TDateTime;
                               APatchRPlotDate        : TDateTime);
const OPNAME = 'TPatchData.Populate';
begin
  try
    FPatchMultiple           := APatchMultiple;
    FClassRInputFileName     := AClassRInputFileName;
    FClassROutputFileName    := AClassROutputFileName;
    FPatchRInputFileName     := APatchRInputFileName;
    FPatchRPrintFileName     := APatchRPrintFileName;
    FPatchRPlotFileName      := APatchRPlotFileName;
    FPatchStartYear          := APatchStartYear;
    FPatchEndYear            := APatchEndYear;
    FChangeDate              := AChangedDate;
    FClassRDate              := AClassRDate;
    FPatchRInputDate         := APatchRInputDate;
    FPatchROutputDate        := APatchROutputDate;
    FPatchRPrintDate         := APatchRPrintDate;
    FPatchRPlotDate          := APatchRPlotDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TPatchData.Get_PatchROutputData : WideString;
const OPNAME = 'TPatchData.Get_PatchROutputData';
var
  lDataList : TStringList;
begin
  Result := '';
  try
    lDataList := TStringList.Create;
    try
      GetPATData(0, 0, lDataList);
      Result := lDataList.CommaText;
    finally
      FreeAndNil(lDataList);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.SavePatchROutputData (AStationID   : Integer;
                                           const AValue : WideString);
const OPNAME = 'TPatchData.SavePatchROutputData';
var
  lIndex    : integer;
  lDataList : TStringList;
begin
  try
    lDataList := TStringList.Create;
    try
      lDataList.Text := AValue;
      if (lDataList.Count > 5) then
      begin
        for lIndex := 1 to 5 do
          lDataList.Delete(0);
      end;
      SaveMonthlyPatchData(AStationID, lDataList);
    finally
      FreeAndNil(lDataList);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.DeleteMonthlyPatchData;
const OPNAME = 'TPatchData.DeleteMonthlyPatchData';
var
  LDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        lSQL := 'DELETE * FROM RainfallMonthlyPatchData ' +
                'WHERE PatchID = ' + IntToStr(FPatchID);
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;
      end;
    finally
      FreeAndNil(LDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.SaveMonthlyPatchData  (AStationID : integer;
                                           AValue     : TStringlist);
const OPNAME = 'TPatchData.SaveMonthlyPatchData';
var
  LDataset   : TAbstractModelDataset;
  lSQL       : string;
  lIndex     : integer;
  lMonth     : integer;
  lLine      : string;
  lPrevLine  : string;
  lRainfall  : string;
  lPatchSign : string;
  lYear      : integer;
  lStartPos  : integer;
  lEndYear   : integer;
  lLastYear  : boolean;
  lStartMonth: integer;
  LStation   : IStationData;
  LRainfallObj   : IRainfallModelData;
  LValue     : double;
  LSign      : WideString;
  LIdentifier : integer;
begin
  LIdentifier := 1;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lStartMonth := (FAppModules.Model.ModelData as IRainfallModelData).HydroStartMonth;
        lLine       := AValue.Strings[0];
        lEndYear    := StrToInt(Copy(lLine, 17, 4));
        lIndex      := 1;
        lPrevLine   := '';
        lLastYear   := FALSE;
        Clear_PatchScaledDownStatus(AStationID,FPatchID);
        while ((lIndex < AValue.Count) AND (NOT lLastYear)) do
        begin
          lLine := AValue.Strings[lIndex];
          lYear := StrToInt(Copy(lLine, 11, 4));
          lLastYear := (lYear = lEndYear);
          if (lIndex = 1) then
          begin
            lSQL := 'INSERT INTO RainfallMonthlyPatchData (PatchID, StationID, [Year], Source ';
            for lMonth := lStartMonth to 12 do
              lSQL := lSQL + ',' + Format('Value%2.2d',[lMonth]) + ',' + Format('Flag%2.2d',[lMonth]);
            lSQL := lSQL +
                    ') VALUES (' +
                    IntToStr(FPatchID) + ',' +
                    IntToStr(AStationID) + ',' +
                    IntToStr(lYear) + ',' +
                    IntToStr(0);
            lStartPos := 16;
            for lMonth := lStartMonth to 12 do
            begin
              lRainfall  := Copy(lLine, lStartPos, 4) + '.' + Copy(lLine, lStartPos + 4, 1);
              lPatchSign := Copy(lLine, lStartPos + 5, 1);
              lStartPos  := lStartPos + 6;
              if (Pos('*',lRainfall) >0) then
              begin
                lRainfall := '0';
                lPatchSign := '*';
              end;
              lSQL := lSQL + ',' + lRainfall;

              lSQL := lSQL + ',' + QuotedStr(lPatchSign);

              if lPatchSign = '*' then
              begin
                lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
                lStation     := lRainfallObj.GetStationDataByID(AStationID);
                LStation.RainfallData.GetBaseDataForYearAndMonth(lYear,lMonth,LValue,LSign);
                if (LValue <> NullFloat)then
                begin
                  Save_PatchScaledDownStatus(AStationID,FPatchID,lYear,lMonth,LIdentifier,False );
                  Inc(LIdentifier);
                end;
              end;

            end;
          end
          else
          begin
            lSQL := 'INSERT INTO RainfallMonthlyPatchData (PatchID, StationID, [Year], Source, ' +
                    'Value01, Flag01, Value02, Flag02, Value03, Flag03, Value04, Flag04, ' +
                    'Value05, Flag05, Value06, Flag06, Value07, Flag07, Value08, Flag08, ' +
                    'Value09, Flag09, Value10, Flag10, Value11, Flag11, Value12, Flag12' +
                    ') VALUES (' +
                    IntToStr(FPatchID) + ',' +
                    IntToStr(AStationID) + ',' +
                    IntToStr(lYear) + ',' +
                    IntToStr(0);
            lStartPos := 16 + (12 - lStartMonth + 1) * 6;
            for lMonth := 1 to lStartMonth - 1 do
            begin
              lRainfall  := Copy(lPrevLine, lStartPos, 5);
              lRainfall  := Copy(lPrevLine, lStartPos, 4) + '.' + Copy(lPrevLine, lStartPos + 4, 1);
              lPatchSign := Copy(lPrevLine, lStartPos + 5, 1);
              lStartPos  := lStartPos + 6;
              if (Pos('*',lRainfall) >0) then
              begin
                lRainfall := '0';
                lPatchSign := '*';
              end;
              lSQL := lSQL + ',' + lRainfall;

              lSQL := lSQL + ',' + QuotedStr(lPatchSign);

              if lPatchSign = '*' then
              begin
                lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
                lStation     := lRainfallObj.GetStationDataByID(AStationID);
                LStation.RainfallData.GetBaseDataForYearAndMonth(lYear,lMonth,LValue,LSign);
                if (LValue <> NullFloat)then
                begin
                  Save_PatchScaledDownStatus(AStationID,FPatchID,lYear,lMonth,LIdentifier,False );
                  Inc(LIdentifier);
                end;
              end;
            end;
            lStartPos := 16;
            for lMonth := lStartMonth to 12 do
            begin
              lRainfall  := Copy(lLine, lStartPos, 4) + '.' + Copy(lLine, lStartPos + 4, 1);
              lPatchSign := Copy(lLine, lStartPos + 5, 1);
              lStartPos  := lStartPos + 6;
              if (Pos('*',lRainfall) >0) then
              begin
                lRainfall := '0';
                lPatchSign := '*';
              end;
              lSQL := lSQL + ',' + lRainfall;
              lSQL := lSQL + ',' + QuotedStr(lPatchSign);

              if lPatchSign = '*' then
              begin
                lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
                lStation     := lRainfallObj.GetStationDataByID(AStationID);
                LStation.RainfallData.GetBaseDataForYearAndMonth(lYear,lMonth,LValue,LSign);
                if (LValue <> NullFloat)then
                begin
                  Save_PatchScaledDownStatus(AStationID,FPatchID,lYear,lMonth,LIdentifier,False );
                  Inc(LIdentifier);
                end;
              end;

            end;
          end;
          lSQL := lSQL + ')';
          LDataset.DataSet.Close;
          LDataset.SetSQL(lSQL);
          LDataset.ExecSQL;
          if (lLastYear) then
          begin
            lSQL := 'INSERT INTO RainfallMonthlyPatchData (PatchID, StationID, [Year], Source ';
            for lMonth := 1 to lStartMonth - 1 do
              lSQL := lSQL + ',' + Format('Value%2.2d',[lMonth]) + ',' + Format('Flag%2.2d',[lMonth]);
            lSQL := lSQL +
                    ') VALUES (' +
                    IntToStr(FPatchID) + ',' +
                    IntToStr(AStationID) + ',' +
                    IntToStr(lYear+1) + ',' +
                    IntToStr(0);
            lStartPos := 16 + (12 - lStartMonth + 1) * 6;
            for lMonth := 1 to lStartMonth - 1 do
            begin
              lRainfall  := Copy(lLine, lStartPos, 5);
              lRainfall  := Copy(lLine, lStartPos, 4) + '.' + Copy(lLine, lStartPos + 4, 1);
              lPatchSign := Copy(lLine, lStartPos + 4, 1);
              lStartPos  := lStartPos + 6;
              if (Pos('*',lRainfall) >0) then
              begin
                lRainfall := '0';
                lPatchSign := '*';
              end;
              lSQL := lSQL + ',' + lRainfall;
              lSQL := lSQL + ',' + QuotedStr(lPatchSign);

              if lPatchSign = '*' then
              begin
                lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
                lStation     := lRainfallObj.GetStationDataByID(AStationID);
                LStation.RainfallData.GetBaseDataForYearAndMonth(lYear,lMonth,LValue,LSign);
                if (LValue <> NullFloat)then
                begin
                  Save_PatchScaledDownStatus(AStationID,FPatchID,lYear,lMonth,LIdentifier,False );
                  Inc(LIdentifier);
                end;
              end;
            end;
            lSQL := lSQL + ')';
            LDataset.DataSet.Close;
            LDataset.SetSQL(lSQL);
            LDataset.ExecSQL;
          end;
          lIndex := lIndex + 1;
          lPrevLine := lLine;
        end;
      end;
    finally
      if Assigned(lDataset) then
        FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPatchData.GetRAWData (AStartYear : integer;
                                AEndYear   : integer;
                                ARawData   : TStringList): boolean;
const OPNAME = 'TPatchData.GetRawData';
var
  lIndex            : integer;
  lYearIndex        : integer;
  lRawLine          : string;
  lValue            : integer;
  lFlag             : string;
  lStationName      : string;
  lStartYear        : integer;
  lEndYear          : integer;
  lYearlyData       : IYearlyData;
  lWarning          : string;
  lYear             : integer;
  lMonth            : integer;
  lGrandTotal       : integer;
  lMAP              : integer;
  lYearCount        : integer;
begin
  Result := FALSE;
  try
    ARawData.Clear;
    lWarning := '';
    lStationName := FRainfallData.StationNumber;
    while (Pos(' ', lStationName) > 0) do
      Delete(lStationName, Pos(' ', lStationName), 1);
    lStationName:= Copy(lStationName, 1, 8);

    if (AStartYear = 0) AND (AEndYear = 0) then
    begin
      lStartYear := FRainfallData.HydroStartYear;
      lEndYear   := FRainfallData.HydroEndYear;
    end
    else
    begin
      lStartYear := AStartYear;
      lEndYear   := AEndYear;
    end;
    if (lStartYear < 1900) then
      lStartYear := 1900;
    lGrandTotal := 0;
    lMAP        := 0;
    lYearCount  := 0;
    for lYearIndex := 0 to FRainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData := FRainfallData.GetHydroYearDataByIndex(lYearIndex);
      // New version of PatchR seems to fail if RAW files contains data before 1900
      if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
      begin
        lYearCount := lYearCount + 1;
        lRawLine := Format('%8s%5d ', [lStationName, lYearlyData.Year]);
        for lIndex := 1 to 12 do
        begin
          if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
            lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
          else
            lValue := 0;
          lFlag    := Trim(lYearlyData.MonthlyPatchSign[lIndex]);
          lRawLine := lRawLine + Format('%4d%1s', [lValue, lFlag]);

          if (lValue >= 9999999) then
          begin
            lYear := lYearlyData.Year;
            if (lIndex < 4) then
              lMonth := lIndex + 9
            else
            begin
              lMonth := lIndex - 3;
              lYear  := lYear + 1;
            end;
            lWarning := lWarning + #13#10 + IntToStr(lYear) + ' ' + FormatSettings.ShortMonthNames[lMonth];
          end;
        end;
        lGrandTotal := lGrandTotal + Round(lYearlyData.Total * 10);
        ARawData.Add(lRawLine);
      end;
    end;
    if (lYearCount > 0) then
      lMAP := Trunc(lGrandTotal/lYearCount);
    ARawData.Insert(0, Format('%10s%5d%5d%7d %s',
                    [lStationName, lStartYear, lEndYear, lMAP, 'L']));

    if (lWarning <> '') then
    begin
      lWarning := FAppModules.Language.GetString('Rainfall.WarningString') +
                  FRainfallData.StationNumber + FAppModules.Language.GetString('Rainfall.On') + lWarning +
                  '.' + #13#10 + FAppModules.Language.GetString('Rainfall.NotReadingCorrect') +
                  FAppModules.Language.GetString('Rainfall.Exceed4Characters');
      MessageDlg(lWarning, mtWarning, [mbOK], 0);
    end;

    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.PopulateCommonBlockRAWData (AStartYear  : integer;
                                                AEndYear    : integer;
                                                AGaugeIndex : Integer;
                                                AAddIndex   : Integer;
                                                var ABEGY   : Integer;
                                                var AENDY   : Integer): WordBool;
const OPNAME = 'TPatchData.PopulateCommonBlockRAWData';
var
  lIndex            : integer;
  LYear             : integer;
  lValue            : integer;
  lFlag             : string;
  lStationName      : string;
  lStartYear        : integer;
  lEndYear          : integer;
  lYearlyData       : IYearlyData;
  lRainfallObj      : IRainfallModelData;
  lRawFlags         : TStringList;
  LREFY             : Integer;
  LYearIndex        : Integer;
  LWarning          : string;
  LMonth            : Integer;
  LYearNo           : Integer;
begin
  Result := FALSE;
  try
    LWarning := '';
    LREFY := 1899;
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRawFlags    := TStringList.Create;
    try
      lRawFlags.CommaText := lRainfallObj.RAWFlags;
      lStationName := FRainfallData.StationNumber;
      while (Pos(' ', lStationName) > 0) do
        Delete(lStationName, Pos(' ', lStationName), 1);
      lStationName:= Copy(lStationName, 1, 8);
      for lIndex := 1 to 8 do
//        GFCB.RCOM02.LABELN[AGaugeIndex, LIndex] := lStationName[LIndex];
        GFCB.RCOM02.NGAUGE[AGaugeIndex, LIndex] := lStationName[LIndex];
      for lIndex := 3 to 8 do
        GFCB.RCOM02.LABELN[AGaugeIndex, LIndex] := lStationName[LIndex];
      if (AStartYear = 0) AND (AEndYear = 0) then
      begin
        lStartYear := FRainfallData.HydroStartYear;
        lEndYear   := FRainfallData.HydroEndYear;
      end
      else
      begin
        lStartYear := AStartYear;
        lEndYear   := AEndYear;
      end;
      if (lStartYear < 1900) then
        lStartYear := 1900;
      ABEGY := MIN(MAX(LREFY, lStartYear), ABEGY);
      AENDY := MAX(lEndYear, AENDY);

      for lYear := 0 to FRainfallData.HydroYearsCount - 1 do
      begin
        lYearlyData := FRainfallData.GetHydroYearDataByIndex(lYear);
//        LYearIndex  := lYearlyData.Year - LREFY + 1;
        LYearIndex  := lYearlyData.Year - LREFY + AAddIndex;
        if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
        begin
          for lIndex := 1 to 12 do
          begin
            if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
              lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
            else
              lValue := 0;

            if (lValue >= 999999999) then
            begin
              LYearNo := lYearlyData.Year;
              if (lIndex < 4) then
                lMonth := lIndex + 9
              else
              begin
                lMonth := lIndex - 3;
                LYearNo  := LYearNo + 1;
              end;
              lWarning := lWarning + #13#10 + IntToStr(LYearNo) + ' ' + FormatSettings.ShortMonthNames[lMonth];
            end;

            lFlag    := Trim(lYearlyData.MonthlyPatchSign[lIndex]);
            if (lRawFlags.IndexOf(lFlag) >= 0) then
              lFlag := '+'
            else
              lFlag := ' ';
            GFCB.RCOM03.IDATA[lIndex, AGaugeIndex, LYearIndex] := lValue;
            GFCB.RCOM04.JCODE[lIndex, AGaugeIndex, LYearIndex] := lFlag[1];
          end;
        end;
      end;
    finally
      FreeAndNil(lRawFlags);
    end;
    if (LWarning <> '') then
    begin
      lWarning := FAppModules.Language.GetString('Rainfall.WarningString') +
                  FRainfallData.StationNumber + FAppModules.Language.GetString('Rainfall.On') + lWarning +
                  '.' + #13#10 + FAppModules.Language.GetString('Rainfall.NotReadingCorrect') +
                  FAppModules.Language.GetString('Rainfall.Exceed4Characters');
      MessageDlg(lWarning, mtWarning, [mbOK], 0);
    end
    else
      Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.GetMPData (AStartYear : integer;
                               AEndYear   : integer;
                               AMPData    : TStringList): boolean;
const OPNAME = 'TPatchData.GetMPData';
var
  lIndex            : integer;
  lYearIndex        : integer;
  lBlockNo          : string;
  lPositionInBlock  : string;
  lMPLine           : string;
  lValue            : integer;
  lStationName      : string;
  lYearlyData       : IYearlyData;
  lStationNumber    : string;
  lGrandTotal       : integer;
  lMAP              : integer;
  lYearCount        : integer;
  lStartYear        : integer;
  lEndYear          : integer;
begin
  Result := FALSE;
  try
    AMPData.Clear;
    lStationNumber := FRainfallData.StationNumber;
    lStationName   := lStationNumber;
    while (Pos(' ', lStationName) > 0) do
      Delete(lStationName, Pos(' ', lStationName), 1);
    lStationName := Copy(lStationName, 1, 8);
    lBlockNo         := Copy(lStationNumber, 1, 4);
    lPositionInBlock := Copy(lStationNumber, 5, 3);
    while CharInSet(lPositionInBlock[Length(lPositionInBlock) - 1],[ 'A'..'Z' ]) do
      delete(lPositionInBlock, Length(lPositionInBlock) - 1, 1);

    if (AStartYear = 0) AND (AEndYear = 0) then
    begin
      lStartYear := FRainfallData.HydroStartYear;
      lEndYear   := FRainfallData.HydroEndYear;
    end
    else
    begin
      lStartYear := AStartYear;
      lEndYear   := AEndYear;
    end;
    lGrandTotal := 0;
    lMAP        := 0;
    lYearCount  := 0;
    for lYearIndex := 0 to FRainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData := FRainfallData.GetHydroYearDataByIndex(lYearIndex);
      if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
      begin
        lYearCount := lYearCount + 1;
        lMPLine  := Format('%7s%6d ', [lBlockNo + lPositionInBlock, lYearlyData.Year]);
        for lIndex := 1 to 12 do
        begin
          if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
            lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
          else
            lValue := 0;
          lMPLine  := lMPLine + Format('%4d ', [lValue]);
        end;
        lGrandTotal := lGrandTotal + Round(lYearlyData.Total * 10);
        AMPData.Add(lMPLine);
      end;
    end;
    if (lYearCount > 0) then
      lMAP := Trunc(lGrandTotal/lYearCount);
    AMPData.Insert(0, Format( ' %4s  %3s%5d%5d%7d%1s',
                    [lBlockNo, lPositionInBlock, lStartYear, lEndYear, lMAP, '.' ] )  );
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.GetPATData (AStartYear : integer;
                                AEndYear   : integer;
                                APATData : TStringList): boolean;
const OPNAME = 'TPatchData.GetPATData';
var
  lIndex            : integer;
  lYearIndex        : integer;
  lPatLine          : string;
  lValue            : integer;
  lFlag             : string;
  lStationName      : string;
  lYearlyData       : IYearlyData;
  lMonthTotal       : array [1..12] of integer;
  lMonthAvg         : array [1..12] of integer;
  lGrandTotal       : integer;
  lGrandAvg         : integer;
  lGrandStdDev      : integer;
  lYearTotal        : integer;
  lYearCount        : integer;
  lYearFlag         : string;
begin
  Result := FALSE;
  try
    APATData.Clear;
    for lIndex := 1 to 12 do
    begin
      lMonthTotal[lIndex] := 0;
      lMonthAvg[lIndex]   := 0;
    end;
    lGrandTotal := 0;
    lGrandAvg   := 0;
    lStationName := FRainfallData.FStationNumber;
    while (Pos(' ', lStationName) > 0) do
      Delete(lStationName, Pos(' ', lStationName), 1);
    lStationName:= Copy(lStationName, 1, 8);
    if (Pos('0', lStationName) = 1) then
      lStationName := Copy(lStationName, 2, Length(lStationName) - 1);
    if (AStartYear = 0) then
      AStartYear := FRainfallData.FHydroStartYear;
    if (AEndYear = 0) then
      AEndYear   := FRainfallData.FHydroEndYear;
    if (AStartYear = 0) AND (AEndYear = 0) then
      lYearCount := 0
    else
      lYearCount := 0;
    // Calculate month totals
    for lYearIndex := 0 to FRainfallData.HydroYearsCount - 1 do
    begin
      lYearlyData := FRainfallData.GetHydroYearDataByIndex(lYearIndex);
      if ((lYearlyData.Year >= AStartYear) AND (lYearlyData.Year <= AEndYear)) then
      begin
        lYearCount := lYearCount + 1;
        lPatLine   := Format('%9s%5d ', [lStationName, lYearlyData.Year]);
        lYearFlag  := '';
        for lIndex := 1 to 12 do
        begin
          if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
            lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
          else
            lValue := 0;
          lFlag    := Trim(lYearlyData.MonthlyPatchSign[lIndex]);
          if ((FPatchTypeID = 2) AND (lFlag <> '')) then
          begin
            if (lYearFlag = '') then
              lYearFlag := lFlag
            else if ((lYearFlag = '@') AND ((lFlag = '+') OR (lFlag = '*'))) then
              lYearFlag := lFlag
            else if ((lYearFlag = '+') AND (lFlag = '*')) then
              lYearFlag := lFlag;
          end;
          lPATLine := lPATLine + Format('%5d%1s', [lValue, lFlag]);
          lMonthTotal[lIndex] := lMonthTotal[lIndex] + lValue;
        end;
        lYearTotal := Round(lYearlyData.Total * 10);
        lPatLine := lPatLine + Format('%6d%1s', [lYearTotal, lYearFlag]);
        lGrandTotal := lGrandTotal + lYearTotal;
        APATData.Add(lPATLine);
      end;
    end;

    APATData.Add('');
    lPatLine := FAppModules.Language.GetString('Rainfall.PatLineString');
    if (lYearCount > 0) then
    begin
      // Calculate month averages
      for lIndex := 1 to 12 do
        lMonthAvg[lIndex] := Trunc(lMonthTotal[lIndex]/lYearCount);
      lGrandAvg := Trunc(lGrandTotal/lYearCount);
    end;
    for lIndex := 1 to 12 do
      lPatLine := lPatLine + Format('%5d', [lMonthAvg[lIndex]]);
    lPatLine := lPatLine + Format('%7d', [lGrandAvg]);
    APATData.Add(lPatLine);
    APATData.Insert(0, Format(FAppModules.Language.GetString('Rainfall.FormatedString'),
                    [lStationName, AStartYear, AEndYear, lGrandAvg,'  ']));
    // Calculate standard deviation
    for lIndex := 1 to 12 do
      lMonthTotal[lIndex] := 0;
    lGrandTotal := 0;
    for lYearIndex := 0 to lYearCount - 1 do
    begin
      lYearlyData := TYearlyData(FRainfallData.FHydroYearlyData.Items[lYearIndex]);
      if ((lYearlyData.Year >= AStartYear) AND (lYearlyData.Year <= AEndYear)) then
      begin
        for lIndex := 1 to 12 do
        begin
          if (lYearlyData.MonthlyRainfall[lIndex] >= 0) then
            lValue := Round(lYearlyData.MonthlyRainfall[lIndex] * 10)
          else
            lValue := 0;
          lMonthTotal[lIndex] := lMonthTotal[lIndex] + Trunc(Power(lValue - lMonthAvg[lIndex], 2));
        end;
      end;
      lValue := Trunc(lYearlyData.Total * 10);
      lGrandTotal := lGrandTotal + Trunc(Power(lValue - lGrandAvg , 2));
    end;
    APATData.Add('');
    lPatLine := FAppModules.Language.GetString('Rainfall.PatLineString1');

    for lIndex := 1 to 12 do
      lMonthAvg[lIndex] := 0;
    lGrandStdDev := 0;
    if (lYearCount > 0) then
    begin
      for lIndex := 1 to 12 do
        lMonthAvg[lIndex] := Trunc(Sqrt(lMonthTotal[lIndex]/(lYearCount - 1)));
      if (lGrandTotal>0) then
        LGrandStdDev := Trunc(Sqrt(lGrandTotal/(lYearCount-1)));
    end;
    for lIndex := 1 to 12 do
      lPatLine := lPatLine + Format('%5d', [lMonthAvg[lIndex]]);
    lPatLine := lPatLine + Format('%7d', [lGrandStdDev]);
    APATData.Add(lPatLine);
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.SavePATFile (AStartYear       : integer;
                                  AEndYear         : integer;
                                  const ADirectory : WideString);
const OPNAME = 'TPatchData.SavePATFile';
var
  lPATData            : TStringList;
  lPATFile            : TFileStream;
  lStationName        : string;
begin
  try
    lPATData := TStringList.Create;
    try
      if (GetPATData(AStartYear, AEndYear, lPATData)) then
      begin
        try
          lStationName := FRainfallData.FStationNumber;
          while (Pos(' ', lStationName) > 0) do
            Delete(lStationName, Pos(' ', lStationName), 1);
          lStationName:= Copy(lStationName, 1, 8);

          lPATFile := TFileStream.Create(ADirectory + lStationName + '.PAT', fmCreate );
          lPATData.SaveToStream(lPATFile);
        finally
          FreeAndNil(lPATFile);
        end;
      end;
    finally
      FreeAndNil(lPATData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPatchData.SaveRAWFile (AStartYear       : integer;
                                  AEndYear         : integer;
                                  const ADirectory : WideString);
const OPNAME = 'TPatchData.SaveRAWFile';
var
  lRawData      : TStringList;
  lRAWFile      : TFileStream;
  lStationName  : string;
begin
  try
    lRawData := TStringList.Create;
    try
      if (GetRAWData(AStartYear, AEndYear, lRawData)) then
      begin
        try
          lStationName := FRainfallData.StationNumber;
          while (Pos(' ', lStationName) > 0) do
            Delete(lStationName, Pos(' ', lStationName), 1);
          lStationName:= Copy(lStationName, 1, 8);

          lRAWFile := TFileStream.Create(ADirectory + lStationName + '.RAW', fmCreate );
          lRawData.SaveToStream(lRAWFile);
        finally
          FreeAndNil(lRAWFile);
        end;
      end;
    finally
      FreeAndNil(lRawData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPatchData.SaveMPFile (AStartYear       : integer;
                                 AEndYear         : integer;
                                 const ADirectory : WideString);
const OPNAME = 'TPatchData.SaveMPFile';
var
  lMPData      : TStringList;
  lMPFile      : TFileStream;
  lStationName : string;
begin
  try
    lMPData  := TStringList.Create;
    try
      if (GetMPData(AStartYear, AEndYear, lMPData)) then
      begin
        try
          lStationName := RainfallData.StationNumber;
          while (Pos(' ', lStationName) > 0) do
            Delete(lStationName, Pos(' ', lStationName), 1);
          lStationName:= Copy(lStationName, 1, 8);

          lMPFile  := TFileStream.Create(ADirectory + lStationName + '.MP', fmCreate );
          lMPData.SaveToStream(lMPFile);
        finally
          FreeAndNil(lMPFile);
        end;
      end;
    finally
      FreeAndNil(lMPData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPatchData.CreateHydrologicalYears;
const OPNAME = 'TPatchData.CreateHydrologicalYears';
var
  lMonth           : integer;
  lHeadNULL        : boolean;
  lTailNULL        : boolean;
  lPrevYear        : TYearlyData;
  lThisYear        : TYearlyData;
  lIndex           : integer;
  lHydroYear       : TYearlyData;
  lHydroStartMonth : integer;
  lHydroMonth      : integer;
  lFieldIndex      : string;
  lDataField       : string;
  lSignField       : string;
  lChangeValue     : double;
  lChangeSign      : string;
  lKeyValues       : string;
begin
  try
    lHydroStartMonth := (FAppModules.Model.ModelData as IRainfallModelData).HydroStartMonth;
    FRainfallData.FHydroYearlyData.Clear;
    FRainfallData.FHydroStartYear := 0;
    FRainfallData.FHydroEndYear   := 0;
    if (FPatchTypeID = 1) then
    begin
      lDataField := 'MonthlyWRCData';
      lSignField := 'MonthlyWRCSign';
    end
    else
    begin
      lDataField := 'MonthlyPatchData';
      lSignField := 'MonthlyPatchSign';
    end;
    if (FRainfallData.FYearlyData.Count > 0) then
    begin
      lPrevYear := nil;
      lThisYear := nil;
      lIndex := 0;
      while (lIndex < FRainfallData.FYearlyData.Count) do
      begin
        lThisYear := TYearlyData(FRainfallData.FYearlyData.Items[lIndex]);
        if (lIndex = 0) then
        begin
          lMonth := 1;
          lHeadNULL := TRUE;
          { Check if rainfall for all months before start of hydrological year is NULL }
          while (lHeadNULL AND (lMonth < lHydroStartMonth)) do
          begin
            if (lThisYear.FMonthlyRainfall[lMonth] <> NullFloat) then
              lHeadNULL := FALSE
            else
            begin
              lFieldIndex := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
              lKeyValues  := GetKeyValues(lDataField, lFieldIndex);
              if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) OR
                 (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
                lHeadNULL := FALSE
              else
                lMonth := lMonth + 1;
            end;
          end;
          if (NOT lHeadNULL) then
          begin
            lHydroYear := TYearlyData.Create(FAppModules);
            lHydroYear.Initialise;
            FRainfallData.FHydroYearlyData.Add(lHydroYear);
            lHydroYear.Year := lThisYear.Year - 1;
            lHydroYear.HydroYear := IntToStr(lHydroYear.Year) + '/' + Copy(IntToStr(lThisYear.Year), 3, 2);
            for lMonth := lHydroStartMonth to 12 do
            begin
              lHydroMonth  := lMonth - (lHydroStartMonth - 1);
              lFieldIndex  := IntToStr(lThisYear.Year-1) + ',' + IntToStr(lMonth);
              lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
              if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                             (lDataField, lKeyValues,
                                              FloatToStr(NullFloat),
                                              IntToStr(lMonth)));
                lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
              end;
              if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeSign := FAppModules.Changes.GetParameterValue
                                             (lSignField, lKeyValues, '', IntToStr(lMonth));
                lHydroYear.FMonthlyPatchSign[lHydroMonth] := lChangeSign;
              end;

            end;
            for lMonth := 1 to lHydroStartMonth - 1 do
            begin
              lHydroMonth := lMonth + (12 - lHydroStartMonth + 1);
              lFieldIndex := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
              lKeyValues  := GetKeyValues(lDataField, lFieldIndex);
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lThisYear.FMonthlyRainfall[lMonth];
              lHydroYear.FMonthlyPatchSign[lHydroMonth] := lThisYear.FMonthlyPatchSign[lMonth];
              if( lHydroYear.FMonthlyPatchSign[lHydroMonth] = '*') then
                lHydroYear.FMonthlyScaledDown[lHydroMonth] :=  lHydroYear.FMonthlyRainfall[lHydroMonth];

              if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                             (lDataField, lKeyValues,
                                              FloatToStr(lThisYear.FMonthlyRainfall[lMonth]),
                                              IntToStr(lMonth)));
                lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
              end;
              if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
              begin
                lChangeSign := FAppModules.Changes.GetParameterValue
                                             (lSignField, lKeyValues,
                                              lThisYear.FMonthlyPatchSign[lMonth],
                                              IntToStr(lMonth));
                lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
              end;

            end;
          end;
        end
        else
        begin
          lHydroYear := TYearlyData.Create(FAppModules);
          lHydroYear.Initialise;
          FRainfallData.FHydroYearlyData.Add(lHydroYear);
          lHydroYear.Year := lThisYear.Year - 1;
          lHydroYear.HydroYear := IntToStr(lHydroYear.Year) + '/' + Copy(IntToStr(lThisYear.Year), 3, 2);
          for lMonth := lHydroStartMonth to 12 do
          begin
            lHydroMonth  := lMonth - (lHydroStartMonth - 1);
            lFieldIndex  := IntToStr(lThisYear.Year-1) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            lHydroYear.FMonthlyRainfall[lHydroMonth]  := lPrevYear.FMonthlyRainfall[lMonth];
            lHydroYear.FMonthlyPatchSign[lHydroMonth] := lPrevYear.FMonthlyPatchSign[lMonth];

            if( lHydroYear.FMonthlyPatchSign[lHydroMonth] = '*') then
              lHydroYear.FMonthlyScaledDown[lHydroMonth] :=  lHydroYear.FMonthlyRainfall[lHydroMonth];

            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                           (lDataField, lKeyValues,
                                            FloatToStr(lPrevYear.FMonthlyRainfall[lMonth]),
                                            IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                           (lSignField, lKeyValues,
                                            lPrevYear.FMonthlyPatchSign[lMonth],
                                            IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;

          end;
          for lMonth := 1 to lHydroStartMonth - 1 do
          begin
            lHydroMonth  := lMonth + (12 - lHydroStartMonth + 1);
            lFieldIndex  := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            lHydroYear.FMonthlyRainfall[lHydroMonth]  := lThisYear.FMonthlyRainfall[lMonth];
            lHydroYear.FMonthlyPatchSign[lHydroMonth] := lThisYear.FMonthlyPatchSign[lMonth];

            if( lHydroYear.FMonthlyPatchSign[lHydroMonth] = '*') then
              lHydroYear.FMonthlyScaledDown[lHydroMonth] :=  lHydroYear.FMonthlyRainfall[lHydroMonth];

            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                           (lDataField, lKeyValues,
                                            FloatToStr(lThisYear.FMonthlyRainfall[lMonth]),
                                            IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                           (lSignField, lKeyValues,
                                            lThisYear.FMonthlyPatchSign[lMonth],
                                            IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;

          end;
        end;
        lPrevYear := lThisYear;
        lIndex    := lIndex + 1;
      end;
      if (lPrevYear <> nil) then
      begin
        lMonth := lHydroStartMonth;
        lTailNULL := TRUE;
        while (lTailNULL AND (lMonth <= 12)) do
        begin
          if (lPrevYear.FMonthlyRainfall[lMonth] <> NullFloat) then
            lTailNULL := FALSE
          else
          begin
            lFieldIndex  := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) OR
               (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
              lTailNULL := FALSE
            else
              lMonth := lMonth + 1;
          end;
        end;
        if (NOT lTailNULL) then
        begin
          lHydroYear := TYearlyData.Create(FAppModules);
          lHydroYear.Initialise;
          FRainfallData.FHydroYearlyData.Add(lHydroYear);
          lHydroYear.Year := lThisYear.Year;
          lHydroYear.HydroYear := IntToStr(lHydroYear.Year) + '/' + Copy(IntToStr(lThisYear.Year+1), 3, 2);
          for lMonth := lHydroStartMonth to 12 do
          begin
            lHydroMonth  := lMonth - (lHydroStartMonth - 1);
            lFieldIndex  := IntToStr(lThisYear.Year) + ',' + IntToStr(lMonth);
            lKeyValues   := GetKeyValues(lDataField, lFieldIndex);
            lHydroYear.FMonthlyRainfall[lHydroMonth]  := lPrevYear.FMonthlyRainfall[lMonth];
            lHydroYear.FMonthlyPatchSign[lHydroMonth] := lPrevYear.FMonthlyPatchSign[lMonth];

            if( lHydroYear.FMonthlyPatchSign[lHydroMonth] = '*') then
              lHydroYear.FMonthlyScaledDown[lHydroMonth] :=  lHydroYear.FMonthlyRainfall[lHydroMonth];

        if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                         (lDataField, lKeyValues,
                                          FloatToStr(lPrevYear.FMonthlyRainfall[lMonth]),
                                          IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                         (lSignField, lKeyValues,
                                          lPrevYear.FMonthlyPatchSign[lMonth],
                                          IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;

          end;
          for lMonth := 1 to lHydroStartMonth - 1 do
          begin
            lHydroMonth := lMonth + (12 - lHydroStartMonth + 1);
            lFieldIndex := IntToStr(lThisYear.Year+1) + ',' + IntToStr(lMonth);
            lKeyValues  := GetKeyValues(lDataField, lFieldIndex);
            if (FAppModules.Changes.HasParamChange(lDataField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeValue := StrToFloat(FAppModules.Changes.GetParameterValue
                                         (lDataField, lKeyValues,
                                          FloatToStr(NullFloat),
                                          IntToStr(lMonth)));
              lHydroYear.FMonthlyRainfall[lHydroMonth]  := lChangeValue;
            end;
            if (FAppModules.Changes.HasParamChange(lSignField, lKeyValues, IntToStr(lMonth))) then
            begin
              lChangeSign := FAppModules.Changes.GetParameterValue
                                         (lSignField, lKeyValues, '', IntToStr(lMonth));
              lHydroYear.FMonthlyPatchSign[lHydroMonth]  := lChangeSign;
            end;
            
          end;
        end;
      end;
      if (FRainfallData.FHydroYearlyData.Count > 0) then
      begin
        lHydroYear      := TYearlyData(FRainfallData.FHydroYearlyData.Items[0]);
        FRainfallData.FHydroStartYear := lHydroYear.Year;
        lHydroYear      := TYearlyData(FRainfallData.FHydroYearlyData.Items[FRainfallData.FHydroYearlyData.Count - 1]);
        FRainfallData.FHydroEndYear   := lHydroYear.Year;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPatchData.SourcesCount: Integer;
const OPNAME = 'TPatchData.SourcesCount';
begin
  Result := 0;
  try
    Result := FSources.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPatchData.Get_SourceInfo : WideString;
const OPNAME = 'TPatchData.Get_SourceInfo';
begin
  Result := '';
  try
    Result := FSources.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPatchData.Set_SourceInfo (const Value: WideString);
const OPNAME = 'TPatchData.Set_SourceInfo';
begin
  try
    FSources.CommaText := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPatchData.GetKeyValues (const AParamField : WideString;
                                  const AFieldIndex : WideString): WideString;
const OPNAME = 'TPatchData.GetKeyValues';
var
  lYear : integer;
begin
  Result := '';
  try
    lYear := StrToInt(Copy(AFieldIndex, 1, 4));
    if ((AParamField = 'MonthlyWRCData') OR
        (AParamField = 'MonthlyWRCSign')) then
    begin
      Result := 'PatchID='    + IntToStr(FPatchID) +
                ',StationID=' + IntToStr(FRainfallData.FStationID) +
                ',Year='      + IntToStr(lYear);
    end
    else
    if ((AParamField = 'MonthlyPatchData') OR
        (AParamField = 'MonthlyPatchSign')) then
//      Result := 'SubArea='    + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
//                ',PatchID='   + IntToStr(FPatchID) +
      Result := 'PatchID='    + IntToStr(FPatchID) +
                ',StationID=' + IntToStr(FRainfallData.FStationID) +
                ',Year='      + IntToStr(lYear);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPatchData.Get_RainfallData : IRainfallData;
const OPNAME = 'TPatchData.Get_RainfallData';
begin
  Result := nil;
  try
    Result := FRainfallData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TYearlyData.Get_MonthlyScaledDown(AMonth: Integer): Double;
const OPNAME = 'TYearlyData.Get_MonthlyScaledDown';
begin
  Result := 0;
  try
    Result := FMonthlyScaledDown[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TYearlyData.Set_MonthlyScaledDown(AMonth: Integer; Value: Double);
const OPNAME = 'TYearlyData.Set_MonthlyScaledDown';
begin
  try
    FMonthlyScaledDown[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Get_PatchScaledDownStatus(AStationID, APatchID, AYear,AMonth: integer): WordBool;
const OPNAME = 'TPatchData.Get_PatchScaledDownStatus';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset) )then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT PatchScaledDownStatus FROM RainfallScaledDownPatchValues' +
                ' WHERE PatchID = ' + IntToStr(APatchID) +
                ' AND StationID = ' + IntToStr(AStationID) +
                ' AND HydroYear = ' + IntToStr(AYear) +
                ' AND Month = ' +  IntToStr(AMonth);
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (not lDataset.DataSet.Eof )then
          Result := lDataset.DataSet.FieldByName('PatchScaledDownStatus').AsBoolean;
        lDataset.DataSet.Close;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Set_PatchScaledDownStatus(AStationID, APatchID, AYear, AMonth: integer; AStatus: WordBool): WordBool;
const OPNAME = 'TPatchData.Set_PatchScaledDownStatus';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        if (AStatus) then
          lSQL := 'UPDATE RainfallScaledDownPatchValues SET PatchScaledDownStatus = ' + QuotedStr('Y')
        else
          lSQL := 'UPDATE RainfallScaledDownPatchValues SET PatchScaledDownStatus = ' + QuotedStr('N');

        lSQL := lSQL + ' WHERE PatchID = ' + IntToStr(APatchID) +
                       ' AND StationID = ' + IntToStr(AStationID) +
                       ' AND HydroYear = ' + IntToStr(AYear) +
                       ' AND Month = ' +  IntToStr(AMonth);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        Result := TRUE;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.Clear_PatchScaledDownStatus(AStationID, APatchID: integer): WordBool;
const OPNAME = 'TPatchData.Clear_PatchScaledDownStatus';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'Delete * From RainfallScaledDownPatchValues' +
                ' Where PatchID = '+ IntToStr(APatchID) +
                ' AND StationID = '+ IntToStr(AStationID);
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;
      end;
    finally
      if Assigned(lDataset) then
      begin
        LDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TPatchData.Save_PatchScaledDownStatus(AStationID, APatchID, AYear, AMonth, AIdentifier: integer; AStatus: WordBool): WordBool;
const OPNAME = 'TPatchData.Save_PatchScaledDownStatus';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
  //LCount   : Integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'Delete * From RainfallScaledDownPatchValues' +
                ' Where PatchID = '+ IntToStr(APatchID) +
                ' AND StationID = '+ IntToStr(AStationID)+
                ' AND HydroYear = '+ IntToStr(AYear)+
                ' AND [Month] = '    + IntToStr(AMonth);
        LDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        //LDataset.DataSet.Open;
        //LCount := lDataset.DataSet.RecordCount;
        if ( (LDataset.DataSet.Eof) {and (LCount = 0)}) then
        begin
          lSQL := 'INSERT INTO RainfallScaledDownPatchValues(PatchID,StationID,Identifier,HydroYear,[Month],' +
                  'PatchScaledDownStatus' +
                  ') VALUES (' +
                      IntToStr(APatchID) + ',' +
                      IntToStr(AStationID) + ',' +
                      IntToStr(AIdentifier) + ','+
                      IntToStr(AYear) + ',' +
                      IntToStr(AMonth) + ','+
                      QuotedStr('N') +')';
          lDataset.DataSet.Close;
          lDataset.SetSQL(lSQL);
          lDataset.ExecSQL;
          Result := TRUE;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPatchData.RainfallData : IRainfallData;
const OPNAME = 'TPatchData.RainfallData';
begin
  Result := nil;
  try
    Result := FRainfallData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchData.GetCompressData (var AData  : string;
                                      AFieldName : string);
const OPNAME = 'TPatchData.GetCompressData';
var
  lDecompressStream : TZDecompressionStream;
  lBlobStream       : TStream;
  lDataset          : TAbstractModelDataset;
  lSQL              : string;
  lDataList         : TStringList;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT ' + AFieldName + ' FROM RainfallPatchR WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.SetSQL(lSQL);
        lDataset.Dataset.Open;
        if (NOT lDataset.DataSet.Eof) then
        begin
          lBlobStream       := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName(AFieldName), bmRead);
          lDecompressStream := nil;
          if lBlobStream.Size>2 then
            lDecompressStream := TZDecompressionStream.Create(lBlobStream);
          lDataList         := TStringList.Create;
          try
            if lDecompressStream <> nil then
            begin
              lDataList.LoadFromStream(lDecompressStream);
              AData := lDataList.Text;
            end;
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(LDecompressStream);
            FreeAndNil(lDataList);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.SetCompressData (AData      : string;
                                      AFieldName : string);
const OPNAME = 'TPatchData.SetCompressData';
var
  lMemoryStream   : TMemoryStream;
  lCompressStream : TZCompressionStream;
  lDataset        : TAbstractModelDataset;
  lBlobStream     : TStream;
  lSQL            : string;
  lDataList       : TStringList;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchR WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.SetSQL(lSQL);
        lDataset.SetReadOnly(False);
        lDataset.Dataset.Open;

        if (NOT lDataset.DataSet.Eof) then
        begin
          lDataset.DataSet.Edit;
          lMemoryStream   := TMemoryStream.Create;
          lBlobStream     := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName(AFieldName), bmWrite);
          lCompressStream := TZCompressionStream.Create(lBlobStream);
          lDataList       := TStringList.Create;
          try
            lDataList.Text := AData;
            lDataList.SaveToStream(lMemoryStream);
            lCompressStream.CopyFrom(lMemoryStream, 0);
            FreeAndNil(lCompressStream);
          finally
            FreeAndNil(lDataList);
            FreeAndNil(lBlobStream);
            FreeAndNil(lMemoryStream);
          end;
          lDataset.DataSet.Post;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;
(*
// No Compress
procedure TPatchData.GetCompressData (var AData  : WideString;
                                      AFieldName : string);
const OPNAME = 'TPatchData.GetCompressData';
var
//  lDecompressStream : TZDecompressionStream;
//  lBlobStream       : TStream;
  lDataset          : TAbstractModelDataset;
  lSQL              : string;
//  lDataList         : TStringList;
  LStrStream : TStringStream;
  LBlobStream : TBlobStream;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT ' + AFieldName + ' FROM RainfallPatchR WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.SetSQL(lSQL);
        lDataset.Dataset.Open;
        if (NOT lDataset.DataSet.Eof) then
        begin
          LStrStream := TStringStream.Create('');
          try
            LBlobStream := TBlobStream(lDataset.DataSet.CreateBlobStream(lDataset.Dataset.FieldByName(AFieldName), bmRead));
            try
              LBlobStream.Seek(0, soFromBeginning);
              LStrStream.CopyFrom(LBlobStream, LBlobStream.Size);
              AData := LStrStream.DataString;
            finally
              LBlobStream.Free;
            end;
          finally    
            LStrStream.Free;
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.SetCompressData (AData      : WideString;
                                      AFieldName : string);
const OPNAME = 'TPatchData.SetCompressData';
var
//  lMemoryStream   : TMemoryStream;
//  lCompressStream : TZCompressionStream;
  lDataset        : TAbstractModelDataset;
//  lBlobStream     : TStream;
  lSQL            : string;
//  lDataList       : TStringList;
  LStrStream : TStringStream;
  LBlobStream : TBlobStream;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchR WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.SetSQL(lSQL);
        lDataset.SetReadOnly(False);
        lDataset.Dataset.Open;

        if (NOT lDataset.DataSet.Eof) then
        begin
          lDataset.DataSet.Edit;

          LStrStream := TStringStream.Create(AData);
          try
            LBlobStream := TBlobStream(lDataset.DataSet.CreateBlobStream(lDataset.Dataset.FieldByName(AFieldName), bmWrite));
            try
              LBlobStream.Seek(0, soFromBeginning);
              LBlobStream.CopyFrom(LStrStream, 0);
            finally
              LBlobStream.Free;
            end;
          finally
            LStrStream.Free;
          end;
          lDataset.DataSet.Post;

        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;
*)

{
Procedure tForm1.SaveBtnClick(Sender:TObject);

var
  fs : TFileStream;
  bs : TBlobStream;

Begin
  with Table1 do
  begin
    Append;
    fs := TFileStream.Create('demo.doc', fmOpenRead);
    Try
      bs := TBlobStream(CreateBlobStream(FieldByName('doc'), bmWrite));
      Try
        bs.CopyFrom(fs, 0);
      Finally
        bs.Free;
      End;
    Finally
      fs.Free;
    End;
    Post;
  end;
End;


Procedure tForm1.LoadImageBtnClick ( Sender:tOject);

var 
  fs : TFileStream; 
  bs : TBlobStream;

Begin  
  with Table1 do
  begin
    fs := TFileStream.Create('demo.doc', fmOpenWrite or fmCreate);
    bs := TBlobStream(CreateBlobStream(FieldByName('doc'), bmRead));
    fs.CopyFrom(bs, 0);
    bs.Free;
    fs.Free;
  End;
end; 
}
{
The following example copies the data in the Notes field of Table1 or SQLDataSet1 to the Remarks field of ClientDataSet1.

Windows-only:

procedure TForm1.Button1Click(Sender: TObject);

var
  Stream1: TBlobStream;
  Stream2: TStream;
begin
  Stream1 := TBlobStream.Create(Table1Notes, bmRead);
  try
    ClientDataSet1.Edit;
    // here’s a different way to create a blob stream
    Stream2 := ClientDataSet1.CreateBlobStream(ClientDataSet1.FieldByName('Remarks'), bmReadWrite);
    try
      Stream2.CopyFrom(Stream1, Stream1.Size);
      ClientDataSet1.Post;
    finally
      Stream2.Free;
    end;

  finally
    Stream1.Free;
  end;
end;

function TBPCompress.CompressFile(SourceFile,
  CompressedFile: string): boolean;
var
  ms1, ms2: TMemoryStream;
begin
  result := False;
  ms1 := TMemoryStream.Create;
  ms2 := TMemoryStream.Create;
  try
    ms1.LoadFromFile(SourceFile);
    if CompressStream(ms1, ms2) then
       result := True;
    ms2.SaveToFile(CompressedFile);
  finally
    ms1.Free;
    ms2.Free;
  end;
end;

function TBPCompress.CompressStream(  inpStream : TMemoryStream;
                                outStream : TMemoryStream
                              ) : boolean;
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
begin
  result := False;
  InpBuf := nil;
  OutBuf := nil;
  try
    GetMem(InpBuf, inpStream.Size);
    inpStream.Position := 0;
    InpBytes := inpStream.Read(InpBuf^, inpStream.Size);
    CompressBuf(InpBuf, InpBytes, OutBuf, OutBytes);
    outStream.Write(OutBuf^, OutBytes);
  finally
    if InpBuf <> nil then FreeMem(InpBuf);
    if OutBuf <> nil then begin
       FreeMem(OutBuf);
       result := True;
    end;
  end;
end;


function TBPCompress.DeCompressFile(CompressedFile,
  DestFile: string): boolean;
var
  ms1, ms2: TMemoryStream;
begin
  result := False;
  ms1 := TMemoryStream.Create;
  ms2 := TMemoryStream.Create;
  try
    ms1.LoadFromFile(CompressedFile);
    if DecompressStream(ms1, ms2) then
       result := True;
    ms2.SaveToFile(DestFile);
  finally
    ms1.Free;
    ms2.Free;
  end;
end;

function TBPCompress.DeCompressStream(  inpStream : TMemoryStream;
                                  outStream : TMemoryStream
                                ) : boolean;
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
begin
  result := False;
  InpBuf := nil;
  OutBuf := nil;
  inpStream.Position := 0;
  InpBytes     := inpStream.Size - inpStream.Position;
  if InpBytes > 0 then
    try
      GetMem(InpBuf, InpBytes);
      inpStream.Read(InpBuf^, InpBytes);
      DecompressBuf(InpBuf, InpBytes, 0, OutBuf, OutBytes);
      outStream.Write(OutBuf^, OutBytes);
    finally
      if InpBuf <> nil then FreeMem(InpBuf);
      if OutBuf <> nil then begin
         FreeMem(OutBuf);
         result := True;
      end;
    end;
  outStream.Position := 0;
end;

}
(*
procedure TPatchData.GetCompressData (var AData  : string;
                                      AFieldName : string);
const OPNAME = 'TPatchData.GetCompressData';
var
  lDecompressStream : TZDecompressionStream;
  lBlobStream       : TStream;
  lDataset          : TAbstractModelDataset;
  lSQL              : string;
  lDataList         : TStringList;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT ' + AFieldName + ' FROM RainfallPatchR WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.SetSQL(lSQL);
        lDataset.Dataset.Open;
        if (NOT lDataset.DataSet.Eof) then
        begin
          lBlobStream       := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName(AFieldName), bmRead);
          lDecompressStream := TZDecompressionStream.Create(lBlobStream);
          lDataList         := TStringList.Create;
          try
            lDataList.LoadFromStream(lDecompressStream);
//            lDataList.LoadFromStream(lBlobStream); //Riana
            AData := lDataList.Text;
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(LDecompressStream);
            FreeAndNil(lDataList);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TPatchData.SetCompressData (AData      : string;
                                      AFieldName : string);
const OPNAME = 'TPatchData.SetCompressData';
var
  lMemoryStream   : TMemoryStream;
  lCompressStream : TZCompressionStream;
  lDataset        : TAbstractModelDataset;
  lBlobStream     : TStream;
  lSQL            : string;
  lDataList       : TStringList;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchR WHERE PatchID = ' + IntToStr(FPatchID);
        lDataset.SetSQL(lSQL);
        lDataset.SetReadOnly(False);
        lDataset.Dataset.Open;

        if (NOT lDataset.DataSet.Eof) then
        begin
          lDataset.DataSet.Edit;
          lMemoryStream   := TMemoryStream.Create;
          lBlobStream     := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName(AFieldName), bmWrite);
          lCompressStream := TZCompressionStream.Create(lBlobStream);
          lDataList       := TStringList.Create;
          try
            lDataList.Text := AData;
            lDataList.SaveToStream(lMemoryStream);
            lCompressStream.CopyFrom(lMemoryStream, 0);
            FreeAndNil(lCompressStream);
          finally
            FreeAndNil(lDataList);
            FreeAndNil(lBlobStream);
            FreeAndNil(lMemoryStream);
          end;
          lDataset.DataSet.Post;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;
*)
end.


