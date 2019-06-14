//
//
//  UNIT      : Contains TDailyDiversionGaugeDataList Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 30/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionGaugeData;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VCL.Controls,
  VoaimsCom_TLB,
  UDailyIFRData,
  UAbstractObject;

const
  CDaysInMonth : array[1..12]of integer = (31,28,31,30,31,30,31,31,30,31,30,31);
  CHydroMonths : array[1..12]of integer = (10,11,12,1,2,3,4,5,6,7,8,9);
  CHydroMonthsDesc : array[1..12]of string = ('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep');
type
  TDailyFlowData = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FIdentifier  : integer;
    FDiversionDate : TDate;
    FAvgFlow     : double;
    FQualityCode : integer;
    FCatchmentFactor : double;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_AvgFlow: double;
    function Get_DiversionDate: TDate;
    function Get_Identifier: integer;
    function Get_QualityCode: integer;
    function Get_StationID: integer;
    function Get_FactoredFlow: double;
    procedure Set_AvgFlow(const Value: double);
    procedure Set_DiversionDate(const Value: TDate);
    procedure Set_QualityCode(const Value: integer);
  public

    function Poulate(AStationID: integer;AIdentifier : integer;ADiversionDate : TDate;
                     AAvgFlow,ACatchmentFactor : double; AQualityCode : integer) : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property StationID : integer read Get_StationID;
    property Identifier : integer read Get_Identifier write FIdentifier;
    property DiversionDate : TDate read Get_DiversionDate write Set_DiversionDate;
    property AvgFlow : double read Get_AvgFlow write Set_AvgFlow;
    property QualityCode : integer read Get_QualityCode write Set_QualityCode;
    property FactoredFlow : double read Get_FactoredFlow;
    function Initialise: Boolean; override;
  end;


  TMonthlyFlowData = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FYear : integer;
    FMissingDaysCount : integer;
    FDaysInMonth : TIntegerArray;
    FAvgFlow : TMonthlyDoubleArray;
    FIncludeAllMonthTotal : TMonthlyDoubleArray;
    FGoodMonthTotal : TMonthlyDoubleArray;
    FSuspectMonthTotal : TMonthlyDoubleArray;
    FInfilledMonthTotal : TMonthlyDoubleArray;
    FDaysWithGabsList : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_AvgFlowByIndex(AIndex: integer): double;safecall;
    function Get_StationID: integer;safecall;
    function Get_IncludeAllMonthTotalByMonth(AMonth : integer): double;
    procedure Set_IncludeAllMonthTotalByMonth(AMonth : integer;const Value: double);
    function Get_SuspectMonthTotalByMonth(AMonth : integer): double;
    procedure Set_SuspectMonthTotalByMonth(AMonth : integer; const Value: double);
    function Get_Year: integer;safecall;
  public
    function Initialise: Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function MonthlyThresholdVerified(AMonth : integer):boolean;safecall;
    property StationID : integer read Get_StationID;
    property AvgFlowByIndex[AIndex : integer] : double read Get_AvgFlowByIndex;
    property IncludeAllMonthTotalByMonth[AMonth : integer] : double read Get_IncludeAllMonthTotalByMonth write Set_IncludeAllMonthTotalByMonth;
    property SuspectMonthTotalByMonth[AMonth : integer] : double read Get_SuspectMonthTotalByMonth write Set_SuspectMonthTotalByMonth;
    property Year : integer read Get_Year;

  end;

  TMonthlyInstreamFlowData = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FYear : integer;
    FMissingDaysCount : integer;
    FDaysInMonth : TIntegerArray;
    FAvgFlow : TMonthlyDoubleArray;
    FMonthTotal : TMonthlyDoubleArray;
    FDailyDiversionTotal : TMonthlyDoubleArray;
    FNonDailyDiversionTotal : TMonthlyDoubleArray;
    FExcludeMissingDailyData : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_DailyDiversionFlowByIndex(AIndex: integer): double;safecall;
    function Get_StationID: integer;safecall;
    function Get_Year: integer;safecall;
    function Get_DailyDiversionTotalByMonth(AMonth: integer): double;
  public
    function Initialise: Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property StationID : integer read Get_StationID;
    property DailyDiversionFlowByIndex[AIndex : integer] : double read Get_DailyDiversionFlowByIndex;
    property Year : integer read Get_Year;
    property DailyDiversionTotalByMonth[AMonth : integer] : double read Get_DailyDiversionTotalByMonth;
  end;


  TDailyInstreamFlowData = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FIdentifier  : integer;
    FInstreamDate : TDate;
    FAvgFlow     : double;
    FQualityCode : integer;
    FScaleFactor : double;
    FCompensationValue : double;
    FCapacityOfDiversion : double;
    FDailyData  :  TDailyFlowData;
    FHasGaps : boolean;
    FMonthlyThreshold : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_AvgFlow: double;
    function Get_InstreamDate: TDate;
    function Get_Identifier: integer;
    function Get_QualityCode: integer;
    function Get_StationID: integer;
    procedure Set_AvgFlow(const Value: double);
    procedure Set_InstreamDate(const Value: TDate);
    procedure Set_QualityCode(const Value: integer);
    procedure Set_DailyData(const Value: TDailyFlowData);
    function Get_DailyData : TDailyFlowData;
    function Get_DailyAvailableFlow(AMonth : integer): double;
    function Get_DailyDiversionFlow(AMonth : integer): double;
    function Get_NonDailyDiversionFlow(AMonth : integer): double;
    function Get_FactoredInstreamFlow: double;
    function Get_HasGaps: boolean;
    procedure Set_HasGaps(const Value: boolean);
    function Get_MonthlyThreshold: boolean;
    procedure Set_MonthlyThreshold(const Value: boolean);


  public
    function Initialise: Boolean; override;
    function Poulate(AStationID: integer;AIdentifier : integer;AAvgFlow,AScaleFactor,ACompensationValue,
                     ACapacityOfDiversion: double; AQualityCode : integer) : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property StationID : integer read Get_StationID;
    property Identifier : integer read Get_Identifier write FIdentifier;
    property InstreamDate : TDate read Get_InstreamDate write Set_InstreamDate;
    property AvgFlow : double read Get_AvgFlow write Set_AvgFlow;
    property QualityCode : integer read Get_QualityCode write Set_QualityCode;
    property FactoredInstreamFlow : double read Get_FactoredInstreamFlow;
    property DailyAvailableFlow[AMonth : integer] : double read Get_DailyAvailableFlow;
    property DailyDiversionFlow[AMonth : integer] : double read Get_DailyDiversionFlow;
    property NonDailyDiversionFlow[AMonth : integer] : double read Get_NonDailyDiversionFlow;
    property DailyData : TDailyFlowData read Get_DailyData write Set_DailyData;
    property HasGaps : boolean read Get_HasGaps write Set_HasGaps;
    property MonthlyThreshold : boolean read Get_MonthlyThreshold write Set_MonthlyThreshold;
  end;

  TFlowDiversionRelationship = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FRelationDate : TDateTime;
    FReferenceFlow : double;
    FDiversionFlow : double;
    FNonDiversionFlow : double;
    FIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_DiversionFlow: double;
    function Get_ReferenceFlow: double;
    function Get_RelationDate: TDateTime;
    function Get_NonDiversionFlow:double;
    procedure Set_NonDiversionFlow(const Value: double);
    procedure Set_DiversionFlow(const Value: double);
    procedure Set_ReferenceFlow(const Value: double);
    procedure Set_RelationDate(const Value: TDateTime);

  public
    function Initialise: Boolean; override;
    function Populate(AIdentifier : integer;AStationID : integer;ARelationDate : TDateTime;
                      AReferenceFlow, ADiversionFlow, ANonDiversionFlow : double) : boolean;

    property Identifier : integer read FIdentifier;
    property RelationDate : TDateTime read Get_RelationDate write Set_RelationDate;
    property ReferenceFlow : double read Get_ReferenceFlow write Set_ReferenceFlow;
    property DiversionFlow : double read Get_DiversionFlow write Set_DiversionFlow;
    property NonDiversionFlow : double read Get_NonDiversionFlow write Set_NonDiversionFlow;
  end;

  TWRYMChannelData = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FReferenceFlow : double;
    FDiversionFlow : double;
    FNonDiversionFlow : double;
    FDivFlowValueEdited : WordBool;
    FNonDivFlowValueEdited :WordBool;
    FRefFlowValueEdited : WordBool;
    FIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_DiversionFlow: double;
    function Get_ReferenceFlow: double;
    function Get_DivFlowValueEdited: WordBool;
    function Get_RefFlowValueEdited: WordBool;

    function Get_NonDivFlowValueEdited: WordBool;
    function Get_NonDiversionFlow: double;

    procedure Set_DivFlowValueEdited(const Value: WordBool);
    procedure Set_RefFlowValueEdited(const Value: WordBool);
    procedure Set_DiversionFlow(const Value: double);
    procedure Set_ReferenceFlow(const Value: double);

    procedure Set_NonDivFlowValueEdited(const Value: WordBool);
    procedure Set_NonDiversionFlow(const Value: double);

  public
    function Initialise: Boolean; override;
    function Populate(AIdentifier : integer;AStationID : integer) : boolean;
    function PopulateAll(AIdentifier : integer;AStationID : integer;
                         AReferenceFlow : double; ADiversionFlow : double;
                         ADivFlowValueEdited : string;ARefFlowValueEdited : string;
                         ANonDiversionFlow : double;ANonDivFlowValueEdited : string) : boolean;
    property Identifier : integer read FIdentifier;
    property ReferenceFlow : double read Get_ReferenceFlow write Set_ReferenceFlow;
    property RefFlowValueEdited : WordBool read Get_RefFlowValueEdited write Set_RefFlowValueEdited;
    property DiversionFlow : double read Get_DiversionFlow write Set_DiversionFlow;
    property DivFlowValueEdited : WordBool read Get_DivFlowValueEdited write Set_DivFlowValueEdited;

    property NonDiversionFlow : double read Get_NonDiversionFlow write Set_NonDiversionFlow;
    property NonDivFlowValueEdited : WordBool read Get_NonDivFlowValueEdited write Set_NonDivFlowValueEdited;

  end;


  TDiversionGauge = class(TAbstractAppObject)
  protected
    FStationNo : WideString;
    FStationID : integer;
    FPlace : WideString;
    FLatitude : WideString;
    FLongitude : WideString;
    FCatchmentArea : double;
    FCatchmentScaleFactor : double;
    FCapacityOfDiversion : double;
    FScaleFactor : double;
    FStartDate : TDateTime;
    FEndDate : TDateTime;
    FCompensationValues : TMonthlyDoubleArray;
    FMonthlyThreshold : TMonthlyDoubleArray;
    FNumberOfGabsByMonth : TStringList;
    FWRYMStartDate : TDateTime;
    FWRYMEndDate : TDateTime;

    FDailyFlowData : TObjectList;
    FSuspectDailyFlowData : TObjectList;
    FNonSuspectDailyFlowData : TObjectList;

    FDailyFlowDataWithInfilledGaps : TObjectList;
    FDaysWithGabsList : TStringList;
    FMonthlyFlowData : TObjectList;
    FInstreamFlowData : TObjectList;
    FMonthlyInstreamFlowData : TObjectList;
    FFlowDiversionRelationship : TObjectList;
    FRankedFlowDiversionRelationship : TObjectList;
    FWRYMData : TObjectList;
    FImportIFR : boolean;
    FIncludeAll,
    FInfillGaps,
    FInclundeOnlyGoodDailyData,
    FExcludeSuspectDailyData : boolean;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_CatchmentArea: double;safecall;
    function Get_CatchmentScaleFactor: double;safecall;
    function Get_Latitude: WideString;safecall;
    function Get_Longitude: WideString;safecall;
    function Get_Place: WideString;safecall;
    function Get_StationNo: WideString;safecall;
    function Get_StationID: integer;safecall;
    procedure Set_CatchmentArea(const Value: double);safecall;
    procedure Set_CatchmentScaleFactor(const Value: double);safecall;
    procedure Set_Latitude(const Value: WideString);safecall;
    procedure Set_Longitude(const Value: WideString);safecall;
    procedure Set_Place(const Value: WideString);safecall;
    procedure Set_ImportIFR(const Value: WordBool);
    function Get_ImportIFR: WordBool;
    function Get_DailyDiversionGaugeByIndex(AIndex: integer): TDailyFlowData;safecall;
    function Get_NonSuspectDailyFlowDataByIndex(AIndex: integer): TDailyFlowData;safecall;
    function Get_DailyDataCount: integer; safecall;
    function Get_SuspectDailyFlowDataCount: integer; safecall;
    function Get_NonSuspectDailyFlowDataCount: integer;

    function Get_DailyInstreamDataCount: integer;
    function GetMonthlyFlowDataByYear(AYear: integer): TMonthlyFlowData;
    function Get_MonthlyFlowDataByMonthYear(AMonth,AYear: integer): double;
    function GetMonthlyInstreamFlowDataByYear(AYear: integer) : TMonthlyInstreamFlowData;
    function Get_MonthlyDailyFlowCount: integer;
    function Get_MonthlyInstreamFlowCount: integer;
    function GetMonthlyFlowDataByIndex(AIndex: integer): TMonthlyFlowData;
    function Get_MonthlyInstreamFlowDataByIndex(AIndex : integer) : TMonthlyInstreamFlowData;
    function Get_CapacityOfDiversion: double;
    function Get_CompensationValueByIndex(AIndex: integer): double;
    function Get_EndDate: TDateTime;
    function Get_InstreamScaleFactor: double;
    function Get_StartDate: TDateTime;
    function Get_WRYMStartDate: TDateTime;
    function Get_WRYMEndDate: TDateTime;
    procedure Set_CapacityOfDiversion(const Value: double);
    procedure Set_CompensationValueByIndex(AIndex: integer;const Value: double);
    procedure Set_EndDate(const Value: TDateTime);
    procedure Set_InstreamScaleFactor(const Value: double);
    procedure Set_StartDate(const Value: TDateTime);
    procedure Set_WRYMStartDate(const Value: TDateTime);
    procedure Set_WRYMEndDate(const Value: TDateTime);
    function Get_InstreamFlowData(AIndex: integer): TDailyInstreamFlowData;
    function Get_FlowDiversionRelationshipCount : integer;
    function Get_RankedFlowDiversionRelationshipCount : integer;
    function Get_WRYMDataCount : integer;
    function Get_RankedFlowDiversionRelationshipByIndex(AIndex: integer): TFlowDiversionRelationship;
    function Get_UnRankedFlowDiversionRelationshipByIndex(AIndex: integer): TFlowDiversionRelationship;
    function Get_WRYMDataByIndex(AIndex: integer): TWRYMChannelData;
    function Get_InfillGaps : boolean;
    procedure Set_InfillGaps(AInfillGaps : boolean);
    function Get_ThresholdByMonth(AMonth: integer): double;
    procedure Set_ThresholdByMonth(AMonth: integer; const Value: double);
    function Get_NumberOfGabsByMonth(AYear,AMonth: integer): integer;
    procedure SetNumberOfGabsPerMonth;
  public
    function GetDailyFlowDataByDate(ADate : TDateTime) : TDailyFlowData;
    function PopulateGaugeData(AStationNo : WideString;AStationID : integer;APlace, ALatitude,
                               ALongitude: WideString; ACatchmentArea: double; ACatchmentScaleFactor : double): boolean;
    function PopulateCompensationValues(ACapacityOfDiversion : double;AScaleFactor : double;AStartDate : TDateTime;
                                           AEndDate : TDateTime;ACompensationValues : TMonthlyDoubleArray): boolean;
    function PopulateThresholdValues(AMonthlyThreshold : TMonthlyDoubleArray): boolean;
    function Initialise: Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function NewDailyInstreamFlowData(ADate : TDateTime) : TDailyInstreamFlowData;
    function NewDailyFlowData : TDailyFlowData; safecall;

    function AddDailyFlowData : TDailyFlowData;
    function AddSuspectDailyFlowData: TDailyFlowData;
    function AddNonSuspectDailyFlowData: TDailyFlowData;

    function AddDailyFlowDataGaps : boolean;
    function AddInstreamFlowData(ADate : TDateTime) : TDailyInstreamFlowData;
    function GetDailyInstreamFlowDataByDate(ADate : TDateTime) : TDailyInstreamFlowData;
    procedure AddMonthlyFlowData(AYear,AMonth,AQualityCode: integer; AAvgFlow: Double;ADiversionDate: string;AStationID : integer);
    procedure AddMonthlyInstreamFlowData(AYear,AMonth,AQualityCode: integer; AAvgFlow, ADailyDiversion: Double;AStationID : integer);
    function ClearFlowDiversionRelationship : boolean;
    function ClearDailyInstreamFlowFileData: boolean;
    function ClearDailyFlowData: boolean;
    function AddFlowDiversionRelationship : TFlowDiversionRelationship;
    function AddRankedFlowDiversionRelationship : TFlowDiversionRelationship;
    function AddWRYMData(var AIdentifier : integer) : TWRYMChannelData;
    function CreateWRYMData : TWRYMChannelData;
    function GetInfilledDailyDataByDate(ADate : TDateTime): TDailyFlowData;
    function RemoveDailyData(AID: integer) : boolean;
    function RemoveDailyInstreamData(AID: integer): boolean;
    function DeleteDailyFlowDataById(AIdentifier : integer) : boolean;
    function DeleteDailyInstreamFlowDataById(AIdentifier : integer) : boolean;
    function GetDailyDiversionDataByMonth(AMonth : integer; AData : TStrings) : WordBool;

    property StationNo : WideString read Get_StationNo;
    property StationID : integer read Get_StationID;
    property Place : WideString read Get_Place write Set_Place;
    property Latitude : WideString read Get_Latitude write Set_Latitude;
    property Longitude : WideString read Get_Longitude write Set_Longitude;
    property CatchmentArea : double read Get_CatchmentArea write Set_CatchmentArea;
    property CatchmentScaleFactor : double read Get_CatchmentScaleFactor write Set_CatchmentScaleFactor;
    property DailyDataCount : integer read Get_DailyDataCount;
    property DailyInstreamDataCount : integer read Get_DailyInstreamDataCount;
    property MonthlyDailyFlowCount : integer read Get_MonthlyDailyFlowCount;
    property MonthlyInstreamFlowCount : integer read Get_MonthlyInstreamFlowCount;
    property DailyFlowDataByIndex[AIndex : integer] : TDailyFlowData  read Get_DailyDiversionGaugeByIndex;
    property MonthlyFlowDataByYear[AYear : integer] : TMonthlyFlowData read GetMonthlyFlowDataByYear;
    property MonthlyFlowDataByMonthYear[AMonth,AYear : integer] : double read Get_MonthlyFlowDataByMonthYear;
    property MonthlyFlowDataByIndex[AIndex : integer] : TMonthlyFlowData read GetMonthlyFlowDataByIndex;
    property MonthlyInstreamFlowDataByIndex[AIndex : integer] : TMonthlyInstreamFlowData read Get_MonthlyInstreamFlowDataByIndex;
    property InstreamFlowDataByIndex[AIndex : integer] : TDailyInstreamFlowData read Get_InstreamFlowData;
    property CapacityOfDiversion : double read Get_CapacityOfDiversion write Set_CapacityOfDiversion;
    property InstreamScaleFactor : double read Get_InstreamScaleFactor write Set_InstreamScaleFactor;
    property StartDate : TDateTime read Get_StartDate write Set_StartDate;
    property EndDate : TDateTime read Get_EndDate write Set_EndDate;
    property CompensationValueByIndex[AIndex : integer] : double read Get_CompensationValueByIndex write Set_CompensationValueByIndex;
    property WRYMStartDate : TDateTime read Get_WRYMStartDate write Set_WRYMStartDate;
    property WRYMEndDate : TDateTime read Get_WRYMEndDate write Set_WRYMEndDate;
    property FlowDiversionRelationshipCount : integer read Get_FlowDiversionRelationshipCount;
    property RankedFlowDiversionRelationshipCount : integer read Get_RankedFlowDiversionRelationshipCount;
    property WRYMDataCount : integer read Get_WRYMDataCount;
    property UnRankedFlowDiversionRelationshipByIndex[AIndex : integer] : TFlowDiversionRelationship read Get_UnRankedFlowDiversionRelationshipByIndex;
    property RankedFlowDiversionRelationshipByIndex[AIndex : integer] : TFlowDiversionRelationship read Get_RankedFlowDiversionRelationshipByIndex;
    property WRYMDataByIndex[AIndex : integer] : TWRYMChannelData read Get_WRYMDataByIndex;
    property ImportIFR : WordBool read Get_ImportIFR write Set_ImportIFR;

    property InclundeOnlyGoodDailyData : boolean read FInclundeOnlyGoodDailyData write FInclundeOnlyGoodDailyData;
    property ExcludeSuspectDailyData : boolean read FExcludeSuspectDailyData write FExcludeSuspectDailyData;
    property IncludeAll : boolean read FIncludeAll write FIncludeAll;
    property InfillGaps : boolean read Get_InfillGaps write Set_InfillGaps;
    property ThresholdByMonth[AMonth : integer] : double read Get_ThresholdByMonth write Set_ThresholdByMonth;
    property NumberOfGabsByMonth[AYear,AMonth : integer] : integer read Get_NumberOfGabsByMonth;
  end;

  TDailyDiversionGaugeDataList = class(TAbstractAppObject)
  protected
    FDiversionGaugeList : TObjectList;
    //FF14DailyIFRData : TDailyIFRDataList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_DiversionGaugeByIndex(AIndex: integer): TDiversionGauge;safecall;
    function Get_DiversionGaugeByStationNo(AStationNo: WideString): TDiversionGauge;safecall;
    function Get_DailyDiversionGaugeData: integer;
    function Get_DiversionGaugeByStationID(AStationID: integer): TDiversionGauge;safecall;
    //function Get_DailyIFRDataFromFile14ByStation : TDailyIFRDataList;safecall;
  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: Boolean; override;
    function NewDiversionGauge(AStationNo : WideString) : TDiversionGauge;safecall;
    function RenameDiversionGauge(ANewStationNo : WideString;AOldStationNo : WideString) : WordBool;safecall;
    function RemoveDiversionGauge(AStationID : integer) : WordBool; safecall;
    function AddDiversionGauge : TDiversionGauge;
    //function AddDailyIFRData(AStationNo : WideString) : TDailyIFRData;
    function DeleteDiversionGaugeByStationNo(AStationNo : WideString) : boolean;
    property DailyDiversionGaugeDataCount : integer read Get_DailyDiversionGaugeData;
    property DiversionGaugeByIndex[AIndex : integer] : TDiversionGauge read Get_DiversionGaugeByIndex;
    property DiversionGaugeByStationNo[AStationNo : WideString] : TDiversionGauge read Get_DiversionGaugeByStationNo;
    property DiversionGaugeByStationID[AStationID : integer] : TDiversionGauge read Get_DiversionGaugeByStationID;
    //property DailyIFRDataFromFile14ByStation : TDailyIFRDataList read Get_DailyIFRDataFromFile14ByStation;

end;

implementation
uses
  System.Types,
  System.UITypes,
  VCL.Dialogs,
  Math,
  UDailyDiversionDataObject,
  UConstants,
  UDailyDiversionGaugeSQLAgent,
  UErrorHandlingOperations, Variants;

{ TDailyFlowData }

procedure TDailyFlowData.CreateMemberObjects;
const OPNAME = 'TDailyFlowData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowData.DestroyMemberObjects;
const OPNAME = 'TDailyFlowData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData._AddRef: Integer;
const OPNAME = 'TDailyFlowData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyFlowData._Release: Integer;
const OPNAME = 'TDailyFlowData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDailyFlowData.Get_AvgFlow: double;
const OPNAME = 'TDailyFlowData.Get_AvgFlow';
begin
  Result := NullFloat;
  try
    Result := FAvgFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Get_DiversionDate: TDate;
const OPNAME = 'TDailyFlowData.Get_DiversionDate';
begin
  Result := 0;
  try
    Result := FDiversionDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Get_Identifier: integer;
const OPNAME = 'TDailyFlowData.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Get_QualityCode: integer;
const OPNAME = 'TDailyFlowData.Get_QualityCode';
begin
  Result := 0;
  try
    Result := FQualityCode;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Get_StationID: integer;
const OPNAME = 'TDailyFlowData.Get_StationID';
begin
  Result := 0;
  try
    Result := FStationID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Poulate(AStationID: integer;
  AIdentifier: integer; ADiversionDate: TDate; AAvgFlow,ACatchmentFactor: double;
  AQualityCode: integer): boolean;
const OPNAME = 'TDailyFlowData.Poulate';
begin
  Result := False;
  try
    FStationID := AStationID;
    FIdentifier := AIdentifier;
    FDiversionDate := ADiversionDate;
    FAvgFlow := AAvgFlow;
    FCatchmentFactor := ACatchmentFactor;
    FQualityCode := AQualityCode;
    
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowData.Set_AvgFlow(const Value: double);
const OPNAME = 'TDailyFlowData.Set_AvgFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FAvgFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('AvgFlow', FloatToStr(Value), FloatToStr(FAvgFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FAvgFlow);
          FAvgFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'AvgFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowData.Set_DiversionDate(const Value: TDate);
const OPNAME = 'TDailyFlowData.Set_DiversionDate';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FDiversionDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),InttoStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('DiversionDate', DateToStr(Value), DateToStr(FDiversionDate), LContextData) then
        begin
          LOldValue := DateToStr(FDiversionDate);
          FDiversionDate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DiversionDate',LOldValue,DateToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyFlowData.Set_QualityCode(const Value: integer);
const OPNAME = 'TDailyFlowData.Set_QualityCode';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FDiversionDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('QualityCode', IntToStr(Value), IntToStr(FQualityCode), LContextData) then
        begin
          LOldValue := IntToStr(FQualityCode);
          FQualityCode := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'QualityCode',LOldValue,IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDailyFlowData.Validate';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Get_FactoredFlow: double;
const OPNAME = 'TDailyFlowData.Get_FactoredFlow';
var
  LYear,LMonth,LDay : word;
  LDiversionGauge : TDiversionGauge;
  LMonthlyFlowData : TMonthlyFlowData;
  LDailyData : TDailyFlowData;
  LHydroYear : integer;
begin
  Result := 0.0;
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if LDiversionGauge <> nil then
    begin

      if FAvgFlow = NullFloat then
      begin
        DecodeDate(FDiversionDate,LYear,LMonth,LDay);
        if LMonth < 10 then
          LHydroYear := LYear -1
        else
          LHydroYear := LYear;
        LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LHydroYear];
        if (LMonthlyFlowData <> nil) and (LDiversionGauge.InfillGaps = True) then
        begin
          if LMonthlyFlowData.MonthlyThresholdVerified(LMonth) then
          begin
            LDailyData := LDiversionGauge.GetInfilledDailyDataByDate(FDiversionDate);
            if LDailyData <> nil then
              Result := LDailyData.AvgFlow * LDiversionGauge.FCatchmentScaleFactor;
          end;
        end
        else
          Result := 0 * LDiversionGauge.FCatchmentScaleFactor
      end
      else
        Result := FAvgFlow * LDiversionGauge.FCatchmentScaleFactor;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyFlowData.Initialise: Boolean;
const OPNAME = 'TDailyFlowData.Initialise';
begin
  Result := False;
  try
    FStationID := 0;
    FIdentifier  := 0;
    FDiversionDate := 0;
    FAvgFlow     := NullFloat;
    FQualityCode := 0;
    FCatchmentFactor := 1.0;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TDiversionGauge }

function TDiversionGauge.AddDailyFlowData: TDailyFlowData;
const OPNAME = 'TDiversionGauge.AddDailyFlowData';
begin
  Result := nil;
  try
    Result := TDailyFlowData.Create(FAppModules);
    FDailyFlowData.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddSuspectDailyFlowData: TDailyFlowData;
const OPNAME = 'TDiversionGauge.AddSuspectDailyFlowData';
begin
  Result := nil;
  try
    Result := TDailyFlowData.Create(FAppModules);
    FSuspectDailyFlowData.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddNonSuspectDailyFlowData: TDailyFlowData;
const OPNAME = 'TDiversionGauge.AddNonSuspectDailyFlowData';
begin
  Result := nil;
  try
    Result := TDailyFlowData.Create(FAppModules);
    FNonSuspectDailyFlowData.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.AddMonthlyFlowData(AYear,AMonth,AQualityCode: integer; AAvgFlow: Double;ADiversionDate: string;AStationID : integer);
const OPNAME = 'TDiversionGauge.AddMonthlyFlowData';
var
  LMonthlyFlowData: TMonthlyFlowData;
  LAAvgFlow : double;
  LHydroYear : integer;
begin
  try
    if AMonth < 10 then
      LHydroYear := AYear -1
    else
      LHydroYear := AYear;

    LMonthlyFlowData := GetMonthlyFlowDataByYear(LHydroYear);
    if(LMonthlyFlowData = nil) then
    begin
      LMonthlyFlowData := TMonthlyFlowData.Create(FAppModules);
      LMonthlyFlowData.Initialise;
      LMonthlyFlowData.FYear := LHydroYear;
      LMonthlyFlowData.FStationID := AStationID;
      FMonthlyFlowData.Add(LMonthlyFlowData);
    end;
    if AAvgFlow = NullFloat then
    begin
      LAAvgFlow := 0.0;
      FDaysWithGabsList.Add(ADiversionDate);
    end
    else
      LAAvgFlow := AAvgFlow;
    LMonthlyFlowData.FIncludeAllMonthTotal[AMonth] := LMonthlyFlowData.FIncludeAllMonthTotal[AMonth] + LAAvgFlow;
    if (AQualityCode in [1,2,3,4,5,6,7,50,60,65,66,91,150]) then
      LMonthlyFlowData.FGoodMonthTotal[AMonth] := LMonthlyFlowData.FGoodMonthTotal[AMonth] + LAAvgFlow
    else
      LMonthlyFlowData.FSuspectMonthTotal[AMonth] := LMonthlyFlowData.FSuspectMonthTotal[AMonth] + LAAvgFlow;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionGauge.GetInfilledDailyDataByDate(ADate : TDateTime): TDailyFlowData;
const OPNAME = 'TDiversionGauge.GetInfilledDailyDataByDate';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDailyFlowDataWithInfilledGaps.Count -1 do
    begin
      if TDailyFlowData(FDailyFlowDataWithInfilledGaps.Items[LIndex]).DiversionDate = ADate then
      begin
        Result := TDailyFlowData(FDailyFlowDataWithInfilledGaps.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddDailyFlowDataGaps : boolean;
const OPNAME = 'TDiversionGauge.AddDailyFlowDataGaps';
var
  LInfilledDailyData,
  LDailyDataWithoutAGap,
  LDailyDataWithAGap: TDailyFlowData;
  LMonthlyFlowData : TMonthlyFlowData;
  LYear,LMonth,LDay : word;
  LIndex : integer;
  LPreDayValue : double;
  LLastDayValue : double;
  LGabValue : double;
  LGapsCount : integer;
  LLastDayWithData : TDateTime;
  LHydroYear : word;
  function GetLastValue : double;
  const OPNAME = 'GetLastValue';
  var
    LDailyData : TDailyFlowData;
  begin
    Result := 0;
    if (LGapsCount > 0) or (LGapsCount < 0) then
      LGapsCount := 0;
    LLastDayWithData := LLastDayWithData + 1;
    while (Result = 0) and (LGapsCount <= FDaysWithGabsList.Count) do
    begin
      LDailyData := GetDailyFlowDataByDate(LLastDayWithData);
      if (LDailyData <> nil) and (LDailyData.FAvgFlow <> NullFloat) then
      begin
        Result := LDailyData.FAvgFlow;
        Break;
      end
      else
      begin
        LGapsCount := LGapsCount + 1;
        LLastDayWithData := LLastDayWithData + 1;
      end;
    end;
  end;
begin
  Result := False;
  try
    LLastDayValue := 0;
    for LIndex := 0 to  FDaysWithGabsList.Count -1 do
    begin
      Decodedate((StrToDate(FDaysWithGabsList[LIndex])),LYear,LMonth,LDay);
      if LMonth < 10 then
        LHydroYear := LYear -1
      else
        LHydroYear := LYear;
      LMonthlyFlowData := GetMonthlyFlowDataByYear(LHydroYear);
      if (LMonthlyFlowData <> nil) then
      begin
        LDailyDataWithAGap := GetDailyFlowDataByDate(StrToDate(FDaysWithGabsList[LIndex]));
        if ((LIndex = 0) and (LDailyDataWithAGap <> nil)) or ((LDailyDataWithAGap <> nil)
          and (GetDailyFlowDataByDate(LDailyDataWithAGap.FDiversionDate-1).FAvgFlow <> NullFloat) ) then
        begin
          LLastDayWithData := LDailyDataWithAGap.FDiversionDate-1;
          LDailyDataWithoutAGap := GetDailyFlowDataByDate(LLastDayWithData);
          if LDailyDataWithoutAGap = nil then
          begin
            LDailyDataWithoutAGap :=  TDailyFlowData.Create(FAppModules);
            LDailyDataWithoutAGap.FStationID := -1;
            LDailyDataWithoutAGap.FDiversionDate := LDailyDataWithAGap.FDiversionDate-1;
            LDailyDataWithoutAGap.FIdentifier := LDailyDataWithAGap.FIdentifier -1;
            LDailyDataWithoutAGap.FAvgFlow := 0;
            LDailyDataWithoutAGap.FQualityCode := 999;
            LGapscount := -1;
            FDailyFlowData.add(LDailyDataWithoutAGap);
          end;
        end
        else
          LDailyDataWithoutAGap := GetInfilledDailyDataByDate(StrToDate(FDaysWithGabsList[LIndex])-1);
        if LDailyDataWithoutAGap <> nil then
        begin
          LInfilledDailyData := TDailyFlowData.Create(FAppModules);
          FDailyFlowDataWithInfilledGaps.Add(LInfilledDailyData);
          LPreDayValue :=  LDailyDataWithoutAGap.FAvgFlow;
          if (GetDailyFlowDataByDate(LDailyDataWithAGap.FDiversionDate-1).FAvgFlow <> NullFloat) then
            LLastDayValue := GetLastValue;
          LGabValue := 0;
          if LGapscount > 0 then
            LGabValue  := LPreDayValue - ((LPreDayValue - LLastDayValue)/(LGapscount+1));
          LGapscount := LGapscount -1;
          if LDailyDataWithAGap <> nil then
          begin
            LInfilledDailyData.FAvgFlow := LGabValue;
            LMonthlyFlowData.FInfilledMonthTotal[LMonth] := LMonthlyFlowData.FInfilledMonthTotal[LMonth] + LGabValue;
            LInfilledDailyData.FStationID := LMonthlyFlowData.FStationID;
            LInfilledDailyData.FDiversionDate := LDailyDataWithAGap.FDiversionDate;
            LInfilledDailyData.FIdentifier := LDailyDataWithAGap.FIdentifier;
            LInfilledDailyData.FQualityCode := 999;
            if (LDailyDataWithoutAGap.FStationID = -1) and (LIndex = 0) then
              FDailyFlowData.Remove(LDailyDataWithoutAGap);
          end;
        end;
      end;
    end;
    SetNumberOfGabsPerMonth;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.SetNumberOfGabsPerMonth;
const OPNAME = 'TDiversionGauge.SetNumberOfGabsPerMonth';
var
  LNumberOfGabsByMonth : integer;
  LCurrentYear,LCurrentMonth : integer;
  LIndex : integer;
  LYear,LMonth,LDay : word;
  LHydroYear : word;
  LData : TStringList;
begin
  try
    LNumberOfGabsByMonth := 0;
    LData := TStringList.Create;
    try
      LData.Assign(FDaysWithGabsList);
      if LData.Count > 0 then
      begin
         LCurrentYear := 0;
         LCurrentMonth := 0;
        for LIndex := 0 to LData.Count-1 do
        begin
          Decodedate((StrToDate(LData[LIndex])),LYear,LMonth,LDay);
          if LMonth < 10 then
            LHydroYear := LYear -1
          else
            LHydroYear := LYear;

          if ((LCurrentYear <> LHydroYear) and (LCurrentMonth <> LMonth)) or
            ((LCurrentYear <> LHydroYear) and (LCurrentMonth = LMonth)) or
            ((LCurrentYear = LHydroYear) and (LCurrentMonth <> LMonth)) then
          begin
            if ((LIndex > 0) and (FNumberOfGabsByMonth.IndexOf(IntToStr(LCurrentYear)+'_'+IntToStr(LCurrentMonth)) < 0)) then
              FNumberOfGabsByMonth.AddObject(IntToStr(LCurrentYear)+'_'+IntToStr(LCurrentMonth),TObject(LNumberOfGabsByMonth));
            LNumberOfGabsByMonth := 0;
            LCurrentYear := LHydroYear;
            LCurrentMonth := LMonth;
          end;
          LNumberOfGabsByMonth := LNumberOfGabsByMonth + 1;
          if ((LIndex = LData.Count-1)and (FNumberOfGabsByMonth.IndexOf(IntToStr(LCurrentYear)+'_'+IntToStr(LCurrentMonth)) < 0))then
            FNumberOfGabsByMonth.AddObject(IntToStr(LCurrentYear)+'_'+IntToStr(LCurrentMonth),TObject(LNumberOfGabsByMonth));
        end;
      end;
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.AddMonthlyInstreamFlowData(AYear,AMonth,AQualityCode: integer; AAvgFlow,ADailyDiversion: Double;AStationID : integer);
const OPNAME = 'TDiversionGauge.AddMonthlyInstreamFlowData';
var
  LMonthlyInstreamFlowData: TMonthlyInstreamFlowData;
  LHydroYear : integer;
begin
  try
    if AMonth < 10 then
      LHydroYear := AYear -1
    else
      LHydroYear := AYear;

    LMonthlyInstreamFlowData := GetMonthlyInstreamFlowDataByYear(LHydroYear);
    if(LMonthlyInstreamFlowData = nil) then
    begin
      LMonthlyInstreamFlowData := TMonthlyInstreamFlowData.Create(FAppModules);
      LMonthlyInstreamFlowData.FYear := LHydroYear;
      LMonthlyInstreamFlowData.FStationID := AStationID;
      FMonthlyInstreamFlowData.Add(LMonthlyInstreamFlowData);
    end;
    LMonthlyInstreamFlowData.FDailyDiversionTotal[AMonth] := LMonthlyInstreamFlowData.FDailyDiversionTotal[AMonth] +  ADailyDiversion;
    LMonthlyInstreamFlowData.FMonthTotal[AMonth] := LMonthlyInstreamFlowData.FMonthTotal[AMonth] + AAvgFlow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionGauge._AddRef: Integer;
const OPNAME = 'TDiversionGauge._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDiversionGauge._Release: Integer;
const OPNAME = 'TDiversionGauge._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDiversionGauge.CreateMemberObjects;
const OPNAME = 'TDiversionGauge.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FDailyFlowData := TObjectList.Create(False);
    FSuspectDailyFlowData := TObjectList.Create(False);
    FNonSuspectDailyFlowData :=TObjectList.Create(False);
    FMonthlyFlowData := TObjectList.Create(False);
    FDaysWithGabsList := TStringList.Create;
    FInstreamFlowData := TObjectList.Create(False);
    FMonthlyInstreamFlowData := TObjectList.Create(False);
    FDailyFlowDataWithInfilledGaps := TObjectList.Create(False);
    FFlowDiversionRelationship := TObjectList.Create(False);
    FRankedFlowDiversionRelationship := TObjectList.Create(False);
    FWRYMData := TObjectList.Create(False);
    FNumberOfGabsByMonth := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.DeleteDailyFlowDataById(AIdentifier: integer): boolean;
const OPNAME = 'TDiversionGauge.DeleteDailyFlowDataById';
var
  LIndex : integer;
begin
  Result := False;
  try
    if ExcludeSuspectDailyData then
    begin
      for LIndex := 0 to FNonSuspectDailyFlowData.Count - 1 do
      begin
        if TDailyFlowData(FNonSuspectDailyFlowData.Items[LIndex]).FIdentifier = AIdentifier then
        begin
          FNonSuspectDailyFlowData.Remove(TDailyFlowData(FNonSuspectDailyFlowData.Items[LIndex]));
          FAppModules.Model.StudyDataHasChanged(sdccDelete,'AvgFlow',
          FloatToStr(TDailyFlowData(FNonSuspectDailyFlowData.Items[LIndex]).AvgFlow),
          FloatToStr(TDailyFlowData(FNonSuspectDailyFlowData.Items[LIndex]).AvgFlow));
          Result := True;
          Exit;
        end;
      end;
    end;

    for LIndex := 0 to FDailyFlowData.Count - 1 do
    begin
      if TDailyFlowData(FDailyFlowData.Items[LIndex]).FIdentifier = AIdentifier then
      begin
        FDailyFlowData.Remove(TDailyFlowData(FDailyFlowData.Items[LIndex]));
        FAppModules.Model.StudyDataHasChanged(sdccDelete,'AvgFlow',
        FloatToStr(TDailyFlowData(FDailyFlowData.Items[LIndex]).AvgFlow),
        FloatToStr(TDailyFlowData(FDailyFlowData.Items[LIndex]).AvgFlow));
        Result := True;
        Break;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.DeleteDailyInstreamFlowDataById(AIdentifier : integer) : boolean;
const OPNAME = 'TDiversionGauge.DeleteDailyInstreamFlowDataById';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FInstreamFlowData.Count - 1 do
    begin
      if TDailyInstreamFlowData(FInstreamFlowData.Items[LIndex]).FIdentifier = AIdentifier then
      begin
        FInstreamFlowData.Remove(TDailyInstreamFlowData(FInstreamFlowData.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.DestroyMemberObjects;
const OPNAME = 'TDiversionGauge.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDailyFlowData);
    FreeAndNil(FSuspectDailyFlowData);
    FreeAndNil(FNonSuspectDailyFlowData);
    FreeAndNil(FMonthlyFlowData);
    FreeAndNil(FDaysWithGabsList);
    FreeAndNil(FInstreamFlowData);
    FreeAndNil(FMonthlyInstreamFlowData);
    FreeAndNil(FDailyFlowDataWithInfilledGaps);
    FreeAndNil(FFlowDiversionRelationship);
    FreeAndNil(FRankedFlowDiversionRelationship);
    FreeAndNil(FNumberOfGabsByMonth);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.GetMonthlyFlowDataByIndex(AIndex: integer): TMonthlyFlowData;
const OPNAME = 'TDiversionGauge.GetMonthlyFlowDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMonthlyFlowData.Count) then
      Result := TMonthlyFlowData(FMonthlyFlowData[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_MonthlyInstreamFlowDataByIndex(AIndex : integer) : TMonthlyInstreamFlowData;
const OPNAME = 'TDiversionGauge.Get_MonthlyInstreamFlowDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMonthlyInstreamFlowData.Count) then
      Result := TMonthlyInstreamFlowData(FMonthlyInstreamFlowData[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.GetMonthlyFlowDataByYear(AYear: integer): TMonthlyFlowData;
const OPNAME = 'TDiversionGauge.GetMonthlyFlowDataByYear';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0  to FMonthlyFlowData.Count-1 do
    begin
      if(TMonthlyFlowData(FMonthlyFlowData.Items[LIndex]).FYear = AYear) then
      begin
        Result := TMonthlyFlowData(FMonthlyFlowData.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_MonthlyFlowDataByMonthYear(AMonth,AYear: integer): double;
const OPNAME = 'TDiversionGauge.Get_MonthlyFlowDataByMonthYear';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0  to FMonthlyFlowData.Count-1 do
    begin
      if (TMonthlyFlowData(FMonthlyFlowData.Items[LIndex]).FYear = AYear) then
      begin
        Result := TMonthlyFlowData(FMonthlyFlowData.Items[LIndex]).AvgFlowByIndex[AMonth];
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.GetMonthlyInstreamFlowDataByYear(AYear: integer) : TMonthlyInstreamFlowData;
const OPNAME = 'TDiversionGauge.GetMonthlyInstreamFlowDataByYear';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0  to FMonthlyInstreamFlowData.Count-1 do
    begin
      if(TMonthlyInstreamFlowData(FMonthlyInstreamFlowData.Items[LIndex]).FYear = AYear) then
      begin
        Result := TMonthlyInstreamFlowData(FMonthlyInstreamFlowData.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_CatchmentArea: double;
const OPNAME = 'TDiversionGauge.Get_CatchmentArea';
begin
  Result := NullFloat;
  try
    Result := FCatchmentArea;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_DailyDataCount: integer;
const OPNAME = 'TDiversionGauge.Get_DailyDataCount';
begin
  Result := 0;
  try
    if ExcludeSuspectDailyData then
      Result := Get_NonSuspectDailyFlowDataCount
    else
      Result := FDailyFlowData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_NonSuspectDailyFlowDataCount: integer;
const OPNAME = 'TDiversionGauge.Get_NonSuspectDailyFlowDataCount';
begin
  Result := 0;
  try
    Result := FNonSuspectDailyFlowData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_SuspectDailyFlowDataCount: integer;
const OPNAME = 'TDiversionGauge.Get_SuspectDailyFlowDataCount';
begin
  Result := 0;
  try
    Result := FSuspectDailyFlowData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_DailyInstreamDataCount: integer;
const OPNAME = 'TDiversionGauge.Get_DailyInstreamDataCount';
begin
  Result := 0;
  try
    Result := FInstreamFlowData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_DailyDiversionGaugeByIndex(AIndex: integer): TDailyFlowData;
const OPNAME = 'TDiversionGauge.Get_DailyDiversionGaugeByIndex';
begin
  Result := nil;
  try
    if ExcludeSuspectDailyData then
    begin
      Result := Get_NonSuspectDailyFlowDataByIndex(AIndex);
      Exit;
    end;

    if(AIndex >= 0) and (AIndex < FDailyFlowData.Count) then
      Result := TDailyFlowData(FDailyFlowData[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_NonSuspectDailyFlowDataByIndex(AIndex: integer): TDailyFlowData;
const OPNAME = 'TDiversionGauge.Get_NonSuspectDailyFlowDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FNonSuspectDailyFlowData.Count) then
      Result := TDailyFlowData(FNonSuspectDailyFlowData[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_Latitude: WideString;
const OPNAME = 'TDiversionGauge.Get_Latitude';
begin
  Result := '';
  try
    Result := FLatitude;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_Longitude: WideString;
const OPNAME = 'TDiversionGauge.Get_Longitude';
begin
  Result := '';
  try
    Result := FLongitude; 
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_MonthlyDailyFlowCount: integer;
const OPNAME = 'TDiversionGauge.Get_MonthlyDailyFlowCount';
begin
  Result := 0;
  try
    Result := FMonthlyFlowData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_MonthlyInstreamFlowCount: integer;
const OPNAME = 'TDiversionGauge.Get_MonthlyInstreamFlowCount';
begin
  Result := 0;
  try
    Result := FMonthlyInstreamFlowData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDiversionGauge.Get_Place: WideString;
const OPNAME = 'TDiversionGauge.Get_Place';
begin
  Result := ''; 
  try
    Result := FPlace;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_StationID: integer;
const OPNAME = 'TDiversionGauge.Get_StationID';
begin
  Result := 0;
  try
    Result := FStationID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_StationNo: WideString;
const OPNAME = 'TDiversionGauge.Get_StationNo';
begin
  Result := '';
  try
    Result := FStationNo;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Initialise: Boolean;
const OPNAME = 'TDiversionGauge.Initialise';
begin
  Result := False;
  try
    Place := '';
    Latitude := '';
    Longitude := '';
    CatchmentArea := 0;
    FScaleFactor := 1.0;
    FIncludeAll := True;
    FInclundeOnlyGoodDailyData := False;
    FExcludeSuspectDailyData := False;
    FNumberOfGabsByMonth.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.NewDailyFlowData: TDailyFlowData;
const OPNAME = 'TDiversionGauge.NewDailyFlowData';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LIndex : integer;
  LIdentifier : integer;
begin
  Result := nil;
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      for LIndex := 0 to FDailyFlowData.Count -1 do
      begin
        if TDailyFlowData(FDailyFlowData.Items[LIndex]).FDiversionDate = Date then
        begin
          MessageDlg(FAppModules.Language.GetString('Message.RecordWithSameDateExist'),mtWarning,[mbOK],0);
          Exit;
        end;
      end;

      if LLoadAgent.InsertDailyData(FStationID,LIdentifier) then
      begin
        Result := AddDailyFlowData;
        Result.FDiversionDate := StrToDate(FormatDateTime('yyyy/mm/dd', Now));
        Result.FIdentifier := LIdentifier;
        Result.FStationID := FStationID;
      end;
    finally
      FreeAndNil(LLoadAgent);
    end
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.NewDailyInstreamFlowData(ADate : TDateTime) : TDailyInstreamFlowData;
const OPNAME = 'TDiversionGauge.NewDailyInstreamFlowData';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LIdentifier : integer;
begin
  Result := nil;
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      LIdentifier := LLoadAgent.GetLastMaxDailyInstreamDataID(FStationID) + 1;
      if LLoadAgent.InsertDailyInstreamData(FStationID,LIdentifier,ADate,0,0) then
      begin
        Result := AddInstreamFlowData(Date);
        Result.FInstreamDate := StrToDate(FormatDateTime('yyyy/mm/dd', ADate));
        Result.FIdentifier := LIdentifier;
        Result.FStationID := FStationID;
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.PopulateGaugeData(AStationNo : WideString;AStationID : integer;APlace, ALatitude,
         ALongitude: WideString; ACatchmentArea: double; ACatchmentScaleFactor : double): boolean;
const OPNAME = 'TDiversionGauge.PopulateGaugeData';
begin
  Result := False;
  try
    FStationNo := AStationNo;
    FStationID := AStationID;
    FPlace := APlace;
    FLatitude := ALatitude;
    FLongitude := ALongitude;
    FCatchmentArea := ACatchmentArea;
    FCatchmentScaleFactor := ACatchmentScaleFactor;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.PopulateCompensationValues(ACapacityOfDiversion : double;AScaleFactor : double;AStartDate : TDateTime;
                                           AEndDate : TDateTime;ACompensationValues : TMonthlyDoubleArray): boolean;
const OPNAME = 'TDiversionGauge.PopulateCompensationValues';
var
  LIndex : integer;
begin
  Result := False;
  try
    FCapacityOfDiversion := ACapacityOfDiversion;
    FScaleFactor := AScaleFactor;
    FStartDate := AStartDate;
    FEndDate := AEndDate;
    for LIndex := MinMonths to MaxMonths do
      FCompensationValues[CHydroMonths[LIndex]] := ACompensationValues[CHydroMonths[LIndex]];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.PopulateThresholdValues(AMonthlyThreshold : TMonthlyDoubleArray): boolean;
const OPNAME = 'TDiversionGauge.PopulateThresholdValues';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := MinMonths to MaxMonths do
      FMonthlyThreshold[CHydroMonths[LIndex]] := AMonthlyThreshold[CHydroMonths[LIndex]];
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_CatchmentArea(const Value: double);
const OPNAME = 'TDiversionGauge.Set_CatchmentArea';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FCatchmentArea then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('CatchmentArea', FloatToStr(Value), FloatToStr(FCatchmentArea), LContextData) then
        begin
          LOldValue := FloatToStr(FCatchmentArea);
          FCatchmentArea := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CatchmentArea',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDiversionGauge.Set_Latitude(const Value: WideString);
const OPNAME = 'TDiversionGauge.Set_Latitude';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Trim(UpperCase(Value)) <> Trim(UpperCase(FLatitude)) then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('Latitude', Value, FLatitude, LContextData) then
        begin
          LOldValue := FLatitude;
          FLatitude := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Latitude',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_Longitude(const Value: WideString);
const OPNAME = 'TDiversionGauge.Set_Longitude';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Trim(UpperCase(Value)) <> Trim(UpperCase(FLongitude)) then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('Longitude', Value, FLongitude, LContextData) then
        begin
          LOldValue := FLongitude;
          FLongitude := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Longitude',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_Place(const Value: WideString);
const OPNAME = 'TDiversionGauge.Set_Place';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Trim(UpperCase(Value)) <> Trim(UpperCase(FPlace)) then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('Place', Value, FStationNo, LContextData) then
        begin
          LOldValue := FPlace;
          FPlace := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Place',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDiversionGauge.Validate';  
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.RemoveDailyData(AID: integer): boolean;
const OPNAME = 'TDiversionGauge.RemoveDailyData';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.DeleteDailyDataRecord(FStationID,AID) then
      begin
        Result := DeleteDailyFlowDataById(AID);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDiversionGauge.RemoveDailyInstreamData(AID: integer): boolean;
const OPNAME = 'TDiversionGauge.RemoveDailyInstreamData';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.DeleteDailyInstreamDataRecord(FStationID,AID) then
        Result := DeleteDailyInstreamFlowDataById(AID);
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddFlowDiversionRelationship : TFlowDiversionRelationship;
const OPNAME = 'TDiversionGauge.AddFlowDiversionRelationship';
begin
  Result := nil;
  try
    Result := TFlowDiversionRelationship.Create(FAppModules);
    Result.Initialise;
    FFlowDiversionRelationship.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddRankedFlowDiversionRelationship : TFlowDiversionRelationship;
const OPNAME = 'TDiversionGauge.AddRankedFlowDiversionRelationship';
begin
  Result := nil;
  try
    Result := TFlowDiversionRelationship.Create(FAppModules);
    Result.Initialise;
    FRankedFlowDiversionRelationship.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddWRYMData(var AIdentifier : integer) : TWRYMChannelData;
const OPNAME = 'TDiversionGauge.AddWRYMData';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
begin
  Result := nil;
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.InsertRelationshipData(FStationID,AIdentifier) then
        Result := CreateWRYMData;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.CreateWRYMData : TWRYMChannelData;
const OPNAME = 'TDiversionGauge.CreateWRYMData';
begin
  Result := nil;
  try
    Result := TWRYMChannelData.Create(FAppModules);
    Result.Initialise;
    FWRYMData.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_CatchmentScaleFactor: double;
const OPNAME = 'TDiversionGauge.Get_CatchmentScaleFactor';
begin
  Result := 0;
  try
    Result := FCatchmentScaleFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_CatchmentScaleFactor(const Value: double);
const OPNAME = 'TDiversionGauge.Set_CatchmentScaleFactor';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FCatchmentScaleFactor then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('CatchmentScaleFactor', FloatToStr(Value), FloatToStr(FCatchmentScaleFactor), LContextData) then
        begin
          LOldValue := FloatToStr(FCatchmentScaleFactor);
          FCatchmentScaleFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CatchmentScaleFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_CapacityOfDiversion: double;
const OPNAME = 'TDiversionGauge.Get_CapacityOfDiversion';
begin
  Result := 0.0;
  try
    Result := FCapacityOfDiversion;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_CompensationValueByIndex(AIndex: integer): double;
const OPNAME = 'TDiversionGauge.Get_CompensationValueByIndex';
begin
  Result := 0.0;
  try
    if (AIndex >= MinMonths) and (AIndex <= MaxMonths) then
      Result := FCompensationValues[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_EndDate: TDateTime;
const OPNAME = 'TDiversionGauge.Get_EndDate';
begin
  Result := 0;
  try
    Result := FEndDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_InstreamScaleFactor: double;
const OPNAME = 'TDiversionGauge.Get_InstreamScaleFactor';
begin
  Result := 0.0;
  try
    Result := FScaleFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_StartDate: TDateTime;
const OPNAME = 'TDiversionGauge.Get_StartDate';
begin
  Result := 0;
  try
    Result := FStartDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_WRYMStartDate: TDateTime;
const OPNAME = 'TDiversionGauge.Get_WRYMStartDate';
begin
  Result := 0;
  try
    Result := FWRYMStartDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_WRYMEndDate: TDateTime;
const OPNAME = 'TDiversionGauge.Get_WRYMEndDate';
begin
  Result := 0;
  try
    Result := FWRYMEndDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_CapacityOfDiversion(const Value: double);
const OPNAME = 'TDiversionGauge.Set_CapacityOfDiversion';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FCapacityOfDiversion then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('CapacityOfDiversion', FloatToStr(Value), FloatToStr(FCapacityOfDiversion), LContextData) then
        begin
          LOldValue := FloatToStr(FCapacityOfDiversion);
          FCapacityOfDiversion := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CapacityOfDiversion',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_CompensationValueByIndex(AIndex: integer;const Value: double);
const OPNAME = 'TDiversionGauge.Set_CompensationValueByIndex';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LFieldProperty : TAbstractFieldProperty;
  LMonth : integer;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('CompensationValue');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      LMonth := CHydroMonths[AIndex];
      if Value <> FCompensationValues[LMonth] then
      begin
        LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadCompensationContextData(LContextData, IntToStr(FStationID), IntToStr(LMonth));
          if FAppModules.FieldProperties.UpdateFieldValue('CompensationValue', FloatToStr(Value), FloatToStr(FCompensationValues[LMonth]), LContextData) then
          begin
            LOldValue := FloatToStr(FCompensationValues[LMonth]);
            FCompensationValues[LMonth] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'CompensationValue',LOldValue,FloatToStr(Value));
          end;
        finally
          FreeAndNil(LLoadAgent);
          FreeAndNil(LContextData);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_EndDate(const Value: TDateTime);
const OPNAME = 'TDiversionGauge.Set_EndDate';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FEndDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('DailyDiversionEndDate', DateToStr(Value), FloatToStr(FEndDate), LContextData) then
        begin
          LOldValue := DateToStr(FEndDate);
          FEndDate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DailyDiversionEndDate',LOldValue,DateToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_InstreamScaleFactor(const Value: double);
const OPNAME = 'TDiversionGauge.Set_InstreamScaleFactor';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FScaleFactor then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('InstreamScaleFactor', FloatToStr(Value), FloatToStr(FScaleFactor), LContextData) then
        begin
          LOldValue := FloatToStr(FScaleFactor);
          FScaleFactor := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InstreamScaleFactor',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_StartDate(const Value: TDateTime);
const OPNAME = 'TDiversionGauge.Set_StartDate';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FStartDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('DailyDiversionStartDate', DateToStr(Value), FloatToStr(FStartDate), LContextData) then
        begin
          LOldValue := DateToStr(FStartDate);
          FStartDate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DailyDiversionStartDate',LOldValue,DateToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_WRYMStartDate(const Value: TDateTime);
const OPNAME = 'TDiversionGauge.Set_WRYMStartDate';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FStartDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('WRYMStartDate', DateToStr(Value), FloatToStr(FStartDate), LContextData) then
        begin
          LOldValue := DateToStr(FStartDate);
          FStartDate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WRYMStartDate',LOldValue,DateToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_WRYMEndDate(const Value: TDateTime);
const OPNAME = 'TDiversionGauge.Set_WRYMEndDate';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FStartDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadStationContextData(LContextData, FStationID);
        if FAppModules.FieldProperties.UpdateFieldValue('WRYMEndDate', DateToStr(Value), FloatToStr(FStartDate), LContextData) then
        begin
          LOldValue := DateToStr(FStartDate);
          FStartDate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WRYMEndDate',LOldValue,DateToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.AddInstreamFlowData(ADate : TDateTime): TDailyInstreamFlowData;
const OPNAME = 'TDiversionGauge.AddInstreamFlowData';
begin
  Result := nil;
  try
    Result := TDailyInstreamFlowData.Create(FAppModules);
    FInstreamFlowData.Add(Result);
    Result.DailyData := GetDailyFlowDataByDate(ADate);
    Result.FInstreamDate := ADate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.GetDailyFlowDataByDate(ADate : TDateTime): TDailyFlowData;
const OPNAME = 'TDiversionGauge.GetDailyFlowDataByDate';
var
  LIndex : integer;
begin
  Result := nil;
  try
    if ExcludeSuspectDailyData then
    begin
      for LIndex := 0 to FNonSuspectDailyFlowData.Count -1 do
      begin
        if TDailyFlowData(FNonSuspectDailyFlowData.Items[LIndex]).DiversionDate = ADate then
        begin
          Result := TDailyFlowData(FNonSuspectDailyFlowData.Items[LIndex]);
          Exit;
        end;
      end;

    end;

    for LIndex := 0 to FDailyFlowData.Count -1 do
    begin
      if TDailyFlowData(FDailyFlowData.Items[LIndex]).DiversionDate = ADate then
      begin
        Result := TDailyFlowData(FDailyFlowData.Items[LIndex]);
        Break;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_InstreamFlowData(AIndex: integer): TDailyInstreamFlowData;
const OPNAME = 'TDiversionGauge.Get_InstreamFlowData';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FInstreamFlowData.Count-1) then
      Result := TDailyInstreamFlowData(FInstreamFlowData.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_FlowDiversionRelationshipCount : integer;
const OPNAME = 'TDiversionGauge.Get_FlowDiversionRelationshipCount';
begin
  Result := 0;
  try
    Result := FFlowDiversionRelationship.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_RankedFlowDiversionRelationshipCount : integer;
const OPNAME = 'TDiversionGauge.Get_RankedFlowDiversionRelationshipCount';
begin
  Result := 0;
  try
    Result := FRankedFlowDiversionRelationship.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_WRYMDataCount : integer;
const OPNAME = 'TDiversionGauge.Get_WRYMDataCount';
begin
  Result := 0;
  try
    Result := FWRYMData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.GetDailyInstreamFlowDataByDate(ADate : TDateTime): TDailyInstreamFlowData;
const OPNAME = 'TDiversionGauge.GetDailyInstreamFlowDataByDate';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FInstreamFlowData.Count -1 do
    begin
      if TDailyInstreamFlowData(FInstreamFlowData.Items[LIndex]).InstreamDate = ADate then
      begin
        Result := TDailyInstreamFlowData(FInstreamFlowData.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_UnRankedFlowDiversionRelationshipByIndex(AIndex: integer): TFlowDiversionRelationship;
const OPNAME = 'TDiversionGauge.Get_UnRankedFlowDiversionRelationshipByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FFlowDiversionRelationship.Count-1) then
      Result := TFlowDiversionRelationship(FFlowDiversionRelationship.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_RankedFlowDiversionRelationshipByIndex(AIndex: integer): TFlowDiversionRelationship;
const OPNAME = 'TDiversionGauge.Get_RankedFlowDiversionRelationshipByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FRankedFlowDiversionRelationship.Count-1) then
      Result := TFlowDiversionRelationship(FRankedFlowDiversionRelationship.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_WRYMDataByIndex(AIndex: integer): TWRYMChannelData;
const OPNAME = 'TDiversionGauge.Get_WRYMDataByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FWRYMData.Count-1) then
      Result := TWRYMChannelData(FWRYMData.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_InfillGaps: boolean;
const OPNAME = 'TDiversionGauge.Get_InfillGaps';
begin
  Result := False;
  try
    Result := FInfillGaps;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_InfillGaps(AInfillGaps: boolean);
const OPNAME = 'TDiversionGauge.Set_InfillGaps';
begin
  try
    FInfillGaps := AInfillGaps;
    FAppModules.Model.StudyDataHasChanged(sdccSelect,'StationNo','','');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDiversionGauge.ClearFlowDiversionRelationship: boolean;
const OPNAME = 'TDiversionGauge.ClearFlowDiversionRelationship';
begin
  Result := False;
  try
    FFlowDiversionRelationship.Clear;
    FRankedFlowDiversionRelationship.Clear;
    FWRYMData.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.ClearDailyInstreamFlowFileData: boolean;
const OPNAME = 'TDiversionGauge.ClearDailyInstreamFlowFileData';
begin
  Result := False;
  try
    FInstreamFlowData.Clear;
    FMonthlyInstreamFlowData.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.ClearDailyFlowData: boolean;
const OPNAME = 'TDiversionGauge.ClearDailyFlowData';
begin
  Result := False;
  try
    FDailyFlowData.Clear;
    FNonSuspectDailyFlowData.Clear;
    FMonthlyFlowData.Clear;

    FDailyFlowDataWithInfilledGaps.Clear;
    FDaysWithGabsList.Clear;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_ImportIFR: WordBool;
const OPNAME = 'TDiversionGauge.Get_ImportIFR';
begin
  Result := False;
  try
    Result := FImportIFR;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_ImportIFR(const Value: WordBool);
const OPNAME = 'TDiversionGauge.Set_ImportIFR';
var
  LNewValue,
  LOldValue : string;
begin
  try
    if FImportIFR <> Value then
    begin
      if Value then
        LNewValue := '1'
      else
        LNewValue := '0';
      if FImportIFR then
        LOldValue := '1'
      else
        LOldValue := '0';
      FImportIFR := Value;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'ImportIFR','DAILYDIVERSIONDATA',IntToStr(FStationID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.GetDailyDiversionDataByMonth(AMonth: integer;AData: TStrings): WordBool;
const OPNAME = 'TDiversionGauge.GetDailyDiversionDataByMonth';
var
  LMonth : word;
  LDay : word;
  LYear : word;
  LIndex : integer;
  LInfilledDailyFlowData,
  LDailyFlowData : TDailyFlowData;
begin
  Result := False;
  try
    if AData <> nil then
    begin
      for LIndex := 0 to DailyDataCount -1 do
      begin
        LDailyFlowData := DailyFlowDataByIndex[LIndex];
        if (LDailyFlowData <> nil) then
        begin
          if (LDailyFlowData.AvgFlow = NullFloat) then
          begin
            if (InfillGaps) then
            begin
              LInfilledDailyFlowData := GetInfilledDailyDataByDate(LDailyFlowData.DiversionDate);
              if LInfilledDailyFlowData <> nil then
              begin
                DecodeDate(LInfilledDailyFlowData.DiversionDate,LYear,LMonth,LDay);
                if (LMonth = AMonth) then
                begin
                  if AData.IndexOf(formatDateTime('yyyy/mm/dd',LInfilledDailyFlowData.DiversionDate))< 0 then
                    AData.AddObject(formatDateTime('yyyy/mm/dd',LInfilledDailyFlowData.DiversionDate),LInfilledDailyFlowData);
                end;
              end;
            end

            else
            if (IncludeAll) then
            begin
              DecodeDate(LDailyFlowData.DiversionDate,LYear,LMonth,LDay);
              if (LMonth = AMonth) then
              begin
                if LDailyFlowData.QualityCode = Null then
                  LDailyFlowData.QualityCode := 170;
                if AData.IndexOf(formatDateTime('yyyy/mm/dd',LDailyFlowData.DiversionDate))< 0 then
                  AData.AddObject(formatDateTime('yyyy/mm/dd',LDailyFlowData.DiversionDate),LDailyFlowData);
              end;
            end;
          end
          else
          begin
            DecodeDate(LDailyFlowData.DiversionDate,LYear,LMonth,LDay);
            if (LMonth = AMonth) then
            begin
              if AData.IndexOf(formatDateTime('yyyy/mm/dd',LDailyFlowData.DiversionDate))< 0 then
                AData.AddObject(formatDateTime('yyyy/mm/dd',LDailyFlowData.DiversionDate),LDailyFlowData);

              {if (ExcludeSuspectDailyData) and not (LDailyFlowData.QualityCode in [1,2,3,4,5,6,7,50,60,65,66,91,150])then
                Continue;
                }
            end;
          end;

        end;

      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_ThresholdByMonth(AMonth: integer): double;
const OPNAME = 'TDiversionGauge.Get_ThresholdByMonth';
begin
  Result := 0.0;
  try
    if (AMonth >= MinMonths) and (AMonth <= MaxMonths) then
      Result := FMonthlyThreshold[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDiversionGauge.Set_ThresholdByMonth(AMonth: integer;const Value: double);
const OPNAME = 'TDiversionGauge.Set_ThresholdByMonth';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LFieldProperty : TAbstractFieldProperty;
  LMonth : integer;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ThresholdValue');
    if (AMonth >= LFieldProperty.ArrayLow) and (AMonth <= LFieldProperty.ArrayHigh) then
    begin
      LMonth := CHydroMonths[AMonth];
      if Value <> FMonthlyThreshold[LMonth] then
      begin
        LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadCompensationContextData(LContextData, IntToStr(FStationID), IntToStr(LMonth));
          if FAppModules.FieldProperties.UpdateFieldValue('ThresholdValue', FloatToStr(Value), FloatToStr(FMonthlyThreshold[LMonth]), LContextData) then
          begin
            LOldValue := FloatToStr(FMonthlyThreshold[LMonth]);
            FMonthlyThreshold[LMonth] := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ThresholdValue',LOldValue,FloatToStr(Value));
          end;
        finally
          FreeAndNil(LLoadAgent);
          FreeAndNil(LContextData);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDiversionGauge.Get_NumberOfGabsByMonth(AYear,AMonth: integer): integer;
const OPNAME = 'TDiversionGauge.Get_NumberOfGabsByMonth';
var
  LIndex : integer;
begin
  Result := 0;
  try
    LIndex := FNumberOfGabsByMonth.IndexOf(IntToStr(AYear)+'_'+IntToStr(AMonth));
    if (LIndex >= 0) then
      Result := Integer(FNumberOfGabsByMonth.Objects[LIndex]);;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TDailyDiversionGaugeDataList }

function TDailyDiversionGaugeDataList.AddDiversionGauge: TDiversionGauge;
const OPNAME = 'TDailyDiversionGaugeDataList.AddDiversionGauge';
begin
  Result := nil;
  try
    Result := TDiversionGauge.Create(FAppModules);
    FDiversionGaugeList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionGaugeDataList.CreateMemberObjects;
const OPNAME = 'TDailyDiversionGaugeDataList.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FDiversionGaugeList := TObjectList.Create(False);
    //FF14DailyIFRData    := TDailyIFRDataList.Create(FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.DeleteDiversionGaugeByStationNo(AStationNo : WideString): boolean;
const OPNAME = 'TDailyDiversionGaugeDataList.DeleteDiversionGaugeByStationNo';
var
  LDiversionGauge : TDiversionGauge;
begin
  Result := False;
  try
    LDiversionGauge := DiversionGaugeByStationNo[AStationNo];
    if (LDiversionGauge <> nil) then
    begin
      FDiversionGaugeList.Remove(LDiversionGauge);
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionGaugeDataList.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionGaugeDataList.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FDiversionGaugeList);
    //FreeAndNil(FF14DailyIFRData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.Get_DailyDiversionGaugeData: integer;
const OPNAME = 'TDailyDiversionGaugeDataList.Get_DailyDiversionGaugeData';
begin
  Result := 0;
  try
    Result := FDiversionGaugeList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.Get_DiversionGaugeByIndex(AIndex: integer): TDiversionGauge;
const OPNAME = 'TDailyDiversionGaugeDataList.Get_DiversionGaugeByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDiversionGaugeList.Count) then
      Result := TDiversionGauge(FDiversionGaugeList[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.Get_DiversionGaugeByStationID(AStationID: integer): TDiversionGauge;
const OPNAME = 'TDailyDiversionGaugeDataList.Get_DiversionGaugeByStationID';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDiversionGaugeList.Count - 1 do
      if (TDiversionGauge(FDiversionGaugeList[LIndex]).FStationID = AStationID) then
      begin
        Result := TDiversionGauge(FDiversionGaugeList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TDailyDiversionGaugeDataList.Get_DailyIFRDataFromFile14ByStation: TDailyIFRDataList;
const OPNAME = 'TDailyDiversionGaugeDataList.Get_DailyIFRDataFromFile14ByStation';
begin
  Result := nil;
  try
    Result := FF14DailyIFRData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
 }
function TDailyDiversionGaugeDataList.Get_DiversionGaugeByStationNo(AStationNo: WideString): TDiversionGauge;
const OPNAME = 'TDailyDiversionGaugeDataList.Get_DiversionGaugeByStationNo';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDiversionGaugeList.Count - 1 do
      if (TDiversionGauge(FDiversionGaugeList[LIndex]).StationNo = AStationNo) then
      begin
        Result := TDiversionGauge(FDiversionGaugeList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionGaugeDataList.Initialise: Boolean;
const OPNAME = 'TDailyDiversionGaugeDataList.Initialise';
begin
  Result := False;
  try
    FDiversionGaugeList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.NewDiversionGauge(AStationNo : WideString): TDiversionGauge;
const OPNAME = 'TDailyDiversionGaugeDataList.NewDiversionGauge';
var
  LDiversionGauge : TDiversionGauge;
  LoadAgent : TDailyDiversionGaugeSQLAgent;
  LStationID : integer;
begin
  Result := nil;
  try
    LoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      if LoadAgent.InsertDailyDiversionStation(AStationNo,LStationID) then
      begin
        LDiversionGauge := AddDiversionGauge;
        LDiversionGauge.Initialise;
        LDiversionGauge.FStationNo := AStationNo;
        LDiversionGauge.FStationID := LStationID;
        Result := LDiversionGauge;
      end;
    finally
      FreeAndNil(LoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.RenameDiversionGauge(ANewStationNo : WideString;AOldStationNo : WideString) : WordBool;safecall;
const OPNAME = 'TDailyDiversionGaugeDataList.RenameDiversionGauge';
var
  LoadAgent : TDailyDiversionGaugeSQLAgent;
  LDiversionGauge : TDiversionGauge;
begin
  Result := False;
  try
    LoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      Result := LoadAgent.RenameDailyDiversionStation(ANewStationNo,AOldStationNo);
      if Result then
      begin
        LDiversionGauge := DiversionGaugeByStationNo[AOldStationNo];
        if LDiversionGauge <> nil then
          LDiversionGauge.FStationNo := ANewStationNo;
      end;
    finally
      FreeAndNil(LoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.RemoveDiversionGauge(AStationID : integer): WordBool;
const OPNAME = 'TDailyDiversionGaugeDataList.RemoveDiversionGauge';
var
  LoadAgent : TDailyDiversionGaugeSQLAgent;
begin
  Result := False;
  try
    LoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      if LoadAgent.DeleteDailyDiversionStation(AStationID) then
        Result := DeleteDiversionGaugeByStationNo(DiversionGaugeByStationID[AStationID].StationNo);
    finally
      FreeAndNil(LoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionGaugeDataList.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDailyDiversionGaugeDataList.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TDailyDiversionGaugeDataList.AddDailyIFRData(AStationNo : WideString): TDailyIFRData;
const OPNAME = 'TDailyDiversionGaugeDataList.AddDailyIFRData';
begin
  Result := nil;
  try
    Result := FF14DailyIFRData.AddDailyIFRData(AStationNo);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
{ TMonthlyFlowData }

function TMonthlyFlowData._AddRef: Integer;
const OPNAME = 'TMonthlyFlowData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMonthlyFlowData._Release: Integer;
const OPNAME = 'TMonthlyFlowData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TMonthlyFlowData.CreateMemberObjects;
const OPNAME = 'TMonthlyFlowData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    SetLength(FDaysInMonth,12);
    FDaysWithGabsList := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyFlowData.DestroyMemberObjects;
const OPNAME = 'TMonthlyFlowData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDaysInMonth);
    FreeAndNil(FDaysWithGabsList);
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Get_AvgFlowByIndex(AIndex: integer): double;
const OPNAME = 'TMonthlyFlowData.Get_AvgFlowByIndex';
var
  LMonth,
  LDaysInMonth : integer;
  LDiversionGauge : TDiversionGauge;
  LYear : integer;
begin
  Result := 0.0;
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if (AIndex >= MinMonths) and (AIndex <= MaxMonths) and (LDiversionGauge <> nil) then
    begin
      LMonth := CHydroMonths[AIndex];
      LYear := FYear;
      if LMonth < 10 then
        LYear := LYear+1;

      if (LMonth = 2) and (IsLeapYear(LYear)) then
        LDaysInMonth := CDaysInMonth[LMonth] + 1
      else
        LDaysInMonth := CDaysInMonth[LMonth];

      if LDiversionGauge.IncludeAll then
        FAvgFlow[LMonth] := (FIncludeAllMonthTotal[LMonth]/LDaysInMonth)*LDiversionGauge.CatchmentScaleFactor;

      if LDiversionGauge.InclundeOnlyGoodDailyData then
        FAvgFlow[LMonth] := (FGoodMonthTotal[LMonth]/LDaysInMonth)*LDiversionGauge.CatchmentScaleFactor;

      if LDiversionGauge.ExcludeSuspectDailyData then
        FAvgFlow[LMonth] := ((FIncludeAllMonthTotal[LMonth] - FSuspectMonthTotal[LMonth])/LDaysInMonth)*LDiversionGauge.CatchmentScaleFactor;

      if LDiversionGauge.InfillGaps then
      begin
        if MonthlyThresholdVerified(LMonth) then
          FAvgFlow[LMonth] := ((FIncludeAllMonthTotal[LMonth] + FInfilledMonthTotal[LMonth])/LDaysInMonth)*LDiversionGauge.CatchmentScaleFactor
        else
          FAvgFlow[LMonth] := NullFloat;
      end;
      Result := FAvgFlow[LMonth];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Get_StationID: integer;
const OPNAME = 'TMonthlyFlowData.Get_StationID';
begin
  Result := 0;
  try
    Result := FStationID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Get_Year: integer;
const OPNAME = 'TMonthlyFlowData.Get_Year';
begin
  Result := 0;
  try
    Result := FYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Initialise: Boolean;
const OPNAME = 'TMonthlyFlowData.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FYear := 0;
    for LIndex := 1 to 12 do
      FAvgFlow[LIndex] := 0;
    for LIndex := 1 to 12 do
      FInfilledMonthTotal[LIndex] := 0;
    for LIndex := 1 to 12 do
      FIncludeAllMonthTotal[LIndex] := 0;
    for LIndex := 1 to 12 do
      FGoodMonthTotal[LIndex] := 0;
    for LIndex := 1 to 12 do
      FSuspectMonthTotal[LIndex] := 0;
    FDaysWithGabsList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TMonthlyFlowData.Validate';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Get_IncludeAllMonthTotalByMonth(AMonth : integer): double;
const OPNAME = 'TMonthlyFlowData.Get_IncludeAllMonthTotalByMonth';
begin
  Result := 0.0;
  try
    Result := FIncludeAllMonthTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyFlowData.Set_IncludeAllMonthTotalByMonth(AMonth : integer;const Value: double);
const OPNAME = 'TMonthlyFlowData.Set_IncludeAllMonthTotalByMonth';
begin
  try
    FIncludeAllMonthTotal[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.MonthlyThresholdVerified(AMonth: integer): boolean;
const OPNAME = 'TMonthlyFlowData.MonthlyThresholdVerified';
var
  LDiversionGauge : TDiversionGauge;
  LIndex : integer;
  LGabsCount : integer;
  LDailyDataDate : TDateTime;
  LHydroYear : integer;
  LDailyData : TDailyFlowData;
begin
  Result := False;
  try
    LGabsCount := 0;
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if LDiversionGauge <> nil then
    begin
      for LIndex := 1 to CDaysInMonth[AMonth] do
      begin
        if AMonth < 10  then
          LHydroYear := Year + 1
        else
          LHydroYear := Year;
        LDailyDataDate := EncodeDate(LHydroYear,AMonth,LIndex);
        LDailyData := LDiversionGauge.GetDailyFlowDataByDate(LDailyDataDate);
        if (LDailyData <> nil) and (LDailyData.AvgFlow = NullFloat) then
          inc(LGabsCount);
      end;
      Result := (LGabsCount <= LDiversionGauge.ThresholdByMonth[AMonth]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyFlowData.Get_SuspectMonthTotalByMonth(AMonth : integer): double;
const OPNAME = 'TMonthlyFlowData.Get_SuspectMonthTotalByMonth';
begin
  Result := 0;
  try
    Result := FSuspectMonthTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyFlowData.Set_SuspectMonthTotalByMonth(AMonth : integer;const Value: double);
const OPNAME = 'TMonthlyFlowData.Set_SuspectMonthTotalByMonth';
begin
  try
     FSuspectMonthTotal[AMonth] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TDailyInstreamFlowData }

procedure TDailyInstreamFlowData.CreateMemberObjects;
const OPNAME = 'TDailyInstreamFlowData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.DestroyMemberObjects;
const OPNAME = 'TDailyInstreamFlowData.DestroyMemberObjects';
begin
  try
    if Assigned(FDailyData) then
      FDailyData := nil;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData._AddRef: Integer;
const OPNAME = 'TDailyInstreamFlowData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyInstreamFlowData._Release: Integer;
const OPNAME = 'TDailyInstreamFlowData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDailyInstreamFlowData.Get_AvgFlow: double;
const OPNAME = 'TDailyInstreamFlowData.Get_AvgFlow';
begin
  Result := NullFloat;
  try
    Result := FAvgFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_InstreamDate: TDate;
const OPNAME = 'TDailyInstreamFlowData.Get_InstreamDate';
begin
  Result := 0;
  try
    Result := FInstreamDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_Identifier: integer;
const OPNAME = 'TDailyInstreamFlowData.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_QualityCode: integer;
const OPNAME = 'TDailyInstreamFlowData.Get_QualityCode';
begin
  Result := 0;
  try
    Result := FQualityCode;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_StationID: integer;
const OPNAME = 'TDailyInstreamFlowData.Get_StationID';
begin
  Result := 0;
  try
    Result := FStationID
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Poulate(AStationID: integer;AIdentifier : integer;AAvgFlow,AScaleFactor,
                                        ACompensationValue, ACapacityOfDiversion: double; AQualityCode : integer) : boolean;
const OPNAME = 'TDailyInstreamFlowData.Poulate';
begin
  Result := False;
  try
    FStationID := AStationID;
    FIdentifier := AIdentifier;
    FAvgFlow := AAvgFlow;
    FScaleFactor := AScaleFactor;
    FQualityCode := AQualityCode;
    FCompensationValue := ACompensationValue;
    FCapacityOfDiversion := ACapacityOfDiversion;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Set_AvgFlow(const Value: double);
const OPNAME = 'TDailyInstreamFlowData.Set_AvgFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FAvgFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('InstreamAvgFlow', FloatToStr(Value), FloatToStr(FAvgFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FAvgFlow);
          FAvgFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InstreamAvgFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Set_InstreamDate(const Value: TDate);
const OPNAME = 'TDailyInstreamFlowData.Set_InstreamDate';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FInstreamDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('InstreamDate', DateToStr(Value), DateToStr(FInstreamDate), LContextData) then
        begin
          LOldValue := DateToStr(FInstreamDate);
          FInstreamDate := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InstreamDate',LOldValue,DateToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Set_QualityCode(const Value: integer);
const OPNAME = 'TDailyInstreamFlowData.Set_QualityCode';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FInstreamDate then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('InstreamQualityCode', IntToStr(Value), IntToStr(FQualityCode), LContextData) then
        begin
          LOldValue := IntToStr(FQualityCode);
          FQualityCode := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InstreamQualityCode',LOldValue,IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDailyInstreamFlowData.Validate';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_DailyAvailableFlow(AMonth : integer): double;
const OPNAME = 'TDailyInstreamFlowData.Get_DailyAvailableFlow';
var
  LDiversionGauge : TDiversionGauge;
  LMonthlyFlowData : TMonthlyFlowData;
  LDailyData : TDailyFlowData;
  LYear,LMonth,LDay : word;
  LHydroYear : integer;
begin
  Result := 0.0;
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if LDiversionGauge <> nil then
    begin
      if FDailyData <> nil then
      begin
        if FDailyData.AvgFlow = NullFloat then
        begin
          HasGaps := True;
          MonthlyThreshold := False;
          DecodeDate(FDailyData.FDiversionDate,LYear,LMonth,LDay);
          if LMonth < 10 then
            LHydroYear := LYear-1
          else
            LHydroYear := LYear;
          LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LHydroYear];
          if (LMonthlyFlowData <> nil) and (LDiversionGauge.InfillGaps = True) then
          begin
            if LMonthlyFlowData.MonthlyThresholdVerified(LMonth) then
            begin
              MonthlyThreshold := True;
              LDailyData := LDiversionGauge.GetInfilledDailyDataByDate(FDailyData.FDiversionDate);
              if LDailyData <> nil then
                Result := LDailyData.FactoredFlow - LDiversionGauge.CompensationValueByIndex[AMonth] - FactoredInstreamFlow;
            end;
          end
          else
            Result := 0 - LDiversionGauge.CompensationValueByIndex[AMonth] - FactoredInstreamFlow;
        end
        else
          Result := FDailyData.FactoredFlow - LDiversionGauge.CompensationValueByIndex[AMonth] - FactoredInstreamFlow;
      end;
    end;
    if Result < 0 then
      Result := 0.0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_DailyDiversionFlow(AMonth : integer): double;
const OPNAME = 'TDailyInstreamFlowData.Get_DailyDiversionFlow';
var
  LDiversionGauge : TDiversionGauge;
begin
  Result := 0.0;
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if LDiversionGauge <> nil then
      Result := Min(LDiversionGauge.CapacityOfDiversion,DailyAvailableFlow[AMonth]);
    if Result < 0 then
      Result := 0.0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_NonDailyDiversionFlow(AMonth : integer): double;
const OPNAME = 'TDailyInstreamFlowData.Get_NonDailyDiversionFlow';
begin
  Result := 0.0;
  try
    if FDailyData <> nil then
      Result := FDailyData.FactoredFlow - DailyDiversionFlow[AMonth];
    if Result < 0 then
      Result := 0.0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_FactoredInstreamFlow: double;
const OPNAME = 'TDailyInstreamFlowData.Get_FactoredInstreamFlow';
var
  LDiversionGauge : TDiversionGauge;
begin
  Result := 0.0;
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if LDiversionGauge <> nil then
      Result := LDiversionGauge.FScaleFactor * FAvgFlow;
    if Result < 0 then
      Result := 0.0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_DailyData: TDailyFlowData;
const OPNAME = 'TDailyInstreamFlowData.Get_DailyData';
begin
  Result := nil;
  try
    Result := FDailyData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Set_DailyData(const Value: TDailyFlowData);
const OPNAME = 'TDailyInstreamFlowData.Set_DailyData';
begin
  try
    FDailyData := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_HasGaps: boolean;
const OPNAME = 'TDailyInstreamFlowData.Get_HasGaps';
begin
  Result := False;
  try
    Result := FHasGaps;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Set_HasGaps(const Value: boolean);
const OPNAME = 'TDailyInstreamFlowData.Set_HasGaps';
begin
  try
    FHasGaps := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Get_MonthlyThreshold: boolean;
const OPNAME = 'TDailyInstreamFlowData.Get_MonthlyThreshold';
begin
  Result := False;
  try
    Result := FMonthlyThreshold;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyInstreamFlowData.Set_MonthlyThreshold(const Value: boolean);
const OPNAME = 'TDailyInstreamFlowData.Set_MonthlyThreshold';
begin
  try
    FMonthlyThreshold := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyInstreamFlowData.Initialise: Boolean;
const OPNAME = 'TDailyInstreamFlowData.Initialise';
begin
  Result := False;
  try
    FAvgFlow := NullFloat;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{TMonthlyInstreamFlowData}

function TMonthlyInstreamFlowData._AddRef: Integer;
const OPNAME = 'TMonthlyInstreamFlowData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMonthlyInstreamFlowData._Release: Integer;
const OPNAME = 'TMonthlyInstreamFlowData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TMonthlyInstreamFlowData.CreateMemberObjects;
const OPNAME = 'TMonthlyInstreamFlowData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    SetLength(FDaysInMonth,12);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyInstreamFlowData.DestroyMemberObjects;
const OPNAME = 'TMonthlyInstreamFlowData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDaysInMonth);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyInstreamFlowData.Get_DailyDiversionFlowByIndex(AIndex: integer): double;
const OPNAME = 'TMonthlyInstreamFlowData.Get_DailyDiversionFlowByIndex';
var
  LDaysInMonth : integer;
  LMonth : integer;
  LDailyDiversionTotalByMonth : double;
  LYear : integer;
begin
  Result := 0.0;
  try
    if (AIndex >= MinMonths) and (AIndex <= MaxMonths) then
    begin
      LMonth := CHydroMonths[AIndex];
      LYear := FYear;

      if LMonth < 10 then
        LYear := LYear+1;
        
      if (LMonth = 2) and (IsLeapYear(LYear)) then
        LDaysInMonth := CDaysInMonth[LMonth] + 1
      else
        LDaysInMonth := CDaysInMonth[LMonth];
      LDailyDiversionTotalByMonth := DailyDiversionTotalByMonth[LMonth];
      if LDailyDiversionTotalByMonth = NullFloat then
        FAvgFlow[LMonth] := NullFloat
      else
        FAvgFlow[LMonth] := LDailyDiversionTotalByMonth/LDaysInMonth;
      Result := FAvgFlow[LMonth];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyInstreamFlowData.Get_StationID: integer;
const OPNAME = 'TMonthlyInstreamFlowData.Get_StationID';
begin
  Result := 0;
  try
    Result := FStationID;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyInstreamFlowData.Get_Year: integer;
const OPNAME = 'TMonthlyInstreamFlowData.Get_Year';
begin
  Result := 0;
  try
    Result := FYear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyInstreamFlowData.Initialise: Boolean;
const OPNAME = 'TMonthlyInstreamFlowData.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FYear := 0;
    for LIndex := 1 to 12 do
      FAvgFlow[LIndex] := 0;
    FExcludeMissingDailyData := False;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyInstreamFlowData.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TMonthlyInstreamFlowData.Validate';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyInstreamFlowData.Get_DailyDiversionTotalByMonth(AMonth: integer): double;
const OPNAME = 'TMonthlyInstreamFlowData.Get_DailyDiversionTotalByMonth';
var
  LDiversionGauge : TDiversionGauge;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LIndex : integer;
  LInstreamFlowDate : TDateTime;
  LHydroYear : word;
begin
  Result := 0.0;
  try
    FDailyDiversionTotal[AMonth] := 0;
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                       DiversionGaugeByStationID[FStationID];
    if LDiversionGauge <> nil then
    begin
      for LIndex := 1 to CDaysInMonth[AMonth] do
      begin
        if AMonth < 10  then
          LHydroYear := Year + 1
        else
          LHydroYear := Year;

        LInstreamFlowDate := EncodeDate(LHydroYear,AMonth,LIndex);
        LDailyInstreamFlowData := LDiversionGauge.GetDailyInstreamFlowDataByDate(LInstreamFlowDate);
        if LDailyInstreamFlowData <> nil then
        begin
          FDailyDiversionTotal[AMonth] := FDailyDiversionTotal[AMonth] + LDailyInstreamFlowData.DailyDiversionFlow[AMonth];
          if {(LDiversionGauge.InfillGaps) and }(LDailyInstreamFlowData.HasGaps) {and not (LDailyInstreamFlowData.MonthlyThreshold) }then
          begin
            FDailyDiversionTotal[AMonth] := NullFloat;
            Break;
          end;
        end;
      end;
    end;
    if (FDailyDiversionTotal[AMonth] < 0) and (FDailyDiversionTotal[AMonth] <> NullFloat) then
      FDailyDiversionTotal[AMonth] := 0.0;
    Result := FDailyDiversionTotal[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TFlowDiversionRelationship }

function TFlowDiversionRelationship._AddRef: Integer;
const OPNAME = 'TFlowDiversionRelationship._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowDiversionRelationship._Release: Integer;
const OPNAME = 'TFlowDiversionRelationship._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFlowDiversionRelationship.CreateMemberObjects;
const OPNAME = 'TFlowDiversionRelationship.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationship.DestroyMemberObjects;
const OPNAME = 'TFlowDiversionRelationship.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationship.Get_DiversionFlow: double;
const OPNAME = 'TFlowDiversionRelationship.Get_DiversionFlow';
begin
  Result := NullFloat;
  try
    Result := FDiversionFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationship.Get_NonDiversionFlow:double;
const OPNAME = 'TFlowDiversionRelationship.Get_NonDiversionFlow';
begin
  Result := NullFloat;
  try
    Result := FNonDiversionFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationship.Get_ReferenceFlow: double;
const OPNAME = 'TFlowDiversionRelationship.Get_ReferenceFlow';
begin
  Result := NullFloat;
  try
    Result := FReferenceFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationship.Get_RelationDate: TDateTime;
const OPNAME = 'TFlowDiversionRelationship.Get_RelationDate';
begin
  Result := 0;
  try
    Result := FRelationDate;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationship.Set_DiversionFlow(const Value: double);
const OPNAME = 'TFlowDiversionRelationship.Set_DiversionFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FDiversionFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('RelationDiversionFlow', FloatToStr(Value), FloatToStr(FDiversionFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FNonDiversionFlow);
          FNonDiversionFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RelationDiversionFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationship.Set_NonDiversionFlow(const Value: double);
const OPNAME = 'TFlowDiversionRelationship.Set_NonDiversionFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FDiversionFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('RelationNonDiversionFlow', FloatToStr(Value), FloatToStr(FDiversionFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FDiversionFlow);
          FDiversionFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RelationNonDiversionFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TFlowDiversionRelationship.Set_ReferenceFlow(const Value: double);
const OPNAME = 'TFlowDiversionRelationship.Set_ReferenceFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FReferenceFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('ReferenceFlow', FloatToStr(Value), FloatToStr(FReferenceFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FReferenceFlow);
          FReferenceFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReferenceFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationship.Set_RelationDate(const Value: TDateTime);
const OPNAME = 'TFlowDiversionRelationship.Set_RelationDate';
begin
  try
    FRelationDate := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationship.Populate(AIdentifier: integer;AStationID: integer;ARelationDate : TDateTime; AReferenceFlow, ADiversionFlow, ANonDiversionFlow : double) : boolean;
const OPNAME = 'TFlowDiversionRelationship.Populate';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FStationID := AStationID;
    FRelationDate := ARelationDate;
    FReferenceFlow := AReferenceFlow;
    FDiversionFlow := ADiversionFlow;
    FNonDiversionFlow := ANonDiversionFlow;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationship.Initialise: Boolean;
const OPNAME = 'TFlowDiversionRelationship.Initialise';
begin
  Result := False;
  try
    FRelationDate := 0;
    FReferenceFlow := NullFloat;
    FDiversionFlow := NullFloat;
    FNonDiversionFlow := NullFloat;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TWRYMChannelData }

function TWRYMChannelData._AddRef: Integer;
const OPNAME = 'TWRYMChannelData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRYMChannelData._Release: Integer;
const OPNAME = 'TWRYMChannelData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWRYMChannelData.CreateMemberObjects;
const OPNAME = 'TWRYMChannelData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWRYMChannelData.DestroyMemberObjects;
const OPNAME = 'TWRYMChannelData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWRYMChannelData.Get_DiversionFlow: double;
const OPNAME = 'TWRYMChannelData.Get_DiversionFlow';
begin
  Result := NullFloat;
  try
    Result := FDiversionFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWRYMChannelData.Get_ReferenceFlow: double;
const OPNAME = 'TWRYMChannelData.Get_ReferenceFlow';
begin
  Result := NullFloat;
  try
    Result := FReferenceFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TWRYMChannelData.Set_DiversionFlow(const Value: double);
const OPNAME = 'TWRYMChannelData.Set_DiversionFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FDiversionFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('WRYMDiversionFlow', FloatToStr(Value), FloatToStr(FDiversionFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FDiversionFlow);
          FDiversionFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WRYMDiversionFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWRYMChannelData.Set_NonDiversionFlow(const Value: double);
const OPNAME = 'TWRYMChannelData.Set_NonDiversionFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FNonDiversionFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('WRYMNonDiversionFlow', FloatToStr(Value), FloatToStr(FNonDiversionFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FNonDiversionFlow);
          FNonDiversionFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WRYMNonDiversionFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWRYMChannelData.Set_ReferenceFlow(const Value: double);
const OPNAME = 'TWRYMChannelData.Set_ReferenceFlow';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    if Value <> FReferenceFlow then
    begin
      LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
        if FAppModules.FieldProperties.UpdateFieldValue('WRYMReferenceFlow', FloatToStr(Value), FloatToStr(FReferenceFlow), LContextData) then
        begin
          LOldValue := FloatToStr(FReferenceFlow);
          FReferenceFlow := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WRYMReferenceFlow',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWRYMChannelData.Populate(AIdentifier: integer;AStationID : integer) : boolean;
const OPNAME = 'TWRYMChannelData.Populate';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FStationID := AStationID;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWRYMChannelData.PopulateAll(AIdentifier : integer;AStationID : integer;
                                      AReferenceFlow : double; ADiversionFlow : double;
                                      ADivFlowValueEdited : string;ARefFlowValueEdited : string;
                                      ANonDiversionFlow : double;ANonDivFlowValueEdited : string) : boolean;
const OPNAME = 'TWRYMChannelData.PopulateAll';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FStationID := AStationID;
    FReferenceFlow := AReferenceFlow;
    FDiversionFlow := ADiversionFlow;
    FDivFlowValueEdited := (ADivFlowValueEdited = 'Y');
    FRefFlowValueEdited := (ARefFlowValueEdited = 'Y');
    FNonDiversionFlow := ANonDiversionFlow;
    FNonDivFlowValueEdited := (ANonDivFlowValueEdited = 'Y');
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWRYMChannelData.Initialise: Boolean;
const OPNAME = 'TWRYMChannelData.Initialise';
begin
  Result := False;
  try
    FReferenceFlow := NullFloat;
    FDiversionFlow := NullFloat;
    FNonDiversionFlow:= NullFloat;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWRYMChannelData.Get_DivFlowValueEdited: WordBool;
const OPNAME = 'TWRYMChannelData.Get_DivFlowValueEdited';
begin
  Result := False;
  try
    Result := FDivFlowValueEdited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWRYMChannelData.Get_NonDivFlowValueEdited: WordBool;
const OPNAME = 'TWRYMChannelData.Get_NonDivFlowValueEdited';
begin
  Result := False;
  try
    Result := FNonDivFlowValueEdited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWRYMChannelData.Get_NonDiversionFlow: double;
const OPNAME = 'TWRYMChannelData.Get_NonDiversionFlow';
begin
  Result := NullFloat;
  try
    Result := FNonDiversionFlow;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWRYMChannelData.Get_RefFlowValueEdited: WordBool;
const OPNAME = 'TWRYMChannelData.Get_RefFlowValueEdited';
begin
  Result := False;
  try
    Result := FRefFlowValueEdited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWRYMChannelData.Set_DivFlowValueEdited(const Value: WordBool);
const OPNAME = 'TWRYMChannelData.Set_DivFlowValueEdited';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue    : string;
begin
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      if Value then
        LNewValue := 'Y'
      else
        LNewValue := 'N';
      if FDivFlowValueEdited then
        LOldValue := 'Y'
      else
        LOldValue := 'N';
      LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
      if FAppModules.FieldProperties.UpdateFieldValue('DivFlowValueEdited', LNewValue, LOldValue, LContextData) then
      begin
        FDivFlowValueEdited := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'DivFlowValueEdited',LOldValue,LNewValue);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWRYMChannelData.Set_NonDivFlowValueEdited(const Value: WordBool);
const OPNAME = 'TWRYMChannelData.Set_NonDivFlowValueEdited';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue    : string;
begin
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      if Value then
        LNewValue := 'Y'
      else
        LNewValue := 'N';
      if FNonDivFlowValueEdited then
        LOldValue := 'Y'
      else
        LOldValue := 'N';
      LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
      if FAppModules.FieldProperties.UpdateFieldValue('NonDivFlowValueEdited', LNewValue, LOldValue, LContextData) then
      begin
        FNonDivFlowValueEdited := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'NonDivFlowValueEdited',LOldValue,LNewValue);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWRYMChannelData.Set_RefFlowValueEdited(const Value: WordBool);
const OPNAME = 'TWRYMChannelData.Set_RefFlowValueEdited';
var
  LLoadAgent : TDailyDiversionGaugeSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue    : string;
begin
  try
    LLoadAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      if Value then
        LNewValue := 'Y'
      else
        LNewValue := 'N';
      if FRefFlowValueEdited then
        LOldValue := 'Y'
      else
        LOldValue := 'N';
      LLoadAgent.LoadContextData_DailyDataFlow(LContextData, IntToStr(FIdentifier),IntToStr(FStationID));
      if FAppModules.FieldProperties.UpdateFieldValue('RefFlowValueEdited', LNewValue, LOldValue, LContextData) then
      begin
        FRefFlowValueEdited := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'RefFlowValueEdited',LOldValue,LNewValue);
      end;
    finally
      FreeAndNil(LLoadAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
