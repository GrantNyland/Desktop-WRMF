(******************************************************************************)
(* Contains : THydroOutput.                                                   *)
(* Manages the import and export of Simulation Result File data.              *)
(******************************************************************************)

unit UHydroOutput;

interface

uses
  Classes, Contnrs,
  UTimeSeries,

  UModule,
  UAbstractObject,
  HydrologyCom_TLB;

type
                     
  TDateStruct = packed record
    Year      : Word;
    Month     : Byte;
    Day       : Byte;
    Hour      : Byte;
    Minute    : Byte;
    Second    : Byte;
    MilliSec  : Byte;
  end;

  TOutputStructType = packed record
    FStructType : Integer;
  end;

  TOutputHeader = packed record
    FStructType : Integer;      // 4 bytes
    FNoOfValues : Integer;      // 4 bytes
    FStartDate  : TDateStruct;  // 8 bytes
  end;

  THydroResultType = class(TAbstractObject, IHydroResultType)
  protected
    FResultTypeID   : Integer;
    FElementType    : String;
    FElementSubType : String;
    FDescription    : String;
    FUnits          : String;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_ResultTypeID: Integer; safecall;
    procedure Set_ResultTypeID(Value: Integer); safecall;
    function Get_ElementType: WideString; safecall;
    procedure Set_ElementType(const Value: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_ElementSubType: WideString; safecall;
    procedure Set_ElementSubType(const Value: WideString); safecall;
    function Get_Units: WideString; safecall;
    procedure Set_Units(const Value: WideString); safecall;
  public
    procedure Populate (AResultTypeID   : Integer;
                        AElementType    : String;
                        AElementSubType : String;
                        ADescription    : String;
                        AUnits          : String);
    property ResultTypeID: Integer read Get_ResultTypeID write Set_ResultTypeID;
    property ElementType: WideString read Get_ElementType write Set_ElementType;
    property Description: WideString read Get_Description write Set_Description;
    property ElementSubType: WideString read Get_ElementSubType write Set_ElementSubType;
    property Units: WideString read Get_Units write Set_Units;
  end;

  THydroOutput = class(TAbstractObject, IHydroOutput)
  protected
    FResultID       : Integer;
    FNetworkID      : Integer;
    FElementType    : String;
    FElementSubType : String;
    FElementID      : Integer;
    FSubElementID   : Integer;
    FResultTypeID   : Integer;
    FAllZero        : Boolean;
    FHeader         : TOutputHeader;
    FTimeSeries     : TTimeSeries;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_ResultID: Integer; safecall;
    function Get_NetworkID: Integer; safecall;
    function Get_ElementType: WideString; safecall;
    procedure Set_ElementType(const Value: WideString); safecall;
    function Get_ElementSubType: WideString; safecall;
    procedure Set_ElementSubType(const Value: WideString); safecall;
    function Get_ElementID: Integer; safecall;
    procedure Set_ElementID(Value: Integer); safecall;
    function Get_SubElementID: Integer; safecall;
    procedure Set_SubElementID(Value: Integer); safecall;
    function Get_ResultTypeID: Integer; safecall;
    procedure Set_ResultTypeID(Value: Integer); safecall;
    function Get_PeriodsPerYear: Integer; safecall;
    function Get_IntervalCount: Integer; safecall;
    function Get_YearCount: Integer; safecall;
    function Get_StartYear: Integer; safecall;
    function Get_EndYear: Integer; safecall;
    function Get_AllZero: WordBool; safecall;
    procedure Set_AllZero(Value: WordBool); safecall;
    function Get_YearTotal(AYearIndex: Integer): Double; safecall;
    function Get_YearAverage(AYearIndex: Integer): Double; safecall;
    function Get_PeriodTotal(APeriodIndex: Integer): Double; safecall;
    function Get_PeriodAverage(APeriodIndex: Integer): Double; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    function Get_Data(AIntervalIndex: Integer): Double; safecall;
    function Get_DataByYearMonth(AYear: Integer; AMonth: Integer): Double; safecall;
    function Get_OverallAverage: Double; safecall;
    function Get_OverallTotal: Double; safecall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Populate (AResultID       : Integer;
                        ANetworkID      : Integer;
                        AElementType    : String;
                        AElementSubType : String;
                        AElementID      : Integer;
                        ASubElementID   : Integer;
                        AResultTypeID   : Integer;
                        AAllZero        : Boolean);

    property ResultID: Integer read Get_ResultID;
    property NetworkID: Integer read Get_NetworkID;
    property ElementType: WideString read Get_ElementType write Set_ElementType;
    property ElementSubType: WideString read Get_ElementSubType write Set_ElementSubType;
    property ElementID: Integer read Get_ElementID write Set_ElementID;
    property SubElementID: Integer read Get_SubElementID write Set_SubElementID;
    property ResultTypeID: Integer read Get_ResultTypeID write Set_ResultTypeID;
    property AllZero : WordBool read Get_AllZero write Set_AllZero;
    property PeriodsPerYear: Integer read Get_PeriodsPerYear;
    property IntervalCount: Integer read Get_IntervalCount;
    property YearCount: Integer read Get_YearCount;
    property StartYear: Integer read Get_StartYear;
    property EndYear: Integer read Get_EndYear;
    property YearTotal[AYearIndex: Integer]: Double read Get_YearTotal;
    property YearAverage[AYearIndex: Integer]: Double read Get_YearAverage;
    property PeriodTotal[APeriodIndex: Integer]: Double read Get_PeriodTotal;
    property PeriodAverage[APeriodIndex: Integer]: Double read Get_PeriodAverage;
    property Year[AYearIndex: Integer]: Integer read Get_Year;
    property Data[AIntervalIndex: Integer]: Double read Get_Data;
    property DataByYearMonth[AYear: Integer; AMonth: Integer]: Double read Get_DataByYearMonth;
    property OverallAverage: Double read Get_OverallAverage;
    property OverallTotal: Double read Get_OverallTotal;
    property Header         : TOutputHeader read FHeader      write FHeader;
    property TimeSeries     : TTimeSeries   read FTimeSeries;
  end;

  THydroOutputAgent = class(TModuleAgent, IHydroOutputAgent)
  protected
    FResultTypes : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_HydroOutput (const AElementType    : WideString;
                              const AElementSubType : WideString;
                              AElementID            : Integer;
                              ASubElementID         : Integer;
                              AResultTypeID         : Integer): IHydroOutput; safecall;
    function Get_ResultTypeCount: Integer; safecall;
    function Get_ResultTypeByIndex(AIndex: Integer): IHydroResultType; safecall;
    function Get_ResultTypeByID(AResultTypeID: Integer): IHydroResultType; safecall;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Initialise : Boolean; override;
    function AddHydroOutput : THydroOutput;
    function AddHydroResultType : THydroResultType;
    function LoadHydroOutputFromDB(ANetworkID: Integer): WordBool; safecall;
    property HydroOutput[const AElementType: WideString; const AElementSubType: WideString; AElementID: Integer;
                         ASubElementID: Integer; AResultTypeID: Integer]: IHydroOutput read Get_HydroOutput;
    property ResultTypeCount: Integer read Get_ResultTypeCount;
    property ResultTypeByIndex[AIndex: Integer]: IHydroResultType read Get_ResultTypeByIndex;
    property ResultTypeByID[AResultTypeID: Integer]: IHydroResultType read Get_ResultTypeByID;
  end;

implementation


uses
  SysUtils,
  Windows,

  UModuleDBManager,
  UHydroOutputDBManager,
  UErrorHandlingOperations;

(* THydroResultType ***********************************************************)

function THydroResultType._AddRef: Integer;
const OPNAME = 'THydroResultType._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroResultType._Release: Integer;
const OPNAME = 'THydroResultType._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroResultType.Get_ResultTypeID: Integer;
const OPNAME = 'THydroResultType.Get_ResultTypeID';
begin
  Result := 0;
  try
    Result := FResultTypeID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroResultType.Set_ResultTypeID(Value: Integer);
const OPNAME = 'THydroResultType.Set_ResultTypeID';
begin
  try
    FResultTypeID := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroResultType.Get_ElementType: WideString;
const OPNAME = 'THydroResultType.Get_ElementType';
begin
  Result := '';
  try
    Result := FElementType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroResultType.Set_ElementType(const Value: WideString);
const OPNAME = 'THydroResultType.Set_ElementType';
begin
  try
    FElementType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroResultType.Get_Description: WideString;
const OPNAME = 'THydroResultType.Get_Description';
begin
  Result := '';
  try
    Result := FDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroResultType.Set_Description(const Value: WideString);
const OPNAME = 'THydroResultType.Set_Description';
begin
  try
    FDescription := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroResultType.Get_ElementSubType: WideString;
const OPNAME = 'THydroResultType.Get_ElementSubType';
begin
  Result := '';
  try
    Result := FElementSubType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroResultType.Set_ElementSubType(const Value: WideString);
const OPNAME = 'THydroResultType.Set_ElementSubType';
begin
  try
    FElementSubType := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroResultType.Get_Units: WideString;
const OPNAME = 'THydroResultType.Get_Units';
begin
  Result := '';
  try
    Result := FUnits;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroResultType.Set_Units(const Value: WideString);
const OPNAME = 'THydroResultType.Set_Units';
begin
  try
    FUnits := Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroResultType.Populate (AResultTypeID   : Integer;
                                     AElementType    : String;
                                     AElementSubType : String;
                                     ADescription    : String;
                                     AUnits          : String);
const OPNAME = 'THydroResultType.Populate';
begin
  try
    FResultTypeID   := AResultTypeID;
    FElementType    := AElementType;
    FElementSubType := AElementSubType;
    FDescription    := ADescription;
    FUnits          := AUnits;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


(* THydroOutput ***************************************************************)

function THydroOutput._AddRef: Integer;
const OPNAME = 'THydroOutput._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutput._Release: Integer;
const OPNAME = 'THydroOutput._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroOutput.CreateMemberObjects;
const OPNAME = 'THydroOutput.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTimeSeries := TTimeSeries.Create(12);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.DestroyMemberObjects;
const OPNAME = 'THydroOutput.DestroyMemberObjects';
begin
  try
    if (FTimeSeries <> nil) then
    begin
      FreeAndNil(FTimeSeries);
    end;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Populate (AResultID       : Integer;
                                 ANetworkID      : Integer;
                                 AElementType    : String;
                                 AElementSubType : String;
                                 AElementID      : Integer;
                                 ASubElementID   : Integer;
                                 AResultTypeID   : Integer;
                                 AAllZero        : Boolean);
const OPNAME = 'THydroOutput.Populate';
begin
  try
    FResultID       := AResultID;
    FNetworkID      := ANetworkID;
    FElementType    := AElementType;
    FElementSubType := AElementSubType;
    FElementID      := AElementID;
    FSubElementID   := ASubElementID;
    FResultTypeID   := AResultTypeID;
    FAllZero        := AAllZero;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_ResultID: Integer;
const OPNAME = 'THydroOutput.Get_ResultID';
begin
  Result := 0;
  try
    Result := FResultID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_NetworkID: Integer;
const OPNAME = 'THydroOutput.Get_NetworkID';
begin
  Result := 0;
  try
    Result := FNetworkID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_ElementType: WideString;
const OPNAME = 'THydroOutput.Get_ElementType';
begin
  Result := '';
  try
    Result := FElementType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Set_ElementType(const Value: WideString);
const OPNAME = 'THydroOutput.Set_ElementType';
begin
  try
    FElementType := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_ElementSubType: WideString;
const OPNAME = 'THydroOutput.Get_ElementSubType';
begin
  Result := '';
  try
    Result := FElementSubType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Set_ElementSubType(const Value: WideString);
const OPNAME = 'THydroOutput.Set_ElementSubType';
begin
  try
    FElementSubType := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_ElementID: Integer;
const OPNAME = 'THydroOutput.Get_ElementID';
begin
  Result := 0;
  try
    Result := FElementID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Set_ElementID(Value: Integer);
const OPNAME = 'THydroOutput.Set_ElementID';
begin
  try
    FElementID := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_SubElementID: Integer;
const OPNAME = 'THydroOutput.Get_SubElementID';
begin
  Result := 0;
  try
    Result := FSubElementID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Set_SubElementID(Value: Integer);
const OPNAME = 'THydroOutput.Set_SubElementID';
begin
  try
    FSubElementID := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_ResultTypeID: Integer;
const OPNAME = 'THydroOutput.Get_ResultTypeID';
begin
  Result := 0;
  try
    Result := FResultTypeID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Set_ResultTypeID(Value: Integer);
const OPNAME = 'THydroOutput.Set_ResultTypeID';
begin
  try
    FResultTypeID := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_AllZero: WordBool;
const OPNAME = 'THydroOutput.Get_AllZero';
begin
  Result := FALSE;
  try
    Result := FAllZero;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutput.Set_AllZero(Value: WordBool);
const OPNAME = 'THydroOutput.Set_AllZero';
begin
  try
    FAllZero := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_PeriodsPerYear: Integer;
const OPNAME = 'THydroOutput.Get_PeriodsPerYear';
begin
  Result := 0;
  try
    Result := FTimeSeries.PeriodsPerYear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_IntervalCount: Integer;
const OPNAME = 'THydroOutput.Get_IntervalCount';
begin
  Result := 0;
  try
    Result := FTimeSeries.IntervalCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_YearCount: Integer;
const OPNAME = 'THydroOutput.Get_YearCount';
begin
  Result := 0;
  try
    Result := FTimeSeries.YearCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_StartYear: Integer;
const OPNAME = 'THydroOutput.Get_StartYear';
begin
  Result := 0;
  try
    Result := FTimeSeries.StartYear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_EndYear: Integer;
const OPNAME = 'THydroOutput.Get_EndYear';
begin
  Result := 0;
  try
    Result := FTimeSeries.EndYear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_YearTotal(AYearIndex: Integer): Double;
const OPNAME = 'THydroOutput.Get_YearTotal';
begin
  Result := 0;
  try
    Result := FTimeSeries.YearTotal[AYearIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_YearAverage(AYearIndex: Integer): Double;
const OPNAME = 'THydroOutput.Get_YearAverage';
begin
  Result := 0;
  try
    Result := FTimeSeries.YearAverage[AYearIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_PeriodTotal(APeriodIndex: Integer): Double;
const OPNAME = 'THydroOutput.Get_PeriodTotal';
begin
  Result := 0;
  try
    Result := FTimeSeries.PeriodTotal[APeriodIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_PeriodAverage(APeriodIndex: Integer): Double;
const OPNAME = 'THydroOutput.Get_PeriodAverage';
begin
  Result := 0;
  try
    Result := FTimeSeries.PeriodAverage[APeriodIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_Year(AYearIndex: Integer): Integer;
const OPNAME = 'THydroOutput.Get_Year';
begin
  Result := 0;
  try
    Result := FTimeSeries.Year[AYearIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_Data(AIntervalIndex: Integer): Double;
const OPNAME = 'THydroOutput.Get_Data';
begin
  Result := 0;
  try
    if (NOT FAllZero) then
      Result := FTimeSeries.Data[AIntervalIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_DataByYearMonth(AYear: Integer; AMonth: Integer): Double;
const OPNAME = 'THydroOutput.Get_DataByYearMonth';
begin
  Result := 0;
  try
    Result := FTimeSeries.DataByYearMonth[AYear, AMonth];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_OverallAverage: Double;
const OPNAME = 'THydroOutput.Get_OverallAverage';
begin
  Result := 0;
  try
    Result := FTimeSeries.OverallAverage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutput.Get_OverallTotal: Double;
const OPNAME = 'THydroOutput.Get_OverallTotal';
begin
  Result := 0;
  try
    Result := FTimeSeries.OverallTotal;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


(* THydroOutputAgent **********************************************************)

function THydroOutputAgent._AddRef: Integer;
const OPNAME = 'THydroOutputAgent._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent._Release: Integer;
const OPNAME = 'THydroOutputAgent._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroOutputAgent.CreateMemberObjects;
const OPNAME = 'THydroOutputAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FResultTypes := TObjectList.Create(FALSE);

//    GHydroOutputDBManager := THydroOutputDBManager(GModuleDBManager);
    GHydroOutputDBManager := THydroOutputDBManager.Create;
    GHydroOutputDBManager.ModuleAgent := Self;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroOutputAgent.DestroyMemberObjects;
const OPNAME = 'THydroOutputAgent.DestroyMemberObjects';
begin
  try
//    Initialise;
//    FreeAndNil(FResultTypes);

    GHydroOutputDBManager.ModuleAgent := nil;
    FreeAndNil(GHydroOutputDBManager);
    inherited DestroyMemberObjects;
    FreeAndNil(FResultTypes);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.Initialise : Boolean;
const OPNAME = 'THydroOutputAgent.Initialise';
begin
  Result := FALSE;
  try
    inherited Initialise;
    while (FResultTypes.Count > 0) do
      FResultTypes.Delete(0);

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.AddHydroOutput : THydroOutput;
const OPNAME = 'THydroOutputAgent.AddHydroOutput';
var
  LHydroOutput : THydroOutput;
begin
  Result := nil;
  try
    LHydroOutput := THydroOutput.Create;
    FList.Add(LHydroOutput);
    Result := LHydroOutput;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.LoadHydroOutputFromDB (ANetworkID: Integer): WordBool;
const OPNAME = 'THydroOutputAgent.LoadHydroOutputFromDB';
begin
  Result := FALSE;
  try
    Result := GHydroOutputDBManager.LoadOutputFromDB(ANetworkID, '', '', 0, 0) AND
              GHydroOutputDBManager.LoadResultTypesFromDB;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.AddHydroResultType : THydroResultType;
const OPNAME = 'THydroOutputAgent.AddHydroResultType';
var
  LHydroResultType : THydroResultType;
begin
  Result := nil;
  try
    LHydroResultType := THydroResultType.Create;
    FResultTypes.Add(LHydroResultType);
    Result := LHydroResultType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.Get_HydroOutput (const AElementType    : WideString;
                                            const AElementSubType : WideString;
                                            AElementID            : Integer;
                                            ASubElementID         : Integer;
                                            AResultTypeID         : Integer): IHydroOutput;
const OPNAME = 'THydroOutputAgent.Get_HydroOutput';
var
  LHydroOutput : THydroOutput;
  LFound       : Boolean;
  LIndex       : Integer;
begin
  Result := nil;
  try
    LFound := FALSE;
    LIndex := 0;
    while ((NOT LFound) AND (LIndex < FList.Count)) do
    begin
      LHydroOutput := THydroOutput(FList.Items[LIndex]);
      if ((LHydroOutput.ElementType = AElementType) AND
          (LHydroOutput.ElementSubType = AElementSubType) AND
          (LHydroOutput.ElementID = AElementID) AND
          (LHydroOutput.SubElementID = ASubElementID) AND
          ((AResultTypeID = 0) OR (LHydroOutput.ResultTypeID = AResultTypeID))) then
      begin
        Result := LHydroOutput;
        LFound := TRUE;
      end
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.Get_ResultTypeCount: Integer;
const OPNAME = 'THydroOutputAgent.Get_ResultTypeCount';
begin
  Result := 0;
  try
    Result := FResultTypes.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.Get_ResultTypeByIndex(AIndex: Integer): IHydroResultType;
const OPNAME = 'THydroOutputAgent.Get_ResultTypeByIndex';
begin
  Result := nil;
  try
    Result := THydroResultType(FResultTypes.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroOutputAgent.Get_ResultTypeByID (AResultTypeID: Integer): IHydroResultType;
const OPNAME = 'THydroOutputAgent.Get_ResultTypeByIndex';
var
  LIndex       : Integer;
  LHydroResult : IHydroResultType;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) AND (LIndex < FResultTypes.Count)) do
    begin
      LHydroResult := THydroResultType(FResultTypes.Items[LIndex]);
      if (LHydroResult.ResultTypeID = AResultTypeID) then
        Result := LHydroResult
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

