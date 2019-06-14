//
//  UNIT      : Contains the class TYieldModelCalendar.
//  AUTHOR    : Dziedzi Ramulondi(DWAF)
//  DATE      : 2002/03/26
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UYieldModelCalendar;

interface

uses
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TYieldModelCalendar = class(TAbstractAppObject,IModelCalendar)
  protected
    function Get_CalenderMonthIndexByModelIndex(AModelMonthIndex: integer): integer;
    function Get_CalenderMonthIndexByName(AModelMonthName: string): integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_CalenderStartDate(AYear: integer = 0): TDateTime; safecall;
    function Get_CalenderDateByMonthIndex(AModelMonthIndex: integer;AYear: integer = 0): TDateTime; safecall;
    function Get_CalenderDateByMonthName(Const AModelMonthName: WideString;AYear: integer = 0): TDateTime; safecall;
    function Get_ModelMonthNameByIndex(AModelMonthIndex: integer): WideString; safecall;
    function Get_ModelMonthIndexByName(Const AModelMonthName: WideString): integer; safecall;
    function Get_ModelMonthDaysByIndex(AModelMonthIndex: integer): Double; safecall;
    function Get_ModelMonthDaysByName(Const AModelMonthName: WideString): Double; safecall;
    function Get_ModelMonthPeriodElapsed(AEndCalenderDate: TDateTime): integer; safecall;
    function Get_CalenderDateFromPeriodElapsed(APeriodElapsed: integer): TDateTime; safecall;

    function Get_DebugStartDate(AStartPeriod: integer): TDateTime;
    function Get_DebugEndDate(AEndPeriod: integer): TDateTime;
    property DebugStartDate[AStartPeriod: integer]   :TDateTime read  Get_DebugStartDate;
    property DebugEndDate[AEndPeriod: integer]     :TDateTime read  Get_DebugEndDate;

    property CalenderStartDate[AYear: Integer]: TDateTime read Get_CalenderStartDate;
    property CalenderDateByMonthIndex[AModelMonthIndex: Integer; AYear: Integer]: TDateTime read Get_CalenderDateByMonthIndex;
    property CalenderDateByMonthName[const AModelMonthName: WideString; AYear: Integer = 0]: TDateTime read Get_CalenderDateByMonthName;
    property ModelMonthNameByIndex[AModelMonthIndex: Integer]: WideString read Get_ModelMonthNameByIndex;
    property ModelMonthIndexByName[const AModelMonthName: WideString]: Integer read Get_ModelMonthIndexByName;
    property ModelMonthDaysByIndex[AModelMonthIndex: Integer]: Double read Get_ModelMonthDaysByIndex;
    property ModelMonthDaysByName[const AModelMonthName: WideString]: Double read Get_ModelMonthDaysByName;
    property ModelMonthPeriodElapsed[AEndDate: TDateTime]: Integer read Get_ModelMonthPeriodElapsed;
    property CalenderDateFromPeriodElapsed[APeriodElapsed: Integer]: TDateTime read Get_CalenderDateFromPeriodElapsed;

  end;

implementation
uses
  SysUtils,
  DateUtils,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TYieldModelCalendar }

function TYieldModelCalendar.Get_ModelMonthDaysByIndex(AModelMonthIndex: integer): Double;
const OPNAME = 'TYieldModelCalendar.Get_ModelMonthDaysByIndex';
var
  LRunConfigurationData: IRunConfigurationData;
begin
  Result := 0.0;
  try
    if(AModelMonthIndex > 0) and (AModelMonthIndex <= 12) then
    begin
      LRunConfigurationData:= TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      Result := LRunConfigurationData.MonthDaysByIndex[AModelMonthIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_ModelMonthDaysByName(Const AModelMonthName: WideString): Double;
const OPNAME = 'TYieldModelCalendar.Get_ModelMonthDaysByName';
var
  LRunConfigurationData: IRunConfigurationData;
  LIndex: integer;
  LName: string;
begin
  Result := 0;
  try
    LRunConfigurationData:= TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    for LIndex := 1 to 12 do
    begin
      LName := LRunConfigurationData.MonthNameByIndex[LIndex];
      if(UpperCase(AModelMonthName) = UpperCase(LName)) then
      begin
        Result := Get_ModelMonthDaysByIndex(LIndex);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_ModelMonthIndexByName(Const AModelMonthName: WideString): integer;
const OPNAME = 'TYieldModelCalendar.Get_ModelMonthIndexByName';
var
  LRunConfigurationData: IRunConfigurationData;
  LIndex: integer;
  LName: string;
begin
  Result := 0;
  try
    LRunConfigurationData:= TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    for LIndex := 1 to 12 do
    begin
      LName := LRunConfigurationData.MonthNameByIndex[LIndex];
      if(UpperCase(AModelMonthName) = UpperCase(LName)) then
      begin
        Result := LIndex;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_ModelMonthNameByIndex(AModelMonthIndex: integer): WideString;
const OPNAME = 'TYieldModelCalendar.Get_ModelMonthNameByIndex';
var
  LRunConfigurationData: IRunConfigurationData;
begin
  Result := '';
  try
    if(AModelMonthIndex > 0) and (AModelMonthIndex <= 12) then
    begin
      LRunConfigurationData:= TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      Result := LRunConfigurationData.MonthNameByIndex[AModelMonthIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//_____________________________________________________________________________________________________________
function TYieldModelCalendar.Get_CalenderStartDate(AYear: integer = 0): TDateTime;
const OPNAME = 'TYieldModelCalendar.Get_CalenderStartDate';
var
  LStudy               :TAbstractStudyArea;
  LRunConfigurationData: IRunConfigurationData;
  LStartYear,
  LStartMonth,
  LStartDay: Word;
begin
  Result := 0.0;
  try
    LStudy               := FAppModules.StudyArea;
    LRunConfigurationData:= TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if(AYear <= 0.0) then
      LStartYear  :=  LRunConfigurationData.StartYearOther
    else
      LStartYear := AYear;
    LStartMonth := LStudy.CalendarStartMonth;
    LStartDay   := 1;
    if(LStartYear > 0) and (LStartMonth > 0) then
      Result      := EncodeDate(LStartYear,LStartMonth,LStartDay);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_CalenderMonthIndexByModelIndex(AModelMonthIndex: integer): integer;
const OPNAME = 'TYieldModelCalendar.Get_CalenderMonthIndexByModelIndex';
var
  LStudy :TAbstractStudyArea;
begin
  Result := 0;
  try
    if(AModelMonthIndex > 0) and (AModelMonthIndex <= 12) then
    begin
      LStudy               := FAppModules.StudyArea;
      Result := LStudy.CalendarStartMonth + AModelMonthIndex -1;
      if(Result > 12) then
        Result := Result - 12;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_CalenderDateByMonthIndex(AModelMonthIndex: integer ;AYear: integer = 0): TDateTime;
const OPNAME = 'TYieldModelCalendar.Get_CalenderDateByMonthIndex';
var
  LStartYear,
  LStartMonth,
  LStartDay: Word;
begin
  Result := 0.0;
  try
    if(AModelMonthIndex > 0) and (AModelMonthIndex <= 12) then
    begin
      Result := Get_CalenderStartDate(AYear);
      DecodeDate(Result,LStartYear,LStartMonth,LStartDay);
      LStartMonth := Get_CalenderMonthIndexByModelIndex(AModelMonthIndex);
      Result      := EncodeDate(LStartYear,LStartMonth,LStartDay);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_CalenderMonthIndexByName(AModelMonthName: string): integer;
const OPNAME = 'TYieldModelCalendar.Get_CalenderMonthIndexByName';
var
  LRunConfigurationData: IRunConfigurationData;
  LIndex: integer;
  LName: string;
begin
  Result := 0;
  try
    LRunConfigurationData:= TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    for LIndex := 1 to 12 do
    begin
      LName := LRunConfigurationData.MonthNameByIndex[LIndex];
      if(UpperCase(AModelMonthName) = UpperCase(LName)) then
      begin
        Result := Get_CalenderMonthIndexByModelIndex(LIndex);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_CalenderDateByMonthName(const AModelMonthName: WideString;AYear: integer = 0): TDateTime;
const OPNAME = 'TYieldModelCalendar.Get_CalenderDateByMonthName';
var
  LStartYear,
  LStartMonth,
  LStartDay: Word;
begin
  Result := 0.0;
  try
    Result := Get_CalenderStartDate(AYear);
    DecodeDate(Result,LStartYear,LStartMonth,LStartDay);
    LStartMonth := Get_CalenderMonthIndexByName(AModelMonthName);
    Result      := EncodeDate(LStartYear,LStartMonth,LStartDay);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
//-----------------------------------------------------------------------------------------
function TYieldModelCalendar.Get_ModelMonthPeriodElapsed(AEndCalenderDate: TDateTime): integer;
const OPNAME = 'TYieldModelCalendar.Get_ModelMonthPeriodElapsed';
var
 LStartCalenderDate: TDateTime;
begin
  Result := 0;
  try
    LStartCalenderDate := Get_CalenderStartDate;
    Result := MonthsBetween(AEndCalenderDate,LStartCalenderDate);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_CalenderDateFromPeriodElapsed(APeriodElapsed: integer): TDateTime;
const OPNAME = 'TYieldModelCalendar.Get_CalenderDateFromPeriodElapsed';
var
 LStartCalenderDate: TDateTime;
begin
  Result := 0;
  try
    LStartCalenderDate := Get_CalenderStartDate;
    Result := IncMonth(LStartCalenderDate,APeriodElapsed);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_DebugStartDate(AStartPeriod: integer): TDateTime;
const OPNAME = 'TYieldModelCalendar.Get_DebugStartDate';
begin
  Result := 0.0;
  try
    Result := Get_CalenderStartDate;
    Result := IncMonth(Result,AStartPeriod-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar.Get_DebugEndDate(AEndPeriod: integer): TDateTime;
const OPNAME = 'TYieldModelCalendar.Get_DebugEndDate';
begin
  Result := 0.0;
  try
    Result := Get_CalenderStartDate;
    Result := IncMonth(Result,AEndPeriod);
    Result := Result - 1.0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelCalendar._AddRef: Integer;
const OPNAME = 'TYieldModelCalendar._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelCalendar._Release: Integer;
const OPNAME = 'TYieldModelCalendar._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
