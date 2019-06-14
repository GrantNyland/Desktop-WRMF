{*******************************************************}
{                                                       }
{  UNIT      : Contains TOutputDailyFileReader             }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/07/2011                               }
{  COPYRIGHT : Copyright © 2011 T-Systems               }
{                                                       }
{*******************************************************}

unit UOutputFileHeader;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  UColourButtons,
  UAbstractObject;

type
  TOutputFileHeader = class(TObject)
  public
    FRunDate             : TDateTime;
    FProvinceName        : string;
    FStationNumber       : string;
    FStationName         : string;
    FPeriod_StartDate    : TDate;
    FPeriod_EndDate      : TDate;
    FRun_TypeID          : integer;
    FRun_StartVolume     : double;
    FRun_StartLevel      : double;
    FRun_StopLevel       : double;
    FRoof_Area           : double;
    FRoof_RunoffCoef     : double;
    FHouseHold_Number    : integer;
    FHouseHold_Members   : integer;
    FHouseHold_DemandPP  : double;
    FTankSizes           : array[1..10] of double;
    FDaysSupplied        : array[1..10] of integer;
    FMonthNumbers        : array[1..12] of integer;
    FMonthNames          : array[1..12] of string;
    procedure Reset;
    function ReadFromStringList(AData : TStrings): boolean;
    function WriteToStringList(AData : TStrings): boolean;
    function WriteDaysSupplied(AData : TStrings): boolean;
    function MonthNamesText: string;
    function TankSizesText: string;
    function TankCount: integer;
    function MonthNumbersCommaText: string;
    function WriteBestTankSize(AData: TStrings): boolean;
    function GetBestTankSize(var ABestInx : integer) : string;
  end;

implementation

uses
  DateUtils,
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TOutputFileHeader }

procedure TOutputFileHeader.Reset;
const OPNAME = 'TOutputFileHeader.Reset';
var
  LIndex : integer;
begin
  try
    FRunDate             := NullDateTime;
    FProvinceName        := '';
    FStationNumber       := '';
    FStationName         := '';
    FPeriod_StartDate    := NullDateTime;
    FPeriod_EndDate      := NullDateTime;
    FRun_TypeID          := NullInteger;
    FRun_StartVolume     := NullFloat;
    FRun_StartLevel      := NullFloat;
    FRun_StopLevel       := NullFloat;
    FRoof_Area           := NullFloat;
    FRoof_RunoffCoef     := NullFloat;
    FHouseHold_Number    := NullInteger;
    FHouseHold_Members   := NullInteger;
    FHouseHold_DemandPP  := NullFloat;
    for LIndex := 1 to 10 do
    begin
      FTankSizes[LIndex]    := NullFloat;
      FDaysSupplied[LIndex] := 0;
    end;

    for LIndex := 1 to 12 do
    begin
      FMonthNumbers[LIndex] := NullInteger;
      FMonthNames[LIndex]   := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.GetBestTankSize(var ABestInx : integer): string;
const OPNAME = 'TOutputFileHeader.GetBestTankSize';
var
  LIndex : integer;
  LDaysSuppleid : integer;
 // LBestIndex : integer;
begin
  Result := '';
  try
    LDaysSuppleid := 0;
    ABestInx := 0;
    for LIndex := 1 to 10 do
    begin
      if((FDaysSupplied[LIndex] <> NullFloat) and (FDaysSupplied[LIndex] > LDaysSuppleid ))then
      begin
        LDaysSuppleid := FDaysSupplied[LIndex];
        ABestInx := LIndex;
      end;
    end;
    if (ABestInx>0)  then
      Result := Result + PadLeftString(FormatFloat('0.000',FTankSizes[ABestInx]),10);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.MonthNamesText: string;
const OPNAME = 'TOutputFileHeader.MonthNamesText';
var
  LIndex : integer;
begin
  Result := '';
  try
    for LIndex := 1 to 12 do
      Result := Result + PadLeftString(FMonthNames[LIndex],10);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.MonthNumbersCommaText: string;
const OPNAME = 'TOutputFileHeader.MonthNumbersCommaText';
var
  LIndex : integer;
begin
  Result := '';
  try
    for LIndex := 1 to 12 do
      Result := Result + FormatFloat('00',FMonthNumbers[LIndex])+',';
    Delete(Result,Length(Result),1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.WriteToStringList(AData: TStrings): boolean;
const OPNAME = 'TOutputFileHeader.WriteToStringList';
var
  LIndex : integer;
  LLineData  : TStringList;
begin
  Result := False;
  try
    LLineData  := TStringList.Create;
    try
      LLineData.Add('ProvinceName='+FProvinceName);
      LLineData.Add('FStationNumber='+FStationNumber);
      LLineData.Add('StationName='+FStationName);
      AData.Add('Identification:'+LLineData.CommaText);
      LLineData.Clear;

      LLineData.Add('DatePrinted='+DateTimeToStr(FRunDate));
      LLineData.Add('StartDate='+DateToStr(FPeriod_StartDate));
      LLineData.Add('EndDate='+DateToStr(FPeriod_EndDate));
      AData.Add('Dates:'+LLineData.CommaText);
      LLineData.Clear;

      LLineData.Add('RunTypeID='+IntToStr(FRun_TypeID));
      LLineData.Add('StartVolume='+FormatFloat('0.000',FRun_StartVolume));
      LLineData.Add('StartLevel='+FormatFloat('0.000',FRun_StartLevel));
      LLineData.Add('StopLevel='+FormatFloat('0.000',FRun_StopLevel));
      AData.Add('Run:'+LLineData.CommaText);
      LLineData.Clear;


      LLineData.Add('RoofArea='+FormatFloat('0.000',FRoof_Area));
      LLineData.Add('RoofRunoffCoef='+FormatFloat('0.000',FRoof_RunoffCoef));
      LLineData.Add('NumberOfHouses='+IntToStr(FHouseHold_Number));
      LLineData.Add('PeoplePerHouse='+IntToStr(FHouseHold_Members));
      LLineData.Add('WaterDemandPerPerson='+FormatFloat('0.000',FHouseHold_DemandPP));
      AData.Add('Household:'+LLineData.CommaText);
      LLineData.Clear;

      for LIndex := 1 to 10 do
      begin
        if(FTankSizes[LIndex] <> NullFloat) then
          LLineData.Add(FormatFloat('0.000',FTankSizes[LIndex]));
      end;
      AData.Add('TankVolumes:'+LLineData.CommaText);
      LLineData.Clear;

      for LIndex := 1 to 12 do
          LLineData.Add(IntToStr(FMonthNumbers[LIndex]));
      AData.Add('MonthNumbers:'+LLineData.CommaText);
      LLineData.Clear;
      for LIndex := 1 to 12 do
          LLineData.Add(FMonthNames[LIndex]);
      AData.Add('MonthNames:'+LLineData.CommaText);

     Result := True;
    finally
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.WriteDaysSupplied(AData: TStrings): boolean;
const OPNAME = 'TOutputFileHeader.WriteDaysSupplied';
var
  LIndex : integer;
  LLineData  : TStringList;
begin
  Result := False;
  try
    LLineData  := TStringList.Create;
    try
      for LIndex := 1 to 10 do
         LLineData.Add(IntToStr(FDaysSupplied[LIndex]));
      AData.Insert(5,'DaysSupplied:'+LLineData.CommaText);
     Result := True;
    finally
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputFileHeader.WriteBestTankSize(AData: TStrings): boolean;
const OPNAME = 'TOutputFileHeader.WriteBestTankSize';
var
  LIndex : integer;
  LLineData  : TStringList;
begin
  Result := False;
  try
    LLineData  := TStringList.Create;
    try
      for LIndex := 1 to 10 do
         LLineData.Add(IntToStr(FDaysSupplied[LIndex]));
      AData.Add('Number of Tanks in the run: ' + IntToStr(TankCount));
      AData.Add('Given Tank Sizes: '+TankSizesText);
      AData.Add('Days Supplied: '+LLineData.CommaText);
      AData.Add('The Best Tank Size To Use: ' +GetBestTankSize(LIndex));
      AData.Add('Tank Index: '+IntToStr(LIndex));
     Result := True;
    finally
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputFileHeader.ReadFromStringList(AData: TStrings): boolean;
const OPNAME = 'TOutputFileHeader.ReadFromStringList';
var
  LIndex     : integer;
  LLineData  : TStringList;
  LLineStr   : String;
  LData      : string;
begin
  Result := False;
  try
    Reset;
    if(AData.Count < 8) then Exit;
    LLineData  := TStringList.Create;
    try
      LLineStr := AData[0];
      if(Pos('Identification:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,16,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        LData               := LLineData.Values['ProvinceName'];
        FProvinceName       := LData;
        LData               := LLineData.Values['FStationNumber'];
        FStationNumber      := LData;
        LData               := LLineData.Values['StationName'];
        FStationName        := LData;
      end;

      LLineStr := AData[1];
      if(Pos('Dates:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,7,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        LData               := LLineData.Values['DatePrinted'];
        FRunDate            := StrToDateTime(LData);
        LData               := LLineData.Values['StartDate'];
        FPeriod_StartDate   := StrToDate(LData);
        LData               := LLineData.Values['EndDate'];
        FPeriod_EndDate     := StrToDate(LData);
      end;

      LLineStr := AData[2];
      if(Pos('Run:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,5,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        LData               := LLineData.Values['RunTypeID'];
        FRun_TypeID         := StrToIntDef(LData,4);
        LData               := LLineData.Values['StartVolume'];
        FRun_StartVolume    := StrToFloatDef(LData,0.0);
        LData               := LLineData.Values['StartLevel'];
        FRun_StartLevel     := StrToFloatDef(LData,0.0);
        LData               := LLineData.Values['StopLevel'];
        FRun_StopLevel      := StrToFloatDef(LData,0.0);
      end;

      LLineStr := AData[3];
      if(Pos('Household:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,11,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        LData               := LLineData.Values['RoofArea'];
        FRoof_Area          := StrToFloatDef(LData,4);
        LData               := LLineData.Values['RoofRunoffCoef'];
        FRoof_RunoffCoef    := StrToFloatDef(LData,0.0);
        LData               := LLineData.Values['NumberOfHouses'];
        FHouseHold_Number   := StrToIntDef(LData,0);
        LData               := LLineData.Values['PeoplePerHouse'];
        FHouseHold_Members  := StrToIntDef(LData,0);
        LData               := LLineData.Values['WaterDemandPerPerson'];
        FHouseHold_DemandPP := StrToFloatDef(LData,0.0);
      end;

      LLineStr := AData[4];
      if(Pos('TankVolumes:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,13,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        for LIndex := 1 to LLineData.Count do
        begin
          LData               := LLineData[LIndex-1];
          FTankSizes[LIndex] := StrToFloatDef(LData,0.0);
        end;
      end;

      LLineStr := AData[5];
      if(Pos('DaysSupplied:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,14,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        for LIndex := 1 to LLineData.Count do
        begin
          LData               := LLineData[LIndex-1];
          FDaysSupplied[LIndex] := StrToIntDef(LData,0);
        end;
      end;

      LLineStr := AData[6];
      if(Pos('MonthNumbers:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,14,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        for LIndex := 1 to LLineData.Count do
        begin
          LData               := LLineData[LIndex-1];
          FMonthNumbers[LIndex] := StrToIntDef(LData,0);
        end;
      end;

      LLineStr := AData[7];
      if(Pos('MonthNames:',LLineStr) = 1) then
      begin
        LLineStr := Copy(LLineStr,12,Length(LLineStr));
        LLineData.CommaText := LLineStr;
        for LIndex := 1 to LLineData.Count do
        begin
          LData               := LLineData[LIndex-1];
          FMonthNames[LIndex] := LData;
        end;
      end;
     Result := True;
    finally
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.TankSizesText: string;
const OPNAME = 'TOutputFileHeader.MonthNamesText';
var
  LIndex : integer;
begin
  Result := '';
  try
    for LIndex := 1 to 10 do
    begin
      if(FTankSizes[LIndex] <> NullFloat) then
        Result := Result + PadLeftString(FormatFloat('0.000',FTankSizes[LIndex]),10);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputFileHeader.TankCount: integer;
const OPNAME = 'TOutputFileHeader.TankCount';
var
  LIndex : integer;
begin
  Result := 0;
  try
    for LIndex := 1 to 10 do
    begin
      if(FTankSizes[LIndex] <> NullFloat) then
        Result := Result + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
