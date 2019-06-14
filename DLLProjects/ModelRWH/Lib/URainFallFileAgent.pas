{*******************************************************}
{                                                       }
{  UNIT      : Contains TRainFallFileAgent             }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/07/2011                               }
{  COPYRIGHT : Copyright © 2011 T-Systems               }
{                                                       }
{*******************************************************}

unit URainFallFileAgent;

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
  TRainfallRecord = class(TObject)
  protected
    FDate : TDateTime;
    FRainfall : Double;
    FDataQuality : string;
    procedure Reset;
  public
    procedure Assign(ASource: TRainfallRecord);
    function ReadFromString(ARowData : TStrings): boolean;
    property Date        : TDateTime read FDate write FDate;
    property Rainfall    : Double    read FRainfall write FRainfall;
    property DataQuality : string    read FDataQuality write FDataQuality;
  end;

  TRainFallFileAgent = class(TObject)
  protected
    FCurrentPosition : integer;
    FFileData        : TStringList;
    FLineData        : TStringList;
    FRainfallRecord  : TRainfallRecord;
    FStationNumber        : string;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Initialise;
    function First: TRainfallRecord;
    function Last: TRainfallRecord;
    function Next: TRainfallRecord;
    function Previous: TRainfallRecord;
    function CurrentPosition:integer;
    function Count:integer;
    function  LoadStationDailyData(AStationNumber: string):boolean;
    function  NonBreakingData :integer;

    function  DaysInAnalysis  : integer;
    function  YearsInAnalysis : integer;
    function  StartYear       : integer;
    function  StartMonth      : integer;
    function StartDate: TDate;
    function EndDate: TDate;
    function StartNoNullDate: TDate;
    function EndNoNullDate: TDate;
    property FileName: string read FStationNumber write FStationNumber;
  end;

implementation

uses
  DateUtils,
  UConstants,
  URWHDataObject,
  UErrorHandlingOperations;

{ TRainfallRecord }

procedure TRainfallRecord.Reset;
const OPNAME = 'TRainfallRecord.Reset';
begin
  try
    FDate        := NullDateTime;
    FRainfall    := NullFloat;
    FDataQuality := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRainfallRecord.Assign(ASource: TRainfallRecord);
const OPNAME = 'TRainfallRecord.Reset';
begin
  try
    if(ASource <> nil) then
    begin
      FDate        := ASource.FDate;
      FRainfall    := ASource.FRainfall;
      FDataQuality := ASource.FDataQuality;
    end
    else
    begin
      FDate        := NullDateTime;
      FRainfall    := NullFloat;
      FDataQuality := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallRecord.ReadFromString(ARowData : TStrings): boolean;
const OPNAME = 'TRainfallRecord.ReadFromString';
var
  LYear,
  LMonth,
  LDay: word;
  LDate: TDateTime;
  LStr : string;
  LDateData : TStringList;
begin
  Result := False;
  try
    Reset;
    if(ARowData.Count > 0) then
    begin
      LDateData := TStringList.Create;
      try
        LStr   := Trim(ARowData[0]);
        LStr   := StringReplace(LStr,'/',',',[rfReplaceAll]);
        LStr   := StringReplace(LStr,'\',',',[rfReplaceAll]);
        LDateData.CommaText := LStr;
        LYear  := 0;
        LMonth := 0;
        LDay   := 0;
        if(LDateData.Count > 0) then
          LYear  := StrToIntDef(LDateData[0],0);
        if(LDateData.Count > 1) then
          LMonth := StrToIntDef(LDateData[1],0);
        if(LDateData.Count > 2) then
          LDay   := StrToIntDef(LDateData[2],0);

        if(LDay > 31) and (LYear <= 31) then
        begin
          if TryEncodeDate(LDay,LMonth,LYear, LDate) then
            FDate := LDate;
        end
        else
        begin
          if TryEncodeDate(LYear,LMonth,LDay, LDate) then
            FDate := LDate;
        end
      finally
        LDateData.Free;
      end;
      {LStr   := Trim(ARowData[0]);
      LYear  := StrToIntDef(Copy(LStr,1,4),0);
      LMonth  := StrToIntDef(Copy(LStr,6,2),0);
      LDay  := StrToIntDef(Copy(LStr,9,2),0);
      if(LMonth > 0) and (LDay > 0) then
      begin
        //FDate := EncodeDate(LYear,LMonth,LDay);
        if TryEncodeDate(LYear,LMonth,LDay, LDate) then
          FDate := LDate;
      end;}
    end;
    if(ARowData.Count > 1) then
      FRainfall := StrToFloatDef(Trim(ARowData[1]),0.00);
    if(ARowData.Count > 2) then
      FDataQuality := Trim(ARowData[2]);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRainFallFileAgent }

procedure TRainFallFileAgent.AfterConstruction;
const OPNAME = 'TRainFallFileAgent.AfterConstruction';
begin
  inherited;
  try
    FFileData      := TStringList.Create;
    FLineData      := TStringList.Create;
    FRainfallRecord := TRainfallRecord.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainFallFileAgent.BeforeDestruction;
const OPNAME = 'TRainFallFileAgent.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FFileData);
    FreeAndNil(FLineData);
    FreeAndNil(FRainfallRecord);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainFallFileAgent.Initialise;
const OPNAME = 'TRainFallFileAgent.Initialise';
begin
  inherited;
  try
    FCurrentPosition := -1;
    FStationNumber        := '';
    FFileData.Clear;
    FLineData.Clear;
    FRainfallRecord.Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.Count: integer;
const OPNAME = 'TRainFallFileAgent.Count';
begin
  Result := 0;
  try
    Result := FFileData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.CurrentPosition: integer;
const OPNAME = 'TRainFallFileAgent.Count';
begin
  Result := -1;
  try
    Result := FCurrentPosition;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.First: TRainfallRecord;
const OPNAME = 'TRainFallFileAgent.First';
begin
  Result := nil;
  try
    if(FFileData.Count = 0) then
      FCurrentPosition := -1
    else
    begin
      FCurrentPosition := 0;
      Result := Next;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.Last: TRainfallRecord;
const OPNAME = 'TRainFallFileAgent.Last';
begin
  Result := nil;
  try
    if(FFileData.Count = 0) then
      FCurrentPosition := -1
    else
    begin
      FCurrentPosition := FFileData.Count-1;
      Result := Next;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRainFallFileAgent.Next: TRainfallRecord;
const OPNAME = 'TRainFallFileAgent.Next';
begin
  Result := nil;
  try
    if(FCurrentPosition >= 0) and (FCurrentPosition < FFileData.Count) then
    begin
      FLineData.CommaText := FFileData[FCurrentPosition];
      if FRainfallRecord.ReadFromString(FLineData) then
        Result           := FRainfallRecord;
      FCurrentPosition := FCurrentPosition + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.Previous: TRainfallRecord;
const OPNAME = 'TRainFallFileAgent.Previous';
begin
  Result := nil;
  try
    if(FCurrentPosition > 0) and (FCurrentPosition <= FFileData.Count) then
    begin
      FCurrentPosition := FCurrentPosition - 1;
      FLineData.CommaText := FFileData[FCurrentPosition];
      if FRainfallRecord.ReadFromString(FLineData) then
        Result           := FRainfallRecord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.LoadStationDailyData(AStationNumber : String): boolean;
const OPNAME = 'TRainFallFileAgent.LoadStationDailyData';
var
  LFileName : string;
begin
  Result := False;
  try
    Initialise;
    if(RWHModelData = nil) then Exit;
    LFileName := RWHModelData.GetRainfallStationInputFileName(AStationNumber);

    FFileData.LoadFromFile(LFileName);
    FStationNumber  :=  AStationNumber;
    if(FFileData.Count = 0) then
      FCurrentPosition := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.DaysInAnalysis: integer;
const OPNAME = 'TRainFallFileAgent.DaysInAnalysis';
begin
  Result := 0;
  try
    Result := FFileData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.StartMonth: integer;
const OPNAME = 'TRainFallFileAgent.StartMonth';
var
  LRainfallRecord  : TRainfallRecord;
begin
  Result := 0;
  try
    if(FFileData.Count > 0) then
    begin
      LRainfallRecord := First;
      Result := MonthOf(LRainfallRecord.Date);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.StartYear: integer;
const OPNAME = 'TRainFallFileAgent.StartYear';
var
  LRainfallRecord  : TRainfallRecord;
begin
  Result := 0;
  try
    if(FFileData.Count > 0) then
    begin
      LRainfallRecord := First;
      Result := YearOf(LRainfallRecord.Date);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.YearsInAnalysis: integer;
const OPNAME = 'TRainFallFileAgent.StartYear';
var
  LRainfallRecord  : TRainfallRecord;
  LStartDate       : TDateTime;
  LEndDate         : TDateTime;
begin
  Result := 0;
  try
    if(FFileData.Count > 0) then
    begin
      LRainfallRecord := First;
      if(LRainfallRecord <> nil) then
      begin
        LStartDate := LRainfallRecord.Date;
        LRainfallRecord   := Last;
        if(LRainfallRecord <> nil) then
        begin
          LEndDate := LRainfallRecord.Date;
          Result := YearsBetween(LEndDate,LStartDate);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.StartDate: TDate;
const OPNAME = 'TRainFallFileAgent.StartDate';
var
  LRainfallRecord  : TRainfallRecord;
begin
  Result := NullDateTime;
  try
    if(FFileData.Count > 0) then
    begin
      LRainfallRecord := First;
      Result := LRainfallRecord.Date;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.EndDate: TDate;
const OPNAME = 'TRainFallFileAgent.EndDate';
var
  LRainfallRecord  : TRainfallRecord;
begin
  Result := NullDateTime;
  try
    if(FFileData.Count > 0) then
    begin
      LRainfallRecord := Last;
      Result := LRainfallRecord.Date;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.StartNoNullDate: TDate;
const OPNAME = 'TRainFallFileAgent.StartNoNullDate';
var
  LRainfallRecord  : TRainfallRecord;
begin
  Result := NullDateTime;
  try
    LRainfallRecord := First;
    while (LRainfallRecord <> nil) do
    begin
      if(LRainfallRecord.Rainfall >= 0.00) then
      begin
        Result := LRainfallRecord.Date;
        Break;
      end;
      LRainfallRecord := Next;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.EndNoNullDate: TDate;
const OPNAME = 'TRainFallFileAgent.StartNoNullDate';
var
  LRainfallRecord  : TRainfallRecord;
begin
  Result := NullDateTime;
  try
    LRainfallRecord := Last;
    while (LRainfallRecord <> nil) do
    begin
      if(LRainfallRecord.Rainfall >= 0.00) then
      begin
        Result := LRainfallRecord.Date;
        Break;
      end;
      LRainfallRecord := Previous;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainFallFileAgent.NonBreakingData: integer;
const OPNAME = 'TRainFallFileAgent.StartNoNullDate';
var
  LRainfallRecord  : TRainfallRecord;
  LCount           : integer;
begin
  Result := 0;
  try
    LCount := 0;
    LRainfallRecord := First;
    while (LRainfallRecord <> nil) do
    begin
      if(LRainfallRecord.Rainfall < 0.00) then
      begin
        if(LCount > Result) then
          Result := LCount
        else
          LCount := 0;
      end
      else
        LCount := LCount + 1;
      LRainfallRecord := Next;
    end;
    if(LCount > Result) then
      Result := LCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
