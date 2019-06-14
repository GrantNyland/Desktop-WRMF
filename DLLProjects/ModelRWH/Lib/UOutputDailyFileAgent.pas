{*******************************************************}
{                                                       }
{  UNIT      : Contains TOutputDailyFileAgent           }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/07/2011                               }
{  COPYRIGHT : Copyright © 2011 T-Systems               }
{                                                       }
{*******************************************************}

unit UOutputDailyFileAgent;

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
  UAbstractObject,
  UOutputFileHeader;

type
  TOutputDailyRecord = class(TObject)
  protected
    FDate        : TDateTime;
    FRainfall    : double;
    FStorage     : double;
    FSpill       : double;
    FDeficit     : double;
    //FDecide      : integer;
  public
    procedure Reset;
    function ReadFromString(ARowData : String): boolean;
    function WriteToString: string;
    property Date       : TDateTime read FDate      write FDate;
    property Rainfall   : Double    read FRainfall  write FRainfall;
    property Storage    : Double    read FStorage   write FStorage;
    property Spill      : Double    read FSpill     write FSpill;
    property Deficit    : Double    read FDeficit   write FDeficit;
    //property Decide     : integer   read FDecide    write FDecide;
  end;

  TOutputDailyFileAgent = class(TObject)
  protected
    FTankVolume        : double;
    FCurrentPosition   : integer;
    FFileData          : TStringList;
    FFileName          : string;
    FOutputFileHeader  : TOutputFileHeader;
    FOutputDailyRecord : TOutputDailyRecord;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Initialise;
    function First: TOutputDailyRecord;
    function Last: TOutputDailyRecord;
    function Next: TOutputDailyRecord;
    function Previous: TOutputDailyRecord;
    function CurrentPosition:integer;
    function Count:integer;
    function LoadFile(AFileName: string):boolean;
    function StartReadingTankVolume(AVolume: double):boolean;
    property FileName: string read FFileName;
    property OutputFileHeader: TOutputFileHeader read FOutputFileHeader;
  end;

implementation

uses
  DateUtils,
  UConstants,
  UUtilities,
  UErrorHandlingOperations;

{ TOutputDailyRecord }

procedure TOutputDailyRecord.Reset;
const OPNAME = 'TOutputDailyRecord.Reset';
begin
  try
    FDate        := NullDateTime;
    FRainfall    := NullFloat;
    FStorage     := NullFloat;
    FSpill       := NullFloat;
    FDeficit     := NullFloat;
    //FDecide      := NullInteger;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyRecord.ReadFromString(ARowData: String): boolean;
const OPNAME = 'TOutputDailyRecord.ReadFromString';
var
  LStr : string;
begin
  Result := False;
  try
    Reset;
    if(Pos(strHeadingFile1,ARowData) > 0) then Exit;
    LStr := Trim(Copy(ARowData,1,10));
    if(LStr <> '') then
      FDate        := StrToDate(LStr);
    LStr := Trim(Copy(ARowData,11,10));
    if(LStr <> '') then
      FRainfall   := StrToFloat(LStr);
    LStr := Trim(Copy(ARowData,21,10));
    if(LStr <> '') then
      FStorage     := StrToFloat(LStr);
    LStr := Trim(Copy(ARowData,31,10));
    if(LStr <> '') then
      FSpill       := StrToFloat(LStr);
    LStr := Trim(Copy(ARowData,41,10));
    if(LStr <> '') then
      FDeficit     := StrToFloat(LStr);
    LStr := Trim(Copy(ARowData,51,10));
    //if(LStr <> '') then
    //  FDecide      := StrToInt(LStr);
    Result := True
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyRecord.WriteToString: string;
const OPNAME = 'TOutputDailyFileAgent.WriteToString';
begin
  Result := '';
  try
    Result := DateToStr(FDate);
    Result := Result + PadLeftString(FormatFloat('#.000',FRainfall),10);
    Result := Result + PadLeftString(FormatFloat('#.000',FStorage),10);
    Result := Result + PadLeftString(FormatFloat('#.000',FSpill),10);
    Result := Result + PadLeftString(FormatFloat('#.000',FDeficit),10);
    //Result := Result + PadLeftString(IntToStr(FDecide),10);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOutputDailyFileAgent }

procedure TOutputDailyFileAgent.AfterConstruction;
const OPNAME = 'TOutputDailyFileAgent.AfterConstruction';
begin
  inherited;
  try
    FFileData          := TStringList.Create;
    FOutputFileHeader  := TOutputFileHeader.Create;
    FOutputDailyRecord := TOutputDailyRecord.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDailyFileAgent.BeforeDestruction;
const OPNAME = 'TOutputDailyFileAgent.AfterConstruction';
begin
  inherited;
  try
    FreeAndNil(FFileData);
    FreeAndNil(FOutputFileHeader);
    FreeAndNil(FOutputDailyRecord);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDailyFileAgent.Initialise;
const OPNAME = 'TOutputDailyFileAgent.Initialise';
begin
  inherited;
  try
    FCurrentPosition := -1;
    FFileName        := '';
    FFileData.Clear;
    FOutputFileHeader.Reset;
    FOutputDailyRecord.Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyFileAgent.Count: integer;
const OPNAME = 'TOutputDailyFileAgent.Count';
begin
  Result := 0;
  try
    Result := FFileData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyFileAgent.CurrentPosition: integer;
const OPNAME = 'TOutputDailyFileAgent.CurrentPosition';
begin
  Result := -1;
  try
    Result := FCurrentPosition;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyFileAgent.LoadFile(AFileName: string): boolean;
const OPNAME = 'TOutputDailyFileAgent.LoadFile';
var
  LIndex : integer;
begin
  Result := False;
  try
    Initialise;
    if FileExists(AFileName) then
    begin
      FFileData.LoadFromFile(AFileName);
      if(FFileData.Count < OutputFileDataStartRow) then
        FFileData.Clear
      else
      begin
        FOutputFileHeader.ReadFromStringList(FFileData);
        for LIndex := 1 to OutputFileDataStartRow -1 do
         FFileData.Delete(0);
        LIndex := FFileData.Count-1;
        while(LIndex >= 0) do
        begin
          if(Trim(FFileData[LIndex]) = '') then
            FFileData.Delete(LIndex);
          LIndex := LIndex - 1;
        end;
      end;
      FFileName  :=  AFileName;
      if(FFileData.Count = 0) then
        FCurrentPosition := -1
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyFileAgent.StartReadingTankVolume(AVolume: double): boolean;
const OPNAME = 'TOutputDailyFileAgent.StartReadingTankVolume';
var
  LIndex : integer;
  LStr   : string;
  LValue : double;
begin
  Result := False;
  try
    for LIndex := 0 to FFileData.Count-1 do
    begin
      if(Pos(strHeadingFile1,FFileData[LIndex]) > 0) then
      begin
        LStr   := FFileData[LIndex];
        LStr   := Copy(LStr,Length(strHeadingFile1)+1,Length(LStr));
        LValue := StrToFloatDef(LStr,0.0);
        if(LValue = AVolume) then
        begin
          FCurrentPosition := LIndex+1;
          Break;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyFileAgent.First: TOutputDailyRecord;
const OPNAME = 'TOutputDailyFileAgent.First';
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

function TOutputDailyFileAgent.Last: TOutputDailyRecord;
const OPNAME = 'TOutputDailyFileAgent.Last';
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

function TOutputDailyFileAgent.Next: TOutputDailyRecord;
const OPNAME = 'TOutputDailyFileAgent.Next';
begin
  Result := nil;
  try
    if(FCurrentPosition >= 0) and (FCurrentPosition < FFileData.Count) then
    begin
      if FOutputDailyRecord.ReadFromString( FFileData[FCurrentPosition]) then
        Result           := FOutputDailyRecord;
      FCurrentPosition := FCurrentPosition + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDailyFileAgent.Previous: TOutputDailyRecord;
const OPNAME = 'TOutputDailyFileAgent.Previous';
begin
  Result := nil;
  try
    if(FCurrentPosition > 0) and (FCurrentPosition <= FFileData.Count) then
    begin
      FCurrentPosition := FCurrentPosition - 1;
      if FOutputDailyRecord.ReadFromString(FFileData[FCurrentPosition]) then
        Result           := FOutputDailyRecord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
