{*******************************************************}
{                                                       }
{  UNIT      : Contains TOutputMonthlyFileAgent           }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/07/2011                               }
{  COPYRIGHT : Copyright © 2011 T-Systems               }
{                                                       }
{*******************************************************}

unit UOutputMonthlyFileAgent;

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
  TOutputMonthlyRecord = class(TObject)
  protected
    FYear          : integer;
    FDaysSupplied  : array[1..12] of integer;
    function Get_DaysSupplied(AIndex : integer): integer;
  public
    procedure Reset;
    function ReadFromString(ARowData : String): boolean;
    function WriteToString: string;
    property Year       : integer read FYear      write FYear;
    property DaysSupplied[AIndex : integer]   : integer    read Get_DaysSupplied;
  end;

  TOutputMonthlyFileAgent = class(TObject)
  protected
    FTankVolume        : double;
    FCurrentPosition   : integer;
    FFileData          : TStringList;
    FFileName          : string;
    FOutputFileHeader  : TOutputFileHeader;
    FOutputMonthlyRecord : TOutputMonthlyRecord;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Initialise;
    function First: TOutputMonthlyRecord;
    function Last: TOutputMonthlyRecord;
    function Next: TOutputMonthlyRecord;
    function Previous: TOutputMonthlyRecord;
    function CurrentPosition:integer;
    function Count:integer;
    function LoadFile(AFileName: string):boolean;
    function StartReadingTankVolume(AVolume: double):boolean;
    property FileName: string read FFileName;
    property OutputFileHeader: TOutputFileHeader read FOutputFileHeader;
  end;

implementation

uses
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TOutputMonthlyRecord }

procedure TOutputMonthlyRecord.Reset;
const OPNAME = 'TOutputMonthlyRecord.Reset';
var
  LIndex : integer;
begin
  try
    FYear        := 0;
    for LIndex := 1 to 12 do
      FDaysSupplied[LIndex]    := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyRecord.Get_DaysSupplied(AIndex: integer): integer;
const OPNAME = 'TOutputMonthlyRecord.Get_DaysSupplied';
begin
  Result := 0;
  try
    if(AIndex >= 1) and (AIndex <= 12) then
      Result := FDaysSupplied[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyRecord.ReadFromString(ARowData: String): boolean;
const OPNAME = 'TOutputMonthlyRecord.ReadFromString';
var
  LStr : string;
begin
  Result := False;
  try
    Reset;
    LStr := Trim(Copy(ARowData,1,5));
    if(LStr <> '') then
      FYear        := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,6,10));
    if(LStr <> '') then
      FDaysSupplied[1]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,16,10));
    if(LStr <> '') then
      FDaysSupplied[2]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,26,10));
    if(LStr <> '') then
      FDaysSupplied[3]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,36,10));
    if(LStr <> '') then
      FDaysSupplied[4]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,46,10));
    if(LStr <> '') then
      FDaysSupplied[5]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,56,10));
    if(LStr <> '') then
      FDaysSupplied[6]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,66,10));
    if(LStr <> '') then
      FDaysSupplied[7]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,76,10));
    if(LStr <> '') then
      FDaysSupplied[8]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,86,10));
    if(LStr <> '') then
      FDaysSupplied[9]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,96,10));
    if(LStr <> '') then
      FDaysSupplied[10]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,106,10));
    if(LStr <> '') then
      FDaysSupplied[11]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,116,10));
    if(LStr <> '') then
      FDaysSupplied[12]   := StrToInt(LStr);
    Result := True
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyRecord.WriteToString: string;
const OPNAME = 'TOutputMonthlyFileAgent.WriteToString';
begin
  Result := '';
  try
    Result := IntToStr(FYear) + '';
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[1]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[2]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[3]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[4]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[5]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[6]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[7]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[8]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[9]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[10]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[11]),10);
    Result := Result + PadLeftString(IntToStr(FDaysSupplied[12]),10);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOutputMonthlyFileAgent }

procedure TOutputMonthlyFileAgent.AfterConstruction;
const OPNAME = 'TOutputMonthlyFileAgent.AfterConstruction';
begin
  inherited;
  try
    FFileData          := TStringList.Create;
    FOutputFileHeader  := TOutputFileHeader.Create;
    FOutputMonthlyRecord := TOutputMonthlyRecord.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyFileAgent.BeforeDestruction;
const OPNAME = 'TOutputMonthlyFileAgent.AfterConstruction';
begin
  inherited;
  try
    FreeAndNil(FFileData);
    FreeAndNil(FOutputFileHeader);
    FreeAndNil(FOutputMonthlyRecord);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputMonthlyFileAgent.Initialise;
const OPNAME = 'TOutputMonthlyFileAgent.Initialise';
begin
  inherited;
  try
    FCurrentPosition := -1;
    FFileName        := '';
    FFileData.Clear;
    FOutputFileHeader.Reset;
    FOutputMonthlyRecord.Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyFileAgent.Count: integer;
const OPNAME = 'TOutputMonthlyFileAgent.Count';
begin
  Result := 0;
  try
    Result := FFileData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyFileAgent.CurrentPosition: integer;
const OPNAME = 'TOutputMonthlyFileAgent.CurrentPosition';
begin
  Result := -1;
  try
    Result := FCurrentPosition;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyFileAgent.LoadFile(AFileName: string): boolean;
const OPNAME = 'TOutputMonthlyFileAgent.LoadFile';
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

function TOutputMonthlyFileAgent.StartReadingTankVolume(AVolume: double): boolean;
const OPNAME = 'TOutputMonthlyFileAgent.StartReadingTankVolume';
var
  LIndex : integer;
  LStr   : string;
  LValue : double;
  LHeading : string;
begin
  Result := False;
  try
    LHeading := FOutputFileHeader.MonthNamesText;
    for LIndex := 0 to FFileData.Count-1 do
    begin
      if(Pos(LHeading,FFileData[LIndex]) > 0) then
      begin
        LStr   := FFileData[LIndex];
        LStr   := ExtractLastSubstring(LStr);
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

function TOutputMonthlyFileAgent.First: TOutputMonthlyRecord;
const OPNAME = 'TOutputMonthlyFileAgent.First';
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

function TOutputMonthlyFileAgent.Last: TOutputMonthlyRecord;
const OPNAME = 'TOutputMonthlyFileAgent.Last';
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

function TOutputMonthlyFileAgent.Next: TOutputMonthlyRecord;
const OPNAME = 'TOutputMonthlyFileAgent.Next';
var
  LHeading : string;
begin
  Result := nil;
  try
    if(FCurrentPosition >= 0) and (FCurrentPosition < FFileData.Count) then
    begin
      if(Pos(strHeadingFile2,FFileData[FCurrentPosition]) > 0) then Exit;
      LHeading := FOutputFileHeader.MonthNamesText;
      if(Pos(LHeading,FFileData[FCurrentPosition]) > 0) then Exit;
      if FOutputMonthlyRecord.ReadFromString( FFileData[FCurrentPosition]) then
        Result           := FOutputMonthlyRecord;
      FCurrentPosition := FCurrentPosition + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputMonthlyFileAgent.Previous: TOutputMonthlyRecord;
const OPNAME = 'TOutputMonthlyFileAgent.Previous';
begin
  Result := nil;
  try
    if(FCurrentPosition > 0) and (FCurrentPosition <= FFileData.Count) then
    begin
      FCurrentPosition := FCurrentPosition - 1;
      if FOutputMonthlyRecord.ReadFromString(FFileData[FCurrentPosition]) then
        Result           := FOutputMonthlyRecord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
