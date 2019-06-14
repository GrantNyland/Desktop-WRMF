{*******************************************************}
{                                                       }
{  UNIT      : Contains TOutputAnnualFileAgent          }
{  AUTHOR    : Dziedzi Ramulondi                        }
{  DATE      : 27/07/2011                               }
{  COPYRIGHT : Copyright © 2011 T-Systems               }
{                                                       }
{*******************************************************}

unit UOutputAnnualFileAgent;

interface

Uses
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
  TOutputAnnualRecord = class(TObject)
  protected
    FReliability    : integer;
    FDaysSupplied  : array[1..10] of integer;
    function Get_DaysSupplied(AIndex : integer): integer;
  public
    procedure Reset;
    function ReadFromString(ARowData : String): boolean;
    function WriteToString: string;
    property Reliability       : integer read FReliability      write FReliability;
    property DaysSupplied[AIndex : integer]   : integer    read Get_DaysSupplied;
  end;

  TOutputAnnualFileAgent = class(TObject)
  protected
    FTankVolume        : double;
    FCurrentPosition   : integer;
    FFileData          : TStringList;
    FFileName          : string;
    FOutputFileHeader  : TOutputFileHeader;
    FOutputAnnualRecord : TOutputAnnualRecord;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Initialise;
    function First: TOutputAnnualRecord;
    function Last: TOutputAnnualRecord;
    function Next: TOutputAnnualRecord;
    function Previous: TOutputAnnualRecord;
    function CurrentPosition:integer;
    function Count:integer;
    function LoadFile(AFileName: string):boolean;
    property FileName: string read FFileName;
    property OutputFileHeader: TOutputFileHeader read FOutputFileHeader;
  end;

implementation

uses
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TOutputAnnualRecord }

procedure TOutputAnnualRecord.Reset;
const OPNAME = 'TOutputAnnualRecord.Reset';
var
  LIndex : integer;
begin
  try
    FReliability := 0;
    for LIndex := 1 to 10 do
      FDaysSupplied[LIndex]    := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualRecord.Get_DaysSupplied(AIndex: integer): integer;
const OPNAME = 'TOutputAnnualRecord.Get_DaysSupplied';
begin
  Result := 0;
  try
    if(AIndex >= 1) and (AIndex <= 12) then
      Result := FDaysSupplied[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualRecord.ReadFromString(ARowData: String): boolean;
const OPNAME = 'TOutputAnnualRecord.ReadFromString';
var
  LStr : string;
begin
  Result := False;
  try
    Reset;
    LStr := Trim(Copy(ARowData,1,11));
    if(LStr <> '') then
      FReliability        := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,12,10));
    if(LStr <> '') then
      FDaysSupplied[1]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,22,10));
    if(LStr <> '') then
      FDaysSupplied[2]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,32,10));
    if(LStr <> '') then
      FDaysSupplied[3]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,42,10));
    if(LStr <> '') then
      FDaysSupplied[4]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,52,10));
    if(LStr <> '') then
      FDaysSupplied[5]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,62,10));
    if(LStr <> '') then
      FDaysSupplied[6]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,72,10));
    if(LStr <> '') then
      FDaysSupplied[7]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,82,10));
    if(LStr <> '') then
      FDaysSupplied[8]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,92,10));
    if(LStr <> '') then
      FDaysSupplied[9]   := StrToInt(LStr);

    LStr := Trim(Copy(ARowData,102,10));
    if(LStr <> '') then
      FDaysSupplied[10]   := StrToInt(LStr);
    Result := True
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualRecord.WriteToString: string;
const OPNAME = 'TOutputAnnualFileAgent.WriteToString';
begin
  Result := '';
  try
    Result := Result + PadLeftString(IntToStr(FReliability),11);
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
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOutputAnnualFileAgent }

procedure TOutputAnnualFileAgent.AfterConstruction;
const OPNAME = 'TOutputAnnualFileAgent.AfterConstruction';
begin
  inherited;
  try
    FFileData          := TStringList.Create;
    FOutputFileHeader  := TOutputFileHeader.Create;
    FOutputAnnualRecord := TOutputAnnualRecord.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputAnnualFileAgent.BeforeDestruction;
const OPNAME = 'TOutputAnnualFileAgent.AfterConstruction';
begin
  inherited;
  try
    FreeAndNil(FFileData);
    FreeAndNil(FOutputFileHeader);
    FreeAndNil(FOutputAnnualRecord);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputAnnualFileAgent.Initialise;
const OPNAME = 'TOutputAnnualFileAgent.Initialise';
begin
  inherited;
  try
    FCurrentPosition := -1;
    FFileName        := '';
    FFileData.Clear;
    FOutputFileHeader.Reset;
    FOutputAnnualRecord.Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualFileAgent.Count: integer;
const OPNAME = 'TOutputAnnualFileAgent.Count';
begin
  Result := 0;
  try
    Result := FFileData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualFileAgent.CurrentPosition: integer;
const OPNAME = 'TOutputAnnualFileAgent.CurrentPosition';
begin
  Result := -1;
  try
    Result := FCurrentPosition;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualFileAgent.LoadFile(AFileName: string): boolean;
const OPNAME = 'TOutputAnnualFileAgent.LoadFile';
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
      else
        FCurrentPosition := 2;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualFileAgent.First: TOutputAnnualRecord;
const OPNAME = 'TOutputAnnualFileAgent.First';
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

function TOutputAnnualFileAgent.Last: TOutputAnnualRecord;
const OPNAME = 'TOutputAnnualFileAgent.Last';
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

function TOutputAnnualFileAgent.Next: TOutputAnnualRecord;
const OPNAME = 'TOutputAnnualFileAgent.Next';
var
  LHeading : string;
begin
  Result := nil;
  try
    if(FCurrentPosition >= 0) and (FCurrentPosition < FFileData.Count) then
    begin
      LHeading := FOutputFileHeader.MonthNamesText;
      if(Pos(LHeading,FFileData[FCurrentPosition]) > 0) then Exit;
      if FOutputAnnualRecord.ReadFromString( FFileData[FCurrentPosition]) then
        Result           := FOutputAnnualRecord;
      FCurrentPosition := FCurrentPosition + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputAnnualFileAgent.Previous: TOutputAnnualRecord;
const OPNAME = 'TOutputAnnualFileAgent.Previous';
begin
  Result := nil;
  try
    if(FCurrentPosition > 0) and (FCurrentPosition <= FFileData.Count) then
    begin
      FCurrentPosition := FCurrentPosition - 1;
      if FOutputAnnualRecord.ReadFromString(FFileData[FCurrentPosition]) then
        Result           := FOutputAnnualRecord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
