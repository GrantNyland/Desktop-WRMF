unit UWRPMResFileManager;
//
//
//  UNIT      : Contains TWRPMResFileManager
//  AUTHOR    :  Diedzi Ramulondi
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2012 DWAF
//
//

interface

uses
  Classes, sysutils,
  UConstants,
  UAbstractObject,
  VoaimsCom_TLB,
  UWRPMOutputSettings;

type

  TWRPMResFileManager = class(TAbstractAppObject)
  protected
    FFile : TextFile;
    FChannelNumber : integer;
    FChannelIndex  : integer;
    function GetBlockSize: integer;
    function ReadBlock(var ASequenceNumber : integer;ABlockSize : integer; ABlockData : TStrings): boolean;
    function ReadChannelBlock(var ASequenceNumber : integer;ABlockSize : integer; ABlockData : TStrings): boolean;
  public
    function ReadElementData(ADataContainer: TStrings;AFileName: string): boolean;
    function ReadInterBasinSupportChannelData(ADataContainer: TStrings;AFileName: string;AChannelNumber: integer): boolean;
  end;


implementation

uses
  DateUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TWRPMResFileManager }

function TWRPMResFileManager.GetBlockSize: integer;
const OPNAME = 'TWRPMResFileManager.ReadBlock';
var
  LLineData   : string;
begin
  Result := 0;
  try
    while not EOF(FFile) do
    begin
      ReadLn(FFile, LLineData);
      if(Pos('ACRES INTERNATIONAL LIMITED',LLineData) > 0) and (Result > 0) then
        Exit;
      Result := Result + 1;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMResFileManager.ReadBlock(var ASequenceNumber : integer;ABlockSize : integer;ABlockData: TStrings): boolean;
const OPNAME = 'TWRPMResFileManager.ReadBlock';
var
  LIndex          : integer;
  LReadLineData   : string;
  LWriteLineData  : string;
  LValue          : string;
begin
  Result := False;
  try
    ABlockData.Clear;
    for LIndex := 1 to ABlockSize do
    begin
      if EOF(FFile) then Break;
      ReadLn(FFile, LReadLineData);

      //Get the sequence number
      if(LIndex = 7) then
      begin
        LValue := Copy(LReadLineData,118,3);
        if(LValue = '***') then
          ASequenceNumber := ASequenceNumber + 1
        else
          ASequenceNumber := StrToIntDef(LValue,ASequenceNumber + 1);
      end;

      //Read demand and supply data
      if(LIndex >= 13) then
      begin
        LWriteLineData := IntToStr(ASequenceNumber)+',';
        LValue         := Trim(ExtractLengthFirstSubstring(9,LReadLineData));
        LWriteLineData := LWriteLineData + LValue+',';
        LValue         := Trim(ExtractLengthFirstSubstring(10,LReadLineData));
        LWriteLineData := LWriteLineData + LValue+',';
        LValue         := Trim(ExtractLengthFirstSubstring(10,LReadLineData));
        LWriteLineData := LWriteLineData + LValue+',';
        LValue         := Trim(ExtractLengthFirstSubstring(10,LReadLineData));
        LWriteLineData := LWriteLineData + LValue;
        ABlockData.Add(LWriteLineData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMResFileManager.ReadChannelBlock(var ASequenceNumber: integer;ABlockSize: integer; ABlockData: TStrings): boolean;
const OPNAME = 'TWRPMResFileManager.ReadChannelBlock';
var
  LChannelIndex,
  LChannelNumber,
  LIndex          : integer;
  LReadLineData   : string;
  LWriteLineData  : string;
  LValue          : string;
begin
  Result := False;
  try
    ABlockData.Clear;
    for LIndex := 1 to ABlockSize do
    begin
      if EOF(FFile) then Break;
      ReadLn(FFile, LReadLineData);

      //Get the sequence number
      if(LIndex = 7) then
      begin
        LValue := Copy(LReadLineData,118,3);
        if(LValue = '***') then
          ASequenceNumber := ASequenceNumber + 1
        else
          ASequenceNumber := StrToIntDef(LValue,ASequenceNumber + 1);
      end;

      if(LIndex = 9) and (FChannelNumber > 0) and (FChannelIndex  = NullInteger) then
      begin
        LReadLineData := Copy(LReadLineData,40,Length(LReadLineData));
        LChannelIndex := 40;
        while (FChannelIndex  = NullInteger) do
        begin
          LValue          := Trim(ExtractLengthFirstSubstring(10,LReadLineData));
          LChannelNumber  := StrToIntDef(LValue,0);
          if(LChannelNumber = FChannelNumber) then
          begin
            FChannelIndex := LChannelIndex;
            Break;
          end;

          LChannelIndex := LChannelIndex + 10;
          if(LReadLineData = '') then
            Break;
        end;
        if(FChannelIndex  = NullInteger) then
          Exit;
      end;

      //Read demand and supply data
      if(LIndex >= 13) then
      begin
        LWriteLineData := IntToStr(ASequenceNumber)+',';
        LValue         := Trim(Copy(LReadLineData,1,9));
        LWriteLineData := LWriteLineData + LValue+',';
        LValue         := Trim(Copy(LReadLineData,FChannelIndex,10));
        LWriteLineData := LWriteLineData + LValue;
        ABlockData.Add(LWriteLineData);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMResFileManager.ReadElementData(ADataContainer: TStrings;AFileName: string): boolean;
const OPNAME = 'TWRPMResFileManager.ReadElementData';
var
  LSequenceNum : integer;
  LBlockSize   : integer;
  LBlockData   : TStringList;
begin
  Result  := False;
  try
    FChannelNumber := 0;
    FChannelIndex  := NullInteger;
    ADataContainer.Clear;
    if FileExists(AFileName) then
    begin
      AssignFile(FFile, AFileName) ;
      Reset(FFile) ;
      LBlockSize := GetBlockSize;
      if(LBlockSize = 0) then Exit;
      Reset(FFile) ;
      LBlockData := TStringList.Create;
      try
        LSequenceNum := 0;
        while not EOF(FFile) do
        begin
         if not ReadBlock(LSequenceNum,LBlockSize,LBlockData) then
           Break
         else
           ADataContainer.AddStrings(LBlockData)
        end;
        Result  := True;
      finally
        LBlockData.Free;
        CloseFile(FFile) ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMResFileManager.ReadInterBasinSupportChannelData(ADataContainer: TStrings; AFileName: string;AChannelNumber: integer): boolean;
const OPNAME = 'TWRPMResFileManager.ReadElementData';
var
  LSequenceNum : integer;
  LBlockSize   : integer;
  LBlockData   : TStringList;
begin
  Result  := False;
  try
    FChannelNumber := AChannelNumber;
    FChannelIndex  := NullInteger;
    ADataContainer.Clear;
    if FileExists(AFileName) then
    begin
      AssignFile(FFile, AFileName) ;
      Reset(FFile) ;
      LBlockSize := GetBlockSize;
      if(LBlockSize = 0) then Exit;
      Reset(FFile) ;
      LBlockData := TStringList.Create;
      try
        LSequenceNum := 0;
        while not EOF(FFile) do
        begin
         if not ReadChannelBlock(LSequenceNum,LBlockSize,LBlockData) then
           Break
         else
           ADataContainer.AddStrings(LBlockData)
        end;
        Result  := True;
      finally
        LBlockData.Free;
        CloseFile(FFile) ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
