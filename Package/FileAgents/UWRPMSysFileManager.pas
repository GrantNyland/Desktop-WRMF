unit UWRPMSysFileManager;
//
//
//  UNIT      : Contains TWRPMSysFileManager
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

  TWRPMSysFileManager = class(TAbstractAppObject)
  protected
    FFile         : TextFile;
    FColumnName   : string;
    FColumnIndex  : integer;
    FColumnSize   : integer;
    FDecisionMonthName  : string;
    FAverage            : boolean;
    function GetBlockSize: integer;
    function ReadSequenceBlock(var ASequenceNumber : integer;ABlockSize : integer; ABlockData : TStrings): boolean;
  public
    function ReadElementData(ADataContainer: TStrings;AFileName,AColumnName,ADecisionMonthName: string): boolean;
  end;


implementation

uses
  DateUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TWRPMSysFileManager }

function TWRPMSysFileManager.GetBlockSize: integer;
const OPNAME = 'TWRPMSysFileManager.ReadBlock';
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

function TWRPMSysFileManager.ReadSequenceBlock(var ASequenceNumber: integer;ABlockSize: integer; ABlockData: TStrings): boolean;
const OPNAME = 'TWRPMSysFileManager.ReadSequenceBlock';
var
  LColumnIndex,
  LIndex          : integer;
  LReadLineData   : string;
  LWriteLineData  : string;
  L10ColsData     : string;
  L12ColsData     : string;
  LValue          : string;
  LPreYear        : string;
  //LCurrYear       : string;
  LStartMonth     : string;
  LCurrMonth       : string;
  LYearTotal      : double;
  LYearCount      : integer;
begin
  Result := False;
  try
    ABlockData.Clear;
    LPreYear     := '';
    //LCurrYear    := '';
    LStartMonth    := '';
    LCurrMonth    := '';
    LYearTotal   := 0.0;
    LYearCount   := 0;

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

      if(LIndex = 9) and (FColumnName <> '') and (FColumnIndex  = NullInteger) then
      begin
        LReadLineData   := Copy(LReadLineData,10,Length(LReadLineData));
        L10ColsData     := Copy(LReadLineData,1,Length(LReadLineData)-46);
        L12ColsData     := Copy(LReadLineData,(Length(LReadLineData)-46)+1,46);
        LColumnIndex := 10;
        while (FColumnIndex  = NullInteger) do
        begin
          LValue          := Trim(ExtractLengthFirstSubstring(10,L10ColsData));
          if(FColumnName = LValue) then
          begin
            FColumnIndex := LColumnIndex;
            FColumnSize  := 10;
            Break;
          end;

          LColumnIndex := LColumnIndex + 10;
          if(L10ColsData = '') then
            Break;
        end;
        while (FColumnIndex  = NullInteger) do
        begin
          LValue          := Trim(ExtractLengthFirstSubstring(12,L12ColsData));
          if(FColumnName = LValue) then
          begin
            FColumnIndex := LColumnIndex;
            FColumnSize  := 12;
            Break;
          end;

          LColumnIndex := LColumnIndex + 12;
          if(L12ColsData = '') then
            Break;
        end;

        if(FColumnIndex  = NullInteger) then
          Exit;
      end;

      //Read demand and supply data
      if(LIndex >= 13) then
      begin
        if FAverage then
        begin
          {LCurrYear    := Trim(Copy(LReadLineData,1,9));
          if(LPreYear <>  LCurrYear) then
          begin
            if(LPreYear <> '') then
            begin
              LWriteLineData := IntToStr(ASequenceNumber)+',';
              LWriteLineData := LWriteLineData + LPreYear+',';
              LWriteLineData := LWriteLineData + FormatFloat('##0.0000',LYearTotal/LYearCount);
              ABlockData.Add(LWriteLineData);
            end;
            LPreYear     := LCurrYear;
            LYearTotal   := 0.0;
            LYearCount   := 0;
          end;}
          LCurrMonth    := GatLastSubstring(LReadLineData);
          if(LStartMonth =  LCurrMonth) then
          begin
            LWriteLineData := IntToStr(ASequenceNumber)+',';
            LWriteLineData := LWriteLineData + LPreYear+',';
            LWriteLineData := LWriteLineData + FormatFloat('##0.0000',LYearTotal/LYearCount);
            ABlockData.Add(LWriteLineData);

            //LPreMonth     := LCurrMonth;
            LYearTotal   := 0.0;
            LYearCount   := 0;
            //LPreYear     := Trim(Copy(LReadLineData,1,9));
          end;
          if(LStartMonth = '') then
          begin
            LStartMonth := LCurrMonth;
          end;
          LPreYear     := Trim(Copy(LReadLineData,1,9));
          LValue       := Trim(Copy(LReadLineData,FColumnIndex,FColumnSize));
          LYearTotal   := LYearTotal + StrToFloatDef(LValue,0.0);
          LYearCount   := LYearCount + 1;
        end
        else
        begin
          LValue         := Trim(Copy(LReadLineData,(Length(LReadLineData)-10)+1,10));
          if(LValue <> FDecisionMonthName) then Continue;

          LWriteLineData := IntToStr(ASequenceNumber)+',';
          LValue         := Trim(Copy(LReadLineData,1,9));
          LWriteLineData := LWriteLineData + LValue+',';
          LValue         := Trim(Copy(LReadLineData,FColumnIndex,FColumnSize));
          LWriteLineData := LWriteLineData + LValue;
          ABlockData.Add(LWriteLineData);
        end;
      end;
    end;
    if FAverage and (LYearCount > 0) then
    begin
      LWriteLineData := IntToStr(ASequenceNumber)+',';
      LWriteLineData := LWriteLineData + LPreYear+',';
      LWriteLineData := LWriteLineData + FormatFloat('##0.0000',LYearTotal/LYearCount);
      ABlockData.Add(LWriteLineData);
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMSysFileManager.ReadElementData(ADataContainer: TStrings;AFileName,AColumnName,ADecisionMonthName: string): boolean;
const OPNAME = 'TWRPMSysFileManager.ReadElementData';
var
  LSequenceNum : integer;
  LBlockSize   : integer;
  LBlockData   : TStringList;
begin
  Result  := False;
  try
    ADataContainer.Clear;
    if FileExists(AFileName) then
    begin
      FAverage           := (ADecisionMonthName = '');
      FColumnName        := AColumnName;
      FDecisionMonthName := ADecisionMonthName;
      FColumnIndex       := NullInteger;
      FColumnSize        := NullInteger;

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
         if not ReadSequenceBlock(LSequenceNum,LBlockSize,LBlockData) then
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
