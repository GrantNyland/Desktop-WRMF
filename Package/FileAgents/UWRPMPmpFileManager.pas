unit UWRPMPmpFileManager;
//
//
//  UNIT      : Contains TWRPMPmpFileManager
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

  TWRPMPmpFileManager = class(TAbstractAppObject)
  protected
    FFileHeaderData    : TWRPMPmpFileHeaderData;
    FStartDataLineNo   : integer;
    FDateTime          : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function ReadHeaderData(AFileName : string): boolean;
    function ReadElementData(ASettings : TWRPMOutputSettings;ADataContainer : TStrings): boolean;
    property HeaderData             : TWRPMPmpFileHeaderData     read FFileHeaderData;
  end;


implementation

uses
  DateUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TWRPMPmpFileManager }

procedure TWRPMPmpFileManager.CreateMemberObjects;
const OPNAME = 'TWRPMPmpFileManager.CreateMemberObjects';
begin
  inherited;
  try
    FFileHeaderData := TWRPMPmpFileHeaderData.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPmpFileManager.DestroyMemberObjects;
const OPNAME = 'TWRPMPmpFileManager.CreateMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FFileHeaderData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPmpFileManager.Initialise: boolean;
const OPNAME = 'TWRPMPmpFileManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FStartDataLineNo         := NullInteger;
    FDateTime                := '';
    FFileHeaderData.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPmpFileManager.ReadHeaderData(AFileName: string): boolean;
const OPNAME = 'TWRPMPmpFileManager.ReadHeaderData';
var
  LIndex     : integer;
  LLineNo    : integer;
  LFile      : TextFile;
  LLineData  : string;
  LTempString : String;
  LFileAge    : TDateTime;
  LPumpingChannelLinesCount : integer;
begin
  Result  := False;
  try
    Initialise;
    if FileExists(AFileName) then
    begin
      AssignFile(LFile, AFileName) ;
      Reset(LFile) ;
      try
        LLineNo                         := 0;
        HeaderData.SequencesCount       := 0;
        HeaderData.PumpingChannelsCount := 0;
        FStartDataLineNo                := 0;

        //Read (Number Months) and (Number of sequences)
        if not EOF(LFile) then
        begin
          ReadLn(LFile, LLineData);
          LLineNo         := LLineNo +1;
          LTempString     := ExtractFirstSubstring(LLineData);
          HeaderData.MonthsCount    := StrToIntDef(LTempString,NullInteger);
          LTempString     := ExtractFirstSubstring(LLineData);
          HeaderData.SequencesCount := StrToIntDef(LTempString,NullInteger);
        end;

        //Read List of sequences
        while (HeaderData.SequencesList.Count < HeaderData.SequencesCount) do
        begin
          if not EOF(LFile) then
          begin
            ReadLn(LFile, LLineData);
            LLineNo  := LLineNo +1;
            while(LLineData <> '') do
            begin
              HeaderData.SequencesList.Add(ExtractFirstSubstring(LLineData));
              LLineData := Trim(LLineData);
            end;
          end;
        end;

        //Read Number of Pumping channels printed out
        if not EOF(LFile) then
        begin
          ReadLn(LFile, LLineData);
          LLineNo  := LLineNo +1;
          LTempString   := ExtractFirstSubstring(LLineData);
          HeaderData.PumpingChannelsCount  := StrToIntDef(LTempString,NullInteger);
        end;

        LPumpingChannelLinesCount := HeaderData.PumpingChannelsCount div 4;
        if((HeaderData.PumpingChannelsCount mod 4) <> 0) then
          LPumpingChannelLinesCount     := LPumpingChannelLinesCount + 1;

        //Read (Pumping channel names)
        for LIndex := 1 to LPumpingChannelLinesCount do
        begin
          if EOF(LFile) then Break;
          ReadLn(LFile, LLineData);
          LLineNo  := LLineNo +1;
          while(Trim(LLineData) <> '') do
          begin
            LTempString   := ExtractLengthFirstSubstring(30,LLineData);
            HeaderData.PumpingChannelsList.Add(Trim(LTempString));
          end;
        end;

        FStartDataLineNo := LLineNo;
        Result  := True;

      finally
        CloseFile(LFile) ;
      end;

      if FileAge(AFileName, LFileAge) then
       FDateTime := DateTimeToStr(LFileAge);

      //FDateTime := DateTimeToStr(FileDateToDateTime(LFileAge));

      HeaderData.FileName := AFileName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPmpFileManager.ReadElementData(ASettings : TWRPMOutputSettings;ADataContainer: TStrings): boolean;
const OPNAME = 'TWRPMPmpFileManager.ReadElementData';
var
  LIndex      : integer;
  LFile       : TextFile;
  LSequenceNo : string;
  LTempData   : string;
  LLineData   : string;
  LWholeLineData   : string;
  LSequenceIndex   : integer;
  LMonthIndex      : integer;
  LDateTime                : string;
  LFileAge,
  LCurrentDate             : TDateTime;
  LPumpingChannelLinesCount : integer;
  LPumpingChannelIndex      : integer;
  LMonthData               : TStringList;
begin
  Result  := False;
  try
    ADataContainer.Clear;
    if FileExists(ASettings.PumpFileName) then
    begin
      //Read File headers
      if(ASettings.PumpFileName <> HeaderData.FileName) then
      begin
        ReadHeaderData(ASettings.PumpFileName);
      end
      else
      begin
        //LDateTime := DateTimeToStr(FileDateToDateTime(FileAge(ASettings.PumpFileName)));
        if FileAge(ASettings.PumpFileName, LFileAge) then
          FDateTime := DateTimeToStr(LFileAge);

        if(FDateTime <> LDateTime) then
          ReadHeaderData(ASettings.PumpFileName)
      end;

      if(FStartDataLineNo = NullInteger) then
        Exit;

      LPumpingChannelIndex := HeaderData.PumpingChannelsList.IndexOf(ASettings.ElementName);
      if(LPumpingChannelIndex < 0) then
        Exit;
      LPumpingChannelIndex := (LPumpingChannelIndex*10)+1;

      LMonthData := TStringList.Create;
      AssignFile(LFile, ASettings.PumpFileName) ;
      Reset(LFile) ;
      try
        //Skip header lines
        for LIndex := 0 to FStartDataLineNo-1 do
        begin
          if EOF(LFile) then Break;
          ReadLn(LFile, LLineData);
        end;

        LPumpingChannelLinesCount := HeaderData.PumpingChannelsCount div 10;
        if((HeaderData.PumpingChannelsCount mod 10) <> 0) then
          LPumpingChannelLinesCount     := LPumpingChannelLinesCount + 1;


        for LSequenceIndex := 1 to HeaderData.SequencesCount do
        begin
          if EOF(LFile) then Break;
          LSequenceNo := HeaderData.SequencesList[LSequenceIndex-1];
          LCurrentDate := EncodeDate(ASettings.StartYear,ASettings.StartMonth,01);
          LCurrentDate := IncMonth(LCurrentDate,-1);
          for LMonthIndex := 1 to HeaderData.MonthsCount do
          begin
            if EOF(LFile) then Break;
            LCurrentDate := IncMonth(LCurrentDate,1);

            //Read Pumping Channel Supplied Flows (m3/s)
            LMonthData.Clear;
            LWholeLineData := '';
            for LIndex := 1 to LPumpingChannelLinesCount do
            begin
              ReadLn(LFile, LLineData);
              if EOF(LFile) then Break;
              LWholeLineData := LWholeLineData + LLineData;
            end;
            LTempData := Copy(LWholeLineData,LPumpingChannelIndex,10);
            if(LTempData = '**********') then LTempData := '       0.0';
            if(LTempData <> '') then
            begin
              LMonthData.Add(LSequenceNo);
              LMonthData.Add(DateToStr(LCurrentDate));
              LMonthData.Add(LTempData);
              ADataContainer.Add(LMonthData.CommaText);
            end;
          end;
        end;
        Result  := True;
      finally
        LMonthData.Free;
        CloseFile(LFile) ;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
