unit UWRPMPltFileManager;
//
//
//  UNIT      : Contains TWRPMPltFileManager
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

  TWRPMPltFileManager = class(TAbstractAppObject)
  protected
    FFileHeaderData    : TWRPMPltFileHeaderData;
    FStartDataLineNo   : integer;
    FDateTime          : string;
    FStreamReader      : TStreamReader;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function ReadHeaderData(AFileName : string): boolean;
    function ReadElementData(ASettings : TWRPMOutputSettings;ADataContainer : TStrings): boolean;
    property HeaderData                : TWRPMPltFileHeaderData     read FFileHeaderData;
    property StreamReader             : TStreamReader     read FStreamReader;
  end;


implementation

uses
  DateUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TWRPMPltFileManager }

procedure TWRPMPltFileManager.CreateMemberObjects;
const OPNAME = 'TWRPMPltFileManager.CreateMemberObjects';
begin
  inherited;
  try
    FStreamReader   := nil;
    FFileHeaderData := TWRPMPltFileHeaderData.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPltFileManager.DestroyMemberObjects;
const OPNAME = 'TWRPMPltFileManager.CreateMemberObjects';
begin
  inherited;
  try
    if(FStreamReader <> nil) then
    begin
      FStreamReader.Close;
      FreeAndNil(FStreamReader);
    end;
    FreeAndNil(FFileHeaderData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPltFileManager.Initialise: boolean;
const OPNAME = 'TWRPMPltFileManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FStartDataLineNo         := NullInteger;
    FDateTime                := '';
    FFileHeaderData.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPltFileManager.ReadHeaderData(AFileName: string): boolean;
const OPNAME = 'TWRPMPltFileManager.ReadHeaderData';
var
  LIndex     : integer;
  LLineNo    : integer;
  LFileAge   : TDateTime;
  LLineData  : string;
  LTempString : String;
  LReservoirLinesCount     : integer;
  LSubSystemLinesCount     : integer;
  LDemandChannelLinesCount : integer;
begin
  Result  := False;
  try
    Initialise;
    if FileExists(AFileName) then
    begin
      FStreamReader := TStreamReader.Create(AFileName);

      LLineNo                        := 0;
      HeaderData.SequencesCount      := 0;
      HeaderData.ReservoirsCount     := 0;
      HeaderData.SubSystemsCount     := 0;
      HeaderData.DemandChannelsCount := 0;
      FStartDataLineNo               := 0;

      //Read (Number Months) and (Number of sequences)
      if not FStreamReader.EndOfStream then
      begin
        LLineData                 := FStreamReader.ReadLine;
        LLineNo                   := LLineNo +1;
        LTempString               := ExtractFirstSubstring(LLineData);
        HeaderData.MonthsCount    := StrToIntDef(LTempString,NullInteger);
        LTempString               := ExtractFirstSubstring(LLineData);
        HeaderData.SequencesCount := StrToIntDef(LTempString,NullInteger);
      end;

      //Read List of sequences
      while (HeaderData.SequencesList.Count < HeaderData.SequencesCount) do
      begin
        if not FStreamReader.EndOfStream then
        begin
          LLineData := FStreamReader.ReadLine;
          LLineNo  := LLineNo +1;
          while(LLineData <> '') do
          begin
            HeaderData.SequencesList.Add(ExtractFirstSubstring(LLineData));
            LLineData := Trim(LLineData);
          end;
        end;
      end;

      //Read (Number of reservoirs printed out) and (Total number of subsystems in all allocation groups) and (Number of Demand channels printed out)
      if not StreamReader.EndOfStream then
      begin
        LLineData := FStreamReader.ReadLine;
        HeaderData.FixedBlock.Add(LLineData);
        LLineNo  := LLineNo +1;
        LTempString   := ExtractFirstSubstring(LLineData);
        HeaderData.ReservoirsCount  := StrToIntDef(LTempString,NullInteger);
        LTempString   := ExtractFirstSubstring(LLineData);
        HeaderData.SubSystemsCount  := StrToIntDef(LTempString,NullInteger);
        LTempString   := ExtractFirstSubstring(LLineData);
        HeaderData.DemandChannelsCount  := StrToIntDef(LTempString,NullInteger);
      end;

      LReservoirLinesCount := HeaderData.ReservoirsCount div 10;
      if((HeaderData.ReservoirsCount mod 10) <> 0) then
        LReservoirLinesCount     := LReservoirLinesCount + 1;

      LSubSystemLinesCount := HeaderData.SubSystemsCount div 10;
      if((HeaderData.SubSystemsCount mod 10) <> 0) then
        LSubSystemLinesCount     := LSubSystemLinesCount + 1;

      {LDemandChannelLinesCount := HeaderData.DemandChannelsCount div 10;
      if((HeaderData.DemandChannelsCount mod 10) <> 0) then
        LDemandChannelLinesCount     := LDemandChannelLinesCount + 1;}

      //Read (Reservoir FSL)
      for LIndex := 1 to LReservoirLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
      end;

      //Read (Reservoir DSL)
      for LIndex := 1 to LReservoirLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
      end;

      //Read (Sub-system storage (Mm3/a))
      for LIndex := 1 to LSubSystemLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
      end;

      //Read (Reservoir volume)
      for LIndex := 1 to LReservoirLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
      end;

      LSubSystemLinesCount := HeaderData.SubSystemsCount div 4;
      if((HeaderData.SubSystemsCount mod 4) <> 0) then
        LSubSystemLinesCount     := LSubSystemLinesCount + 1;

      LReservoirLinesCount := HeaderData.ReservoirsCount div 4;
      if((HeaderData.ReservoirsCount mod 4) <> 0) then
        LReservoirLinesCount     := LReservoirLinesCount + 1;

      LDemandChannelLinesCount := HeaderData.DemandChannelsCount div 4;
      if((HeaderData.DemandChannelsCount mod 4) <> 0) then
        LDemandChannelLinesCount     := LDemandChannelLinesCount + 1;

      //Read (Subsystem names(4A30) trim it)
      for LIndex := 1 to LSubSystemLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
        while(Trim(LLineData) <> '') do
        begin
          LTempString   := ExtractLengthFirstSubstring(30,LLineData);
          HeaderData.SubSystemsList.Add(Trim(LTempString));
        end;
      end;

      //Read (Reservoir names)
      for LIndex := 1 to LReservoirLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
        while(Trim(LLineData) <> '') do
        begin
          LTempString   := ExtractLengthFirstSubstring(30,LLineData);
          HeaderData.ReservoirsList.Add(Trim(LTempString));
        end;
      end;

      //Read (Master control channel names)
      for LIndex := 1 to LDemandChannelLinesCount do
      begin
        if StreamReader.EndOfStream then Break;
        LLineData := StreamReader.ReadLine;
        LLineNo  := LLineNo +1;
        HeaderData.FixedBlock.Add(LLineData);
        while(Trim(LLineData) <> '') do
        begin
          LTempString   := ExtractLengthFirstSubstring(30,LLineData);
          HeaderData.DemandChannelsList.Add(Trim(LTempString));
        end;
      end;

      FStartDataLineNo := LLineNo;
      Result  := True;


      if FileAge(AFileName, LFileAge) then
       FDateTime := DateTimeToStr(LFileAge);

      //FDateTime := DateTimeToStr(FileDateToDateTime(FileAge(AFileName)));
      HeaderData.FileName := AFileName;
    end;
    if(FStreamReader <> nil) then
    begin
      FStreamReader.Close;
      FreeAndNil(FStreamReader);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPltFileManager.ReadElementData(ASettings : TWRPMOutputSettings;ADataContainer: TStrings): boolean;
const OPNAME = 'TWRPMPltFileManager.ReadElementData';
var
  LIndex      : integer;
  LFile       : TextFile;
  LSequenceNo : string;
  LTempData   : string;
  LLineData   : string;
  LElementIndex    : integer;
  LSequenceIndex   : integer;
  LMonthIndex      : integer;
  LDateTime                : string;
  LFileAge,
  LCurrentDate             : TDateTime;
  LReservoirLinesCount     : integer;
  LSubSystemLinesCount     : integer;
  LDemandChannelLinesCount : integer;
  LReservoirIndex          : integer;
  LSubSystemIndex          : integer;
  LDemandChannelIndex      : integer;
  LMonthData               : TStringList;
begin
  Result  := False;
  try
    ADataContainer.Clear;
    if FileExists(ASettings.PlotFileName) then
    begin
      //Read File headers
      if(ASettings.PlotFileName <> HeaderData.FileName) then
      begin
        ReadHeaderData(ASettings.PlotFileName);
      end
      else
      begin
        //LDateTime := DateTimeToStr(FileDateToDateTime(FileAge(ASettings.PlotFileName)));
       if FileAge(ASettings.PlotFileName, LFileAge) then
         FDateTime := DateTimeToStr(LFileAge);

        if(FDateTime <> LDateTime) then
          ReadHeaderData(ASettings.PlotFileName)
      end;

      if(FStartDataLineNo = NullInteger) then
        Exit;

      LReservoirIndex          := NullInteger;
      LSubSystemIndex          := NullInteger;
      LDemandChannelIndex      := NullInteger;

      if(ASettings.ElementType = votMasterControl) then
        LDemandChannelIndex := HeaderData.DemandChannelsList.IndexOf(ASettings.ElementName);
      if(ASettings.ElementType = votReviewDamStorage) then
        LReservoirIndex := HeaderData.ReservoirsList.IndexOf(ASettings.ElementName);
      if(ASettings.ElementType = votReviewSubSystemStorage) then
        LSubSystemIndex := HeaderData.SubSystemsList.IndexOf(ASettings.ElementName);

      if(LReservoirIndex < 0) and
        (LSubSystemIndex < 0) and
        (LDemandChannelIndex < 0) then
        Exit;


      LMonthData := TStringList.Create;
      AssignFile(LFile, ASettings.PlotFileName) ;
      Reset(LFile) ;
      try
        //Skip header lines
        for LIndex := 0 to FStartDataLineNo-1 do
        begin
          if EOF(LFile) then Break;
          ReadLn(LFile, LLineData);
        end;

        LReservoirLinesCount := HeaderData.ReservoirsCount div 10;
        if((HeaderData.ReservoirsCount mod 10) <> 0) then
          LReservoirLinesCount     := LReservoirLinesCount + 1;

        LSubSystemLinesCount := HeaderData.SubSystemsCount div 10;
        if((HeaderData.SubSystemsCount mod 10) <> 0) then
          LSubSystemLinesCount     := LSubSystemLinesCount + 1;

        LDemandChannelLinesCount := HeaderData.DemandChannelsCount div 10;
        if((HeaderData.DemandChannelsCount mod 10) <> 0) then
          LDemandChannelLinesCount     := LDemandChannelLinesCount + 1;


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

            //Read Sub-system storage (Mm3)
            LMonthData.Clear;
            LElementIndex := 0;
            for LIndex := 1 to LSubSystemLinesCount do
            begin
              ReadLn(LFile, LLineData);
              if EOF(LFile) then Break;
              if(ASettings.ElementType = votReviewSubSystemStorage) then
              begin
                while(LLineData <> '') do
                begin
                  LTempData := ExtractLengthFirstSubstring(10,LLineData);
                  if(LElementIndex = LSubSystemIndex) then
                  begin
                    LMonthData.Add(LSequenceNo);
                    LMonthData.Add(DateToStr(LCurrentDate));
                    LMonthData.Add(LTempData);
                    ADataContainer.Add(LMonthData.CommaText);
                  end;
                  LElementIndex := LElementIndex + 1;
                end;
              end;
            end;

            //Read Reservoir storage (Mm3)
            LMonthData.Clear;
            LElementIndex := 0;
            for LIndex := 1 to LReservoirLinesCount do
            begin
              ReadLn(LFile, LLineData);
              if EOF(LFile) then Break;
              if(ASettings.ElementType = votReviewDamStorage) then
              begin
                while(LLineData <> '') do
                begin
                  LTempData := ExtractLengthFirstSubstring(10,LLineData);
                  if(LElementIndex = LReservoirIndex) then
                  begin
                    LMonthData.Add(LSequenceNo);
                    LMonthData.Add(DateToStr(LCurrentDate));
                    LMonthData.Add(LTempData);
                    ADataContainer.Add(LMonthData.CommaText);
                  end;
                  LElementIndex := LElementIndex + 1;
                end;
              end;
            end;

            //Read Master Control Channel Demanded Flows (m3/s)
            LMonthData.Clear;
            LElementIndex := 0;
            for LIndex := 1 to LDemandChannelLinesCount do
            begin
              ReadLn(LFile, LLineData);
              if EOF(LFile) then Break;
              if(ASettings.ElementType = votMasterControl) and (ASettings.DataType = ovtDemand) then
              begin
                while(LLineData <> '') do
                begin
                  LTempData := ExtractLengthFirstSubstring(10,LLineData);
                  if(LElementIndex = LDemandChannelIndex) then
                  begin
                    LMonthData.Add(LSequenceNo);
                    LMonthData.Add(DateToStr(LCurrentDate));
                    LMonthData.Add(LTempData);
                    ADataContainer.Add(LMonthData.CommaText);
                  end;
                  LElementIndex := LElementIndex + 1;
                end;
              end;
            end;

            //Read Master Control Channel Allocated Flows (m3/s)
            LMonthData.Clear;
            LElementIndex := 0;
            for LIndex := 1 to LDemandChannelLinesCount do
            begin
              ReadLn(LFile, LLineData);
              if EOF(LFile) then Break;
              if(ASettings.ElementType = votMasterControl) and (ASettings.DataType = ovtAllocated) then
              begin
                while(LLineData <> '') do
                begin
                  LTempData := ExtractLengthFirstSubstring(10,LLineData);
                  if(LElementIndex = LDemandChannelIndex) then
                  begin
                    LMonthData.Add(LSequenceNo);
                    LMonthData.Add(DateToStr(LCurrentDate));
                    LMonthData.Add(LTempData);
                    ADataContainer.Add(LMonthData.CommaText);
                  end;
                  LElementIndex := LElementIndex + 1;
                end;
              end;
            end;

            //Read Master Control Channel Supplied Flows (m3/s)
            LMonthData.Clear;
            LElementIndex := 0;
            for LIndex := 1 to LDemandChannelLinesCount do
            begin
              ReadLn(LFile, LLineData);
              if EOF(LFile) then Break;
              if(ASettings.ElementType = votMasterControl) and (ASettings.DataType = ovtSupply) then
              begin
                while(LLineData <> '') do
                begin
                  LTempData := ExtractLengthFirstSubstring(10,LLineData);
                  if(LElementIndex = LDemandChannelIndex) then
                  begin
                    LMonthData.Add(LSequenceNo);
                    LMonthData.Add(DateToStr(LCurrentDate));
                    LMonthData.Add(LTempData);
                    ADataContainer.Add(LMonthData.CommaText);
                  end;
                  LElementIndex := LElementIndex + 1;
                end;
              end;
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
