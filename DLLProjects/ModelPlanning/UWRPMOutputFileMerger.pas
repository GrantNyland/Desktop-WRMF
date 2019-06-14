unit UWRPMOutputFileMerger;

interface
uses
  classes,
  sysutils,
  contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractDataObject,
  UAbstractFileNamesObject,
  UWRPMPltFileManager,
  UWRPMResFileManager,
  UWRPMSysFileManager,
  UWRPMPmpFileManager,
  UWRPMOutputSettings,
  UConstants;

type
  TWRPMOutputFileMerger = class(TAbstractAppObject)
  protected
    FSequencesCount          : integer;
    FFirstSequence           : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure MergePltHeaderData(AHeaderData:TStrings; AManagerList: TObjectList);

    function MergeGeneralFiles(AFileNameList: TStrings;AOutputFileName : string; AProgressUpdate : TProgressUpdateFuntion): boolean;
    function MergePltFiles(APltFileNameList: TStrings;AOutputFileName : string; AProgressUpdate : TProgressUpdateFuntion): boolean;
    function MergeSysFiles(ASysFileNameList: TStrings;AOutputFileName : string; AProgressUpdate : TProgressUpdateFuntion): boolean;
    function MergeResFiles(AResFileNameList: TStrings;AOutputFileName : string; AProgressUpdate : TProgressUpdateFuntion): boolean;
    function MergePMPFiles(APmpFileNameList: TStrings;AOutputFileName : string; AProgressUpdate : TProgressUpdateFuntion): boolean;
  public
    function Initialise: boolean; override;
    function MergeAllOutputFiles(APltFileNameList, ASysFileNameList, AResFileNameList,APmpFileNameList : TStrings; AProgressUpdate : TProgressUpdateFuntion) : boolean;
    property SequencesCount          : integer     read FSequencesCount      write FSequencesCount ;
    property FirstSequenceNumber     : integer     read FFirstSequence       write FFirstSequence ;
  end;

implementation
uses
  Math,
  UPlanningModelDataObject,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  URunConfigurationData,
  UOutputData,
  UUtilities,
  DateUtils;

{ TWRPMOutputFileMerger }


procedure TWRPMOutputFileMerger.CreateMemberObjects;
const OPNAME = 'TWRPMOutputFileMerger.CreateMemberObjects';
begin
  try
    //FWRPMOutputSettings := TWRPMOutputSettings.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMOutputFileMerger.DestroyMemberObjects;
const OPNAME = 'TWRPMOutputFileMerger.DestroyMemberObjects';
begin
  try
    //FreeAndNil(FWRPMOutputSettings);
 except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputFileMerger.Initialise;
const OPNAME = 'TWRPMOutputFileMerger.Initialise';
begin
  Result := False;
  try
    FSequencesCount     := 0;
    FFirstSequence      := 0;
    //FWRPMOutputSettings.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputFileMerger.MergeAllOutputFiles(APltFileNameList, ASysFileNameList, AResFileNameList,APmpFileNameList : TStrings; AProgressUpdate: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWRPMOutputFileMerger.MergeAllOutputFiles';
var
  LFilePath,
  LFileNamePrefix,
  LPltFileName,
  LSysFileName,
  LResFileName,
  LPmpFileName : string;
begin
  Result := True;
  try
    LFilePath := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.OutputFilesPath;
    LFilePath       := IncludeTrailingPathDelimiter(LFilePath);
    LFileNamePrefix :=  TYieldModelDataObject(FAppModules.Model.ModelData).CastDataFilePaths.DataFilePrefix;

    LPltFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.PltOut');
    LSysFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.SysOut');
    LResFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.ResOut');
    LPmpFileName := LFilePath + LFileNamePrefix + FAppModules.Language.GetString('TWRPMOutputFilesCollator.PmpOut');

    if not DirectoryExists(LFilePath) then
      ForceDirectories(LFilePath);

    if(APltFileNameList.Count > 0) then
       Result := Result and MergePltFiles(APltFileNameList,LPltFileName,AProgressUpdate);

    if(AResFileNameList.Count > 0) then
       Result := Result and MergeSysFiles(ASysFileNameList,LSysFileName,AProgressUpdate);

    if(ASysFileNameList.Count > 0) then
       Result := Result and MergeResFiles(AResFileNameList,LResFileName,AProgressUpdate);

    if(APmpFileNameList.Count > 0) then
       Result := Result and MergePMPFiles(APmpFileNameList,LPmpFileName,AProgressUpdate);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMOutputFileMerger.MergePltHeaderData(AHeaderData: TStrings; AManagerList: TObjectList);
const OPNAME = 'TWRPMOutputFileMerger.MergePltFiles';
var
  LPltFileManager     : TWRPMPltFileManager;
  LManagerIndex       : integer;
  LItemIndex          : integer;
  LSequenceIndex      : integer;
  LLineData           : string;
  LTotalMonths,
  LTotalSequences     : integer;
begin
  try
    LTotalMonths        := 0;
    LTotalSequences     := 0;
    FSequencesCount     := 0;
    FFirstSequence      := 0;
    AHeaderData.Clear;

    if(AManagerList.Count < 2) then Exit;
    LPltFileManager   := TWRPMPltFileManager(AManagerList.Items[0]);
    FFirstSequence    := StrToInt(LPltFileManager.HeaderData.SequencesList[0]);


    //Calculate total sequences
    for LManagerIndex := 0 to AManagerList.Count-1 do
    begin
      LPltFileManager := TWRPMPltFileManager(AManagerList.Items[LManagerIndex]);
      LTotalMonths    := LPltFileManager.HeaderData.MonthsCount;
      LTotalSequences := LTotalSequences + LPltFileManager.HeaderData.SequencesCount;
    end;
    LLineData := Format('%5d',[LTotalMonths]) + Format('%5d',[LTotalSequences]);
    AHeaderData.Add(LLineData);
    FSequencesCount     := LTotalSequences;


    LLineData      := '';
    LSequenceIndex := 0;
    for LManagerIndex := 0 to AManagerList.Count-1 do
    begin
      LPltFileManager := TWRPMPltFileManager(AManagerList.Items[LManagerIndex]);
      for LItemIndex := 0 to LPltFileManager.HeaderData.SequencesList.Count-1 do
      begin
        LLineData := LLineData + Format('%5d',[StrToInt(LPltFileManager.HeaderData.SequencesList[LItemIndex])]);
        LSequenceIndex := LSequenceIndex + 1;
        if(LSequenceIndex = 20) then
        begin
          AHeaderData.Add(LLineData);
          LLineData      := '';
          LSequenceIndex := 0;
        end;
      end;

      if(LManagerIndex = (AManagerList.Count-1)) then
      begin
        for LItemIndex := 0 to LPltFileManager.HeaderData.FixedBlock.Count-1 do
        begin
          AHeaderData.Add(LPltFileManager.HeaderData.FixedBlock[LItemIndex]);
        end;
      end;
    end;

    if(LLineData <> '') then
    begin
      AHeaderData.Add(LLineData);
      LLineData      := '';
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputFileMerger.MergePltFiles(APltFileNameList: TStrings; AOutputFileName: string; AProgressUpdate: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWRPMOutputFileMerger.MergePltFiles';
var
  LPltFileManager     : TWRPMPltFileManager;
  LOutputFile         : TStreamWriter;
  LManagerList        : TObjectList;
  LHeaderData         : TStringList;
  LIndex              : integer;
  LMessage,
  LLineData           : string;
  LStop               : boolean;
begin
  Result := False;
  try
    LManagerList        := TObjectList.Create(True);
    LHeaderData         := TStringList.Create;
    try
      for LIndex := 0 to APltFileNameList.Count-1 do
      begin
        LPltFileManager := TWRPMPltFileManager.Create(FAppModules);
        LPltFileManager.Initialise;
        if LPltFileManager.ReadHeaderData(APltFileNameList[LIndex]) then
          LManagerList.Add(LPltFileManager);
      end;

      if(LManagerList.Count > 0) then
      begin
        MergePltHeaderData(LHeaderData,LManagerList);
        LOutputFile := TStreamWriter.Create(AOutputFileName);
        try
          for LIndex := 0 to LHeaderData.Count-1 do
          begin
            LOutputFile.WriteLine(LHeaderData[LIndex]);
          end;

          for LIndex := 0 to LManagerList.Count-1 do
          begin
            LPltFileManager := TWRPMPltFileManager(LManagerList.Items[LIndex]);
            LMessage  := FAppModules.Language.GetString('TOutpuCollateFilesValidator.CollatingFile');
            LMessage  := Format(LMessage,[LPltFileManager.HeaderData.FileName]);
            AProgressUpdate(LMessage,ptNone,LStop);

            while not LPltFileManager.StreamReader.EndOfStream do
            begin
              LLineData  := LPltFileManager.StreamReader.ReadLine;
              LOutputFile.WriteLine(LLineData);
            end;
            LPltFileManager.StreamReader.Close;
          end;
        finally
          LOutputFile.Close;
          LOutputFile.Free;
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LHeaderData);
      FreeAndNil(LManagerList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TWRPMOutputFileMerger.MergeSysFiles(ASysFileNameList: TStrings; AOutputFileName: string; AProgressUpdate: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWRPMOutputFileMerger.MergeSysFiles';
begin
  Result := False;
  try
    Result := MergeGeneralFiles(ASysFileNameList,AOutputFileName,AProgressUpdate);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputFileMerger.MergeResFiles(AResFileNameList: TStrings; AOutputFileName: string; AProgressUpdate: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWRPMOutputFileMerger.MergeResFiles';
begin
  Result := False;
  try
    Result := MergeGeneralFiles(AResFileNameList,AOutputFileName,AProgressUpdate);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputFileMerger.MergePMPFiles(APmpFileNameList: TStrings; AOutputFileName: string; AProgressUpdate: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWRPMOutputFileMerger.MergePMPFiles';
var
  LOutputFile         : TStreamWriter;
  LInputFile          : TStreamReader;
  LSequenceData       : TStringList;
  LPos,
  LFileIndex,
  LSequenceIndex,
  LIndex              : integer;
  LTempString,
  LLineData,
  LFileName,
  LMessage            : string;
  LStop               : boolean;
  LTotalMonths,
  LSequenceLinesCount,
  LCurrentSequenceCount     : integer;
  LTotalSequenceCount     : integer;
begin
  Result := False;
  try
    LOutputFile   := TStreamWriter.Create(AOutputFileName);
    LSequenceData := TStringList.Create;
    try
      LTotalMonths            := 0;
      LTotalSequenceCount     := 0;
      for LFileIndex := 0 to APmpFileNameList.Count-1 do
      begin
        LFileName := APmpFileNameList[LFileIndex];
        if FileExists(LFileName) then
        begin
          LMessage  := FAppModules.Language.GetString('TOutpuCollateFilesValidator.CollatingFile');
          LMessage  := Format(LMessage,[LFileName]);
          AProgressUpdate(LMessage,ptNone,LStop);

          LInputFile := TStreamReader.Create(LFileName);
          try
            LCurrentSequenceCount := 0;
            if not LInputFile.EndOfStream then
            begin
              LLineData       := LInputFile.ReadLine;
              LTempString     := ExtractFirstSubstring(LLineData);
              LTotalMonths    := StrToIntDef(LTempString,0);
              LTempString     := ExtractFirstSubstring(LLineData);
              LCurrentSequenceCount  := StrToIntDef(LTempString,0);
            end;
            LTotalSequenceCount := LTotalSequenceCount + LCurrentSequenceCount;

            LSequenceLinesCount := LCurrentSequenceCount div 20;
            if((LSequenceLinesCount mod 20) <> 0) then
              LSequenceLinesCount     := LSequenceLinesCount + 1;

            for LIndex := 1 to LSequenceLinesCount do
            begin
              if LInputFile.EndOfStream then
                Break;
              LLineData := LInputFile.ReadLine;
              LPos      := 1;
              while(LPos < Length(LLineData)) do
              begin
                LTempString := Copy(LLineData,LPos,5);
                LSequenceData.Add(LTempString);
                LPos      := LPos + 5;
              end;
            end;
          finally
            LInputFile.Close;
            LInputFile.Free;
          end;
        end;
      end;

      LLineData := Format('%5d',[LTotalMonths]) + Format('%5d',[LTotalSequenceCount]);
      LOutputFile.WriteLine(LLineData);

      LLineData := '';
      LSequenceIndex := 0;
      for LIndex := 0 to LSequenceData.Count-1 do
      begin
        LLineData := LLineData + LSequenceData[LIndex];
        LSequenceIndex := LSequenceIndex + 1;
        if(LSequenceIndex = 20) then
        begin
          LOutputFile.WriteLine(LLineData);
          LLineData      := '';
          LSequenceIndex := 0;
        end;
      end;

      if(LLineData <> '') then
      begin
        LOutputFile.WriteLine(LLineData);
        LLineData      := '';
      end;
      LOutputFile.WriteLine('    0');
      LOutputFile.WriteLine;

      Result := True;
    finally
      LOutputFile.Close;
      LOutputFile.Free;
      LSequenceData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMOutputFileMerger.MergeGeneralFiles(AFileNameList: TStrings; AOutputFileName: string; AProgressUpdate: TProgressUpdateFuntion): boolean;
const OPNAME = 'TWRPMOutputFileMerger.MergeGeneralFiles';
var
  LOutputFile         : TStreamWriter;
  LInputFile          : TStreamReader;
  LIndex              : integer;
  LFileName,
  LMessage            : string;
  LStop               : boolean;
begin
  Result := False;
  try
    LOutputFile := TStreamWriter.Create(AOutputFileName);
    try
      for LIndex := 0 to AFileNameList.Count-1 do
      begin
        LFileName := AFileNameList[LIndex];
        if FileExists(LFileName) then
        begin
          LMessage  := FAppModules.Language.GetString('TOutpuCollateFilesValidator.CollatingFile');
          LMessage  := Format(LMessage,[LFileName]);
          AProgressUpdate(LMessage,ptNone,LStop);

          LInputFile := TStreamReader.Create(LFileName);
          try
            while not LInputFile.EndOfStream do
              LOutputFile.WriteLine(LInputFile.ReadLine);
          finally
            LInputFile.Close;
            LInputFile.Free;
          end;
        end;
      end;
      Result := True;
    finally
      LOutputFile.Close;
      LOutputFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
