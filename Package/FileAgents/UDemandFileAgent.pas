//
//
//  UNIT      : Contains TDemandFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 17/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDemandFileAgent;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UHydrologyFilesObject,
  UYieldModelDataObject;


type
  TDemandFileAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
    function ReadDataCommaText(AFileName:string;  ADataContainer: TStrings): boolean;
   end;

implementation



uses UUtilities,
     UErrorHandlingOperations;


function TDemandFileAgent.GetFileStatistics(AFileName: TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
const OPNAME = 'TDemandFileAgent.GetFileStatistics';
Var
  LFileData: TStringList;
  LReadString,
  LTempString,
  LPatchSource : String;
  LReadInteger,
  LCurrentline,
  LErrorCode : Integer;
begin
  Result := False;
  try
    AStarYear := 0;
    AEndYear  := 0;
    ARowCount := 0;

    if (AFilename.FileName = '') then Exit;
    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then Exit;
    If not FileExists(AFilename.FileName) then Exit;

    LFileData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFilename.FileName);
      for LCurrentline := 0 to LFileData.Count - 1 do
      begin
        //Remove the last line if it contains one character.
        if(Length(Trim(LFileData[LCurrentline])) < 2) then
          Continue;

        LReadString  := LFileData[LCurrentline];
        LTempString  := GetSubstring(LReadString,1,8);
        LPatchSource := GetSubstring(LReadString,9,1)+' ';
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode = 0) then
        begin
          if(AStarYear = 0) then
            AStarYear := LReadInteger;
          AEndYear    := LReadInteger;
          ARowCount   := ARowCount + 1;
        end;
      end;
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandFileAgent.ReadDataCommaText(AFileName: string; ADataContainer: TStrings): boolean;
const OPNAME = 'TDemandFileAgent.ReadDataCommaText';
Var
  LFileData: TStringList;
  LLineData : string;
  LCurrentline : integer;
  LIndex : integer;
  LChar  : Char;
begin
  Result := False;
  try
    ADataContainer.Clear;
    //Check if file exists.
    If not FileExists(AFileName) then
      Exit;

    LFileData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFileName);
      for LCurrentline := 0 to LFileData.Count - 1 do
      begin
        LLineData  := Trim(LFileData[LCurrentline]);
        //Remove the last line if it contains one character.
        if(Length(LLineData) < 2) then
          Continue;

        for LIndex := 1 to Length(LLineData) do
        begin
          LChar := LLineData[LIndex];
          if not CharInSet(LChar,['0','1','2','3','4','5','6','7','8','9','.']) then
            LLineData[LIndex] := ' ';
        end;
        while(Pos('  ',LLineData) > 0) do
          LLineData := StringReplace(LLineData,'  ',' ',[rfReplaceAll]);
        LLineData := StringReplace(LLineData,' ',',',[rfReplaceAll]);
        ADataContainer.Add(LLineData);
      end;
      Result := ADataContainer.Count > 0;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandFileAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDemandFileAgent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString,
  LPatchSource : String;
  LReadInteger,
  LCount,
  LStartPos,
  LCurrentline,
  LErrorCode : Integer;
  LTotalValue : Double;
  LReadReal : Double;
  LDemandFileObject: THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDemandFileAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TDemandFileAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    ADataObject.FDemandFilesObject.AddFile(AFilename.FileNumber);
    LDemandFileObject := THydrologyFileObject(ADataObject.FDemandFilesObject.FFiles[AFilename.FileNumber]);
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LDemandFileObject.Initialise then
    Exit;

    LFileData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFilename.FileName);

      LDemandFileObject.FType.FData := GetHydrologyFileType(FAppModules,AFilename.FileName);
      LDemandFileObject.FType.FInitalised := True;

      LDemandFileObject.FFileNumber.FData := AFilename.FileNumber;
      LDemandFileObject.FFileNumber.FInitalised := True;

      for LCurrentline := 0 to LFileData.Count - 1 do
      begin

        if((LCurrentline mod 1000) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        //Remove the last line if it contains one character.
        if(Length(Trim(LFileData[LCurrentline])) < 2) then
          Continue;

        if not LDemandFileObject.AddDetailsLines then
         Exit;

        THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FFileName.FData :=AFilename.FileName;
        LReadString  := LFileData[LCurrentline];
        LTempString  := GetSubstring(LReadString,1,8);
        LPatchSource := GetSubstring(LReadString,9,1)+' ';
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TDemandFileAgent.strYearErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FData :=LReadInteger;
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FInitalised := True;
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FYearValuePatch.FData :=LPatchSource[1];
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FYearValuePatch.FInitalised := True;
        end;

        LStartPos:= 10;
        LTotalValue := 0.0;
        for LCount := MinMonths to MaxMonths do
        begin
          LTempString  := GetSubstring(LReadString,LStartPos,7);
          LPatchSource := GetSubstring(LReadString,LStartPos+7,1)+' ';
          LTempString  := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TDemandFileAgent.strMonthvalueErr');
            LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FData := LReadReal;
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FInitalised:= True;
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FMonthValuesPatch[LCount].FData := LPatchSource[1];
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FMonthValuesPatch[LCount].FInitalised:= True;
            LTotalValue := LTotalValue + LReadReal;
          end;
          Inc(LStartPos,8);
        end;

        {LTempString:=GetSubstring(LReadString,106,9);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          //LMessage := FAppModules.Language.GetString('TDemandFileAgent.strAverageErr');
          //LMessage := Format(LMessage,[LCurrentLine+1,106,114]);
          //AProgressFunction(LMessage,ptError,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin}
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FTotalValue.FData :=LTotalValue;
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LCurrentLine]).FTotalValue.FInitalised := True;
        {end;}
      end;

      LMessage := FAppModules.Language.GetString('TDemandFileAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandFileAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
                              AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDemandFileAgent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LCount,
  LLinesCount  : Integer;
  LDemandFile :TStringlist;
  LDemandFileObject: THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDemandFileAgent.strWritingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end;

    //ADataObject.FDemandFilesObject.AddFile(AFilename.FileNumber);
    LDemandFileObject := THydrologyFileObject(ADataObject.FDemandFilesObject.FFiles[AFilename.FileNumber]);

    LDemandFile:= TStringList.Create;
    try
      for LLinesCount := 0  to  LDemandFileObject.FProjectsDetails.Count - 1  do
      begin
        if((LLinesCount mod 1000) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LOutString:='';
        LTempString:=PadInt(THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValue)+
                            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FData;
        LOutString:=LOutString+LTempString;

        for LCount := MinMonths to MaxMonths do
        begin
          LTempString := PadDouble(THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount]);
          LTempString := LTempString + THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FData;
          LOutString:=LOutString + LTempString;
        end;
        LTempString:=PadDouble(THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FTotalValue);
        LOutString:=LOutString+LTempString;
        LDemandFile.Add(LOutString);
      end;

      if(LDemandFile.Count > 0) then
      begin
        LDemandFile.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);
      end;
      
      LMessage := FAppModules.Language.GetString('TDemandFileAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LDemandFile.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


