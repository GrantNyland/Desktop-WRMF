//
//
//  UNIT      : Contains TRunParametersObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 20/03/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyFileAgent;

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
  THydrologyFileAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
  end;

implementation



uses UUtilities,
     UErrorHandlingOperations;


function THydrologyFileAgent.GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
const OPNAME = 'THydrologyFileAgent.GetFileStatistics';
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

function THydrologyFileAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
                                               AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'THydrologyFileAgent.ReadModelDataFromFile';
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
  LHydrologyFileObject: THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    ADataObject.FHydrologyFilesObject.AddFile(AFilename.FileNumber);
    LHydrologyFileObject := THydrologyFileObject(ADataObject.FHydrologyFilesObject.FFiles[AFilename.FileNumber]);
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LHydrologyFileObject.Initialise then
    Exit;

    LFileData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFilename.FileName);

      LHydrologyFileObject.FType.FData := GetHydrologyFileType(FAppModules,AFilename.FileName);
      LHydrologyFileObject.FType.FInitalised := True;

      LHydrologyFileObject.FFileNumber.FData := AFilename.FileNumber;
      LHydrologyFileObject.FFileNumber.FInitalised := True;

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

        if not LHydrologyFileObject.AddDetailsLines then
         Exit;

        THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FFileName.FData :=AFilename.FileName;
        LReadString  := LFileData[LCurrentline];

        //SFR monthly unit runoff time-series data
        if(LHydrologyFileObject.FType.FData = 200) then
        begin
          LTempString  := ExtractFirstSubstring(LReadString);
          LStartPos    := Length(LTempString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0)  then
          begin
            LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strYearErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,LStartPos]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FData :=LReadInteger;
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FInitalised := True;
          end;

          LTotalValue := 0.0;
          for LCount := MinMonths to MaxMonths do
          begin
            LTempString  := ExtractFirstSubstring(LReadString);
            LStartPos    := LStartPos + Length(LTempString);
            LTempString  := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strMonthvalueErr');
              LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FData := LReadReal;
              THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FInitalised:= True;
              LTotalValue := LTotalValue + LReadReal;
            end;
          end
        end
        else
        begin
          LTempString  := GetSubstring(LReadString,1,8);
          LPatchSource := GetSubstring(LReadString,9,1)+' ';
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0)  then
          begin
            if(LHydrologyFileObject.FType.FData <> 114) then
            begin
              LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strYearErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,8]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FData :=LReadInteger;
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FInitalised := True;
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValuePatch.FData :=LPatchSource[1];
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValuePatch.FInitalised := True;
          end;

          LTotalValue := 0.0;
          if(LHydrologyFileObject.FType.FData = 114) and
            (not THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FYearValue.FInitalised) then
          begin
            LStartPos:= 9;
            for LCount := MinMonths to MaxMonths do
            begin
              LTempString  := GetSubstring(LReadString,LStartPos,8);
              LTempString  := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strMonthvalueErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+8]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FData := LReadReal;
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FInitalised:= True;
                LTotalValue := LTotalValue + LReadReal;
              end;
              Inc(LStartPos,8);
            end;
          end
          else
          begin
            LStartPos:= 10;
            for LCount := MinMonths to MaxMonths do
            begin
              LTempString  := GetSubstring(LReadString,LStartPos,7);
              LPatchSource := GetSubstring(LReadString,LStartPos+7,1)+' ';
              LTempString  := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strMonthvalueErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FData := LReadReal;
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValues[LCount].FInitalised:= True;
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValuesPatch[LCount].FData := LPatchSource[1];
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FMonthValuesPatch[LCount].FInitalised:= True;
                LTotalValue := LTotalValue + LReadReal;
              end;
              Inc(LStartPos,8);
            end;
            if(FAppModules.Model.ModelName = CPlanning) then
            begin
              LTempString  := Copy(LReadString,LStartPos,Length(LReadString));
              if(Trim(LTempString) <> '') then
              begin
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FComment.FData := LTempString;
                THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FComment.FInitalised := True;
              end;
            end;
          end;
        end;

        {LTempString:=GetSubstring(LReadString,106,9);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          //LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strAverageErr');
          //LMessage := Format(LMessage,[LCurrentLine+1,106,114]);
          //AProgressFunction(LMessage,ptError,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin}
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FTotalValue.FData :=LTotalValue;
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LCurrentLine]).FTotalValue.FInitalised := True;
        {end; }
      end;

      LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
                              AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'THydrologyFileAgent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LCount,
  LLinesCount  : Integer;
  LHydrologyFile :TStringlist;
  LHydrologyFileObject: THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strWritingStarted');
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

    //ADataObject.FHydrologyFilesObject.AddFile(AFilename.FileNumber);
    LHydrologyFileObject := THydrologyFileObject(ADataObject.FHydrologyFilesObject.FFiles[AFilename.FileNumber]);

    LHydrologyFile:= TStringList.Create;
    try
      for LLinesCount := 0  to  LHydrologyFileObject.FProjectsDetails.Count - 1  do
      begin
        if((LLinesCount mod 1000) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;
        LOutString:='';
        if(LHydrologyFileObject.FType.FData = 114) and
          (not THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValue.FInitalised) then
        begin
          LOutString:='        ';
          for LCount := MinMonths to MaxMonths do
          begin
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FLength := 8;
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FDecimal := 3;
            LTempString := PadDouble(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount]);
            LOutString  := LOutString + LTempString;
          end;
        end
        else
        begin
          LTempString:=PadInt(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValue)+
                              THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FData;
          LOutString:=LOutString+LTempString;

          for LCount := MinMonths to MaxMonths do
          begin
            LTempString := PadDouble(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount]);
            LTempString := LTempString + THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FData;
            LOutString  := LOutString + LTempString;
          end;
        end;

        if(FAppModules.Model.ModelName = CPlanning) then
        begin
          if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FComment.FInitalised then
          begin
            LTempString:=THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FComment.FData;
            LOutString:=LOutString+LTempString;
          end;
        end
        else
        begin
          if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FInitalised then
          begin
            LTempString:=PadDouble(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FTotalValue);
            LOutString:=LOutString+LTempString;
          end;
        end;
        LHydrologyFile.Add(LOutString);
      end;

      if(LHydrologyFile.Count > 0) then
      begin
        LHydrologyFile.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);
      end;

      LMessage := FAppModules.Language.GetString('THydrologyFileAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LHydrologyFile.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


