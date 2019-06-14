//
//
//  UNIT      : Contains TFile06Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile06Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UReservoirInitialLevelsObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile06Agent = class(TAbstractFileAgent)
  public
    { Public declarations }
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  System.Contnrs,
  UUtilities,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

function TFile06Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile06Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LLeftString,
  LTempString : String;
  LReadInteger,
  LCount,
  LLinesCount,
  LLinesRead,
  LErrorCode : Integer;
  LReadReal : Double;
  LReservoirInitialLevelValuesObject: TReservoirInitialLevelValuesObject;
  LReservoirInitialLevelValues:TReservoirInitialLevelValues;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile06Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile06Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LReservoirInitialLevelValuesObject := ADataObject.FReservoirInitialLevelValuesObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LReservoirInitialLevelValuesObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F06 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      for LLinesCount := 0 to LFileData.Count - 1 do
      begin
        LReadString := LFileData[LLinesCount];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        if(LReadString = '') then Break;
        LTempString := Trim(ExtractFirstSubstring(LReadString));
        if(LReadString = '') then Break;
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then Break;
        if(LReadInteger = 0) then Break;


        LLinesRead := LLinesRead + 1;
        LReservoirInitialLevelValues := TReservoirInitialLevelValues.Create;
        LReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines.Add(LReservoirInitialLevelValues);

        LReservoirInitialLevelValues.FReservoirNodeNumber.FData := LReadInteger;
        LReservoirInitialLevelValues.FReservoirNodeNumber.FInitalised := True;

        LLeftString := LReadString;
        for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          if(Trim(LTempString) = '') then
            Break;
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            Break;
            {LMessage := FAppModules.Language.GetString('TFile06Agent.strInitialLevelErr');
            LMessage := Format(LMessage,[LLinesCount+1,5,12]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;}
          end
          else
          begin
            LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FData := LReadReal;
            LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FInitalised := True;
          end;
          LLeftString := LReadString;
        end;

        if(Trim(LLeftString) <> '') then
        begin
          LReservoirInitialLevelValues.FComment.FData :=LLeftString;
          LReservoirInitialLevelValues.FComment.FInitalised := True;
        end;
      end;

      LLinesRead := LLinesRead + 1;
      for LLinesCount := (LLinesRead) to LFileData.Count - 1 do
        LReservoirInitialLevelValuesObject.FF06ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile06Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile06Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile06Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LCount,
  LLinesCount: Integer;
  LF06File :TStringlist;
  LReservoirInitialLevelValuesObject: TReservoirInitialLevelValuesObject;
  LReservoirInitialLevelValues:TReservoirInitialLevelValues;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile06Agent.strWritingStarted');
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

    LF06File:= TStringList.Create;
    LReservoirInitialLevelValuesObject := ADataObject.FReservoirInitialLevelValuesObject;
    if not Assigned(LReservoirInitialLevelValuesObject) then
      Exit;

    try
      for LLinesCount := 0 to LReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines.Count -1 do
      begin
        LReservoirInitialLevelValues := TReservoirInitialLevelValues(LReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines[LLinesCount]);

        LOutString:='';
        LTempString:=PadInt(LReservoirInitialLevelValues.FReservoirNodeNumber);
        LOutString:=LOutString+LTempString;

        for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
        begin
          if not LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FInitalised then
            Break;
          LTempString := PadDouble(LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount]);
          LOutString:=LOutString+LTempString;
        end;

        if LReservoirInitialLevelValues.FComment.FInitalised then
        begin
          LOutString:=LOutString+LReservoirInitialLevelValues.FComment.FData;
        end;

        LF06File.Add(LOutString);
      end;

      if(LF06File.Count > 0) {and (FAppModules.Model.ModelName <> CPlanning)} then
        LF06File.Add('   0    0.00');

      for LLinesCount := 0 to LReservoirInitialLevelValuesObject.FF06ExtraLines.Count -1 do
      begin
        LF06File.Add(LReservoirInitialLevelValuesObject.FF06ExtraLines[LLinesCount]);
      end;

      LF06File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile06Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF06File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

