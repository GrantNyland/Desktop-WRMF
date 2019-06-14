//
//
//  UNIT      : Contains TFile08Agent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 03/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile08Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UMinimumPowerDemandsObject,
  UYieldModelDataObject;

type

  TFile08Agent = class(TAbstractFileAgent)
  public
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

function TFile08Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile08Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LReadInteger,
  LCount,
  LLinesCount,
  LLinesRead,
  LStartPos,
  LErrorCode : Integer;
  LReadReal : Double;
  LMinimumPowerDetails :TPowerGenerationObject;
  LMinimumPowerDemands: TMinimumPowerDemandsObject;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile08Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile08Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LMinimumPowerDemands := ADataObject.FMinimumPowerDemandsObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LMinimumPowerDemands.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F08 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      while (LLinesRead < LFileData.Count) do
      begin
        if(LFileData.Count = 0) or
          (Trim(LFileData[LLinesRead]) = '') then
          Break;

        //Line 1
        LReadString := LFileData[LLinesRead];

        //Check if this is the zero line
        LTempString:=GetSubstring(LReadString,37,6);
        LTempString := Trim(LTempString);
        if(LTempString = '') or (StrToInt(LTempString) = 0) then
          Break;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        LMinimumPowerDetails :=TPowerGenerationObject.Create;
        LMinimumPowerDemands.FMinimumPowerDetailsList.Add(LMinimumPowerDetails);

        LTempString := Copy(LReadString,1,36);
        if(Trim(LTempString) <> '') then
        begin
          LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FData := LTempString;
          LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FInitalised := True;
        end;

        LTempString := Copy(LReadString,37,6);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPowerChannelNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if ((LReadInteger < 0) OR (LReadInteger > 1000)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPowerChannelNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMinimumPowerDetails.FMinimumPowerData.FPowerChannelNumber.FData := LReadInteger;
          LMinimumPowerDetails.FMinimumPowerData.FPowerChannelNumber.FInitalised := True;
          if not LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FInitalised then
          begin
            LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FData := 'Channel '+ IntToStr(LReadInteger);
            LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FInitalised := True;
          end;
        end;
        LLinesRead := LLinesRead + 1;

        //Line 1a +++++++++++++++++++++++++++++
        LStartPos:=-5;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');
        for LCount := MinPowerDemand to MaxPowerDemand do
        begin
          Inc(LStartPos,6);
          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile08Agent.strMinMonthlyEnergyGenerationErr');
            LMessage := Format(LMessage,[LLinesRead,LStartPos,LStartPos+5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount].FData := LReadReal;
            LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount].FInitalised := True;
          end;
        end;
        LLinesRead := LLinesRead + 1;

        //Line 1b +++++++++++++++++++++++++++++
        LStartPos:=-5;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1b');
        for LCount := MinPowerDemand to MaxPowerDemand do
        begin
          Inc(LStartPos,6);
          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile08Agent.strMinMonthlyPowerReleaseErr');
            LMessage := Format(LMessage,[LLinesRead,LStartPos,LStartPos+5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount].FData := LReadReal;
            LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount].FInitalised := True;
          end;
        end;
        LLinesRead := LLinesRead + 1;
      end;

      for LLinesCount := LLinesRead + 1 to LFileData.Count - 1 do
        LMinimumPowerDemands.FF08ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile08Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile08Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LCount,
  LLinesCount: Integer;
  LF08File :TStringlist;
  LMinimumPowerDetails :TPowerGenerationObject;
  LMinimumPowerDemands: TMinimumPowerDemandsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile08Agent.strWritingStarted');
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

    LMinimumPowerDemands := ADataObject.FMinimumPowerDemandsObject;

    LF08File:= TStringList.Create;
    try

      for LLinesCount := 0 to LMinimumPowerDemands.FMinimumPowerDetailsList.Count - 1 do
      begin
        LMinimumPowerDetails := TPowerGenerationObject(LMinimumPowerDemands.FMinimumPowerDetailsList[LLinesCount]);
        if Assigned(LMinimumPowerDetails) then
        begin
          if(LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FInitalised and
             LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FInitalised) then
          begin
            //Line 1
            LOutString:='';

            LTempString:=PadString(LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LMinimumPowerDetails.FMinimumPowerData.FPowerChannelNumber);
            LOutString:=LOutString+LTempString;

            LF08File.Add(LOutString);

            //Line 1a
            LOutString :='';
            for LCount := MinPowerDemand to MaxPowerDemand do
            begin
              LTempString:=PadDouble(LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount]);
              LOutString:=LOutString+LTempString;
            end;

            LF08File.Add(LOutString);


            //Line 1b
            LOutString :='';
            for LCount := MinPowerDemand to MaxPowerDemand do
            begin
              LTempString:=PadDouble(LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount]);
              LOutString:=LOutString+LTempString;
            end;

            LF08File.Add(LOutString);
          end;
        end;
      end;

      //Three empty lines
      LF08File.Add('');
      LF08File.Add('');
      LF08File.Add('');

      for LCount := 0 to LMinimumPowerDemands.FF08ExtraLines.Count -1 do
      begin
        LF08File.Add(LMinimumPowerDemands.FF08ExtraLines[LCount]);
      end;

      LF08File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile08Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF08File.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
