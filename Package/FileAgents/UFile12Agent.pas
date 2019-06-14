//
//
//  UNIT      : Contains TFile12Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile12Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UChannelMinMaxObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type
  //LineType = (ltNone,ltChannelName,ltFlowValues);
  TFile12Agent = class(TAbstractFileAgent)
  protected
    //function GetLineType(AlineData: string): LineType;
    function GetFlowContraintsLinesCount(AChannelNumber: integer;  ADataObject: TDataFileObjects): integer;
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
  UChannelDescriptionObject,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

function TFile12Agent.GetFlowContraintsLinesCount(AChannelNumber: integer;ADataObject: TDataFileObjects): integer;
const OPNAME = 'TFile12Agent.GetFlowContraintsLinesCount';
var
  LChannelDescrObject: TChannelDescrObject;
  LMinMaxChannelObject:TMultiPurposeChannelObject;
  LPumpingChannel:TPumpingChannelObject;
  LChannelPenaltyStructure: integer;
  LPenaltyChannelObject: TPenaltyChannelObject;
begin
  Result := NullInteger;
  try
    LChannelDescrObject := ADataObject.FChannelDescrObject;
    if Assigned(LChannelDescrObject) then
    begin
      LMinMaxChannelObject := LChannelDescrObject.FindMultiPurposeChannel(AChannelNumber);
      if Assigned(LMinMaxChannelObject) then
      begin
        LChannelPenaltyStructure := LMinMaxChannelObject.FPenaltyStructType.FData;
        LPenaltyChannelObject :=  LChannelDescrObject.FindPenaltyStructure(LChannelPenaltyStructure);
        if Assigned(LPenaltyChannelObject) then
        begin
          Result := LPenaltyChannelObject.FArcCount.FData;
        end;
      end
      else
      begin
        LPumpingChannel := LChannelDescrObject.FindPumpingChannel(AChannelNumber);
        if Assigned(LPumpingChannel) then
        begin
          LChannelPenaltyStructure := LPumpingChannel.FPenaltyStructType.FData;
          LPenaltyChannelObject :=  LChannelDescrObject.FindPenaltyStructure(LChannelPenaltyStructure);
          if Assigned(LPenaltyChannelObject) then
          begin
            Result := LPenaltyChannelObject.FArcCount.FData;
          end;
       end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TFile12Agent.GetLineType(AlineData: string): LineType;
const OPNAME = 'TTFile12Agent.ReadFromFile';
var
  LValue: Double;
  LCount,
  LStartPos: integer;
  LChar: char;
  LSubString: string;
  LError: integer;
begin
  Result := ltNone;
  try
    if(Trim(AlineData) = '') then
    begin
      Result := ltNone;
    end
    else
    begin
      LSubString := Copy(AlineData,1,36);
      for LCount := 1 to Length(LSubString) do
      begin
        LChar := LSubString[LCount];
        if (LChar in ['A'..'Z','a'..'z']) then
        begin
          Result := ltChannelName;
          Break;
        end;
      end;

      if (Result = ltNone) then
      begin
        Result := ltFlowValues;
        LCount := 1;
        LStartPos := 1;
        while LStartPos < Length(AlineData) do
        begin
          if (LCount > 12) then
            Break;

          LSubString := Trim(Copy(AlineData,LStartPos,6));
          Val(LSubString,LValue,LError);
          if(LError <> 0) then
          begin
             Result := ltNone;
             Break;
          end;
          if(LValue > 0.0) then;
          LCount := LCount + 1;
          LStartPos := LStartPos + 6;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TFile12Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile12Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  //LInteger :TInteger;
  LMessage,
  LReadString,
  LTempString : String;
  LStart,
  LReadInteger,
  LFlowConstraintsLinesCount,
  LIndex,
  LCount,
  //LLinesCount,
  LLinesRead,
  LErrorCode : Integer;
  LReadReal : Double;
  LChannelMinMaxObject: TChannelMinMaxObject;
  LChannelMinMax:TChannelMinMax;
  LMonthlyFlowConstraints:TMonthlyFlowConstraints;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile12Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile12Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LChannelMinMaxObject := ADataObject.FChannelMinMaxObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LChannelMinMaxObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F12 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      while(LLinesRead < LFileData.Count) do
      begin
        LReadString := LFileData[LLinesRead];
        LLinesRead  := LLinesRead + 1;
        if(Trim(LReadString) = '') then Break;


        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        LChannelMinMax := TChannelMinMax.Create;
        LChannelMinMaxObject.FChannelMinMaxContainer.Add(LChannelMinMax);
        LTempString := Copy(LReadString,1,36);
        if(Trim(LTempString) <> '') then
        begin
          LChannelMinMax.FChannelMinMaxName.FData := LTempString;
          LChannelMinMax.FChannelMinMaxName.FInitalised := True;
        end;

        LTempString := Copy(LReadString,37,6);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile12Agent.strIrrigationNodeNumberErr');
          LMessage := Format(LMessage,[LLinesRead,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelMinMax.FChannelMinMaxNumber.FData := LReadInteger;
          LChannelMinMax.FChannelMinMaxNumber.FInitalised := True;

          if not LChannelMinMax.FChannelMinMaxName.FInitalised then
          begin
            LChannelMinMax.FChannelMinMaxName.FData := 'Channel '+ IntToStr(LReadInteger);
            LChannelMinMax.FChannelMinMaxName.FInitalised := True;
          end;
        end;
        LTempString := Copy(LReadString,43,Length(LReadString));
        if(Trim(LTempString)  <> '') then
        begin
           LChannelMinMax.FComment.FData       := LTempString;
           LChannelMinMax.FComment.FInitalised := True;
           LChannelMinMax.FComment.FLength     := Length(LTempString);
        end;


        if LChannelMinMax.FChannelMinMaxNumber.FInitalised  then
        begin
          LFlowConstraintsLinesCount := GetFlowContraintsLinesCount(LChannelMinMax.FChannelMinMaxNumber.FData,ADataObject);
          if(LFlowConstraintsLinesCount = NullInteger) then
          begin
            LMessage := FAppModules.Language.GetString('TFile12Agent.strMinMaxNoArcCountErr');
            LMessage := Format(LMessage,[LLinesRead,LChannelMinMax.FChannelMinMaxNumber.FData]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            for LIndex := 1 to LFlowConstraintsLinesCount do
            begin
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
              LLinesRead  := LLinesRead + 1;

              LMonthlyFlowConstraints := LChannelMinMax.AddMonthlyFlowConstraints;
              if Assigned(LMonthlyFlowConstraints) then
              begin
                LStart := 1;
                for LCount := MinFlowConstraints to MaxFlowConstraints do
                begin
                  LTempString := Trim(Copy(LReadString,LStart,6));
                  if(LTempString = '') then
                    Break;
                  Val(LTempString,LReadReal,LErrorCode);
                  if(LErrorCode <> 0) then
                  begin
                    LMessage := FAppModules.Language.GetString('TFile12Agent.strInitialLevelErr');
                    LMessage := Format(LMessage,[LLinesRead+1,LStart,LStart+5]);
                    AProgressFunction(LMessage,ptError,LStop);
                    if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                  end
                  else
                  begin
                    LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FData := LReadReal;
                    LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FInitalised := True;
                  end;
                  LStart := LStart + 6;
                end;
                LTempString := Copy(LReadString,73,Length(LReadString));
                if(Trim(LTempString)  <> '') then
                begin
                   LMonthlyFlowConstraints.FComment.FData       := LTempString;
                   LMonthlyFlowConstraints.FComment.FInitalised := True;
                   LMonthlyFlowConstraints.FComment.FLength     := Length(LTempString);
                end;
              end;
            end;
          end;
        end;
      end;

      for LCount := LLinesRead to LFileData.Count - 1 do
        LChannelMinMaxObject.FF12ExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFile12Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile12Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile12Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LLinesRepeat,
  //LLength,
  LCount: Integer;
  LF12File :TStringlist;
  LChannelMinMaxObject: TChannelMinMaxObject;
  LChannelMinMax:TChannelMinMax;
  LMonthlyFlowConstraints:TMonthlyFlowConstraints;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile12Agent.strWritingStarted');
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

    LF12File:= TStringList.Create;
    LChannelMinMaxObject := ADataObject.FChannelMinMaxObject;
    if not Assigned(LChannelMinMaxObject) then
      Exit;

    try
      for LLinesCount := 0 to LChannelMinMaxObject.FChannelMinMaxContainer.Count -1 do
      begin
        LChannelMinMax := TChannelMinMax(LChannelMinMaxObject.FChannelMinMaxContainer[LLinesCount]);
        LOutString:='';
        LTempString:=PadString(LChannelMinMax.FChannelMinMaxName);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LChannelMinMax.FChannelMinMaxNumber);
        LOutString:=LOutString+LTempString;
        if LChannelMinMax.FComment.FInitalised then
          LOutString:=LOutString+LChannelMinMax.FComment.FData;

        LF12File.Add(LOutString);
        LOutString := '';

        for LLinesRepeat := 0 to LChannelMinMax.FMonthlyFlowConstraintsContainer.Count - 1 do
        begin
          LMonthlyFlowConstraints := TMonthlyFlowConstraints(LChannelMinMax.FMonthlyFlowConstraintsContainer[LLinesRepeat]);
          for LCount := MinDiversionFlow to MaxDiversionFlow do
          begin
            if not LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FInitalised then
              Break;

            LTempString :=SmartPadDouble(LMonthlyFlowConstraints.FFlowConstraintsValues[LCount],False,True);
            {LTempString := ' '+LTempString;
            LLength    := LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FLength;
            if(LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FData = 0.0) then
              LTempString := ' ' + Copy(LTempString,1,LLength-1);
            if(LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FData >= 10.0) and (LTempString[LLength] = '0') then
              LTempString := ' ' + Copy(LTempString,1,LLength-1);}

            LOutString:=LOutString+LTempString;
          end;
          if LMonthlyFlowConstraints.FComment.FInitalised then
            LOutString:=LOutString+LMonthlyFlowConstraints.FComment.FData;
          LF12File.Add(LOutString);
          LOutString := '';
        end;
      end;

      LF12File.Add('');
      LF12File.Add('');
      for LLinesCount := 0 to LChannelMinMaxObject.FF12ExtraLines.Count -1 do
      begin
        LF12File.Add(LChannelMinMaxObject.FF12ExtraLines[LLinesCount]);
      end;

      LF12File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile12Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF12File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

