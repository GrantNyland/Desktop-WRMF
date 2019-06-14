//
//
//  UNIT      : Contains TFile10Agent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 10/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile10Agent;

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
  UDivChannelDemandObject,
  UYieldModelDataObject;

type

  TFile10Agent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses UUtilities,
     UFilesLineTypeObject,
     UChannelDescriptionObject,
     UErrorHandlingOperations;

function TFile10Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile10Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LStartPos,
  LReadInteger,
  LCount,LIndex,
  LLinesRead,
  LLinesCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LFlowLine:TFlowLine;
  LDiversionChannelData12:TDiversionChannelData12;
  LDiversionChannelData3 : TDiversionChannelData3;
  LChannelDescrObject: TChannelDescrObject;
  LDivChannelDemandObject: TDivChannelDemandObject;
  LDiversionChannelObject: TDiversionChannelObject;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile10Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile10Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LDivChannelDemandObject := ADataObject.FDivChannelDemandObject;
    LChannelDescrObject := ADataObject.FChannelDescrObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LDivChannelDemandObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F10 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      while (LLinesRead < LFileData.Count) do
      begin
        if(LFileData.Count = 0) or
          (Trim(LFileData[LLinesRead]) = '') then
          Break;

        //Line 1 +++++++++++++++++++++++++++++
        LReadString := LFileData[LLinesRead];

        //Check if this is the zero line
        LTempString:=GetSubstring(LReadString,37,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
          Break;
        LDiversionChannelObject := LChannelDescrObject.FindDiversionChannel(LReadInteger);
        if not Assigned(LDiversionChannelObject) then
          Break;

        //Line 1 +++++++++++++++++++++++++++++
        if LDiversionChannelObject.FChannelType.FData < 3 then
        begin
          LDiversionChannelData12 := LDivChannelDemandObject.AddDiversionChannelData12;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

          LDiversionChannelData12.FHeading.FChannelType.FData := LDiversionChannelObject.FChannelType.FData;
          LDiversionChannelData12.FHeading.FChannelType.FInitalised := True;

          LTempString := Copy(LReadString,1,36);
          if(Trim(LTempString) <> '') then
          begin
            LDiversionChannelData12.FHeading.FDivChannelName.FData := LTempString;
            LDiversionChannelData12.FHeading.FDivChannelName.FInitalised := True;
          end;

          LTempString := Copy(LReadString,37,6);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile10Agent.strDivChannelNumberErr');
            LMessage := Format(LMessage,[LLinesRead+1,37,42]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          {if ((LReadInteger < 0) OR (LReadInteger > 1000)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile10Agent.strDivChannelNumberErr');
            LMessage := Format(LMessage,[LLinesRead+1,37,42]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else}
          begin
            LDiversionChannelData12.FHeading.FDivChannelNumber.FData := LReadInteger;
            LDiversionChannelData12.FHeading.FDivChannelNumber.FInitalised := True;
            if not LDiversionChannelData12.FHeading.FDivChannelName.FInitalised then
            begin
              LDiversionChannelData12.FHeading.FDivChannelName.FData := 'Channel '+IntToStr(LReadInteger);
              LDiversionChannelData12.FHeading.FDivChannelName.FInitalised := True;
            end;
          end;

          LLinesRead := LLinesRead + 1;

          //Line 2 +++++++++++++++++++++++++++++
          LStartPos:=-5;
          LReadString := LFileData[LLinesRead];
          if(LReadString = '') then
            Break;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
          for LCount := MinDiversionDemand to MaxDiversionDemand do
          begin
            Inc(LStartPos,6);
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strDiversionDemandFactorErr');
              LMessage := Format(LMessage,[LLinesRead+1,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDiversionChannelData12.FLine1Array.FLineValues[LCount].FData := LReadReal;
              LDiversionChannelData12.FLine1Array.FLineValues[LCount].FInitalised := True;
            end;
          end;
          LLinesRead := LLinesRead + 1;

          //Line 3 +++++++++++++++++++++++++++++
          LStartPos:=-5;
          LReadString := LFileData[LLinesRead];
//          LReadString := Trim(LReadString);
//          NB!!!! DO NOT TRIM THE STRING THAT WAS READ. THE SYSTEM WORKS ON COLUMNS WITH
//          SPECIFIC WIDTHS AND TRIMMING LEADING BLANKS RIGHT SHIFTS THE WHOLE PROCESS.
          if(LReadString = '') then
            Break;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
          for LCount := MinDiversionDemand to MaxDiversionDemand do
          begin
            Inc(LStartPos,6);
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strNaturalInflowFactorErr');
              LMessage := Format(LMessage,[LLinesRead+1,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDiversionChannelData12.FLine2Array.FLineValues[LCount].FData := LReadReal;
              LDiversionChannelData12.FLine2Array.FLineValues[LCount].FInitalised := True;
            end;
          end;
        end
        else
        begin
          if LDiversionChannelObject.FChannelType.FData = 3 then
          //Line 1 +++++++++++++++++++++++++++++
          begin
            LDiversionChannelData3 := LDivChannelDemandObject.AddDiversionChannelData3;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

            LDiversionChannelData3.FHeading.FChannelType.FData := LDiversionChannelObject.FChannelType.FData;
            LDiversionChannelData3.FHeading.FChannelType.FInitalised := True;

            LTempString := Copy(LReadString,1,36);
            if(LTempString <> '') then
            begin
              LDiversionChannelData3.FHeading.FDivChannelName.FData := LTempString;
              LDiversionChannelData3.FHeading.FDivChannelName.FInitalised := True;
            end;

            LTempString := Copy(LReadString,37,6);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strDivChannelNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,37,42]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            if ((LReadInteger < 0) OR (LReadInteger > 1000)) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strDivChannelNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,37,42]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDiversionChannelData3.FHeading.FDivChannelNumber.FData := LReadInteger;
              LDiversionChannelData3.FHeading.FDivChannelNumber.FInitalised := True;
            end;

            LLinesRead := LLinesRead + 1;
            //Line 4 +++++++++++++++++++++++++++++
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            if(LReadString = '') then
              Break;

            LTempString:=GetSubstring(LReadString,1,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strNodeNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,6]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strNodeNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,6]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDiversionChannelData3.FProportionCounts.FNodeNumber.FData := LReadInteger;
              LDiversionChannelData3.FProportionCounts.FNodeNumber.FInitalised:= True;
            end;

            LTempString:=GetSubstring(LReadString,7,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strResLevelsCountErr');
              LMessage := Format(LMessage,[LLinesRead+1,7,12]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData:=LReadInteger;
              LDiversionChannelData3.FProportionCounts.FResLevelsCount.FInitalised:= True;
            end;

            LTempString:=GetSubstring(LReadString,13,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile10Agent.strRefFlowsCountErr');
              LMessage := Format(LMessage,[LLinesRead+1,13,18]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FData:=LReadInteger;
              LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FInitalised:= True;
            end;

            LLinesRead := LLinesRead + 1;
            //Line 5 +++++++++++++++++++++++++++++
            LStartPos:=1;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
            for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
            begin
              Inc(LStartPos,6);
              LTempString:=GetSubstring(LReadString,LStartPos,6);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile10Agent.strResDiversionLevelsErr');
                LMessage := Format(LMessage,[LLinesRead+1,LStartPos,LStartPos+5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
              LDiversionChannelData3.FLineArray.FLineValues[LCount].FData := LReadReal;
              LDiversionChannelData3.FLineArray.FLineValues[LCount].FInitalised := True;
              end;
            end;

            //Line 6 +++++++++++++++++++++++++++++
            for LIndex := 1 to LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FData do
            begin
              LLinesRead := LLinesRead + 1;
              LFlowLine := LDiversionChannelData3.FChannelProportion.AddProportionFlowArray;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
              LTempString := Copy(LReadString,1,6);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile10Agent.strFlowValueErr');
                LMessage := Format(LMessage,[LLinesRead+1,1,6]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LFlowLine.FFlowValue.FData:=LReadReal;
                LFlowLine.FFlowValue.FInitalised:= True;
              end;
              LStartPos:=1;
              for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
              begin
                Inc(LStartPos,6);
                LTempString:=GetSubstring(LReadString,LStartPos,6);
                LTempString := Trim(LTempString);
                Val(LTempString,LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFile10Agent.strDivertedFlowErr');
                  LMessage := Format(LMessage,[LLinesRead+1,LStartPos,LStartPos+5]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LFlowLine.FLineValues[LCount].FData := LReadReal;
                  LFlowLine.FLineValues[LCount].FInitalised := True;
                end;
              end;
            end;
          end;
        end;
        LLinesRead := LLinesRead + 1;
      end;
      for LLinesCount := LLinesRead to LFileData.Count - 1 do
        LDivChannelDemandObject.FF10ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile10Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile10Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LCount,LIndex: Integer;
  LF10File :TStringlist;
  LFlowLine:TFlowLine;
  LDiversionChannelData12:TDiversionChannelData12;
  LDiversionChannelData3 : TDiversionChannelData3;
  LDivChannelDemandObject: TDivChannelDemandObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile10Agent.strWritingStarted');
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

    LF10File:= TStringList.Create;
    LDivChannelDemandObject := ADataObject.FDivChannelDemandObject;
    if not Assigned(LDivChannelDemandObject) then
      Exit;

    try
      for LLinesCount := 0 to LDivChannelDemandObject.DiversionChannelCount - 1 do
      begin
        if LDivChannelDemandObject.IsDiversionChannelData12(LLinesCount) then
        begin
          LDiversionChannelData12 := LDivChannelDemandObject.DiversionChannelData12[LLinesCount];

          LOutString:='';
          //Line 1 +++++++++++++++++++
          LTempString:=PadString(LDiversionChannelData12.FHeading.FDivChannelName);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LDiversionChannelData12.FHeading.FDivChannelNumber);
          LOutString:=LOutString+LTempString;
          LF10File.Add(LOutString);

          //Line 2 +++++++++++++++++++
          LOutString :='';
          for LCount := MinDiversionDemand to MaxDiversionDemand do
          begin
            LTempString:=PadDouble(LDiversionChannelData12.FLine1Array.FLineValues[LCount]);
            LOutString:=LOutString+LTempString;
          end;
          LF10File.Add(LOutString);

          //Line 3 +++++++++++++++++++
          LOutString :='';
          for LCount := MinDiversionDemand to MaxDiversionDemand do
          begin
            LTempString:=PadDouble(LDiversionChannelData12.FLine2Array.FLineValues[LCount]);
            LOutString:=LOutString+LTempString;
          end;
          LF10File.Add(LOutString);
        end
        else
        begin
          if LDivChannelDemandObject.IsDiversionChannelData3(LLinesCount) then
          begin
            LDiversionChannelData3 := LDivChannelDemandObject.DiversionChannelData3[LLinesCount];

            LOutString:='';
            //Line 1 +++++++++++++++++++++++++++++
            LTempString:=PadString(LDiversionChannelData3.FHeading.FDivChannelName);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LDiversionChannelData3.FHeading.FDivChannelNumber);
            LOutString:=LOutString+LTempString;
            LF10File.Add(LOutString);

            //Line 4 +++++++++++++++++++
            LOutString:='';
            LTempString:=PadInt(LDiversionChannelData3.FProportionCounts.FNodeNumber);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LDiversionChannelData3.FProportionCounts.FResLevelsCount);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LDiversionChannelData3.FProportionCounts.FRefFlowsCount);
            LOutString:=LOutString+LTempString;
            LF10File.Add(LOutString);

            //Line 5 +++++++++++++++++++
            LOutString:='      ';
            for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
            begin
              LTempString:=PadDouble(LDiversionChannelData3.FLineArray.FLineValues[LCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF10File.Add(LOutString);

            //Line 6 +++++++++++++++++++
            for LIndex := 0 to LDiversionChannelData3.FChannelProportion.ProportionCount -1 do
            begin
              LOutString:='';
              LFlowLine := LDiversionChannelData3.FChannelProportion.FlowProportion[LIndex];
              LTempString:=PadDouble(LFlowLine.FFlowValue);
              LOutString:=LOutString+LTempString;
              for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
              begin
                LTempString:=PadDouble(LFlowLine.FLineValues[LCount]);
                LOutString:=LOutString+LTempString;
              end;
              LF10File.Add(LOutString);
            end;
          end;
        end;
      end;

      for LLinesCount := 0 to LDivChannelDemandObject.FF10ExtraLines.Count -1 do
      begin
        LF10File.Add(LDivChannelDemandObject.FF10ExtraLines[LLinesCount]);
      end;

      LF10File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile10Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF10File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
