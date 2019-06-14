//
//
//  UNIT      : Contains TFile07Agent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile07Agent;

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
  UPowerChannelObject,
  UYieldModelDataObject;

type

  TFile07Agent = class(TAbstractFileAgent)
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

function TFile07Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile07Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  //LInteger :TInteger;
  LMessage,
  LReadString,
  LTempString : String;
  LStartPos,
  LReadInteger,
  LCount,
  LLinesRead,
  LLinesCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LPowerPlant :TPowerPlantObject;
  LPowerChannelObject: TPowerChannelsObject;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
  //lPowerPlantID : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile07Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile07Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LPowerChannelObject := ADataObject.FPowerChannelObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LPowerChannelObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F07 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;
      //lPowerPlantID := 0;
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
        LPowerPlant :=TPowerPlantObject.Create;
        LPowerChannelObject.FPowerPlantItemList.Add(LPowerPlant);
        //lPowerPlantID := lPowerPlantID + 1;
        //LPowerPlant.FPowerPlantID := lPowerPlantID;

        LTempString := Copy(LReadString,1,36);
        if(Trim(LTempString) <> '') then
        begin
          LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FData := LTempString;
          LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised := True;
        end;

        LTempString := Copy(LReadString,37,6);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPowerPlantNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if ((LReadInteger < 0) OR (LReadInteger > 1000)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPowerPlantNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FData := LReadInteger;
          LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FInitalised := True;
          if not LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised then
          begin
            LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FData := 'Power Plant '+IntToStr(LReadInteger);
            LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised := True;
          end;
        end;

        LTempString:=GetSubstring(LReadString,43,6);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strMaxCapGeneratorErr');
          LMessage := Format(LMessage,[LLinesRead+1,43,48]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator.FData :=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator.FInitalised := True;
        end;

        LTempString:=GetSubstring(LReadString,49,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strMaxCapTurbineErr');
          LMessage := Format(LMessage,[LLinesRead+1,49,54]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine.FData :=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine.FInitalised := True;
        end;

        LTempString:=GetSubstring(LReadString,55,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strEfficiencyErr');
          LMessage := Format(LMessage,[LLinesRead+1,55,60]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FEfficiency.FData :=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FEfficiency.FInitalised := True;
        end;

        LTempString:=GetSubstring(LReadString,61,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPowerPlantStatusErr');
          LMessage := Format(LMessage,[LLinesRead+1,61,66]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if ((LReadInteger < 0) OR (LReadInteger > 1)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPowerPlantStatusErr');
          LMessage := Format(LMessage,[LLinesRead+1,61,66]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus.FData:=LReadInteger;
          LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus.FInitalised:= True;
        end;
        LLinesRead := LLinesRead + 1;

        //Line 1a
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');

        LTempString:=GetSubstring(LReadString,1,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strHeadLossErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FHeadLoss.FData:=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FHeadLoss.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,7,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strDesignHeadErr');
          LMessage := Format(LMessage,[LLinesRead+1,7,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FDesignHead.FData:=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FDesignHead.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,13,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strMaxNetHeadErr');
          LMessage := Format(LMessage,[LLinesRead+1,13,18]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FMaxNetHead.FData:=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FMaxNetHead.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,19,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strMinNetHeadErr');
          LMessage := Format(LMessage,[LLinesRead+1,19,24]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FMinNetHead.FData:=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FMinNetHead.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,25,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode = 0) then
        begin
          LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FData:=LReadReal;
          LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FInitalised:= True;
        end;

        LLinesRead := LLinesRead + 1;

        //Line 2
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LTempString := Copy(LReadString,1,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPointsCountErr');
          LMessage := Format(LMessage,[LLinesRead + 1,1,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if ((LReadInteger < 0) OR (LReadInteger > 10))then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strPointsCountErr');
          LMessage := Format(LMessage,[LLinesRead + 1,1,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FPointsCount.FData := LReadInteger;
          LPowerPlant.FPowerPlantHeadData.FPointsCount.FInitalised := True;
        end;
        LLinesRead := LLinesRead + 1;

      //Line 3 +++++++++++++++++++++++++++++
        LStartPos:=-5;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        if (LPowerPlant.FPowerPlantHeadData.FPointsCount.FInitalised) then
        begin
          for LCount := MinNumberOfPoints to LPowerPlant.FPowerPlantHeadData.FPointsCount.FData do
          begin
            Inc(LStartPos,6);
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile07Agent.strEfficiencyFactorErr');
              LMessage := Format(LMessage,[LLinesRead+1,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
            LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount].FData := LReadReal;
            LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount].FInitalised := True;
            end;
          end;
        end;
        LLinesRead := LLinesRead + 1;


      //Line 4 +++++++++++++++++++++++++++++
        LStartPos:=-5;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
        if (LPowerPlant.FPowerPlantHeadData.FPointsCount.FInitalised) then
        begin
          for LCount := MinNumberOfPoints to LPowerPlant.FPowerPlantHeadData.FPointsCount.FData do
          begin
            Inc(LStartPos,6);
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile07Agent.strNetHeadFactorErr');
              LMessage := Format(LMessage,[LLinesRead+1,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
            LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount].FData := LReadReal;
            LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount].FInitalised := True;
            end;
          end;
        end;
        LLinesRead := LLinesRead + 1;

      //line5 +++++++++++++++++++++++++++++
       LReadString := LFileData[LLinesRead];
       TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
       LTempString:=GetSubstring(LReadString,1,6);
       Val(LTempString,LReadInteger,LErrorCode);
       if (LErrorCode <> 0) then
       begin
         LMessage := FAppModules.Language.GetString('TFile07Agent.strTailWaterPointsCountErr');
         LMessage := Format(LMessage,[LLinesRead + 1,1,6]);
         AProgressFunction(LMessage,ptError,LStop);
         if FAppModules.GlobalData.StopOnFirstErr then Exit ;
       end
       else
       if ((LReadInteger < 0) OR (LReadInteger > 10))then
       begin
         LMessage := FAppModules.Language.GetString('TFile07Agent.strTailWaterPointsCountErr');
         LMessage := Format(LMessage,[LLinesRead + 1,1,6]);
         AProgressFunction(LMessage,ptError,LStop);
         if FAppModules.GlobalData.StopOnFirstErr then Exit ;
       end
       else
       begin
          LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FData := LReadInteger;
          LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FInitalised := True;
       end;

        LTempString:=GetSubstring(LReadString,7,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strTailWaterTypeCodeErr');
          LMessage := Format(LMessage,[LLinesRead+1,7,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if ((LReadInteger < 1) OR (LReadInteger > 3)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile07Agent.strTailWaterTypeCodeErr');
          LMessage := Format(LMessage,[LLinesRead+1,7,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode.FData:=LReadInteger;
          LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode.FInitalised:= True;
        end;
        LLinesRead := LLinesRead + 1;

      //line6 ++++++++++++++++++++++++++
        LStartPos:=-5;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
        if (LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FInitalised) then
        begin
          for LCount := MinNumberOfPoints to LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FData do
          begin
            Inc(LStartPos,6);
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile07Agent.strDischargeErr');
              LMessage := Format(LMessage,[LLinesRead,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
            LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount].FData := LReadReal;
            LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount].FInitalised := True;
            end;
          end;
        end;
        LLinesRead := LLinesRead + 1;

      //line7 ++++++++++++++++++++++++++
        LStartPos:=-5;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
        if (LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FInitalised) then
        begin
          for LCount := MinNumberOfPoints to LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FData do
          begin
            Inc(LStartPos,6);
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile07Agent.strTailWaterElevationErr');
              LMessage := Format(LMessage,[LLinesRead,LStartPos,LStartPos+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
            LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount].FData := LReadReal;
            LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount].FInitalised := True;
            end;
          end;
        end;
        LLinesRead := LLinesRead + 1;
      end;

      for LLinesCount := LLinesRead to LFileData.Count - 1 do
        LPowerChannelObject.FF07ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile07Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile07Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LCount: Integer;
  LF07File :TStringlist;
  LPowerPlant :TPowerPlantObject;
  LPowerChannelObject: TPowerChannelsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile07Agent.strWritingStarted');
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

    LF07File:= TStringList.Create;
    LPowerChannelObject := ADataObject.FPowerChannelObject;
    if not Assigned(LPowerChannelObject) then
      Exit;

    try
      for LLinesCount := 0 to LPowerChannelObject.FPowerPlantItemList.Count - 1 do
      begin
        LPowerPlant := TPowerPlantObject(LPowerChannelObject.FPowerPlantItemList[LLinesCount]);
        if Assigned(LPowerPlant) then
        begin
          if(LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised and
             LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised) then
          begin
            //Line 1
            LOutString:='';

            LTempString:=PadString(LPowerPlant.FPowerPlantHeadData.FPowerPlantName);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber);
            LOutString:=LOutString+LTempString;

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator,True);
            LOutString:=LOutString+LTempString;

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine,True);
            LOutString:=LOutString+LTempString;

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FEfficiency,True);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus);
            LOutString:=LOutString+LTempString;

            LF07File.Add(LOutString);

            //Line 1a
            LOutString:='';

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FHeadLoss,True);
            LOutString:=LOutString+LTempString;

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FDesignHead,True);
            LOutString:=LOutString+LTempString;

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FMaxNetHead,True);
            LOutString:=LOutString+LTempString;

            LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FMinNetHead,True);
            LOutString:=LOutString+LTempString;

            if LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FInitalised then
            begin
              LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel,True);
              LOutString:=LOutString+LTempString;
            end;

            LF07File.Add(LOutString);
          end;
        end;
        //Line 2
        LOutString :='';

        LTempString:=PadInt(LPowerPlant.FPowerPlantHeadData.FPointsCount);
        LOutString:=LOutString+LTempString;

        LF07File.Add(LOutString);

        //Line 3
        LOutString :='';
        for LCount := MinNumberOfPoints to MaxNumberOfPoints do
        begin
          LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount],True);
          LOutString:=LOutString+LTempString;
        end;

        LF07File.Add(LOutString);

        //Line 4
        LOutString :='';
        for LCount := MinNumberOfPoints to MaxNumberOfPoints do
        begin
          LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount],False,True);
          LOutString:=LOutString+LTempString;
        end;

        LF07File.Add(LOutString);

        //Line 5
        LOutString :='';

        LTempString:=PadInt(LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode);
        LOutString:=LOutString+LTempString;

        LF07File.Add(LOutString);

        //Line 6
        LOutString :='';
        for LCount := MinNumberOfPoints to MaxNumberOfPoints do
        begin
          LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount],False,True);
          LOutString:=LOutString+LTempString;
        end;

        LF07File.Add(LOutString);

        //Line 7
        LOutString :='';
        for LCount := MinNumberOfPoints to MaxNumberOfPoints do
        begin
          LTempString:=SmartPadDouble(LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount],False,True);
          LOutString:=LOutString+LTempString;
        end;

        LF07File.Add(LOutString);
      end;

      for LLinesCount := 0 to LPowerChannelObject.FF07ExtraLines.Count -1 do
      begin
        LF07File.Add(LPowerChannelObject.FF07ExtraLines[LLinesCount]);
      end;

      LF07File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile07Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF07File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
