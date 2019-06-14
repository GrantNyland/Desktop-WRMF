{
  UNIT      : Contains TFile19Agent Class
  AUTHOR    : Maurice Marinus
  DATE      : 05/08/2006
  COPYRIGHT : Copyright © 2006 DWAF
}

unit UFile19Agent;

interface

uses
  Classes, sysutils, 

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UYMDemandCentreObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UBasicObjects;

type

  TFile19Agent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;


implementation

uses
  System.Contnrs,
  Math,
  UUtilities,
  UFilesLineTypeObject,
  UErrorHandlingOperations,
  UChannelDescriptionObject;

function TFile19Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFile19Agent.ReadModelDataFromFile';
var
  LFileData             : TStringList;
  LMessage,
  LReadString,
  LTempString           : string;
  LReadInteger,
  LCount,
  LCount1,
  LCount2,
  LLinesRead,
  LColNumber,
  LErrorCode            : Integer;
  LReadReal             : Double;

  LYMDemandCentre       : TYMDemandCentre;
  LYMDemandCentreObject : TYMDemandCentreObject;
  LFileLineTypesObject  : TAbstractFileLineTypesObject;
  LStop                 : Boolean;
  LDemandCentreNodeNrList : TStringList;
  LChannelDescr      : TChannelDescrObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile19Agent.strReadingStarted');
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
    if not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFile19Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LYMDemandCentreObject               := ADataObject.FYMDemandCentreObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not LYMDemandCentreObject.Initialise then
      Exit;

    LFileData := TStringList.Create;

    LDemandCentreNodeNrList := TStringList.Create;
    LDemandCentreNodeNrList.Duplicates := dupIgnore;
    LChannelDescr := ADataObject.FChannelDescrObject;
    try
      //Read the F19 file
      LFileData.LoadFromFile(AFilename.FileName);
      if(LFileData.Count = 0) then
      begin
        Result := True;
        Exit;
      end;

      if(ADataObject.FChannelDescrObject.FDemandCentreCount.FData = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile19Agent.NoDataInF03File');
        AProgressFunction(LMessage,ptError,LStop);
        Exit;
      end;

      LLinesRead  := 0;
      LCount1     := 1;

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      while (LLinesRead < LFileData.Count - 1) do
      begin
        if SameText(Trim(LFileData[LLinesRead]), '') then
          Exit;
        LYMDemandCentre  := LYMDemandCentreObject.AddDemandCentre;

        //Line 1
        LColNumber  := 0;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

        //Identifier
        LYMDemandCentre.Identifier.FData       := LCount1;
        LYMDemandCentre.Identifier.FInitalised := True;

        //Node Number
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strNodeNumber');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.NodeNumber.FData       := LReadInteger;
          LYMDemandCentre.NodeNumber.FInitalised := True;
        end;

        //Name
        Inc(LColNumber);
        LTempString := '';
        LTempString := ExtractDelemetedFirstSubstring('''',LReadString);
        if Length(LTempString) > 255 then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strNameValErr');
          LMessage := Format(LMessage,[LLinesRead+1,LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.Name.FData        := LTempString;
          LYMDemandCentre.Name.FLength      := Length(LYMDemandCentre.Name.FData);
          LYMDemandCentre.Name.FInitalised  := True;
        end;

        //Line 2
        LColNumber  := 0;
        Inc(LLinesRead);
        Inc(LColNumber);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');

        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strNodeRefNumber');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.NodeRefNr.FData       := LReadInteger;
          LYMDemandCentre.NodeRefNr.FInitalised := True;
        end;

        //Line 3
        LColNumber  := 0;
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');

        //Line 3a
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strReturnFlowFactor');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.AveReturnFlowFactor.FData       := LReadReal;
          LYMDemandCentre.AveReturnFlowFactor.FInitalised := True;
        end;

        //Line 3b
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strEvaporation');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.AveEvaporation.FData       := LReadReal;
          LYMDemandCentre.AveEvaporation.FInitalised := True;
        end;

        //Line 3c
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strDeviationFactor');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.StdDeviationFactor.FData       := LReadReal;
          LYMDemandCentre.StdDeviationFactor.FInitalised := True;
        end;

        //Line 3d
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strRoutingConstant');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.RoutingConstant.FData       := LReadReal;
          LYMDemandCentre.RoutingConstant.FInitalised := True;
        end;

        //Line 3e
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strRainfallScalingFactor');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.RainfallScalingFactor.FData       := LReadReal;
          LYMDemandCentre.RainfallScalingFactor.FInitalised := True;
        end;

        //Line 4
        LColNumber := 0;

        LDemandCentreNodeNrList.Clear;

        for LCount := 0 to ADataObject.FChannelDescrObject.FReturnFLowChannelList.Count - 1 do
        begin
          if TReturnFlowChannelObject(ADataObject.FChannelDescrObject.FReturnFLowChannelList[LCount]).
              FDemandCentreNodeNr.FData = LYMDemandCentre.NodeNumber.FData then
          begin
            LDemandCentreNodeNrList.Add(IntToStr(LYMDemandCentre.NodeNumber.FData));
          end;
        end;

        for LCount := 0 to LDemandCentreNodeNrList.Count - 1 do
        begin
          Inc(LLinesRead);
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
          LYMDemandCentre.ReturnFlowChannel[LCount+1] := TYMDemandCentreReturnFlowChannel.Create;

          //Line 4a
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile19Agent.strReturnFlowChannelNo');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LYMDemandCentre.ReturnFlowChannel[LCount+1].ChannelNr.FData       := LReadInteger;
            LYMDemandCentre.ReturnFlowChannel[LCount+1].ChannelNr.FInitalised := True;
          end;

          //Line 4b
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile19Agent.strTotalReturnFlow');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LYMDemandCentre.ReturnFlowChannel[LCount+1].TotalReturnFlow.FData       := LReadReal;
            LYMDemandCentre.ReturnFlowChannel[LCount+1].TotalReturnFlow.FInitalised := True;
          end;

          //Line 4c
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile19Agent.strFlowDiversion');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LYMDemandCentre.ReturnFlowChannel[LCount+1].FlowDiversion.FData       := LReadReal;
            LYMDemandCentre.ReturnFlowChannel[LCount+1].FlowDiversion.FInitalised := True;
          end;

          LYMDemandCentre.ReturnFlowChannel[LCount+1].Identifier.FData       := LCount+1;
          LYMDemandCentre.ReturnFlowChannel[LCount+1].Identifier.FInitalised := True;

          LYMDemandCentre.ReturnFlowChannel[LCount+1].DemandCentreID.FData       := LYMDemandCentre.Identifier.FData;
          LYMDemandCentre.ReturnFlowChannel[LCount+1].DemandCentreID.FInitalised := True;
        end;
        LYMDemandCentre.ChannelCount := LDemandCentreNodeNrList.Count;

        //Line 5
        LColNumber  := 0;
        Inc(LLinesRead);
        Inc(LColNumber);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile19Agent.strTotalFlowLost');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LYMDemandCentre.TotalFlowLost.FData       := LReadReal;
          LYMDemandCentre.TotalFlowLost.FInitalised := True;
        end;

        //Line 6
        LColNumber  := 0;
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
        for LCount := MinMonths to MaxMonths do
        begin
          Inc(LColNumber);
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile19Agent.strEvapoTranspiration');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LYMDemandCentre.EvapoTranspiration[LCount].FData       := LReadReal;
            LYMDemandCentre.EvapoTranspiration[LCount].FInitalised := True;
          end;
        end;

        //Other data
        for LCount := 0 to LChannelDescr.FDemandCentreList.Count - 1 do
        begin
          if LYMDemandCentre.NodeNumber.FData =
                TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNodeNumber.FData then
          begin
            LYMDemandCentre.ConsumptiveChannelNr.FData  :=
                    TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FConsumptiveChannelNr.FData;
            LYMDemandCentre.ConsumptiveChannelNr.FInitalised := True;

            for LCount2 := 0 to LChannelDescr.FReclaimationChannelList.Count - 1 do
            begin
              if LYMDemandCentre.NodeNumber.FData =
                 TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount2]).FUpStreamNodeNr.FData then
              begin
                LYMDemandCentre.ReclaimationChannelNr.FData       := TReclaimationChannelObject(LChannelDescr.
                                                                      FReclaimationChannelList[LCount2]).
                                                                      FChannelNr.FData;
                LYMDemandCentre.ReclaimationChannelNr.FInitalised := True;
              end;  
            end;
            Break;
          end;
        end;

        Inc(LCount1);
        Inc(LLinesRead);
      end;

      Inc(LLinesRead);
      for LCount := LLinesRead to LFileData.Count - 1 do
        LYMDemandCentreObject.ExtraLines.Add(LFileData[LCount]);

      LMessage  := FAppModules.Language.GetString('TFile19Agent.strReadingCompleted');
      LMessage  := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result    := True;
    finally
      LFileData.Free;
      if LDemandCentreNodeNrList <> nil then
        LDemandCentreNodeNrList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile19Agent.WriteModelDataToFile';
var
  LMessage,
  LOutString,
  LTempString   : String;
  LLinesCount,
  LCount        : Integer;
  LStop         : Boolean;
  LF19File       : TStringlist;

  LYMDemandCentre        : TYMDemandCentre;
  LYMDemandCentreObject  : TYMDemandCentreObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile19Agent.strWritingStarted');
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

    LF19File  := TStringList.Create;
    LYMDemandCentreObject := ADataObject.FYMDemandCentreObject;
    if not Assigned(LYMDemandCentreObject) then
      Exit;

    try
      for LLinesCount := 0 to LYMDemandCentreObject.DemandCentreCount-1 do
      begin
        LYMDemandCentre := TYMDemandCentre(LYMDemandCentreObject.DemandCentreObjectByIndex[LLinesCount]);
        
        LOutString  := '';
        //Line 1a
        LTempString := PadInt(LYMDemandCentre.NodeNumber);
        LOutString  := LOutString + LTempString;
        //Line 1b
        LTempString := LYMDemandCentre.Name.FData;
        LOutString  := LOutString + ' ' + QuotedStr(LTempString);
        LF19File.Add(LOutString);

        //Line 2
        LOutString  := '';
        LTempString := PadInt(LYMDemandCentre.NodeRefNr);
        LOutString  := LOutString + LTempString;
        LF19File.Add(LOutString);

        //Line 3a
        LOutString  := '';
        LTempString := PadDouble(LYMDemandCentre.AveReturnFlowFactor);
        LOutString  := LOutString + LTempString;

        //Line 3b
        LTempString := PadDouble(LYMDemandCentre.AveEvaporation);
        LOutString  := LOutString + LTempString;

        //Line 3c
        LTempString := PadDouble(LYMDemandCentre.StdDeviationFactor);
        LOutString  := LOutString + LTempString;

        //Line 3d
        LTempString := PadDouble(LYMDemandCentre.RoutingConstant);
        LOutString  := LOutString + LTempString;

        //Line 3e
        LTempString := PadDouble(LYMDemandCentre.RainfallScalingFactor);
        LOutString  := LOutString + LTempString;
        LF19File.Add(LOutString);

        //Line 4
        for LCount  := 1 to LYMDemandCentre.ChannelCount do
        begin
          LOutString  := '';
          //Line 4a
          LTempString := PadInt(LYMDemandCentre.ReturnFlowChannel[LCount].ChannelNr);
          LOutString  := LOutString + LTempString;

          //Line 4b
          LTempString := PadDouble(LYMDemandCentre.ReturnFlowChannel[LCount].TotalReturnFlow);
          LOutString  := LOutString + LTempString;

          //Line 4c
          LTempString := PadDouble(LYMDemandCentre.ReturnFlowChannel[LCount].FlowDiversion);
          LOutString  := LOutString + LTempString;
          LF19File.Add(LOutString);
        end;

        //Line 5
        LOutString  := '';
        LTempString := PadDouble(LYMDemandCentre.TotalFlowLost);
        LOutString  := LOutString + LTempString;
        LF19File.Add(LOutString);

        //Line 6
        LOutString  := '';
        for LCount  := MinMonths to MaxMonths do
        begin
          LTempString := PadDouble(LYMDemandCentre.EvapoTranspiration[LCount]);
          LOutString  := LOutString + LTempString;
        end;
        LF19File.Add(LOutString);
      end;

      for LLinesCount := 0 to LYMDemandCentreObject.ExtraLines.Count -1 do
        LF19File.Add(LYMDemandCentreObject.ExtraLines[LLinesCount]);

      LF19File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      if(Trim(LF19File.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile19Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LF19File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
