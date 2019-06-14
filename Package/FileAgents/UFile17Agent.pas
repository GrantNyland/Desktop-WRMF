{
  UNIT      : Contains TFile17Agent Class
  AUTHOR    : Maurice Marinus
  DATE      : 06/06/2006
  COPYRIGHT : Copyright © 2006 DWAF
}

unit UFile17Agent;

interface

uses
  Classes, sysutils, 

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UIrrigationBlockObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UBasicObjects;

type

  TFile17Agent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;


implementation

uses UUtilities,
     UFilesLineTypeObject,
     UErrorHandlingOperations, Math;

function TFile17Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFile17Agent.ReadModelDataFromFile';
var
  LFileData                   : TStringList;
  LMessage,
  LReadString,
  LTempString                 : string;
  LReadInteger,
  LCount,
  LCount1,
  LCount2,
  LLinesRead,
  LColNumber,
  LErrorCode                  : Integer;
  LReadReal                   : Double;

  LIrrigationBlock            : TIrrigationBlock;
  LIrrigationBlockObject      : TIrrigationBlockObject;
  LFileLineTypesObject        : TAbstractFileLineTypesObject;
  LStop                       : Boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile17Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile17Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LIrrigationBlockObject              := ADataObject.FIrrigationBlockObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not LIrrigationBlockObject.Initialise then
      Exit;

    LFileData := TStringList.Create;
    try
      //Read the F17 file
      LFileData.LoadFromFile(AFilename.FileName);
      if(LFileData.Count = 0) then
      begin
        Result := True;
        Exit;
      end;

      if(ADataObject.FChannelDescrObject.FIrrigationBlockCount.FData = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile17Agent.NoDataInF03File');
        AProgressFunction(LMessage,ptError,LStop);
        Exit;
      end;

      LLinesRead  := 0;
      LCount1     := 0;

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      while (LLinesRead < LFileData.Count - 1) do
      begin
        if SameText(Trim(LFileData[LLinesRead]), '') then
          Exit;
        LIrrigationBlock  := LIrrigationBlockObject.AddIrrigationBlock;
        LReadString       := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

        //Line 1
        LColNumber  := 0;

        // Block number
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strBlockNumber');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.BlockNodeNumber.FData := LReadInteger;
          LIrrigationBlock.BlockNodeNumber.FInitalised := True;
        end;

        // Block Name
        Inc(LColNumber);
        LTempString := '';
        LTempString := ExtractDelemetedFirstSubstring('''',LReadString);
        if Length(LTempString) > 20 then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strBlockNameValErr');
          LMessage := Format(LMessage,[LLinesRead+1,LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.BlockName.FData        := LTempString;
          LIrrigationBlock.BlockName.FLength      := Length(LIrrigationBlock.BlockName.FData);
          LIrrigationBlock.BlockName.FInitalised  := True;
        end;

        // Maximum water allocation
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strMaxWaterAllocation');
          LMessage := Format(LMessage,[LLinesRead+1,LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.MaxWaterAllocation.FData       := LReadReal;
          LIrrigationBlock.MaxWaterAllocation.FInitalised := True;
        end;

        // FileName
        Inc(LColNumber);
        LTempString := '';
        LTempString := ExtractDelemetedFirstSubstring('''',LReadString);
        if Length(LTempString) > 255 then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strFileNameValErr');
          LMessage := Format(LMessage,[LLinesRead+1,LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.FileName.FData       := LTempString;
          LIrrigationBlock.FileName.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strNodeNumber');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.HydrologyNodeNumber.FData := LReadInteger;
          LIrrigationBlock.HydrologyNodeNumber.FInitalised := True;
        end;

        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LIrrigationBlock.DroughtApplicable.FData       := 0;
          LIrrigationBlock.DroughtApplicable.FInitalised := True;
        end
        else
        begin
          LIrrigationBlock.DroughtApplicable.FData       := LReadInteger;
          LIrrigationBlock.DroughtApplicable.FInitalised := True;
        end;

        LIrrigationBlock.BlockIdentifier.FData       := LCount1;
        LIrrigationBlock.BlockIdentifier.FInitalised := True;

        //Line 2
        LColNumber  := 0;
        Inc(LLinesRead);
        Inc(LColNumber);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strCanalTransportLoss');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.CanalTransportLoss.FData       := LReadReal;
          LIrrigationBlock.CanalTransportLoss.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('strEfficiencyFactor.');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.EfficiencyFactor.FData       := LReadReal;
          LIrrigationBlock.EfficiencyFactor.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strReturnFlowFactor');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.ReturnFlowFactor.FData       := LReadReal;
          LIrrigationBlock.ReturnFlowFactor.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strUpperZoneReturnFlow');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.UpperZoneReturnFlow.FData       := LReadReal;
          LIrrigationBlock.UpperZoneReturnFlow.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strLowerZoneReturnFlow');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.LowerZoneReturnFlow.FData       := LReadReal;
          LIrrigationBlock.LowerZoneReturnFlow.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strReturnFlowLoss');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.ReturnFlowLoss.FData       := LReadReal;
          LIrrigationBlock.ReturnFlowLoss.FInitalised := True;
        end;

        //Line 3
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LColNumber  := 0;
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strUpperZoneSoilMoistureCapacity');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.UpperZoneSoilMoistureCapacity.FData       := LReadReal;
          LIrrigationBlock.UpperZoneSoilMoistureCapacity.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strLowerZoneSoilMoistureCapacity');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.LowerZoneSoilMoistureCapacity.FData       := LReadReal;
          LIrrigationBlock.LowerZoneSoilMoistureCapacity.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strUpperZoneSoilMoistureTarget');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.UpperZoneSoilMoistureTarget.FData       := LReadReal;
          LIrrigationBlock.UpperZoneSoilMoistureTarget.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strInitialSoilMoistureStorage');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.InitialSoilMoistureStorage.FData       := LReadReal;
          LIrrigationBlock.InitialSoilMoistureStorage.FInitalised := True;
        end;


        //Line 4 : Monthly rainfall factors
        LColNumber  := 0;
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
        for LCount  := MinMonths to MaxMonths do
        begin
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile17Agent.strRainfallFactor');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LIrrigationBlock.RainfallFactor[LCount].FData       := LReadReal;
            LIrrigationBlock.RainfallFactor[LCount].FInitalised := True;
          end;
        end;

        //Line 5 : Monthly mean pan evaporation
        LColNumber  := 0;
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
        for LCount := MinMonths to MaxMonths do
        begin
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile17Agent.strPanEvaporation');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LIrrigationBlock.PanEvaporation[LCount].FData       := LReadReal;
            LIrrigationBlock.PanEvaporation[LCount].FInitalised := True;
          end;
        end;

        //Line 6 : Monthly mean APan conversion
        LColNumber  := 0;
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
        for LCount := MinMonths to MaxMonths do
        begin
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile17Agent.strAPanConvFactor');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LIrrigationBlock.APanConvFactor[LCount].FData       := LReadReal;
            LIrrigationBlock.APanConvFactor[LCount].FInitalised := True;
          end;
        end;

        //Line 7
        Inc(LLinesRead);
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
        LColNumber  := 0;
        LReadString := LFileData[LLinesRead];
        Inc(LColNumber);
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strNumberOfCropTypes');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.NumberOfCropTypes.FData       := LReadInteger;
          LIrrigationBlock.NumberOfCropTypes.FInitalised := True;
        end;

        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LIrrigationBlock.CropWaterUseType.FData       := 1;
          LIrrigationBlock.CropWaterUseType.FInitalised := True;
        end
        else
        begin
          LIrrigationBlock.CropWaterUseType.FData       := LReadInteger;
          LIrrigationBlock.CropWaterUseType.FInitalised := True;
        end;

        //Line 8
        LColNumber  := 0;
        for LCount := MinNoOfCrops to LIrrigationBlock.NumberOfCropTypes.FData do
        begin
          Inc(LLinesRead);
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
          LIrrigationBlock.WaterUsageFactor[LCount] := TWaterUsage.Create;
          for LCount2 := MinMonths to MaxMonths do
          begin
            Inc(LColNumber);
            LTempString   := '';
            LTempString   := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile17Agent.strMonthlyWaterUse');
              LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
              AProgressFunction(LMessage,ptWarning,LStop);
              //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LIrrigationBlock.WaterUsageFactor[LCount].MonthlyWaterUse[LCount2].FData       := LReadReal;
              LIrrigationBlock.WaterUsageFactor[LCount].MonthlyWaterUse[LCount2].FInitalised := True;
            end;
          end;
          Inc(LColNumber);
          LTempString   := '';
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile17Agent.strPercAreaUnderCropType');
            LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LIrrigationBlock.WaterUsageFactor[LCount].PercAreaUnderCropType.FData       := LReadReal;
            LIrrigationBlock.WaterUsageFactor[LCount].PercAreaUnderCropType.FInitalised := True;
          end;
          LIrrigationBlock.WaterUsageFactor[LCount].Identifier.FData       := LCount;
          LIrrigationBlock.WaterUsageFactor[LCount].Identifier.FInitalised := True;

          LIrrigationBlock.WaterUsageFactor[LCount].BlockIdentifier.FData       := LIrrigationBlock.BlockIdentifier.FData;
          LIrrigationBlock.WaterUsageFactor[LCount].BlockIdentifier.FInitalised := True;
          
          Inc(LColNumber);
          LTempString   := ExtractDelemetedFirstSubstring('''',LReadString);
          LIrrigationBlock.WaterUsageFactor[LCount].CropName.FData         := LTempString;
          LIrrigationBlock.WaterUsageFactor[LCount].CropName.FLength       := Length(LTempString);
          LIrrigationBlock.WaterUsageFactor[LCount].CropName.FInitalised   := True;
        end;

        //Line 9
        Inc(LLinesRead);
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9');
        LColNumber  := 0;
        LReadString := LFileData[LLinesRead];
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strRainAboveRainFactorSpecValue');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.RainAboveRainFactorSpecValue.FData       := LReadReal;
          LIrrigationBlock.RainAboveRainFactorSpecValue.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strRainBelowRainFactor');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.RainBelowRainFactor.FData       := LReadReal;
          LIrrigationBlock.RainBelowRainFactor.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strRainCatchmentScalingFactor');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.RainCatchmentScalingFactor.FData       := LReadReal;
          LIrrigationBlock.RainCatchmentScalingFactor.FInitalised := True;
        end;

        //Line 10
        Inc(LLinesRead);
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'10');
        LColNumber  := 0;
        LReadString := LFileData[LLinesRead];
        Inc(LColNumber);

        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile17Agent.strAllocatedIrrigationArea');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LIrrigationBlock.AllocatedIrrigationArea.FData       := LReadReal;
          LIrrigationBlock.AllocatedIrrigationArea.FInitalised := True;
        end;

        Inc(LCount1);
        Inc(LLinesRead);
      end;

      Inc(LLinesRead);
      for LCount := LLinesRead to LFileData.Count - 1 do
        LIrrigationBlockObject.ExtraLines.Add(LFileData[LCount]);

      LMessage  := FAppModules.Language.GetString('TFile17Agent.strReadingCompleted');
      LMessage  := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result    := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile17Agent.WriteModelDataToFile';
var
  LMessage,
  LOutString,
  LTempString             : String;
  LLinesCount,
  LCount,
  LCount2                 : Integer;
  LStop                   : Boolean;
  LF17File                : TStringlist;

  LIrrigationBlock        : TIrrigationBlock;
  LIrrigationBlockObject  : TIrrigationBlockObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile17Agent.strWritingStarted');
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

    LIrrigationBlockObject := ADataObject.FIrrigationBlockObject;
    if not Assigned(LIrrigationBlockObject) then
      Exit;
    if (LIrrigationBlockObject.IrrigationBlockCount = 0) and
       (LIrrigationBlockObject.ExtraLines.Count = 0) then
    begin
      if FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);
      Result := True;
      Exit;
    end;

    LF17File  := TStringList.Create;
    try

      for LLinesCount := 0 to LIrrigationBlockObject.IrrigationBlockCount-1 do
      begin
        LIrrigationBlock := TIrrigationBlock(LIrrigationBlockObject.IrrigationBlockObjectByIndex[LLinesCount]);
        //Line 1
        LOutString  := '';

        LTempString := IntToStr(LIrrigationBlock.BlockNodeNumber.FData);
        LOutString  := LOutString + ' ' + LTempString;

        LTempString := LIrrigationBlock.BlockName.FData;
        LOutString  := LOutString + ' ' + QuotedStr(LTempString);

        LTempString := Trim(PadDouble(LIrrigationBlock.MaxWaterAllocation));
        LOutString  := LOutString + ' ' + LTempString;

        LTempString := LIrrigationBlock.FileName.FData;
        LOutString  := LOutString + ' ' + QuotedStr(LTempString);      

        LTempString := IntToStr(LIrrigationBlock.HydrologyNodeNumber.FData);
        LOutString  := LOutString + ' ' + LTempString;

        LTempString := IntToStr(LIrrigationBlock.DroughtApplicable.FData);
        LOutString  := LOutString + ' ' + LTempString;

        LF17File.Add(LOutString);

        //Line 2
        LOutString  := '';
        LTempString := Trim(PadDouble(LIrrigationBlock.CanalTransportLoss));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.EfficiencyFactor));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.ReturnFlowFactor));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.UpperZoneReturnFlow));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.LowerZoneReturnFlow));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.ReturnFlowLoss));
        LOutString  := LOutString + ' ' + LTempString;
        LF17File.Add(LOutString);

        //Line 3
        LOutString  := '';
        LTempString := Trim(PadDouble(LIrrigationBlock.UpperZoneSoilMoistureCapacity));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.LowerZoneSoilMoistureCapacity));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.UpperZoneSoilMoistureTarget));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.InitialSoilMoistureStorage));
        LOutString  := LOutString + ' ' + LTempString;
        LF17File.Add(LOutString);

        //Line 4
        LOutString  := '';
        for LCount  := MinMonths to MaxMonths do
        begin
          LTempString := Trim(PadDouble(LIrrigationBlock.RainfallFactor[LCount]));
          LOutString  := LOutString + ' ' + LTempString;
        end;
        LF17File.Add(LOutString);

        //Line 5
        LOutString  := '';
        for LCount  := MinMonths to MaxMonths do
        begin
          LTempString := Trim(PadDouble(LIrrigationBlock.PanEvaporation[LCount]));
          LOutString  := LOutString + ' ' + LTempString;
        end;
        LF17File.Add(LOutString);

        //Line 6
        LOutString  := '';
        for LCount  := MinMonths to MaxMonths do
        begin
          LTempString := Trim(PadDouble(LIrrigationBlock.APanConvFactor[LCount]));
          LOutString  := LOutString + ' ' + LTempString;
        end;
        LF17File.Add(LOutString);

        //Line 7
        LOutString  := '';
        LTempString := IntToStr(LIrrigationBlock.NumberOfCropTypes.FData);
        LOutString  := LOutString + ' ' + LTempString;

        LTempString := IntToStr(LIrrigationBlock.CropWaterUseType.FData);
        LOutString  := LOutString + ' ' + LTempString;

        LF17File.Add(LOutString);

        //Line 8
        for LCount  := MinNoOfCrops to LIrrigationBlock.NumberOfCropTypes.FData do
        begin
          LOutString  := '';
          for LCount2  := MinMonths to MaxMonths do
          begin
            LTempString := Trim(PadDouble(LIrrigationBlock.WaterUsageFactor[LCount].MonthlyWaterUse[LCount2]));
            LOutString  := LOutString + ' ' + LTempString;
          end;
          LTempString := Trim(PadDouble(LIrrigationBlock.WaterUsageFactor[LCount].PercAreaUnderCropType));
          LOutString  := LOutString + ' ' + LTempString;

          if(LIrrigationBlock.WaterUsageFactor[LCount].CropName.FData <> '') then
          begin
            LTempString := QuotedStr(LIrrigationBlock.WaterUsageFactor[LCount].CropName.FData);
            LOutString  := LOutString + ' ' + LTempString;
          end;
          
          LF17File.Add(LOutString);
        end;

        //Line 9
        LOutString  := '';
        LTempString := Trim(PadDouble(LIrrigationBlock.RainAboveRainFactorSpecValue));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.RainBelowRainFactor));
        LOutString  := LOutString + ' ' + LTempString;
        LTempString := Trim(PadDouble(LIrrigationBlock.RainCatchmentScalingFactor));
        LOutString  := LOutString + ' ' + LTempString;
        LF17File.Add(LOutString);

        //Line 10
        LOutString  := '';
        LTempString := Trim(PadDouble(LIrrigationBlock.AllocatedIrrigationArea));
        LOutString  := LOutString + ' ' + LTempString;
        LF17File.Add(LOutString);
      end;
      
      for LLinesCount := 0 to LIrrigationBlockObject.ExtraLines.Count -1 do
        LF17File.Add(LIrrigationBlockObject.ExtraLines[LLinesCount]);

      LF17File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      if(Trim(LF17File.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile17Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LF17File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
