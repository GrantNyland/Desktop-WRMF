//
//
//  UNIT      : Contains TFile01Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UFile01Agent;

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
  URunParametersObject,
  UYieldModelDataObject;

type

  TFile01Agent = class(TAbstractFileAgent)
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
  VoaimsCom_TLB,
  UErrorHandlingOperations, UPathsObject;

function TFile01Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile01Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LSign,
  LTempString : String;
  LTemp,
  LStart,
  LReadInteger,
  LCount,
  LStartPos,
  LErrorCode : Integer;
  LReadReal : Double;
  LRunParameters: TRunParametersObject;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LLocalCount : integer;
  LDemandCentre:TDemandCentreObject;
  LInterBasinTransfer:TInterBasinTransferObject;
  LMinMaxBoundsObject : TMinMaxBoundsObject;
  LWQConstraintsObject : TWQConstraintsObject;
  LStop: boolean;
  LIndex: Integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile01Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile01Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LRunParameters := ADataObject.FRunParametersObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LRunParameters.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F01 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Check F01 file contains at leat 11 lines
      if ((FAppModules.StudyArea.ModelVersion = '6') AND (LFileData.Count < 11)) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strF01StructureErr');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;
      if ((FAppModules.StudyArea.ModelVersion <> '6') AND (LFileData.Count < 12)) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strF01StructureErrB');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      LStart := -1;
      //Title Line 1..3
      LReadString := Trim(LFileData[LStart + 1]);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      if(length(LReadString) > 80) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strTitleErr');
        LMessage := Format(LMessage,[1,1,80]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FTitle[1].FData :=LReadString;
        LRunParameters.FTitle[1].FInitalised := True;
      end;

      LReadString := Trim(LFileData[LStart + 2]);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(2,'2');
      if(length(LReadString) > 80) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strTitleErr');
        LMessage := Format(LMessage,[2,1,80]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FTitle[2].FData :=LReadString;
        LRunParameters.FTitle[2].FInitalised := True;
      end;

      LReadString := Trim(LFileData[LStart + 3]);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(3,'3');
      if(length(LReadString) > 80) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strTitleErr');
        LMessage := Format(LMessage,[3,1,80]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FTitle[3].FData :=LReadString;
        LRunParameters.FTitle[3].FInitalised := True;
      end;

      //line4 +++++++++++++++++++++++++
      LReadString := LFileData[LStart + 4];
      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
      LTempString:=GetSubstring(LReadString,1,6);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strTimePeriodsErr');
        LMessage := Format(LMessage,[4,1,6]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FTimePeriods.FData :=LReadInteger;
        LRunParameters.FTimePeriods.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,7,6);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strStartYearErr');
        LMessage := Format(LMessage,[4,7,12]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FStartGregorian.FData :=LReadInteger;
        LRunParameters.FStartGregorian.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,13,6);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strStartOtherErr');
        LMessage := Format(LMessage,[4,13,18]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FStartOther.FData :=LReadInteger;
        LRunParameters.FStartOther.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,19,6);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strStartDebugErr');
        LMessage := Format(LMessage,[4,19,24]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FStartDebug.FData :=LReadInteger;
        LRunParameters.FStartDebug.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,25,6);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strEndDebugErr');
        LMessage := Format(LMessage,[4,25,30]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FEndDebug.FData :=LReadInteger;
        LRunParameters.FEndDebug.FInitalised := True;
      end;

      LTemp      := -3;
      if(FAppModules.Model.ModelName = CPlanning) then
        LTemp      := -4;

      LTempString:=GetSubstring(LReadString,31,6);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strDebugLevelErr');
        LMessage := Format(LMessage,[4,31,36]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger < LTemp) or (LReadInteger > 7) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strDebugLevelValErr');
        LMessage := Format(LMessage,[4,31,36]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FDebugLevel.FData:=LReadInteger;
        LRunParameters.FDebugLevel.FInitalised:= True;
      end;

      if(FAppModules.Model.ModelName = CPlanning) then
      begin
        LTempString:=Trim(GetSubstring(LReadString,37,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strDetailedOptionErr');
          LMessage := Format(LMessage,[4,42,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FDetailedOption.FData:=LTempString[1];
          LRunParameters.FDetailedOption.FInitalised:= True;
        end;

        LTempString:=Trim(GetSubstring(LReadString,43,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strSupplyOptionErr');
          LMessage := Format(LMessage,[4,48,48]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FSupplyOption.FData:=LTempString[1];
          LRunParameters.FSupplyOption.FInitalised:= True;
        end;

        LTempString:=Trim(GetSubstring(LReadString,49,6));
        if(Uppercase(LTempString) <> 'Q') and (Uppercase(LTempString) <> 'D') and
          (Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strAnnualSummaryErr');
          LMessage := Format(LMessage,[4,54,54]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FAnnualSummary.FData:=LTempString[1];
          LRunParameters.FAnnualSummary.FInitalised:= True;
        end;

        LTempString:=Trim(GetSubstring(LReadString,55,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strEconomicOptionErr');
          LMessage := Format(LMessage,[4,60,60]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FEconomicOption.FData:=LTempString[1];
          LRunParameters.FEconomicOption.FInitalised:= True;
        end;

        LTempString:=Trim(GetSubstring(LReadString,61,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strPlanningSummaryErr');
          LMessage := Format(LMessage,[4,66,66]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FPlanningSummary.FData:=LTempString[1];
          LRunParameters.FPlanningSummary.FInitalised:= True;
        end;

        LTempString:=Trim(GetSubstring(LReadString,67,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strInputSummaryErr');
          LMessage := Format(LMessage,[4,72,72]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FInputSummary.FData:=LTempString[1];
          LRunParameters.FInputSummary.FInitalised:= True;
        end;

        LTempString:=Trim(GetSubstring(LReadString,73,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strInputSummaryErr');
          LMessage := Format(LMessage,[4,78,78]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FWaterQualityOption.FData:=LTempString[1];
          LRunParameters.FWaterQualityOption.FInitalised:= True;
        end;
      end
      else
      begin
        LTempString:=GetSubstring(LReadString,37,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strSummaryLevelErr');
          LMessage := Format(LMessage,[4,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger < 0) or (LReadInteger > 2) then
         begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strSummaryLevelValErr');
           LMessage := Format(LMessage,[4,37,42]);
           AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FSummaryLevel.FData:=LReadInteger;
          LRunParameters.FSummaryLevel.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,43,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strSummaryOutputErr');
          LMessage := Format(LMessage,[4,43,48]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
         begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strSummaryOutputValErr');
           LMessage := Format(LMessage,[4,43,48]);
           AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FSummaryOutput.FData:=LReadInteger;
          LRunParameters.FSummaryOutput.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,49,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strStoreYieldErr');
          LMessage := Format(LMessage,[4,49,54]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
         begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strStoreYieldValErr');
           LMessage := Format(LMessage,[4,49,54]);
           AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FStoreYield.FData:=LReadInteger;
          LRunParameters.FStoreYield.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,55,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strRandomNumberErr');
          LMessage := Format(LMessage,[4,55,60]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strRandomNumberValErr');
          LMessage := Format(LMessage,[4,55,60]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FRandomNumber.FData:=LReadInteger;
          LRunParameters.FRandomNumber.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,66,1);
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strPlotOptionErr');
          LMessage := Format(LMessage,[4,61,61]);
          AProgressFunction(LMessage,ptWarning,LStop);
         // if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FPlotOption.FData:=LTempString[1];
          LRunParameters.FPlotOption.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,67,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strLimitOptionErr');
          LMessage := Format(LMessage,[4,67,72]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
         begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strLimitOptionValErr');
           LMessage := Format(LMessage,[4,67,72]);
           AProgressFunction(LMessage,ptWarning,LStop);
           //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FLimitOption.FData:=LReadInteger;
          LRunParameters.FLimitOption.FInitalised:= True ;
        end;

        LTempString:=GetSubstring(LReadString,73,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strMultiplePeriodsErr');
          LMessage := Format(LMessage,[4,73,78]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
         begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strMultiplePeriodsValErr');
           LMessage := Format(LMessage,[4,73,78]);
           AProgressFunction(LMessage,ptWarning,LStop);
           //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FMultiplePeriods.FData:=LReadInteger;
          LRunParameters.FMultiplePeriods.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,79,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strCalcHistYieldErr');
          LMessage := Format(LMessage,[4,79,84]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if not (LReadInteger in [0,1,2]) then
        begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strCalcHistYieldValErr');
           LMessage := Format(LMessage,[4,79,84]);
           AProgressFunction(LMessage,ptWarning,LStop);
           //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FCalcHistYield.FData:=LReadInteger;
          LRunParameters.FCalcHistYield.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,85,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strReduceSequenceErr');
          LMessage := Format(LMessage,[4,85,90]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
         begin
           LMessage := FAppModules.Language.GetString('TFile01Agent.strReduceSequenceValErr');
           LMessage := Format(LMessage,[4,85,90]);
           AProgressFunction(LMessage,ptWarning,LStop);
           //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FReduceSequence.FData:=LReadInteger;
          LRunParameters.FReduceSequence.FInitalised:= True;
        end;
      end;

      //line5 +++++++++++++++++++++++++++++
      //Months
      LStartPos:=4;
      LReadString := LFileData[LStart + 5];

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(5,'5');

      for LCount := 1 to 12 do
      begin
        LTempString:=GetSubstring(LReadString,LStartPos,6);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strMonthNameErr');
          LMessage := Format(LMessage,[5,LStartPos,LStartPos+5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FMonthNames[LCount].FData:=Trim(LTempString);
          LRunParameters.FMonthNames[LCount].FInitalised:= True;
        end;
        Inc(LStartPos,6);
      end;

      //monthdays
      //line6 +++++++++++++++++++++++++++++
      LStartPos:=1;
      LReadString := LFileData[LStart + 6];
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(6,'6');
      for LCount := 1 to 12 do
      begin
        LTempString:=GetSubstring(LReadString,LStartPos,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strMonthDayErr');
          LMessage := Format(LMessage,[6,LStartPos,LStartPos+5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FMonthDays[LCount].FData:=LReadReal;
          LRunParameters.FMonthDays[LCount].FInitalised:= True;
        end;
        Inc(LStartPos,6);
      end;

      //line7 ++++++++++++++++++++++++++
      LReadString := LFileData[LStart + 7];
      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
      LTempString:=GetSubstring(LReadString,1,6);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile01Agent.strHydYearsErr');
        LMessage := Format(LMessage,[7,1,6]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LRunParameters.FHydYears.FData :=LReadInteger;
        LRunParameters.FHydYears.FInitalised:= True;
      end;

      if(FAppModules.Model.ModelName = CPlanning) then
      begin
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,7,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strEconomicTimePeriodsErr');
          LMessage := Format(LMessage,[7,7,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FEconomicTimePeriods.FData:=LReadInteger;
          LRunParameters.FEconomicTimePeriods.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,13,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strAnalysisStartYearErr');
          LMessage := Format(LMessage,[7,13,18]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FAnalysisStartYear.FData:=LReadInteger;
          LRunParameters.FAnalysisStartYear.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,19,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strMaxHydSequences2Err');
          LMessage := Format(LMessage,[7,19,24]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FMaxHydSequences.FData:=LReadInteger;
          LRunParameters.FMaxHydSequences.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,25,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strNumberOfDemandCentresErr');
          LMessage := Format(LMessage,[7,25,30]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FNumberOfDemandCentres.FData:=LReadInteger;
          LRunParameters.FNumberOfDemandCentres.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,31,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strNumberOfInterBasinChannelsErr');
          LMessage := Format(LMessage,[7,31,36]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FNumberOfInterBasinChannels.FData:=LReadInteger;
          LRunParameters.FNumberOfInterBasinChannels.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,37,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strStartMonth2Err');
          LMessage := Format(LMessage,[7,37,42]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FStartMonth.FData :=LReadInteger;
          LRunParameters.FStartMonth.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,43,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strMonthStartNewYearErr');
          LMessage := Format(LMessage,[7,43,48]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FMonthStartNewYear.FData :=LReadInteger;
          LRunParameters.FMonthStartNewYear.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,49,6);
        LTempString := Trim(LTempString)+ ' ';
        if(LTempString[1] <> 'H') and (LTempString[1] <> 'S') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistStochValErr');
          LMessage := Format(LMessage,[7,49,54]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FHistStoch.FData :=LTempString[1];
          LRunParameters.FHistStoch.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,55,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistRandErr');
          LMessage := Format(LMessage,[7,55,60]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger < 0) or (LReadInteger > 3) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistRandValErr');
          LMessage := Format(LMessage,[7,55,60]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FHistRand.FData :=LReadInteger;
          LRunParameters.FHistRand.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=Trim(GetSubstring(LReadString,61,6));
        if(Uppercase(LTempString) <> 'N') and (Uppercase(LTempString) <> 'P') and(Uppercase(LTempString) <> 'M') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strShortTermPlanningOptionErr');
          LMessage := Format(LMessage,[7,61,66]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FShortTermPlanningOption.FData:=LTempString[1];
          LRunParameters.FShortTermPlanningOption.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=Trim(GetSubstring(LReadString,67,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N')then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strPlotOptionErr');
          LMessage := Format(LMessage,[7,67,72]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FPlotOption.FData:=LTempString[1];
          LRunParameters.FPlotOption.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=GetSubstring(LReadString,73,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strRandomNumberErr');
          LMessage := Format(LMessage,[7,73,78]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger <> 0) and (LReadInteger <> 1) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strRandomNumberValErr');
          LMessage := Format(LMessage,[7,73,78]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FRandomNumber.FData:=LReadInteger;
          LRunParameters.FRandomNumber.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=Trim(GetSubstring(LReadString,79,6));
        if(Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N') and (Uppercase(LTempString) <> 'X')  then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHydroPowerOptionErr');
          LMessage := Format(LMessage,[7,79,84]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FHydroPowerOption.FData:=LTempString[1];
          LRunParameters.FHydroPowerOption.FInitalised:= True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=Trim(GetSubstring(LReadString,85,6));
        if(LTempString = '') then LTempString := 'N';
        if ((Uppercase(LTempString) <> 'I') and (Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N'))  then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strAllocationControlOptionErr');
          LMessage := Format(LMessage,[7,85,90]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FAllocationControlOption.FData:=LTempString[1];
          LRunParameters.FAllocationControlOption.FInitalised:= True;
        end;

         //line8++++++++++++++++++++++++++++
        LReadString := LFileData[LStart + 8];
        if LReadString <> '' then
        begin
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
          LTempString:=GetSubstring(LReadString,1,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strDecisionMonthsNumberErr');
            LMessage := Format(LMessage,[8,1,6]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LRunParameters.FDecisionMonthsNumber.FData:=LReadInteger;
            LRunParameters.FDecisionMonthsNumber.FInitalised:= True;
          end;

          LStartPos:=7;
          for LCount := 1 to LRunParameters.FDecisionMonthsNumber.FData do
          begin
            //Sequences to be analysed
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode = 0) then
            begin
              LRunParameters.FDecisionMonths[LCount].FData :=LReadInteger;
              LRunParameters.FDecisionMonths[LCount].FInitalised:= True;
            end;
            Inc(LStartPos,6);
          end;
        end;

        //line9++++++++++++++++++++++++++++
        LReadString := LFileData[LStart + 9];
        if LReadString <> '' then
        begin
          LStartPos:=7;
          LLocalCount := LFileLineTypesObject.LinesCount;
          for LCount := 1 to LRunParameters.FDecisionMonthsNumber.FData do
          begin
            //Sequences to be analysed
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'9');
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            if(Uppercase(LTempString) = 'M') or (Uppercase(LTempString) = 'R')  then
            begin
              LRunParameters.FDecisionMonthsType[LCount].FData :=LTempString[1];
              LRunParameters.FDecisionMonthsType[LCount].FInitalised:= True;
            end;
            Inc(LStartPos,6);
          end;
        end;

        //line10++++++++++++++++++++++++++++
        LStart := 9;
        if(UpperCase(LRunParameters.FHydroPowerOption.FData) = 'Y')  then
        begin
          LReadString := LFileData[LStart];
          LStart := LStart + 1;
          LStartPos:=7;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'10');
          for LCount := 1 to LRunParameters.FDecisionMonthsNumber.FData do
          begin
            //Sequences to be analysed
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            if(Length(LTempString) >= 1)  then
            begin
              LRunParameters.FHydroPowerDecision[LCount].FData :=LTempString[1];
              LRunParameters.FHydroPowerDecision[LCount].FInitalised:= True;
            end;
            Inc(LStartPos,6);
          end;
        end;

        //line11++++++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart := LStart + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
        LTempString := Trim(LReadString);
        if(Trim(LTempString) = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strParamNameErr');
          LMessage := Format(LMessage,[LStart+1,1,40]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if not FilePathIsDosCompatible(FAppModules,LTempString) then
        begin
          LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
          LMessage := Format(LMessage,[LTempString]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        //Check if file exists.
        If not FileExists(Trim(LTempString)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strParamNameValErr');
          LMessage := Format(LMessage,[LStart+1,1,40]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FParamName.FData    := ExtractFileName(LTempString);
          LRunParameters.FParamName.FLength  := Length(LRunParameters.FParamName.FData);
          LRunParameters.FParamName.FInitalised:= True;
          ADataObject.FPathsObject.HydrologyPath.FData := Trim(ExtractFilePath(LTempString));
          ADataObject.FPathsObject.HydrologyPath.FLength  := Length(ADataObject.FPathsObject.HydrologyPath.FData);
          ADataObject.FPathsObject.HydrologyPath.FInitalised := True;
        end;

        //line12++++++++++++++++++++++++++++
        LRunParameters.AddDemandCenters;
        for LCount := 1 to LRunParameters.FNumberOfDemandCentres.FData do
        begin
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'12');
          LDemandCentre := TDemandCentreObject(LRunParameters.FDemandCentresList[LCount-1]);
          LReadString := LFileData[LStart];
          LStart := LStart + 1;

          LTempString:=GetSubstring(LReadString,1,2);
          LTempString := Trim(LTempString);
          if (LTempString = '') or ((Uppercase(LTempString) <> 'D') and (Uppercase(LTempString) <> 'R'))  then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strDemandCenterChannelTypeErr');
            LMessage := Format(LMessage,[LStart,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LDemandCentre.FDemandCentreType.FData:=LTempString[1];
            LDemandCentre.FDemandCentreType.FInitalised:= True;
          end;

          LTempString:=GetSubstring(LReadString,3,4);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strDemandCenterChannelNumberErr');
            LMessage := Format(LMessage,[LStart,3,6]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LDemandCentre.FChannelNumber.FData :=LReadInteger;
            LDemandCentre.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,7,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strDemandCenterAnnualDemandErr');
            LMessage := Format(LMessage,[LStart,7,12]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LDemandCentre.FAnnualDemand.FData :=LReadReal;
            LDemandCentre.FAnnualDemand.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,13,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            if(LRunParameters.FShortTermPlanningOption.FData = 'N') then
            begin
              LMessage := FAppModules.Language.GetString('TFile01Agent.strDemandCenterMinimumDemandErr');
              LMessage := Format(LMessage,[LStart,13,18]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end
          else
          begin
            LDemandCentre.FMinimumDemand.FData :=LReadReal;
            LDemandCentre.FMinimumDemand.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,19,6);
          LTempString := Trim(LTempString);
          if (LTempString = '') or ((Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N'))  then
          begin
            {LMessage := FAppModules.Language.GetString('TFile01Agent.strDemandCenterOutputResultsErr');
            LMessage := Format(LMessage,[LStart,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;}
          end
          else
          begin
            LDemandCentre.FOutputResults.FData:=LTempString[1];
            LDemandCentre.FOutputResults.FInitalised:= True;
          end;

          LTempString := Copy(LReadString,25,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            LDemandCentre.FComment.FData := LTempString;
            LDemandCentre.FComment.FLength := Length(LTempString);
            LDemandCentre.FComment.FInitalised := True;
          end;

        end;

        //line13++++++++++++++++++++++++++++
        LRunParameters.AddInterBasinTransferTransfers;
        for LCount := 1 to LRunParameters.FNumberOfInterBasinChannels.FData do
        begin
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
          LInterBasinTransfer := TInterBasinTransferObject(LRunParameters.FInterBasinTransferList[LCount-1]);
          LReadString := LFileData[LStart];
          LStart := LStart + 1;

          LTempString:=GetSubstring(LReadString,2,1);
          LTempString := Trim(LTempString);
          if (LTempString = '') or ((Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N'))  then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strInterBasinSummaryRequiredErr');
            LMessage := Format(LMessage,[LStart,2,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LInterBasinTransfer.FSummaryRequired.FData:=LTempString[1];
            LInterBasinTransfer.FSummaryRequired.FInitalised:= True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
          LTempString:=GetSubstring(LReadString,3,4);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strInterBasinChannelNumberErr');
            LMessage := Format(LMessage,[LStart,3,6]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LInterBasinTransfer.FChannelNumber.FData :=LReadInteger;
            LInterBasinTransfer.FChannelNumber.FInitalised := True;
          end;
          if Pos('.',LReadString) = 11 then
            LTempString:=GetSubstring(LReadString,7,7)
          else
            LTempString:=GetSubstring(LReadString,7,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strInterBasinUpperTransferLimitErr');
            LMessage := Format(LMessage,[LStart,7,12]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LInterBasinTransfer.FUpperTransferLimit.FData :=LReadReal;
            LInterBasinTransfer.FUpperTransferLimit.FInitalised := True;
          end;

          //if Pos('.',LReadString) = 11 then
          //  LTempString :=GetSubstring(LReadString,14,6)
          //else
            LTempString:=GetSubstring(LReadString,13,6);
          //end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            {LMessage := FAppModules.Language.GetString('TFile01Agent.strInterBasinemandCentreNumberErr');
            LMessage := Format(LMessage,[LStart,13,18]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;}
          end
          else
          begin
            LInterBasinTransfer.FDemandCentreNumber.FData :=LReadInteger;
            LInterBasinTransfer.FDemandCentreNumber.FInitalised := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
          LTempString := Copy(LReadString,19,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            LInterBasinTransfer.FComment.FData := LTempString;
            LInterBasinTransfer.FComment.FLength := Length(LTempString);
            LInterBasinTransfer.FComment.FInitalised := True;
          end;
        end;

        //line14++++++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart := LStart + 1;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'14');
        LTempString:=GetSubstring(LReadString,1,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strNumberOfDiscountRatesErr');
          LMessage := Format(LMessage,[8,1,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FNumberOfDiscountRates.FData:=LReadInteger;
          LRunParameters.FNumberOfDiscountRates.FInitalised:= True;
        end;

        LStartPos:=7;
        for LCount := 1 to LRunParameters.FNumberOfDiscountRates.FData do
        begin
          //Actual discount rates
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'14');
          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LRunParameters.FDiscountRates[LCount].FData :=LReadReal;
            LRunParameters.FDiscountRates[LCount].FInitalised:= True;
          end;
          Inc(LStartPos,6);
        end;

        //line15++++++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart := LStart + 1;
        LStartPos:=1;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'15');
        for LCount := 1 to 10 do
        begin
          //Sequences to be analysed
          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LRunParameters.FAnalSequences[LCount].FData :=LReadInteger;
            LRunParameters.FAnalSequences[LCount].FInitalised:= True;
          end;
          Inc(LStartPos,6);
        end;


        //line16++++++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart := LStart + 1;
        LStartPos:=1;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'16');
        for LCount := 1 to 5 do
        begin

          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LRunParameters.FHistManualAnalysis[LCount].FData :=LReadInteger;
            LRunParameters.FHistManualAnalysis[LCount].FInitalised := True;
          end;
          Inc(LStartPos,6);
        end;

        //line17++++++++++++++++++++++++++++

        LReadString := LFileData[LStart];
        LStartPos:=1;
        LStart := LStart + 1;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'17');
        for LCount := 1 to 5 do
        begin

          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LRunParameters.FStocManualAnalysis[LCount].FData :=LReadInteger;
            LRunParameters.FStocManualAnalysis[LCount].FInitalised := True;
          end;
          Inc(LStartPos,6);
        end;

          //line18++++++++++++++++++++++++++++
        if (LRunParameters.FWaterQualityOption.FInitalised) and (LRunParameters.FWaterQualityOption.FData = 'Y') then
        begin
          LReadString := LFileData[LStart];
          LStartPos:=1;
          LStart := LStart + 1;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'18');
          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LRunParameters.FNoOfFlowsUpperBounds.FData :=LReadInteger;
            LRunParameters.FNoOfFlowsUpperBounds.FInitalised := True;
          end;

          if LRunParameters.FNoOfFlowsUpperBounds.FInitalised then
          begin

            for LIndex := 1 to LRunParameters.FNoOfFlowsUpperBounds.FData do
            begin
              LMinMaxBoundsObject := LRunParameters.AddMinMaxBoundsObject;
              LMinMaxBoundsObject.Initialise;
              LReadString := LFileData[LStart];
              LStartPos:=1;
              LStart := LStart + 1;
              LLocalCount := LFileLineTypesObject.LinesCount;
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'18a');
              LTempString:=GetSubstring(LReadString,LStartPos,6);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LMinMaxBoundsObject.FMinMaxBoundedChannel.FData :=LReadInteger;
                LMinMaxBoundsObject.FMinMaxBoundedChannel.FInitalised := True;
              end;
              Inc(LStartPos,6);

              LTempString:=GetSubstring(LReadString,LStartPos,6);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LMinMaxBoundsObject.FNoOfRefChannels.FData :=LReadInteger;
                LMinMaxBoundsObject.FNoOfRefChannels.FInitalised := True;
              end;

              if LMinMaxBoundsObject.FNoOfRefChannels.FInitalised  then
              begin
                for LCount := 1 to LMinMaxBoundsObject.FNoOfRefChannels.FData do
                begin
                  LReadString := LFileData[LStart];
                  LStartPos:=1;
                  LStart := LStart + 1;
                  LLocalCount := LFileLineTypesObject.LinesCount;
                  TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'18b');
                  LTempString:=GetSubstring(LReadString,LStartPos,6);
                  LTempString := Trim(LTempString);
                  LMinMaxBoundsObject.FRefChannels.Add(LTempString);
                end;

              end;
            end;
          end;

          //line19++++++++++++++++++++++++++++
          LReadString := LFileData[LStart];
          LStartPos:=1;
          LStart := LStart + 1;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19');
          LTempString:=GetSubstring(LReadString,LStartPos,6);
          LTempString := Trim(LTempString);
           Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LRunParameters.FNoOfWQConstraints.FData :=LReadInteger;
            LRunParameters.FNoOfWQConstraints.FInitalised := True;
          end;

          if LRunParameters.FNoOfWQConstraints.FInitalised then
          begin
            for LIndex := 1 to LRunParameters.FNoOfWQConstraints.FData do
            begin
              //=========
              LWQConstraintsObject := LRunParameters.AddWQConstraintsObject;
              LWQConstraintsObject.Initialise;
              LReadString := LFileData[LStart];
              LStartPos:=1;
              LStart := LStart + 1;
              LLocalCount := LFileLineTypesObject.LinesCount;
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19a');
              LTempString:=GetSubstring(LReadString,LStartPos,6);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LWQConstraintsObject.FWQConMinMaxChannel.FData :=LReadInteger;
                LWQConstraintsObject.FWQConMinMaxChannel.FInitalised := True;
              end;
              Inc(LStartPos,6);

              LTempString:=GetSubstring(LReadString,LStartPos,6);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LWQConstraintsObject.FWQConTarget.FData :=LReadReal;
                LWQConstraintsObject.FWQConTarget.FInitalised := True;
              end;
               Inc(LStartPos,6);

              LTempString:=GetSubstring(LReadString,LStartPos,4);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LWQConstraintsObject.FNoOfRefChannelsBlending.FData :=LReadInteger;
                LWQConstraintsObject.FNoOfRefChannelsBlending.FInitalised := True;
              end;
               Inc(LStartPos,4);

              LTempString:=GetSubstring(LReadString,LStartPos,4);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LWQConstraintsObject.FReservoirRef.FData :=LReadInteger;
                LWQConstraintsObject.FReservoirRef.FInitalised := True;
              end;
               Inc(LStartPos,4);
              LTempString:=GetSubstring(LReadString,LStartPos,4);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LWQConstraintsObject.FWQConType.FData :=LReadInteger;
                LWQConstraintsObject.FWQConType.FInitalised := True;
              end;

              if LWQConstraintsObject.FNoOfRefChannelsBlending.FInitalised  then
              begin
                for LCount := 1 to LWQConstraintsObject.FNoOfRefChannelsBlending.FData do
                begin
                  LReadString := LFileData[LStart];
                  LStartPos:=1;
                  LStart := LStart + 1;
                  LLocalCount := LFileLineTypesObject.LinesCount;
                  TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19b');
                  LTempString:=GetSubstring(LReadString,LStartPos,6);
                  LTempString := Trim(LTempString);
                  Val(LTempString,LReadInteger,LErrorCode);
                  if(LErrorCode = 0) then
                    LWQConstraintsObject.FReferenceChannel.Add(IntToStr(LReadInteger));

                  Inc(LStartPos,6);
                  LTempString:=GetSubstring(LReadString,LStartPos,4);
                  LTempString := Trim(LTempString);
                  LSign := ' ';
                  if LTempString <> '' then
                    LSign := LTempString;
                  Inc(LStartPos,4);
                  LTempString:=GetSubstring(LReadString,LStartPos,6);
                  if(LErrorCode = 0) then
                    LWQConstraintsObject.FRefChannelFactor.Add(LSign+LTempString);

                end;
              end;

              if LWQConstraintsObject.FWQConType.FInitalised then
              begin
                if LWQConstraintsObject.FWQConType.FData = 2 then
                begin
                  LReadString := LFileData[LStart];
                  LStartPos:=1;
                  LStart := LStart + 1;
                  LLocalCount := LFileLineTypesObject.LinesCount;
                  TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19c');
                  LTempString:=GetSubstring(LReadString,LStartPos,6);
                  LTempString := Trim(LTempString);
                  Val(LTempString,LReadInteger,LErrorCode);
                  if(LErrorCode = 0) then
                  begin
                    LWQConstraintsObject.FSlopeLimit.FData  := LReadInteger;
                    LWQConstraintsObject.FSlopeLimit.FInitalised := True;
                  end;
                  LReadString := LFileData[LStart];
                  LStartPos:=1;
                  LStart := LStart + 1;
                  for LCount := 1 to 10 do
                  begin

                    LLocalCount := LFileLineTypesObject.LinesCount;
                    TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19d');
                    LTempString:=GetSubstring(LReadString,LStartPos,6);
                    LTempString := Trim(LTempString);
                    LWQConstraintsObject.FEstimatedRelease.Add(LTempString);
                  end;
                  LReadString := LFileData[LStart];
                  LStartPos:=1;
                  LStart := LStart + 1;
                  for LCount := 1 to 10 do
                  begin
                    LTempString:=GetSubstring(LReadString,LStartPos,6);
                    LTempString := Trim(LTempString);
                    LWQConstraintsObject.FConcentration.Add(LTempString);
                  end;

                end;

              end;

              //=======
            end;

          end;

        end;
        for LCount := LStart  to LFileData.Count - 1 do
          LRunParameters.FF01ExtraLines.Add(LFileData[LCount]);
      end
      else
      begin
        // Read Yield model data
        LTempString:=GetSubstring(LReadString,7,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strMaxHydSequencesErr');
          LMessage := Format(LMessage,[7,7,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FMaxHydSequences.FData:=LReadInteger;
          LRunParameters.FMaxHydSequences.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,13,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strNoLoadCasesErr');
          LMessage := Format(LMessage,[7,13,18]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FNoLoadCases.FData :=LReadInteger;
          LRunParameters.FNoLoadCases.FInitalised:= True;

          for LCount := 1 to LRunParameters.FNoLoadCases.FData do
          begin
            LRunParameters.FTargetYield[LCount].FData       := 0.0;
            LRunParameters.FTargetYield[LCount].FInitalised := True;
            LRunParameters.FMaxYield[LCount].FData          := 0.0;
            LRunParameters.FMaxYield[LCount].FInitalised    := True;
            LRunParameters.FTargetPower[LCount].FData       := 0.0;
            LRunParameters.FTargetPower[LCount].FInitalised := True;
          end;
        end;

        LTempString:=GetSubstring(LReadString,19,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strStartMonthErr');
          LMessage := Format(LMessage,[7,13,24]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FStartMonth.FData :=LReadInteger;
          LRunParameters.FStartMonth.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,25,6);
        LTempString := Trim(LTempString)+' ';
        if(LTempString = ' ') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistStochErr');
          LMessage := Format(LMessage,[7,25,30]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LTempString[1] <> 'H') and (LTempString[1] <> 'S') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistStochValErr');
          LMessage := Format(LMessage,[7,25,25]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FHistStoch.FData :=LTempString[1];
          LRunParameters.FHistStoch.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,31,6);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistRandErr');
          LMessage := Format(LMessage,[7,31,36]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LReadInteger < 0) or (LReadInteger > 3) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strHistRandValErr');
          LMessage := Format(LMessage,[7,31,36]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FHistRand.FData :=LReadInteger;
          LRunParameters.FHistRand.FInitalised:= True;
        end;

        LTempString:=GetSubstring(LReadString,37,42);
        if(Trim(LTempString) = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strParamNameErr');
          LMessage := Format(LMessage,[7,37,77]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if not FilePathIsDosCompatible(FAppModules,Trim(LTempString)) then
        begin
          LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
          LMessage := Format(LMessage,[Trim(LTempString)]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        //Check if file exists.
        If not FileExists(Trim(LTempString)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strParamNameValErr');
          LMessage := Format(LMessage,[7,37,77]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LRunParameters.FParamName.FData    := ExtractFileName(LTempString);
          LRunParameters.FParamName.FLength  := Length(LRunParameters.FParamName.FData);
          LRunParameters.FParamName.FInitalised:= True;
          ADataObject.FPathsObject.HydrologyPath.FData := Trim(ExtractFilePath(LTempString));
          ADataObject.FPathsObject.HydrologyPath.FLength  := Length(ADataObject.FPathsObject.HydrologyPath.FData);
          ADataObject.FPathsObject.HydrologyPath.FInitalised := True;
        end;

         //line8++++++++++++++++++++++++++++
        LReadString := LFileData[LStart + 8];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(8,'8');
        if LReadString <> '' then
        begin
          LStartPos:=1;
          for LCount := 1 to 10 do
          begin
          //Sequences to be analysed
            LTempString:=GetSubstring(LReadString,LStartPos,6);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode = 0) then
            begin
              LRunParameters.FAnalSequences[LCount].FData :=LReadInteger;
              LRunParameters.FAnalSequences[LCount].FInitalised:= True;
            end;
            Inc(LStartPos,6);
          end;
        end;

        //line9++++++++++++++++++++++++++++
        LReadString := LFileData[LStart + 9];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(9,'9');
        LCount    := 1;
        LStartPos := 1;

  //      while ((LCount <= 10) AND (LStartPos < Length(LReadString))) do
        while ((LCount <= 10) AND (Trim(LReadString) <> '')) do
        begin
          //Target system yield
          LTempString := Trim(GetSubstring(LReadString, 1, 6));
          LReadString := Copy(LReadString, 7, Length(LReadString)-6);
  //        LTempString := GetSubstring(LReadString, LStartPos, 6);
  //        LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if (LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strTargetYieldErr');
            LMessage := Format(LMessage,[9,LStartPos,LStartPos+5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LRunParameters.FTargetYield[LCount].FData       := LReadReal;
            LRunParameters.FTargetYield[LCount].FInitalised := True;
            LRunParameters.FMaxYield[LCount].FData          := 0.0;
            LRunParameters.FMaxYield[LCount].FInitalised    := True;
            LRunParameters.FTargetPower[LCount].FData       := 0.0;
            LRunParameters.FTargetPower[LCount].FInitalised := True;
          end;
          Inc(LStartPos,6);
          LCount := LCount + 1;
        end;
  //      if(Trim(Copy(LReadString,LStartPos,Length(LReadString))) <> '') then
        if (Trim(LReadString) <> '')  and (LCount < 10)then
        begin
          LMessage := FAppModules.Language.GetString('TFile01Agent.strTargetYieldNumberErr');
          LMessage := Format(LMessage,[LStart + 9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;

        //line10+++++++++++++++++++++++++++
        LReadString := LFileData[LStart + 10];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(10,'10');
        LStartPos := 1;
        LCount    := 1;
  //      while ((LCount <= 10) AND (LStartPos < Length(LReadString))) do
        while ((LCount <= 10) AND (Trim(LReadString) <> '')) do
        begin
          //Maximum yield
          LTempString := Trim(GetSubstring(LReadString, 1, 6));
          LReadString := Copy(LReadString, 7, Length(LReadString)-6);
  //        LTempString := GetSubstring(LReadString,LStartPos,6);
  //        LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if (LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile01Agent.strMaxYieldErr');
            LMessage := Format(LMessage,[10,LStartPos,LStartPos+5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
         begin
            LRunParameters.FMaxYield[LCount].FData :=LReadReal;
            LRunParameters.FMaxYield[LCount].FInitalised:= True;
          end;
          Inc(LStartPos,6);

          if LRunParameters.FTargetYield[LCount].FData > LRunParameters.FMaxYield[LCount].FData then
          begin
            LMessage := FAppModules.Language.GetString('TDataFileObjects.strTargetSystemYield');
            AProgressFunction(LMessage,ptWarning,LStop);
          end;

          LCount := LCount + 1;
        end;

        //line11+++++++++++++++++++++++++++
        LReadString := LFileData[LStart + 11];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(11,'11');

        if (Trim(LReadString) <> '') then
        begin
  //        LStartPos := 1;
          LCount    := 1;
  //        while ((LCount <= 10) AND (LStartPos < Length(LReadString))) do
          while ((LCount <= 10) AND (Trim(LReadString) <> '')) do
          begin
          //Target system power demand
            LTempString := Trim(GetSubstring(LReadString, 1, 6));
            LReadString := Copy(LReadString, 7, Length(LReadString)-6);
  //          LTempString := GetSubstring(LReadString,LStartPos,6);
  //          LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode = 0) then
            begin
              LRunParameters.FTargetPower[LCount].FData := LReadReal;
              LRunParameters.FTargetPower[LCount].FInitalised:= True;
            end;
  //          Inc(LStartPos,6);
            LCount := LCount + 1;
          end;
        end
        else
        begin
          for LCount := 1 to LRunParameters.FNoLoadCases.FData do
          begin
            LRunParameters.FTargetPower[LCount].FData := 0;
            LRunParameters.FTargetPower[LCount].FInitalised:= True;
          end;
        end;

        //line12+++++++++++++++++++++++++++
        if FAppModules.FieldProperties.FieldAvailableInModel('TargetRecurrenceInterval') AND
           (LFileData.Count >= 12) then
        begin
          if(LRunParameters.FCalcHistYield.FData = 2) then
          begin
            LReadString := LFileData[LStart + 12];
            if (Trim(LReadString) <> '') then
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(12,'12');
              LTempString := GetSubstring(LReadString,1,6);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile01Agent.strTargetRecurrenceInterval');
                LMessage := Format(LMessage,[12,1,6]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LRunParameters.FTargetRecurrenceInterval.FData := LReadInteger;
                LRunParameters.FTargetRecurrenceInterval.FInitalised := True;
              end;
            end;
          end;
        end;

        for LCount := LStart + 12 to LFileData.Count - 1 do
          LRunParameters.FF01ExtraLines.Add(LFileData[LCount]);
      end;

      LMessage := FAppModules.Language.GetString('TFile01Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile01Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile01Agent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LCount         : Integer;
  LF01File       : TStringlist;
  LRunParameters : TRunParametersObject;
  LStop          : boolean;
  lRunConfig     : IRunConfigurationData;
  LNrOfLoadCases : integer;
  LDemandCentre:TDemandCentreObject;
  LInterBasinTransfer:TInterBasinTransferObject;
  LMinMaxBoundsObject : TMinMaxBoundsObject;
  LWQConstraintsObject : TWQConstraintsObject;
  LIndex : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile01Agent.strWritingStarted');
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

    LRunParameters := ADataObject.FRunParametersObject;

    LF01File:= TStringList.Create;
    try

      //Title
      LF01File.Add(LRunParameters.FTitle[1].FData);
      LF01File.Add(LRunParameters.FTitle[2].FData);
      LF01File.Add(LRunParameters.FTitle[3].FData);

      //line4 +++++++++++++++++++++++++
      //ErrorStr:='F01 Data File - General data'; //Message to help the user locate the errors
      LOutString:='';
      LTempString:=PadInt(LRunParameters.FTimePeriods);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LRunParameters.FStartGregorian);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LRunParameters.FStartOther);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LRunParameters.FStartDebug);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LRunParameters.FEndDebug);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LRunParameters.FDebugLevel);
      LOutString:=LOutString+LTempString;

      if(FAppModules.Model.ModelName = CPlanning) then
      begin
        LTempString:= PadChar(LRunParameters.FDetailedOption);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FSupplyOption);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FAnnualSummary);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FEconomicOption);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FPlanningSummary);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FInputSummary);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FWaterQualityOption);
        LOutString:=LOutString+LTempString;
        LF01File.Add(LOutString);

        //line5 +++++++++++++++++++++++++++++
        //Months
        LOutString:='';
        for LCount := 1 to 12 do
        begin
          LTempString:= PadString(LRunParameters.FMonthNames[LCount]);
          LOutString:=LOutString + LTempString;
        end;
        LF01File.Add(LOutString);

        //monthdays
        //line6 +++++++++++++++++++++++++++++
        LOutString:='';
        for LCount := 1 to 12 do
        begin
          LTempString:=PadDouble(LRunParameters.FMonthDays[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);

        //line7 ++++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LRunParameters.FHydYears);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FEconomicTimePeriods);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FAnalysisStartYear);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FMaxHydSequences);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FNumberOfDemandCentres);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FNumberOfInterBasinChannels);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FStartMonth);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FMonthStartNewYear);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FHistStoch);
        LOutString:=LOutString+LTempString;

        LTempString:= PadInt(LRunParameters.FHistRand);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FShortTermPlanningOption);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FPlotOption);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FRandomNumber);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FHydroPowerOption);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FAllocationControlOption);
        LOutString:=LOutString+LTempString;
        LF01File.Add(LOutString);

        //line8 ++++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LRunParameters.FDecisionMonthsNumber);
        LOutString:=LOutString+LTempString;

        for LCount := 1 to LRunParameters.FDecisionMonthsNumber.FData do
        begin
          LTempString:=PadInt(LRunParameters.FDecisionMonths[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);

        //line9 ++++++++++++++++++++++++++
        LOutString:='      ';
        for LCount := 1 to LRunParameters.FDecisionMonthsNumber.FData do
        begin
          LTempString:=PadChar(LRunParameters.FDecisionMonthsType[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);

        //line10 ++++++++++++++++++++++++++
        if(UpperCase(LRunParameters.FHydroPowerOption.FData) = 'Y')  then
        begin
          LOutString:='      ';
          for LCount := 1 to LRunParameters.FDecisionMonthsNumber.FData do
          begin
            LTempString:=PadChar(LRunParameters.FHydroPowerDecision[LCount]);
            LOutString:=LOutString+LTempString;
          end;
          LF01File.Add(LOutString);
        end;

        //line11 ++++++++++++++++++++++++++
        LOutString:='';
        LTempString:= PadString(LRunParameters.FParamName);
        LOutString:=LOutString+LTempString;
        LF01File.Add(LOutString);

        //line12 ++++++++++++++++++++++++++
        for LCount := 0 to LRunParameters.FDemandCentresList.Count-1 do
        begin
          LOutString:='';
          LDemandCentre := TDemandCentreObject(LRunParameters.FDemandCentresList[LCount]);

          LTempString:=PadChar(LDemandCentre.FDemandCentreType);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LDemandCentre.FChannelNumber);
          LOutString:=LOutString+LTempString;

          LTempString:=PadDouble(LDemandCentre.FAnnualDemand);
          LOutString:=LOutString+LTempString;

          LTempString:=PadDouble(LDemandCentre.FMinimumDemand);
          LOutString:=LOutString+LTempString;

          LTempString:=PadChar(LDemandCentre.FOutputResults);
          LOutString:=LOutString+LTempString;

          if LDemandCentre.FComment.FInitalised then
          begin
            LTempString:=PadString(LDemandCentre.FComment);
            LOutString:=LOutString+LTempString;
          end;

          LF01File.Add(LOutString);
        end;

        //line13 ++++++++++++++++++++++++++
        for LCount := 0 to LRunParameters.FInterBasinTransferList.Count -1 do
        begin
          LOutString:='';
          LInterBasinTransfer := TInterBasinTransferObject(LRunParameters.FInterBasinTransferList[LCount]);

          LTempString:=PadChar(LInterBasinTransfer.FSummaryRequired);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LInterBasinTransfer.FChannelNumber);
          LOutString:=LOutString+LTempString;

          LTempString:=PadDouble(LInterBasinTransfer.FUpperTransferLimit);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LInterBasinTransfer.FDemandCentreNumber);
          LOutString:=LOutString+LTempString;

          LOutString:=LOutString+LInterBasinTransfer.FComment.FData;

          LF01File.Add(LOutString);
        end;

        //line14 ++++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LRunParameters.FNumberOfDiscountRates);
        LOutString:=LOutString+LTempString;

        for LCount := 1 to LRunParameters.FNumberOfDiscountRates.FData do
        begin
          LTempString:=PadDouble(LRunParameters.FDiscountRates[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);
        //line15 ++++++++++++++++++++++++++
        LOutString:='';
        for LCount := 1 to 10 do
        begin
          LTempString:=PadInt(LRunParameters.FAnalSequences[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);
        //line16 ++++++++++++++++++++++++++
        LOutString:='';
        for LCount := 1 to 5 do
        begin
          LTempString:=PadInt(LRunParameters.FHistManualAnalysis[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);
        //Line17 +++++++++++++++++++++++++++
        LOutString:='';
        for LCount := 1 to 5 do
        begin
          LTempString:=PadInt(LRunParameters.FStocManualAnalysis[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);
        LOutString:='';
        if (LRunParameters.FWaterQualityOption.FInitalised) and (LRunParameters.FWaterQualityOption.FData = 'Y') then
        begin
          //Line18+++++++++++++++++++++++++++
          LTempString:=PadInt(LRunParameters.FNoOfFlowsUpperBounds);
          LOutString:=LOutString+LTempString;
          LF01File.Add(LOutString);
          LOutString:='';
          for LCount := 0 to LRunParameters.FMinMaxUpperBoundList.Count-1 do
          begin
            LMinMaxBoundsObject := TMinMaxBoundsObject(LRunParameters.FMinMaxUpperBoundList[LCount]);
            LTempString:=PadInt(LMinMaxBoundsObject.FMinMaxBoundedChannel);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LMinMaxBoundsObject.FNoOfRefChannels);
            LOutString:=LOutString+LTempString;
            LF01File.Add(LOutString);
            LOutString:='';
            for LIndex := 0 to LMinMaxBoundsObject.FRefChannels.Count-1 do
            begin
              LTempString:= Format('%6s',[LMinMaxBoundsObject.FRefChannels[LIndex]]);
              LOutString:=LOutString+LTempString;
              LF01File.Add(LOutString);
              LOutString:='';
            end;
          end;
          //Line19+++++++++++++++++++++++++++
          LTempString:=PadInt(LRunParameters.FNoOfWQConstraints);
          LOutString:=LOutString+LTempString;
          LF01File.Add(LOutString);
          LOutString:='';
          //Line19b+++++++++++++++++++++++++++

          for LCount := 0 to LRunParameters.FWQConstraintsList.Count-1 do
          begin
            LWQConstraintsObject := TWQConstraintsObject(LRunParameters.FWQConstraintsList[LCount]);
            LTempString:=PadInt(LWQConstraintsObject.FWQConMinMaxChannel);
            LOutString:=LOutString+LTempString;

            LTempString:=PadDouble(LWQConstraintsObject.FWQConTarget);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LWQConstraintsObject.FNoOfRefChannelsBlending);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LWQConstraintsObject.FReservoirRef);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LWQConstraintsObject.FWQConType);
            LOutString:=LOutString+LTempString;

            LF01File.Add(LOutString);
            LOutString:='';

            for LIndex := 0 to LWQConstraintsObject.FReferenceChannel.Count-1 do
            begin
              LTempString:= Format('%6s',[LWQConstraintsObject.FReferenceChannel[LIndex]]);
              LOutString:=LOutString+LTempString;

              LTempString:=Format('%8s',[LWQConstraintsObject.FRefChannelFactor[LIndex]]);
              LOutString:=LOutString+LTempString;

              LF01File.Add(LOutString);
              LOutString:='';
            end;

            if (LWQConstraintsObject.FWQConType.FData = 2) then
            begin
              LTempString:=PadInt(LWQConstraintsObject.FSlopeLimit);
              LOutString:=LOutString+LTempString;
              LF01File.Add(LOutString);
              LOutString:='';
              for LIndex := 0 to 9 do
              begin
                LTempString:=LWQConstraintsObject.FEstimatedRelease[LIndex];
                LOutString:=LOutString+' '+LTempString;

              end;
              LF01File.Add(LOutString);
              LOutString:='';
              for LIndex := 0 to 9 do
              begin
                LTempString:=LWQConstraintsObject.FConcentration[LIndex];
                LOutString:=LOutString+' '+LTempString;
              end;
              LF01File.Add(LOutString);
              LOutString:='';

            end;
          end;
        end;
      end
      else
      begin
        LTempString:=PadInt(LRunParameters.FSummaryLevel);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FSummaryOutput);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FStoreYield);
        LOutString:=LOutString+LTempString;

        LTempString:= PadInt(LRunParameters.FRandomNumber);
        LOutString:=LOutString+LTempString;

        LTempString:= PadChar(LRunParameters.FPlotOption);
        LOutString:=LOutString+LTempString;

        if LRunParameters.FLimitOption.FInitalised then
        begin
        end;
          LTempString:= PadInt(LRunParameters.FLimitOption) ;
          LOutString:=LOutString+LTempString;

        if LRunParameters.FMultiplePeriods.FInitalised then
        begin
          LTempString:=PadInt(LRunParameters.FMultiplePeriods);
          LOutString:=LOutString+LTempString;
        end;

        if LRunParameters.FCalcHistYield.FInitalised then
        begin
          LTempString:= PadInt(LRunParameters.FCalcHistYield);
          LOutString:=LOutString+LTempString;
        end;

        if LRunParameters.FReduceSequence.FInitalised then
        begin
          LTempString:= PadInt(LRunParameters.FReduceSequence);
          LOutString:=LOutString+LTempString;
        end;

        LF01File.Add(LOutString);

        //line5 +++++++++++++++++++++++++++++
        //Months
        LOutString:='';
        for LCount := 1 to 12 do
        begin
          LTempString:= PadString(LRunParameters.FMonthNames[LCount]);
          LOutString:=LOutString + LTempString;
        end;
        LF01File.Add(LOutString);

        //monthdays
        //line6 +++++++++++++++++++++++++++++
        LOutString:='';
        for LCount := 1 to 12 do
        begin
          LTempString:=PadDouble(LRunParameters.FMonthDays[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LF01File.Add(LOutString);

        //line7 ++++++++++++++++++++++++++

        LOutString:='';
        LTempString:=PadInt(LRunParameters.FHydYears);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FMaxHydSequences);
        LOutString:=LOutString+LTempString;

        lRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
        if(lRunConfig.CalculateHistoricFirmYield = 1) then
          LNrOfLoadCases := 2
        else
          LNrOfLoadCases := lRunConfig.NrOfActiveLoadCases;
        LTempString:= IntToStr(LNrOfLoadCases);
        while (Length(LTempString) < 6) do
          LTempString := ' '+LTempString;
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FStartMonth);
        LOutString:=LOutString+LTempString;

        LTempString:=PadChar(LRunParameters.FHistStoch);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LRunParameters.FHistRand);
        LOutString:=LOutString+LTempString;

        LTempString:= '    '+ PadString(LRunParameters.FParamName);
        LOutString:=LOutString+LTempString;
        LF01File.Add(LOutString);

        //line8++++++++++++++++++++++++++++
        LOutString:='';
        LTempString := '';
        for LCount := 1 to 10 do
        begin
          //Sequences to be analysed
          if(LRunParameters.FAnalSequences[LCount].FInitalised) then
          begin
            LTempString:=LTempString +
                         PadInt(LRunParameters.FAnalSequences[LCount]);
          end;
        end;
        LOutString:=LOutString+LTempString;
        LF01File.Add(LOutString);

        //line9++++++++++++++++++++++++++++
        LOutString:='';
        LTempString:= '';
  {      lCaseCount := 0;
        LCount     := 1;
        while (lCount < 10) do
        begin
          if (LRunParameters.FTargetYield[LCount].FInitalised OR
              LRunParameters.FMaxYield[LCount].FInitalised    OR
              LRunParameters.FTargetPower[LCount].FInitalised) then
            lCaseCount := LCount
          else
            Break;
          LCount := LCount + 1;
        end;
        if(LCaseCount > LRunParameters.FNoLoadCases.FData) then
          LCaseCount  := LRunParameters.FNoLoadCases.FData;
  }
        // Target system yield
  {      for LCount := 1 to lCaseCount do
        begin
          if (LRunParameters.FTargetYield[LCount].FInitalised) then
            LTempString := LTempString + PadDouble(LRunParameters.FTargetYield[LCount])
          else
            LTempString := LTempString + '   0.0';
        end;
        LOutString:=LOutString+LTempString;}
        //lLoadCaseCount := lRunConfig.NrOfActiveLoadCases;

        for LCount := 1 to lRunConfig.NrOfActiveLoadCases do
        begin
          if (lRunConfig.TargetYieldByIndex[LCount] = NullFloat) then
            LTempString := SmartFloatFormat(0.0,6,0)
          else
            LTempString := SmartFloatFormat(lRunConfig.TargetYieldByIndex[LCount],6,0);

          {if (lRunConfig.TargetYieldByIndex[LCount] = NullFloat) then
            LTempString := '0'
          else
            LTempString := Format('%g', [lRunConfig.TargetYieldByIndex[LCount]]);
          if (Pos('.', LTempString) = 0) then
            LTempString := LTempString + '.';
          while (Length(LTempString) < 6) do
            LTempString := ' ' + LTempString;
          }
          LOutString := LOutString + LTempString;
          if((lRunConfig.CalculateHistoricFirmYield = 1) and (LCount = 2)) then Break;
        end;
        LF01File.Add(LOutString);

        //line10+++++++++++++++++++++++++++
        LOutString:='';
        LTempString:= '';
        // Maximum yield
  {      for LCount := 1 to lCaseCount do
        begin
          if (LRunParameters.FMaxYield[LCount].FInitalised) then
            LTempString:=LTempString+ PadDouble(LRunParameters.FMaxYield[LCount])
          else
            LTempString := LTempString + '   0.0';
        end;
        LOutString:=LOutString+LTempString;}
        for LCount := 1 to lRunConfig.NrOfActiveLoadCases do
        begin
          if (lRunConfig.MaximumYieldByIndex[LCount] = NullFloat) then
            LTempString := SmartFloatFormat(0.0,6,0)
          else
            LTempString := SmartFloatFormat(lRunConfig.MaximumYieldByIndex[LCount],6,0);
  {        if (lRunConfig.MaximumYieldByIndex[LCount] = NullFloat) then
            LTempString := '0'
          else
            LTempString := Format('%g', [lRunConfig.MaximumYieldByIndex[LCount]]);
          if (Pos('.', LTempString) = 0) then
            LTempString := LTempString + '.';
          while (Length(LTempString) < 6) do
            LTempString := ' ' + LTempString; }
          LOutString := LOutString + LTempString;
          if((lRunConfig.CalculateHistoricFirmYield = 1) and (LCount = 2)) then Break;
        end;
        LF01File.Add(LOutString);

        //line11+++++++++++++++++++++++++++
        LOutString:='';
        LTempString := '';
        // Target system power demand
  {      for LCount := 1 to lCaseCount do
        begin
          if (LRunParameters.FTargetPower[LCount].FInitalised) then
            LTempString := LTempString + PadDouble(LRunParameters.FTargetPower[LCount])
          else
            LTempString := LTempString + '   0.0';
        end;
        LOutString:=LOutString+LTempString;}
        for LCount := 1 to lRunConfig.NrOfActiveLoadCases do
        begin
          if (lRunConfig.TargetPowerByIndex[LCount] = NullFloat) then
            LTempString := SmartFloatFormat(0.0,6,0)
          else
            LTempString := SmartFloatFormat(lRunConfig.TargetPowerByIndex[LCount],6,0);

   {       if (lRunConfig.TargetPowerByIndex[LCount] = NullFloat) then
            lTempString := '      '
          else
          begin
            LTempString := Format('%g', [lRunConfig.TargetPowerByIndex[LCount]]);
            if (Pos('.', LTempString) = 0) then
              LTempString := LTempString + '.';
            while (Length(LTempString) < 6) do
              LTempString := ' ' + LTempString;
          end;}
          LOutString := LOutString + LTempString;
          if((lRunConfig.CalculateHistoricFirmYield = 1) and (LCount = 2)) then Break;
        end;
        LF01File.Add(LOutString);

        //Line12+++++++++++++++++++++++++++++
        if (lRunConfig.CalculateHistoricFirmYield = 2) then
        begin
          LOutString  := '';
          LTempString := '';
          LTempString := PadInt(LRunParameters.FTargetRecurrenceInterval);
          LOutString  := LOutString + LTempString;
          LF01File.Add(LOutString);
        end;
      end;

      for LCount := 0 to LRunParameters.FF01ExtraLines.Count -1 do
      begin
        LF01File.Add(LRunParameters.FF01ExtraLines[LCount]);
      end;

      LF01File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile01Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF01File.Free;
    end;
    //MyGeneral.UpdateGen(tmpGen);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
