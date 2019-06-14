//
//
//  UNIT      : Contains TFileAllocationDefinitionAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileAllocationDefinitionAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UPlanningFileDataObjects,
  UAllocationDefinitionFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UPlanningModelDataObject;

type

  TFileAllocationDefinitionAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  Math,
  UUtilities,
  UFilesLineTypeObject,
  VoaimsCom_TLB,
  UErrorHandlingOperations;

function TFileAllocationDefinitionAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAllocationDefinitionAgent.ReadModelDataFromFile';
var
  LLine11Data,
  LFileData: TStringList;
  LLineData,
  LMessage,
  LReadString,
  LTempString : String;
  LStart,
  LCount,
  LCount1,
  LCount2,
  LIndex1,
  LIndex2,
  LIndex3,
  LStartPos,
  LReadInteger,
  LSubSystemNumber,
  LOldSubSystemNumber,
  LLocalCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LDistribution,
  LCurtailment             : TClassValuesArrayObject;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LDemandSupportDefinition : TDemandSupportDefinition;
  LFamilyFileDataObject    : TAllocationDefinitionFileDataObject;
  LNonFirmSubsystem        : TNonFirmSubsystem;
  LCoefficient             : TCoefficients;
  LVolumeAndCoefficient    : TStartVolumeAndScenarioCoefficients;
  LScenarioCoefficient     : TScenarioCoefficients;
  LSystemFullPerNode       : TSystemFullPerNodes;
  LFixedSubSystem          : TFixedSubSystem;
  LSupportChannel          : TSupportChannel;
  LRoutingSupportChannelNumbers : TRoutingSupportChannelNumbers;
  LFileLineTypesObject : TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strReadingStarted');
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

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LFamilyFileDataObject    := LPlanningFileDataObject.AddAllocationDefinitionFileDataObject(AFilename.FileNumber);
    if(LFamilyFileDataObject = nil) then
      Exit;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LFamilyFileDataObject.Initialise then
      Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData   := TStringList.Create;
    LLine11Data := TStringList.Create;
    try
      //Read the F01 file
      LFileData.LoadFromFile(AFilename.FileName);
      LStart := 0;

      //line 1 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strClassCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 5) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strClassCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.ClassCount.FData       := LReadInteger;
        LFamilyFileDataObject.ClassCount.FInitalised := True;
      end;

      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strLevelCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 5) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strLevelCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.LevelCount.FData       := LReadInteger;
        LFamilyFileDataObject.LevelCount.FInitalised := True;
      end;

      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strCategoryCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 10) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strCategoryCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.CategoryCount.FData       := LReadInteger;
        LFamilyFileDataObject.CategoryCount.FInitalised := True;
      end;

      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strPeriodLengthErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.PeriodLength.FData       := LReadInteger;
        LFamilyFileDataObject.PeriodLength.FInitalised := True;
      end;

      LTempString:=ExtractDelemetedFirstSubstring('''',LReadString);
      if(LTempString =  '') then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strImplemntationDecisionErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.ImplemntationDecision.FData         := LTempString;
        LFamilyFileDataObject.ImplemntationDecision.FInitalised   := True;
        LFamilyFileDataObject.ImplemntationDecision.FLength       := Length(LFamilyFileDataObject.ImplemntationDecision.FData)+2;
      end;

      //line 2 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;
      for LCount := 1 to LFamilyFileDataObject.ClassCount.FData do
      begin
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(2,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strRIValueErr');
          LMessage := Format(LMessage,[LStart,LCount]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LFamilyFileDataObject.RIValueArray[LCount].FData       := LReadInteger;
          LFamilyFileDataObject.RIValueArray[LCount].FInitalised := True;
        end;
      end;

      //line 3 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;
      LStartPos   := 1;
      for LCount := 1 to LFamilyFileDataObject.ClassCount.FData do
      begin
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(3,'3');
        LTempString:=GetSubstring(LReadString,LStartPos,10);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strRILabelErr');
          LMessage := Format(LMessage,[LStart,LStartPos,LStartPos+10]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LFamilyFileDataObject.RILabelArray[LCount].FData       := LTempString;
          LFamilyFileDataObject.RILabelArray[LCount].FInitalised := True;
        end;
        Inc(LStartPos,10);
      end;

      //line 4 +++++++++++++++++++++++++
      for LIndex1 := 1 to LFamilyFileDataObject.LevelCount.FData do
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LLocalCount := LFileLineTypesObject.LinesCount;
        LCurtailment := LFamilyFileDataObject.AddCurtailment;
        for LCount := 1 to LFamilyFileDataObject.ClassCount.FData do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strCurtailmentErr');
            LMessage := Format(LMessage,[LStart,LCount]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCurtailment.ClassValuesArray[LCount].FData         := LReadReal;
            LCurtailment.ClassValuesArray[LCount].FInitalised   := True;
          end;
        end;

        if(Trim(LReadString) <> '') then
        begin
          LCurtailment.Comment.FData := LReadString;
          LCurtailment.Comment.FInitalised   := True;
        end;
      end;

      //line 5 +++++++++++++++++++++++++
      for LIndex1 := 1 to LFamilyFileDataObject.CategoryCount.FData do
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LDistribution := LFamilyFileDataObject.AddDistribution;
        LLocalCount := LFileLineTypesObject.LinesCount;
        for LCount := 1 to LFamilyFileDataObject.ClassCount.FData do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'5');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strDistributionErr');
            LMessage := Format(LMessage,[LStart,LCount]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LDistribution.ClassValuesArray[LCount].FData         := LReadReal;
            LDistribution.ClassValuesArray[LCount].FInitalised   := True;
          end;
        end;

        if(Trim(LReadString) <> '') then
        begin
          LDistribution.Comment.FData := LReadString;
          LDistribution.Comment.FInitalised   := True;
        end;
      end;

      //line 6 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strDemandSupportDefCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 100) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strDemandSupportDefCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.DemandSupportDefCount.FData       := LReadInteger;
        LFamilyFileDataObject.DemandSupportDefCount.FInitalised := True;
      end;

      //line 7 +++++++++++++++++++++++++
      for LIndex1 := 1 to LFamilyFileDataObject.DemandSupportDefCount.FData do
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LDemandSupportDefinition := LFamilyFileDataObject.AddDemandSupportDefinition;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo1Err');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.SubsystemNo1.FData         := LReadInteger;
          LDemandSupportDefinition.SubsystemNo1.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strGrowthFlagErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.GrowthFlag.FData         := LReadReal;
          LDemandSupportDefinition.GrowthFlag.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strTargetDemandErr');
          LMessage := Format(LMessage,[LStart,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.TargetDemand.FData         := LReadReal;
          LDemandSupportDefinition.TargetDemand.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractDelemetedFirstSubstring('''',LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strDemandDefNameErr');
          LMessage := Format(LMessage,[LStart,4]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.DemandDefName.FData         := LTempString;
          LDemandSupportDefinition.DemandDefName.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strDemandCentreIDErr');
          LMessage := Format(LMessage,[LStart,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.DemandCentreID.FData         := LReadInteger;
          LDemandSupportDefinition.DemandCentreID.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strUserCategoryErr');
          LMessage := Format(LMessage,[LStart,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.UserCategory.FData         := LReadInteger;
          LDemandSupportDefinition.UserCategory.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportArc1Err');
          LMessage := Format(LMessage,[LStart,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.SupportArc1.FData         := LReadInteger;
          LDemandSupportDefinition.SupportArc1.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportArc2Err');
          LMessage := Format(LMessage,[LStart,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.SupportArc2.FData         := LReadInteger;
          LDemandSupportDefinition.SupportArc2.FInitalised   := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportSubsystemsCountErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.SupportSubsystemsCount.FData         := LReadInteger;
          LDemandSupportDefinition.SupportSubsystemsCount.FInitalised   := True;
        end;

        //line 7j
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportSystemNrErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandSupportDefinition.SupportSystemNr.FData         := LReadInteger;
          LDemandSupportDefinition.SupportSystemNr.FInitalised   := True;
        end;

        for LCount := 2 to LDemandSupportDefinition.SupportSubsystemsCount.FData do
        begin
          LRoutingSupportChannelNumbers := LDemandSupportDefinition.AddRoutingSupportChannelNumbers;
          for LIndex2 := 1 to 5 do
          begin
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportChannelNumbersNumbersErr');
              LMessage := Format(LMessage,[LStart,LCount+10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LRoutingSupportChannelNumbers.ChannelNumbers[LIndex2].FData         := LReadInteger;
              LRoutingSupportChannelNumbers.ChannelNumbers[LIndex2].FInitalised   := True;
            end;
          end;
        end;

        for LCount := 2 to LDemandSupportDefinition.SupportSubsystemsCount.FData do
        begin
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportSubsystemsNumbersErr');
            LMessage := Format(LMessage,[LStart,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LDemandSupportDefinition.SupportSubsystemsNumbers[LCount-1].FData         := LReadInteger;
            LDemandSupportDefinition.SupportSubsystemsNumbers[LCount-1].FInitalised   := True;
          end;
        end;

        if(Trim(LReadString) <> '') then
        begin
          LDemandSupportDefinition.Comment.FData         := LReadString;
          LDemandSupportDefinition.Comment.FLength       := Length(LDemandSupportDefinition.Comment.FData);
          LDemandSupportDefinition.Comment.FInitalised   := True;
        end;
      end;

      //line 8 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;
      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strYieldCurveSubsCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 10) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strYieldCurveSubsCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.YieldCurveSubsCount.FData       := LReadInteger;
        LFamilyFileDataObject.YieldCurveSubsCount.FInitalised := True;
      end;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strYieldCurveSetsCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 10) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strYieldCurveSetsCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.YieldCurveSetsCount.FData       := LReadInteger;
        LFamilyFileDataObject.YieldCurveSetsCount.FInitalised := True;
      end;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFamilyLoadCaseCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 10) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFamilyLoadCaseCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.FamilyLoadCaseCount.FData       := LReadInteger;
        LFamilyFileDataObject.FamilyLoadCaseCount.FInitalised := True;
      end;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strCurveSetsPerMonthCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.CurveSetsPerMonthCount.FData       := LReadInteger;
        LFamilyFileDataObject.CurveSetsPerMonthCount.FInitalised := True;
      end;

      //line 9 +++++++++++++++++++++++++
      if(LFamilyFileDataObject.CurveSetsPerMonthCount.FData >= 1) then
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LLocalCount := LFileLineTypesObject.LinesCount;
        for LCount := 1 to 12 do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'9');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strDecisionIndicatorArrayErr');
            LMessage := Format(LMessage,[LStart,LCount]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LFamilyFileDataObject.DecisionIndicatorArray[LCount].FData         := LReadInteger;
            LFamilyFileDataObject.DecisionIndicatorArray[LCount].FInitalised   := True;
          end;
        end;
      end;

      //line 10 +++++++++++++++++++++++++
      for LIndex1 := 1 to LFamilyFileDataObject.YieldCurveSubsCount.FData do
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LNonFirmSubsystem := LFamilyFileDataObject.AddNonFirmSubsystem;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'10');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strNonFirmSubsystemNoErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LNonFirmSubsystem.NonFirmSubsystemNo.FData         := LReadInteger;
          LNonFirmSubsystem.NonFirmSubsystemNo.FInitalised   := True;
        end;

        for LCount := 1 to 5 do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'10');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strRoutingChannelNoArrayErr');
            LMessage := Format(LMessage,[LStart,LCount+1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LNonFirmSubsystem.RoutingChannelNoArray[LCount].FData         := LReadInteger;
            LNonFirmSubsystem.RoutingChannelNoArray[LCount].FInitalised   := True;
          end;
        end;
      end;

      //line 11 & 12 & 13 +++++++++++++++++++++++++
      for LCount1 := 1 to LFamilyFileDataObject.CurveSetsPerMonthCount.FData do
      begin
        for LCount2 := 1 to LFamilyFileDataObject.YieldCurveSubsCount.FData do
        begin
          LCoefficient := LFamilyFileDataObject.AddCoefficient;
          LCoefficient.CurveSetIndex := LCount1;
          LCoefficient.SubsystemIndex := LCount2;

          //line 11 +++++++++++++++++++++++++
          LReadString := LFileData[LStart];
          LLineData   := LReadString;
          LStart      := LStart + 1;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractDelemetedFirstSubstring('''',LReadString);
          if(LTempString = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNameErr');
            LMessage := Format(LMessage,[LStart,1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.SubsystemName.FData         := LTempString;
            LCoefficient.Subsystem.SubsystemName.FInitalised   := True;
            LCoefficient.Subsystem.SubsystemName.FLength       := Length(LCoefficient.Subsystem.SubsystemName.FData)+2;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LSubSystemNumber := NullInteger;
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo2Err');
            LMessage := Format(LMessage,[LStart,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.SubsystemNo2.FData         := LReadInteger;
            LCoefficient.Subsystem.SubsystemNo2.FInitalised   := True;
            LSubSystemNumber                                  := LReadInteger;
          end;

          if(LSubSystemNumber <> NullInteger) and (LSubSystemNumber <> 0)then
          begin
            LLine11Data.AddObject(LLineData,TObject(LSubSystemNumber));
            for LIndex1 := LLine11Data.Count-1 downto 0 do
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
              LOldSubSystemNumber := integer(LLine11Data.Objects[LIndex1]);
              if(LOldSubSystemNumber = LSubSystemNumber) then
              begin
                if(LLineData <> LLine11Data[LIndex1]) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strLine11UnequalErr');
                  LMessage := Format(LMessage,[LStart]);
                  AProgressFunction(LMessage,ptWarning,LStop);
                  Break
                end;
              end;
            end;
          end;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportSubsystemNoErr');
            LMessage := Format(LMessage,[LStart,3]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.SupportSubsystemNo.FData         := LReadInteger;
            LCoefficient.Subsystem.SupportSubsystemNo.FInitalised   := True;
          end;

          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strChannelNoErr');
            LMessage := Format(LMessage,[LStart,4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.ChannelNo.FData         := LReadInteger;
            LCoefficient.Subsystem.ChannelNo.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strShortTermFirmYieldErr');
            LMessage := Format(LMessage,[LStart,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.ShortTermFirmYield.FData         := LReadReal;
            LCoefficient.Subsystem.ShortTermFirmYield.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strStreamFlowErr');
            LMessage := Format(LMessage,[LStart,6]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.StreamFlow.FData         := LReadReal;
            LCoefficient.Subsystem.StreamFlow.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strLongTermFirmYieldErr');
            LMessage := Format(LMessage,[LStart,7]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.LongTermFirmYield.FData         := LReadReal;
            LCoefficient.Subsystem.LongTermFirmYield.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFamilyCurveStartYearErr');
            LMessage := Format(LMessage,[LStart,8]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.FamilyCurveStartYear.FData         := LReadInteger;
            LCoefficient.Subsystem.FamilyCurveStartYear.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFamilyCurveStartMonthErr');
            LMessage := Format(LMessage,[LStart,9]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.FamilyCurveStartMonth.FData         := LReadInteger;
            LCoefficient.Subsystem.FamilyCurveStartMonth.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFamilyCurveEndYearErr');
            LMessage := Format(LMessage,[LStart,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.FamilyCurveEndYear.FData         := LReadInteger;
            LCoefficient.Subsystem.FamilyCurveEndYear.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFamilyCurveEndMonthErr');
            LMessage := Format(LMessage,[LStart,11]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.FamilyCurveEndMonth.FData         := LReadInteger;
            LCoefficient.Subsystem.FamilyCurveEndMonth.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'11');
          LTempString:=ExtractDelemetedFirstSubstring('"',LReadString);
          if(LTempString = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFirmFlagErr');
            LMessage := Format(LMessage,[LStart,12]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LCoefficient.Subsystem.FirmFlag.FData         := LTempString;
            LCoefficient.Subsystem.FirmFlag.FInitalised   := True;
          end;

          //line 12 +++++++++++++++++++++++++
          for LIndex1 := 1 to LFamilyFileDataObject.YieldCurveSetsCount.FData do
          begin
            LVolumeAndCoefficient := LCoefficient.AddStartVolumeAndScenarioCoefficients;
            LReadString := LFileData[LStart];
            LStart      := LStart + 1;
            LLocalCount := LFileLineTypesObject.LinesCount;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'12');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strStartVolumeErr');
              LMessage := Format(LMessage,[LStart,7]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LVolumeAndCoefficient.StartVolume.StartVolume.FData         := LReadReal;
              LVolumeAndCoefficient.StartVolume.StartVolume.FInitalised   := True;
            end;

            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'12');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strCurveSetNoErr');
              LMessage := Format(LMessage,[LStart,8]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LVolumeAndCoefficient.StartVolume.CurveSetNo.FData         := LReadInteger;
              LVolumeAndCoefficient.StartVolume.CurveSetNo.FInitalised   := True;
            end;

            //line 13 +++++++++++++++++++++++++
            for LIndex2 := 1 to LFamilyFileDataObject.FamilyLoadCaseCount.FData do
            begin
              LScenarioCoefficient := LVolumeAndCoefficient.AddScenarioCoefficients;

              LReadString := LFileData[LStart];
              LStart      := LStart + 1;
              LLocalCount := LFileLineTypesObject.LinesCount;
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
              LTempString:=ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strLineTargetDraftErr');
                LMessage := Format(LMessage,[LStart,1]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LScenarioCoefficient.LineTargetDraft.FData         := LReadReal;
                LScenarioCoefficient.LineTargetDraft.FInitalised   := True;
              end;

              for LIndex3 := 1 to 4 do
              begin
                TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
                LTempString:=ExtractFirstSubstring(LReadString);
                LTempString := Trim(LTempString);
                Val(LTempString,LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strCoefArrayValErr');
                  LMessage := Format(LMessage,[LStart,LIndex3+1]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LScenarioCoefficient.CoefArray[LIndex3].FData         := LReadReal;
                  LScenarioCoefficient.CoefArray[LIndex3].FInitalised   := True;
                end;
              end;

              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'13');
              LTempString:=ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strRiskPropErr');
                LMessage := Format(LMessage,[LStart,7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LScenarioCoefficient.RiskProp.FData         := LReadReal;
                LScenarioCoefficient.RiskProp.FInitalised   := True;
              end;
            end;
          end;
        end;
      end;

      //line 14 +++++++++++++++++++++++++
      for LIndex1 := 1 to LFamilyFileDataObject.YieldCurveSubsCount.FData do
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LSystemFullPerNode := LFamilyFileDataObject.AddSystemFullPerNode;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'14');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo3Err');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSystemFullPerNode.SubsystemNo3.FData         := LReadInteger;
          LSystemFullPerNode.SubsystemNo3.FInitalised   := True;
        end;

        for LCount := 1 to 20 do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'14');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strNodeNoArrayErr');
            LMessage := Format(LMessage,[LStart,LCount+1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSystemFullPerNode.NodeNoArray[LCount].FData         := LReadInteger;
            LSystemFullPerNode.NodeNoArray[LCount].FInitalised   := True;
          end;
        end;
      end;

      //line 15 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'15');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportStrategyTypeErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger < 1) or (LReadInteger > 5) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportStrategyTypeValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.SupportStrategyType.FData       := LReadInteger;
        LFamilyFileDataObject.SupportStrategyType.FInitalised := True;
      end;

      //line 16 +++++++++++++++++++++++++
      if(LFamilyFileDataObject.SupportStrategyType.FData >= 2) then
      begin
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'16');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFixedPosSubsystemCountErr');
          LMessage := Format(LMessage,[LStart]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LFamilyFileDataObject.FixedPosSubsystemCount.FData       := LReadInteger;
          LFamilyFileDataObject.FixedPosSubsystemCount.FInitalised := True;
        end;

        //line 17 +++++++++++++++++++++++++
        if(LFamilyFileDataObject.FixedPosSubsystemCount.FData > 0) then
        begin
          for LIndex1 := 1 to LFamilyFileDataObject.FixedPosSubsystemCount.FData do
          begin
            LReadString := LFileData[LStart];
            LStart      := LStart + 1;

            LFixedSubSystem := LFamilyFileDataObject.AddFixedSubSystem;
            LLocalCount := LFileLineTypesObject.LinesCount;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'17');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo4Err');
              LMessage := Format(LMessage,[LStart,1]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LFixedSubSystem.SubsystemNo4.FData         := LReadInteger;
              LFixedSubSystem.SubsystemNo4.FInitalised   := True;
            end;

            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'17');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemPosErr');
              LMessage := Format(LMessage,[LStart,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LFixedSubSystem.SubsystemPos.FData         := LReadInteger;
              LFixedSubSystem.SubsystemPos.FInitalised   := True;
            end;
          end;
        end;

        //line 18 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'18');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSequentialPosSubsystemCountErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LFamilyFileDataObject.SequentialPosSubsystemCount.FData       := LReadInteger;
          LFamilyFileDataObject.SequentialPosSubsystemCount.FInitalised := True;
        end;

        //line 19 +++++++++++++++++++++++++
        if(LFamilyFileDataObject.SequentialPosSubsystemCount.FData > 0) then
        begin
          for LIndex1 := 1 to LFamilyFileDataObject.SequentialPosSubsystemCount.FData do
          begin
            LReadString := LFileData[LStart];
            LStart      := LStart + 1;

            LFixedSubSystem := LFamilyFileDataObject.AddSequentialSubSystem;

            LLocalCount := LFileLineTypesObject.LinesCount;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo4Err');
              LMessage := Format(LMessage,[LStart,1]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LFixedSubSystem.SubsystemNo4.FData         := LReadInteger;
              LFixedSubSystem.SubsystemNo4.FInitalised   := True;
            end;

            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'19');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemPosErr');
              LMessage := Format(LMessage,[LStart,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LFixedSubSystem.SubsystemPos.FData         := LReadInteger;
              LFixedSubSystem.SubsystemPos.FInitalised   := True;
            end;
          end;
        end;
      end;

      //line 20 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'20');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportStructureCountErr');
        LMessage := Format(LMessage,[LStart,1]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 5) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportStructureCountErr');
        LMessage := Format(LMessage,[LStart,1]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.SupportStructureCount.FData       := LReadInteger;
        LFamilyFileDataObject.SupportStructureCount.FInitalised := True;
      end;

      //line 21 +++++++++++++++++++++++++
      if(LFamilyFileDataObject.SupportStructureCount.FData > 0) then
      begin

        for LIndex1 := 1 to LFamilyFileDataObject.SupportStructureCount.FData do
        begin
          LReadString := LFileData[LStart];
          LStart      := LStart + 1;

          LSupportChannel := LFamilyFileDataObject.AddSupportChannel;

          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'21');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSupportChannelNoErr');
            LMessage := Format(LMessage,[LStart,1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSupportChannel.SupportChannelNo.FData         := LReadInteger;
            LSupportChannel.SupportChannelNo.FInitalised   := True;
          end;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'21');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo5Err');
            LMessage := Format(LMessage,[LStart,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if(LReadInteger > 10) then
          begin
            LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strSubsystemNo5Err');
            LMessage := Format(LMessage,[LStart,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSupportChannel.SubsystemNo5.FData       := LReadInteger;
            LSupportChannel.SubsystemNo5.FInitalised := True;
          end;

          for LCount := 1 to LSupportChannel.SubsystemNo5.FData  do
          begin
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'21');
            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strInfluencedSubSytemNoErr');
              LMessage := Format(LMessage,[LStart,LCount*2-1]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LSupportChannel.InfluencedSubSytem[LCount].FData         := LReadInteger;
              LSupportChannel.InfluencedSubSytem[LCount].FInitalised   := True;
            end;

            LTempString:=ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'21');
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strInfluenceFactorErr');
              LMessage := Format(LMessage,[LStart,LCount*1]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LSupportChannel.InfluenceFactor[LCount].FData         := LReadReal;
              LSupportChannel.InfluenceFactor[LCount].FInitalised   := True;
            end;
          end;
        end;
      end;


      //line 22 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'22');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strBalancingOptionErr');
        LMessage := Format(LMessage,[LStart,1]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 1) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strBalancingOptionErr');
        LMessage := Format(LMessage,[LStart,1]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LFamilyFileDataObject.BalancingOption.FData       := LReadInteger;
        LFamilyFileDataObject.BalancingOption.FInitalised := True;
      end;

      for LCount := LStart  to LFileData.Count - 1 do
        LFamilyFileDataObject.FMExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
      LLine11Data.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationDefinitionAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAllocationDefinitionAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LCount,
  LCount1,
  LMaxLength,
  LIndex1,
  LIndex2,
  LIndex3        : integer;

  LFamilyFile    : TStringlist;
  LStop          : boolean;
  LDistribution,
  LCurtailment             : TClassValuesArrayObject;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LDemandSupportDefinition : TDemandSupportDefinition;
  LFamilyFileDataObject    : TAllocationDefinitionFileDataObject;
  LNonFirmSubsystem        : TNonFirmSubsystem;
  LCoefficient             : TCoefficients;
  LVolumeAndCoefficient    : TStartVolumeAndScenarioCoefficients;
  LScenarioCoefficient     : TScenarioCoefficients;
  LSystemFullPerNode       : TSystemFullPerNodes;
  LFixedSubSystem          : TFixedSubSystem;
  LSupportChannel          : TSupportChannel;
  LRoutingSupportChannelNumbers : TRoutingSupportChannelNumbers;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject := TPlanningFileDataObjects(ADataObject);
    LFamilyFileDataObject    := LPlanningFileDataObject.AllocationDefinitionFileDataObjectByFileNumber[AFilename.FileNumber];

    if(LFamilyFileDataObject = nil) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFleDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strWritingStarted');
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


    LFamilyFile:= TStringList.Create;
    try
      //line 1 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LFamilyFileDataObject.ClassCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LFamilyFileDataObject.LevelCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LFamilyFileDataObject.CategoryCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LFamilyFileDataObject.PeriodLength);
      LOutString:=LOutString+LTempString;

      LFamilyFileDataObject.ImplemntationDecision.FLength := Length(LFamilyFileDataObject.ImplemntationDecision.FData)+2;
      LTempString:= PadStringWhithChar(LFamilyFileDataObject.ImplemntationDecision,'''');
      LOutString:=LOutString+'  '+LTempString;

      LFamilyFile.Add(LOutString);

      //line 2 +++++++++++++++++++++++++
      LOutString:='';
      for LCount := Low(LFamilyFileDataObject.RIValueArray) to High(LFamilyFileDataObject.RIValueArray) do
      begin
        if not LFamilyFileDataObject.RIValueArray[LCount].FInitalised then Break;
        LTempString:=PadInt(LFamilyFileDataObject.RIValueArray[LCount]);
        LOutString:=LOutString+LTempString;
      end;
      LFamilyFile.Add(LOutString);

      //line 3 +++++++++++++++++++++++++
      LOutString:='';
      for LCount := Low(LFamilyFileDataObject.RILabelArray) to High(LFamilyFileDataObject.RILabelArray) do
      begin
        if not LFamilyFileDataObject.RILabelArray[LCount].FInitalised then Break;
        LTempString:=PadString(LFamilyFileDataObject.RILabelArray[LCount]);
        LOutString:=LOutString+LTempString;
      end;
      LFamilyFile.Add(LOutString);

      //line 4 +++++++++++++++++++++++++
      for LIndex1 := 0 to LFamilyFileDataObject.CurtailmentCount-1 do
      begin
        LOutString:='';
        LCurtailment := LFamilyFileDataObject.CurtailmentByIndex[LIndex1];
        for LIndex2 := Low(LCurtailment.ClassValuesArray) to High(LCurtailment.ClassValuesArray) do
        begin
          if not LCurtailment.ClassValuesArray[LIndex2].FInitalised then Break;
          LTempString:=PadDouble(LCurtailment.ClassValuesArray[LIndex2]);
          LOutString:=LOutString+LTempString;
        end;

        if LCurtailment.Comment.FInitalised then
          LOutString:=LOutString+LCurtailment.Comment.FData;

        LFamilyFile.Add(LOutString);
      end;


      //line 5 +++++++++++++++++++++++++
      for LIndex1 := 0 to LFamilyFileDataObject.DistributionCount-1 do
      begin
        LOutString:='';
        LDistribution := LFamilyFileDataObject.DistributionByIndex[LIndex1];
        for LIndex2 := Low(LDistribution.ClassValuesArray) to High(LDistribution.ClassValuesArray) do
        begin
          if not LDistribution.ClassValuesArray[LIndex2].FInitalised then Break;
          LTempString:=PadDouble(LDistribution.ClassValuesArray[LIndex2]);
          LOutString:=LOutString+LTempString;
        end;

        if LDistribution.Comment.FInitalised then
          LOutString:=LOutString+LDistribution.Comment.FData;

        LFamilyFile.Add(LOutString);
      end;

      //line 6 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LFamilyFileDataObject.DemandSupportDefCount);
      LOutString:=LOutString+LTempString;
      LFamilyFile.Add(LOutString);

      //line 7 +++++++++++++++++++++++++
      LMaxLength := 0;
      for LIndex1 := 0 to LFamilyFileDataObject.DemandSupportDefinitionCount-1 do
      begin
        LDemandSupportDefinition := LFamilyFileDataObject.DemandSupportDefinitionByIndex[LIndex1];
        LMaxLength := Max(LMaxLength,Length(LDemandSupportDefinition.DemandDefName.FData));
      end;

      LMaxLength := LMaxLength + 2;
      for LIndex1 := 0 to LFamilyFileDataObject.DemandSupportDefinitionCount-1 do
      begin
        LDemandSupportDefinition := LFamilyFileDataObject.DemandSupportDefinitionByIndex[LIndex1];
        LDemandSupportDefinition.DemandDefName.FData := QuotedStr(LDemandSupportDefinition.DemandDefName.FData);
        LDemandSupportDefinition.DemandDefName.FLength := LMaxLength;
      end;

      for LIndex1 := 0 to LFamilyFileDataObject.DemandSupportDefinitionCount-1 do
      begin
        LDemandSupportDefinition := LFamilyFileDataObject.DemandSupportDefinitionByIndex[LIndex1];
        LOutString:='';

        LTempString:=PadInt(LDemandSupportDefinition.SubsystemNo1);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LDemandSupportDefinition.GrowthFlag);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LDemandSupportDefinition.TargetDemand);
        LOutString:=LOutString+LTempString;

        LTempString  := PadString(LDemandSupportDefinition.DemandDefName);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadInt(LDemandSupportDefinition.DemandCentreID);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDemandSupportDefinition.UserCategory);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDemandSupportDefinition.SupportArc1);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDemandSupportDefinition.SupportArc2);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDemandSupportDefinition.SupportSubsystemsCount);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDemandSupportDefinition.SupportSystemNr);
        LOutString:=LOutString+LTempString;


        for LCount := 0 to LDemandSupportDefinition.RoutingSupportChannelNumbersCount -1 do
        begin
          LRoutingSupportChannelNumbers := LDemandSupportDefinition.RoutingSupportChannelNumbersByIndex[LCount];
          for LIndex2 := 1 to 5 do
          begin
            LTempString:=PadInt(LRoutingSupportChannelNumbers.ChannelNumbers[LIndex2]);
            LOutString:=LOutString+LTempString;
          end;
        end;

        for LIndex2 := 1 to 10 do
        begin
          if LDemandSupportDefinition.SupportSubsystemsNumbers[LIndex2].FInitalised then
          begin
            LTempString:=PadInt(LDemandSupportDefinition.SupportSubsystemsNumbers[LIndex2]);
            LOutString:=LOutString+LTempString;
          end;
        end;

        if LDemandSupportDefinition.Comment.FInitalised then
        begin
          LOutString:=LOutString+LDemandSupportDefinition.Comment.FData;
        end;

        LFamilyFile.Add(LOutString);
      end;

      //line 8 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LFamilyFileDataObject.YieldCurveSubsCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LFamilyFileDataObject.YieldCurveSetsCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LFamilyFileDataObject.FamilyLoadCaseCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LFamilyFileDataObject.CurveSetsPerMonthCount);
      LOutString:=LOutString+LTempString;
      LFamilyFile.Add(LOutString);


      //line 9 +++++++++++++++++++++++++
      if(LFamilyFileDataObject.CurveSetsPerMonthCount.FData >= 1) then
      begin
        LOutString:='';
        for LIndex1 := Low(LFamilyFileDataObject.DecisionIndicatorArray) to High(LFamilyFileDataObject.DecisionIndicatorArray) do
        begin
          LTempString:=PadInt(LFamilyFileDataObject.DecisionIndicatorArray[LIndex1]);
          LOutString:=LOutString+LTempString;
        end;
        LFamilyFile.Add(LOutString);
      end;

      //line 10 +++++++++++++++++++++++++
      for LIndex1 := 0 to LFamilyFileDataObject.NonFirmSubsystemCount-1 do
      begin
        LOutString:='';
        LNonFirmSubsystem := LFamilyFileDataObject.NonFirmSubsystemByIndex[LIndex1];
        LTempString:=PadInt(LNonFirmSubsystem.NonFirmSubsystemNo);
        LOutString:=LOutString+LTempString;
        for LIndex2 := 1 to 5 do
        begin
          LTempString:=PadInt(LNonFirmSubsystem.RoutingChannelNoArray[LIndex2]);
          LOutString:=LOutString+LTempString;
        end;
        LFamilyFile.Add(LOutString);
      end;

      //line 11 & 12 & 13 +++++++++++++++++++++++++
      for LCount1 := 0 to LFamilyFileDataObject.CoefficientCount-1 do
      begin
        LCoefficient := LFamilyFileDataObject.CoefficientByIndex[LCount1];

        LOutString:='';
        LCoefficient.Subsystem.SubsystemName.FLength := Length(LCoefficient.Subsystem.SubsystemName.FData)+2;
        LTempString:= PadStringWhithChar(LCoefficient.Subsystem.SubsystemName,'''');
        LOutString:=LOutString+'  '+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.SubsystemNo2);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.SupportSubsystemNo);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.ChannelNo);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LCoefficient.Subsystem.ShortTermFirmYield);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LCoefficient.Subsystem.StreamFlow);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LCoefficient.Subsystem.LongTermFirmYield);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.FamilyCurveStartYear);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.FamilyCurveStartMonth);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.FamilyCurveEndYear);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LCoefficient.Subsystem.FamilyCurveEndMonth);
        LOutString:=LOutString+LTempString;

        LTempString:= PadStringWhithChar(LCoefficient.Subsystem.FirmFlag,'"');
        LOutString:=LOutString+'  '+LTempString;

        LFamilyFile.Add(LOutString);

        for LIndex1 := 0 to  LCoefficient.StartVolumeAndScenarioCoefficientCount-1 do
        begin
          LVolumeAndCoefficient := LCoefficient.StartVolumeAndScenarioCoefficientsByIndex[LIndex1];
          LOutString:='';
          LTempString:=PadDouble(LVolumeAndCoefficient.StartVolume.StartVolume);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LVolumeAndCoefficient.StartVolume.CurveSetNo);
          LOutString:=LOutString+LTempString;

          LFamilyFile.Add(LOutString);
          for LIndex2 := 0 to  LVolumeAndCoefficient.ScenarioCoefficientCount-1 do
          begin
            LScenarioCoefficient := LVolumeAndCoefficient.ScenarioCoefficientsByIndex[LIndex2];
            LOutString:='';

            LTempString:=PadDouble(LScenarioCoefficient.LineTargetDraft);
            LOutString:=LOutString+LTempString;

            for LIndex3 := Low(LScenarioCoefficient.CoefArray) to High(LScenarioCoefficient.CoefArray) do
            begin
              if not LScenarioCoefficient.CoefArray[LIndex3].FInitalised then Break;
              LTempString:=PadDouble(LScenarioCoefficient.CoefArray[LIndex3]);
              LOutString:=LOutString+LTempString;
            end;
            LTempString:=PadDouble(LScenarioCoefficient.RiskProp);
            LOutString:=LOutString+LTempString;

            LFamilyFile.Add(LOutString);
          end;
        end;
      end;

      //line 14 +++++++++++++++++++++++++
      for LIndex1 := 0 to LFamilyFileDataObject.SystemFullPerNodeCount-1 do
      begin
        LSystemFullPerNode := LFamilyFileDataObject.SystemFullPerNodeByIndex[LIndex1];
        LOutString:='';

        LTempString:=PadInt(LSystemFullPerNode.SubsystemNo3);
        LOutString:=LOutString+LTempString;

        for LCount := 1 to 20 do
        begin
          LTempString:=PadInt(LSystemFullPerNode.NodeNoArray[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LFamilyFile.Add(LOutString);
      end;

      //line 15 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LFamilyFileDataObject.SupportStrategyType);
      LOutString:=LOutString+LTempString;
      LFamilyFile.Add(LOutString);

      //line 16 +++++++++++++++++++++++++
      if(LFamilyFileDataObject.SupportStrategyType.FData >= 2) then
      begin
        LOutString:='';

        LTempString:=PadInt(LFamilyFileDataObject.FixedPosSubsystemCount);
        LOutString:=LOutString+LTempString;

        LFamilyFile.Add(LOutString);

        //line 17 +++++++++++++++++++++++++
        for LIndex1 := 0 to LFamilyFileDataObject.FixedSubSystemCount-1 do
        begin
          LFixedSubSystem := LFamilyFileDataObject.FixedSubSystemsByIndex[LIndex1];
          LOutString:='';

          LTempString:=PadInt(LFixedSubSystem.SubsystemNo4);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LFixedSubSystem.SubsystemPos);
          LOutString:=LOutString+LTempString;

          LFamilyFile.Add(LOutString);
        end;

        //line 18 +++++++++++++++++++++++++
        LOutString:='';

        LTempString:=PadInt(LFamilyFileDataObject.SequentialPosSubsystemCount);
        LOutString:=LOutString+LTempString;

        LFamilyFile.Add(LOutString);

        //line 19 +++++++++++++++++++++++++
        for LIndex1 := 0 to LFamilyFileDataObject.SequentialSubSystemCount-1 do
        begin
          LFixedSubSystem := LFamilyFileDataObject.SequentialSubSytemByIndex[LIndex1];
          LOutString:='';

          LTempString:=PadInt(LFixedSubSystem.SubsystemNo4);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LFixedSubSystem.SubsystemPos);
          LOutString:=LOutString+LTempString;

          LFamilyFile.Add(LOutString);
        end;
      end;

      //line 20 +++++++++++++++++++++++++
      LOutString:='';

      LTempString:=PadInt(LFamilyFileDataObject.SupportStructureCount);
      LOutString:=LOutString+LTempString;

      LFamilyFile.Add(LOutString);

      //line 21 +++++++++++++++++++++++++
      for LIndex1 := 0 to LFamilyFileDataObject.SupportChannelCount-1 do
      begin
        LSupportChannel := LFamilyFileDataObject.SupportChannelByIndex[LIndex1];
        LOutString:='';

        LTempString:=PadInt(LSupportChannel.SupportChannelNo);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LSupportChannel.SubsystemNo5);
        LOutString:=LOutString+LTempString;

        for LCount := 1 to LSupportChannel.SubsystemNo5.FData  do
        begin
          LTempString:=PadInt(LSupportChannel.InfluencedSubSytem[LCount]);
          LOutString:=LOutString+LTempString;

          LTempString:=PadDouble(LSupportChannel.InfluenceFactor[LCount]);
          LOutString:=LOutString+LTempString;
        end;
        LFamilyFile.Add(LOutString);
      end;


      //line 22 +++++++++++++++++++++++++
      LOutString:='';

      LTempString:=PadInt(LFamilyFileDataObject.BalancingOption);
      LOutString:=LOutString+LTempString;

      LFamilyFile.Add(LOutString);

      for LCount := 0 to LFamilyFileDataObject.FMExtraLines.Count -1 do
      begin
        LFamilyFile.Add(LFamilyFileDataObject.FMExtraLines[LCount]);
      end;

      LFamilyFile.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFamilyFile.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
