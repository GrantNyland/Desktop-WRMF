//
//
//  UNIT      : Contains TFile05Agent Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 14/02/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile05Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UZoneDefinitionsObject,
  UAbstractFileAgent,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile05Agent = class(TAbstractFileAgent)
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
  VoaimsCom_TLB,
  UErrorHandlingOperations;

function TFile05Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile05Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  //LInteger :TInteger;
  LMessage,
  LReadString,
  LTempString          : String;
  LStart,
  LReadInteger,
  LCount,
  LLinesCount,
  LCurrentLine,
  LNodeCount,
  LStartPos,
  LErrorCode           : Integer;
  LReadReal            : Double;
  LFileIncomplete      : Boolean;
  LZoneDefinitions     : TZoneDefinitionsObject;
  LStorageZones        : TStorageZones;
  LNode                : TNodes;
  LReservoirLevel      : TReservoirLevels;
  LFileLineTypesObject : TAbstractFileLineTypesObject;
  LStop                : boolean;
  LReservoirName       : String;
  LReservoirNodeNumber : Integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile05Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile05Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LZoneDefinitions := ADataObject.FZoneDefinitionsObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LZoneDefinitions.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F05 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Check F05 file contains at leat 3 lines
      if(LFileData.Count < 1) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strF05ZoneErr');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      LCurrentLine := 0;
      // Line 1+++++++++++++++++++++++++++++
      LStart := 1;
      LReadString := LFileData[LCurrentLine];
      LCurrentLine := LCurrentLine + 1;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LTempString:=GetSubstring(LReadString,LStart,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strStorageZoneCountErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 20) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strMaxStorageZoneCountErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      if(LReadInteger < 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strMinStorageZoneCountErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      begin
         LZoneDefinitions.FStorageZoneCount.FData       := LReadInteger;
         LZoneDefinitions.FStorageZoneCount.FInitalised := True;
      end;

      LStart := 6;
      LTempString:=GetSubstring(LReadString,LStart,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strZoneLowerBoundaryErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LZoneDefinitions.FZoneLowerBoundary.FData       := LReadInteger;
        LZoneDefinitions.FZoneLowerBoundary.FInitalised := True;
      end;

      LStart := 11;
      LTempString:=GetSubstring(LReadString,LStart,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strPenaltyStructureCountErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger > 20) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strMaxPenaltyStructureCountErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      if(LReadInteger < 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strMinPenaltyStructureCountErr');
        LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      begin
        LZoneDefinitions.FPenaltyStructureCount.FData :=LReadInteger;
        LZoneDefinitions.FPenaltyStructureCount.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,16,Length(LReadString));
      if(LTempString <> '') then
      begin
        LZoneDefinitions.FComment.FData := LTempString;
        LZoneDefinitions.FComment.FLength := Length(LTempString);
        LZoneDefinitions.FComment.FInitalised := True;
      end;

      LFileIncomplete := False;
      if(LZoneDefinitions.FStorageZoneCount.FData > 0) then
      begin
        if not LZoneDefinitions.AddStorageZones then
         Exit;

        for LCount := 1 to LZoneDefinitions.FStorageZoneCount.FData do
        begin
          LStorageZones := TStorageZones(LZoneDefinitions.FStorageZones[LCount]);

          // Line 1a+++++++++++++++++++++++++++++

          LReadString := LFileData[LCurrentLine];
          LCurrentLine := LCurrentLine + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');
          LTempString:=GetSubstring(LReadString,5,6);
          if(Trim(LTempString) = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strZoneNameErr');
            LMessage := Format(LMessage,[LCurrentLine,5,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStorageZones.FResevoirZoneName.FData :=LTempString;
            LStorageZones.FResevoirZoneName.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strStrategyIndicatorErr');
            LMessage := Format(LMessage,[LCurrentLine,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
        if(LReadInteger > 3) then
        begin
          LMessage := FAppModules.Language.GetString('TFile05Agent.strMaxStrategyIndicatorErr');
          LMessage := Format(LMessage,[LCurrentLine,11,15]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
       { end
        else
        if(LReadInteger < 1) then
        begin
          LMessage := FAppModules.Language.GetString('TFile05Agent.strMinStrategyIndicatorErr');
          LMessage := Format(LMessage,[LCurrentLine,11,15]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;}
        end
        else
        begin
          LStorageZones.FStrategyIndicator.FData :=LReadInteger;
          LStorageZones.FStrategyIndicator.FInitalised := True;
        end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strBalancingVariableErr');
            LMessage := Format(LMessage,[LCurrentLine,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
        if(LReadInteger > 2) then
        begin
          LMessage := FAppModules.Language.GetString('TFile05Agent.strMaxBalancingVariableErr');
          LMessage := Format(LMessage,[LCurrentLine,16,20]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        {end
        else
        if(LReadInteger < 1) then
        begin
          LMessage := FAppModules.Language.GetString('TFile05Agent.strMinBalancingVariableErr');
          LMessage := Format(LMessage,[LCurrentLine,16,20]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;}
        end
        else
        begin
          LStorageZones.FBalancingVariable.FData :=LReadInteger;
          LStorageZones.FBalancingVariable.FInitalised := True;
        end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strBalancingPolicyErr');
            LMessage := Format(LMessage,[LCurrentLine,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if(LReadInteger > 3) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strMaxBalancingPolicyErr');
            LMessage := Format(LMessage,[LCurrentLine,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          {end
          else
          if(LReadInteger < 1) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strMinBalancingPolicyErr');
            LMessage := Format(LMessage,[LCurrentLine,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;}
          end
          else
          begin
            LStorageZones.FBalancingPolicy.FData :=LReadInteger;
            LStorageZones.FBalancingPolicy.FInitalised := True;
          end;

          LStartPos := 16;
          for LLinesCount := MinPenaltyStructure to LZoneDefinitions.FPenaltyStructureCount.FData do
          begin
            Inc(LStartPos,10);
            LTempString:=GetSubstring(LReadString,LStartPos,10);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile05Agent.strBalanceReferenceErr');
              LMessage := Format(LMessage,[LCurrentLine,LStartPos,LStartPos+9]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LStorageZones.FBalanceReference[LLinesCount].FData :=LReadReal;
              LStorageZones.FBalanceReference[LLinesCount].FInitalised := True;
            end;
          end;

          LTempString:=GetSubstring(LReadString,LStartPos+10,Length(LReadString));
          if(LTempString <> '') then
          begin
            LStorageZones.FComment.FData := LTempString;
            LStorageZones.FComment.FLength := Length(LTempString);
            LStorageZones.FComment.FInitalised := True;
          end;
        end;


        //line2 +++++++++++++++++++++++++++++
        LFileIncomplete := False;
        LCount := 0;
        LNodeCount:= 0;
        While  True do
        begin
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;

          LStart := 1;
          LCount := LCount + 1;
          LReadString := LFileData[LCurrentLine];
          LCurrentLine := LCurrentLine + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
          LTempString:=GetSubstring(LReadString,LStart,5);

          if  Trim(LTempString) = '0' then
          begin
            LZoneDefinitions.FZeroComment.FData :=LReadString;
            LZoneDefinitions.FZeroComment.FInitalised := True;
            LZoneDefinitions.FZeroComment.FLength := Length(LReadString);
            Break;
          end;
          if not LZoneDefinitions.AddNode then
          Exit;

          LNode := TNodes(LZoneDefinitions.FNodes[LCount]);

          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strNodeNumberStorageErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LNode.FNodeNumberStorage.FData :=LReadInteger;
            LNode.FNodeNumberStorage.FInitalised := True;
          end;

          LStart := LStart + 5;
          LTempString:=GetSubstring(LReadString,LStart,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strStatusIndicatorErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if(LReadInteger > 1) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strMaxStatusIndicatorErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strMinStatusIndicatorErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
            AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LNode.FStatusIndicator.FData       := LReadInteger;
            LNode.FStatusIndicator.FInitalised := True;

            if((LNode.FStatusIndicator.FData = 0) and (ADataObject.FReservoirObject.ReservoirByReservoirNumber(LNode.FNodeNumberStorage.FData) <> nil)) then
            begin
              LReservoirNodeNumber := ADataObject.FReservoirObject.ReservoirByReservoirNumber(LNode.FNodeNumberStorage.FData).FNodeNo.FData;
              LReservoirName       := ADataObject.FReservoirObject.ReservoirByReservoirNumber(LNode.FNodeNumberStorage.FData).FName.FData;
              LMessage             := FAppModules.Language.GetString('TFile05Agent.strSelectionStatusErr');
              LMessage             := Format(LMessage,[LReservoirNodeNumber,Trim(LReservoirName)]);
              AProgressFunction(LMessage,ptWarning,LStop);
            end;
          end;

          if (FAppModules.Model.ModelName = CPlanning) then
          begin
            LStart := LStart + 5;
            LTempString:=GetSubstring(LReadString,LStart,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile05Agent.strPlottingOptionErr');
              LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            if(LReadInteger <> 1) and (LReadInteger <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile05Agent.strPlottingOptionErr');
              LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              LNode.FPlottingOption.FData :=LReadInteger;
              LNode.FPlottingOption.FInitalised := True;
            end;
          end;

          LStart := LStart + 5;
          LTempString:=GetSubstring(LReadString,LStart,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strReservoirPriorityErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+9]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LNode.FReservoirPriority.FData :=LReadReal;
            LNode.FReservoirPriority.FInitalised := True;
          end;

          LStart := LStart + 10;
          LTempString:=GetSubstring(LReadString,LStart,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strFullSupplyLevelErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+9]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LNode.FFullSupplyLevel.FData :=LReadReal;
            LNode.FFullSupplyLevel.FInitalised := True;
          end;

          LStart := LStart + 10;
          LTempString:=GetSubstring(LReadString,LStart,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strDeadStorageLevelErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+9]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LNode.FDeadStorageLevel.FData :=LReadReal;
            LNode.FDeadStorageLevel.FInitalised := True;
          end;

          LStart := LStart + 10;
          LTempString:=GetSubstring(LReadString,LStart,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strBottomOfReservoirErr');
            LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+9]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LNode.FBottomOfReservoir.FData :=LReadReal;
            LNode.FBottomOfReservoir.FInitalised := True;
          end;

          if (FAppModules.Model.ModelName = CPlanning) then
          begin
            LStart := LStart + 10;
            LTempString:=GetSubstring(LReadString,LStart,10);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile05Agent.strFullSupplyAllocationErr');
              LMessage := Format(LMessage,[LCurrentLine,LStart,LStart+9]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              LNode.FFullSupplyAllocation.FData :=LReadReal;
              LNode.FFullSupplyAllocation.FInitalised := True;
            end;
          end;

          LStart := LStart + 10;
          LTempString:=GetSubstring(LReadString,LStart,Length(LReadString));
          if(LTempString <> '') then
          begin
            LNode.FComment.FData := LTempString;
            LNode.FComment.FLength := Length(LTempString);
            LNode.FComment.FInitalised := True;
          end;

          LNodeCount := LNodeCount + 1;
        end;

        if (LNodeCount > 0) then
        begin
          LZoneDefinitions.FNodesCount.FData := LNodeCount;
          LZoneDefinitions.FNodesCount.FInitalised := True;
        end;

        //line3 +++++++++++++++++++++++++++++
        LZoneDefinitions.FReservoirLevelsCount.FData := (LZoneDefinitions.FStorageZoneCount.FData - 3) *  LZoneDefinitions.FNodesCount.FData;
        LZoneDefinitions.FReservoirLevelsCount.FInitalised := True;
        if not LZoneDefinitions.AddReservoirLevels then
         Exit;
        for LCount := 1 to  LZoneDefinitions.FReservoirLevelsCount.FData do
        begin

          LReservoirLevel := TReservoirLevels(LZoneDefinitions.FReservoirLevels[LCount]);

          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;

          LReadString := LFileData[LCurrentLine];
          LCurrentLine := LCurrentLine + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile05Agent.strReservoirNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
           LReservoirLevel.FReservoirIdentifier.FData :=LReadInteger;
           LReservoirLevel.FReservoirIdentifier.FInitalised := True;
         end;

          LStartPos:=-2;
          for LLinesCount := MinReservoirMonths to MaxReservoirMonths do
          begin
            Inc(LStartPos,8);
            LTempString:=GetSubstring(LReadString,LStartPos,8);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile05Agent.strResLevelErr');
              LMessage := Format(LMessage,[LCurrentLine,LStartPos,LStartPos+7]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LReservoirLevel.FResLevel[LLinesCount].FData :=LReadReal;
              LReservoirLevel.FResLevel[LLinesCount].FInitalised := True;
            end;
          end;

          LTempString:=GetSubstring(LReadString,LStartPos+8,Length(LReadString));
          if(LTempString <> '') then
          begin
            LReservoirLevel.FComment.FData := LTempString;
            LReservoirLevel.FComment.FLength := Length(LTempString);
            LReservoirLevel.FComment.FInitalised := True;
          end;
       end;
      end;

      for LLinesCount := LCurrentLine to LFileData.Count - 1 do
        LZoneDefinitions.FF05ExtraLines.Add(LFileData[LLinesCount]);

      if LFileIncomplete then
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strFileIncomplete');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        LMessage := FAppModules.Language.GetString('TFile05Agent.strReadingCompleted');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
        Result := True;
      end;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile05Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile05Agent.WriteModelDataToFile';
var
  LMessage         : string;
  LOutString       : string;
  LTempString      : string;
  LLinesCount      : integer;
  LCount           : integer;
  LF05File         : TStringlist;
  LStop            : boolean;
  lPenaltyList     : IReservoirPenaltyList;
  lPenalty         : IReservoirPenalty;
  LZoneDefinitions : TZoneDefinitionsObject;
  LStorageZones    : TStorageZones;
  LNode            : TNodes;
  LReservoirLevel : TReservoirLevels;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile05Agent.strWritingStarted');
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

    LF05File:= TStringList.Create;
    LZoneDefinitions := ADataObject.FZoneDefinitionsObject;
    try

     //Line1 ++++++++++++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LZoneDefinitions.FStorageZoneCount);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LZoneDefinitions.FZoneLowerBoundary);
      LOutString:=LOutString+LTempString;

      LTempString:=PadInt(LZoneDefinitions.FPenaltyStructureCount);
      LOutString:=LOutString+LTempString;

      LOutString:=LOutString + PadString(LZoneDefinitions.FComment);
      LF05File.Add(LOutString);

      //Line1a ++++++++++++++++++++++++++++++++++
      lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ReservoirPenaltyStructureList;

      for LCount := 1 to LZoneDefinitions.FStorageZoneCount.FData do
      begin
        LTempString := '';
        LOutString:='';
        LStorageZones := TStorageZones(LZoneDefinitions.FStorageZones[LCount]);

        LTempString:=PadString(LStorageZones.FResevoirZoneName);
        LOutString:=LOutString + '    ' + LTempString;

        LTempString:=PadInt(LStorageZones.FStrategyIndicator);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LStorageZones.FBalancingVariable);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LStorageZones.FBalancingPolicy);
        LOutString:=LOutString+LTempString;

        for LLinesCount := 0 to lPenaltyList.PenaltyCount-1 do
        begin
          lPenalty := lPenaltyList.ReservoirPenaltyByIndex[lLinesCount];
          if (lPenalty.ReservoirPenaltyValueByIndex[LCount] = NullFloat) then
            LTempString := '          '
          else
          begin
            LTempString := Format('%g', [lPenalty.ReservoirPenaltyValueByIndex[LCount]]);
            if (Pos('.', LTempString) = 0) then
              LTempString := LTempString + '.';
            while (Length(LTempString) < 10) do
              LTempString := ' ' + LTempString;
          end;    
          LOutString := LOutString + LTempString;
        end;
{        for LLinesCount := MinPenaltyStructure to LZoneDefinitions.FPenaltyStructureCount.FData do
        begin
          LTempString := LTempString+
                         PadDouble(LStorageZones.FBalanceReference[LLinesCount]);
        end;
        LOutString:=LOutString+LTempString;}
        LOutString:=LOutString + PadString(LStorageZones.FComment);
        LF05File.Add(LOutString);
      end;

      //line2+++++++++++++++++++++++++

      for LCount := 1 to LZoneDefinitions.FNodesCount.FData do
      begin
        LNode := TNodes(LZoneDefinitions.FNodes[LCount]);

        LOutString:='';
        LTempString := '';
        LTempString:=PadInt(LNode.FNodeNumberStorage);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LNode.FStatusIndicator);
        LOutString:=LOutString+LTempString;

        if (FAppModules.Model.ModelName = CPlanning) then
        begin
          LTempString:=PadInt(LNode.FPlottingOption);
          LOutString:=LOutString+LTempString;
        end;

        LTempString := PadDouble(LNode.FReservoirPriority);
        LOutString:=LOutString+LTempString;

        LTempString := PadDouble(LNode.FFullSupplyLevel);
        LOutString:=LOutString+LTempString;

        LTempString := PadDouble(LNode.FDeadStorageLevel);
        LOutString:=LOutString+LTempString;

        LTempString := PadDouble(LNode.FBottomOfReservoir);
        LOutString:=LOutString+LTempString;

        if (FAppModules.Model.ModelName = CPlanning) then
        begin
          LTempString:=PadDouble(LNode.FFullSupplyAllocation);
          LOutString:=LOutString+LTempString;
        end;

        LTempString := PadString(LNode.FComment);
        LOutString:=LOutString + LTempString;
        LF05File.Add(LOutString);
      end;

      //LF05File.Add(LZoneDefinitions.FZeroComment.FData);
      if(LF05File.Count > 0) then
        LF05File.Add('    0');

      //line3 +++++++++++++++++++++++++
      for LCount := 1 to  LZoneDefinitions.FReservoirLevelsCount.FData do
      begin
        LReservoirLevel := TReservoirLevels(LZoneDefinitions.FReservoirLevels[LCount]);

        LOutString:='';
        LTempString:=PadInt(LReservoirLevel.FReservoirIdentifier);
        LOutString:=LOutString+LTempString;

        LTempString := '';
        for LLinesCount := MinReservoirMonths to MaxReservoirMonths do
        begin
          LTempString := LTempString+
                       PadDouble(LReservoirLevel.FResLevel[LLinesCount]);
        end;
        LOutString:=LOutString+LTempString;
        LOutString:=LOutString + PadString(LReservoirLevel.FComment);
        LF05File.Add(LOutString);
      end;

      for LLinesCount := 0 to LZoneDefinitions.FF05ExtraLines.Count -1 do
      begin
        LF05File.Add(LZoneDefinitions.FF05ExtraLines[LLinesCount]);
      end;

      LF05File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile05Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF05File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
