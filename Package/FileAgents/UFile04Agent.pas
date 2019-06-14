//
//
//  UNIT      : Contains TFile04Agent Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 07/02/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UFile04Agent;

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
  UFlowConstraintsObject,
  UYieldModelDataObject;
type

  TFile04Agent = class(TAbstractFileAgent)
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

function TFile04Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile04Agent.ReadModelDataFromFile';
Var
  LFileData            : TStringList;
  LMessage,
  LReadString,
  LTempString          : String;
  LReadInteger,
  LCount,
  LLinesCount,
  LCurrentLine,
  LStartPos,
  LErrorCode           : Integer;
  LReadReal            : Double;
  LFileIncomplete      : Boolean;
  LFlowConstraints     : TFlowConstraintsObject;
  LStructure           : TStructure;
  LFileLineTypesObject : TAbstractFileLineTypesObject;
  LStop                : boolean;
  lRow                 : integer;
  lCol                 : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile04Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile04Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LFlowConstraints := ADataObject.FFlowConstraintsObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LFlowConstraints.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      LFileIncomplete := False;

      //Read the F04 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Check F04 file contains at leat 4 lines
      if(LFileData.Count < 1) then
      begin
        LMessage := FAppModules.Language.GetString('TFile04Agent.strF04StructureErr');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptWarning,LStop);
      end;

      // Line 1+++++++++++++++++++++++++++++
      LCurrentLine := 0;
      LReadString := LFileData[lCurrentLine];
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LTempString:=GetSubstring(LReadString,1,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) and (LTempString <> '') then
      begin
        LMessage := FAppModules.Language.GetString('TFile04Agent.strControlStructureCountErr');
        LMessage := Format(LMessage,[1,1,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if ((LReadInteger < 0) OR (LReadInteger > 30)) then
      begin
        LMessage := FAppModules.Language.GetString('TFile04Agent.strMaxControlStructureCountErr');
        LMessage := Format(LMessage,[1,1,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      begin
        LFlowConstraints.FControlStructureCount.FData :=LReadInteger;
        LFlowConstraints.FControlStructureCount.FInitalised := True;
      end;

      if (LReadInteger < 1) then
      begin
         LFlowConstraints.FF04ExtraLines.Assign(LFileData);
      end
      else
      begin
        if not LFlowConstraints.AddStructures then
        Exit;

        for LCount := 0 to LFlowConstraints.FControlStructureCount.FData-1 do
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;

          LStructure := TStructure(LFlowConstraints.FStructure[LCount]);
          LStructure.FIdentifier := LCount+ 1;

          // Line 2+++++++++++++++++++++++++++++
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strChannelNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > 3000)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strChannelNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FChannelNum.FData :=LReadInteger;
            LStructure.FChannelNum.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strUpStreamReservoirNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strUpStreamReservoirNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FUpStreamReservoirNum.FData :=LReadInteger;
            LStructure.FUpStreamReservoirNum.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strDownStreamReservoirNumrErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strDownStreamReservoirNumrErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FDownStreamReservoirNum.FData :=LReadInteger;
            LStructure.FDownStreamReservoirNum.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strPointsElevationNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > 10)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strMaxPointsElevationNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LStructure.FPointsElevationNum.FData :=LReadInteger;
            LStructure.FPointsElevationNum.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strFSillElevationErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FSillElevation.FData :=LReadReal;
            LStructure.FSillElevation.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,31,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strGateHeightErr');
            LMessage := Format(LMessage,[LCurrentLine+1,31,40]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FGateHeight.FData:=LReadReal;
            LStructure.FGateHeight.FInitalised:= True;
          end;

          LTempString:=GetSubstring(LReadString,44,2);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strStructureTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,44,45]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FStructureType.FData:= LReadInteger;
            LStructure.FStructureType.FInitalised:= True;
          end;

          LTempString:=GetSubstring(LReadString,46,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strDischargeCoefficientErr');
            LMessage := Format(LMessage,[LCurrentLine+1,46,55]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FDischargeCoefficient.FData:= LReadReal;
            LStructure.FDischargeCoefficient.FInitalised:= True;
          end;

          LTempString:=GetSubstring(LReadString,56,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile04Agent.strControlStructureLengthtErr');
            LMessage := Format(LMessage,[LCurrentLine+1,56,65]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LStructure.FControlStructureLength.FData:= LReadReal;
            LStructure.FControlStructureLength.FInitalised:= True;
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType in [4,5,7,8,9,14]
          if (LStructure.FStructureType.FData in [4,5,7,8,9,14]) then
          begin
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strElevationsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FElevations[LLinesCount].FData :=LReadReal;
                LStructure.FElevations[LLinesCount].FInitalised:= True;
              end;
            end;

            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strDischargesErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FDischarges[LLinesCount].FData :=LReadReal;
                LStructure.FDischarges[LLinesCount].FInitalised:= True;
              end;
            end;
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType = 10
          if (LStructure.FStructureType.FData = 10) then
          begin
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strChannelNumbersErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FChannelNumbers[LLinesCount].FData :=LReadReal;
                LStructure.FChannelNumbers[LLinesCount].FInitalised:= True;
              end;
            end;

            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strKFactorsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FKFactors[LLinesCount].FData :=LReadReal;
                LStructure.FKFactors[LLinesCount].FInitalised:= True;
              end;
            end;
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType = 11
          if (LStructure.FStructureType.FData = 11) then
          begin
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strHeadDifferencesErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FHeadDifferences[LLinesCount].FData :=LReadReal;
                LStructure.FHeadDifferences[LLinesCount].FInitalised:= True;
              end;
            end;

            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strAquiferFlowsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FAquiferFlows[LLinesCount].FData :=LReadReal;
                LStructure.FAquiferFlows[LLinesCount].FInitalised:= True;
              end;
            end;
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strDownStreamNodeInflowsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FDownStreamNodeInflows[LLinesCount].FData :=LReadReal;
                LStructure.FDownStreamNodeInflows[LLinesCount].FInitalised:= True;
              end;
            end;

            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strRiverDepthsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FRiverDepths[LLinesCount].FData :=LReadReal;
                LStructure.FRiverDepths[LLinesCount].FInitalised:= True;
              end;
            end;
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType = 12
          if (LStructure.FStructureType.FData = 12) then
          begin
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=1;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
            for LLinesCount := MinCurvePoints to MaxCurvePoints do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strElevationDifferencesErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FElevationDifferences[LLinesCount].FData :=LReadReal;
                LStructure.FElevationDifferences[LLinesCount].FInitalised:= True;
              end;
            end;

            for lRow := 1 to 10 do
            begin
              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LStartPos:=-7;
              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');

              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strMonthlyAverageInflowsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FMonthlyAverageInflows[lRow].FData :=LReadReal;
                LStructure.FMonthlyAverageInflows[lRow].FInitalised:= True;
              end;



              for LCol := MinCurvePoints to MaxCurvePoints do
              begin
                Inc(LStartPos,8);
                LTempString:=GetSubstring(LReadString,LStartPos,8);
                LTempString := Trim(LTempString);
                Val(LTempString,LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFile04Agent.strMonthlyAverageDivertedFlowErr');
                  LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LStructure.FMonthlyAverageDivertedFlow[lRow,LCol].FData :=LReadReal;
                  LStructure.FMonthlyAverageDivertedFlow[lRow,LCol].FInitalised:= True;
                end;
              end;
            end;
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType = 13
          if (LStructure.FStructureType.FData = 13) then
          begin
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strPumpingHeadsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FPumpingHeads[LLinesCount].FData :=LReadReal;
                LStructure.FPumpingHeads[LLinesCount].FInitalised:= True;
              end;
            end;

            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LStartPos:=-7;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              Inc(LStartPos,8);
              LTempString:=GetSubstring(LReadString,LStartPos,8);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile04Agent.strPumpingDischargesErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+7]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LStructure.FPumpingDischarges[LLinesCount].FData :=LReadReal;
                LStructure.FPumpingDischarges[LLinesCount].FInitalised:= True;
              end;
            end;
          end;
        end;

        for LLinesCount := LCurrentLine + 1 to LFileData.Count - 1 do
          LFlowConstraints.FF04ExtraLines.Add(LFileData[LLinesCount]);
      end;


      if LFileIncomplete then
      begin
        LMessage := FAppModules.Language.GetString('TFile04Agent.strFileIncomplete');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        LMessage := FAppModules.Language.GetString('TFile04Agent.strReadingCompleted');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
        Result := True;
      end;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile04Agent.WriteModelDataToFile';
var
  LMessage         : String;
  LOutString,
  LTempString      : String;
  LLinesCount,
  LCount           : Integer;
  LF04File         : TStringlist;
  LStructure       : TStructure;
  LFlowConstraints : TFlowConstraintsObject;
  LStop            : boolean;
  LRow             : integer;
  LCol             : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile04Agent.strWritingStarted');
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

    LFlowConstraints := ADataObject.FFlowConstraintsObject;

    LF04File:= TStringList.Create;
    try
      if (LFlowConstraints.FControlStructureCount.FData < 1) or (not LFlowConstraints.FControlStructureCount.FInitalised) then
      begin
        LF04File.Assign(LFlowConstraints.FF04ExtraLines)
      end
      else
      begin

       //Line1 ++++++++++++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LFlowConstraints.FControlStructureCount);
        LOutString:=LOutString+LTempString;
        LF04File.Add(LOutString);

        //Line2 ++++++++++++++++++++++++++++++++++
        for LCount := 0 to LFlowConstraints.FControlStructureCount.FData-1 do
        begin
          LStructure       := TStructure(LFlowConstraints.FStructure[LCount]);

          LOutString:='';
          LTempString:=PadInt(LStructure.FChannelNum);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LStructure.FUpstreamReservoirNum);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LStructure.FDownstreamReservoirNum);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LStructure.FPointsElevationNum);
          LOutString:=LOutString+LTempString;

//          LTempString:= PadDouble(LStructure.FSillElevation);
          LTempString:= SmartPadDouble(LStructure.FSillElevation);
          LOutString:=LOutString+LTempString;

//          LTempString:= PadDouble(LStructure.FGateHeight);
          LTempString:= SmartPadDouble(LStructure.FGateHeight);
          LOutString:=LOutString+LTempString;

          LTempString:=PadInt(LStructure.FStructureType);
          LOutString:=LOutString + '   ' + LTempString ;

//          LTempString:= PadDouble(LStructure.FDischargeCoefficient);
          LTempString:= SmartPadDouble(LStructure.FDischargeCoefficient);
          LOutString:=LOutString+LTempString;

//          LTempString:= PadDouble(LStructure.FControlStructureLength);
          LTempString:= SmartPadDouble(LStructure.FControlStructureLength);
          LOutString:=LOutString+LTempString;
          LF04File.Add(LOutString);

          // Line 3+++++++++++++++++++++++++++++ StructureType in [4,5,7,8,9,14]
          if (LStructure.FStructureType.FData in [4,5,7,8,9,14]) then
          begin
            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString :=  PadDouble(LStructure.FElevations[LLinesCount]);
              LTempString :=  SmartPadDouble(LStructure.FElevations[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);

            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FDischarges[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FDischarges[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType = 10
          if (LStructure.FStructureType.FData = 10) then
          begin
            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FChannelNumbers[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FChannelNumbers[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);

            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FKFactors[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FKFactors[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);
          end;

          // Line 3+++++++++++++++++++++++++++++ StructureType = 11
          if (LStructure.FStructureType.FData = 11) then
          begin
            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FHeadDifferences[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FHeadDifferences[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);

            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FAquiferFlows[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FAquiferFlows[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);
            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
              LTempString := SmartPadDouble(LStructure.FDownStreamNodeInflows[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FDownStreamNodeInflows[LLinesCount]);
//              LTempString := PadDouble(LStructure.FDownStreamNodeInflows[LLinesCount]);
//              LTempString := PadDouble(LStructure.FDownStreamNodeInflows[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);

            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FRiverDepths[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FRiverDepths[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);
          end;


          // Line 3+++++++++++++++++++++++++++++ StructureType = 12
          if (LStructure.FStructureType.FData = 12) then
          begin
            LOutString:='        ';
            LTempString := '';
            for LLinesCount := 1 to 10 do
            begin
//              LTempString := PadDouble(LStructure.FElevationDifferences[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FElevationDifferences[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);

            for LRow := 1 to 10 do
            begin
              LOutString:='';
              LTempString := '';
//              LTempString := PadDouble(LStructure.FMonthlyAverageInflows[LRow]);
              LTempString := SmartPadDouble(LStructure.FMonthlyAverageInflows[LRow]);
              LOutString:=LOutString+LTempString;
              for LCol := 1 to 10 do
              begin
//                LTempString := PadDouble(LStructure.FMonthlyAverageDivertedFlow[LRow,LCol]);
                LTempString := SmartPadDouble(LStructure.FMonthlyAverageDivertedFlow[LRow,LCol]);
                LOutString:=LOutString+LTempString;
              end;
              LF04File.Add(LOutString);
            end;
          end;


          // Line 3+++++++++++++++++++++++++++++ StructureType = 13
          if (LStructure.FStructureType.FData = 13) then
          begin
            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FPumpingHeads[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FPumpingHeads[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);

            LOutString:='';
            LTempString := '';
            for LLinesCount := 1 to LStructure.FPointsElevationNum.FData do
            begin
//              LTempString := PadDouble(LStructure.FPumpingDischarges[LLinesCount]);
              LTempString := SmartPadDouble(LStructure.FPumpingDischarges[LLinesCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF04File.Add(LOutString);
          end;
        end;

        for LLinesCount := 0 to LFlowConstraints.FF04ExtraLines.Count -1 do
        begin
          LF04File.Add(LFlowConstraints.FF04ExtraLines[LLinesCount]);
        end;
      end;

      if(LF04File.Count = 0) then
      begin
        LF04File.Add('    0');
        LF04File.Add('');
        LF04File.Add('');
        LF04File.Add('');
      end;

      LF04File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile04Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF04File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
