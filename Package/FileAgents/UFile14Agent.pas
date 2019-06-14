//
//
//  UNIT      : Contains TFile14Agent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 15/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile14Agent;

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
  UReleaseStructureObject,
  UYieldModelDataObject;

type

  TFile14Agent = class(TAbstractFileAgent)
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

function TFile14Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile14Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LReadInteger,
  LReleaseStructCount,
  LCount,
  LLinesRead,
  LLinesCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LReleaseControlStructureObject: TReleaseControlStructureObject;
  LControlStructureDetails:TReleaseControlStructureDetails;
  LInflowVariableLine :TInflowVariableLine;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LReference: TReference;
  LReferenceRelease: TReferenceRelease;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    If not FileExists(AFilename.FileName) then  Exit;

    LMessage := FAppModules.Language.GetString('TFile14Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile14Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    LReleaseControlStructureObject := ADataObject.FReleaseControlStructureObject;
    if not LReleaseControlStructureObject.Initialise then
      Exit;

    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F14 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;

      //Line 1 +++++++++++++++++++++++++++++
      LReadString := LFileData[LLinesRead];

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile14Agent.strControlStructCountErr');
        LMessage := Format(LMessage,[LLinesRead+1,1,6]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if (LReadInteger > MaxMonthlyControlStructCount) then
      begin
        LMessage := FAppModules.Language.GetString('TFile14Agent.strMonthlyStructureCountErr');
        LMessage := Format(LMessage,[LReadInteger]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LReleaseControlStructureObject.FControlStructCount.FData := LReadInteger;
        LReleaseControlStructureObject.FControlStructCount.FInitalised := True;
      end;

      if FAppModules.FieldProperties.FieldAvailableInModel('IFRInflowOption') or (FAppModules.Model.ModelName = CPlanning) then
      begin
        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceTypeErr');
          LMessage := Format(LMessage,[LLinesRead+1,4,5]);
          AProgressFunction(LMessage,ptWarning,LStop);
          LReleaseControlStructureObject.FInflowOption.FData := 1;
          LReleaseControlStructureObject.FInflowOption.FInitalised := True;
        end
        else
        if(LReadInteger < 1) or (LReadInteger > 2) then
         begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceTypeErr');
          LMessage := Format(LMessage,[1,4,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReleaseControlStructureObject.FInflowOption.FData := LReadInteger;
          LReleaseControlStructureObject.FInflowOption.FInitalised := True;
        end;
      end;

      for LReleaseStructCount := 1 to  LReleaseControlStructureObject.FControlStructCount.FData do
      begin
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then
          Break;
        if(Trim(LFileData[LLinesRead]) = '') then
          Break;

        LControlStructureDetails := LReleaseControlStructureObject.AddReleaseControlStructureDetails;
        if not Assigned(LControlStructureDetails) then
          Break;

        //Line 2 +++++++++++++++++++++++++++++
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LReadString := LFileData[LLinesRead];
        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strIFRChannelNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,3,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FData := LReadInteger;
          LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FInitalised := True;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceNodeCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,4,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if (LReadInteger > MaxRefNodeNumber) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceNodeCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,4,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FData := LReadInteger;
          LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FInitalised := True;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strLagInMonthsCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,6,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if ((LReadInteger < -12) OR (LReadInteger > 12)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strLagInMonthsCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,6,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FData := LReadInteger;
          LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FInitalised := True;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strPointsCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,8,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if (LReadInteger > 30) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strPointsCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,8,12]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LControlStructureDetails.FControlStructureData.FPointsCount.FData := LReadInteger;
          LControlStructureDetails.FControlStructureData.FPointsCount.FInitalised := True;
        end;

        LReadString := Trim(LReadString);
        if(LReadString <> '') then
        begin
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LControlStructureDetails.FControlStructureData.FIFRLoss.FData       := LReadInteger;
            LControlStructureDetails.FControlStructureData.FIFRLoss.FInitalised := True;
          end;
        end;

        LReadString := Trim(LReadString);
        if(LReadString <> '') then
        begin
          LTempString   := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LControlStructureDetails.FControlStructureData.FIFRUnknown.FData       := LReadInteger;
            LControlStructureDetails.FControlStructureData.FIFRUnknown.FInitalised := True;
          end;
        end;
        LLinesRead := LLinesRead + 1;

        //Optional line 3
        if (LControlStructureDetails.FControlStructureData.FIFRLoss.FInitalised) and
           (LControlStructureDetails.FControlStructureData.FIFRLoss.FData = 1) then
        begin
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;
          for LCount := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            if(LTempString <> '') then
              LControlStructureDetails.FControlStructureData.FMonthlyIFRLoss.Add(LTempString);
          end;
        end;

        //Line 3 +++++++++++++++++++++++++++++
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LReadString := LFileData[LLinesRead];
        if (LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FInitalised) then
        begin
          for LCount := 1 to LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FData do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strRefNodeNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,6]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LControlStructureDetails.FControlStructureData.FRefNodeNumber.Add(IntToStr(LReadInteger));
            end;
          end;
        end;

        //Line 4 +++++++++++++++++++++++++++++
//        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
        if (LControlStructureDetails.FControlStructureData.FPointsCount.FInitalised) then
        begin
          for LLinesCount := 1 to LControlStructureDetails.FControlStructureData.FPointsCount.FData do
          begin
            LLinesRead := LLinesRead + 1;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            if(LLinesRead >= LFileData.Count) then
              Break;
            if(Trim(LFileData[LLinesRead]) = '') then
              Break;

            LInflowVariableLine := LControlStructureDetails.AddInflowVariableLine;
            if not Assigned(LInflowVariableLine) then
              Break;

            LReadString := LFileData[LLinesRead]; {RHS}
            for LCount := MinReleaseStructure to MaxReleaseStructure do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile14Agent.strInflowVariableErr');
                LMessage := Format(LMessage,[LLinesRead+1,1,3]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                Break;
              end
              else
              begin
                LInflowVariableLine.FInflowVariable[LCount].FData := LReadReal;
                LInflowVariableLine.FInflowVariable[LCount].FInitalised := True;
              end;

              LTempString := ExtractFirstSubstring(LReadString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile14Agent.strReleaseVariableErr');
                LMessage := Format(LMessage,[LLinesRead+1,1,3]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                Break;
              end
              else
              begin
                LInflowVariableLine.FReleaseVariable[LCount].FData := LReadReal;
                LInflowVariableLine.FReleaseVariable[LCount].FInitalised := True;
              end;
            end;
          end;
        end;
      end;

      {if LReleaseControlStructureObject.FInflowOption.FInitalised then
      begin
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then
          Break;

        LReferenceDataObject := LReleaseControlStructureObject.FReferenceDataObject;

        LReadString := LFileData[LLinesRead];
        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceChannelsCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReferenceDataObject.FReferenceChannelsCount.FData := LReadInteger;
          LReferenceDataObject.FReferenceChannelsCount.FInitalised := True;
        end;

        for LLinesCount := 1 to LReferenceDataObject.FReferenceChannelsCount.FData do
        begin
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then
            Break;
          if(Trim(LFileData[LLinesRead]) = '') then
            Break;

          LReference := LReferenceDataObject.ReferenceByIndex(LLinesCount -1);

          LReadString := LFileData[LLinesRead];
          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceChannelNumberErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LReference.FReferenceChannel.FChannelNumber.FData := LReadInteger;
            LReference.FReferenceChannel.FChannelNumber.FInitalised := True;
          end;
        end;
      end;}

      //Line 5 +++++++++++++++++++++++++++++
      LLinesRead := LLinesRead + 1;
      if FAppModules.FieldProperties.FieldAvailableInModel('IFRInflowOption') or (FAppModules.Model.ModelName = CPlanning) and
         (LLinesRead < LFileData.Count) then
      begin
        LReadString := LFileData[LLinesRead];
        if (Trim(LFileData[LLinesRead]) = '') then
        begin
          LLinesRead := LLinesRead - 1;
        end
        else
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');

          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile14Agent.strControlChannelCountErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,6]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if (LReadInteger > MaxAnnualControlStructCount) then
          begin
            LMessage := FAppModules.Language.GetString('TFile14Agent.strAnnualStructureCountErr');
            LMessage := Format(LMessage,[LReadInteger]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LReleaseControlStructureObject.FReferenceChannelsCount.FData := LReadInteger;
            LReleaseControlStructureObject.FReferenceChannelsCount.FInitalised := True;
          end;

          for LReleaseStructCount := 1 to  LReleaseControlStructureObject.FReferenceChannelsCount.FData do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;
            if(Trim(LFileData[LLinesRead]) = '') then
              Break;

            LReference := LReleaseControlStructureObject.AddReference;
            if not Assigned(LReference) then
              Break;

            //Line 6 +++++++++++++++++++++++++++++
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
            LReadString := LFileData[LLinesRead];
            LTempString := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strIFRChannelNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,3,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LReference.FReferenceChannel.FChannelNumber.FData := LReadInteger;
              LReference.FReferenceChannel.FChannelNumber.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceNodeCountErr');
              LMessage := Format(LMessage,[LLinesRead+1,4,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            if (LReadInteger > MaxRefNodeNumber) then  //used to be 100
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strReferenceNodeCountErr');
              LMessage := Format(LMessage,[LLinesRead+1,4,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LReference.FReferenceChannel.FNodeCount.FData := LReadInteger;
              LReference.FReferenceChannel.FNodeCount.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strClassCountErr');
              LMessage := Format(LMessage,[LLinesRead+1,6,7]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            if ((LReadInteger < 0) OR (LReadInteger > 10)) then
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strClassCountErr');
              LMessage := Format(LMessage,[LLinesRead+1,6,7]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LReference.FReferenceChannel.FClassCount.FData := LReadInteger;
              LReference.FReferenceChannel.FClassCount.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile14Agent.strCalculateOptionErr');
              LMessage := Format(LMessage,[LLinesRead+1,8,12]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LReference.FReferenceChannel.FCalculateOption.FData := LReadReal;
              LReference.FReferenceChannel.FCalculateOption.FInitalised := True;
            end;

            //Line 7 +++++++++++++++++++++++++++++
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
            LLinesRead := LLinesRead + 1;
            LReadString := LFileData[LLinesRead];
            if (LReference.FReferenceChannel.FNodeCount.FInitalised) then
            begin
              for LCount := 1 to LReference.FReferenceChannel.FNodeCount.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(LTempString,LReadInteger,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFile14Agent.strRefAnnualNodeNumberErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,6]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LReference.FReferenceNodeNumbers.Add(IntToStr(LReadInteger));
                end;
              end;
            end;

            //Line 8 +++++++++++++++++++++++++++++
            if (LReference.FReferenceChannel.FClassCount.FInitalised) then
            begin
              for LLinesCount := 1 to LReference.FReferenceChannel.FClassCount.FData do
              begin
                TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
                LLinesRead := LLinesRead + 1;
                if(LLinesRead >= LFileData.Count) then
                  Break;
                if(Trim(LFileData[LLinesRead]) = '') then
                  Break;

                LReferenceRelease := LReference.AddReferenceRelease;
                if not Assigned(LReferenceRelease) then
                  Break;

                LReadString := LFileData[LLinesRead];
                LTempString := ExtractFirstSubstring(LReadString);
                Val(LTempString,LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFile14Agent.strAnualInflowErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,3]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                  Break;
                end
                else
                begin
                  LReferenceRelease.FAnualInflow.FData := LReadReal;
                  LReferenceRelease.FAnualInflow.FInitalised := True;
                end;

                for LCount := MinReleaseStructure to MaxReleaseStructure do
                begin
                  LTempString := ExtractFirstSubstring(LReadString);
                  Val(LTempString,LReadReal,LErrorCode);
                  if(LErrorCode <> 0) then
                  begin
                    LMessage := FAppModules.Language.GetString('TFile14Agent.strMonthlyReleaseErr');
                    LMessage := Format(LMessage,[LLinesRead+1,1,3]);
                    AProgressFunction(LMessage,ptError,LStop);
                    if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                    Break;
                  end
                  else
                  begin
                    LReferenceRelease.FMonthlyRelease[LCount].FData := LReadReal;
                    LReferenceRelease.FMonthlyRelease[LCount].FInitalised := True;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      for LLinesCount := LLinesRead + 1 to LFileData.Count - 1 do
        LReleaseControlStructureObject.FF14ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile14Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile14Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLineCount,
  LLinesCount,
  LReleaseStructCount,
  LCount: Integer;
  LF14File :TStringlist;
  LReleaseControlStructureObject: TReleaseControlStructureObject;
  LControlStructureDetails:TReleaseControlStructureDetails;
  LInflowVariableLine :TInflowVariableLine;
  LStop: boolean;
  LReference: TReference;
  LReferenceRelease: TReferenceRelease;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile14Agent.strWritingStarted');
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

    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LReleaseControlStructureObject := ADataObject.FReleaseControlStructureObject;
    if not Assigned(LReleaseControlStructureObject) then Exit;

    LF14File:= TStringList.Create;
    try
      if(LReleaseControlStructureObject.FReleaseControlStructureDetails.Count = 0) and
        (LReleaseControlStructureObject.FReferenceDetails.Count = 0) then
      begin
        LOutString := '   0   1';
        LF14File.Add(LOutString);
        LOutString := '   0';
        LF14File.Add(LOutString);
      end
      else
      begin
        //Line 1 +++++++++++++++++++
        LOutString:='';

        LTempString:= Trim(PadInt(LReleaseControlStructureObject.FControlStructCount));
        LOutString:=LOutString+LTempString;
        if FAppModules.FieldProperties.FieldAvailableInModel('IFRInflowOption') or (FAppModules.Model.ModelName = CPlanning) then
          LOutString := Trim(LOutString)+ ' ' + IntToStr(LReleaseControlStructureObject.FInflowOption.FData);
        LF14File.Add(LOutString);

        //Line 2 +++++++++++++++++++
        for LReleaseStructCount := 0 to  LReleaseControlStructureObject.FReleaseControlStructureDetails.Count - 1 do
        begin
          LControlStructureDetails := TReleaseControlStructureDetails(LReleaseControlStructureObject.FReleaseControlStructureDetails[LReleaseStructCount]);
          if Assigned(LControlStructureDetails) then
          begin
            LOutString :='';
            LTempString:= Trim(PadInt(LControlStructureDetails.FControlStructureData.FIFRChannelNumber)) + ' ';
            LOutString:=LOutString+LTempString;

            LTempString:= Trim(PadInt(LControlStructureDetails.FControlStructureData.FReferenceNodeCount)) + ' ';
            LOutString:=LOutString+LTempString;

            LTempString:= Trim(PadInt(LControlStructureDetails.FControlStructureData.FLagInMonthsCount)) + ' ';
            LOutString:=LOutString+LTempString;

            LTempString:= Trim(PadInt(LControlStructureDetails.FControlStructureData.FPointsCount));
            LOutString:=LOutString+LTempString;

            if LControlStructureDetails.FControlStructureData.FIFRLoss.FInitalised then
            begin
              LTempString:= Trim(PadInt(LControlStructureDetails.FControlStructureData.FIFRLoss));
              LOutString:=LOutString+' '+LTempString;
              if LControlStructureDetails.FControlStructureData.FIFRUnknown.FInitalised then
              begin
                LTempString:= Trim(PadInt(LControlStructureDetails.FControlStructureData.FIFRUnknown));
                LOutString:=LOutString+' '+LTempString;
              end;
            end;

            LF14File.Add(LOutString);

            //Optionaml Line 3 +++++++++++++++++++
            if (LControlStructureDetails.FControlStructureData.FIFRLoss.FInitalised) and
               (LControlStructureDetails.FControlStructureData.FIFRLoss.FData = 1) then
            begin
             LOutString :=UnCommaTextString(LControlStructureDetails.FControlStructureData.FMonthlyIFRLoss.CommaText);
              LF14File.Add(LOutString);
            end;

            //Line 3 +++++++++++++++++++
            LOutString :=Trim(LControlStructureDetails.FControlStructureData.FRefNodeNumber.CommaText);
            if(LOutString <> '') then
              LOutString := StringReplace(LOutString,',',' ',[rfReplaceAll, rfIgnoreCase]);
            LF14File.Add(LOutString);

            //Line 4 +++++++++++++++++++

            for LLineCount := 0 to LControlStructureDetails.FInflowVariableLine.Count-1 do
            begin
              LInflowVariableLine := TInflowVariableLine(LControlStructureDetails.FInflowVariableLine[LLineCount]);
              LOutString:='';
              LTempString := '';
              for LCount := MinReleaseStructure to MaxReleaseStructure do
              begin
                if Assigned(LInflowVariableLine) then
                begin
                  LTempString := LTempString + PadDouble(LInflowVariableLine.FInflowVariable[LCount]) + PadDouble(LInflowVariableLine.FReleaseVariable[LCount]) + ' ';
                end;
              end;
              //LTempString := Trim(LTempString);
              LOutString:=LOutString+LTempString;
              LF14File.Add(LOutString);
            end;
          end;
        end;

        //Line 5 +++++++++++++++++++
        if (FAppModules.FieldProperties.FieldAvailableInModel('IFRInflowOption')) or (FAppModules.Model.ModelName = CPlanning) then
        begin
          LOutString:='';

          LTempString:= Trim(PadInt(LReleaseControlStructureObject.FReferenceChannelsCount));
          LOutString:=LOutString+LTempString;
          LF14File.Add(LOutString);

          for LReleaseStructCount := 0 to  LReleaseControlStructureObject.FReferenceDetails.Count - 1 do
          begin
            LReference := LReleaseControlStructureObject.ReferenceByIndex(LReleaseStructCount);
            if Assigned(LReference) then
            begin
              //Line 6 +++++++++++++++++++
              LOutString :='';
              LTempString:= Trim(PadInt(LReference.FReferenceChannel.FChannelNumber)) + ' ';
              LOutString:=LOutString+LTempString;

              LTempString:= Trim(PadInt(LReference.FReferenceChannel.FNodeCount)) + ' ';
              LOutString:=LOutString+LTempString;

              LTempString:= Trim(PadInt(LReference.FReferenceChannel.FClassCount)) + ' ';
              LOutString:=LOutString+LTempString;

              LTempString:= Trim(PadDouble(LReference.FReferenceChannel.FCalculateOption));
              LOutString:=LOutString+LTempString;

              LF14File.Add(LOutString);

              //Line 7 +++++++++++++++++++
              LOutString :=Trim(LReference.FReferenceNodeNumbers.CommaText);
              if(LOutString <> '') then
                 LOutString := StringReplace(LOutString,',',' ',[rfReplaceAll, rfIgnoreCase]);
              LF14File.Add(LOutString);

              //Line 8 +++++++++++++++++++
              for LLineCount := 0 to LReference.FReferenceReleaseList.Count-1 do
              begin
                LOutString:='';
                LReferenceRelease := LReference.ReferenceReleaseByIndex(LLineCount);
                LTempString := SmartPadDouble(LReferenceRelease.FAnualInflow);
                for LCount := MinReleaseStructure to MaxReleaseStructure do
                begin
                  if LReferenceRelease.FMonthlyRelease[LCount].FInitalised then
                  begin
                    LTempString := LTempString + PadDouble(LReferenceRelease.FMonthlyRelease[LCount]);
                  end;
                end;
                //LTempString := Trim(LTempString);
                LOutString:=LOutString+LTempString;
                LF14File.Add(LOutString);
              end;
            end;
          end;
        end;
      end;

      for LLinesCount := 0 to LReleaseControlStructureObject.FF14ExtraLines.Count -1 do
      begin
        LF14File.Add(LReleaseControlStructureObject.FF14ExtraLines[LLinesCount]);
      end;

      LF14File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      if(Trim(LF14File.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile14Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF14File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
