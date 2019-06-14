//
//
//  UNIT      : Contains TFile02Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/02/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile02Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UReservoirObject,
  UYieldModelDataObject;

type

  TFile02Agent = class(TAbstractFileAgent)
  public
    { Public declarations }
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  System.Contnrs,
  UUtilities,
  UFilesLineTypeObject,
  UErrorHandlingOperations,StrUtils;

function TFile02Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile02Agent.ReadModelDataFromFile';
Var
  LFileData            : TStringList;
  LPointsCount         : TStringList;
  LPointsCountEqual    : Boolean;
  //LInteger :TInteger;
  LMessage,
  LReadString,
  LTempString          : String;
  LCurrentLine,
  LReadInteger,
  LCount,
  LLineCount,
  LStartPos,
  LErrorCode           : Integer;
  LReadReal            : Double;
  LFileIncomplete      : Boolean;
  LReservoirList       :TReservoirObject;
  LReservoir           : TReservoir;
  LFileLineTypesObject : TAbstractFileLineTypesObject;
  LStop                : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile02Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile02Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LReservoirList := ADataObject.FReservoirObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LReservoirList.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData    := TStringList.Create;
    LPointsCount := TStringList.Create;
    try
      //Read the F02 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Check F02 file contains at leat 3 lines
      if(LFileData.Count < 1) then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strF02StructureErr');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      LCurrentLine := 0;
      //___________________________________________________________________________________________________
      //LINE1 ++++++++++++++++++++++++++++++++++
      LReadString := LFileData[LCurrentLine];
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LTempString:=GetSubstring(LReadString,1,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strReserviorNumErr');
        LMessage := Format(LMessage,[1,1,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      {else
      if(LReadInteger > 500) then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strMaxReserviorNumErr');
        LMessage := Format(LMessage,[1,1,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end}
      else
      if(LReadInteger < 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strMinReserviorNumErr');
        LMessage := Format(LMessage,[1,1,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      begin
        LReservoirList.FReserviorNum.FData :=LReadInteger;
        LReservoirList.FReserviorNum.FInitalised := True;
        LReservoirList.FReserviorAndNodesNum.FData :=LReadInteger;
        LReservoirList.FReserviorAndNodesNum.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,8,3);
      if(Trim(LTempString) = '') then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strHydroUnitsErr');
        LMessage := Format(LMessage,[1,8,10]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      if(Uppercase(LTempString) <> 'MCM') and (Uppercase(LTempString) <> 'CMS') then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strHydroUnitsTypeErr');
        LMessage := Format(LMessage,[1,8,10]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit;
      end
      else
      begin
        LReservoirList.FHydroUnits.FData :=LTempString;
        LReservoirList.FHydroUnits.FInitalised := True;
      end;

      LReservoirList.FComment.FData := Copy(LReadString,11,Length(LReadString));
      LReservoirList.FComment.FLength := Length(LReservoirList.FComment.FData);
      LReservoirList.FComment.FInitalised := True;

      LFileIncomplete := False;
      if (LReservoirList.FReserviorNum.FData > 0) then
      begin
        if not LReservoirList.AddReservoirs then
         Exit;

        for LCount := 1 to LReservoirList.FReserviorNum.FData do
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;

          LReservoir := TReservoir(LReservoirList.FReservoirs[LCount]);

          //LINE2 ++++++++++++++++++++++++++++++++++
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');

          LTempString:=GetSubstring(LReadString,1,36);
          if(Trim(LTempString) = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strNameErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,36]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FName.FData :=LTempString;
            LReservoir.FName.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,40,1);
          if((Trim(LTempString) <> '') and (Uppercase(LTempString) <> 'Y') and (Uppercase(LTempString) <> 'N'))then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strSummaryIncludeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,40,40]);
            AProgressFunction(LMessage,ptWarning,LStop);
            LReservoir.FSummaryInclude.FData :=LTempString[1];
            //if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            if(Trim(LTempString) = '') then
              LTempString := 'Y';
            LReservoir.FSummaryInclude.FData :=LTempString[1];
            LReservoir.FSummaryInclude.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,41,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strNodeNoErr');
            LMessage := Format(LMessage,[LCurrentLine+1,41,45]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FNodeNo.FData :=LReadInteger;
            LReservoir.FNodeNo.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,46,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strPenaltyStructureErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FPenaltyStructure.FData :=LReadInteger;
            LReservoir.FPenaltyStructure.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,51,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strMaxPointsErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FMaxPoints.FData :=LReadInteger;
            LReservoir.FMaxPoints.FInitalised := True;
            if(LReadInteger > 0) then
              LReservoir.FNodeType.FData := ntReservoir
            else
              LReservoir.FNodeType.FData := ntNodeWithInflow;
            LReservoir.FNodeType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,56,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode = 0) then
          begin
            LReservoir.FDrainScale.FData :=LReadReal;
            LReservoir.FDrainScale.FInitalised := True;
          end;
        
          if(FAppModules.Model.ModelName = CPlanning) then
          begin
            LTempString:=GetSubstring(LReadString,66,10);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadReal,LErrorCode);
            if(LErrorCode = 0) then
            begin
              LReservoir.FUrbanRunoff.FData :=LReadReal;
              LReservoir.FUrbanRunoff.FInitalised := True;
            end
            else
            begin
              if (LReservoir.FDrainScale.FInitalised) then
              begin
                LReservoir.FUrbanRunoff.FData := LReservoir.FDrainScale.FData;
                LReservoir.FUrbanRunoff.FInitalised := True;
                LMessage := FAppModules.Language.GetString('TFile02Agent.strUrbanRunoffDefaultedToDrainageScale');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptWarning,LStop);
              end;
            end;
          end;

          if(FAppModules.Model.ModelName = CPlanning) then
            LTempString:=GetSubstring(LReadString,76,10)
          else
            LTempString:=GetSubstring(LReadString,66,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode = 0)then
          begin
            LReservoir.FForestScale.FData :=LReadReal;
            //else
            //LReservoir.FForestScale.FData := LReservoir.FDrainScale.FData;
            LReservoir.FForestScale.FInitalised := True;
          end;
          
          if(FAppModules.Model.ModelName = CPlanning) then
            LTempString:=GetSubstring(LReadString,86,10)
          else
            LTempString:=GetSubstring(LReadString,76,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if (LErrorCode = 0)then
          begin
            LReservoir.FIrrigateScale.FData :=LReadReal;
          //else
          //  LReservoir.FIrrigateScale.FData := LReservoir.FDrainScale.FData;
            LReservoir.FIrrigateScale.FInitalised := True;
          end;

          if(FAppModules.Model.ModelName = CPlanning) then
            LReservoir.FComment01.FData := Copy(LReadString,96,Length(LReadString))
          else
            LReservoir.FComment01.FData := Copy(LReadString,86,Length(LReadString));
            
          LReservoir.FComment01.FLength := Length(LReservoir.FComment01.FData);
          LReservoir.FComment01.FInitalised := True;

          //line3+++++++++++++++++++++++++++++++++
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');

          LTempString:=GetSubstring(LReadString,1,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strFullSurfErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FFullSurf.FData :=LReadReal;
            LReservoir.FFullSurf.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strRainCoefErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FRainCoef.FData :=LReadReal;
            LReservoir.FRainCoef.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strCatchRefErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LReservoir.FCatchRef.FData :=LReadInteger;
            LReservoir.FCatchRef.FInitalised := True;
          end;

          if(FAppModules.Model.ModelName = CPlanning) then
          begin
            LTempString:=GetSubstring(LReadString,26,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile02Agent.strNaturalInflowChannelErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              LReservoir.FNaturalInflowChannel.FData :=LReadInteger;
              LReservoir.FNaturalInflowChannel.FInitalised := True;
            end;
          end;

          LTempString := Copy(LReadString,31,Length(LReadString));
          if(Trim(LTempString)  <> '') then
          begin
            LReservoir.FComment04.FData := LTempString;
            LReservoir.FComment04.FLength := Length(LReservoir.FComment04.FData);
            LReservoir.FComment04.FInitalised := True;
          end;

          //line4++++++++++++++++++++++++++++++++
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');

          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile02Agent.strPowerNumErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end;
          LReservoir.FPowerNum.FData :=LReadInteger;
          LReservoir.FPowerNum.FInitalised := True;
          if LReservoir.FPowerNum.FData = 0 then
          begin
            LReservoir.FComment03.FData := Copy(LReadString,6,Length(LReadString));
            LReservoir.FComment03.FLength := Length(LReservoir.FComment03.FData);
            LReservoir.FComment03.FInitalised := True;
          end;

          for LLineCount := MinPowerChannels to LReservoir.FPowerNum.FData do
          begin
            LTempString:=GetSubstring(LReadString,(LLineCount*5)+1,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile02Agent.strPowerChannelsErr');
              LMessage := Format(LMessage,[LCurrentLine+1,(LLineCount*5)+1,(LLineCount*5)+6]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              LReservoir.FPowerChannels[LLineCount].FData :=LReadInteger;
              LReservoir.FPowerChannels[LLineCount].FInitalised := True;
            end;
          end;

          if(LReservoir.FMaxPoints.FData > 0) then
          begin
            //line5++++++++++++++++++++++++++++++++
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;
            LStartPos := -9;

            LPointsCountEqual := True;
            if(LReservoir.FMaxPoints.FData > 8) then
              LPointsCount.CommaText := LFileData[LCurrentLine] + ' ' + LFileData[LCurrentLine + 1]
            else
              LPointsCount.CommaText := LFileData[LCurrentLine];

            LPointsCountEqual := LPointsCountEqual and (LPointsCount.Count = LReservoir.FMaxPoints.FData);
            
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
            for LLineCount := MinPoints to LReservoir.FMaxPoints.FData do
            begin
              LStartPos := LStartPos + 10;
              LTempString:=GetSubstring(LReadString,LStartPos,10);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode = 0) then
              begin
                LReservoir.FSurfElev[LLineCount].FData :=LReadReal;
                LReservoir.FSurfElev[LLineCount].FInitalised := True;
              end;

              if(LLineCount = WrapTextCount) and (LReservoir.FMaxPoints.FData > WrapTextCount) then
              begin
                LCurrentLine := LCurrentLine + 1;
                if (LCurrentLine >= LFileData.Count) then
                begin
                  LFileIncomplete := True;
                  Break;
                end;
                LStartPos :=  -9;
                LReadString := LFileData[LCurrentLine];
                TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
              end;
            //  else
              //  LStartPos := LStartPos + 10;
            end;

            //line6++++++++++++++++++++++++++++++++
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;
            LStartPos := 1;

            if(LReservoir.FMaxPoints.FData > 8) then
              LPointsCount.CommaText := LFileData[LCurrentLine] + ' ' + LFileData[LCurrentLine + 1]
            else
              LPointsCount.CommaText := LFileData[LCurrentLine];

            LPointsCountEqual := LPointsCountEqual and (LPointsCount.Count = LReservoir.FMaxPoints.FData);

            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
            for LLineCount := MinPoints to LReservoir.FMaxPoints.FData do
            begin
              LTempString:=GetSubstring(LReadString,LStartPos,10);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile02Agent.strResVolErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+9]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LReservoir.FResVol[LLineCount].FData :=LReadReal;
                LReservoir.FResVol[LLineCount].FInitalised := True;
              end;

              if(LLineCount = WrapTextCount) and (LReservoir.FMaxPoints.FData > WrapTextCount) then
              begin
                LCurrentLine := LCurrentLine + 1;
                if (LCurrentLine >= LFileData.Count) then
                begin
                  LFileIncomplete := True;
                  Break;
                end;
                LStartPos := 1;
                LReadString := LFileData[LCurrentLine];
                TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
              end
              else
                LStartPos := LStartPos + 10;
            end;

            //line7++++++++++++++++++++++++++++++++
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;
            LStartPos := 1;

            if(LReservoir.FMaxPoints.FData > 8) then
              LPointsCount.CommaText := LFileData[LCurrentLine] + ' ' + LFileData[LCurrentLine + 1]
            else
              LPointsCount.CommaText := LFileData[LCurrentLine];

            LPointsCountEqual := LPointsCountEqual and (LPointsCount.Count = LReservoir.FMaxPoints.FData);

            if not LPointsCountEqual then
            begin
              LMessage := FAppModules.Language.GetString('TFile02Agent.strElevationStorageAndAreaLessMorePoints');
              LMessage := Format(LMessage,[LReservoir.FNodeNo.FData,Trim(LReservoir.FName.FData)]);
              AProgressFunction(LMessage,ptWarning,LStop);
            end;

            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
            for LLineCount := MinPoints to LReservoir.FMaxPoints.FData do
            begin
              LTempString:=GetSubstring(LReadString,LStartPos,10);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile02Agent.strSurfAreaErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+9]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LReservoir.FSurfArea[LLineCount].FData :=LReadReal;
                LReservoir.FSurfArea[LLineCount].FInitalised := True;
              end;

              if(LLineCount = WrapTextCount) and (LReservoir.FMaxPoints.FData > WrapTextCount) then
              begin
                LCurrentLine := LCurrentLine + 1;
                if (LCurrentLine >= LFileData.Count) then
                begin
                  LFileIncomplete := True;
                  Break;
                end;
                LStartPos := 1;
                LReadString := LFileData[LCurrentLine];
                TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
              end
              else
                LStartPos := LStartPos + 10;
            end;

            //line8++++++++++++++++++++++++++++++++
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;
            LStartPos := 1;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
            for LLineCount := 1 to 12 do
            begin
              LTempString:=GetSubstring(LReadString,LStartPos,10);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile02Agent.strMonthEvapErr');
                LMessage := Format(LMessage,[LCurrentLine+1,LStartPos,LStartPos+9]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LReservoir.FMonthEvap[LLineCount].FData :=LReadReal;
                LReservoir.FMonthEvap[LLineCount].FInitalised := True;
              end;

              if(LLineCount = 8) then
              begin
                LCurrentLine := LCurrentLine + 1;
                if (LCurrentLine >= LFileData.Count) then
                begin
                  LFileIncomplete := True;
                  Break;
                end;
                LStartPos := 1;
                LReadString := LFileData[LCurrentLine];
                TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
              end
              else
                LStartPos := LStartPos + 10;
            end;
          end;
        end;
      end;

      for LCount := LCurrentLine + 1 to LFileData.Count - 1 do
        LReservoirList.FF02ExtraLines.Add(LFileData[LCount]);

      if LFileIncomplete then
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strFileIncomplete');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        LMessage := FAppModules.Language.GetString('TFile02Agent.strReadingCompleted');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
        Result := True;
      end;
    finally
      LFileData.Free;
      LPointsCount.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile02Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile02Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LCount,
  LLineCount : Integer;
  LF02File :TStringlist;
  LReservoirList:TReservoirObject;
  LReservoir : TReservoir;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile02Agent.strWritingStarted');
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

    LReservoirList := ADataObject.FReservoirObject;

    LF02File:= TStringList.Create;
    try

      //Line1 ++++++++++++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LReservoirList.FReserviorNum);
      LOutString:=LOutString+LTempString;

      LTempString:=PadString(LReservoirList.FHydroUnits);
      LOutString:=LOutString + '  ' + LTempString;
      if  LReservoirList.FComment.FInitalised then
        LOutString := LOutString + LReservoirList.FComment.FData;
      LF02File.Add(LOutString);

      for LCount := 1 to LReservoirList.FReserviorNum.FData do
      begin
        LReservoir := TReservoir(LReservoirList.FReservoirs[LCount]);

        if not (LReservoir.FNodeType.FData in ReservoirsAndNodeWithInflowSet) then
          Continue;

        LOutString:='';
        //Line2 ++++++++++++++++++++++++++++++++++
        LTempString:=PadString(LReservoir.FName);
        LOutString:=LOutString+LTempString+'   ';

        LTempString:=PadChar(LReservoir.FSummaryInclude);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoir.FNodeNo);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoir.FPenaltyStructure);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoir.FMaxPoints);
        LOutString:=LOutString+LTempString;

        if LReservoir.FDrainScale.FInitalised then
          LTempString:= SmartFloatFormatForFiles(LReservoir.FDrainScale.FData,10,7)
        else
          LTempString := '          ';
        LOutString:=LOutString+LTempString;

        if(FAppModules.Model.ModelName = CPlanning) then
        begin
          if LReservoir.FUrbanRunoff.FInitalised then
            LTempString:= SmartFloatFormatForFiles(LReservoir.FUrbanRunoff.FData,10,7)
          else
            LTempString := '          ';
          LOutString:=LOutString+LTempString;
        end;

        if LReservoir.FForestScale.FInitalised then
          LTempString:= SmartFloatFormatForFiles(LReservoir.FForestScale.FData,10,7)
        else
          LTempString := '          ';
        LOutString:=LOutString+LTempString;

        if LReservoir.FIrrigateScale.FInitalised then
          LTempString:= SmartFloatFormatForFiles(LReservoir.FIrrigateScale.FData,10,7)
        else
          LTempString := '          ';
        LOutString:=LOutString+LTempString;

        {LTempString:= PadDouble(LReservoir.FDrainScale);
        LOutString:=LOutString+LTempString;

        LTempString:= PadDouble(LReservoir.FForestScale);
        LOutString:=LOutString+LTempString;

        LTempString:= PadDouble(LReservoir.FIrrigateScale);
        LOutString:=LOutString+LTempString;
        }
        if LReservoir.FComment01.FInitalised then
          LOutString := LOutString + LReservoir.FComment01.FData;
        LF02File.Add(LOutString);

        //Line3 ++++++++++++++++++++++++++++++++++
        LOutString:='';

        LTempString:= PadDouble(LReservoir.FFullSurf);
        LOutString:=LOutString+LTempString;

        LTempString:= PadDouble(LReservoir.FRainCoef);
        LOutString:=LOutString+LTempString;

        LTempString:= PadInt(LReservoir.FCatchRef);
        LOutString:=LOutString+LTempString;

        if(FAppModules.Model.ModelName = CPlanning) then
        begin
          if LReservoir.FNaturalInflowChannel.FInitalised then
          begin
            LTempString:= PadInt(LReservoir.FNaturalInflowChannel);
            LOutString:=LOutString+LTempString;
          end;
        end;

        if LReservoir.FComment04.FInitalised then
        begin
          LTempString:= LReservoir.FComment04.FData;
          LOutString:=LOutString+LTempString;
        end;

        LF02File.Add(LOutString);

        //Line4 ++++++++++++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LReservoir.FPowerNum);
        if LReservoir.FPowerNum.FData = 0 then
        begin
          LOutString := LTempString;
          if  LReservoir.FComment03.FInitalised then
            LOutString := LTempString + LReservoir.FComment03.FData;
          LF02File.Add(LOutString);
        end
        else
        begin
          for LLineCount := 1 to LReservoir.FPowerNum.FData do
          begin
            LTempString := LTempString+ PadInt(LReservoir.FPowerChannels[LLineCount]);
          end;
          LOutString:=LOutString+LTempString;
          LF02File.Add(LOutString);
        end;

        //Line5 ++++++++++++++++++++++++++++++++++
        if(LReservoir.FMaxPoints.FData > 0) then
        begin
          LOutString:='';
          LTempString := '';
          for LLineCount := MinPoints to LReservoir.FMaxPoints.FData do
          begin
            LTempString := LTempString+
                           PadDouble(LReservoir.FSurfElev[LLineCount]);
            if(LLineCount = WrapTextCount) and (LReservoir.FMaxPoints.FData > WrapTextCount) then
            begin
              LOutString:=LOutString+LTempString;
              LF02File.Add(LOutString);
              LOutString:='';
              LTempString := '';
            end;
          end;
          {if  LReservoir.FComment02.FInitalised then
             LOutString := LOutString + LReservoir.FComment02.FData;}
          LOutString:=LOutString+LTempString;
          LF02File.Add(LOutString);

          //Line6 ++++++++++++++++++++++++++++++++++
          LOutString:='';
          LTempString := '';
          for LLineCount := MinPoints to LReservoir.FMaxPoints.FData do
          begin
            LTempString := LTempString+
                           PadDouble(LReservoir.FResVol[LLineCount]);
            if(LLineCount = WrapTextCount) and (LReservoir.FMaxPoints.FData > WrapTextCount) then
            begin
              LOutString:=LOutString+LTempString;
              LF02File.Add(LOutString);
              LOutString:='';
             LTempString := '';
            end;
          end;
          LOutString:=LOutString+LTempString;
          LF02File.Add(LOutString);

          //Line7 ++++++++++++++++++++++++++++++++++
          LOutString:='';
          LTempString := '';
          for LLineCount := MinPoints to LReservoir.FMaxPoints.FData do
          begin
            LTempString := LTempString+
                           PadDouble(LReservoir.FSurfArea[LLineCount]);
            if(LLineCount = WrapTextCount) and (LReservoir.FMaxPoints.FData > WrapTextCount) then
            begin
              LOutString:=LOutString+LTempString;
              LF02File.Add(LOutString);
              LOutString:='';
              LTempString := '';
            end;
          end;
          LOutString:=LOutString+LTempString;
          LF02File.Add(LOutString);

          //Line8 ++++++++++++++++++++++++++++++++++
          LOutString:='';
          LTempString := '';
          for LLineCount := MinMonths to MaxMonths do
          begin
            LTempString := LTempString + PadDouble(LReservoir.FMonthEvap[LLineCount]);
            if(LLineCount = WrapTextCount) then
            begin
              LOutString:=LOutString+LTempString;
              LF02File.Add(LOutString);
              LOutString:='';
              LTempString := '';
            end;
          end;
          LOutString:=LOutString+LTempString;
          LF02File.Add(LOutString);
        end;
      end;
      for LCount := 0 to LReservoirList.FF02ExtraLines.Count -1 do
      begin
        LF02File.Add(LReservoirList.FF02ExtraLines[LCount]);
      end;

      LF02File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile02Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LF02File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
