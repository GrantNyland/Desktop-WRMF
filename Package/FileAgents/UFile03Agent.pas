//
//
//  UNIT      : Contains TFile03Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/02/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile03Agent;

interface

uses
  Classes, sysutils, vcl.dialogs,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UChannelDescriptionObject,
  UYieldModelDataObject,
  UYMDemandCentreObject;
type

  TFile03Agent = class(TAbstractFileAgent)
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
  StrUtils,
  UUtilities,
  UFilesLineTypeObject,
  VoaimsCom_TLB,
  UErrorHandlingOperations;

function TFile03Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile03Agent.ReadModelDataFromFile';
Var
  LIsModelPlanning               : boolean;
  LFileData                      : TStringList;
  LMessage                       : string;
  LReadString                    : string;
  LTempString                    : string;
  LCurrentLine                   : integer;
  LReadInteger                   : integer;
  LCount                         : integer;
  LCount1                        : integer;
  LCount2                        : integer;
  LCount3                        : integer;
  LCountOne                      : integer;
  LSubCount                      : integer;
  LStartPos                      : integer;
  LErrorCode                     : Integer;
  LReadReal                      : Double;
  LFileIncomplete                : Boolean;
  LChannelDescr                  : TChannelDescrObject;
  LFileLineTypesObject           : TAbstractFileLineTypesObject;
  LStop                          : boolean;
  lChannelPenaltyObject          : TPenaltyChannelObject;
  lMasterChannelObject           : TMasterChannelObject;
  lPowerChannelObject            : TPowerChannelObject;
  lIrrigationObject              : TIrrigationChannelObject;
  lDiversionObject               : TDiversionChannelObject;
  lMinFlowChannelObject          : TMinFlowChannelObject;
  lLossChannelObject             : TLossChannelObject;
  lMinMaxChannelObject           : TMultiPurposeChannelObject;
  lPumpingChannelObject          : TPumpingChannelObject;
  lReturnFlowChannelObject       : TReturnFlowChannelObject;
  lInflowChannelObject           : TInflowChannelObject;
  lDemandChannelObject           : TDemandChannelObject;
  lGeneralChannelObject          : TGeneralChannelObject;
  lIrrigationBlockChannelObject  : TIrrigationBlockChannelObject;
  lWetlandObject                 : TWetlandChannelObject;
  lDemandCentreObject            : TDemandCentreObject;
  lMaxChannelNumber              : integer;
  lReturnFlowChannelCounter      : Integer;
  lReclaimationChannelObject     : TReclaimationChannelObject;
  LGroundwaterObject             : TGroundWaterObject;
  LPath1                         : string;
  LPath2                         : string;
  LLinesCount                    : integer;
begin
  Result := False;
  try
    LLinesCount := 0;
    lMaxChannelNumber  := 99999;
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile03Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile03Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LIsModelPlanning  := (FAppModules.Model.ModelName = CPlanning);
    LChannelDescr     := ADataObject.FChannelDescrObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LChannelDescr.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F03 file
      LFileData.LoadFromFile(AFilename.FileName);

      //Check F03 file contains at leat 13 lines
      if(LFileData.Count < 13) then
      begin
        LMessage := FAppModules.Language.GetString('TFile03Agent.strF03StructureErr');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      LCurrentLine := 0;
      LFileIncomplete := False;

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');
      for LCountOne := 1 to 1 do
      begin

        //Line type 1 ++++++++++++++++++++++++++++++++++
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strPenaltyChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPenaltyChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FPenaltyChannelCount.FData :=LReadInteger;
          LChannelDescr.FPenaltyChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment01.FData := LTempString;
          LChannelDescr.FComment01.FLength := Length(LTempString);
          LChannelDescr.FComment01.FInitalised := True;
        end;


        if (FAppModules.Model.ModelName = CPlanning) then
        begin
          LTempString :=  Trim(Copy(LReadString,6,10));
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowPenaltyNoErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LChannelDescr.FInflowPenaltyNo.FData :=LReadInteger;
            LChannelDescr.FInflowPenaltyNo.FInitalised := True;
          end;
        end;

        //Line type 1a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddPenaltyChannels then Exit;

        for LCount := 1 to LChannelDescr.FPenaltyChannelCount.FData do
        begin
          lChannelPenaltyObject := TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;

          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPenaltyChannelObject_PenaltyTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            if (LReadInteger <= 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strPenaltyChannelObject_PenaltyTypeErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end;
            //else
            //begin
              lChannelPenaltyObject.FPenaltyType.FData :=LReadInteger;
              lChannelPenaltyObject.FPenaltyType.FInitalised := True;
              lChannelPenaltyObject.FPenaltyName.FData := IntToStr(LReadInteger);
              lChannelPenaltyObject.FPenaltyName.FInitalised := True;
            //end;
          end;
          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPenaltyChannelObject_ArcCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 5)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPenaltyChannelObject_ArcCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lChannelPenaltyObject.FArcCount.FData :=LReadInteger;
            lChannelPenaltyObject.FArcCount.FInitalised := True;
          end;

          if (lChannelPenaltyObject.FArcCount.FInitalised) then
          begin
            for LSubCount := 1 to lChannelPenaltyObject.FArcCount.FData do
            begin
              LTempString:=GetSubstring(LReadString,(10*LSubCount)+1,10);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strPenaltyChannelObject_ArcPenaltyErr');
                LMessage := Format(LMessage,[LCurrentLine+1,(10*LSubCount)+1,(10*LSubCount)+10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              if (FAppModules.Model.ModelName = CYield) and (LReadReal < 0)  then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strPenaltyChannelObject_ArcPenaltyErr');
                LMessage := Format(LMessage,[LCurrentLine+1,(10*LSubCount)+1,(10*LSubCount)+10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lChannelPenaltyObject.FArcPenalty[LSubCount].FData       := LReadReal;
                lChannelPenaltyObject.FArcPenalty[LSubCount].FInitalised := True;
              end;
            end;
          end;

          LStartPos := 1 + (10 * (lChannelPenaltyObject.FArcCount.FData +1));
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(LTempString <> '') then
          begin
            lChannelPenaltyObject.FComment.FData := LTempString;
            lChannelPenaltyObject.FComment.FLength := Length(LTempString);
            lChannelPenaltyObject.FComment.FInitalised := True;
          end;
        end;

        //Line type 2 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 2)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxMasterChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FMasterChannelCount.FData :=LReadInteger;
          LChannelDescr.FMasterChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment02.FData := LTempString;
          LChannelDescr.FComment02.FLength := Length(LTempString);
          LChannelDescr.FComment02.FInitalised := True;
        end;

        //Line type 2a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddMasterChannels then Exit;
        for LCount := 1 to LChannelDescr.FMasterChannelCount.FData do
        begin
          lMasterChannelObject := TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > lMaxChannelNumber)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxMasterChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMasterChannelObject.FChannelNumber.FData :=LReadInteger;
            lMasterChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMasterChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lMasterChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMasterChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lMasterChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMasterChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lMasterChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,25,1);
          if(LTempString <> 'W') and (LTempString <> 'P') and (LTempString <> 'H')then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMasterChannelObject_ChannelTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,25,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMasterChannelObject.FChannelType.FData :=LTempString[1];
            lMasterChannelObject.FChannelType.FInitalised := True;
          end;

          LStartPos := 26;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lMasterChannelObject.FComment.FData       := LTempString;
            lMasterChannelObject.FComment.FLength     := Length(LTempString);
            lMasterChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lMasterChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lMasterChannelObject.FChannelName.FLength     := Length(lMasterChannelObject.FChannelName.FData);
              lMasterChannelObject.FChannelName.FInitalised := True;
            end;
          end
          else
          begin
            if (lMasterChannelObject.FChannelType.FInitalised) then
            begin
              if (lMasterChannelObject.FChannelType.FData = 'W') then
              begin
                lMasterChannelObject.FChannelName.FData :=
                  IntToStr(lMasterChannelObject.FChannelNumber.FData) + ' - Master Control (Water)';
                lMasterChannelObject.FChannelName.FInitalised := True;
                lMasterChannelObject.FChannelName.FLength := Length(lMasterChannelObject.FChannelName.FData);
              end;
              if (lMasterChannelObject.FChannelType.FData = 'P') then
              begin
                lMasterChannelObject.FChannelName.FData :=
                  IntToStr(lMasterChannelObject.FChannelNumber.FData) + ' - Master Control (Power)';
                lMasterChannelObject.FChannelName.FInitalised := True;
                lMasterChannelObject.FChannelName.FLength := Length(lMasterChannelObject.FChannelName.FData);
              end;
              if (lMasterChannelObject.FChannelType.FData = 'H') then
              begin
                lMasterChannelObject.FChannelName.FData :=
                  IntToStr(lMasterChannelObject.FChannelNumber.FData) + ' - Master Control (Power allocation control)';
                lMasterChannelObject.FChannelName.FInitalised := True;
                lMasterChannelObject.FChannelName.FLength := Length(lMasterChannelObject.FChannelName.FData);
              end;
            end;
          end;
        end;  

        //Line type 3 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 20)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPowerChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FPowerChannelCount.FData :=LReadInteger;
          LChannelDescr.FPowerChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment03.FData := LTempString;
          LChannelDescr.FComment03.FLength := Length(LTempString);
          LChannelDescr.FComment03.FInitalised := True;
        end;

        //Line type 3a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddPowerChannels then Exit;
        for LCount := 1 to LChannelDescr.FPowerChannelCount.FData do
        begin
          lPowerChannelObject := TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > lMaxChannelNumber)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPowerChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FChannelNumber.FData :=LReadInteger;
            lPowerChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0)then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lPowerChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0)then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lPowerChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lPowerChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > lMaxChannelNumber)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPowerChannelObject_SpillChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FSpillChannelNumber.FData :=LReadInteger;
            lPowerChannelObject.FSpillChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,26,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillUpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillUpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FSpillUpNodeNumber.FData :=LReadInteger;
            lPowerChannelObject.FSpillUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,31,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillDownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillDownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FSpillDownNodeNumber.FData :=LReadInteger;
            lPowerChannelObject.FSpillDownNodeNumber.FLength :=Length(LTempString);
            lPowerChannelObject.FSpillDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,36,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillPenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,36,40]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPowerChannelObject_SpillPenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,36,40]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPowerChannelObject.FSpillPenaltyStructType.FData :=LReadInteger;
            lPowerChannelObject.FSpillPenaltyStructType.FLength :=Length(LTempString);
            lPowerChannelObject.FSpillPenaltyStructType.FInitalised := True;
          end;

          LStartPos := 41;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lPowerChannelObject.FComment.FData := LTempString;
            lPowerChannelObject.FComment.FLength := Length(LTempString);
            lPowerChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lPowerChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lPowerChannelObject.FChannelName.FLength     := Length(lPowerChannelObject.FChannelName.FData);
              lPowerChannelObject.FChannelName.FInitalised := True;
            end;
          end;

          //Line type 3b ++++++++++++++++++++++++++++++++++
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3b');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString(
              'TFile03Agent.strPowerChannelObject_DownStreamPowerChannelCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger > 10) OR (LReadInteger < 0)) then
          begin
            LMessage := FAppModules.Language.GetString(
              'TFile03Agent.strPowerChannelObject_DownStreamPowerChannelCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannelCount.FData :=
              LReadInteger;
            TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannelCount.FInitalised :=
              True;
          end;

          if (TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannelCount.FInitalised) then
          begin
            for LSubCount := 1 to
              TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannelCount.FData do
            begin
              LTempString:=GetSubstring(LReadString,(5*LSubCount)+1,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString(
                  'TFile03Agent.strPowerChannelObject_DownStreamPowerChannelsErr');
                LMessage := Format(LMessage,[LCurrentLine+1,(5*LSubCount)+1,(5*LSubCount)+5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannels[LSubCount].FData :=
                  LReadInteger;
                TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannels[LSubCount].FInitalised :=
                  True;
              end;
            end;
          end;
        end;

        //Line type 4 ++++++++++++++++++++++++++++++++++
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
          LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if ((LReadInteger < 0) OR (LReadInteger > 20)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxIrrigationChannelErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FIrrigationChannelCount.FData :=LReadInteger;
          LChannelDescr.FIrrigationChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment04.FData := LTempString;
          LChannelDescr.FComment04.FLength := Length(LTempString);
          LChannelDescr.FComment04.FInitalised := True;
        end;

        //Line type 4a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddIrrigationChannels then Exit;
        for LCount := 1 to LChannelDescr.FIrrigationChannelCount.FData do
        begin
          lIrrigationObject := TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_IrrigationNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_IrrigationNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FIrrigationNodeNumber.FData := LReadInteger;
            lIrrigationObject.FIrrigationNodeNumber.FInitalised := True;
            lIrrigationObject.FComment.FData := IntToStr(LReadInteger);
            lIrrigationObject.FComment.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_IrrigationNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,5,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_IrrigationNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,5,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FUpstreamNodeNumber.FData := LReadInteger;
            lIrrigationObject.FUpstreamNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_DiversionChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_DiversionChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FDiversionChannelNumber.FData := LReadInteger;
            lIrrigationObject.FDiversionChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_IrrigationPenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_IrrigationPenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FIrrigationPenaltyStructType.FData := LReadInteger;
            lIrrigationObject.FIrrigationPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_DownStreamNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0)then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_DownStreamNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FDownStreamNodeNumber.FData := LReadInteger;
            lIrrigationObject.FDownStreamNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,26,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_ReturnChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > lMaxChannelNumber)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_ReturnChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FReturnChannelNumber.FData := LReadInteger;
            lIrrigationObject.FReturnChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,31,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString(
              'TFile03Agent.strIrrigationChannelObject_ReturnPenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_ReturnPenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FReturnPenaltyStructType.FData := LReadInteger;
            lIrrigationObject.FReturnPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,36,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString(
              'TFile03Agent.strIrrigationChannelObject_ConsumptiveChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,36,40]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > lMaxChannelNumber))then
          begin
            LMessage := FAppModules.Language.GetString(
              'TFile03Agent.strIrrigationChannelObject_ConsumptiveChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,36,40]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FConsumptiveChannelNumber.FData := LReadInteger;
            lIrrigationObject.FConsumptiveChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,41,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString(
              'TFile03Agent.strIrrigationChannelObject_ConsumptivePenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,41,45]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_ConsumptivePenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,41,45]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FConsumptivePenaltyStructType.FData := LReadInteger;
            lIrrigationObject.FConsumptivePenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,46,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_RelaxationDemandErr');
            LMessage := Format(LMessage,[LCurrentLine+1,46,50]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (NOT (LReadInteger in [0,1,2])) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationChannelObject_RelaxationDemandErr');
            LMessage := Format(LMessage,[LCurrentLine+1,46,50]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lIrrigationObject.FRelaxationDemand.FData := LReadInteger;
            lIrrigationObject.FRelaxationDemand.FInitalised := True;
          end;

          LStartPos := 51;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lIrrigationObject.FComment.FData := LTempString;
            lIrrigationObject.FComment.FLength := Length(LTempString);
            lIrrigationObject.FComment.FInitalised := True;
            lIrrigationObject.FConsumptiveChannelName.FData       := ExtractChannelNameFromComment(LTempString);
            lIrrigationObject.FConsumptiveChannelName.FLength     := Length(lIrrigationObject.FConsumptiveChannelName.FData);
            lIrrigationObject.FConsumptiveChannelName.FInitalised := True;
          end;
        end;

        //Line type 5 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 100)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxDiversionChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FDiversionChannelCount.FData :=LReadInteger;
          LChannelDescr.FDiversionChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment05.FData := LTempString;
          LChannelDescr.FComment05.FLength := Length(LTempString);
          LChannelDescr.FComment05.FInitalised := True;
        end;

        //Line type 5a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddDiversionChannels then Exit;
        for LCount := 1 to LChannelDescr.FDiversionChannelCount.FData do
        begin
          lDiversionObject := TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxDiversionChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDiversionObject.FChannelNumber.FData :=LReadInteger;
            lDiversionObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDiversionObject.FUpNodeNumber.FData :=LReadInteger;
            lDiversionObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDiversionObject.FDownNodeNumber.FData :=LReadInteger;
            lDiversionObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDiversionObject.FPenaltyStructType.FData :=LReadInteger;
            lDiversionObject.FPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_ChannelTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 1) OR (LReadInteger > 3)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDiversionChannelObject_ChannelTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDiversionObject.FChannelType.FData :=LReadInteger;
            lDiversionObject.FChannelType.FInitalised := True;
          end;

          LStartPos := 26;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lDiversionObject.FComment.FData := LTempString;
            lDiversionObject.FComment.FLength := Length(LTempString);
            lDiversionObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lDiversionObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lDiversionObject.FChannelName.FLength     := Length(lDiversionObject.FChannelName.FData);
              lDiversionObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 6 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 100)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxMinFlowChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FMinFlowChannelCount.FData :=LReadInteger;
          LChannelDescr.FMinFlowChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment06.FData := LTempString;
          LChannelDescr.FComment06.FLength := Length(LTempString);
          LChannelDescr.FComment06.FInitalised := True;
        end;

        //Line type 6a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddMinFlowChannels then Exit;
        for LCount := 1 to LChannelDescr.FMinFlowChannelCount.FData do
        begin
          lMinFlowChannelObject := TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxMinFlowChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinFlowChannelObject.FChannelNumber.FData :=LReadInteger;
            lMinFlowChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinFlowChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lMinFlowChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinFlowChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lMinFlowChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMinFlowChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinFlowChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lMinFlowChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LStartPos := 21;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lMinFlowChannelObject.FComment.FData := LTempString;
            lMinFlowChannelObject.FComment.FLength := Length(LTempString);
            lMinFlowChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lMinFlowChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lMinFlowChannelObject.FChannelName.FLength     := Length(lMinFlowChannelObject.FChannelName.FData);
              lMinFlowChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 7 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 100)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxLossChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FLossChannelCount.FData :=LReadInteger;
          LChannelDescr.FLossChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment07.FData := LTempString;
          LChannelDescr.FComment07.FLength := Length(LTempString);
          LChannelDescr.FComment07.FInitalised := True;
        end;

        //Line type 7a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddLossChannels then Exit;
        for LCount := 1 to LChannelDescr.FLossChannelCount.FData do
        begin
          lLossChannelObject := TLossChannelObject(LChannelDescr.FLossChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxLossChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lLossChannelObject.FChannelNumber.FData :=LReadInteger;
            lLossChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lLossChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lLossChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lLossChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lLossChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strLossChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lLossChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lLossChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if (LErrorCode = 0) then
          begin
            lLossChannelObject.FChannelType.FData :=LReadInteger;
            lLossChannelObject.FChannelType.FLength := Length(LTempString);
            lLossChannelObject.FChannelType.FInitalised := True;
          end
          else
          begin
            lLossChannelObject.FChannelType.FData := 0;
            lLossChannelObject.FChannelType.FInitalised := True;
          end;
          LTempString:=GetSubstring(LReadString,26,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode = 0) then
          begin
            lLossChannelObject.FReference.FData :=LReadInteger;
            lLossChannelObject.FReference.FLength := Length(LTempString);
            lLossChannelObject.FReference.FInitalised := True;
          end;
          LStartPos := 31;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lLossChannelObject.FComment.FData := LTempString;
            lLossChannelObject.FComment.FLength := Length(LTempString);
            lLossChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lLossChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lLossChannelObject.FChannelName.FLength     := Length(lLossChannelObject.FChannelName.FData);
              lLossChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 8 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 800)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxMultiPurposeChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FMultiPurposeChannelCount.FData :=LReadInteger;
          LChannelDescr.FMultiPurposeChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment08.FData := LTempString;
          LChannelDescr.FComment08.FLength := Length(LTempString);
          LChannelDescr.FComment08.FInitalised := True;
        end;

        //Line type 8a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddMultiPurposeChannels then Exit;
        for LCount := 1 to LChannelDescr.FMultiPurposeChannelCount.FData do
        begin
          lMinMaxChannelObject := TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxMultiPurposeChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinMaxChannelObject.FChannelNumber.FData :=LReadInteger;
            lMinMaxChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinMaxChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lMinMaxChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinMaxChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lMinMaxChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMultiPurposeChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lMinMaxChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lMinMaxChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LStartPos := 21;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lMinMaxChannelObject.FComment.FData := LTempString;
            lMinMaxChannelObject.FComment.FLength := Length(LTempString);
            lMinMaxChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lMinMaxChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lMinMaxChannelObject.FChannelName.FLength     := Length(lMinMaxChannelObject.FChannelName.FData);
              lMinMaxChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 9 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 20)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPumpingChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FPumpingChannelCount.FData :=LReadInteger;
          LChannelDescr.FPumpingChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment09.FData := LTempString;
          LChannelDescr.FComment09.FLength := Length(LTempString);
          LChannelDescr.FComment09.FInitalised := True;
        end;

        //Line type 9a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddPumpingChannels then Exit;
        for LCount := 1 to LChannelDescr.FPumpingChannelCount.FData do
        begin
          lPumpingChannelObject := TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxPumpingChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPumpingChannelObject.FChannelNumber.FData :=LReadInteger;
            lPumpingChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPumpingChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lPumpingChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPumpingChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lPumpingChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPumpingChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lPumpingChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_PumpingHeadErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPumpingChannelObject.FPumpingHead.FData :=LReadReal;
            lPumpingChannelObject.FPumpingHead.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,31,10);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strPumpingChannelObject_EfficiencyErr');
            LMessage := Format(LMessage,[LCurrentLine+1,31,40]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lPumpingChannelObject.FEfficiency.FData :=LReadReal;
            lPumpingChannelObject.FEfficiency.FInitalised := True;
          end;

          LStartPos := 41;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lPumpingChannelObject.FComment.FData := LTempString;
            lPumpingChannelObject.FComment.FLength := Length(LTempString);
            lPumpingChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lPumpingChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lPumpingChannelObject.FChannelName.FLength     := Length(lPumpingChannelObject.FChannelName.FData);
              lPumpingChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 10 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'10');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 100)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxInflowChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FInflowChannelCount.FData :=LReadInteger;
          LChannelDescr.FInflowChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment10.FData := LTempString;
          LChannelDescr.FComment10.FLength := Length(LTempString);
          LChannelDescr.FComment10.FInitalised := True;
        end;

        //Line type 10a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddInflowChannels then Exit;
        for LCount := 1 to LChannelDescr.FInflowChannelCount.FData do
        begin
          lInflowChannelObject := TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'10a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxInflowChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lInflowChannelObject.FChannelNumber.FData :=LReadInteger;
            lInflowChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lInflowChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lInflowChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lInflowChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lInflowChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strInflowChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lInflowChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lInflowChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LStartPos := 21;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lInflowChannelObject.FComment.FData := LTempString;
            lInflowChannelObject.FComment.FLength := Length(LTempString);
            lInflowChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lInflowChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lInflowChannelObject.FChannelName.FLength     := Length(lInflowChannelObject.FChannelName.FData);
              lInflowChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 11 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'11');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 200)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxDemandChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FDemandChannelCount.FData :=LReadInteger;
          LChannelDescr.FDemandChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment11.FData := LTempString;
          LChannelDescr.FComment11.FLength := Length(LTempString);
          LChannelDescr.FComment11.FInitalised := True;
        end;

        //Line type 11a ++++++++++++++++++++++++++++++++++
        LPath1 := '';
        LPath2 := '';
        if not LChannelDescr.AddDemandChannels then Exit;
        for LCount := 1 to LChannelDescr.FDemandChannelCount.FData do
        begin
          lDemandChannelObject := TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'11a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxDemandChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDemandChannelObject.FChannelNumber.FData :=LReadInteger;
            lDemandChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDemandChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lDemandChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDemandChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lDemandChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDemandChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lDemandChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,21,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_GaugeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDemandChannelObject.FGaugeNumber.FData :=LReadInteger;
            lDemandChannelObject.FGaugeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,30,1);
          if(LTempString <> 'S') and (LTempString <> 'H') then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_ChannelTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,30,30]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lDemandChannelObject.FStochastic.FData :=LTempString[1];
            lDemandChannelObject.FStochastic.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,31,50);
          if(Trim(LTempString) <> '') then
          begin
            if not FilePathIsDosCompatible(FAppModules,Trim(LTempString)) then
            begin
              LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
              LMessage := Format(LMessage,[Trim(LTempString)]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            If not FileExists(Trim(LTempString)) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_strFileNoExist');
              LMessage := Format(LMessage,[LTempString]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lDemandChannelObject.FFullname.FData   := LTempString;
              lDemandChannelObject.FFullname.FLength := Length(lDemandChannelObject.FFullname.FData);
              lDemandChannelObject.FFullname.FInitalised := True;
              if not ADataObject.FPathsObject.SpecifiedDemandPath.FInitalised then
              begin
                ADataObject.FPathsObject.SpecifiedDemandPath.FData := ExtractFilePath(LTempString);
                ADataObject.FPathsObject.SpecifiedDemandPath.FLength := Length(ADataObject.FPathsObject.SpecifiedDemandPath.FData);
                ADataObject.FPathsObject.SpecifiedDemandPath.FInitalised := True;
              end;

              if(LPath1 = '') then
              begin
                LPath1 := UpperCase(ExtractFilePath(LTempString));
                LPath2 := LPath1;
              end
              else
              begin
                if(LPath1 <> UpperCase(ExtractFilePath(LTempString))) then
                begin
                  LPath2 := UpperCase(ExtractFilePath(LTempString));
                end;
              end;
            end;
          end;

          LStartPos := 81;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lDemandChannelObject.FComment.FData := LTempString;
            lDemandChannelObject.FComment.FLength := Length(LTempString);
            lDemandChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lDemandChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lDemandChannelObject.FChannelName.FLength     := Length(lDemandChannelObject.FChannelName.FData);
              lDemandChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        if(FAppModules.Model.ModelName = CYield) and (LPath1 <> LPath2) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strDemandChannelObject_PathsErr');
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end;

        //Line type 12 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'12');
        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 800)) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxGeneralChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          LChannelDescr.FGeneralChannelCount.FData :=LReadInteger;
          LChannelDescr.FGeneralChannelCount.FInitalised := True;
        end;

        LTempString := Copy(LReadString,6,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment12.FData := LTempString;
          LChannelDescr.FComment12.FLength := Length(LTempString);
          LChannelDescr.FComment12.FInitalised := True;
        end;

        //Line type 12a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddGeneralChannels then Exit;
        for LCount := 1 to LChannelDescr.FGeneralChannelCount.FData do
        begin
          lGeneralChannelObject := TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]);
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'12a');
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if(LReadInteger > lMaxChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxGeneralChannelObject_ChannelNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lGeneralChannelObject.FChannelNumber.FData :=LReadInteger;
            lGeneralChannelObject.FChannelNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_UpNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lGeneralChannelObject.FUpNodeNumber.FData :=LReadInteger;
            lGeneralChannelObject.FUpNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (LReadInteger < 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_DownNodeNumberErr');
            LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lGeneralChannelObject.FDownNodeNumber.FData :=LReadInteger;
            lGeneralChannelObject.FDownNodeNumber.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 150)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strGeneralChannelObject_PenaltyStructTypeErr');
            LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            lGeneralChannelObject.FPenaltyStructType.FData :=LReadInteger;
            lGeneralChannelObject.FPenaltyStructType.FInitalised := True;
          end;

          LStartPos := 21;
          LTempString := Copy(LReadString,LStartPos,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            lGeneralChannelObject.FComment.FData := LTempString;
            lGeneralChannelObject.FComment.FLength := Length(LTempString);
            lGeneralChannelObject.FComment.FInitalised := True;
            if not LIsModelPlanning then
            begin
              lGeneralChannelObject.FChannelName.FData       := ExtractChannelNameFromComment(LTempString);
              lGeneralChannelObject.FChannelName.FLength     := Length(lGeneralChannelObject.FChannelName.FData);
              lGeneralChannelObject.FChannelName.FInitalised := True;
            end;
          end;
        end;

        //Line type 13 for version 7 ++++++++++++++++++++++++++++++++++
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'13');
          LTempString := GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage  := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockCountErr');
            LMessage  := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          if ((LReadInteger < 0) OR (LReadInteger > 200)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxIrrigationBlockChannelErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            LChannelDescr.FIrrigationBlockCount.FData       := LReadInteger;
            LChannelDescr.FIrrigationBlockCount.FInitalised := True;
          end;

          LTempString := Copy(LReadString,6,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            LChannelDescr.FComment13.FData := LTempString;
            LChannelDescr.FComment13.FLength := Length(LTempString);
            LChannelDescr.FComment13.FInitalised := True;
          end;

          //Line type 13a for version 7 ++++++++++++++++++++++++++++++++++
          if not LChannelDescr.AddIrrigationBlockChannels then Exit;
          for LCount := 1 to LChannelDescr.FIrrigationBlockCount.FData do
          begin
            lIrrigationBlockChannelObject := TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]);
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'13a');

            //Upstream node number for abstraction channel
            LTempString := GetSubstring(LReadString,1,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockUpstreamNodeNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FUpNodeNumber.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FUpNodeNumber.FInitalised := True;
            end;

            //Downstream node number for return flow channel
            LTempString:=GetSubstring(LReadString,6,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockDownstreamNodeNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockDownstreamNodeNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FDownNodeNumber.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FDownNodeNumber.FInitalised := True;
            end;

            //Irrigation block abstraction channel number
            LTempString:=GetSubstring(LReadString,11,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockAbstractionChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockAbstractionChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FAbstractChannelNr.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FAbstractChannelNr.FInitalised := True;
            end;

            //Penalty structure type for abstraction channel
            LTempString:=GetSubstring(LReadString,16,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockAbstractionPenaltyErr');
              LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockAbstractionPenaltyErr');
              LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FAbstractPenaltyType.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FAbstractPenaltyType.FInitalised := True;
            end;

            //Irrigation block return flow channel number
            LTempString := GetSubstring(LReadString,21,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockReturnFlowChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockReturnFlowChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FReturnFlowChannelNr.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FReturnFlowChannelNr.FInitalised := True;
            end;

            //Penalty structure type for return flow channel
            LTempString := GetSubstring(LReadString,26,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockReturnFlowPenaltyErr');
              LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockReturnFlowPenaltyErr');
              LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FReturnFlowPenaltyType.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FReturnFlowPenaltyType.FInitalised := True;
            end;

            //Irrigation block number
            LTempString := GetSubstring(LReadString,31,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockReturnFlowPenaltyErr');
              LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strIrrigationBlockReturnFlowPenaltyErr');
              LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lIrrigationBlockChannelObject.FBlockNumber.FData       := LReadInteger;
              lIrrigationBlockChannelObject.FBlockNumber.FInitalised := True;
            end;

            LStartPos := 36;
            LTempString := Copy(LReadString,LStartPos,Length(LReadString));
            if(Trim(LTempString) <> '') then
            begin
              lIrrigationBlockChannelObject.FComment.FData := LTempString;
              lIrrigationBlockChannelObject.FComment.FLength := Length(LTempString);
              lIrrigationBlockChannelObject.FComment.FInitalised := True;
            end;
          end;
        end;

        //Line type 14 for version 7 (wetland) ++++++++++++++++++++++++++++++++++
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'14');
          LTempString := GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage  := FAppModules.Language.GetString('TFile03Agent.strWetlandCountErr');
            LMessage  := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          {else
          if ((LReadInteger < 0) OR (LReadInteger > 100)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxWetlandChannelErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end}
          else
          begin
            LChannelDescr.FWetlandCount.FData       := LReadInteger;
            LChannelDescr.FWetlandCount.FInitalised := True;
          end;

          LTempString := Copy(LReadString,6,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            LChannelDescr.FComment14.FData := LTempString;
            LChannelDescr.FComment14.FLength := Length(LTempString);
            LChannelDescr.FComment14.FInitalised := True;
          end;

          //Line type 14a for version 7 ++++++++++++++++++++++++++++++++++
          if not LChannelDescr.AddWetlandChannels then Exit;
          for LCount := 1 to LChannelDescr.FWetlandCount.FData do
          begin
            lWetlandObject := TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]);
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'14a');

            //Wetland node number
            LTempString := GetSubstring(LReadString,1,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandNodeNumberErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FNodeNumber.FData       := LReadInteger;
              lWetlandObject.FNodeNumber.FInitalised := True;
            end;

            //Upstream node number for wetland inflow channel
            LTempString:=GetSubstring(LReadString,6,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandUpNodeNumberErr');
              LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandUpNodeNumberErr');
              LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FUpNodeNumber.FData       := LReadInteger;
              lWetlandObject.FUpNodeNumber.FInitalised := True;
            end;

            //Downstream node number for wetland outflow channel
            LTempString:=GetSubstring(LReadString,11,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandDownNodeNumberErr');
              LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandDownNodeNumberErr');
              LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FDownNodeNumber.FData       := LReadInteger;
              lWetlandObject.FDownNodeNumber.FInitalised := True;
            end;

            //Wetland inflow channel number
            LTempString:=GetSubstring(LReadString,16,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandInflowChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandInflowChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FInflowChannelNr.FData       := LReadInteger;
              lWetlandObject.FInflowChannelNr.FInitalised := True;
            end;

            //Penalty structure type for wetland inflow channel
            LTempString := GetSubstring(LReadString,21,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandInflowPenaltyTypeErr');
              LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandInflowPenaltyTypeErr');
              LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FInflowPenaltyType.FData       := LReadInteger;
              lWetlandObject.FInflowPenaltyType.FInitalised := True;
            end;

            //Wetland outflow channel number
            LTempString := GetSubstring(LReadString,26,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandOutflowChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandOutflowChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,26,30]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FOutflowChannelNr.FData       := LReadInteger;
              lWetlandObject.FOutflowChannelNr.FInitalised := True;
            end;

            //Penalty structure type for wetland outflow channel
            LTempString := GetSubstring(LReadString,31,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandOutflowPenaltyTypeErr');
              LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if (LReadInteger < 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strWetlandOutflowPenaltyTypeErr');
              LMessage := Format(LMessage,[LCurrentLine+1,31,35]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lWetlandObject.FOutflowPenaltyType.FData       := LReadInteger;
              lWetlandObject.FOutflowPenaltyType.FInitalised := True;
            end;
          end;
        end;

        //Line type 15 for version 7 (demand centre) ++++ not yet implemented in Planning Model+++++++++++++++++++++
        if (FAppModules.StudyArea.ModelVersion = '7') and (FAppModules.Model.ModelName = CYield) then
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15');
          LTempString := GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage  := FAppModules.Language.GetString('TFile03Agent.strDemandCentreCountErr');
            LMessage  := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          {else
          if ((LReadInteger < 0) OR (LReadInteger > 100)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxDemandCentreErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end}
          else
          begin
            LChannelDescr.FDemandCentreCount.FData       := LReadInteger;
            LChannelDescr.FDemandCentreCount.FInitalised := True;
          end;

          LTempString := Copy(LReadString,6,Length(LReadString));
          if(Trim(LTempString) <> '') then
          begin
            LChannelDescr.FComment15.FData := LTempString;
            LChannelDescr.FComment15.FLength := Length(LTempString);
            LChannelDescr.FComment15.FInitalised := True;
          end;

          //Line type 15a for version 7 ++++++++++++++++++++++++++++++++++
          if not LChannelDescr.AddDemandCentreChannels then Exit;
          lReturnFlowChannelCounter := 1;
          for LCount := 1 to LChannelDescr.FDemandCentreCount.FData  do
          begin
            lDemandCentreObject := TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]);
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;

            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15a');

            //Demand centre node number
            LTempString := GetSubstring(LReadString,1,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strCentreNodeNoErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lDemandCentreObject.FNodeNumber.FData       := LReadInteger;
              lDemandCentreObject.FNodeNumber.FInitalised := True;
            end;

            //Demand centre consumptive water use channel nr
            LTempString := GetSubstring(LReadString,6,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strConsumptiveChannelNrErr');
              LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lDemandCentreObject.FConsumptiveChannelNr.FData       := LReadInteger;
              lDemandCentreObject.FConsumptiveChannelNr.FInitalised := True;
            end;

            //Demand centre No of return flow channels
            LTempString := GetSubstring(LReadString,11,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) or (LReadInteger <= 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strNoOfReturnFlowChannelsErr');
              LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lDemandCentreObject.FNoOfReturnFlowChannels.FData       := LReadInteger;
              lDemandCentreObject.FNoOfReturnFlowChannels.FInitalised := True;
            end;

            //Demand centre No of reclaimation plant loss channels
            LTempString := GetSubstring(LReadString,16,5);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strNrOfPlantLossChannelsErr');
              LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              lDemandCentreObject.FNoOfReclaimationChannels.FData       := LReadInteger;
              lDemandCentreObject.FNoOfReclaimationChannels.FInitalised := True;
              // Need this
              LChannelDescr.FReclaimationChannelCount.FData       := LReadInteger;
              LChannelDescr.FReclaimationChannelCount.FInitalised := True;
            end;

            // Line 15 b
            for LCount1 := 1 to lDemandCentreObject.FNoOfReturnFlowChannels.FData do
            begin
              if not LChannelDescr.AddReturnFlowChannels then Exit;
              lReturnFlowChannelObject := TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[lReturnFlowChannelCounter]);
              Inc(lReturnFlowChannelCounter);
              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15b');

              //demand centre return flow channel number
              LTempString := GetSubstring(LReadString,1,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strReturnFlowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lReturnFlowChannelObject.FChannelNr.FData       := LReadInteger;
                lReturnFlowChannelObject.FChannelNr.FInitalised := True;
              end;

              //demand centre return flow downstream node number
              LTempString := GetSubstring(LReadString,6,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strReturnFlowDownNodeNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lReturnFlowChannelObject.FDownStreamNodeNr.FData       := LReadInteger;
                lReturnFlowChannelObject.FDownStreamNodeNr.FInitalised := True;
              end;

              // Need to set the upstream node number to the demand centre node number
              lReturnFlowChannelObject.FUpstreamNodeNr.FData       := LReadInteger;
              lReturnFlowChannelObject.FUpstreamNodeNr.FInitalised := True;

              //demand centre return flow penalty structure type
              LTempString := GetSubstring(LReadString,11,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strReturnFlowPenaltyStructErr');
                LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lReturnFlowChannelObject.FPenaltyStructureType.FData       := LReadInteger;
                lReturnFlowChannelObject.FPenaltyStructureType.FInitalised := True;
              end;

              lReturnFlowChannelObject.FDemandCentreNodeNr.FData       := lDemandCentreObject.FNodeNumber.FData;
              lReturnFlowChannelObject.FDemandCentreNodeNr.FInitalised := True;
            end;
            //=============================================================================
            //=============================================================================
            for LCount2 := 1 to lDemandCentreObject.FNoOfReclaimationChannels.FData do
            begin
              if not LChannelDescr.AddReclaimationChannels then Exit;
              LCount3                 := LChannelDescr.FReclaimationChannelList.Count - 1;
              lReclaimationChannelObject := TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount3]);
              LCurrentLine            := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15c');

              //demand centre reclaimation channel no
              LTempString := GetSubstring(LReadString,1,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strPlantLossChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lReclaimationChannelObject.FChannelNr.FData       := LReadInteger;
                lReclaimationChannelObject.FChannelNr.FInitalised := True;
              end;

              //demand centre reclaimation downstream node nr
              LTempString := GetSubstring(LReadString,6,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strPlantLossDownNodeNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lReclaimationChannelObject.FDownStreamNodeNr.FData       := LReadInteger;
                lReclaimationChannelObject.FDownStreamNodeNr.FInitalised := True;
              end;

              // Need to set the upstream node number to the demand centre node number
              lReclaimationChannelObject.FUpstreamNodeNr.FData       := lDemandCentreObject.FNodeNumber.FData;
              lReclaimationChannelObject.FUpstreamNodeNr.FInitalised := True;

              //demand centre reclaimation penalty structure
              LTempString := GetSubstring(LReadString,11,5);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strPlantLossPenaltyStructErr');
                LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                lReclaimationChannelObject.FPenaltyStructureType.FData       := LReadInteger;
                lReclaimationChannelObject.FPenaltyStructureType.FInitalised := True;
              end;

              lReclaimationChannelObject.FDemandCentreNodeNr.FData       := lDemandCentreObject.FNodeNumber.FData;
              lReclaimationChannelObject.FDemandCentreNodeNr.FInitalised := True;
            end;
          end;
        end;

        if (FAppModules.StudyArea.ModelVersion = '7') and (FAppModules.Model.ModelName = CYield) and
            TYieldModelDataObject(FAppModules.Model.ModelData).ImplementedNetworkFeatures.GroundWaterFeatureImplemented then
        begin
          //Line type 16 for version 7 (Groundwater) ++++++++++++++++++++++++++++++++++
          if (FAppModules.StudyArea.ModelVersion = '7') then
          begin
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;
            LReadString := LFileData[LCurrentLine];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16');
            //LTempString := GetSubstring(LReadString,1,5);
            LTempString := ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            Val(LTempString,LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage  := FAppModules.Language.GetString('TFile03Agent.strGroundwaterCountErr');
              LMessage  := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            if ((LReadInteger < 0) OR (LReadInteger > 100)) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxGroundwaterErr');
              LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end
            else
            begin
              LChannelDescr.FGroundWaterCount.FData       := LReadInteger;
              LChannelDescr.FGroundWaterCount.FInitalised := True;
            end;

            LTempString := Copy(LReadString,6,Length(LReadString));
            if(Trim(LTempString) <> '') then
            begin
              LChannelDescr.FComment16.FData := LTempString;
              LChannelDescr.FComment16.FLength := Length(LTempString);
              LChannelDescr.FComment16.FInitalised := True;
            end;

            //Line type 16a for version 7 ++++++++++++++++++++++++++++++++++
            if not LChannelDescr.AddGroundWaterChannels then Exit;

            for LCount := 1 to LChannelDescr.FGroundWaterCount.FData do
            begin
              LGroundwaterObject := TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]);
              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16a');

              //Reference Node Number
              //LTempString := GetSubstring(LReadString,1,5);
              {LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strReferenceNodeNumberNoErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FReferenceNodeNr.FData       := LReadInteger;
                LGroundwaterObject.FReferenceNodeNr.FInitalised := True;
              end;}

              //Aquifer Node Number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAquiferNodeNumberErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAquiferNodeNumber.FData       := LReadInteger;
                LGroundwaterObject.FAquiferNodeNumber.FInitalised := True;
                LGroundwaterObject.FReferenceNodeNr.FData       := LReadInteger;
                LGroundwaterObject.FReferenceNodeNr.FInitalised := True;
              end;

              //Abstraction Node Number
              //LTempString := GetSubstring(LReadString,11,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAbstractionNodeNumberErr');
                LMessage := Format(LMessage,[LCurrentLine+1,11,15]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAbstractionNodeNumber.FData       := LReadInteger;
                LGroundwaterObject.FAbstractionNodeNumber.FInitalised := True;
              end;

              //Abstraction Collection Number
              //LTempString := GetSubstring(LReadString,16,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strCollectionNodeNumberErr');
                LMessage := Format(LMessage,[LCurrentLine+1,16,20]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FCollectionNodeNumber.FData       := LReadInteger;
                LGroundwaterObject.FCollectionNodeNumber.FInitalised := True;
              end;

              //Base flow Node Number
              //LTempString := GetSubstring(LReadString,21,25);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strBaseflowNodeNumberErr');
                LMessage := Format(LMessage,[LCurrentLine+1,21,25]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FBaseflowNodeNumber.FData       := LReadInteger;
                LGroundwaterObject.FBaseflowNodeNumber.FInitalised := True;
              end;

              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16b');

              //Aquifer Inflow Channel and penalty number
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAquiferInflowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAquiferInflowChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FAquiferInflowChannelNr.FInitalised := True;
              end;

              //Aquifer Inflow penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAquiferInflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAquiferInflowPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FAquiferInflowPenaltyType.FInitalised := True;
              end;

              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16c');

              //Aquifer Excess Interflow ChannelNr
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAquiferExcessInterflowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAquiferExcessInterflowChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FAquiferExcessInterflowChannelNr.FInitalised := True;
              end;

              //Aquifer Excess Inter flow penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAquiferExcessInterflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAquiferExcessInterflowPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FAquiferExcessInterflowPenaltyType.FInitalised := True;
              end;

              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16d');

              // Groundwater base flow channel
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strAbstractionFromAquiferChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FGroundWaterBaseflowChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FGroundWaterBaseflowChannelNr.FInitalised := True;
              end;

              //Groundwater base flow Penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FGroundWaterBaseflowPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FGroundWaterBaseflowPenaltyType.FInitalised := True;
              end;

              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;

              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16e');

              // Abstraction from aquifer channel number
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAbstractionFromAquiferChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FAbstractionFromAquiferChannelNr.FInitalised := True;
              end;

              //Abstraction from aquifer Penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAbstractionFromAquiferPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FAbstractionFromAquiferPenaltyType.FInitalised := True;
              end;

              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;
              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16f');

              // abstraction from base flow regulation ChannelNr
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAbstractionFromBaseflowChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FAbstractionFromBaseflowChannelNr.FInitalised := True;
              end;

              //abstraction from base flow  Penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FAbstractionFromBaseflowPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FAbstractionFromBaseflowPenaltyType.FInitalised := True;
              end;
//__________________________________________________________________________________________________________________

              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;
              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16f');

              // abstraction from base flow regulation ChannelNr
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FGroundWaterBaseFlowRemainderChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FGroundWaterBaseFlowRemainderChannelNr.FInitalised := True;
              end;

              //abstraction from base flow  Penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strGroundWaterBaseflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FGroundWaterBaseFlowRemainderPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FGroundWaterBaseFlowRemainderPenaltyType.FInitalised := True;
              end;
//__________________________________________________________________________________________________________________
              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;
              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16g');

              // Surface runoff and interflow channel number
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strSurfaceRunoffAndSoilInterflowChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FSurfaceRunoffAndSoilInterflowChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FSurfaceRunoffAndSoilInterflowChannelNr.FInitalised := True;
              end;

              //Surface runoff and interflow penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strSurfaceRunoffAndSoilInterflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FSurfaceRunoffAndSoilInterflowPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FSurfaceRunoffAndSoilInterflowPenaltyType.FInitalised := True;
              end;
//__________________________________________________________________________________________________________________
              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;
              LReadString := LFileData[LCurrentLine];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16g');

              // Inflow From Upstream aquifer channelNr
              //LTempString := GetSubstring(LReadString,1,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strOutflowToDownstreamAquiferChannelNrErr');
                LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FInflowFromUpStreamAquiferChannelNr.FData       := LReadInteger;
                LGroundwaterObject.FInflowFromUpStreamAquiferChannelNr.FInitalised := True;
              end;

              //Inflow From Upstream aquifer penalty Type
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strOutflowToDownstreamAquiferPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FInflowFromUpStreamAquiferPenaltyType.FData       := LReadInteger;
                LGroundwaterObject.FInflowFromUpStreamAquiferPenaltyType.FInitalised := True;
              end;

              //Surface runoff and interflow penalty number
              //LTempString := GetSubstring(LReadString,6,5);
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile03Agent.strSurfaceRunoffAndSoilInterflowPenaltyTypeErr');
                LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit;
              end
              else
              begin
                LGroundwaterObject.FInflowFromUpStreamAquiferAquiferNumber.FData       := LReadInteger;
                LGroundwaterObject.FInflowFromUpStreamAquiferAquiferNumber.FInitalised := True;
              end;

//__________________________________________________________________________________________________________________
            end;
          end;
        end;

        //Line type 17 ++++++++++++++++++++++++++++++++++
        LCurrentLine := LCurrentLine + 1;
        if (LCurrentLine >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;
        LReadString := LFileData[LCurrentLine];
        if  (FAppModules.Model.ModelName = CYield) then
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'17')
        else if  (FAppModules.Model.ModelName = CPlanning) then
        begin
          LLinesCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLinesCount,'15')
        end;

        LTempString:=GetSubstring(LReadString,1,5);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile03Agent.strSummaryChannelCountErr');
          LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit;
        end
        else
        begin
          {if  (FAppModules.Model.ModelName = CYield) and ((LReadInteger < 0) OR (LReadInteger > 180)) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxSummaryChannelCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end;}
          //else
          //begin
            LChannelDescr.FSummaryChannelCount.FData :=LReadInteger;
            LChannelDescr.FSummaryChannelCount.FInitalised := True;
          //end;
        end;

        LTempString:=GetSubstring(LReadString,6,5);
        if(Trim(LTempString) <> '') then
        begin
          if LIsModelPlanning then
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLinesCount,'15');
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strFirmYieldAnalysesCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            if (FAppModules.Model.ModelName = CYield) and (LReadInteger > MNSYLDMAX) then
            begin
              LMessage := FAppModules.Language.GetString('TFile03Agent.strMaxFirmYieldAnalysesCountErr');
              LMessage := Format(LMessage,[LCurrentLine+1,6,10]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit;
            end;
            //else
            //begin
              LChannelDescr.FFirmYieldAnalysesCount.FData :=LReadInteger;
              LChannelDescr.FFirmYieldAnalysesCount.FInitalised := True;
            //end;
          end;

        end;

        LTempString := Copy(LReadString,11,Length(LReadString));
        if(Trim(LTempString) <> '') then
        begin
          LChannelDescr.FComment17.FData := LTempString;
          LChannelDescr.FComment17.FLength := Length(LTempString);
          LChannelDescr.FComment17.FInitalised := True;
        end;

        //Line type 17a ++++++++++++++++++++++++++++++++++
        if not LChannelDescr.AddSummaryChannels then Exit;

        for LCount := 1 to LChannelDescr.FSummaryChannelCount.FData  do
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LCurrentLine];
          if LIsModelPlanning then
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15a')
          else
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'17a');

          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile03Agent.strFirmYieldAnalysesCountErr');
            LMessage := Format(LMessage,[LCurrentLine+1,1,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit;
          end
          else
          begin
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FChannelNumber.FData :=LReadInteger;
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FChannelNumber.FInitalised := True;
          end;

          if (lCount <= LChannelDescr.FFirmYieldAnalysesCount.FData) then
          begin
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FCalculateFirmYield.FData := 'Y';
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FCalculateFirmYield.FInitalised := True;
          end;

          LTempString:=GetSubstring(LReadString,8,1);
          if(Length(LTempString) = 0) then
            LTempString := ' ';
          TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FFlowOutput.FData := LTempString;
          TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FFlowOutput.FInitalised := True;

          if LIsModelPlanning then
          begin
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15a');
            LTempString := Trim(Copy(LReadString,11,36));
            if(LTempString <> '') then
            begin
              TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FChannelName.FData :=LTempString;
              TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FChannelName.FLength :=Length(LTempString);
              TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FChannelName.FInitalised := True;
            end;
          end;

          LTempString := Copy(LReadString,11,Length(LReadString));
          TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FComment.FData :=LTempString;
          TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FComment.FLength :=Length(LTempString);
          TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FComment.FInitalised := True;
{
          LStartPos := 47;
          LTempString:=GetSubstring(LReadString,LStartPos,Length(LReadString));
          if(LTempString <> '') then
          begin
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FComment.FData := LTempString;
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FComment.FLength := Length(LTempString);
            TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]).FComment.FInitalised := True;
          end;
}
        end;

        for LCount := LCurrentLine + 1 to LFileData.Count - 1 do
          LChannelDescr.FF03ExtraLines.Add(LFileData[LCount]);

      end;//used only to break at end of file

      if LFileIncomplete then
      begin
        LMessage := FAppModules.Language.GetString('TFile03Agent.strFileIncomplete');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        LMessage := FAppModules.Language.GetString('TFile03Agent.strReadingCompleted');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
        Result := True;
      end;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile03Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile03Agent.WriteModelDataToFile';
var
  LMessage      : string;
  LOutString    : string;
  LTempString   : string;
  LCount,
  LCount1       : integer;
  LTmpIrrBlockCount,
  LTmpWetlandCount,
  LTempInt : Integer;
  LSubCount     : integer;
  LF03File      : TStringlist;
  LChannelDescr : TChannelDescrObject;
  LStop         : boolean;
  lSumChanObj   : TSummaryChannelObject;
  lPenaltyList  : IChannelPenaltyList;
  lPenalty      : IChannelPenalty;
  LTmpReturnFlowChannelCount,
  LTmpReclaimationChannelCount : Integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile03Agent.strWritingStarted');
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

    LChannelDescr := ADataObject.FChannelDescrObject;

    LF03File:= TStringList.Create;
    try

      //Line type 1 ++++++++++++++++++++++++++++++++++
      LOutString := PadInt(LChannelDescr.FPenaltyChannelCount);
      if  (FAppModules.Model.ModelName = CPlanning) then
      begin
        if LChannelDescr.FInflowPenaltyNo.FInitalised then
          LOutString := LOutString + '    '+IntToStr(LChannelDescr.FInflowPenaltyNo.FData);
      end
      else
        if LChannelDescr.FComment01.FInitalised then
          LOutString := LOutString + LChannelDescr.FComment01.FData;
      LF03File.Add(LOutString);

      //Line type 1a ++++++++++++++++++++++++++++++++++
      lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelPenaltyList;

      for LCount := 0 to lPenaltyList.ChannelPenaltyCount - 1 do
      begin
        LOutString  := '';
        lPenalty    := lPenaltyList.ChannelPenaltyByIndex[LCount];

        LTempString := IntToStr(lPenalty.ChannelPenaltyID);
        while (Length(LTempString) < 5) do
          LTempString := ' ' + LTempString;
        LOutString := LOutString + LTempString;

        LTempString := IntToStr(lPenalty.ChannelPenaltyArcCount);
        while (Length(LTempString) < 5) do
          LTempString := ' ' + LTempString;
        LOutString := LOutString + LTempString;

        for LSubCount := 1 to lPenalty.ChannelPenaltyArcCount do
        begin
          if (lPenalty.ChannelPenaltyValueByIndex[LSubCount] = NullFloat) then
            LTempString := '          '
          else
          begin
            LTempString := Format('%g', [lPenalty.ChannelPenaltyValueByIndex[LSubCount]]);
            if (Pos('.', LTempString) = 0) then
              LTempString := LTempString + '.';
            while (Length(LTempString) < 10) do
              LTempString := ' ' + LTempString;
          end;
          LOutString := LOutString + LTempString;
        end;

        if(lPenalty.ChannelPenaltyName <> '') then
        begin
          while (Length(LOutString) < 60) do
            LOutString := LOutString + ' ';
          LOutString := LOutString + '\ '  + Trim(lPenalty.ChannelPenaltyName);
        end;
        LF03File.Add(LOutString);
      end;

{
      for LCount := 1 to LChannelDescr.FPenaltyChannelCount.FData do
      begin
        LOutString := '';
        LTempString:=PadInt(TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LCount]).FPenaltyType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LCount]).FArcCount);
        LOutString:=LOutString + LTempString;

        for LSubCount := 1 to TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LCount]).FArcCount.FData do
        begin
          LTempString:=PadDouble(TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LCount]).FArcPenalty[LSubCount]);
          LOutString:=LOutString + LTempString;
        end;

        while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ '  + Trim(TPenaltyChannelObject(LChannelDescr.FPenaltyChannelList[LCount]).FComment.FData);

        LF03File.Add(LOutString);
      end;
}
      //Line type 2 ++++++++++++++++++++++++++++++++++
      LOutString := PadInt(LChannelDescr.FMasterChannelCount);
      if LChannelDescr.FComment02.FInitalised then
        LOutString := LOutString + LChannelDescr.FComment02.FData;
      LF03File.Add(LOutString);

      //Line type 2a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FMasterChannelCount.FData do
      begin
        LOutString := '';
        LTempString:=PadInt(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadChar(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FChannelType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TMasterChannelObject(LChannelDescr.FMasterChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 3 ++++++++++++++++++++++++++++++++++
      LOutString := PadInt(LChannelDescr.FPowerChannelCount);
      if LChannelDescr.FComment03.FInitalised then
        LOutString := LOutString + LChannelDescr.FComment03.FData;
      LF03File.Add(LOutString);

      for LCount := 1 to LChannelDescr.FPowerChannelCount.FData do
      begin
        //Line type 3a ++++++++++++++++++++++++++++++++++
        LOutString := '';
        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FSpillChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FSpillUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FSpillDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FSpillPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);

        //Line type 3b ++++++++++++++++++++++++++++++++++
        LOutString := '';

        LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannelCount);
        LOutString:=LOutString + LTempString;

        for LSubCount := 1 to TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannelCount.FData do
        begin
          LTempString:=PadInt(TPowerChannelObject(LChannelDescr.FPowerChannelList[LCount]).FDownStreamPowerChannels[LSubCount]);
          LOutString:=LOutString + LTempString;
        end;
        LF03File.Add(LOutString);

      end;

      //Line type 4 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FIrrigationChannelCount);
      if LChannelDescr.FComment04.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment04.FData;
      LF03File.Add(LOutString);

      //Line type 4a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FIrrigationChannelCount.FData do
      begin
        LOutString := '';
        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FIrrigationNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FUpstreamNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FDiversionChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FIrrigationPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FDownStreamNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FReturnChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FReturnPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FConsumptiveChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FConsumptivePenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FRelaxationDemand);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TIrrigationChannelObject(LChannelDescr.FIrrigationChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 5 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FDiversionChannelCount);
      if LChannelDescr.FComment05.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment05.FData;
      LF03File.Add(LOutString);

      //Line type 5a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FDiversionChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FChannelType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TDiversionChannelObject(LChannelDescr.FDiversionChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 6 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FMinFlowChannelCount);
      if LChannelDescr.FComment06.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment06.FData;
      LF03File.Add(LOutString);

      //Line type 6a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FMinFlowChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TMinFlowChannelObject(LChannelDescr.FMinFlowChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 7 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FLossChannelCount);
      if LChannelDescr.FComment07.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment07.FData;
      LF03File.Add(LOutString);

      //Line type 7a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FLossChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        if TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FChannelType.FInitalised then
        begin
          LTempString:=PadInt(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FChannelType);
          LOutString:=LOutString + LTempString;
        end
        else
        begin
          LTempString:=StringOfChar(' ',TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FChannelType.FLength);
          LOutString:=LOutString + LTempString;
        end;

        if TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FReference.FInitalised then
        begin
          LTempString:=PadInt(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FReference);
          LOutString:=LOutString + LTempString;
        end
        else
        begin
          LTempString:=StringOfChar(' ',TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FReference.FLength);
          LOutString:=LOutString + LTempString;
        end;

        LOutString:=LOutString + PadString(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FComment);

        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FComment.FData);}

        {if TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FComment.FInitalised then
        begin
          LTempString:= PadString(TLossChannelObject(LChannelDescr.FLossChannelList[LCount]).FComment);
          LOutString:=LOutString + LTempString;
        end;}

        LF03File.Add(LOutString);
      end;

      //Line type 8 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FMultiPurposeChannelCount);
      if LChannelDescr.FComment08.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment08.FData;
      LF03File.Add(LOutString);

      //Line type 8a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FMultiPurposeChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TMultiPurposeChannelObject(LChannelDescr.FMultiPurposeChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 9 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FPumpingChannelCount);
      if LChannelDescr.FComment09.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment09.FData;
      LF03File.Add(LOutString);

      //Line type 9a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FPumpingChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadDouble(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FPumpingHead);
        LOutString:=LOutString + LTempString;

        LTempString:=PadDouble(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FEfficiency);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TPumpingChannelObject(LChannelDescr.FPumpingChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 10 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FInflowChannelCount);
      if LChannelDescr.FComment10.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment10.FData;
      LF03File.Add(LOutString);

      //Line type 10a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FInflowChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TInflowChannelObject(LChannelDescr.FInflowChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 11 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FDemandChannelCount);
      if LChannelDescr.FComment11.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment11.FData;
      LF03File.Add(LOutString);

      //Line type 11a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FDemandChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FGaugeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadChar(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FStochastic);
        LOutString:=LOutString + LTempString;

        LTempString:=PadString(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FFullname);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FComment);
        {while (Length(LOutString) < 80) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TDemandChannelObject(LChannelDescr.FDemandChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      //Line type 12 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FGeneralChannelCount);
      if LChannelDescr.FComment12.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment12.FData;
      LF03File.Add(LOutString);

      //Line type 12a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FGeneralChannelCount.FData do
      begin
        LOutString := '';

        LTempString:=PadInt(TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]).FChannelNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]).FUpNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]).FDownNodeNumber);
        LOutString:=LOutString + LTempString;

        LTempString:=PadInt(TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]).FPenaltyStructType);
        LOutString:=LOutString + LTempString;

        LOutString:=LOutString + PadString(TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]).FComment);
        {while (Length(LOutString) < 60) do
          LOutString := LOutString + ' ';
        LOutString := LOutString + '\ ' + Trim(TGeneralChannelObject(LChannelDescr.FGeneralChannelList[LCount]).FComment.FData);}

        LF03File.Add(LOutString);
      end;

      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        //Line type 13 for version 7 ++++++++++++++++++++++++++++++++++
        LTmpIrrBlockCount := 0;
        for LCount := 1 to LChannelDescr.FIrrigationBlockCount.FData  do
        begin
          if LChannelDescr.FIrrigationBlockList[LCount] = nil then
            Break;
          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FChannelType.FData = 14 then
            Inc(LTmpIrrBlockCount);
        end;
        LTempInt    := LChannelDescr.FIrrigationBlockCount.FData;
        LChannelDescr.FIrrigationBlockCount.FData := LTmpIrrBlockCount;
        LOutString  := PadInt(LChannelDescr.FIrrigationBlockCount);
        if LChannelDescr.FComment13.FInitalised then
          LOutString:=LOutString + LChannelDescr.FComment13.FData;
        LChannelDescr.FIrrigationBlockCount.FData := LTempInt;
        LF03File.Add(LOutString);

        //Line type 13a ++++++++++++++++++++++++++++++++++ Only use obj with channel type 14 as it contains the 'full' record
        for LCount := 1 to LChannelDescr.FIrrigationBlockCount.FData do
        begin
          if LChannelDescr.FIrrigationBlockList[LCount] = nil then
            Break;
          if TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FChannelType.FData = 14 then
          begin
            LOutString := '';

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FUpNodeNumber);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FDownNodeNumber);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FAbstractChannelNr);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FAbstractPenaltyType);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FReturnFlowChannelNr);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FReturnFlowPenaltyType);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FBlockNumber);
            LOutString:=LOutString + LTempString;

            LOutString:=LOutString + PadString(TIrrigationBlockChannelObject(LChannelDescr.FIrrigationBlockList[LCount]).FComment);
            LF03File.Add(LOutString);
          end;
        end;

        //Line type 14 for version 7 ++++++++++++++++++++++++++++++++++
        LTmpWetlandCount := 0;
        for LCount := 1 to LChannelDescr.FWetlandCount.FData do
        begin
          if LChannelDescr.FWetlandList[LCount] = nil then
            Break;
          if TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FChannelType.FData = 16 then
            Inc(LTmpWetlandCount);
        end;
        LTempInt    := LChannelDescr.FWetlandCount.FData;
        LChannelDescr.FWetlandCount.FData := LTmpWetlandCount;
        LOutString  := PadInt(LChannelDescr.FWetlandCount);
        if LChannelDescr.FComment14.FInitalised then
          LOutString:=LOutString + LChannelDescr.FComment14.FData;
        LChannelDescr.FWetlandCount.FData := LTempInt;
        LF03File.Add(LOutString);

        //Line type 14a ++++++++++++++++++++++++++++++++++
        for LCount := 1 to LChannelDescr.FWetlandCount.FData do
        begin
          if LChannelDescr.FWetlandList[LCount] = nil then
            Break;
          if TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FChannelType.FData = 16 then
          begin
            LOutString := '';

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FNodeNumber);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FUpNodeNumber);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FDownNodeNumber);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FInflowChannelNr);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FInflowPenaltyType);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FOutflowChannelNr);
            LOutString:=LOutString + LTempString;

            LTempString:=PadInt(TWetlandChannelObject(LChannelDescr.FWetlandList[LCount]).FOutflowPenaltyType);
            LOutString:=LOutString + LTempString;

            LF03File.Add(LOutString);
          end;
        end;

        if (FAppModules.StudyArea.ModelVersion = '7') and (FAppModules.Model.ModelName = CYield) then
        begin
          //Line type 15 for version 7 DemandCentre not yet implemented in Planning Model+++++++++++++++++++++++++++
          LOutString  := PadInt(LChannelDescr.FDemandCentreCount);
          if LChannelDescr.FComment15.FInitalised then
            LOutString:=LOutString + LChannelDescr.FComment15.FData;
          LF03File.Add(LOutString);

          //Line type 15a ++++++++++++++++++++++++++++++++++
          for LCount := 1 to LChannelDescr.FDemandCentreCount.FData do
          begin
            if LChannelDescr.FDemandCentreList[LCount] = nil then
              Break;
            begin
              LTmpReturnFlowChannelCount    := 0;
              LTmpReclaimationChannelCount  := 0;

              for LCount1 := 0 to LChannelDescr.FReturnFLowChannelList.Count - 1 do
              begin
                if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LCount1]).FUpstreamNodeNr.FData =
                     TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNodeNumber.FData then
                  Inc(LTmpReturnFlowChannelCount);
              end;

              for LCount1 := 0 to LChannelDescr.FReclaimationChannelList.Count - 1 do
              begin
                if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount1]).FUpstreamNodeNr.FData =
                     TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNodeNumber.FData then
                  Inc(LTmpReclaimationChannelCount);
              end;

              LOutString  := '';

              LTempString := PadInt(TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNodeNumber);
              LOutString  := LOutString + LTempString;

              LTempString := PadInt(TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FConsumptiveChannelNr);
              LOutString  := LOutString + LTempString;

              TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNoOfReturnFlowChannels.FData        := LTmpReturnFlowChannelCount;
              TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNoOfReturnFlowChannels.FInitalised  := True;
              LTempString := PadInt(TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNoOfReturnFlowChannels);
              LOutString  := LOutString + LTempString;

              TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNoOfReclaimationChannels.FData        := LTmpReclaimationChannelCount;
              TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNoOfReclaimationChannels.FInitalised  := True;
              LTempString := PadInt(TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNoOfReclaimationChannels);
              LOutString  := LOutString + LTempString;

              LF03File.Add(LOutString);

              for LCount1 := 1 to LChannelDescr.FReturnFLowChannelList.Count - 1 do
              begin
                if TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LCount1]).FUpstreamNodeNr.FData =
                     TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNodeNumber.FData then
                begin
                  LOutString := '';

                  LTempString:=PadInt(TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LCount1]).FChannelNr);
                  LOutString:=LOutString + LTempString;

                  LTempString:=PadInt(TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LCount1]).FDownStreamNodeNr);
                  LOutString:=LOutString + LTempString;

                  LTempString:=PadInt(TReturnFlowChannelObject(LChannelDescr.FReturnFLowChannelList[LCount1]).FPenaltyStructureType);
                  LOutString:=LOutString + LTempString;

                  LF03File.Add(LOutString);
                end;
              end;

              for LCount1 := 1 to LChannelDescr.FReclaimationChannelList.Count - 1 do
              begin
                if TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount1]).FUpstreamNodeNr.FData =
                     TDemandCentreObject(LChannelDescr.FDemandCentreList[LCount]).FNodeNumber.FData then
                begin
                  LOutString := '';

                  LTempString:=PadInt(TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount1]).FChannelNr);
                  LOutString:=LOutString + LTempString;

                  LTempString:=PadInt(TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount1]).FDownStreamNodeNr);
                  LOutString:=LOutString + LTempString;

                  LTempString:=PadInt(TReclaimationChannelObject(LChannelDescr.FReclaimationChannelList[LCount1]).FPenaltyStructureType);
                  LOutString:=LOutString + LTempString;

                  LF03File.Add(LOutString);
                end;
              end;
            end;
          end;
        end;

        if (FAppModules.StudyArea.ModelVersion = '7') and (FAppModules.Model.ModelName = CYield) and
            TYieldModelDataObject(FAppModules.Model.ModelData).ImplementedNetworkFeatures.GroundWaterFeatureImplemented then
        begin
          //Line type 16 for version 7 ++++++++++++++++++++++++++++++++++
          LOutString  := PadInt(LChannelDescr.FGroundWaterCount);
          if LChannelDescr.FComment16.FInitalised then
            LOutString:=LOutString + LChannelDescr.FComment16.FData;
          LF03File.Add(LOutString);

          for LCount := 1 to LChannelDescr.FGroundWaterCount.FData do
          begin
            if (LChannelDescr.FGroundWaterList[LCount] = nil) then
              break;

            LOutString := '';

            //line type 16a
            //LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FReferenceNodeNr);
            //LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAquiferNodeNumber);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAbstractionNodeNumber);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FCollectionNodeNumber);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FBaseflowNodeNumber);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16b
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAquiferInflowChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAquiferInflowPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16c
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAquiferExcessInterflowChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAquiferExcessInterflowPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16d
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FGroundWaterBaseflowChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FGroundWaterBaseflowPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16e
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAbstractionFromAquiferChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAbstractionFromAquiferPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16f
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAbstractionFromBaseflowChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FAbstractionFromBaseflowPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16g
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FGroundWaterBaseFlowRemainderChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FGroundWaterBaseFlowRemainderPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16h
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FSurfaceRunoffAndSoilInterflowChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FSurfaceRunoffAndSoilInterflowPenaltyType);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);

            //line type 16i
            LOutString := '';
            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FInflowFromUpStreamAquiferChannelNr);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FInflowFromUpStreamAquiferPenaltyType);
            LOutString  := LOutString + LTempString;

            LTempString := PadInt(TGroundWaterObject(LChannelDescr.FGroundWaterList[LCount]).FInflowFromUpStreamAquiferAquiferNumber);
            LOutString  := LOutString + LTempString;
            LF03File.Add(LOutString);
          end;
        end;
      end;

      //Line type 17 ++++++++++++++++++++++++++++++++++
      LOutString:=PadInt(LChannelDescr.FSummaryChannelCount);
      if LChannelDescr.FFirmYieldAnalysesCount.FInitalised then
        LOutString:= LOutString + PadInt(LChannelDescr.FFirmYieldAnalysesCount);
      if LChannelDescr.FComment17.FInitalised then
        LOutString:=LOutString + LChannelDescr.FComment17.FData;
      LF03File.Add(LOutString);

      //Line type 17a ++++++++++++++++++++++++++++++++++
      for LCount := 1 to LChannelDescr.FSummaryChannelCount.FData do
      begin
        LOutString := '';

        lSumChanObj := TSummaryChannelObject(LChannelDescr.FSummaryChannelList[LCount]);
        LTempString:=PadInt(lSumChanObj.FChannelNumber);
        LOutString:=LOutString + LTempString ;

        LTempString:=PadString(lSumChanObj.FFlowOutput);
        LOutString:=LOutString + '  ' +LTempString;

        if lSumChanObj.FComment.FInitalised then
          LOutString := LOutString  + '  ' + lSumChanObj.FComment.FData;

        LF03File.Add(LOutString);
      end;

      for LCount := 0 to LChannelDescr.FF03ExtraLines.Count -1 do
      begin
        LF03File.Add(LChannelDescr.FF03ExtraLines[LCount]);
      end;

      LF03File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile03Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LF03File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
