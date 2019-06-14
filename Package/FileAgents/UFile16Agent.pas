//
//
//  UNIT      : Contains TFile16Agent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 15/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile16Agent;

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
  UDemandReconciliationObject,
  UYieldModelDataObject;

type

  TFile16Agent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses UUtilities,
     UFilesLineTypeObject,
     UErrorHandlingOperations;

function TFile16Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile16Agent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LReadInteger,
  LChannelsCount,
  LRiskCriteriaCount,
  LCount,
  LLinesRead,
  LLinesCount,
  LErrorCode : Integer;
  LReadReal : Double;
  LDemandReconciliationObject: TDemandReconciliationDataObject;
  LDemandReconciliationData:TDemandReconciliationData;
  LDemandPortion :TDemandPortion;
  LDemandChannel :TDemandChannel;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile16Agent.strReadingStarted');
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
      //LMessage := FAppModules.Language.GetString('TFile16Agent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    LDemandReconciliationObject := ADataObject.FDemandReconciliationDataObject;
    if not LDemandReconciliationObject.Initialise then
      Exit;

    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F16 file
      LFileData.LoadFromFile(AFilename.FileName);
      if(LFileData.Count = 0) then
      begin
        Result := True;
        Exit;
      end;

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LLinesRead := 0;

      //Line 1 +++++++++++++++++++++++++++++
      LReadString := LFileData[LLinesRead];

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
      LDemandReconciliationData := LDemandReconciliationObject.FDemandReconciliationData;

      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strUserTypeCountErr');
        LMessage := Format(LMessage,[LLinesRead+1,1,2]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger < 0) or (LReadInteger > 10) then
       begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strUserTypeCountValueErr');
        LMessage := Format(LMessage,[1,4,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LDemandReconciliationData.FUserTypeCount.FData := LReadInteger;
        LDemandReconciliationData.FUserTypeCount.FInitalised := True;
      end;

      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strRiskCriteriaCountErr');
        LMessage := Format(LMessage,[LLinesRead+1,4,5]);
        AProgressFunction(LMessage,ptWarning,LStop);
        //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger < 0) or (LReadInteger > 10) then
       begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strRiskCriteriaCountValueErr');
        LMessage := Format(LMessage,[1,4,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LDemandReconciliationData.FRiskCriteriaCount.FData := LReadInteger;
        LDemandReconciliationData.FRiskCriteriaCount.FInitalised := True;
      end;

      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strScenarioCountErr');
        LMessage := Format(LMessage,[LLinesRead+1,4,5]);
        AProgressFunction(LMessage,ptWarning,LStop);
        //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger < 0) or (LReadInteger > 10) then
       begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strScenarioCountValueErr');
        LMessage := Format(LMessage,[1,4,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LDemandReconciliationData.FScenarioCount.FData := LReadInteger;
        LDemandReconciliationData.FScenarioCount.FInitalised := True;
      end;

      //Line 2 +++++++++++++++++++++++++++++
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
      LLinesRead := LLinesRead + 1;

      LReadString := LFileData[LLinesRead];
      for LRiskCriteriaCount := 1 to  LDemandReconciliationData.FRiskCriteriaCount.FData do
      begin
        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile16Agent.strRecurrenceIntervalErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          Break;
        end
        else
        begin
          LDemandReconciliationData.FRecurrenceInterval[LRiskCriteriaCount].FData := LReadReal;
          LDemandReconciliationData.FRecurrenceInterval[LRiskCriteriaCount].FInitalised := True;
        end;
      end;

      //Line 3 +++++++++++++++++++++++++++++
      for LCount := 1 to  LDemandReconciliationData.FUserTypeCount.FData do
      begin
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then
          Break;
        if(Trim(LFileData[LLinesRead]) = '') then
          Break;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LDemandPortion := LDemandReconciliationObject.AddDemandPortion;
        if not Assigned(LDemandPortion) then
          Break;

        LReadString := LFileData[LLinesRead];
        for LRiskCriteriaCount := 1 to  LDemandReconciliationData.FRiskCriteriaCount.FData do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile16Agent.strDemandPortionValueErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,3]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            Break;
          end
          else
          begin
            LDemandPortion.FDemandPortionValues[LRiskCriteriaCount].FData := LReadReal;
            LDemandPortion.FDemandPortionValues[LRiskCriteriaCount].FInitalised := True;
          end;
        end;
      end;

      //Line 4 +++++++++++++++++++++++++++++
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
      LLinesRead := LLinesRead + 1;
      LReadString := LFileData[LLinesRead];
      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strChannelsCountErr');
        LMessage := Format(LMessage,[LLinesRead+1,2,2]);
        AProgressFunction(LMessage,ptWarning,LStop);
        //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if(LReadInteger < 0) or (LReadInteger > 200) then
       begin
        LMessage := FAppModules.Language.GetString('TFile16Agent.strChannelsCountValueErr');
        LMessage := Format(LMessage,[1,4,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LDemandReconciliationData.FChannelsCount.FData := LReadInteger;
        LDemandReconciliationData.FChannelsCount.FInitalised := True;
      end;

      //Line 5 +++++++++++++++++++++++++++++
      for LChannelsCount := 1 to LDemandReconciliationData.FChannelsCount.FData do
      begin
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then
          Break;
        if(Trim(LFileData[LLinesRead]) = '') then
          Break;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
        LDemandChannel := LDemandReconciliationObject.AddDemandChannel;
        if not Assigned(LDemandChannel)then
          Break;

        LReadString := LFileData[LLinesRead];

        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile16Agent.strChannelNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,4,5]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandChannel.FChannelNumber.FData := LReadInteger;
          LDemandChannel.FChannelNumber.FInitalised := True;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile16Agent.strWaterUserTypeErr');
          LMessage := Format(LMessage,[LLinesRead+1,4,5]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandChannel.FUserType.FData := LReadInteger;
          LDemandChannel.FUserType.FInitalised := True;
        end;


        for LCount := 1 to  LDemandReconciliationData.FScenarioCount.FData do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile16Agent.strImposedDemandPortionValueErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,3]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            Break;
          end
          else
          begin
            LDemandChannel.FDemandPortionValues[LCount].FData := LReadReal;
            LDemandChannel.FDemandPortionValues[LCount].FInitalised := True;
          end;
        end;
      end;

      for LLinesCount := LLinesRead + 1 to LFileData.Count - 1 do
        LDemandReconciliationObject.FF16ExtraLines.Add(LFileData[LLinesCount]);

      LMessage := FAppModules.Language.GetString('TFile16Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile16Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LRiskCriteriaCount,
  LCount: Integer;
  LF16File :TStringlist;
  LDemandReconciliationObject: TDemandReconciliationDataObject;
  LDemandReconciliationData:TDemandReconciliationData;
  LDemandPortion :TDemandPortion;
  LDemandChannel :TDemandChannel;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile16Agent.strWritingStarted');
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

    LDemandReconciliationObject := ADataObject.FDemandReconciliationDataObject;
    if not Assigned(LDemandReconciliationObject) then
      Exit;
    LDemandReconciliationData := LDemandReconciliationObject.FDemandReconciliationData;
    if (LDemandReconciliationData.FUserTypeCount.FData = 0) and
       (LDemandReconciliationObject.FF16ExtraLines.Count = 0) then
    begin
      if FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);
      Result := True;
      Exit;
    end;

    if not TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WaterDemandConfiguration.ImplementReconciliation then
    begin
      if FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);
      Result := True;
      Exit;
    end;


    LF16File:= TStringList.Create;
    try

      //Line 1 +++++++++++++++++++
      LOutString:='';

      LOutString := IntToStr(LDemandReconciliationData.FUserTypeCount.FData);
      LOutString := LOutString + ' '+ IntToStr(LDemandReconciliationData.FRiskCriteriaCount.FData);
      LOutString := LOutString + ' '+ IntToStr(LDemandReconciliationData.FScenarioCount.FData);
      LF16File.Add(LOutString);

      //Line 2 +++++++++++++++++++
      LOutString :='';
      LTempString := '';
      for LCount := 1 to  LDemandReconciliationData.FRiskCriteriaCount.FData do
      begin
        LTempString:= Trim(PadDouble(LDemandReconciliationData.FRecurrenceInterval[LCount]));
        LOutString:=LOutString+LTempString + ' ';
      end;
      LOutString := Trim(LOutString);
      LF16File.Add(LOutString);

      //Line 3 +++++++++++++++++++++++++++++
      for LCount := 1 to  LDemandReconciliationData.FUserTypeCount.FData do
      begin
        LDemandPortion := LDemandReconciliationObject.DemandPortionByIndex(LCount-1);
        if not Assigned(LDemandPortion) then
          Break;

        LOutString :='';
        LTempString := '';
        for LRiskCriteriaCount := 1 to  LDemandReconciliationData.FRiskCriteriaCount.FData do
        begin
          LTempString:= Trim(PadDouble(LDemandPortion.FDemandPortionValues[LRiskCriteriaCount]));
          LOutString:=LOutString+LTempString + ' ';
        end;
        LOutString := Trim(LOutString);
        LF16File.Add(LOutString);
      end;

      //Line 4 +++++++++++++++++++++++++++++
      LOutString := Trim(IntToStr(LDemandReconciliationData.FChannelsCount.FData));
      LF16File.Add(LOutString);

      //Line 5 +++++++++++++++++++++++++++++
      for LCount := 1 to LDemandReconciliationData.FChannelsCount.FData do
      begin

        LDemandChannel := LDemandReconciliationObject.DemandChannelByIndex(LCount-1);
        if not Assigned(LDemandChannel) then
          Break;

        LOutString :='';
        LTempString := '';
        LOutString := IntToStr(LDemandChannel.FChannelNumber.FData);
        LOutString := LOutString + ' ' + IntToStr(LDemandChannel.FUserType.FData);
        for LRiskCriteriaCount := 1 to  LDemandReconciliationData.FScenarioCount.FData do
        begin
          LTempString:= Trim(PadDouble(LDemandChannel.FDemandPortionValues[LRiskCriteriaCount]));
          LOutString:=LOutString + ' '+ LTempString;
        end;
        LOutString := Trim(LOutString);
        LF16File.Add(LOutString);
      end;

      for LLinesCount := 0 to LDemandReconciliationObject.FF16ExtraLines.Count -1 do
      begin
        LF16File.Add(LDemandReconciliationObject.FF16ExtraLines[LLinesCount]);
      end;


      LF16File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      if(Trim(LF16File.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile16Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF16File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
