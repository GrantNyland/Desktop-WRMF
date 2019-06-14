//
//
//  UNIT      : Contains TFileReturnFlowAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFileReturnFlowChannelAgent;

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
  UReturnFlowChannelFileDataObjects,
  UPlanningFileDataObjects,
  UYieldModelDataObject;

type

  TFileReturnFlowChannelAgent = class(TAbstractFileAgent)
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
  UErrorHandlingOperations;


{ TFileReturnFlowChannelAgent }

function TFileReturnFlowChannelAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
                                                    AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReturnFlowChannelAgent.ReadModelDataFromFile';
var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString: String;
  LReadInteger,
  LCount,
  LCurrentline,
  LFieldCount,
  LIndex,
  LErrorCode : Integer;
  LReadReal : Double;
  LReturnFlow : TReturnFlow;
  LAssumedFactor : TAssumedFactor;
  LReturnFlowFileObject: TReturnFlowChannelFileObject;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LFileIncomplete,
  LStop: boolean;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
  LLocalCount             : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strReadingStarted');
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
    LReturnFlowFileObject := LPlanningFileDataObject.ReturnFlowChannelFileObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileIncomplete := False;
    LFileData := TStringList.Create;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    try
      LFileData.LoadFromFile(AFilename.FileName);
      if LFileData.Count > 0 then
      begin
        //Line 1 .............................................................
        lCurrentLine := 0;
        LReadString := LFileData[lCurrentLine];
        LTempString := ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
        if (LTempString = '') then
          LReturnFlowFileObject.FExtraLines.Add(LFileData[lCurrentLine])
        else
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strTotalSpecErr');
          LMessage := Format(LMessage,[lCurrentLine]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          if not LReturnFlowFileObject.FReturnFlowCount.FInitalised then
          begin
            LReturnFlowFileObject.FReturnFlowCount.FData := LReadInteger;
            LReturnFlowFileObject.FReturnFlowCount.FInitalised := True;
          end;
        end;

        for LCount := 1 to LReturnFlowFileObject.FReturnFlowCount.FData do
        begin
          LCurrentLine := LCurrentLine + 1;
          if (LCurrentLine >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          // Line 2 ..............................................................
          LReturnFlow := LReturnFlowFileObject.AddReturnFlow;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
          if not LReturnFlow.Initialise then
            Exit;
          LReadString := LFileData[lCurrentLine];
          if (LTempString = '') then
            LReturnFlowFileObject.FExtraLines.Add(LFileData[lCurrentLine])
          else
          begin
            LFieldCount := 1;
            while (Length(LReadString) > 0) and (LFieldCount < 10 )do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              LTempString := Trim(LTempString);
              Val(LTempString,LReadInteger,LErrorCode);
              //Demand Channel............................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 1) and (LErrorCode = 0) then
              begin
                LReturnFlow.FDemandChannel.FData := LReadInteger;
                LReturnFlow.FDemandChannel.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 1) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strNoDemandChannelData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              //Number of Corresponding Channels............................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 2) and (LErrorCode = 0) then
              begin
                LReturnFlow.FNumOfCorrespondingChannels.FData := LReadInteger;
                LReturnFlow.FNumOfCorrespondingChannels.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 2) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strNoCorrespondingChannelData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              //Rainfall Gauge Number ............................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 3) and (LErrorCode = 0) then
              begin
                LReturnFlow.FGaugeNumber.FData := LReadInteger;
                LReturnFlow.FGaugeNumber.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 3) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strGaugeNumberDataErr');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              //Long-term monthly average return flow factor..................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              Val(LTempString,LReadReal,LErrorCode);
              if (Trim(LTempString) <> '') and (LFieldCount = 4) and (LErrorCode = 0) then
              begin
                LReturnFlow.FMonthlyAvrgFactor.FData := LReadReal;
                LReturnFlow.FMonthlyAvrgFactor.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 4) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strMonthlyAvrgFactorData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
             //Calibration Factor..................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 5) and (LErrorCode = 0) then
              begin
                LReturnFlow.FCalibrationFactor.FData := LReadReal;
                LReturnFlow.FCalibrationFactor.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 5)  then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strCalibrationFactorData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;

              //Long-therm monthly average(evaporation minus rainfall)..................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 6) and (LErrorCode = 0) then
              begin
                LReturnFlow.FMonthlyAvrgNetEvap.FData := LReadReal;
                LReturnFlow.FMonthlyAvrgNetEvap.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 6) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strMonthlyAvrgNetEvapData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              //Routing constant................................................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 7) and (LErrorCode = 0) then
              begin
                LReturnFlow.FRoutingConstant.FData := LReadReal;
                LReturnFlow.FRoutingConstant.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 7) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strRoutingConstantData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              //Curtailment factor................................................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 8) and (LErrorCode = 0) then
              begin
                LReturnFlow.FCurtailmentFactor.FData := LReadReal;
                LReturnFlow.FCurtailmentFactor.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 8) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strCurtailmentFactorData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              //Multiplication factor................................................
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
              if (Trim(LTempString) <> '') and (LFieldCount = 9) and (LErrorCode = 0) then
              begin
                LReturnFlow.FMultiplicationFactor.FData := LReadReal;
                LReturnFlow.FMultiplicationFactor.FInitalised := True;
              end
              else
              if (LErrorCode <> 0) and (LFieldCount = 9) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strMultiplicationFactorData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              LFieldCount := LFieldCount + 1;
            end;
            // Line 3 Monthly Evaporation.........................................
            LLocalCount := LFileLineTypesObject.LinesCount;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
            LCurrentLine := LCurrentLine + 1;
            if (LCurrentLine >= LFileData.Count) then
            begin
              LFileIncomplete := True;
              Break;
            end;
            LReadString := LFileData[lCurrentLine];
            LFieldCount := 1;
            while Length(LReadString) > 0 do
            begin
              LTempString := Trim(ExtractFirstSubstring(LReadString));
              Val(LTempString,LReadReal,LErrorCode);
              if (LTempString <> '') and (LErrorCode = 0) then
              begin
                LReturnFlow.FMonthlyPotentialEvap[LFieldCount].FData := LReadReal;
                LReturnFlow.FMonthlyPotentialEvap[LFieldCount].FInitalised := True;
              end
              else
              if (LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strMonthlyPotentialEvapData');
                LMessage := Format(LMessage,[lCurrentLine]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end;
              inc(LFieldCount);
            end;
            // Line 4 Assumed return flow factor.........................................
            for LIndex := 1 to LReturnFlow.FNumOfCorrespondingChannels.FData do
            begin
              LLocalCount := LFileLineTypesObject.LinesCount;
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
              LCurrentLine := LCurrentLine + 1;
              if (LCurrentLine >= LFileData.Count) then
              begin
                LFileIncomplete := True;
                Break;
              end;
              LReadString := LFileData[lCurrentLine];
              LTempString := Trim(LReadString);
              if LTempString <> '' then
              begin
                LAssumedFactor := LReturnFlow.AddAssumedFactor;

                if not LAssumedFactor.Initialise then
                  Exit;
                LFieldCount := 1;
                while Length(LReadString) > 0 do
                begin
                  LTempString := Trim(ExtractFirstSubstring(LReadString));
                  if (LFieldCount <> 3) then
                    Val(LTempString,LReadInteger,LErrorCode);

                  if (LTempString<>'') and (LErrorCode = 0) then
                  begin
                    if (LFieldCount = 1) then
                    begin
                      LAssumedFactor.FChannelNumber.FData :=  LReadInteger;
                      LAssumedFactor.FChannelNumber.FInitalised := True;
                    end;
                    if (LFieldCount = 2) then
                    begin
                      LAssumedFactor.FAbstractionChannel.FData :=  LReadInteger;
                      LAssumedFactor.FAbstractionChannel.FInitalised := True;
                    end;
                    Val(LTempString,LReadReal,LErrorCode);
                    if (LFieldCount = 3) then
                    begin
                      LAssumedFactor.FFactor.FData :=  LReadReal;
                      LAssumedFactor.FFactor.FInitalised := True;
                    end;
                  end
                  else
                  if (LErrorCode <> 0) then
                  begin
                    LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strAssumedFactorData');
                    LMessage := Format(LMessage,[lCurrentLine]);
                    AProgressFunction(LMessage,ptError,LStop);
                    if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                  end;
                  inc(LFieldCount);
                end;
              end;
            end;

          end;
        end;
        if (LCurrentLine < LFileData.Count-1) then
        begin
          LCurrentLine := LCurrentLine + 1; 
          for LCount := LCurrentLine to LFileData.Count-1 do
            LReturnFlowFileObject.FExtraLines.Add(LFileData[LCount])
        end;
      end
      else
      begin
        LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strNoDataReturned');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    finally
      FreeAndNil(LFileData);
    end;

    if LFileIncomplete then
    begin
      LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strFileIncomplete');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
    end
    else
    begin
      LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;
          Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelAgent.WriteModelDataToFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
                                                   AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReturnFlowChannelAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LIndex,
  LCount         : Integer;
  LReturnFlow : TReturnFlow;
  LAssumedFactor : TAssumedFactor;
  LReturnFlowFileObject: TReturnFlowChannelFileObject;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LReturnFlowFileObject := LPlanningFileDataObject.ReturnFlowChannelFileObject;

    {if LReturnFlowFileObject.FReturnFlowCount.FData = 0 then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strNoDataReturned');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;}

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strWritingStarted');
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

    LFileData := TStringList.Create;
    try
      //line 1 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LReturnFlowFileObject.FReturnFlowCount);
      LOutString:=LOutString+(LTempString);
      LFileData.Add(LOutString);

      for LCount := 0 to LReturnFlowFileObject.FReturnFlowCount.FData -1 do
      begin
        //line 2 +++++++++++++++++++++++++
        LReturnFlow := TReturnFlow(LReturnFlowFileObject.FReturnFlowList.Items[LCount]);
        LOutString:='';
        LTempString:=PadInt(LReturnFlow.FDemandChannel);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadInt(LReturnFlow.FNumOfCorrespondingChannels);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadInt(LReturnFlow.FGaugeNumber);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LReturnFlow.FMonthlyAvrgFactor);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LReturnFlow.FCalibrationFactor);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LReturnFlow.FMonthlyAvrgNetEvap);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LReturnFlow.FRoutingConstant);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LReturnFlow.FCurtailmentFactor);
        LOutString:=LOutString+' '+LTempString;

        LTempString:=PadDouble(LReturnFlow.FMultiplicationFactor);
        LOutString:=LOutString+' '+LTempString;
        LFileData.Add(LOutString);
        // Line 3 +++++++++++++++++++++++++++++++++++++++
        LOutString:='';
        for LIndex := MinMonths to MaxMonths do
        begin
          LTempString := PadDouble(LReturnFlow.FMonthlyPotentialEvap[LIndex]);
          LOutString := LOutString+LTempString;
          if LIndex = MaxMonths then
            LFileData.Add(LOutString);
        end;
        // Line 4 ++++++++++++++++++++++++++++++++++++++++
        for LIndex := 0 to LReturnFlow.FNumOfCorrespondingChannels.FData -1 do
        begin
          LOutString := '';
          LAssumedFactor := TAssumedFactor(LReturnFlow.FAssumedFactorList.Items[LIndex]);
          LTempString := PadInt(LAssumedFactor.FChannelNumber);
          LOutString := LOutString+' '+LTempString;

          LTempString := PadInt(LAssumedFactor.FAbstractionChannel);
          LOutString := LOutString+' '+LTempString;

          LTempString := PadDouble(LAssumedFactor.FFactor);
          LOutString := LOutString+' '+LTempString;
          LFileData.Add(LOutString);
        end;
      end;

      for LCount := 0 to LReturnFlowFileObject.FExtraLines.Count -1 do
      begin
        LFileData.Add(LReturnFlowFileObject.FExtraLines[LCount]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
