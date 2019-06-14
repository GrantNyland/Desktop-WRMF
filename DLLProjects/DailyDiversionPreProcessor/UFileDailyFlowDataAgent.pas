unit UFileDailyFlowDataAgent;

interface
uses
  Classes,
  sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UDailyFlowDataObject,
  UDailyDiversionFileDataObject,
  UAbstractFileNamesObject,
  UAbstractFileAgent;
type
  TFileDailyFlowDataAgent = class(TAbstractFileAgent)
  protected
      procedure CreateMemberObjects; override;
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  System.UITypes,
  VCL.Controls,
  VCL.Dialogs,
  UUtilities,
  UFilesLineTypeObject,
  VoaimsCom_TLB,
  UErrorHandlingOperations, UBasicObjects;

procedure TFileDailyFlowDataAgent.CreateMemberObjects;
const OPNAME = 'TFileDailyFlowDataAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyFlowDataAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyFlowDataAgent.ReadModelDataFromFile';
var
  LMessage,
  LTempString,
  LReadString                   : string;
  LStop,
  LReplaceNegativeWithZero,
  LNegativeValueFound           : boolean;
  LDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
  LDailyFlowDataObject          : TDailyFlowDataObject;
  LDailyFlowData                : TDailyFlowData;
  LFileData                     : TStringList;
  LReadReal                     : Double;
  LLinesRead,
  LReadInteger,
  LTotalDaysOfMissingFlow,
  LErrorCode                    : Integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    //Check if file exists.
    if not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LDailyDiversionFileDataObject := TDailyDiversionFileDataObject(ADataObject);
    LDailyFlowDataObject := LDailyDiversionFileDataObject.FDailyFlowDataObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LDailyFlowDataObject.Initialise then
      Exit;
    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      LTotalDaysOfMissingFlow := 0;
      //Validate no data
      if (LFileData.Count = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strNoDataReturned');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
        Result := True;
        Exit;
      end;
      // Read File header
      LLinesRead := 0;
      LReadString := LFileData[LLinesRead];

      LTempString := GetCommaDelimetedValue(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadReal,LErrorCode);
      if(LErrorCode <> 0) then
      begin

        LDailyFlowDataObject.FFileHeader.FData := LFileData[LLinesRead];
        LDailyFlowDataObject.FFileHeader.FInitalised := True;
        LDailyFlowDataObject.FFileHeader.FLength := Length(LFileData[LLinesRead]);
      end
      else
      begin
        LDailyFlowData :=  LDailyFlowDataObject.AddDailyFlowData;
        if not LDailyFlowData.Initialise then
          Exit;
        LDailyFlowData.FIdentifier.FData := LLinesRead + 1;
        LDailyFlowData.FIdentifier.FInitalised := True;
        LDailyFlowData.FDiversionDate.FData := FloatToStr(LReadReal);
        LDailyFlowData.FDiversionDate.FInitalised := True;
        LDailyFlowDataObject.FStartDate.FData :=FloatToStr(LReadReal);
        LDailyFlowDataObject.FStartDate.FInitalised := True;
        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strAvgFactorErr');
          if LDailyFlowData.FDiversionDate.FInitalised then
            LMessage := Format(LMessage,[LDailyFlowData.FDiversionDate.FData]);
          AProgressFunction(LMessage,ptWarning,LStop);
          inc(LTotalDaysOfMissingFlow);
          //if FAppModules.GlobalData.StopOnFirstErr then
          //  Exit;

        end
        else
        begin
          LDailyFlowData.FAvgFlow.FData := LReadReal;
          LDailyFlowData.FAvgFlow.FInitalised := True;

          LTempString := GetCommaDelimetedValue(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strQualityCodeErr');
            if LDailyFlowData.FDiversionDate.FInitalised then
              LMessage := Format(LMessage,[LDailyFlowData.FDiversionDate.FData]);
            AProgressFunction(LMessage,ptWarning,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then
            //  Exit;
          end
          else
          begin
            LDailyFlowData.FQualityCode.FData := LReadInteger;
            LDailyFlowData.FQualityCode.FInitalised := True;
          end;
        end;
      end;
      
      // Read Data...
      LReplaceNegativeWithZero := False;
      LNegativeValueFound      := False;
      for LLinesRead := 1 to  LFileData.Count -1 do
      begin
        LReadString := LFileData[LLinesRead];
        if (Pos('ZZZ',LReadString) > 0 ) then
        begin
          Exit;
        end;
        if (Trim(LReadString) = '') then
          Continue;
        LDailyFlowData :=  LDailyFlowDataObject.AddDailyFlowData;
        if not LDailyFlowData.Initialise then
          Exit;
        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strDateErr');
          LMessage := Format(LMessage,[LLinesRead]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then
          //  Exit;
        end
        else
        begin
          if (LDailyFlowDataObject.FFileHeader.FInitalised) then
            LDailyFlowData.FIdentifier.FData := LLinesRead
          else
            LDailyFlowData.FIdentifier.FData := LLinesRead + 1;
          LDailyFlowData.FIdentifier.FInitalised := True;
          LDailyFlowData.FDiversionDate.FData := FloatToStr(LReadReal);
          LDailyFlowData.FDiversionDate.FInitalised := True;

          if not (LDailyFlowDataObject.FStartDate.FInitalised) and (LLinesRead = 1) then
          begin
            LDailyFlowDataObject.FStartDate.FData := FloatToStr(LReadReal);
            LDailyFlowDataObject.FStartDate.FInitalised := True;
          end;
        end;
        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          inc(LTotalDaysOfMissingFlow);
          LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strAvgFactorErr');
          if LDailyFlowData.FDiversionDate.FInitalised then
            LMessage := Format(LMessage,[LDailyFlowData.FDiversionDate.FData]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then
          //  Exit;
        end
        else
        begin
          if(LReadReal < 0) then
          begin
            if not LNegativeValueFound then
            begin
              LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.ReplaceNegativeReferenceFlowValues');;
              if MessageDlg(LMessage,mtWarning,mbOKCancel,0) = mrOk then
                LReplaceNegativeWithZero := True
              else
                Exit;
              LNegativeValueFound := True;
            end;
            
            if LReplaceNegativeWithZero then
              LReadReal := 0;
          end;
          LDailyFlowData.FAvgFlow.FData       := LReadReal;
          LDailyFlowData.FAvgFlow.FInitalised := True;
        end;

        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strQualityCodeErr');
          if LDailyFlowData.FDiversionDate.FInitalised then
            LMessage := Format(LMessage,[LDailyFlowData.FDiversionDate.FData]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then
          //  Exit;
        end
        else
        begin
          LDailyFlowData.FQualityCode.FData := LReadInteger;
          LDailyFlowData.FQualityCode.FInitalised := True;
        end;
      end;

      if LDailyFlowDataObject.DailyFlowDataCount > 0 then
      begin
        LDailyFlowDataObject.FEndDate.FData  := LDailyFlowDataObject.GetDailyFlowDataByIndex(LDailyFlowDataObject.DailyFlowDataCount-1).FDiversionDate.FData;
        LDailyFlowDataObject.FEndDate.FInitalised := True;
      end;
      LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strStartEndDate');
      if (LDailyFlowDataObject.FStartDate.FInitalised) and (LDailyFlowDataObject.FEndDate.FInitalised) then
        LMessage := Format(LMessage,[LDailyFlowDataObject.FStartDate.FData,LDailyFlowDataObject.FEndDate.FData]);
      AProgressFunction(LMessage,ptNone,LStop);

      LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strTotalDaysOfMissingFlow');
      LMessage := Format(LMessage,[LTotalDaysOfMissingFlow]);
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      FreeAndNil(LFileData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyFlowDataAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyFlowDataAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LStop          : boolean;
begin
  Result := False;
  try

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strWritingStarted');
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
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
