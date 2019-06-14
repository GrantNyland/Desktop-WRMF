//
//
//  UNIT      : Contains  TFileDailyInstreamFlowDataAgent   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 05/09/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFileDailyInstreamFlowDataAgent;

interface
uses
  Classes,
  sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UDailyInstreamFlowDataObject,
  UDailyDiversionFileDataObject,
  UAbstractFileNamesObject,
  UAbstractFileAgent;
type
  TFileDailyInstreamFlowDataAgent = class(TAbstractFileAgent)
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
  UDailyDiversionDataObject,
  UErrorHandlingOperations, UBasicObjects;

procedure TFileDailyInstreamFlowDataAgent.CreateMemberObjects;
const OPNAME = 'TFileDailyInstreamFlowDataAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyInstreamFlowDataAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyInstreamFlowDataAgent.ReadModelDataFromFile';
var
  LMessage,
  LTempString,
  LReadString                   : string;
  LStop,
  LReplaceNegativeWithZero,
  LNegativeValueFound           : boolean;
  LDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
  LDailyInstreamFlowDataObject  : TDailyInstreamFlowDataObject;
  LDailyInstreamFlowData        : TDailyInstreamFlowData;
  LFileData                     : TStringList;
  LReadReal                     : Double;
  LLinesRead,
  LReadInteger,
  LTotalDaysOfMissingFlow,
  LErrorCode                    : Integer;
  LDailyDiversionGaugeData      : TDailyDiversionDataObject;
  LInstreamStartDate,
  LInstreamEndDate              : TDateTime;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    //Check if file exists.
    if not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LDailyDiversionFileDataObject := TDailyDiversionFileDataObject(ADataObject);
    LDailyInstreamFlowDataObject := LDailyDiversionFileDataObject.FDailyInstreamFlowDataObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LDailyInstreamFlowDataObject.Initialise then
      Exit;
    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      LTotalDaysOfMissingFlow := 0;
      //Validate no data
      if (LFileData.Count = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strNoDataReturned');
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

        LDailyInstreamFlowDataObject.FFileHeader.FData := LFileData[LLinesRead];
        LDailyInstreamFlowDataObject.FFileHeader.FInitalised := True;
        LDailyInstreamFlowDataObject.FFileHeader.FLength := Length(LFileData[LLinesRead]);
      end
      else
      begin
        LDailyInstreamFlowData :=  LDailyInstreamFlowDataObject.AddDailyInstreamFlowData;
        if not LDailyInstreamFlowData.Initialise then
          Exit;
        LDailyInstreamFlowData.FIdentifier.FData := LLinesRead + 1;
        LDailyInstreamFlowData.FIdentifier.FInitalised := True;
        LDailyInstreamFlowData.FInstreamDate.FData := FloatToStr(LReadReal);
        LDailyInstreamFlowData.FInstreamDate.FInitalised := True;

        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          inc(LTotalDaysOfMissingFlow);
          LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strAvgFactorErr');
          LMessage := Format(LMessage,[LDailyInstreamFlowData.FInstreamDate.FData]);
          AProgressFunction(LMessage,ptWarning,LStop);
        end
        else
        begin
          LDailyInstreamFlowData.FAvgFlow.FData := LReadReal;
          LDailyInstreamFlowData.FAvgFlow.FInitalised := True;

          LTempString := GetCommaDelimetedValue(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strQualityCodeErr');
            LMessage := Format(LMessage,[LDailyInstreamFlowData.FInstreamDate.FData]);
            AProgressFunction(LMessage,ptWarning,LStop);
          end
          else
          begin
            LDailyInstreamFlowData.FQualityCode.FData := LReadInteger;
            LDailyInstreamFlowData.FQualityCode.FInitalised := True;
          end;
        end;
      end;

      // Read Data...
      LReplaceNegativeWithZero := False;
      LNegativeValueFound      := False;
      for LLinesRead := 1 to  LFileData.Count -1 do
      begin
        LReadString := '';
        LReadString := LFileData[LLinesRead];
        if (Pos('ZZZ',LReadString) > 0 ) then
        begin
          Exit;
        end;
        if (Trim(LReadString) = '') then
          Continue;
        LDailyInstreamFlowData :=  LDailyInstreamFlowDataObject.AddDailyInstreamFlowData;
        if not LDailyInstreamFlowData.Initialise then
          Exit;
        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strDateErr');
          LMessage := Format(LMessage,[LLinesRead]);
          AProgressFunction(LMessage,ptWarning,LStop);
        end
        else
        begin
          if (LDailyInstreamFlowDataObject.FFileHeader.FInitalised) then
            LDailyInstreamFlowData.FIdentifier.FData := LLinesRead
          else
            LDailyInstreamFlowData.FIdentifier.FData := LLinesRead + 1;
          LDailyInstreamFlowData.FIdentifier.FInitalised := True;
          LDailyInstreamFlowData.FInstreamDate.FData := FloatToStr(LReadReal);
          LDailyInstreamFlowData.FInstreamDate.FInitalised := True;

          if not (LDailyInstreamFlowDataObject.FStartDate.FInitalised) and (LLinesRead = 1) then
          begin
            LDailyInstreamFlowDataObject.FStartDate.FData := FloatToStr(LReadReal);
            LDailyInstreamFlowDataObject.FStartDate.FInitalised := True;
          end;
        end;
        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          inc(LTotalDaysOfMissingFlow);
          LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strAvgFactorErr');
          LMessage := Format(LMessage,[LDailyInstreamFlowData.FInstreamDate.FData]);
          AProgressFunction(LMessage,ptWarning,LStop);
        end
        else
        begin
          if(LReadReal < 0) then
          begin
            if not LNegativeValueFound then
            begin
              LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.ReplaceNegativeDiversionFlowValues');
              if MessageDlg(LMessage,mtWarning,mbOKCancel,0) = mrOk then
                LReplaceNegativeWithZero := True
              else
                Exit;
              LNegativeValueFound := True;
            end;
            
            if LReplaceNegativeWithZero then
              LReadReal := 0;
          end;
          LDailyInstreamFlowData.FAvgFlow.FData       := LReadReal;
          LDailyInstreamFlowData.FAvgFlow.FInitalised := True;
        end;

        LTempString := GetCommaDelimetedValue(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strQualityCodeErr');
          LMessage := Format(LMessage,[LDailyInstreamFlowData.FInstreamDate.FData]);
          AProgressFunction(LMessage,ptWarning,LStop);
        end
        else
        begin
          LDailyInstreamFlowData.FQualityCode.FData := LReadInteger;
          LDailyInstreamFlowData.FQualityCode.FInitalised := True;
        end;

      end;
      if LDailyInstreamFlowDataObject.DailyFlowDataCount > 0 then
      begin
        LDailyInstreamFlowDataObject.FEndDate.FData  := LDailyInstreamFlowDataObject.GetDailyInstreamFlowDataByIndex(LDailyInstreamFlowDataObject.DailyFlowDataCount-1).FInstreamDate.FData;
        LDailyInstreamFlowDataObject.FEndDate.FInitalised := True;
      end;

      LDailyDiversionGaugeData := TDailyDiversionDataObject(FAppModules.Model.ModelData);

      LInstreamStartDate := EncodeDate(StrToInt(Copy(LDailyInstreamFlowDataObject.FStartDate.FData, 1, 4)),
                       StrToInt(Copy(LDailyInstreamFlowDataObject.FStartDate.FData, 5, 2)),
                       StrToInt(Copy(LDailyInstreamFlowDataObject.FStartDate.FData, 7, 2)));

      LInstreamEndDate := EncodeDate(StrToInt(Copy(LDailyInstreamFlowDataObject.FEndDate.FData, 1, 4)),
                       StrToInt(Copy(LDailyInstreamFlowDataObject.FEndDate.FData, 5, 2)),
                       StrToInt(Copy(LDailyInstreamFlowDataObject.FEndDate.FData, 7, 2)));


      if (LDailyDiversionGaugeData.DailyDiversionGaugeDataList.DiversionGaugeByIndex[0].StartDate<>LInstreamStartDate)or
         (LDailyDiversionGaugeData.DailyDiversionGaugeDataList.DiversionGaugeByIndex[0].EndDate<>LInstreamEndDate) then
      begin
        LMessage := 'Error: Diversion Flow Start and End Period must be the same as Reference Flow Start and End Period.';
        AProgressFunction(LMessage,ptError,LStop);
        if (LStop) then
          Exit;
      end;

      LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strStartEndDate');
      if (LDailyInstreamFlowDataObject.FStartDate.FInitalised) and (LDailyInstreamFlowDataObject.FEndDate.FInitalised) then
        LMessage := Format(LMessage,[LDailyInstreamFlowDataObject.FStartDate.FData,LDailyInstreamFlowDataObject.FEndDate.FData]);
      AProgressFunction(LMessage,ptNone,LStop);

      LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strTotalDaysOfMissingFlow');
      LMessage := Format(LMessage,[LTotalDaysOfMissingFlow]);
      AProgressFunction(LMessage,ptNone,LStop);

    finally
      FreeAndNil(LFileData);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyInstreamFlowDataAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyInstreamFlowDataAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LStop          : boolean;
begin
  Result := False;
  try

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDataAgent.strWritingStarted');
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
