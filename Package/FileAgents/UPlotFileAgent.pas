unit UPlotFileAgent;
//
//
//  UNIT      : Contains TPlotFileAgentClass
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 29/01/2009
//  COPYRIGHT : Copyright © 2001 DWAF
//
//

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
  URunParametersObject;

  type
  TPlotFileAgent = class(TAbstractFileAgent)
  protected
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ReadDataFromFile(ADataType: TOutputDataType;AFileName: TAbstractModelFileName;AContainer: TStrings;
                                          ALoadCase,ASeqNo : integer;AElementType : TNetworkElementType;AElementName,AErrors : string): boolean;

  end;


implementation

uses
  UUtilities,
  UFilesLineTypeObject,
  UYieldModelDataObject,
  UErrorHandlingOperations, UPathsObject;


function TPlotFileAgent.ReadDataFromFile(ADataType: TOutputDataType;AFileName: TAbstractModelFileName;AContainer: TStrings;
                                         ALoadCase,ASeqNo : integer;AElementType : TNetworkElementType;AElementName,AErrors : string): boolean;
const OPNAME = 'TPlotFileAgent.ReadDataFromFile';
var
  //LFile : TextFile;
  LFileData : TStringList;
  LTempData : TStringList;
  LMessage,
  LTempString,
  LComment,
  LReadString : string;
  LCount,
  LLine,
  //LLineCount,
  LErrorCode,
  LReadInteger,
  LCurrentLineNumber,
  LSequenceCount,
  LMonthsCount,
  LChannelCount,
  LChannelIndex,
  //LElementIndex,
  LReservoirIndex,
  LLoadCaseIndex,
  LSequenceIndex,
  LMonthIndex,
  LLoadCases,
  LReservoirCount : integer;
  LReadReal : double;
begin
  Result := False;
  try
    if (AFileName <> nil) and (AContainer <> nil) then
    begin
      LMessage := '';
      if not FileExists(AFileName.FileName) then Exit;
      LFileData := TStringList.Create;
      LTempData := TStringList.Create;
      LFileData.LoadFromFile(AFileName.FileName);
      try
        if (LFileData.Count=0) then
        begin
          LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileEmptyErr');
          LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
          Exit;
        end
        else
        begin
          LCurrentLineNumber := 0;
          LLine := 0;
          LReadString := LFileData[LLine];
          LTempString:=GetSubstring(LReadString,1,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strReservoirCountErr');
            LMessage := Format(LMessage,[LLine,1,5]);
            Exit;
          end
          else
            LReservoirCount:=LReadInteger;

          LTempString:=GetSubstring(LReadString,6,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strSequenceCountErr');
            LMessage := Format(LMessage,[LLine,6,10]);
            Exit;
          end
          else
            LSequenceCount:=LReadInteger;

          LTempString:=GetSubstring(LReadString,11,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strMonthsCountErr');
            LMessage := Format(LMessage,[LLine,11,15]);
            Exit;
          end
          else
            LMonthsCount:=LReadInteger;

          LTempString:=GetSubstring(LReadString,16,5);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strMonthsCountErr');
            LMessage := Format(LMessage,[LLine,16,20]);
            Exit;
          end
          else
            LChannelCount:=LReadInteger;
        end;

        if(FAppModules.StudyArea.ModelVersion = '6') then
        begin

          if (LFileData.Count-1<=LLine) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileNoLine2Err');
            LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
            Exit;
          end
          else
          begin
            LReadString := LFileData[LLine];
            LLine := LLine+1;
            LTempString:=Trim(GetSubstring(LReadString,21,10));
            if(LTempString <> '') then
              LComment:=LTempString;
          end;
        end;
        LReservoirIndex := 0;
        LChannelIndex := 0;
        LLoadCases := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NrOfActiveLoadCases;
        if (Trim(AElementName) = '') then Exit;
        for LCount := 1 to LReservoirCount do
        begin
          LLine := LLine + 1;
          LReadString := LFileData[LLine];
          if UpperCase(Trim(AElementName)) = UpperCase(Trim(Copy(LReadString,1,32))) then
            LReservoirIndex := LCount-1;
        end;

        for LCount := 1 to LChannelCount do
        begin
          LLine := LLine + 1;
          LReadString := LFileData[LLine];
          if UpperCase(Trim(AElementName)) = UpperCase(Trim(Copy(LReadString,1,32))) then
            LChannelIndex := LCount;
        end;
        AContainer.Clear;
        LMonthsCount := LMonthsCount div 12;
        if (AElementType = votReservoir) and (ADataType = btMonthEndReservoirVolume) and (LReservoirCount>0) then
        begin
          if (LFileData.Count-1>LLine) then
          begin
            for LLoadCaseIndex := 1 to LLoadCases do
            begin
              if (LFileData.Count-1<=LLine) then Break;
              for LSequenceIndex := 1 to LSequenceCount do
              begin
                if (ASeqNo<>0) and (ASeqNo<LSequenceIndex) then Break;
                if (LFileData.Count-1<=LLine) then Break;
                for LMonthIndex := 1 to LMonthsCount do
                begin
                  if ((LFileData.Count-1) <= LLine) then Break;
                  LLine := LLine + LReservoirIndex+LChannelIndex+1;
                  LReadString := LFileData[LLine];
                  LTempString := Trim(LReadString);
                  LTempData.Clear;
                  for LCount := MinMonths to MaxMonths do
                  begin
                    LTempString:=Trim(ExtractFirstSubstring(LReadString));
                    Val(LTempString,LReadReal,LErrorCode);
                    if(LErrorCode <> 0) then
                    begin
                      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileResevoirVolumeErr');
                      LMessage := Format(LMessage,[LCurrentLineNumber,1,5]);
                      Break;
                    end
                    else
                      LTempData.Add(FormatFloat('####0.0000',LReadReal));
                  end;
                  LLine := LLine+(LReservoirCount-LReservoirIndex)+LChannelCount+1;
                  if (ALoadCase=LLoadCaseIndex) and ((ASeqNo=LSequenceIndex) or (ASeqNo=0))  then
                    AContainer.Add(LTempData.CommaText);
                end;
              end;
            end;
          end;
        end;

        if (LChannelCount=0) and (AElementType = votChannel) then Exit;

        if (AElementType in  [votChannel,votMasterControl]) and (ADataType = btMonthlyAverageChannelFlow) then
        begin
          if (LFileData.Count-1>LLine) then
          begin
            for LLoadCaseIndex := 1 to LLoadCases do
            begin
              if (LFileData.Count-1<=LLine) then Break;
              for LSequenceIndex := 1 to LSequenceCount do
              begin
                if (ASeqNo<>0) and (ASeqNo<LSequenceIndex) then Break;
                if (LFileData.Count-1<=LLine) then Break;
                for LMonthIndex := 1 to LMonthsCount do
                begin
                  if LChannelIndex = 0 then
                    LLine := LLine + LReservoirCount+LChannelCount+1
                  else
                    LLine := LLine+LReservoirCount+LChannelIndex;
                  if ((LFileData.Count-1) <= LLine) then Break;
                  LReadString := LFileData[LLine];
                  LTempString := Trim(LReadString);
                  LTempData.Clear;
                  for LCount := MinMonths to MaxMonths do
                  begin
                    LTempString:=Trim(ExtractFirstSubstring(LReadString));
                    Val(LTempString,LReadReal,LErrorCode);
                    if(LErrorCode <> 0) then
                    begin
                      LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileChannelFlowErr');
                      LMessage := Format(LMessage,[LCurrentLineNumber,1,5]);
                      Break;
                    end
                    else
                      LTempData.Add(FormatFloat('####0.0000',LReadReal));
                  end;

                  if LChannelIndex = 0 then
                    LLine := LLine+1
                  else
                    LLine := LLine+(LChannelCount-LChannelIndex)+2;
                  if (ALoadCase=LLoadCaseIndex) and ((ASeqNo=LSequenceIndex) or(ASeqNo=0))  then
                    AContainer.Add(LTempData.CommaText);
                end;
              end;
            end;
          end;
        end;
      finally
        LFileData.Free;
        LTempData.Free;
      end;
      Result := True;
    end;
    AErrors := LMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileAgent.ReadModelDataFromFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileAgent.WriteModelDataToFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end. 
