//
//
//  UNIT      : Contains TFile11Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile11Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UMinFlowChannelObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile11Agent = class(TAbstractFileAgent)
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
  UErrorHandlingOperations,
  UChannelDescriptionObject;

function TFile11Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile11Agent.ReadModelDataFromFile';
Var
  LFileData             : TStringList;
  LMessage              : string;
  LReadString           : string;
  LTempString           : string;
  LStart                : integer;
  LCount                : integer;
  LLinesRead            : integer;
  LErrorCode            : integer;
  LReadReal             : Double;
  LMinFlowChannelObject : TMinFlowAndLossChannelObject;
  LMinFlowChannel       : TMinFlowChannel;
  LLossChannel          : TLossChannel;
  LFileLineTypesObject  : TAbstractFileLineTypesObject;
  LStop                 : boolean;
  lChannelDescr         : TChannelDescrObject;
  lLossChannelObj       : TLossChannelObject;
  lChannelNumber        : integer;
  lFeatureName          : string;
  lChannelCount         : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile11Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile11Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LMinFlowChannelObject := ADataObject.FMinFlowChannelObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LMinFlowChannelObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F11 file
      LFileData.LoadFromFile(AFilename.FileName);
      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      LChannelDescr := ADataObject.FChannelDescrObject;

      LLinesRead := 0;

      if(lChannelDescr.FMinFlowChannelCount.FData > 0) then
      begin
        lChannelCount := 0;
        while ((lChannelCount < lChannelDescr.FMinFlowChannelCount.FData) AND
               (LLinesRead < LFileData.Count)) do
        begin
          if(Trim(LFileData[LLinesRead]) = '') then
            Break;

          LReadString  := LFileData[LLinesRead];

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
          LMinFlowChannel := TMinFlowChannel.Create;

          LFeatureName   := Copy(LReadString,1,36);
          if (Trim(LFeatureName) <> '') then
          begin
            LMinFlowChannel.FMinFlowChannelName.FData := Trim(LFeatureName);
            LMinFlowChannel.FMinFlowChannelName.FInitalised := True;
          end;

          LTempString    := Copy(LReadString,37,6);
          Val(LTempString, lChannelNumber, LErrorCode);
          if (LErrorCode <> 0) or (lChannelNumber = 0) then
          begin
            LMinFlowChannel.Free;
            Break;
            //LMessage := FAppModules.Language.GetString('TFile11Agent.strMinFlowChannelNumberErr');
            //LMessage := Format(LMessage,[LLinesRead+1,37,42]);
            //AProgressFunction(LMessage,ptError,LStop);
            //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMinFlowChannel.FMinFlowChannelNumber.FData := lChannelNumber;
            LMinFlowChannel.FMinFlowChannelNumber.FInitalised := True;
            if not LMinFlowChannel.FMinFlowChannelName.FInitalised then
            begin
              LMinFlowChannel.FMinFlowChannelName.FData := 'Channel '+IntToStr(lChannelNumber);
              LMinFlowChannel.FMinFlowChannelName.FInitalised :=True;
            end;
          end;

          LLinesRead := LLinesRead + 1;
          LMinFlowChannelObject.FMinFlowChannelsLines.Add(LMinFlowChannel);

          // Line 1a
          if (LLinesRead < LFileData.Count) then
          begin
            LReadString := LFileData[LLinesRead];
            LLinesRead := LLinesRead + 1;

            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1a');
            LStart := 1;
            for LCount := MinMinFlow to MaxMinFlow do
            begin
              LTempString := Copy(LReadString,LStart,6);
              if(Trim(LTempString) = '') then
                Break;
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile11Agent.strMinFlowChannelErr');
                LMessage := Format(LMessage,[LLinesRead+1,LStart,LStart+5]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LMinFlowChannel.FMinFlowValues[LCount].FData := LReadReal;
                LMinFlowChannel.FMinFlowValues[LCount].FInitalised := True;
              end;
              LStart := LStart + 6;
            end;
          end;
          lChannelCount := lChannelCount + 1;
        end;

        //Skip 2 empty lines
        for LCount := 1 to 2 do
        begin
          if (LLinesRead >= LFileData.Count) then Break;
          LMinFlowChannelObject.FF11ExtraLines.Add(LFileData[LLinesRead]);
          LLinesRead := LLinesRead + 1;
        end;
      end;

      lChannelCount := 0;
      //lLossChannelObj := nil;
      while ((lChannelCount < lChannelDescr.FLossChannelCount.FData) AND
             (LLinesRead < LFileData.Count)) do
      begin
        if (Trim(LFileData[LLinesRead]) = '') then
          Break;

        LReadString  := LFileData[LLinesRead];

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LLossChannel := TLossChannel.Create;

        LFeatureName   := Copy(LReadString,1,36);
        if (Trim(LFeatureName) <> '') then
        begin
          LLossChannel.FLossChannelName.FData := Trim(LFeatureName);
          LLossChannel.FLossChannelName.FInitalised := True;
        end;

        LTempString    := Copy(LReadString,37,6);
        Val(LTempString, lChannelNumber, LErrorCode);
        if (LErrorCode <> 0) or (lChannelNumber = 0) then
        begin
          LLossChannel.Free;
          Break;
          //LMessage := FAppModules.Language.GetString('TFile11Agent.strMinFlowChannelNumberErr');
          //LMessage := Format(LMessage,[LLinesRead+1,37,42]);
          //AProgressFunction(LMessage,ptError,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LLossChannel.FLossChannelNumber.FData := lChannelNumber;
          LLossChannel.FLossChannelNumber.FInitalised := True;
          lLossChannelObj := lChannelDescr.FindLossChannels(lChannelNumber);
        end;

        LLinesRead := LLinesRead + 1;
        LMinFlowChannelObject.FLossChannelsLines.Add(LLossChannel);

        // Line 2a
        if (LLinesRead < LFileData.Count) then
        begin
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;

          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2a');
          LStart := 1;
          for LCount := MinLoss to MaxLoss do
          begin
            LTempString := Copy(LReadString,LStart,6);
            if(Trim(LTempString) = '') then
              Break;
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if (LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile11Agent.strLossChannelErr');
              LMessage := Format(LMessage,[LLinesRead+1,LStart,LStart+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LLossChannel.FLossValues[LCount].FData := LReadReal;
              LLossChannel.FLossValues[LCount].FInitalised := True;
            end;
            LStart := LStart + 6;
          end;
        end;

        if ((LLinesRead < LFileData.Count) AND
            (Assigned(lLossChannelObj)) AND
            (lLossChannelObj.FChannelType.FData = 1)) then
        begin
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;
          // Line 2b
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2b');
          LStart := 1;
          for LCount := MinDiverted to MaxDiverted do
          begin
            LTempString := Copy(LReadString,LStart,6);
            if(Trim(LTempString) = '') then
              Break;
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile11Agent.strDivertedChannelErr');
              LMessage := Format(LMessage,[LLinesRead+1,LStart,LStart+5]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LLossChannel.FDivertedValues[LCount].FData := LReadReal;
              LLossChannel.FDivertedValues[LCount].FInitalised := True;
            end;
            LStart := LStart + 6;
          end;
        end;
        lChannelCount := lChannelCount + 1;
      end;

      //Skip 2 empty lines
      //if (lChannelDescr.FLossChannelCount.FData > 0) then
      //  LLinesRead := LLinesRead + 2;

      for LLinesRead := LLinesRead to LFileData.Count - 1 do
        LMinFlowChannelObject.FF11ExtraLines.Add(LFileData[LLinesRead]);

      LMessage := FAppModules.Language.GetString('TFile11Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile11Agent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString,
  LTempString  : String;
  LLinesCount,
  LCount: Integer;
  LF11File :TStringlist;
  LMinFlowChannelObject: TMinFlowAndLossChannelObject;
  LMinFlowChannel:TMinFlowChannel;
  LLossChannel:TLossChannel;
  //LLossChannelWritten: boolean;
  LMinFlowChannelWritten: boolean;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile11Agent.strWritingStarted');
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

    LF11File:= TStringList.Create;
    LMinFlowChannelObject := ADataObject.FMinFlowChannelObject;
    if not Assigned(LMinFlowChannelObject) then
      Exit;

    try
      LMinFlowChannelWritten := False;
      for LLinesCount := 0 to LMinFlowChannelObject.FMinFlowChannelsLines.Count - 1 do
      begin
        LMinFlowChannel := TMinFlowChannel(LMinFlowChannelObject.FMinFlowChannelsLines[LLinesCount]);
        if Assigned(LMinFlowChannel) then
        begin
          if(LMinFlowChannel.FMinFlowChannelName.FInitalised and
             LMinFlowChannel.FMinFlowChannelNumber.FInitalised) then
          begin
            //Line 1
            LOutString:='';

            LTempString:=PadString(LMinFlowChannel.FMinFlowChannelName);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LMinFlowChannel.FMinFlowChannelNumber);
            LOutString:=LOutString+LTempString;

            LF11File.Add(LOutString);

            //Line 1a
            LOutString:='';
            for LCount := MinMinFlow to MaxMinFlow do
            begin
              if not LMinFlowChannel.FMinFlowValues[LCount].FInitalised then
                Break;
              LTempString:=PadDouble(LMinFlowChannel.FMinFlowValues[LCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF11File.Add(LOutString);
            LMinFlowChannelWritten := True;
          end;
        end;
      end;

      //Two empty lines
      if LMinFlowChannelWritten then
      begin
        LF11File.Add(LMinFlowChannelObject.FF11ExtraLines[0]);
        LF11File.Add(LMinFlowChannelObject.FF11ExtraLines[1]);
      end;

      //LLossChannelWritten := False;
      for LLinesCount := 0 to LMinFlowChannelObject.FLossChannelsLines.Count - 1 do
      begin
        LLossChannel := TLossChannel(LMinFlowChannelObject.FLossChannelsLines[LLinesCount]);
        if Assigned(LLossChannel) then
        begin
          if(LLossChannel.FLossChannelName.FInitalised and
             LLossChannel.FLossChannelNumber.FInitalised) then
          begin
            //Line 2
            LOutString:='';

            LTempString:=PadString(LLossChannel.FLossChannelName);
            LOutString:=LOutString+LTempString;

            LTempString:=PadInt(LLossChannel.FLossChannelNumber);
            LOutString:=LOutString+LTempString;

            LF11File.Add(LOutString);

            //Line 2a
            LOutString:='';
            for LCount := MinLoss to MaxLoss do
            begin
              if not LLossChannel.FLossValues[LCount].FInitalised then
                Break;
              LTempString:=PadDouble(LLossChannel.FLossValues[LCount]);
              LOutString:=LOutString+LTempString;
            end;
            LF11File.Add(LOutString);

            //Line 2b
            LOutString:='';
            for LCount := MinDiverted to MaxDiverted do
            begin
              if not LLossChannel.FDivertedValues[LCount].FInitalised then
                Break;
              LTempString:=PadDouble(LLossChannel.FDivertedValues[LCount]);
              LOutString:=LOutString+LTempString;
            end;
            if(LOutString <> '') then
              LF11File.Add(LOutString);

          end;
        end;
      end;

      //Two empty lines
      LCount := 0;
      if LMinFlowChannelWritten then
        LCount := 2;

      for LLinesCount := LCount to LMinFlowChannelObject.FF11ExtraLines.Count -1 do
      begin
        LF11File.Add(LMinFlowChannelObject.FF11ExtraLines[LLinesCount]);
      end;

      LF11File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFile11Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LF11File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

