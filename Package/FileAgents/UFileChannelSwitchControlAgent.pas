//
//
//  UNIT      : Contains TFileChannelSwitchControlAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileChannelSwitchControlAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UChannelSwitchControlFileDataObjects,
  UPlanningFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UPlanningModelDataObject;

type

  TFileChannelSwitchControlAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  UUtilities,
  UFilesLineTypeObject,
  VoaimsCom_TLB,
  UErrorHandlingOperations;

function TFileChannelSwitchControlAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileChannelSwitchControlAgent.ReadModelDataFromFile';
var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LChannelCount,
  LStart,
  LCount,
  LReadInteger,
  LErrorCode : Integer;
  LReadReal : Double;
  LStop: boolean;
  LSwitchChannel         : TChannelSwitchControlObject;
  LChannelSwitchControl  : TChannelSwitchControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
  LLocalCount             : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strReadingStarted');
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
    LChannelSwitchControl    := LPlanningFileDataObject.AddChannelSwitchControlFileDataObject(AFilename.FileNumber);
    if(LChannelSwitchControl = nil) then
      Exit;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LChannelSwitchControl.Initialise then
      Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;


    LChannelSwitchControl.SwitchDefFileName.FData       := AFilename.FileName;
    LChannelSwitchControl.SwitchDefFileName.FLength     := Length(AFilename.FileName);
    LChannelSwitchControl.SwitchDefFileName.FInitalised := True;

    LFileData := TStringList.Create;
    try
      //Read the F01 file
      LFileData.LoadFromFile(AFilename.FileName);
      LStart := 0;

      //line 1 +++++++++++++++++++++++++
      LChannelCount := 0;
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strChannelSwitchControlCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      {if(LReadInteger > 20) then
      begin
        LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strChannelSwitchControlCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      }begin
        LChannelCount := LReadInteger;
      end;

      //line 2 +++++++++++++++++++++++++
      for LCount := 1 to LChannelCount do
      begin

        if(LStart >= LFileData.Count) or  (Trim(LFileData[LStart]) = '')  then
          Exit;

        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LSwitchChannel := LChannelSwitchControl.AddChannelSwitchControl;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strSwitchTypeErr');
          LMessage := Format(LMessage,[LStart,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSwitchChannel.SwitchType.FData       := LReadInteger;
          LSwitchChannel.SwitchType.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strChannelNumberErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSwitchChannel.ChannelNumber.FData       := LReadInteger;
          LSwitchChannel.ChannelNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strNodeNumberErr');
          LMessage := Format(LMessage,[LStart,4]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSwitchChannel.NodeNumber.FData       := LReadInteger;
          LSwitchChannel.NodeNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strSwitchLevelErr');
          LMessage := Format(LMessage,[LStart,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSwitchChannel.SwitchLevel.FData       := LReadReal;
          LSwitchChannel.SwitchLevel.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strSwitchInitialStateErr');
          LMessage := Format(LMessage,[LStart,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSwitchChannel.SwitchInitialState.FData       := LReadInteger;
          LSwitchChannel.SwitchInitialState.FInitalised := True;
        end;
      end;

      for LCount := LStart  to LFileData.Count - 1 do
        LChannelSwitchControl.FMExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileChannelSwitchControlAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileChannelSwitchControlAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LCount         : Integer;
  LSwitchChannel         : TChannelSwitchControlObject;
  LChannelSwitchControl  : TChannelSwitchControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LChannelSwitchControl   := LPlanningFileDataObject.ChannelSwitchControlFileDataObjectByFileNumber[AFilename.FileNumber];

    if(LChannelSwitchControl = nil) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileAllocationDefinitionAgent.strFleDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strWritingStarted');
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

    LFileData:= TStringList.Create;
    try
      //line 1 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=IntToStr(LChannelSwitchControl.ChannelSwitchControlCount);
      LOutString:=LOutString+ '   ' + LTempString;
      LFileData.Add(LOutString);

      for LCount := 0 to LChannelSwitchControl.ChannelSwitchControlCount -1 do
      begin
        //line 3 +++++++++++++++++++++++++
        LSwitchChannel := LChannelSwitchControl.ChannelSwitchControlByIndex[LCount];
        LOutString:='';

        LTempString:=PadInt(LSwitchChannel.SwitchType);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LSwitchChannel.ChannelNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LSwitchChannel.NodeNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LSwitchChannel.SwitchLevel);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LSwitchChannel.SwitchInitialState);
        LOutString:=LOutString+LTempString;
        LFileData.Add(LOutString);
      end;

      for LCount := 0 to LChannelSwitchControl.FMExtraLines.Count -1 do
      begin
        LFileData.Add(LChannelSwitchControl.FMExtraLines[LCount]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileChannelSwitchControlAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
