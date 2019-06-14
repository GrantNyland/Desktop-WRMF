//
//  UNIT      : Contains TFileTariffCalculationAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileTariffCalculationAgent;

interface
uses
  Classes, sysutils,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UTariffCalculationFileDataObject,
  UAbstractFileAgent,
  UFilesActionAbstractManager;

Type
  TFileTariffCalculationAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction:TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFilename: TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;

implementation
uses
   UUtilities,
   UFilesLineTypeObject,
   UErrorHandlingOperations,
   UBasicObjects,
   UFilesActionYieldManager;

function TFileTariffCalculationAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileTariffCalculationAgent.ReadModelDataFromFile';
Var
  LFileData  : TStringList;
  LFileAge   : TDateTime;
  LFileName  : string;
  LMessage   : String;
  LReadString,
  LTempString : String;
  LStop      : boolean;
  LStart     : Integer;
  LCount,
  LReadInteger,
  LErrorCode : Integer;
  LReadReal : Double;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LTariffCalculationData  : TTariffCalculationFileDataObject;
  LChannelTariffData      : TChannelTariffData;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileTariffCalculationAgent.strFileNoExist'); // Update
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileTariffCalculationAgent.strReadingStarted'); // Update
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileTariffCalculationAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //TFileNameObject(AFilename).FileDate := FileDateToDateTime(FileAge(AFileName.FileName));
    LFileName := AFileName.FileName;
    FileAge(LFileName,LFileAge);
    TFileNameObject(AFilename).FileDate := LFileAge;

    LPlanningFileDataObject    := TPlanningFileDataObjects(ADataObject);
    LTariffCalculationData     := LPlanningFileDataObject.TariffCalculationData;
    if(LTariffCalculationData = nil) then  Exit;

    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    if not LTariffCalculationData.Initialise then  Exit;

    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);

      LStart := 0;

      //line 1 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strPumpingChannelControlCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      {if(LReadInteger > 100) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strPumpingChannelControlCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      }begin
        LTariffCalculationData.DataYears.FData       := LReadInteger;
        LTariffCalculationData.DataYears.FInitalised := True;
      end;

      while (LStart < LFileData.Count) and  (Trim(LFileData[LStart]) <> '')  do
      begin

        //line 2 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LChannelTariffData := LTariffCalculationData.AddChannelTariff;

        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strChannelNumberErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelTariffData.FChannelNumber.FData       := LReadInteger;
          LChannelTariffData.FChannelNumber.FInitalised := True;
          if(LChannelTariffData.FChannelNumber.FData = 0) then Break;
        end;

        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strCapitalCostErr');
          LMessage := Format(LMessage,[LStart,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelTariffData.FChannelTariff.FData       := LReadReal;
          LChannelTariffData.FChannelTariff.FInitalised := True;
        end;

        //line 3 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LTempString := Trim(LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strCostScheduleErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelTariffData.FEscalationFactors.FData       := LTempString;
          LChannelTariffData.FEscalationFactors.FLength     := Length(LTempString);
          LChannelTariffData.FEscalationFactors.FInitalised := True;
        end;
      end;

      for LCount := LStart  to LFileData.Count - 1 do
        LTariffCalculationData.HDextraLines.Add(LFileData[LStart]);

      LMessage := FAppModules.Language.GetString('TFileTariffCalculationAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E:Exception do  HandleError(E,OPNAME) end;
end;

function TFileTariffCalculationAgent.WriteModelDataToFile(AFilename: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileTariffCalculationAgent.WriteModelDataToFile';
Var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LCount         : Integer;
  LStop          : boolean;
  LFileData      : TStringList;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LTariffCalculationData   : TTariffCalculationFileDataObject;
  LChannelTariffData       : TChannelTariffData;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LTariffCalculationData   :=  LPlanningFileDataObject.TariffCalculationData;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileTariffCalculationAgent.strWritingStarted'); //Update
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
      LOutString:=Trim(PadInt(LTariffCalculationData.DataYears));
      LFileData.Add(LOutString);

      for LCount := 0 to LTariffCalculationData.ChannelTariffCount -1 do
      begin
        //line 2 +++++++++++++++++++++++++
        LChannelTariffData := LTariffCalculationData.ChannelTariffByIndex[LCount];
        LOutString:='';

        LTempString:=PadInt(LChannelTariffData.FChannelNumber);
        LOutString:=LOutString+LTempString;
        LTempString:=PadDouble(LChannelTariffData.FChannelTariff);
        LOutString:=LOutString+LTempString;
        LFileData.Add(LOutString);
        LOutString:='        '+PadString(LChannelTariffData.FEscalationFactors);
        LFileData.Add(LOutString);
      end;

      for LCount := 0 to LTariffCalculationData.HDextraLines.Count - 1 do
        LFileData.Add(LTariffCalculationData.HDextraLines[LCount]);

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

       LMessage := FAppModules.Language.GetString('TFileTariffCalculationAgent.strWritingCompleted'); // Update
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


