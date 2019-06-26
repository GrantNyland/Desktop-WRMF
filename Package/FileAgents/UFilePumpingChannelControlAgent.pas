//
//
//  UNIT      : Contains TFilePumpingChannelControlAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFilePumpingChannelControlAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UPumpingChannelControlFileDataObjects,
  UPlanningFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UPlanningModelDataObject;

type

  TFilePumpingChannelControlAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  UUtilities,
  VoaimsCom_TLB,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

function TFilePumpingChannelControlAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePumpingChannelControlAgent.ReadModelDataFromFile';
var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  //LIndex,
  //LChannelCount,
  LStart,
  LCount,
  LReadInteger,
  LErrorCode : Integer;
  LReadReal : Double;
  LStop: boolean;
  LFactors: TStringList;
  LFactorLines: TStringList;
  LPumpingChannel         : TPumpingChannelControlObject;
  LPumpingChannelControl  : TPumpingChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
  LLocalCount             : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strReadingStarted');
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
    LPumpingChannelControl    := LPlanningFileDataObject.PumpingChannelControlFileData;
    if(LPumpingChannelControl = nil) then
      Exit;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LPumpingChannelControl.Initialise then
      Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    LFactors  := TStringList.Create;
    LFactorLines  := TStringList.Create;
    try
      //Read the F01 file
      LFileData.LoadFromFile(AFilename.FileName);
      LStart := 0;

      //line 1 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
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
        LPumpingChannelControl.DataYears.FData       := LReadInteger;
        LPumpingChannelControl.DataYears.FInitalised := True;
      end;

      while (LStart < LFileData.Count) and  (Trim(LFileData[LStart]) <> '')  do
      begin

        //line 2 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LPumpingChannel := LPumpingChannelControl.AddPumpingChannelControl;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
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
          LPumpingChannel.ChannelNumber.FData       := LReadInteger;
          LPumpingChannel.ChannelNumber.FInitalised := True;
          if(LPumpingChannel.ChannelNumber.FData = 0) then Break;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strYearChannelActiveErr');
          LMessage := Format(LMessage,[LStart,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.YearChannelActive.FData       := LReadInteger;
          LPumpingChannel.YearChannelActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strMonthChannelActiveErr');
          LMessage := Format(LMessage,[LStart,4]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.MonthChannelActive.FData       := LReadInteger;
          LPumpingChannel.MonthChannelActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strYearChannelAbsoleteErr');
          LMessage := Format(LMessage,[LStart,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.YearChannelAbsolete.FData       := LReadInteger;
          LPumpingChannel.YearChannelAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strMonthChannelAbsoleteErr');
          LMessage := Format(LMessage,[LStart,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.MonthChannelAbsolete.FData       := LReadInteger;
          LPumpingChannel.MonthChannelAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strEconomicLifeOfChannelErr');
          LMessage := Format(LMessage,[LStart,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.EconomicLifeOfChannel.FData       := LReadInteger;
          LPumpingChannel.EconomicLifeOfChannel.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
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
          LPumpingChannel.CapitalCost.FData       := LReadReal;
          LPumpingChannel.CapitalCost.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strFixedMaintenanceCostErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.FixedMaintenanceCost.FData       := LReadReal;
          LPumpingChannel.FixedMaintenanceCost.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strVariableMaintenanceCostErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.VariableMaintenanceCost.FData       := LReadReal;
          LPumpingChannel.VariableMaintenanceCost.FInitalised := True;
        end;

        //line 3 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strYearsInConstructionErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPumpingChannel.YearsInConstruction.FData       := LReadInteger;
          LPumpingChannel.YearsInConstruction.FInitalised := True;
        end;

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
          LTempString  := CommaTextString(LTempString);
          LPumpingChannel.CostSchedule.FData       := LTempString;
          LPumpingChannel.CostSchedule.FLength     := Length(LTempString);
          LPumpingChannel.CostSchedule.FInitalised := True;
        end;

        //line 4 +++++++++++++++++++++++++
        LFactors.Clear;
        LFactorLines.Clear;
        for LCount := 1 to LPumpingChannelControl.DataYears.FData do
        begin
          if (LStart >= LFileData.Count) then
          begin
            Break;
          end;
          LReadString := LFileData[LStart];
          LStart      := LStart + 1;

          LTempString := CommaTextString(LReadString);
          if(Trim(LTempString) <> '') then
          begin
            LFactorLines.Add(LTempString);
            if(LFactors.Count > 0) then
              LFactors.CommaText := LFactors.CommaText + ',' + LTempString
            else
             LFactors.CommaText := LTempString;
          end;

          if(StringsItemsCount(LFactors) >= LPumpingChannelControl.DataYears.FData) then
            Break;
        end;

        for LCount := 0 to LFactors.Count-1 do
        begin
          if(pos('*',LFactors[LCount]) > 0) then Continue;
          Val(LFactors[LCount],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strMinMaxChannelGrowthFactorsErr');
            LMessage := Format(LMessage,[LStart]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        LPumpingChannel.EscaltionCosts.FData       := LFactorLines.Text;
        LPumpingChannel.EscaltionCosts.FLength     := Length(LPumpingChannel.EscaltionCosts.FData);
        LPumpingChannel.EscaltionCosts.FInitalised := True;

        {LTempString:=Trim(LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strEscaltionCostsErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LTempString  := CommaTextString(LTempString);
          LPumpingChannel.EscaltionCosts.FData       := LTempString;
          LPumpingChannel.EscaltionCosts.FLength     := Length(LTempString);
          LPumpingChannel.EscaltionCosts.FInitalised := True;
        end;}
      end;

      for LCount := LStart  to LFileData.Count - 1 do
        LPumpingChannelControl.FMExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
      FreeAndNil(LFactors);
      FreeAndNil(LFactorLines);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePumpingChannelControlAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePumpingChannelControlAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LCount         : Integer;
  LIndex         : Integer;
  LFactors       : TStringList;
  LPumpingChannel         : TPumpingChannelControlObject;
  LPumpingChannelControl  : TPumpingChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LPumpingChannelControl   := LPlanningFileDataObject.PumpingChannelControlFileData;

    if(LPumpingChannelControl.PumpingChannelControlCount  = 0) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strWritingStarted');
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
    LFactors  := TStringList.Create;
    try
      //line 1 +++++++++++++++++++++++++
      LOutString:=PadInt(LPumpingChannelControl.DataYears);
      LFileData.Add(LOutString);

      for LCount := 0 to LPumpingChannelControl.PumpingChannelControlCount -1 do
      begin
        //line 2 +++++++++++++++++++++++++
        LPumpingChannel := LPumpingChannelControl.PumpingChannelControlByIndex[LCount];
        LOutString:='';

        LTempString:=PadInt(LPumpingChannel.ChannelNumber);
        LOutString:=LOutString+LTempString;
        if(LPumpingChannel.ChannelNumber.FData = 0) then
        begin
          LFileData.Add(LOutString);
          Continue;
        end;

        LTempString:=PadInt(LPumpingChannel.YearChannelActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LPumpingChannel.MonthChannelActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LPumpingChannel.YearChannelAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LPumpingChannel.MonthChannelAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LPumpingChannel.EconomicLifeOfChannel);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LPumpingChannel.CapitalCost);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LPumpingChannel.FixedMaintenanceCost);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LPumpingChannel.VariableMaintenanceCost);
        LOutString:=LOutString+LTempString;

        LFileData.Add(LOutString);

        //line 3 +++++++++++++++++++++++++
        LOutString:='';

        LTempString:=PadInt(LPumpingChannel.YearsInConstruction);
        LOutString:=LOutString+LTempString;
        LOutString:=LOutString+ ' ' + TrimRight(LPumpingChannel.CostSchedule.FData);

        LFileData.Add(LOutString);

        //line 4 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LPumpingChannel.EscaltionCosts.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add('     ' + UnCommaTextString(LFactors[LIndex]));
        end;

        {LOutString:='';
        LOutString:=LOutString+ '     ' + LPumpingChannel.EscaltionCosts.FData;
        LFileData.Add(LOutString);}
      end;

      for LCount := 0 to LPumpingChannelControl.FMExtraLines.Count -1 do
      begin
        LFileData.Add(LPumpingChannelControl.FMExtraLines[LCount]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
      LFactors.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
