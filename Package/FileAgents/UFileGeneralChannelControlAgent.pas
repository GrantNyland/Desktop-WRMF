//
//
//  UNIT      : Contains TFileGeneralChannelControlAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileGeneralChannelControlAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UGeneralChannelControlFileDataObjects,
  UPlanningFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UPlanningModelDataObject;

type

  TFileGeneralChannelControlAgent = class(TAbstractFileAgent)
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

function TFileGeneralChannelControlAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGeneralChannelControlAgent.ReadModelDataFromFile';
var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LIndex,
  LChannelCount,
  LStart,
  LCount,
  LReadInteger,
  LErrorCode : Integer;
  LReadReal : Double;
  LStop: boolean;
  LFactors: TStringList;
  LFactorLines: TStringList;
  LGeneralChannel         : TGeneralChannelControlObject;
  LGeneralChannelControl  : TGeneralChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
  LLocalCount             : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;
    
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strReadingStarted');
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
    LGeneralChannelControl    := LPlanningFileDataObject.GeneralChannelControlFileData;
    if(LGeneralChannelControl = nil) then
      Exit;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LGeneralChannelControl.Initialise then
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
        LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strGeneralChannelControlCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      {if(LReadInteger > 100) then
      begin
        LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strGeneralChannelControlCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      }begin
        LGeneralChannelControl.DataYears.FData       := LReadInteger;
        LGeneralChannelControl.DataYears.FInitalised := True;
      end;

      //line 2 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;
      LChannelCount := 0;

      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strGeneralChannelCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      {if(LReadInteger > 100) then
      begin
        LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strGeneralChannelCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      }begin
        LChannelCount       := LReadInteger;
      end;

      for LCount := 1 to LChannelCount do
      begin

        if(LStart >= LFileData.Count) or  (Trim(LFileData[LStart]) = '')  then
          Exit;

        //line 3 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LGeneralChannel := LGeneralChannelControl.AddGeneralChannelControl;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strChannelNumberErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.ChannelNumber.FData       := LReadInteger;
          LGeneralChannel.ChannelNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strYearChannelActiveErr');
          LMessage := Format(LMessage,[LStart,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.YearChannelActive.FData       := LReadInteger;
          LGeneralChannel.YearChannelActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strMonthChannelActiveErr');
          LMessage := Format(LMessage,[LStart,4]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.MonthChannelActive.FData       := LReadInteger;
          LGeneralChannel.MonthChannelActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strYearChannelAbsoleteErr');
          LMessage := Format(LMessage,[LStart,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.YearChannelAbsolete.FData       := LReadInteger;
          LGeneralChannel.YearChannelAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strMonthChannelAbsoleteErr');
          LMessage := Format(LMessage,[LStart,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.MonthChannelAbsolete.FData       := LReadInteger;
          LGeneralChannel.MonthChannelAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strEconomicLifeOfChannelErr');
          LMessage := Format(LMessage,[LStart,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.EconomicLifeOfChannel.FData       := LReadInteger;
          LGeneralChannel.EconomicLifeOfChannel.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strCapitalCostErr');
          LMessage := Format(LMessage,[LStart,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.CapitalCost.FData       := LReadReal;
          LGeneralChannel.CapitalCost.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strFixedMaintenanceCostErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.FixedMaintenanceCost.FData       := LReadReal;
          LGeneralChannel.FixedMaintenanceCost.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strVariableMaintenanceCostErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.VariableMaintenanceCost.FData       := LReadReal;
          LGeneralChannel.VariableMaintenanceCost.FInitalised := True;
        end;

        //line 4 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strYearsInConstructionErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGeneralChannel.YearsInConstruction.FData       := LReadInteger;
          LGeneralChannel.YearsInConstruction.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
        LTempString := Trim(LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strCostScheduleErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LTempString  := CommaTextString(LTempString);
          LGeneralChannel.CostSchedule.FData       := LTempString;
          LGeneralChannel.CostSchedule.FLength     := Length(LTempString);
          LGeneralChannel.CostSchedule.FInitalised := True;
        end;

        //line 5 +++++++++++++++++++++++++
        LFactors.Clear;
        LFactorLines.Clear;
        for LIndex := 1 to LGeneralChannelControl.DataYears.FData do
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

          if(StringsItemsCount(LFactors) >= LGeneralChannelControl.DataYears.FData) then
            Break;
        end;

        for LIndex := 0 to LFactors.Count-1 do
        begin
          if(pos('*',LFactors[LIndex]) > 0) then Continue;
          Val(LFactors[LIndex],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strMinMaxChannelGrowthFactorsErr');
            LMessage := Format(LMessage,[LStart]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        LGeneralChannel.EscaltionCosts.FData       := LFactorLines.Text;
        LGeneralChannel.EscaltionCosts.FLength     := Length(LGeneralChannel.EscaltionCosts.FData);
        LGeneralChannel.EscaltionCosts.FInitalised := True;

        {LTempString := Trim(LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strEscaltionCostsErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LTempString  := CommaTextString(LTempString);
          LGeneralChannel.EscaltionCosts.FData       := LTempString;
          LGeneralChannel.EscaltionCosts.FLength     := Length(LTempString);
          LGeneralChannel.EscaltionCosts.FInitalised := True;
        end;}
      end;

      for LCount := LStart  to LFileData.Count - 1 do
        LGeneralChannelControl.FMExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strReadingCompleted');
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

function TFileGeneralChannelControlAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGeneralChannelControlAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LCount         : Integer;
  LIndex         : Integer;
  LFactors       : TStringList;
  LGeneralChannel         : TGeneralChannelControlObject;
  LGeneralChannelControl  : TGeneralChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGeneralChannelControl   := LPlanningFileDataObject.GeneralChannelControlFileData;

    {if(LGeneralChannelControl.GeneralChannelControlCount  = 0) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;}

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strWritingStarted');
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
    LFactors := TStringList.Create;
    try
      //line 1 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=PadInt(LGeneralChannelControl.DataYears);
      LOutString:=LOutString+ ' ' + LTempString;
      LFileData.Add(LOutString);

      //line 2 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:= IntToStr(LGeneralChannelControl.GeneralChannelControlCount);
      LOutString:=LOutString+ '    ' + LTempString;
      LFileData.Add(LOutString);

      for LCount := 0 to LGeneralChannelControl.GeneralChannelControlCount -1 do
      begin
        //line 3 +++++++++++++++++++++++++
        LGeneralChannel := LGeneralChannelControl.GeneralChannelControlByIndex[LCount];
        LOutString:='';

        LTempString:=PadInt(LGeneralChannel.ChannelNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGeneralChannel.YearChannelActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGeneralChannel.MonthChannelActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGeneralChannel.YearChannelAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGeneralChannel.MonthChannelAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LGeneralChannel.EconomicLifeOfChannel);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGeneralChannel.CapitalCost);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGeneralChannel.FixedMaintenanceCost);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LGeneralChannel.VariableMaintenanceCost);
        LOutString:=LOutString+LTempString;

        LFileData.Add(LOutString);

        //line 4 +++++++++++++++++++++++++
        LOutString:='';

        LTempString:=PadInt(LGeneralChannel.YearsInConstruction);
        LOutString:=LOutString+LTempString;
        LOutString:=LOutString+ ' ' + TrimRight(LGeneralChannel.CostSchedule.FData);

        LFileData.Add(LOutString);

        //line 5 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LGeneralChannel.EscaltionCosts.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add('     ' + UnCommaTextString(LFactors[LIndex]));
        end;

        {LOutString:='';
        LOutString:=LOutString+ '     ' + LGeneralChannel.EscaltionCosts.FData;
        LFileData.Add(LOutString);}
      end;

      for LCount := 0 to LGeneralChannelControl.FMExtraLines.Count -1 do
      begin
        LFileData.Add(LGeneralChannelControl.FMExtraLines[LCount]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileGeneralChannelControlAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
