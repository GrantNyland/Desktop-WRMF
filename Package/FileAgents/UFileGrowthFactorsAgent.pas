
//
//
//  UNIT      : Contains TFileGrowthFactorsAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 03/05/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFileGrowthFactorsAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  VoaimsCom_TLB,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UGrowthFactorFileObject,
  UAbstractFileAgent,
  UYieldModelDataObject;

type

  TFileGrowthFactorsAgent = class(TAbstractFileAgent)
  protected
    procedure CreateMemberObjects; override;
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

procedure TFileGrowthFactorsAgent.CreateMemberObjects;
const OPNAME = 'TFileGrowthFactorsAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorsAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGrowthFactorsAgent.ReadModelDataFromFile';
Var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LReadInteger,
  LCount,
  LIndex,
  LLinesRead,
  LErrorCode : Integer;
  LFileIncomplete,
  LStop: boolean;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LGrowthFactorFileObject : TGrowthFactorFileObject;
  LDemandCentresFileObject:TGrowthFactorDemandCentresFileObject;
  LMinMaxChannelFileObject:TGrowthFactorMinMaxChannelFileObject;
  LHydroDataFileObject:TGrowthFactorHydroDataFileObject;
  LReadReal : Double;
  LFactors: TStringList;
  LFactorLines: TStringList;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
  LLocalCount             : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGrowthFactorFileObject    := LPlanningFileDataObject.GrowthFactorFileObject;

    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    if not LGrowthFactorFileObject.Initialise then
      Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    LFactors  := TStringList.Create;
    LFactorLines  := TStringList.Create;
    try
      //Read the Growth Factor file
      LFileData.LoadFromFile(AFilename.FileName);
      //Validate no data
      if (LFileData.Count = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strNoDataReturned');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      //Line 1....Number of years.
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LLinesRead := 0;
      LReadString := LFileData[LLinesRead];
      LLinesRead := LLinesRead + 1;
      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0)  then
      begin
        LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strNumberOfYearsErr');
        LMessage := Format(LMessage,[LLinesRead+1]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        LGrowthFactorFileObject.NumberOfYears.FData := LReadInteger;
        LGrowthFactorFileObject.NumberOfYears.FInitalised := True;
      end;

      LFileIncomplete := False;
      for LIndex := 1 to ADataObject.FRunParametersObject.FNumberOfDemandCentres.FData do
      begin
        {//Skip blank lines
        while (Trim(LFileData[LLinesRead]) = '') and (LLinesRead < LFileData.Count) do
        begin
          LLinesRead := LLinesRead + 1;
        end;}

        if (LLinesRead >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;

        LDemandCentresFileObject := LGrowthFactorFileObject.AddDemandCentresGrowthFactorFileObject;

        // Line 2.... Demand Channel Number
        LReadString := LFileData[LLinesRead];
        LLinesRead := LLinesRead + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');

        LTempString:=ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0)  then
        begin
          LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strDemandChannelNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDemandCentresFileObject.DemandChannelNumber.FData := LReadInteger;
          LDemandCentresFileObject.DemandChannelNumber.FInitalised := True;
        end;

        if(Trim(LReadString) <> '') then
        begin
          LDemandCentresFileObject.Comment.FData := LReadString;
          LDemandCentresFileObject.Comment.FInitalised := True;
        end;

        LFactors.Clear;
        LFactorLines.Clear;
        for LCount := 1 to LGrowthFactorFileObject.NumberOfYears.FData do
        begin

          // Line 3.... Growth factors
          if (LLinesRead >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;

          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
          LTempString := CommaTextString(LReadString);
          if(Trim(LTempString) <> '') then
          begin
            LFactorLines.Add(LTempString);
            if(LFactors.Count > 0) then
              LFactors.CommaText := LFactors.CommaText + ',' + LTempString
            else
             LFactors.CommaText := LTempString;
          end;

          if(StringsItemsCount(LFactors) >= LGrowthFactorFileObject.NumberOfYears.FData) then
            Break;
        end;

        for LCount := 0 to LFactors.Count-1 do
        begin
          if(pos('*',LFactors[LCount]) > 0) then Continue;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
          Val(LFactors[LCount],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strDemandCentresGrowthFactorsErr');
            LMessage := Format(LMessage,[LLinesRead]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        //LDemandCentresFileObject.GrowthFactorsForEachYear.FData       := LFactors.CommaText;
        LDemandCentresFileObject.GrowthFactorsForEachYear.FData       := LFactorLines.Text;
        LDemandCentresFileObject.GrowthFactorsForEachYear.FInitalised := True;
        LDemandCentresFileObject.GrowthFactorsForEachYear.FLength     := Length(LDemandCentresFileObject.GrowthFactorsForEachYear.FData);
      end;
      
      //Skip blank lines
      {while (Trim(LFileData[LLinesRead]) = '') and (LLinesRead < LFileData.Count) do
      begin
        LLinesRead := LLinesRead + 1;
      end;}

      //Line 4... Number of Supply min-max Channels with grouth...
      LReadString := LFileData[LLinesRead];
      LLinesRead := LLinesRead + 1;
      LTempString := ExtractFirstSubstring(LReadString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0)  then
      begin
        LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strInvalidNumberOfSupply');
        LMessage := Format(LMessage,[LLinesRead+1]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        LGrowthFactorFileObject.NumberOfSupply.FData := LReadInteger;
        LGrowthFactorFileObject.NumberOfSupply.FInitalised := True;
      end;

      //Line 5...Min-max Channel Number with growth...
      for LIndex := 1 to LGrowthFactorFileObject.NumberOfSupply.FData do
      begin
        //Skip blank lines
        {while (Trim(LFileData[LLinesRead]) = '') and (LLinesRead < LFileData.Count) do
        begin
          LLinesRead := LLinesRead + 1;
        end;}

        if (LLinesRead >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;

        LMinMaxChannelFileObject := LGrowthFactorFileObject.AddMinMaxChannelGrowthFactorFileObject;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'5');
        LReadString := LFileData[LLinesRead];
        LLinesRead := LLinesRead + 1;

        LTempString:=ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0)  then
        begin
          LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strMinMaxChannelNumWithGrowthrErr');
          LMessage := Format(LMessage,[LLinesRead+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMinMaxChannelFileObject.ChannelNumber.FData := LReadInteger;
          LMinMaxChannelFileObject.ChannelNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'5');
        LTempString:=ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0)  then
        begin
          LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strBoundNumOfChannelErr');
          LMessage := Format(LMessage,[LLinesRead+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMinMaxChannelFileObject.ArcNumber.FData := LReadInteger;
          LMinMaxChannelFileObject.ArcNumber.FInitalised := True;
        end;

        if(Trim(LReadString) <> '') then
        begin
          LMinMaxChannelFileObject.Comment.FData := LReadString;
          LMinMaxChannelFileObject.Comment.FInitalised := True;
        end;

        LFactors.Clear;
        LFactorLines.Clear;
        //Line 6... Growth Factor for each bound with growth factor for each year of the analysis...
        for LCount := 1 to LGrowthFactorFileObject.NumberOfYears.FData do
        begin

          if (LLinesRead >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;

          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'6');
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;

          LTempString := CommaTextString(LReadString);
          if(Trim(LTempString) <> '') then
          begin
            LFactorLines.Add(LTempString);
            if(LFactors.Count > 0) then
              LFactors.CommaText := LFactors.CommaText + ',' + LTempString
            else
             LFactors.CommaText := LTempString;
          end;

          if(StringsItemsCount(LFactors) >= LGrowthFactorFileObject.NumberOfYears.FData) then
            Break;
        end;

        for LCount := 0 to LFactors.Count-1 do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'6');
          if(pos('*',LFactors[LCount]) > 0) then Continue;
          Val(LFactors[LCount],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strMinMaxChannelGrowthFactorsErr');
            LMessage := Format(LMessage,[LLinesRead]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        //LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData       := LFactors.CommaText;
        LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData       := LFactorLines.Text;
        LMinMaxChannelFileObject.GrowthFactorsForEachYear.FInitalised := True;
        LMinMaxChannelFileObject.GrowthFactorsForEachYear.FLength     := Length(LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData);
      end;

      //Skip blank lines
      {while (Trim(LFileData[LLinesRead]) = '') and (LLinesRead < LFileData.Count) do
      begin
        LLinesRead := LLinesRead + 1;
      end;}

      //Line 7...Hydrology growth factors...
      for LIndex := 1 to ADataObject.FParamObject.GaugeStochasticsContainer.ItemsCount do
      begin
        //Skip blank lines
        {while (Trim(LFileData[LLinesRead]) = '') and (LLinesRead < LFileData.Count) do
        begin
          LLinesRead := LLinesRead + 1;
        end;}

        if (LLinesRead >= LFileData.Count) then
        begin
          LFileIncomplete := True;
          Break;
        end;

        LHydroDataFileObject := LGrowthFactorFileObject.AddHydroDataGrowthFactorFileObject;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');

        LReadString := LFileData[LLinesRead];
        LLinesRead := LLinesRead + 1;

        LTempString:=ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0)  then
        begin
          LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strGaugeNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LHydroDataFileObject.GaugeNumber.FData := LReadInteger;
          LHydroDataFileObject.GaugeNumber.FInitalised := True;
        end;

        if(Trim(LReadString) <> '') then
        begin
          LHydroDataFileObject.Comment.FData := LReadString;
          LHydroDataFileObject.Comment.FInitalised := True;
        end;

        //Line 8... Affrorestation Growth Factors
        LFactors.Clear;
        LFactorLines.Clear;
        for LCount := 1 to LGrowthFactorFileObject.NumberOfYears.FData do
        begin

          if (LLinesRead >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;

          LTempString := CommaTextString(LReadString);
          if(Trim(LTempString) <> '') then
          begin
            LFactorLines.Add(LTempString);
            if(LFactors.Count > 0) then
              LFactors.CommaText := LFactors.CommaText + ',' + LTempString
            else
             LFactors.CommaText := LTempString;
          end;

          if(StringsItemsCount(LFactors) >= LGrowthFactorFileObject.NumberOfYears.FData) then
            Break;
        end;

        for LCount := 0 to LFactors.Count-1 do
        begin
          if(pos('*',LFactors[LCount]) > 0) then Continue;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'8');
          Val(LFactors[LCount],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strAFFGrowthFactorsErr');
            LMessage := Format(LMessage,[LLinesRead]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        //LHydroDataFileObject.GrowthFactorsForAFF.FData       := LFactors.CommaText;
        LHydroDataFileObject.GrowthFactorsForAFF.FData       := LFactorLines.Text;
        LHydroDataFileObject.GrowthFactorsForAFF.FInitalised := True;
        LHydroDataFileObject.GrowthFactorsForAFF.FLength     := Length(LHydroDataFileObject.GrowthFactorsForAFF.FData);

        //----------------------------------------------------------------------------------------------------------
        //Line 9... Irrigation Growth Factors
        LFactors.Clear;
        LFactorLines.Clear;
        for LCount := 1 to LGrowthFactorFileObject.NumberOfYears.FData do
        begin

          if (LLinesRead >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'9');
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;

          LTempString := CommaTextString(LReadString);
          if(Trim(LTempString) <> '') then
          begin
            LFactorLines.Add(LTempString);
            if(LFactors.Count > 0) then
              LFactors.CommaText := LFactors.CommaText + ',' + LTempString
            else
             LFactors.CommaText := LTempString;
          end;

          if(StringsItemsCount(LFactors) >= LGrowthFactorFileObject.NumberOfYears.FData) then
            Break;
        end;

        for LCount := 0 to LFactors.Count-1 do
        begin
          if(pos('*',LFactors[LCount]) > 0) then Continue;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'9');
          Val(LFactors[LCount],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strIRRGrowthFactorsErr');
            LMessage := Format(LMessage,[LLinesRead]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        //LHydroDataFileObject.GrowthFactorsForIRR.FData       := LFactors.CommaText;
        LHydroDataFileObject.GrowthFactorsForIRR.FData       := LFactorLines.Text;
        LHydroDataFileObject.GrowthFactorsForIRR.FInitalised := True;
        LHydroDataFileObject.GrowthFactorsForIRR.FLength     := Length(LHydroDataFileObject.GrowthFactorsForIRR.FData);

                                      
        //----------------------------------------------------------------------------------------------------------
        //Line 10... Urban abstraction Growth Factors
        LFactors.Clear;
        LFactorLines.Clear;
        for LCount := 1 to LGrowthFactorFileObject.NumberOfYears.FData do
        begin

          if (LLinesRead >= LFileData.Count) then
          begin
            LFileIncomplete := True;
            Break;
          end;
          LLocalCount := LFileLineTypesObject.LinesCount;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'10');
          LReadString := LFileData[LLinesRead];
          LLinesRead := LLinesRead + 1;

          LTempString := CommaTextString(LReadString);
          if(Trim(LTempString) <> '') then
          begin
            LFactorLines.Add(LTempString);
            if(LFactors.Count > 0) then
              LFactors.CommaText := LFactors.CommaText + ',' + LTempString
            else
             LFactors.CommaText := LTempString;
          end;

          if(StringsItemsCount(LFactors) >= LGrowthFactorFileObject.NumberOfYears.FData) then
            Break;
        end;

        for LCount := 0 to LFactors.Count-1 do
        begin
          if(pos('*',LFactors[LCount]) > 0) then Continue;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'10');
          Val(LFactors[LCount],LReadReal,LErrorCode);
          if LErrorCode <> 0 then
          begin
            LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strURBGrowthFactorsErr');
            LMessage := Format(LMessage,[LLinesRead]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;
          if(LReadReal > 0.0) then; //To remove compilation error;
        end;

        //LHydroDataFileObject.GrowthFactorsForURB.FData       := LFactors.CommaText;
        LHydroDataFileObject.GrowthFactorsForURB.FData       := LFactorLines.Text;
        LHydroDataFileObject.GrowthFactorsForURB.FInitalised := True;
        LHydroDataFileObject.GrowthFactorsForURB.FLength     := Length(LHydroDataFileObject.GrowthFactorsForURB.FData);
      end;

      for LCount := LLinesRead to LFileData.Count - 1 do
        LGrowthFactorFileObject.FExtraLines.Add(LFileData[LCount]);
    finally
      FreeAndNil(LFileData);
      FreeAndNil(LFactors);
      FreeAndNil(LFactorLines);
    end;

    if LFileIncomplete then
    begin
      LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strFileIncomplete');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
    end
    else
    begin
      LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileGrowthFactorsAgent.WriteModelDataToFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
                                                         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGrowthFactorsAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LIndex,
  LCount         : Integer;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LGrowthFactorFileObject : TGrowthFactorFileObject;
  LDemandCentresFileObject:TGrowthFactorDemandCentresFileObject;
  LMinMaxChannelFileObject:TGrowthFactorMinMaxChannelFileObject;
  LHydroDataFileObject:TGrowthFactorHydroDataFileObject;
  LFactors: TStringList;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGrowthFactorFileObject    := LPlanningFileDataObject.GrowthFactorFileObject;

    if(LGrowthFactorFileObject.DemandCentresGrowthFactorsCount  = 0) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strWritingStarted');
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
      LOutString:='';
      LTempString:=PadInt(LGrowthFactorFileObject.NumberOfYears);
      LOutString:=LOutString+Trim(LTempString);
      LFileData.Add(LOutString);

      for LCount := 0 to LGrowthFactorFileObject.DemandCentresGrowthFactorsCount -1 do
      begin
        //line 2 +++++++++++++++++++++++++
        LDemandCentresFileObject := LGrowthFactorFileObject.DemandCentresGrowthFactorObjectByIndex[LCount];
        LOutString:='';

        LTempString:=PadInt(LDemandCentresFileObject.DemandChannelNumber);
        LOutString:=LOutString+Trim(LTempString);
        if LDemandCentresFileObject.Comment.FInitalised then
          LOutString:=LOutString+LDemandCentresFileObject.Comment.FData;

        LFileData.Add(LOutString);

        //line 3 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LDemandCentresFileObject.GrowthFactorsForEachYear.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add(UnCommaTextString(LFactors[LIndex]));
        end;
        {LFactors.CommaText := LDemandCentresFileObject.GrowthFactorsForEachYear.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LOutString:=LOutString+' '+LFactors[LIndex];
          if(((LIndex+1) mod 8) = 0) or (LIndex = (LFactors.Count -1))then
          begin
            LFileData.Add(LOutString);
            LOutString:='';
          end;
        end;}
      end;

      //line 4 +++++++++++++++++++++++++
      LOutString:='';
      LTempString:=IntToStr(LGrowthFactorFileObject.MinMaxChannelGrowthFactorsCount);
      LOutString:=LOutString+ ' ' + LTempString;
      LFileData.Add(LOutString);

      for LCount := 0 to LGrowthFactorFileObject.MinMaxChannelGrowthFactorsCount -1 do
      begin
        LMinMaxChannelFileObject := LGrowthFactorFileObject.MinMaxChannelGrowthFactorByIndex[LCount];

        //line 5 +++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LMinMaxChannelFileObject.ChannelNumber);
        LOutString:=LOutString+Trim(LTempString);

        LTempString:=PadInt(LMinMaxChannelFileObject.ArcNumber);
        LOutString:=LOutString+'  '+Trim(LTempString);

        if LMinMaxChannelFileObject.Comment.FInitalised then
          LOutString:=LOutString+LMinMaxChannelFileObject.Comment.FData;

        LFileData.Add(LOutString);

        //line 6 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add(UnCommaTextString(LFactors[LIndex]));
        end;
        {LOutString:='';
        LFactors.CommaText := LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LOutString:=LOutString+' '+LFactors[LIndex];
          if(((LIndex+1) mod 8) = 0) or (LIndex = (LFactors.Count -1))then
          begin
            LFileData.Add(LOutString);
            LOutString:='';
          end;
        end;}
      end;

      for LCount := 0 to LGrowthFactorFileObject.HydrologyGrowthFactorsCount -1 do
      begin
        LHydroDataFileObject := LGrowthFactorFileObject.HydroDataGrowthFactorByIndex[LCount];

        //line 7 +++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadInt(LHydroDataFileObject.GaugeNumber);
        LOutString:=LOutString+Trim(LTempString);
        LFileData.Add(LOutString);

        if LHydroDataFileObject.Comment.FInitalised then
          LOutString:=LOutString+LHydroDataFileObject.Comment.FData;

        //line 8 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LHydroDataFileObject.GrowthFactorsForAFF.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add(UnCommaTextString(LFactors[LIndex]));
        end;
        {LOutString:='';
        LFactors.CommaText := LHydroDataFileObject.GrowthFactorsForAFF.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LOutString:=LOutString+' '+LFactors[LIndex];
          if(((LIndex+1) mod 8) = 0) or (LIndex = (LFactors.Count -1))then
          begin
            LFileData.Add(LOutString);
            LOutString:='';
          end;
        end;}

        //line 9 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LHydroDataFileObject.GrowthFactorsForIRR.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add(UnCommaTextString(LFactors[LIndex]));
        end;
        {LOutString:='';
        LFactors.CommaText := LHydroDataFileObject.GrowthFactorsForIRR.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LOutString:=LOutString+' '+LFactors[LIndex];
          if(((LIndex+1) mod 8) = 0) or (LIndex = (LFactors.Count -1))then
          begin
            LFileData.Add(LOutString);
            LOutString:='';
          end;
        end;}

        //line 10 +++++++++++++++++++++++++
        LOutString:='';
        LFactors.Text := LHydroDataFileObject.GrowthFactorsForURB.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LFileData.Add(UnCommaTextString(LFactors[LIndex]));
        end;
        {LOutString:='';
        LFactors.CommaText := LHydroDataFileObject.GrowthFactorsForURB.FData;
        for LIndex := 0 to LFactors.Count -1 do
        begin
          LOutString:=LOutString+' '+LFactors[LIndex];
          if(((LIndex+1) mod 8) = 0) or (LIndex = (LFactors.Count -1))then
          begin
            LFileData.Add(LOutString);
            LOutString:='';
          end;
        end;}
      end;

      for LCount := 0 to LGrowthFactorFileObject.FExtraLines.Count -1 do
      begin
        LFileData.Add(LGrowthFactorFileObject.FExtraLines[LCount]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileGrowthFactorsAgent.strWritingCompleted');
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
