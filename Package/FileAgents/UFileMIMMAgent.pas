//
//
//  UNIT      : Contains TFileMIMMAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 12/03/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFileMIMMAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UMIMMFileObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UYieldModelDataObject;
type

  TFileMIMMAgent = class(TAbstractFileAgent)
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
  UErrorHandlingOperations;

function TFileMIMMAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileMIMMAgent.ReadModelDataFromFile';
var
  LFileData                  : TStringList;
  LMessage                   : string;
  LReadString                : string;
  LTempString                : string;
  LReadReal                  : Double;
  LStop                      : boolean;
  //LAssSaltsWashoffNo         : integer;
  LMineSubModelNo            : integer;
  LOpenCastCount             : integer;
  LUnderGroundCount          : integer;
  LSlurryDumpCount           : integer;
  LMineSubCatchmentCount     : integer;
  //LMinesIndex                : integer;
  LOpenCastIndex             : integer;
  LUnderGroundIndex          : integer;
  LSlurryDumpIndex           : integer;
  //LMineSubCatchmentIndex     : integer;
  LPos                       : integer;
  LLinesRead                 : integer;
  LErrorCode                 : integer;
  LReadInteger               : integer;
  LIndex                     : integer;
  LPlanningFileDataObject    : TPlanningFileDataObjects;
  LOpenCast                  : TMIMMOpenCastFileObject;
  LUndeground                : TMIMMUndegroundFileObject;
  LSlurryDump                : TMIMMSlurryDumpFileObject;
  LMine                      : TMIMMFileObject;
  LMineList                  : TMIMMListFileObject;
  //LMineSubCatchment          : TMIMMSubCatchmentFileObject;
  //LMineSubCatchmentList      : TMIMMSubCatchmentListFileObject;
  LGrowthFactors             : TGrowthFactors;
  LLoadGeneration            : TLoadGeneration;

  LFileLineTypesObject       : TAbstractFileLineTypesObject;

  LCommaTextStr              : TStringList;
  LCount,
  LOGTHCount                 : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

   // LMineList := ADataObject.FMineListFileObject;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LMineList    := LPlanningFileDataObject.AddMIMMFileObject(AFilename.FileNumber);
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;
    if not LMineList.Initialise then
    Exit;
    LFileData := TStringList.Create;
    LCommaTextStr := TStringList.Create;
    try
      //Read the MIMM.dat file
      LFileData.LoadFromFile(AFilename.FileName);
      LMineList.MIMMFileName.FData := AFileName.FileName;
      LMineList.MIMMFileName.FInitalised := True;

      //Read Line 1
      LLinesRead := 0;
      if (LFileData.Count > 0) then
      begin
        LMineSubModelNo        := 0;
        LOpenCastCount         := 0;
        LUnderGroundCount      := 0;
        LSlurryDumpCount       := 0;
        LMineSubCatchmentCount := 0;

        LMine := LMineList.AddMineFileObject;

        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strNodeNumberErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.NodeNumber.FData       := LReadInteger;
          LMine.NodeNumber.FInitalised := True;
        end;

        LTempString := Trim(LReadString);
        LPos := Pos('  ',LTempString);
        if(LPos > 1) then
          LTempString := Copy(LTempString,1,LPos-1);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strMineNameErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.MineName.FData       := LTempString;
          LMine.MineName.FLength     := Length(LTempString);
          LMine.MineName.FInitalised := True;
        end;

          //Read Line 2
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;
        //Associated Salt Washoff number
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strOpenCastCountErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.FAssSaltsWashoffNo.FData := LReadInteger;
          LMine.FAssSaltsWashoffNo.FInitalised := True;
        end;
       // LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strOpenCastCountErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LOpenCastCount := LReadInteger;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strUndegroundCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,5,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LUnderGroundCount := LReadInteger;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSlurryDumpCountErr');
           LMessage := Format(LMessage,[LLinesRead+1,9,12]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSlurryDumpCount := LReadInteger;
        end;

        //Read Line 3
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strRiverChannelNumberErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.RiverChannelNumber.FData       := LReadInteger;
          LMine.RiverChannelNumber.FInitalised := True;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDChannelNumberErr');
           LMessage := Format(LMessage,[LLinesRead+1,5,8]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.PCDChannelNumber.FData       := LReadInteger;
          LMine.PCDChannelNumber.FInitalised := True;
        end;

          //Read Line 4
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;

        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
        for LIndex := 1 to 12 do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPanEvaporationErr');
             LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMine.PanEvaporation[LIndex].FData       := LReadReal;
            LMine.PanEvaporation[LIndex].FInitalised := True;
          end;
        end;

        //Read Line 5
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;

        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
        for LIndex := 1 to 12 do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strLakeEvaporationErr');
             LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMine.LakeEvaporation[LIndex].FData       := LReadReal;
            LMine.LakeEvaporation[LIndex].FInitalised := True;
          end;
        end;

          //Read Line 6
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;

        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
        LTempString := ExtractFirstSubstring(LReadString);

        LTempString := Trim(LTempString);
        LPos := Pos(' ',LTempString);
        if(LPos > 1) then
          LTempString := Copy(LTempString,1,LPos-1);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strRainfallFileNameErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.FRainfallFileName.FData       := LTempString;
          LMine.FRainfallFileName.FInitalised := True;
        end;

        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strRainfallMAPErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,2]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.FRainfallMAP.FData       := LReadReal;
          LMine.FRainfallMAP.FInitalised := True;
        end;

        // Beneficiation Plant Area
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;

        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strBeneficiationPlantAreaErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.BeneficiationPlantArea.FData       := LReadReal;
          LMine.BeneficiationPlantArea.FInitalised := True;
        end;
        // Beneficiation RunOff Factor
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strBeneficiationRunOffFactorErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.BeneficiationRunOffFactor.FData       := LReadReal;
          LMine.BeneficiationRunOffFactor.FInitalised := True;
        end;

        //Read Line 7  Salt Build Up Rate
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSaltsBuildUpRatesErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.FSaltsBuildUpRate.FData       := LReadReal;
          LMine.FSaltsBuildUpRate.FInitalised := True;
        end;

        //Salts Washoff Efficiency Factor

       // LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSaltsWashoffEfficiencyFactorErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.FSaltsWashoffEfficiencyFactor.FData       := LReadReal;
          LMine.FSaltsWashoffEfficiencyFactor.FInitalised := True;
        end;

        //Initial Salts Store
        //LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strIniSaltStoreErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMine.FIniSaltStore.FData       := LReadReal;
          LMine.FIniSaltStore.FInitalised := True;
        end;

        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;

        // Line 8 NPG1  Number of point to define growth in benificiation plant area (grow variable MMAPNT)
        LGrowthFactors := nil;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHNoOfPointsErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LGrowthFactors := LMine.AddMIMMGrowthFactors;
          LGrowthFactors.FGrowthCode.FData := 1;
          LGrowthFactors.FGrowthCode.FInitalised := True;
          LGrowthFactors.FNoOfPoints.FData := LReadInteger;
          LGrowthFactors.FNoOfPoints.FInitalised := True;
        end;

        //  // Line 8  Interpolation option

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHInterpolationMethodErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,4]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          if LGrowthFactors = nil then
          begin
            LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHNotCreatedErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if LGrowthFactors <> nil then
          begin
            LGrowthFactors.FInterpolationMethod.FData := LReadInteger;
            LGrowthFactors.FInterpolationMethod.FInitalised := True;
          end;
        end;

         // Line 9 Year data points (row should have NPG1 years)
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9');

        LCommaTextStr.Clear;
        for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
            LCommaTextStr.Add(IntToStr(LReadInteger));
        end;

        if (LGrowthFactors <> nil) and (LCommaTextStr.Count>0) then
        begin
          LGrowthFactors.FYearDatapoints.FData := LCommaTextStr.CommaText;
          LGrowthFactors.FYearDatapoints.FInitalised := True;
        end;
        // Line 10 Benificiation plant area growth factor associated with year data in previous line – factor of 1.0 signifies no growth
        LLinesRead := LLinesRead + 1;
        if(LLinesRead >= LFileData.Count) then Exit;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'10');

        LCommaTextStr.Clear;
        for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
        begin
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
            LCommaTextStr.Add(FloatToStr(LReadReal));
        end;

        if (LGrowthFactors <> nil) and (LCommaTextStr.Count>0) then
        begin
          LGrowthFactors.FGrowthFactors.FData := LCommaTextStr.CommaText;
          LGrowthFactors.FGrowthFactors.FInitalised := True;
        end;



          //___________________________________________OpenCast_____________________________________________________
        for LOpenCastIndex := 1 to LOpenCastCount do
        begin
          //Read Line 11
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;
          LOpenCast := LMine.AddOpenCast;
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'11');

          LTempString := ExtractFirstSubstring(LReadString);
          LPos := Pos('  ',LTempString);
          if(LPos > 1) then
            LTempString := Copy(LTempString,1,LPos-1);

          if(LTempString = '') then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPitNameErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.PitName.FData       := LTempString;
            LOpenCast.PitName.FLength     := Length(LTempString);
            LOpenCast.PitName.FInitalised := True;
          end;

          //Read Line 11

          LTempString := ExtractFirstSubstring(LReadString);
          //Val(Trim(LTempString),LReadInteger,LErrorCode);
          LPos := Pos('  ',LTempString);
          if(LPos > 1) then
            LTempString := Copy(LTempString,1,LPos-1);

          if(LTempString = '') then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strCoalReserveAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FAbstractionOpt.FData       := LTempString[1];
            LOpenCast.FAbstractionOpt.FInitalised := True;
          end;

          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          // Line 12
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'12');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strCoalReserveAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.CoalReserveArea.FData       := LReadReal;
            LOpenCast.CoalReserveArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strWorkingsAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.WorkingsArea.FData       := LReadReal;
            LOpenCast.WorkingsArea.FInitalised := True;
          end;

          //Read Line 13

          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'13');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedWorkingsAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FWorkingCommYear.FData       := LReadInteger;
            LOpenCast.FWorkingCommYear.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedWorkingsAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FWorkingCommMonth.FData       := LReadInteger;
            LOpenCast.FWorkingCommMonth.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedWorkingsAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FWorkingDecommYear.FData       := LReadInteger;
            LOpenCast.FWorkingDecommYear.FInitalised := True;
          end;

           LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedWorkingsAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FWorkingDecommMonth.FData       := LReadInteger;
            LOpenCast.FWorkingDecommMonth.FInitalised := True;
          end;

          // Read line 14
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'14');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedWorkingsAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.DisturbedWorkingsArea.FData       := LReadReal;
            LOpenCast.DisturbedWorkingsArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.DisturbedArea.FData       := LReadReal;
            LOpenCast.DisturbedArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strWaterSurfaceEvapAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.WaterSurfaceEvapArea.FData       := LReadReal;
            LOpenCast.WaterSurfaceEvapArea.FInitalised := True;
          end;
          LGrowthFactors := nil;
          //Read Line 15 ...26
          for LOGTHCount := 1 to 4 do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Exit;

          // Line 15 NPG2  Number of point to define growth in working area (grow variable MMAW)


            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHNoOfPointsErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LGrowthFactors := LOpenCast.AddGrowthFactors;
              LGrowthFactors.FGrowthCode.FData := LOGTHCount + 1;
              LGrowthFactors.FGrowthCode.FInitalised := True;
              LGrowthFactors.FNoOfPoints.FData := LReadInteger;
              LGrowthFactors.FNoOfPoints.FInitalised := True;


            end;

            if (LGrowthFactors <> nil) then
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHInterpolationMethodErr');
                 LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LGrowthFactors.FInterpolationMethod.FData := LReadInteger;
                LGrowthFactors.FInterpolationMethod.FInitalised := True;
              end;

              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Break;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

              LCommaTextStr.Clear;
              for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadInteger,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                  LCommaTextStr.Add(IntToStr(LReadInteger));
              end;

              if (LCommaTextStr.Count>0) then
              begin
                LGrowthFactors.FYearDatapoints.FData := LCommaTextStr.CommaText;
                LGrowthFactors.FYearDatapoints.FInitalised := True;
              end;
              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Exit;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

              LCommaTextStr.Clear;
              for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                  LCommaTextStr.Add(FloatToStr(LReadReal));
              end;

              if (LCommaTextStr.Count>0) then
              begin
                LGrowthFactors.FGrowthFactors.FData := LCommaTextStr.CommaText;
                LGrowthFactors.FGrowthFactors.FInitalised := True;
              end;
            end
            else
            begin
              LMessage := FAppModules.Language.GetString('TFileMIMMAgent.str2GTHNotCreatedErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end;

          // Line 27
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'27');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedAreaRunOffErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.DisturbedAreaRunOff.FData       := LReadReal;
            LOpenCast.DisturbedAreaRunOff.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedWorkingsAreaRunOffErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.DisturbedWorkingsAreaRunOff.FData       := LReadReal;
            LOpenCast.DisturbedWorkingsAreaRunOff.FInitalised := True;
          end;

          //Read Line 28
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'28');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strWorkingAreaRechargeFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.DisturbedRechargeFactor[LIndex].FData       := LReadReal;
              LOpenCast.DisturbedRechargeFactor[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 29
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'29');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.WorkingAreaRechargeFactor[LIndex].FData       := LReadReal;
              LOpenCast.WorkingAreaRechargeFactor[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 30

          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'30');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDecantVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FRunoffSaltsWashoffEfficiencyFactor.FData       := LReadReal;
            LOpenCast.FRunoffSaltsWashoffEfficiencyFactor.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDecantVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FIniSaltStore.FData       := LReadReal;
            LOpenCast.FIniSaltStore.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDecantVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FReChargeRate.FData       := LReadReal;
            LOpenCast.FReChargeRate.FInitalised := True;
          end;


          // Line 31

          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDecantVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.DecantVolume.FData       := LReadReal;
            LOpenCast.DecantVolume.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSeepageVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.SeepageVolume.FData       := LReadReal;
            LOpenCast.SeepageVolume.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strAnalysisStartVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.AnalysisStartVolume.FData       := LReadReal;
            LOpenCast.AnalysisStartVolume.FInitalised := True;
          end;

          //Read Line 32a
          if (LOpenCast.FAbstractionOpt.FInitalised) and (LOpenCast.FAbstractionOpt.FData = '1') then
          begin

            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'32a');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strAbstractToEvapErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.FAbstractToEvap.FData       := LReadReal;
              LOpenCast.FAbstractToEvap.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strAbstrctToRiverErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.FAbstrctToRiver.FData       := LReadReal;
              LOpenCast.FAbstrctToRiver.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strFAbstrctToCPDErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.FAbstrctToCPD.FData       := LReadReal;
              LOpenCast.FAbstrctToCPD.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            LTempString := Trim(LTempString);
            LPos := Pos(' ',LTempString);
            if(LPos > 1) then
              LTempString := Copy(LTempString,1,LPos-1);
            if(LTempString = '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strFAbstrctMonthTimeSeriesFileErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.FAbstrctMonthTimeSeriesFile.FData       := LTempString;
              LOpenCast.FAbstrctMonthTimeSeriesFile.FInitalised := True;
            end;

          end;// end abstraction

          // Line 32b
          LGrowthFactors := nil;
          //Read Line 3b ...37
          for LOGTHCount := 1 to 2 do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Exit;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHNoOfPointsErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin

              LGrowthFactors := LOpenCast.AddGrowthFactors;

              LGrowthFactors.FGrowthCode.FData := LOGTHCount + 5;
              LGrowthFactors.FGrowthCode.FInitalised := True;

              LGrowthFactors.FNoOfPoints.FData := LReadInteger;
              LGrowthFactors.FNoOfPoints.FInitalised := True;



            end;

            if (LGrowthFactors <> nil) then
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHInterpolationMethodErr');
                 LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LGrowthFactors.FInterpolationMethod.FData := LReadInteger;
                LGrowthFactors.FInterpolationMethod.FInitalised := True;
              end;

              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Break;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

              LCommaTextStr.Clear;
              for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadInteger,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                  LCommaTextStr.Add(IntToStr(LReadInteger));
              end;

              if (LCommaTextStr.Count>0) then
              begin
                LGrowthFactors.FYearDatapoints.FData := LCommaTextStr.CommaText;
                LGrowthFactors.FYearDatapoints.FInitalised := True;
              end;
              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Exit;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

              LCommaTextStr.Clear;
              for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                  LCommaTextStr.Add(FloatToStr(LReadReal));
              end;

              if (LCommaTextStr.Count>0) then
              begin
                LGrowthFactors.FGrowthFactors.FData := LCommaTextStr.CommaText;
                LGrowthFactors.FGrowthFactors.FInitalised := True;
              end;
            end
            else
            begin
              LMessage := FAppModules.Language.GetString('TFileMIMMAgent.str2GTHNotCreatedErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end;

          //Line 38
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'38');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strMaximumSeepageRateErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.MaximumSeepageRate.FData       := LReadReal;
            LOpenCast.MaximumSeepageRate.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSeepageExponentErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.SeepageExponent.FData       := LReadReal;
            LOpenCast.SeepageExponent.FInitalised := True;
          end;

          //Read Line 39

          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'39');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDSurfaceAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.PCDSurfaceArea.FData       := LReadReal;
            LOpenCast.PCDSurfaceArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDStorageCapacityErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.PCDStorageCapacity.FData       := LReadReal;
            LOpenCast.PCDStorageCapacity.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDAnalysisStartVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.PCDAnalysisStartVolume.FData       := LReadReal;
            LOpenCast.PCDAnalysisStartVolume.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strIniConcentrationErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LOpenCast.FIniConcentration.FData       := LReadReal;
            LOpenCast.FIniConcentration.FInitalised := True;
          end;
          //Load Generation variables of working area

          for LCount := 1 to 2 do
          begin
            LLoadGeneration := LOpenCast.AddLoadGeneration;
            if (LLoadGeneration <> nil) then
            begin
              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Break;

              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strStdDeviationErr');
                 LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LLoadGeneration.FStdDeviation.FData       := LReadReal;
                LLoadGeneration.FStdDeviation.FInitalised := True;

                LLoadGeneration.FLoadGenType.FData := LCount;
                LLoadGeneration.FLoadGenType.FInitalised := True;

              end;
              // Flows ...
              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Break;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
              for LIndex := 1 to 10 do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                   LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
                   LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                   AProgressFunction(LMessage,ptError,LStop);
                   if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LLoadGeneration.FFlow[LIndex].FData       := LReadReal;
                  LLoadGeneration.FFlow[LIndex].FInitalised := True;
                end;
              end;

              // Mean of Salts Load from...
              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Break;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
              for LIndex := 1 to 10 do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                   LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
                   LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                   AProgressFunction(LMessage,ptError,LStop);
                   if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                begin
                  LLoadGeneration.FMeanOfSalt[LIndex].FData       := LReadReal;
                  LLoadGeneration.FMeanOfSalt[LIndex].FInitalised := True;
                end;
              end;


            end;

          end;

        end;  //OpenCast

          //___________________________________________Underground_____________________________________________________
        for LUnderGroundIndex := 1 to LUnderGroundCount do
        begin
          //Read Line 46
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LUndeground := LMine.AddUndeground;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'46');
          LTempString := ExtractFirstSubstring(LReadString);
          if(LTempString = '') then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strUnderGroundSectionNameErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LUndeground.UnderGroundSectionName.FData       := LTempString;
            LUndeground.UnderGroundSectionName.FLength     := Length(LTempString);
            LUndeground.UnderGroundSectionName.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strChannelNumberToUGDamErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LUndeground.ChannelNumberToUGDam.FData       := LReadInteger;
            LUndeground.ChannelNumberToUGDam.FInitalised := True;
          end;

          //Read Line 47
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'47');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strUpstreamCatchmentAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LUndeground.UpstreamCatchmentArea.FData       := LReadReal;
            LUndeground.UpstreamCatchmentArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strBoardPillarCatchmentAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LUndeground.BoardPillarCatchmentArea.FData       := LReadReal;
            LUndeground.BoardPillarCatchmentArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strHighExtractionCatchmentAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LUndeground.HighExtractionCatchmentArea.FData       := LReadReal;
            LUndeground.HighExtractionCatchmentArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strHighExtractionAreaRunoffFactorErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LUndeground.HighExtractionAreaRunoffFactor.FData       := LReadReal;
            LUndeground.HighExtractionAreaRunoffFactor.FInitalised := True;
          end;

          //Read Line 48
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'48');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strUpstreamRunoffPortionErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LUndeground.UpstreamRunoffPortion[LIndex].FData       := LReadReal;
              LUndeground.UpstreamRunoffPortion[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 49
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'49');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strBoardAndPilarRechargeFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LUndeground.BoardAndPilarRechargeFactor[LIndex].FData       := LReadReal;
              LUndeground.BoardAndPilarRechargeFactor[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 50
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'50');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strHighExtractionRechargeFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LUndeground.HighExtractionRechargeFactor[LIndex].FData       := LReadReal;
              LUndeground.HighExtractionRechargeFactor[LIndex].FInitalised := True;
            end;
          end;
          // Line 51

          for LOGTHCount := 1 to 2 do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Exit;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHNoOfPointsErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LGrowthFactors := LUndeground.AddGrowthFactors;
              LGrowthFactors.FGrowthCode.FData := LOGTHCount + 7;
              LGrowthFactors.FGrowthCode.FInitalised := True;
              LGrowthFactors.FNoOfPoints.FData := LReadInteger;
              LGrowthFactors.FNoOfPoints.FInitalised := True;


            end;

            if (LGrowthFactors <> nil) then
            begin
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHInterpolationMethodErr');
                 LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LGrowthFactors.FInterpolationMethod.FData := LReadInteger;
                LGrowthFactors.FInterpolationMethod.FInitalised := True;
              end;

              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Break;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

              LCommaTextStr.Clear;
              for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadInteger,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                  LCommaTextStr.Add(IntToStr(LReadInteger));
              end;

              if (LCommaTextStr.Count>0) then
              begin
                LGrowthFactors.FYearDatapoints.FData := LCommaTextStr.CommaText;
                LGrowthFactors.FYearDatapoints.FInitalised := True;
              end;
              LLinesRead := LLinesRead + 1;
              if(LLinesRead >= LFileData.Count) then Exit;
              LReadString := LFileData[LLinesRead];
              TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

              LCommaTextStr.Clear;
              for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
              begin
                LTempString := ExtractFirstSubstring(LReadString);
                Val(Trim(LTempString),LReadReal,LErrorCode);
                if(LErrorCode <> 0) then
                begin
                  LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                  LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                  AProgressFunction(LMessage,ptError,LStop);
                  if FAppModules.GlobalData.StopOnFirstErr then Exit ;
                end
                else
                  LCommaTextStr.Add(FloatToStr(LReadReal));
              end;

              if (LCommaTextStr.Count>0) then
              begin
                LGrowthFactors.FGrowthFactors.FData := LCommaTextStr.CommaText;
                LGrowthFactors.FGrowthFactors.FInitalised := True;
              end;
            end
            else
            begin
              LMessage := FAppModules.Language.GetString('TFileMIMMAgent.str8GTHNotCreatedErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end;
          end;

          // Line 57
          LLoadGeneration := LUndeground.AddLoadGeneration;
          if (LLoadGeneration <> nil) then
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strStdDeviationErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LLoadGeneration.FStdDeviation.FData       := LReadReal;
              LLoadGeneration.FStdDeviation.FInitalised := True;

              LLoadGeneration.FLoadGenType.FData := 3;
              LLoadGeneration.FLoadGenType.FInitalised := True;

            end;
            // Flows ...
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
            for LIndex := 1 to 10 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
                 LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LLoadGeneration.FFlow[LIndex].FData       := LReadReal;
                LLoadGeneration.FFlow[LIndex].FInitalised := True;
              end;
            end;

            // Mean of Salts Load from...
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
            for LIndex := 1 to 10 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
                 LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LLoadGeneration.FMeanOfSalt[LIndex].FData       := LReadReal;
                LLoadGeneration.FMeanOfSalt[LIndex].FInitalised := True;
              end;
            end;
          end;


        end;  //Underground

          //___________________________________________SlurryDump_____________________________________________________
        for LSlurryDumpIndex := 1 to LSlurryDumpCount do
        begin
          //Read Line 60
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LSlurryDump := LMine.AddSlurryDump;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'60');
          LTempString := ExtractFirstSubstring(LReadString);
          if(LTempString = '') then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDumpNameErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.DumpName.FData       := LTempString;
            LSlurryDump.DumpName.FLength     := Length(LTempString);
            LSlurryDump.DumpName.FInitalised := True;
          end;


          //Read Line 61
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'61');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDumpSurfaceAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.DumpSurfaceArea.FData       := LReadReal;
            LSlurryDump.DumpSurfaceArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strRunoffFactorToPCDErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.RunoffFactorToPCD.FData       := LReadReal;
            LSlurryDump.RunoffFactorToPCD.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSeepageSplitFactorErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.SeepageSplitFactor.FData       := LReadReal;
            LSlurryDump.SeepageSplitFactor.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDStorageCapacityErr');
             LMessage := Format(LMessage,[LLinesRead+1,13,16]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.PCDStorageCapacity.FData       := LReadReal;
            LSlurryDump.PCDStorageCapacity.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDSurfaceAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,17,20]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.PCDSurfaceArea.FData       := LReadReal;
            LSlurryDump.PCDSurfaceArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strPCDAnalysisStartVolumeErr');
             LMessage := Format(LMessage,[LLinesRead+1,21,24]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.PCDAnalysisStartVolume.FData       := LReadReal;
            LSlurryDump.PCDAnalysisStartVolume.FInitalised := True;
          end;
          // Salt concentration

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strSaltConcentrationErr');
             LMessage := Format(LMessage,[LLinesRead+1,21,24]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDump.FSaltConcentration.FData       := LReadReal;
            LSlurryDump.FSaltConcentration.FInitalised := True;
          end;
          //Read Line 62
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'25');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strRechargeFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LSlurryDump.RechargeFactor[LIndex].FData       := LReadReal;
              LSlurryDump.RechargeFactor[LIndex].FInitalised := True;
            end;
          end;

          // Line 63

          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Exit;
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHNoOfPointsErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGrowthFactors := LSlurryDump.AddGrowthFactors;
            LGrowthFactors.FGrowthCode.FData := 10;
            LGrowthFactors.FGrowthCode.FInitalised := True;
            LGrowthFactors.FNoOfPoints.FData := LReadInteger;
            LGrowthFactors.FNoOfPoints.FInitalised := True;
          end;

          if (LGrowthFactors <> nil) then
          begin
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHInterpolationMethodErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LGrowthFactors.FInterpolationMethod.FData := LReadInteger;
              LGrowthFactors.FInterpolationMethod.FInitalised := True;
            end;

            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

            LCommaTextStr.Clear;
            for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadInteger,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
                LCommaTextStr.Add(IntToStr(LReadInteger));
            end;

            if (LCommaTextStr.Count>0) then
            begin
              LGrowthFactors.FYearDatapoints.FData := LCommaTextStr.CommaText;
              LGrowthFactors.FYearDatapoints.FInitalised := True;
            end;
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Exit;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead +1));

            LCommaTextStr.Clear;
            for LIndex := 1 to LGrowthFactors.FNoOfPoints.FData do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strGTHYearDataPointErr');
                LMessage := Format(LMessage,[LLinesRead+1,1,4]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
                LCommaTextStr.Add(FloatToStr(LReadReal));
            end;

            if (LCommaTextStr.Count>0) then
            begin
              LGrowthFactors.FGrowthFactors.FData := LCommaTextStr.CommaText;
              LGrowthFactors.FGrowthFactors.FInitalised := True;
            end;
          end
          else
          begin
            LMessage := FAppModules.Language.GetString('TFileMIMMAgent.str10GTHNotCreatedErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,4]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;


          // Line 66
          LLoadGeneration := LSlurryDump.AddLoadGeneration;
          if (LLoadGeneration <> nil) then
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strStdDeviationErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LLoadGeneration.FStdDeviation.FData       := LReadReal;
              LLoadGeneration.FStdDeviation.FInitalised := True;

              LLoadGeneration.FLoadGenType.FData := 4;
              LLoadGeneration.FLoadGenType.FInitalised := True;

            end;
            // Flows ...
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
            for LIndex := 1 to 10 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
                 LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LLoadGeneration.FFlow[LIndex].FData       := LReadReal;
                LLoadGeneration.FFlow[LIndex].FInitalised := True;
              end;
            end;

            // Mean of Salts Load from...
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,IntToStr(LLinesRead + 1));
            for LIndex := 1 to 10 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strDisturbedRechargeFactorErr');
                 LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LLoadGeneration.FMeanOfSalt[LIndex].FData       := LReadReal;
                LLoadGeneration.FMeanOfSalt[LIndex].FInitalised := True;
              end;
            end;
          end;


        end;  //SlurryDump
      //end;  //for number of mines

       
        for LIndex := LLinesRead+1 to  LFileData.Count-1 do
          LMineList.Comment.Add(LFileData[LIndex]);
      end;

      LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
      LCommaTextStr.Free
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileMIMMAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileMIMMAgent.WriteModelDataToFile';
var
  LStop                  : boolean;
  LMinesIndex            : integer;
  LOpenCastIndex         : integer;
  LUnderGroundIndex      : integer;
  LSlurryDumpIndex       : integer;
//  LMineSubCatchmentIndex   : integer;
  LMessage                 : string;
  LTempStr                 : string;
  LOutString               : string;
  LIndex                   : Integer;
  LFileData                : TStringlist;
  LOpenCast                : TMIMMOpenCastFileObject;
  LUndeground              : TMIMMUndegroundFileObject;
  LSlurryDump              : TMIMMSlurryDumpFileObject;
  LMine                    : TMIMMFileObject;
  LMineList                : TMIMMListFileObject;
  LGFactorsList             : TStringList;
  LGrowthFactors             : TGrowthFactors;
  LLoadGeneration            : TLoadGeneration;
  LGIndex,
  LCount                     : integer;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strWritingStarted');
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

    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject := TPlanningFileDataObjects(ADataObject);
    LMineList := LPlanningFileDataObject.MIMMFileObjectByFileNumber[AFilename.FileNumber];

    //LMineList := ADataObject.FMineListFileObject;
    if not Assigned(LMineList) then
      Exit;

    LFileData:= TStringList.Create;
    LGFactorsList := TStringList.Create;
    try
      if(LMineList.MineFileObjectCount > 0) then
      begin
        //Write Line 1
       // LOutString := IntToStr(LMineList.MineFileObjectCount);
     //   LFileData.Add(LOutString);

        //___________________________________________Mine_____________________________________________________
        for LMinesIndex := 0 to LMineList.MineFileObjectCount-1 do
        begin
          //Write Line 1

          LMine := TMIMMFileObject(LMineList.MineFileObjectByIndex[LMinesIndex]);

          LOutString  := IntToStr(LMine.NodeNumber.FData);
          LTempStr    := LMine.MineName.FData;
          LOutString  := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);

          //Write Line 2
          LOutString  := IntToStr(LMine.FAssSaltsWashoffNo.FData);
          LOutString  := LOutString+' '+IntToStr(LMine.OpenCastCount);
          LTempStr    := IntToStr(LMine.UndegroundCount);
          LOutString  := LOutString + ' '+ LTempStr;
          LTempStr    := IntToStr(LMine.SlurryDumpCount);
          LOutString  := LOutString + ' '+ LTempStr;

          LFileData.Add(LOutString);

          //Write Line 3
          LOutString  := IntToStr(LMine.RiverChannelNumber.FData);
          LTempStr    := IntToStr(LMine.PCDChannelNumber.FData);
          LOutString  := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);

          //Write Line 4
          LOutString  := '';
          for Lindex := 1 to 12 do
          begin
            LTempStr := Trim(PadDouble(LMine.PanEvaporation[Lindex]));
            LTempStr := RightTrimChars(LTempStr,'0');
            LOutString  := LOutString + ' '+ LTempStr;
          end;
          LFileData.Add(LOutString);

          //Write Line 5
          LOutString  := '';
          for Lindex := 1 to 12 do
          begin
            LTempStr := Trim(PadDouble(LMine.LakeEvaporation[Lindex]));
            LOutString  := LOutString + ' '+ LTempStr;
          end;
          LOutString := Trim(LOutString);
          LFileData.Add(LOutString);

          // write Line 6..
          LOutString  := LMine.FRainfallFileName.FData;
          LOutString  := LOutString+' '+Trim(PadDouble(LMine.FRainfallMAP));
          LFileData.Add(LOutString);


          //Write Line 7
         // LOutString  := IntToStr(LMine.HydrologyNodeNumber.FData);
         // LFileData.Add(LOutString);

          //Write Line 7
          LOutString := Trim(PadDouble(LMine.BeneficiationPlantArea));
          LTempStr   := Trim(PadDouble(LMine.BeneficiationRunOffFactor));
          LOutString := LOutString + ' '+ LTempStr;

          LOutString := LOutString + ' '+ Trim(PadDouble(LMine.FSaltsBuildUpRate));
          LTempStr   := Trim(PadDouble(LMine.FSaltsWashoffEfficiencyFactor));

          LOutString := LOutString + ' '+ LTempStr;

          LOutString := LOutString + ' '+ Trim(PadDouble(LMine.FIniSaltStore));

          LFileData.Add(LOutString);



          LGrowthFactors := TGrowthFactors(LMine.FMIMMGrowthFactorList[0]);
          if LGrowthFactors <> nil then
          begin
            // Line 8
            LOutString   := Trim(IntToStr(LGrowthFactors.FNoOfPoints.FData));
            LTempStr   := Trim(IntToStr(LGrowthFactors.FInterpolationMethod.FData));
            LOutString := LOutString+' '+ LTempStr;
            LFileData.Add(LOutString);
             // Line 9
            LGFactorsList.Clear;
            LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
            for LGIndex := 0 to LGFactorsList.Count-1 do
            begin
              if LGIndex = 0 then
                LOutString := Trim(LGFactorsList[LGIndex])
              else
                LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
            end;
            LFileData.Add(LOutString);
             // Line 10
            LGFactorsList.Clear;
            LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
            for LGIndex := 0 to LGFactorsList.Count-1 do
            begin
              if LGIndex = 0 then
                LOutString := Trim(LGFactorsList[LGIndex])
              else
                LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
            end;
            LFileData.Add(LOutString);
          end;

          //___________________________________________OpenCast_____________________________________________________
          for LOpenCastIndex := 0 to LMine.OpenCastCount-1 do
          begin
            LOpenCast := TMIMMOpenCastFileObject(LMine.OpenCastByIndex[LOpenCastIndex]);

            //Write Line 11
            LOutString  := LOpenCast.PitName.FData;
            LTempStr   := LOpenCast.FAbstractionOpt.FData;
            LOutString  := LOutString+' '+LTempStr;
            LFileData.Add(LOutString);

            //Write Line 12
            LOutString := Trim(PadDouble(LOpenCast.CoalReserveArea));
            LTempStr   := Trim(PadDouble(LOpenCast.WorkingsArea));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);
            // Write Line 13
            LOutString := IntToStr(LOpenCast.FWorkingCommYear.FData);
            LTempStr   := IntToStr(LOpenCast.FWorkingCommMonth.FData);
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := IntToStr(LOpenCast.FWorkingDecommYear.FData);
            LTempStr   := LTempStr+' '+ IntToStr(LOpenCast.FWorkingDecommMonth.FData);
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 14

            LOutString := Trim(PadDouble(LOpenCast.DisturbedWorkingsArea));
            LTempStr   := Trim(PadDouble(LOpenCast.DisturbedArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.WaterSurfaceEvapArea));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 15 .. 26
            for LIndex := 0 to LOpenCast.FGrowthFactors.Count-1 do
            begin
              LGrowthFactors := TGrowthFactors(LOpenCast.FGrowthFactors[LIndex]);
              if LGrowthFactors <> nil then
              begin

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 2) then
                begin
                  // Write Line 15
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 16
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 17
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 3) then
                begin
                  // Write Line 18
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 19
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 20
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 4) then
                begin
                  // Write Line 21
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 22
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 23
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 5) then
                begin
                  // Write Line 24
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 25
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 26
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;

              end;

            end;
            // write line 27
            LOutString := Trim(PadDouble(LOpenCast.DisturbedAreaRunOff));
            LTempStr   := Trim(PadDouble(LOpenCast.DisturbedWorkingsAreaRunOff));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 28
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LOpenCast.DisturbedRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 29
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LOpenCast.WorkingAreaRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);
            //Write Line 30

            LOutString := Trim(PadDouble(LOpenCast.FRunoffSaltsWashoffEfficiencyFactor));
            LTempStr   := Trim(PadDouble(LOpenCast.FIniSaltStore));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.FReChargeRate));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 31
            LOutString := Trim(PadDouble(LOpenCast.DecantVolume));
            LTempStr   := Trim(PadDouble(LOpenCast.SeepageVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.AnalysisStartVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            // Write Line 32a
            if (LOpenCast.FAbstractionOpt.FInitalised) and (LOpenCast.FAbstractionOpt.FData = '1') then
            begin
              LOutString := Trim(PadDouble(LOpenCast.FAbstractToEvap));
              LTempStr   := Trim(PadDouble(LOpenCast.FAbstrctToRiver));
              LOutString := LOutString + ' '+ LTempStr;
              LTempStr   := Trim(PadDouble(LOpenCast.FAbstrctToCPD));
              LTempStr   := LTempStr+' '+LOpenCast.FAbstrctMonthTimeSeriesFile.FData;
              LOutString := LOutString + ' '+ LTempStr;
              LFileData.Add(LOutString);
            end;




            for LIndex := 0 to LOpenCast.FGrowthFactors.Count-1 do
            begin
              LGrowthFactors := TGrowthFactors(LOpenCast.FGrowthFactors[LIndex]);
              if LGrowthFactors <> nil then
              begin

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 6) then
                begin
                  // Write Line 32b
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 33
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 34
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 7) then
                begin
                  // Write Line 35
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 36
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 37
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;


              end;

            end;

            //Write Line 38
            LOutString := Trim(PadDouble(LOpenCast.MaximumSeepageRate));
            LTempStr   := Trim(PadDouble(LOpenCast.SeepageExponent));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 39
            LOutString := Trim(PadDouble(LOpenCast.PCDSurfaceArea));
            LTempStr   := Trim(PadDouble(LOpenCast.PCDStorageCapacity));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.PCDAnalysisStartVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LOutString := LOutString + ' '+Trim(PadDouble(LOpenCast.FIniConcentration));
            LFileData.Add(LOutString);


            for LIndex := 0 to LOpenCast.FLoadGeneration.Count-1 do
            begin
              LLoadGeneration := TLoadGeneration(LOpenCast.FLoadGeneration[LIndex]);
              if LLoadGeneration <> nil then
              begin

                if (LLoadGeneration.FLoadGenType.FInitalised) and (LLoadGeneration.FLoadGenType.FData = 1) then
                begin
                  // Write Line 40
                  LOutString := Trim(PadDouble(LLoadGeneration.FStdDeviation));
                  LFileData.Add(LOutString);
                  // Write Line 41
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FFlow[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);

                 // Write Line 42
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FMeanOfSalt[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);
                end;

                if (LLoadGeneration.FLoadGenType.FInitalised) and (LLoadGeneration.FLoadGenType.FData = 2) then
                begin
                  // Write Line 43
                  LOutString := Trim(PadDouble(LLoadGeneration.FStdDeviation));
                  LFileData.Add(LOutString);
                  // Write Line 44
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FFlow[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);

                 // Write Line 45
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FMeanOfSalt[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);
                end;
              end;
            end;
          end;

          //___________________________________________Underground_____________________________________________________
          for LUnderGroundIndex := 0 to LMine.UndegroundCount-1 do
          begin
            LUndeground := TMIMMUndegroundFileObject(LMine.UndegroundByIndex[LUnderGroundIndex]);

            //Write Line 46
            LOutString  := LUndeground.UnderGroundSectionName.FData;
            LTempStr    := IntToStr(LUndeground.ChannelNumberToUGDam.FData);
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 47
            LOutString := Trim(PadDouble(LUndeground.UpstreamCatchmentArea));
            LTempStr   := Trim(PadDouble(LUndeground.BoardPillarCatchmentArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LUndeground.HighExtractionCatchmentArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LUndeground.HighExtractionAreaRunoffFactor));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 48
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LUndeground.UpstreamRunoffPortion[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 49
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LUndeground.BoardAndPilarRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 50
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LUndeground.HighExtractionRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);



            for LIndex := 0 to LUndeground.FGrowthFactors.Count-1 do
            begin
              LGrowthFactors := TGrowthFactors(LUndeground.FGrowthFactors[LIndex]);
              if LGrowthFactors <> nil then
              begin

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 8) then
                begin
                   // Write Line 51
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                   // Write Line 52
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 53
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;

                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 9) then
                begin
                  // Write Line 54
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                  // Write Line 55
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 56
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;
              end;
            end;


            for LIndex := 0 to LUndeground.FLoadGeneration.Count-1 do
            begin
              LLoadGeneration := TLoadGeneration(LUndeground.FLoadGeneration[LIndex]);
              if LLoadGeneration <> nil then
              begin

                if (LLoadGeneration.FLoadGenType.FInitalised) and (LLoadGeneration.FLoadGenType.FData = 3) then
                begin
                  // Write Line 57
                  LOutString := Trim(PadDouble(LLoadGeneration.FStdDeviation));
                  LFileData.Add(LOutString);
                  // Write Line 58
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FFlow[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);

                 // Write Line 59
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FMeanOfSalt[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);
                end;
              end;
            end;
          end;

          //___________________________________________SlurryDump_____________________________________________________
          for LSlurryDumpIndex := 0 to LMine.SlurryDumpCount-1 do
          begin
            LSlurryDump := TMIMMSlurryDumpFileObject(LMine.SlurryDumpByIndex[LSlurryDumpIndex]);

            //Write Line 60

            LOutString  := LSlurryDump.DumpName.FData;
            LFileData.Add(LOutString);

            //Write Line 61

            LOutString := Trim(PadDouble(LSlurryDump.DumpSurfaceArea));
            LTempStr   := Trim(PadDouble(LSlurryDump.RunoffFactorToPCD));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LSlurryDump.SeepageSplitFactor));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LSlurryDump.PCDStorageCapacity));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LSlurryDump.PCDSurfaceArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LSlurryDump.PCDAnalysisStartVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LSlurryDump.FSaltConcentration));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 62

            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LSlurryDump.RechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            for LIndex := 0 to LSlurryDump.FGrowthFactors.Count-1 do
            begin
              LGrowthFactors := TGrowthFactors(LSlurryDump.FGrowthFactors[LIndex]);
              if LGrowthFactors <> nil then
              begin
                if (LGrowthFactors.FGrowthCode.FInitalised) and (LGrowthFactors.FGrowthCode.FData = 10) then
                begin
                   //Write Line 63
                  LOutString := IntToStr(LGrowthFactors.FNoOfPoints.FData);
                  LOutString := LOutString+' '+IntToStr(LGrowthFactors.FInterpolationMethod.FData);
                  LFileData.Add(LOutString);
                   //Write Line 64
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FYearDatapoints.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                   // Line 65
                  LGFactorsList.Clear;
                  LGFactorsList.CommaText := LGrowthFactors.FGrowthFactors.FData;
                  for LGIndex := 0 to LGFactorsList.Count-1 do
                  begin
                    if LGIndex = 0 then
                      LOutString := Trim(LGFactorsList[LGIndex])
                    else
                      LOutString := LOutString +' '+Trim(LGFactorsList[LGIndex]);
                  end;
                  LFileData.Add(LOutString);
                end;
              end;
            end;

            for LIndex := 0 to LSlurryDump.FLoadGeneration.Count-1 do
            begin
              LLoadGeneration := TLoadGeneration(LSlurryDump.FLoadGeneration[LIndex]);
              if LLoadGeneration <> nil then
              begin
                if (LLoadGeneration.FLoadGenType.FInitalised) and (LLoadGeneration.FLoadGenType.FData = 3) then
                begin
                  // Write Line 66
                  LOutString := Trim(PadDouble(LLoadGeneration.FStdDeviation));
                  LFileData.Add(LOutString);
                  // Write Line 67
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FFlow[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);

                 // Write Line 68
                  LOutString  := '';
                  for LCount := 1 to 10 do
                  begin
                    LTempStr := Trim(PadDouble(LLoadGeneration.FMeanOfSalt[LCount]));
                    LOutString  := LOutString + ' '+ LTempStr;
                  end;
                  LOutString := Trim(LOutString);
                  LFileData.Add(LOutString);
                end;
              end;
            end;
          end;
        end;
      end;

      //Write Line 69 onwards
        for LIndex := 0 to LMineList.Comment.Count-1 do
          LFileData.Add(LMineList.Comment[LIndex]);

        LFileData.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);
      //end;

      if(Trim(LFileData.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFileMIMMAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
      LGFactorsList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

