//
//
//  UNIT      : Contains TFile21Agent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 12/03/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFile21Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UMineFileObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile21Agent = class(TAbstractFileAgent)
  public
    { Public declarations }
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses UUtilities,
     UFilesLineTypeObject,
     UErrorHandlingOperations;

function TFile21Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile21Agent.ReadModelDataFromFile';
var
  LFileData                  : TStringList;
  LMessage                   : string;
  LReadString                : string;
  LTempString                : string;
  LReadReal                  : Double;
  LStop                      : boolean;
  LMinesCount                : integer;
  LOpenCastCount             : integer;
  LUnderGroundCount          : integer;
  LSlurryDumpCount           : integer;
  LMineSubCatchmentCount     : integer;
  LMinesIndex                : integer;
  LOpenCastIndex             : integer;
  LUnderGroundIndex          : integer;
  LSlurryDumpIndex           : integer;
  LMineSubCatchmentIndex     : integer;
  LPos                       : integer;
  LLinesRead                 : integer;
  LErrorCode                 : integer;
  LReadInteger               : integer;
  LIndex                     : integer;
  LOpenCast                  : TMineOpenCastFileObject;
  LUndeground                : TMineUndegroundFileObject;
  LSlurryDump                : TMineSlurryDumpFileObject;
  LMine                      : TMineFileObject;
  LMineList                  : TMineListFileObject;
  LMineSubCatchment          : TMineSubCatchmentFileObject;
  LMineSubCatchmentList      : TMineSubCatchmentListFileObject;
  LFileLineTypesObject       : TAbstractFileLineTypesObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile21Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile21Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LMineList := ADataObject.FMineListFileObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];

    if not LMineList.Initialise then
    Exit;

    LFileData := TStringList.Create;
    try
      //Read the F20.dat file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      //Read Line 1
      LLinesRead := 0;
      if (LFileData.Count > 0) then
      begin
        LMinesCount            := 0;
        LOpenCastCount         := 0;
        LUnderGroundCount      := 0;
        LSlurryDumpCount       := 0;
        LMineSubCatchmentCount := 0;

        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFile06Agent.strMineCountErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,2]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LMinesCount := LReadInteger;
        end;

        for LMinesIndex := 1 to  LMinesCount do
        begin
          //___________________________________________Mine_____________________________________________________
          //Read Line 2
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LMine := LMineList.AddMineFileObject;
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strNodeNumberErr');
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
             LMessage := FAppModules.Language.GetString('TFile21Agent.strMineNameErr');
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

          //Read Line 3
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strOpenCastCountErr');
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
             LMessage := FAppModules.Language.GetString('TFile21Agent.strUndegroundCountErr');
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
             LMessage := FAppModules.Language.GetString('TFile21Agent.strSlurryDumpCountErr');
             LMessage := Format(LMessage,[LLinesRead+1,9,12]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSlurryDumpCount := LReadInteger;
          end;

          //Read Line 4
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strRiverChannelNumberErr');
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
             LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDChannelNumberErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMine.PCDChannelNumber.FData       := LReadInteger;
            LMine.PCDChannelNumber.FInitalised := True;
          end;

          //Read Line 5
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPanEvaporationErr');
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

          //Read Line 6
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strLakeEvaporationErr');
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

          //Read Line 7
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strHydrologyNodeNumberErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMine.HydrologyNodeNumber.FData       := LReadInteger;
            LMine.HydrologyNodeNumber.FInitalised := True;
          end;

          //Read Line 8
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strBeneficiationPlantAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMine.BeneficiationPlantArea.FData       := LReadReal;
            LMine.BeneficiationPlantArea.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strBeneficiationRunOffFactorErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMine.BeneficiationRunOffFactor.FData       := LReadReal;
            LMine.BeneficiationRunOffFactor.FInitalised := True;
          end;

          //___________________________________________OpenCast_____________________________________________________
          for LOpenCastIndex := 1 to LOpenCastCount do
          begin
            //Read Line 9
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LOpenCast := LMine.AddOpenCast;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9');
            LTempString := Trim(LReadString);
            LPos := Pos('  ',LTempString);
            if(LPos > 1) then
              LTempString := Copy(LTempString,1,LPos-1);
            if(LTempString = '') then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPitNameErr');
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

            //Read Line 10
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'10');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strCoalReserveAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strWorkingsAreaErr');
               LMessage := Format(LMessage,[LLinesRead+1,5,8]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.WorkingsArea.FData       := LReadReal;
              LOpenCast.WorkingsArea.FInitalised := True;
            end;

            //Read Line 11
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'11');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDisturbedWorkingsAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDisturbedAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strWaterSurfaceEvapAreaErr');
               LMessage := Format(LMessage,[LLinesRead+1,9,12]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.WaterSurfaceEvapArea.FData       := LReadReal;
              LOpenCast.WaterSurfaceEvapArea.FInitalised := True;
            end;

            //Read Line 12
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'12');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDisturbedAreaRunOffErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDisturbedWorkingsAreaRunOffErr');
               LMessage := Format(LMessage,[LLinesRead+1,5,8]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.DisturbedWorkingsAreaRunOff.FData       := LReadReal;
              LOpenCast.DisturbedWorkingsAreaRunOff.FInitalised := True;
            end;

            //Read Line 13
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'13');
            for LIndex := 1 to 12 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strWorkingAreaRechargeFactorErr');
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

            //Read Line 14
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'14');
            for LIndex := 1 to 12 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strDisturbedRechargeFactorErr');
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

            //Read Line 15
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'15');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDecantVolumeErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strSeepageVolumeErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strAnalysisStartVolumeErr');
               LMessage := Format(LMessage,[LLinesRead+1,9,12]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.AnalysisStartVolume.FData       := LReadReal;
              LOpenCast.AnalysisStartVolume.FInitalised := True;
            end;

            //Read Line 16
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'16');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strMaximumSeepageRateErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strSeepageExponentErr');
               LMessage := Format(LMessage,[LLinesRead+1,5,8]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.SeepageExponent.FData       := LReadReal;
              LOpenCast.SeepageExponent.FInitalised := True;
            end;

            //Read Line 17
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'17');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDSurfaceAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDStorageCapacityErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDAnalysisStartVolumeErr');
               LMessage := Format(LMessage,[LLinesRead+1,9,12]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LOpenCast.PCDAnalysisStartVolume.FData       := LReadReal;
              LOpenCast.PCDAnalysisStartVolume.FInitalised := True;
            end;
          end;  //OpenCast

          //___________________________________________Underground_____________________________________________________
          for LUnderGroundIndex := 1 to LUnderGroundCount do
          begin
            //Read Line 18
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LUndeground := LMine.AddUndeground;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'18');
            LTempString := ExtractFirstSubstring(LReadString);
            if(LTempString = '') then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strUnderGroundSectionNameErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strChannelNumberToUGDamErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,2]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LUndeground.ChannelNumberToUGDam.FData       := LReadInteger;
              LUndeground.ChannelNumberToUGDam.FInitalised := True;
            end;

            //Read Line 19
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'19');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strUpstreamCatchmentAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strBoardPillarCatchmentAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strHighExtractionCatchmentAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strHighExtractionAreaRunoffFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,9,12]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LUndeground.HighExtractionAreaRunoffFactor.FData       := LReadReal;
              LUndeground.HighExtractionAreaRunoffFactor.FInitalised := True;
            end;

            //Read Line 20
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'20');
            for LIndex := 1 to 12 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strUpstreamRunoffPortionErr');
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

            //Read Line 21
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'21');
            for LIndex := 1 to 12 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strBoardAndPilarRechargeFactorErr');
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

            //Read Line 22
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'22');
            for LIndex := 1 to 12 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strHighExtractionRechargeFactorErr');
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
          end;  //Underground

          //___________________________________________SlurryDump_____________________________________________________
          for LSlurryDumpIndex := 1 to LSlurryDumpCount do
          begin
            //Read Line 23
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LSlurryDump := LMine.AddSlurryDump;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'23');
            LTempString := ExtractFirstSubstring(LReadString);
            if(LTempString = '') then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDumpNameErr');
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


            //Read Line 24
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'24');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strDumpSurfaceAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strRunoffFactorToPCDErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strSeepageSplitFactorErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDStorageCapacityErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDSurfaceAreaErr');
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
               LMessage := FAppModules.Language.GetString('TFile21Agent.strPCDAnalysisStartVolumeErr');
               LMessage := Format(LMessage,[LLinesRead+1,21,24]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LSlurryDump.PCDAnalysisStartVolume.FData       := LReadReal;
              LSlurryDump.PCDAnalysisStartVolume.FInitalised := True;
            end;

            //Read Line 25
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
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strRechargeFactorErr');
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
          end;  //SlurryDump
        end;  //for number of mines

        //Read line 26
        LLinesRead := LLinesRead + 1;
        if(LLinesRead < LFileData.Count) then
        begin
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'26');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile21Agent.strMineSubCatchmentCountErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LMineSubCatchmentCount := LReadInteger;
          end;


          LMineSubCatchmentList := ADataObject.FMineSubCatchmentListObject;
          if not LMineSubCatchmentList.Initialise then
            Exit;

          for LMineSubCatchmentIndex := 0 to LMineSubCatchmentCount-1 do
          begin
            //Read line 27
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            LMineSubCatchment := LMineSubCatchmentList.AddMineSubCatchmentFileObject;
            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'27');

            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strCatchmentRefNumberErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,2]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LMineSubCatchment.CatchmentRefNumber.FData       := LReadInteger;
              LMineSubCatchment.CatchmentRefNumber.FInitalised := True;
            end;

            //Read Line 28
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Exit;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'28');
            for LIndex := 1 to 12 do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                 LMessage := FAppModules.Language.GetString('TFile21Agent.strMineSubCatchmentFlowVolumeErr');
                 LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                 AProgressFunction(LMessage,ptError,LStop);
                 if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LMineSubCatchment.MineSubCatchmentFlowVolume[LIndex].FData       := LReadReal;
                LMineSubCatchment.MineSubCatchmentFlowVolume[LIndex].FInitalised := True;
              end;
            end;

            //Read Line 29
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then
              Break;

            LReadString := LFileData[LLinesRead];
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'29');
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strProportionAntecedentFlowErr');
               LMessage := Format(LMessage,[LLinesRead+1,1,4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LMineSubCatchment.ProportionAntecedentFlow.FData       := LReadReal;
              LMineSubCatchment.ProportionAntecedentFlow.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strGroundwaterFlowVolumeErr');
               LMessage := Format(LMessage,[LLinesRead+1,5,8]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LMineSubCatchment.GroundwaterFlowVolume.FData       := LReadReal;
              LMineSubCatchment.GroundwaterFlowVolume.FInitalised := True;
            end;

            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile21Agent.strAntecedentRunoffDecayFactorErr');
               LMessage := Format(LMessage,[LLinesRead+1,9,12]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LMineSubCatchment.AntecedentRunoffDecayFactor.FData       := LReadReal;
              LMineSubCatchment.AntecedentRunoffDecayFactor.FInitalised := True;
            end;
          end;
        end;
        //Read Line  25 onwards
        for LIndex := LLinesRead+1 to  LFileData.Count-1 do
          LMineList.Comment.Add(LFileData[LIndex]);
      end;

      LMessage := FAppModules.Language.GetString('TFile21Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile21Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile21Agent.WriteModelDataToFile';
var
  LStop                  : boolean;
  LMinesIndex            : integer;
  LOpenCastIndex         : integer;
  LUnderGroundIndex      : integer;
  LSlurryDumpIndex       : integer;
  LMineSubCatchmentIndex : integer;
  LMessage               : string;
  LTempStr               : string;
  LOutString             : string;
  LIndex                 : Integer;
  LFileData              : TStringlist;
  LOpenCast              : TMineOpenCastFileObject;
  LUndeground            : TMineUndegroundFileObject;
  LSlurryDump            : TMineSlurryDumpFileObject;
  LMine                  : TMineFileObject;
  LMineList              : TMineListFileObject;
  LMineSubCatchment      : TMineSubCatchmentFileObject;
  LMineSubCatchmentList  : TMineSubCatchmentListFileObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile21Agent.strWritingStarted');
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

    LMineList := ADataObject.FMineListFileObject;
    if not Assigned(LMineList) then
      Exit;

    LFileData:= TStringList.Create;
    try
      if(LMineList.MineFileObjectCount > 0) then
      begin
        //Write Line 1
        LOutString := IntToStr(LMineList.MineFileObjectCount);
        LFileData.Add(LOutString);

        //___________________________________________Mine_____________________________________________________
        for LMinesIndex := 0 to LMineList.MineFileObjectCount-1 do
        begin
          //Write Line 2
          LMine := LMineList.MineFileObjectByIndex[LMinesIndex];

          LOutString  := IntToStr(LMine.NodeNumber.FData);
          LTempStr    := LMine.MineName.FData;
          LOutString  := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);

          //Write Line 3
          LOutString  := IntToStr(LMine.OpenCastCount);
          LTempStr    := IntToStr(LMine.UndegroundCount);
          LOutString  := LOutString + ' '+ LTempStr;
          LTempStr    := IntToStr(LMine.SlurryDumpCount);
          LOutString  := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);

          //Write Line 4
          LOutString  := IntToStr(LMine.RiverChannelNumber.FData);
          LTempStr    := IntToStr(LMine.PCDChannelNumber.FData);
          LOutString  := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);

          //Write Line 5
          LOutString  := '';
          for Lindex := 1 to 12 do
          begin
            LTempStr := Trim(PadDouble(LMine.PanEvaporation[Lindex]));
            LTempStr := RightTrimChars(LTempStr,'0');
            LOutString  := LOutString + ' '+ LTempStr;
          end;
          LFileData.Add(LOutString);

          //Write Line 6
          LOutString  := '';
          for Lindex := 1 to 12 do
          begin
            LTempStr := Trim(PadDouble(LMine.LakeEvaporation[Lindex]));
            LOutString  := LOutString + ' '+ LTempStr;
          end;
          LOutString := Trim(LOutString);
          LFileData.Add(LOutString);

          //Write Line 7
          LOutString  := IntToStr(LMine.HydrologyNodeNumber.FData);
          LFileData.Add(LOutString);

          //Write Line 8
          LOutString := Trim(PadDouble(LMine.BeneficiationPlantArea));
          LTempStr   := Trim(PadDouble(LMine.BeneficiationRunOffFactor));
          LOutString := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);

          //___________________________________________OpenCast_____________________________________________________
          for LOpenCastIndex := 0 to LMine.OpenCastCount-1 do
          begin
            LOpenCast := LMine.OpenCastByIndex[LOpenCastIndex];

            //Write Line 9
            LOutString  := LOpenCast.PitName.FData;
            LFileData.Add(LOutString);

            //Write Line 10
            LOutString := Trim(PadDouble(LOpenCast.CoalReserveArea));
            LTempStr   := Trim(PadDouble(LOpenCast.WorkingsArea));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 11
            LOutString := Trim(PadDouble(LOpenCast.DisturbedWorkingsArea));
            LTempStr   := Trim(PadDouble(LOpenCast.DisturbedArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.WaterSurfaceEvapArea));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 12
            LOutString := Trim(PadDouble(LOpenCast.DisturbedAreaRunOff));
            LTempStr   := Trim(PadDouble(LOpenCast.DisturbedWorkingsAreaRunOff));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 13
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LOpenCast.DisturbedRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 14
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LOpenCast.WorkingAreaRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 15
            LOutString := Trim(PadDouble(LOpenCast.DecantVolume));
            LTempStr   := Trim(PadDouble(LOpenCast.SeepageVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.AnalysisStartVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 16
            LOutString := Trim(PadDouble(LOpenCast.MaximumSeepageRate));
            LTempStr   := Trim(PadDouble(LOpenCast.SeepageExponent));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 17
            LOutString := Trim(PadDouble(LOpenCast.PCDSurfaceArea));
            LTempStr   := Trim(PadDouble(LOpenCast.PCDStorageCapacity));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LOpenCast.PCDAnalysisStartVolume));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);
          end;

          //___________________________________________Underground_____________________________________________________
          for LUnderGroundIndex := 0 to LMine.UndegroundCount-1 do
          begin
            LUndeground := LMine.UndegroundByIndex[LUnderGroundIndex];

            //Write Line 18
            LOutString  := LUndeground.UnderGroundSectionName.FData;
            LTempStr    := IntToStr(LUndeground.ChannelNumberToUGDam.FData);
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 19
            LOutString := Trim(PadDouble(LUndeground.UpstreamCatchmentArea));
            LTempStr   := Trim(PadDouble(LUndeground.BoardPillarCatchmentArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LUndeground.HighExtractionCatchmentArea));
            LOutString := LOutString + ' '+ LTempStr;
            LTempStr   := Trim(PadDouble(LUndeground.HighExtractionAreaRunoffFactor));
            LOutString := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 20
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LUndeground.UpstreamRunoffPortion[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 21
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LUndeground.BoardAndPilarRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

            //Write Line 22
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LUndeground.HighExtractionRechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);
          end;

          //___________________________________________SlurryDump_____________________________________________________
          for LSlurryDumpIndex := 0 to LMine.SlurryDumpCount-1 do
          begin
            LSlurryDump := LMine.SlurryDumpByIndex[LSlurryDumpIndex];

            //Write Line 23
            LOutString  := LSlurryDump.DumpName.FData;
            LFileData.Add(LOutString);

            //Write Line 24
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
            LFileData.Add(LOutString);

            //Write Line 25
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LSlurryDump.RechargeFactor[Lindex]));
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LOutString := Trim(LOutString);
            LFileData.Add(LOutString);

          end;
        end;

        LMineSubCatchmentList := ADataObject.FMineSubCatchmentListObject;
        if not Assigned(LMineSubCatchmentList) then
         Exit;

        //Write Line 26
        LOutString := IntToStr(LMineSubCatchmentList.MineSubCatchmentObjectCount);
        LFileData.Add(LOutString);

        for LMineSubCatchmentIndex := 0 to LMineSubCatchmentList.MineSubCatchmentObjectCount-1 do
        begin
          LMineSubCatchment := LMineSubCatchmentList.MineSubCatchmentObjectByIndex[LMineSubCatchmentIndex];

          //Write Line 27
          LOutString  := IntToStr(LMineSubCatchment.CatchmentRefNumber.FData);
          LFileData.Add(LOutString);

          //Write Line 28
          LOutString  := '';
          for Lindex := 1 to 12 do
          begin
            LTempStr := Trim(PadDouble(LMineSubCatchment.MineSubCatchmentFlowVolume[Lindex]));
            LOutString  := LOutString + ' '+ LTempStr;
          end;
          LOutString := Trim(LOutString);
          LFileData.Add(LOutString);

          //Write Line 29
          LOutString := Trim(PadDouble(LMineSubCatchment.ProportionAntecedentFlow));
          LTempStr   := Trim(PadDouble(LMineSubCatchment.GroundwaterFlowVolume));
          LOutString := LOutString + ' '+ LTempStr;
          LTempStr   := Trim(PadDouble(LMineSubCatchment.AntecedentRunoffDecayFactor));
          LOutString := LOutString + ' '+ LTempStr;
          LFileData.Add(LOutString);
      end;

      //Write Line 30 onwards
        for LIndex := 0 to LMineList.Comment.Count-1 do
          LFileData.Add(LMineList.Comment[LIndex]);

        LFileData.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);
      end;

      if(Trim(LFileData.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile21Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

