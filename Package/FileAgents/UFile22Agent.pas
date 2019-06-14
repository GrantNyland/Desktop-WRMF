//
//
//  UNIT      : Contains TFile22Agent Class
//  AUTHOR    : Presley Mudau
//  DATE      : 02/10/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFile22Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UGroundWaterFileObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile22Agent = class(TAbstractFileAgent)
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

function TFile22Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile22Agent.ReadModelDataFromFile';
var
  LFileData         : TStringList;
  LMessage          : string;
  LReadString       : String;
  LTempString       : string;
  LReadReal         : Double;
  LLinesRead        : integer;
  LErrorCode        : integer;
  LReadInteger      : integer;
  LIndex            : Integer;
  LGroundWaterIndex : integer;
  LGroundWater      : TGroundWaterFileObject;
  LGroundWaterList  : TGroundWaterListFileObject;
  LStop             : boolean;
  LFileLineTypesObject       : TAbstractFileLineTypesObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile22Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile22Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LGroundWaterList := ADataObject.FGroundWaterListFileObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];

    if not LGroundWaterList.Initialise then
    Exit;

    LFileData := TStringList.Create;
    try
      //Read the F22.dat file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      //Read Line 1
      LLinesRead := 0;
      if (LFileData.Count > 0) then
      begin

        LGroundWater  := LGroundWaterList.AddGroundWaterObject;
        for LGroundWaterIndex := 0 to LGroundWaterList.GroundWaterObjectCount-1 do
        begin
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strAquiferNodeNumberErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.AquiferNodeNumber.FData  := LReadInteger;
            LGroundWater.AquiferNodeNumber.FInitalised := True;
          end;

          //Read Line 2
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;
          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strAquiferStorativityErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.AquiferStorativity.FData := LReadReal;
            LGroundWater.AquiferStorativity.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strAquiferStaticWaterLevelErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.AquiferStaticWaterLevel.FData := LReadReal;
            LGroundWater.AquiferStaticWaterLevel.FInitalised := True;
          end;

          //Read Line 3
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile21Agent.strUnsaturatedStorageCapacityErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.UnsaturatedStorageCapacity.FData := LReadReal;
            LGroundWater.UnsaturatedStorageCapacity.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strInitialUnsaturatedStorageErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.InitialUnsaturatedStorage.FData := LReadReal;
            LGroundWater.InitialUnsaturatedStorage.FInitalised := True;
          end;

          //Read Line 4
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strMaximumDischargeRateErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.MaximumDischargeRate.FData := LReadReal;
            LGroundWater.MaximumDischargeRate.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strMovingAverageRechargeErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.MovingAverageRecharge.FData := LReadReal;
            LGroundWater.MovingAverageRecharge.FInitalised := True;
          end;

          //Read Line 5
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmanSoilMoistureCapacityErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmanSoilMoistureCapacity.FData := LReadReal;
            LGroundWater.PitmanSoilMoistureCapacity.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmanSoilMoistureStorageCapacityErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmanSoilMoistureStorageCapacity.FData := LReadReal;
            LGroundWater.PitmanSoilMoistureStorageCapacity.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmansoilMoistureFlowStateErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmansoilMoistureFlowState.FData := LReadReal;
            LGroundWater.PitmansoilMoistureFlowState.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmanSoilMoistureFlowEquationErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmanSoilMoistureFlowEquation.FData := LReadReal;
            LGroundWater.PitmanSoilMoistureFlowEquation.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmanMaximumGroundwaterFlowErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmanMaximumGroundwaterFlow.FData := LReadReal;
            LGroundWater.PitmanMaximumGroundwaterFlow.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmanSoilMoistureRechargeEquationErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmanSoilMoistureRechargeEquation.FData := LReadReal;
            LGroundWater.PitmanSoilMoistureRechargeEquation.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPitmanGroundwaterFlowErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PitmanGroundwaterFlow.FData := LReadReal;
            LGroundWater.PitmanGroundwaterFlow.FInitalised := True;
          end;

          //Read Line 6
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strMaximumRateOfGroundwaterBaseFlowErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.MaximumRateOfGroundwaterBaseFlow.FData := LReadReal;
            LGroundWater.MaximumRateOfGroundwaterBaseFlow.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strPowerHeadDifferenceBaseFlowEquationErr');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.PowerHeadDifferenceBaseFlowEquation.FData := LReadReal;
            LGroundWater.PowerHeadDifferenceBaseFlowEquation.FInitalised := True;
          end;

          //Read Line 7
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strMaximumHydrologicalGradientErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.MaximumHydrologicalGradient.FData := LReadReal;
            LGroundWater.MaximumHydrologicalGradient.FInitalised := True;
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
             LMessage := FAppModules.Language.GetString('TFile22Agent.strAquiferTransmissivityErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.AquiferTransmissivity.FData := LReadReal;
            LGroundWater.AquiferTransmissivity.FInitalised := True;
          end;

          //Read Line 9
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strBoreHoleDistanceToRiverErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.BoreHoleDistanceToRiver.FData := LReadReal;
            LGroundWater.BoreHoleDistanceToRiver.FInitalised := True;
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
             LMessage := FAppModules.Language.GetString('TFile22Agent.strMaximumGroundwaterAbstractionErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.MaximumGroundwaterAbstraction.FData := LReadReal;
            LGroundWater.MaximumGroundwaterAbstraction.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strParameterK2Err');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.ParameterK2.FData := LReadReal;
            LGroundWater.ParameterK2.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strParameterK3Err');
             LMessage := Format(LMessage,[LLinesRead+1,5,8]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.ParameterK3.FData := LReadReal;
            LGroundWater.ParameterK3.FInitalised := True;
          end;

          //Read Line 11
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'11');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile22Agent.strMonthlyWaterEvaporationErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LGroundWater.MonthlyWaterEvaporation[LIndex].FData       := LReadReal;
              LGroundWater.MonthlyWaterEvaporation[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 12
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'12');
          for LIndex := 1 to 12 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
               LMessage := FAppModules.Language.GetString('TFile22Agent.strMonthlyWaterUsageFactorsErr');
               LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
               AProgressFunction(LMessage,ptError,LStop);
               if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LGroundWater.MonthlyWaterUsageFactors[LIndex].FData       := LReadReal;
              LGroundWater.MonthlyWaterUsageFactors[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 13
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'13');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile22Agent.strGroundWaterEvaporationAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,4]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LGroundWater.GroundWaterEvaporationArea.FData := LReadReal;
            LGroundWater.GroundWaterEvaporationArea.FInitalised := True;
          end;

          //Read Line 14 onwards
          for LIndex := LLinesRead+1 to  LFileData.Count-1 do
            LGroundWaterList.Comment.Add(LFileData[LIndex]);
        end;
      end;

      LMessage := FAppModules.Language.GetString('TFile22Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile22Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile22Agent.WriteModelDataToFile';
var
  LGroundWaterIndex  : integer;
  LMessage           : string;
  LTempStr           : string;
  LOutString         : string;
  LIndex             : Integer;
  LFileData          : TStringlist;
  LGroundWater       : TGroundWaterFileObject;
  LGroundWaterList   : TGroundWaterListFileObject;
  LStop              : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile22Agent.strWritingStarted');
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

    LGroundWaterList := ADataObject.FGroundWaterListFileObject;
    if not Assigned(LGroundWaterList) then
      Exit;

    LFileData:= TStringList.Create;
    try
      if(LGroundWaterList.GroundWaterObjectCount > 0) then
      begin
        for LGroundWaterIndex := 0 to LGroundWaterList.GroundWaterObjectCount-1 do
        begin
          LGroundWater := LGroundWaterList.GroundWaterObjectByIndex[LGroundWaterIndex];
          if Assigned(LGroundWater) then
          begin
            //Write Line 1
            LOutString  := IntToStr(LGroundWater.AquiferNodeNumber.FData);
            LFileData.Add(LOutString);

            //Write Line 2
            LOutString  := Trim(PadDouble(LGroundWater.AquiferStorativity));
            LTempStr    := Trim(PadDouble(LGroundWater.AquiferStaticWaterLevel));
            LOutString  := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 3
            LOutString  := Trim(PadDouble(LGroundWater.UnsaturatedStorageCapacity));
            LTempStr    := Trim(PadDouble(LGroundWater.InitialUnsaturatedStorage));
            LOutString  := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 4
            //LOutString  := Trim(PadDouble(LGroundWater.MaximumDischargeRate));
            //LTempStr    := Trim(PadDouble(LGroundWater.MovingAverageRecharge));
            LOutString  := IntToStr(Trunc(LGroundWater.MaximumDischargeRate.FData));
            LTempStr    := IntToStr(Trunc(LGroundWater.MovingAverageRecharge.FData));
            LOutString  := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 5
            LOutString  := Trim(PadDouble(LGroundWater.PitmanSoilMoistureCapacity));
            LTempStr    := Trim(PadDouble(LGroundWater.PitmanSoilMoistureStorageCapacity));
            LOutString  := LOutString + ' ' + LTempStr;
            LTempStr := Trim(PadDouble(LGroundWater.PitmansoilMoistureFlowState));
            LOutString  := LOutString + ' ' + LTempStr;
            LTempStr := Trim(PadDouble(LGroundWater.PitmanSoilMoistureFlowEquation));
            LOutString  := LOutString + ' ' + LTempStr;
            LTempStr := Trim(PadDouble(LGroundWater.PitmanMaximumGroundwaterFlow));
            LOutString  := LOutString + ' ' + LTempStr;
            LTempStr := Trim(PadDouble(LGroundWater.PitmanSoilMoistureRechargeEquation));
            LOutString  := LOutString + ' ' + LTempStr;
            LTempStr := Trim(PadDouble(LGroundWater.PitmanGroundwaterFlow));
            LOutString  := LOutString + ' ' + LTempStr;
            LFileData.Add(LOutString);

            //Write Line 6
            LOutString  := Trim(PadDouble(LGroundWater.MaximumRateOfGroundwaterBaseFlow));
            LTempStr    := Trim(PadDouble(LGroundWater.PowerHeadDifferenceBaseFlowEquation));
            LOutString  := LOutString + ' '+ LTempStr;
            LFileData.Add(LOutString);

            //Write Line 7
            LOutString  := Trim(PadDouble(LGroundWater.MaximumHydrologicalGradient));
            LFileData.Add(LOutString);

            //Write Line 8
            LOutString  := Trim(PadDouble(LGroundWater.AquiferTransmissivity));
            LFileData.Add(LOutString);

            //Write Line 9
            LOutString  := Trim(PadDouble(LGroundWater.BoreHoleDistanceToRiver));
            LFileData.Add(LOutString);

            //Write Line 10
            LOutString  := Trim(PadDouble(LGroundWater.MaximumGroundwaterAbstraction));
            LTempStr := Trim(PadDouble(LGroundWater.ParameterK2));
            LOutString  := LOutString + ' ' + LTempStr;
            LTempStr := Trim(PadDouble(LGroundWater.ParameterK3));
            LOutString  := LOutString + ' ' + LTempStr;
            LFileData.Add(LOutString);

            //Write Line 11
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LGroundWater.MonthlyWaterEvaporation[Lindex]));
              LTempStr := RightTrimChars(LTempStr,'0');
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LFileData.Add(LOutString);

            //Write Line 12
            LOutString  := '';
            for Lindex := 1 to 12 do
            begin
              LTempStr := Trim(PadDouble(LGroundWater.MonthlyWaterUsageFactors[Lindex]));
              LTempStr := RightTrimChars(LTempStr,'0');
              LOutString  := LOutString + ' '+ LTempStr;
            end;
            LFileData.Add(LOutString);

            //Write Line 13
            LOutString  := Trim(PadDouble(LGroundWater.GroundWaterEvaporationArea));
            LFileData.Add(LOutString);
          end;
        end;

        //Write Line 14 onwards
        for LIndex := 0 to LGroundWaterList.Comment.Count-1 do
          LFileData.Add(LGroundWaterList.Comment[LIndex]);

        LFileData.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);
      end;

      if(Trim(LFileData.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile22Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

