//
//
//  UNIT      : Contains TFileReservoirImplementationAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileReservoirImplementationAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UReservoirImplementationFileDataObjects,
  UPlanningFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UPlanningModelDataObject;

type

  TFileReservoirImplementationAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses
  UBasicObjects,
  UUtilities,
  VoaimsCom_TLB,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

function TFileReservoirImplementationAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReservoirImplementationAgent.ReadModelDataFromFile';
var
  LFileData: TStringList;
  LMessage,
  LReadString,
  LTempString : String;
  LStart,
  LCount,
  LReadInteger,
  LErrorCode : Integer;
  LReadReal : Double;
  LStop: boolean;
  LReservoirImplementation  : TReservoirImplementationObject;
  LShortTermFamilyGroup     : TShortTermFamilyGroupObject;
  LChannelSwitchFile        : TChannelSwitchFileObject;
  LHydroPowerAllocFile      : THydroPowerAllocFileObject;
  LPlanningFileDataObject   : TPlanningFileDataObjects;
  LReservoirAndFilesImplementation : TReservoirAndFilesImplementationObject;
  LReservoirImplementationCount    : integer;
  LShortTermFamilyGroupCount       : integer;
  LShortTermFamilyFileCount        : integer;
  //LMonthFileActive                 : integer;
  //LYearFileActive                  : integer;
  LIndex                           : integer;
  LSwitchFileCount                 : integer;
  LHydroPowerAllocFileCount        : integer;
  LLocalCount                      : integer;
  //LHydroPowerFileCount             : integer;
  LFileName                        : TString;
  LFileLineTypesObject             : TAbstractFileLineTypesObject;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strReadingStarted');
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
    LReservoirAndFilesImplementation    := LPlanningFileDataObject.ReservoirAndFilesImplementation;
    if(LReservoirAndFilesImplementation = nil) then
      Exit;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LReservoirAndFilesImplementation.Initialise then
      Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F01 file
      LFileData.LoadFromFile(AFilename.FileName);
      LStart := 0;

      //line 1 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      LReservoirImplementationCount    := 0;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strReservoirImplementationCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      {if(LReadInteger > 100) then
      begin
        LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strReservoirImplementationCountValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      }begin
        LReservoirImplementationCount       := LReadInteger;
      end;

      for LCount := 1 to LReservoirImplementationCount do
      begin
        //line 2 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LReservoirImplementation := LReservoirAndFilesImplementation.AddReservoirImplementationObject;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strReservoirNumberErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.ReservoirNumber.FData       := LReadInteger;
          LReservoirImplementation.ReservoirNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strReplacedReservoirNumberErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.ReplacedReservoirNumber.FData       := LReadInteger;
          LReservoirImplementation.ReplacedReservoirNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strYearDamActiveErr');
          LMessage := Format(LMessage,[LStart,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.YearDamActive.FData       := LReadInteger;
          LReservoirImplementation.YearDamActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strMonthDamActiveErr');
          LMessage := Format(LMessage,[LStart,4]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.MonthDamActive.FData       := LReadInteger;
          LReservoirImplementation.MonthDamActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strYearDamAbsoleteErr');
          LMessage := Format(LMessage,[LStart,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.YearDamAbsolete.FData       := LReadInteger;
          LReservoirImplementation.YearDamAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strMonthDamAbsoleteErr');
          LMessage := Format(LMessage,[LStart,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.MonthDamAbsolete.FData       := LReadInteger;
          LReservoirImplementation.MonthDamAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strEconomicLifeOfDamErr');
          LMessage := Format(LMessage,[LStart,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.EconomicLifeOfDam.FData       := LReadInteger;
          LReservoirImplementation.EconomicLifeOfDam.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strCapitalCostErr');
          LMessage := Format(LMessage,[LStart,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.CapitalCost.FData       := LReadReal;
          LReservoirImplementation.CapitalCost.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strMaintenanceCostErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.MaintenanceCost.FData       := LReadReal;
          LReservoirImplementation.MaintenanceCost.FInitalised := True;
        end;

        //line 3 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strYearsInConstructionErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirImplementation.YearsInConstruction.FData       := LReadInteger;
          LReservoirImplementation.YearsInConstruction.FInitalised := True;
        end;

        if(LReservoirImplementation.YearsInConstruction.FData > 0) then
        begin
          LTempString := Trim(LReadString);
          if(LTempString <> '') then
          {begin
            LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strCostScheduleErr');
            LMessage := Format(LMessage,[LStart,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          }begin
            LTempString  := CommaTextString(LTempString);
            LReservoirImplementation.CostSchedule.FData       := LTempString;
            LReservoirImplementation.CostSchedule.FLength     := Length(LTempString);
            LReservoirImplementation.CostSchedule.FInitalised := True;
          end;
        end;
      end;

      //line 4 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      LShortTermFamilyGroupCount := 0;
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strNumberOfPeriodsErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
        LShortTermFamilyGroupCount := LReadInteger;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
        LShortTermFamilyFileCount := NullInteger
      else
        LShortTermFamilyFileCount := LReadInteger;

      for LIndex := 1 to LShortTermFamilyGroupCount do
      begin
        LShortTermFamilyGroup := LReservoirAndFilesImplementation.AddShortTermFamilyGroupObject;
        //line 5a +++++++++++++++++++++++++
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'5');
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strYearFileActiveErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LShortTermFamilyGroup.YearFileActive.FInitalised := True;
          LShortTermFamilyGroup.YearFileActive.FData       := LReadInteger;
        end;

        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'5');
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strMonthFileActiveErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LShortTermFamilyGroup.MonthFileActive.FInitalised := True;
          LShortTermFamilyGroup.MonthFileActive.FData       := LReadInteger;
        end;

        if(LShortTermFamilyFileCount = NullInteger) then
        begin
          LTempString := Trim(LReadString);
          LTempString := AnsiDequotedStr(LTempString,'''');
          LTempString := Trim(LTempString);
          if(LTempString = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileNameErr');
            LMessage := Format(LMessage,[LStart,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
           LFileName := LShortTermFamilyGroup.CreateShortTermFamilyFileObject;
           LFileName.FData       := LTempString;
           LFileName.FLength     := Length(LTempString);
           LFileName.FInitalised := True;
          end;
        end
        else
        begin
          for LCount := 1 to LShortTermFamilyFileCount do
          begin
            LFileName := LShortTermFamilyGroup.CreateShortTermFamilyFileObject;
            //line 5b +++++++++++++++++++++++++
            LReadString := LFileData[LStart];
            LStart      := LStart + 1;
            LTempString := Trim(LReadString);
            LTempString := AnsiDequotedStr(LTempString,'''');
            LTempString := Trim(LTempString);
            if(LTempString = '') then
            begin
              LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileNameErr');
              LMessage := Format(LMessage,[LStart,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LFileName.FData       := LTempString;
              LFileName.FLength     := Length(LTempString);
              LFileName.FInitalised := True;
            end;
          end;
        end;
      end;

      //line 6 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;

      LLocalCount := LFileLineTypesObject.LinesCount;
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'6');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      LSwitchFileCount := 0;
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strSwitchFileCountErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LSwitchFileCount       := LReadInteger;
      end;

      for LCount := 1 to LSwitchFileCount do
      begin
        //line 7 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LChannelSwitchFile := LReservoirAndFilesImplementation.AddChannelSwitchFileObject;
        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strYearFileActive1Err');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelSwitchFile.YearFileActive.FData       := LReadInteger;
          LChannelSwitchFile.YearFileActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'7');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strMonthFileActive1Err');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelSwitchFile.MonthFileActive.FData       := LReadInteger;
          LChannelSwitchFile.MonthFileActive.FInitalised := True;
        end;

        LTempString := Trim(LReadString);
        LTempString := AnsiDequotedStr(LTempString,'''');
        LTempString := Trim(LTempString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileName1Err');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LChannelSwitchFile.FileName.FData       := LTempString;
          LChannelSwitchFile.FileName.FLength     := Length(LTempString);
          LChannelSwitchFile.FileName.FInitalised := True;
       end;
      end;

      if(ADataObject.FRunParametersObject.FHydroPowerOption.FData <> 'N') then
      begin
        //line 8 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        LHydroPowerAllocFileCount := 0;
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strSwitchFileCountErr');
          LMessage := Format(LMessage,[LStart]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LHydroPowerAllocFileCount       := LReadInteger;
        end;

        //line 9 +++++++++++++++++++++++++
        for LCount := 1 to LHydroPowerAllocFileCount do
        begin
          LReadString := LFileData[LStart];
          LStart      := LStart + 1;
          LHydroPowerAllocFile := LReservoirAndFilesImplementation.AddHydroPowerAllocFileObject;

          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strYearFileActive1Err');
            LMessage := Format(LMessage,[LStart,1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LHydroPowerAllocFile.YearFileActive.FData       := LReadInteger;
            LHydroPowerAllocFile.YearFileActive.FInitalised := True;
          end;

          LTempString := Trim(LReadString);
          LTempString := AnsiDequotedStr(LTempString,'''');
          LTempString := Trim(LTempString);
          if(LTempString = '') then
          begin
            LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileName1Err');
            LMessage := Format(LMessage,[LStart,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LHydroPowerAllocFile.FileName.FData       := LTempString;
            LHydroPowerAllocFile.FileName.FLength     := Length(LTempString);
            LHydroPowerAllocFile.FileName.FInitalised := True;
          end;
        end;
      end;

      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'Y') then
      begin
        //line 10 +++++++++++++++++++++++++
        LTempString := Trim(LReadString);
        LTempString := AnsiDequotedStr(LTempString,'''');
        LTempString := Trim(LTempString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileName1Err');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData       := LTempString;
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FLength     := Length(LTempString);
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised := True;
        end;
      end;

      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'I') then
      begin
        //line 10 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LTempString := Trim(LReadString);
        LTempString := AnsiDequotedStr(LTempString,'''');
        LTempString := Trim(LTempString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileName1Err');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData       := LTempString;
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FLength     := Length(LTempString);
          LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised := True;
        end;
      end;
      LStart      := LStart + 1;
      for LCount := LStart  to LFileData.Count - 1 do
        LReservoirAndFilesImplementation.FMExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReservoirImplementationAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReservoirImplementationAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LIndex         : Integer;
  LIndex2        : Integer;

  LFileName                 : TString;
  LReservoirImplementation  : TReservoirImplementationObject;
  LShortTermFamilyGroup     : TShortTermFamilyGroupObject;
  LChannelSwitchFile        : TChannelSwitchFileObject;
  LHydroPowerAllocFile      : THydroPowerAllocFileObject;
  LPlanningFileDataObject   : TPlanningFileDataObjects;
  LReservoirAndFilesImplementation : TReservoirAndFilesImplementationObject;
begin
  Result := False;
  try
    If FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject             := TPlanningFileDataObjects(ADataObject);
    LReservoirAndFilesImplementation    := LPlanningFileDataObject.ReservoirAndFilesImplementation;

    if(LReservoirAndFilesImplementation.ReservoirImplementationObjectCount = 0) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strWritingStarted');
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
      LOutString:=Format('%5d',[LReservoirAndFilesImplementation.ReservoirImplementationObjectCount]);
      LFileData.Add(LOutString);

      for LIndex := 0 to LReservoirAndFilesImplementation.ReservoirImplementationObjectCount -1 do
      begin
        //line 2 +++++++++++++++++++++++++
        LReservoirImplementation := LReservoirAndFilesImplementation.ReservoirImplementationObjectByIndex[LIndex];
        LOutString:='';

        LTempString:=PadInt(LReservoirImplementation.ReservoirNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoirImplementation.ReplacedReservoirNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoirImplementation.YearDamActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoirImplementation.MonthDamActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoirImplementation.YearDamAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoirImplementation.MonthDamAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LReservoirImplementation.EconomicLifeOfDam);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LReservoirImplementation.CapitalCost);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LReservoirImplementation.MaintenanceCost);
        LOutString:=LOutString+LTempString;

        LFileData.Add(LOutString);
        //line 3 +++++++++++++++++++++++++
        LOutString:='';

        LTempString:=PadInt(LReservoirImplementation.YearsInConstruction);
        LOutString:=LOutString+LTempString;
        LOutString:=LOutString+ ' ' + LReservoirImplementation.CostSchedule.FData;

        LFileData.Add(LOutString);
      end;

      for LIndex := 0 to LReservoirAndFilesImplementation.ShortTermFamilyGroupObjectCount -1 do
      begin
        LShortTermFamilyGroup := LReservoirAndFilesImplementation.ShortTermFamilyGroupObjectByIndex[LIndex];

        //line 4 +++++++++++++++++++++++++
        if(LIndex = 0) then
        begin
          LOutString:=Format('%5d',[LReservoirAndFilesImplementation.ShortTermFamilyGroupObjectCount]);
          LOutString:=LOutString + Format('%5d',[LShortTermFamilyGroup.ShortTermFamilyFileObjectCount]);
          LFileData.Add(LOutString);
        end;

        //line 5a +++++++++++++++++++++++++
        LOutString  := '';
        LTempString := PadInt(LShortTermFamilyGroup.YearFileActive);
        LOutString:=LOutString+LTempString;
        LTempString := PadInt(LShortTermFamilyGroup.MonthFileActive);
        LOutString:=LOutString+LTempString;
        LFileData.Add(LOutString);
        for LIndex2 := 0 to LShortTermFamilyGroup.ShortTermFamilyFileObjectCount -1 do
        begin
          LFileName := LShortTermFamilyGroup.ShortTermFamilyFileObjectByIndex[LIndex2];
          LOutString:='   ' +  QuotedStr(LFileName.FData);
          LFileData.Add(LOutString);
        end;
      end;

      //line 6 +++++++++++++++++++++++++
      LOutString:=Format('%5d',[LReservoirAndFilesImplementation.ChannelSwitchFileObjectCount]);
      LFileData.Add(LOutString);
      for LIndex := 0 to LReservoirAndFilesImplementation.ChannelSwitchFileObjectCount -1 do
      begin
        //line 7 +++++++++++++++++++++++++
        LChannelSwitchFile := LReservoirAndFilesImplementation.ChannelSwitchFileObjectByIndex[LIndex];
        LOutString  := '';
        LTempString := PadInt(LChannelSwitchFile.YearFileActive);
        LOutString:=LOutString+LTempString;
        LTempString := PadInt(LChannelSwitchFile.MonthFileActive);
        LOutString:=LOutString+LTempString;
        LOutString:=LOutString+ '   ' +  QuotedStr(LChannelSwitchFile.FileName.FData);
        LFileData.Add(LOutString);
      end;

      if(ADataObject.FRunParametersObject.FHydroPowerOption.FData <> 'N') then
      begin
        //line 8 +++++++++++++++++++++++++
        LOutString:=Format('%5d',[LReservoirAndFilesImplementation.HydroPowerAllocFileObjectCount]);
        LFileData.Add(LOutString);
        for LIndex := 0 to LReservoirAndFilesImplementation.HydroPowerAllocFileObjectCount -1 do
        begin
          //line 9 +++++++++++++++++++++++++
          LHydroPowerAllocFile := LReservoirAndFilesImplementation.HydroPowerAllocFileObjectByIndex[LIndex];
          LOutString  := '';
          LTempString := PadInt(LHydroPowerAllocFile.YearFileActive);
          LOutString:=LOutString+LTempString;
          LOutString:=LOutString+ '  ' +  QuotedStr(LHydroPowerAllocFile.FileName.FData);
          LFileData.Add(LOutString);
        end;
      end;

      if(ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'Y') or
        (ADataObject.FRunParametersObject.FAllocationControlOption.FData = 'I') then
      begin
        //line 10 +++++++++++++++++++++++++
        if LReservoirAndFilesImplementation.AllocationControlChannelFileName.FInitalised  then
        begin
          LOutString:='  ' +  QuotedStr(LReservoirAndFilesImplementation.AllocationControlChannelFileName.FData);
          LFileData.Add(LOutString);
        end;
      end;


      for LIndex := 0 to LReservoirAndFilesImplementation.FMExtraLines.Count -1 do
      begin
        LFileData.Add(LReservoirAndFilesImplementation.FMExtraLines[LIndex]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
