//
//
//  UNIT      : Contains TFileDisbenefitAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileDisbenefitAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UDataFileObjects,
  UDisbenefitFileDataObjects,
  UPlanningFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UPlanningModelDataObject;

type

  TFileDisbenefitAgent = class(TAbstractFileAgent)
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

function TFileDisbenefitAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDisbenefitAgent.ReadModelDataFromFile';
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
  LDisbenefit             : TDisbenefitFileObject;
  LDisbenefitFileData     : TDisbenefitFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
  LLocalCount             : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strReadingStarted');
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
    LDisbenefitFileData      := LPlanningFileDataObject.DisbenefitFileDataObject;
    if(LDisbenefitFileData = nil) then
      Exit;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LDisbenefitFileData.Initialise then
      Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData := TStringList.Create;
    try
      //Read the F01 file
      LFileData.LoadFromFile(AFilename.FileName);
      if(LFileData.Count = 0) then Exit;
      LStart := 0;

      //line 1 +++++++++++++++++++++++++
      LReadString := LFileData[LStart];
      LStart      := LStart + 1;
      if(LStart >= LFileData.Count) then Exit;

      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      LTempString:=ExtractFirstSubstring(LReadString);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strDataYearsErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      {if(LReadInteger > 100) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strDataYearsValErr');
        LMessage := Format(LMessage,[LStart]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      }begin
        LDisbenefitFileData.DataYears.FData       := LReadInteger;
        LDisbenefitFileData.DataYears.FInitalised := True;
      end;

      while (LStart < LFileData.Count) and  (Trim(LFileData[LStart]) <> '')  do
      begin
        //line 2 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;
        LDisbenefit := LDisbenefitFileData.AddDisbenefit;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');

        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strChannelNumberErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.ChannelNumber.FData       := LReadInteger;
          LDisbenefit.ChannelNumber.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strYearChannelActiveErr');
          LMessage := Format(LMessage,[LStart,3]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.YearChannelActive.FData       := LReadInteger;
          LDisbenefit.YearChannelActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strMonthChannelActiveErr');
          LMessage := Format(LMessage,[LStart,4]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.MonthChannelActive.FData       := LReadInteger;
          LDisbenefit.MonthChannelActive.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strYearChannelObsoleteErr');
          LMessage := Format(LMessage,[LStart,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.YearChannelAbsolete.FData       := LReadInteger;
          LDisbenefit.YearChannelAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strMonthChannelObsoleteErr');
          LMessage := Format(LMessage,[LStart,6]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.MonthChannelAbsolete.FData       := LReadInteger;
          LDisbenefit.MonthChannelAbsolete.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strFunctionXErr');
          LMessage := Format(LMessage,[LStart,7]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.FunctionX.FData       := LReadReal;
          LDisbenefit.FunctionX.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strFunctionYErr');
          LMessage := Format(LMessage,[LStart,8]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.FunctionY.FData       := LReadReal;
          LDisbenefit.FunctionY.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strFunctionNonSupplyErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.FunctionNonSupply.FData       := LReadReal;
          LDisbenefit.FunctionNonSupply.FInitalised := True;
        end;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'2');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strFunctionCostErr');
          LMessage := Format(LMessage,[LStart,9]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.FunctionCost.FData       := LReadReal;
          LDisbenefit.FunctionCost.FInitalised := True;
        end;

        //line 3 +++++++++++++++++++++++++
        if(LStart >= LFileData.Count) then Break;
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;


        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'3');
        LTempString:=Trim(LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strEscalationRateErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LTempString  := CommaTextString(LTempString);
          LDisbenefit.EscaltionRate.FData       := LTempString;
          LDisbenefit.EscaltionRate.FLength     := Length(LTempString);
          LDisbenefit.EscaltionRate.FInitalised := True;
        end;

        //line 4 +++++++++++++++++++++++++
        if(LStart >= LFileData.Count) then Break;
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;

        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
        LTempString:=ExtractFirstSubstring(LReadString);
        LTempString := Trim(LTempString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strWaterQualityConstraintErr');
          LMessage := Format(LMessage,[LStart,1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LDisbenefit.WaterQualityConstraint.FData       := LReadReal;
          LDisbenefit.WaterQualityConstraint.FInitalised := True;
        end;

        for LCount := 1  to 4 do
        begin
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'4');
          LTempString:=ExtractFirstSubstring(LReadString);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strTDSConcentrationErr');
            LMessage := Format(LMessage,[LStart,1]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LDisbenefit.TDSConcentration[LCount].FData       := LReadReal;
            LDisbenefit.TDSConcentration[LCount].FInitalised := True;
          end;
        end;

        //line 5 +++++++++++++++++++++++++
        LReadString := LFileData[LStart];
        LStart      := LStart + 1;


        LLocalCount := LFileLineTypesObject.LinesCount;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LLocalCount,'5');
        LTempString:=Trim(LReadString);
        if(LTempString = '') then
        begin
          LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strWQDisbenefitErr');
          LMessage := Format(LMessage,[LStart,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LTempString  := CommaTextString(LTempString);
          LDisbenefit.WQDisbenefit.FData       := LTempString;
          LDisbenefit.WQDisbenefit.FLength     := Length(LTempString);
          LDisbenefit.WQDisbenefit.FInitalised := True;
        end;
      end;

      for LCount := LStart  to LFileData.Count - 1 do
        LDisbenefitFileData.FMExtraLines.Add(LFileData[LCount]);

      LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDisbenefitAgent.WriteModelDataToFile';
var
  LMessage       : string;
  LOutString     : string;
  LTempString    : string;
  LStop          : boolean;
  LFileData      : TStringList;
  LIndex,
  LCount         : Integer;
  LDisbenefit         : TDisbenefitFileObject;
  LDisbenefitFileData  : TDisbenefitFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LDisbenefitFileData   := LPlanningFileDataObject.DisbenefitFileDataObject;

    if(LDisbenefitFileData.DisbenefitCount  = 0) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReservoirImplementationAgent.strFileDataNotLoaded');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strWritingStarted');
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
      LTempString:=PadInt(LDisbenefitFileData.DataYears);
      LOutString:=LOutString+ ' ' + LTempString;
      LFileData.Add(LOutString);

      for LCount := 0 to LDisbenefitFileData.DisbenefitCount -1 do
      begin
        //line 2 +++++++++++++++++++++++++
        LDisbenefit := LDisbenefitFileData.DisbenefitByIndex[LCount];
        LOutString:='';

        LTempString:=PadInt(LDisbenefit.ChannelNumber);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDisbenefit.YearChannelActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDisbenefit.MonthChannelActive);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDisbenefit.YearChannelAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadInt(LDisbenefit.MonthChannelAbsolete);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LDisbenefit.FunctionX);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LDisbenefit.FunctionY);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LDisbenefit.FunctionNonSupply);
        LOutString:=LOutString+LTempString;

        LTempString:=PadDouble(LDisbenefit.FunctionCost);
        LOutString:=LOutString+LTempString;

        LFileData.Add(LOutString);

        //line 3 +++++++++++++++++++++++++
        LOutString:='';
        LOutString:=LOutString+ '     ' + StringReplace(LDisbenefit.EscaltionRate.FData,',',' ',[rfReplaceAll]);
        LFileData.Add(LOutString);

        //line 4 +++++++++++++++++++++++++
        LOutString:='';
        LTempString:=PadDouble(LDisbenefit.WaterQualityConstraint);
        LOutString:=LOutString+LTempString;
        for LIndex := 1  to 4 do
        begin
          LTempString:=PadDouble(LDisbenefit.TDSConcentration[LIndex]);
          LOutString:=LOutString+LTempString;
        end;
        LFileData.Add(LOutString);

        //line 3 +++++++++++++++++++++++++
        LOutString:='';
        LOutString:=LOutString+ '     ' + StringReplace(LDisbenefit.WQDisbenefit.FData,',',' ',[rfReplaceAll]);
        LFileData.Add(LOutString);
      end;

      for LCount := 0 to LDisbenefitFileData.FMExtraLines.Count -1 do
      begin
        LFileData.Add(LDisbenefitFileData.FMExtraLines[LCount]);
      end;

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileDisbenefitAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
