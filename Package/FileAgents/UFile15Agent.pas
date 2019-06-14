//
//
//  UNIT      : Contains TFile15Agent Class
//  AUTHOR    : Presley Mudau
//  DATE      : 11/07/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFile15Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UCurtailmentAndDroughtFileObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile15Agent = class(TAbstractFileAgent)
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
     UErrorHandlingOperations, UAbstractModelData;

function TFile15Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName;
                                            ADataObject: TDataFileObjects;
                                            AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile15Agent.ReadModelDataFromFile';
var
  LFileData               : TStringList;
  LMessage                : string;
  LReadString             : string;
  LTempString             : string;
  LReadInteger            : integer;
  LCurtailedChannelsCount : integer;
  LDroughtRestrictionCount: integer;
  LCount                  : integer;
  LLinesRead              : integer;
  LErrorCode              : Integer;
  LIndex                  : integer;
  LIndexA                 : integer;
  LReadReal               : Double;
  LChannelNumberCount     : integer;
  LReservoirNumberCount   : integer;
  LCurtailmentAndDrought  : TCurtailmentAndDroughtFileObject;
  LCurtailment            : TCurtailmentFileObject;
  LCurtailedChannel       : TCurtailedChannelFileObject;
  LDroughtRestriction     : TDroughtRestrictionFileObject;
  LStop                   : boolean;
  LCommaTextData          : TStringList;
  LFileLineTypesObject    : TAbstractFileLineTypesObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile15Agent.strReadingStarted');
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
    if not FileExists(AFilename.FileName) then
    begin
      Result := True;
      Exit;
    end;

    LCurtailmentAndDrought := ADataObject.FCurtailmentAndDroughtFileObject;
    if not LCurtailmentAndDrought.Initialise then
      Exit;

    TFileNameObject(AFileName).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;
    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    LFileData       := TStringList.Create;
    LCommaTextData  := TStringList.Create;
    try
      //Read the F15.dat file
      LFileData.LoadFromFile(AFilename.FileName);

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');
      
      LLinesRead := 0;
      if (LFileData.Count > 0) then
      begin
        //LCurtailedChannelsCount  := 0;
        //LDroughtRestrictionCount := 0;

        //Read Line 1
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');

        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);

        if LReadInteger > 0 then
        begin
          LCurtailment := LCurtailmentAndDrought.AddCurtailment;
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile15Agent.strCurtailmentPeiordCountErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if(LReadInteger < 1) or (LReadInteger > 10) then
          begin
            LMessage := FAppModules.Language.GetString('TFile15Agent.strCurtailmentPeiordCountValueErr');
            LMessage := Format(LMessage,[1,4,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;

          LCurtailment.FCurtailmentPeriodCount.FData       := LReadInteger;
          LCurtailment.FCurtailmentPeriodCount.FInitalised := True;

          //Read Line 2
          LLinesRead := LLinesRead + 1;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
          for LIndex := 1 to LCurtailment.FCurtailmentPeriodCount.FData  do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile15Agent.strStartMonthErr');
              LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LCurtailment.FStartMonth[LIndex].FData       := LReadInteger;
              LCurtailment.FStartMonth[LIndex].FInitalised := True;
            end;
          end;

          //Read Line 3
          LLinesRead := LLinesRead + 1;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
          LTempString := ExtractFirstSubstring(LReadString);
          Val(LTempString,LCurtailedChannelsCount,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile15Agent.strCurtailedChannelCountErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          if(LCurtailedChannelsCount < 1) or (LCurtailedChannelsCount > 50) then
          begin
            LMessage := FAppModules.Language.GetString('TFile15Agent.strCurtailedChannelCountValueErr');
            LMessage := Format(LMessage,[LLinesRead+1,4,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end;

          //Read Line 4
          for LCount := 1 to LCurtailedChannelsCount do
          begin
            LLinesRead := LLinesRead + 1;
            if(LLinesRead >= LFileData.Count) then Break;

            LCurtailedChannel := LCurtailmentAndDrought.AddCurtailedChannel;
            TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
            LReadString := LFileData[LLinesRead];

            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile15Agent.strChannelNumberErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LCurtailedChannel.FChannelNumber.FData   := LReadInteger;
              LCurtailedChannel.FChannelNumber.FInitalised := True;
            end;

            for Lindex := 1 to LCurtailment.FCurtailmentPeriodCount.FData do
            begin
              LTempString := ExtractFirstSubstring(LReadString);
              Val(Trim(LTempString),LReadReal,LErrorCode);
              if(LErrorCode <> 0) then
              begin
                LMessage := FAppModules.Language.GetString('TFile15Agent.strAllocationFactorsErr');
                LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
                AProgressFunction(LMessage,ptError,LStop);
                if FAppModules.GlobalData.StopOnFirstErr then Exit ;
              end
              else
              begin
                LCurtailedChannel.FAllocationFactors[LIndex].FData  := LReadReal;
                LCurtailedChannel.FAllocationFactors[LIndex].FInitalised := True;
              end;
            end;
          end;
        end;

          //Read Line 5
        LLinesRead := LLinesRead + 1;

        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'5');
        LReadString := LFileData[LLinesRead];
        LTempString := ExtractFirstSubstring(LReadString);
        Val(LTempString,LDroughtRestrictionCount,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile15Agent.strDroughtRestrictionCountErr');
          LMessage := Format(LMessage,[LLinesRead+1,1,2]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        if(LDroughtRestrictionCount < 0) or (LDroughtRestrictionCount > 20) then
        begin
          LMessage := FAppModules.Language.GetString('TFile15Agent.strDroughtRestrictionCountValueErr');
          LMessage := Format(LMessage,[LLinesRead+1,4,5]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end;

        for LIndex := 1 to LDroughtRestrictionCount do
        begin
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          //Read Line 6
          LDroughtRestriction := LCurtailmentAndDrought.AddDroughtRestriction;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'6');
          LReadString := LFileData[LLinesRead];
          LChannelNumberCount := 0;
          
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile15Agent.strChannelNumberCountErr');
            LMessage := Format(LMessage,[LLinesRead+1,1,2]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LChannelNumberCount := LReadInteger;
          end;

          LReservoirNumberCount := 0;
          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TFile15Agent.strReservoirNumberCountErr');
            LMessage := Format(LMessage,[LLinesRead+1,4,5]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LReservoirNumberCount := LReadInteger;
          end;

          //Read Line 7
          LLinesRead := LLinesRead + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'7');
          LReadString := LFileData[LLinesRead];

          LCommaTextData.Clear;
          for LCount := 1 to LChannelNumberCount do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile15Agent.strChannelNumbersErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LCommaTextData.Add(IntToStr(LReadInteger));
            end;
          end;

          if(LCommaTextData.Count > 0) then
          begin
            LDroughtRestriction.FChannelNumbers.FData       := LCommaTextData.CommaText;
            LDroughtRestriction.FChannelNumbers.FLength     := Length(LTempString);
            LDroughtRestriction.FChannelNumbers.FInitalised := True;
          end;

          //Read Line 8
          LLinesRead := LLinesRead + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'8');
          LReadString := LFileData[LLinesRead];

          LCommaTextData.Clear;
          for LCount := 1 to LReservoirNumberCount do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadInteger,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile15Agent.strReservoirNumbersErr');
              LMessage := Format(LMessage,[LLinesRead+1,1,2]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LCommaTextData.Add(IntToStr(LReadInteger));
            end;
          end;

          if(LCommaTextData.Count > 0) then
          begin
              LDroughtRestriction.FReservoirNumbers.FData       := LCommaTextData.CommaText;
              LDroughtRestriction.FReservoirNumbers.FLength     := Length(LTempString);
              LDroughtRestriction.FReservoirNumbers.FInitalised := True;
          end;
          //Read Line 9
          LLinesRead := LLinesRead + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'9');
          LReadString := LFileData[LLinesRead];
          for LIndexA := 1 to 10 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile15Agent.strStorageVolumesErr');
              LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDroughtRestriction.FStorageVolumes[LIndexA].FData  := LReadReal;
              LDroughtRestriction.FStorageVolumes[LIndexA].FInitalised := True;
            end;
          end;

          //Read Line 10
          LLinesRead := LLinesRead + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'10');
          LReadString := LFileData[LLinesRead];
          for LIndexA := 1 to 10 do
          begin
            LTempString := ExtractFirstSubstring(LReadString);
            Val(Trim(LTempString),LReadReal,LErrorCode);
            if(LErrorCode <> 0) then
            begin
              LMessage := FAppModules.Language.GetString('TFile15Agent.strAllocationFactorsErr');
              LMessage := Format(LMessage,[LLinesRead+1,LIndex*4-3,LIndex*4]);
              AProgressFunction(LMessage,ptError,LStop);
              if FAppModules.GlobalData.StopOnFirstErr then Exit ;
            end
            else
            begin
              LDroughtRestriction.FAllocationFactors[LIndexA].FData  := LReadReal;
              LDroughtRestriction.FAllocationFactors[LIndexA].FInitalised := True;
            end;
          end;
        end;
      end;
      Result := True;
    finally
      LFileData.Free;
      LCommaTextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile15Agent.WriteModelDataToFile';
var
  LCurtailmentIndex       : integer;
  LCurtailedChannelIndex  : integer;
  LDroughtRestrictionIndex: integer;
  LMessage                : string;
  LTempStr                : string;
  LOutString              : string;
  LCount                  : integer;
  LIndex                  : Integer;
  LCurtailedChannelCount  : integer;
  LFileData               : TStringlist;
  LCurtailment            : TCurtailmentFileObject;
  LCurtailedChannel       : TCurtailedChannelFileObject;
  LDroughtRestriction     : TDroughtRestrictionFileObject;
  LCurtailementAndDrought : TCurtailmentAndDroughtFileObject;
  LStop                   : boolean;
  LChannelCountStr        : TStringList;
  LChannelCount           : integer;
  LReservoirCount         : integer;
  LReservoirCountStr      : TStringList;
  LImplementCurtailmentFile : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile15Agent.strWritingStarted');
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

    {if not TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.CurtailmentAndDrought.ImplementCurtailmentFile then
    begin
      if FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      Result := True;
      Exit;
    end;
     }
    LChannelCount   := 0;
    LReservoirCount := 0;
    LImplementCurtailmentFile := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.CurtailmentAndDrought.ImplementCurtailmentFile;

    LCurtailementAndDrought := ADataObject.FCurtailmentAndDroughtFileObject;
    if not Assigned(LCurtailementAndDrought) then
      Exit;

    if (LCurtailementAndDrought.CurtailedChannelCount = 0) and
       (LCurtailementAndDrought.CurtailmentCount = 0) and
       (LCurtailementAndDrought.DroughtRestrictionCount = 0) and
       (LCurtailementAndDrought.Comment.Count = 0) then
    begin
      if FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);
      Result := True;
      Exit;
    end;

    LFileData:= TStringList.Create;
    try
      if (LImplementCurtailmentFile) then
      begin
        for LCount := 1 to LCurtailementAndDrought.CurtailmentCount do
        begin
          LCurtailment := LCurtailementAndDrought.CurtailmentByIndex[LCount-1];

          if Assigned(LCurtailment) then
          begin
             //Write Line 1
              LOutString := IntToStr(LCurtailment.FCurtailmentPeriodCount.FData);
              LFileData.Add(LOutString);

            if (LCurtailementAndDrought.CurtailmentCount > 0)  then
            begin
              LOutString := '';
              for LCurtailmentIndex := 1 to LCurtailment.FCurtailmentPeriodCount.FData do
              begin
                //Write Line 2
                LTempStr   := '';
                LCurtailment.FStartMonth[LCurtailmentIndex].FLength := 10;
                LTempStr:= Trim(PadInt(LCurtailment.FStartMonth[LCurtailmentIndex]));
                LOutString := LOutString + ' ' + LTempStr;
              end;
              LFileData.Add(LOutString);

              //Write Line 3
              LOutString := '';
              LOutString := IntToStr(LCurtailementAndDrought.CurtailedChannelCount);
              LFileData.Add(LOutString);

              //write line 4
              for LCurtailedChannelIndex := 1 to LCurtailementAndDrought.CurtailedChannelCount do
              begin
                LOutString := '';
                LCurtailedChannel := LCurtailementAndDrought.CurtailedChannelByIndex[LCurtailedChannelIndex-1];
                if Assigned(LCurtailedChannel) then
                begin
                  LOutString := IntToStr(LCurtailedChannel.FChannelNumber.FData);
                  for LCurtailedChannelCount := 1 to LCurtailment.FCurtailmentPeriodCount.FData do
                  begin
                    LTempStr   := '';
                    //LCurtailedChannel.FAllocationFactors[LCurtailedChannelCount].FLength := 2;
                    LTempStr:= PadDouble(LCurtailedChannel.FAllocationFactors[LCurtailedChannelCount]);
                    LOutString := LOutString + ' ' + LTempStr;
                  end;
                end;
                LFileData.Add(LOutString);
              end;
            end;
          end;
        end;
      end
      else
        //for LCount := 1 to 4 do
          LFileData.Add('0');

      //Write Line 5
      LOutString := '';
      LOutString := IntToStr(LCurtailementAndDrought.DroughtRestrictionCount);
      LFileData.Add(LOutString);

      for LDroughtRestrictionIndex := 0 to LCurtailementAndDrought.DroughtRestrictionCount-1 do
      begin
        LDroughtRestriction := LCurtailementAndDrought.DroughtRestrictionByIndex[LDroughtRestrictionIndex];

         LChannelCountStr   := TStringList.Create;
         LReservoirCountStr := TStringList.Create;
         try
           if LDroughtRestriction.FChannelNumbers.FInitalised then
           begin
             LChannelCountStr.CommaText := LDroughtRestriction.FChannelNumbers.FData;
             LChannelCount := LChannelCountStr.Count;
           end;

           if LDroughtRestriction.FReservoirNumbers.FInitalised then
           begin
             LReservoirCountStr.CommaText := LDroughtRestriction.FReservoirNumbers.FData;
             LReservoirCount := LReservoirCountStr.Count;
           end;

         finally
           LChannelCountStr.Free;
           LReservoirCountStr.Free;
         end;

        //Write Line 6
        LOutString := '';
        LOutString := IntToStr(LChannelCount);
        LTempStr   := IntToStr(LReservoirCount);
        LOutString := LOutString + ' ' + LTempStr;
        LFileData.Add(LOutString);

        //Write Line 7
        LOutString:='';
        LOutString:=LOutString + ' ' + StringReplace(LDroughtRestriction.FChannelNumbers.FData,',',' ',[rfReplaceAll]);
        LFileData.Add(LOutString);

        //Write Line 8
        LOutString:='';
        LOutString:=LOutString + ' ' + StringReplace(LDroughtRestriction.FReservoirNumbers.FData,',',' ',[rfReplaceAll]);
        LFileData.Add(LOutString);

        //Write Line 9
        LOutString := '';
        for LIndex := 1 to 10 do
        begin
          LTempStr   := '';
          //LDroughtRestriction.FStorageVolumes[LIndex].FLength := 4;
          LTempStr   := Trim(PadDouble(LDroughtRestriction.FStorageVolumes[LIndex]));
          LOutString := LOutString + ' '+ LTempStr;
        end;
        LFileData.Add(LOutString);

        //Write Line 10
        LOutString := '';
        for LIndex := 1 to 10 do
        begin
          LTempStr   := '';
          //LDroughtRestriction.FAllocationFactors[LIndex].FLength := 4;
          LTempStr   := Trim(PadDouble(LDroughtRestriction.FAllocationFactors[LIndex]));
          LOutString := LOutString + ' '+ LTempStr;
        end;
        LFileData.Add(LOutString);
      end;

      //Write Line 11 onwards
      for LIndex := 0 to LCurtailementAndDrought.Comment.Count-1 do
        LFileData.Add(LCurtailementAndDrought.Comment[LIndex]);

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      if(Trim(LFileData.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile15Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

