{
  UNIT      : Contains TFile18Agent Class
  AUTHOR    : Maurice Marinus
  DATE      : 05/08/2006
  COPYRIGHT : Copyright © 2006 DWAF
}

unit UFile18Agent;

interface

uses
  Classes, sysutils, 

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UWetlandObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UReservoirObject,
  UBasicObjects;

type

  TFile18Agent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;


implementation

uses
  System.Contnrs, Math,
  UUtilities,
  VoaimsCom_TLB,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

function TFile18Agent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFile18Agent.ReadModelDataFromFile';
var
  LFileData             : TStringList;
  LMessage,
  LReadString,
  LTempString           : string;
  LReadInteger,
  LCount,
  LCount1,
  LLinesRead,
  LColNumber,
  LErrorCode            : Integer;
  LReadReal             : Double;

  LReservoir            : TReservoir;
  LWetland              : TWetland;
  LWetlandObject        : TWetlandObject;
  LFileLineTypesObject  : TAbstractFileLineTypesObject;
  LStop                 : Boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile18Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile18Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LWetlandObject                      := ADataObject.FWetlandObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not LWetlandObject.Initialise then
      Exit;

    LFileData := TStringList.Create;
    try
      //Read the F18 file
      LFileData.LoadFromFile(AFilename.FileName);
      if(LFileData.Count = 0) then
      begin
        Result := True;
        Exit;
      end;

      if(ADataObject.FChannelDescrObject.FWetlandCount.FData = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile19Agent.NoDataInF03File');
        AProgressFunction(LMessage,ptError,LStop);
        Exit;
      end;

      LLinesRead  := 0;
      LCount1     := 0;

      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      while (LLinesRead < LFileData.Count - 1) do
      begin
        if SameText(Trim(LFileData[LLinesRead]), '') then
          Exit;
        LWetland  := LWetlandObject.AddWetland;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        
        //Line 1
        LColNumber  := 0;
        LReadString := LFileData[LLinesRead];

        //Identifier
        LWetland.Identifier.FData       := LCount1;
        LWetland.Identifier.FInitalised := True;

        //Node Number
        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile18Agent.strNodeNumber');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LWetland.NodeNumber.FData       := LReadInteger;
          LWetland.NodeNumber.FInitalised := True;
        end;

        //Name
        Inc(LColNumber);
        LTempString := '';
        LTempString := ExtractDelemetedFirstSubstring('''',LReadString);
        if Length(LTempString) > 255 then
        begin
          LMessage := FAppModules.Language.GetString('TFile18Agent.strWetlandNameValErr');
          LMessage := Format(LMessage,[LLinesRead+1,LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LWetland.Name.FData        := LTempString;
          LWetland.Name.FLength      := Length(LWetland.Name.FData);
          LWetland.Name.FInitalised  := True;
        end;

        //Line 2
        LColNumber  := 0;
        Inc(LLinesRead);
        Inc(LColNumber);
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
        LReadString := LFileData[LLinesRead];


        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile18Agent.strUpstreamThreshold');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LWetland.UpstreamThreshold.FData       := LReadReal;
          LWetland.UpstreamThreshold.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile18Agent.strInflowProportion');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LWetland.InflowProportion.FData       := LReadReal;
          LWetland.InflowProportion.FInitalised := True;
        end;

        //Line 3
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
        LColNumber  := 0;
        Inc(LLinesRead);
        LReadString := LFileData[LLinesRead];

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile18Agent.strStorageVolume');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LWetland.StorageVolume.FData       := LReadReal;
          LWetland.StorageVolume.FInitalised := True;
        end;

        Inc(LColNumber);
        LTempString   := '';
        LTempString   := ExtractFirstSubstring(LReadString);
        Val(LTempString,LReadReal,LErrorCode);
        if(LErrorCode <> 0) then
        begin
          LMessage := FAppModules.Language.GetString('TFile18Agent.strOutflowProportion');
          LMessage := Format(LMessage,[LLinesRead+1, LColNumber, LColNumber+1]);
          AProgressFunction(LMessage,ptWarning,LStop);
          //if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LWetland.OutflowProportion.FData       := LReadReal;
          LWetland.OutflowProportion.FInitalised := True;
        end;

        Inc(LCount1);
        Inc(LLinesRead);
      end;

      for LCount := 0 to LWetlandObject.WetlandCount-1 do
      begin
        LWetland   := LWetlandObject.WetlandObjectByIndex[LCount];
        LReservoir := nil;
        for LCount1 := 0 to ADataObject.FReservoirObject.FReservoirs.Count-1 do
        begin
          if(TReservoir(ADataObject.FReservoirObject.FReservoirs[LCount1]).FNodeNo.FData = LWetland.NodeNumber.FData) then
          begin
            LReservoir := TReservoir(ADataObject.FReservoirObject.FReservoirs[LCount1]);
            Break;
          end;
        end;
        if(LReservoir <> nil) then
        begin
          LReservoir.FNodeType.FData := Integer(ntWetlandNode);
        end;
      end;

      Inc(LLinesRead);
      for LCount := LLinesRead to LFileData.Count - 1 do
        LWetlandObject.ExtraLines.Add(LFileData[LCount]);

      LMessage  := FAppModules.Language.GetString('TFile18Agent.strReadingCompleted');
      LMessage  := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result    := True;
    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile18Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile18Agent.WriteModelDataToFile';
var
  LMessage,
  LOutString,
  LTempString     : String;
  LLinesCount     : Integer;
  LStop           : Boolean;
  LF18File        : TStringlist;

  LWetland        : TWetland;
  LWetlandObject  : TWetlandObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile18Agent.strWritingStarted');
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

    LWetlandObject := ADataObject.FWetlandObject;
    if not Assigned(LWetlandObject) then
      Exit;

    if (LWetlandObject.WetlandCount = 0) and
       (LWetlandObject.ExtraLines.Count = 0) then
    begin
      if FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);
      Result := True;
      Exit;
    end;

    LF18File  := TStringList.Create;
    try

      for LLinesCount := 0 to LWetlandObject.WetlandCount-1 do
      begin
        LWetland := TWetland(LWetlandObject.WetlandObjectByIndex[LLinesCount]);
        //Line 1
        LOutString  := '';
        LTempString := PadInt(LWetland.NodeNumber);
        LOutString  := LOutString + LTempString;
        LTempString := LWetland.Name.FData;
        LOutString  := LOutString + ' ' + QuotedStr(LTempString);
        LF18File.Add(LOutString);

        //Line 2
        LOutString  := '';
        LTempString := PadDouble(LWetland.UpstreamThreshold);
        LOutString  := LOutString + LTempString;
        LTempString := PadDouble(LWetland.InflowProportion);
        LOutString  := LOutString + ' ' + Trim(LTempString);
        LF18File.Add(LOutString);

        //Line 3
        LOutString  := '';
        LTempString := PadDouble(LWetland.StorageVolume);
        LOutString  := LOutString + LTempString;
        LTempString := PadDouble(LWetland.OutflowProportion);
        LOutString  := LOutString + ' ' + Trim(LTempString);
        LF18File.Add(LOutString);
      end;

      for LLinesCount := 0 to LWetlandObject.ExtraLines.Count -1 do
        LF18File.Add(LWetlandObject.ExtraLines[LLinesCount]);

      LF18File.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      if(Trim(LF18File.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile18Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;
    finally
      LF18File.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
