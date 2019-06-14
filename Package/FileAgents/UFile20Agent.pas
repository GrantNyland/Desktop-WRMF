//
//
//  UNIT      : Contains TFile20Agent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 22/11/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFile20Agent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  USFRFileObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFile20Agent = class(TAbstractFileAgent)
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

function TFile20Agent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile20Agent.ReadModelDataFromFile';
      LSingleQuote = '''';
      LDoubleQuote = '"';
var
  LFileData: TStringList;
  LMessage,
  LReadString: String;
  LTempString: string;
  LReadReal : Double;
  LSFRCount: integer;
  LLinesRead,
  LErrorCode,
  LReadInteger,
  LCount: Integer;
  LSFRObject: TSFRObject;
  LSFRFileObject:TSFRFileObject;
  LFileLineTypesObject : TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile20Agent.strReadingStarted');
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
      LMessage := FAppModules.Language.GetString('TFile20Agent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LSFRFileObject := ADataObject.FSFRFileObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);
    LFileLineTypesObject                := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];

    if not LSFRFileObject.Initialise then
    Exit;

    LFileData := TStringList.Create;
    try
      //Read the F20.dat file
      LFileData.LoadFromFile(AFilename.FileName);

      //Read Line 1
      LLinesRead := 0;
      //Add this dummy for line 0
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'0');

      if (LFileData.Count > 0) then
      begin
        LSFRCount  := 0;
        LReadString := LFileData[LLinesRead];
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'1');
        LTempString := ExtractFirstSubstring(LReadString);
        Val(Trim(LTempString),LReadInteger,LErrorCode);
        if(LErrorCode <> 0) then
        begin
           LMessage := FAppModules.Language.GetString('TFile20Agent.strSFRCountErr');
           LMessage := Format(LMessage,[LLinesRead+1,1,2]);
           AProgressFunction(LMessage,ptError,LStop);
           if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LSFRCount := LReadInteger;
        end;

        for LCount := 1 to  LSFRCount do
        begin
          //Read Line 2
          LLinesRead := LLinesRead + 1;
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'2');
          if(LLinesRead >= LFileData.Count) then Break;

          LSFRObject := LSFRFileObject.AddSFRObject;
          LReadString := LFileData[LLinesRead];

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile20Agent.strSFRInflowNodeNumberErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSFRObject.InflowNodeNumber.FData       := LReadInteger;
            LSFRObject.InflowNodeNumber.FInitalised := True;
          end;

          LTempString := ExtractFirstSubstring(LReadString);
          Val(Trim(LTempString),LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
             LMessage := FAppModules.Language.GetString('TFile20Agent.strSFRCoveredAreaErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSFRObject.CoveredArea.FData       := LReadReal;
            LSFRObject.CoveredArea.FInitalised := True;
          end;

          LReadString  := Trim(LReadString);
          if(Pos(LSingleQuote,LReadString) = 1) then
            LTempString := Trim(ExtractDelemetedFirstSubstring(LSingleQuote,LReadString))
          else
          if(Pos(LDoubleQuote,LReadString) = 1) then
            LTempString := Trim(ExtractDelemetedFirstSubstring(LDoubleQuote,LReadString))
          else
            LTempString := ExtractFirstSubstring(LReadString);

          if(LTempString <> '') then
          begin
            LSFRObject.SFRName.FData       := LTempString;
            LSFRObject.SFRName.FLength     := Length(LTempString);
            LSFRObject.SFRName.FInitalised := True;

            LSFRObject.SFRDescr.FData       := LTempString;
            LSFRObject.SFRDescr.FLength     := Length(LTempString);
            LSFRObject.SFRDescr.FInitalised := True;
          end;

          if(Trim(LReadString) <> '') then
          begin
            LTempString                     := Trim(LReadString);
            LSFRObject.Comment1.FData       := LTempString;
            LSFRObject.Comment1.FLength     := Length(LTempString);
            LSFRObject.Comment1.FInitalised := True;
          end;

          //Read Line 3
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'3');
          LReadString  := Trim(LReadString);
          if(Pos(LSingleQuote,LReadString) = 1) then
            LTempString := Trim(ExtractDelemetedFirstSubstring(LSingleQuote,LReadString))
          else
          if(Pos(LDoubleQuote,LReadString) = 1) then
            LTempString := Trim(ExtractDelemetedFirstSubstring(LDoubleQuote,LReadString))
          else
            LTempString := ExtractFirstSubstring(LReadString);
          if(LTempString = '') then
          begin
             LMessage := FAppModules.Language.GetString('TFile20Agent.strSFRUnitRunoffFileNameErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSFRObject.UnitRunoffFileName.FData       := LTempString;
            LSFRObject.UnitRunoffFileName.FLength     := Length(LTempString);
            LSFRObject.UnitRunoffFileName.FInitalised := True;

            if(Trim(LReadString) <> '') then
            begin
              LTempString                     := Trim(LReadString);
              LSFRObject.Comment2.FData       := LTempString;
              LSFRObject.Comment2.FLength     := Length(LTempString);
              LSFRObject.Comment2.FInitalised := True;
            end;
          end;

          //Read Line 4
          LLinesRead := LLinesRead + 1;
          if(LLinesRead >= LFileData.Count) then Break;

          LReadString := LFileData[LLinesRead];
          TFileLineTypesObject(LFileLineTypesObject).AddLineType(LFileLineTypesObject.LinesCount,'4');
          LReadString  := Trim(LReadString);
          if(Pos(LSingleQuote,LReadString) = 1) then
            LTempString := Trim(ExtractDelemetedFirstSubstring(LSingleQuote,LReadString))
          else
          if(Pos(LDoubleQuote,LReadString) = 1) then
            LTempString := Trim(ExtractDelemetedFirstSubstring(LDoubleQuote,LReadString))
          else
            LTempString := ExtractFirstSubstring(LReadString);
          if(LTempString = '') then
          begin
             LMessage := FAppModules.Language.GetString('TFile20Agent.strSFRSoilMoistureFileNameErr');
             LMessage := Format(LMessage,[LLinesRead+1,1,2]);
             AProgressFunction(LMessage,ptError,LStop);
             if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            LSFRObject.SoilMoistureFileName.FData       := LTempString;
            LSFRObject.SoilMoistureFileName.FLength     := Length(LTempString);;
            LSFRObject.SoilMoistureFileName.FInitalised := True;


            if(Trim(LReadString) <> '') then
            begin
              LTempString                     := Trim(LReadString);
              LSFRObject.Comment3.FData       := LTempString;
              LSFRObject.Comment3.FLength     := Length(LTempString);
              LSFRObject.Comment3.FInitalised := True;
            end;
          end;
        end;

        //Read Line  5 onwards
        for LCount := LLinesRead+1 to  LFileData.Count-1 do
          LSFRFileObject.Comment.Add(LFileData[LCount]);
      end;


      LMessage := FAppModules.Language.GetString('TFile20Agent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile20Agent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile20Agent.WriteModelDataToFile';
var
  LMessage : String;
  LTempStr1,
  LTempStr2,
  LOutString : String;
  LCount: Integer;
  LFileData :TStringlist;
  LSFRObject: TSFRObject;
  LSFRFileObject:TSFRFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile20Agent.strWritingStarted');
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

    LSFRFileObject := ADataObject.FSFRFileObject;
    if not Assigned(LSFRFileObject) then
      Exit;

    LFileData:= TStringList.Create;
    try
      if(LSFRFileObject.SFRCount > 0) then
      begin
        //Write Line 1
        LOutString := IntToStr(LSFRFileObject.SFRCount);
        LFileData.Add(LOutString);

        for LCount := 0 to LSFRFileObject.SFRCount-1 do
        begin
          //Write Line 2
          LSFRObject := LSFRFileObject.SFRObjectByIndex[LCount];


          LTempStr1   := IntToStr(LSFRObject.InflowNodeNumber.FData);
          LTempStr2   := FormatFloat('##0.000',LSFRObject.CoveredArea.FData);
          LOutString  := LTempStr1 + ' '+ LTempStr2;
          //if(Pos(' ',LSFRObject.SFRName.FData) > 0) then
            LOutString  := LOutString + ' '+ QuotedStr(LSFRObject.SFRName.FData);
          //else
          //  LOutString  := LOutString + ' '+ LSFRObject.SFRName.FData;
          if(Trim(LSFRObject.Comment1.FData) <> '') then
            LOutString  := LOutString + '    '+ LSFRObject.Comment1.FData;
          LFileData.Add(LOutString);

          //Write Line 3
          //if(Pos(' ',LSFRObject.UnitRunoffFileName.FData) > 0) then
            LOutString  := QuotedStr(LSFRObject.UnitRunoffFileName.FData);
          //else
          //  LOutString  := LSFRObject.UnitRunoffFileName.FData;
          if(Trim(LSFRObject.Comment2.FData) <> '') and (LOutString <> '') then
            LOutString  := LOutString + '    '+ LSFRObject.Comment2.FData;
          LFileData.Add(LOutString);

          //Write Line 4
          //if(Pos(' ',LSFRObject.SoilMoistureFileName.FData) > 0) then
            LOutString  := QuotedStr(LSFRObject.SoilMoistureFileName.FData);
          //else
          //  LOutString  := LSFRObject.SoilMoistureFileName.FData;
          if(Trim(LSFRObject.Comment3.FData) <> '') then
            LOutString  := LOutString + '    '+ LSFRObject.Comment3.FData;
          LFileData.Add(LOutString);
        end;

        //Write Line 5 onwards
        for LCount := 0 to LSFRFileObject.Comment.Count-1 do
          LFileData.Add(LSFRFileObject.Comment[LCount]);

        LFileData.SaveToFile(AFilename.FileName);
        SetFileDate(AFileName);
      end;

      if(Trim(LFileData.Text) = '')  and FileExists(AFilename.FileName) then
        DeleteFile(AFilename.FileName);

      LMessage := FAppModules.Language.GetString('TFile20Agent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

