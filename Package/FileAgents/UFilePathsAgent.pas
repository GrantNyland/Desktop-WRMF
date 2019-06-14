//
//
//  UNIT      : Contains TFilePathsAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFilePathsAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UPathsObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;
type

  TFilePathsAgent = class(TAbstractFileAgent)
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

function TFilePathsAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePathsAgent.ReadModelDataFromFile';
var
  LFileData: TStringList;
  LMessage,
  LReadString: String;
  LTempString: string;
  LCount: Integer;
  LPathsObject: TPathsObject;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePathsAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if(FAppModules.Model.ModelName = CYield) or (FAppModules.Model.ModelName = CPlanning) then
    begin
      if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
      begin
        LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
        LMessage := Format(LMessage,[AFilename.FileName]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;
    end;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFilePathsAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LPathsObject := ADataObject.FPathsObject;
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LPathsObject.Initialise then
    Exit;

    LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
    if not Assigned(LFileLineTypesObject) then
       Exit;

    TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;

    if(FAppModules.Model.ModelName = CYield) or (FAppModules.Model.ModelName = CPlanning) then
    begin
      if (TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath <> '') then
      begin
        LPathsObject.HydrologyPath.FData := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
        LPathsObject.HydrologyPath.FLength := Length(LPathsObject.HydrologyPath.FData);
        LPathsObject.HydrologyPath.FInitalised := True;
      end;
      if (TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath <> '') then
      begin
        LPathsObject.SpecifiedDemandPath.FData := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath;
        LPathsObject.SpecifiedDemandPath.FLength := Length(LPathsObject.SpecifiedDemandPath.FData);
        LPathsObject.SpecifiedDemandPath.FInitalised := True;
      end;
    end;

    LFileData := TStringList.Create;
    try
      //Read the WRYM.DAT file
      LFileData.LoadFromFile(AFilename.FileName);

      //Read Line 1
      LReadString := '';
      if (LFileData.Count > 0) then
        LReadString := LFileData[00];

      LTempString := Copy(LReadString,1,8);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
      if (Trim(LTempString) = '') then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsAgent.strNoPrefixErr');
        LMessage := Format(LMessage,[1,1,8]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        LPathsObject.FileNamePrefix.FData := LTempString;
        LPathsObject.FileNamePrefix.FInitalised := True;

        LTempString := Copy(LReadString,9,Length(LReadString));
        LPathsObject.FileNamePrefixComment.FData := LTempString;
        LPathsObject.FileNamePrefixComment.FLength := Length(LTempString);
        LPathsObject.FileNamePrefixComment.FInitalised := True;
      end;

      //Read Line 2
      LReadString := '';
      if (LFileData.Count > 1) then
      LReadString := LFileData[01];

      LTempString := Copy(LReadString,1,40);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(2,'2');
      if (Trim(LTempString) = '') then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsAgent.strNoInputFilesPathErr');
        LMessage := Format(LMessage,[2,1,40]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if not FilePathIsDosCompatible(FAppModules,Trim(LTempString)) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsAgent.strInputFilesPathDOSErr');
        LMessage := Format(LMessage,[2,Trim(LTempString)]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        if (not DirectoryExists(Trim(LTempString))) then
           ForceDirectories(Trim(LTempString));

        if (not DirectoryExists(Trim(LTempString))) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePathsAgent.strInputFilesPathNoExistErr');
          LMessage := Format(LMessage,[2,Trim(LTempString)]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPathsObject.InputFilesPath.FData := LTempString;
          LPathsObject.InputFilesPath.FInitalised := True;

          LTempString := Copy(LReadString,41,Length(LReadString));
          LPathsObject.InputFilesPathComment.FData := LTempString;
          LPathsObject.InputFilesPathComment.FLength := Length(LTempString);
          LPathsObject.InputFilesPathComment.FInitalised := True;
        end;
      end;
      //Read Line 3
      LReadString := '';
      if (LFileData.Count > 2) then
        LReadString := LFileData[02];

      LTempString := Copy(LReadString,1,40);
      TFileLineTypesObject(LFileLineTypesObject).AddLineType(3,'3');
      if (Trim(LTempString) = '') then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsAgent.strNoOutputFilesPathErr');
        LMessage := Format(LMessage,[2,1,40]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      if not FilePathIsDosCompatible(FAppModules,Trim(LTempString)) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsAgent.strNoOutputFilesPathDOSErr');
        LMessage := Format(LMessage,[2,Trim(LTempString)]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        if (not DirectoryExists(Trim(LTempString))) then
           ForceDirectories(Trim(LTempString));
        if (not DirectoryExists(Trim(LTempString))) then
        begin
          LMessage := FAppModules.Language.GetString('TFilePathsAgent.strOutputFilesPathNoExistErr');
          LMessage := Format(LMessage,[3,Trim(LTempString)]);
          AProgressFunction(LMessage,ptError,LStop);
          if FAppModules.GlobalData.StopOnFirstErr then Exit ;
        end
        else
        begin
          LPathsObject.OutputFilesPath.FData := LTempString;
          LPathsObject.OutputFilesPath.FInitalised := True;

          LTempString := Copy(LReadString,41,Length(LReadString));
          LPathsObject.OutputFilesPathComment.FData := LTempString;
          LPathsObject.OutputFilesPathComment.FLength := Length(LTempString);
          LPathsObject.OutputFilesPathComment.FInitalised := True;
        end;
      end;

      if (LFileData.Count > 3) then
      begin
        for LCount := 0 to 2 do
          LFileData.Delete(0);
        LPathsObject.Comment.Assign(LFileData);
      end;

      LMessage := FAppModules.Language.GetString('TFilePathsAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePathsAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePathsAgent.WriteModelDataToFile';
var
  LMessage : String;
  LOutString : String;
  LCount: Integer;
  LFileData :TStringlist;
  LPathsObject: TPathsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePathsAgent.strWritingStarted');
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

    LPathsObject := ADataObject.FPathsObject;
    if not Assigned(LPathsObject) then
      Exit;

    LFileData:= TStringList.Create;
    try
      if not LPathsObject.FileNamePrefix.FInitalised then
        LFileData.Add('')
      else
      begin
        LOutString := PadString(LPathsObject.FileNamePrefix) + PadString(LPathsObject.FileNamePrefixComment);
        LFileData.Add(LOutString);
      end;

      if not LPathsObject.InputFilesPath.FInitalised then
        LFileData.Add('')
      else
      begin
        LOutString := PadString(LPathsObject.InputFilesPath) + PadString(LPathsObject.InputFilesPathComment);
        LFileData.Add(LOutString);
      end;

      if not LPathsObject.OutputFilesPath.FInitalised then
        LFileData.Add('')
      else
      begin
        LOutString := PadString(LPathsObject.OutputFilesPath) + PadString(LPathsObject.OutputFilesPathComment);
        LFileData.Add(LOutString);
      end;

      for LCount := 0 to LPathsObject.Comment.Count -1 do
        LFileData.Add(LPathsObject.Comment[LCount]);

      if not DirectoryExists(LPathsObject.InputFilesPath.FData) then
        ForceDirectories(LPathsObject.InputFilesPath.FData);
      if not DirectoryExists(LPathsObject.OutputFilesPath.FData) then
        ForceDirectories(LPathsObject.OutputFilesPath.FData);

      LFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFilePathsAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      Result := True;

    finally
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

