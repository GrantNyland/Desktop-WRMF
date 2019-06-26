//
//
//  UNIT      : Contains TRunYieldModelAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 17/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit URunYieldModelAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  VoaimsCom_TLB,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractModelData;

type
  TRunVer7YieldModel = procedure (AServer: IYieldModel); stdcall;

  TRunYieldModelAgent = class(TAbstractAppObject)
  protected
    function CopyModelApplication(AProgressFunction: TProgressUpdateFuntion;var AAppFileName: string): boolean; virtual;
    procedure GenerateSecurityKey; virtual;

    function CompileSecurityStringKey(ACurrentDateTime: TDateTime;  AModelVersionString: string): string; virtual;
    function ExtractVersionField(var AModelVersionString: string): integer; virtual;
    procedure GenerateModelSecurityKeyFile(ALockFileLocation, AModelVersionString: string); virtual;
    function RunCommModel(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion;AWindowHandle: THandle): boolean; virtual;
    procedure ChangeShortcutProperties(AShortCutFileName: String; ACloseOnExit: Boolean);
  public
    { Public declarations }
    function CheckInputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function RunModel(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion;AWindowHandle: THandle): boolean; virtual;
    function CheckOutputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ModelData: TAbstractModelData; virtual;
  end;

implementation

uses
  vcl.Forms,
  Windows,
  Contnrs,
  DateUtils,
  UUtilities,
  UDosFunctions,
  UDLLOperations,
  UMainMenuEventType,
  UAbstractComponent,
  UYieldModelServerAgent,
  UFileNameConstants,
  UYieldModelDataObject,
  UErrorHandlingOperations;


function TRunYieldModelAgent.CheckInputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TRunYieldModelAgent.CheckInputFiles';
var
  LCount: integer;
  LFileName: string;
  LFileList: TStringList;
  LMessage : string;
  LStop: boolean;
begin
  Result := False;
  try

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

//    if not Assigned(ADataObject) then
 //     raise Exception.Create('Data object parameter is not yet assigned.');

    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCheckInputFilesStart');
    AProgressFunction(LMessage,ptNone,LStop);

    if Assigned(ADataFileObjects.FPathsObject)then
    begin
      if(ADataFileObjects.FPathsObject.OutputFilesPath.FInitalised) then
      begin
        LFileName := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
        if(LFileName = '') then
        begin
          LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strWrymDataFileNotLoaded');
          AProgressFunction(LMessage,ptNone,LStop);
        end
        else
        begin
          LFileList := TStringList.Create;
          try
            LFileName := IncludeTrailingPathDelimiter(LFileName);
            if not DirectoryExists(LFileName) then
              ForceDirectories(LFileName);

            LFileName := LFileName + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData) + '*.OUT';
            LFileList.Clear;
            if(UUtilities.SearchFiles(LFileName,LFileList)) then
            begin
              for LCount := 0 to LFileList.Count - 1 do
              begin
                SysUtils.DeleteFile(Trim(LFileList[LCount]));
              end;
            end;
            Result := True;
          finally
            FreeAndNil(LFileList);
          end;
        end;
      end;
    end;

    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCheckInputFilesEnd');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.CheckOutputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TRunYieldModelAgent.CheckOutputFiles';
var
  LCount: integer;
  LFileName: string;
  LFileList: TStringList;
  LMessage : string;
  LFileNameObject: TAbstractModelFileName;
  LOutputFileType:TOutputFileType;
  LStop: boolean;
begin
  Result := False;
  try

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

   // if not Assigned(ADataObject) then
   //   raise Exception.Create('Data object parameter is not yet assigned.');

    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCheckOutputFilesStart');
    AProgressFunction(LMessage,ptNone,LStop);

    if Assigned(ADataFileObjects.FPathsObject)then
    begin
      if(ADataFileObjects.FPathsObject.OutputFilesPath.FInitalised) then
      begin
        LFileName := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
        if(LFileName = '') then
        begin
          LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strWrymDataFileNotLoaded');
          AProgressFunction(LMessage,ptNone,LStop);
        end
        else
        begin
          LFileList := TStringList.Create;
          try
            LFileName := IncludeTrailingPathDelimiter(LFileName);
            LFileName := LFileName + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData) + '*.OUT';
            LFileList.Clear;
            if(UUtilities.SearchFiles(LFileName,LFileList)) then
            begin
              //Update new file created.
              for LCount := 0 to LFileList.Count - 1 do
              begin
                LOutputFileType := GetOutputFileType(LFileList[LCount]);
                if (LOutputFileType <> oftNone) then
                begin
                  LFileNameObject := ModelData.FileNamesObject.OutputFileNames.FindFile(LFileList[LCount]);
                  if  Assigned(LFileNameObject) and
                     (not LFileNameObject.SavedInDB) and
                     LFileNameObject.FileDateHasChanged(LFileList[LCount]) then
                  begin
                    TModelFileNames(ModelData.FileNamesObject).UpdateOutputFileName(LOutputFileType,LFileList[LCount],
                                        False,0.0,FileLastWriteDate(LFileList[LCount]));
                  end;
                end;
              end;
            end;
          finally
            FreeAndNil(LFileList);
          end;
        end;
      end;
    end;
    Result := True;
    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCheckOutputFilesEnd');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.CopyModelApplication( AProgressFunction: TProgressUpdateFuntion;var AAppFileName: string): boolean;
const OPNAME = 'TRunYieldModelAgent.CopyModelApplication';
var
  LDefaultShortCutName,
  LCopyShortCutToPath,
  LCopyFile,
  LErrFilename: string;
  LDataFilesPath: string;
  LMessage : string;
  LStop: boolean;
begin
  Result := False;
  AAppFileName := '';
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCopyFilesStart');
    AProgressFunction(LMessage,ptNone,LStop);

    LCopyFile := ExtractFilePath(ApplicationExeName);
    LCopyShortCutToPath := IncludeTrailingPathDelimiter(GetWindowsDir) + '_default.pif';
    if(LCopyFile <> '') then
    begin
      LCopyFile := IncludeTrailingPathDelimiter(LCopyFile) + 'Dos\';
      LDefaultShortCutName := LCopyFile + FAppModules.Language.GetString('YieldModelFileAgent.DefaultShortCutFile');
      LErrFilename         := LCopyFile + FAppModules.Language.GetString('YieldModelFileAgent.Lf90File');
      if(FAppModules.StudyArea.ModelVersion = '6.2') then
        LCopyFile := LCopyFile +FAppModules.Language.GetString('YieldModelFileAgent.WRYM-62File')
      else
      if(FAppModules.StudyArea.ModelVersion = '6.1') then
        LCopyFile := LCopyFile + FAppModules.Language.GetString('YieldModelFileAgent.WRYM-61File')
      else
        LCopyFile := LCopyFile + FAppModules.Language.GetString('YieldModelFileAgent.WRYM-6File');
      LDataFilesPath := ExtractFilePath(ModelData.FileNamesObject.DirectoryFileNames.FileNameObject[0].FilePath);
      if not DirectoryExists(LDataFilesPath) then
        ForceDirectories(LDataFilesPath);
      if not FileExists(LErrFilename) then
      begin
        LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strErrFilesNotFound');
        AProgressFunction(LMessage,ptNone,LStop);
      end
      else
      begin
        if not FileExists(LCopyFile) then
        begin
          LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strAppFilesNotFound');
          AProgressFunction(LMessage,ptNone,LStop);
        end
        else
        begin
          if not DirectoryExists(LDataFilesPath) then
          begin
            LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strDataDirectoryNotFound');
            AProgressFunction(LMessage,ptNone,LStop);
          end
          else
          begin
            CopyFile(PChar(LDefaultShortCutName),PChar(LCopyShortCutToPath),false);
            AAppFileName := LDataFilesPath + ExtractFilename(LErrFilename);
            CopyFile(PChar(LErrFilename),PChar(AAppFileName),False);
            AAppFileName := LDataFilesPath + ExtractFilename(LCopyFile);
            CopyFile(PChar(LCopyFile),PChar(AAppFileName),False);
            Result := True;
          end;
        end;
      end;
    end;
    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCopyFilesComplete');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.RunCommModel(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion;
                                          AWindowHandle: THandle): boolean;
const OPNAME = 'TRunYieldModelAgent.RunCommModel';
var
  //LAgent: TYieldModelServerAgent;
  //LRunModelFunc : TRunVer7YieldModel;
  LMessage,
  LPath,
  LDllName: string;
  LStop   : boolean;
  //LDLLHandle: longword;
  LYieldModel : IYieldModel;
  //LScenarioLocked : boolean;
  LPathsFile : string;
begin
  Result := False;
  try
    LYieldModel := ((FAppModules.Model) as IYieldModel);
    LYieldModel.WRYMRunOptions.FirmYield := NullFloat;
    LYieldModel.WRYMRunOptions.SumOutBlobAddress := 0;
    LYieldModel.WRYMRunOptions.BlobSize := 0;

    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strRunStart');
    AProgressFunction(LMessage,ptNone,LStop);
    //LPath    := IncludeTrailingPathDelimiter(ExtractFilePath(ApplicationExeName))+ 'bin\';
    //LDllName := LPath + 'WRYM_761_Wrapper.dll';
    LPath    := IncludeTrailingPathDelimiter(ExtractFilePath(ApplicationExeName));
    LDllName := LPath + 'WRYM\WRYM_4.exe';
    if not FileExists(LDllName) then
    begin
      LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strDLLNotExist');
      LMessage := Format(LMessage,[LDllName]);
      AProgressFunction(LMessage,ptError,LStop);
    end
    else
    begin
      LPathsFile := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DirectoryFileName;
      if not FileExists(LPathsFile) then
      begin
        LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strDLLNotExist');
        LMessage := Format(LMessage,[LDllName]);
        AProgressFunction(LMessage,ptError,LStop);
      end
      else
      begin
        ExecAndWait(LDllName,'/o'+LPathsFile);
      end;
      {LDLLHandle := 0;
      LoadDLL(LDllName,LDLLHandle,True,OPNAME);
      if (LDLLHandle <> 0) then
      begin
        try
          if GetDLLFunction(LDLLHandle,LDllName,'RunYieldModel',@LRunModelFunc,LPath) then
          begin
            LYieldModel.YieldModelData.OutputData.SummaryOutputData.SumOutBlob.BlobLoaded := False;
            LRunModelFunc(LYieldModel);
            if LYieldModel.YieldModelData.OutputData.SummaryOutputData.SumOutBlob.BlobLoaded then
            begin
              if LYieldModel.WRYMRunOptions.SaveOutputToDB then
                 LYieldModel.YieldModelData.OutputData.SummaryOutputData.SumOutBlob.SaveBlobToDB;
              if LYieldModel.WRYMRunOptions.CreateSumOutFile then
                 LYieldModel.YieldModelData.OutputData.SummaryOutputData.SumOutBlob.WriteSumOutFile;
              if LYieldModel.WRYMRunOptions.SaveOutputAsBinaryFile then
                 LYieldModel.YieldModelData.OutputData.SummaryOutputData.SumOutBlob.WriteBlobToFile;
            end;
          end;
        finally
          LYieldModel.YieldModelIterationTracker.SimulationInProgress := False;
          FreeLibrary(LDLLHandle);
        end;
      end;}
    end;
    LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strRunCompleted');
    AProgressFunction(LMessage,ptNone,LStop);
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.RunModel(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion;AWindowHandle: THandle): boolean;
const OPNAME = 'TRunYieldModelAgent.RunModel';
var
  LMessage,
  LAppShortCut,
  LAppDirectory,
  LAppName: string;
  LStop: boolean;
  LOptions:   IWRYMRunOptions;

begin
  Result := False;
  try
    if(FAppModules.StudyArea.ModelVersion = '7' ) then
    begin
      Result := RunCommModel(ADataFileObjects,AProgressFunction,AWindowHandle);
    end
    else
    begin
      if not Assigned(AProgressFunction) then
        AProgressFunction := DummyShowProgress;

      if (AWindowHandle = 0) then
        raise Exception.Create('Window handle parameter is not yet assigned.');

      LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strRunStart');
      AProgressFunction(LMessage,ptNone,LStop);

      Result := CopyModelApplication(AProgressFunction,LAppName);
      if Result then
      begin
        LAppDirectory := ExtractFilePath(LAppName);
        LAppDirectory := IncludeTrailingPathDelimiter(LAppDirectory);
        LAppShortCut := CreateShortcut(LAppName,'',LAppDirectory);
        if (LAppShortCut = '') then
        begin
          LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strCreateShortCutFailed');
          AProgressFunction(LMessage,ptNone,LStop);
        end
        else
        begin
          LOptions := (FAppModules.Model as IYieldModel).WRYMRunOptions;
          ChangeShortcutProperties(LAppShortCut, LOptions.CloseOnComplete);
          LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strWaitModelRunning');
          AProgressFunction(LMessage,ptNone,LStop);
          if(FAppModules.StudyArea.ModelVersion = '6.2') then
          begin
            GenerateSecurityKey;
            Result := RunDosApplication(AWindowHandle,LAppName,True);
          end
          else
          begin
            Result := Result and RunDosApplication(AWindowHandle,LAppName,False);
            if not Result then
            begin
              LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strModelRunningFailed');
              AProgressFunction(LMessage,ptNone,LStop);
            end;
          end;
        end;
      end;

      LMessage := FAppModules.Language.GetString('TRunYieldModelAgent.strRunCompleted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.ModelData: TAbstractModelData;
const OPNAME = 'TRunYieldModelAgent.ModelData';
begin
  Result := Nil;
  try
    Result := TAbstractModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunYieldModelAgent.GenerateSecurityKey;
const OPNAME = 'TRunYieldModelAgent.GenerateSecurityKey';
var
  LLockFileLocation: string;
begin
  try
    if(FAppModules.StudyArea.ModelVersion = '6.2') then
    begin
     LLockFileLocation := ExtractFilePath(ModelData.FileNamesObject.DirectoryFileNames.FileNameObject[0].FilePath);
     GenerateModelSecurityKeyFile(LLockFileLocation, '4.1.11');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.ExtractVersionField(var AModelVersionString: string): integer;
const OPNAME = 'TRunYieldModelAgent.ExtractVersionField';
var
  LPosOfPoint: integer;
  LVersionNumberAsString: string;
begin
  Result := 0;
  try
    LPosOfPoint := Pos('.', AModelVersionString);
    if (LPosOfPoint > 0) then
    begin
      LVersionNumberAsString := Copy(AModelVersionString, 1, LPosOfPoint - 1);
    end else begin
      LVersionNumberAsString := AModelVersionString;
    end;
    AModelVersionString := Copy(AModelVersionString, LPosOfPoint + 1, Length(AModelVersionString));
    try
      Result := StrToInt(LVersionNumberAsString);
    except
      Result := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunYieldModelAgent.CompileSecurityStringKey(ACurrentDateTime: TDateTime; AModelVersionString: string): string;
const OPNAME = 'TRunYieldModelAgent.CompileSecurityStringKey';
var
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond: Word;
  LMajorVersion, LMinorVersion, LBuildNumber: integer;
  LCheckSum: integer;
begin
  try

    // Decode the date and time information.
    DecodeDate(ACurrentDateTime, LYear, LMonth, LDay);
    DecodeTime(ACurrentDateTime, LHour, LMinute, LSecond, LMilliSecond);

    // Decode the version information.
    LMajorVersion := ExtractVersionField(AModelVersionString);
    LMinorVersion := ExtractVersionField(AModelVersionString);
    LBuildNumber  := ExtractVersionField(AModelVersionString);

    // Scale all values.
    LSecond       := LSecond       *  2;
    LMajorVersion := LMajorVersion * 16;
    LMinute       := LMinute       *  4;
    LMinorVersion := LMinorVersion *  8;
    LHour         := LHour         *  6;
    LBuildNumber  := LBuildNumber  *  3;
    LDay          := LDay          *  5;
    LCheckSum := LSecond + LMajorVersion + LMinute + LMinorVersion + LHour + LBuildNumber + LDay;

    // Compile the string.
    Result :=
      Format('%4d', [LSecond]) +
      Format('%4d', [LMajorVersion]) +
      Format('%4d', [LMinute]) +
      Format('%4d', [LMinorVersion]) +
      Format('%4d', [LHour]) +
      Format('%4d', [LBuildNumber]) +
      Format('%4d', [LDay]) +
      Format('%6d', [LCheckSum]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunYieldModelAgent.GenerateModelSecurityKeyFile(ALockFileLocation, AModelVersionString: string);
const OPNAME = 'TRunYieldModelAgent.GenerateModelSecurityKeyFile';
var
  LRunLockFile: TStringList;
  LMiliseconds : integer;
  LDateTime: TDateTime;
begin
  try
    LRunLockFile := TStringList.Create;
    try
      LDateTime    := Now;
      LMiliseconds := FAppModules.IniFile.ReadInteger(ClassName,'Miliseconds',0);
      if(LMiliseconds > 0) then
        LDateTime    := IncMilliSecond(LDateTime,LMiliseconds);
      LRunLockFile.Add(CompileSecurityStringKey(LDateTime, AModelVersionString));
      LRunLockFile.SaveToFile(ALockFileLocation + 'RunLock.dat');
    finally
      LRunLockFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunYieldModelAgent.ChangeShortcutProperties(AShortCutFileName: String; ACloseOnExit: Boolean);
const OPNAME = 'TRunYieldModelAgent.ChangeShortcutProperties';
var
  LPIFFile     : File of Byte;
  LPIFData     : Array of Byte;
  LPIFLength   : LongInt;
  LCount       : Integer;

begin
  try
    if FileExists(AShortCutFileName) then
    begin
      AssignFile(LPIFFile, AShortCutFileName);
      Reset(LPIFFile);
      LPIFLength := 0;
      while not EOF(LPIFFile) do
      begin
        inc(LPIFLength);
        SetLength(LPIFData, LPIFLength);
        Read(LPIFFile, LPIFData[LPIFLength - 1]);
      end;

      if ACloseOnExit then
        LPIFData[99] := 16
      else
        LPIFData[99] := 0;

      Reset(LPIFFile);
      for LCount := 0 to LPIFLength - 1 do
      begin
        Write(LPIFFile,LPIFData[LCount]);
      end;
      CloseFile( LPIFFile );
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
