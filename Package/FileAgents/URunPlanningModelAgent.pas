//
//
//  UNIT      : Contains TRunPlanningModelAgent Class
//  AUTHOR    : Dziedzi Ramulondi (ARAVIA)
//  DATE      : 12/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit URunPlanningModelAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractModelData,
  URunYieldModelAgent;

type

  TRunPlanningModelAgent = class(TRunYieldModelAgent)
  protected
    function CopyModelApplication(AProgressFunction: TProgressUpdateFuntion;var AAppFileName: string): boolean; override;
    procedure GenerateSecurityKey; override;
    function CompileSecurityStringKey(ACurrentDateTime: TDateTime;  AModelVersionString: string): string; override;
    function ExtractVersionField(var AModelVersionString: string): integer; override;
    procedure GenerateModelSecurityKeyFile(ALockFileLocation, AModelVersionString: string); override;
  public
    { Public declarations }
    function CheckInputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function RunModel(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion;AWindowHandle: THandle): boolean; override;
    function CheckOutputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ModelData: TAbstractModelData; override;
  end;

implementation

uses
  vcl.Forms,
  Windows,
  Contnrs,
  DateUtils,
  UUtilities,
  UDosFunctions,
  UFileNameConstants,
  UErrorHandlingOperations;

function TRunPlanningModelAgent.CheckInputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TRunPlanningModelAgent.CheckInputFiles';
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

    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strCheckInputFilesStart');
    AProgressFunction(LMessage,ptNone,LStop);

    if Assigned(ADataFileObjects.FPathsObject)then
    begin
      if(ADataFileObjects.FPathsObject.OutputFilesPath.FInitalised) then
      begin
        LFileName := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
        if(LFileName = '') then
        begin
          LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strWrymDataFileNotLoaded');
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
                DeleteFile(PChar(Trim(LFileList[LCount])));
              end;
            end;
            Result := True;
          finally
            FreeAndNil(LFileList);
          end;
        end;
      end;
    end;

    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strCheckInputFilesEnd');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunPlanningModelAgent.CheckOutputFiles(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TRunPlanningModelAgent.CheckOutputFiles';
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

    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strCheckOutputFilesStart');
    AProgressFunction(LMessage,ptNone,LStop);

    if Assigned(ADataFileObjects.FPathsObject)then
    begin
      if(ADataFileObjects.FPathsObject.OutputFilesPath.FInitalised) then
      begin
        LFileName := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
        if(LFileName = '') then
        begin
          LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strWrymDataFileNotLoaded');
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
    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strCheckOutputFilesEnd');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunPlanningModelAgent.CopyModelApplication( AProgressFunction: TProgressUpdateFuntion;var AAppFileName: string): boolean;
const OPNAME = 'TRunPlanningModelAgent.CopyModelApplication';
var
  LSourceFilesPath: string;
  LDestFilesPath: string;
  LSourceFileName: string;
  LDestFileName: string;
  LMessage : string;
  LStop: boolean;
begin
  Result := False;
  AAppFileName := '';
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strCopyFilesStart');
    AProgressFunction(LMessage,ptNone,LStop);

    LSourceFilesPath := IncludeTrailingPathDelimiter(ExtractFilePath(ApplicationExeName))+'WRPM\';
    LDestFilesPath   := IncludeTrailingPathDelimiter(ExtractFilePath(ModelData.FileNamesObject.DirectoryFileNames.FileNameObject[0].FilePath));
    if not DirectoryExists(LDestFilesPath) then
      ForceDirectories(LDestFilesPath);

    LDestFileName := LDestFilesPath + 'WRPMAgents_4417.dll';
    if not FileExists(LDestFileName) then
    begin
       LSourceFileName := LSourceFilesPath + 'WRPMAgents_4417.dll';
       CopyFile(PChar(LSourceFileName),PChar(LDestFileName),false);
    end;

    LDestFileName := LDestFilesPath + 'WRPM_4417d.exe';
    if not FileExists(LDestFileName) then
    begin
       LSourceFileName := LSourceFilesPath + 'WRPM_4417d.exe';
       CopyFile(PChar(LSourceFileName),PChar(LDestFileName),false);
    end;

    LDestFileName  := LDestFilesPath + 'WRPM_4417r.exe';
    if not FileExists(LDestFileName) then
    begin
       LSourceFileName := LSourceFilesPath + 'WRPM_4417r.exe';
       CopyFile(PChar(LSourceFileName),PChar(LDestFileName),false);
    end;

    AAppFileName  := LDestFilesPath + 'WRPM_4417r.exe';
    Result := True;

    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strCopyFilesComplete');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunPlanningModelAgent.RunModel(ADataFileObjects:TDataFileObjects; AProgressFunction: TProgressUpdateFuntion;AWindowHandle: THandle): boolean;
const OPNAME = 'TRunPlanningModelAgent.RunModel';
var
  LMessage,
  LAppName: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if (AWindowHandle = 0) then
      raise Exception.Create('Window handle parameter is not yet assigned.');

    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strRunStart');
    AProgressFunction(LMessage,ptNone,LStop);

    Result := CopyModelApplication(AProgressFunction,LAppName);
    if Result then
    begin
      LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strWaitModelRunning');
      AProgressFunction(LMessage,ptNone,LStop);
      Result := Result and UUtilities.ExecAndWait(LAppName,'');
      if not Result then
      begin
        LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strModelRunningFailed');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    end;
    LMessage := FAppModules.Language.GetString('TRunPlanningModelAgent.strRunCompleted');
    AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunPlanningModelAgent.ModelData: TAbstractModelData;
const OPNAME = 'TRunPlanningModelAgent.ModelData';
begin
  Result := Nil;
  try
    Result := TAbstractModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunPlanningModelAgent.GenerateSecurityKey;
const OPNAME = 'TRunPlanningModelAgent.GenerateSecurityKey';
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

function TRunPlanningModelAgent.ExtractVersionField(var AModelVersionString: string): integer;
const OPNAME = 'TRunPlanningModelAgent.ExtractVersionField';
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

function TRunPlanningModelAgent.CompileSecurityStringKey(ACurrentDateTime: TDateTime; AModelVersionString: string): string;
const OPNAME = 'TRunPlanningModelAgent.CompileSecurityStringKey';
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

procedure TRunPlanningModelAgent.GenerateModelSecurityKeyFile(ALockFileLocation, AModelVersionString: string);
const OPNAME = 'TRunPlanningModelAgent.GenerateModelSecurityKeyFile';
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


end.
