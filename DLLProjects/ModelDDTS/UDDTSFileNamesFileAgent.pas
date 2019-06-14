//
//
//  UNIT      : Contains TDDTSFileNamesFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDDTSFileNamesFileAgent;

interface

uses
  Classes,Contnrs, sysutils,

  //  DWAF VCL
  UConstants,
  UAbstractObject,
  UFileNames,
  UDataFileObjects;

type

  TDDTSFileNamesFileAgent = class(TAbstractAppObject)
  protected
  public
    { Public declarations }
    function ReadDirectoryFileContents(ADataFileObjects:TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadDirectoryFileName(ADataFileObjects:TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadInputFileNames(ADataFileObjects:TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadOutputFileNames(ADataFileObjects:TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
  end;


implementation

uses UUtilities,
     UAbstractFileNamesObject,
     UErrorHandlingOperations;


{ TDDTSFileNamesFileAgent }

function TDDTSFileNamesFileAgent.ReadDirectoryFileContents(ADataFileObjects:TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSFileNamesFileAgent.ReadDirectoryFileContents';
var
  LFileData: TStringList;
  LMessage: String;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not AFileNamesObject.DirectoryFileNames.FileNameObject[0].Populated then
      ReadDirectoryFileName(ADataFileObjects,AFileNamesObject,AProgressFunction);

    if not FileExists(AFileNamesObject.CastDirectoryFileNames[0].FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFilePathsAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName('Wrym.dat')]);
      AProgressFunction(LMessage,ptError,LStop);
    end
    else
    begin
      LFileData := TStringList.Create;
      try
        LFileData.LoadFromFile(AFileNamesObject.CastDirectoryFileNames[0].FileName);
        if(LFileData.Count >= 3) then
        begin
          Result := (Trim(LFileData[0]) <> '') and (Trim(LFileData[1]) <> '') and (Trim(LFileData[2]) <> '');
          if Result then
          begin
            AFileNamesObject.FileNamePrefix := Trim(LFileData[0]);
            AFileNamesObject.PopulateInputFilesPaths(IncludeTrailingPathDelimiter(Trim(LFileData[1])));
            AFileNamesObject.PopulateOutputPaths(IncludeTrailingPathDelimiter(Trim(LFileData[2])));
          end;
        end;
      finally
        LFileData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesFileAgent.ReadDirectoryFileName(ADataFileObjects:TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSFileNamesFileAgent.ReadDirectoryFileName';
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if FileExists(FAppModules.StudyArea.DataFilesPath) then
    begin
      AFileNamesObject.PopulateInputFilesPaths(ExtractFilePath(FAppModules.StudyArea.DataFilesPath));
      AFileNamesObject.UpdateDirectoryFileName(0,FAppModules.StudyArea.DataFilesPath,False,0.0,
      FileLastWriteDate(FAppModules.StudyArea.DataFilesPath))
    end
    else
      AFileNamesObject.UpdateDirectoryFileName(0,FAppModules.StudyArea.DataFilesPath,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesFileAgent.ReadInputFileNames(ADataFileObjects:TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSFileNamesFileAgent.ReadInputFileNames';
var
  LCount: integer;
  LFileList: TStringList;
  LSearchFile,
  LFileName: string;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if(AFileNamesObject.FileNamePrefix = '') then
      ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,AProgressFunction);

    if(AFileNamesObject.FileNamePrefix <> '') then
    begin
      LFileList := TStringList.Create;
      try
        LSearchFile := Trim(AFileNamesObject.InputFilesPath);
        LSearchFile := IncludeTrailingPathDelimiter(LSearchFile);
        LSearchFile := LSearchFile + Trim(AFileNamesObject.FileNamePrefix);
        LSearchFile := LSearchFile + '*.csv';
        if(UUtilities.SearchFiles(LSearchFile,LFileList)) then
        begin
          for LCount := 0 to LFileList.Count - 1 do
          begin
            LFileName := ExtractFileName(LFileList[LCount]);

            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[00].ShortName) = UpperCase(LFileName)) then
              AFileNamesObject.UpdateConfigFileName(00,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[01].ShortName) = UpperCase(LFileName)) then
              AFileNamesObject.UpdateConfigFileName(01,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[02].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(02,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[03].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(03,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[04].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(04,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[05].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(05,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
          end;
        end;
      finally
        LFileList.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesFileAgent.ReadOutputFileNames(ADataFileObjects:TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSFileNamesFileAgent.ReadOutputFileNames';
var
  LFileName       : string;
  LFilePrefix     : string;
  LOutputFileType : TOutputFileType;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if(AFileNamesObject.FileNamePrefix = '') then
      ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,AProgressFunction);

    if(AFileNamesObject.FileNamePrefix <> '') then
    begin
      LFilePrefix := Trim(AFileNamesObject.OutputFilesPath)+
                     Trim(AFileNamesObject.FileNamePrefix);

      LFileName   := LFilePrefix + 'Run.csv';
      LOutputFileType := oftYield; //GetOutputFileType(LFileName);
      AFileNamesObject.UpdateOutputFileName(LOutputFileType,LFileName+'', False,0.0,FileLastWriteDate(LFileName));

      LFileName   := LFilePrefix + 'Result.xlsx';
      LOutputFileType := oftDebug; //GetOutputFileType(LFileName);
      AFileNamesObject.UpdateOutputFileName(LOutputFileType,LFileName+'', False,0.0,FileLastWriteDate(LFileName));

    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
