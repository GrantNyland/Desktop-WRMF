//
//
//  UNIT      : Contains TYieldFileNamesFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldFileNamesFileAgent;

interface

uses
  Classes,Contnrs, sysutils,

  //  DWAF VCL
  UConstants,
  UAbstractObject,
  UFileNames,
  UDataFileObjects;

type

  TYieldFileNamesFileAgent = class(TAbstractAppObject)
  protected
    function FindHydrologyFileNames(ADataFileObjects:TDataFileObjects;AFileNamesObject: TModelFileNames;
             AHydrologyFileNames: TStrings;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadParamFileContents(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean;
  public
    { Public declarations }
    function ReadDirectoryFileContents(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadDirectoryFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadConfigFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadParamFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadAltParamFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadHydrologyFilesPath(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadDemandFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
    function ReadOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;
  end;


implementation

uses UUtilities,
     UFile01Agent,
     UFile02Agent,
     UFile03Agent,
     UFile20Agent,
     UParamObject,
     USFRFileObject,
     UFilePathsAgent,
     UFileParamAgent,
     URunParametersObject,
     UYieldModelDataObject,
     UAbstractFileNamesObject,
     UChannelDescriptionObject,
     UErrorHandlingOperations;


{ TYieldFileNamesFileAgent }

function TYieldFileNamesFileAgent.ReadDirectoryFileContents(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadDirectoryFileContents';
var
  LFilePathsAgent: TFilePathsAgent;
  LMessage: String;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

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
      LFilePathsAgent := TFilePathsAgent.Create(FAppModules);
      try
        LFilePathsAgent.ReadModelDataFromFile(AFileNamesObject.CastDirectoryFileNames[0],ADataFileObjects,AProgressFunction);
        Result := (Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData) <> '') and
                  (Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData) <> '') and
                  (Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData) <> '');
        if Result then
        begin
          AFileNamesObject.PopulateInputFilesPaths(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData));
          AFileNamesObject.PopulateOutputPaths(Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData));
        end;
      finally
        LFilePathsAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadDirectoryFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadDirectoryFileName';
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

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

function TYieldFileNamesFileAgent.ReadConfigFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadConfigFileNames';
var
  LCount: integer;
  LFileList: TStringList;
  LSearchFile,
  LFileName: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ADataFileObjects.FPathsObject.Populated then
      ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,AProgressFunction);
    if ADataFileObjects.FPathsObject.Populated then
    begin
      LFileList := TStringList.Create;
      try
        LSearchFile := Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData);
        LSearchFile := IncludeTrailingPathDelimiter(LSearchFile);
        LSearchFile := LSearchFile + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);
        LSearchFile := LSearchFile + '*.DAT';
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
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[06].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(06,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[07].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(07,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[08].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(08,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[09].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(09,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[10].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(10,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[11].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(11,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[12].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(12,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[13].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(13,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[14].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(14,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[15].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(15,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[16].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(16,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[17].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(17,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[18].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(18,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[19].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(19,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
            if(UpperCase(AFileNamesObject.ConfigFileNames.FileNameObject[20].ShortName) = UpperCase(LFileName)) then
             AFileNamesObject.UpdateConfigFileName(20,LFileList[LCount],False,0.0,FileLastWriteDate(LFileList[LCount]));
          end;
        end;
      finally
        LFileList.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadParamFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadParamFileName';
var
  LFile01ReaderAgent: TFile01Agent;
  LCurrentFile: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if Assigned(AFileNamesObject.ConfigFileNames.FileNameObject[0]) then
    begin
      if(AFileNamesObject.ConfigFileNames.FileNameObject[0].FileFound)then
      begin
        LFile01ReaderAgent := TFile01Agent.Create(FAppModules);
        try
          LFile01ReaderAgent.ReadModelDataFromFile(AFileNamesObject.ConfigFileNames.FileNameObject[0],ADataFileObjects,AProgressFunction);
        finally
          LFile01ReaderAgent.Free;
        end;

        LCurrentFile := IncludeTrailingPathDelimiter(ADataFileObjects.FPathsObject.HydrologyPath.FData)+
                        Trim(ADataFileObjects.FRunParametersObject.FParamName.FData);
        if FileExists(LCurrentFile) then
        begin
          AFileNamesObject.PopulateHydrologyPaths(ExtractFilePath(LCurrentFile));
          AFileNamesObject.UpdateParamFileName(00,LCurrentFile,False,0.0,FileLastWriteDate(LCurrentFile));
        end
        else
         AFileNamesObject.UpdateParamFileName(00,LCurrentFile,False,0.0,0.0);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadHydrologyFilesPath(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadHydrologyFilesPath';
var
  LFile01ReaderAgent: TFile01Agent;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if Assigned(AFileNamesObject.ConfigFileNames.FileNameObject[0]) then
    begin
      if(AFileNamesObject.ConfigFileNames.FileNameObject[0].FileFound)then
      begin
        LFile01ReaderAgent := TFile01Agent.Create(FAppModules);
        try
          LFile01ReaderAgent.ReadModelDataFromFile(AFileNamesObject.ConfigFileNames.FileNameObject[0],ADataFileObjects,AProgressFunction);
        finally
          LFile01ReaderAgent.Free;
        end;
        if (ADataFileObjects.FPathsObject.HydrologyPath.FData <> '') then
        begin
          AFileNamesObject.PopulateHydrologyPaths(ADataFileObjects.FPathsObject.HydrologyPath.FData);
        end
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadDemandFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadDemandFileNames';
var
  LCount: integer;
  LDemandChanel: TDemandChannelObject;
  LFileReaderAgent: TFile03Agent;
  LCurrentFile: string;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
     if Assigned(AFileNamesObject.ConfigFileNames.FileNameObject[2]) then
    begin
      LCurrentFile := AFileNamesObject.ConfigFileNames.FileNameObject[2].FileName;
      if(Trim(LCurrentFile) <> '')then
      begin
        LFileReaderAgent := TFile03Agent.Create(FAppModules);
        try
          LFileReaderAgent.ReadModelDataFromFile(AFileNamesObject.ConfigFileNames.FileNameObject[2],
          ADataFileObjects,AProgressFunction);
        finally
          LFileReaderAgent.Free;
        end;

        if Assigned(ADataFileObjects.FChannelDescrObject) and
           Assigned(ADataFileObjects.FChannelDescrObject.FDemandChannelList)then
        begin
          if(ADataFileObjects.FChannelDescrObject.FDemandChannelList.Count > 0) then
          begin
            AFileNamesObject.PopulateDemandFilesPaths(Trim(ADataFileObjects.FPathsObject.SpecifiedDemandPath.FData));
            for LCount := 0 to ADataFileObjects.FChannelDescrObject.FDemandChannelList.Count -1 do
            begin
              LDemandChanel := TDemandChannelObject(ADataFileObjects.FChannelDescrObject.FDemandChannelList[LCount]);
              if(Trim(LDemandChanel.FFullname.FData) <> '') then
              begin
                LNewFileIndex := AFileNamesObject.DemandFileNames.Count + 1;
                if FileExists(Trim(LDemandChanel.FFullname.FData)) then
                  AFileNamesObject.AddDemandFileName(LNewFileIndex,Trim(LDemandChanel.FFullname.FData),False,0.0,
                    FileLastWriteDate(Trim(LDemandChanel.FFullname.FData)))
                else
                  AFileNamesObject.AddDemandFileName(LNewFileIndex,Trim(LDemandChanel.FFullname.FData),False,0.0,0.0);
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadHydrologyFileNames';
var
  LFileList: TStringList;
  LCount: integer;
  LFile02ReaderAgent: TFile02Agent;
  LNewFileIndex : integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if FileExists(AFileNamesObject.ConfigFileNames.FileNameObject[01].FileName) then
    begin
      LFile02ReaderAgent := TFile02Agent.Create(FAppModules);
      try
        LFile02ReaderAgent.ReadModelDataFromFile(AFileNamesObject.ConfigFileNames.FileNameObject[01],
                                                 ADataFileObjects,AProgressFunction)
      finally
        LFile02ReaderAgent.Free;
      end;
    end;

    LFileList := TStringList.Create;
    try
      ReadParamFileContents(ADataFileObjects,AFileNamesObject,AProgressFunction);
      if (ADataFileObjects.FParamObject.GaugeStochasticsContainer.ItemsCount > 0)then
      begin
          LFileList.Sorted := True;
          LFileList.Duplicates := dupIgnore;
          if FindHydrologyFileNames(ADataFileObjects,AFileNamesObject,LFileList,AProgressFunction) then
          begin
            LNewFileIndex := AFileNamesObject.HydrologyFileNames.Count;
            for LCount := 0 to LFileList.Count -1 do
            begin
              LNewFileIndex := LNewFileIndex + 1;
              AFileNamesObject.AddHydrologyFileName(LNewFileIndex,LFileList[LCount],False,0.0,
              FileLastWriteDate(LFileList[LCount]));
            end;
          end;
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadOutputFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadOutputFileNames';
var
  LFileList: TStringList;
  LSearchFile: string;
  LOutputFileType:TOutputFileType;
  LCount: integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ADataFileObjects.FPathsObject.Populated then
      ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,AProgressFunction);

    if ADataFileObjects.FPathsObject.Populated then
    begin
      LSearchFile := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
      LSearchFile := IncludeTrailingPathDelimiter(LSearchFile);
      LSearchFile := LSearchFile + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);
      LSearchFile := LSearchFile + '*.OUT';

      LFileList := TStringList.Create;
      try
        if(UUtilities.SearchFiles(LSearchFile,LFileList)) then
        begin
          for LCount := 0 to LFileList.Count - 1 do
          begin
            LOutputFileType := GetOutputFileType(LFileList[LCount]);
            AFileNamesObject.UpdateOutputFileName(LOutputFileType,LFileList[LCount],
              False,0.0,FileLastWriteDate(LFileList[LCount]));
          end;
        end;
      finally
        LFileList.Free;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.FindHydrologyFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames; AHydrologyFileNames: TStrings;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.FindHydrologyFileNames';
var
  LCount,
  LCount2: integer;
  LFileExt,
  LFileName: string;
  LSFRObject:TSFRObject;
  LFile20Agent:TFile20Agent;
  LFileList: TStringList;
  LNodesContainer: TObjectList;
  LModelFileName:TAbstractModelFileName;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects)  and Assigned(AHydrologyFileNames)then
    begin
      AHydrologyFileNames.Clear;

      if Assigned(ADataFileObjects.FParamObject) and
         (ADataFileObjects.FParamObject.GaugeStochasticsContainer.ItemsCount > 0) then
      begin
        LFileList := TStringList.Create;
        LNodesContainer := TObjectList.Create(False);
        try
          for LCount := 0 to ADataFileObjects.FParamObject.GaugeStochasticsContainer.ItemsCount-1 do
          begin
            LNodesContainer.Clear;
            LFileName := ADataFileObjects.FParamObject.GaugeStochasticsContainer.GaugeStochasticsByIndex[LCount].GaugePathName.FData;
            LFileName := Trim(LFileName);
            if(LFileName <> '') then
            begin
              LFileName := LFileName + '*.*';
              LFileList.Clear;
              if(UUtilities.SearchFiles(LFileName,LFileList)) then
              begin
                for LCount2 := 0 to LFileList.Count - 1 do
                begin
                  LFileExt := UpperCase(ExtractFileExt(LFileList[LCount2]));
                  if(FAppModules.StudyArea.ModelCode = CPlanning) then
                  begin
                    if(LFileExt = '.INC') or
                      (LFileExt = '.INF') or
                      (LFileExt = '.RNK') or
                      (LFileExt = '.RAN') or
                      (LFileExt = '.AFF') or
                      (LFileExt = '.URB') or
                      (LFileExt = '.IRR') then
                    begin
                      AHydrologyFileNames.Add(LFileList[LCount2]);
                    end;
                  end
                  else
                  begin
                    if(LFileExt = '.INC') or
                      (LFileExt = '.INF') or
                      (LFileExt = '.RNK') or
                      (LFileExt = '.RAN') or
                      (LFileExt = '.AFF') or
                      (LFileExt = '.IRR') then
                    begin
                      AHydrologyFileNames.Add(LFileList[LCount2]);
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          LFileList.Free;
          LNodesContainer.Free;
        end;
      end;

      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LModelFileName := AFileNamesObject.ConfigFileNames.FileNameObject[19];
        if(LModelFileName <> nil) and  FileExists(LModelFileName.FileName) then
        begin
          LFile20Agent := TFile20Agent.Create(FAppModules);
          try
            if LFile20Agent.ReadModelDataFromFile(LModelFileName,ADataFileObjects,AProgressFunction) then
            begin
              for LCount := 0 to ADataFileObjects.FSFRFileObject.SFRCount-1 do
              begin
                LSFRObject := ADataFileObjects.FSFRFileObject.SFRObjectByIndex[LCount];
                if FileExists(LSFRObject.UnitRunoffFileName.FData) then
                  AHydrologyFileNames.Add(LSFRObject.UnitRunoffFileName.FData);
                if FileExists(LSFRObject.SoilMoistureFileName.FData) then
                  AHydrologyFileNames.Add(LSFRObject.SoilMoistureFileName.FData);
              end;
            end;
          finally
            LFile20Agent.Free;
          end;
        end;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadParamFileContents(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadParamFileContents';
var
  LFileReaderAgent: TFileParamAgent;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if FileExists(AFileNamesObject.ParamFileNames.FileNameObject[0].FileName) then
    begin
      LFileReaderAgent := TFileParamAgent.Create(FAppModules);
      try
        LFileReaderAgent.ReadFileNamesOnly := True;
        LFileReaderAgent.ReadModelDataFromFile(AFileNamesObject.ParamFileNames.FileNameObject[0],
        ADataFileObjects,AProgressFunction);
      finally
        LFileReaderAgent.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesFileAgent.ReadAltParamFileName(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYieldFileNamesFileAgent.ReadParamFileName';
var
  LCurrentFile: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LCurrentFile := IncludeTrailingPathDelimiter(ADataFileObjects.FPathsObject.HydrologyPath.FData)+
                    'PARAMFOR.DAT';
    if FileExists(LCurrentFile) then
    begin
      AFileNamesObject.UpdateAltParamFileName(00,LCurrentFile,False,0.0,FileLastWriteDate(LCurrentFile));
    end
    else
     AFileNamesObject.UpdateAltParamFileName(00,LCurrentFile,False,0.0,0.0);
     
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
