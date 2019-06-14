//
//
//  UNIT      : Contains TDDTSFileNamesDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDDTSFileNamesDatabaseAgent;

interface

uses
  Classes,Contnrs, sysutils,Db,

  //  DWAF VCL
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UFileNames,
  UDataFileObjects;

type

  TDDTSFileNamesDatabaseAgent = class(TAbstractAppObject)
  protected
    function ReadDirectoryFileNameSQL: string;
    function ReadConfigFileNamesSQL: string;
    function ReadOutputFileNamesSQL: string;
  public
    { Public declarations }
    function ReadDirectoryFileContents(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
    function ReadDirectoryFileName(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
    function ReadInputFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
    function ReadOutputFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
  end;


implementation

uses UUtilities,
     UFileNameConstants,
     UFilePathsDatabaseAgent,
     UStringDateTimeOperations,
     UDataSetType,
     UErrorHandlingOperations;


function TDDTSFileNamesDatabaseAgent.ReadDirectoryFileNameSQL: string;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadDirectoryFileNameSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgDirectories)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesDatabaseAgent.ReadConfigFileNamesSQL: string;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadConfigFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgConfiguration)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesDatabaseAgent.ReadOutputFileNamesSQL: string;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadOutputFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgOutput)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesDatabaseAgent.ReadDirectoryFileContents(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadDirectoryFileContents';
var
  LFilePathsDatabaseAgent : TFilePathsDatabaseAgent;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if ReadDirectoryFileName(ADataFileObjects,AFileNamesObject) then
    begin
      LFilePathsDatabaseAgent := TFilePathsDatabaseAgent.Create(FAppModules);
      try
        LFilePathsDatabaseAgent.ReadModelDataFromDatabase(
          TFileNameObject(AFileNamesObject.DirectoryFileNames.FileNameObject[0]),ADataFileObjects,nil);
        Result := (Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData) <> '') and
                  (Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData) <> '') and
                  (Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData) <> '');
        if Result then
        begin
          AFileNamesObject.FileNamePrefix := Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);
          AFileNamesObject.PopulateInputFilesPaths(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData));
          AFileNamesObject.PopulateOutputPaths(Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData));
          if(Trim(ADataFileObjects.FPathsObject.HydrologyPath.FData) <> '') then
            AFileNamesObject.PopulateHydrologyPaths(Trim(ADataFileObjects.FPathsObject.HydrologyPath.FData));
          if(Trim(ADataFileObjects.FPathsObject.SpecifiedDemandPath.FData) <> '') then
            AFileNamesObject.PopulateDemandFilesPaths(Trim(ADataFileObjects.FPathsObject.SpecifiedDemandPath.FData));
        end;
      finally
        LFilePathsDatabaseAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesDatabaseAgent.ReadDirectoryFileName(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadDirectoryFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadDirectoryFileNameSQL);
      LDataSet.DataSet.Open;
      if (LDataSet.DataSet.RecordCount > 0) then
      begin
        if (not LDataSet.DataSet.FieldByName('FileName').IsNull) then
        begin
          AFileNamesObject.UpdateDirectoryFileName(0,
            Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
            DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
            DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
            Result := True;
        end;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesDatabaseAgent.ReadInputFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadInputFileNames';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadConfigFileNamesSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateConfigFileName(LDataSet.DataSet.FieldByName('Identifier').AsInteger -1,
              Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
        Result :=  True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSFileNamesDatabaseAgent.ReadOutputFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TDDTSFileNamesDatabaseAgent.ReadOutputFileNames';
var
  LDataSet : TAbstractModelDataset;
  LOutputFileType:TOutputFileType;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadOutputFileNamesSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LOutputFileType := GetOutputFileType(Trim(LDataSet.DataSet.FieldByName('FileName').AsString));
            if(LOutputFileType <> oftNone) then
              AFileNamesObject.UpdateOutputFileName(LOutputFileType,
                Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
                DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
                DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
        Result :=  True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
