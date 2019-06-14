//
//
//  UNIT      : Contains TYieldFileNamesDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldFileNamesDatabaseAgent;

interface

uses
  Classes,Contnrs, sysutils,Db,

  //  DWAF VCL
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UFileNames,
  UDataFileObjects,
  UYieldFileNamesAbstractAgent,
  UYieldModelDataObject;

type

  TYieldFileNamesDatabaseAgent = class(TYieldFileNamesAbstractAgent)
  protected
    function ReadDirectoryFileNameSQL: string;
    function ReadConfigFileNamesSQL: string;
    function ReadParamFileNameSQL: string;
    function ReadAltParamFileNameSQL: string;
    function ReadDemandFileNamesSQL: string;
    function ReadDamLevelFileNamesSQL: string;
    function ReadHydrologyFileNamesSQL: string;
    function ReadOutputFileNamesSQL: string;
    function ReadYieldFileNameSQL: string;
  public
    { Public declarations }
    function ReadDirectoryFileContents(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadDirectoryFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadConfigFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadParamFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadAltParamFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadDemandFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadDamLevelFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
  end;


implementation

uses UUtilities,
     UFileNameConstants,
     UFilePathsDatabaseAgent,
     UStringDateTimeOperations,
     UDataSetType,
     UErrorHandlingOperations;


function TYieldFileNamesDatabaseAgent.ReadYieldFileNameSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadYieldFileNameSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup  IN (1,2,3,4,5,7))'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesDatabaseAgent.ReadDirectoryFileNameSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDirectoryFileNameSQL';
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

function TYieldFileNamesDatabaseAgent.ReadConfigFileNamesSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadConfigFileNamesSQL';
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

function TYieldFileNamesDatabaseAgent.ReadDemandFileNamesSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDemandFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgDemand)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesDatabaseAgent.ReadDamLevelFileNamesSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDamLevelFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgDamWaterLevels)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesDatabaseAgent.ReadHydrologyFileNamesSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadHydrologyFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgHydrology)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesDatabaseAgent.ReadOutputFileNamesSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadOutputFileNamesSQL';
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

function TYieldFileNamesDatabaseAgent.ReadParamFileNameSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadParamFileNameSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgParameter)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesDatabaseAgent.ReadAltParamFileNameSQL: string;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadAltParamFileNameSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(fgAltParameter)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldFileNamesDatabaseAgent.ReadDirectoryFileContents(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDirectoryFileContents';
var
  LFilePathsDatabaseAgent : TFilePathsDatabaseAgent;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

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

function TYieldFileNamesDatabaseAgent.ReadDirectoryFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDirectoryFileName';
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

function TYieldFileNamesDatabaseAgent.ReadConfigFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadConfigFileNames';
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

function TYieldFileNamesDatabaseAgent.ReadDemandFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDemandFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadDemandFileNamesSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddDemandFileName(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
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

function TYieldFileNamesDatabaseAgent.ReadDamLevelFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadDamLevelFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadDamLevelFileNamesSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddDamLevelsFileName(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
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

function TYieldFileNamesDatabaseAgent.ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadHydrologyFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadHydrologyFileNamesSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddHydrologyFileName(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString), True,
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

function TYieldFileNamesDatabaseAgent.ReadOutputFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadOutputFileNames';
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

function TYieldFileNamesDatabaseAgent.ReadParamFileName(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadParamFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadParamFileNameSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateParamFileName(0,
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

function TYieldFileNamesDatabaseAgent.ReadAltParamFileName(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileNamesDatabaseAgent.ReadParamFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadAltParamFileNameSQL);
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateAltParamFileName(0,
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
