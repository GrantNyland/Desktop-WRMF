//
//
//  UNIT      : Contains TProjectFileDatabaseAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 02/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UProjectFileDatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UHydrologyFilesObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;
type

  TProjectFileDatabaseAgent = class(TAbstractAppObject)
  protected
    function ReadProjectFileDataSQL: string;
    function WriteProjectFileDataSQL: string;
    function DeleteFileData(AProgressFunction: TProgressUpdateFuntion): boolean;
    function DeleteFileDataSQL: string;
  public
    { Public declarations }
    function ExportFile(AProgressFunction: TProgressUpdateFuntion): boolean;
    function ImportFile(AProgressFunction: TProgressUpdateFuntion): boolean;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UDataModule,
     UStomsaData,
     UErrorHandlingOperations;



function TProjectFileDatabaseAgent.ReadProjectFileDataSQL: string;
const OPNAME = 'TProjectFileDatabaseAgent.ReadProjectFileDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT * '+
              ' FROM  StomsaProjectFile'+
              ' WHERE Model             = '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              '       AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              '       AND SubArea       = '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              '       AND Scenario      = '+QuotedStr(FAppModules.StudyArea.ScenarioCode);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProjectFileDatabaseAgent.WriteProjectFileDataSQL: string;
const OPNAME = 'TProjectFileDatabaseAgent.WriteProjectFileDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO StomsaProjectFile'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileName)'+
              ' Values (:Model,:StudyAreaName,:SubArea,:Scenario,:FileName)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProjectFileDatabaseAgent.DeleteFileDataSQL: string;
const OPNAME = 'TProjectFileDatabaseAgent.DeleteFileDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM StomsaProjectFile'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProjectFileDatabaseAgent.DeleteFileData(AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TProjectFileDatabaseAgent.DeleteFileData';
var
  //LMessage: string;
  LDataSet : TAbstractModelDataset;
  //LStop: boolean;
begin
  Result := False;

  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    //LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strDeleteModelData');
    //LMessage := Format(LMessage,['StomsaProjectFile']);
    //AProgressFunction(LMessage,ptNone,LStop);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteFileDataSQL);
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProjectFileDatabaseAgent.ImportFile(AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TProjectFileDatabaseAgent.ImportFile';
var
  LFileName,
  LMessage:string;
  LFileStream : TFileStream;
  LBlobStream   : TStream;
  LDataSet : TAbstractModelDataset;
  LStop: boolean;
begin
  Result := False;
  try
    if not ((fmData <> nil) and (fmData.DataStorage <> nil) and FileExists(fmData.DataStorage.ProjectFileName)) then
      Exit;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LFileName := ExtractFileName(fmData.DataStorage.ProjectFileName);

    LMessage := FAppModules.Language.GetString('TProjectFileDatabaseAgent.strWriteStarted');
    LMessage := Format(LMessage,[LFileName]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not DeleteFileData(AProgressFunction) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteProjectFileDataSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['FileName'], [LFileName]);
      LDataSet.ExecSQL;
      LDataSet.DataSet.Close;

      LDataSet.SetSQL(ReadProjectFileDataSQL);
      LDataSet.SetReadOnly(False);
      LDataSet.DataSet.Open;
      if not LDataset.DataSet.Eof then
      begin
        LDataSet.DataSet.Edit;
        LFileStream := TFileStream.Create(fmData.DataStorage.ProjectFileName,fmOpenRead);
        LBlobStream := LDataSet.DataSet.CreateBlobStream(LDataSet.DataSet.FieldByName('FileData'),bmWrite);
        try
          LFileStream.Position := 0;
          LBlobStream.CopyFrom(LFileStream, LFileStream.Size);
          LDataSet.DataSet.Post;
        finally
          FreeAndNil(LFileStream);
          FreeAndNil(LBlobStream);
        end;
      end;

      LMessage := FAppModules.Language.GetString('TProjectFileDatabaseAgent.strWriteEnded');
      LMessage := Format(LMessage,[LFileName]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProjectFileDatabaseAgent.ExportFile(AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TProjectFileDatabaseAgent.ExportFile';
var
  LFileName,
  LMessage          : string;
  LDataSet          : TAbstractModelDataset;
  LFileStream : TFileStream;
  LBlobStream   : TStream;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TProjectFileDatabaseAgent.strReadStarted');
    LMessage := Format(LMessage,['Project File']);
    AProgressFunction(LMessage,ptNone,LStop);

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadProjectFileDataSQL);
      LDataSet.SetReadOnly(False);
      LDataSet.DataSet.Open;
      if not LDataset.DataSet.Eof then
      begin
        LFileName   := IncludeTrailingPathDelimiter(FAppModules.StudyArea.DataFilesPath) +
                       Trim(LDataSet.DataSet.FieldByName('FileName').AsString);
        LDataSet.DataSet.Edit;
        LFileStream := TFileStream.Create(LFileName,fmCreate);
        LBlobStream := LDataSet.DataSet.CreateBlobStream(LDataSet.DataSet.FieldByName('FileData'),bmReadWrite);
        try
          LBlobStream.Position := 0;
          LFileStream.CopyFrom(LBlobStream, LBlobStream.Size);
        finally
          FreeAndNil(LFileStream);
          FreeAndNil(LBlobStream);
        end;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TProjectFileDatabaseAgent.strReadEnded');
      LMessage := Format(LMessage,[ExtractFileName(LFileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
