//
//
//  UNIT      : Contains TAbstractDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UAbstractDatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UAbstractFileNamesObject,
  UAbstractObject,
  UDataFileObjects,
  UYieldModelDataObject;

type

  TAbstractDatabaseAgent = class(TAbstractAppObject)
  protected
    function DeleteModelData(ATableNamesCommText,AdditionalWhereClause: string;
             AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean = False): boolean; virtual;
    function DeleteModelDataSQL(ATableName,AdditionalWhereClause: string): string; virtual;
    function DeleteUnknownModelDataSQL(AFileNamesObject: TFileNameObject): string; virtual;
    function DeleteUnknownModelData(AFileNamesObject: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; virtual;
    function DeleteFileName(AFileNamesObject: TFileNameObject): boolean; virtual;
    function DeleteFileNameSQL(AFileNamesObject: TFileNameObject): string; virtual;
    function InsertFileName(AFileNamesObject: TFileNameObject): boolean; virtual;
    function InsertFileNameSQL: string; virtual;
    function IsFileFoundInMultipleStudies(AFileNamesObject: TFileNameObject): boolean; virtual;
    function IsFileFoundInMultipleStudiesSQL(AFileNamesObject: TFileNameObject): string; virtual;
  public
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual; abstract;
    function WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual; abstract;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; virtual; abstract;
  end;


implementation

uses UUtilities,
     UStringDateTimeOperations,
     UDatabaseUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TAbstractDatabaseAgent.DeleteModelDataSQL(ATableName,AdditionalWhereClause: string): string;
const OPNAME = 'TAbstractDatabaseAgent.DeleteModelDataSQL';
begin

  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM '+ ATableName +
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              AdditionalWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.DeleteModelData(ATableNamesCommText,AdditionalWhereClause: string;
         AProgressFunction: TProgressUpdateFuntion;  AQuetly: boolean = False): boolean;
const OPNAME = 'TAbstractDatabaseAgent.DeleteModelData';
var
  LTableNames: TStringList;
  LMessage: string;
  LCount: integer;
  LDataSet : TAbstractModelDataset;
  LStop: boolean;
begin
  Result := False;

  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if (Trim(ATableNamesCommText) = '') then
      raise Exception.Create('There are no tables to be cleared of model data.');

    LTableNames := TStringList.Create;
    try
      LTableNames.CommaText := Trim(ATableNamesCommText);
      if (LTableNames.Count > 0) then
      begin
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
        try
          for LCount := 0 to LTableNames.Count - 1 do
          begin
            if not AQuetly then
            begin
              LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strDeleteModelData');
              LMessage := Format(LMessage,[LTableNames[LCount]]);
              AProgressFunction(LMessage,ptNone,LStop);
            end;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(DeleteModelDataSQL(LTableNames[LCount],AdditionalWhereClause));
            LDataSet.ExecSQL;
          end;
        finally
          LDataSet.DataSet.Close;
          LDataSet.Free;
        end;
      end;
    finally
      LTableNames.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.DeleteUnknownModelDataSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TAbstractDatabaseAgent.DeleteUnknownModelDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  '+ IntToStr(AFileNamesObject.FileNumber)+
              ' AND FileGroup     =  '+ IntToStr(AFileNamesObject.FileGroup);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.DeleteUnknownModelData(AFileNamesObject: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion;AQuetly: boolean = False): boolean;
const OPNAME = 'TAbstractDatabaseAgent.DeleteUnknownModelData';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LStop: boolean;
begin
  Result := False;

  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if not AQuetly then
      begin
        LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strDeleteModelData');
        LMessage := Format(LMessage,['WRYMFileLines']);
        AProgressFunction(LMessage,ptNone,LStop);
      end;
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteUnknownModelDataSQL(AFileNamesObject));
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.DeleteFileNameSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TAbstractDatabaseAgent.DeleteFileNameSQL';
begin
  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM FileNames'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND Identifier    =  '+IntToStr(AFileNamesObject.FileNumber)+
              ' AND FileGroup     =  '+IntToStr(AFileNamesObject.FileGroup);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.DeleteFileName(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TAbstractDatabaseAgent.DeleteFileName';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteFileNameSQL(AFileNamesObject));
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;

    AFileNamesObject.SavedInDB := False;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.IsFileFoundInMultipleStudiesSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TAbstractDatabaseAgent.IsFileFoundInMultipleStudiesSQL';
begin
  Result := '';
  try
    Result := 'SELECT FileName  '+
              ' FROM FileNames WHERE'+
              ' (Model           <>  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' OR StudyAreaName <>  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' OR SubArea       <>  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' OR Scenario      <>  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ')AND FileGroup    =  '+IntToStr(AFileNamesObject.FileGroup)+
              ' AND FileName     =  '+QuotedStr(UpperCase(Trim(ExtractFileName(AFileNamesObject.FileName))));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.IsFileFoundInMultipleStudies(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TAbstractDatabaseAgent.IsFileFoundInMultipleStudies';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(IsFileFoundInMultipleStudiesSQL(AFileNamesObject));
      LDataSet.DataSet.Open;
      Result :=(LDataSet.DataSet.RecordCount > 0);
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.InsertFileNameSQL: string;
const OPNAME = 'TAbstractDatabaseAgent.InsertFileNameSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FileNames'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:FileName,:FileGroup,:ImportDate,:FileDate)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractDatabaseAgent.InsertFileName(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TAbstractDatabaseAgent.InsertFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      AFileNamesObject.ImportDate := Now;
      AFileNamesObject.FileDate := FileLastWriteDate(AFileNamesObject.FileName);

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(InsertFileNameSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['Identifier'], [IntToStr(AFileNamesObject.FileNumber)]);
      LDataSet.SetParams(['FileName'], [UpperCase(ExtractFileName((Trim(AFileNamesObject.FileName))))]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileNamesObject.FileGroup)]);
      LDataSet.SetParams(['ImportDate'], [DateTimeToStamp(AFileNamesObject.ImportDate)]);
      LDataSet.SetParams(['FileDate'], [DateTimeToStamp(AFileNamesObject.FileDate)]);
      LDataSet.ExecSQL;

      LDataSet.DataSet.Close;
      AFileNamesObject.SavedInDB := True;
    finally
      LDataSet.Free;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
