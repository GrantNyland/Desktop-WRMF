//
//
//  UNIT      : Contains TAbstractWrpmDatabaseAgent Class
//  AUTHOR    : Lethabo Phatedi
//  DATE      : 2016/11/02
//  COPYRIGHT : Copyright © 2016 DWS
//
//
unit UAbstractWrpmDatabaseAgent;

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

  TAbstractWrpmDatabaseAgent = class(TAbstractAppObject)
  protected
    function DeleteWrpmModelData(ATableNamesCommText,AdditionalWhereClause: string;
             AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean = False): boolean; virtual;
    function DeleteWrpmModelDataSQL(ATableName,AdditionalWhereClause: string): string; virtual;
    function DeleteWrpmUnknownModelDataSQL(AFileNamesObject: TFileNameObject): string; virtual;
    function DeleteWrpmUnknownModelData(AFileNamesObject: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
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

function TAbstractWrpmDatabaseAgent.DeleteWrpmModelDataSQL(ATableName,AdditionalWhereClause: string): string;
const OPNAME = 'TAbstractWrpmDatabaseAgent.DeleteWrpmModelDataSQL';
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

function TAbstractWrpmDatabaseAgent.DeleteWrpmModelData(ATableNamesCommText,AdditionalWhereClause: string;
         AProgressFunction: TProgressUpdateFuntion;  AQuetly: boolean = False): boolean;
const OPNAME = 'TAbstractWrpmDatabaseAgent.DeleteWrpmModelData';
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
              LMessage := FAppModules.Language.GetString('TAbstractWrpmDatabaseAgent.strDeleteModelData');
              LMessage := Format(LMessage,[LTableNames[LCount]]);
              AProgressFunction(LMessage,ptNone,LStop);
            end;
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(DeleteWrpmModelDataSQL(LTableNames[LCount],AdditionalWhereClause));
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

function TAbstractWrpmDatabaseAgent.DeleteWrpmUnknownModelDataSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TAbstractWrpmDatabaseAgent.DeleteWrpmUnknownModelDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM WRPMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  '+ IntToStr(AFileNamesObject.FileNumber)+
              ' AND FileGroup     =  '+ IntToStr(AFileNamesObject.FileGroup);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractWrpmDatabaseAgent.DeleteWrpmUnknownModelData(AFileNamesObject: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion;AQuetly: boolean = False): boolean;
const OPNAME = 'TAbstractWrpmDatabaseAgent.DeleteWrpmUnknownModelData';
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
        LMessage := FAppModules.Language.GetString('TAbstractWrpmDatabaseAgent.strDeleteModelData');
        LMessage := Format(LMessage,['WRPMFileLines']);
        AProgressFunction(LMessage,ptNone,LStop);
      end;
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteWrpmUnknownModelDataSQL(AFileNamesObject));
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractWrpmDatabaseAgent.DeleteFileNameSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TAbstractWrpmDatabaseAgent.DeleteFileNameSQL';
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

function TAbstractWrpmDatabaseAgent.DeleteFileName(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TAbstractWrpmDatabaseAgent.DeleteFileName';
var
  LDataSet : TAbstractModelDataset;
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

function TAbstractWrpmDatabaseAgent.IsFileFoundInMultipleStudiesSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TAbstractWrpmDatabaseAgent.IsFileFoundInMultipleStudiesSQL';
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

function TAbstractWrpmDatabaseAgent.IsFileFoundInMultipleStudies(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TAbstractWrpmDatabaseAgent.IsFileFoundInMultipleStudies';
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

function TAbstractWrpmDatabaseAgent.InsertFileNameSQL: string;
const OPNAME = 'TAbstractWrpmDatabaseAgent.InsertFileNameSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FileNames'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:FileName,:FileGroup,:ImportDate,:FileDate)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractWrpmDatabaseAgent.InsertFileName(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TAbstractWrpmDatabaseAgent.InsertFileName';
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
