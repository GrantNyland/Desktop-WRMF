//
//
//  UNIT      : Contains TYieldFileNamesDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStomsaFileNamesDatabaseAgent;

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

  TStomsaFileNamesDatabaseAgent = class(TYieldFileNamesAbstractAgent)
  protected
    function ReadParamFileNameSQL: string;
    function ReadHydrologyFileNamesSQL: string;
  public
    { Public declarations }
    function ReadParamFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
  end;


implementation

uses UUtilities,
     UFileNameConstants,
     UFilePathsDatabaseAgent,
     UStringDateTimeOperations,
     UDataSetType,
     UErrorHandlingOperations;


function TStomsaFileNamesDatabaseAgent.ReadParamFileNameSQL: string;
const OPNAME = 'TStomsaFileNamesDatabaseAgent.ReadParamFileNameSQL';
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

function TStomsaFileNamesDatabaseAgent.ReadHydrologyFileNamesSQL: string;
const OPNAME = 'TStomsaFileNamesDatabaseAgent.ReadHydrologyFileNamesSQL';
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

function TStomsaFileNamesDatabaseAgent.ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TStomsaFileNamesDatabaseAgent.ReadHydrologyFileNames';
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

function TStomsaFileNamesDatabaseAgent.ReadParamFileName(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TStomsaFileNamesDatabaseAgent.ReadParamFileName';
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

end.
