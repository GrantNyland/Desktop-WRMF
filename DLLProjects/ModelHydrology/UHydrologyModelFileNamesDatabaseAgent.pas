//
//
//  UNIT      : Contains THydrologyModelFileNamesDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyModelFileNamesDatabaseAgent;

interface

uses
  Classes,Contnrs, sysutils,Db,

  //  DWAF VCL
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UFileNames,
  UDataFileObjects,
  UYieldFileNamesAbstractAgent;

type

  THydrologyModelFileNamesDatabaseAgent = class(TYieldFileNamesAbstractAgent)
  protected
    function ReadHydrologyFileNamesSQL: string;
  public
    { Public declarations }
    function ReadHydrologyFileNames(AFileNamesObject: TModelFileNames): boolean;
  end;


implementation

uses UUtilities,
     UFileNameConstants,
     UStringDateTimeOperations,
     UDataSetType,
     UErrorHandlingOperations;


function THydrologyModelFileNamesDatabaseAgent.ReadHydrologyFileNamesSQL: string;
const OPNAME = 'THydrologyModelFileNamesDatabaseAgent.ReadHydrologyFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames'+
              ' WHERE StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND   SubArea =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND FileGroup       =  5 '+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModelFileNamesDatabaseAgent.ReadHydrologyFileNames(AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'THydrologyModelFileNamesDatabaseAgent.ReadHydrologyFileNames';
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
    finally
      LDataSet.Free;
    end;
    Result :=  True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
