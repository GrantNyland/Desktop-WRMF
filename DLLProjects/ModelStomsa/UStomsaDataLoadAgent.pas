//
//
//  UNIT      : Contains TStomsaDataLoadAgent Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 30/07/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UStomsaDataLoadAgent;

interface

uses
  Classes,
  Contnrs,
  UStomsaData,
  UAbstractObject;

type
  TStomsaDataLoadAgent = class(TAbstractAppObject)
  public
    function LoadDataFromDB(AStomsaData:TStomsaData): boolean;
    function SaveDataToDB(AStomsaData:TStomsaData): boolean;
  end;

implementation
uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UUtilities,
  UDataSetType,
  UConstants,
  UStomsaDataSQLAgent,
  UErrorHandlingOperations,
  DB;

{ TStomsaDataLoadAgent }

function TStomsaDataLoadAgent.SaveDataToDB(AStomsaData: TStomsaData): boolean;
const OPNAME = 'TStomsaDataLoadAgent.SaveDataToDB';
var
  LSQLAgent : TStomsaDataSQLAgent;
  LDataSet  : TAbstractModelDataset;
  LIndex    : integer;
  LIncData  : TIncData;
begin
  Result := False;
  try
    LSQLAgent := TStomsaDataSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet)  then
      begin
        LDataSet.SetSQL(LSQLAgent.GetDeleteAreaDataSQL);
        LDataset.ExecSQL;

        for LIndex := 0 to AStomsaData.IncFileCount-1 do
        begin
          LIncData := AStomsaData.FindIncFileByIndex(LIndex);
          LDataset.DataSet.Close;
          LDataSet.SetSQL(LSQLAgent.GetNewAreaDataSQL);
          LDataSet.ClearQueryParams();

          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['FileName'], [LIncData.FileName]);
          LDataSet.SetParams(['Area'], [FloatToStr(LIncData.Area)]);
          LDataset.ExecSQL;
        end;
      end;
    finally
      LSQLAgent.Free;
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaDataLoadAgent.LoadDataFromDB(AStomsaData:TStomsaData): boolean;
const OPNAME = 'TStomsaDataLoadAgent.LoadDataFromDB';
var
  LSQLAgent : TStomsaDataSQLAgent;
  LDataSet  : TAbstractModelDataset;
  LPath,
  LFileName : string;
  LArea     : double;
  LIncData  : TIncData;
begin
  Result := False;
  try
    LSQLAgent := TStomsaDataSQLAgent.Create(FAppModules);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet)  then
      begin
        LPath := FAppModules.StudyArea.DataFilesPath;
        LDataSet.SetSQL(LSQLAgent.GetAreaDataSQL);
        LDataset.DataSet.Open;
        while not LDataset.DataSet.Eof do
        begin
          LFileName := Trim(LDataset.DataSet.FieldByName('FileName').AsString);
          LArea     := LDataset.DataSet.FieldByName('Area').AsFloat;
          LIncData  := AStomsaData.FindIncFileByName(LPath,LFileName);
          if Assigned(LIncData) then
             LIncData.Area := LArea;
          LDataset.DataSet.Next;
        end;
      end;
    finally
      LSQLAgent.Free;
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
