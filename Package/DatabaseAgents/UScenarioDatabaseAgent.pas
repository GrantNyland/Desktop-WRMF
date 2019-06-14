//
//
//  UNIT      : Contains TScenarioDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 04/04/2003
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UScenarioDatabaseAgent;

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
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TScenarioDatabaseAgent = class(TAbstractDatabaseAgent)
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
    function SetFilesLoaded(AFilesLoaded: boolean): boolean;
  end;


implementation

uses UStudyArea,
     UUtilities,
     UDataSetType,
     UErrorHandlingOperations;


{ TScenarioDatabaseAgent }

function TScenarioDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean): boolean;
const OPNAME = 'TScenarioDatabaseAgent.ClearModelDataInDatabase';
begin
  Result := False;
  try
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioDatabaseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TScenarioDatabaseAgent.ReadModelDataFromDatabase';
begin
  Result := False;
  try
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioDatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TScenarioDatabaseAgent.WriteModelDataToDatabase';
begin
  Result := False;
  try
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioDatabaseAgent.SetFilesLoaded(AFilesLoaded: boolean): boolean;
const OPNAME = 'TScenarioDatabaseAgent.SetFilesLoaded';
var
  LDataSet : TAbstractModelDataset;
  LFilesLoaded: string;
begin
  Result := False;
  try
    if AFilesLoaded then
      LFilesLoaded  := 'Y'
    else
      LFilesLoaded := 'N';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL('UPDATE StudyScenario SET FilesLoaded = '+ QuotedStr(LFilesLoaded)+
                      ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                      ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                      ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                      ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode));
      LDataSet.ExecSQL;
      TStudyArea(FAppModules.StudyArea).FilesLoaded := AFilesLoaded;
    finally
      LDataSet.Free;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
