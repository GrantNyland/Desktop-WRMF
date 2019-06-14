//
//  UNIT      : Contains TFileControlledReleaseStructureDatabaseAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileControlledReleaseStructureDatabaseAgent;

interface
uses
  Classes, sysutils,Db,
  VCL.Dialogs,
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDWADBComponents,
  UFileNameConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UControlledReleaseStructureFileDataObject,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type
  TFileControlledReleaseStructureDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadUnkownDataSQL(AFileNumber:Integer): string;
    function WriteUnknownDataSQL: string;
  public
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;

end;

implementation
uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations, UBasicObjects, Math;

function TFileControlledReleaseStructureDatabaseAgent.ReadUnkownDataSQL(AFileNumber:Integer): string;
CONST OPNAME = 'TFileControlledReleaseStructureDatabaseAgent.ReadUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+ QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  ' + IntToStr(AFileNumber) +
              ' AND FileGroup     =  '+ IntToStr(fgReleaseStructure)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
     except on E: Exception do HandleError(E,OPNAME) end;

end;

function TFileControlledReleaseStructureDatabaseAgent.WriteUnknownDataSQL: string;
CONST OPNAME = 'TFileControlledReleaseStructureDatabaseAgent.WriteUnknownDataSQL';
begin

    Result := '';
    try
      Result := 'INSERT INTO WRYMFileLines'+
                ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
                ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,'+
                ' :LineData)';
    except on E: Exception do HandleError(E,OPNAME) end;
end;

function TFileControlledReleaseStructureDatabaseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): boolean;
CONST OPNAME = 'TFileControlledReleaseStructureDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage : String;
  LDataSet : TAbstractModelDataset;
  LStop    : boolean;
  LControlReleaseObject   : TControlledReleaseStructureFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;

begin

  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileControlledReleaseStructureDatabaseAgent.strReadStarted'); // Update

    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LControlReleaseObject:= LPlanningFileDataObject.ControlReleaseStructure;

    if not LControlReleaseObject.Initialise then
      exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadUnkownDataSQL(AFileName.FileNumber));
      LDataSet.DataSet.Open;
      while not (LDataSet.DataSet.Eof) do
      begin
        LControlReleaseObject.HDextraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileControlledReleaseStructureDatabaseAgent.strReadEnded'); // Update
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileControlledReleaseStructureDatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileControlledReleaseStructureDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage   : string;
  LDataSet   : TAbstractModelDataset;
  LStop      : boolean;
  LCount     : integer;
  LControlRelease : TControlledReleaseStructureFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin

  Result := False;
  try
     if not Assigned(AProgressFunction) then
        AProgressFunction := DummyShowProgress;

     LMessage := FAppModules.Language.GetString('TFileControlledReleaseStructureDatabaseAgent.strWriteStarted'); // Update
     AProgressFunction(LMessage,ptNone,LStop);

     if not Assigned(ADataObject) then
       raise Exception.Create('File object parameter is not yet assigned.');

     if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
        Exit;

     LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
     LControlRelease  := LPlanningFileDataObject.ControlReleaseStructure;

     if LControlRelease = nil then
        Exit;
   try
     FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
       for LCount := 0 to LControlRelease.HDextraLines.Count - 1 do
       begin
         LDataSet.DataSet.Close;
         LDataSet.SetSQL(WriteUnknownDataSQL);
         LDataset.ClearQueryParams();
         LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
         LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
         LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
         LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
         LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
         LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
         LDataSet.SetParams(['LineNumber'], [IntToStr(LCount + 1)]);
         LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
         LDataSet.SetParams(['LineData'], [LControlRelease.HDextraLines[LCount]]);
         LDataSet.ExecSQL;
       end;
       LDataSet.DataSet.Close;

       Result := InsertFileName(AFileName);
       if Result then
       begin
          LMessage := FAppModules.Language.GetString('TFileControlledReleaseStructureDatabaseAgent.strWriteEnded'); // Update
          AProgressFunction(LMessage,ptNone,LStop);
       end;
       LDataSet.DataSet.Close;
     finally
       LDataSet.Free;
     end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileControlledReleaseStructureDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean): boolean;
const OPNAME = 'TFileControlledReleaseStructureDatabaseAgent.ClearModelDataInDatabase';
var
  LMessage: string;
  LStop: boolean;
begin

  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    Result := DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


