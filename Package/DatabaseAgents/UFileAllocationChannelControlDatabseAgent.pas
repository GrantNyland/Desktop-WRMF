//
//  UNIT      : Contains TFileAllocationChannelControlDatabseAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileAllocationChannelControlDatabseAgent;

interface
uses
  Classes, sysutils,Db,
  VCL.Dialogs,
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UFileNameConstants,
  UDWADBComponents,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAllocationChannelControlFileDataObject,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type
  TFileAllocationChannelControlDatabseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadUnkownDataSQL: string;
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

{ TFileAllocationChannelControlDatabseAgent }

function TFileAllocationChannelControlDatabseAgent.ReadUnkownDataSQL: string;
CONST OPNAME = 'TFileAllocationChannelControlDatabseAgent.ReadUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+ QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  1' +
              ' AND FileGroup     =  '+ IntToStr(fgAllocationChannel)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
     except on E: Exception do HandleError(E,OPNAME) end;
end;

function TFileAllocationChannelControlDatabseAgent.WriteUnknownDataSQL: string;
CONST OPNAME = 'TFileAllocationChannelControlDatabseAgent.WriteUnknownDataSQL';
begin

    Result := '';
    try
      Result := 'INSERT INTO WRYMFileLines'+
                ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
                ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,'+
                ' :LineData)';
    except on E: Exception do HandleError(E,OPNAME) end;
end;

function TFileAllocationChannelControlDatabseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): boolean;
CONST OPNAME = 'TFileAllocationChannelControlDatabseAgent.ReadModelDataFromDatabase';
var
  LMessage : String;
  LDataSet : TAbstractModelDataset;
  LStop    : boolean;
  LAllocationObject  : TAllocationChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin

  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlDatabseAgent.strReadStarted'); // Update

    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LAllocationObject:= LPlanningFileDataObject.AllocationChannelObject;
    if not LAllocationObject.Initialise then
      exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadUnkownDataSQL);
      LDataSet.DataSet.Open;
      while not (LDataSet.DataSet.Eof) do
      begin
        LAllocationObject.HDextraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlDatabseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TFileAllocationChannelControlDatabseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAllocationChannelControlDatabseAgent.WriteModelDataToDatabase';
var
  LMessage   : string;
  LDataSet   : TAbstractModelDataset;
  LStop      : boolean;
  LCount     : integer;
  LHydroAllocation : TAllocationChannelControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin

  Result := False;
  try
     if not Assigned(AProgressFunction) then
        AProgressFunction := DummyShowProgress;

     LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlDatabseAgent.strWriteStarted'); // Update
     AProgressFunction(LMessage,ptNone,LStop);

     if not Assigned(ADataObject) then
       raise Exception.Create('File object parameter is not yet assigned.');

     if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
        Exit;

     LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
     LHydroAllocation  := LPlanningFileDataObject.AllocationChannelObject;
     if LHydroAllocation = nil then
       Exit;
     try
       FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
         for LCount := 0 to LHydroAllocation.HDextraLines.Count - 1 do
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
           LDataSet.SetParams(['LineData'], [LHydroAllocation.HDextraLines[LCount]]);
           LDataSet.ExecSQL;
         end;
         LDataSet.DataSet.Close;

       Result := InsertFileName(AFileName);
       if Result then
       begin
         LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlDatabseAgent.strWriteEnded');
         AProgressFunction(LMessage,ptNone,LStop);
       end;
       LDataSet.DataSet.Close;
     finally
       LDataSet.Free;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAllocationChannelControlDatabseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean): boolean;
const OPNAME = 'TFileAllocationChannelControlDatabseAgent.ClearModelDataInDatabase';
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

 