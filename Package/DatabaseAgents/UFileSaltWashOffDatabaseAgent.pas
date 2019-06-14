//
//
//  UNIT      : Contains TFileSaltWashOffDatabaseAgent Class
//  AUTHOR    : Lethabo Phatedi(Cornastone)
//  DATE      : 14/10/2016
//  COPYRIGHT : Copyright © 2016 DWS
//
//
unit UFileSaltWashOffDatabaseAgent;

interface

uses
  Classes,
  sysutils,
  Db,

  // DWS VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  USaltWashOffObject,
  UAbstractWrpmDatabaseAgent,
  UYieldModelDataObject,
  UPlanningFileDataObjects;


type

  TFileSaltWashOffDatabaseAgent = class(TAbstractWrpmDatabaseAgent)
  protected
    //Read and write SQL functions
    function ReadSaltWOffUnkownDataSQL : String;
    function WriteSaltWOffUnKnownSQL   : String;

    public
    { Public declarations }
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
     UErrorHandlingOperations;

 { TFileSaltWashOffDatabaseAgent }

function TFileSaltWashOffDatabaseAgent.WriteSaltWOffUnKnownSQL: string;
CONST OPNAME = 'TFileSaltWashOffDatabaseAgent.WriteSaltWOffUnKnownSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRPMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileSaltWashOffDatabaseAgent.ReadSaltWOffUnkownDataSQL: string;
CONST OPNAME = 'TFileSaltWashOffDatabaseAgent.ReadSaltWOffUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData  '+
              ' FROM WRPMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileSaltWashOffDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileSaltWashOffDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage: string;
  LStop                    : boolean;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LSaltsWashOff            : TSaltWashOffObject;
  LDataSet                 : TAbstractModelDataset;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileSaltWashOffDatabaseAgent.strReadStarted');

    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    //LSaltsWashOff := TPlanningFileDataObjects(ADataObject).SaltWashOffObject;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LSaltsWashOff    := LPlanningFileDataObject.AddSaltWashOffObject(AFileName.FileNumber);

    if not LSaltsWashOff.Initialise then
      exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadSaltWOffUnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not (LDataSet.DataSet.Eof) do
      begin
        LSaltsWashOff.FFileSaltWashOff01ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileSaltWashOffDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;

    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileSaltWashOffDatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
Const OPNAME = 'TFileSaltWashOffDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage                : string;
  LDataSet                : TAbstractModelDataset;
  LStop                   : boolean;
  LCount                  : integer;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LSaltWashOff            : TSaltWashOffObject;
begin
 Result := False;
  try
     if not Assigned(AProgressFunction) then
        AProgressFunction := DummyShowProgress;

     LMessage := FAppModules.Language.GetString('TFileSaltWashOffDatabaseAgent.strWriteStarted');
     AProgressFunction(LMessage,ptNone,LStop);

     if not Assigned(TPlanningFileDataObjects(ADataObject)) then
       raise Exception.Create('File object parameter is not yet assigned.');

     if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
     Exit;

     //LSaltWashOff := TPlanningFileDataObjects(ADataObject).SaltWashOffObject;

     LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
     LSaltWashOff             := LPlanningFileDataObject.SaltWashOffObjectByFileNumber[AFileName.FileNumber];

     if LSaltWashOff = nil then
       Exit;

       FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
     try

        begin
         for LCount := 0 to LSaltWashOff.FFileSaltWashOff01ExtraLines.Count - 1 do
         begin

           LDataSet.DataSet.Close;
           LDataSet.SetSQL(WriteSaltWOffUnKnownSQL);
           LDataset.ClearQueryParams();
           LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
           LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
           LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
           LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
           LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
           LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
           LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
           LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
           LDataSet.SetParams(['LineData'], [LSaltWashOff.FFileSaltWashOff01ExtraLines[LCount]]);
           LDataSet.ExecSQL;
         end;
         LDataSet.DataSet.Close;

       Result := InsertFileName(AFileName);
       if Result then
       begin
         LMessage := FAppModules.Language.GetString('TFileSaltWashOffDatabaseAgent.strWriteEnded');
         AProgressFunction(LMessage,ptNone,LStop);
       end;
       LDataSet.DataSet.Close;
      end;
     finally
       LDataSet.Free;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileSaltWashOffDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileSaltWashOffDatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LWhereClause,
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractWrpmDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    LWhereClause := ' And FileType = ' + IntToStr(AFileName.FileNumber);
    LTableNames := 'WRPMFileLines';
    Result := DeleteWrpmModelData(LTableNames,LWhereClause,AProgressFunction,AQuetly);
    Result := Result and DeleteWrpmUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractWrpmDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
