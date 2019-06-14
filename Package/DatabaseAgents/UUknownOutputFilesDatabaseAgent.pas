//
//
//  UNIT      : Contains TUknownOutputFilesDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UUknownOutputFilesDatabaseAgent;

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
  UUknownDatabaseAgent,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TUknownOutputFilesDatabaseAgent = class(TUknownDatabaseAgent)
  protected
    function ReadUnkownOutputDataSQL(AFileName:TFileNameObject)  : string ;
    function WriteUnkownOutputDataSQL: string ;
    function DeleteUnknownModelDataSQL(AFileNamesObject: TFileNameObject): string; override;
    function DeleteUnknownModelData(AFileNamesObject: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;

  public
    { Public declarations }

    function ReadModelDataFromDatabase(AFileName:TFileNameObject; AFileData: TStrings;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;AFileData: TStrings;
             AProgressFunction: TProgressUpdateFuntion): boolean;  override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TUknownOutputFilesDatabaseAgent.ReadUnkownOutputDataSQL(AFileName:TFileNameObject) : string;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.ReadUnkownOutputDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,FileNumber,LineNumber,LineSection,LineData'+
              ' FROM suUnknownData'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileNumber      = '+IntToStr(AFileName.FileNumber)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TUknownOutputFilesDatabaseAgent.WriteUnkownOutputDataSQL: string;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.WriteUnkownOutputDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO suUnknownData'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileNumber,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileNumber,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUknownOutputFilesDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; AFileData: TStrings;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  //LNoData: Boolean;
  LStop: boolean;
  LCount : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;
    LMessage := FAppModules.Language.GetString('TUknownOutputFilesDatabaseAgent.strReadStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('File object parameter is not yet assigned.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadUnkownOutputDataSQL(TFileNameObject(AFilename)));
      LDataSet.DataSet.Open;


      //Check if there is any data.
      //LNoData := False;
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        //LNoData := True;
        LMessage := FAppModules.Language.GetString('TUknownOutputFilesDatabaseAgent.strNoDataReturned');
        LMessage := Format(LMessage,[TFileNameObject(AFilename).ShortName]);
        AProgressFunction(LMessage,ptError,LStop);
      end;

      LCount := 0;
      AFileData.Clear;
      while not LDataSet.DataSet.EOF do
      begin
        if((LCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;
        LCount := LCount + 1;

        AFileData.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TUknownOutputFilesDatabaseAgent.strReadEnded');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUknownOutputFilesDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;AFileData: TStrings;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LCount : Integer;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TUknownOutputFilesDatabaseAgent.strWriteStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteUnkownOutputDataSQL);
      LDataSet.ClearQueryParams();
      for LCount := 0 to  AFileData.Count - 1 do
      begin
        if((LCount mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileNumber'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCount + 1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [AFileData[LCount]]);
        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TUknownOutputFilesDatabaseAgent.strWriteEnded');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
     end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUknownOutputFilesDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean): boolean;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.ClearModelDataInDatabase';
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

function TUknownOutputFilesDatabaseAgent.DeleteUnknownModelData(AFileNamesObject: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean): boolean;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.DeleteUnknownModelData';
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
        LMessage := Format(LMessage,['suUnknownData']);
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

function TUknownOutputFilesDatabaseAgent.DeleteUnknownModelDataSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TUknownOutputFilesDatabaseAgent.DeleteUnknownModelDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM suUnknownData'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileNumber    =  '+ IntToStr(AFileNamesObject.FileNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
