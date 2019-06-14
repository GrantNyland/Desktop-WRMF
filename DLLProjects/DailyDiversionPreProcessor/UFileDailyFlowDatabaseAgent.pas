unit UFileDailyFlowDatabaseAgent;
interface
uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  sysutils,
  Db,
  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UDailyFlowDataObject,
  UDailyDiversionFileDataObject;

type

  TFileDailyFlowDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function WriteDailyFlowDataSQL : string;
    function ReadDailyFlowDataSQL: string;
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
uses
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;


{ TFileDailyFlowDatabaseAgent }

function TFileDailyFlowDatabaseAgent.ClearModelDataInDatabase(
  AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
  AQuetly: boolean): boolean;
const OPNAME = 'TFileDailyFlowDatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
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

    LTableNames := 'RunTitle,AnlySequences,DaysPerMonth,MaxYield,MonthNames,RunParameters,TargetPower,TargetYield';
    if(FAppModules.Model.ModelName = CDailyDiversion) then
      LTableNames := LTableNames + ',DailyDiversionFileData';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);


    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyFlowDatabaseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
                                                               AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyFlowDatabaseAgent.ReadModelDataFromDatabase';                                                               
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
  LDailyFlowDataObject : TDailyFlowDataObject;
  LDailyFlowData : TDailyFlowData;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDailyFlowDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LDailyDiversionFileDataObject  := TDailyDiversionFileDataObject(ADataObject);
    LDailyFlowDataObject := LDailyDiversionFileDataObject.FDailyFlowDataObject;
    if LDailyFlowDataObject <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.DataSet.Close;
        LDataSet.ClearSQL;
        LDataSet.SetSQL(ReadDailyFlowDataSQL);

        while not LDataSet.DataSet.Eof do
        begin
          LDailyFlowDataObject.FStationID := LDataSet.DataSet.FieldByName('StationID').AsInteger;
          LDailyFlowData := LDailyFlowDataObject.AddDailyFlowData;
          LDailyFlowData.Initialise;

          LDailyFlowData.FIdentifier.FData := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LDailyFlowData.FIdentifier.FInitalised := True;
          if not LDataSet.DataSet.FieldByName('DiversionDate').IsNull then
          begin
            LDailyFlowData.FDiversionDate.FData := DateToStr(LDataSet.DataSet.FieldByName('DiversionDate').AsDateTime);
            LDailyFlowData.FDiversionDate.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('AvgFlow').IsNull then
          begin
            LDailyFlowData.FAvgFlow.FData := LDataSet.DataSet.FieldByName('AvgFlow').AsFloat;
            LDailyFlowData.FAvgFlow.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('QualityCode').IsNull then
          begin
            LDailyFlowData.FQualityCode.FData := LDataSet.DataSet.FieldByName('QualityCode').AsInteger;
            LDailyFlowData.FQualityCode.FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;

      finally
        FreeAndNil(LDataSet);
      end;
    end;
    LMessage := FAppModules.Language.GetString('TFileDailyFlowDatabaseAgent.strReadEnded');
    AProgressFunction(LMessage,ptNone,LStop);
    Result :=  True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileDailyFlowDatabaseAgent.WriteDailyFlowDataSQL: string;
const OPNAME = 'TFileDailyFlowDatabaseAgent.WriteDailyFlowDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DailyDiversionFileData '+
              ' (Model,StudyAreaName,SubArea,Scenario,StationID,Identifier,DiversionDate,AvgFlow,QualityCode)'+
              ' VALUES '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:StationID,:Identifier,:DiversionDate,:AvgFlow,:QualityCode)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyFlowDatabaseAgent.ReadDailyFlowDataSQL: string;
const OPNAME = 'TFileDailyFlowDatabaseAgent.ReadDailyFlowDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,StationID,DiversionDate,AvgFlow,QualityCode'+
              ' FROM DailyDiversionFileData'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileDailyFlowDatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
                                                              AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyFlowDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage : string;
  LDataSet : TAbstractModelDataset;
  LStop : boolean;
  LIndex : integer;
  LDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
  LDailyFlowDataObject : TDailyFlowDataObject;
  LDailyFlowData : TDailyFlowData;
  LDiversionDate : TDateTime;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;
    LMessage := FAppModules.Language.GetString('TFileDailyFlowDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);
    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
    LDailyDiversionFileDataObject  := TDailyDiversionFileDataObject(ADataObject);
    LDailyFlowDataObject := LDailyDiversionFileDataObject.FDailyFlowDataObject;
    if(LDailyFlowDataObject = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteDailyFlowDataSQL);
      for LIndex := 0 to LDailyFlowDataObject.DailyFlowDataCount -1 do
      begin
        LDailyFlowData := LDailyFlowDataObject.GetDailyFlowDataByIndex(LIndex);
        if LDailyFlowData <> nil then
        begin
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','StationID'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(LDailyFlowData.FIdentifier.FData), IntToStr(LDailyFlowDataObject.FStationID)]);

          if LDailyFlowData.FDiversionDate.FInitalised then
          begin
            LDiversionDate := EncodeDate(StrToInt(Copy(LDailyFlowData.FDiversionDate.FData, 1, 4)),
                                 StrToInt(Copy(LDailyFlowData.FDiversionDate.FData, 5, 2)),
                                 StrToInt(Copy(LDailyFlowData.FDiversionDate.FData, 7, 2)));
            LDataSet.SetParams(['DiversionDate'],[FormatDateTime('yyyy/mm/dd', LDiversionDate)]);
          end;

          if LDailyFlowData.FAvgFlow.FInitalised then
            LDataSet.SetParams(['AvgFlow'],[FloatToStr(LDailyFlowData.FAvgFlow.FData)])
          else
            LDataSet.SetParams(['AvgFlow'], [FloatToStr(NullFloat)]);

          if LDailyFlowData.FQualityCode.FInitalised then
            LDataSet.SetParams(['QualityCode'],[IntToStr(LDailyFlowData.FQualityCode.FData)]);

          LDataSet.ExecSQL;
        end;
      end;
      LMessage := FAppModules.Language.GetString('TFileDailyFlowDatabaseAgent.strWriteEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      FreeAndNil(LDataSet);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
