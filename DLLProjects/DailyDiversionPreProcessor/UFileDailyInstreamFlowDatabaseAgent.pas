unit UFileDailyInstreamFlowDatabaseAgent;
interface
uses
  Classes,
  VCL.ComCtrls,
  VCL.Controls,
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
  UDailyInstreamFlowDataObject,
  UDailyDiversionFileDataObject;

type

  TFileDailyInstreamFlowDatabaseAgent = class(TAbstractDatabaseAgent)
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


{ TFileDailyInstreamFlowDatabaseAgent }

function TFileDailyInstreamFlowDatabaseAgent.ClearModelDataInDatabase(
  AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
  AQuetly: boolean): boolean;
const OPNAME = 'TFileDailyInstreamFlowDatabaseAgent.ClearModelDataInDatabase';
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
      LTableNames := LTableNames + ',DailyInstreamFileData';
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

function TFileDailyInstreamFlowDatabaseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
                                                               AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyInstreamFlowDatabaseAgent.ReadModelDataFromDatabase';                                                               
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
  LDailyInstreamFlowDataObject : TDailyInstreamFlowDataObject;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LDailyDiversionFileDataObject  := TDailyDiversionFileDataObject(ADataObject);
    LDailyInstreamFlowDataObject := LDailyDiversionFileDataObject.FDailyInstreamFlowDataObject;
    if LDailyInstreamFlowDataObject <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.DataSet.Close;
        LDataSet.ClearSQL;
        LDataSet.SetSQL(ReadDailyFlowDataSQL);

        while not LDataSet.DataSet.Eof do
        begin
          LDailyInstreamFlowDataObject.FStationID := LDataSet.DataSet.FieldByName('StationID').AsInteger;
          LDailyInstreamFlowData := LDailyInstreamFlowDataObject.AddDailyInstreamFlowData;
          LDailyInstreamFlowData.Initialise;

          LDailyInstreamFlowData.FIdentifier.FData := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          LDailyInstreamFlowData.FIdentifier.FInitalised := True;
          if not LDataSet.DataSet.FieldByName('InstreamDate').IsNull then
          begin
            LDailyInstreamFlowData.FInstreamDate.FData := DateToStr(LDataSet.DataSet.FieldByName('InstreamDate').AsDateTime);
            LDailyInstreamFlowData.FInstreamDate.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('AvgFlow').IsNull then
          begin
            LDailyInstreamFlowData.FAvgFlow.FData := LDataSet.DataSet.FieldByName('AvgFlow').AsFloat;
            LDailyInstreamFlowData.FAvgFlow.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('QualityCode').IsNull then
          begin
            LDailyInstreamFlowData.FQualityCode.FData := LDataSet.DataSet.FieldByName('QualityCode').AsInteger;
            LDailyInstreamFlowData.FQualityCode.FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;

      finally
        FreeAndNil(LDataSet);
      end;
    end;
    LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDatabaseAgent.strReadEnded');
    AProgressFunction(LMessage,ptNone,LStop);
    Result :=  True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileDailyInstreamFlowDatabaseAgent.WriteDailyFlowDataSQL: string;
const OPNAME = 'TFileDailyInstreamFlowDatabaseAgent.WriteDailyFlowDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DailyInstreamFileData '+
              ' (Model,StudyAreaName,SubArea,Scenario,StationID,Identifier,InstreamDate,AvgFlow,QualityCode)'+
              ' VALUES '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:StationID,:Identifier,:InstreamDate,:AvgFlow,:QualityCode)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDailyInstreamFlowDatabaseAgent.ReadDailyFlowDataSQL: string;
const OPNAME = 'TFileDailyInstreamFlowDatabaseAgent.ReadDailyFlowDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,StationID,DiversionDate,AvgFlow,QualityCode'+
              ' FROM DailyInstreamFileData'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileDailyInstreamFlowDatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
                                                              AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDailyInstreamFlowDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage : string;
  LDataSet : TAbstractModelDataset;
  LStop : boolean;
  LIndex : integer;
  LDailyDiversionFileDataObject : TDailyDiversionFileDataObject;
  LDailyInstreamFlowDataObject : TDailyInstreamFlowDataObject;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LInstreamDate : TDateTime;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;
    LMessage := FAppModules.Language.GetString('TFileDailyInstreamFlowDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);
    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
//    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
//      Exit;
    LDailyDiversionFileDataObject  := TDailyDiversionFileDataObject(ADataObject);
    LDailyInstreamFlowDataObject := LDailyDiversionFileDataObject.FDailyInstreamFlowDataObject;
    if(LDailyInstreamFlowDataObject = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteDailyFlowDataSQL);
      for LIndex := 0 to LDailyInstreamFlowDataObject.DailyFlowDataCount -1 do
      begin
        LDailyInstreamFlowData := LDailyInstreamFlowDataObject.GetDailyInstreamFlowDataByIndex(LIndex);
        if LDailyInstreamFlowData <> nil then
        begin
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','StationID'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(LDailyInstreamFlowData.FIdentifier.FData), IntToStr(LDailyInstreamFlowDataObject.FStationID)]);

          if LDailyInstreamFlowData.FInstreamDate.FInitalised then
          begin
            LInstreamDate := EncodeDate(StrToInt(Copy(LDailyInstreamFlowData.FInstreamDate.FData, 1, 4)),
                                 StrToInt(Copy(LDailyInstreamFlowData.FInstreamDate.FData, 5, 2)),
                                 StrToInt(Copy(LDailyInstreamFlowData.FInstreamDate.FData, 7, 2)));
            LDataSet.SetParams(['InstreamDate'],[FormatDateTime('yyyy/mm/dd', LInstreamDate)]);
          end;

          if LDailyInstreamFlowData.FAvgFlow.FInitalised then
            LDataSet.SetParams(['AvgFlow'],[FloatToStr(LDailyInstreamFlowData.FAvgFlow.FData)])
          else
            LDataSet.SetParams(['AvgFlow'], [FloatToStr(NullFloat)]);


          if LDailyInstreamFlowData.FQualityCode.FInitalised then
            LDataSet.SetParams(['QualityCode'],[IntToStr(LDailyInstreamFlowData.FQualityCode.FData)]);

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
