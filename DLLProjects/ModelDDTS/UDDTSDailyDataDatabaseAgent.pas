//
//
//  UNIT      : Contains TDDTSDailyDataDatabaseAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 02/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDDTSDailyDataDatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractModelData,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UDDTSDailyDataObject,
  UAbstractDatabaseAgent;

type

  TDDTSDailyDataDatabaseAgent = class(TAbstractDatabaseAgent)
    FFileType : TDailyFileType;
  protected
    function ReadInputDataFileDataSQL: string;
    function WriteInputDataFileDataSQL: string;
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;



function TDDTSDailyDataDatabaseAgent.ReadInputDataFileDataSQL: string;
const OPNAME = 'TDDTSDailyDataDatabaseAgent.ReadInputDataFileDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,RowID,DataDate,Runoff,OtherInflow,Rainfall,Evaporation,IncreamentalRunoff,EWR'+
              ' FROM DDTSInputData'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,RowID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataDatabaseAgent.WriteInputDataFileDataSQL: string;
const OPNAME = 'TDDTSDailyDataDatabaseAgent.WriteInputDataFileDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DDTSInputData'+
              ' (Model,StudyAreaName,SubArea,Scenario,RowID,DataDate,Runoff,OtherInflow,Rainfall,Evaporation,IncreamentalRunoff,EWR)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:RowID,:DataDate,:Runoff,:OtherInflow,:Rainfall,:Evaporation,:IncreamentalRunoff,:EWR)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSDailyDataDatabaseAgent.ReadModelDataFromDatabase';
var
  LStop           : boolean;
  LMessage        : string;
  LDataSet        : TAbstractModelDataset;
  LDailyDataObject: TDDTSDailyDataObject;
  LCombinedDailyDataObject: TCombinedDailyDataObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDDTSDailyDataDatabaseAgent.strReadStarted');
    //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LDailyDataObject := ADataObject.FDDTSDailyDataObject;
    if not LDailyDataObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadInputDataFileDataSQL);
      LDataSet.DataSet.Open;

      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TDDTSDailyDataDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      //  Exit;
      end;

      while not LDataSet.DataSet.Eof do
      begin
        LCombinedDailyDataObject                         := LDailyDataObject.AddCombinedDailyDataObject;
        LCombinedDailyDataObject.DailyDate               := NullDateTime;
        LCombinedDailyDataObject.RunoffValue             := NullFloat;
        LCombinedDailyDataObject.OtherInflowValue        := NullFloat;
        LCombinedDailyDataObject.IncreamentalRunoffValue := NullFloat;
        LCombinedDailyDataObject.EWRValue                := NullFloat;
        LCombinedDailyDataObject.RainfallValue           := NullFloat;
        LCombinedDailyDataObject.EvaporationValue        := NullFloat;

        if not LDataSet.DataSet.FieldByName('DataDate').IsNull then
          LCombinedDailyDataObject.DailyDate := LDataSet.DataSet.FieldByName('DataDate').AsDateTime;
        if not LDataSet.DataSet.FieldByName('Runoff').IsNull then
          LCombinedDailyDataObject.RunoffValue := LDataSet.DataSet.FieldByName('Runoff').AsFloat;
        if not LDataSet.DataSet.FieldByName('OtherInflow').IsNull then
          LCombinedDailyDataObject.OtherInflowValue := LDataSet.DataSet.FieldByName('OtherInflow').AsFloat;
        if not LDataSet.DataSet.FieldByName('Rainfall').IsNull then
          LCombinedDailyDataObject.RainfallValue := LDataSet.DataSet.FieldByName('Rainfall').AsFloat;
        if not LDataSet.DataSet.FieldByName('Evaporation').IsNull then
          LCombinedDailyDataObject.EvaporationValue := LDataSet.DataSet.FieldByName('Evaporation').AsFloat;
        if not LDataSet.DataSet.FieldByName('IncreamentalRunoff').IsNull then
          LCombinedDailyDataObject.IncreamentalRunoffValue := LDataSet.DataSet.FieldByName('IncreamentalRunoff').AsFloat;
        if not LDataSet.DataSet.FieldByName('EWR').IsNull then
          LCombinedDailyDataObject.EWRValue := LDataSet.DataSet.FieldByName('EWR').AsFloat;

         LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      if(LDailyDataObject.CombinedDailyDataCount > 0) then
         LDailyDataObject.SplitCombinedDailyData;

      LMessage := FAppModules.Language.GetString('TDDTSDailyDataDatabaseAgent.strReadEnded');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSDailyDataDatabaseAgent.WriteModelDataToDatabase';
var
  LStop           : boolean;
  LIndex          : integer;
  LMessage        : string;
  LDataSet        : TAbstractModelDataset;
  LDailyDataObject: TDDTSDailyDataObject;
  LCombinedDailyDataObject: TCombinedDailyDataObject;
  LConfigFileNames : TFileNamesList;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDDTSDailyDataDatabaseAgent.strWriteStarted');
    //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LDailyDataObject := ADataObject.FDDTSDailyDataObject;
    LDailyDataObject.GenerateCombinedDailyData;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LIndex := 0  to  LDailyDataObject.CombinedDailyDataCount - 1  do
      begin
        if((LIndex mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LCombinedDailyDataObject := LDailyDataObject.CombinedDailyDataObjectByIndex[LIndex];;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteInputDataFileDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(
          ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
          [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['RowID'], [IntToStr(LIndex+1)]);
        LDataSet.SetParams(['DataDate'], [DateToStr(LCombinedDailyDataObject.DailyDate)]);

        if not (LCombinedDailyDataObject.RunoffValue = NullFloat)  then
          LDataSet.SetParams(['Runoff'], [FormatFloat('##0.000',LCombinedDailyDataObject.RunoffValue)]);

        if not (LCombinedDailyDataObject.OtherInflowValue = NullFloat)  then
          LDataSet.SetParams(['OtherInflow'], [FormatFloat('##0.000',LCombinedDailyDataObject.OtherInflowValue)]);

        if not (LCombinedDailyDataObject.RainfallValue = NullFloat)  then
          LDataSet.SetParams(['Rainfall'], [FormatFloat('##0.000',LCombinedDailyDataObject.RainfallValue)]);

        if not (LCombinedDailyDataObject.EvaporationValue = NullFloat)  then
          LDataSet.SetParams(['Evaporation'], [FormatFloat('##0.000',LCombinedDailyDataObject.EvaporationValue)]);

        if not (LCombinedDailyDataObject.IncreamentalRunoffValue = NullFloat)  then
          LDataSet.SetParams(['IncreamentalRunoff'], [FormatFloat('##0.000',LCombinedDailyDataObject.IncreamentalRunoffValue)]);

        if not (LCombinedDailyDataObject.EWRValue = NullFloat)  then
          LDataSet.SetParams(['EWR'], [FormatFloat('##0.000',LCombinedDailyDataObject.EWRValue)]);
        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;
    finally
      LDataSet.Free;
    end;

    LConfigFileNames := TModelFileNames(TAbstractModelData(FAppModules.Model.ModelData).FileNamesObject).CastConfigFileNames;
    Result :=            InsertFileName(LConfigFileNames.CastFileObject[0]);
    Result := Result and InsertFileName(LConfigFileNames.CastFileObject[1]);
    Result := Result and InsertFileName(LConfigFileNames.CastFileObject[2]);
    Result := Result and InsertFileName(LConfigFileNames.CastFileObject[3]);
    Result := Result and InsertFileName(LConfigFileNames.CastFileObject[4]);
    Result := Result and InsertFileName(LConfigFileNames.CastFileObject[5]);
    if Result then
    begin
      LMessage := FAppModules.Language.GetString('TDDTSDailyDataDatabaseAgent.strWriteEnded');
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion;AQuetly: boolean = False): boolean;
const OPNAME = 'TDDTSDailyDataDatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
  LConfigFileNames : TFileNamesList;
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

    //if not Assigned(AFileName) then
    //  raise Exception.Create('File name object parameter is not yet assigned.');

    LTableNames := 'DDTSInputData';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    //Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);

    LConfigFileNames := TModelFileNames(TAbstractModelData(FAppModules.Model.ModelData).FileNamesObject).CastConfigFileNames;
    Result :=            DeleteFileName(LConfigFileNames.CastFileObject[0]);
    Result := Result and DeleteFileName(LConfigFileNames.CastFileObject[1]);
    Result := Result and DeleteFileName(LConfigFileNames.CastFileObject[2]);
    Result := Result and DeleteFileName(LConfigFileNames.CastFileObject[3]);
    Result := Result and DeleteFileName(LConfigFileNames.CastFileObject[4]);
    Result := Result and DeleteFileName(LConfigFileNames.CastFileObject[5]);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
