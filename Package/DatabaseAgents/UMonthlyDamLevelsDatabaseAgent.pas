//
//
//  UNIT      : Contains TMonthlyDamLevelsDatabaseAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 02/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMonthlyDamLevelsDatabaseAgent;

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
//  UMonthlyDamLevelsFilesObject,
  UMonthlyDamLevelsObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TMonthlyDamLevelsDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadMonthlyDamWaterLevelsFileDataSQL: string;
    function WriteMonthlyDamWaterLevelsFileDataSQL: string;
    function DeleteModelData(AFileName:TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean;  reintroduce; virtual;
    function DeleteModelDataSQL(AFileName:TFileNameObject): string; reintroduce; virtual;
    function MonthlyDamLevelsFileFoundInOtherStudies(AFileNamesObject: TFileNameObject): boolean; virtual;
    function MonthlyDamLevelsFileFoundInOtherStudiesSQL(AFileNamesObject: TFileNameObject): string; virtual;
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
     System.Variants,
     UErrorHandlingOperations;



function TMonthlyDamLevelsDatabaseAgent.ReadMonthlyDamWaterLevelsFileDataSQL: string;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.ReadMonthlyDamWaterLevelsFileDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Model,StudyAreaName,SubArea,Scenario,FileName,YearValue'+
      ' ,MonthValue01,MonthValue02,MonthValue03,MonthValue04,MonthValue05,MonthValue06'+
      ' ,MonthValue07,MonthValue08,MonthValue09,MonthValue10,MonthValue11,MonthValue12'+
      ' FROM  HistoricDamLevels '+
      ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
      ' AND FileName      = :FileName'+
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileName,YearValue';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.WriteMonthlyDamWaterLevelsFileDataSQL: string;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.WriteMonthlyDamWaterLevelsFileDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO HistoricDamLevels'+
              '( Model,StudyAreaName,SubArea,Scenario'+
              ' ,FileName,YearValue'+
              ' ,MonthValue01,MonthValue02,MonthValue03,MonthValue04,MonthValue05,MonthValue06'+
              ' ,MonthValue07,MonthValue08,MonthValue09,MonthValue10,MonthValue11,MonthValue12'+
              ')'+
              ' Values'+
              '( :Model,:StudyAreaName,:SubArea,:Scenario'+
              ' ,:FileName,:YearValue'+
              ' ,:MonthValue01,:MonthValue02,:MonthValue03,:MonthValue04,:MonthValue05,:MonthValue06'+
              ' ,:MonthValue07,:MonthValue08,:MonthValue09,:MonthValue10,:MonthValue11,:MonthValue12'+
              ')';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage             : string;
  LDataSet             : TAbstractModelDataset;
  LYearValue,
  LCount,
  LLinesCount          : Integer;
  LMonthlyDamLevelsObject : TMonthlyDamLevelsObject;
  LDamLevel: double;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsDatabaseAgent.strReadStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

//    ADataObject.FMonthlyDamLevelsObject.MonthlyDamLevelsContainer.AddMonthlyDamLevelsObjects;
    LMonthlyDamLevelsObject := ADataObject.FMonthlyDamLevelsObject;
    if not LMonthlyDamLevelsObject.Initialise then
    Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadMonthlyDamWaterLevelsFileDataSQL);
      LDataSet.SetParams(['FileName'], [UpperCase(Trim(AFileName.FileName))]);
      LDataSet.DataSet.Open;

      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      //  Exit;
      end;

      for LLinesCount := 0  to  LDataSet.DataSet.RecordCount - 1  do
      begin
        {
        if((LLinesCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;
         }
        if not LDataSet.DataSet.FieldByName('FileName').IsNull then
          TMonthlyDamLevelsObject(LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.MonthlyDamLevelsObjectByIndex [ LLinesCount ] ).DamWaterLevelFileName  :=
            Trim(LDataSet.DataSet.FieldByName('FileName').AsString);

        LYearValue := LDataSet.DataSet.FieldByName('YearValue').AsInteger;

        for LCount := MinMonths to MaxMonths do
        begin
          LFieldName := Format('%s%2.2d',['MonthValue',LCount]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LDamLevel := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.AddMonthlyDamLevels(LYearValue,LCount,LDamLevel);
          end;
        end;
        LDataSet.DataSet.next;
      end;
      LDataSet.DataSet.Close;
      LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsDatabaseAgent.strReadEnded');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount: integer;
  LDataSet : TAbstractModelDataset;
  LMonthlyDamLevelsObject : TMonthlyDamLevelsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsDatabaseAgent.strWriteStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LMonthlyDamLevelsObject := ADataObject.FMonthlyDamLevelsObject;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LLinesCount := 0  to  LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.ItemsCount - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteMonthlyDamWaterLevelsFileDataSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario'],
                           [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
                            FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['FileName'], [UpperCase(Trim(ExtractFileName(AFileName.FileName)))]);

        if TMonthlyDamLevels (LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.MonthlyDamLevelsObjectByIndex[LLinesCount]).FYear.FInitalised  then
          LDataSet.SetParams(['YearValue'], [IntToStr(TMonthlyDamLevels(LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.MonthlyDamLevelsObjectByIndex[LLinesCount]).FYear.FData)]);

        for LCount := MinMonths to MaxMonths do
        begin
          LFieldName := Format('%s%2.2d',['MonthValue',LCount]);
          if TMonthlyDamLevels (LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.MonthlyDamLevelsObjectByIndex[LLinesCount]).DamLevels[LCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(TMonthlyDamLevels(LMonthlyDamLevelsObject.MonthlyDamLevelsContainer.MonthlyDamLevelsObjectByIndex[LLinesCount]).DamLevels[LCount].FData)])
          else
           LDataSet.GetParamByName(LFieldName).Value := Unassigned;
        end;
        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;
    finally
      LDataSet.Free;
    end;

    Result := InsertFileName(AFileName);
    if Result then
    begin
      LMessage := FAppModules.Language.GetString('TMonthlyDamLevelsDatabaseAgent.strWriteEnded');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion;AQuetly: boolean = False): boolean;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.ClearModelDataInDatabase';
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

    if not IsFileFoundInMultipleStudies(AFileName) then
      Result := DeleteModelData(AFileName,AProgressFunction,AQuetly)
    else
      Result := True;
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.DeleteModelData(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.DeleteModelData';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LStop: boolean;
begin
  Result := False;

  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if (not Assigned(AFileName)) then
      raise Exception.Create('There are no tables to be cleared of model data.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if not AQuetly then
      begin
        LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strDeleteModelData');
        LMessage := Format(LMessage,['HistoricDamLevels']);
        AProgressFunction(LMessage,ptNone,LStop);
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteModelDataSQL(AFileName));
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.DeleteModelDataSQL(AFileName: TFileNameObject): string;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.DeleteModelDataSQL';
begin

  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM HistoricDamLevels'+
              ' WHERE FileName    =  '+QuotedStr(UpperCase(Trim(AFileName.FileName)))

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.MonthlyDamLevelsFileFoundInOtherStudies(AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.MonthlyDamLevelsFileFoundInOtherStudies';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(MonthlyDamLevelsFileFoundInOtherStudiesSQL(AFileNamesObject));
      LDataSet.DataSet.Open;
      Result :=(LDataSet.DataSet.RecordCount > 0);
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDamLevelsDatabaseAgent.MonthlyDamLevelsFileFoundInOtherStudiesSQL(AFileNamesObject: TFileNameObject): string;
const OPNAME = 'TMonthlyDamLevelsDatabaseAgent.MonthlyDamLevelsFileFoundInOtherStudiesSQL';
begin
  Result := '';
  try
    Result := 'SELECT FileName  '+
              ' FROM FileNames'+
              ' WHERE FileGroup   =  '+IntToStr(AFileNamesObject.FileGroup)+
              ' AND FileName    =  '+QuotedStr(UpperCase(Trim(AFileNamesObject.FileName)));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end. 
