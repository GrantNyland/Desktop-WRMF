//
//
//  UNIT      : Contains TFile06DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile06DatabaseAgent;

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
  UReservoirInitialLevelsObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile06DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF06UnkownDataSQL: string;
    function ReadReservoirInitialLevelsDataSQL: string;

    function WriteReservoirInitialLevelsDataSQL: string;
    function WriteUnknownDataSQL: string;

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
  System.Contnrs,
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

function TFile06DatabaseAgent.ReadF06UnkownDataSQL: string;
const OPNAME = 'TFile06DatabaseAgent.ReadF06UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TFile06DatabaseAgent.ReadReservoirInitialLevelsDataSQL: string;
const OPNAME = 'TFile06DatabaseAgent.ReadReservoirInitialLevelsDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Rs.Model,Rs.StudyAreaName,Rs.SubArea,Rs.Scenario,Rs.Identifier,Rs.ReservoirNodeNumber'+
      ' ,Rs.ResInitialLevelsLev01,Rs.ResInitialLevelsLev02,Rs.ResInitialLevelsLev03,Rs.ResInitialLevelsLev04'+
      ' ,Rs.ResInitialLevelsLev05,Rs.ResInitialLevelsLev06,Rs.ResInitialLevelsLev07,Rs.ResInitialLevelsLev08'+
      ' ,Rs.ResInitialLevelsComment'+
      ' FROM ReservoirInitialLevels Rs'+
      ' WHERE  (Rs.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (Rs.StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (Rs.SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Rs.Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
      ' ORDER BY Rs.Model,Rs.StudyAreaName,Rs.SubArea,Rs.Scenario,Rs.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile06DatabaseAgent.WriteReservoirInitialLevelsDataSQL: string;
const OPNAME = 'TFile06DatabaseAgent.WriteReservoirInitialLevelsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReservoirInitialLevels'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ReservoirNodeNumber'+
              ' ,ResInitialLevelsLev01,ResInitialLevelsLev02,ResInitialLevelsLev03,ResInitialLevelsLev04'+
              ' ,ResInitialLevelsLev05,ResInitialLevelsLev06,ResInitialLevelsLev07,ResInitialLevelsLev08'+
              ',ResInitialLevelsComment)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ReservoirNodeNumber'+
              ' ,:ResInitialLevelsLev01,:ResInitialLevelsLev02,:ResInitialLevelsLev03,:ResInitialLevelsLev04'+
              ' ,:ResInitialLevelsLev05,:ResInitialLevelsLev06,:ResInitialLevelsLev07,:ResInitialLevelsLev08'+
              ' ,:ResInitialLevelsComment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile06DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile06DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile06DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile06DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LCount : Integer;
  LReservoirInitialLevelValuesObject: TReservoirInitialLevelValuesObject;
  LReservoirInitialLevelValues:TReservoirInitialLevelValues;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile06DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LReservoirInitialLevelValuesObject := ADataObject.FReservoirInitialLevelValuesObject;

    if not LReservoirInitialLevelValuesObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadReservoirInitialLevelsDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile06DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LReservoirInitialLevelValues := TReservoirInitialLevelValues.Create;
          LReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines.Add(LReservoirInitialLevelValues);

          if not LDataSet.DataSet.FieldByName('ReservoirNodeNumber').IsNull then
          begin
            LReservoirInitialLevelValues.FReservoirNodeNumber.FData :=LDataSet.DataSet.FieldByName('ReservoirNodeNumber').AsInteger;
            LReservoirInitialLevelValues.FReservoirNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ResInitialLevelsComment').IsNull then
          begin
            LReservoirInitialLevelValues.FComment.FData   := TrimRight(LDataSet.DataSet.FieldByName('ResInitialLevelsComment').AsString);
            LReservoirInitialLevelValues.FComment.FLength := Length(LReservoirInitialLevelValues.FComment.FData);
            LReservoirInitialLevelValues.FComment.FInitalised := True;
          end;

          for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
          begin

            LFieldName := Format('%s%2.2d',['ResInitialLevelsLev',LCount]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FInitalised := True;
            end
            else
              Break;
          end;
          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF06UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LReservoirInitialLevelValuesObject.FF06ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile06DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile06DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile06DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount,
  LCounter : integer;
  LDataSet : TAbstractModelDataset;
  LReservoirInitialLevelValuesObject: TReservoirInitialLevelValuesObject;
  LReservoirInitialLevelValues:TReservoirInitialLevelValues;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile06DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LReservoirInitialLevelValuesObject := ADataObject.FReservoirInitialLevelValuesObject;
    if not Assigned(LReservoirInitialLevelValuesObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      for LLinesCount := 0 to LReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines.Count-1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReservoirInitialLevelsDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount)]);

        LReservoirInitialLevelValues := TReservoirInitialLevelValues(
                                       LReservoirInitialLevelValuesObject.FReservoirInitialLevelsLines[LLinesCount]);

        if LReservoirInitialLevelValues.FReservoirNodeNumber.FInitalised then
         LDataSet.SetParams(['ReservoirNodeNumber'], [IntToStr(LReservoirInitialLevelValues.FReservoirNodeNumber.FData)]);

        if LReservoirInitialLevelValues.FComment.FInitalised then
         LDataSet.SetParams(['ResInitialLevelsComment'], [LReservoirInitialLevelValues.FComment.FData]);

        for LCount := MinReservoirInitialLevel to MaxReservoirInitialLevel do
        begin
          if not LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FInitalised then
            Break;

          LFieldName := Format('%s%2.2d',['ResInitialLevelsLev',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LReservoirInitialLevelValues.FReservoirInitialLevelValues[LCount].FData)]);
        end;
         LDataSet.ExecSQL;
      end;

      for LCounter := 0 to LReservoirInitialLevelValuesObject.FF06ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteUnknownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LReservoirInitialLevelValuesObject.FF06ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile06DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile06DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile06DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'ReservoirInitialLevels';
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

end.
