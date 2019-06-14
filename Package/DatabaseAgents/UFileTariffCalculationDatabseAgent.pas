//
//  UNIT      : Contains TFileTariffCalculationDatabseAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileTariffCalculationDatabseAgent;

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
  UTariffCalculationFileDataObject,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type
  TFileTariffCalculationDatabseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadTariffCalculationConfigSQL: string;
    function WriteTariffCalculationConfigSQL: string;
    function ReadTariffCalculationSQL: string;
    function WriteTariffCalculationSQL: string;
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

{ TFileTariffCalculationDatabseAgent }

function TFileTariffCalculationDatabseAgent.ReadTariffCalculationConfigSQL: string;
const OPNAME = 'TFileTariffCalculationDatabseAgent.ReadTariffCalculationConfigSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,DataYears'+
              ' FROM TariffCalculationConfig'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.WriteTariffCalculationConfigSQL: string;
const OPNAME = 'TFileTariffCalculationDatabseAgent.WriteDisbenefitSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO TariffCalculationConfig'+
              ' (Model,StudyAreaName,SubArea,Scenario, DataYears)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:DataYears)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.ReadTariffCalculationSQL: string;
const OPNAME = 'TFileTariffCalculationDatabseAgent.ReadTariffCalculationSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber, Tariff,EscalationFactors'+
              ' FROM TariffCalculation'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.WriteTariffCalculationSQL: string;
const OPNAME = 'TFileTariffCalculationDatabseAgent.WriteTariffCalculationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO TariffCalculation'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber, Tariff,EscalationFactors)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier, :ChannelNumber, :Tariff, :EscalationFactors)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.ReadUnkownDataSQL: string;
CONST OPNAME = 'TFileTariffCalculationDatabseAgent.ReadUnkownDataSQL';
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
              ' AND FileGroup     =  '+ IntToStr(fgTariffCalculation)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
     except on E: Exception do HandleError(E,OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.WriteUnknownDataSQL: string;
CONST OPNAME = 'TFileTariffCalculationDatabseAgent.WriteUnknownDataSQL';
begin

    Result := '';
    try
      Result := 'INSERT INTO WRYMFileLines'+
                ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
                ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,'+
                ' :LineData)';
    except on E: Exception do HandleError(E,OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): boolean;
CONST OPNAME = 'TFileTariffCalculationDatabseAgent.ReadModelDataFromDatabase';
var
  LMessage : String;
  LDataSet : TAbstractModelDataset;
  LStop    : boolean;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LTariffCalculationData   : TTariffCalculationFileDataObject;
  LChannelTariffData       : TChannelTariffData;
begin

  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileTariffCalculationDatabseAgent.strReadStarted'); // Update

    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LTariffCalculationData   := LPlanningFileDataObject.TariffCalculationData;
    if not LTariffCalculationData.Initialise then
      exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LTariffCalculationData.DataYears.FData := 0;
      LTariffCalculationData.DataYears.FInitalised := True;

      LDataSet.SetSQL(ReadTariffCalculationConfigSQL);
      LDataSet.DataSet.Open;
      if not LDataSet.DataSet.Eof then
      begin
        LTariffCalculationData.DataYears.FData       :=  LDataSet.DataSet.FieldByName('DataYears').AsInteger;;
        LTariffCalculationData.DataYears.FInitalised := True;
      end;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePumpingChannelControlDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadTariffCalculationSQL);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin

        LChannelTariffData := LTariffCalculationData.AddChannelTariff;

        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LChannelTariffData.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LChannelTariffData.FChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('Tariff').IsNull then
        begin
          LChannelTariffData.FChannelTariff.FData := LDataSet.DataSet.FieldByName('Tariff').AsFloat;
          LChannelTariffData.FChannelTariff.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EscalationFactors').IsNull then
        begin
          LChannelTariffData.FEscalationFactors.FData := Trim(LDataSet.DataSet.FieldByName('EscalationFactors').AsString);
          LChannelTariffData.FEscalationFactors.FInitalised := True;
          LChannelTariffData.FEscalationFactors.FLength     := Length(LChannelTariffData.FEscalationFactors.FData)+3;
        end;
        LDataSet.DataSet.Next;
      end;

      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadUnkownDataSQL);
      LDataSet.DataSet.Open;
      while not (LDataSet.DataSet.Eof) do
      begin
        LTariffCalculationData.HDextraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileTariffCalculationDatabseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TFileTariffCalculationDatabseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileTariffCalculationDatabseAgent.WriteModelDataToDatabase';
var
  LMessage   : string;
  LDataSet   : TAbstractModelDataset;
  LStop      : boolean;
  LCount     : integer;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LTariffCalculationData   : TTariffCalculationFileDataObject;
  LChannelTariffData       : TChannelTariffData;
begin

  Result := False;
  try
     if not Assigned(AProgressFunction) then
        AProgressFunction := DummyShowProgress;

     LMessage := FAppModules.Language.GetString('TFileTariffCalculationDatabseAgent.strWriteStarted'); // Update
     AProgressFunction(LMessage,ptNone,LStop);

     if not Assigned(ADataObject) then
       raise Exception.Create('File object parameter is not yet assigned.');

     if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
        Exit;

     LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
     LTariffCalculationData  := LPlanningFileDataObject.TariffCalculationData;
     if LTariffCalculationData = nil then
       Exit;

     FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
     try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteTariffCalculationConfigSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario'],
                         [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
                          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['DataYears'], [IntToStr(LTariffCalculationData.DataYears.FData)]);
      LDataSet.ExecSQL;

      for LCount := 0 to LTariffCalculationData.ChannelTariffCount -1 do
      begin
        LChannelTariffData := LTariffCalculationData.ChannelTariffByIndex[LCount];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteTariffCalculationSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);
        if LChannelTariffData.FChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LChannelTariffData.FChannelNumber.FData)]);

        if LChannelTariffData.FChannelTariff.FInitalised then
          LDataSet.SetParams(['Tariff'], [FloatToStr(LChannelTariffData.FChannelTariff.FData)]);

        if LChannelTariffData.FEscalationFactors.FInitalised then
          LDataSet.SetParams(['EscalationFactors'], [LChannelTariffData.FEscalationFactors.FData]);
        LDataSet.ExecSQL;
      end;


       for LCount := 0 to LTariffCalculationData.HDextraLines.Count - 1 do
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
         LDataSet.SetParams(['LineData'], [LTariffCalculationData.HDextraLines[LCount]]);
         LDataSet.ExecSQL;
       end;
       LDataSet.DataSet.Close;

       Result := InsertFileName(AFileName);
       if Result then
       begin
         LMessage := FAppModules.Language.GetString('TFileTariffCalculationDatabseAgent.strWriteEnded');
         AProgressFunction(LMessage,ptNone,LStop);
       end;
       LDataSet.DataSet.Close;
     finally
       LDataSet.Free;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileTariffCalculationDatabseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion; AQuetly: boolean): boolean;
const OPNAME = 'TFileTariffCalculationDatabseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'TariffCalculationConfig,TariffCalculation';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
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

