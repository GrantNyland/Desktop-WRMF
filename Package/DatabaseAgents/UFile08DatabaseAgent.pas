//
//
//  UNIT      : Contains TFile07DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 04/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile08DatabaseAgent;

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
  UMinimumPowerDemandsObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile08DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF08UnknownDataSQL: string;
    function ReadPowerChannelsSQL: string;
    function ReadPowerPlantsDemandsSQL: string;

    function WritePowerPlantsDemandsSQL: string;
    function WritePowerChannelsSQL: string;
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
  UErrorHandlingOperations,
  UPowerChannelObject;

function TFile08DatabaseAgent.ReadF08UnknownDataSQL: string;
const OPNAME = 'TFile08DatabaseAgent.ReadF08UnknownDataSQL';
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

function TFile08DatabaseAgent.ReadPowerChannelsSQL: string;
const OPNAME = 'TFile08DatabaseAgent.ReadPowerChannelsSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ChannelName, ChannelNumber ' +
      'FROM PowerChannels ' +
      'WHERE (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND' +
      '      (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND' +
      '      (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND' +
      '      (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ' +
      'ORDER BY Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.WritePowerChannelsSQL: string;
const OPNAME = 'TFile08DatabaseAgent.WritePowerChannelsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO PowerChannels'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelName,ChannelNumber)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ChannelName,:ChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.ReadPowerPlantsDemandsSQL: string;
const OPNAME = 'TFile08DatabaseAgent.ReadPowerPlantsDemandsSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,PowerCode,MinPower01,MinPower02'+
      ' ,MinPower03,MinPower04,MinPower05,MinPower06,MinPower07,MinPower08,MinPower09,MinPower10,MinPower11,MinPower12'+
      ' FROM PowerPlantsDemands'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '        Identifier       = :Identifier' +
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier,PowerCode';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.WritePowerPlantsDemandsSQL: string;
const OPNAME = 'TFile08DatabaseAgent.WritePowerPlantsDemandsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO PowerPlantsDemands'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,PowerCode'+
              ' ,MinPower01,MinPower02,MinPower03,MinPower04,MinPower05,MinPower06,MinPower07,MinPower08,MinPower09,MinPower10,MinPower11,MinPower12)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:PowerCode,:MinPower01,:MinPower02'+
              ' ,:MinPower03,:MinPower04,:MinPower05,:MinPower06,:MinPower07,:MinPower08,:MinPower09,:MinPower10,:MinPower11,:MinPower12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile08DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile08DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LCount : Integer;
  LPowerGeneration : TPowerGenerationObject;
  LMinimumPowerDemands: TMinimumPowerDemandsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile08DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LMinimumPowerDemands := ADataObject.FMinimumPowerDemandsObject;

    if not LMinimumPowerDemands.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      LDataSet.SetSQL(ReadPowerChannelsSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile08DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LPowerGeneration   :=TPowerGenerationObject.Create;
          LMinimumPowerDemands.FMinimumPowerDetailsList.Add(LPowerGeneration);

          if not LDataSet.DataSet.FieldByName('ChannelName').IsNull then
          begin
            LPowerGeneration.FMinimumPowerData.FPowerChannelName.FData := Trim(LDataSet.DataSet.FieldByName('ChannelName').AsString);
            LPowerGeneration.FMinimumPowerData.FPowerChannelName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LPowerGeneration.FMinimumPowerData.FPowerChannelNumber.FData :=LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LPowerGeneration.FMinimumPowerData.FPowerChannelNumber.FInitalised := True;
          end;

          LSubDataSet.DataSet.Close;
          LSubDataSet.SetSQL(ReadPowerPlantsDemandsSQL);
          LSubDataSet.SetParams(['Identifier'], [IntToStr(LDataSet.DataSet.FieldByName('Identifier').AsInteger)]);
          LSubDataSet.DataSet.Open;

          //Line 3, 4,6,7
          while not LSubDataSet.DataSet.Eof do
          begin
            for LCount := MinPowerDemand to MaxPowerDemand do
            begin

              LFieldName := Format('%s%2.2d',['MinPower',LCount]);
              //Line 1a
              Case LSubDataSet.DataSet.FieldByName('PowerCode').AsInteger of
              1:begin
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LPowerGeneration.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LPowerGeneration.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount].FInitalised := True;
                  end;
                end;
              //Line 1b
              2:begin
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LPowerGeneration.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LPowerGeneration.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount].FInitalised := True;
                  end;
                end;
              end;//case
            end;
            LSubDataSet.DataSet.Next;
          end;
          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF08UnknownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LMinimumPowerDemands.FF08ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile08DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile08DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage,LFieldName:string;
  LLinesCount,
  LPowerCode,
  //LIdentifier,
  LCounter,
  LCount   : integer;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LMinimumPowerDetails :TPowerGenerationObject;
  LMinimumPowerDemands: TMinimumPowerDemandsObject;
  LStop: boolean;
  //lPowerChannelNumber : integer;
  //lPowerPlant         : TPowerPlantObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile08DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LMinimumPowerDemands := ADataObject.FMinimumPowerDemandsObject;
    if not Assigned(LMinimumPowerDemands) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try
      for LLinesCount := 0 to LMinimumPowerDemands.FMinimumPowerDetailsList.Count-1 do
      begin
        LMinimumPowerDetails := TPowerGenerationObject(LMinimumPowerDemands.FMinimumPowerDetailsList[LLinesCount]);
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WritePowerChannelsSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);
        LDataSet.SetParams(['ChannelName'], [LMinimumPowerDetails.FMinimumPowerData.FPowerChannelName.FData]);
        LDataSet.SetParams(['ChannelNumber'], [IntToStr(LMinimumPowerDetails.FMinimumPowerData.FPowerChannelNumber.FData)]);
        LDataSet.ExecSQL;


          //SubQuery                        ,
        for LPowerCode := 1 to 2 do
        begin
          LSubDataSet.DataSet.Close;
          LSubDataSet.SetSQL(WritePowerPlantsDemandsSQL);
          LSubDataSet.ClearQueryParams();
          LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LSubDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);
          LSubDataSet.SetParams(['PowerCode'], [IntToStr(LPowerCode)]);

          //Line 3
          for LCount := MinPowerDemand to MaxPowerDemand do
          begin
            LFieldName := Format('%s%2.2d',['MinPower',LCount]);
            case LPowerCode of
            1:begin
                if LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyEnergyGeneration[LCount].FData)]);
              end;
            2:begin
                if LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount].FInitalised then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LMinimumPowerDetails.FMinimumPowerDetails.FMinMonthlyPowerRelease[LCount].FData)]);
              end;
            end;//case
          end;
          LSubDataSet.ExecSQL;
        end;
      end;

      for LCounter := 0 to LMinimumPowerDemands.FF08ExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineData'], [LMinimumPowerDemands.FF08ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile08DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile08DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile08DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'PowerPlantsDemands,PowerChannels';
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
