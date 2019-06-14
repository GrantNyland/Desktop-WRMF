//
//
//  UNIT      : Contains TFile07DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 04/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile07DatabaseAgent;

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
  UPowerChannelObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile07DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF07UnknownDataSQL: string;
    function ReadPowerPlantsSQL: string;
    function ReadPowerPlantsDetailsSQL: string;

    function WritePowerPlantsSQL: string;
    function WritePowerPlantsDetailsSQL: string;
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
  UChannelDescriptionObject;

function TFile07DatabaseAgent.ReadF07UnknownDataSQL: string;
const OPNAME = 'TFile07DatabaseAgent.ReadF07UnknownDataSQL';
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

function TFile07DatabaseAgent.ReadPowerPlantsSQL: string;
const OPNAME = 'TFile07DatabaseAgent.ReadPowerPlantsSQL';
begin

  Result := '';
  try
    Result := 'SELECT '+
      'Pd.Model, Pd.StudyAreaName, Pd.SubArea, Pd.Scenario, Pd.Identifier, ' +
      'Pd.PowerPlantName, Pd.PowerChannelNumber, Pd.SpillChannelNumber, Pd.MaxCapGenerator, ' +
      'Pd.MaxCapTurbine, Pd.Efficiency, Pd.PowerPlantStatus, Pd.HeadLoss, ' +
      'Pd.DesignHead, Pd.MaxNetHead, Pd.MinNetHead, Pd.HydropowerMinLevel, Pd.PointsCount, Pd.TailWaterCount, Pd.TailWaterTypeCode '+
      'FROM PowerPlants Pd ' +
      'WHERE  (Pd.Model            = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND '+
      '       (Pd.StudyAreaName    = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND '+
      '       (Pd.SubArea          = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND '+
      '       (Pd.Scenario         = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') '+
      'ORDER BY Pd.Model, Pd.StudyAreaName, Pd.SubArea, Pd.Scenario, Pd.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.ReadPowerPlantsDetailsSQL: string;
const OPNAME = 'TFile07DatabaseAgent.ReadPowerPlantsDetailsSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,FactorCode'+
      ' ,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,Factor07,Factor08,Factor09,Factor10'+
      ' FROM PowerPlantsDetails'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '        Identifier       = :Identifier' +
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier,FactorCode';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.WritePowerPlantsSQL: string;
const OPNAME = 'TFile07DatabaseAgent.WritePowerPlantsSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO PowerPlants ' +
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'PowerPlantName, PowerChannelNumber, SpillChannelNumber, MaxCapGenerator, ' +
      'MaxCapTurbine, Efficiency, PowerPlantStatus, HeadLoss, DesignHead, ' +
      'MaxNetHead, MinNetHead, HydropowerMinLevel, PointsCount, TailWaterCount, TailWaterTypeCode, ' +
      'DownStreamPowerChannelCount, Channel01, Channel02, Channel03, Channel04, ' +
      'Channel05, Channel06, Channel07, Channel08, Channel09, Channel10, Channel11, ' +
      'Channel12, Channel13, Channel14, Channel15, Channel16, Channel17, Channel18, ' +
      'Channel19, Channel20) '+
      'Values '+
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ':PowerPlantName, :PowerChannelNumber, :SpillChannelNumber, :MaxCapGenerator, ' +
      ':MaxCapTurbine, :Efficiency, :PowerPlantStatus, :HeadLoss, :DesignHead, ' +
      ':MaxNetHead, :MinNetHead, :HydropowerMinLevel, :PointsCount, :TailWaterCount, :TailWaterTypeCode, ' +
      ':DownStreamPowerChannelCount, :Channel01, :Channel02, :Channel03, :Channel04, ' +
      ':Channel05, :Channel06, :Channel07, :Channel08, :Channel09, :Channel10, :Channel11, ' +
      ':Channel12, :Channel13, :Channel14, :Channel15, :Channel16, :Channel17, :Channel18, ' +
      ':Channel19, :Channel20)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.WritePowerPlantsDetailsSQL: string;
const OPNAME = 'TFile07DatabaseAgent.WritePowerPlantsDetailsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO PowerPlantsDetails'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,FactorCode'+
              ' ,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,Factor07,Factor08,Factor09,Factor10)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:FactorCode'+
              ' ,:Factor01,:Factor02,:Factor03,:Factor04,:Factor05,:Factor06,:Factor07,:Factor08,:Factor09,:Factor10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile07DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile07DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LCount : Integer;
  LPowerPlant :TPowerPlantObject;
  LPowerChannelObject: TPowerChannelsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile07DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPowerChannelObject := ADataObject.FPowerChannelObject;

    if not LPowerChannelObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      LDataSet.SetSQL(ReadPowerPlantsSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile07DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LPowerPlant :=TPowerPlantObject.Create;
          LPowerChannelObject.FPowerPlantItemList.Add(LPowerPlant);

          if not LDataSet.DataSet.FieldByName('PowerPlantName').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FData := Trim(LDataSet.DataSet.FieldByName('PowerPlantName').AsString);
            LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PowerChannelNumber').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FData :=LDataSet.DataSet.FieldByName('PowerChannelNumber').AsInteger;
            LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MaxCapGenerator').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator.FData :=LDataSet.DataSet.FieldByName('MaxCapGenerator').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MaxCapTurbine').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine.FData :=LDataSet.DataSet.FieldByName('MaxCapTurbine').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Efficiency').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FEfficiency.FData :=LDataSet.DataSet.FieldByName('Efficiency').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FEfficiency.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PowerPlantStatus').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus.FData :=LDataSet.DataSet.FieldByName('PowerPlantStatus').AsInteger;
            LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('HeadLoss').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FHeadLoss.FData :=LDataSet.DataSet.FieldByName('HeadLoss').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FHeadLoss.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('DesignHead').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FDesignHead.FData :=LDataSet.DataSet.FieldByName('DesignHead').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FDesignHead.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MaxNetHead').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FMaxNetHead.FData :=LDataSet.DataSet.FieldByName('MaxNetHead').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FMaxNetHead.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MinNetHead').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FMinNetHead.FData :=LDataSet.DataSet.FieldByName('MinNetHead').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FMinNetHead.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('HydropowerMinLevel').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FData :=LDataSet.DataSet.FieldByName('HydropowerMinLevel').AsFloat;
            LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PointsCount').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FPointsCount.FData :=LDataSet.DataSet.FieldByName('PointsCount').AsInteger;
            LPowerPlant.FPowerPlantHeadData.FPointsCount.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TailWaterCount').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FData :=LDataSet.DataSet.FieldByName('TailWaterCount').AsInteger;
            LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TailWaterTypeCode').IsNull then
          begin
            LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode.FData :=LDataSet.DataSet.FieldByName('TailWaterTypeCode').AsInteger;
            LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode.FInitalised := True;
          end;

          LSubDataSet.DataSet.Close;
          LSubDataSet.SetSQL(ReadPowerPlantsDetailsSQL);
          LSubDataSet.SetParams(['Identifier'], [IntToStr(LDataSet.DataSet.FieldByName('Identifier').AsInteger)]);
          LSubDataSet.DataSet.Open;

          //Line 3, 4,6,7
          while not LSubDataSet.DataSet.Eof do
          begin
            for LCount := MinNumberOfPoints to MaxNumberOfPoints do
            begin

              LFieldName := Format('%s%2.2d',['Factor',LCount]);
              //Line 3
              Case LSubDataSet.DataSet.FieldByName('FactorCode').AsInteger of
              1:begin
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount].FInitalised := True;
                  end;
                end;
              //Line 4
              2:begin
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount].FInitalised := True;
                  end;
                end;
              //Line 6
              3:begin
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount].FInitalised := True;
                  end;
                end;
              //Line 7
              4:begin
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount].FInitalised := True;
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
      LDataSet.SetSQL(ReadF07UnknownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LPowerChannelObject.FF07ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile07DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile07DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage,LFieldName:string;
  LLinesCount,
  LFactorCode,
  LIdentifier,
  LCounter,
  LCount   : integer;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LPowerPlant :TPowerPlantObject;
  LPowerChannelObject: TPowerChannelsObject;
  LPowerChannel : TPowerChannelObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile07DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LPowerChannelObject := ADataObject.FPowerChannelObject;
    if not Assigned(LPowerChannelObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      LIdentifier := 0;
      for LLinesCount := 0 to LPowerChannelObject.FPowerPlantItemList.Count-1 do
      begin
        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WritePowerPlantsSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);

        LPowerPlant := TPowerPlantObject(LPowerChannelObject.FPowerPlantItemList[LLinesCount]);

        if LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FInitalised then
         LDataSet.SetParams(['PowerPlantName'], [LPowerPlant.FPowerPlantHeadData.FPowerPlantName.FData]);

        if LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FInitalised then
        begin
          LDataSet.SetParams(['PowerChannelNumber'], [IntToStr(LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FData)]);
          lPowerChannel := ADataObject.FChannelDescrObject.FindPowerChannels(LPowerPlant.FPowerPlantHeadData.FPowerPlantNumber.FData);
          LDataSet.SetParams(['SpillChannelNumber'], [IntToStr(LPowerChannel.FSpillChannelNumber.FData)]);
          LDataSet.SetParams(['DownStreamPowerChannelCount'], [IntToStr(LPowerChannel.FDownStreamPowerChannelCount.FData)]);
          for LCount :=  MinDownStreamPowerChannels to MaxDownStreamPowerChannels do
          begin
            LFieldName := Format('%s%2.2d',['Channel',LCount]);
            if LPowerChannel.FDownStreamPowerChannels[LCount].FInitalised then
              LDataSet.SetParams([LFieldName], [FloatToStr(
              LPowerChannel.FDownStreamPowerChannels[LCount].FData)]);
          end;
        end;

        if LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator.FInitalised then
         LDataSet.SetParams(['MaxCapGenerator'], [FloatToStr(LPowerPlant.FPowerPlantHeadData.FMaxCapGenerator.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine.FInitalised then
         LDataSet.SetParams(['MaxCapTurbine'], [FloatToStr(LPowerPlant.FPowerPlantHeadData.FMaxCapTurbine.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FEfficiency.FInitalised then
         LDataSet.SetParams(['Efficiency'], [FloatToStr(LPowerPlant.FPowerPlantHeadData.FEfficiency.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus.FInitalised then
         LDataSet.SetParams(['PowerPlantStatus'], [IntToStr( LPowerPlant.FPowerPlantHeadData.FPowerPlantStatus.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FHeadLoss.FInitalised then
         LDataSet.SetParams(['HeadLoss'], [FloatToStr( LPowerPlant.FPowerPlantHeadData.FHeadLoss.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FDesignHead.FInitalised then
         LDataSet.SetParams(['DesignHead'], [FloatToStr( LPowerPlant.FPowerPlantHeadData.FDesignHead.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FMaxNetHead.FInitalised then
         LDataSet.SetParams(['MaxNetHead'], [FloatToStr( LPowerPlant.FPowerPlantHeadData.FMaxNetHead.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FMinNetHead.FInitalised then
         LDataSet.SetParams(['MinNetHead'], [FloatToStr( LPowerPlant.FPowerPlantHeadData.FMinNetHead.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FInitalised then
         LDataSet.SetParams(['HydropowerMinLevel'], [FloatToStr( LPowerPlant.FPowerPlantHeadData.FHydropowerMinLevel.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FPointsCount.FInitalised then
         LDataSet.SetParams(['PointsCount'], [IntToStr( LPowerPlant.FPowerPlantHeadData.FPointsCount.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FInitalised then
         LDataSet.SetParams(['TailWaterCount'], [IntToStr( LPowerPlant.FPowerPlantHeadData.FTailWaterPointsCount.FData)]);

        if LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode.FInitalised then
         LDataSet.SetParams(['TailWaterTypeCode'], [IntToStr( LPowerPlant.FPowerPlantHeadData.FTailWaterTypeCode.FData)]);

        LDataSet.ExecSQL;

        //SubQuery
        for LFactorCode := 1 to 4 do
        begin
          LSubDataSet.DataSet.Close;
          LSubDataSet.SetSQL(WritePowerPlantsDetailsSQL);
          LSubDataSet.ClearQueryParams(prFloat);
          LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
          LSubDataSet.SetParams(['FactorCode'], [IntToStr(LFactorCode)]);

            //Line 3
          for LCount := MinNumberOfPoints to MaxNumberOfPoints do
          begin
            LFieldName := Format('%s%2.2d',['Factor',LCount]);
            Case LFactorCode of
            1:begin
                if LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LPowerPlant.FPowerPlantHeadDetails.FEfficiencyFactor[LCount].FData)]);
              end;
            2:begin
                if LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount].FInitalised then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LPowerPlant.FPowerPlantHeadDetails.FNetHeadFactor[LCount].FData)]);
              end;
            3:begin
                if LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LPowerPlant.FPowerPlantHeadDetails.FDischarge[LCount].FData)]);
              end;
            4:begin
                if LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LPowerPlant.FPowerPlantHeadDetails.FTailWaterElevation[LCount].FData)]);
              end;
            end;//case
          end;
          LSubDataSet.ExecSQL;
        end;
      end;

      for LCounter := 0 to LPowerChannelObject.FF07ExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineData'], [LPowerChannelObject.FF07ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile07DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile07DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile07DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'PowerPlants,PowerPlantsDetails';
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
