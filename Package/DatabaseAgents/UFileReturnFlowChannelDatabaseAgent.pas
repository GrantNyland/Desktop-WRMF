//
//
//  UNIT      : Contains TFileReturnFlowChannelDatabaseAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 12/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFileReturnFlowChannelDatabaseAgent;

interface
uses
  Classes, sysutils,Db,

  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UReturnFlowChannelFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFileReturnFlowChannelDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadReturnFlowChannelCountSQL : string;
    function ReadReturnFlowMonthlyEvaporationSQL(ADemandChannel:integer) : string;
    function WriteReturnFlowMonthlyEvaporationSQL : string;
    function ReadReturnFlowChannelDetailSQL : string;
    function WriteReturnFlowChannelDetailSQL : string;
    function ReadReturnFlowChannelAssumedFactorSQL(ADemandChannel : integer) : string;
    function WriteReturnFlowChannelAssumedFactorSQL : string;
    function WriteReturnFlowChannelUnknownDataSQL : string;
    function ReadReturnFlowChannelUnknownDataSQL(AFileID:integer): string;
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
     UFileNameConstants,
     UErrorHandlingOperations, Contnrs, UBasicObjects;

{ TFileReturnFlowChannelDatabaseAgent }

function TFileReturnFlowChannelDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
                                                                      AQuetly: boolean): boolean;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ClearModelDataInDatabase';                                                                      
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
    LTableNames := 'ReturnFlowMonthlyEvaporation,ReturnFlowChannelDetail,ReturnFlowCorrespondingChannel';
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

function TFileReturnFlowChannelDatabaseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
                                                                       AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage : string;
  LDataSet: TAbstractModelDataset;
  LAssumedFactorDataSet: TAbstractModelDataset;
  LMonthlyEvaporationDataSet: TAbstractModelDataset;
  LStop : boolean;
  LIndex : integer;
  LReturnFlow : TReturnFlow;
  LAssumedFactor : TAssumedFactor;
  LReturnFlowFileObject: TReturnFlowChannelFileObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LReturnFlowFileObject := LPlanningFileDataObject.ReturnFlowChannelFileObject;
    if LReturnFlowFileObject = nil then
      Exit;
    if not LReturnFlowFileObject.Initialise then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LAssumedFactorDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LMonthlyEvaporationDataSet);
    try
      LReturnFlowFileObject.FReturnFlowCount.FData := 0;
      LReturnFlowFileObject.FReturnFlowCount.FInitalised := True;

      //Line 1....Total Number of Return Flow specifications.
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadReturnFlowChannelCountSQL);
      LDataSet.DataSet.Open;
      //Check if there is any data.
      if (LDataSet.DataSet.FieldByName('TotalNumOfSpecifications').AsInteger = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end
      else
      begin
        if LDataSet.DataSet.FieldByName('TotalNumOfSpecifications').AsInteger > 0 then
        begin
          LReturnFlowFileObject.FReturnFlowCount.FData := LDataSet.DataSet.FieldByName('TotalNumOfSpecifications').AsInteger;
          LReturnFlowFileObject.FReturnFlowCount.FInitalised := True;
        end;
      end;
      //Line 2 ...
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadReturnFlowChannelDetailSQL);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LReturnFlow := LReturnFlowFileObject.AddReturnFlow;
        if not LReturnFlow.Initialise then
          Exit;
        if not LDataSet.DataSet.FieldByName('DemandChannel').IsNull then
        begin
          LReturnFlow.FDemandChannel.FData := LDataSet.DataSet.FieldByName('DemandChannel').AsInteger;
          LReturnFlow.FDemandChannel.FInitalised := True;
        end;
        if not LDataSet.DataSet.FieldByName('GaugeNumber').IsNull then
        begin
          LReturnFlow.FGaugeNumber.FData := LDataSet.DataSet.FieldByName('GaugeNumber').AsInteger;
          LReturnFlow.FGaugeNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('MonthlyAvrgFactor').IsNull then
        begin
          LReturnFlow.FMonthlyAvrgFactor.FData := LDataSet.DataSet.FieldByName('MonthlyAvrgFactor').AsFloat;
          LReturnFlow.FMonthlyAvrgFactor.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('CalibrationFactor').IsNull then
        begin
          LReturnFlow.FCalibrationFactor.FData := LDataSet.DataSet.FieldByName('CalibrationFactor').AsFloat;
          LReturnFlow.FCalibrationFactor.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('MonthlyAvrgFactorEvap').IsNull then
        begin
          LReturnFlow.FMonthlyAvrgNetEvap.FData := LDataSet.DataSet.FieldByName('MonthlyAvrgFactorEvap').AsFloat;
          LReturnFlow.FMonthlyAvrgNetEvap.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('RoutingConstant').IsNull then
        begin
          LReturnFlow.FRoutingConstant.FData := LDataSet.DataSet.FieldByName('RoutingConstant').AsFloat;
          LReturnFlow.FRoutingConstant.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('CurtailmentFactor').IsNull then
        begin
          LReturnFlow.FCurtailmentFactor.FData := LDataSet.DataSet.FieldByName('CurtailmentFactor').AsFloat;
          LReturnFlow.FCurtailmentFactor.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('MultiplicationFactor').IsNull then
        begin
          LReturnFlow.FMultiplicationFactor.FData := LDataSet.DataSet.FieldByName('MultiplicationFactor').AsFloat;
          LReturnFlow.FMultiplicationFactor.FInitalised := True;
        end;
        // Line 3...Monthly potential evaporation - transpiration
        LMonthlyEvaporationDataSet.DataSet.Close;
        LMonthlyEvaporationDataSet.SetSQL(ReadReturnFlowMonthlyEvaporationSQL(LDataSet.DataSet.FieldByName('DemandChannel').AsInteger));
        LMonthlyEvaporationDataSet.DataSet.Open;
        while not LMonthlyEvaporationDataSet.DataSet.Eof do
        begin
          for LIndex := MinMonths to MaxMonths do
          begin
            LFieldName := Format('%s%2.2d',['PotentialMonthlyEvap',LIndex]);
            if not LMonthlyEvaporationDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LReturnFlow.FMonthlyPotentialEvap[LIndex].FData := LMonthlyEvaporationDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LReturnFlow.FMonthlyPotentialEvap[LIndex].FInitalised := True;
            end;

          end;

          LMonthlyEvaporationDataSet.DataSet.Next;
        end;
        LMonthlyEvaporationDataSet.DataSet.Close;
        // Line 4...
        LAssumedFactorDataSet.DataSet.Close;
        LAssumedFactorDataSet.ClearSQL;
        LAssumedFactorDataSet.SetSQL(ReadReturnFlowChannelAssumedFactorSQL(LDataSet.DataSet.FieldByName('DemandChannel').AsInteger));
        LAssumedFactorDataSet.DataSet.Open;
        while not LAssumedFactorDataSet.DataSet.Eof do
        begin
          LAssumedFactor := LReturnFlow.AddAssumedFactor;
          if not LAssumedFactor.Initialise then
            Exit;
          if not LAssumedFactorDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LAssumedFactor.FChannelNumber.FData := LAssumedFactorDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LAssumedFactor.FChannelNumber.FInitalised := True;
          end;
          if not LAssumedFactorDataSet.DataSet.FieldByName('AbstractionChannel').IsNull then
          begin
            LAssumedFactor.FAbstractionChannel.FData := LAssumedFactorDataSet.DataSet.FieldByName('AbstractionChannel').AsInteger;
            LAssumedFactor.FAbstractionChannel.FInitalised := True;
          end;
          if not LAssumedFactorDataSet.DataSet.FieldByName('AssumedFactor').IsNull then
          begin
            LAssumedFactor.FFactor.FData := LAssumedFactorDataSet.DataSet.FieldByName('AssumedFactor').AsFloat;
            LAssumedFactor.FFactor.FInitalised := True;
          end;
          LReturnFlow.FNumOfCorrespondingChannels.FData := LReturnFlow.FNumOfCorrespondingChannels.FData + 1;
          LReturnFlow.FNumOfCorrespondingChannels.FInitalised := True;
          LAssumedFactorDataSet.DataSet.Next;
        end;
        LAssumedFactorDataSet.DataSet.Close;
        LDataSet.DataSet.Next;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadReturnFlowChannelUnknownDataSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;
      while not LDataSet.DataSet.Eof do
      begin
        LReturnFlowFileObject.FExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;
    finally
      FreeAndNil(LDataSet);
      FreeAndNil(LAssumedFactorDataSet);
      FreeAndNil(LMonthlyEvaporationDataSet);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelAssumedFactorSQL(ADemandChannel : integer): string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelAssumedFactorSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber,AbstractionChannel,AssumedFactor'+
              ' FROM ReturnFlowCorrespondingChannel'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DemandChannel =  '+IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelCountSQL: string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelCountSQL';
begin
  Result := '';
  try
    Result := 'SELECT count(*) as TotalNumOfSpecifications'+
              ' FROM ReturnFlowChannelDetail'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelDetailSQL: string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelDetailSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+
              ' GaugeNumber,MonthlyAvrgFactor,CalibrationFactor,MonthlyAvrgFactorEvap,RoutingConstant,'+
              ' CurtailmentFactor,MultiplicationFactor '+
              ' FROM ReturnFlowChannelDetail'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelUnknownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowChannelUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  '+IntToStr(AFileID)+
              ' AND FileGroup     =  '+IntToStr(fgReturnFlowChannel)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
                                                                      AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LIndex,
  LCount         : integer;
  LReturnFlow : TReturnFlow;
  LAssumedFactor : TAssumedFactor;
  LReturnFlowFileObject: TReturnFlowChannelFileObject;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;
    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LReturnFlowFileObject := LPlanningFileDataObject.ReturnFlowChannelFileObject;
    if LReturnFlowFileObject <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
      {
        LDataSet.DataSet.Close;
        LDataSet.ClearSQL;
        LDataSet.SetSQL(WriteReturnFlowChannelCountSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['TotalNumOfSpecifications'],[IntToStr(LReturnFlowFileObject.FReturnFlowCount.FData)]);
        LDataSet.ExecSQL;
       }

        for LCount := 0 to LReturnFlowFileObject.FReturnFlowCount.FData -1 do
        begin
          LReturnFlow := TReturnFlow(LReturnFlowFileObject.FReturnFlowList.Items[LCount]);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.SetSQL(WriteReturnFlowChannelDetailSQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);

          if LReturnFlow <> nil then
          begin
            if LReturnFlow.FDemandChannel.FInitalised then
              LDataSet.SetParams(['DemandChannel'], [IntToStr(LReturnFlow.FDemandChannel.FData)]);

//            if LReturnFlow.FNumOfCorrespondingChannels.FInitalised then
//              LDataSet.SetParams(['CorrespondingChannels'], [IntToStr(LReturnFlow.FNumOfCorrespondingChannels.FData)]);

            if LReturnFlow.FGaugeNumber.FInitalised then
              LDataSet.SetParams(['GaugeNumber'], [IntToStr(LReturnFlow.FGaugeNumber.FData)]);

            if LReturnFlow.FMonthlyAvrgFactor.FInitalised then
              LDataSet.SetParams(['MonthlyAvrgFactor'], [FloatToStr(LReturnFlow.FMonthlyAvrgFactor.FData)]);

            if LReturnFlow.FCalibrationFactor.FInitalised then
              LDataSet.SetParams(['CalibrationFactor'], [FloatToStr(LReturnFlow.FCalibrationFactor.FData)]);

            if LReturnFlow.FMonthlyAvrgNetEvap.FInitalised then
              LDataSet.SetParams(['MonthlyAvrgFactorEvap'], [FloatToStr(LReturnFlow.FMonthlyAvrgNetEvap.FData)]);

            if LReturnFlow.FRoutingConstant.FInitalised then
              LDataSet.SetParams(['RoutingConstant'], [FloatToStr(LReturnFlow.FRoutingConstant.FData)]);

            if LReturnFlow.FCurtailmentFactor.FInitalised then
              LDataSet.SetParams(['CurtailmentFactor'], [FloatToStr(LReturnFlow.FCurtailmentFactor.FData)]);

            if LReturnFlow.FMultiplicationFactor.FInitalised then
              LDataSet.SetParams(['MultiplicationFactor'], [FloatToStr(LReturnFlow.FMultiplicationFactor.FData)]);

            LDataSet.ExecSQL;

            LDataSet.DataSet.Close;
            LDataSet.ClearSQL;
            LDataSet.ClearQueryParams();
            LDataSet.SetSQL(WriteReturnFlowMonthlyEvaporationSQL);
            LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);
            if LReturnFlow.FDemandChannel.FInitalised then
              LDataSet.SetParams(['DemandChannel'], [IntToStr(LReturnFlow.FDemandChannel.FData)]);
            for LIndex := MinMonths to MaxMonths do
            begin
              LFieldName := Format('%s%2.2d',['PotentialMonthlyEvap',LIndex]);
              if LReturnFlow.FMonthlyPotentialEvap[LIndex].FInitalised then
              begin
                LDataSet.SetParams([LFieldName], [FloatToStr(LReturnFlow.FMonthlyPotentialEvap[LIndex].FData)]);
              end;
            end;
            LDataSet.ExecSQL;

            if LReturnFlow.FNumOfCorrespondingChannels.FInitalised then
            begin
              for LIndex := 0 to LReturnFlow.FNumOfCorrespondingChannels.FData -1 do
              begin
                LAssumedFactor := TAssumedFactor(LReturnFlow.FAssumedFactorList.Items[LIndex]);
                LDataSet.DataSet.Close;
                LDataSet.ClearSQL;
                LDataSet.ClearQueryParams();
                LDataSet.SetSQL(WriteReturnFlowChannelAssumedFactorSQL);
                LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
                LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
                LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
                LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
                LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);

                if LReturnFlow.FDemandChannel.FInitalised then
                  LDataSet.SetParams(['DemandChannel'], [IntToStr(LReturnFlow.FDemandChannel.FData)]);

                if LAssumedFactor.FChannelNumber.FInitalised then
                  LDataSet.SetParams(['ChannelNumber'],[IntToStr(LAssumedFactor.FChannelNumber.FData)]);

                if LAssumedFactor.FAbstractionChannel.FInitalised then
                  LDataSet.SetParams(['AbstractionChannel'],[IntToStr(LAssumedFactor.FAbstractionChannel.FData)]);
                if LAssumedFactor.FFactor.FInitalised then
                  LDataSet.SetParams(['AssumedFactor'],[FloatToStr(LAssumedFactor.FFactor.FData)]);
                LDataSet.ExecSQL;
              end;
            end;
          end;
        end;
      if LReturnFlowFileObject.FExtraLines.Count > 0 then
      begin
        for LCount := 0 to LReturnFlowFileObject.FExtraLines.Count - 1 do
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteReturnFlowChannelUnknownDataSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
          LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
          LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
          LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
          LDataSet.SetParams(['LineData'], [LReturnFlowFileObject.FExtraLines[LCount]]);
          LDataSet.ExecSQL;
        end;

      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileReturnFlowChannelDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;

      finally
        FreeAndNil(LDataSet);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowChannelAssumedFactorSQL: string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowChannelAssumedFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowCorrespondingChannel '+
              ' ( Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,ChannelNumber,AbstractionChannel,AssumedFactor )'+
              ' VALUES '+
              ' ( :Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DemandChannel,:ChannelNumber,:AbstractionChannel,:AssumedFactor )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowMonthlyEvaporationSQL: string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowMonthlyEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowMonthlyEvaporation '+
              ' ( Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,PotentialMonthlyEvap01,PotentialMonthlyEvap02,PotentialMonthlyEvap03,'+
              '   PotentialMonthlyEvap04,PotentialMonthlyEvap05,PotentialMonthlyEvap06,PotentialMonthlyEvap07,'+
              '   PotentialMonthlyEvap08,PotentialMonthlyEvap09,PotentialMonthlyEvap10,PotentialMonthlyEvap11,'+
              '   PotentialMonthlyEvap12 )'+
              ' VALUES '+
              ' ( :Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DemandChannel,:PotentialMonthlyEvap01,:PotentialMonthlyEvap02,:PotentialMonthlyEvap03,'+
              ' :PotentialMonthlyEvap04,:PotentialMonthlyEvap05,:PotentialMonthlyEvap06,:PotentialMonthlyEvap07,'+
              ' :PotentialMonthlyEvap08,:PotentialMonthlyEvap09,:PotentialMonthlyEvap10,:PotentialMonthlyEvap11,'+
              ' :PotentialMonthlyEvap12)';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowChannelDetailSQL: string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowChannelDetailSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ReturnFlowChannelDetail '+
              ' ( Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+
              '   GaugeNumber,MonthlyAvrgFactor,CalibrationFactor,MonthlyAvrgFactorEvap,RoutingConstant,'+
              '   CurtailmentFactor,MultiplicationFactor )'+
              ' VALUES '+
              ' ( :Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DemandChannel,'+
              ' :GaugeNumber,:MonthlyAvrgFactor,:CalibrationFactor,:MonthlyAvrgFactorEvap,:RoutingConstant,'+
              ' :CurtailmentFactor,:MultiplicationFactor )';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowChannelUnknownDataSQL: string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.WriteReturnFlowChannelUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowMonthlyEvaporationSQL(ADemandChannel: integer): string;
const OPNAME = 'TFileReturnFlowChannelDatabaseAgent.ReadReturnFlowMonthlyEvaporationSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,DemandChannel,'+
              ' PotentialMonthlyEvap01,PotentialMonthlyEvap02,PotentialMonthlyEvap03,'+
              ' PotentialMonthlyEvap04,PotentialMonthlyEvap05,PotentialMonthlyEvap06,PotentialMonthlyEvap07,'+
              ' PotentialMonthlyEvap08,PotentialMonthlyEvap09,PotentialMonthlyEvap10,PotentialMonthlyEvap11,'+
              ' PotentialMonthlyEvap12 '+
              ' FROM ReturnFlowMonthlyEvaporation '+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND DemandChannel =  '+IntToStr(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
