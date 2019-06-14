//  UNIT      : Contains TFile19DatabaseAgent Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 30/10/2006
//  COPYRIGHT : Copyright © 2006 DWAF
unit UFile19DatabaseAgent;

interface

uses
  Classes, Sysutils, Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYMDemandCentreObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile19DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF19UnkownDataSQL: string;
    function ReadYMDemandCentreDataSQL: string;
    function ReadYMDemandCentreReturnFlowChannelSQL: string;

    function WriteF19UnkownDataSQL: string;
    function WriteYMDemandCentreDataSQL: string;
    function WriteYMDemandCentreReturnFlowChannelSQL: string;
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion; AQuietly: boolean = False): boolean; override;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TFile19DatabaseAgent.ReadF19UnkownDataSQL: string;
const OPNAME = 'TFile19DatabaseAgent.ReadF19UnkownDataSQL';
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

function TFile19DatabaseAgent.ReadYMDemandCentreDataSQL: string;
const
  OPNAME      = 'TFile19DatabaseAgent.ReadYMDemandCentreDataSQL';
begin
  Result := '';
  try
    Result  := Format(' SELECT Model, StudyAreaName, SubArea, Scenario, '+
                      ' Identifier, NodeNumber, CentreName, NodeRefNr, '+
                      ' AveReturnFlowFactor, AveEvaporation, StdDeviationFactor, RoutingConstant, '+
                      ' RainfallScalingFactor, TotalFlowLost, '+
                      ' EvapoTranspiration01, EvapoTranspiration02, EvapoTranspiration03, EvapoTranspiration04, '+
                      ' EvapoTranspiration05, EvapoTranspiration06, EvapoTranspiration07, EvapoTranspiration08, '+
                      ' EvapoTranspiration09, EvapoTranspiration10, EvapoTranspiration11, EvapoTranspiration12 '+
                      ' FROM YMDemandCentre '+
                      ' WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s '+
                      ' ORDER BY Model ,StudyAreaName, SubArea, Scenario, Identifier ',[
                                  QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19DatabaseAgent.ReadYMDemandCentreReturnFlowChannelSQL: string;
const
  OPNAME      = 'TFile19DatabaseAgent.ReadYMDemandCentreReturnFlowChannelSQL';
begin
  Result := '';
  try
    Result  := Format(' SELECT Model, StudyAreaName, SubArea, Scenario, '+
                      ' DemandCentreID, Identifier, ChannelNr, TotalReturnFlow, FlowDiversion '+
                      ' FROM YMDemandCentreReturnFlowChannel '+
                      ' WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s AND DemandCentreID=:DemandCentreID '+
                      ' ORDER BY Model, StudyAreaName, SubArea, Scenario, DemandCentreID, Identifier ',
                                 [QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19DatabaseAgent.WriteYMDemandCentreDataSQL: string;
const
  OPNAME      = 'TFile19DatabaseAgent.WriteYMDemandCentreDataSQL';
begin
  Result := '';
  try

    Result := 'INSERT INTO YMDemandCentre '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, NodeNumber, CentreName, '+
              ' NodeRefNr, AveReturnFlowFactor, AveEvaporation, StdDeviationFactor, RoutingConstant, '+
              ' RainfallScalingFactor, TotalFlowLost, '+
              ' EvapoTranspiration01, EvapoTranspiration02, EvapoTranspiration03, EvapoTranspiration04, '+
              ' EvapoTranspiration05, EvapoTranspiration06, EvapoTranspiration07, EvapoTranspiration08, '+
              ' EvapoTranspiration09, EvapoTranspiration10, EvapoTranspiration11, EvapoTranspiration12, '+
              ' CentreDescription ,ConsumptiveChannelNr, ReclaimationChannelNr) '+
              'Values '+
              '(:Model ,:StudyAreaName, :SubArea, :Scenario, :Identifier, :NodeNumber, :CentreName, '+
              ' :NodeRefNr, :AveReturnFlowFactor, :AveEvaporation, :StdDeviationFactor, :RoutingConstant, '+
              ' :RainfallScalingFactor, :TotalFlowLost, '+
              ' :EvapoTranspiration01, :EvapoTranspiration02, :EvapoTranspiration03, :EvapoTranspiration04, '+
              ' :EvapoTranspiration05, :EvapoTranspiration06, :EvapoTranspiration07, :EvapoTranspiration08, '+
              ' :EvapoTranspiration09, :EvapoTranspiration10, :EvapoTranspiration11, :EvapoTranspiration12, '+
              ' :CentreDescription, :ConsumptiveChannelNr, :ReclaimationChannelNr) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19DatabaseAgent.WriteYMDemandCentreReturnFlowChannelSQL: string;
const
  OPNAME      = 'TFile19DatabaseAgent.WriteYMDemandCentreReturnFlowChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO YMDemandCentreReturnFlowChannel '+
                '(Model, StudyAreaName, SubArea, Scenario, DemandCentreID, '+
                ' Identifier, ChannelNr, TotalReturnFlow, FlowDiversion) '+
                'Values '+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :DemandCentreID, '+
                ' :Identifier, :ChannelNr, :TotalReturnFlow, :FlowDiversion)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19DatabaseAgent.WriteF19UnkownDataSQL: string;
const OPNAME = 'TFile19DatabaseAgent.WriteF19UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile19DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage                : string;
  LDataSet,
  LDataSet2               : TAbstractModelDataset;
  LCount,
  LCount2                 : Integer;
  LYMDemandCentreObject   : TYMDemandCentreObject;
  LYMDemandCentre         : TYMDemandCentre;
  LStop                   : Boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile19DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LYMDemandCentreObject := ADataObject.FYMDemandCentreObject;

    if not LYMDemandCentreObject.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet2);
    try
      LDataSet.SetSQL(ReadYMDemandCentreDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile19DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LYMDemandCentre  := LYMDemandCentreObject.AddDemandCentre;
          
          if not LDataSet.DataSet.FieldByName('Identifier').IsNull then
          begin
            LYMDemandCentre.Identifier.FData        := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LYMDemandCentre.Identifier.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('NodeNumber').IsNull then
          begin
            LYMDemandCentre.NodeNumber.FData        := LDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
            LYMDemandCentre.NodeNumber.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('CentreName').IsNull then
          begin
            LYMDemandCentre.Name.FData        := Trim(LDataSet.DataSet.FieldByName('CentreName').AsString);
            LYMDemandCentre.Name.FLength      := Length(LYMDemandCentre.Name.FData);
            LYMDemandCentre.Name.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('NodeRefNr').IsNull then
          begin
            LYMDemandCentre.NodeRefNr.FData        := LDataSet.DataSet.FieldByName('NodeRefNr').AsInteger;
            LYMDemandCentre.NodeRefNr.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('AveReturnFlowFactor').IsNull then
          begin
            LYMDemandCentre.AveReturnFlowFactor.FData := LDataSet.DataSet.FieldByName('AveReturnFlowFactor').AsFloat;
            LYMDemandCentre.AveReturnFlowFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('AveEvaporation').IsNull then
          begin
            LYMDemandCentre.AveEvaporation.FData := LDataSet.DataSet.FieldByName('AveEvaporation').AsFloat;
            LYMDemandCentre.AveEvaporation.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('StdDeviationFactor').IsNull then
          begin
            LYMDemandCentre.StdDeviationFactor.FData := LDataSet.DataSet.FieldByName('StdDeviationFactor').AsFloat;
            LYMDemandCentre.StdDeviationFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('RoutingConstant').IsNull then
          begin
            LYMDemandCentre.RoutingConstant.FData :=LDataSet.DataSet.FieldByName('RoutingConstant').AsFloat;
            LYMDemandCentre.RoutingConstant.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('RainfallScalingFactor').IsNull then
          begin
            LYMDemandCentre.RainfallScalingFactor.FData :=LDataSet.DataSet.FieldByName('RainfallScalingFactor').AsFloat;
            LYMDemandCentre.RainfallScalingFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('TotalFlowLost').IsNull then
          begin
            LYMDemandCentre.TotalFlowLost.FData := LDataSet.DataSet.FieldByName('TotalFlowLost').AsFloat;
            LYMDemandCentre.TotalFlowLost.FInitalised := True;
          end;
          for LCount2 := MinMonths to MaxMonths do
          begin
            LFieldName := Format('%s%2.2d',['EvapoTranspiration',LCount2]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LYMDemandCentre.EvapoTranspiration[LCount2].FData       := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LYMDemandCentre.EvapoTranspiration[LCount2].FInitalised := True;
            end else
              Break;
          end;

          // Demand Centre Return Flow Channel
          LDataSet2.SetSQL(ReadYMDemandCentreReturnFlowChannelSQL);
          LDataSet2.SetParams(['DemandCentreID'], [IntToStr(LYMDemandCentre.Identifier.FData)]);
          LDataSet2.DataSet.Open;

          LCount := 1;
          LYMDemandCentre.ChannelCount := 0;
          while not LDataSet2.DataSet.Eof do
          begin
            LYMDemandCentre.ChannelCount := LYMDemandCentre.ChannelCount + 1;
            //LYMDemandCentre.ReturnFlowChannel[LCount] := TYMDemandCentreReturnFlowChannel.Create;
            if not LDataSet2.DataSet.FieldByName('DemandCentreID').IsNull then
            begin
              LYMDemandCentre.ReturnFlowChannel[LCount].DemandCentreID.FData       := LDataSet2.DataSet.FieldByName('DemandCentreID').AsInteger;
              LYMDemandCentre.ReturnFlowChannel[LCount].DemandCentreID.FInitalised := True;
            end;
            if not LDataSet2.DataSet.FieldByName('Identifier').IsNull then
            begin
              LYMDemandCentre.ReturnFlowChannel[LCount].Identifier.FData       := LDataSet2.DataSet.FieldByName('Identifier').AsInteger;
              LYMDemandCentre.ReturnFlowChannel[LCount].Identifier.FInitalised := True;
            end;
            if not LDataSet2.DataSet.FieldByName('ChannelNr').IsNull then
            begin
              LYMDemandCentre.ReturnFlowChannel[LCount].ChannelNr.FData        := LDataSet2.DataSet.FieldByName('ChannelNr').AsInteger;
              LYMDemandCentre.ReturnFlowChannel[LCount].ChannelNr.FInitalised  := True;
            end;
            if not LDataSet2.DataSet.FieldByName('TotalReturnFlow').IsNull then
            begin
              LYMDemandCentre.ReturnFlowChannel[LCount].TotalReturnFlow.FData        := LDataSet2.DataSet.FieldByName('TotalReturnFlow').AsFloat;
              LYMDemandCentre.ReturnFlowChannel[LCount].TotalReturnFlow.FInitalised  := True;
            end;
            if not LDataSet2.DataSet.FieldByName('FlowDiversion').IsNull then
            begin
              LYMDemandCentre.ReturnFlowChannel[LCount].FlowDiversion.FData        := LDataSet2.DataSet.FieldByName('FlowDiversion').AsFloat;
              LYMDemandCentre.ReturnFlowChannel[LCount].FlowDiversion.FInitalised  := True;
            end;
            LDataSet2.DataSet.Next;
            Inc(LCount);            
          end;
          LDataSet2.DataSet.Close;


          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF19UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LYMDemandCentreObject.ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile19DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile19DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile19DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage                : string;
  LCount,
  LLinesCount,
  LCounter,
  LNoOfReturnFlowChannels : Integer;
  LDataSet                : TAbstractModelDataset;
  LYMDemandCentreObject   : TYMDemandCentreObject;
  LYMDemandCentre         : TYMDemandCentre;
  LStop                   : Boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile19DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LYMDemandCentreObject := ADataObject.FYMDemandCentreObject;
    if not Assigned(LYMDemandCentreObject) then
      Exit;

    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    try
      for LLinesCount := 0 to LYMDemandCentreObject.DemandCentreCount-1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteYMDemandCentreDataSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);

        LYMDemandCentre := LYMDemandCentreObject.DemandCentreObjectByIndex[LLinesCount];

        if LYMDemandCentre.Identifier.FInitalised then
          LDataSet.SetParams(['Identifier'], [IntToStr(LYMDemandCentre.Identifier.FData)]);
        if LYMDemandCentre.NodeNumber.FInitalised then
          LDataSet.SetParams(['NodeNumber'], [IntToStr(LYMDemandCentre.NodeNumber.FData)]);
        if LYMDemandCentre.Name.FInitalised then
          LDataSet.SetParams(['CentreName'], [LYMDemandCentre.Name.FData]);
        if LYMDemandCentre.NodeRefNr.FInitalised then
          LDataSet.SetParams(['NodeRefNr'], [IntToStr(LYMDemandCentre.NodeRefNr.FData)]);
        if LYMDemandCentre.AveReturnFlowFactor.FInitalised then
          LDataSet.SetParams(['AveReturnFlowFactor'], [FloatToStr(LYMDemandCentre.AveReturnFlowFactor.FData)]);
        if LYMDemandCentre.AveEvaporation.FInitalised then
          LDataSet.SetParams(['AveEvaporation'], [FloatToStr(LYMDemandCentre.AveEvaporation.FData)]);
        if LYMDemandCentre.StdDeviationFactor.FInitalised then
          LDataSet.SetParams(['StdDeviationFactor'], [FloatToStr(LYMDemandCentre.StdDeviationFactor.FData)]);
        if LYMDemandCentre.RoutingConstant.FInitalised then
          LDataSet.SetParams(['RoutingConstant'], [FloatToStr(LYMDemandCentre.RoutingConstant.FData)]);
        if LYMDemandCentre.RainfallScalingFactor.FInitalised then
          LDataSet.SetParams(['RainfallScalingFactor'], [FloatToStr(LYMDemandCentre.RainfallScalingFactor.FData)]);
        if LYMDemandCentre.TotalFlowLost.FInitalised then
          LDataSet.SetParams(['TotalFlowLost'], [FloatToStr(LYMDemandCentre.TotalFlowLost.FData)]);
        if LYMDemandCentre.Name.FInitalised then
          LDataSet.SetParams(['CentreDescription'], [LYMDemandCentre.Name.FData]); 
        if LYMDemandCentre.ConsumptiveChannelNr.FInitalised then
          LDataSet.SetParams(['ConsumptiveChannelNr'], [IntToStr(LYMDemandCentre.ConsumptiveChannelNr.FData)]);
        if LYMDemandCentre.ReclaimationChannelNr.FInitalised then
          LDataSet.SetParams(['ReclaimationChannelNr'], [IntToStr(LYMDemandCentre.ReclaimationChannelNr.FData)]);
        for LCount := MinMonths to MaxMonths do
        begin
          if not LYMDemandCentre.EvapoTranspiration[LCount].FInitalised then
            Break;
          LFieldName := Format('%s%2.2d',['EvapoTranspiration',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LYMDemandCentre.EvapoTranspiration[LCount].FData)]);
        end;
        LDataSet.ExecSQL;

        //YMDemandCentreReturnFlowChannel
        LDataSet.DataSet.Close;
        for LNoOfReturnFlowChannels := MinNoReturnFlowChannels to MaxNoReturnFlowChannels do
        begin
          LDataSet.SetSQL(WriteYMDemandCentreReturnFlowChannelSQL);
          LDataSet.ClearQueryParams();

          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
//          LDataSet.SetParams(['Identifier'], [IntToStr(LNoOfReturnFlowChannels+1)]);

          if Assigned(LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels]) then
          begin
            if not LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].DemandCentreID.FInitalised then
              Break;
            if LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].Identifier.FInitalised then
              LDataSet.SetParams(['Identifier'], [IntToStr(LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].Identifier.FData)]);
            if LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].DemandCentreID.FInitalised then
              LDataSet.SetParams(['DemandCentreID'], [IntToStr(LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].DemandCentreID.FData)]);
            if LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].ChannelNr.FInitalised then
              LDataSet.SetParams(['ChannelNr'], [IntToStr(LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].ChannelNr.FData)]);
            if LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].TotalReturnFlow.FInitalised then
              LDataSet.SetParams(['TotalReturnFlow'], [FloatToStr(LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].TotalReturnFlow.FData)]);
            if LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].FlowDiversion.FInitalised then
              LDataSet.SetParams(['FlowDiversion'], [FloatToStr(LYMDemandCentre.ReturnFlowChannel[LNoOfReturnFlowChannels].FlowDiversion.FData)]);
          end;
          LDataSet.ExecSQL;
        end;
      end;

      for LCounter := 0 to LYMDemandCentreObject.ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF19UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LYMDemandCentreObject.ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile19DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;    
end;

function TFile19DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion; AQuietly: boolean = False): boolean;
const OPNAME = 'TFile19DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage  : string;
  LStop     : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuietly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    LTableNames := ' YMDemandCentre, YMDemandCentreReturnFlowChannel ';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuietly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuietly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuietly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
