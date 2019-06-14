//  UNIT      : Contains TFile17DatabaseAgent Class
//  AUTHOR    : Maurice Marinus
//  DATE      : 06/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
unit UFile17DatabaseAgent;

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
  UIrrigationBlockObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject,
  UChannelDescriptionObject;

type

  TFile17DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF17UnkownDataSQL: string;
    function ReadIrrigationBlockNodesDataSQL: string;
    function ReadIrrigationBlockAPanConvFactorSQL: string;
    function ReadIrrigationBlockPanEvaporationSQL: string;
    function ReadIrrigationBlockRainfallFactorSQL: string;
    function ReadIrrigationBlockWaterUsageFactorSQL: string;

    function WriteF17UnkownDataSQL: string;
    function WriteIrrigationBlockNodesDataSQL: string;
    function WriteIrrigationBlockAPanConvFactorSQL: string;
    function WriteIrrigationBlockPanEvaporationSQL: string;
    function WriteIrrigationBlockRainfallFactorSQL: string;
    function WriteIrrigationBlockWaterUsageFactorSQL: string;
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

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TFile17DatabaseAgent.ReadF17UnkownDataSQL: string;
const OPNAME = 'TFile17DatabaseAgent.ReadF17UnkownDataSQL';
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

function TFile17DatabaseAgent.ReadIrrigationBlockNodesDataSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.ReadIrrigationBlockNodesDataSQL';
begin
  Result := '';
  try
    Result  := Format('SELECT Model ,StudyAreaName, SubArea, Scenario, Identifier, BlockNumber, BlockName, '+
                'MaxWaterAllocation, FileName, NodeNumber, CanalTransportLoss, EfficiencyFactor, '+
                'ReturnFlowFactor, UpperZoneReturnFlow, LowerZoneReturnFlow, ReturnFlowLoss, '+
                'UpperZoneSoilMoistureCapacity, LowerZoneSoilMoistureCapacity, UpperZoneSoilMoistureTarget, '+
                'InitialSoilMoistureStorage, RainAboveRainFactorSpecValue, RainBelowRainFactor, '+
                'RainCatchmentScalingFactor, AllocatedIrrigationArea,CropWaterUseType,DroughtApplicable '+
                'FROM IrrigationBlock '+
                'WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s '+
                'ORDER BY Model ,StudyAreaName, SubArea, Scenario, Identifier ',[QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.ReadIrrigationBlockAPanConvFactorSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.ReadIrrigationBlockAPanConvFactorSQL';
begin
  Result := '';
  try
    Result  := Format('SELECT Model ,StudyAreaName, SubArea, Scenario, Identifier,'+
                'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, Factor07, Factor08, '+
                'Factor09, Factor10, Factor11, Factor12 '+
                'FROM IrrigationBlockAPanConvFactor '+
                'WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s AND Identifier=:Identifier '+
                'ORDER BY Model, StudyAreaName, SubArea, Scenario, Identifier ',[QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.ReadIrrigationBlockPanEvaporationSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.ReadIrrigationBlockPanEvaporationSQL';
begin
  Result := '';
  try
    Result  := Format('SELECT Model ,StudyAreaName, SubArea, Scenario, Identifier,'+
                'Evaporation01, Evaporation02, Evaporation03, Evaporation04, Evaporation05, Evaporation06, '+
                'Evaporation07, Evaporation08, Evaporation09, Evaporation10, Evaporation11, Evaporation12 '+
                'FROM IrrigationBlockPanEvaporation '+
                'WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s AND Identifier=:Identifier '+
                'ORDER BY Model, StudyAreaName, SubArea, Scenario, Identifier ',[QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.ReadIrrigationBlockRainfallFactorSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.ReadIrrigationBlockRainfallFactorSQL';
begin
  Result := '';
  try
    Result  := Format('SELECT Model ,StudyAreaName, SubArea, Scenario, Identifier,'+
                'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, Factor07, Factor08, '+
                'Factor09, Factor10, Factor11, Factor12 '+
                'FROM IrrigationBlockRainfallFactor '+
                'WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s  AND Identifier=:Identifier '+
                'ORDER BY Model, StudyAreaName, SubArea, Scenario,Identifier ',[QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.ReadIrrigationBlockWaterUsageFactorSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.ReadIrrigationBlockWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result  := Format('SELECT Model ,StudyAreaName, SubArea, Scenario, BlockIdentifier, Identifier, '+
                'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, Factor07, Factor08, '+
                'Factor09, Factor10, Factor11, Factor12, PercAreaUnderCropType, CropName '+
                'FROM IrrigationBlockWaterUsageFactor '+
                'WHERE Model=%s AND StudyAreaName=%s AND SubArea=%s AND Scenario=%s AND BlockIdentifier=:BlockIdentifier '+
                'ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockIdentifier, Identifier ',
                                 [QuotedStr(FAppModules.StudyArea.ModelCode),
                                  QuotedStr(FAppModules.StudyArea.StudyAreaCode),
                                  QuotedStr(FAppModules.StudyArea.SubAreaCode),
                                  QuotedStr(FAppModules.StudyArea.ScenarioCode)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteIrrigationBlockAPanConvFactorSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.WriteIrrigationBlockAPanConvFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationBlockAPanConvFactor '+
                '(Model, StudyAreaName, SubArea, Scenario, Identifier,'+
                'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, '+
                'Factor07, Factor08, Factor09, Factor10, Factor11, Factor12) '+
                'Values '+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier,'+
                ':Factor01, :Factor02, :Factor03, :Factor04, :Factor05, :Factor06, '+
                ':Factor07, :Factor08, :Factor09, :Factor10, :Factor11, :Factor12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteIrrigationBlockNodesDataSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.WriteIrrigationBlockNodesDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationBlock '+
              '(Model ,StudyAreaName, SubArea, Scenario, Identifier, BlockNumber, BlockName, '+
              'MaxWaterAllocation, FileName, NodeNumber, CanalTransportLoss, EfficiencyFactor, '+
              'ReturnFlowFactor, UpperZoneReturnFlow, LowerZoneReturnFlow, ReturnFlowLoss, '+
              'UpperZoneSoilMoistureCapacity, LowerZoneSoilMoistureCapacity, UpperZoneSoilMoistureTarget, '+
              'InitialSoilMoistureStorage, RainAboveRainFactorSpecValue, '+
              'RainBelowRainFactor, RainCatchmentScalingFactor, AllocatedIrrigationArea,[Description], '+
              'CropWaterUseType, DroughtApplicable, DiversionChannelNumber, ReturnFlowChannelNumber) '+
              'Values '+
              '(:Model ,:StudyAreaName, :SubArea, :Scenario, :Identifier, :BlockNumber, :BlockName, '+
              ':MaxWaterAllocation, :FileName, :NodeNumber, :CanalTransportLoss, :EfficiencyFactor, '+
              ':ReturnFlowFactor, :UpperZoneReturnFlow, :LowerZoneReturnFlow, :ReturnFlowLoss, '+
              ':UpperZoneSoilMoistureCapacity, :LowerZoneSoilMoistureCapacity, :UpperZoneSoilMoistureTarget, '+
              ':InitialSoilMoistureStorage, :RainAboveRainFactorSpecValue, '+
              ':RainBelowRainFactor, :RainCatchmentScalingFactor, :AllocatedIrrigationArea, :Description,'+
              ':CropWaterUseType, :DroughtApplicable,:DiversionChannelNumber,:ReturnFlowChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteIrrigationBlockPanEvaporationSQL: string;
const OPNAME = 'TFile17DatabaseAgent.WriteIrrigationBlockPanEvaporationSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationBlockPanEvaporation '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier,'+
              'Evaporation01, Evaporation02, Evaporation03, Evaporation04, Evaporation05, Evaporation06, '+
              'Evaporation07, Evaporation08, Evaporation09, Evaporation10, Evaporation11, Evaporation12) '+
              'Values '+
              '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier,'+
              ':Evaporation01, :Evaporation02, :Evaporation03, :Evaporation04, :Evaporation05, :Evaporation06, '+
              ':Evaporation07, :Evaporation08, :Evaporation09, :Evaporation10, :Evaporation11, :Evaporation12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteIrrigationBlockRainfallFactorSQL: string;
const OPNAME = 'TFile17DatabaseAgent.WriteIrrigationBlockRainfallFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationBlockRainfallFactor '+
                '(Model, StudyAreaName, SubArea, Scenario, Identifier,'+
                'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, '+
                'Factor07, Factor08, Factor09, Factor10, Factor11, Factor12) '+
                'Values '+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier,'+
                ':Factor01, :Factor02, :Factor03, :Factor04, :Factor05, :Factor06, '+
                ':Factor07, :Factor08, :Factor09, :Factor10, :Factor11, :Factor12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteIrrigationBlockWaterUsageFactorSQL: string;
const
  OPNAME      = 'TFile17DatabaseAgent.WriteIrrigationBlockWaterUsageFactorSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationBlockWaterUsageFactor '+
                '(Model, StudyAreaName, SubArea, Scenario, Identifier, '+
                'Factor01, Factor02, Factor03, Factor04, Factor05, Factor06, '+
                'Factor07, Factor08, Factor09, Factor10, Factor11, Factor12, PercAreaUnderCropType, CropName, BlockIdentifier) '+
                'Values '+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, '+
                ':Factor01, :Factor02, :Factor03, :Factor04, :Factor05, :Factor06, '+
                ':Factor07, :Factor08, :Factor09, :Factor10, :Factor11, :Factor12, :PercAreaUnderCropType, :CropName, :BlockIdentifier)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteF17UnkownDataSQL: string;
const OPNAME = 'TFile17DatabaseAgent.WriteF17UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile17DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage                : string;
  LDataSet,
  LDataSet2               : TAbstractModelDataset;
  LCount,
  LCount2                 : Integer;
  LIrrigationBlockObject  : TIrrigationBlockObject;
  LIrrigationBlock        : TIrrigationBlock;
  LStop                   : Boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile17DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LIrrigationBlockObject := ADataObject.FIrrigationBlockObject;

    if not LIrrigationBlockObject.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet2);
    try
      LDataSet.SetSQL(ReadIrrigationBlockNodesDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile17DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LIrrigationBlock  := LIrrigationBlockObject.AddIrrigationBlock;

          if not LDataSet.DataSet.FieldByName('BlockName').IsNull then
          begin
            LIrrigationBlock.BlockName.FData        := Trim(LDataSet.DataSet.FieldByName('BlockName').AsString);
            LIrrigationBlock.BlockName.FLength      := Length(LIrrigationBlock.BlockName.FData);
            LIrrigationBlock.BlockName.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('BlockNumber').IsNull then
          begin
            LIrrigationBlock.BlockNodeNumber.FData        := LDataSet.DataSet.FieldByName('BlockNumber').AsInteger;
            LIrrigationBlock.BlockNodeNumber.FInitalised  := True;
          end;
          if not LDataSet.DataSet.FieldByName('MaxWaterAllocation').IsNull then
          begin
            LIrrigationBlock.MaxWaterAllocation.FData := LDataSet.DataSet.FieldByName('MaxWaterAllocation').AsFloat;
            LIrrigationBlock.MaxWaterAllocation.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('FileName').IsNull then
          begin
            LIrrigationBlock.FileName.FData := Trim(LDataSet.DataSet.FieldByName('FileName').AsString);
            LIrrigationBlock.FileName.FLength := Length(LIrrigationBlock.FileName.FData);
            LIrrigationBlock.FileName.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('NodeNumber').IsNull then
          begin
            LIrrigationBlock.HydrologyNodeNumber.FData := LDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
            LIrrigationBlock.HydrologyNodeNumber.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('CanalTransportLoss').IsNull then
          begin
            LIrrigationBlock.CanalTransportLoss.FData := LDataSet.DataSet.FieldByName('CanalTransportLoss').AsFloat;
            LIrrigationBlock.CanalTransportLoss.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('EfficiencyFactor').IsNull then
          begin
            LIrrigationBlock.EfficiencyFactor.FData :=LDataSet.DataSet.FieldByName('EfficiencyFactor').AsFloat;
            LIrrigationBlock.EfficiencyFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('ReturnFlowFactor').IsNull then
          begin
            LIrrigationBlock.ReturnFlowFactor.FData :=LDataSet.DataSet.FieldByName('ReturnFlowFactor').AsFloat;
            LIrrigationBlock.ReturnFlowFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('UpperZoneReturnFlow').IsNull then
          begin
            LIrrigationBlock.UpperZoneReturnFlow.FData :=LDataSet.DataSet.FieldByName('UpperZoneReturnFlow').AsFloat;
            LIrrigationBlock.UpperZoneReturnFlow.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('LowerZoneReturnFlow').IsNull then
          begin
            LIrrigationBlock.LowerZoneReturnFlow.FData :=LDataSet.DataSet.FieldByName('LowerZoneReturnFlow').AsFloat;
            LIrrigationBlock.LowerZoneReturnFlow.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('ReturnFlowLoss').IsNull then
          begin
            LIrrigationBlock.ReturnFlowLoss.FData :=LDataSet.DataSet.FieldByName('ReturnFlowLoss').AsFloat;
            LIrrigationBlock.ReturnFlowLoss.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('UpperZoneSoilMoistureCapacity').IsNull then
          begin
            LIrrigationBlock.UpperZoneSoilMoistureCapacity.FData :=LDataSet.DataSet.FieldByName('UpperZoneSoilMoistureCapacity').AsFloat;
            LIrrigationBlock.UpperZoneSoilMoistureCapacity.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('LowerZoneSoilMoistureCapacity').IsNull then
          begin
            LIrrigationBlock.LowerZoneSoilMoistureCapacity.FData :=LDataSet.DataSet.FieldByName('LowerZoneSoilMoistureCapacity').AsFloat;
            LIrrigationBlock.LowerZoneSoilMoistureCapacity.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('UpperZoneSoilMoistureTarget').IsNull then
          begin
            LIrrigationBlock.UpperZoneSoilMoistureTarget.FData :=LDataSet.DataSet.FieldByName('UpperZoneSoilMoistureTarget').AsFloat;
            LIrrigationBlock.UpperZoneSoilMoistureTarget.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('InitialSoilMoistureStorage').IsNull then
          begin
            LIrrigationBlock.InitialSoilMoistureStorage.FData :=LDataSet.DataSet.FieldByName('InitialSoilMoistureStorage').AsFloat;
            LIrrigationBlock.InitialSoilMoistureStorage.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('RainAboveRainFactorSpecValue').IsNull then
          begin
            LIrrigationBlock.RainAboveRainFactorSpecValue.FData :=LDataSet.DataSet.FieldByName('RainAboveRainFactorSpecValue').AsFloat;
            LIrrigationBlock.RainAboveRainFactorSpecValue.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('RainBelowRainFactor').IsNull then
          begin
            LIrrigationBlock.RainBelowRainFactor.FData :=LDataSet.DataSet.FieldByName('RainBelowRainFactor').AsFloat;
            LIrrigationBlock.RainBelowRainFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('RainCatchmentScalingFactor').IsNull then
          begin
            LIrrigationBlock.RainCatchmentScalingFactor.FData :=LDataSet.DataSet.FieldByName('RainCatchmentScalingFactor').AsFloat;
            LIrrigationBlock.RainCatchmentScalingFactor.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('AllocatedIrrigationArea').IsNull then
          begin
            LIrrigationBlock.AllocatedIrrigationArea.FData :=LDataSet.DataSet.FieldByName('AllocatedIrrigationArea').AsFloat;
            LIrrigationBlock.AllocatedIrrigationArea.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('Identifier').IsNull then
          begin
            LIrrigationBlock.BlockIdentifier.FData       := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LIrrigationBlock.BlockIdentifier.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('CropWaterUseType').IsNull then
          begin
            LIrrigationBlock.CropWaterUseType.FData       := LDataSet.DataSet.FieldByName('CropWaterUseType').AsInteger;
            LIrrigationBlock.CropWaterUseType.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('DroughtApplicable').IsNull then
          begin
            LIrrigationBlock.DroughtApplicable.FData       := LDataSet.DataSet.FieldByName('DroughtApplicable').AsInteger;
            LIrrigationBlock.DroughtApplicable.FInitalised := True;
          end;

          // APan
          LDataSet2.SetSQL(ReadIrrigationBlockAPanConvFactorSQL);
          LDataSet2.SetParams(['Identifier'], [IntToStr(LIrrigationBlock.BlockIdentifier.FData)]);
          LDataSet2.DataSet.Open;
          while not LDataSet2.DataSet.Eof do
          begin
            for LCount2 := MinMonths to MaxMonths do
            begin
              LFieldName := Format('%s%2.2d',['Factor',LCount2]);
              if not LDataSet2.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LIrrigationBlock.APanConvFactor[LCount2].FData       := LDataSet2.DataSet.FieldByName(LFieldName).AsFloat;
                LIrrigationBlock.APanConvFactor[LCount2].FInitalised := True;
              end
              else
                Break;
            end;
            LDataSet2.DataSet.Next;
          end;
          LDataSet2.DataSet.Close;

          // Pan
          LDataSet2.SetSQL(ReadIrrigationBlockPanEvaporationSQL);
          LDataSet2.SetParams(['Identifier'], [IntToStr(LIrrigationBlock.BlockIdentifier.FData)]);
          LDataSet2.DataSet.Open;
          while not LDataSet2.DataSet.Eof do
          begin
            for LCount2 := MinMonths to MaxMonths do
            begin
              LFieldName := Format('%s%2.2d',['Evaporation',LCount2]);
              if not LDataSet2.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LIrrigationBlock.PanEvaporation[LCount2].FData       := LDataSet2.DataSet.FieldByName(LFieldName).AsFloat;
                LIrrigationBlock.PanEvaporation[LCount2].FInitalised := True;
              end
              else
                Break;
            end;
            LDataSet2.DataSet.Next;
          end;
          LDataSet2.DataSet.Close;

          // Rainfall
          LDataSet2.SetSQL(ReadIrrigationBlockRainfallFactorSQL);
          LDataSet2.SetParams(['Identifier'], [IntToStr(LIrrigationBlock.BlockIdentifier.FData)]);
          LDataSet2.DataSet.Open;
          while not LDataSet2.DataSet.Eof do
          begin
            for LCount2 := MinMonths to MaxMonths do
            begin
              LFieldName := Format('%s%2.2d',['Factor',LCount2]);
              if not LDataSet2.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LIrrigationBlock.RainfallFactor[LCount2].FData       := LDataSet2.DataSet.FieldByName(LFieldName).AsFloat;
                LIrrigationBlock.RainfallFactor[LCount2].FInitalised := True;
              end
              else
                Break;
            end;
            LDataSet2.DataSet.Next;
          end;
          LDataSet2.DataSet.Close;

          // Water usage
          LDataSet2.SetSQL(ReadIrrigationBlockWaterUsageFactorSQL);
          LDataSet2.SetParams(['BlockIdentifier'], [IntToStr(LIrrigationBlock.BlockIdentifier.FData)]);
          LDataSet2.DataSet.Open;

          LCount := 1;
          LIrrigationBlock.NumberOfCropTypes.FData := 0;
          while not LDataSet2.DataSet.Eof do
          begin
            Inc(LIrrigationBlock.NumberOfCropTypes.FData);
            for LCount2 := MinMonths to MaxMonths do
            begin
              LFieldName := Format('%s%2.2d',['Factor',LCount2]);
              if not LDataSet2.DataSet.FieldByName(LFieldName).IsNull then
              begin
                if not Assigned(LIrrigationBlock.WaterUsageFactor[LCount]) then
                  LIrrigationBlock.WaterUsageFactor[LCount] := TWaterUsage.Create;
                LIrrigationBlock.WaterUsageFactor[LCount].MonthlyWaterUse[LCount2].FData       := LDataSet2.DataSet.FieldByName(LFieldName).AsFloat;
                LIrrigationBlock.WaterUsageFactor[LCount].MonthlyWaterUse[LCount2].FInitalised := True;
              end
              else
                Break;
            end;
            if not LDataSet2.DataSet.FieldByName('PercAreaUnderCropType').IsNull then
            begin
              LIrrigationBlock.WaterUsageFactor[LCount].PercAreaUnderCropType.FData        := LDataSet2.DataSet.FieldByName('PercAreaUnderCropType').AsFloat;
              LIrrigationBlock.WaterUsageFactor[LCount].PercAreaUnderCropType.FInitalised  := True;
            end;
            if not LDataSet2.DataSet.FieldByName('Identifier').IsNull then
            begin
              LIrrigationBlock.WaterUsageFactor[LCount].Identifier.FData       := LDataSet2.DataSet.FieldByName('Identifier').AsInteger;
              LIrrigationBlock.WaterUsageFactor[LCount].Identifier.FInitalised := True;
            end;
            if not LDataSet2.DataSet.FieldByName('CropName').IsNull then
            begin
              LIrrigationBlock.WaterUsageFactor[LCount].CropName.FData       := Trim(LDataSet2.DataSet.FieldByName('CropName').AsString);
              LIrrigationBlock.WaterUsageFactor[LCount].CropName.FLength     := Length(LIrrigationBlock.WaterUsageFactor[LCount].CropName.FData);
              LIrrigationBlock.WaterUsageFactor[LCount].CropName.FInitalised := True;
            end;
            if not LDataSet2.DataSet.FieldByName('BlockIdentifier').IsNull then
            begin
              LIrrigationBlock.WaterUsageFactor[LCount].BlockIdentifier.FData       := LDataSet2.DataSet.FieldByName('BlockIdentifier').AsInteger;
              LIrrigationBlock.WaterUsageFactor[LCount].BlockIdentifier.FInitalised := True;
            end;
            LDataSet2.DataSet.Next;
            Inc(LCount);            
          end;
          LDataSet2.DataSet.Close;


          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF17UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LIrrigationBlockObject.ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile17DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile17DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage                : string;
  LCount,
  LLinesCount,
  LCounter,
  LNoOfCropTypes          : Integer;
  LStop                   : Boolean;
  LDataSet                : TAbstractModelDataset;
  LIrrigationBlockObject  : TIrrigationBlockObject;
  LIrrigationBlock        : TIrrigationBlock;
  LIrrigationBlockChannel : TIrrigationBlockChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile17DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');
    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;
    LIrrigationBlockObject := ADataObject.FIrrigationBlockObject;
    if not Assigned(LIrrigationBlockObject) then
      Exit;

    FAppModules.Database.CreateDataset(Integer(dtExecSQL), LDataSet);
    try
      for LLinesCount := 0 to LIrrigationBlockObject.IrrigationBlockCount-1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIrrigationBlockNodesDataSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        LIrrigationBlock := LIrrigationBlockObject.IrrigationBlockObjectByIndex[LLinesCount];
        if LIrrigationBlock.BlockName.FInitalised then
          LDataSet.SetParams(['BlockName'], [LIrrigationBlock.BlockName.FData]);

        if LIrrigationBlock.BlockNodeNumber.FInitalised then
          LDataSet.SetParams(['BlockNumber'], [IntToStr(LIrrigationBlock.BlockNodeNumber.FData)]);
        if LIrrigationBlock.MaxWaterAllocation.FInitalised then
          LDataSet.SetParams(['MaxWaterAllocation'], [FloatToStr(LIrrigationBlock.MaxWaterAllocation.FData)]);
        if LIrrigationBlock.FileName.FInitalised then
          LDataSet.SetParams(['FileName'], [LIrrigationBlock.FileName.FData]);
        if LIrrigationBlock.HydrologyNodeNumber.FInitalised then
          LDataSet.SetParams(['NodeNumber'], [FloatToStr(LIrrigationBlock.HydrologyNodeNumber.FData)]);
        if LIrrigationBlock.CanalTransportLoss.FInitalised then
          LDataSet.SetParams(['CanalTransportLoss'], [FloatToStr(LIrrigationBlock.CanalTransportLoss.FData)]);
        if LIrrigationBlock.EfficiencyFactor.FInitalised then
          LDataSet.SetParams(['EfficiencyFactor'], [FloatToStr(LIrrigationBlock.EfficiencyFactor.FData)]);
        if LIrrigationBlock.ReturnFlowFactor.FInitalised then
          LDataSet.SetParams(['ReturnFlowFactor'], [FloatToStr(LIrrigationBlock.ReturnFlowFactor.FData)]);
        if LIrrigationBlock.UpperZoneReturnFlow.FInitalised then
          LDataSet.SetParams(['UpperZoneReturnFlow'], [FloatToStr(LIrrigationBlock.UpperZoneReturnFlow.FData)]);
        if LIrrigationBlock.LowerZoneReturnFlow.FInitalised then
          LDataSet.SetParams(['LowerZoneReturnFlow'], [FloatToStr(LIrrigationBlock.LowerZoneReturnFlow.FData)]);
        if LIrrigationBlock.ReturnFlowLoss.FInitalised then
          LDataSet.SetParams(['ReturnFlowLoss'], [FloatToStr(LIrrigationBlock.ReturnFlowLoss.FData)]);
        if LIrrigationBlock.UpperZoneSoilMoistureCapacity.FInitalised then
          LDataSet.SetParams(['UpperZoneSoilMoistureCapacity'], [FloatToStr(LIrrigationBlock.UpperZoneSoilMoistureCapacity.FData)]);
        if LIrrigationBlock.LowerZoneSoilMoistureCapacity.FInitalised then
          LDataSet.SetParams(['LowerZoneSoilMoistureCapacity'], [FloatToStr(LIrrigationBlock.LowerZoneSoilMoistureCapacity.FData)]);
        if LIrrigationBlock.UpperZoneSoilMoistureTarget.FInitalised then
          LDataSet.SetParams(['UpperZoneSoilMoistureTarget'], [FloatToStr(LIrrigationBlock.UpperZoneSoilMoistureTarget.FData)]);
        if LIrrigationBlock.InitialSoilMoistureStorage.FInitalised then
          LDataSet.SetParams(['InitialSoilMoistureStorage'], [FloatToStr(LIrrigationBlock.InitialSoilMoistureStorage.FData)]);
        if LIrrigationBlock.RainAboveRainFactorSpecValue.FInitalised then
          LDataSet.SetParams(['RainAboveRainFactorSpecValue'], [FloatToStr(LIrrigationBlock.RainAboveRainFactorSpecValue.FData)]);
        if LIrrigationBlock.RainBelowRainFactor.FInitalised then
          LDataSet.SetParams(['RainBelowRainFactor'], [FloatToStr(LIrrigationBlock.RainBelowRainFactor.FData)]);
        if LIrrigationBlock.RainCatchmentScalingFactor.FInitalised then
          LDataSet.SetParams(['RainCatchmentScalingFactor'], [FloatToStr(LIrrigationBlock.RainCatchmentScalingFactor.FData)]);
        if LIrrigationBlock.AllocatedIrrigationArea.FInitalised then
          LDataSet.SetParams(['AllocatedIrrigationArea'], [FloatToStr(LIrrigationBlock.AllocatedIrrigationArea.FData)]);
        if LIrrigationBlock.BlockName.FInitalised then
          LDataSet.SetParams(['IBDescription'], [LIrrigationBlock.BlockName.FData]);
        if LIrrigationBlock.CropWaterUseType.FInitalised then
          LDataSet.SetParams(['CropWaterUseType'], [FloatToStr(LIrrigationBlock.CropWaterUseType.FData)]);
        if LIrrigationBlock.DroughtApplicable.FInitalised then
          LDataSet.SetParams(['DroughtApplicable'], [FloatToStr(LIrrigationBlock.DroughtApplicable.FData)]);

        if LIrrigationBlock.BlockNodeNumber.FInitalised then
        begin
          LIrrigationBlockChannel := ADataObject.FChannelDescrObject.FindIrrigationBlockByNodeNumber(LIrrigationBlock.BlockNodeNumber.FData);
          if(LIrrigationBlockChannel <> nil) then
          begin
            if  LIrrigationBlockChannel.FAbstractChannelNr.FInitalised then
              LDataSet.SetParams(['DiversionChannelNumber'], [IntToStr(LIrrigationBlockChannel.FAbstractChannelNr.FData)]);
            if  LIrrigationBlockChannel.FReturnFlowChannelNr.FInitalised then
              LDataSet.SetParams(['ReturnFlowChannelNumber'], [IntToStr(LIrrigationBlockChannel.FReturnFlowChannelNr.FData)]);
          end;
        end;
        LDataSet.ExecSQL;

        // IrrigationBlockAPanConvFactor
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIrrigationBlockAPanConvFactorSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);
        
        for LCount := MinMonths to MaxMonths do
        begin
          if not LIrrigationBlock.APanConvFactor[LCount].FInitalised then
            Break;
          LFieldName := Format('%s%2.2d',['Factor',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LIrrigationBlock.APanConvFactor[LCount].FData)]);
        end;
        LDataSet.ExecSQL;

        // IrrigationBlockPanEvaporation
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIrrigationBlockPanEvaporationSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        for LCount := MinMonths to MaxMonths do
        begin
          if not LIrrigationBlock.PanEvaporation[LCount].FInitalised then
            Break;
          LFieldName := Format('%s%2.2d',['Evaporation',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LIrrigationBlock.PanEvaporation[LCount].FData)]);
        end;
        LDataSet.ExecSQL;

        // IrrigationBlockRainfallFactor
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIrrigationBlockRainfallFactorSQL);
        LDataSet.ClearQueryParams();

        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        for LCount := MinMonths to MaxMonths do
        begin
          if not LIrrigationBlock.RainfallFactor[LCount].FInitalised then
            Break;
          LFieldName := Format('%s%2.2d',['Factor',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LIrrigationBlock.RainfallFactor[LCount].FData)]);
        end;
        LDataSet.ExecSQL;

        //IrrigationBlockWaterUsageFactor
        LDataSet.DataSet.Close;
        for LNoOfCropTypes := MinNoOfCrops to LIrrigationBlock.NumberOfCropTypes.FData do
        begin
          LDataSet.SetSQL(WriteIrrigationBlockWaterUsageFactorSQL);
          LDataSet.ClearQueryParams();

          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['BlockIdentifier'], [IntToStr(LLinesCount+1)]);

          for LCount := MinMonths to MaxMonths do
          begin
            if Assigned(LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes]) then
            begin
              if not LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].MonthlyWaterUse[MinMonths].FInitalised then
                Break;
              LFieldName := Format('%s%2.2d',['Factor',LCount]);
              LDataSet.SetParams([LFieldName], [FloatToStr(LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].MonthlyWaterUse[LCount].FData)]);
              if LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].Identifier.FInitalised then
                LDataSet.SetParams(['Identifier'], [IntToStr(LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].Identifier.FData)]);
              if LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].PercAreaUnderCropType.FInitalised then
                LDataSet.SetParams(['PercAreaUnderCropType'], [FloatToStr(LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].PercAreaUnderCropType.FData)]);
              if LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].CropName.FInitalised then
                LDataSet.SetParams(['CropName'], [LIrrigationBlock.WaterUsageFactor[LNoOfCropTypes].CropName.FData]);
            end;
          end;
          LDataSet.ExecSQL;
        end;
      end;

      for LCounter := 0 to LIrrigationBlockObject.ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF17UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LIrrigationBlockObject.ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile17DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile17DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile17DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage  : string;
  LStop     : boolean;
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

    LTableNames := ' IrrigationBlock, IrrigationBlockAPanConvFactor, IrrigationBlockPanEvaporation,'+
                   ' IrrigationBlockRainfallFactor, IrrigationBlockWaterUsageFactor ';
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
