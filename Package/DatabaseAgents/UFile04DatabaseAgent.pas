//
//
//  UNIT      : Contains TFile04DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 12/02/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile04DatabaseAgent;

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
  UFlowConstraintsObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject,
  VoaimsCom_TLB;


type

  TFile04DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF04KnownDataSQL: string;
    function ReadPhysicalFlowConstraintValuesSQL(ARecordID : integer) : string;
    function ReadF04UnkownDataSQL: string;

    function WriteConstraintsDetailsDataSQL: string;
    function WriteUnknownDataSQL: string;
    function WritePhysicalFlowConstraintValuesSQL : string;

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

function TFile04DatabaseAgent.ReadF04KnownDataSQL: string;
const OPNAME = 'TFile04DatabaseAgent.ReadF04KnownDataSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ConstraintsChannelNumber, FeatureName, UpStreamReservoirNumber, DownStreamReservoirNumber, ' +
      'PointsElevationNumber, SillElevation, GateHeight, StructureType, ' +
      'DischargeCoefficient, ControlStructureLength, ' +
      'WaterLevelAtDownstreamNode, ReferenceElevation ' +
      'FROM FlowConstraints ' +
      'WHERE Model        = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
      ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
      ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
      ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' ORDER BY Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.WriteConstraintsDetailsDataSQL: string;
const OPNAME = 'TFile04DatabaseAgent.WriteConstraintsDetailsDataSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO FlowConstraints ' +
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'ConstraintsChannelNumber, FeatureName, UpStreamReservoirNumber, ' +
      'DownStreamReservoirNumber, PointsElevationNumber, SillElevation, ' +
      'GateHeight, StructureType, DischargeCoefficient, ControlStructureLength, ' +
      'WaterLevelAtDownstreamNode, ReferenceElevation)' +
      'Values'+
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ':ConstraintsChannelNumber, :FeatureName, :UpStreamReservoirNumber, ' +
      ':DownStreamReservoirNumber, :PointsElevationNumber, :SillElevation, ' +
      ':GateHeight, :StructureType, :DischargeCoefficient, :ControlStructureLength, ' +
      ':WaterLevelAtDownstreamNode, :ReferenceElevation)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.ReadPhysicalFlowConstraintValuesSQL(ARecordID : integer): string;
const OPNAME = 'TFile04DatabaseAgent.ReadPhysicalFlowConstraintValuesSQL';
begin
  Result := '';
  try
    Result :=
      'SELECT Identifier , '+
      ' GroupNumber,SubGroupNumber, LineNumber, '+
      ' Value01, Value02, Value03, Value04, Value05, '+
      ' Value06, Value07, Value08, Value09, Value10  '+
      ' FROM FlowConstraintsValue  ' +
      'WHERE Model        = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
      ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
      ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
      ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' AND (Identifier = ' + IntToStr(ARecordID) + ') '+
      ' ORDER BY Identifier ,GroupNumber, SubGroupNumber, LineNumber';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.WritePhysicalFlowConstraintValuesSQL: string;
const OPNAME = 'TFile04DatabaseAgent.WritePhysicalFlowConstraintValuesSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO FlowConstraintsValue ' +
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      ' GroupNumber,SubGroupNumber, LineNumber, '+
      ' Value01, Value02, Value03, Value04, Value05, '+
      ' Value06, Value07, Value08, Value09, Value10  )'+
      ' Values '+
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ' :GroupNumber, :SubGroupNumber, :LineNumber, '+
      ' :Value01, :Value02, :Value03, :Value04, :Value05, '+
      ' :Value06, :Value07, :Value08, :Value09, :Value10  )';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.ReadF04UnkownDataSQL: string;
const OPNAME = 'TFile04DatabaseAgent.ReadF04UnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData,FileType'+
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

function TFile04DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile04DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile04DatabaseAgent.ReadModelDataFromDatabase';
var
  LSQL             : string;
  LMessage         : string;
  LDataSet         : TAbstractModelDataset;
  LGroupNumber     : integer;
  LSubGroupNumber  : integer;
  LRecordCount,
  LCount           : Integer;
  LNoData          : Boolean;
  LFlowConstraints : TFlowConstraintsObject;
  LStop            : boolean;
  lRow             : integer;
  lCol             : integer;
  LStructure       : TStructure;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile04DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFlowConstraints := ADataObject.FFlowConstraintsObject;

    if not LFlowConstraints.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := 'SELECT COUNT(*) AS RecordCount FROM FlowConstraints  ' +
           ' WHERE Model        = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
           ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
           ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
           ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode);
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;

      LRecordCount := LDataSet.DataSet.FieldByName('RecordCount').AsInteger;
      //Check if there is any data.
      LNoData := False;
      if (LRecordCount = 0) then
      begin
        LNoData := True;
        LMessage := FAppModules.Language.GetString('TFile04DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      if (LRecordCount > 0) then
      begin

        //Read the F04 file
        // Line 1
        LFlowConstraints.FControlStructureCount.FData := LRecordCount;
        LFlowConstraints.FControlStructureCount.FInitalised := True;

        if not LFlowConstraints.AddStructures then
          Exit;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadF04KnownDataSQL);
        LDataSet.DataSet.Open;

        // Line 2
        for LCount := 0 to LFlowConstraints.FControlStructureCount.FData-1 do
        begin
          if LDataSet.DataSet.Eof then Break;
          LStructure       := TStructure(LFlowConstraints.FStructure[LCount]);

          LStructure.FIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;

          if not LDataSet.DataSet.FieldByName('ConstraintsChannelNumber').IsNull then
          begin
            LStructure.FChannelNum.FData := LDataSet.DataSet.FieldByName('ConstraintsChannelNumber').AsInteger;
            LStructure.FChannelNum.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('UpStreamReservoirNumber').IsNull then
          begin
            LStructure.FUpStreamReservoirNum.FData := LDataSet.DataSet.FieldByName('UpStreamReservoirNumber').AsInteger;
            LStructure.FUpStreamReservoirNum.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('DownStreamReservoirNumber').IsNull then
          begin
            LStructure.FDownStreamReservoirNum.FData := LDataSet.DataSet.FieldByName('DownStreamReservoirNumber').AsInteger;
            LStructure.FDownStreamReservoirNum.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('PointsElevationNumber').IsNull then
          begin
            LStructure.FPointsElevationNum.FData := LDataSet.DataSet.FieldByName('PointsElevationNumber').AsInteger;
            LStructure.FPointsElevationNum.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('SillElevation').IsNull then
          begin
            LStructure.FSillElevation.FData := LDataSet.DataSet.FieldByName('SillElevation').AsFloat;
            LStructure.FSillElevation.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('GateHeight').IsNull then
          begin
            LStructure.FGateHeight.FData := LDataSet.DataSet.FieldByName('GateHeight').AsFloat;
            LStructure.FGateHeight.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('StructureType').IsNull then
          begin
            LStructure.FStructureType.FData := LDataSet.DataSet.FieldByName('StructureType').AsInteger;
            LStructure.FStructureType.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('DischargeCoefficient').IsNull then
          begin
            LStructure.FDischargeCoefficient.FData := LDataSet.DataSet.FieldByName('DischargeCoefficient').AsFloat;
            LStructure.FDischargeCoefficient.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('WaterLevelAtDownstreamNode').IsNull then
          begin
            LStructure.FWaterLevelAtDownstreamNode.FData := LDataSet.DataSet.FieldByName('WaterLevelAtDownstreamNode').AsFloat;
            LStructure.FWaterLevelAtDownstreamNode.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('ReferenceElevation').IsNull then
          begin
            LStructure.FReservoirElevation.FData := LDataSet.DataSet.FieldByName('ReferenceElevation').AsFloat;
            LStructure.FReservoirElevation.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('ControlStructureLength').IsNull then
          begin
            LStructure.FControlStructureLength.FData := LDataSet.DataSet.FieldByName('ControlStructureLength').AsFloat;
            LStructure.FControlStructureLength.FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;

        for LCount := 0 to LFlowConstraints.FControlStructureCount.FData-1 do
        begin
          LStructure       := TStructure(LFlowConstraints.FStructure[LCount]);
          if not (LStructure.FStructureType.FData in [4,5,7,8,9,10,11,12,13,14]) then Continue;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(ReadPhysicalFlowConstraintValuesSQL(LStructure.FIdentifier));
          LDataSet.DataSet.Open;
          if (LDataset.DataSet.Bof and LDataset.DataSet.EOF) then Continue;

          LGroupNumber := LDataset.DataSet.FieldByName('GroupNumber').AsInteger;
          while (not LDataset.DataSet.EOF)  do
          begin
            LSubGroupNumber := LDataset.DataSet.FieldByName('SubGroupNumber').AsInteger;
            case LGroupNumber of
              pfcgDischargeCurve:
                begin
                  for LCol := 1 to LStructure.FPointsElevationNum.FData do
                  begin
                    if(LSubGroupNumber = pfcstElevation) then
                    begin
                      LStructure.FElevations[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FElevations[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstDischarge) then
                    begin
                      LStructure.FDischarges[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FDischarges[LCol].FInitalised := True;
                    end;
                  end;
                end;
              pfcgKFactors:
                begin
                  for LCol := 1 to LStructure.FPointsElevationNum.FData do
                  begin
                    if(LSubGroupNumber = pfcstChannelNumber) then
                    begin
                      LStructure.FChannelNumbers[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FChannelNumbers[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstKFactor) then
                    begin
                      LStructure.FKFactors[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FKFactors[LCol].FInitalised := True;
                    end;
                  end;
                end;
              pfcgSandAquifer:
                begin
                  for LCol := 1 to LStructure.FPointsElevationNum.FData do
                  begin
                    if(LSubGroupNumber = pfcstHeadDifference) then
                    begin
                      LStructure.FHeadDifferences[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FHeadDifferences[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstAquiferFlow) then
                    begin
                      LStructure.FAquiferFlows[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FAquiferFlows[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstDownStreamNodeInflow) then
                    begin
                      LStructure.FDownStreamNodeInflows[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FDownStreamNodeInflows[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstRiverDepth) then
                    begin
                      LStructure.FRiverDepths[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FRiverDepths[LCol].FInitalised := True;
                    end;
                  end;
                end;
              pfcgSubmergedOutlet:
                begin
                  for LCol := 1 to 10 do
                  begin
                    if(LSubGroupNumber = pfcstElevationDifference) then
                    begin
                      LStructure.FElevationDifferences[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FElevationDifferences[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstMonthlyAverageInflow) then
                    begin
                      LStructure.FMonthlyAverageInflows[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FMonthlyAverageInflows[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstMonthlyAverageDivertedFlow) then
                    begin
                      LRow := LDataset.DataSet.FieldByName('LineNumber').AsInteger;
                      LStructure.FMonthlyAverageDivertedFlow[LRow,LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FMonthlyAverageDivertedFlow[LRow,LCol].FInitalised := True;
                    end;
                  end;
                end;
              pfcgPumpStation:
                begin
                  for LCol := 1 to LStructure.FPointsElevationNum.FData do
                  begin
                    if(LSubGroupNumber = pfcstPumpingHead) then
                    begin
                      LStructure.FPumpingHeads[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FPumpingHeads[LCol].FInitalised := True;
                    end;
                    if(LSubGroupNumber = pfcstPumpingDischarge) then
                    begin
                      LStructure.FPumpingDischarges[LCol].FData := LDataset.DataSet.FieldByName(Format('Value%2.2d',[LCol])).AsFloat;
                      LStructure.FPumpingDischarges[LCol].FInitalised := True;
                    end;
                  end;
                end;
            end;//case
            LDataset.DataSet.Next;
          end;
        end;
      end;

      //Line 5 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF04UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      //Check if there is any unknown data.
      LNoData := LNoData and (LDataSet.DataSet.RecordCount = 0);

      while not LDataSet.DataSet.Eof do
      begin
        LFlowConstraints.FF04ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile04DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  not LNoData;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile04DatabaseAgent.WriteModelDataToDatabase';
procedure PrepareInsertValues(ADataSet : TAbstractModelDataset;AStructure : TStructure);
const OPNAME = 'UFile04DatabaseAgent.PrepareInsertValues';
begin
  ADataSet.DataSet.Close;
  ADataSet.SetSQL(WritePhysicalFlowConstraintValuesSQL);
  ADataSet.ClearQueryParams();
  ADataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
  ADataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
  ADataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
  ADataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
  ADataSet.SetParams(['Identifier'], [IntToStr(AStructure.FIdentifier)]);
end;
var
  LFeatureName,
  LMessage         : string;
  LCount,
  LCounter         : integer;
  LDataSet         : TAbstractModelDataset;
  LFlowConstraints : TFlowConstraintsObject;
  LStop            : boolean;
  lRow             : integer;
  lCol             : integer;
  lStructure       : TStructure;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile04DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LFlowConstraints := ADataObject.FFlowConstraintsObject;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if LFlowConstraints.FControlStructureCount.FInitalised and
         (LFlowConstraints.FControlStructureCount.FData > 0) then
      begin

        //Line 2++++++++++++++++++++++++++++
        for LCount := 0 to LFlowConstraints.FControlStructureCount.FData -1 do
        begin
          LStructure       := TStructure(LFlowConstraints.FStructure[LCount]);
          LFeatureName     := UpperCase(FAppModules.Language.GetString('NetworkFeatures.PhysicalFlowConstraint')) +
                              ' ' + IntToStr(LStructure.FIdentifier);

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteConstraintsDetailsDataSQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LStructure.FIdentifier)]);
          LDataSet.SetParams(['FeatureName'], [LFeatureName]);
          if LStructure.FChannelNum.FInitalised  then
            LDataSet.SetParams(['ConstraintsChannelNumber'], [IntToStr(LStructure.FChannelNum.FData)]);

          if LStructure.FUpStreamReservoirNum.FInitalised then
           LDataSet.SetParams(['UpStreamReservoirNumber'], [IntToStr(LStructure.FUpStreamReservoirNum.FData)]);

          if LStructure.FDownStreamReservoirNum.FInitalised then
           LDataSet.SetParams(['DownStreamReservoirNumber'], [IntToStr(LStructure.FDownStreamReservoirNum.FData)]);

          if LStructure.FPointsElevationNum.FInitalised then
           LDataSet.SetParams(['PointsElevationNumber'], [IntToStr(LStructure.FPointsElevationNum.FData)]);

          if LStructure.FSillElevation.FInitalised then
           LDataSet.SetParams(['SillElevation'], [FloatToStr(LStructure.FSillElevation.FData)]);

          if LStructure.FGateHeight.FInitalised then
           LDataSet.SetParams(['GateHeight'], [FloatToStr(LStructure.FGateHeight.FData)]);

          if LStructure.FStructureType.FInitalised then
           LDataSet.SetParams(['StructureType'], [IntToStr(LStructure.FStructureType.FData)]);

          if LStructure.FDischargeCoefficient.FInitalised then
           LDataSet.SetParams(['DischargeCoefficient'], [FloatToStr(LStructure.FDischargeCoefficient.FData)]);

          if LStructure.FControlStructureLength.FInitalised then
           LDataSet.SetParams(['ControlStructureLength'], [FloatToStr(LStructure.FControlStructureLength.FData)]);

          if LStructure.FWaterLevelAtDownstreamNode.FInitalised then
           LDataSet.SetParams(['WaterLevelAtDownstreamNode'], [FloatToStr(LStructure.FWaterLevelAtDownstreamNode.FData)]);


          LDataSet.ExecSQL;
        end;

        for LCount := 0 to LFlowConstraints.FControlStructureCount.FData -1 do
        begin
          LStructure   := TStructure(LFlowConstraints.FStructure[LCount]);
          case LStructure.FStructureType.FData of
            4,5,7,8,9,14:
            begin
              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgDischargeCurve)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstElevation)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FElevations[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgDischargeCurve)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstDischarge)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FDischarges[LCol].FData)]);
              LDataSet.ExecSQL;
            end;

            10:
            begin
              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgKFactors)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstChannelNumber)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FChannelNumbers[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgKFactors)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstKFactor)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FKFactors[LCol].FData)]);
              LDataSet.ExecSQL;
            end;


            11:
            begin
              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstHeadDifference)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FHeadDifferences[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstAquiferFlow)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FAquiferFlows[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstDownStreamNodeInflow)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FDownStreamNodeInflows[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSandAquifer)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstRiverDepth)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FRiverDepths[LCol].FData)]);
              LDataSet.ExecSQL;
            end;

            12:
            begin
              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSubmergedOutlet)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstElevationDifference)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to 10 do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FElevationDifferences[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSubmergedOutlet)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstMonthlyAverageInflow)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to 10 do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FMonthlyAverageInflows[LCol].FData)]);
              LDataSet.ExecSQL;

              for LRow := 1 to 10 do
              begin
                PrepareInsertValues(LDataSet,LStructure);
                LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgSubmergedOutlet)]);
                LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstMonthlyAverageDivertedFlow)]);
                LDataSet.SetParams(['LineNumber'], [IntToStr(LRow)]);
              for LCol := 1 to 10 do
                  LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FMonthlyAverageDivertedFlow[LRow,LCol].FData)]);
                LDataSet.ExecSQL;
              end;
            end;

            13:
            begin
              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgPumpStation)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstPumpingHead)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FPumpingHeads[LCol].FData)]);
              LDataSet.ExecSQL;

              PrepareInsertValues(LDataSet,LStructure);
              LDataSet.SetParams(['GroupNumber'], [IntToStr(pfcgPumpStation)]);
              LDataSet.SetParams(['SubGroupNumber'], [IntToStr(pfcstPumpingDischarge)]);
              LDataSet.SetParams(['LineNumber'], [IntToStr(0)]);
              for LCol := 1 to LStructure.FPointsElevationNum.FData do
                LDataSet.SetParams([Format('Value%2.2d',[LCol])], [FloatToStr(LStructure.FPumpingDischarges[LCol].FData)]);
              LDataSet.ExecSQL;
            end;
          end;
        end;
      end;

       //line 5++++++++++++++++++++++++++++
      for LCounter := 0 to LFlowConstraints.FF04ExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineNumber'], [IntToStr(4 + LCounter)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LFlowConstraints.FF04ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile04DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile04DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile04DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'FlowConstraints,FlowConstraintsValue';
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
