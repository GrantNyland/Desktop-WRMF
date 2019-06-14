//
//
//  UNIT      : Contains TFile09DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile09DatabaseAgent;

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
  UIrrigationAreasObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile09DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF09UnkownDataSQL: string;
    function ReadIrrigationDataSQL: string;

    function WriteIrrigationNodesDataSQL: string;
    function WriteDiversionFlowDataSQL: string;
    function WriteReturnFlowDataSQL: string;
    function WriteF09UnkownDataSQL: string;

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
  UChannelDescriptionObject,
  UErrorHandlingOperations;

function TFile09DatabaseAgent.ReadF09UnkownDataSQL: string;
const OPNAME = 'TFile09DatabaseAgent.ReadF09UnkownDataSQL';
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

function TFile09DatabaseAgent.ReadIrrigationDataSQL: string;
const OPNAME = 'TFile09DatabaseAgent.ReadIrrigationDataSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Irn.Model,Irn.StudyAreaName,Irn.SubArea,Irn.Scenario'+
      '  ,Irn.Identifier,Irn.AreaName,Irn.IrrigationNodeNumber'+
      '  ,DFlow01,DFlow02,DFlow03,DFlow04,DFlow05,DFlow06'+
      '  ,DFlow07,DFlow08,DFlow09,DFlow10,DFlow11,DFlow12'+
      '  ,RFlow01,RFlow02,RFlow03,RFlow04,RFlow05,RFlow06'+
      '  ,RFlow07,RFlow08,RFlow09,RFlow10,RFlow11,RFlow12'+
      ' FROM IrrigationAreas Irn, IrrigationAreasDiversionFlow Idf, IrrigationAreasReturnFlow Irf'+
      ' WHERE (Irn.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '       (Irn.StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '       (Irn.SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '       (Irn.Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '       (Idf.Model            = Irn.Model) AND'+
      '       (Idf.StudyAreaName    = Irn.StudyAreaName) AND'+
      '       (Idf.SubArea          = Irn.SubArea) AND'+
      '       (Idf.Scenario         = Irn.Scenario) AND'+
      '       (Idf.Identifier       = Irn.Identifier) AND'+
      '       (Irf.Model            = Irn.Model) AND'+
      '       (Irf.StudyAreaName    = Irn.StudyAreaName) AND'+
      '       (Irf.SubArea          = Irn.SubArea) AND'+
      '       (Irf.Scenario         = Irn.Scenario) AND'+
      '       (Irf.Identifier       = Irn.Identifier) '+
      ' ORDER BY Irn.Model,Irn.StudyAreaName,Irn.SubArea,Irn.Scenario,Irn.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09DatabaseAgent.WriteIrrigationNodesDataSQL: string;
const OPNAME = 'TFile09DatabaseAgent.WriteIrrigationNodesDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationAreas '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              'AreaName, IrrigationNodeNumber, DiversionChannelNumber, ConsumptiveChannelNumber, ' +
              'ReturnFlowChannelNumber, RelaxationDemand) ' +
              'Values ' +
              '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
              ':AreaName, :IrrigationNodeNumber, :DiversionChannelNumber, :ConsumptiveChannelNumber, ' +
              ':ReturnFlowChannelNumber, :RelaxationDemand)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09DatabaseAgent.WriteDiversionFlowDataSQL: string;
const OPNAME = 'TFile09DatabaseAgent.WriteDiversionFlowDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationAreasDiversionFlow '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, '+
              'DFlow01, DFlow02, DFlow03, DFlow04, DFlow05, DFlow06, '+
              'DFlow07, DFlow08, DFlow09, DFlow10, DFlow11, DFlow12) '+
              'Values '+
              '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, '+
              ':DFlow01, :DFlow02, :DFlow03, :DFlow04, :DFlow05, :DFlow06, '+
              ':DFlow07, :DFlow08, :DFlow09, :DFlow10, :DFlow11, :DFlow12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09DatabaseAgent.WriteReturnFlowDataSQL: string;
const OPNAME = 'TFile09DatabaseAgent.WriteReturnFlowDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IrrigationAreasReturnFlow '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, '+
              'RFlow01, RFlow02, RFlow03, RFlow04, RFlow05, RFlow06, '+
              'RFlow07, RFlow08, RFlow09, RFlow10, RFlow11, RFlow12) '+
              'Values '+
              '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, '+
              ':RFlow01, :RFlow02, :RFlow03, :RFlow04, :RFlow05, :RFlow06, '+
              ':RFlow07, :RFlow08, :RFlow09, :RFlow10, :RFlow11, :RFlow12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile09DatabaseAgent.WriteF09UnkownDataSQL: string;
const OPNAME = 'TFile09DatabaseAgent.WriteF09UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile09DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LCount : Integer;
  LIrrigationAreaObject: TIrrigationAreaObject;
  LIrrigationArea:TIrrigationArea;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile09DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LIrrigationAreaObject := ADataObject.FIrrigationAreaObject;

    if not LIrrigationAreaObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadIrrigationDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile09DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LIrrigationArea := TIrrigationArea.Create;
          LIrrigationAreaObject.FIrrigationAreasLines.Add(LIrrigationArea);

          if not LDataSet.DataSet.FieldByName('AreaName').IsNull then
          begin
            LIrrigationArea.FIrrigationAreaName.FData := Trim(LDataSet.DataSet.FieldByName('AreaName').AsString);
            LIrrigationArea.FIrrigationAreaName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('IrrigationNodeNumber').IsNull then
          begin
            LIrrigationArea.FIrrigationNodeNumber.FData :=LDataSet.DataSet.FieldByName('IrrigationNodeNumber').AsInteger;
            LIrrigationArea.FIrrigationNodeNumber.FInitalised := True;
          end;

          for LCount := MinDiversionFlow to MaxDiversionFlow do
          begin
            LFieldName := Format('%s%2.2d',['DFlow',LCount]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LIrrigationArea.FDiversionFlowValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LIrrigationArea.FDiversionFlowValues[LCount].FInitalised := True;
            end
            else
              Break;
          end;

          for LCount := MinReturnFlow to MaxReturnFlow do
          begin
            LFieldName := Format('%s%2.2d',['RFlow',LCount]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LIrrigationArea.FReturnFlowValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LIrrigationArea.FReturnFlowValues[LCount].FInitalised := True;
            end
            else
              Break;
          end;
          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF09UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LIrrigationAreaObject.FF09ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile09DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile09DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount,
  LCounter : integer;
  LDataSet : TAbstractModelDataset;
  LIrrigationAreaObject: TIrrigationAreaObject;
  LIrrigationArea:TIrrigationArea;
  LStop: boolean;
  lNodeNumber : integer;
  lIrrChannel : TIrrigationChannelObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile09DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LIrrigationAreaObject := ADataObject.FIrrigationAreaObject;
    if not Assigned(LIrrigationAreaObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      for LLinesCount := 0 to LIrrigationAreaObject.FIrrigationAreasLines.Count-1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIrrigationNodesDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        LIrrigationArea := TIrrigationArea(LIrrigationAreaObject.FIrrigationAreasLines[LLinesCount]);

        if LIrrigationArea.FIrrigationAreaName.FInitalised then
         LDataSet.SetParams(['AreaName'], [LIrrigationArea.FIrrigationAreaName.FData]);

        if LIrrigationArea.FIrrigationNodeNumber.FInitalised then
        begin
         lNodeNumber := LIrrigationArea.FIrrigationNodeNumber.FData;
         LDataSet.SetParams(['IrrigationNodeNumber'], [IntToStr(lNodeNumber)]);
         lIrrChannel := ADataObject.FChannelDescrObject.FindIrrigationChannel(lNodeNumber);
         LDataSet.SetParams(['DiversionChannelNumber'], [IntToStr(lIrrChannel.FDiversionChannelNumber.FData)]);
         LDataSet.SetParams(['ConsumptiveChannelNumber'], [IntToStr(lIrrChannel.FConsumptiveChannelNumber.FData)]);
         LDataSet.SetParams(['ReturnFlowChannelNumber'], [IntToStr(lIrrChannel.FReturnChannelNumber.FData)]);
         LDataSet.SetParams(['RelaxationDemand'], [IntToStr(lIrrChannel.FRelaxationDemand.FData)]);
        end;

        LDataSet.ExecSQL;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDiversionFlowDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);


        for LCount := MinDiversionFlow to MaxDiversionFlow do
        begin
          if not LIrrigationArea.FDiversionFlowValues[LCount].FInitalised then
            Break;

          LFieldName := Format('%s%2.2d',['DFlow',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LIrrigationArea.FDiversionFlowValues[LCount].FData)]);
        end;
        LDataSet.ExecSQL;


        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteReturnFlowDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);


        for LCount := MinReturnFlow to MaxReturnFlow do
        begin
          if not LIrrigationArea.FReturnFlowValues[LCount].FInitalised then
            Break;

          LFieldName := Format('%s%2.2d',['RFlow',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LIrrigationArea.FReturnFlowValues[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;

      for LCounter := 0 to LIrrigationAreaObject.FF09ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF09UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LIrrigationAreaObject.FF09ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile09DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile09DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile09DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'IrrigationAreas,IrrigationAreasDiversionFlow,IrrigationAreasReturnFlow';
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
