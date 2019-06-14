//
//
//  UNIT      : Contains TFile16DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 15/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile16DatabaseAgent;

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
  UDemandReconciliationObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile16DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF16UnknownDataSQL: string;
    function ReadWaterDemandCategoriesSQL: string;
    function ReadWaterDemandFeaturesSQL: string;
    function ReadWaterDemandRiskCriteriaSQL: string;
    function ReadWaterDemandCountsSQL: string;

    function WriteWaterDemandCategoriesSQL: string;
    function WriteWaterDemandFeaturesSQL: string;
    function WriteWaterDemandRiskCriteriaSQL: string;
    function WriteWaterDemandCountsSQL: string;
    function WriteWaterUseFileSQL: string;
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

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TFile16DatabaseAgent.ReadF16UnknownDataSQL: string;
const OPNAME = 'TFile16DatabaseAgent.ReadF16UnknownDataSQL';
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
function TFile16DatabaseAgent.ReadWaterDemandCategoriesSQL: string;
const OPNAME = 'TFile16DatabaseAgent.ReadWaterDemandCategoriesSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT ' +
      'Model, StudyAreaName, SubArea, Scenario, CategoryID,CategoryDescription, ' +
      'CriteriaPortion01, CriteriaPortion02, CriteriaPortion03, ' +
      'CriteriaPortion04, CriteriaPortion05, CriteriaPortion06, CriteriaPortion07, ' +
      'CriteriaPortion08, CriteriaPortion09, CriteriaPortion10 ' +
      'FROM WaterDemandCategories ' +
      'WHERE Model        = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
      ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
      ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
      ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' ORDER BY CategoryID';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.ReadWaterDemandFeaturesSQL: string;
const OPNAME = 'TFile16DatabaseAgent.ReadWaterDemandFeaturesSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Model,StudyAreaName,SubArea,Scenario,FeatureID, '+
      ' FeatureName,ChannelNumber,CategoryID, '+
      ' ScenarioPortion01,ScenarioPortion02,ScenarioPortion03,ScenarioPortion04, '+
      ' ScenarioPortion05,ScenarioPortion06,ScenarioPortion07,ScenarioPortion08, '+
      ' ScenarioPortion09,ScenarioPortion10'+
      ' FROM WaterDemandFeatures'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
      ' ORDER BY FeatureID';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.ReadWaterDemandRiskCriteriaSQL: string;
const OPNAME = 'TFile16DatabaseAgent.ReadWaterDemandRiskCriteriaSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Model,StudyAreaName,SubArea,Scenario, '+
      ' Interval01,Interval02,Interval03,Interval04,Interval05,'+
      ' Interval06,Interval07,Interval08,Interval09,Interval10'+
      ' FROM WaterDemandRiskCriteria'+
      ' WHERE  Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +' AND'+
      '        StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +' AND'+
      '        SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +' AND'+
      '        Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.ReadWaterDemandCountsSQL: string;
const OPNAME = 'TFile16DatabaseAgent.ReadWaterDemandCountsSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Model,StudyAreaName,SubArea,Scenario,ScenarioCount '+
      ' FROM WaterDemandCounts'+
      ' WHERE  Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +' AND'+
      '        StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +' AND'+
      '        SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +' AND'+
      '        Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteWaterDemandCategoriesSQL: string;
const OPNAME = 'TFile16DatabaseAgent.WriteWaterDemandCategoriesSQL';
begin
  Result := '';
  try
    Result :=
      ' INSERT INTO WaterDemandCategories' +
      ' (Model, StudyAreaName, SubArea, Scenario, CategoryID,CategoryDescription ' +
      ' ,CriteriaPortion01, CriteriaPortion02, CriteriaPortion03, CriteriaPortion04, CriteriaPortion05 ' +
      ' ,CriteriaPortion06, CriteriaPortion07, CriteriaPortion08, CriteriaPortion09, CriteriaPortion10) ' +
      ' Values ' +
      ' (:Model, :StudyAreaName, :SubArea, :Scenario, :CategoryID,:CategoryDescription ' +
      ' ,:CriteriaPortion01, :CriteriaPortion02, :CriteriaPortion03, :CriteriaPortion04, :CriteriaPortion05 ' +
      ' ,:CriteriaPortion06, :CriteriaPortion07, :CriteriaPortion08, :CriteriaPortion09, :CriteriaPortion10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteWaterDemandFeaturesSQL: string;
const OPNAME = 'TFile16DatabaseAgent.WriteWaterDemandFeaturesSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WaterDemandFeatures'+
              ' (Model,StudyAreaName,SubArea,Scenario,FeatureID '+
              ' ,FeatureName,ChannelNumber,CategoryID '+
              ' ,ScenarioPortion01,ScenarioPortion02,ScenarioPortion03,ScenarioPortion04'+
              ' ,ScenarioPortion05,ScenarioPortion06,ScenarioPortion07,ScenarioPortion08'+
              ' ,ScenarioPortion09,ScenarioPortion10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:FeatureID'+
              ' ,:FeatureName,:ChannelNumber,:CategoryID'+
              ' ,:ScenarioPortion01,:ScenarioPortion02,:ScenarioPortion03,:ScenarioPortion04'+
              ' ,:ScenarioPortion05,:ScenarioPortion06,:ScenarioPortion07,:ScenarioPortion08'+
              ' ,:ScenarioPortion09,:ScenarioPortion10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteWaterDemandRiskCriteriaSQL: string;
const OPNAME = 'TFile16DatabaseAgent.WriteWaterDemandRiskCriteriaSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WaterDemandRiskCriteria'+
              ' (Model,StudyAreaName,SubArea,Scenario'+
              ' ,Interval01,Interval02,Interval03,Interval04,Interval05'+
              ' ,Interval06,Interval07,Interval08,Interval09,Interval10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario'+
              ' ,:Interval01,:Interval02,:Interval03,:Interval04,:Interval05'+
              ' ,:Interval06,:Interval07,:Interval08,:Interval09,:Interval10)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteWaterUseFileSQL: string;
const OPNAME = 'TFile16DatabaseAgent.WriteWaterUseFileSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO FileCreate'+
              ' (Model,StudyAreaName,SubArea,Scenario,ImplementFile)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:ImplementFile)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteWaterDemandCountsSQL: string;
const OPNAME = 'TFile16DatabaseAgent.WriteWaterDemandCountsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WaterDemandCounts'+
              ' (Model,StudyAreaName,SubArea,Scenario,ScenarioCount)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:ScenarioCount)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile16DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile16DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LArrayCount,
  LCount: Integer;
  LDemandReconciliationObject: TDemandReconciliationDataObject;
  LDemandReconciliationData:TDemandReconciliationData;
  LDemandPortion :TDemandPortion;
  LDemandChannel :TDemandChannel;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile16DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LDemandReconciliationObject := ADataObject.FDemandReconciliationDataObject;

    if not LDemandReconciliationObject.Initialise then
    Exit;
    LDemandReconciliationData := LDemandReconciliationObject.FDemandReconciliationData;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      //Line1+++++++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadWaterDemandCountsSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (not LDataSet.DataSet.Eof) then
      begin
        LDemandReconciliationData.FScenarioCount.FData := LDataSet.DataSet.FieldByName('ScenarioCount').AsInteger;
        LDemandReconciliationData.FScenarioCount.FInitalised := True;
      end;

      //Line2 +++++++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadWaterDemandRiskCriteriaSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      LCount := 0;
      if (not LDataSet.DataSet.Eof) then
      begin
        for LArrayCount := 1 to 10 do
        begin
          LFieldName := Format('%s%2.2d',['Interval',LArrayCount]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LCount := LCount + 1;
            LDemandReconciliationData.FRecurrenceInterval[LArrayCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LDemandReconciliationData.FRecurrenceInterval[LArrayCount].FInitalised := True;
          end;
        end;
      end;
      LDemandReconciliationData.FRiskCriteriaCount.FData := LCount;
      LDemandReconciliationData.FUserTypeCount.FInitalised := True;

      //Line3 +++++++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadWaterDemandCategoriesSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      while not LDataSet.DataSet.Eof do
      begin
        LDemandPortion := LDemandReconciliationObject.AddDemandPortion;
        if not Assigned(LDemandPortion) then
          Break;

        for LArrayCount := 1 to 10 do
        begin
          LFieldName := Format('%s%2.2d',['CriteriaPortion',LArrayCount]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LDemandPortion.FDemandPortionValues[LArrayCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LDemandPortion.FDemandPortionValues[LArrayCount].FInitalised := True;
          end;
        end;
        LDataSet.DataSet.Next;
      end;
      LDemandReconciliationData.FUserTypeCount.FData := LDemandReconciliationObject.FDemandPortionList.Count;
      LDemandReconciliationData.FUserTypeCount.FInitalised := True;


      //Line5 +++++++++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadWaterDemandFeaturesSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      while not LDataSet.DataSet.Eof do
      begin
        LDemandChannel := LDemandReconciliationObject.AddDemandChannel;
        if not Assigned(LDemandChannel) then
          Break;

        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LDemandChannel.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LDemandChannel.FChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('CategoryID').IsNull then
        begin
          LDemandChannel.FUserType.FData := LDataSet.DataSet.FieldByName('CategoryID').AsInteger;
          LDemandChannel.FUserType.FInitalised := True;
        end;

        for LArrayCount := 1 to 10 do
        begin
          LFieldName := Format('%s%2.2d',['ScenarioPortion',LArrayCount]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LDemandChannel.FDemandPortionValues[LArrayCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LDemandChannel.FDemandPortionValues[LArrayCount].FInitalised := True;
          end;
        end;
        LDataSet.DataSet.Next;
      end;
      LDemandReconciliationData.FChannelsCount.FData := LDemandReconciliationObject.FDemandChannelsList.Count;
      LDemandReconciliationData.FChannelsCount.FInitalised := True;


      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF16UnknownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.EOF do
      begin
        LDemandReconciliationObject.FF16ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile16DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile16DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage,LFieldName:string;
  LIdentifier,
  LCounter,
  LCount,
  LArrayCount : integer;
  LDataSet : TAbstractModelDataset;
  LDemandReconciliationObject: TDemandReconciliationDataObject;
  LDemandReconciliationData:TDemandReconciliationData;
  LDemandPortion :TDemandPortion;
  LDemandChannel :TDemandChannel;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile16DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LDemandReconciliationObject := ADataObject.FDemandReconciliationDataObject;
    if not Assigned(LDemandReconciliationObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDemandReconciliationData := LDemandReconciliationObject.FDemandReconciliationData;

      //Line 1++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if LDemandReconciliationData.FScenarioCount.FInitalised then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteWaterDemandCountsSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['ScenarioCount'], [IntToStr(LDemandReconciliationData.FScenarioCount.FData)]);
        LDataSet.ExecSQL;
      end;

      //Line 2++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if LDemandReconciliationData.FRecurrenceInterval[1].FInitalised then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteWaterDemandRiskCriteriaSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        for LArrayCount := 1 to 10 do
        begin
          if LDemandReconciliationData.FRecurrenceInterval[LArrayCount].FInitalised then
          begin
            LFieldName := Format('%s%2.2d',['Interval',LArrayCount]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LDemandReconciliationData.FRecurrenceInterval[LArrayCount].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;

      //Line 3++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      LIdentifier := 0;
      for LCount := 0 to  LDemandReconciliationObject.FDemandPortionList.Count -1 do
      begin
        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteWaterDemandCategoriesSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['CategoryID'], [IntToStr(LIdentifier)]);

        LDemandPortion := LDemandReconciliationObject.DemandPortionByIndex(LCount);
        if Assigned(LDemandPortion) then
        begin
          for LArrayCount := 1 to 10 do
          begin
            if LDemandPortion.FDemandPortionValues[LArrayCount].FInitalised then
            begin
              LFieldName := Format('%s%2.2d',['CriteriaPortion',LArrayCount]);
              LDataSet.SetParams([LFieldName], [FloatToStr(LDemandPortion.FDemandPortionValues[LArrayCount].FData)]);
            end;
          end;
        end;
        LDataSet.ExecSQL;
      end;

      //Line 5++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      LIdentifier := 0;
      for LCount := 0 to  LDemandReconciliationObject.FDemandChannelsList.Count -1 do
      begin
        LDemandChannel := LDemandReconciliationObject.DemandChannelByIndex(LCount);
        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteWaterDemandFeaturesSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FeatureID'], [IntToStr(LIdentifier)]);
        LDataSet.SetParams(['CategoryID'], [IntToStr(LDemandChannel.FUserType.FData)]);
        LDataSet.SetParams(['ChannelNumber'], [IntToStr(LDemandChannel.FChannelNumber.FData)]);

        if Assigned(LDemandChannel) then
        begin
          for LArrayCount := 1 to 10 do
          begin
            if LDemandChannel.FDemandPortionValues[LArrayCount].FInitalised then
            begin
              LFieldName := Format('%s%2.2d',['ScenarioPortion',LArrayCount]);
              LDataSet.SetParams([LFieldName], [FloatToStr(LDemandChannel.FDemandPortionValues[LArrayCount].FData)]);
            end;
          end;
        end;
        LDataSet.ExecSQL;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteWaterUseFileSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['ImplementFile'], [IntToStr(1)]);
      LDataSet.ExecSQL;

      for LCounter := 0 to LDemandReconciliationObject.FF16ExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineData'], [LDemandReconciliationObject.FF16ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile16DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile16DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile16DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'WaterDemandCategories,WaterDemandCounts,WaterDemandFeatures,WaterDemandRiskCriteria,FileCreate';
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
