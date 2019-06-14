//
//
//  UNIT      : Contains TFile11DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile11DatabaseAgent;

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
  UMinFlowChannelObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile11DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF11UnkownDataSQL: string;
    function ReadMinFlowChannelSQL: string;
    function ReadLossFeatureSQL: string;
    function ReadDiversionFeatureType4SQL: string;

    function WriteMinFlowChannelSQL: string;
    function WriteMinFlowChannelDataSQL: string;
    function WriteLossFeatureSQL: string;
    function WriteLossFeatureDataSQL: string;
    function WriteDiversionFeatureType4SQL: string;
    function WriteDiversionFeatureType4DataSQL: string;
    function WriteF11UnkownDataSQL: string;

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
  UDivChannelDemandObject,
  UErrorHandlingOperations,
  UChannelDescriptionObject;

function TFile11DatabaseAgent.ReadF11UnkownDataSQL: string;
const OPNAME = 'TFile11DatabaseAgent.ReadF11UnkownDataSQL';
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

function TFile11DatabaseAgent.ReadMinFlowChannelSQL: string;
const OPNAME = 'TFile11DatabaseAgent.ReadMinFlowChannelSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Mc.Model,Mc.StudyAreaName,Mc.SubArea,Mc.Scenario'+
      ' ,Mc.Identifier,Mc.MinFlowChannelName,Mc.MinFlowChannelNumber'+
      ' ,Value01,Value02,Value03,Value04,Value05,Value06'+
      ' ,Value07,Value08,Value09,Value10,Value11,Value12'+
      ' FROM MinFlowChannel Mc, MinFlowChannelValue Mcv'+
      ' WHERE (Mc.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '       (Mc.StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '       (Mc.SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '       (Mc.Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '       (Mcv.Model            = Mc.Model) AND'+
      '       (Mcv.StudyAreaName    = Mc.StudyAreaName) AND'+
      '       (Mcv.SubArea          = Mc.SubArea) AND'+
      '       (Mcv.Scenario         = Mc.Scenario) AND'+
      '       (Mcv.Identifier       = Mc.Identifier) '+
      ' ORDER BY Mc.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.ReadLossFeatureSQL: string;
const OPNAME = 'TFile11DatabaseAgent.ReadLossFeatureSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Lf.Model, Lf.StudyAreaName, Lf.SubArea, Lf.Scenario, ' +
      'Lf.Identifier, Lf.FeatureName, Lf.ChannelNumber, Lfv.SubIdentifier, ' +
      'Value01, Value02, Value03, Value04, Value05, Value06, ' +
      'Value07, Value08, Value09, Value10, Value11, Value12 ' +
      'FROM LossFeature Lf, LossFeatureValue Lfv ' +
      'WHERE (Lf.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '      (Lf.StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '      (Lf.SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '      (Lf.Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '      (Lfv.Model            = Lf.Model) AND'+
      '      (Lfv.StudyAreaName    = Lf.StudyAreaName) AND'+
      '      (Lfv.SubArea          = Lf.SubArea) AND'+
      '      (Lfv.Scenario         = Lf.Scenario) AND'+
      '      (Lfv.Identifier       = Lf.Identifier) '+
      ' ORDER BY Lf.Identifier,Lfv.SubIdentifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.ReadDiversionFeatureType4SQL: string;
const OPNAME = 'TFile11DatabaseAgent.ReadDiversionFeatureType4SQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Df.Model, Df.StudyAreaName, Df.SubArea, Df.Scenario, ' +
      'Df.Identifier, DivChannelName, DivChannelNumber, DivChannelType, ' +
      'DiversionCode, ' +
      'DivFactor01, DivFactor02, DivFactor03, DivFactor04, DivFactor05, DivFactor06, ' +
      'DivFactor07, DivFactor08, DivFactor09, DivFactor10, DivFactor11, DivFactor12 ' +
      'FROM DiversionFeatures Df, DiversionFeaturesType1n2 Dv ' +
      'WHERE (Df.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)     +') AND'+
      '      (Df.StudyAreaName    ='+ QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '      (Df.SubArea          ='+ QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '      (Df.Scenario         ='+ QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '      (Df.DivChannelType   = 4) AND'+
      '      (Dv.Model            = Df.Model) AND'+
      '      (Dv.StudyAreaName    = Df.StudyAreaName) AND'+
      '      (Dv.SubArea          = Df.SubArea) AND'+
      '      (Dv.Scenario         = Df.Scenario) AND'+
      '      (Dv.Identifier       = Df.Identifier) '+
      ' ORDER BY Df.Identifier, DiversionCode';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteMinFlowChannelSQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteMinFlowChannelSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MinFlowChannel'+
              ' (Model,StudyAreaName,SubArea,Scenario'+
              ' ,Identifier,MinFlowChannelName,MinFlowChannelNumber)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario'+
              ' ,:Identifier,:MinFlowChannelName,:MinFlowChannelNumber)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteMinFlowChannelDataSQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteMinFlowChannelDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MinFlowChannelValue'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,Value01,Value02,Value03,Value04,Value05,Value06'+
              ' ,Value07,Value08,Value09,Value10,Value11,Value12)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              ' ,:Value01,:Value02,:Value03,:Value04,:Value05,:Value06'+
              ' ,:Value07,:Value08,:Value09,:Value10,:Value11,:Value12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteLossFeatureSQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteLossFeatureSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO LossFeature '+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
              'FeatureName, ChannelNumber, Reference, LossType) ' +
              'Values ' +
              '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
              ':FeatureName, :ChannelNumber, :Reference, :LossType)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteDiversionFeatureType4SQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteDiversionFeatureType4SQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DiversionFeatures'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,DivChannelName,DivChannelNumber'+
              ' ,DivChannelType)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DivChannelName,:DivChannelNumber'+
              ' ,:DivChannelType)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteDiversionFeatureType4DataSQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteDiversionFeatureType4DataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DiversionFeaturesType1n2'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode'+
              ' ,DivFactor01,DivFactor02,DivFactor03,DivFactor04,DivFactor05,DivFactor06,DivFactor07,DivFactor08,DivFactor09,DivFactor10,DivFactor11,DivFactor12)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DiversionCode'+
              ' ,:DivFactor01,:DivFactor02,:DivFactor03,:DivFactor04,:DivFactor05,:DivFactor06,:DivFactor07,:DivFactor08,:DivFactor09,:DivFactor10,:DivFactor11,:DivFactor12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteLossFeatureDataSQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteLossFeatureDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO LossFeatureValue ' +
              '(Model, StudyAreaName, SubArea, Scenario, ' +
              'Identifier, SubIdentifier, Value01, Value02, Value03, Value04, ' +
              'Value05, Value06, Value07, Value08, Value09, Value10, Value11, Value12) ' +
              'Values ' +
              '(:Model, :StudyAreaName, :SubArea, :Scenario, ' +
              ':Identifier, :SubIdentifier, :Value01, :Value02, :Value03, :Value04, ' +
              ':Value05, :Value06, :Value07, :Value08, :Value09, :Value10, :Value11, :Value12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteF11UnkownDataSQL: string;
const OPNAME = 'TFile11DatabaseAgent.WriteF11UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile11DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LOldIdentifier,
  LCount : Integer;
  LMinFlowChannelObject: TMinFlowAndLossChannelObject;
  LMinFlowChannel:TMinFlowChannel;
  LLossChannel:TLossChannel;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LMinFlowChannelObject := ADataObject.FMinFlowChannelObject;

    if not LMinFlowChannelObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadMinFlowChannelSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.EOF do
        begin
          LMinFlowChannel := TMinFlowChannel.Create;
          LMinFlowChannelObject.FMinFlowChannelsLines.Add(LMinFlowChannel);
          if not LDataSet.DataSet.FieldByName('MinFlowChannelName').IsNull then
          begin
            LMinFlowChannel.FMinFlowChannelName.FData := Trim(LDataSet.DataSet.FieldByName('MinFlowChannelName').AsString);
            LMinFlowChannel.FMinFlowChannelName.FInitalised := True;
          end;
          if not LDataSet.DataSet.FieldByName('MinFlowChannelNumber').IsNull then
          begin
            LMinFlowChannel.FMinFlowChannelNumber.FData :=LDataSet.DataSet.FieldByName('MinFlowChannelNumber').AsInteger;
            LMinFlowChannel.FMinFlowChannelNumber.FInitalised := True;
          end;
          for LCount := MinMinFlow to MaxMinFlow do
          begin
            LFieldName := Format('%s%2.2d',['Value',LCount]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LMinFlowChannel.FMinFlowValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LMinFlowChannel.FMinFlowValues[LCount].FInitalised := True;
            end
            else
              Break;
          end;
          LDataSet.DataSet.Next;
        end;//while
      end;//if

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadLossFeatureSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        LLossChannel := nil;
        LOldIdentifier := -1;
        while not LDataSet.DataSet.EOF do
        begin
          if(LDataSet.DataSet.FieldByName('Identifier').AsInteger <> LOldIdentifier) then
          begin
            LOldIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LLossChannel := TLossChannel.Create;
            LMinFlowChannelObject.FLossChannelsLines.Add(LLossChannel);
          end;

          if not LDataSet.DataSet.FieldByName('SubIdentifier').IsNull then
          begin
            if (LDataSet.DataSet.FieldByName('SubIdentifier').AsInteger = 0) then
            begin
              if not LDataSet.DataSet.FieldByName('FeatureName').IsNull then
              begin
                LLossChannel.FLossChannelName.FData := Trim(LDataSet.DataSet.FieldByName('FeatureName').AsString);
                LLossChannel.FLossChannelName.FInitalised := True;
              end;
              if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
              begin
                LLossChannel.FLossChannelNumber.FData :=LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
                LLossChannel.FLossChannelNumber.FInitalised := True;
              end;
              for LCount := MinLoss to MaxLoss do
              begin
                LFieldName := Format('%s%2.2d',['Value',LCount]);
                if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LLossChannel.FLossValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LLossChannel.FLossValues[LCount].FInitalised := True;
                end
                else
                  Break;
              end;
            end
            else
            begin
              for LCount := MinDiverted to MaxDiverted do
              begin
                LFieldName := Format('%s%2.2d',['Value',LCount]);
                if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LLossChannel.FDivertedValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LLossChannel.FDivertedValues[LCount].FInitalised := True;
                end
                else
                  Break;
              end;
            end;
          end;
          LDataSet.DataSet.Next;
        end;//while
      end;//if

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadDiversionFeatureType4SQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        LLossChannel := nil;
        LOldIdentifier := -1;
        while not LDataSet.DataSet.EOF do
        begin
          if(LDataSet.DataSet.FieldByName('Identifier').AsInteger <> LOldIdentifier) then
          begin
            LOldIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LLossChannel := TLossChannel.Create;
            LMinFlowChannelObject.FLossChannelsLines.Add(LLossChannel);
          end;

          if not LDataSet.DataSet.FieldByName('DiversionCode').IsNull then
          begin
            if (LDataSet.DataSet.FieldByName('DiversionCode').AsInteger = 1) then
            begin
              if not LDataSet.DataSet.FieldByName('DivChannelName').IsNull then
              begin
                LLossChannel.FLossChannelName.FData := Trim(LDataSet.DataSet.FieldByName('DivChannelName').AsString);
                LLossChannel.FLossChannelName.FInitalised := True;
              end;
              if not LDataSet.DataSet.FieldByName('DivChannelNumber').IsNull then
              begin
                LLossChannel.FLossChannelNumber.FData :=LDataSet.DataSet.FieldByName('DivChannelNumber').AsInteger;
                LLossChannel.FLossChannelNumber.FInitalised := True;
              end;
              for LCount := MinDiversionDemand to MaxDiversionDemand do
              begin
                LFieldName := Format('%s%2.2d',['DivFactor',LCount]);
                if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LLossChannel.FLossValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LLossChannel.FLossValues[LCount].FInitalised := True;
                end
                else
                  Break;
              end;
            end
            else
            begin
              for LCount := MinDiversionDemand to MaxDiversionDemand do
              begin
                LFieldName := Format('%s%2.2d',['DivFactor',LCount]);
                if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LLossChannel.FDivertedValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LLossChannel.FDivertedValues[LCount].FInitalised := True;
                end
                else
                  Break;
              end;
            end;
          end;
          LDataSet.DataSet.Next;
        end;//while
      end;//if

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF11UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.EOF do
      begin
        LMinFlowChannelObject.FF11ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile11DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName            : string;
  LMessage              : string;
  LCount                : integer;
  LLinesCountLoss       : integer;
  LLinesCountMinFlow    : integer;
  LCounter              : integer;
  LDataSet              : TAbstractModelDataset;
  LMinFlowChannelObject : TMinFlowAndLossChannelObject;
  LMinFlowChannel       : TMinFlowChannel;
  LLossChannel          : TLossChannel;
  LLossChannelObj       : TLossChannelObject;
  LStop                 : boolean;
  LLinesCountDiversion  : integer;
  LDivChannelObject     : TDivChannelDemandObject;
  lDiversionCode        : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LMinFlowChannelObject := ADataObject.FMinFlowChannelObject;
    if not Assigned(LMinFlowChannelObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LLinesCountMinFlow := 0;
      for LCounter := 0 to LMinFlowChannelObject.FMinFlowChannelsLines.Count-1 do
      begin
        LLinesCountMinFlow := LLinesCountMinFlow + 1;
        LMinFlowChannel := TMinFlowChannel(LMinFlowChannelObject.FMinFlowChannelsLines[LCounter]);

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteMinFlowChannelSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountMinFlow)]);

        if LMinFlowChannel.FMinFlowChannelName.FInitalised then
         LDataSet.SetParams(['MinFlowChannelName'], [LMinFlowChannel.FMinFlowChannelName.FData]);

        if LMinFlowChannel.FMinFlowChannelNumber.FInitalised then
         LDataSet.SetParams(['MinFlowChannelNumber'], [IntToStr(LMinFlowChannel.FMinFlowChannelNumber.FData)]);

        LDataSet.ExecSQL;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteMinFlowChannelDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountMinFlow)]);

        for LCount := MinMinFlow to MaxMinFlow do
        begin
          if not LMinFlowChannel.FMinFlowValues[LCount].FInitalised then
            Break;
          LFieldName := Format('%s%2.2d',['Value',LCount]);
          LDataSet.SetParams([LFieldName], [FloatToStr(LMinFlowChannel.FMinFlowValues[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;

      LDivChannelObject := ADataObject.FDivChannelDemandObject;
      LLinesCountDiversion := LDivChannelObject.DiversionChannelCount;
      LLinesCountLoss := 0;
      for LCounter := 0 to LMinFlowChannelObject.FLossChannelsLines.Count-1 do
      begin
        LLossChannel := TLossChannel(LMinFlowChannelObject.FLossChannelsLines[LCounter]);
        LLossChannelObj := ADataObject.FChannelDescrObject.FindLossChannels(LLossChannel.FLossChannelNumber.FData);
        if (LLossChannelObj.FChannelType.FInitalised) AND (LLossChannelObj.FChannelType.FData = 1) then
        begin
          // Loss type 1 = diversion feature
          LLinesCountDiversion := LLinesCountDiversion + 1;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteDiversionFeatureType4SQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountDiversion)]);

          if LLossChannel.FLossChannelName.FInitalised then
           LDataSet.SetParams(['DivChannelName'], [LLossChannel.FLossChannelName.FData]);

          if LLossChannel.FLossChannelNumber.FInitalised then
           LDataSet.SetParams(['DivChannelNumber'], [IntToStr(LLossChannel.FLossChannelNumber.FData)]);

          if LLossChannelObj.FChannelType.FInitalised then
           LDataSet.SetParams(['DivChannelType'], [IntToStr(4)]);

          LDataSet.ExecSQL;

          //SubQuery
          for LDiversionCode := 1 to 2 do
          begin
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(WriteDiversionFeatureType4DataSQL);
            LDataSet.ClearQueryParams(prFloat);
            LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountDiversion)]);
            LDataSet.SetParams(['DiversionCode'], [IntToStr(LDiversionCode)]);

            for LCount := MinDiversionDemand to MaxDiversionDemand do
            begin
              if (NOT LLossChannel.FLossValues[LCount].FInitalised) then
                Break;
              LFieldName := Format('%s%2.2d',['DivFactor',LCount]);
              if (lDiversionCode = 1) then
                LDataSet.SetParams([LFieldName], [FloatToStr(LLossChannel.FLossValues[LCount].FData)])
              else
                LDataSet.SetParams([LFieldName], [FloatToStr(LLossChannel.FDivertedValues[LCount].FData)]);
            end;
            LDataSet.ExecSQL;
          end;
        end
        else
        begin
          // Loss type 0
          LLinesCountLoss := LLinesCountLoss + 1;
          LLossChannel := TLossChannel(LMinFlowChannelObject.FLossChannelsLines[LCounter]);
          LLossChannelObj := ADataObject.FChannelDescrObject.FindLossChannels(LLossChannel.FLossChannelNumber.FData);
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteLossFeatureSQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountLoss)]);

          if LLossChannel.FLossChannelName.FInitalised then
           LDataSet.SetParams(['FeatureName'], [LLossChannel.FLossChannelName.FData]);

          if LLossChannel.FLossChannelNumber.FInitalised then
           LDataSet.SetParams(['ChannelNumber'], [IntToStr(LLossChannel.FLossChannelNumber.FData)]);

          if LLossChannelObj.FReference.FInitalised then
           LDataSet.SetParams(['Reference'], [IntToStr(LLossChannelObj.FReference.FData)]);

          if LLossChannelObj.FChannelType.FInitalised then
           LDataSet.SetParams(['LossType'], [IntToStr(LLossChannelObj.FChannelType.FData)]);

          LDataSet.ExecSQL;

          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteLossFeatureDataSQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountLoss)]);
          LDataSet.SetParams(['SubIdentifier'], [IntToStr(0)]);

          for LCount := MinLoss to MaxLoss do
          begin
            if not LLossChannel.FLossValues[LCount].FInitalised then
              Break;
            LFieldName := Format('%s%2.2d',['Value',LCount]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LLossChannel.FLossValues[LCount].FData)]);
          end;
          LDataSet.ExecSQL;

          if LLossChannel.FDivertedValues[MinDiverted].FInitalised then
          begin
            LDataSet.DataSet.Close;
            LDataSet.SetSQL(WriteLossFeatureDataSQL);
            LDataSet.ClearQueryParams();
            LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCountLoss)]);
            LDataSet.SetParams(['SubIdentifier'], [IntToStr(1)]);

            for LCount := MinDiverted to MaxDiverted do
            begin
              if not LLossChannel.FDivertedValues[LCount].FInitalised then
                Break;
              LFieldName := Format('%s%2.2d',['Value',LCount]);
              LDataSet.SetParams([LFieldName], [FloatToStr(LLossChannel.FDivertedValues[LCount].FData)]);
            end;
            LDataSet.ExecSQL;
          end;
        end;
      end;

      for LCounter := 0 to LMinFlowChannelObject.FF11ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF11UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LMinFlowChannelObject.FF11ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile11DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile11DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile11DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'MinFlowChannel,MinFlowChannelValue,LossFeature,LossFeatureValue';
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
