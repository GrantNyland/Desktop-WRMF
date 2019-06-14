//
//
//  UNIT      : Contains TFile12DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFile12DatabaseAgent;

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
  UChannelMinMaxObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile12DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF12UnkownDataSQL: string;
    function ReadChannelMinMaxDataSQL: string;

    function WriteChannelMinMaxDataSQL: string;
    function WriteChannelMinMaxValuesSQL: string;
    function WriteF12UnkownDataSQL: string;

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

function TFile12DatabaseAgent.ReadF12UnkownDataSQL: string;
const OPNAME = 'TFile12DatabaseAgent.ReadF12UnkownDataSQL';
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
function TFile12DatabaseAgent.ReadChannelMinMaxDataSQL: string;
const OPNAME = 'TFile12DatabaseAgent.ReadChannelMinMaxDataSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT Mm.Model,Mm.StudyAreaName,Mm.SubArea,Mm.Scenario'+
      '  ,Mm.Identifier,Mm.MinMaxChannelName,Mm.MinMaxChannelNumber,Mmf.SubIdentifier'+
      '  ,Mmf.MFlow01,Mmf.MFlow02,Mmf.MFlow03,Mmf.MFlow04,Mmf.MFlow05,Mmf.MFlow06'+
      '  ,Mmf.MFlow07,Mmf.MFlow08,Mmf.MFlow09,Mmf.MFlow10,Mmf.MFlow11,Mmf.MFlow12'+
      '  ,Mm.Comment as Comment1,Mmf.Comment as Comment2'+
      ' FROM MinMaxChannel Mm, MinMaxChannelFlow Mmf'+
      ' WHERE (Mm.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '       (Mm.StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '       (Mm.SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '       (Mm.Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '       (Mmf.Model            = Mm.Model) AND'+
      '       (Mmf.StudyAreaName    = Mm.StudyAreaName) AND'+
      '       (Mmf.SubArea          = Mm.SubArea) AND'+
      '       (Mmf.Scenario         = Mm.Scenario) AND'+
      '       (Mmf.Identifier       = Mm.Identifier) '+
      ' ORDER BY Mm.Model,Mm.StudyAreaName,Mm.SubArea,Mm.Scenario,Mm.Identifier,Mmf.SubIdentifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile12DatabaseAgent.WriteChannelMinMaxDataSQL: string;
const OPNAME = 'TFile12DatabaseAgent.WriteChannelMinMaxDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MinMaxChannel'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,MinMaxChannelName,MinMaxChannelNumber,Comment)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:MinMaxChannelName,:MinMaxChannelNumber,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile12DatabaseAgent.WriteChannelMinMaxValuesSQL: string;
const OPNAME = 'TFile12DatabaseAgent.WriteChannelMinMaxValuesSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MinMaxChannelFlow'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,SubIdentifier'+
              ' ,MFlow01,MFlow02,MFlow03,MFlow04,MFlow05,MFlow06'+
              ' ,MFlow07,MFlow08,MFlow09,MFlow10,MFlow11,MFlow12,Comment)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:SubIdentifier'+
              ' ,:MFlow01,:MFlow02,:MFlow03,:MFlow04,:MFlow05,:MFlow06'+
              ' ,:MFlow07,:MFlow08,:MFlow09,:MFlow10,:MFlow11,:MFlow12,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFile12DatabaseAgent.WriteF12UnkownDataSQL: string;
const OPNAME = 'TFile12DatabaseAgent.WriteF12UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile12DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile12DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LPrevioudID,
  LCount : Integer;

  LChannelMinMaxObject: TChannelMinMaxObject;
  LChannelMinMax:TChannelMinMax;
  LMonthlyFlowConstraints:TMonthlyFlowConstraints;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile12DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');


    LChannelMinMaxObject := ADataObject.FChannelMinMaxObject;

    if not LChannelMinMaxObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadChannelMinMaxDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile12DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.EOF do
        begin
          LChannelMinMax := TChannelMinMax.Create;
          LChannelMinMaxObject.FChannelMinMaxContainer.Add(LChannelMinMax);

          if not LDataSet.DataSet.FieldByName('MinMaxChannelName').IsNull then
          begin
            LChannelMinMax.FChannelMinMaxName.FData := Trim(LDataSet.DataSet.FieldByName('MinMaxChannelName').AsString);
            LChannelMinMax.FChannelMinMaxName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('MinMaxChannelNumber').IsNull then
          begin
            LChannelMinMax.FChannelMinMaxNumber.FData :=LDataSet.DataSet.FieldByName('MinMaxChannelNumber').AsInteger;
            LChannelMinMax.FChannelMinMaxNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment1').IsNull then
          begin
            LChannelMinMax.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment1').AsString);
            LChannelMinMax.FComment.FInitalised := True;
          end;

          LPrevioudID    := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
          while not LDataSet.DataSet.EOF do
          begin
            LMonthlyFlowConstraints := LChannelMinMax.AddMonthlyFlowConstraints;

            for LCount := MinFlowConstraints to MaxFlowConstraints do
            begin
              LFieldName := Format('%s%2.2d',['MFlow',LCount]);
              if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FInitalised := True;
              end
              else
                Break;
            end;

            if not LDataSet.DataSet.FieldByName('Comment2').IsNull then
            begin
              LMonthlyFlowConstraints.FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment2').AsString);
              LMonthlyFlowConstraints.FComment.FInitalised := True;
            end;

            LDataSet.DataSet.Next;
            if (LPrevioudID <> LDataSet.DataSet.FieldByName('Identifier').AsInteger) then
            begin
              LDataSet.DataSet.Prior;
              Break;
            end;
          end;
          LDataSet.DataSet.Next;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF12UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.EOF do
      begin
        LChannelMinMaxObject.FF12ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile12DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile12DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile12DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount,
  LCounter,
  LRepeatCount : integer;
  LDataSet : TAbstractModelDataset;
  LChannelMinMaxObject: TChannelMinMaxObject;
  LChannelMinMax:TChannelMinMax;
  LMonthlyFlowConstraints:TMonthlyFlowConstraints;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile12DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LChannelMinMaxObject := ADataObject.FChannelMinMaxObject;
    if not Assigned(LChannelMinMaxObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      for LLinesCount := 0 to LChannelMinMaxObject.FChannelMinMaxContainer.Count-1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteChannelMinMaxDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

        LChannelMinMax := TChannelMinMax(LChannelMinMaxObject.FChannelMinMaxContainer[LLinesCount]);

        if LChannelMinMax.FChannelMinMaxName.FInitalised then
         LDataSet.SetParams(['MinMaxChannelName'], [LChannelMinMax.FChannelMinMaxName.FData]);

        if LChannelMinMax.FChannelMinMaxNumber.FInitalised then
         LDataSet.SetParams(['MinMaxChannelNumber'], [IntToStr(LChannelMinMax.FChannelMinMaxNumber.FData)]);

        if LChannelMinMax.FComment.FInitalised then
         LDataSet.SetParams(['Comment'], [LChannelMinMax.FComment.FData]);

        LDataSet.ExecSQL;
        for LRepeatCount := 0 to LChannelMinMax.FMonthlyFlowConstraintsContainer.Count - 1 do
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(WriteChannelMinMaxValuesSQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);
          LDataSet.SetParams(['SubIdentifier'], [IntToStr(LRepeatCount+1)]);


          LMonthlyFlowConstraints := TMonthlyFlowConstraints(LChannelMinMax.FMonthlyFlowConstraintsContainer[LRepeatCount]);
          for LCount := MinDiversionFlow to MaxDiversionFlow do
          begin
            if not LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FInitalised then
              Break;

            LFieldName := Format('%s%2.2d',['MFlow',LCount]);
            LDataSet.SetParams([LFieldName], [FloatToStr(LMonthlyFlowConstraints.FFlowConstraintsValues[LCount].FData)]);
          end;

          if LMonthlyFlowConstraints.FComment.FInitalised then
           LDataSet.SetParams(['Comment'], [LMonthlyFlowConstraints.FComment.FData]);

          LDataSet.ExecSQL;
        end;
      end;


      for LCounter := 0 to LChannelMinMaxObject.FF12ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF12UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LChannelMinMaxObject.FF12ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile12DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile12DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile12DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'MinMaxChannel,MinMaxChannelFlow,MinMaxChannelDistribution';
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
