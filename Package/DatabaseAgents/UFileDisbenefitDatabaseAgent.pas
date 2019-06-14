//
//
//  UNIT      : Contains TFileDisbenefitDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileDisbenefitDatabaseAgent;

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
  UDisbenefitFileDataObjects,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFileDisbenefitDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadDisbenefitSQL: string;
    function WriteDisbenefitSQL: string;
    function ReadDisbenefitFunctionSQL: string;
    function WriteDisbenefitFunctionSQL: string;
    function ReadDisbenefitFunctionUnkownDataSQL(AFileID:integer): string;
    function WriteDisbenefitFunctionUnkownDataSQL: string;
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
     UFileNameConstants,
     UErrorHandlingOperations;

function TFileDisbenefitDatabaseAgent.ReadDisbenefitFunctionUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileDisbenefitDatabaseAgent.ReadDisbenefitFunctionUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileType      =  '+IntToStr(AFileID)+
              ' AND FileGroup     =  '+IntToStr(fgDisbenefitDefinition)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.WriteDisbenefitFunctionUnkownDataSQL: string;
const OPNAME = 'TFileDisbenefitDatabaseAgent.WriteDisbenefitFunctionUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.ReadDisbenefitSQL: string;
const OPNAME = 'TFileDisbenefitDatabaseAgent.ReadDisbenefitSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,YearsCount'+
              ' FROM Disbenefit'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.WriteDisbenefitSQL: string;
const OPNAME = 'TFileDisbenefitDatabaseAgent.WriteDisbenefitSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO Disbenefit'+
              ' (Model,StudyAreaName,SubArea,Scenario, YearsCount)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:YearsCount)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.ReadDisbenefitFunctionSQL: string;
const OPNAME = 'TFileDisbenefitDatabaseAgent.ReadDisbenefitFunctionSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Identifier, ChannelNumber, YearActive, MonthActive,'+
              ' YearObsolete, MonthObsolete, EquationDisbenefitX, EquationDisbenefitY, EquationDisbenefitCost,'+
              ' EquationDisbenefitNonSupply, WQConstraint, TDSConcentration01, TDSConcentration02, TDSConcentration03,'+
              ' TDSConcentration04, EscalationFactors, EscalationRate'+
              ' FROM DisbenefitFunction'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.WriteDisbenefitFunctionSQL: string;
const OPNAME = 'TFileDisbenefitDatabaseAgent.WriteDisbenefitFunctionSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DisbenefitFunction'+
              ' (Model,StudyAreaName,SubArea,Scenario, Identifier, ChannelNumber, YearActive, MonthActive,'+
              ' YearObsolete, MonthObsolete, EquationDisbenefitX, EquationDisbenefitY, EquationDisbenefitCost,'+
              ' EquationDisbenefitNonSupply, WQConstraint, TDSConcentration01, TDSConcentration02, TDSConcentration03,'+
              ' TDSConcentration04)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier, :ChannelNumber, :YearActive, :MonthActive,'+
              ' :YearObsolete, :MonthObsolete, :EquationDisbenefitX, :EquationDisbenefitY, :EquationDisbenefitCost,'+
              ' :EquationDisbenefitNonSupply, :WQConstraint, :TDSConcentration01, :TDSConcentration02, :TDSConcentration03,'+
              ' :TDSConcentration04)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDisbenefitDatabaseAgent.ReadModelDataFromDatabase';
var
  LCommaTextData,
  LMessage        : string;
  LDataSet        : TAbstractModelDataset;
  LStop           : boolean;
  LDisbenefit     : TDisbenefitFileObject;
  LDisbenefitFileData     : TDisbenefitFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDisbenefitDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LDisbenefitFileData    := LPlanningFileDataObject.DisbenefitFileDataObject;
    if(LDisbenefitFileData = nil) then
      Exit;
    if not LDisbenefitFileData.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadDisbenefitSQL);
      LDataSet.DataSet.Open;
      if not LDataSet.DataSet.Eof then
      begin
        LDisbenefitFileData.DataYears.FData       :=  LDataSet.DataSet.FieldByName('YearsCount').AsInteger;;
        LDisbenefitFileData.DataYears.FInitalised := True;
      end;

      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadDisbenefitFunctionSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDisbenefitDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end;

      while not LDataSet.DataSet.Eof do
      begin
        LDisbenefit := LDisbenefitFileData.AddDisbenefit;

        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LDisbenefit.ChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LDisbenefit.ChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('YearActive').IsNull then
        begin
          LDisbenefit.YearChannelActive.FData := LDataSet.DataSet.FieldByName('YearActive').AsInteger;
          LDisbenefit.YearChannelActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('MonthActive').IsNull then
        begin
          LDisbenefit.MonthChannelActive.FData := LDataSet.DataSet.FieldByName('MonthActive').AsInteger;
          LDisbenefit.MonthChannelActive.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('YearObsolete').IsNull then
        begin
          LDisbenefit.YearChannelAbsolete.FData := LDataSet.DataSet.FieldByName('YearObsolete').AsInteger;
          LDisbenefit.YearChannelAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('MonthObsolete').IsNull then
        begin
          LDisbenefit.MonthChannelAbsolete.FData := LDataSet.DataSet.FieldByName('MonthObsolete').AsInteger;
          LDisbenefit.MonthChannelAbsolete.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EquationDisbenefitX').IsNull then
        begin
          LDisbenefit.FunctionX.FData := LDataSet.DataSet.FieldByName('EquationDisbenefitX').AsFloat;
          LDisbenefit.FunctionX.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EquationDisbenefitY').IsNull then
        begin
          LDisbenefit.FunctionY.FData := LDataSet.DataSet.FieldByName('EquationDisbenefitY').AsFloat;
          LDisbenefit.FunctionY.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EquationDisbenefitNonSupply').IsNull then
        begin
          LDisbenefit.FunctionNonSupply.FData := LDataSet.DataSet.FieldByName('EquationDisbenefitNonSupply').AsFloat;
          LDisbenefit.FunctionNonSupply.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EquationDisbenefitCost').IsNull then
        begin
          LDisbenefit.FunctionCost.FData := LDataSet.DataSet.FieldByName('EquationDisbenefitCost').AsFloat;
          LDisbenefit.FunctionCost.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('WQConstraint').IsNull then
        begin
          LDisbenefit.WaterQualityConstraint.FData := LDataSet.DataSet.FieldByName('WQConstraint').AsFloat;
          LDisbenefit.WaterQualityConstraint.FInitalised := True;
        end;

        LCommaTextData := '';
        if not LDataSet.DataSet.FieldByName('EscalationRate').IsNull then
        begin
          LDisbenefit.EscaltionRate.FData := Trim(LDataSet.DataSet.FieldByName('EscalationRate').AsString);
          LCommaTextData := LDisbenefit.EscaltionRate.FData;
          LDisbenefit.EscaltionRate.FData := StringReplace(LDisbenefit.EscaltionRate.FData,',',' ',[rfReplaceAll]);
          LDisbenefit.EscaltionRate.FLength := Length(LDisbenefit.EscaltionRate.FData);
          LDisbenefit.EscaltionRate.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('EscalationFactors').IsNull then
        begin
          LDisbenefit.WQDisbenefit.FData := Trim(LDataSet.DataSet.FieldByName('EscalationFactors').AsString);
          if(LCommaTextData = '') then LCommaTextData := LDisbenefit.WQDisbenefit.FData ;
          LDisbenefit.WQDisbenefit.FData := StringReplace(LDisbenefit.WQDisbenefit.FData,',',' ',[rfReplaceAll]);
          LDisbenefit.WQDisbenefit.FLength := Length(LDisbenefit.WQDisbenefit.FData);
          LDisbenefit.WQDisbenefit.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('TDSConcentration01').IsNull then
        begin
          LDisbenefit.TDSConcentration[1].FData := LDataSet.DataSet.FieldByName('TDSConcentration01').AsFloat;
          LDisbenefit.TDSConcentration[1].FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('TDSConcentration02').IsNull then
        begin
          LDisbenefit.TDSConcentration[2].FData := LDataSet.DataSet.FieldByName('TDSConcentration02').AsFloat;
          LDisbenefit.TDSConcentration[2].FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('TDSConcentration03').IsNull then
        begin
          LDisbenefit.TDSConcentration[3].FData := LDataSet.DataSet.FieldByName('TDSConcentration03').AsFloat;
          LDisbenefit.TDSConcentration[3].FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('TDSConcentration04').IsNull then
        begin
          LDisbenefit.TDSConcentration[4].FData := LDataSet.DataSet.FieldByName('TDSConcentration04').AsFloat;
          LDisbenefit.TDSConcentration[4].FInitalised := True;
        end;

        {if (not LDisbenefitFileData.DataYears.FInitalised) and  (LCommaTextData <> '')then
        begin
          LDisbenefitFileData.DataYears.FData       :=  StringsItemsCount(LCommaTextData);
          LDisbenefitFileData.DataYears.FInitalised := (LDisbenefitFileData.DataYears.FData <> 0);
        end;}
        LDataSet.DataSet.Next;
      end;

      //line5 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadDisbenefitFunctionUnkownDataSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LDisbenefitFileData.FMExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileDisbenefitDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileDisbenefitDatabaseAgent.WriteModelDataToDatabase';
var
  LWhereClause,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LCount         : integer;
  LDisbenefit         : TDisbenefitFileObject;
  LDisbenefitFileData  : TDisbenefitFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileDisbenefitDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LDisbenefitFileData    := LPlanningFileDataObject.DisbenefitFileDataObject;
    if(LDisbenefitFileData = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteDisbenefitSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario'],
                         [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
                          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['YearsCount'], [IntToStr(LDisbenefitFileData.DataYears.FData)]);
      LDataSet.ExecSQL;


      for LCount := 0 to LDisbenefitFileData.DisbenefitCount -1 do
      begin
        LDisbenefit := LDisbenefitFileData.DisbenefitByIndex[LCount];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDisbenefitFunctionSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);

        if LDisbenefit.ChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'], [IntToStr(LDisbenefit.ChannelNumber.FData)]);

        if LDisbenefit.YearChannelActive.FInitalised then
          LDataSet.SetParams(['YearActive'], [IntToStr(LDisbenefit.YearChannelActive.FData)]);

        if LDisbenefit.MonthChannelActive.FInitalised then
          LDataSet.SetParams(['MonthActive'], [IntToStr(LDisbenefit.MonthChannelActive.FData)]);

        if LDisbenefit.YearChannelAbsolete.FInitalised then
          LDataSet.SetParams(['YearObsolete'], [IntToStr(LDisbenefit.YearChannelAbsolete.FData)]);

        if LDisbenefit.MonthChannelAbsolete.FInitalised then
          LDataSet.SetParams(['MonthObsolete'], [IntToStr(LDisbenefit.MonthChannelAbsolete.FData)]);

        if LDisbenefit.FunctionX.FInitalised then
          LDataSet.SetParams(['EquationDisbenefitX'], [FloatToStr(LDisbenefit.FunctionX.FData)]);

        if LDisbenefit.FunctionY.FInitalised then
          LDataSet.SetParams(['EquationDisbenefitY'], [FloatToStr(LDisbenefit.FunctionY.FData)]);

        if LDisbenefit.FunctionCost.FInitalised then
          LDataSet.SetParams(['EquationDisbenefitCost'], [FloatToStr(LDisbenefit.FunctionCost.FData)]);

        if LDisbenefit.FunctionNonSupply.FInitalised then
          LDataSet.SetParams(['EquationDisbenefitNonSupply'], [FloatToStr(LDisbenefit.FunctionNonSupply.FData)]);

        if LDisbenefit.WaterQualityConstraint.FInitalised then
          LDataSet.SetParams(['WQConstraint'], [FloatToStr(LDisbenefit.WaterQualityConstraint.FData)]);

        if LDisbenefit.TDSConcentration[01].FInitalised then
          LDataSet.SetParams(['TDSConcentration01'], [FloatToStr(LDisbenefit.TDSConcentration[01].FData)]);

        if LDisbenefit.TDSConcentration[02].FInitalised then
          LDataSet.SetParams(['TDSConcentration02'], [FloatToStr(LDisbenefit.TDSConcentration[02].FData)]);

        if LDisbenefit.TDSConcentration[03].FInitalised then
          LDataSet.SetParams(['TDSConcentration03'], [FloatToStr(LDisbenefit.TDSConcentration[03].FData)]);

        if LDisbenefit.TDSConcentration[04].FInitalised then
          LDataSet.SetParams(['TDSConcentration04'], [FloatToStr(LDisbenefit.TDSConcentration[04].FData)]);

        LDataSet.ExecSQL;

        {if LDisbenefit.EscaltionRate.FInitalised then
          LDataSet.SetParams(['EscalationRate'], [LDisbenefit.EscaltionRate.FData]);

        if LDisbenefit.WQDisbenefit.FInitalised then
          LDataSet.SetParams(['EscalationFactors'], [LDisbenefit.WQDisbenefit.FData]);}

        LWhereClause := ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                        ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                        ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                        ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                        ' AND Identifier    =  '+IntToStr(LCount+1);

        if LDisbenefit.EscaltionRate.FInitalised then
          FAppModules.Database.UpdateMemoField('DisbenefitFunction','EscalationRate',LWhereClause,LDisbenefit.EscaltionRate.FData);

        if LDisbenefit.WQDisbenefit.FInitalised then
          FAppModules.Database.UpdateMemoField('DisbenefitFunction','EscalationFactors',LWhereClause,LDisbenefit.WQDisbenefit.FData);
      end;

       //line13 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LDisbenefitFileData.FMExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDisbenefitFunctionUnkownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LDisbenefitFileData.FMExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileDisbenefitDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileDisbenefitDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileDisbenefitDatabaseAgent.ClearModelDataInDatabase';
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
    LTableNames := 'Disbenefit,DisbenefitFunction';
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
