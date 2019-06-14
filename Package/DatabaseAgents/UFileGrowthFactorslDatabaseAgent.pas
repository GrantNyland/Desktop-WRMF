//
//
//  UNIT      : Contains TFileGrowthFactorslDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileGrowthFactorslDatabaseAgent;

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
  UGrowthFactorFileObject,
  UAbstractFileNamesObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects;

type

  TFileGrowthFactorslDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadGrowthFactorConfigSQL: string;
    function WriteGrowthFactorConfigSQL: string;
    function ReadGrowthFactorDemandSQL: string;
    function WriteGrowthFactorDemandSQL: string;
    function ReadGrowthFactorMinMaxSQL: string;
    function WriteGrowthFactorMinMaxSQL: string;
    function ReadGrowthFactorHydrologySQL: string;
    function WriteGrowthFactorHydrologySQL: string;
    function ReadGrowthFactorUnkownDataSQL(AFileID:integer): string;
    function WriteGrowthFactorUnkownDataSQL: string;
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

function TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorConfigSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorConfigSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,YearsCount'+
              ' FROM GrowthFactorConfig'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorConfigSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorConfigSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorConfig '+
              ' (Model,StudyAreaName,SubArea,Scenario,YearsCount)'+
              ' VALUES (:Model,:StudyAreaName,:SubArea,:Scenario,:YearsCount)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorDemandSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorDemandSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,Comment,Factors'+
              ' FROM GrowthFactorDemand'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorDemandSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorDemandSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorDemand '+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ValidFactors,Comment)'+
              ' VALUES '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ChannelNumber,:ValidFactors,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorMinMaxSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorMinMaxSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ArcNumber,Comment,Factors'+
              ' FROM GrowthFactorMinMax'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorMinMaxSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorMinMaxSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO  GrowthFactorMinMax '+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ValidFactors,ChannelNumber,ArcNumber,Comment)'+
              ' VALUES '+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ValidFactors,:ChannelNumber,:ArcNumber,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorHydrologySQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorHydrologySQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,GaugeNumber,Comment,AFFFactors,IRRFactors,URBFactors'+
              ' FROM GrowthFactorHydrology'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorHydrologySQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorHydrologySQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorHydrology '+
              '(Model,StudyAreaName,SubArea,Scenario,Identifier,GaugeNumber,Comment)'+
              ' VALUES '+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:GaugeNumber,:Comment)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorUnkownDataSQL(AFileID:integer): string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ReadGrowthFactorUnkownDataSQL';
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
              ' AND FileGroup     =  '+IntToStr(fgGrowthFactors)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorUnkownDataSQL: string;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.WriteGrowthFactorUnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LGrowthFactorFileObject : TGrowthFactorFileObject;
  LDemandCentresFileObject:TGrowthFactorDemandCentresFileObject;
  LMinMaxChannelFileObject:TGrowthFactorMinMaxChannelFileObject;
  LHydroDataFileObject:TGrowthFactorHydroDataFileObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGrowthFactorslDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGrowthFactorFileObject    := LPlanningFileDataObject.GrowthFactorFileObject;
    if(LGrowthFactorFileObject = nil) then
      Exit;
    if not LGrowthFactorFileObject.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LGrowthFactorFileObject.NumberOfYears.FData := 0;
      LGrowthFactorFileObject.NumberOfYears.FInitalised := True;

      //Line 1....Number of years.
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadGrowthFactorConfigSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileGrowthFactorslDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        //Exit;
      end
      else
      begin
        if not LDataSet.DataSet.FieldByName('YearsCount').IsNull then
        begin
          LGrowthFactorFileObject.NumberOfYears.FData := LDataSet.DataSet.FieldByName('YearsCount').AsInteger;
          LGrowthFactorFileObject.NumberOfYears.FInitalised := True;
        end;
      end;

      // Line 2.... Demand Channel Number
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadGrowthFactorDemandSQL);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LDemandCentresFileObject := LGrowthFactorFileObject.AddDemandCentresGrowthFactorFileObject;
        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LDemandCentresFileObject.DemandChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LDemandCentresFileObject.DemandChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('Comment').IsNull then
        begin
          LDemandCentresFileObject.Comment.FData := Trim(LDataSet.DataSet.FieldByName('Comment').AsString);
          LDemandCentresFileObject.Comment.FInitalised := True;
        end;

        // Line 3.... Growth factors
        if not LDataSet.DataSet.FieldByName('Factors').IsNull then
        begin
          LDemandCentresFileObject.GrowthFactorsForEachYear.FData := Trim(LDataSet.DataSet.FieldByName('Factors').AsString);
          LDemandCentresFileObject.GrowthFactorsForEachYear.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;

      //Line 5...Min-max Channel Number with growth...
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadGrowthFactorMinMaxSQL);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LMinMaxChannelFileObject := LGrowthFactorFileObject.AddMinMaxChannelGrowthFactorFileObject;
        if not LDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
        begin
          LMinMaxChannelFileObject.ChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
          LMinMaxChannelFileObject.ChannelNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('ArcNumber').IsNull then
        begin
          LMinMaxChannelFileObject.ArcNumber.FData := LDataSet.DataSet.FieldByName('ArcNumber').AsInteger;
          LMinMaxChannelFileObject.ArcNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('Comment').IsNull then
        begin
          LMinMaxChannelFileObject.Comment.FData := Trim(LDataSet.DataSet.FieldByName('Comment').AsString);
          LMinMaxChannelFileObject.Comment.FInitalised := True;
        end;

        //Line 6... Growth Factor for each bound with growth factor for each year of the analysis...
        if not LDataSet.DataSet.FieldByName('Factors').IsNull then
        begin
          LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData := Trim(LDataSet.DataSet.FieldByName('Factors').AsString);
          LMinMaxChannelFileObject.GrowthFactorsForEachYear.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;

      //Line 4... Number of Supply min-max Channels with grouth...
      LGrowthFactorFileObject.NumberOfSupply.FData := LGrowthFactorFileObject.MinMaxChannelGrowthFactorsCount;
      LGrowthFactorFileObject.NumberOfSupply.FInitalised := True;

      //Line 7...Hydrology growth factors...
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadGrowthFactorHydrologySQL);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LHydroDataFileObject := LGrowthFactorFileObject.AddHydroDataGrowthFactorFileObject;
        if not LDataSet.DataSet.FieldByName('GaugeNumber').IsNull then
        begin
          LHydroDataFileObject.GaugeNumber.FData := LDataSet.DataSet.FieldByName('GaugeNumber').AsInteger;
          LHydroDataFileObject.GaugeNumber.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('Comment').IsNull then
        begin
          LHydroDataFileObject.Comment.FData := Trim(LDataSet.DataSet.FieldByName('Comment').AsString);
          LHydroDataFileObject.Comment.FInitalised := True;
        end;

        //Line 8... Affrorestation Growth Factors
        if not LDataSet.DataSet.FieldByName('AFFFactors').IsNull then
        begin
          LHydroDataFileObject.GrowthFactorsForAFF.FData := Trim(LDataSet.DataSet.FieldByName('AFFFactors').AsString);
          LHydroDataFileObject.GrowthFactorsForAFF.FInitalised := True;
        end;

        //Line 9... Irrigation Growth Factors
        if not LDataSet.DataSet.FieldByName('IRRFactors').IsNull then
        begin
          LHydroDataFileObject.GrowthFactorsForIRR.FData := Trim(LDataSet.DataSet.FieldByName('IRRFactors').AsString);
          LHydroDataFileObject.GrowthFactorsForIRR.FInitalised := True;
        end;

        //Line 10... Urban abstraction Growth Factors
        if not LDataSet.DataSet.FieldByName('URBFactors').IsNull then
        begin
          LHydroDataFileObject.GrowthFactorsForURB.FData := Trim(LDataSet.DataSet.FieldByName('URBFactors').AsString);
          LHydroDataFileObject.GrowthFactorsForURB.FInitalised := True;
        end;
        LDataSet.DataSet.Next;
      end;

      //line11 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Active := False;
      LDataSet.SetSQL(ReadGrowthFactorUnkownDataSQL(AFilename.FileNumber));
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LGrowthFactorFileObject.FExtraLines.Add(Trim(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileGrowthFactorslDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.WriteModelDataToDatabase';
var
  LWhereClause,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LStop          : boolean;
  LCount         : integer;
  LGrowthFactorFileObject : TGrowthFactorFileObject;
  LDemandCentresFileObject:TGrowthFactorDemandCentresFileObject;
  LMinMaxChannelFileObject:TGrowthFactorMinMaxChannelFileObject;
  LHydroDataFileObject:TGrowthFactorHydroDataFileObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileGrowthFactorslDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LGrowthFactorFileObject  := LPlanningFileDataObject.GrowthFactorFileObject;
    if(LGrowthFactorFileObject = nil) then
      Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteGrowthFactorConfigSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(
      ['Model','StudyAreaName','SubArea','Scenario','YearsCount'],
      [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
      FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
      IntToStr(LGrowthFactorFileObject.NumberOfYears.FData)]);
      LDataSet.ExecSQL;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteGrowthFactorDemandSQL);
      for LCount := 0 to LGrowthFactorFileObject.DemandCentresGrowthFactorsCount -1 do
      begin
        LDemandCentresFileObject := LGrowthFactorFileObject.DemandCentresGrowthFactorObjectByIndex[LCount];
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
        ['Model','StudyAreaName','SubArea','Scenario','Identifier','ChannelNumber','ValidFactors'],
        [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
        FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
        IntToStr(LCount+1),IntToStr(LDemandCentresFileObject.DemandChannelNumber.FData),'Y']);
        if LDemandCentresFileObject.Comment.FInitalised then
          LDataSet.SetParams(['Comment'],[LDemandCentresFileObject.Comment.FData]);

        LDataSet.ExecSQL;
        if LDemandCentresFileObject.GrowthFactorsForEachYear.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier = ' + IntToStr(LCount+1);
          FAppModules.Database.UpdateMemoField('GrowthFactorDemand','Factors',LWhereClause,
                                               LDemandCentresFileObject.GrowthFactorsForEachYear.FData);
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteGrowthFactorMinMaxSQL);
      for LCount := 0 to LGrowthFactorFileObject.MinMaxChannelGrowthFactorsCount -1 do
      begin
        LMinMaxChannelFileObject := LGrowthFactorFileObject.MinMaxChannelGrowthFactorByIndex[LCount];
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
        ['Model','StudyAreaName','SubArea','Scenario','Identifier','ValidFactors'],
        [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
        FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
        IntToStr(LCount+1),'Y']);
        if LMinMaxChannelFileObject.ChannelNumber.FInitalised then
          LDataSet.SetParams(['ChannelNumber'],[IntToStr(LMinMaxChannelFileObject.ChannelNumber.FData)]);

        if LMinMaxChannelFileObject.ArcNumber.FInitalised then
          LDataSet.SetParams(['ArcNumber'],[IntToStr(LMinMaxChannelFileObject.ArcNumber.FData)]);

        if LMinMaxChannelFileObject.Comment.FInitalised then
          LDataSet.SetParams(['Comment'],[LMinMaxChannelFileObject.Comment.FData]);
        LDataSet.ExecSQL;

        if LMinMaxChannelFileObject.GrowthFactorsForEachYear.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier = '+IntToStr(LCount+1);
          FAppModules.Database.UpdateMemoField('GrowthFactorMinMax','Factors',LWhereClause,
                                             LMinMaxChannelFileObject.GrowthFactorsForEachYear.FData);
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteGrowthFactorHydrologySQL);
      for LCount := 0 to LGrowthFactorFileObject.HydrologyGrowthFactorsCount -1 do
      begin
        LHydroDataFileObject := LGrowthFactorFileObject.HydroDataGrowthFactorByIndex[LCount];
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
        ['Model','StudyAreaName','SubArea','Scenario','Identifier'],
        [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
        FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
        IntToStr(LCount+1)]);
        if LHydroDataFileObject.GaugeNumber.FInitalised then
          LDataSet.SetParams(['GaugeNumber'],[IntToStr(LHydroDataFileObject.GaugeNumber.FData)]);

        if LHydroDataFileObject.Comment.FInitalised then
          LDataSet.SetParams(['Comment'],[LHydroDataFileObject.Comment.FData]);
        LDataSet.ExecSQL;
        if LHydroDataFileObject.GrowthFactorsForAFF.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier = ' + IntToStr(LCount+1);
          FAppModules.Database.UpdateMemoField('GrowthFactorHydrology','AFFFactors',LWhereClause,
                                               LHydroDataFileObject.GrowthFactorsForAFF.FData);
        end;
        if LHydroDataFileObject.GrowthFactorsForIRR.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier = ' + IntToStr(LCount+1);
          FAppModules.Database.UpdateMemoField('GrowthFactorHydrology','IRRFactors',LWhereClause,
                                               LHydroDataFileObject.GrowthFactorsForIRR.FData);
        end;
        if LHydroDataFileObject.GrowthFactorsForURB.FInitalised then
        begin
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier = ' + IntToStr(LCount+1);
          FAppModules.Database.UpdateMemoField('GrowthFactorHydrology','URBFactors',LWhereClause,
                                               LHydroDataFileObject.GrowthFactorsForURB.FData);
        end;
      end;
       //line 6 onwards++++++++++++++++++++++++++++
      for LCount := 0 to LGrowthFactorFileObject.FExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteGrowthFactorUnkownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LGrowthFactorFileObject.FExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(TFileNameObject(AFileName));
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileGrowthFactorslDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileGrowthFactorslDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileGrowthFactorslDatabaseAgent.ClearModelDataInDatabase';
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
    LTableNames := 'GrowthFactorConfig,GrowthFactorDemand,GrowthFactorMinMax,GrowthFactorHydrology';
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
