//
//
//  UNIT      : Contains TFile20DatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 22/11/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFile20DatabaseAgent;

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
  USFRFileObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile20DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadSFRDataSQL: string;
    function ReadF20UnkownDataSQL: string;
    function WriteSFRDataSQL: string;
    function WriteF20UnkownDataSQL: string;
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

function TFile20DatabaseAgent.ReadSFRDataSQL: string;
const OPNAME = 'TFile20DatabaseAgent.ReadSFRDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,InflowNodeNumber,CoveredArea,UnitRunoffFileName'+
              ' ,SoilMoistureFileName,SFRName,SFRDescr,Comment1,Comment2,Comment3'+
              ' FROM SFRSubCatchment WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile20DatabaseAgent.WriteSFRDataSQL: string;
const OPNAME = 'TFile20DatabaseAgent.WriteSFRDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO SFRSubCatchment'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,InflowNodeNumber,CoveredArea,UnitRunoffFileName'+
              ' ,SoilMoistureFileName,SFRName,SFRDescr,Comment1,Comment2,Comment3)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              ' ,:InflowNodeNumber,:CoveredArea,:UnitRunoffFileName'+
              ' ,:SoilMoistureFileName,:SFRName,:SFRDescr,:Comment1,:Comment2,:Comment3 )';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile20DatabaseAgent.ReadF20UnkownDataSQL: string;
const OPNAME = 'TFile20DatabaseAgent.ReadF20UnkownDataSQL';
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

function TFile20DatabaseAgent.WriteF20UnkownDataSQL: string;
const OPNAME = 'TFile20DatabaseAgent.WriteF20UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile20DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile20DatabaseAgent.ReadModelDataFromDatabase';
var
  LPath,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LSFRObject: TSFRObject;
  LSFRFileObject:TSFRFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile20DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LSFRFileObject := ADataObject.FSFRFileObject;
    if not LSFRFileObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadSFRDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile20DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        LPath := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath;
        while not LDataSet.DataSet.Eof do
        begin
          LSFRObject := LSFRFileObject.AddSFRObject;

          //Read Line 2
          if not LDataSet.DataSet.FieldByName('InflowNodeNumber').IsNull then
          begin
            LSFRObject.InflowNodeNumber.FData :=LDataSet.DataSet.FieldByName('InflowNodeNumber').AsInteger;
            LSFRObject.InflowNodeNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('CoveredArea').IsNull then
          begin
            LSFRObject.CoveredArea.FData :=LDataSet.DataSet.FieldByName('CoveredArea').AsFloat;
            LSFRObject.CoveredArea.FInitalised := True;
          end;

          //Read Line 3
          if not LDataSet.DataSet.FieldByName('UnitRunoffFileName').IsNull then
          begin
            LSFRObject.UnitRunoffFileName.FData :=LPath + Trim(LDataSet.DataSet.FieldByName('UnitRunoffFileName').AsString);
            LSFRObject.UnitRunoffFileName.FInitalised := True;
          end;

          //Read Line 4
          if not LDataSet.DataSet.FieldByName('SoilMoistureFileName').IsNull then
          begin
            LSFRObject.SoilMoistureFileName.FData :=LPath + Trim(LDataSet.DataSet.FieldByName('SoilMoistureFileName').AsString);
            LSFRObject.SoilMoistureFileName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SFRName').IsNull then
          begin
            LSFRObject.SFRName.FData := Trim(LDataSet.DataSet.FieldByName('SFRName').AsString);
            LSFRObject.SFRName.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('SFRDescr').IsNull then
          begin
            LSFRObject.SFRDescr.FData := Trim(LDataSet.DataSet.FieldByName('SFRDescr').AsString);
            LSFRObject.SFRDescr.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment1').IsNull then
          begin
            LSFRObject.Comment1.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment1').AsString);
            LSFRObject.Comment1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment2').IsNull then
          begin
            LSFRObject.Comment2.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment2').AsString);
            LSFRObject.Comment2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Comment3').IsNull then
          begin
            LSFRObject.Comment3.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment3').AsString);
            LSFRObject.Comment3.FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;


        //Line 5 on wards+++++++++++++++++++++++++++
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadF20UnkownDataSQL);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.DataSet.Open;

        while not LDataSet.DataSet.Eof do
        begin
          LSFRFileObject.Comment.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;

      Result :=  True;
      LMessage := FAppModules.Language.GetString('TFile20DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile20DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile20DatabaseAgent.WriteModelDataToDatabase';
var
  LCount: integer;
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LSFRObject: TSFRObject;
  LSFRFileObject:TSFRFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile20DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LSFRFileObject := ADataObject.FSFRFileObject;
    if not Assigned(LSFRFileObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LCount := 0 to LSFRFileObject.SFRCount -1 do
      begin
        LSFRObject := LSFRFileObject.SFRObjectByIndex[LCount];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteSFRDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LCount+1)]);

        if LSFRObject.InflowNodeNumber.FInitalised  then
          LDataSet.SetParams(['InflowNodeNumber'], [IntToStr(LSFRObject.InflowNodeNumber.FData)]);

        if LSFRObject.CoveredArea.FInitalised then
         LDataSet.SetParams(['CoveredArea'], [FloatToStr(LSFRObject.CoveredArea.FData)]);

        if LSFRObject.UnitRunoffFileName.FInitalised then
         LDataSet.SetParams(['UnitRunoffFileName'], [ExtractFileName(LSFRObject.UnitRunoffFileName.FData)]);

        if LSFRObject.SoilMoistureFileName.FInitalised then
         LDataSet.SetParams(['SoilMoistureFileName'], [ExtractFileName(LSFRObject.SoilMoistureFileName.FData)]);

        if LSFRObject.SFRName.FInitalised then
         LDataSet.SetParams(['SFRName'], [LSFRObject.SFRName.FData]);

        if LSFRObject.SFRDescr.FInitalised then
         LDataSet.SetParams(['SFRDescr'], [LSFRObject.SFRDescr.FData]);

        if LSFRObject.Comment1.FInitalised then
         LDataSet.SetParams(['Comment1'], [LSFRObject.Comment1.FData]);

        if LSFRObject.Comment2.FInitalised then
         LDataSet.SetParams(['Comment2'], [LSFRObject.Comment2.FData]);

        if LSFRObject.Comment3.FInitalised then
         LDataSet.SetParams(['Comment3'], [LSFRObject.Comment3.FData]);
        LDataSet.ExecSQL;
      end;

       //line 5++++++++++++++++++++++++++++
      for LCount := 0 to LSFRFileObject.Comment.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF20UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+ LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LSFRFileObject.Comment[LCount]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile20DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile20DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile20DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'SFRSubCatchment';
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
