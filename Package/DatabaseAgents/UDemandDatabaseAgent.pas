//
//
//  UNIT      : Contains TDemandDatabaseAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 02/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDemandDatabaseAgent;

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
  UHydrologyFilesObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;
type

  TDemandDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadDemandFileDataSQL: string;
    function WriteDemandFileDataSQL: string;
    function DeleteModelData(AFileName:TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean;  reintroduce; virtual;
    function DeleteModelDataSQL(AFileName:TFileNameObject): string; reintroduce; virtual;
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
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



function TDemandDatabaseAgent.ReadDemandFileDataSQL: string;
const OPNAME = 'TDemandDatabaseAgent.ReadDemandFileDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      ' Hy.Model,Hy.StudyAreaName,Hy.SubArea,Hy.Scenario,Hy.Identifier,Hy.FileType,Hy.FileNumber'+
      ' ,Hy.DemandYearValue,Hy.DemandYearValuePatch,Hy.DemandTotalValue'+
      ' ,Hy.DemandMonthValue01,Hy.DemandMonthValue02,Hy.DemandMonthValue03,Hy.DemandMonthValue04,Hy.DemandMonthValue05,Hy.DemandMonthValue06'+
      ' ,Hy.DemandMonthValue07,Hy.DemandMonthValue08,Hy.DemandMonthValue09,Hy.DemandMonthValue10,Hy.DemandMonthValue11,Hy.DemandMonthValue12'+
      ' ,Hy.DemandMonthValue01Patch,Hy.DemandMonthValue02Patch,Hy.DemandMonthValue03Patch,Hy.DemandMonthValue04Patch,Hy.DemandMonthValue05Patch,Hy.DemandMonthValue06Patch'+
      ' ,Hy.DemandMonthValue07Patch,Hy.DemandMonthValue08Patch,Hy.DemandMonthValue09Patch,Hy.DemandMonthValue10Patch,Hy.DemandMonthValue11Patch,Hy.DemandMonthValue12Patch'+
      ' FROM  DemandFileData Hy'+
      ' WHERE Hy.Model =         '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      '       AND Hy.StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      '       AND Hy.SubArea =       '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      '       AND Hy.Scenario =      '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
      //'       AND Hy.FileType = :FileType'+
      '       AND Hy.FileNumber = :FileNumber'+
      ' ORDER BY Hy.Model,Hy.StudyAreaName,Hy.SubArea,Hy.Scenario,Hy.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDatabaseAgent.WriteDemandFileDataSQL: string;
const OPNAME = 'TDemandDatabaseAgent.WriteDemandFileDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DemandFileData'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,FileType,FileNumber'+
              ' ,DemandYearValue,DemandYearValuePatch,DemandTotalValue'+
              ' ,DemandMonthValue01,DemandMonthValue02,DemandMonthValue03,DemandMonthValue04,DemandMonthValue05,DemandMonthValue06'+
              ' ,DemandMonthValue07,DemandMonthValue08,DemandMonthValue09,DemandMonthValue10,DemandMonthValue11,DemandMonthValue12'+
              ' ,DemandMonthValue01Patch,DemandMonthValue02Patch,DemandMonthValue03Patch,DemandMonthValue04Patch,DemandMonthValue05Patch,DemandMonthValue06Patch'+
              ' ,DemandMonthValue07Patch,DemandMonthValue08Patch,DemandMonthValue09Patch,DemandMonthValue10Patch,DemandMonthValue11Patch,DemandMonthValue12Patch'+
              ')'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:FileType,:FileNumber'+
              ' ,:DemandYearValue,:DemandYearValuePatch,:DemandTotalValue'+
              ' ,:DemandMonthValue01,:DemandMonthValue02,:DemandMonthValue03,:DemandMonthValue04,:DemandMonthValue05,:DemandMonthValue06'+
              ' ,:DemandMonthValue07,:DemandMonthValue08,:DemandMonthValue09,:DemandMonthValue10,:DemandMonthValue11,:DemandMonthValue12'+
              ' ,:DemandMonthValue01Patch,:DemandMonthValue02Patch,:DemandMonthValue03Patch,:DemandMonthValue04Patch,:DemandMonthValue05Patch,:DemandMonthValue06Patch'+
              ' ,:DemandMonthValue07Patch,:DemandMonthValue08Patch,:DemandMonthValue09Patch,:DemandMonthValue10Patch,:DemandMonthValue11Patch,:DemandMonthValue12Patch'+
              ')';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDemandDatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage          : string;
  LDataSet          : TAbstractModelDataset;
  LCount,
  LLinesCount       : Integer;
  LDemandFileObject : THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDemandDatabaseAgent.strReadStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    ADataObject.FDemandFilesObject.AddFile(AFileName.FileNumber);
    LDemandFileObject := THydrologyFileObject(ADataObject.FDemandFilesObject.FFiles[AFileName.FileNumber]);
    if not LDemandFileObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadDemandFileDataSQL);
      //LDataSet.SetParams(['FileType'], [IntToStr(GetHydrologyFileType(FAppModules,AFileName.FileName))]);
      LDataSet.SetParams(['FileNumber'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.DataSet.Open;

      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TDemandDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      //  Exit;
      end;

      if not LDataSet.DataSet.FieldByName('FileType').IsNull then
      begin
        LDemandFileObject.FType.FData := LDataSet.DataSet.FieldByName('FileType').AsInteger;
        LDemandFileObject.FType.FInitalised := True;
      end;

      if not LDataSet.DataSet.FieldByName('FileNumber').IsNull then
      begin
        LDemandFileObject.FFileNumber.FData := LDataSet.DataSet.FieldByName('FileNumber').AsInteger;
        LDemandFileObject.FFileNumber.FInitalised := True;
      end;

      for LLinesCount := 0  to  LDataSet.DataSet.RecordCount - 1  do
        if not LDemandFileObject.AddDetailsLines then
         Exit;

      for LLinesCount := 0  to  LDataSet.DataSet.RecordCount - 1  do
      begin
        if((LLinesCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        if not LDataSet.DataSet.FieldByName('DemandYearValue').IsNull then
        begin
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValue.FData :=LDataSet.DataSet.FieldByName('DemandYearValue').AsInteger;
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValue.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('DemandYearValuePatch').IsNull then
        begin
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FData :=(Trim(LDataSet.DataSet.FieldByName('DemandYearValuePatch').AsString)+' ')[1];
          THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('DemandTotalValue').IsNull then
          begin
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FData :=LDataSet.DataSet.FieldByName('DemandTotalValue').AsFloat;
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FInitalised := True;
          end;


        for LCount := MinMonths to MaxMonths do
         begin
           LFieldName := Format('%s%2.2d',['DemandMonthValue',LCount]);
           if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
           begin
             THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
             THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FInitalised := True;
           end;

           LFieldName := Format('DemandMonthValue%2.2dPatch',[LCount]);
           if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
           begin
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FData :=(Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString)+' ')[1];
            THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FInitalised := True;
           end;
        end;

        LDataSet.DataSet.next;
      end;

      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TDemandDatabaseAgent.strReadEnded');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDemandDatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount : integer;
  LDataSet : TAbstractModelDataset;
  LDemandFileObject : THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDemandDatabaseAgent.strWriteStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LDemandFileObject := THydrologyFileObject(ADataObject.FDemandFilesObject.FFiles[AFileName.FileNumber]);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      for LLinesCount := 0  to  LDemandFileObject.FProjectsDetails.Count - 1  do
      begin
        if((LLinesCount mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDemandFileDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);

      if LDemandFileObject.FType.FInitalised then
        LDataSet.SetParams(['FileType'], [IntToStr(LDemandFileObject.FType.FData)]);

      if LDemandFileObject.FFileNumber.FInitalised then
        LDataSet.SetParams(['FileNumber'], [IntToStr(AFileName.FileNumber)]);

        if THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValue.FInitalised  then
          LDataSet.SetParams(['DemandYearValue'], [IntToStr(THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValue.FData)]);

        if THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FInitalised  then
          LDataSet.SetParams(['DemandYearValuePatch'], [THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FData]);

        if THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FInitalised then
         LDataSet.SetParams(['DemandTotalValue'], [FloatToStr(THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FData)]);

        for LCount := MinMonths to MaxMonths do
         begin
           LFieldName := Format('%s%2.2d',['DemandMonthValue',LCount]);
           if THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FData)]);

           LFieldName := Format('DemandMonthValue%2.2dPatch',[LCount]);
           if THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FInitalised then
            LDataSet.SetParams([LFieldName], [THydrologyFileLine(LDemandFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FData]);
        end;

        LDataSet.ExecSQL;
      end;

      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TDemandDatabaseAgent.strWriteEnded');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TDemandDatabaseAgent.ClearModelDataInDatabase';
var
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

    Result := DeleteModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDatabaseAgent.DeleteModelData(AFileName: TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TDemandDatabaseAgent.DeleteModelData';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LStop: boolean;
begin
  Result := False;

  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if (not Assigned(AFileName)) then
      raise Exception.Create('There are no tables to be cleared of model data.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if not AQuetly  then
      begin
        LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strDeleteModelData');
        LMessage := Format(LMessage,['DemandFileData']);
        AProgressFunction(LMessage,ptNone,LStop);
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(DeleteModelDataSQL(AFileName));
      LDataSet.ExecSQL;
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandDatabaseAgent.DeleteModelDataSQL(AFileName: TFileNameObject): string;
const OPNAME = 'TDemandDatabaseAgent.DeleteModelDataSQL';
begin
  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM DemandFileData'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileNumber    =  '+IntToStr(AFileName.FileNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
