//
//
//  UNIT      : Contains THydrologyDatabaseAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 02/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyDatabaseAgent;                           

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UAbstractModelData,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UHydrologyFilesObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  THydrologyDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadHydrologyFileDataSQL: string;
    function WriteHydrologyFileDataSQL: string;
    function DeleteModelData(AFileName:TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean;  reintroduce; virtual;
    function DeleteModelDataSQL(AFileName:TFileNameObject): string; reintroduce; virtual;
    function HydrologyFileFoundInOtherStudies(AStudyAreaName: string; AFileNamesObject: TFileNameObject): boolean; virtual;
    function HydrologyFileFoundInOtherStudiesSQL(AStudyAreaName: string; AFileNamesObject: TFileNameObject): string; virtual;
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;override;
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



function THydrologyDatabaseAgent.ReadHydrologyFileDataSQL: string;
const OPNAME = 'THydrologyDatabaseAgent.ReadHydrologyFileDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Hy.StudyAreaName,Hy.FileName,Hy.Identifier,Hy.HydroYearValue,Hy.HydroYearValuePatch,Hy.HydroTotalValue,Hy.Comment'+
      ' ,Hy.HydroMonthValue01,Hy.HydroMonthValue02,Hy.HydroMonthValue03,Hy.HydroMonthValue04,Hy.HydroMonthValue05,Hy.HydroMonthValue06'+
      ' ,Hy.HydroMonthValue07,Hy.HydroMonthValue08,Hy.HydroMonthValue09,Hy.HydroMonthValue10,Hy.HydroMonthValue11,Hy.HydroMonthValue12'+
      ' ,Hy.HydroMonthValue01Patch,Hy.HydroMonthValue02Patch,Hy.HydroMonthValue03Patch,Hy.HydroMonthValue04Patch,Hy.HydroMonthValue05Patch,Hy.HydroMonthValue06Patch'+
      ' ,Hy.HydroMonthValue07Patch,Hy.HydroMonthValue08Patch,Hy.HydroMonthValue09Patch,Hy.HydroMonthValue10Patch,Hy.HydroMonthValue11Patch,Hy.HydroMonthValue12Patch'+
      ' FROM  HydrologyFileData Hy'+
      ' WHERE Hy.FileName = :FileName'+
      ' AND Hy.StudyAreaName = :StudyAreaName'+
      ' ORDER BY Hy.FileName,Hy.Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyDatabaseAgent.WriteHydrologyFileDataSQL: string;
const OPNAME = 'THydrologyDatabaseAgent.WriteHydrologyFileDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO HydrologyFileData'+
              ' (StudyAreaName,FileName,Identifier,HydroYearValue,HydroYearValuePatch,HydroTotalValue,Comment'+
              ' ,HydroMonthValue01,HydroMonthValue02,HydroMonthValue03,HydroMonthValue04,HydroMonthValue05,HydroMonthValue06'+
              ' ,HydroMonthValue07,HydroMonthValue08,HydroMonthValue09,HydroMonthValue10,HydroMonthValue11,HydroMonthValue12'+
              ' ,HydroMonthValue01Patch,HydroMonthValue02Patch,HydroMonthValue03Patch,HydroMonthValue04Patch,HydroMonthValue05Patch,HydroMonthValue06Patch'+
              ' ,HydroMonthValue07Patch,HydroMonthValue08Patch,HydroMonthValue09Patch,HydroMonthValue10Patch,HydroMonthValue11Patch,HydroMonthValue12Patch'+
              ')'+
              ' Values'+
              ' (:StudyAreaName,:FileName,:Identifier,:HydroYearValue,:HydroYearValuePatch,:HydroTotalValue,:Comment'+
              ' ,:HydroMonthValue01,:HydroMonthValue02,:HydroMonthValue03,:HydroMonthValue04,:HydroMonthValue05,:HydroMonthValue06'+
              ' ,:HydroMonthValue07,:HydroMonthValue08,:HydroMonthValue09,:HydroMonthValue10,:HydroMonthValue11,:HydroMonthValue12'+
              ' ,:HydroMonthValue01Patch,:HydroMonthValue02Patch,:HydroMonthValue03Patch,:HydroMonthValue04Patch,:HydroMonthValue05Patch,:HydroMonthValue06Patch'+
              ' ,:HydroMonthValue07Patch,:HydroMonthValue08Patch,:HydroMonthValue09Patch,:HydroMonthValue10Patch,:HydroMonthValue11Patch,:HydroMonthValue12Patch'+
              ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'THydrologyDatabaseAgent.ReadModelDataFromDatabase';
var
  LPath,
  LFieldName,
  LMessage             : string;
  LDataSet             : TAbstractModelDataset;
  LCount,
  LLinesCount          : Integer;
  LHydrologyFileObject : THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('THydrologyDatabaseAgent.strReadStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    ADataObject.FHydrologyFilesObject.AddFile(AFileName.FileNumber);
    LHydrologyFileObject := THydrologyFileObject(ADataObject.FHydrologyFilesObject.FFiles[AFileName.FileNumber]);
    if not LHydrologyFileObject.Initialise then
    Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadHydrologyFileDataSQL);
      LDataSet.SetParams(['FileName'], [UpperCase(Trim(ExtractFileName(AFileName.FileName)))]);
      LDataSet.SetParams(['StudyAreaName'], [UpperCase(Trim(FAppModules.StudyArea.StudyAreaCode))]);
      LDataSet.DataSet.Open;

      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('THydrologyDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      //  Exit;
      end;



      for LLinesCount := 0  to  LDataSet.DataSet.RecordCount - 1  do
        if not LHydrologyFileObject.AddDetailsLines then
         Exit;

      LPath := TAbstractModelData(FAppModules.Model.ModelData).FileNamesObject.HydrologyFilesPath;
      for LLinesCount := 0  to  LDataSet.DataSet.RecordCount - 1  do
      begin
        if((LLinesCount mod 100) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        if not LDataSet.DataSet.FieldByName('FileName').IsNull then
        begin
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FFileName.FData := LPath + Trim(LDataSet.DataSet.FieldByName('FileName').AsString);
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FFileName.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('HydroYearValue').IsNull then
        begin
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValue.FData :=LDataSet.DataSet.FieldByName('HydroYearValue').AsInteger;
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValue.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('HydroYearValuePatch').IsNull then
        begin
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FData :=(Trim(LDataSet.DataSet.FieldByName('HydroYearValuePatch').AsString)+' ')[1];
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('HydroTotalValue').IsNull then
          begin
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FData :=LDataSet.DataSet.FieldByName('HydroTotalValue').AsFloat;
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FInitalised := True;
          end;

        if not LDataSet.DataSet.FieldByName('Comment').IsNull then
        begin
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FComment.FData := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
          THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FComment.FInitalised := True;
        end;


        for LCount := MinMonths to MaxMonths do
         begin
           LFieldName := Format('%s%2.2d',['HydroMonthValue',LCount]);
           if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
           begin
             THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
             THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FInitalised := True;
           end;

           LFieldName := Format('HydroMonthValue%2.2dPatch',[LCount]);
           if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
           begin
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FData :=(Trim(LDataSet.DataSet.FieldByName(LFieldName).AsString)+' ')[1];
            THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FInitalised := True;
           end;
        end;

        LDataSet.DataSet.next;
      end;

      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('THydrologyDatabaseAgent.strReadEnded');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'THydrologyDatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName,
  LMessage:string;
  LCount,
  LLinesCount: integer;
  LDataSet : TAbstractModelDataset;
  LHydrologyFileObject : THydrologyFileObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('THydrologyDatabaseAgent.strWriteStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;
    LHydrologyFileObject := THydrologyFileObject(ADataObject.FHydrologyFilesObject.FFiles[AFileName.FileNumber]);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      for LLinesCount := 0  to  LHydrologyFileObject.FProjectsDetails.Count - 1  do
      begin
        if((LLinesCount mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteHydrologyFileDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['StudyAreaName'], [UpperCase(FAppModules.StudyArea.StudyAreaCode)]);
        LDataSet.SetParams(['FileName'], [UpperCase(ExtractFileName(Trim(AFileName.FileName)))]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LLinesCount+1)]);
        if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValue.FInitalised  then
          LDataSet.SetParams(['HydroYearValue'], [IntToStr(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValue.FData)]);

        if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FInitalised  then
          LDataSet.SetParams(['HydroYearValuePatch'], [THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FYearValuePatch.FData]);

        if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FInitalised then
         LDataSet.SetParams(['HydroTotalValue'], [FloatToStr(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FTotalValue.FData)]);

        if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FComment.FInitalised  then
          LDataSet.SetParams(['Comment'], [THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FComment.FData]);

        for LCount := MinMonths to MaxMonths do
         begin
           LFieldName := Format('%s%2.2d',['HydroMonthValue',LCount]);
           if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FInitalised then
            LDataSet.SetParams([LFieldName], [FloatToStr(THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValues[LCount].FData)]);

           LFieldName := Format('HydroMonthValue%2.2dPatch',[LCount]);
           if THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FInitalised then
            LDataSet.SetParams([LFieldName], [THydrologyFileLine(LHydrologyFileObject.FProjectsDetails[LLinesCount]).FMonthValuesPatch[LCount].FData]);
        end;
        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;
    finally
      LDataSet.Free;
    end;
    Result := InsertFileName(AFileName);
    if Result then
    begin
      LMessage := FAppModules.Language.GetString('THydrologyDatabaseAgent.strWriteEnded');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
         AProgressFunction: TProgressUpdateFuntion;AQuetly: boolean = False): boolean;
const OPNAME = 'THydrologyDatabaseAgent.ClearModelDataInDatabase';
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

function THydrologyDatabaseAgent.DeleteModelData(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'THydrologyDatabaseAgent.DeleteModelData';
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
      if not AQuetly then
      begin
        LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strDeleteModelData');
        LMessage := Format(LMessage,['HydrologyFileData']);
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

function THydrologyDatabaseAgent.DeleteModelDataSQL(AFileName: TFileNameObject): string;
const OPNAME = 'THydrologyDatabaseAgent.DeleteModelDataSQL';
begin

  Result := '';
  try
    Result := 'DELETE  '+
              ' FROM HydrologyFileData'+
              ' WHERE FileName    =  '+QuotedStr(UpperCase(ExtractFileName(Trim(AFileName.FileName))))+
              ' AND StudyAreaName = '+QuotedStr(UpperCase(Trim(FAppModules.StudyArea.StudyAreaCode)))+
              ' AND Identifier >= 0';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyDatabaseAgent.HydrologyFileFoundInOtherStudies(AStudyAreaName: string; AFileNamesObject: TFileNameObject): boolean;
const OPNAME = 'THydrologyDatabaseAgent.HydrologyFileFoundInOtherStudies';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(HydrologyFileFoundInOtherStudiesSQL(AStudyAreaName,AFileNamesObject));
      LDataSet.DataSet.Open;
      LDataSet.DataSet.Last;
      LDataSet.DataSet.First;
      Result :=(LDataSet.DataSet.RecordCount > 0);
    finally
      LDataSet.DataSet.Close;
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyDatabaseAgent.HydrologyFileFoundInOtherStudiesSQL(AStudyAreaName: string; AFileNamesObject: TFileNameObject): string;
const OPNAME = 'THydrologyDatabaseAgent.HydrologyFileFoundInOtherStudiesSQL';
begin
  Result := '';
  try
    Result := 'SELECT FileName,StudyAreaName  '+
              ' FROM FileNames'+
              ' WHERE FileGroup   =  '+IntToStr(AFileNamesObject.FileGroup)+
              ' AND FileName    =  '+QuotedStr(UpperCase(ExtractFilename(Trim(AFileNamesObject.FileName))))+
              ' AND StudyAreaName = '+QuotedStr(UpperCase(Trim(AStudyAreaName)));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
