//
//
//  UNIT      : Contains TFileAltParamDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 04/08/2009
//  COPYRIGHT : Copyright © 2009 DWAF
//
//
unit UFileAltParamDatabaseAgent;

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
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFileAltParamDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadAltParamDataSQL(AFileName:TFileNameObject)  : string ;
    function WriteAltParamDataSQL: string ;

  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;  override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses UUtilities,
     UdataSetType,
     UErrorHandlingOperations;

function TFileAltParamDatabaseAgent.ReadAltParamDataSQL(AFileName:TFileNameObject) : string;
const OPNAME = 'TFileAltParamDatabaseAgent.ReadAltParamDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,FileType,LineNumber,LineSection,LineData'+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = '+IntToStr(AFileName.FileGroup)+
              ' AND FileType      = '+IntToStr(AFileName.FileNumber)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileAltParamDatabaseAgent.WriteAltParamDataSQL: string;
const OPNAME = 'TFileAltParamDatabaseAgent.WriteAltParamDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAltParamDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAltParamDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  //LNoData: Boolean;
  LStop: boolean;
  LCount : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadAltParamDataSQL(TFileNameObject(AFilename)));
      LDataSet.DataSet.Open;


      //Check if there is any data.
      //LNoData := False;
      if (LDataSet.DataSet.RecordCount > 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAltParamDatabaseAgent.strReadStarted');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
        LCount := 0;
        ADataObject.FAltParamObject.Clear;
        while not LDataSet.DataSet.EOF do
        begin
          if((LCount mod 100) = 0) then
          begin
            AProgressFunction('',ptNone,LStop);
            if LStop then Exit;
          end;
          LCount := LCount + 1;

          ADataObject.FAltParamObject.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
          LDataSet.DataSet.Next;
        end;
        LMessage := FAppModules.Language.GetString('TFileAltParamDatabaseAgent.strReadEnded');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
      end;
      LDataSet.DataSet.Close;

      Result := True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAltParamDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAltParamDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LCount : Integer;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;
    if(ADataObject.FAltParamObject.Count > 0) then
    begin
      LMessage := FAppModules.Language.GetString('TFileAltParamDatabaseAgent.strWriteStarted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      if not Assigned(ADataObject) then
        raise Exception.Create('File object parameter is not yet assigned.');

      if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
        Exit;

      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteAltParamDataSQL);
        LDataSet.ClearQueryParams();
        for LCount := 0 to  ADataObject.FAltParamObject.Count - 1 do
        begin
          if((LCount mod 50) = 0) then
          begin
            AProgressFunction('',ptNone,LStop);
            if LStop then Exit;
          end;

          LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
          LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
          LDataSet.SetParams(['LineNumber'], [IntToStr(LCount + 1)]);
          LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
          LDataSet.SetParams(['LineData'], [ADataObject.FAltParamObject[LCount]]);
          LDataSet.ExecSQL;
        end;
        LDataSet.DataSet.Close;

        Result := InsertFileName(AFileName);
        if Result then
        begin
          LMessage := FAppModules.Language.GetString('TFileAltParamDatabaseAgent.strWriteEnded');
          LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
          AProgressFunction(LMessage,ptNone,LStop);
        end;
      finally
        LDataSet.Free;
      end;
    end
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileAltParamDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileAltParamDatabaseAgent.ClearModelDataInDatabase';
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

    Result := DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
