//
//
//  UNIT      : Contains TFilePathsDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 02/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
//
unit UFilePathsDatabaseAgent;

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
  UPathsObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFilePathsDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadPathsDataSQL: string;
    function WritePathsDataSQL: string;

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

function TFilePathsDatabaseAgent.ReadPathsDataSQL: string;
const OPNAME = 'TFilePathsDatabaseAgent.ReadPathsDataSQL';
var
  LModelCode : string;
begin
  Result := '';
  try
    if FAppModules.StudyArea.ModelCode = CDailyDiversion then
      LModelCode := CYield
    else
      LModelCode := FAppModules.StudyArea.ModelCode;
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,FilePrefix,PrefixComment'+
      '  ,InputPath,InputComment,OutputPath,OutputComment,HydrologyPath'+
      ' ,SpecifiedDemandPath,Comment'+
      ' FROM WRYMDat'+
      ' WHERE  (Model            =' + QuotedStr(LModelCode)    +') AND'+
      '        (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePathsDatabaseAgent.WritePathsDataSQL: string;
const OPNAME = 'TFilePathsDatabaseAgent.WritePathsDataSQL';
var
  LModelCode : string;
begin
  Result := '';
  try
    if FAppModules.StudyArea.ModelCode = CDailyDiversion then
      LModelCode := CYield
    else
      LModelCode := FAppModules.StudyArea.ModelCode;
    Result := 'SELECT * '+
      ' FROM WRYMDat'+
      ' WHERE  (Model            =' + QuotedStr(LModelCode)    +') AND'+
      '        (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';

    {Result := 'INSERT INTO WRYMDat'+
              ' (Model,StudyAreaName,SubArea,Scenario,FilePrefix,PrefixComment'+
              ' ,InputPath,InputComment,OutputPath,OutputComment,Comment)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:FilePrefix,:PrefixComment'+
              ' ,:InputPath,:InputComment,:OutputPath,:OutputComment,:Comment)';}

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePathsDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePathsDatabaseAgent.ReadModelDataFromDatabase';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LPathsObject: TPathsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePathsDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LPathsObject := ADataObject.FPathsObject;
    if not LPathsObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadPathsDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        if not LDataSet.DataSet.FieldByName('FilePrefix').IsNull then
        begin
          LPathsObject.FileNamePrefix.FData := Trim(LDataSet.DataSet.FieldByName('FilePrefix').AsString);
          LPathsObject.FileNamePrefix.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('PrefixComment').IsNull then
        begin
          LPathsObject.FileNamePrefixComment.FData := TrimRight(LDataSet.DataSet.FieldByName('PrefixComment').AsString);
          LPathsObject.FileNamePrefixComment.FInitalised := True;
          LPathsObject.FileNamePrefixComment.FLength := Length(LPathsObject.FileNamePrefixComment.FData);
        end;

        if not LDataSet.DataSet.FieldByName('InputPath').IsNull then
        begin
          LPathsObject.InputFilesPath.FData := Trim(LDataSet.DataSet.FieldByName('InputPath').AsString);
          LPathsObject.InputFilesPath.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('InputComment').IsNull then
        begin
          LPathsObject.InputFilesPathComment.FData := TrimRight(LDataSet.DataSet.FieldByName('InputComment').AsString);
          LPathsObject.InputFilesPathComment.FInitalised := True;
          LPathsObject.InputFilesPathComment.FLength := Length(LPathsObject.InputFilesPathComment.FData);
        end;

        if not LDataSet.DataSet.FieldByName('OutputPath').IsNull then
        begin
          LPathsObject.OutputFilesPath.FData := Trim(LDataSet.DataSet.FieldByName('OutputPath').AsString);
          LPathsObject.OutputFilesPath.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('OutputComment').IsNull then
        begin
          LPathsObject.OutputFilesPathComment.FData := TrimRight(LDataSet.DataSet.FieldByName('OutputComment').AsString);
          LPathsObject.OutputFilesPathComment.FInitalised := True;
          LPathsObject.OutputFilesPathComment.FLength := Length(LPathsObject.OutputFilesPathComment.FData);
        end;

        if not LDataSet.DataSet.FieldByName('HydrologyPath').IsNull then
        begin
          LPathsObject.HydrologyPath.FData := Trim(LDataSet.DataSet.FieldByName('HydrologyPath').AsString);
          LPathsObject.HydrologyPath.FInitalised := True;
          LPathsObject.HydrologyPath.FLength := Length(LPathsObject.HydrologyPath.FData);
        end;

        if not LDataSet.DataSet.FieldByName('SpecifiedDemandPath').IsNull then
        begin
          LPathsObject.SpecifiedDemandPath.FData := Trim(LDataSet.DataSet.FieldByName('SpecifiedDemandPath').AsString);
          LPathsObject.SpecifiedDemandPath.FInitalised := True;
          LPathsObject.SpecifiedDemandPath.FLength := Length(LPathsObject.SpecifiedDemandPath.FData);
        end;

        if not LDataSet.DataSet.FieldByName('Comment').IsNull then
        begin
          LPathsObject.Comment.Text := TrimRight(LDataSet.DataSet.FieldByName('Comment').AsString);
        end;
      end;
      LDataSet.DataSet.Active := False;

      Result :=  True;
      LMessage := FAppModules.Language.GetString('TFilePathsDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePathsDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilePathsDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LPathsObject: TPathsObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFilePathsDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LPathsObject := ADataObject.FPathsObject;
    if not Assigned(LPathsObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(WritePathsDataSQL);
      LDataSet.SetReadOnly(FALSE);
      LDataSet.DataSet.Open;
      LDataSet.DataSet.Insert;
      LDataSet.DataSet.FieldByName('Model').AsString         := FAppModules.StudyArea.ModelCode;
      LDataSet.DataSet.FieldByName('StudyAreaName').AsString := FAppModules.StudyArea.StudyAreaCode;
      LDataSet.DataSet.FieldByName('SubArea').AsString       := FAppModules.StudyArea.SubAreaCode;
      LDataSet.DataSet.FieldByName('Scenario').AsString      := FAppModules.StudyArea.ScenarioCode;

      if LPathsObject.FileNamePrefix.FInitalised then
        LDataSet.DataSet.FieldByName('FilePrefix').AsString   := LPathsObject.FileNamePrefix.FData;

      if LPathsObject.FileNamePrefixComment.FInitalised then
        LDataSet.DataSet.FieldByName('PrefixComment').AsString   := LPathsObject.FileNamePrefixComment.FData;

      if LPathsObject.InputFilesPath.FInitalised then
        LDataSet.DataSet.FieldByName('InputPath').AsString   := LPathsObject.InputFilesPath.FData;

      if LPathsObject.InputFilesPathComment.FInitalised then
        LDataSet.DataSet.FieldByName('InputComment').AsString   := LPathsObject.InputFilesPathComment.FData;

      if LPathsObject.OutputFilesPath.FInitalised then
        LDataSet.DataSet.FieldByName('OutputPath').AsString   := LPathsObject.OutputFilesPath.FData;

      if LPathsObject.OutputFilesPathComment.FInitalised then
        LDataSet.DataSet.FieldByName('OutputComment').AsString   := LPathsObject.OutputFilesPathComment.FData;

      if LPathsObject.HydrologyPath.FInitalised then
        LDataSet.DataSet.FieldByName('HydrologyPath').AsString   := LPathsObject.HydrologyPath.FData;

      if LPathsObject.SpecifiedDemandPath.FInitalised then
        LDataSet.DataSet.FieldByName('SpecifiedDemandPath').AsString   := LPathsObject.SpecifiedDemandPath.FData;

      if LPathsObject.Comment.Count > 0 then
        LDataSet.DataSet.FieldByName('Comment').Value   := LPathsObject.Comment.Text;

      LDataSet.DataSet.Post;

      if not LPathsObject.SpecifiedDemandPath.FInitalised then
        if LPathsObject.HydrologyPath.FInitalised then
           TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath :=  LPathsObject.HydrologyPath.FData;


      {ClearQueryParams(LDataSet);
      LDataSet.ParamByName('Model').AsString         := FAppModules.StudyArea.ModelCode;
      LDataSet.ParamByName('StudyAreaName').AsString := FAppModules.StudyArea.StudyAreaCode;
      LDataSet.ParamByName('SubArea').AsString       := FAppModules.StudyArea.SubAreaCode;
      LDataSet.ParamByName('Scenario').AsString      := FAppModules.StudyArea.ScenarioCode;

      if LPathsObject.FileNamePrefix.FInitalised then
        LDataSet.ParamByName('FilePrefix').AsString   := LPathsObject.FileNamePrefix.FData;

      if LPathsObject.FileNamePrefixComment.FInitalised then
        LDataSet.ParamByName('PrefixComment').AsString   := LPathsObject.FileNamePrefixComment.FData;

      if LPathsObject.InputFilesPath.FInitalised then
        LDataSet.ParamByName('InputPath').AsString   := LPathsObject.InputFilesPath.FData;

      if LPathsObject.InputFilesPathComment.FInitalised then
        LDataSet.ParamByName('InputComment').AsString   := LPathsObject.InputFilesPathComment.FData;

      if LPathsObject.OutputFilesPath.FInitalised then
        LDataSet.ParamByName('OutputPath').AsString   := LPathsObject.OutputFilesPath.FData;

      if LPathsObject.OutputFilesPathComment.FInitalised then
        LDataSet.ParamByName('OutputComment').AsString   := LPathsObject.OutputFilesPathComment.FData;

      if LPathsObject.Comment.Count > 0 then
        LDataSet.ParamByName('Comment').AsMemo   := LPathsObject.Comment.Text;

      LDataSet.ExecSQL;}
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFilePathsDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilePathsDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFilePathsDatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'WRYMDat';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
