//
//
//  UNIT      : Contains TIFRFilesActionManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRFilesActionManager;

interface
uses
  classes,
  Controls,
  contnrs,
  UAbstractFileNamesObject,
  UAbstractModelData,
  //UDailyDiversionFileDataObject,
  UProgressDialog,
  UAbstractObject,
  UFilesActionAbstractManager,
  UFileNames,

  UFilePathsDatabaseAgent,
  UFile14Agent,

  UFileDailyFlowDataAgent,
  UFileDailyFlowDatabaseAgent,

  UFileDailyInstreamFlowDataAgent,
  UFileDailyInstreamFlowDatabaseAgent;

type
  TIFRFilesActionManager = class(TFilesActionAbstractManager)
  protected

    FFile14Agent: TFile14Agent;
    FFileDailyFlowDataAgent : TFileDailyFlowDataAgent;
    FFileDailyFlowDatabaseAgent : TFileDailyFlowDatabaseAgent;

    FFileDailyInstreamFlowAgent : TFileDailyInstreamFlowDataAgent;
    FFileDailyInstreamFlowDatabaseAgent : TFileDailyInstreamFlowDatabaseAgent;

    FFilePathsDatabaseAgent : TFilePathsDatabaseAgent;

    FDailyDiversionFileDataObject : TDailyDiversionFileDataObject;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

  public
    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ImportDailyDataFlowFile(const AFileName: WideString; AStationNo : WideString): boolean;
    function ImportDailyInstreamFlowFile(const AFileName: WideString; AStationNo : WideString): boolean;
    function ImportFile14(const AFileName: WideString): boolean;

end;
implementation
uses
  SysUtils,
  UUtilities,
  UConstants,
  UDailyDiversionDataObject,
  UReleaseStructureObject,
  UYieldModelDataObject,
  UFileNameConstants,
  UDailyDiversionGaugeDataLoadAgent,
  UErrorHandlingOperations;


{ TIFRFilesActionManager }


function TIFRFilesActionManager.ImportDailyDataFlowFile(const AFileName: WideString; AStationNo : WideString): boolean;
const OPNAME = 'TIFRFilesActionManager.ImportDailyDataFlowFile';
var
  LFileNameObject: TFileNameObject;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    LNewFileIndex := TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DailyDataFlowFileNames.Count + 1;
    TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddDailyDataFlowFileName(LNewFileIndex,AFileNAme,False,Now,Now);
    LFileNameObject :=
    TFileNameObject(TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DailyDataFlowFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
      (LFileNameObject.FileFound) and
      (not LFileNameObject.SavedInDB)  then
    begin
      try
        FProgressDialog.ProgressRichEdit.Clear;
        TDailyDiversionFileDataObject(FDailyDiversionFileDataObject).FDailyFlowDataObject.FStationNo := AStationNo;
        Result := FFileDailyFlowDataAgent.ReadModelDataFromFile(LFileNameObject,FDailyDiversionFileDataObject,FProgressDialog.ShowProgress);//FDataFileObjects
        Result := Result and FFileDailyFlowDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDailyDiversionFileDataObject,nil);
        if Assigned(FAppModules.MainForm()) then
        begin
          FProgressDialog.FStartButton.Visible := False;
          FProgressDialog.FStopButton.Visible := False;
          FProgressDialog.ActionProgressBar.Visible := False;
          FProgressDialog.ShowModal;

          FProgressDialog.FStartButton.Visible := True;
          FProgressDialog.FStopButton.Visible := True;
          FProgressDialog.ActionProgressBar.Visible := True;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFilesActionManager.ImportDailyInstreamFlowFile(const AFileName: WideString; AStationNo : WideString): boolean;
const OPNAME = 'TIFRFilesActionManager.ImportDailyInstreamFlowFile';
var
  LFileNameObject: TFileNameObject;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    LNewFileIndex := TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DailyInstreamFlowFileNames.Count + 1;
    TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddDailyInstreamFlowFileName(LNewFileIndex,AFileNAme,False,Now,Now);
    LFileNameObject :=
    TFileNameObject(TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DailyInstreamFlowFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
      (LFileNameObject.FileFound) and
      (not LFileNameObject.SavedInDB)  then
    begin
      try
        FProgressDialog.ProgressRichEdit.Clear;
        TDailyDiversionFileDataObject(FDailyDiversionFileDataObject).FDailyInstreamFlowDataObject.FStationNo := AStationNo;
        Result := FFileDailyInstreamFlowAgent.ReadModelDataFromFile(LFileNameObject,FDailyDiversionFileDataObject,FProgressDialog.ShowProgress);
        Result := Result and FFileDailyInstreamFlowDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDailyDiversionFileDataObject,nil);
        if Assigned(FAppModules.MainForm()) then
        begin
          FProgressDialog.FStartButton.Visible := False;
          FProgressDialog.FStopButton.Visible := False;
          FProgressDialog.ActionProgressBar.Visible := False;
          FProgressDialog.ShowModal;

          FProgressDialog.FStartButton.Visible := True;
          FProgressDialog.FStopButton.Visible := True;
          FProgressDialog.ActionProgressBar.Visible := True;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFilesActionManager.ImportFile14(const AFileName: WideString): boolean;
const OPNAME = 'TIFRFilesActionManager.ImportFile14';
var
  LFileNameObject: TFileNameObject;
  LDailyDiversionGaugeDataLoadAgent : TDailyDiversionGaugeDataLoadAgent;
begin
  Result := False;
  try

    Result := True;
    FProgressDialog.ProgressRichEdit.Clear;
    TDailyDiversionDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastConfigFileNames.FileNameObject[13].FileName := AFileName;
    LFileNameObject := TFileNameObject.Create(FAppModules);
    LFileNameObject.SetFileName(ExtractFilePath(AFileName),Trim(AFileName),False,fgConfiguration,fgConfiguration,Now,Now);
    LFileNameObject.FileReadOnly := False;
    LFileNameObject.Importable   := True;
    LFileNameObject.Exportable   := True;
    LFileNameObject.Validatable  := True;
    FFilePathsDatabaseAgent.ReadModelDataFromDatabase(LFileNameObject,FDailyDiversionFileDataObject,FProgressDialog.ShowProgress);

    if FileExists(AFileName) then
      Result := FFile14Agent.ReadModelDataFromFile(LFileNameObject,FDailyDiversionFileDataObject,FProgressDialog.ShowProgress);
    if Result then
    begin
      LDailyDiversionGaugeDataLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
      try
        LDailyDiversionGaugeDataLoadAgent.LoadFile14IFRData(TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRData,FDailyDiversionFileDataObject);
        FAppModules.ViewIni.WriteString(ClassName,'File14',AFileName);
        FAppModules.ViewIni.WriteString(ClassName,'Model',FAppModules.StudyArea.ModelCode);
        FAppModules.ViewIni.WriteString(ClassName,'StudyAreaName',FAppModules.StudyArea.StudyAreaCode);
        FAppModules.ViewIni.WriteString(ClassName,'SubArea',FAppModules.StudyArea.SubAreaCode);
        FAppModules.ViewIni.WriteString(ClassName,'Scenario',FAppModules.StudyArea.ScenarioCode);
      finally
        FreeAndNil(LDailyDiversionGaugeDataLoadAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TIFRFilesActionManager.CreateMemberObjects;
const OPNAME = 'TIFRFilesActionManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FFileDailyFlowDataAgent := TFileDailyFlowDataAgent.Create(FAppModules);
    FFileDailyFlowDatabaseAgent := TFileDailyFlowDatabaseAgent.Create(FAppModules);
    FFileDailyInstreamFlowAgent := TFileDailyInstreamFlowDataAgent.Create(FAppModules);
    FFileDailyInstreamFlowDatabaseAgent := TFileDailyInstreamFlowDatabaseAgent.Create(FAppModules);
    FFile14Agent := TFile14Agent.Create(FAppModules);
    FFilePathsDatabaseAgent := TFilePathsDatabaseAgent.Create(FAppModules);
    FDailyDiversionFileDataObject := TDailyDiversionFileDataObject.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRFilesActionManager.DestroyMemberObjects;
const OPNAME = 'TIFRFilesActionManager.DestroyMemberObjects';
begin
  inherited  DestroyMemberObjects;
  try
    FreeAndNil(FFileDailyFlowDataAgent);
    FreeAndNil(FFileDailyFlowDatabaseAgent);
    FreeAndNil(FFileDailyInstreamFlowAgent);
    FreeAndNil(FFileDailyInstreamFlowDatabaseAgent);
    FreeAndNil(FDailyDiversionFileDataObject);
    FreeAndNil(FFile14Agent);
    FreeAndNil(FFilePathsDatabaseAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFilesActionManager.ExecClearModelData(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIFRFilesActionManager.ExecClearModelData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFilesActionManager.ExecLoadDataFromDatabase(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIFRFilesActionManager.ExecLoadDataFromDatabase';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFilesActionManager.ExecLoadDataFromFiles(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIFRFilesActionManager.ExecLoadDataFromFiles';
begin
  Result := inherited ExecLoadDataFromFiles(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFilesActionManager.ExecSaveDataToDatabase(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIFRFilesActionManager.ExecSaveDataToDatabase';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFilesActionManager.ExecSaveDataToFiles(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIFRFilesActionManager.ExecSaveDataToFiles';  
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFilesActionManager.ExecValidateModelData(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TIFRFilesActionManager.ExecValidateModelData';
var
  LColumns,
  LValidationErrors: TStringList;
  LErrors: WideString;
  LCount: integer;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressUpdateFuntion) then
      AProgressUpdateFuntion := DummyShowProgress;
    LColumns := TStringList.Create;
    LValidationErrors := TStringList.Create;
    try
      AProgressUpdateFuntion('Started validating model data',ptNone,LStop);
      Result := TDailyDiversionDataObject(FAppModules.Model.ModelData).Validate(LErrors,'');
      while (LErrors <> '') do
      begin
        LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
        for LCount := 0 to LValidationErrors.Count -1 do
        begin
          if(Pos('WARNING:',LValidationErrors[LCount]) = 1) then
            AProgressUpdateFuntion(LValidationErrors[LCount],ptWarning,LStop)
          else
            AProgressUpdateFuntion(LValidationErrors[LCount],ptError,LStop);
        end;
      end;
      AProgressUpdateFuntion('Completed validating model data',ptNone,LStop);
    finally
      LColumns.Free;
      LValidationErrors.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
