//
//
//  UNIT      : Contains TFilesActionDailyDiversionManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFilesActionDailyDiversionManager;

interface
uses
  classes,
  VCL.Controls,
  contnrs,
  UAbstractFileNamesObject,
  UAbstractModelData,
  UDailyDiversionFileDataObject,
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
  TFilesActionDailyDiversionManager = class(TFilesActionAbstractManager)
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

    function CheckFileImportDate(AFilename: TAbstractModelFileName): Boolean;override;
    function CheckModelFilesAreComplete(AProgressUpdateFuntion: TProgressUpdateFuntion): Boolean; override;
    function GetFileNamesObjectCast: TModelFileNames; override;
    function ExecRunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): Boolean; override;
    function ExecValidateFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): Boolean; override;


  public
    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ImportDailyDataFlowFile(const AFileName: WideString; AStationID : integer): boolean;
    function ImportDailyInstreamFlowFile(const AFileName: WideString; AStationID : integer): boolean;
    function ImportFile14(const AFileName: WideString;AStationID : integer; AUserImport : boolean = True): boolean;

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


{ TFilesActionDailyDiversionManager }


function TFilesActionDailyDiversionManager.ImportDailyDataFlowFile(const AFileName: WideString; AStationID : integer): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ImportDailyDataFlowFile';
var
  LFileNameObject: TFileNameObject;
  LNewFileIndex: integer;
  LStop : boolean;
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
        LStop := FAppModules.GlobalData.StopOnFirstErr;
        FAppModules.GlobalData.SetStopOnFirstErr(False);
        TDailyDiversionFileDataObject(FDailyDiversionFileDataObject).FDailyFlowDataObject.FStationID := AStationID;
        Result := FFileDailyFlowDataAgent.ReadModelDataFromFile(LFileNameObject,FDailyDiversionFileDataObject,FProgressDialog.ShowProgress);//FDataFileObjects
        Result := Result and FFileDailyFlowDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDailyDiversionFileDataObject,nil);
        if Assigned(FAppModules.MainForm()) then
        begin
          FProgressDialog.FStartButton.Visible := False;
          FProgressDialog.FStopButton.Visible := False;
          FProgressDialog.ActionProgressBar.Visible := False;
          FProgressDialog.ShowModal;
          FAppModules.GlobalData.SetStopOnFirstErr(LStop);
          FProgressDialog.FStartButton.Visible := True;
          FProgressDialog.FStopButton.Visible := True;
          FProgressDialog.ActionProgressBar.Visible := True;
          TDailyDiversionDataObject(FAppModules.Model.ModelData).RefreshDailyReferenceData(AStationID);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDailyDiversionManager.ImportDailyInstreamFlowFile(const AFileName: WideString; AStationID : integer): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ImportDailyInstreamFlowFile';
var
  LFileNameObject: TFileNameObject;
  LNewFileIndex: integer;
  LStop : boolean;
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
        LStop := FAppModules.GlobalData.StopOnFirstErr;
        FAppModules.GlobalData.SetStopOnFirstErr(False);
        TDailyDiversionFileDataObject(FDailyDiversionFileDataObject).FDailyInstreamFlowDataObject.FStationID := AStationID;
        Result := FFileDailyInstreamFlowAgent.ReadModelDataFromFile(LFileNameObject,FDailyDiversionFileDataObject,FProgressDialog.ShowProgress);
        Result := Result and FFileDailyInstreamFlowDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDailyDiversionFileDataObject,nil);

        if Assigned(FAppModules.MainForm()) then
        begin
          FProgressDialog.FStartButton.Visible := False;
          FProgressDialog.FStopButton.Visible := False;
          FProgressDialog.ActionProgressBar.Visible := False;
          FProgressDialog.ShowModal;
          FAppModules.GlobalData.SetStopOnFirstErr(LStop);
          FProgressDialog.FStartButton.Visible := True;
          FProgressDialog.FStopButton.Visible := True;
          FProgressDialog.ActionProgressBar.Visible := True;
          TDailyDiversionDataObject(FAppModules.Model.ModelData).RefreshDailyInstreamFlowData(
          TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID]);
        end;
      finally
      
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDailyDiversionManager.ImportFile14(const AFileName: WideString; AStationID : integer; AUserImport : boolean = True): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ImportFile14';
var
  LFileNameObject: TFileNameObject;
  LDailyDiversionGaugeDataLoadAgent : TDailyDiversionGaugeDataLoadAgent;
  LStop : boolean;
  LStationNo : string;
begin
  Result := False;
  try

    Result := True;
    FProgressDialog.ProgressRichEdit.Clear;
    LStop := FAppModules.GlobalData.StopOnFirstErr;
    FAppModules.GlobalData.SetStopOnFirstErr(False);
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
    if Assigned(FAppModules.MainForm()) and (AUserImport) then
    begin
      FProgressDialog.FStartButton.Visible := False;
      FProgressDialog.FStopButton.Visible := False;
      FProgressDialog.ActionProgressBar.Visible := False;
      FProgressDialog.ShowModal;
      FAppModules.GlobalData.SetStopOnFirstErr(LStop);
      FProgressDialog.FStartButton.Visible := True;
      FProgressDialog.FStopButton.Visible := True;
      FProgressDialog.ActionProgressBar.Visible := True;
    end;
    if Result then
    begin
      LDailyDiversionGaugeDataLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
      try
        LStationNo := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID].StationNo;
        LDailyDiversionGaugeDataLoadAgent.LoadFile14IFRData(
        TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRDataList.GetDailyIFRDataByStationNo(LStationNo),
        FDailyDiversionFileDataObject);
        FAppModules.ViewIni.WriteString(ClassName,'Model',FAppModules.StudyArea.ModelCode);
        FAppModules.ViewIni.WriteString(ClassName,'StudyAreaName_'+FAppModules.StudyArea.StudyAreaCode,FAppModules.StudyArea.StudyAreaCode);
        FAppModules.ViewIni.WriteString(ClassName,'SubArea_'+FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.SubAreaCode);
        FAppModules.ViewIni.WriteString(ClassName,'Scenario_'+FAppModules.StudyArea.ScenarioCode,FAppModules.StudyArea.ScenarioCode);
        FAppModules.ViewIni.WriteString(ClassName,'StationNo_'+LStationNo,LStationNo);
        FAppModules.ViewIni.WriteString(ClassName,'File14_'+LStationNo,AFileName);
      finally
        FreeAndNil(LDailyDiversionGaugeDataLoadAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TFilesActionDailyDiversionManager.CheckFileImportDate(AFilename: TAbstractModelFileName): Boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.CheckFileImportDate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDailyDiversionManager.CheckModelFilesAreComplete(AProgressUpdateFuntion: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.CheckModelFilesAreComplete';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionDailyDiversionManager.CreateMemberObjects;
const OPNAME = 'TFilesActionDailyDiversionManager.CreateMemberObjects';
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

procedure TFilesActionDailyDiversionManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionDailyDiversionManager.DestroyMemberObjects';
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

function TFilesActionDailyDiversionManager.ExecClearModelData(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecClearModelData';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecLoadDataFromDatabase(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecLoadDataFromDatabase';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecLoadDataFromFiles(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecLoadDataFromFiles';
begin
  Result := inherited ExecLoadDataFromFiles(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecRunModel(
  AProgressUpdateFuntion: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecRunModel';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecSaveDataToDatabase(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecSaveDataToDatabase';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecSaveDataToFiles(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecSaveDataToFiles';  
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecValidateFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecValidateFiles';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionDailyDiversionManager.ExecValidateModelData(
  AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDailyDiversionManager.ExecValidateModelData';
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

function TFilesActionDailyDiversionManager.GetFileNamesObjectCast: TModelFileNames;
const OPNAME = 'TFilesActionDailyDiversionManager.GetFileNamesObjectCast';
begin
  Result := nil;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
