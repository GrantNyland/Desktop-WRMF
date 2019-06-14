//
//
//  UNIT      : Contains TFilesActionStomsaManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/03/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFilesActionStomsaManager;

interface

uses
  classes,
  Contnrs,
  VCL.Controls,
  VCL.Dialogs,
  VCL.StdCtrls,
  UDataFileObjects,
  UAbstractObject,
  UFileNames,
  UFilesActionAbstractManager,
  UFileParamAgent,
  UHydrologyFileAgent,
  UFileParamDatabaseAgent,
  UHydrologyDatabaseAgent,
  UScenarioDatabaseAgent,
  UProjectFileDatabaseAgent,
  UAbstractFileNamesObject;
type
  TFilesActionStomsaManager = class(TFilesActionAbstractManager)
  protected
    FFileParamAgent: TFileParamAgent;
    FHydrologyFileAgent: THydrologyFileAgent;
    FFileParamDatabaseAgent:TFileParamDatabaseAgent;
    FScenarioDatabaseAgent: TScenarioDatabaseAgent;
    FHydrologyDatabaseAgent: THydrologyDatabaseAgent;
    FProjectFileDatabaseAgent : TProjectFileDatabaseAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean; override;
    function AdvanceProgressBar(ASteps: integer): boolean;
    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecValidateFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecRunModel(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecImportOutputFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; virtual;
    function GetFileNamesObjectCast: TModelFileNames;override;
    function CheckFileImportDate(AFilename: TAbstractModelFileName): Boolean;override;

  public
    function DoImportAllFiles: boolean; override;
    function DoExportAllFiles: boolean; override;
  end;

implementation

uses
  SysUtils,
  System.UITypes,
  UUtilities,
  UConstants,
  URunOptionsDialog,
  UYieldContextValidationType,
//  UYRCGraphDataObject,
  UErrorHandlingOperations, Math;


{ TFilesActionStomsaManager }

procedure TFilesActionStomsaManager.CreateMemberObjects;
const OPNAME = 'TFilesActionStomsaManager.CreateMemberObjects';
begin
  inherited  CreateMemberObjects;

  try
    FFileParamAgent      := TFileParamAgent.Create(FAppModules);
    FHydrologyFileAgent  := THydrologyFileAgent.Create(FAppModules);
    FFileParamDatabaseAgent := TFileParamDatabaseAgent.Create(FAppModules);
    FScenarioDatabaseAgent  := TScenarioDatabaseAgent.Create(FAppModules);
    FHydrologyDatabaseAgent := THydrologyDatabaseAgent.Create(FAppModules);
    FProjectFileDatabaseAgent := TProjectFileDatabaseAgent.Create(FAppModules);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionStomsaManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionStomsaManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFileParamAgent);
    FreeAndNil(FHydrologyFileAgent);
    FreeAndNil(FFileParamDatabaseAgent);
    FreeAndNil(FScenarioDatabaseAgent);
    FreeAndNil(FHydrologyDatabaseAgent);
    FreeAndNil(FProjectFileDatabaseAgent);

    inherited  DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionStomsaManager.AdvanceProgressBar(ASteps: integer): boolean;
const OPNAME = 'TFilesActionStomsaManager.AdvanceProgressBar';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 1 to ASteps do
      UpdateProgress(True);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecValidateFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecValidateFiles';
begin
  Result := False;
  try
    Result := ExecLoadDataFromFiles(AProgressUpdateFuntion);
    if(FFileAction in [fatValidateAll,fatImportAll]) then
    begin
      if not UpdateProgress(Result) then Exit;
      Result := Result and FDataFileObjects.ValidateFileData(FAppModules,AProgressUpdateFuntion);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecLoadDataFromFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecLoadDataFromFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try

    Result := True;
    //Parameter file(Param.dat)
    LCurrentFileNames :=   FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecSaveDataToFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecSaveDataToFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    FProjectFileDatabaseAgent.ExportFile(AProgressUpdateFuntion);

    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionStomsaManager.ExecLoadDataFromDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecLoadDataFromDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecSaveDataToDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecSaveDataToDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LImportDate: TDateTime;
begin
  Result := False;
  try
    Result := True;
    // Set study import date to now
    LImportDate := Now();

    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
      LResult := FFileParamDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FHydrologyDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    FProjectFileDatabaseAgent.ImportFile(AProgressUpdateFuntion);

    if Result then
    begin
      Result := FScenarioDatabaseAgent.SetFilesLoaded(True);
      FAppModules.StudyArea.StudyImportDate := LImportDate;
      FAppModules.StudyArea.LastUpdateDate  := LImportDate;
    end;

    if Result then
       FProgressDialog.KeepStartBtnDisabled := True;

    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecClearModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecClearModelData';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if Result then
    begin
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;
      Result := FScenarioDatabaseAgent.SetFilesLoaded(False);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.CheckFileImportDate(AFilename: TAbstractModelFileName): Boolean;
const OPNAME = 'TFilesActionStomsaManager.CheckFileImportDate';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.CheckModelFilesAreComplete';
begin
  Result := False;
  try
    FExtraSteps := 0;
    FileNamesObject.CastParamFileNames.SetAllSelected(True);
    FileNamesObject.CastHydrologyFileNames.SetAllSelected(True);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecRunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecRunModel';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.GetFileNamesObjectCast: TModelFileNames;
const OPNAME = 'TFilesActionStomsaManager.GetFileNamesObjectCast';
begin
  Result := nil;
  try
    Result := TModelFileNames(ModelData.FileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecValidateModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecValidateModelData';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.ExecImportOutputFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionStomsaManager.ExecImportOutputFiles';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.DoExportAllFiles: boolean;
const OPNAME = 'TFilesActionStomsaManager.DoExportAllFiles';
begin
  Result := False;
  try
    if (MessageDlg(COverriteFilesMsg,mtConfirmation,mbOKCancel,0) <> mrOk) then
      Exit;
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatExportAll;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles((ModelData.FileNamesObject.FilesSavedInDatabaseCount * 2)  + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
      end;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strBatchWrite');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionStomsaManager.DoImportAllFiles: boolean;
const OPNAME = 'TFilesActionStomsaManager.DoImportAllFiles';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatImportAll;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles(((ModelData.FileNamesObject.FilesCount - ModelData.FileNamesObject.FilesSavedInDatabaseCount) * 2) + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToDatabase);
      end;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strBatchRead');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

