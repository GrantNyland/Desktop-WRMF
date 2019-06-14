//
//
//  UNIT      : Contains TFilesActionDDTSManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/03/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFilesActionDDTSManager;

interface

uses
  classes,
  Contnrs,
  VCL.Controls,
  VCL.Dialogs,
  VCL.StdCtrls,
  VoaimsCom_TLB,
  UDataFileObjects,
  UAbstractObject,
  UFileNames,
  UFilesActionAbstractManager,
  UDDTSModel,
  UFilePathsAgent,
  UDDTSDailyDataFileAgent,
  UDDTSOutputCSVFileAgent,

  UFilePathsDatabaseAgent,
  UDDTSDailyDataDatabaseAgent,
  UAbstractFileNamesObject;

  //UFileNamesAgent,
  //USumOutFileManager,
  //URunYieldModelAgent;

type

  TFilesActionDDTSManager = class(TFilesActionAbstractManager,IIterationEventHandler)
  private
  protected
    FDailyDataFileAgent: TDDTSDailyDataFileAgent;
    FOutputCSVFileAgent : TDDTSOutputCSVFileAgent;
    FFilePathsAgent: TFilePathsAgent;

    FDailyDataDatabaseAgent: TDDTSDailyDataDatabaseAgent;
    FFilePathsDatabaseAgent:TFilePathsDatabaseAgent;
    FOldProgressMsg : string;
    FRunModel : TDDTSModel;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure AddExtraButtons;override;
    procedure OnViewRunOptionsDialog(Sender: TObject);
    function CheckFileImportDate(AFilename: TAbstractModelFileName): boolean;override;
    function CheckModelFilesAreComplete(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean; override;
    function ValidateFileName(AFileName: TAbstractModelFileName;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean; override;
    function AdvanceProgressBar(ASteps: integer): boolean;

    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecValidateFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecRunModel(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function GetFileNamesObjectCast: TModelFileNames;override;
    function OnIterationEvent(const AIterationName: WideString): WordBool; safecall;

  public
    function DoRunModel: boolean; override;
    function ImportPathsFile(const AFileNAme: WideString): boolean;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  UUtilities,
  UConstants,
  URunOptionsDialog,
  UFileNameConstants,
  UDDTSDataObject,
  UDDTSModelManager,
  UYieldContextValidationType,
//  UYRCGraphDataObject,
  UErrorHandlingOperations, Math;


{ TFilesActionDDTSManager }

procedure TFilesActionDDTSManager.CreateMemberObjects;
const OPNAME = 'TFilesActionDDTSManager.CreateMemberObjects';
begin
  inherited  CreateMemberObjects;

  try
    FDailyDataFileAgent  := TDDTSDailyDataFileAgent.Create(FAppModules);
    FOutputCSVFileAgent  := TDDTSOutputCSVFileAgent.Create(FAppModules);
    FFilePathsAgent      := TFilePathsAgent.Create(FAppModules);

    FDailyDataDatabaseAgent := TDDTSDailyDataDatabaseAgent.Create(FAppModules);
    FFilePathsDatabaseAgent := TFilePathsDatabaseAgent.Create(FAppModules);
    FRunModel := TDDTSModel.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionDDTSManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionDDTSManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDailyDataFileAgent);
    FreeAndNil(FOutputCSVFileAgent);
    FreeAndNil(FFilePathsAgent);

    FreeAndNil(FDailyDataDatabaseAgent);
    FreeAndNil(FFilePathsDatabaseAgent);
    FreeAndNil(FRunModel);
    inherited  DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionDDTSManager.ExecValidateFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecValidateFiles';
begin
  Result := False;
  try
    Result := ExecLoadDataFromFiles(AProgressUpdateFuntion);
    if(FFileAction in [fatValidateAll,fatImportAll]) then
    begin
      if not UpdateProgress(Result) then Exit;
      Result := Result and FDataFileObjects.ValidateDDTSFileData(FAppModules,AProgressUpdateFuntion);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ExecLoadDataFromFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecLoadDataFromFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat) (Start with the paths in case they are wrong)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Configuration files(F01..F13)
    LCurrentFileNames := FileNamesObject.ConfigFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      FDailyDataFileAgent.FileType := dftRunoff;
      LResult := FDailyDataFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
    begin
      FDailyDataFileAgent.FileType := dftOtherInflow;
      LResult := FDailyDataFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[01],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[01]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
    begin
      FDailyDataFileAgent.FileType := dftIncreamentalRunoff;
      LResult := FDailyDataFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[02],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[02]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
    begin
      FDailyDataFileAgent.FileType := dftEWR;
      LResult := FDailyDataFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[03],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[03]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
    begin
      FDailyDataFileAgent.FileType := dftRainfall;
      LResult := FDailyDataFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[04],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[04]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
    begin
      FDailyDataFileAgent.FileType := dftEvaporation;
      LResult := FDailyDataFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[05],FDataFileObjects,
                 AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[05]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Output files
    LCurrentFileNames := FileNamesObject.OutputFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
    begin
      LResult := FOutputCSVFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[01],FDataFileObjects,
                 AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ExecSaveDataToFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecSaveDataToFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LResult: boolean;
  LFileAction : TFileActionType;
begin
  Result := False;
  try
    Result := True;

    LFileAction := FFileAction;
    if (FFileAction = fatRunModel) then
      FFileAction := fatExportAll;
    try

      //Directory file(Wrym.dat)
      LCurrentFileNames :=  FileNamesObject.DirectoryFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFilePathsAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                   FDataFileObjects, AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

        //Configuration files(F01..F13)
      LCurrentFileNames := FileNamesObject.ConfigFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        FDailyDataFileAgent.FileType := dftRunoff;
        LResult := FDailyDataFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
      begin
        FDailyDataFileAgent.FileType := dftOtherInflow;
        LResult := FDailyDataFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[01],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[01]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[02],AProgressUpdateFuntion) then
      begin
        FDailyDataFileAgent.FileType := dftIncreamentalRunoff;
        LResult := FDailyDataFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[02],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[02]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[03],AProgressUpdateFuntion) then
      begin
        FDailyDataFileAgent.FileType := dftEWR;
        LResult := FDailyDataFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[03],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[03]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[04],AProgressUpdateFuntion) then
      begin
        FDailyDataFileAgent.FileType := dftRainfall;
        LResult := FDailyDataFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[04],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[04]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      if ValidateFileName(LCurrentFileNames.FileNameObject[05],AProgressUpdateFuntion) then
      begin
        FDailyDataFileAgent.FileType := dftEvaporation;
        LResult := FDailyDataFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[05],FDataFileObjects,
                   AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[05]).HintsSet := False;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

    //Output files
    LCurrentFileNames := FileNamesObject.OutputFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[01],AProgressUpdateFuntion) then
    begin
      LResult := FOutputCSVFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[01],FDataFileObjects,
                 AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    finally
     FFileAction := LFileAction;
    end;
    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionDDTSManager.ExecLoadDataFromDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecLoadDataFromDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LResult: boolean;
  LFileAction : TFileActionType;
begin
  Result := False;
  try
    Result := True;

    LFileAction := FFileAction;
    if (FFileAction = fatRunModel) then
      FFileAction := fatExportAll;
    try

      //Directory file(Wrym.dat)
      LCurrentFileNames := FileNamesObject.DirectoryFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFilePathsDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        FileNamesObject.PopulateHydrologyPaths(FDataFileObjects.FPathsObject.HydrologyPath.FData);
        if not UpdateProgress(LResult) then Exit;
      end;

      //Configuration files(F01..F06)
      LResult := FDailyDataDatabaseAgent.ReadModelDataFromDatabase(nil,FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;

     finally
      FFileAction := LFileAction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ExecSaveDataToDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecSaveDataToDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LResult: boolean;
  LImportDate: TDateTime;
begin
  Result := False;
  try
    Result := True;
    // Set study import date to now
    LImportDate := Now();

    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
      LResult := FFilePathsDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Configuration files(F01..F06)
    LResult := FDailyDataDatabaseAgent.WriteModelDataToDatabase(nil,FDataFileObjects,AProgressUpdateFuntion);
    Result := Result and LResult;
    if not UpdateProgress(LResult) then Exit;

    if Result then
    begin
      FAppModules.StudyArea.StudyImportDate := LImportDate;
      FAppModules.StudyArea.LastUpdateDate  := LImportDate;
    end;

    if Result then
       FProgressDialog.KeepStartBtnDisabled := True;

    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ExecClearModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecClearModelData';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Configuration files(F01..F13)
    LResult := FDailyDataDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
    Result := Result and LResult;
    if not UpdateProgress(LResult) then Exit;

    if Result then
    begin
      TDDTSDataObject(TDDTSModelManager(FAppModules.Model).ModelData).ClearModelData;
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.CheckModelFilesAreComplete';
begin
  Result := False;
  try
      Result := True;
    {FExtraSteps := 0;
    if (FFileAction = fatExportSingle ) then
    begin
      if (FileNamesObject.ConfigFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastConfigFileNames.SetAllSelected(True);
        FileNamesObject.CastParamFileNames.SetAllSelected(True);
        FileNamesObject.CastAltParamFileNames.SetAllSelected(True);
        FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      end;
      if (FileNamesObject.CastParamFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      end;
      Result := True;
    end;

    if(FFileAction = fatValidateSingle) then
    begin
      Result := True;
    end;

    if(FFileAction in [fatImportSingle,fatExportSingle]) then
    begin
      FileNamesObject.CastConfigFileNames.SetAllSelected(True);
      FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      Result := True;
    end;

    if FFileAction in [fatValidateAll,fatImportAll,fatImportSingle,fatRunModel] then
    begin
      FileNamesObject.CastConfigFileNames.SetAllSelected(True);
      FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
      Result := True;
    end;

    if Result then
    begin
      // This is used to make the track bar show progress when readind/saving network visualiser data
      if (FFileAction = fatClearModelData) then
        FExtraSteps := 0;
      if (FFileAction = fatExportAll) then
        FExtraSteps := 0;
      if (FFileAction = fatValidateAll) then
        FExtraSteps := 0;
      if (FFileAction = fatImportAll) then
        FExtraSteps := 1;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ExecRunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecRunModel';
begin
  Result := False;
  try
    FProgressDialog.FStopButton.Enabled := False;
    Result := FRunModel.RunModel(FDataFileObjects,AProgressUpdateFuntion);
    FProgressDialog.FStopButton.Enabled := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.GetFileNamesObjectCast: TModelFileNames;
const OPNAME = 'TFilesActionDDTSManager.GetFileNamesObjectCast';
begin
  Result := nil;
  try
    Result := TModelFileNames(ModelData.FileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.CheckFileImportDate(AFilename : TAbstractModelFileName) : boolean;
const OPNAME = 'TFilesActionDDTSManager.CheckFileImportDate';
begin
  Result := False;
  try
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.AdvanceProgressBar(ASteps: integer): boolean;
const OPNAME = 'TFilesActionDDTSManager.AdvanceProgressBar';
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

function TFilesActionDDTSManager.ExecValidateModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ExecValidateModelData';

  function ErrorSeverity(var AErrorMsg: string): integer;
  const OPNAME = 'ErrorSeverity';
  begin
    Result := 0;
    try
      if(Pos('WARNING:',AErrorMsg) = 1) then
      begin
        AErrorMsg := Copy(AErrorMsg,9,Length(AErrorMsg));
        Result    := 1;
      end
      else
      if(Pos('ERROR:',AErrorMsg) = 1) then
      begin
        AErrorMsg := Copy(AErrorMsg,7,Length(AErrorMsg));
        Result    := 2;
      end;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

var
  LColumns,
  LValidationErrors: TStringList;
  LStop: boolean;
  LErrors: WideString;
  LCount : integer;
  LErrorMsg : string;
begin
  Result := False;
  try
    if not Assigned(AProgressUpdateFuntion) then
      AProgressUpdateFuntion := DummyShowProgress;
    LColumns := TStringList.Create;
    LValidationErrors := TStringList.Create;
    try
      AProgressUpdateFuntion('Started validating model data',ptNone,LStop);


      Result := TDDTSDataObject(FAppModules.Model.ModelData).Validate(LErrors,'');
      while (LErrors <> '') do
      begin
        LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
        for LCount := 0 to LValidationErrors.Count -1 do
        begin
          LErrorMsg := LValidationErrors[LCount];
          if(ErrorSeverity(LErrorMsg)= 1) then
            AProgressUpdateFuntion(LErrorMsg,ptWarning,LStop)
          else
            AProgressUpdateFuntion(LErrorMsg,ptError,LStop);
        end;
      end;

     { Result := TYieldModelDataObject(FAppModules.Model.ModelData).Validate(LErrors,'');
      while (LErrors <> '') do
      begin
        LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
        for LCount := 0 to LValidationErrors.Count -1 do
        begin
          LErrorMsg := LValidationErrors[LCount];
          if(ErrorSeverity(LErrorMsg)= 1) then
            AProgressUpdateFuntion(LErrorMsg,ptWarning,LStop)
          else
            AProgressUpdateFuntion(LErrorMsg,ptError,LStop);
        end;
      end;}
      AProgressUpdateFuntion('Completed validating model data',ptNone,LStop);
    finally
      LColumns.Free;
      LValidationErrors.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.DoRunModel: boolean;
const OPNAME = 'TFilesActionDDTSManager.DoRunModel';
var
  LExportFirst : boolean;
  LResult      : word;
  
begin
  Result := False;
  try
    LResult := MessageDlg('Files in the harddrive will be used for the analysis. Would you like to update these files with data in the database?',mtConfirmation,[mbYes,mbNo,mbCancel],0);
    if(LResult = mrCancel) then Exit;

    LExportFirst := (LResult = mrYes);
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FGlobalDataIndex.Reset;
      FExtraSteps     := 0;
      FFileAction     := fatRunModel;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Clear;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      SetTotalFiles(3 + FExtraSteps);
      FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
      if LExportFirst then
      begin
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
      end
      else
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
      FProgressDialog.AddExecutionFunctions(ExecRunModel);

      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strRunModel');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      //Result := FProgressDialog.Succsessful;
      Result := True;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ImportPathsFile(const AFileNAme: WideString): boolean;
const OPNAME = 'TFilesActionDDTSManager.ImportPathsFile';
{var
  LF01FileNameObject: TFileNameObject;
  LFileNameObject: TFileNameObject;}
begin
  Result := False;
  try
    {LFileNameObject :=  TFileNameObject(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DirectoryFileNames.FindFile(AFileNAme));
    if(LFileNameObject <> nil) and
     (LFileNameObject.FileFound) and
     (not LFileNameObject.SavedInDB)  then
    begin
      CreateDataFileObject;
      try
        Result := FFilePathsAgent.ReadModelDataFromFile(LFileNameObject,FDataFileObjects,nil);

        LF01FileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastConfigFileNames.CastFileObject[0];
        if(LF01FileNameObject <> nil) then
          FFile01Agent.ReadModelDataFromFile(LF01FileNameObject,FDataFileObjects,nil);

        Result := Result and FFilePathsDatabaseAgent.WriteModelDataToDatabase(LFileNameObject,FDataFileObjects,nil);
      finally
        DestroyDataFileObject;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionDDTSManager.AddExtraButtons;
const OPNAME = 'TFilesActionDDTSManager.AddExtraButtons';
//var
//  LButton: TButton;
begin
  inherited;
  try
   {if (FFileAction = fatRunModel) then
   begin
     LButton := FProgressDialog.AddButton(AppModules.Language.GetString('TFilesActionAbstractManager.strRunOptions'));
     LButton.OnClick := OnViewRunOptionsDialog;
   end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionDDTSManager.OnViewRunOptionsDialog(Sender: TObject);
const OPNAME = 'TFilesActionDDTSManager.OnViewRunOptionsDialog';
var
  LForm : TRunOptionsDialog;
  LOptions:   IWRYMRunOptions;
begin
  try
    LForm := TRunOptionsDialog.Create(nil,FAppModules);
    try
      LForm.ChklBoxRunOptions.Items.Clear;
      LForm.ChklBoxRunOptions.Items.Add('Run Silent');
      LForm.ChklBoxRunOptions.Items.Add('Auto Run');
      LForm.ChklBoxRunOptions.Items.Add('Close On Completion');
      LForm.ChklBoxRunOptions.Items.Add('Run Debug Version');
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LForm.ChklBoxRunOptions.Items.Add('Create SUM.OUT file on the harddrive');
        LForm.ChklBoxRunOptions.Items.Add('Save output as a binary file on the harddrive');
        LForm.ChklBoxRunOptions.Items.Add('Save binary output to the database');
      end;
      LOptions := (FAppModules.Model as IYieldModel).WRYMRunOptions;
      LForm.ChklBoxRunOptions.Checked[0] :=  LOptions.RunSilent;
      LForm.ChklBoxRunOptions.Checked[1] :=  LOptions.AutoRun;
      LForm.ChklBoxRunOptions.Checked[2] :=  LOptions.CloseOnComplete;
      LForm.ChklBoxRunOptions.Checked[3] :=  LOptions.RunDebugVersion;
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LForm.ChklBoxRunOptions.Checked[4] :=  LOptions.CreateSumOutFile;
        LForm.ChklBoxRunOptions.Checked[5] :=  LOptions.SaveOutputAsBinaryFile;
        LForm.ChklBoxRunOptions.Checked[6] :=  LOptions.SaveOutputToDB;
      end;
      LForm.ShowModal;
      if(LForm.ModalResult = mrOk) then
      begin
        LOptions.RunSilent       := LForm.ChklBoxRunOptions.Checked[0];
        LOptions.AutoRun         := LForm.ChklBoxRunOptions.Checked[1];
        LOptions.CloseOnComplete := LForm.ChklBoxRunOptions.Checked[2];
        LOptions.RunDebugVersion := LForm.ChklBoxRunOptions.Checked[3];
        if (FAppModules.StudyArea.ModelVersion = '7') then
        begin
          LOptions.CreateSumOutFile       := LForm.ChklBoxRunOptions.Checked[4];
          LOptions.SaveOutputAsBinaryFile := LForm.ChklBoxRunOptions.Checked[5];
          LOptions.SaveOutputToDB         := LForm.ChklBoxRunOptions.Checked[6];
        end;
        LOptions.SaveToINI;
      end;
    finally
      LForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.ValidateFileName(AFileName: TAbstractModelFileName; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionDDTSManager.ValidateFileName';
begin
  Result := True;
  try
    if(FFileAction in [fatValidateAll,fatValidateSingle,fatImportAll,fatImportSingle]) then
    begin
      if (TFileNameObject(AFileName).FileGroup  = fgOutput) then
      begin
         if not FileExists(AFileName.FileName) then
            Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionDDTSManager.OnIterationEvent(const AIterationName: WideString): WordBool;
const OPNAME = 'TFilesActionDDTSManager.OnIterationEvent';
var
  LMessage : string;
  LStop    : boolean;
  LIterationTracker : IYieldModelIterationTracker;
begin
  Result := False;
  try
    if not (FAppModules.Model as IYieldModel).WRYMRunOptions.RunSilent then
    begin
      Result := True;
    end
    else
    begin
      LStop := False;
      LIterationTracker := (FAppModules.Model as IYieldModel).YieldModelIterationTracker;
      if(AIterationName = 'ATimePeriodEnd') then
      begin
        LMessage := AIterationName + ' : ' +
        ' Curent Sequence = '     + IntToStr(LIterationTracker.CurrentSequence)      + ' Total Sequences = '   + IntToStr(LIterationTracker.SequenceCount)+
        ' Curent TargetDraft  = ' + IntToStr(LIterationTracker.CurrentTargetDraft)   + ' Total TargetDrafts = '+ IntToStr(LIterationTracker.TargetDraftCount)+
        ' Curent Year  = '        + IntToStr(LIterationTracker.CurrentYearGregorian) + ' Total Years= '        + IntToStr(LIterationTracker.YearCount)+
        ' Curent Month  = '       + IntToStr(LIterationTracker.CurrentMonth)         + ' Total Months = '      + IntToStr(LIterationTracker.MonthCount)+
        ' Curent Interval  = '    + IntToStr(LIterationTracker.CurrentInterval)      + ' Total Intervals = '   + IntToStr(LIterationTracker.IntervalCount);
        FProgressDialog.ShowProgressInPlace(FOldProgressMsg,LMessage,ptNone,LStop);
        FOldProgressMsg := LMessage;
      end;
      LIterationTracker.Abort := LStop;
      Result := not LStop;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

