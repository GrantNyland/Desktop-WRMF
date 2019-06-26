//
//
//  UNIT      : Contains TFilesActionAbstractManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/03/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFilesActionAbstractManager;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  classes,
  VCL.Controls,
  contnrs,
  UAbstractFileNamesObject,
  UAbstractModelData,
  UDataFileObjects,
  UProgressDialog,
  UAbstractObject,
  UFileNames;
const
   COverriteFilesMsg = 'Files on your harddrive will be overwritten by files created from data in the database. Would you like to continue?';

type
  TFileGroups     = 1..50;
  TFileGroupsSet  =  set of TFileGroups;
  TFileActionType = (fatNone,fatValidateAll,fatValidateSingle,fatImportAll,fatExportAll,fatExportSingle,fatImportSingle,
                     fatImportSumOut,fatSaveOutputFiles,fatClearModelData,fatRunModel,fatReadYRCFile,fatReadYRCDB,
                     fatSaveYRCDB,fatValidateModelData);

  TGlobalDataIndex = class(TObject)
  protected
    FStopOnFirstErrIndex        : integer;
    FIncludeHydrologyFilesIndex : integer;
    FIncludeDemandFilesIndex    : integer;
  public
    procedure Reset;
    property StopOnFirstErrIndex        : integer read FStopOnFirstErrIndex        write FStopOnFirstErrIndex;
    property IncludeHydrologyFilesIndex : integer read FIncludeHydrologyFilesIndex write FIncludeHydrologyFilesIndex;
    property IncludeDemandFilesIndex    : integer read FIncludeDemandFilesIndex    write FIncludeDemandFilesIndex;
  end;

  TFilesActionAbstractManager = class(TAbstractAppObject)
  protected
    FFileAction        : TFileActionType;
    FProgressDialog    : TProgressDialog;
    FExtraSteps        : integer;
    FDataFileObjects   : TDataFileObjects;
    FGlobalDataIndex   : TGlobalDataIndex;
    FOptionalFileGoups : TFileGroupsSet;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDataFileObject; virtual;
    procedure DestroyDataFileObject; virtual;
    procedure AddExtraButtons; virtual;

    function ModelData: TAbstractModelData;
    procedure OnItemCheckUncheck(Sender: TObject);
    procedure SetTotalFiles(ATotalFiles: integer);
    function UpdateProgress(AResult: boolean): boolean;
    function ValidateFileName(AFileName: TAbstractModelFileName;AProgressUpdateFuntion:TProgressUpdateFuntion): boolean;virtual;
    function CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean; virtual; abstract;
    function CheckFileImportDate(AFilename : TAbstractModelFileName) : boolean; virtual; abstract;

    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecValidateFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecRunModel(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual; abstract;
    function ExecReadYRCDataFromFile(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ExecReadYRCDataFromDB(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function ExecSaveYRCDataToDB(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;virtual;
    function GetFileNamesObjectCast: TModelFileNames;virtual; abstract;
    property FileNamesObject: TModelFileNames read GetFileNamesObjectCast;
  public
    function DoValidateAllFiles: boolean; virtual;
    function DoImportAllFiles: boolean; virtual;
    function DoExportAllFiles: boolean; virtual;
    function DoValidateSingleFile: boolean; virtual;
    function DoExportSingleFile: boolean; virtual;
    function DoImportSingleFile: boolean; virtual;
    function DoClearModelData: boolean; virtual;
    function DoRunModel: boolean; virtual;
    function ImportOutputFiles: boolean; virtual;
    function DoRefreshFileHints: boolean; virtual;
    function DoExportAllFilesAndRunModel(ASilent:boolean): boolean;
    function DoValidateModelData: boolean; virtual;
    function DoGenerateSystemConfigurationDataFiles: Boolean; virtual;
    function CheckFileModifiedDate(AFileName:TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
  end;

implementation

uses
  UUtilities,
  System.UITypes,
  SysUtils,
  VCL.Dialogs,
  VCL.Forms,
  UFileNameConstants,
  UErrorHandlingOperations,
  VCL.FileCtrl,
  Math;


{ TFilesActionAbstractManager }

procedure TFilesActionAbstractManager.CreateMemberObjects;
const OPNAME = 'TFilesActionAbstractManager.CreateMemberObjects';
begin
  inherited  CreateMemberObjects;
  try
    FFileAction      := fatNone;
    FGlobalDataIndex := TGlobalDataIndex.Create;
    FProgressDialog  := TProgressDialog.Create(nil,FAppModules);
    FProgressDialog.clbOptions.OnClickCheck := OnItemCheckUncheck;
    FDataFileObjects := nil;
    FAppModules.GlobalData.SetStopOnFirstErr(False);
    FOptionalFileGoups :=  [fgAllocationDefinition,fgReservoirImplementation,fgDisbenefitDefinition,fgGrowthFactors,
                            fgMonthlyWaterRequirement,fgHydropowerAllocation,fgPumpingChannelControl,
                            fgGeneralChannelControl,fgReclamationPlantControl,fgReturnFlowChannel,fgAltParameter,
                            fgChannelSwitchControl,fgTariffCalculation,fgAllocationChannel,fgReleaseStructure,fgCurtail];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionAbstractManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionAbstractManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGlobalDataIndex);
    FreeAndNil(FProgressDialog);
    inherited  DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionAbstractManager.CreateDataFileObject;
const OPNAME = 'TFilesActionAbstractManager.CreateDataFileObject';
begin
  try
    DestroyDataFileObject;
    FDataFileObjects := TDataFileObjects.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionAbstractManager.DestroyDataFileObject;
const OPNAME = 'TFilesActionAbstractManager.DestroyDataFileObject';
begin
  try
    FreeAndNil(FDataFileObjects);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionAbstractManager.SetTotalFiles(ATotalFiles: integer);
const OPNAME = 'TFilesActionAbstractManager.SetTotalFiles';
begin
  try
    FProgressDialog.ActionProgressBar.Min := 0;
    FProgressDialog.ActionProgressBar.Max := ATotalFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.UpdateProgress(AResult: boolean): boolean;
const OPNAME = 'TFilesActionAbstractManager.UpdateProgress';
begin
  Result := False;
  try
    FProgressDialog.UpdateProgressBar;
    if FProgressDialog.Stopped  or ((not AResult) and FAppModules.GlobalData.StopOnFirstErr)then
      Result := False
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.ValidateFileName(AFileName: TAbstractModelFileName;
         AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionAbstractManager.ValidateFileName';
var
  LPath,
  LMesg: string;
  LStop: boolean;
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin

      if (AFileName.FileGroup = 1) and (AFileName.FileNumber = 16) then
      begin
        if not ((FAppModules.StudyArea.ModelVersion = '6.1') or
                (FAppModules.StudyArea.ModelVersion = '6.2') or
                (FAppModules.StudyArea.ModelVersion = '7'  ))   then
        Exit;


        case FFileAction of
          fatValidateSingle,
          fatImportSingle,
          fatImportAll,
          fatValidateAll  :
          begin
            if not FileExists(AFileName.FileName) then Exit;
          end;

          fatExportAll,
          fatExportSingle,
          fatClearModelData:
          begin
            if not AFileName.SavedInDB then Exit;
          end;

          fatImportSumOut,
          fatSaveOutputFiles,
          fatRunModel,
          fatReadYRCFile,
          fatReadYRCDB,
          fatSaveYRCDB,
          fatValidateModelData:
          begin
            Exit;
          end;
        end;//
        Result := True;
        Exit;
      end;

      case FFileAction of
        fatValidateAll,  fatImportSumOut, fatSaveOutputFiles,fatRunModel:
          begin
            if not FileExists(AFileName.FileName) then
            begin
              if (TFileNameObject(AFileName).FileGroup in FOptionalFileGoups)  then
                Exit
              else if (TFileNameObject(AFileName).FileGroup  = fgConfiguration) and (TFileNameObject(AFileName).FileNumber > 13) then
                Exit
              else
              begin
                LMesg := Format('File to be read does not exists (%s)',[AFileName.FileName]);
                AProgressUpdateFuntion(LMesg,ptError,LStop);
                Exit;
              end;
            end;
          end;
        fatImportAll:
          begin
            if not FileExists(AFileName.FileName) then
            begin
              if (TFileNameObject(AFileName).FileGroup in FOptionalFileGoups)  then
                Exit
              else if (TFileNameObject(AFileName).FileGroup  = fgConfiguration) and (TFileNameObject(AFileName).FileNumber > 13) then
                Exit
              else
              begin
                LMesg := Format('File to be read does not exists (%s)',[AFileName.FileName]);
                AProgressUpdateFuntion(LMesg,ptError,LStop);
                Exit;
              end;
            end;
          end;
        fatImportSingle,fatValidateSingle:
          begin
            if not TFileNameObject(AFileName).Selected then
              Exit
            else
            begin
              if not FileExists(AFileName.FileName) then
              begin
                if (TFileNameObject(AFileName).FileGroup in FOptionalFileGoups)  then
                  Exit
                else if (TFileNameObject(AFileName).FileGroup  = fgConfiguration) and (TFileNameObject(AFileName).FileNumber > 13) then
                  Exit
                else
                begin
                  LMesg := Format('File to be read does not exists (%s)',[AFileName.FileName]);
                  AProgressUpdateFuntion(LMesg,ptError,LStop);
                  Exit;
                end;
              end;
            end;
          end;
        fatExportSingle:
          begin
            if not TFileNameObject(AFileName).Selected then
              Exit
            else
            begin
              if not TFileNameObject(AFileName).SavedInDB then
              begin
                if (TFileNameObject(AFileName).FileGroup in FOptionalFileGoups)  then
                  Exit
                else if (TFileNameObject(AFileName).FileGroup  = fgConfiguration) and (TFileNameObject(AFileName).FileNumber > 13) then
                  Exit
                else
                begin
                  LMesg := Format('File not yet imported into the database (%s)',[AFileName.FileName]);
                  AProgressUpdateFuntion(LMesg,ptError,LStop);
                  Exit;
                end;
              end
              else
              begin
                LPath := ExtractFilePath(AFileName.FileName);
                LPath := IncludeTrailingPathDelimiter(LPath);
                if not SysUtils.DirectoryExists(LPath) then
                begin
                  if not SysUtils.ForceDirectories(LPath) then
                  begin
                    LMesg := Format('Could not create a non existing directory (%s)',[LPath]);
                    AProgressUpdateFuntion(LMesg,ptError,LStop);
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        fatExportAll:
          begin
            //if not TFileNameObject(AFileName).SavedInDB then
            //begin
            //  LMesg := Format('File not yet imported into the database (%s)',[AFileName.FileName]);
            //  AProgressUpdateFuntion(LMesg,ptError,LStop);
            //  Exit;
            //end
            //else
            //begin
              LPath := ExtractFilePath(AFileName.FileName);
              LPath := IncludeTrailingPathDelimiter(LPath);
              if not SysUtils.DirectoryExists(LPath) then
              begin
                if not SysUtils.ForceDirectories(LPath) then
                begin
                  LMesg := Format('Could not create a non existing directory (%s)',[LPath]);
                  AProgressUpdateFuntion(LMesg,ptError,LStop);
                  Exit;
                end;
              end;
            //end;
          end;
        fatClearModelData:
        begin
          if (AFileName.FileGroup <> 1) then
          begin
            if not TFileNameObject(AFileName).SavedInDB then
              Exit;
          end;
        end;
      end;//Case
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoValidateAllFiles: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoValidateAllFiles';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatValidateAll;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;

      if(FAppModules.Model.ModelName <>  CDDTS) then
      begin
        FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
        FProgressDialog.clbOptions.Items.Add('Include Demand Files');
        FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
        FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
      end;

      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      FGlobalDataIndex.IncludeHydrologyFilesIndex := 1;
      FGlobalDataIndex.IncludeDemandFilesIndex    := 2;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles(ModelData.FileNamesObject.FilesCount + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
      end;
      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strValidateModelFiles');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoImportAllFiles: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoImportAllFiles';
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

      if(FAppModules.Model.ModelName <>  CDDTS) then
      begin
        FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
        FProgressDialog.clbOptions.Items.Add('Include Demand Files');
        FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
        FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
      end;

      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      FGlobalDataIndex.IncludeHydrologyFilesIndex := 1;
      FGlobalDataIndex.IncludeDemandFilesIndex    := 2;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles(((ModelData.FileNamesObject.FilesCount - ModelData.FileNamesObject.FilesSavedInDatabaseCount) * 2) + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToDatabase);
      end;
      AddExtraButtons;
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

function TFilesActionAbstractManager.DoExportAllFiles: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoExportAllFiles';
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

      if(FAppModules.Model.ModelName <>  CDDTS) then
      begin
        FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
        FProgressDialog.clbOptions.Items.Add('Include Demand Files');
        FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
        FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
      end;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      FGlobalDataIndex.IncludeHydrologyFilesIndex := 1;
      FGlobalDataIndex.IncludeDemandFilesIndex    := 2;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles((ModelData.FileNamesObject.FilesSavedInDatabaseCount * 2)  + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
      end;
      AddExtraButtons;
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

function TFilesActionAbstractManager.DoExportSingleFile: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoExportSingleFile';
begin
  Result := False;
  try
    if (MessageDlg(COverriteFilesMsg,mtConfirmation,mbOKCancel,0) <> mrOk) then
      Exit;

    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatExportSingle;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles((ModelData.FileNamesObject.SelectedCount * 2)  + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
      end;
      AddExtraButtons;
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

function TFilesActionAbstractManager.DoImportSingleFile: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoImportSingleFile';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatImportSingle;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles((ModelData.FileNamesObject.SelectedCount * 2) + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToDatabase);
      end;
      AddExtraButtons;
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

function TFilesActionAbstractManager.ImportOutputFiles: boolean;
const OPNAME = 'TFilesActionAbstractManager.ImportOutputFiles';
var
  LModelFileName: TAbstractModelFileName;
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatImportSingle;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      FileNamesObject.SelectAllFiles(False);
      LModelFileName := FileNamesObject.GetSumOutFile;
      if(LModelFileName = nil) or (not FileExists(LModelFileName.FileName))then
        ShowMessage('SUM.OUT file for this study does not exist.')
      else
      begin
        LModelFileName.Selected := True;
        if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
        begin
          SetTotalFiles((ModelData.FileNamesObject.SelectedCount * 2) + FExtraSteps);
          FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
          FProgressDialog.AddExecutionFunctions(ExecSaveDataToDatabase);
        end;
        AddExtraButtons;
        FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strBatchRead');
        FProgressDialog.Succsessful := False;
        FProgressDialog.ShowModal;
        Result := FProgressDialog.Succsessful;
        FProgressDialog.Hide;
      end;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoClearModelData: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoClearModelData';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatClearModelData;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;

      if(FAppModules.Model.ModelName <>  CDDTS) then
      begin
        FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
        FProgressDialog.clbOptions.Items.Add('Include Demand Files');
        FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
        FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
      end;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      FGlobalDataIndex.IncludeHydrologyFilesIndex := 1;
      FGlobalDataIndex.IncludeDemandFilesIndex    := 2;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles(ModelData.FileNamesObject.FilesCount + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecClearModelData);
      end;
      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strClearModelData');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoValidateSingleFile: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoValidateSingleFile';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatValidateSingle;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles(ModelData.FileNamesObject.SelectedCount + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
      end;
      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strValidateModelFiles');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoRunModel: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoRunModel';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatRunModel;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles(3 + FExtraSteps);
        FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
        FProgressDialog.AddExecutionFunctions(ExecRunModel);
      end;
      AddExtraButtons;
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strRunModel');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionAbstractManager.DoRefreshFileHints: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoRefreshFileHints';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FGlobalDataIndex.Reset;
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatValidateSingle;
      //FProgressDialog.ProgressRichEdit.Clear;
      //if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      //begin
      //  SetTotalFiles(FModelFileNames.SelectedCount + FExtraSteps);
      //  FProgressDialog.AddExecutionFunctions(ExecValidateFiles);
      //end;
      //AddExtraButtons;
      //FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strValidateModelFiles');
      //FProgressDialog.Succsessful := False;
      //FProgressDialog.ShowModal;
      //Result := FProgressDialog.Succsessful;
      //FProgressDialog.Hide;
      Result := ExecValidateFiles(Nil);
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.ExecReadYRCDataFromDB(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionAbstractManager.ExecReadYRCDataFromDB';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.ExecReadYRCDataFromFile(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionAbstractManager.ExecReadYRCDataFromFile';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.ExecSaveYRCDataToDB(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionAbstractManager.ExecSaveYRCDataToDB';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoExportAllFilesAndRunModel(ASilent: boolean): boolean;
const OPNAME = 'TFilesActionAbstractManager.DoExportAllFilesAndRunModel';
begin
  Result := False;
  try
    if not ASilent then
      if (MessageDlg(COverriteFilesMsg,mtConfirmation,mbOKCancel,0) <> mrOk) then
        Exit;

    FProgressDialog.Silent := ASilent;
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatExportAll;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;

      if(FAppModules.Model.ModelName <>  CDDTS) then
      begin
        FProgressDialog.clbOptions.Items.Add('Include Hydrology Files');
        FProgressDialog.clbOptions.Items.Add('Include Demand Files');
        FProgressDialog.clbOptions.Checked[1] :=  FAppModules.GlobalData.IncludeHydrologyFiles;
        FProgressDialog.clbOptions.Checked[2] :=  FAppModules.GlobalData.IncludeDemandFiles;
      end;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;
      FGlobalDataIndex.IncludeHydrologyFilesIndex := 1;
      FGlobalDataIndex.IncludeDemandFilesIndex    := 2;

      if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
      begin
        SetTotalFiles((ModelData.FileNamesObject.FilesSavedInDatabaseCount * 2)  + FExtraSteps + 3);
        if not ASilent then
          FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
        FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
        FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
        FProgressDialog.AddExecutionFunctions(ExecRunModel);
      end;
      if ASilent then
      begin
        FProgressDialog.FStartButton.OnClick(nil);
      end
      else
      begin
        AddExtraButtons;
        FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strRunModel');
        FProgressDialog.Succsessful := False;
        FProgressDialog.ShowModal;
        Result := FProgressDialog.Succsessful;
        FProgressDialog.Hide;
      end;
    finally
      FProgressDialog.Silent := False;
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.ModelData: TAbstractModelData;
const OPNAME = 'TFilesActionAbstractManager.ModelData';
begin
  Result := Nil;
  try
    Result := TAbstractModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoValidateModelData: boolean;
const OPNAME = 'TFilesActionAbstractManager.DoValidateModelData';
begin
  Result := False;
  try
    CreateDataFileObject;
    try
      FDataFileObjects.Initialise;
      FExtraSteps     := 0;
      FFileAction     := fatValidateModelData;
      FProgressDialog.Initialise;
      FProgressDialog.clbOptions.Items.Add('Stop on first error');
      FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
      FGlobalDataIndex.Reset;
      FGlobalDataIndex.StopOnFirstErrIndex        := 0;

      SetTotalFiles(1);
      AddExtraButtons;
      FProgressDialog.AddExecutionFunctions(ExecValidateModelData);
      FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strValidateModelData');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    finally
      DestroyDataFileObject;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.CheckFileModifiedDate(AFileName: TAbstractModelFileName;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionAbstractManager.CheckFileModifiedDate';
var
  LMessage        : string;
  LStop           : boolean;
  LNewFileAge     : integer;
  LFileCreateDate : TDateTime;
begin
  Result          := False;
  try
    if not Assigned(AFileName) then Exit;
    if not FileExists(AFileName.FileName) then Exit;

    if (not FileAge(AFileName.FileName,LFileCreateDate)) then   //if (FileAge(AFileName.FileName) = -1) then
    begin
      LFileCreateDate := FileCreationDate(AFileName.FileName);
      LNewFileAge     :=  DateTimeToFileDate(LFileCreateDate);
      FileSetDate( AFileName.FileName , LNewFileAge);
      LMessage := AppModules.Language.GetString('TAbstractFileAgent.strDateDoesNotExists');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptWarning,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionAbstractManager.DoGenerateSystemConfigurationDataFiles: Boolean;
const OPNAME = 'TFilesActionAbstractManager.DoGenerateSystemConfigurationDataFiles';
var
  LPath,
  lDirectory : string;
begin
  Result := False;
  try
    if (MessageDlg(COverriteFilesMsg,mtConfirmation,mbOKCancel,0) <> mrOk) then
      Exit;

    lDirectory := FAppModules.ViewIni.ReadString('SysConfigDataFiles','FileDirectory','');
    if not SysUtils.DirectoryExists(lDirectory) then
      lDirectory := '';
    if SelectDirectory(lDirectory, [sdAllowCreate,sdPerformCreate], 0) then
    begin
      FAppModules.ViewIni.WriteString('SysConfigDataFiles','FileDirectory',lDirectory);

      LPath := FileNamesObject.InputFilesPath;
      FileNamesObject.InputFilesPath := lDirectory;
      try
        CreateDataFileObject;
        try
          FDataFileObjects.Initialise;
          FExtraSteps     := 0;
          FFileAction     := fatExportSingle;
          FProgressDialog.Initialise;
          FProgressDialog.clbOptions.Items.Add('Stop on first error');
          FProgressDialog.clbOptions.Checked[0] := False;// FAppModules.GlobalData.StopOnFirstErr;
          FGlobalDataIndex.Reset;
          FGlobalDataIndex.StopOnFirstErrIndex        := 0;

          if CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then
          begin
            SetTotalFiles((ModelData.FileNamesObject.SelectedCount * 2)  + FExtraSteps);
            FProgressDialog.AddExecutionFunctions(ExecLoadDataFromDatabase);
            FProgressDialog.AddExecutionFunctions(ExecSaveDataToFiles);
          end;
          FileNamesObject.SelectAllFiles(False);
          FileNamesObject.CastConfigFileNames.SetAllSelected(True);
          FileNamesObject.CastDirectoryFileNames.SetAllSelected(True);
          AddExtraButtons;
          FProgressDialog.Caption := AppModules.Language.GetString('TFilesActionAbstractManager.strBatchWrite');
          FProgressDialog.Succsessful := False;
          FProgressDialog.ShowModal;
          Result := FProgressDialog.Succsessful;
          FProgressDialog.Hide;
        finally
          DestroyDataFileObject;
        end;
      finally
        FileNamesObject.InputFilesPath := LPath;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionAbstractManager.AddExtraButtons;
const OPNAME = 'TFilesActionAbstractManager.AddExtraButtons';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionAbstractManager.OnItemCheckUncheck(Sender: TObject);
const OPNAME = 'TFilesActionAbstractManager.OnItemCheckUncheck';
begin
  try
    if(FGlobalDataIndex.StopOnFirstErrIndex >= 0) then
      FAppModules.GlobalData.SetStopOnFirstErr(FProgressDialog.clbOptions.Checked[FGlobalDataIndex.StopOnFirstErrIndex]);

    if(FGlobalDataIndex.IncludeHydrologyFilesIndex >= 0) then
      FAppModules.GlobalData.SetIncludeHydrologyFiles(FProgressDialog.clbOptions.Checked[FGlobalDataIndex.IncludeHydrologyFilesIndex]);

    if(FGlobalDataIndex.IncludeDemandFilesIndex >= 0) then
      FAppModules.GlobalData.SetIncludeDemandFiles(FProgressDialog.clbOptions.Checked[FGlobalDataIndex.IncludeDemandFilesIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGlobalDataIndex }

procedure TGlobalDataIndex.Reset;
const OPNAME = 'TGlobalDataIndex.Reset';
begin
  try
    FStopOnFirstErrIndex        := -1;
    FIncludeHydrologyFilesIndex := -1;
    FIncludeDemandFilesIndex    := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
