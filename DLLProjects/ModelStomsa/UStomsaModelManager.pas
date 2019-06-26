//
//
//  UNIT      : Contains TStomsaModelManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 13/06/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UStomsaModelManager;

interface

uses
  UDataViewerManager,
  UFilesActionAbstractManager,
  UFileSelectionManager,
  UStomsaDLLManager,
  UStomsaGUIManager,
  UStomsaMenuItemManager,
  UStomsaModelDataObject,
  USystemModelManager;

type
  TStomsaModelManager = class(TSystemModelManager)
  protected
    FModelData                  : TStomsaModelDataObject;
    FDLLManager                 : TStomsaDLLManager;
    FMenuItemManager            : TStomsaMenuItemManager;
    FModelGUIManager            : TStomsaGUIManager;
    FFileEditManager            : TAbstractFileEditManager;
    FFileSelectionManager       : TFileSelectionManager;
    FModelFilesActionManager    : TFilesActionAbstractManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FileNew;
    procedure FileOpen;
    procedure FileOpenParam;
    procedure FileSave;
    procedure FileSaveAs;
    procedure FileSaveANS;
    procedure FileMerge;
    procedure FileClose;
    procedure FilesExport;
    procedure FilesImport;
    procedure SaveData;
    procedure LoadData;
    procedure OnFilesAdded;
    procedure OnFilesRemoved;
    procedure OnRefreshFileHints;
    procedure DoLaunchStomsaUserGuide;
    procedure CreateFileEditManager; virtual;
    procedure CreateFileSelectionManager;virtual;
    procedure CreateFileActionManager; virtual;
    function SelectModelFileNames: boolean; virtual;
    function DisplayModelFileNames: boolean; virtual;
    function LoadModelData: boolean; virtual;
    procedure RefreshMenuItems; override;
  public
    procedure OnTabChangeRequest(ASender: TObject; var AAllowChange: Boolean); override;
    procedure OnTabHasChanged(ASender: TObject); override;
    function ModelName: string; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: Boolean; override;
    function StudyHasChanged: Boolean; override;
    function ModelData: TInterfacedObject; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
    property DLLManager: TStomsaDLLManager read FDLLManager;
  end;

implementation

uses
  SysUtils,
  VCL.Forms,
  Dialogs,
  UAbstractObject,
  UDataModule,
  UStomsaDataLoadAgent,
  UStomsaMainForm,
  UTreeViewTabSheet,
  UMainMenuEventType,
  UStomsaFileSelectionManager,
  UFilesActionStomsaManager,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UErrorHandlingOperations;

procedure TStomsaModelManager.CreateMemberObjects;
const OPNAME = 'TStomsaModelManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FModelData := TStomsaModelDataObject.Create(FAppModules);
    FDLLManager := TStomsaDLLManager.Create;
    if (not FAppModules.GlobalData.COMServerMode) then
    begin
      FMenuItemManager := TStomsaMenuItemManager.Create(FAppModules);
      FModelGUIManager := TStomsaGUIManager.Create(FAppModules);
      CreateFileEditManager;
    end;
    CreateFileSelectionManager;
    CreateFileActionManager;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStomsaModelManager.DestroyMemberObjects;
const OPNAME = 'TStomsaModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FModelData);
    FreeAndNil(FDLLManager);
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FModelGUIManager);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaModelManager.Initialise: boolean;
const OPNAME = 'TStomsaModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FDLLManager) then
      Result := Result and FDLLManager.LoadAll;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.Initialise;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.Initialise;
    if Assigned(FFileEditManager) then
      FFileEditManager.Initialise;
    if Assigned(FFileSelectionManager) then
      FFileSelectionManager.Initialise;

    LoadModelData;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaModelManager.LanguageHasChanged: Boolean;
const OPNAME = 'TStomsaModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.LanguageHasChanged;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.LanguageHasChanged;
    if Assigned(FFileEditManager) then
      FFileEditManager.LanguageHasChanged;
    if Assigned(FFileSelectionManager) then
      FFileSelectionManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaModelManager.StudyHasChanged: Boolean;
const OPNAME = 'TStomsaModelManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.StudyHasChanged;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyHasChanged;
    if Assigned(FFileEditManager) then
      FFileEditManager.StudyHasChanged;
    if Assigned(FFileSelectionManager) then
      FFileSelectionManager.StudyHasChanged;

    LoadModelData;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TStomsaModelManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
//    Result := fmData.DataStorage;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaModelManager.ModelName: string;
const OPNAME = 'TStomsaModelManager.ModelName';
begin
  Result := '';
  try
    Result := CStomsa;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.CreateFileEditManager;
const OPNAME = 'TStomsaModelManager.CreateFileEditManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDLLObject(ExtractFilePath(Application.ExeName) + 'bin\FileEditor.dll',
      TAbstractAppObject(FFileEditManager), FAppModules, False, OPNAME);
    if Assigned(FFileEditManager) then
    begin
      FOwnedAppObjects.Add(FFileEditManager);
      FFileEditManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.CreateFileSelectionManager;
const OPNAME = 'TStomsaModelManager.CreateFileSelectionManager';
begin
  try
    FFileSelectionManager := TStomsaFileSelectionManager.Create(FAppModules);
    if Assigned(FFileSelectionManager) then
    begin
      FOwnedAppObjects.Add(FFileSelectionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStomsaModelManager.CreateFileActionManager;
const OPNAME = 'TStomsaModelManager.CreateFileActionManager';
begin
  try
    FModelFilesActionManager := TFilesActionStomsaManager.Create(FAppModules);
    if Assigned(FModelFilesActionManager) then
    begin
      FOwnedAppObjects.Add(FModelFilesActionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TStomsaModelManager.ProcessEvent';
begin
  Result := True;
  try
    case AEventType of
      CmeStomsaFileNew        : FileNew;
      CmeStomsaFileOpen       : FileOpen;
      CmeStomsaFileOpenParam  : FileOpenParam;
      CmeStomsaFileSave       : FileSave;
      CmeStomsaFileSaveAs     : FileSaveAs;
      CmeStomsaFileSaveANS    : FileSaveANS;
      CmeStomsaFileMerge      : FileMerge;
      CmeStomsaFileClose      : FileClose;
      CmeStomsaFileExport     : FilesExport;
      CmeStomsaFileImport     : FilesImport;
      CmeStomsaSaveData       : SaveData;
      CmeStomsaFilesAdded     : OnFilesAdded;
      CmeStomsaFilesRemoved   : OnFilesRemoved;
      CmeRefreshFileHints     : OnRefreshFileHints;
      CmeSTOMSAUserGuide      : DoLaunchStomsaUserGuide;
    else
     Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaModelManager.FileClose;
const OPNAME = 'TStomsaModelManager.FileClose';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileClose then
    begin
      FMenuItemManager.SetStomsaFileNew(True);
      FMenuItemManager.SetStomsaFileOpen(True);
      FMenuItemManager.SetStomsaFileOpenParam(True);
      FMenuItemManager.SetStomsaFileSave(False);
      FMenuItemManager.SetStomsaFileSaveAs(False);
      FMenuItemManager.SetStomsaFileSaveANS(False);
      FMenuItemManager.SetStomsaFileMerge(False);
      FMenuItemManager.SetStomsaFileClose(False);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileMerge;
const OPNAME = 'TStomsaModelManager.FileMerge';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileMerge then
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileNew;
const OPNAME = 'TStomsaModelManager.FileNew';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileNew then
    begin
      FMenuItemManager.SetStomsaFileNew(False);
      FMenuItemManager.SetStomsaFileOpen(False);
      FMenuItemManager.SetStomsaFileOpenParam(False);
      FMenuItemManager.SetStomsaFileSave(True);
      FMenuItemManager.SetStomsaFileSaveAs(True);
      FMenuItemManager.SetStomsaFileSaveANS(True);
      FMenuItemManager.SetStomsaFileMerge(True);
      FMenuItemManager.SetStomsaFileClose(True);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileOpen;
const OPNAME = 'TStomsaModelManager.FileOpen';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileOpen then
    begin
      FMenuItemManager.SetStomsaFileNew(False);
      FMenuItemManager.SetStomsaFileOpen(False);
      FMenuItemManager.SetStomsaFileOpenParam(False);
      FMenuItemManager.SetStomsaFileSave(True);
      FMenuItemManager.SetStomsaFileSaveAs(True);
      FMenuItemManager.SetStomsaFileSaveANS(True);
      FMenuItemManager.SetStomsaFileMerge(True);
      FMenuItemManager.SetStomsaFileClose(True);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileOpenParam;
const OPNAME = 'TStomsaModelManager.FileOpenParam';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileOpenParam then
    begin
      FMenuItemManager.SetStomsaFileNew(False);
      FMenuItemManager.SetStomsaFileOpen(False);
      FMenuItemManager.SetStomsaFileOpenParam(True);
      FMenuItemManager.SetStomsaFileSave(False);
      FMenuItemManager.SetStomsaFileSaveAs(False);
      FMenuItemManager.SetStomsaFileSaveANS(False);
      FMenuItemManager.SetStomsaFileMerge(False);
      FMenuItemManager.SetStomsaFileClose(True);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileSave;
const OPNAME = 'TStomsaModelManager.FileSave';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileSave then
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileSaveANS;
const OPNAME = 'TStomsaModelManager.FileSaveANS';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileSaveANS then
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FileSaveAs;
const OPNAME = 'TStomsaModelManager.FileSaveAs';
begin
  try
    if(fmStomsaMainForm <> nil) and fmStomsaMainForm.OnFileSaveAs then
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.OnTabChangeRequest(ASender: TObject; var AAllowChange: Boolean);
const OPNAME = 'TStomsaModelManager.OnTabChangeRequest';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TStomsaModelManager.OnTabHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaModelManager.LoadModelData: boolean;
const OPNAME = 'TStomsaModelManager.LoadModelData';
begin
  Result := False;
  try
    ShowMessage(FAppModules.StudyArea.DataFilesPath);
    if Assigned(fmData) then
    begin
      fmData.DataStorage.CastFileNamesObject.PopulateHydrologyPaths(FAppModules.StudyArea.DataFilesPath);
      Result := SelectModelFileNames;
      Result := Result and DisplayModelFileNames;
      LoadData;
      RefreshMenuItems;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaModelManager.SelectModelFileNames: boolean;
const OPNAME = 'TStomsaModelManager.SelectModelFileNames';
begin
  Result := False;
  try
    if not Assigned(FFileSelectionManager) then
    begin
      Result := True;
    end else begin
      Result := FFileSelectionManager.PopulateFileNames(nil, fmData.DataStorage.CastFileNamesObject);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaModelManager.DisplayModelFileNames: boolean;
const OPNAME = 'TStomsaModelManager.DisplayModelFileNames';
begin
  Result := False;
  try
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
       Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                 fmData.DataStorage.CastFileNamesObject);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FilesExport;
const OPNAME = 'TStomsaModelManager.FilesExport';
var
  LPath : string;
begin
  try
    LPath := FAppModules.StudyArea.DataFilesPath;
    //if SelectDirectory(LPath,[sdAllowCreate, sdPerformCreate, sdPrompt],0) then
    //begin
      fmData.DataStorage.CastFileNamesObject.PopulateHydrologyPaths(LPath);
      FAppModules.GlobalData.SetIncludeHydrologyFiles(True);
      FModelFilesActionManager.DoExportAllFiles;
    //end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.FilesImport;
const OPNAME = 'TStomsaModelManager.FilesImport';
begin
  try
    if fmData.DataStorage.CheckHydrologyFilesPaths then
    begin
      FAppModules.GlobalData.SetIncludeHydrologyFiles(True);
      FModelFilesActionManager.DoImportAllFiles;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.RefreshMenuItems;
const OPNAME = 'TStomsaModelManager.RefreshMenuItems';
begin
  inherited RefreshMenuItems;
  try
    FMenuItemManager.SetStomsaFileExport(fmData.DataStorage.FileNamesObject.FilesSavedInDatabaseCount > 0);
    FMenuItemManager.SetStomsaFileImport(fmData.DataStorage.ParamFileName <> '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.LoadData;
const OPNAME = 'TStomsaModelManager.LoadData';
var
  LLoadAgent: TStomsaDataLoadAgent;
begin
  try
    ShowMessage(OPNAME);
    LLoadAgent := TStomsaDataLoadAgent.Create(FAppModules);
    try
      LLoadAgent.LoadDataFromDB(fmData.DataStorage);
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.SaveData;
const OPNAME = 'TStomsaModelManager.SaveData';
var
  LLoadAgent: TStomsaDataLoadAgent;
begin
  try
    ShowMessage(OPNAME);
    LLoadAgent := TStomsaDataLoadAgent.Create(FAppModules);
    try
      LLoadAgent.SaveDataToDB(fmData.DataStorage);
    finally
      LLoadAgent.Free;
    end;
    DisplayModelFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.OnFilesAdded;
const OPNAME = 'TStomsaModelManager.OnFilesAdded';
begin
  try
    DisplayModelFileNames;
    LoadData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.OnFilesRemoved;
const OPNAME = 'TStomsaModelManager.OnFilesRemoved';
begin
  try
    DisplayModelFileNames;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.OnRefreshFileHints;
const OPNAME = 'TStomsaModelManager.OnRefreshFileHints';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaModelManager.DoLaunchStomsaUserGuide;
const OPNAME = 'TStomsaModelManager.DoLaunchStomsaUserGuide';
var
  LFileName : string;
begin
  try
    if IsAcrobatReaderInstalled then
    begin
      LFileName := ExtractFilePath(ApplicationExeName);
      TPDFDocumentLauncher.Launch(LFileName + 'help\STOMSA USER GUIDE_WRMF4.0_FinalDRAFT.pdf');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
