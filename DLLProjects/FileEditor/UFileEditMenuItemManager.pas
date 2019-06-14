//
//
//  UNIT      : Contains TFileEditMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFileEditMenuItemManager;

interface

uses
  UAbstractObject,
  UGUIConstants,
  UFileEditToolBar,
  UMenuItemManager;

type

  TFileEditMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TFileEditToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    procedure SetMenuSaveFile(AAction: TMenuSetAction);
    procedure SetMenuValidateFile(AAction: TMenuSetAction);
    procedure SetMenuImportFile(AAction: TMenuSetAction);
    procedure SetMenuExportFile(AAction: TMenuSetAction);
    function Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    property  ToolBar: TFileEditToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UAbstractComponent,
  UMainMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

const
  // Model menu items.
  CSaveFile      : array[0..1] of string = ('File','SaveFile');
  CValidateFile  : array[0..1] of string = ('File','ValidateFile');
  CExportFile    : array[0..1] of string = ('File','ExportFile');
  CImportFile    : array[0..1] of string = ('File','ImportFile');
  CSaveFileSep   : array[0..1] of string = ('File','SaveFileSep');

{ TFileEditMenuItemManager }

procedure TFileEditMenuItemManager.AddMenuItems;
const OPNAME = 'TFileEditMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CSaveFileSep,  310, CmeSeparator);
    AddMenuItemEntry(CSaveFile,     311, CmeCustomModelEvent, TModelMenuData.Create(meSaveFile));
    AddMenuItemEntry(CValidateFile, 312, CmeValidateFile,nil);
    AddMenuItemEntry(CImportFile,   313, CmeImportFile,nil);
    AddMenuItemEntry(CExportFile,   314, CmeExportFile,nil);
    Disable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.CreateMemberObjects;
const OPNAME = 'TFileEditMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TFileEditToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TFileEditMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function TFileEditMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TFileEditMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    DisableAllMenus;
    Result := Result and FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TFileEditMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.SetMenuSaveFile( AAction: TMenuSetAction);
const OPNAME = 'TFileEditMenuItemManager.SetMenuSaveFile';
begin
  try
    FToolBar.SetSaveFileState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CSaveFile, AAction, 'SaveFileDisabled');
    else
      FAppModules.SetMenuItem(CSaveFile, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.SetMenuExportFile(AAction: TMenuSetAction);
const OPNAME = 'TFileEditMenuItemManager.SetMenuExportFile';
begin
  try
    FToolBar.SetExportFileState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CExportFile, AAction, 'ExportFileDisabled');
    else
      FAppModules.SetMenuItem(CExportFile, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.SetMenuImportFile(AAction: TMenuSetAction);
const OPNAME = 'TFileEditMenuItemManager.SetMenuImportFile';
begin
  try
    if (AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;

    FToolBar.SetImportFileState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CImportFile, AAction, 'ImportFileDisabled');
    else
      FAppModules.SetMenuItem(CImportFile, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.SetMenuValidateFile(AAction: TMenuSetAction);
const OPNAME = 'TFileEditMenuItemManager.SetMenuValidateFile';
begin
  try
    FToolBar.SetValidateFileState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CValidateFile, AAction, 'ValidateFileDisabled');
    else
      FAppModules.SetMenuItem(CValidateFile, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditMenuItemManager.Initialise: boolean;
const OPNAME = 'TFileEditMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    DisableAllMenus;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditMenuItemManager.DisableAllMenus;
const OPNAME = 'TFileEditMenuItemManager.DisableAllMenus';
begin
  try
    SetMenuSaveFile(msDisable);
    SetMenuValidateFile(msDisable);
    SetMenuImportFile(msDisable);
    SetMenuExportFile(msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
