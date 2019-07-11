
//
//
//  UNIT      : Contains TStomsaMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UStomsaMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  UStomsaMenuItemToolBar,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  TStomsaMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TStomsaMenuItemToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    procedure SetStomsaFileNew(AEnabled: boolean);
    procedure SetStomsaFileOpen(AEnabled: boolean);
    procedure SetStomsaFileOpenParam(AEnabled: boolean);
    procedure SetStomsaFileSave(AEnabled: boolean);
    procedure SetStomsaFileSaveAs(AEnabled: boolean);
    procedure SetStomsaFileSaveANS(AEnabled: boolean);
    procedure SetStomsaFileMerge(AEnabled: boolean);
    procedure SetStomsaFileClose(AEnabled: boolean);
    procedure SetStomsaFileImport(AEnabled: boolean);
    procedure SetStomsaFileExport(AEnabled: boolean);

    property ToolBar: TStomsaMenuItemToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CStomsaData                : array[0..0] of WideString = ('StomsaData');
  CStomsaFileNew             : array[0..1] of WideString = ('StomsaData','StomsaFileNew');
  CStomsaFileOpen            : array[0..1] of WideString = ('StomsaData','StomsaFileOpen');
  CStomsaFileOpenParam       : array[0..1] of WideString = ('StomsaData','StomsaFileOpenParam');
  CStomsaSeparator1          : array[0..1] of WideString = ('StomsaData','StomsaSeparator1');
  CStomsaFileSave            : array[0..1] of WideString = ('StomsaData','StomsaFileSave');
  CStomsaFileSaveAs          : array[0..1] of WideString = ('StomsaData','StomsaFileSaveAs');
  CStomsaFileSaveANS         : array[0..1] of WideString = ('StomsaData','StomsaFileSaveANS');
  CStomsaSeparator2          : array[0..1] of WideString = ('StomsaData','StomsaSeparator2');
  CStomsaFileMerge           : array[0..1] of WideString = ('StomsaData','StomsaFileMerge');
  CStomsaFileClose           : array[0..1] of WideString = ('StomsaData','StomsaFileClose');
  CStomsaSeparator3          : array[0..1] of WideString = ('StomsaData','StomsaSeparator3');
  CStomsaFileExport          : array[0..1] of WideString = ('StomsaData','StomsaFileExport');
  CStomsaFileImport          : array[0..1] of WideString = ('StomsaData','StomsaFileImport');
  CHelpStomsaUserGuide       : array[0..1] of WideString = ('Help','HelpStomsaUserGuide');

procedure TStomsaMenuItemManager.AddMenuItems;
const OPNAME = 'TStomsaMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CStomsaData, 100);
    AddMenuItemEntry(CStomsaFileNew, 110, CmeStomsaFileNew);
    AddMenuItemEntry(CStomsaFileOpen, 120, CmeStomsaFileOpen);
    AddMenuItemEntry(CStomsaFileOpenParam, 130, CmeStomsaFileOpenParam);
    AddMenuItemEntry(CStomsaSeparator1, 140, CmeSeparator);
    AddMenuItemEntry(CStomsaFileSave, 150, CmeStomsaFileSave);
    AddMenuItemEntry(CStomsaFileSaveAs, 160, CmeStomsaFileSaveAs);
    AddMenuItemEntry(CStomsaFileSaveANS, 170, CmeStomsaFileSaveANS);
    AddMenuItemEntry(CStomsaSeparator2, 180, CmeSeparator);
    AddMenuItemEntry(CStomsaFileMerge, 190, CmeStomsaFileMerge);
    AddMenuItemEntry(CStomsaFileClose, 200, CmeStomsaFileClose);
    AddMenuItemEntry(CStomsaSeparator3, 210, CmeSeparator);
    AddMenuItemEntry(CStomsaFileExport, 220, CmeStomsaFileExport);
    AddMenuItemEntry(CStomsaFileImport, 230, CmeStomsaFileImport);

    AddMenuItemEntry(CHelpStomsaUserGuide, 230, CmeSTOMSAUserGuide);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TStomsaMenuItemManager.CreateMemberObjects;
const OPNAME = 'TStomsaMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TStomsaMenuItemToolBar.Create(nil, FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TStomsaMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.DisableAllMenus;
const OPNAME = 'TStomsaMenuItemManager.DisableAllMenus';
begin
  try
    SetStomsaFileNew(False);
    SetStomsaFileOpen(False);
    SetStomsaFileOpenParam(False);
    SetStomsaFileSave(False);
    SetStomsaFileSaveAs(False);
    SetStomsaFileSaveANS(False);
    SetStomsaFileMerge(False);
    SetStomsaFileClose(False);
    SetStomsaFileExport(False);
    SetStomsaFileImport(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaMenuItemManager.Initialise: boolean;
const OPNAME = 'TStomsaMenuItemManager.Initialise';
begin
  Result := false;
  try
    DisableAllMenus;
    SetStomsaFileNew(True);
    SetStomsaFileOpen(True);
    SetStomsaFileOpenParam(True);
    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TStomsaMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStomsaMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TStomsaMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try
    FToolBar.StudyHasChanged;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileNew(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileNew';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileNew, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileNew, msDisable, 'StomsaFileNewDisabled');
    FToolBar.SetStomsaFileNew(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileOpen(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileOpen';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileOpen, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileOpen, msDisable, 'StomsaFileOpenDisabled');
    FToolBar.SetStomsaFileOpen(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileSave(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileSave';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileSave, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileSave, msDisable, 'StomsaFileSaveDisabled');
    FToolBar.SetStomsaFileSave(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileClose(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileClose';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileClose, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileClose, msDisable, 'StomsaFileCloseDisabled');
    FToolBar.SetStomsaFileClose(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileMerge(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileMerge';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileMerge, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileMerge, msDisable, 'StomsaFileMergeDisabled');
    FToolBar.SetStomsaFileMerge(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileOpenParam(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileOpenParam';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileOpenParam, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileOpenParam, msDisable, 'StomsaFileOpenParamDisabled');
    FToolBar.SetStomsaFileOpenParam(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileSaveANS(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileSaveANS';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileSaveANS, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileSaveANS, msDisable, 'StomsaFileSaveANSDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileSaveAs(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileSaveAs';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileExport, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileExport, msDisable, 'StomsaFileSaveAsDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileExport(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileExport';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileExport, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileExport, msDisable, 'StomsaFileExportDisabled');
    FToolBar.SetStomsaFileExport(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemManager.SetStomsaFileImport(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemManager.SetStomsaFileImport';
begin
  try
    AEnabled :=  AEnabled and (FAppModules.User.UserRights in CUR_EditData) and (not FAppModules.StudyArea.ScenarioLocked);
    if AEnabled then
      FAppModules.SetMenuItem(CStomsaFileImport, msEnable)
    else
      FAppModules.SetMenuItem(CStomsaFileImport, msDisable, 'StomsaFileImportDisabled');
    FToolBar.SetStomsaFileImport(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

