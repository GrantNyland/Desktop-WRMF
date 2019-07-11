
//
//
//  UNIT      : Contains TDDTSMenuItemManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDDTSMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  UDDTSMenuItemToolBar,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  TDDTSMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TDDTSMenuItemToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    procedure Show; override;
    procedure TabHasChanged (AGridTabSelected: boolean);

    procedure SetMenuValidateFiles(AAction: TMenuSetAction);
    procedure SetMenuImportFiles(AAction: TMenuSetAction);
    procedure SetMenuExportFiles(AAction: TMenuSetAction);
    procedure SetMenuClearModelData(AAction: TMenuSetAction);
    procedure SetMenuValidateModelData(AAction: TMenuSetAction);
    procedure SetMenuRunModel(AAction: TMenuSetAction);

    procedure TreeNodeHasChanged (ADataType : string;AElementID : integer);
    property ToolBar: TDDTSMenuItemToolBar read FToolBar;



  end;

implementation

uses
  SysUtils,
  UConstants,
  UMainMenuEventType,
  VoaimsCom_TLB,
  UDDTSDataObject,
  UErrorHandlingOperations;

const
 // CDDTSData                        : array[0..0] of WideString = ('Data');
  //CCreateDDTSDam                   : array[0..1] of WideString = ('Data','CreateDDTSDam');

  CModel                        : array[0..0] of WideString = ('Model');
  CValidateFiles                : array[0..1] of WideString = ('Model','ValidateFiles');
  CExportFiles                  : array[0..1] of WideString = ('Model','ExportFiles');
  CImportFiles                  : array[0..1] of WideString = ('Model','ImportFiles');
  CClearModelData               : array[0..1] of WideString = ('Model','ClearModelData');
  CValidateModelData            : array[0..1] of WideString = ('Model','ValidateModelData');
  CGenerateSysConfigFiles       : array[0..1] of WideString = ('Model','GenerateSystemConfigDataFiles');
  CRunModel                     : array[0..1] of WideString = ('Model','RunModel');

procedure TDDTSMenuItemManager.AddMenuItems;
const OPNAME = 'TDDTSMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    //AddMenuItemEntry(CDDTSData,  800);
    //AddMenuItemEntry(CCreateDDTSDam,     820, cmeCreateDDTSDam);

    AddMenuItemEntry(CModel,            600);
    AddMenuItemEntry(CValidateFiles,    610, CmeValidateFiles,nil);
    AddMenuItemEntry(CImportFiles,      620, CmeImportFiles,nil);
    AddMenuItemEntry(CExportFiles,      630, CmeExportFiles,nil);
    AddMenuItemEntry(CClearModelData,   640, CmeClearModelData,nil);
    AddMenuItemEntry(CValidateModelData,650, CmeValidateModelData);
    AddMenuItemEntry(CRunModel,         655, CmeRunModel);

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSMenuItemManager.CreateMemberObjects;
const OPNAME = 'TDDTSMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TDDTSMenuItemToolBar.Create(nil, FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TDDTSMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.DisableAllMenus;
const OPNAME = 'TDDTSMenuItemManager.DisableAllMenus';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSMenuItemManager.Initialise: boolean;
const OPNAME = 'TDDTSMenuItemManager.Initialise';
begin
  Result := false;
  try
    SetMenuValidateFiles(msDisable);
    SetMenuImportFiles(msDisable);
    SetMenuExportFiles(msDisable);
    SetMenuClearModelData(msDisable);
    SetMenuValidateModelData(msDisable);
    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.Show;
const OPNAME = 'TDDTSMenuItemManager.Show';
begin
  try
    inherited Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TDDTSMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try

    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TDDTSMenuItemManager.TabHasChanged';
begin
  try
    FToolBar.TabHasChanged(AGridTabSelected);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSMenuItemManager.TreeNodeHasChanged(ADataType: string; AElementID: integer);
const OPNAME = 'TDDTSMenuItemManager.TreeNodeHasChanged';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSMenuItemManager.SetMenuValidateModelData(AAction: TMenuSetAction);
const OPNAME = 'TDDTSMenuItemManager.SetMenuValidateModelData';
begin
  try
    FToolBar.SetValidateModelDataClickState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CValidateModelData, AAction, 'ValidateModelDataDisabled');
    else
      FAppModules.SetMenuItem(CValidateModelData, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.SetMenuExportFiles(AAction: TMenuSetAction);
const OPNAME = 'TDDTSMenuItemManager.SetMenuExportFiles';
begin
  try
    FToolBar.SetExportFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CExportFiles, AAction, 'ExportFilesDisabled');
    else
      FAppModules.SetMenuItem(CExportFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.SetMenuImportFiles(AAction: TMenuSetAction);
const OPNAME = 'TDDTSMenuItemManager.SetMenuImportFiles';
begin
  try
    if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;

    FToolBar.SetImportFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CImportFiles, AAction, 'ImportFilesDisabled');
    else
      FAppModules.SetMenuItem(CImportFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.SetMenuValidateFiles(AAction: TMenuSetAction);
const OPNAME = 'TDDTSMenuItemManager.SetMenuValidateFiles';
begin
  try
    FToolBar.SetValidateFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CValidateFiles, AAction, 'ValidateFilesDisabled');
    else
      FAppModules.SetMenuItem(CValidateFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.SetMenuClearModelData(AAction: TMenuSetAction);
const OPNAME = 'TDDTSMenuItemManager.SetMenuClearModelData';
begin
  try
    if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetClearModelDataState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CClearModelData, AAction, 'ClearFilesDisabled');
    else
      FAppModules.SetMenuItem(CClearModelData, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemManager.SetMenuRunModel(AAction: TMenuSetAction);
const OPNAME = 'TDDTSMenuItemManager.SetMenuRunModel';
begin
  try
    FToolBar.SetRunModelState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CRunModel, AAction, 'RunModelDisabled');
    else
      FAppModules.SetMenuItem(CRunModel, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

