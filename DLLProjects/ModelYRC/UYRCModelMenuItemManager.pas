//
//
//  UNIT      : Contains TYRCModelMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCModelMenuItemManager;

interface

uses
  UAbstractObject,
  UMenuItemManager,
  UYRCModelToolBar,
  UYRCModelLinkClasses,
  UDataModelMenuItemManager;

type
  TYRCModelMenuItemManager = class(TDataModelMenuItemManager)
  protected
    FIsResultsLoaded: boolean;
    procedure CreateToolBar;override;
    procedure DestroyMemberObjects; override;
  public
    FToolBar: TYRCModelToolBar;
    constructor Create(AAppModules: TAppModules;
      AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded, AIsResultsLoaded: boolean); reintroduce;
    procedure AddMenuItems; override;
    procedure SetMenuValidateFiles(AAction: TMenuSetAction);
    procedure SetMenuImportFiles(AAction: TMenuSetAction);
    procedure SetMenuExportFiles(AAction: TMenuSetAction);
    procedure SetMenuClearModelData(AAction: TMenuSetAction);
    procedure SetMenuRunModel(AAction: TMenuSetAction);

    function Initialise: Boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    property ToolBar: TYRCModelToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CModel:                  array[0..0] of string = ('Model');
  CValidateFiles:          array[0..1] of string = ('Model','ValidateFiles');
  CExportFiles:            array[0..1] of string = ('Model','ExportFiles');
  CImportFiles:            array[0..1] of string = ('Model','ImportFiles');
  CClearModelData:         array[0..1] of string = ('Model','ClearModelData');
  CRunModel:               array[0..1] of string = ('Model','RunModel');

  CViewResults:      array[0..1] of string = ('View','ViewResults');
  CViewTabSheetsSep: array[0..1] of string = ('View','ViewTabSheetsSep');

{ TYRCModelMenuItemManager }

constructor TYRCModelMenuItemManager.Create(AAppModules: TAppModules;
  AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded, AIsResultsLoaded: boolean);
const OPNAME = 'TYRCModelMenuItemManager.Create';
begin
  try
    FIsResultsLoaded := AIsResultsLoaded;
    inherited Create(AAppModules, AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.AddMenuItems;
const OPNAME = 'TYRCModelMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CModel,          600);
    AddMenuItemEntry(CValidateFiles,  610, CmeValidateFiles,nil);
    AddMenuItemEntry(CImportFiles,    620, CmeImportFiles,nil);
    AddMenuItemEntry(CExportFiles,    630, CmeExportFiles,nil);
    AddMenuItemEntry(CClearModelData, 640, CmeClearModelData,nil);
    AddMenuItemEntry(CRunModel,       650, CmeRunModel);

    if FIsResultsLoaded then
      AddMenuItemEntry(CViewResults, 240, CmeViewResultGraphs);
    if FIsResultsLoaded then
      if (not (FIsNetworkLoaded or FIsGridLoaded or FIsGraphLoaded or FIsFileSelectionLoaded)) then
        AddMenuItemEntry(CViewTabSheetsSep, 250, CmeSeparator);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TYRCModelMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  try
    if Assigned(FAppModules.MainForm()) then
      FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar)
    else
    FToolBar.Parent := nil;

    FreeAndNil(FToolBar);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelMenuItemManager.Initialise: Boolean;
const OPNAME = 'TYRCModelMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetMenuValidateFiles(msDisable);
    SetMenuImportFiles(msDisable);
    SetMenuExportFiles(msDisable);
    SetMenuClearModelData(msDisable);
    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.CreateToolBar;
const OPNAME = 'TYRCModelMenuItemManager.CreateToolBar';
begin
  try
    FToolBar := TYRCModelToolBar.Create(nil,FAppModules);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TYRCModelMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TYRCModelMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.SetMenuExportFiles(AAction: TMenuSetAction);
const OPNAME = 'TYRCModelMenuItemManager.SetMenuExportFiles';
begin
  try
    if (FAppModules.User.UserRights in [2,3]) then
    begin
      FToolBar.SetExportFilesState(AAction = msEnable);
      case AAction of
        msDisable : FAppModules.SetMenuItem(CExportFiles, AAction, 'ExportFilesDisabled');
      else
        FAppModules.SetMenuItem(CExportFiles, AAction);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.SetMenuImportFiles(AAction: TMenuSetAction);
const OPNAME = 'TYRCModelMenuItemManager.SetMenuImportFiles';
begin
  try
    if (FAppModules.User.UserRights in [1,3]) then
    begin
      FToolBar.SetImportFilesState(AAction = msEnable);
      case AAction of
        msDisable : FAppModules.SetMenuItem(CImportFiles, AAction, 'ImportFilesDisabled');
      else
        FAppModules.SetMenuItem(CImportFiles, AAction);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.SetMenuValidateFiles(AAction: TMenuSetAction);
const OPNAME = 'TYRCModelMenuItemManager.SetMenuValidateFiles';
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

procedure TYRCModelMenuItemManager.SetMenuClearModelData(AAction: TMenuSetAction);
const OPNAME = 'TYRCModelMenuItemManager.SetMenuClearModelData';
begin
  try
    FToolBar.SetClearModelDataState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CClearModelData, AAction, 'ClearFilesDisabled');
    else
      FAppModules.SetMenuItem(CClearModelData, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelMenuItemManager.SetMenuRunModel(AAction: TMenuSetAction);
const OPNAME = 'TYRCModelMenuItemManager.SetMenuRunModel';
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
