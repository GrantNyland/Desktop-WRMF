{******************************************************************************}
{* UNIT : Contains the class UHydroNVMenuItemManager.                         *}
{*        (Network Visualiser for Hydrology model)                            *}
{******************************************************************************}

unit UHydroNVMenuItemManager;

interface

uses
  UAbstractObject,
  UGUIConstants,
  UHydroNVToolBar,
  UMenuItemManager;

type

  THydroNVMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : THydroNVToolbar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetMenuItems(AnAction: TMenuSetAction; AStatusReason: string = ''); override;
  public
    procedure DisableAllMenus;
    procedure AddMenuItems; override;
    procedure SetMenuNewDrawing(AAction: TMenuSetAction);
    procedure SetMenuDeleteDrawing(AAction: TMenuSetAction);
    procedure SetMenuRenameDrawing(AAction: TMenuSetAction);
    procedure SetMenuEditDrawing(AAction: TMenuSetAction);
    procedure SetMenuViewDrawing(AAction: TMenuSetAction);
    procedure SetMenuCopyDrawing(AAction: TMenuSetAction);

    function Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    property  ToolBar: THydroNVToolbar read FToolBar;
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
  CDrawingSep         : array[0..1] of string = ('Edit','VNVDrawingSep');
  CNewDrawing         : array[0..1] of string = ('Edit','VNVNewDrawing');
  CDeleteDrawing      : array[0..1] of string = ('Edit','VNVDeleteDrawing');
  CRenameDrawing      : array[0..1] of string = ('Edit','VNVRenameDrawing');
  CViewDrawing        : array[0..1] of string = ('Edit','VNVViewDrawing');
  CEditDrawing        : array[0..1] of string = ('Edit','VNVEditDrawing');
  CCopyDrawing        : array[0..1] of string = ('Edit','VNVCopyDrawing');

{* THydroNVMenuItemManager ****************************************************}

procedure THydroNVMenuItemManager.AddMenuItems;
const OPNAME = 'THydroNVMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CDrawingSep,    300);
    AddMenuItemEntry(CNewDrawing,    304, CmeCustomModelEvent, TModelMenuData.Create(meHydroNVNewDrawing));
    AddMenuItemEntry(CDeleteDrawing, 305, CmeCustomModelEvent, TModelMenuData.Create(meHydroNVDeleteDrawing));
    AddMenuItemEntry(CRenameDrawing, 306, CmeCustomModelEvent, TModelMenuData.Create(meHydroNVRenameDrawing));
    AddMenuItemEntry(CViewDrawing,   307, CmeCustomModelEvent, TModelMenuData.Create(meHydroNVViewDrawing));
    AddMenuItemEntry(CEditDrawing,   308, CmeCustomModelEvent, TModelMenuData.Create(meHydroNVEditDrawing));
    AddMenuItemEntry(CCopyDrawing,   309, CmeCustomModelEvent, TModelMenuData.Create(meHydroNVCopyDrawing));
    Hide;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.CreateMemberObjects;
const OPNAME = 'THydroNVMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := THydroNVToolbar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.DestroyMemberObjects;
const OPNAME = 'THydroNVMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function THydroNVMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'THydroNVMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    DisableAllMenus;
    Result := Result and FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'THydroNVMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuDeleteDrawing(AAction: TMenuSetAction);
const OPNAME = 'THydroNVMenuItemManager.SetMenuDeleteDrawing';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetDeleteDrawingState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CDeleteDrawing, AAction, 'VNVDeleteDrawingDisabled');
    else
      FAppModules.SetMenuItem(CDeleteDrawing, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuRenameDrawing(AAction: TMenuSetAction);
const OPNAME = 'THydroNVMenuItemManager.SetMenuRenameDrawing';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetRenameDrawingState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CRenameDrawing, AAction, 'VNVRenameDrawingDisabled');
    else
      FAppModules.SetMenuItem(CRenameDrawing, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuNewDrawing(AAction: TMenuSetAction);
const OPNAME = 'THydroNVMenuItemManager.SetMenuNewDrawing';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetNewDrawingState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CNewDrawing, AAction, 'VNVNewDrawingDisabled');
    else
      FAppModules.SetMenuItem(CNewDrawing, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuEditDrawing(AAction: TMenuSetAction);
const OPNAME = 'THydroNVMenuItemManager.SetMenuEditDrawing';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetEditDrawingState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CEditDrawing, AAction, 'VNVEditDrawingDisabled');
    else
      FAppModules.SetMenuItem(CEditDrawing, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuViewDrawing(AAction: TMenuSetAction);
const OPNAME = 'THydroNVMenuItemManager.SetMenuViewDrawing';
begin
  try
    FToolBar.SetViewDrawingState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CViewDrawing, AAction, 'VNVViewDrawingDisabled');
    else
      FAppModules.SetMenuItem(CViewDrawing, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuCopyDrawing(AAction: TMenuSetAction);
const OPNAME = 'THydroNVMenuItemManager.SetMenuCopyDrawing';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetCopyDrawingState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CCopyDrawing, AAction, 'VNVCopyDrawingDisabled');
    else
      FAppModules.SetMenuItem(CCopyDrawing, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVMenuItemManager.Initialise: boolean;
const OPNAME = 'THydroNVMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    DisableAllMenus;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.DisableAllMenus;
const OPNAME = 'THydroNVMenuItemManager.DisableAllMenus';
begin
  try
    SetMenuNewDrawing(msDisable);
    SetMenuDeleteDrawing(msDisable);
    SetMenuRenameDrawing(msDisable);
    SetMenuEditDrawing(msDisable);
    SetMenuViewDrawing(msDisable);
    SetMenuCopyDrawing(msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVMenuItemManager.SetMenuItems(AnAction: TMenuSetAction; AStatusReason: string = '');
const OPNAME = 'THydroNVMenuItemManager.SetMenuItems';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
