//
//
//  UNIT      : Contains TNetworkVisualiserMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/03/01
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UNetworkVisualiserMenuItemManager;

interface

uses
  UAbstractObject,
  UGUIConstants,
  UNetworkVisualiserToolBar,
  UMenuItemManager;

type

  TNetworkVisualiserMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TNetworkVisualiserToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
//    procedure SetMenuItems(AnAction: TMenuSetAction; AStatusReason: string = ''); override;
  public
//    procedure DisableAllMenus;
    procedure AddMenuItems; override;
    procedure SetMenuNewDrawingGroup(AAction: TMenuSetAction);
    procedure SetMenuDeleteDrawingGroup(AAction: TMenuSetAction);
    procedure SetMenuRenameDrawingGroup(AAction: TMenuSetAction);
    procedure SetMenuNewDrawing(AAction: TMenuSetAction);
    procedure SetMenuDeleteDrawing(AAction: TMenuSetAction);
    procedure SetMenuRenameDrawing(AAction: TMenuSetAction);
    procedure SetMenuEditDrawing(AAction: TMenuSetAction);
    procedure SetMenuViewDrawing(AAction: TMenuSetAction);
    procedure SetMenuCopyDrawing(AAction: TMenuSetAction);

    function Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    property  ToolBar: TNetworkVisualiserToolBar read FToolBar;
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
  CDrawingSep         : array[0..1] of WideString = ('Edit','VNVDrawingSep');
  CNewDrawingGroup    : array[0..1] of WideString = ('Edit','VNVNewDrawingGroup');
  CDeleteDrawingGroup : array[0..1] of WideString = ('Edit','VNVDeleteDrawingGroup');
  CRenameDrawingGroup : array[0..1] of WideString = ('Edit','VNVRenameDrawingGroup');
  CNewDrawing         : array[0..1] of WideString = ('Edit','VNVNewDrawing');
  CDeleteDrawing      : array[0..1] of WideString = ('Edit','VNVDeleteDrawing');
  CRenameDrawing      : array[0..1] of WideString = ('Edit','VNVRenameDrawing');
  CViewDrawing        : array[0..1] of WideString = ('Edit','VNVViewDrawing');
  CEditDrawing        : array[0..1] of WideString = ('Edit','VNVEditDrawing');
  CCopyDrawing        : array[0..1] of WideString = ('Edit','VNVCopyDrawing');

{ TNetworkVisualiserMenuItemManager }

procedure TNetworkVisualiserMenuItemManager.AddMenuItems;
const OPNAME = 'TNetworkVisualiserMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CDrawingSep,          300);
    //AddMenuItemEntry(CNewDrawingGroup,     301, CmeCustomModelEvent, TModelMenuData.Create(meNewDrawingGroup));
    //AddMenuItemEntry(CDeleteDrawingGroup,  302, CmeCustomModelEvent, TModelMenuData.Create(meDeleteDrawingGroup));
    //AddMenuItemEntry(CRenameDrawingGroup,  303, CmeCustomModelEvent, TModelMenuData.Create(meRenameDrawingGroup));
    AddMenuItemEntry(CNewDrawing,          304, CmeCustomModelEvent, TModelMenuData.Create(meNewDrawing));
    AddMenuItemEntry(CDeleteDrawing,       305, CmeCustomModelEvent, TModelMenuData.Create(meDeleteDrawing));
    AddMenuItemEntry(CRenameDrawing,       306, CmeCustomModelEvent, TModelMenuData.Create(meRenameDrawing));
    AddMenuItemEntry(CViewDrawing,         307, CmeCustomModelEvent, TModelMenuData.Create(meViewDrawing));
    AddMenuItemEntry(CEditDrawing,         308, CmeCustomModelEvent, TModelMenuData.Create(meEditDrawing));
    AddMenuItemEntry(CCopyDrawing,         309, CmeCustomModelEvent, TModelMenuData.Create(meCopyDrawing));
    Hide;
//    Disable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.CreateMemberObjects;
const OPNAME = 'TNetworkVisualiserMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TNetworkVisualiserToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TNetworkVisualiserMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function TNetworkVisualiserMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
//    DisableAllMenus;
    Result := Result and FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.SetMenuNewDrawingGroup( AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuNewDrawingGroup';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetNewDrawingGroupState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CNewDrawingGroup, AAction, 'VNVNewDrawingGroupDisabled');
    else
      FAppModules.SetMenuItem(CNewDrawingGroup, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.SetMenuDeleteDrawing(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuDeleteDrawing';
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

procedure TNetworkVisualiserMenuItemManager.SetMenuRenameDrawing(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuRenameDrawing';
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

procedure TNetworkVisualiserMenuItemManager.SetMenuNewDrawing(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuNewDrawing';
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

procedure TNetworkVisualiserMenuItemManager.SetMenuDeleteDrawingGroup(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuDeleteDrawingGroup';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetDeleteDrawingGroupState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CDeleteDrawingGroup, AAction, 'VNVDeleteDrawingGroupDisabled');
    else
      FAppModules.SetMenuItem(CDeleteDrawingGroup, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.SetMenuRenameDrawingGroup(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuRenameDrawingGroup';
begin
  try
   if(AAction = msEnable) then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AAction := msDisable;
    FToolBar.SetRenameDrawingGroupState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CRenameDrawingGroup, AAction, 'VNVRenameDrawingGroupDisabled');
    else
      FAppModules.SetMenuItem(CRenameDrawingGroup, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.SetMenuEditDrawing(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuEditDrawing';
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

procedure TNetworkVisualiserMenuItemManager.SetMenuViewDrawing(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuViewDrawing';
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

procedure TNetworkVisualiserMenuItemManager.SetMenuCopyDrawing(AAction: TMenuSetAction);
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuCopyDrawing';
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

function TNetworkVisualiserMenuItemManager.Initialise: boolean;
const OPNAME = 'TNetworkVisualiserMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
//    DisableAllMenus;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



(*
procedure TNetworkVisualiserMenuItemManager.DisableAllMenus;
const OPNAME = 'TNetworkVisualiserMenuItemManager.DisableAllMenus';
begin
  try
    SetMenuNewDrawingGroup(msDisable);
    SetMenuDeleteDrawingGroup(msDisable);
    SetMenuRenameDrawingGroup(msDisable);
    SetMenuNewDrawing(msDisable);
    SetMenuDeleteDrawing(msDisable);
    SetMenuRenameDrawing(msDisable);
    SetMenuEditDrawing(msDisable);
    SetMenuViewDrawing(msDisable);
    SetMenuCopyDrawing(msDisable);}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserMenuItemManager.SetMenuItems(AnAction: TMenuSetAction; AStatusReason: string = '');
const OPNAME = 'TNetworkVisualiserMenuItemManager.SetMenuItems';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
end.
