{******************************************************************************}
{*  UNIT      : Contains the class TChangeAdminMenuItemManager.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/11                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeAdminMenuItemManager;

interface

uses
  UChangeAdminToolBar,
  UMenuItemManager;

type

  TChangeAdminMenuItemManager = class ( TMenuItemManager )
  protected
    FToolBar : TChangeAdminToolBar;
  public
    procedure CreateMemberObjects; override;
    procedure AddMenuItems; override;
    function  Initialise : boolean; override;
    function  LanguageHasChanged : boolean; override;
    procedure SetCreateNewChangeGroup (AEnabled : boolean);
    procedure SetDeleteChangeGroup (AEnabled : boolean);
    procedure SetCreateNewChangeList (AEnabled : boolean);
    procedure SetDeleteChangeList (AEnabled : boolean);
    procedure SetCopyChangeList (AEnabled : boolean);
    procedure SetMoveUpChangeElement (AEnabled : boolean);
    procedure SetMoveDownChangeElement (AEnabled : boolean);
    procedure SetActivateChangeElement (AEnabled : boolean);
    procedure SetDeactivateChangeElement (AEnabled : boolean);
    procedure SetApplyChangeList (AEnabled : boolean);
    procedure SetImportChangeList (AEnabled : boolean);
    procedure SetExportChangeList (AEnabled : boolean);
    procedure SetStationFilter   (AEnabled : boolean);

    property  ToolBar: TChangeAdminToolBar read FToolBar;

end;

implementation

uses
  SysUtils,
  UConstants,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CChanges                 : array[0..0] of string = ('Changes');
  CCreateChangeGroup       : array[0..1] of string = ('Changes','CGCreateNew');
  CDeleteChangeGroup       : array[0..1] of string = ('Changes','CGDelete');
  CCreateChangeList        : array[0..1] of string = ('Changes','CLCreateNew');
  CDeleteChangeList        : array[0..1] of string = ('Changes','CLDelete');
  CCopyChangeList          : array[0..1] of string = ('Changes','CLCopy');
  CMoveUpChangeElement     : array[0..1] of string = ('Changes','CLMoveUp');
  CMoveDownChangeElement   : array[0..1] of string = ('Changes','CLMoveDown');
  CActivateChangeElement   : array[0..1] of string = ('Changes','CEActivate');
  CDeactivateChangeElement : array[0..1] of string = ('Changes','CEDeactivate');
  CApplyChangeList         : array[0..1] of string = ('Changes','CLApply');
  CImportChangeList        : array[0..1] of string = ('Changes','CLImport');
  CExportChangeList        : array[0..1] of string = ('Changes','CLExport');
  CChangeStationFilter     : array[0..1] of string = ('Changes','CStationFilter');

{ TChangeAdminMenuItemManager }

procedure TChangeAdminMenuItemManager.CreateMemberObjects;
const OPNAME = 'TChangeAdminMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TChangeAdminToolBar.Create ( nil, FAppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminMenuItemManager.Initialise : boolean;
const OPNAME = 'TChangeAdminMenuItemManager.Initialise';
begin
  Result := True;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminMenuItemManager.LanguageHasChanged : boolean;
const OPNAME = 'TChangeAdminMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.AddMenuItems;
const OPNAME = 'TChangeAdminMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CChanges,              400);

    AddMenuItemEntry(CCreateChangeGroup,        10, CmeChangeGroupCreate,        nil);
    AddMenuItemEntry(CDeleteChangeGroup,        20, CmeChangeGroupDelete,        nil);
    AddMenuItemEntry(CCreateChangeList,         30, CmeChangeListCreate,         nil);
    AddMenuItemEntry(CDeleteChangeList,         40, CmeChangeListDelete,         nil);
    AddMenuItemEntry(CCopyChangeList,           50, CmeChangeListCopy,           nil);
    AddMenuItemEntry(CMoveUpChangeElement,      60, CmeChangeElementMoveUp,      nil);
    AddMenuItemEntry(CMoveDownChangeElement,    70, CmeChangeElementMoveDown,    nil);
    AddMenuItemEntry(CActivateChangeElement,    80, CmeChangeElementActivate,    nil);
    AddMenuItemEntry(CDeActivateChangeElement,  90, CmeChangeElementDeactivate,  nil);
    AddMenuItemEntry(CApplyChangeList,         100, CmeChangeListApply,          nil);
    AddMenuItemEntry(CImportChangeList,        110, CmeChangeListImport,         nil);
    AddMenuItemEntry(CExportChangeList,        120, CmeChangeListExport,         nil);
    AddMenuItemEntry(CChangeStationFilter,     130, CmeChangeListStationFilter,  nil);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetCreateNewChangeGroup (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetCreateNewChangeGroup';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CCreateChangeGroup, msEnable)
    else
      FAppModules.SetMenuItem(CCreateChangeGroup, msDisable, '');
    FToolBar.SetCreateNewChangeGroup(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetDeleteChangeGroup (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetDeleteChangeGroup';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteChangeGroup, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteChangeGroup, msDisable, '');
    FToolBar.SetDeleteChangeGroup(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetCreateNewChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetCreateNewChangeList';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CCreateChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CCreateChangeList, msDisable, '');
    FToolBar.SetCreateNewChangeList(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetDeleteChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetDeleteChangeList';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteChangeList, msDisable, '');
    FToolBar.SetDeleteChangeList(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetCopyChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetCopyChangeList';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CCopyChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CCopyChangeList, msDisable, '');
    FToolBar.SetCopyChangeList(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetMoveUpChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetMoveUpChangeElement';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CMoveUpChangeElement, msEnable)
    else
      FAppModules.SetMenuItem(CMoveUpChangeElement, msDisable, '');
    FToolBar.SetMoveUpChangeElement(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetMoveDownChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetMoveDownChangeElement';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CMoveDownChangeElement, msEnable)
    else
      FAppModules.SetMenuItem(CMoveDownChangeElement, msDisable, '');
    FToolBar.SetMoveDownChangeElement(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetActivateChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetActivateChangeElement';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CActivateChangeElement, msEnable)
    else
      FAppModules.SetMenuItem(CActivateChangeElement, msDisable, '');
    FToolBar.SetActivateChangeElement(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetDeactivateChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetDeactivateChangeElement';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CDeactivateChangeElement, msEnable)
    else
      FAppModules.SetMenuItem(CDeactivateChangeElement, msDisable, '');
    FToolBar.SetDeactivateChangeElement(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetApplyChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetApplyChangeList';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CApplyChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CApplyChangeList, msDisable, '');
    FToolBar.SetApplyChangeList(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetExportChangeList(AEnabled: boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetExportChangeList';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CExportChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CExportChangeList, msDisable, '');
    FToolBar.SetExportChangeList(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetImportChangeList(AEnabled: boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetImportChangeList';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CImportChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CImportChangeList, msDisable, '');
    FToolBar.SetImportChangeList(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminMenuItemManager.SetStationFilter(AEnabled: boolean);
const OPNAME = 'TChangeAdminMenuItemManager.SetStationFilter';
begin
  try
    AEnabled := AEnabled and (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    AEnabled := AEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if AEnabled then
      FAppModules.SetMenuItem(CChangeStationFilter, msEnable)
    else
      FAppModules.SetMenuItem(CChangeStationFilter, msDisable, '');
    FToolBar.SetFilterStation(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
