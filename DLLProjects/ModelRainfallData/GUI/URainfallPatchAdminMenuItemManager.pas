//
//
//  UNIT      : Contains TRainfallPatchAdminMenuItemManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 03/12/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallPatchAdminMenuItemManager;

interface

uses
  URainfallPatchAdminToolBar,
  UMenuItemManager;

const
  CCreatePatch          : array[0..1] of WideString = ('Data','RDCreatePatch');
  CDeletePatch          : array[0..1] of WideString = ('Data','RDDeletePatch');
  CRenamePatch          : array[0..1] of WideString = ('Data','RDRenamePatch');
  CAddGaugeToPatch      : array[0..1] of WideString = ('Data','RDAddGaugeToPatch');
  CRemoveGaugeFromPatch : array[0..1] of WideString = ('Data','RDRemoveGaugeFromPatch');
  CToggleGrid           : array[0..1] of WideString = ('View','RDAdminToggleGrid');
  CToggleGraph          : array[0..1] of WideString = ('View','RDAdminToggleGraph');
  CToggleTree           : array[0..1] of WideString = ('View','RDAdminToggleTree');

type

  TRainfallPatchAdminMenuItemManager = class ( TMenuItemManager )
  protected
    FToolBar : TRainfallPatchAdminToolBar;
  public
    procedure CreateMemberObjects; override;
    procedure AddMenuItems; override;
    function  Initialise : boolean; override;
    function  LanguageHasChanged : boolean; override;
    procedure SetCreatePatch (AEnabled : boolean);
    procedure SetDeletePatch (AEnabled : boolean);
    procedure SetRenamePatch (AEnabled : boolean);
    procedure SetAddGaugeToPatch (AEnabled : boolean);
    procedure SetRemoveGaugeFromPatch (AEnabled : boolean);
    procedure SetToggleGrid (AEnabled : boolean);
    procedure SetToggleGraph (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);

    property  ToolBar: TRainfallPatchAdminToolBar read FToolBar;

end;

implementation

uses
  SysUtils,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TRainfallPatchAdminMenuItemManager }

procedure TRainfallPatchAdminMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallPatchAdminMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TRainfallPatchAdminToolBar.Create ( nil, FAppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminMenuItemManager.Initialise : boolean;
const OPNAME = 'TRainfallPatchAdminMenuItemManager.Initialise';
begin
  Result := True;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminMenuItemManager.LanguageHasChanged : boolean;
const OPNAME = 'TRainfallPatchAdminMenuItemManager.LanguageHasChanged';
begin
  Result := True;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallPatchAdminMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CCreatePatch,          10, CmeCustomModelEvent, TModelMenuData.Create(meCreatePatch));
    AddMenuItemEntry(CDeletePatch,          20, CmeCustomModelEvent, TModelMenuData.Create(meDeletePatch));
    AddMenuItemEntry(CRenamePatch,          30, CmeCustomModelEvent, TModelMenuData.Create(meRenamePatch));
    AddMenuItemEntry(CAddGaugeToPatch,      40, CmeCustomModelEvent, TModelMenuData.Create(meAddGaugeToPatch));
    AddMenuItemEntry(CRemoveGaugeFromPatch, 50, CmeCustomModelEvent, TModelMenuData.Create(meRemoveGaugeFromPatch));
    AddMenuItemEntry(CToggleGrid,          700, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainAdminGrid));
    AddMenuItemEntry(CToggleGraph,         710, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainAdminGraph));
    AddMenuItemEntry(CToggleTree,          720, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainAdminTree));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetCreatePatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetCreatePatch';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreatePatch, msEnable)
    else
      FAppModules.SetMenuItem(CCreatePatch, msDisable, '');
    FToolBar.SetCreatePatch(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetDeletePatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetDeletePatch';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeletePatch, msEnable)
    else
      FAppModules.SetMenuItem(CDeletePatch, msDisable, '');
    FToolBar.SetDeletePatch(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetRenamePatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetRenamePatch';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CRenamePatch, msEnable)
    else
      FAppModules.SetMenuItem(CRenamePatch, msDisable, '');
    FToolBar.SetRenamePatch(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetAddGaugeToPatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetAddGaugeToPatch';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CAddGaugeToPatch, msEnable)
    else
      FAppModules.SetMenuItem(CAddGaugeToPatch, msDisable, '');
    FToolBar.SetAddGaugeToPatch(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetRemoveGaugeFromPatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetRemoveGaugeFromPatch';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CRemoveGaugeFromPatch, msEnable)
    else
      FAppModules.SetMenuItem(CRemoveGaugeFromPatch, msDisable, '');
    FToolBar.SetRemoveGaugeFromPatch(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetToggleGrid (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetToggleGrid';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleGrid, msEnable)
    else
      FAppModules.SetMenuItem(CToggleGrid, msDisable, '');
    FToolBar.SetToggleGrid(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetToggleGraph (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetToggleGraph';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleGraph, msEnable)
    else
      FAppModules.SetMenuItem(CToggleGraph, msDisable, '');
    FToolBar.SetToggleGraph(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminMenuItemManager.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminMenuItemManager.SetToggleTree';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleTree, msEnable)
    else
      FAppModules.SetMenuItem(CToggleTree, msDisable, '');
    FToolBar.SetToggleTree(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
