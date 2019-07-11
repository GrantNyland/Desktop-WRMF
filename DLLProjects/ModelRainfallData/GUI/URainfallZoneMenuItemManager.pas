{******************************************************************************}
{*  UNIT      : Contains TRainfallZoneMenuItemManager Class                   *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 13/01/2005                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit URainfallZoneMenuItemManager;

interface

uses
  URainfallZoneToolBar,
  UMenuItemManager;

const
  CCreateCatchmentZone : array[0..1] of WideString = ('Data','RDCreateCatchmentZone');
  CDeleteCatchmentZone : array[0..1] of WideString = ('Data','RDDeleteCatchmentZone');
  CAddGaugeToZone      : array[0..1] of WideString = ('Data','RDAddGaugeToZone');
  CRemoveGaugeFromZone : array[0..1] of WideString = ('Data','RDRemoveGaugeFromZone');
  CToggleTree          : array[0..1] of WideString = ('View','RDZoneToggleTree');

type
  TRainfallZoneMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TRainfallZoneToolBar;
  public
    procedure CreateMemberObjects; override;
    procedure AddMenuItems; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetAddToZone (AEnabled : boolean);
    procedure SetCreateCatchmentZone(AEnabled : boolean);
    procedure SetDeleteCatchmentZone(AEnabled : boolean);

    procedure SetRemoveFromZone (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    property ToolBar: TRainfallZoneToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TRainfallZoneMenuItemManager }

procedure TRainfallZoneMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallZoneMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CCreateCatchmentZone, 700, CmeCustomModelEvent, TModelMenuData.Create(meCreateCatchmentZone));
    AddMenuItemEntry(CDeleteCatchmentZone, 710,  CmeCustomModelEvent, TModelMenuData.Create(meDeleteCatchmentZone));
    AddMenuItemEntry(CAddGaugeToZone,      720, CmeCustomModelEvent, TModelMenuData.Create(meAddGaugeToZone));
    AddMenuItemEntry(CRemoveGaugeFromZone, 730, CmeCustomModelEvent, TModelMenuData.Create(meRemoveGaugeFromZone));
    AddMenuItemEntry(CToggleTree,          740, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainZoneTree));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallZoneMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TRainfallZoneToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneMenuItemManager.Initialise: boolean;
const OPNAME = 'TRainfallZoneMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetCreateCatchmentZone(True);
    SetDeleteCatchmentZone(False);
    SetAddToZone(False);
    SetRemoveFromZone(False);
    SetToggleTree(TRUE);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallZoneMenuItemManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneMenuItemManager.SetCreateCatchmentZone(AEnabled : boolean);
const OPNAME = 'TRainfallZoneMenuItemManager.SetCreateCatchmentZone';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateCatchmentZone, msEnable)
    else
      FAppModules.SetMenuItem(CCreateCatchmentZone, msDisable, '');
    FToolBar.SetCreateCatchmentZone(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneMenuItemManager.SetDeleteCatchmentZone(AEnabled : boolean);
const OPNAME = 'TRainfallZoneMenuItemManager.SetDeleteCatchmentZone';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteCatchmentZone, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteCatchmentZone, msDisable, '');
    FToolBar.SetDeleteCatchmentZone(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



procedure TRainfallZoneMenuItemManager.SetAddToZone (AEnabled : boolean);
const OPNAME = 'TRainfallZoneMenuItemManager.SetAddToZone';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CAddGaugeToZone, msEnable)
    else
      FAppModules.SetMenuItem(CAddGaugeToZone, msDisable, '');
    FToolBar.SetAddToZone(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneMenuItemManager.SetRemoveFromZone (AEnabled : boolean);
const OPNAME = 'TRainfallZoneMenuItemManager.SetRemoveFromZone';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CRemoveGaugeFromZone, msEnable)
    else
      FAppModules.SetMenuItem(CRemoveGaugeFromZone, msDisable, '');
    FToolBar.SetRemoveFromZone(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneMenuItemManager.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallZoneMenuItemManager.SetToggleTree';
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
