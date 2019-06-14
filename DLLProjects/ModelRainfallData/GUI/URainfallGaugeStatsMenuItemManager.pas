{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeStatsMenuItemManager Class             *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 13/01/2005                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeStatsMenuItemManager;

interface

uses
  URainfallGaugeStatsToolBar,
  UMenuItemManager;

const
  CRainCreateFiles : array[0..1] of string = ('Data','RDCreateFiles');
  CCreateSplit     : array[0..1] of string = ('Data','RDCreateSplit');
  CDeleteSplit     : array[0..1] of string = ('Data','RDDeleteSplit');
  CUpdateSplit     : array[0..1] of string = ('Data','RDUpdateSplit');

  CToggleGrid      : array[0..1] of string = ('View','RDStatsToggleGrid');
  CToggleGraph     : array[0..1] of string = ('View','RDStatsToggleGraph');
  CToggleTree      : array[0..1] of string = ('View','RDStatsToggleTree');

type
  TRainfallGaugeStatsMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TRainfallGaugeStatsToolBar;
  public
    procedure CreateMemberObjects; override;
    procedure AddMenuItems; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetCreateFiles(AEnabled: boolean);
    procedure SetCreateSplit(AEnabled: boolean);
    procedure SetUpdateSplit(AEnabled: boolean);
    procedure SetDeleteSplit(AEnabled: boolean);
    procedure SetToggleGrid (AEnabled : boolean);
    procedure SetToggleGraph (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    property ToolBar: TRainfallGaugeStatsToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TRainfallGaugeStatsMenuItemManager }

procedure TRainfallGaugeStatsMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CRainCreateFiles, 10, CmeCustomModelEvent, TModelMenuData.Create(meCreateFiles));
    AddMenuItemEntry(CCreateSplit,     20, CmeCustomModelEvent, TModelMenuData.Create(meCreateSplit));
    AddMenuItemEntry(CUpdateSplit,     30, CmeCustomModelEvent, TModelMenuData.Create(meUpdateSplit));
    AddMenuItemEntry(CDeleteSplit,     40, CmeCustomModelEvent, TModelMenuData.Create(meDeleteSplit));
    AddMenuItemEntry(CToggleGrid,     700, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainStatsGrid));
    AddMenuItemEntry(CToggleGraph,    710, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainStatsGraph));
    AddMenuItemEntry(CToggleTree,     720, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainStatsTree));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TRainfallGaugeStatsToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsMenuItemManager.Initialise: boolean;
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetCreateFiles(TRUE);
    SetCreateSplit(FALSE);
    SetDeleteSplit(FALSE);
    SetUpdateSplit(False);
    SetToggleGrid(TRUE);
    SetToggleGraph(TRUE);
    SetToggleTree(TRUE);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetCreateFiles(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetCreateFiles';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CRainCreateFiles, msEnable)
    else
      FAppModules.SetMenuItem(CRainCreateFiles, msDisable, '');
    FToolBar.SetCreateFiles(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetCreateSplit(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetCreateSplit';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateSplit, msEnable)
    else
      FAppModules.SetMenuItem(CCreateSplit, msDisable, '');
    FToolBar.SetCreateSplit(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetDeleteSplit(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetDeleteSplit';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteSplit, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteSplit, msDisable, '');
    FToolBar.SetDeleteSplit(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetUpdateSplit(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetUpdateSplit';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CUpdateSplit, msEnable)
    else
      FAppModules.SetMenuItem(CUpdateSplit, msDisable, '');
    FToolBar.SetUpdateSplit(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetToggleGrid (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetToggleGrid';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleGrid, msEnable)
    else
      FAppModules.SetMenuItem(CToggleGrid, msDisable, '');
    FToolBar.SetToggleGrid(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetToggleGraph (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetToggleGraph';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleGraph, msEnable)
    else
      FAppModules.SetMenuItem(CToggleGraph, msDisable, '');
    FToolBar.SetToggleGraph(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsMenuItemManager.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeStatsMenuItemManager.SetToggleTree';
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
