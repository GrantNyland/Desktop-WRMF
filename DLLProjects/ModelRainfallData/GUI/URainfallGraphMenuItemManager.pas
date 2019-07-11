{******************************************************************************}
{*  UNIT      : Contains TRainfallGraphMenuItemManager Class                  *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 17/01/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}


unit URainfallGraphMenuItemManager;

interface

uses
  URainfallGraphToolBar,
  UMenuItemManager;

const
  CToggleGrid      : array[0..1] of WideString = ('View', 'RDGraphToggleGrid');
  CToggleGraph     : array[0..1] of WideString = ('View', 'RDGraphToggleGraph');
  CToggleTree      : array[0..1] of WideString = ('View', 'RDGraphToggleTree');
  CCreatePATFiles  : array[0..1] of WideString = ('Data', 'RDCreatePATFiles');
  CHighLight       : array[0..1] of WideString = ('Data', 'RDHighLightOutliers');
  CSelectRAWFlags  : array[0..1] of WideString = ('Data', 'RDSelectRAWFlags');
  CFlagDataBlock   : array[0..1] of WideString = ('Data', 'RDFlagDataBlock');
  CUnFlagDataBlock : array[0..1] of WideString = ('Data', 'RDUnFlagDataBlock');
  CFlagSetup       : array[0..1] of WideString = ('Data', 'RDFlagSetup');
  CFlagClick       : array[0..1] of WideString = ('Data', 'RDFlagClick');
  CWeatherEvents   : array[0..1] of WideString = ('Data', 'RDWeatherEvents');
  CPatchChangeList : array[0..1] of WideString = ('Data', 'CLPatchParameter');

type

  TRainfallGraphMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TRainfallGraphToolBar;
  public
    procedure CreateMemberObjects; override;
    procedure AddMenuItems; override;
    function  Initialise : boolean; override;
    function  LanguageHasChanged : boolean; override;
    procedure SetCreatePATFiles (AEnabled : boolean);
    procedure SetHighLight (AEnabled : boolean);
    procedure SetSelectRAWFlags (AEnabled : boolean);
    procedure SetFlagData (AEnabled : boolean);
    procedure SetUnFlagData (AEnabled : boolean);
    procedure SetFlagSetup (AEnabled : boolean);
    procedure SetFlagClick (AEnabled : boolean);
    procedure SetToggleGrid (AEnabled : boolean);
    procedure SetToggleGraph (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    procedure SetWeatherEvents (AEnabled : boolean);
    procedure SetPatchChangeList (AEnabled : boolean);

    property  ToolBar: TRainfallGraphToolBar read FToolBar;

end;

implementation

uses
  SysUtils,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TRainfallGraphMenuItemManager }

procedure TRainfallGraphMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallGraphMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TRainfallGraphToolBar.Create ( nil, FAppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphMenuItemManager.Initialise : boolean;
const OPNAME = 'TRainfallGraphMenuItemManager.Initialise';
begin
  Result := True;
  try
    SetToggleGrid(TRUE);
    SetToggleGraph(TRUE);
    SetToggleTree(TRUE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphMenuItemManager.LanguageHasChanged : boolean;
const OPNAME = 'TRainfallGraphMenuItemManager.LanguageHasChanged';
begin
  Result := True;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallGraphMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CToggleGrid,      700, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainGraphGrid));
    AddMenuItemEntry(CToggleGraph,     710, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainGraphGraph));
    AddMenuItemEntry(CToggleTree,      720, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainGraphTree));
    AddMenuItemEntry(CCreatePATFiles,  200, CmeCustomModelEvent, TModelMenuData.Create(meCreatePATFiles));
    AddMenuItemEntry(CHighLight,       210, CmeCustomModelEvent, TModelMenuData.Create(meHighLightOutliers));
    AddMenuItemEntry(CSelectRAWFlags,  220, CmeCustomModelEvent, TModelMenuData.Create(meSelectRAWFlags));
    AddMenuItemEntry(CFlagDataBlock,   230, CmeCustomModelEvent, TModelMenuData.Create(meFlagRainfallDataBlock));
    AddMenuItemEntry(CUnFlagDataBlock, 240, CmeCustomModelEvent, TModelMenuData.Create(meUnFlagRainfallDataBlock));
    AddMenuItemEntry(CFlagSetup,       250, CmeCustomModelEvent, TModelMenuData.Create(meFlagSetup));
    AddMenuItemEntry(CFlagClick,       260, CmeCustomModelEvent, TModelMenuData.Create(meFlagClick));
    AddMenuItemEntry(CWeatherEvents,   300, CmeCustomModelEvent, TModelMenuData.Create(meWeatherEvents));
    AddMenuItemEntry(CPatchChangeList, 320, CmeCustomModelEvent, TModelMenuData.Create(mePatchChangeList));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetCreatePATFiles (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetCreatePATFiles';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreatePATFiles, msEnable)
    else
      FAppModules.SetMenuItem(CCreatePATFiles, msDisable, '');
    FToolBar.SetCreatePATFiles(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetHighLight (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetHighLight';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CHighLight, msEnable)
    else
      FAppModules.SetMenuItem(CHighLight, msDisable, '');
    FToolBar.SetHighLight(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetSelectRAWFlags (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetSelectRAWFlags';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CSelectRAWFlags, msEnable)
    else
      FAppModules.SetMenuItem(CSelectRAWFlags, msDisable, '');
    FToolBar.SetSelectRAWFlags(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetFlagData (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetFlagData';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CFlagDataBlock, msEnable)
    else
      FAppModules.SetMenuItem(CFlagDataBlock, msDisable, '');
    FToolBar.SetFlagData(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetUnFlagData (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetUnFlagData';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CUnFlagDataBlock, msEnable)
    else
      FAppModules.SetMenuItem(CUnFlagDataBlock, msDisable, '');
    FToolBar.SetUnFlagData(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetFlagSetup (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetFlagSetup';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CFlagSetup, msEnable)
    else
      FAppModules.SetMenuItem(CFlagSetup, msDisable, '');
    FToolBar.SetFlagSetup(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetFlagClick (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetFlagClick';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CFlagClick, msEnable)
    else
      FAppModules.SetMenuItem(CFlagClick, msDisable, '');
    FToolBar.SetFlagClick(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetToggleGrid (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetToggleGrid';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleGrid, msEnable)
    else
      FAppModules.SetMenuItem(CToggleGrid, msDisable, '');
    FToolBar.SetToggleGrid(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetToggleGraph (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetToggleGraph';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleGraph, msEnable)
    else
      FAppModules.SetMenuItem(CToggleGraph, msDisable, '');
    FToolBar.SetToggleGraph(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetToggleTree';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CToggleTree, msEnable)
    else
      FAppModules.SetMenuItem(CToggleTree, msDisable, '');
    FToolBar.SetToggleTree(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetWeatherEvents (AEnabled : boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetWeatherEvents';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CWeatherEvents, msEnable)
    else
      FAppModules.SetMenuItem(CWeatherEvents, msDisable, '');
    FToolBar.SetWeatherEvents(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphMenuItemManager.SetPatchChangeList(  AEnabled: boolean);
const OPNAME = 'TRainfallGraphMenuItemManager.SetPatchChangeList';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CPatchChangeList, msEnable)
    else
      FAppModules.SetMenuItem(CPatchChangeList, msDisable, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
