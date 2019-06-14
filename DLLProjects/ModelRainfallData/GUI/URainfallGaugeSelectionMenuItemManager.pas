//
//
//  UNIT      : Contains  TRainfallToolBar and TRainfallGaugeSelectionMenuItemManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallGaugeSelectionMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  URainfallGaugeSelectionToolBar,
  UAbstractComponent,
  UAbstractObject,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

const
  CCreateReport            : array[0..1] of string = ('Data','RDCreateReport' );
  CImportUserData          : array[0..1] of string = ('Data','RDImportUserData');
  CClearUserData           : array[0..1] of string = ('Data','RDClearUserData');
  CImportSawsDwafData      : array[0..1] of string = ('Data','RDImportSawsDwafData');

  CSelectionSepEnd         : array[0..1] of string = ('View','RDSelectionSepEnd');
  CSelectUnSelectAll       : array[0..1] of string = ('View','RDSelectUnSelectAll');
  CSelectUnSelect          : array[0..1] of string = ('View','RDSelectUnSelect');
  CSelectFromSelectionFile : array[0..1] of string = ('View','RDFromSelectionFile');
  CSelectByRectangle       : array[0..1] of string = ('View','RDSelectByRectangle');
  CSelectByDistance        : array[0..1] of string = ('View','RDSelectByDistance');
  CSelectByStationName     : array[0..1] of string = ('View','RDSelectByStationName');
  CSelectByStationNumber   : array[0..1] of string = ('View','RDSelectByStationNumber');
  CSelectToggle            : array[0..1] of string = ('View','RDOptions');
  CSelectReplaceSelection  : array[0..2] of string = ('View','RDOptions','RDOptionsReplaceSelection');
  CSelectUpdateGISLive     : array[0..2] of string = ('View','RDOptions','RDOptionsUpdateGISLive');
  CViewStatusBar           : array[0..1] of string = ('View','RDViewStatusBar');
  CToggleTree              : array[0..1] of string = ('View','RDGaugeToggleTree');

type
  TRainfallMenuState = (
    mcFileIsLoaded,
    mcFileIsNotLoaded,
    mcSelectionIsAvailable,
    mcSelectionIsNotAvailable,
    mcReplaceSelection,
    mcUnReplaceSelection,
    mcViewStatusBar,
    mcHideStatusBar);

type
  TRainfallGaugeSelectionMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar          : TRainfallGaugeSelectionToolBar;
//    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function SetMenuState ( AMenuState : TRainfallMenuState ) : boolean;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    property  ToolBar: TRainfallGaugeSelectionToolBar read FToolBar;
//    procedure Show; override;
    function GetCheckedState (AMenuEvent : TModelMenuAction { TRGMenuEvent } ): boolean;
    procedure SetCreateReport(AEnabled: boolean);
    procedure SetImportUserData (AEnabled : boolean);
    procedure SetClearUserData (AEnabled : boolean);
    procedure SetImportSawsDwafData(AEnabled : boolean);


  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TRainfallGaugeSelectionMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CCreateReport,           700, CmeCustomModelEvent, TModelMenuData.Create(MeFileReport ) );
    AddMenuItemEntry(CImportUserData,         710, CmeCustomModelEvent, TModelMenuData.Create(meImportUserData));
    AddMenuItemEntry(CClearUserData,          720, CmeCustomModelEvent, TModelMenuData.Create(meClearUserData));
    AddMenuItemEntry(CImportSawsDwafData,     730, CmeCustomModelEvent, TModelMenuData.Create(meImportSawsDwafData));

    AddMenuItemEntry(CSelectUnSelectAll,      620, CmeCustomModelEvent, TModelMenuData.Create(MeUnSelectAll ) );
    AddMenuItemEntry(CSelectUnSelect,         625, CmeCustomModelEvent, TModelMenuData.Create(MeUnSelect ) );
    AddMenuItemEntry(CSelectByRectangle,      635, CmeCustomModelEvent, TModelMenuData.Create(MeSelectRect ) );
    AddMenuItemEntry(CSelectByDistance,       640, CmeCustomModelEvent, TModelMenuData.Create(MeSelectByDistance ) );
    AddMenuItemEntry(CSelectByStationNumber,  650, CmeCustomModelEvent, TModelMenuData.Create(MeSelectByStationNumber ) );
    AddMenuItemEntry(CSelectByStationName,    655, CmeCustomModelEvent, TModelMenuData.Create(MeSelectByStationName ) );
    AddMenuItemEntry(CSelectionSepEnd,        660);
    AddMenuItemEntry(CSelectToggle,           665);
    AddMenuItemEntry(CSelectReplaceSelection, 670, CmeCustomModelEvent, TModelMenuData.Create(MeOptionAddToSelection ) );
    AddMenuItemEntry(CSelectUpdateGISLive,    675, CmeCustomModelEvent, TModelMenuData.Create(MeOptionUpdateGISLive ) );
    AddMenuItemEntry(CToggleTree,             720, CmeCustomModelEvent, TModelMenuData.Create(meToggleRainGaugeTree));
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRainfallGaugeSelectionMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TRainfallGaugeSelectionToolBar.Create ( nil, AppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TRainfallGaugeSelectionMenuItemManager.DisableAllMenus;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.DisableAllMenus';
begin
  try
    SetMenuState(mcFileIsNotLoaded);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
function TRainfallGaugeSelectionMenuItemManager.GetCheckedState( AMenuEvent: TModelMenuAction): boolean;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.GetCheckedState';
begin
  Result := false;
  try
    case AMenuEvent of
      MeOptionAddToSelection:
        Result := msChecked in FAppModules.GetMenuItemProperties(CSelectReplaceSelection);
      MeOptionUpdateGISLive:
        Result := msChecked in FAppModules.GetMenuItemProperties(CSelectReplaceSelection);
      MeViewStatusBar:
        Result := msChecked in FAppModules.GetMenuItemProperties(CViewStatusBar);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionMenuItemManager.Initialise: boolean;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.Initialise';
begin
  Result := false;
  try
    SetCreateReport(TRUE);
    SetImportUserData(TRUE);
    SetClearUserData(FALSE);
    SetImportSawsDwafData(True);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionMenuItemManager.SetMenuState(AMenuState: TRainfallMenuState): boolean;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.SetMenuState';
begin
  Result := false;
  try
    FAppModules.SetMenuItem(CToggleTree, msEnable);
    if (AMenuState = mcFileIsNotLoaded) then
    begin
      FAppModules.SetMenuItem(CSelectUnSelectAll,msDisable);
      FAppModules.SetMenuItem(CSelectUnSelect,msDisable);
      FAppModules.SetMenuItem(CSelectByRectangle,msDisable);
      FAppModules.SetMenuItem(CSelectByDistance,msDisable);
      FAppModules.SetMenuItem(CSelectByStationName,msDisable);
      FAppModules.SetMenuItem(CSelectByStationNumber,msDisable);
      FAppModules.SetMenuItem(CSelectToggle,msDisable);
      FAppModules.SetMenuItem(CSelectReplaceSelection,msDisable);
      FAppModules.SetMenuItem(CSelectUpdateGISLive,msDisable);
      ToolBar.SetState(geFileIsNotLoaded);
    end
    else if AMenuState in [mcFileIsLoaded, mcSelectionIsNotAvailable] then
    begin
      FAppModules.SetMenuItem(CSelectUnSelectAll,msEnable);
      FAppModules.SetMenuItem(CSelectUnSelect,msEnable);

      FAppModules.SetMenuItem(CSelectByRectangle,msEnable);
      FAppModules.SetMenuItem(CSelectByDistance,msEnable);
      FAppModules.SetMenuItem(CSelectByStationName,msEnable);
      FAppModules.SetMenuItem(CSelectByStationNumber,msEnable);
      ToolBar.SetState(geSelectionIsNotAvailable);
    end
    else if AMenuState in [mcFileIsLoaded, mcSelectionIsAvailable] then
    begin
      FAppModules.SetMenuItem(CSelectUnSelectAll,msEnable);
      FAppModules.SetMenuItem(CSelectUnSelect,msEnable);
      FAppModules.SetMenuItem(CSelectToggle,msEnable);
      FAppModules.SetMenuItem(CSelectByRectangle,msEnable);
      FAppModules.SetMenuItem(CSelectByDistance,msEnable);
      FAppModules.SetMenuItem(CSelectByStationName,msEnable);
      FAppModules.SetMenuItem(CSelectByStationNumber,msEnable);
      FAppModules.SetMenuItem(CSelectToggle,msEnable);
      ToolBar.SetState(geSelectionIsAvailable);
    end;
  case (AMenuState) of
    mcReplaceSelection   : FAppModules.SetMenuItem(CSelectReplaceSelection,msChecked);
    mcUnReplaceSelection : FAppModules.SetMenuItem(CSelectReplaceSelection,msUnChecked);
    mcHideStatusBar      : FAppModules.SetMenuItem(CViewStatusBar,msUnChecked);
    mcViewStatusBar      : FAppModules.SetMenuItem(CViewStatusBar,msChecked);
  end;

  Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TRainfallGaugeSelectionMenuItemManager.Show;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.Show';
begin
  try
    inherited Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)
function TRainfallGaugeSelectionMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionMenuItemManager.SetCreateReport(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.SetCreateReport';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateReport, msEnable)
    else
      FAppModules.SetMenuItem(CCreateReport, msDisable, '');
    FToolBar.SetCreateReport(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionMenuItemManager.SetImportUserData (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.SetImportUserData';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CImportUserData, msEnable)
    else
      FAppModules.SetMenuItem(CImportUserData, msDisable, '');
    FToolBar.SetImportUserData(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionMenuItemManager.SetClearUserData (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.SetClearUserData';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CClearUserData, msEnable)
    else
      FAppModules.SetMenuItem(CClearUserData, msDisable, '');
    FToolBar.SetClearUserData(AEnabled);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionMenuItemManager.SetImportSawsDwafData(AEnabled : boolean);
const OPNAME = 'TRainfallGaugeSelectionMenuItemManager.SetImportSawsDwafData';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CImportSawsDwafData, msEnable)
    else
      FAppModules.SetMenuItem(CImportSawsDwafData, msDisable, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
