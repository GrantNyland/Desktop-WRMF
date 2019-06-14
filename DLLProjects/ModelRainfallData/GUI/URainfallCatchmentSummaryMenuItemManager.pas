unit URainfallCatchmentSummaryMenuItemManager;

interface
uses
  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  URainfallCatchmentSummaryToolBar,
  UAbstractComponent,
  UAbstractObject,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;
const
  CExportCatchmentData         : array[0..1] of string = ('Data','RDEXPORTDATA');
  CToggleStationUsedGrid       : array[0..1] of string = ('View','RDAVRGRAINFALL');
  CToggleCatchmentInputGrid    : array[0..1] of string = ('View','RDINPUTRAINFALL');
  CToggleRunGrid               : array[0..1] of string = ('View','RDRESULT');

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
  TRainfallCatchmentSummaryMenuItemManager = class (TMenuItemManager)

  protected
    FToolBar: TRainfallCatchmentSummaryToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

  public
    function LanguageHasChanged: Boolean; override;
    procedure AddMenuItems; override;
    function SetMenuState(AMenuState : TRainfallMenuState) : boolean;
    function  Initialise: boolean; override;
    property  ToolBar: TRainfallCatchmentSummaryToolBar read FToolBar;
    procedure SetExportData (AEnabled : boolean);
    procedure SetToggleTopGrid (AEnabled : boolean);
    procedure SetToggleMiddleGrid (AEnabled : boolean);
    procedure SetToggleBottomGrid (AEnabled : boolean);
  end;
implementation
uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ URainfallCatchmentSummaryMenuItemManager }

function TRainfallCatchmentSummaryMenuItemManager.Initialise: boolean;
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetExportData(True);
    SetToggleTopGrid(True);
    SetToggleMiddleGrid(True);
    SetToggleBottomGrid(True);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FToolBar := TRainfallCatchmentSummaryToolBar.Create(nil, AppModules );
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryMenuItemManager.LanguageHasChanged: Boolean;
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.SetExportData(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.SetExportData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryMenuItemManager.SetMenuState(AMenuState: TRainfallMenuState): boolean;
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.SetMenuState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CExportCatchmentData,        700, CmeCustomModelEvent,
                    TModelMenuData.Create(meExportCatchmentData));
    AddMenuItemEntry(CToggleStationUsedGrid,      710, CmeCustomModelEvent,
                    TModelMenuData.Create(meToggleStationUsedGrid));
    AddMenuItemEntry(CToggleCatchmentInputGrid,  720, CmeCustomModelEvent,
                    TModelMenuData.Create(meToggleCatchmentOutputGrid));
    AddMenuItemEntry(CToggleRunGrid,              730, CmeCustomModelEvent,
                    TModelMenuData.Create(meToggleRUNGrid));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.SetToggleTopGrid(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.SetToggleTopGrid';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.SetToggleMiddleGrid(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.SetToggleMiddleGrid';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryMenuItemManager.SetToggleBottomGrid(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryMenuItemManager.SetToggleBottomGrid';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
