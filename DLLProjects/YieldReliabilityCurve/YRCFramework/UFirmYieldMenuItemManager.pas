//
//
//  UNIT      : Contains TFirmYieldMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/09/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFirmYieldMenuItemManager;

interface

uses
  Classes,
  VCL.Menus,
  UAbstractObject,
  UGUIConstants,
  UFirmYieldToolBar,
  UMenuItemManager;

type
  TFirmYieldMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TFirmYieldToolBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure AddMenuItems; override;
    procedure SetMenuNewChart(AAction: TMenuSetAction);
    procedure SetMenuOpenChart(AAction: TMenuSetAction);
    procedure SetMenuSaveChart(AAction: TMenuSetAction);
    procedure SetMenuAddSeries(AAction: TMenuSetAction);
    procedure SetMenuDeleteSeries(AAction: TMenuSetAction);
    property ToolBar: TFirmYieldToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

{const
  // View menu items.
  CYRCFYChartSep       : array[0..1] of string = ('File','YRCFYChartSep');
  CYRCFYNewChart       : array[0..1] of string = ('File','YRCFYNewChart');
  CYRCFYOpenChart      : array[0..1] of string = ('File','YRCFYOpenChart');
  CYRCFYSaveChart      : array[0..1] of string = ('File','YRCFYSaveChart');
  CYRCFYAddSeries      : array[0..1] of string = ('Edit','YRCFYAddSeries');
  CYRCFYDeleteSeries   : array[0..1] of string = ('Edit','YRCFYDeleteSeries');
}

{ TFirmYieldMenuItemManager }

procedure TFirmYieldMenuItemManager.AddMenuItems;
const OPNAME = 'TFirmYieldMenuItemManager.AddMenuItems';
begin
  try
    // View menu items.
    {AddMenuItemEntry(CYRCFYChartSep,     7100, CmeSeparator);
    AddMenuItemEntry(CYRCFYNewChart,     7105, CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldNewChart));
    AddMenuItemEntry(CYRCFYOpenChart,    7110, CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldOpenChart));
    AddMenuItemEntry(CYRCFYSaveChart,    7115, CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldSaveChart));
    AddMenuItemEntry(CYRCFYAddSeries,    7120, CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldAddSeries));
    AddMenuItemEntry(CYRCFYDeleteSeries, 7125, CmeCustomModelEvent, TModelMenuData.Create(meFirmYieldDeleteSeries));
    Disable; }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldMenuItemManager.CreateMemberObjects;
const OPNAME = 'TFirmYieldMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TFirmYieldToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TFirmYieldMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function TFirmYieldMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TFirmYieldMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;
               
function TFirmYieldMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TFirmYieldMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result then
      Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldMenuItemManager.SetMenuNewChart(AAction: TMenuSetAction);
const OPNAME = 'TFirmYieldMenuItemManager.SetMenuNewChart';
begin
  try
    FToolBar.SetNewChartState(AAction = msEnable);
    {case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCFYNewChart, AAction, 'FirmYieldNewChartDisabled');
    else
      FAppModules.SetMenuItem(CYRCFYNewChart, AAction);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldMenuItemManager.SetMenuOpenChart(AAction: TMenuSetAction);
const OPNAME = 'TFirmYieldMenuItemManager.SetMenuOpenChart';
begin
  try
    FToolBar.SetOpenChartState(AAction = msEnable);
    {case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCFYOpenChart, AAction, 'FirmYieldOpenChartDisabled');
    else
      FAppModules.SetMenuItem(CYRCFYOpenChart, AAction);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFirmYieldMenuItemManager.SetMenuSaveChart(AAction: TMenuSetAction);
const OPNAME = 'TFirmYieldMenuItemManager.SetMenuSaveChart';
begin
  try
    FToolBar.SetSaveChartState(AAction = msEnable);
    {case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCFYSaveChart, AAction, 'FirmYieldSaveChartDisabled');
    else
      FAppModules.SetMenuItem(CYRCFYSaveChart, AAction);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldMenuItemManager.SetMenuAddSeries(AAction: TMenuSetAction);
const OPNAME = 'TFirmYieldMenuItemManager.SetMenuAddSeries';
begin
  try
    FToolBar.SetAddSeriesState(AAction = msEnable);
    {case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCFYAddSeries, AAction, 'FirmYieldAddSeriesDisabled');
    else
      FAppModules.SetMenuItem(CYRCFYAddSeries, AAction);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFirmYieldMenuItemManager.SetMenuDeleteSeries(AAction: TMenuSetAction);
const OPNAME = 'TFirmYieldMenuItemManager.SetMenuDeleteSeries';
begin
  try
    FToolBar.SetDeleteSeriesState(AAction = msEnable);
    {case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCFYDeleteSeries, AAction, 'FirmYieldDeleteSeriesDisabled');
    else
      FAppModules.SetMenuItem(CYRCFYDeleteSeries, AAction);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
