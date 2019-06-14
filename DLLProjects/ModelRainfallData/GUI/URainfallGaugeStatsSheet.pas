{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeStatsSheet Class                       *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 01/11/2004                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeStatsSheet;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  VCL.Controls,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  URainfallGaugeStatsMenuItemManager,
  URainfallGaugeStatsValidator,
  UGenericModelLinkClasses;

type

  TRainfallGaugeStatsSheet = class (TAbstractTabSheet)
  protected
    FMenuItemManager             : TMenuItemManager;
    FRainfallGaugeStatsValidator : TRainfallGaugeStatsValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMenuItemManager : TRainfallGaugeStatsMenuItemManager;
    function GetToolBar : TAbstractToolBar; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): boolean; override;
    procedure TabHasChanged; override;
    procedure SetMenuVisible (AVisible : Boolean);override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData): Boolean; override;
    property MenuItemManager: TRainfallGaugeStatsMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TRainfallGaugeStatsSheet }

procedure TRainfallGaugeStatsSheet.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeStatsSheet.CreateMemberObjects';
begin
  inherited;
  try
    FTabCaptionKey := 'GaugeStats';

    FRainfallGaugeStatsValidator := TRainfallGaugeStatsValidator.Create(Self, FAppModules);
    FRainfallGaugeStatsValidator.Panel.Parent := Self;
    FRainfallGaugeStatsValidator.Panel.Align  := alClient;

    FMenuItemManager := TRainfallGaugeStatsMenuItemManager.Create(FAppModules);
    FRainfallGaugeStatsValidator.MenuItemManager := TRainfallGaugeStatsMenuItemManager(FMenuItemManager);
    MenuItemManager.ToolBar.CreateFilesBtn.OnClick := FRainfallGaugeStatsValidator.OnClickCreateFiles;
    MenuItemManager.ToolBar.CreateSplitBtn.OnClick := FRainfallGaugeStatsValidator.DoCreateSplit;
    MenuItemManager.ToolBar.UpdateSplitBtn.OnClick := FRainfallGaugeStatsValidator.DoUpdateSplit;
    MenuItemManager.ToolBar.DeleteSplitBtn.OnClick := FRainfallGaugeStatsValidator.DoDeleteSplit;
    MenuItemManager.ToolBar.ToggleGridBtn.OnClick  := FRainfallGaugeStatsValidator.DoToggleGrid;
    MenuItemManager.ToolBar.ToggleGraphBtn.OnClick := FRainfallGaugeStatsValidator.DoToggleGraph;
    MenuItemManager.ToolBar.ToggleTreeBtn.OnClick  := FRainfallGaugeStatsValidator.DoToggleTree;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeStatsSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeStatsSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FRainfallGaugeStatsValidator);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeStatsSheet.Initialise: boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FRainfallGaugeStatsValidator.Initialise;

    TRainfallGaugeStatsMenuItemManager(FMenuItemManager).SetToggleGrid(TRUE);
    TRainfallGaugeStatsMenuItemManager(FMenuItemManager).SetToggleGraph(TRUE);
    TRainfallGaugeStatsMenuItemManager(FMenuItemManager).SetToggleTree(TRUE);

    FAppModules.SetMenuItem(CToggleGrid, msChecked);
    FAppModules.SetMenuItem(CToggleGraph, msChecked);
    FAppModules.SetMenuItem(CToggleTree, msChecked);

    SetMenuVisible(FALSE);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeStatsSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FRainfallGaugeStatsValidator.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                                       AFieldName : string;
                                                       AOldValue  : string;
                                                       ANewValue  : string): boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    Result := FRainfallGaugeStatsValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsSheet.TabHasChanged;
const OPNAME = 'TRainfallGaugeStatsSheet.TabHasChanged';
begin
  inherited;
  try
    FRainfallGaugeStatsValidator.PopulateDataViewer;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeStatsSheet.CanPrint: Boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeStatsSheet.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeStatsSheet.CanExport: Boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsSheet.DoPrint;
const OPNAME = 'TRainfallGaugeStatsSheet.DoPrint';
begin
  try
    FRainfallGaugeStatsValidator.DoPrint;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsSheet.DoCopyToClipboard;
const OPNAME = 'TRainfallGaugeStatsSheet.DoCopyToClipboard';
begin
  try
    FRainfallGaugeStatsValidator.DoCopyToClipboard;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsSheet.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallGaugeStatsSheet.DoExport';
begin
  try
    FRainfallGaugeStatsValidator.DoExport(AFileName);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeStatsSheet.GetMenuItemManager: TRainfallGaugeStatsMenuItemManager;
const OPNAME = 'TRainfallGaugeStatsSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallGaugeStatsMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRainfallGaugeStatsSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsSheet.SetMenuVisible (AVisible : Boolean);
const OPNAME = 'TRainfallGaugeStatsSheet.SetMenuVisible';
begin
  inherited SetMenuVisible(AVisible);
  try
    if Assigned(MenuItemManager) then
    begin
      if AVisible then
        MenuItemManager.Show
      else
        MenuItemManager.Hide;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeStatsSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TRainfallGaugeStatsSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if (ACustomModelEvent.Action = meCreateFiles) then
    begin
      FRainfallGaugeStatsValidator.OnClickCreateFiles(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainStatsGrid) then
    begin
      FRainfallGaugeStatsValidator.DoToggleGrid(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainStatsGraph) then
    begin
      FRainfallGaugeStatsValidator.DoToggleGraph(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainStatsTree) then
    begin
      FRainfallGaugeStatsValidator.DoToggleTree(Self);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



