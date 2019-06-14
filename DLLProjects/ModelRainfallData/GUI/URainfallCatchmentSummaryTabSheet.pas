unit URainfallCatchmentSummaryTabSheet;


interface
uses
  VCL.Controls,
  UAbstractComponent,
  URainfallCatchmentSummaryValidator,
  UMenuItemManager,
  URainfallCatchmentSummaryMenuItemManager,
  UAbstractObject,
  UGenericModelLinkClasses;
type
  TRainfallCatchmentSummaryTabSheet = class(TAbstractTabSheet)

  protected
    FRainfallCatchmentSummaryValidator : TRainfallCatchmentSummaryValidator;
    FMenuItemManager             : TMenuItemManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetToolBar : TAbstractToolBar; override;
    function GetMenuItemManager : TRainfallCatchmentSummaryMenuItemManager;
  public
    function Initialise : boolean; override;
    function LanguageHasChanged : boolean; override;
    function StudyHasChanged : boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    procedure TabHasChanged; override;
    procedure SetMenuVisible (AVisible : Boolean); override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    property MenuItemManager: TRainfallCatchmentSummaryMenuItemManager read GetMenuItemManager;
end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations;

{ URainfallCatchmentSummaryTabSheet }

procedure TRainfallCatchmentSummaryTabSheet.CreateMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey := 'CatchmentResults';
    FRainfallCatchmentSummaryValidator := TRainfallCatchmentSummaryValidator.Create(Self, FAppModules);
    FRainfallCatchmentSummaryValidator.Panel.Parent := Self;
    FRainfallCatchmentSummaryValidator.Panel.Align  := alClient;

    FMenuItemManager := TRainfallCatchmentSummaryMenuItemManager.Create(FAppModules);
    FRainfallCatchmentSummaryValidator.MenuItemManager := TRainfallCatchmentSummaryMenuItemManager(FMenuItemManager);
    MenuItemManager.ToolBar.ToggleTopGridBtn.OnClick   := FRainfallCatchmentSummaryValidator.DoToggleTopGrid;
    MenuItemManager.ToolBar.ToggleMiddleGridBtn.OnClick:= FRainfallCatchmentSummaryValidator.DoToggleMiddleGrid;
    MenuItemManager.ToolBar.ToggleBottomGridBtn.OnClick:= FRainfallCatchmentSummaryValidator.DoToggleBottomGrid;
    MenuItemManager.ToolBar.ExportDataBtn.OnClick      := FRainfallCatchmentSummaryValidator.DoExportData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryTabSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.DestroyMemberObjects';
begin
  try
    FreeAndNil(FRainfallCatchmentSummaryValidator);
    FreeAndNil(FMenuItemManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.Initialise: boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FRainfallCatchmentSummaryValidator.Initialise;
    FAppModules.SetMenuItem(CToggleStationUsedGrid,    msChecked);
    FAppModules.SetMenuItem(CToggleCatchmentInputGrid, msChecked);
    FAppModules.SetMenuItem(CToggleRunGrid,            msChecked);
    SetMenuVisible(FALSE);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                               ANewValue: string): Boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    if (Self.PageControl.ActivePage = Self) then
      Result := FRainfallCatchmentSummaryValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if (Self.PageControl.ActivePage = Self) then
      Result := FRainfallCatchmentSummaryValidator.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryTabSheet.TabHasChanged;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.TabHasChanged';
begin
  inherited TabHasChanged;
  try
    FRainfallCatchmentSummaryValidator.PopulateDataViewer;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.GetMenuItemManager: TRainfallCatchmentSummaryMenuItemManager;
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallCatchmentSummaryMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryTabSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME ='TRainfallCatchmentSummaryTabSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
  if (ACustomModelEvent.Action = meToggleStationUsedGrid) then
  begin
      FRainfallCatchmentSummaryValidator.DoToggleTopGrid(Self);
      Result := True;
  end
  else
  if (ACustomModelEvent.Action = meToggleCatchmentOutputGrid) then
  begin
    FRainfallCatchmentSummaryValidator.DoToggleMiddleGrid(Self);
    Result := True;
  end
  else
  if (ACustomModelEvent.Action = meToggleRUNGrid) then
  begin
    FRainfallCatchmentSummaryValidator.DoToggleBottomGrid(Self);
    Result := True;
  end
  else
  if (ACustomModelEvent.Action = meExportCatchmentData) then
  begin
    FRainfallCatchmentSummaryValidator.DoExportData(Self);
    Result := True;
  end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryTabSheet.SetMenuVisible(AVisible: Boolean);
const OPNAME = 'TRainfallCatchmentSummaryTabSheet.SetMenuVisible';
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

end.
