//
//
//  UNIT      : Contains TRainTabSheetManager Class
//  AUTHOR    : Sam Dhlamini(arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallPatchAdminSheet;

interface

uses
  VCL.Controls,
  VCL.ComCtrls,
  UAbstractObject,
  UAbstractComponent,
  URainfallPatchAdminMenuItemManager,
  URainfallPatchAdminValidator,
  UMenuItemManager,
  UGenericModelLinkClasses;

type

  TRainfallPatchAdminSheet = class (TAbstractTabSheet)
  protected
    FMenuItemManager             : TMenuItemManager;
    FRainfallPatchAdminValidator : TRainfallPatchAdminValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMenuItemManager : TRainfallPatchAdminMenuItemManager;
    function GetToolBar: TAbstractToolBar; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    procedure TabHasChanged; override;
    procedure SetMenuVisible (AVisible: Boolean);override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent(ACustomModelEvent : TModelMenuData): Boolean; override;
    property MenuItemManager: TRainfallPatchAdminMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  VCL.Printers,

  UConstants,
  UDatasetType,
  UErrorHandlingOperations;

{ TRainfallPatchAdminSheet }

procedure TRainfallPatchAdminSheet.CreateMemberObjects;
const OPNAME = 'TRainfallPatchAdminSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey := 'PatchAdmin';

    FRainfallPatchAdminValidator := TRainfallPatchAdminValidator.Create(Self, FAppModules);
    FRainfallPatchAdminValidator.Panel.Parent := Self;
    FRainfallPatchAdminValidator.Panel.Align  := alClient;

    FMenuItemManager := TRainfallPatchAdminMenuItemManager.Create(FAppModules);
    FRainfallPatchAdminValidator.MenuItemManager := TRainfallPatchAdminMenuItemManager(FMenuItemManager);
    MenuItemManager.ToolBar.CreatePatchBtn.OnClick          := FRainfallPatchAdminValidator.DoCreatePatch;
    MenuItemManager.ToolBar.DeletePatchBtn.OnClick          := FRainfallPatchAdminValidator.DoDeletePatch;
    MenuItemManager.ToolBar.RenamePatchBtn.OnClick          := FRainfallPatchAdminValidator.DoUpdatePatch;
    MenuItemManager.ToolBar.AddGaugeToPatchBtn.OnClick      := FRainfallPatchAdminValidator.DoSetAddGaugeMode;
    MenuItemManager.ToolBar.RemoveGaugeFromPatchBtn.OnClick := FRainfallPatchAdminValidator.DoRemoveGauge;
    MenuItemManager.ToolBar.ToggleGridBtn.OnClick           := FRainfallPatchAdminValidator.DoToggleGrid;
    MenuItemManager.ToolBar.ToggleGraphBtn.OnClick          := FRainfallPatchAdminValidator.DoToggleGraph;
    MenuItemManager.ToolBar.ToggleTreeBtn.OnClick           := FRainfallPatchAdminValidator.DoToggleTree;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallPatchAdminSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallPatchAdminSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FRainfallPatchAdminValidator);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallPatchAdminSheet.Initialise: boolean;
const OPNAME = 'TRainfallPatchAdminSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FRainfallPatchAdminValidator.Initialise;

    FAppModules.SetMenuItem(CToggleGrid, msChecked);
    FAppModules.SetMenuItem(CToggleGraph, msChecked);
    FAppModules.SetMenuItem(CToggleTree, msChecked);

    SetMenuVisible(FALSE);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallPatchAdminSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallPatchAdminSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallPatchAdminSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallPatchAdminSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FRainfallPatchAdminValidator.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallPatchAdminSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                                       AFieldName : string;
                                                       AOldValue  : string;
                                                       ANewValue  : string): boolean;
const OPNAME = 'TRainfallPatchAdminSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    Result := FRainfallPatchAdminValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminSheet.TabHasChanged;
const OPNAME = 'TRainfallPatchAdminSheet.TabHasChanged';
begin
  inherited TabHasChanged;
  try
    FRainfallPatchAdminValidator.PopulateDataViewer;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminSheet.CanPrint: Boolean;
const OPNAME = 'TRainfallPatchAdminSheet.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminSheet.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallPatchAdminSheet.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminSheet.CanExport: Boolean;
const OPNAME = 'TRainfallPatchAdminSheet.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminSheet.DoPrint;
const OPNAME = 'TRainfallPatchAdminSheet.DoPrint';
begin
  try
    FRainfallPatchAdminValidator.DoPrint;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminSheet.DoCopyToClipboard;
const OPNAME = 'TRainfallPatchAdminSheet.DoCopyToClipboard';
begin
  try
    FRainfallPatchAdminValidator.DoCopyToClipboard;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminSheet.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallPatchAdminSheet.DoExport';
begin
  try
    FRainfallPatchAdminValidator.DoExport(AFileName);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminSheet.GetMenuItemManager : TRainfallPatchAdminMenuItemManager;
const OPNAME = 'TRainfallPatchAdminSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallPatchAdminMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminSheet.GetToolBar : TAbstractToolBar;
const OPNAME = 'TRainfallPatchAdminSheet.GetToolBar';
begin
  Result := nil;
  try
    Result :=  MenuItemManager.ToolBar;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminSheet.SetMenuVisible (AVisible : Boolean);
const OPNAME = 'TRainfallPatchAdminSheet.SetMenuVisible';
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

function TRainfallPatchAdminSheet.DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData): boolean;
const OPNAME = 'TRainfallPatchAdminSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if (ACustomModelEvent.Action = meCreatePatch) then
    begin
      FRainfallPatchAdminValidator.DoCreatePatch(nil);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meDeletePatch) then
    begin
      FRainfallPatchAdminValidator.DoDeletePatch(nil);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meRenamePatch) then
    begin
      FRainfallPatchAdminValidator.DoUpdatePatch(nil);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meAddGaugeToPatch) then
    begin
      FRainfallPatchAdminValidator.DoAddGauge(nil);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meRemoveGaugeFromPatch) then
    begin
      FRainfallPatchAdminValidator.DoRemoveGauge(nil);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainAdminGrid) then
    begin
      FRainfallPatchAdminValidator.DisplayGridGraph;
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainAdminGraph) then
    begin
      FRainfallPatchAdminValidator.DisplayGridGraph;
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainAdminTree) then
    begin
      FRainfallPatchAdminValidator.DoToggleTree(Self);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



