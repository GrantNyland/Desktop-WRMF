{******************************************************************************}
{*  UNIT      : Contains TRainfallZoneSheet Class                             *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 04/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallZoneSheet;

interface

uses
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  URainfallZoneMenuItemManager,
  URainfallZoneValidator,
  UGenericModelLinkClasses;

type
  { Rain TRainfallZoneSheet class }
  TRainfallZoneSheet = class(TAbstractTabSheet)
  protected
    FMenuItemManager       : TMenuItemManager;
    FRainfallZoneValidator : TRainfallZoneValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMenuItemManager : TRainfallZoneMenuItemManager;
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
    procedure ProcessCustomEvent ( AData: TObject ); override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure SetMenuVisible (AVisible : Boolean); override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    property MenuItemManager : TRainfallZoneMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.Printers,
  UConstants,
  UErrorHandlingOperations;

{ TRainfallZoneSheet }

procedure TRainfallZoneSheet.CreateMemberObjects;
const OPNAME = 'TRainfallZoneSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey := 'RainfallZoneAdmin';
    FRainfallZoneValidator := TRainfallZoneValidator.Create(Self, FAppModules);
    FRainfallZoneValidator.Panel.Parent := Self;
    FRainfallZoneValidator.Panel.Align  := alClient;

    FMenuItemManager := TRainfallZoneMenuItemManager.Create(FAppModules);
    FRainfallZoneValidator.MenuItemManager := TRainfallZoneMenuItemManager(FMenuItemManager);

    MenuItemManager.ToolBar.AddToZoneBtn.OnClick      := FRainfallZoneValidator.DoAddToZone;
    MenuItemManager.ToolBar.CreateCatchmentZoneBtn.OnClick      := FRainfallZoneValidator.DoCreateCatchmentFile;
    MenuItemManager.ToolBar.DeleteCatchmentZoneBtn.OnClick      := FRainfallZoneValidator.DoDeleteCatchmentFile;
    MenuItemManager.ToolBar.RemoveFromZoneBtn.OnClick := FRainfallZoneValidator.DoDeleteFromZone;
    MenuItemManager.ToolBar.ToggleTreeBtn.OnClick     := FRainfallZoneValidator.DoToggleTree;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallZoneSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallZoneSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FRainfallZoneValidator);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallZoneSheet.ProcessCustomEvent(AData : TObject);
const OPNAME = 'TRainfallZoneSheet.ProcessCustomEvent';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallZoneSheet.Initialise: boolean;
const OPNAME = 'TRainfallZoneSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FRainfallZoneValidator.Initialise;
    TRainfallZoneMenuItemManager(FMenuItemManager).SetAddToZone(False);
    TRainfallZoneMenuItemManager(FMenuItemManager).SetRemoveFromZone(False);
    TRainfallZoneMenuItemManager(FMenuItemManager).SetCreateCatchmentZone(False);
    TRainfallZoneMenuItemManager(FMenuItemManager).SetDeleteCatchmentZone(False);

    FAppModules.SetMenuItem(CAddGaugeToZone, msChecked);
    FAppModules.SetMenuItem(CRemoveGaugeFromZone, msChecked);
    FAppModules.SetMenuItem(CCreateCatchmentZone, msChecked);
    FAppModules.SetMenuItem(CDeleteCatchmentZone, msChecked);
    SetMenuVisible(FALSE);
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallZoneSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallZoneSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallZoneSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FRainfallZoneValidator.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                                 AFieldName : string;
                                                 AOldValue  : string;
                                                 ANewValue  : string): boolean;
const OPNAME = 'TRainfallZoneSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    Result := FRainfallZoneValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneSheet.TabHasChanged;
const OPNAME = 'TRainfallZoneSheet.TabHasChanged';
begin
  inherited TabHasChanged;
  try
    FRainfallZoneValidator.PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneSheet.CanPrint: Boolean;
const OPNAME = 'TRainfallZoneSheet.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallZoneSheet.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallZoneSheet.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallZoneSheet.CanExport: Boolean;
const OPNAME = 'TRainfallZoneSheet.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneSheet.DoPrint;
const OPNAME = 'TRainfallZoneSheet.DoPrint';
begin
  try
    FRainfallZoneValidator.DoPrint;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneSheet.DoCopyToClipboard;
const OPNAME = 'TRainfallZoneSheet.DoCopyToClipboard';
begin
  try
    FRainfallZoneValidator.DoCopyToClipboard;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneSheet.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallZoneSheet.DoExport';
begin
  try
    FRainfallZoneValidator.DoExport(AFileName);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallZoneSheet.GetMenuItemManager : TRainfallZoneMenuItemManager;
const OPNAME = 'TRainfallZoneSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallZoneMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRainfallZoneSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TRainfallZoneSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if (ACustomModelEvent.Action = meAddGaugeToZone) then
    begin
      FRainfallZoneValidator.DoAddToZone(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meRemoveGaugeFromZone) then
    begin
      FRainfallZoneValidator.DoDeleteFromZone(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meToggleRainZoneTree) then
    begin
      FRainfallZoneValidator.DoToggleTree(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meCreateCatchmentZone) then
    begin
      FRainfallZoneValidator.DoCreateCatchmentFile(Self);
      Result := True;
    end
    else
    if (ACustomModelEvent.Action = meDeleteCatchmentZone) then
    begin
      FRainfallZoneValidator.DoDeleteCatchmentFile(Self);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneSheet.SetMenuVisible (AVisible : Boolean);
const OPNAME = 'TRainfallZoneSheet.SetMenuVisible';
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


