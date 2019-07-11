unit USedimentationMenuItemManager;


interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  USediMenuItemToolBar,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  TSedimentationMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TSediMenuItemToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;

    procedure SetCreateDamSedimentation(AEnabled: boolean);
    procedure SetDeleteDamSedimentation(AEnabled: boolean);
    procedure SetSaveDamSedimentation(AEnabled: boolean);
    property ToolBar: TSediMenuItemToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CDamSedimentationData                : array[0..0] of WideString = ('Data');
  CCreateDamSedimentation              : array[0..1] of WideString = ('Data','CreateDamSedimentation');
  CDeleteDamSedimentation              : array[0..1] of WideString = ('Data','DeleteDamSedimentation');
  CSaveDamSedimentation                : array[0..1] of WideString = ('Data','SaveDamSedimentation');
  CHelpDamSedimentationUserGuide       : array[0..1] of WideString = ('Help','HelpSediUserGuide');

procedure TSedimentationMenuItemManager.AddMenuItems;
const OPNAME = 'TSedimentationMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CDamSedimentationData,  800);
    AddMenuItemEntry(CCreateDamSedimentation, 820, CmeCreateDamSedimentation);
    AddMenuItemEntry(CDeleteDamSedimentation, 830, CmeDeleteDamSedimentation);
    AddMenuItemEntry(CSaveDamSedimentation,   840, CmeSaveDamSedimentation);
    AddMenuItemEntry(CHelpDamSedimentationUserGuide, 230, CmeSediUserGuide);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TSedimentationMenuItemManager.CreateMemberObjects;
const OPNAME = 'TSedimentationMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TSediMenuItemToolBar.Create(nil, FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSedimentationMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TSedimentationMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationMenuItemManager.DisableAllMenus;
const OPNAME = 'TSedimentationMenuItemManager.DisableAllMenus';
begin
  try
    SetCreateDamSedimentation(False);
    SetDeleteDamSedimentation(False);
    SetSaveDamSedimentation(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationMenuItemManager.Initialise: boolean;
const OPNAME = 'TSedimentationMenuItemManager.Initialise';
begin
  Result := false;
  try
    DisableAllMenus;
    SetCreateDamSedimentation(True);
    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TSedimentationMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSedimentationMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TSedimentationMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try

    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationMenuItemManager.SetCreateDamSedimentation(AEnabled: boolean);
const OPNAME = 'TSedimentationMenuItemManager.SetCreateDamSedimentation';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateDamSedimentation, msEnable)
    else
      FAppModules.SetMenuItem(CCreateDamSedimentation, msDisable, 'CreateDamSedimentationDisabled');
    FToolBar.SetCreateDamSedimentation(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationMenuItemManager.SetDeleteDamSedimentation(AEnabled: boolean);
const OPNAME = 'TSedimentationMenuItemManager.SetDeleteDamSedimentation';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteDamSedimentation, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteDamSedimentation, msDisable, 'DeleteDamSedimentationDisabled');
    FToolBar.SetDeleteDamSedimentation(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationMenuItemManager.SetSaveDamSedimentation(AEnabled: boolean);
const OPNAME = 'TSedimentationMenuItemManager.SetSaveDamSedimentation';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CSaveDamSedimentation, msEnable)
    else
      FAppModules.SetMenuItem(CSaveDamSedimentation, msDisable, 'SaveDamSedimentationDisabled');
    FToolBar.SetSaveDamSedimentation(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

