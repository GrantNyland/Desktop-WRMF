
//
//
//  UNIT      : Contains IFRMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  UIFRMenuItemToolBar,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  IFRMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TIFRMenuItemToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;

    procedure SetCreateIFRSite(AEnabled: boolean);
    procedure SetDeleteIFRSite(AEnabled: boolean);
    procedure SetSaveIFRSite(AEnabled: boolean);
    property ToolBar: TIFRMenuItemToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CIFRSiteData                : array[0..0] of string = ('Data');
  CCreateIFRSite              : array[0..1] of string = ('Data','CreateIFRSite');
  CDeleteIFRSite              : array[0..1] of string = ('Data','DeleteIFRSite');
  CSaveIFRSite                : array[0..1] of string = ('Data','SaveIFRSite');
  CHelpIFRUserGuide           : array[0..1] of string = ('Help','HelpIFRUserGuide');

procedure IFRMenuItemManager.AddMenuItems;
const OPNAME = 'IFRMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CIFRSiteData,  800);
    AddMenuItemEntry(CCreateIFRSite, 820, CmeCreateIFRSite);
    AddMenuItemEntry(CDeleteIFRSite, 830, CmeDeleteIFRSite);
    AddMenuItemEntry(CSaveIFRSite,   840, CmeSaveIFRSite);
    AddMenuItemEntry(CHelpIFRUserGuide, 230, CmeIFRUserGuide);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure IFRMenuItemManager.CreateMemberObjects;
const OPNAME = 'IFRMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TIFRMenuItemToolBar.Create(nil, FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure IFRMenuItemManager.DestroyMemberObjects;
const OPNAME = 'IFRMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure IFRMenuItemManager.DisableAllMenus;
const OPNAME = 'IFRMenuItemManager.DisableAllMenus';
begin
  try
    SetCreateIFRSite(False);
    SetDeleteIFRSite(False);
    SetSaveIFRSite(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function IFRMenuItemManager.Initialise: boolean;
const OPNAME = 'IFRMenuItemManager.Initialise';
begin
  Result := false;
  try
    DisableAllMenus;
    SetCreateIFRSite(True);
    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function IFRMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'IFRMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function IFRMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'IFRMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try

    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure IFRMenuItemManager.SetCreateIFRSite(AEnabled: boolean);
const OPNAME = 'IFRMenuItemManager.SetCreateIFRSite';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateIFRSite, msEnable)
    else
      FAppModules.SetMenuItem(CCreateIFRSite, msDisable, 'CreateIFRSiteDisabled');
    FToolBar.SetCreateIFRSite(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure IFRMenuItemManager.SetDeleteIFRSite(AEnabled: boolean);
const OPNAME = 'IFRMenuItemManager.SetDeleteIFRSite';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteIFRSite, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteIFRSite, msDisable, 'DeleteIFRSiteDisabled');
    FToolBar.SetDeleteIFRSite(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure IFRMenuItemManager.SetSaveIFRSite(AEnabled: boolean);
const OPNAME = 'IFRMenuItemManager.SetSaveIFRSite';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CSaveIFRSite, msEnable)
    else
      FAppModules.SetMenuItem(CSaveIFRSite, msDisable, 'SaveIFRSiteDisabled');
    FToolBar.SetSaveIFRSite(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

