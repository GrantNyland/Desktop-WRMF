
//
//
//  UNIT      : Contains IFRMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit URWHMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  URWHMenuItemToolBar,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  RWHMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar : TRWHMenuItemToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;

    //procedure SetCreateRWHSite(AEnabled: boolean);
    //procedure SetDeleteRWHSite(AEnabled: boolean);
    //procedure SetSaveRWHSite(AEnabled: boolean);
    property ToolBar: TRWHMenuItemToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CRWHSiteData                : array[0..0] of string = ('Data');
  //CCreateRWHSite              : array[0..1] of string = ('Data','CreateIFRSite');
  //CDeleteRWHSite              : array[0..1] of string = ('Data','DeleteIFRSite');
  //CSaveRWHSite                : array[0..1] of string = ('Data','SaveIFRSite');
  CHelpRWHUserGuide           : array[0..1] of string = ('Help','HelpRWHUserGuide');

procedure RWHMenuItemManager.AddMenuItems;
const OPNAME = 'RWHMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CRWHSiteData,  800);
    //AddMenuItemEntry(CCreateRWHSite, 820, CmeCreateIFRSite);
    //AddMenuItemEntry(CDeleteRWHSite, 830, CmeDeleteIFRSite);
    //AddMenuItemEntry(CSaveRWHSite,   840, CmeSaveIFRSite);
    AddMenuItemEntry(CHelpRWHUserGuide, 230, CmeRWHUserGuide);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure RWHMenuItemManager.CreateMemberObjects;
const OPNAME = 'RWHMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FToolBar := TRWHMenuItemToolBar.Create(nil, FAppModules);
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure RWHMenuItemManager.DestroyMemberObjects;
const OPNAME = 'RWHMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar);
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure RWHMenuItemManager.DisableAllMenus;
const OPNAME = 'RWHMenuItemManager.DisableAllMenus';
begin
  try
    //SetCreateRWHSite(False);
    //SetDeleteRWHSite(False);
    //SetSaveRWHSite(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function RWHMenuItemManager.Initialise: boolean;
const OPNAME = 'RWHMenuItemManager.Initialise';
begin
  Result := false;
  try
    DisableAllMenus;
    //SetCreateRWHSite(True);
    //FAppModules.MainForm.AddSystemChildToolBar(FToolBar);
    //Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function RWHMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'RWHMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function RWHMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'RWHMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure RWHMenuItemManager.SetCreateRWHSite(AEnabled: boolean);
const OPNAME = 'RWHMenuItemManager.SetCreateRWHSite';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CCreateRWHSite, msEnable)
    else
      FAppModules.SetMenuItem(CCreateRWHSite, msDisable, 'CreateIFRSiteDisabled');
    FToolBar.SetCreateRWHSite(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure RWHMenuItemManager.SetDeleteRWHSite(AEnabled: boolean);
const OPNAME = 'RWHMenuItemManager.SetDeleteRWHSite';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CDeleteRWHSite, msEnable)
    else
      FAppModules.SetMenuItem(CDeleteRWHSite, msDisable, 'DeleteIFRSiteDisabled');
    FToolBar.SetDeleteRWHSite(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure RWHMenuItemManager.SetSaveRWHSite(AEnabled: boolean);
const OPNAME = 'RWHMenuItemManager.SetSaveRWHSite';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CSaveRWHSite, msEnable)
    else
      FAppModules.SetMenuItem(CSaveRWHSite, msDisable, 'SaveIFRSiteDisabled');
    FToolBar.SetSaveRWHSite(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.

