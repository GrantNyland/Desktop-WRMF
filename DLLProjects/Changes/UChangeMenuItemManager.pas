{******************************************************************************}
{*  UNIT      : Contains the class TChangeMenuItemManager.                    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/16                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  Vcl.Dialogs,
  Vcl.Menus,
  Classes,
  Windows;

type
  TChangeMenuItemManager = class(TMenuItemManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    procedure Show; override;
    procedure SetParameterChanges (AEnabled : boolean);
  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CChangeParameter : array[0..1] of string = ('View','CLParameter');

procedure TChangeMenuItemManager.AddMenuItems;
const OPNAME = 'TChangeMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CChangeParameter, 1010, CmeChangeParameter, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeMenuItemManager.CreateMemberObjects;
const OPNAME = 'TChangeMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TChangeMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeMenuItemManager.Initialise: boolean;
const OPNAME = 'TChangeMenuItemManager.Initialise';
begin
  Result := false;
  try
    SetParameterChanges(FALSE);
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TChangeMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeMenuItemManager.Show;
const OPNAME = 'TChangeMenuItemManager.Show';
begin
  try
    inherited Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TChangeMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeMenuItemManager.SetParameterChanges(AEnabled: boolean);
const OPNAME = 'TChangeMenuItemManager.SetParameterChanges';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CChangeParameter, msEnable)
    else
      FAppModules.SetMenuItem(CChangeParameter, msDisable, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
