{******************************************************************************}
{*  UNIT      : Contains the class TMetaDataMenuItemManager.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UMetaDataMenuItemManager;

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
  TMetaDataMenuItemManager = class(TMenuItemManager)
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
  UConstants,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
//  CChangeParameter : array[0..1] of WideString = ('Changes','CLParameter');
  CMetaData : array[0..1] of WideString = ('View','MetaData');

procedure TMetaDataMenuItemManager.AddMenuItems;
const OPNAME = 'TMetaDataMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CMetaData, 1000, CmeMetaData, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMetaDataMenuItemManager.CreateMemberObjects;
const OPNAME = 'TMetaDataMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMetaDataMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TMetaDataMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataMenuItemManager.Initialise: boolean;
const OPNAME = 'TMetaDataMenuItemManager.Initialise';
begin
  Result := false;
  try
    SetParameterChanges(FALSE);
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TMetaDataMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataMenuItemManager.Show;
const OPNAME = 'TMetaDataMenuItemManager.Show';
begin
  try
    inherited Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TMetaDataMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataMenuItemManager.SetParameterChanges(AEnabled: boolean);
const OPNAME = 'TMetaDataMenuItemManager.SetParameterChanges';
begin
  try
    if AEnabled then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AEnabled := False;

    if AEnabled then
      FAppModules.SetMenuItem(CMetaData, msEnable)
    else
      FAppModules.SetMenuItem(CMetaData, msDisable, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
