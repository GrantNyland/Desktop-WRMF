{******************************************************************************}
{* UNIT : Contains the class UHydrologyModelMenuItemManager.                  *}
{******************************************************************************}

unit UHydrologyModelMenuItemManager;

interface

uses
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UHydrologyModelToolbar,
//  UAppModulesConstructionGeneric,
  UDataModelMenuItemManager;

  type
  THydrologyModelMenuItemManager = class(TDataModelMenuItemManager)
  protected
    FToolBar : THydrologyModelToolbar;
    procedure CreateToolBar;override;
    procedure DestroyMemberObjects; override;
  public
    constructor Create(AAppModules: TAppModules; AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded: boolean); reintroduce;
    function Initialise: Boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure AddMenuItems; override;
    procedure TabHasChanged (AGridTabSelected: boolean);
    procedure TreeNodeHasChanged (ADataType  : string;  AElementID : integer);

    procedure SetMenuValidateFiles(AAction: TMenuSetAction);

    property ToolBar: THydrologyModelToolbar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UConstants,
  VoaimsCom_TLB,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CModel                        : array[0..0] of string = ('Model');
  CValidateFiles                : array[0..1] of string = ('Model','ValidateFiles');

{* THydrologyModelMenuItemManager *********************************************}

constructor THydrologyModelMenuItemManager.Create(AAppModules: TAppModules;
  AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded: boolean);
const OPNAME = 'THydrologyModelMenuItemManager.Create';
begin
  try
    inherited Create(AAppModules, AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelMenuItemManager.AddMenuItems;
const OPNAME = 'THydrologyModelMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CModel,            600);
    AddMenuItemEntry(CValidateFiles,    610, CmeValidateFiles,nil);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelMenuItemManager.DestroyMemberObjects;
const OPNAME = 'THydrologyModelMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  try
    if Assigned(FAppModules.MainForm()) then
      FAppModules.MainForm.RemoveSystemChildToolBar(FToolBar)
    else
    FToolBar.Parent := nil;
    FreeAndNil(FToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModelMenuItemManager.Initialise: Boolean;
const OPNAME = 'THydrologyModelMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetMenuValidateFiles(msDisable);

    FAppModules.MainForm.AddSystemChildToolBar(FToolBar);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelMenuItemManager.CreateToolBar;
const OPNAME = 'THydrologyModelMenuItemManager.CreateToolBar';
begin
  try
    FToolBar := THydrologyModelToolbar.Create(nil,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModelMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'THydrologyModelMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyModelMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'THydrologyModelMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Initialise;
    Result := FToolBar.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelMenuItemManager.SetMenuValidateFiles(AAction: TMenuSetAction);
const OPNAME = 'THydrologyModelMenuItemManager.SetMenuValidateFiles';
begin
  try
    FToolBar.SetValidateFilesState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CValidateFiles, AAction, 'ValidateFilesDisabled');
    else
      FAppModules.SetMenuItem(CValidateFiles, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelMenuItemManager.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'THydrologyModelMenuItemManager.TabHasChanged';
{var
  LAction: TMenuSetAction;}
begin
  try
    FToolBar.TabHasChanged(AGridTabSelected);

{    if AGridTabSelected then
      LAction := msShow
     else
      LAction := msHide;}

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyModelMenuItemManager.TreeNodeHasChanged (ADataType  : string;
                                                         AElementID : integer);
const OPNAME = 'THydrologyModelMenuItemManager.TreeNodeHasChanged';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

