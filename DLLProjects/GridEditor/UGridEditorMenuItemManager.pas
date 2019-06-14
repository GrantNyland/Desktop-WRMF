//
//
//  UNIT      : Contains TGridEditorMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorMenuItemManager;

interface

uses
  UGridEditorToolBar,
  UMenuItemManager;

type
  TGridEditorMenuAction = (geRotateView);
  TGridEditorMenuData = class(TObject)
  public
    Action: TGridEditorMenuAction;
    constructor Create(AAction: TGridEditorMenuAction);
  end;

  TGridEditorMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TGridEditorToolBar;
    FIsReadOnly: boolean;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AddMenuItems; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetPivotState(AEnabled: boolean);
    property ToolBar: TGridEditorToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UAbstractObject,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TGridEditorMenuData }

const
  CData : array[0..0] of string = ('Data');
  CPivot: array[0..1] of string = ('Data','RotateView');

constructor TGridEditorMenuData.Create(AAction: TGridEditorMenuAction);
const OPNAME = 'TGridEditorMenuItemManager.Create';
begin
  try
    inherited Create;
    Action := AAction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGridEditorMenuItemManager }

procedure TGridEditorMenuItemManager.AddMenuItems;
const OPNAME = 'TGridEditorMenuItemManager.AddMenuItems';
begin
  try
    AddMenuItemEntry(CData,  800);
    AddMenuItemEntry(CPivot, 150, CmeTabSheetCustomEvent, TGridEditorMenuData.Create(geRotateView));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorMenuItemManager.CreateMemberObjects;
const OPNAME = 'TGridEditorMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIsReadOnly := True;
    FToolBar := TGridEditorToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TGridEditorMenuItemManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FToolBar);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorMenuItemManager.Initialise: boolean;
const OPNAME = 'TGridEditorMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try
    SetPivotState(False);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorMenuItemManager.SetPivotState(AEnabled: boolean);
const OPNAME = 'TGridEditorMenuItemManager.SetPivotState';
begin
  try
    if AEnabled then
      if(not (FAppModules.User.UserRights in CUR_EditData)) or
        FAppModules.StudyArea.ScenarioLocked then
          AEnabled := False;

    if AEnabled then
      FAppModules.SetMenuItem(CPivot, msEnable)
    else
      FAppModules.SetMenuItem(CPivot, msDisable, 'ActionPivotDisabled');
    FToolBar.SetPivotState(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TGridEditorMenuItemManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
