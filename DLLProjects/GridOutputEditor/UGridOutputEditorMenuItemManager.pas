//
//
//  UNIT      : Contains TGridOutputEditorMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridOutputEditorMenuItemManager;

interface

uses
  UGridOutputEditorToolBar,
  UMenuItemManager;

type
  TGridOutputEditorMenuAction = (geRotateView);
  TGridOutputEditorMenuData = class(TObject)
  public
    Action: TGridOutputEditorMenuAction;
    constructor Create(AAction: TGridOutputEditorMenuAction);
  end;

  TGridOutputEditorMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TGridOutPutEditorToolBar;
    FIsReadOnly: boolean;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AddMenuItems; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetPivotState(AEnabled: boolean);
    property ToolBar: TGridOutputEditorToolBar read FToolBar;
  end;

implementation

uses
  SysUtils,
  UAbstractObject,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TGridOutputEditorMenuData }

const

//  CData : array[0..0] of WideString = ('Data');
  CPivot: array[0..1] of WideString = ('Data','RotateView');

constructor TGridOutputEditorMenuData.Create(AAction: TGridOutputEditorMenuAction);
const OPNAME = 'TGridOutputEditorMenuItemManager.Create';
begin
  try
    inherited Create;
    Action := AAction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGridOutputEditorMenuItemManager }

procedure TGridOutputEditorMenuItemManager.AddMenuItems;
const OPNAME = 'TGridOutputEditorMenuItemManager.AddMenuItems';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorMenuItemManager.CreateMemberObjects;
const OPNAME = 'TGridOutputEditorMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIsReadOnly := True;
    FToolBar := TGridOutputEditorToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TGridOutputEditorMenuItemManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FToolBar);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorMenuItemManager.Initialise: boolean;
const OPNAME = 'TGridOutputEditorMenuItemManager.Initialise';
begin
  Result := inherited Initialise;
  try

    SetPivotState(False);
    Result := FToolBar.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TGridOutputEditorMenuItemManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorMenuItemManager.SetPivotState(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorMenuItemManager.SetPivotState';
begin
  try
    if AEnabled then
      FAppModules.SetMenuItem(CPivot, msEnable)
    else
      FAppModules.SetMenuItem(CPivot, msDisable, 'ActionPivotDisabled');
    FToolBar.SetPivotState(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
