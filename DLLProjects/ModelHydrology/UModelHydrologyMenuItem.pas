

unit UModelHydrologyMenuItem;

interface
uses

  UMenuItemManager,
  UHelpContexts,
  UChildToolBar,
  UAbstractObject;

type
  TModelHydrologyToolbar = class ( TChildToolBar )
  protected
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;

  public
    function LanguageHasChanged: boolean; override;
  end;

type
  TModelHydrologyMenuItem = class ( TMenuItemManager ) //TDataModelMenuItemManager
  protected
    FIsGridLoaded, FIsGraphLoaded : boolean;
    FToolBar: TModelHydrologyToolbar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    constructor Create ( AAppModules: TAppModules; AIsGridLoaded,
                         AIsGraphLoaded: boolean); reintroduce;
    procedure AddMenuItems; override;
    function Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    property  ToolBar: TModelHydrologyToolbar read FToolBar;
  end;


implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

const
  CHydrology :             array [ 0..1 ] of string = ( 'View','Hydrology' );
//  CViewEditGrid:           array[0..1] of string = ( 'View','ViewEditGrid' );
  CViewGraph:              array[0..1] of string = ( 'View','ViewGraph' );
  CViewTabSheetsSep:       array[0..1] of string = ( 'View','ViewTabSheetsSep' );

{ TModelHydrologyMenuItem }

procedure TModelHydrologyMenuItem.AddMenuItems;
const OPNAME = 'TModelHydrologyMenuItem.AddMenuItems';
begin
  inherited;
  try

   if FIsGraphLoaded then
      AddMenuItemEntry( CViewGraph, 220, CmeViewGraph );

    AddMenuItemEntry( CViewTabSheetsSep,  350, CmeSeparator);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TModelHydrologyMenuItem.Create ( AAppModules: TAppModules;
                                             AIsGridLoaded, AIsGraphLoaded: boolean );
const OPNAME = 'TModelHydrologyMenuItem.Create';
begin

  try
   FIsGridLoaded := AIsGridLoaded;
   FIsGraphLoaded := AIsGraphLoaded;
   inherited create ( AAppModules );

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TModelHydrologyMenuItem.CreateMemberObjects;
const OPNAME = 'TModelHydrologyMenuItem.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelHydrologyMenuItem.DestroyMemberObjects;
const OPNAME = 'TModelHydrologyMenuItem.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;

  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelHydrologyMenuItem.DisableAllMenus;
const OPNAME = 'TModelHydrologyMenuItem.DisableAllMenus';
begin
  try
  {do nothing }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyMenuItem.Initialise: boolean;
const OPNAME = 'TModelHydrologyMenuItem.Initialise';
begin
  result := false;
  try
  result := true;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelHydrologyMenuItem.LanguageHasChanged: boolean;
const OPNAME = 'TModelHydrologyMenuItem.LanguageHasChanged';
begin
  result := false;
  try
    result := false;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyMenuItem.StudyHasChanged: boolean;
const OPNAME = 'TModelHydrologyMenuItem.StudyHasChanged';
begin
  result := false;
  try
    result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TModelHydrologyToolbar }

procedure TModelHydrologyToolbar.AssignHelpContext;
const OPNAME = 'TModelHydrologyToolbar.AssignHelpContext';
begin
  inherited;
  try

  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelHydrologyToolbar.CreateMemberObjects;
const OPNAME = 'TModelHydrologyToolbar.CreateMemberObjects';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyToolbar.LanguageHasChanged: boolean;
const OPNAME = 'TModelHydrologyToolbar.LanguageHasChanged';
begin
  result := false;
  try

  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelHydrologyToolbar.SetHorizontalPositions;
const OPNAME = 'TModelHydrologyToolbar.SetHorizontalPositions';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
