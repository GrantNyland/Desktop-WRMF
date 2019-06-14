unit UModelYRCMenuItem;

interface
uses

  UMenuItemManager,
  UHelpContexts,
  UChildToolBar;

type
  TModelYRCToolBar = class ( TChildToolBar )
  protected
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
  end;

type
  TModelYRCMenuItem = class ( TMenuItemManager )
  protected
    FToolBar: TModelYRCToolBar;
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    property  ToolBar: TModelYRCToolBar read FToolBar;
  end;


implementation

uses
  SysUtils,
  UAbstractComponent,
  UVaalDBMSMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

const
  CYRC : array [ 0..0 ] of string = ( 'YRC' );

{ TModelYRCMenuItem }

procedure TModelYRCMenuItem.AddMenuItems;
const OPNAME = 'TModelYRCMenuItem.AddMenuItems';
begin
  inherited;
  try
   AddMenuItemEntry( CYRC,600 );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCMenuItem.CreateMemberObjects;
const OPNAME = 'TModelYRCMenuItem.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCMenuItem.DestroyMemberObjects;
const OPNAME = 'TModelYRCMenuItem.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;

  try

  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCMenuItem.DisableAllMenus;
const OPNAME = 'TModelYRCMenuItem.DisableAllMenus';
begin
  try
  {do nothing }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCMenuItem.Initialise: boolean;
const OPNAME = 'TModelYRCMenuItem.Initialise';
begin
  result := false;
  try
  result := true;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelYRCMenuItem.LanguageHasChanged: boolean;
const OPNAME = 'TModelYRCMenuItem.LanguageHasChanged';
begin
  result := false;
  try
    result := false;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCMenuItem.StudyHasChanged: boolean;
const OPNAME = 'TModelYRCMenuItem.StudyHasChanged';
begin
  result := false;
  try
    result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TModelYRCToolBar }

procedure TModelYRCToolBar.AssignHelpContext;
const OPNAME = 'TModelYRCToolBar.AssignHelpContext';
begin
  inherited;
  try

  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCToolBar.CreateMemberObjects;
const OPNAME = 'TModelYRCToolBar.CreateMemberObjects';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelYRCToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TModelYRCToolBar.LanguageHasChanged';
begin
  result := false;
  try

  except
    on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelYRCToolBar.SetHorizontalPositions;
const OPNAME = 'TModelYRCToolBar.SetHorizontalPositions';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
 