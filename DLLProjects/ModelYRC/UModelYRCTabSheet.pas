unit UModelYRCTabSheet;

interface

uses
  DB,
  Menus,
  Classes,
  stdctrls,
  ComCtrls,
  contnrs,
  extctrls,
  CheckLst,
  Dialogs,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UModelYRCMenuItem,
  UTreeViewTabSheet;
type

  TModelYRCTabSheet = class ( TTreeViewTabSheet )
    protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;


  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function CanCopyToClipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToClipboard; override;
    procedure DoExport; override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent ( ACustomModelEvent: TModelMenuData ): boolean; override;
end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TModelYRCTabSheet }

function TModelYRCTabSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TModelYRCTabSheet.CanCopyToClipboard';
begin
  result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelYRCTabSheet.CanExport: boolean;
const OPNAME = 'TModelYRCTabSheet.CanExport';
begin
  result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelYRCTabSheet.CanPrint: boolean;
const OPNAME = 'TModelYRCTabSheet.CanPrint';
begin
  result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelYRCTabSheet.CreateMemberObjects;
const OPNAME = 'TModelYRCTabSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey := 'YRC';
    FMenuItemManager := TModelYRCMenuItem.Create ( FAppModules );
    Caption := FAppModules.Language.GetString ( 'TabCaption.' + FTabCaptionKey );
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TModelYRCTabSheet.DestroyMemberObjects;
const OPNAME = 'TModelYRCTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil ( FMenuItemManager );
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TModelYRCTabSheet.DoCopyToClipboard;
const OPNAME = 'TModelYRCTabSheet.DoCopyToClipboard';
begin
  inherited;
  { Do nothing }
end;

function TModelYRCTabSheet.DoCustomTabSheetEvent(
  ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TModelYRCTabSheet.DoCustomTabSheetEvent';
begin
  result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelYRCTabSheet.DoExport;
const OPNAME = 'TModelYRCTabSheet.DoExport';
begin
  inherited;
  { do nothing }
end;

procedure TModelYRCTabSheet.DoPrint;
const OPNAME = 'TModelYRCTabSheet.DoPrint';
begin
  inherited;
  { do nothing }
end;

function TModelYRCTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TModelYRCTabSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := TModelYRCMenuItem ( FMenuItemManager ).ToolBar;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelYRCTabSheet.Initialise: boolean;
const OPNAME = 'TModelYRCTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelYRCTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TModelYRCTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    result := true;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelYRCTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TModelYRCTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

