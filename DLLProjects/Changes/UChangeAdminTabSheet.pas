{******************************************************************************}
{*  UNIT      : Contains the class TChangeAdminTabSheet.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/11                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeAdminTabSheet;

interface

uses
  Vcl.Menus,
  Classes,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Contnrs,
  Vcl.Controls,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UChangeAdminMenuItemManager,
  UChangeAdminToolBar,
  UDataComponent,
  UTreeViewTabSheet;
type

  TChangeAdminTabSheet = class(TAbstractTabSheet)
  protected
    FMenuItemManager   : TMenuItemManager;
    FValidator:TAbstractDataDialogValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    function GetMenuItemManager : TChangeAdminMenuItemManager;
    procedure DoShow; override;
    procedure InitialiseMenuItems;
  public
    function Initialise: boolean; override;
    procedure TabHasChanged; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure SetMenuVisible (AVisible : Boolean); override;
    property MenuItemManager  : TChangeAdminMenuItemManager  read GetMenuItemManager;
    property Validator       : TAbstractDataDialogValidator read FValidator write FValidator;
  end;

implementation

{$WARN UNIT_PLATFORM OFF}

uses
  UConstants,
  Windows,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ImgList,
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations, Math;

{ TChangeAdminTabSheet }

procedure TChangeAdminTabSheet.CreateMemberObjects;
const OPNAME = 'TChangeAdminTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey   := 'ChangeList';
    FMenuItemManager := TChangeAdminMenuItemManager.Create(FAppModules);
    FValidator       := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChangeAdminTabSheet.DestroyMemberObjects;
const OPNAME = 'TChangeAdminTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
    FValidator := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeAdminTabSheet.Initialise: boolean;
const OPNAME = 'TChangeAdminTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    InitialiseMenuItems;
    SetMenuVisible(FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeAdminTabSheet.TabHasChanged;
const OPNAME = 'TChangeAdminTabSheet.TabHasChanged';
begin
  inherited;
  try
    FAppModules.Changes.TabHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChangeAdminTabSheet.AssignHelpContext;
const OPNAME = 'TChangeAdminTabSheet.AssignHelpContext';
begin
  try
{    FValidationPanel.AssignHelpContext;
    FArrayPanel.AssignHelpContext;
    FFieldPanel.AssignHelpContext;}
    {SetControlHelpContext(Self,HC_FileSelection);
    SetControlHelpContext(FTreeView,HC_FileSelectionTreeView);
    SetControlHelpContext(FRichEdit,HC_FileViewer);}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeAdminTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TChangeAdminTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeAdminTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TChangeAdminTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FMenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeAdminTabSheet.GetMenuItemManager: TChangeAdminMenuItemManager;
const OPNAME = 'TChangeAdminTabSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TChangeAdminMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminTabSheet.GetToolbar : TAbstractToolBar;
const OPNAME = 'TChangeAdminTabSheet.GetToolbar';
begin
  Result := nil;
  try
    Result := TChangeAdminMenuItemManager(FMenuItemManager).ToolBar;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminTabSheet.SetMenuVisible (AVisible : Boolean );
const OPNAME = 'TChangeAdminTabSheet.SetMenuVisible';
begin
  inherited SetMenuVisible(AVisible);
  try
    if Assigned(MenuItemManager) then
    begin
      if AVisible then
        MenuItemManager.Show
      else
        MenuItemManager.Hide;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminTabSheet.DoShow;
const OPNAME = 'TChangeAdminTabSheet.DoShow';
begin
  try
    try
      InitialiseMenuItems;
    finally
      inherited DoShow;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminTabSheet.InitialiseMenuItems;
const OPNAME = 'TChangeAdminTabSheet.InitialiseMenuItems';
begin
  try
{
    TChangeAdminMenuItemManager(FMenuItemManager).SetCreateNewChangeGroup(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetDeleteChangeGroup(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetCreateNewChangeList(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetDeleteChangeList(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetCopyChangeList(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetMoveUpChangeElement(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetMoveDownChangeElement(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetActivateChangeElement(TRUE);
    TChangeAdminMenuItemManager(FMenuItemManager).SetDeactivateChangeElement(TRUE);
}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminTabSheet.CanCopyToCLipboard: boolean;
const OPNAME = 'TChangeAdminTabSheet.CanCopyToCLipboard';
begin
  Result := False;
  try
    if Assigned(FValidator) then
      Result := FValidator.CanCopyToCLipboard;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminTabSheet.CanExport: boolean;
const OPNAME = 'TChangeAdminTabSheet.CanExport';
begin
  Result := False;
  try
    if Assigned(FValidator) then
      Result := FValidator.CanExport;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminTabSheet.CanPrint: boolean;
const OPNAME = 'TChangeAdminTabSheet.CanPrint';
begin
  Result := False;
  try
    if Assigned(FValidator) then
      Result := FValidator.CanPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminTabSheet.DoCopyToCLipboard;
const OPNAME = 'TChangeAdminTabSheet.DoCopyToCLipboard';
begin
  try
    if Assigned(FValidator) then
      FValidator.DoCopyToCLipboard;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TChangeAdminTabSheet.DoExport';
begin
  try
    if Assigned(FValidator) then
      FValidator.DoExport(AFileName);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminTabSheet.DoPrint;
const OPNAME = 'TChangeAdminTabSheet.DoPrint';
begin
  try
    if Assigned(FValidator) then
      FValidator.DoPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

