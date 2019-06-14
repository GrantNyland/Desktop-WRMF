//
//
//  UNIT      : Contains the class TMainFormManager.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMainFormManager;

interface

uses
  Classes,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  UMainFormMenuItemManager,
  UMainForm,
  UAbstractObject;

type
  TMainFormManager = class(TAbstractMainFormManager)
  protected
    FMainForm: TMainForm;
    FMainFormMenuItemManager: TMainFormMenuItemManager;
    FMainFormMenuItemManagerRef: IMainFormMenuItemManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMainFormClassReference: TComponentClass; override;
    function GetMainForm: TForm; override;
    procedure SetMainForm(AForm: TForm); override;
    function GetMenu: TMainMenu; override;
    function GetPageControl: TPageControl; override;
    function GetActivePage: TTabSheet; override;
    procedure SetActivePage(ATabSheet: TTabSheet); override;
    function GetToolBar: TCustomPanel; override;
    function GetChildToolBar: TCustomPanel; override;
    procedure SetChildToolBar(AChildToolBar: TCustomPanel); override;
    function GetMenuItemManager: IMainFormMenuItemManager; override;
    function GetStudyPanelVisible: boolean; override;
    procedure SetStudyPanelVisible(AVisible: boolean); override;
    function GetToolBarVisible: boolean; override;
    procedure SetToolBarVisible(AVisible: boolean); override;
    procedure OnFormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    procedure RefreshState; override;
    procedure ApplyPreviousTabIndex; override;
    procedure SaveCurrentTabIndex; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure AddSystemChildToolBar(ASystemChildToolBar:TCustomPanel);override;
    procedure RemoveSystemChildToolBar(ASystemChildToolBar: TCustomPanel);override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
    function  GetBtnWhatIsThis() : TObject; override;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TMainFormManager.CreateMemberObjects;
const OPNAME = 'TMainFormManager.CreateMemberObjects';
begin
  try
    GAppModules := FAppModules;
    inherited CreateMemberObjects;
    FMainFormMenuItemManager := TMainFormMenuItemManager.Create(FAppModules);
    FMainFormMenuItemManagerRef := GetMenuItemManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.DestroyMemberObjects;
const OPNAME = 'TMainFormManager.DestroyMemberObjects';
begin
  try
    FMainFormMenuItemManager.DeleteMenuItems;
    FMainFormMenuItemManagerRef := nil; // Calls free.
    FMainFormMenuItemManager := nil;
    if Assigned(FMainForm) then
    begin
      FAppModules.ViewIni.SaveFormView(FMainForm);
      FMainForm.AppModules := nil;
    end;
    FMainForm := nil;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetMainFormClassReference: TComponentClass;
const OPNAME = 'TMainFormManager.GetMainFormClassReference';
begin
  Result := TMainForm;
end;

function TMainFormManager.GetMainForm: TForm;
const OPNAME = 'TMainFormManager.GetMainForm';
begin
  Result := nil;
  try
    if Assigned(FMainForm) then
      Result := FMainForm;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.SetMainForm(AForm: TForm);
const OPNAME = 'TMainFormManager.SetMainForm';
begin
  try
    FMainForm := TMainForm(AForm);
    FMainFormMenuItemManager.SetParent(FMainForm);
    FMainForm.MainFormMenuItemManager := FMainFormMenuItemManager;
    FMainForm.OnCloseQuery := OnFormCloseQuery;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetMenu: TMainMenu;
const OPNAME = 'TMainFormManager.GetMenu';
begin
  Result := nil;
  try
    if Assigned(FMainForm) then
      Result := FMainForm.Menu;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetPageControl: TPageControl;
const OPNAME = 'TMainFormManager.GetPageControl';
begin
  Result := nil;
  try
    if Assigned(FMainForm) then
      Result := FMainForm.PageControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetActivePage: TTabSheet;
const OPNAME = 'TMainFormManager.GetActivePage';
begin
  Result := nil;
  try
    if Assigned(FMainForm) then
      Result := FMainForm.PageControl.ActivePage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.SetActivePage(ATabSheet: TTabSheet);
const OPNAME = 'TMainFormManager.SetActivePage';
begin
  try
    if Assigned(FMainForm) then
      FMainForm.PageControl.ActivePage := ATabSheet;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetToolBar: TCustomPanel;
const OPNAME = 'TMainFormManager.GetToolBar';
begin
  Result := nil;
  try
    if Assigned(FMainFormMenuItemManager) then
      Result := FMainFormMenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetChildToolBar: TCustomPanel;
const OPNAME = 'TMainFormManager.GetChildToolBar';
begin
  Result := nil;
  try
    if Assigned(FMainFormMenuItemManager) then
      if Assigned(FMainFormMenuItemManager.ToolBar) then
        Result := FMainFormMenuItemManager.ToolBar.ChildToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.SetChildToolBar(AChildToolBar: TCustomPanel);
const OPNAME = 'TMainFormManager.SetChildToolBar';
begin
  try
    if Assigned(FMainFormMenuItemManager) then
      if Assigned(FMainFormMenuItemManager.ToolBar) then
        FMainFormMenuItemManager.ToolBar.ChildToolBar := TAbstractToolBar(AChildToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetMenuItemManager: IMainFormMenuItemManager;
const OPNAME = 'TMainFormManager.GetMenuItemManager';
begin
  Result := nil;
  try
    if Assigned(FMainFormMenuItemManager) then
      Result := FMainFormMenuItemManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.RefreshState;
const OPNAME = 'TMainFormManager.RefreshState';
begin
  try
    if Assigned(FMainForm) then
      FMainForm.RefreshState;
    if Assigned(FMainFormMenuItemManager) then
      FMainFormMenuItemManager.RefreshState;

    FMainFormMenuItemManager.SetViewStudyPanelChecked(FMainForm.StudyPanel.Visible);
    FMainFormMenuItemManager.SetViewToolBarChecked(FMainFormMenuItemManager.IsToolBarVisible);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.ApplyPreviousTabIndex;
const OPNAME = 'TMainFormManager.ApplyPreviousTabIndex';
begin
  try
    if Assigned(FMainForm) then
      FMainForm.ApplyPreviousTabIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.SaveCurrentTabIndex;
const OPNAME = 'TMainFormManager.SaveCurrentTabIndex';
begin
  try
    if Assigned(FMainForm) then
      FMainForm.SaveCurrentTabIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.LanguageHasChanged: boolean;
const OPNAME = 'TMainFormManager.LanguageHasChanged';
begin
  Result := True;
  try
    if Assigned(FMainForm) then
      FMainForm.LanguageHasChanged;
    if Assigned(FMainFormMenuItemManager) then
      FMainFormMenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetStudyPanelVisible: boolean;
const OPNAME = 'TMainFormManager.GetStudyPanelVisible';
begin
  Result := False;
  try
    if Assigned(FMainForm) then
      if Assigned(FMainForm.StudyPanel) then
        Result := FMainForm.StudyPanel.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.SetStudyPanelVisible(AVisible: boolean);
const OPNAME = 'TMainFormManager.SetStudyPanelVisible';
begin
  try
    if Assigned(FMainForm) then
      if Assigned(FMainForm.StudyPanel) then
      begin
        FMainForm.StudyPanel.Visible := AVisible;
        FMainFormMenuItemManager.SetViewStudyPanelChecked(AVisible);
        FMainForm.StudyPanel.SaveState;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetToolBarVisible: boolean;
const OPNAME = 'TMainFormManager.GetToolBarVisible';
begin
  Result := False;
  try
    if Assigned(FMainFormMenuItemManager) then
      Result := FMainFormMenuItemManager.IsToolBarVisible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.SetToolBarVisible(AVisible: boolean);
const OPNAME = 'TMainFormManager.SetToolBarVisible';
begin
  try
    if Assigned(FMainFormMenuItemManager) then
    begin
      FMainFormMenuItemManager.ToolBar.Visible := AVisible;
      FMainFormMenuItemManager.SetViewToolBarChecked(AVisible);
      FMainFormMenuItemManager.ToolBar.SaveState;      
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.AddSystemChildToolBar(ASystemChildToolBar: TCustomPanel);
const OPNAME = 'TMainFormManager.AddSystemChildToolBar';
begin
  try
    if Assigned(FMainFormMenuItemManager) then
      if Assigned(FMainFormMenuItemManager.ToolBar) then
        FMainFormMenuItemManager.ToolBar.AddSystemChildToolBar(ASystemChildToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.RemoveSystemChildToolBar(ASystemChildToolBar: TCustomPanel);
const OPNAME = 'TMainFormManager.RemoveSystemChildToolBar';
begin
  try
    if Assigned(FMainFormMenuItemManager) then
      if Assigned(FMainFormMenuItemManager.ToolBar) then
        FMainFormMenuItemManager.ToolBar.RemoveSystemChildToolBar(ASystemChildToolBar);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.SaveState: boolean;
const OPNAME = 'TMainFormManager.SaveState';
begin
  Result := inherited SaveState;
  try
    if Assigned(FMainForm) then
       FMainForm.SaveState;
    if Assigned(FMainFormMenuItemManager) then
       FMainFormMenuItemManager.SaveState;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.ResetState: boolean;
const OPNAME = 'TMainFormManager.ResetState';
begin
  Result := inherited ResetState;
  try
    if Assigned(FMainForm) then
       FMainForm.ResetState;

    if Assigned(FMainFormMenuItemManager) then
       FMainFormMenuItemManager.ResetState;

    FMainFormMenuItemManager.SetViewToolBarChecked(
      FMainFormMenuItemManager.IsToolBarVisible);

    FMainFormMenuItemManager.SetViewStudyPanelChecked(
      StudyPanelVisible);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainFormManager.OnFormCloseQuery(Sender: TObject; var CanClose: Boolean);
const OPNAME = 'TMainFormManager.OnFormCloseQuery';
begin
  try
    SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TMainFormManager.ProcessEvent';
begin
  Result := False;
  try
    case AEventType of
      CmeHelpWhatIsThis:
      begin
        FMainFormMenuItemManager.SetMenuToggleWhatIsThisDown(not FMainFormMenuItemManager.IsButtonWhatIsThisDown);
        FMainForm.OnHelpWhatIsThisClick(FMainFormMenuItemManager.IsButtonWhatIsThisDown);
        Result := True;
      end;

      CmeSetHelpWhatIsThisOff:
      begin
        FMainFormMenuItemManager.SetMenuToggleWhatIsThisDown(False);
        FMainForm.OnHelpWhatIsThisClick(False);
        Result := True;
      end;
    end;//case

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.StudyHasChanged: boolean;
const OPNAME = 'TMainFormManager.StudyHasChanged';
begin
  Result := True;
  try
    if Assigned(FMainForm) then
      FMainForm.StudyHasChanged;
    if Assigned(FMainFormMenuItemManager) then
      FMainFormMenuItemManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainFormManager.GetBtnWhatIsThis: TObject;
const OPNAME = 'TMainFormManager.GetBtnWhatIsThis';
begin
  Result := nil;
  try
    Result := FMainFormMenuItemManager.ToolBar.BtnWhatIsThis;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
