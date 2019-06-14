//
//
//  UNIT      : Contains TIFRMenuItemToolBar Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRMenuItemToolBar;


interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TIFRMenuItemToolBar = class(TChildToolBar)
  protected
    FCreateIFRSiteBtn : TAbstractSpeedButton;
    FDeleteIFRSiteBtn : TAbstractSpeedButton;
    FSaveIFRSiteBtn   : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnClickCreateIFRSite(Sender: TObject);
    procedure OnClickDeleteIFRSite(Sender: TObject);
    procedure OnClickSaveIFRSite(Sender: TObject);
  public
    function LanguageHasChanged: boolean; override;
    procedure SetCreateIFRSite(AEnabled: boolean);
    procedure SetDeleteIFRSite(AEnabled: boolean);
    procedure SetSaveIFRSite(AEnabled: boolean);

    property CreateIFRSiteBtn : TAbstractSpeedButton read FCreateIFRSiteBtn;
    property DeleteIFRSiteBtn : TAbstractSpeedButton read FDeleteIFRSiteBtn;
    property SaveIFRSiteBtn   : TAbstractSpeedButton read FSaveIFRSiteBtn;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TIFRMenuItemToolBar.CreateMemberObjects;
const OPNAME = 'TIFRMenuItemToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreateIFRSiteBtn   := CreateButton('CreateIFRSite');
    FDeleteIFRSiteBtn   := CreateButton('DeleteIFRSite');
    FSaveIFRSiteBtn     := CreateButton('SaveIFRSite');

    FCreateIFRSiteBtn.OnClick  := OnClickCreateIFRSite;
    FDeleteIFRSiteBtn.OnClick  := OnClickDeleteIFRSite;
    FSaveIFRSiteBtn.OnClick  := OnClickSaveIFRSite;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRMenuItemToolBar.SetHorizontalPositions;
const OPNAME = 'TIFRMenuItemToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FCreateIFRSiteBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteIFRSiteBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FSaveIFRSiteBtn, True,  False, LButtonCount, LGaps);
    Width := FSaveIFRSiteBtn.Left + FSaveIFRSiteBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRMenuItemToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TIFRMenuItemToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FCreateIFRSiteBtn.LanguageHasChanged;
    FDeleteIFRSiteBtn.LanguageHasChanged;
    FSaveIFRSiteBtn.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRMenuItemToolBar.OnClickCreateIFRSite(Sender: TObject);
const OPNAME = 'TIFRMenuItemToolBar.OnClickCreateIFRSite';
begin
  try
    FAppModules.Model.ProcessEvent(cmeCreateIFRSite, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRMenuItemToolBar.OnClickDeleteIFRSite(Sender: TObject);
const OPNAME = 'TIFRMenuItemToolBar.OnClickDeleteIFRSite';
begin
  try
    FAppModules.Model.ProcessEvent(cmeDeleteIFRSite, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRMenuItemToolBar.OnClickSaveIFRSite(Sender: TObject);
const OPNAME = 'TIFRMenuItemToolBar.OnClickSaveIFRSite';
begin
  try
    FAppModules.Model.ProcessEvent(cmeSaveIFRSite, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TIFRMenuItemToolBar.SetCreateIFRSite(AEnabled: boolean);
const OPNAME = 'TIFRMenuItemToolBar.SetCreateIFRSite';
begin
  try
    SetButtonEnabled(FCreateIFRSiteBtn, AEnabled, 'CreateIFRSiteDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRMenuItemToolBar.SetDeleteIFRSite(AEnabled: boolean);
const OPNAME = 'TIFRMenuItemToolBar.SetDeleteIFRSite';
begin
  try
    SetButtonEnabled(FDeleteIFRSiteBtn, AEnabled, 'DeleteIFRSiteDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRMenuItemToolBar.SetSaveIFRSite(AEnabled: boolean);
const OPNAME = 'TIFRMenuItemToolBar.SetSaveIFRSite';
begin
  try
    SetButtonEnabled(FSaveIFRSiteBtn, AEnabled, 'SaveIFRSiteDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
