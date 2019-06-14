//
//
//  UNIT      : Contains TRWHMenuItemToolBar Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit URWHMenuItemToolBar;


interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TRWHMenuItemToolBar = class(TChildToolBar)
  protected
    //FCreateRWHSiteBtn : TAbstractSpeedButton;
    //FDeleteRWHSiteBtn : TAbstractSpeedButton;
    //FSaveRWHSiteBtn   : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    //procedure OnClickCreateRWHSite(Sender: TObject);
    //procedure OnClickDeleteRWHSite(Sender: TObject);
    //procedure OnClickSaveRWHSite(Sender: TObject);
  public
    function LanguageHasChanged: boolean; override;
    //procedure SetCreateRWHSite(AEnabled: boolean);
    //procedure SetDeleteRWHSite(AEnabled: boolean);
    //procedure SetSaveRWHSite(AEnabled: boolean);

    //property CreateRWHSiteBtn : TAbstractSpeedButton read FCreateRWHSiteBtn;
    //property DeleteRWHSiteBtn : TAbstractSpeedButton read FDeleteRWHSiteBtn;
    //property SaveRWHSiteBtn   : TAbstractSpeedButton read FSaveRWHSiteBtn;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TRWHMenuItemToolBar.CreateMemberObjects;
const OPNAME = 'TRWHMenuItemToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    {FCreateRWHSiteBtn   := CreateButton('CreateIFRSite');
    FDeleteRWHSiteBtn   := CreateButton('DeleteIFRSite');
    FSaveRWHSiteBtn     := CreateButton('SaveIFRSite');

    FCreateRWHSiteBtn.OnClick  := OnClickCreateRWHSite;
    FDeleteRWHSiteBtn.OnClick  := OnClickDeleteRWHSite;
    FSaveRWHSiteBtn.OnClick  := OnClickSaveRWHSite;
    SetHorizontalPositions; }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHMenuItemToolBar.SetHorizontalPositions;
const OPNAME = 'TRWHMenuItemToolBar.SetHorizontalPositions';
//var LButtonCount, LGaps: integer;
begin
    inherited SetHorizontalPositions;
  try
    {LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FCreateRWHSiteBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteRWHSiteBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FSaveRWHSiteBtn, True,  False, LButtonCount, LGaps);
    Width := FSaveRWHSiteBtn.Left + FSaveRWHSiteBtn.Width;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHMenuItemToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TRWHMenuItemToolBar.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    {FCreateRWHSiteBtn.LanguageHasChanged;
    FDeleteRWHSiteBtn.LanguageHasChanged;
    FSaveRWHSiteBtn.LanguageHasChanged;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TRWHMenuItemToolBar.OnClickCreateRWHSite(Sender: TObject);
const OPNAME = 'TRWHMenuItemToolBar.OnClickCreateRWHSite';
begin
  try
    FAppModules.Model.ProcessEvent(cmeCreateIFRSite, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHMenuItemToolBar.OnClickDeleteRWHSite(Sender: TObject);
const OPNAME = 'TRWHMenuItemToolBar.OnClickDeleteRWHSite';
begin
  try
    FAppModules.Model.ProcessEvent(cmeDeleteIFRSite, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHMenuItemToolBar.OnClickSaveRWHSite(Sender: TObject);
const OPNAME = 'TRWHMenuItemToolBar.OnClickSaveRWHSite';
begin
  try
    FAppModules.Model.ProcessEvent(cmeSaveIFRSite, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRWHMenuItemToolBar.SetCreateRWHSite(AEnabled: boolean);
const OPNAME = 'TRWHMenuItemToolBar.SetCreateRWHSite';
begin
  try
    SetButtonEnabled(FCreateRWHSiteBtn, AEnabled, 'CreateIFRSiteDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHMenuItemToolBar.SetDeleteRWHSite(AEnabled: boolean);
const OPNAME = 'TRWHMenuItemToolBar.SetDeleteRWHSite';
begin
  try
    SetButtonEnabled(FDeleteRWHSiteBtn, AEnabled, 'DeleteIFRSiteDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHMenuItemToolBar.SetSaveRWHSite(AEnabled: boolean);
const OPNAME = 'TRWHMenuItemToolBar.SetSaveRWHSite';
begin
  try
    SetButtonEnabled(FSaveRWHSiteBtn, AEnabled, 'SaveIFRSiteDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.
