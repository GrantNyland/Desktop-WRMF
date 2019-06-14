//
//
//  UNIT      : Contains the class TPrintManagerToolBar.
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/01/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UPrintManagerToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TPrintManagerToolBar = class(TChildToolBar)
  protected
    FPrintSettings,
    FPrint : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnPrintSettingsClick(Sender: TObject);
    procedure OnPrintClick(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure SetPrintSettingsState(AEnabled: boolean);
    procedure SetPrintState(AEnabled: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TPrintManagerToolBar.CreateMemberObjects;
const OPNAME = 'TPrintManagerToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPrintSettings := CreateButton('PrintSettings');
    FPrintSettings.OnClick := OnPrintSettingsClick;
    FPrintSettings.Enabled := False;

    FPrint := CreateButton('Print');
    FPrint.OnClick := OnPrintClick;
    FPrint.Enabled := False;

    SetHorizontalPositions;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerToolBar.AssignHelpContext;
const OPNAME = 'TPrintManagerToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FPrintSettings, HC_FilePrintSettings);
    SetControlHelpContext(FPrint,         HC_Print);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManagerToolBar.SetHorizontalPositions;
const OPNAME = 'TPrintManagerToolBar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;

    SetButtonHorizontalPosition(FPrintSettings,  True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FPrint,  True, False, LButtonCount, LGaps);
    Width := FPrint.Left + FPrint.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPrintManagerToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TPrintManagerToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    if Assigned(FPrint) then
      FPrint.LanguageHasChanged;

    if Assigned(FPrintSettings) then
      FPrintSettings.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPrintManagerToolBar.StudyHasChanged: boolean;
const OPNAME = 'TPrintManagerToolBar.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerToolBar.SetPrintState(AEnabled: boolean);
const OPNAME = 'TPrintManagerToolBar.SetPrintState';
begin
  try
    if Assigned(FPrint) then
      SetButtonEnabled(FPrint, AEnabled, 'PrintDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerToolBar.SetPrintSettingsState(AEnabled: boolean);
const OPNAME = 'TPrintManagerToolBar.SetPrintSettingsState';
begin
  try
    if Assigned(FPrintSettings) then
      SetButtonEnabled(FPrintSettings, AEnabled, 'PrintSettingsDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerToolBar.OnPrintClick(Sender: TObject);
const OPNAME = 'TPrintManagerToolBar.OnPrintClick';
begin
  try
    FAppModules.ProcessEvent(CmePrint,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManagerToolBar.OnPrintSettingsClick(Sender: TObject);
const OPNAME = 'TPrintManagerToolBar.OnPrintSettingsClick';
begin
  try
    FAppModules.ProcessEvent(CmePrintSettings, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
