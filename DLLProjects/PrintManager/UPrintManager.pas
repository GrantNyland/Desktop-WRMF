//
//
//  UNIT      : Contains TPrintManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPrintManager;

interface

uses
  Vcl.Dialogs,
  UPrintManagerMenuItemManager,
  UAbstractObject;

type
  TPrintManager = class(TAbstractPrintManager)
  protected
    FPrintMenuItemManager: TPrintManagerMenuItemManager;
    FPrinterSetupDialog: TPrinterSetupDialog;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure SetPrintEnabled(AEnabled: boolean);override;
    procedure DoPrintSettings;override;
    procedure DoPrintPreview(AReportData: TObject);override;
    procedure DoPrint(AReportData: TObject);override;

    procedure SetMenuPrintState(AAction: TMenuSetAction);override;
    procedure SetMenuPrintPreviewState(AAction: TMenuSetAction);override;
    procedure SetMenuPrintSettingsState(AAction: TMenuSetAction);override;

    property MenuItemManager:TPrintManagerMenuItemManager read FPrintMenuItemManager;
  end;

implementation

uses
  SysUtils,
  Vcl.Printers,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TPrintManager }

procedure TPrintManager.CreateMemberObjects;
const OPNAME = 'TPrintManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPrintMenuItemManager := TPrintManagerMenuItemManager.Create(FAppModules);
    FOwnedAppObjects.Add(FPrintMenuItemManager);
    FPrinterSetupDialog := TPrinterSetupDialog.Create(nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.DestroyMemberObjects;
const OPNAME = 'TPrintManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FPrinterSetupDialog);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPrintManager.DoPrintSettings;
const OPNAME = 'TPrintManager.DoPrintSettings';
begin
  try
    if FPrinterSetupDialog.Execute then
    begin
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.DoPrintPreview(AReportData: TObject);
const OPNAME = 'TPrintManager.DoPrintPreview';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.DoPrint(AReportData: TObject);
const OPNAME = 'TPrintManager.DoPrint';
begin
  try
    TAbstractTabSheet(FAppModules.MainForm.ActivePage).DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.SetMenuPrintPreviewState(AAction: TMenuSetAction);
const OPNAME = 'TPrintManager.SetMenuPrintPreviewState';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.SetMenuPrintSettingsState(AAction: TMenuSetAction);
const OPNAME = 'TPrintManager.SetMenuPrintSettingsState';
begin
  try
    if(Printer.Printers.Count = 0) then
    begin
      FPrintMenuItemManager.SetMenuPrintSettingsState(msDisable);
      if(AAction <> msEnable) then
        FPrintMenuItemManager.SetMenuPrintSettingsState(AAction);
    end
    else
      FPrintMenuItemManager.SetMenuPrintSettingsState(AAction);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.SetMenuPrintState(AAction: TMenuSetAction);
const OPNAME = 'TPrintManager.SetMenuPrintState';
begin
  try
    if(Printer.Printers.Count = 0) then
    begin
      FPrintMenuItemManager.SetMenuPrintState(msDisable);
      if(AAction <> msEnable) then
        FPrintMenuItemManager.SetMenuPrintState(AAction);
    end
    else
      FPrintMenuItemManager.SetMenuPrintState(AAction);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPrintManager.SetPrintEnabled(AEnabled: boolean);
const OPNAME = 'TPrintManager.SetPrintEnabled';
begin
  try
    if AEnabled and (Printer.Printers.Count > 0) then
      FPrintMenuItemManager.SetMenuPrintState(msEnable)
    else
      FPrintMenuItemManager.SetMenuPrintState(msDisable);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPrintManager.Initialise: boolean;
const OPNAME = 'TPrintManager.Initialise';
begin
  Result := False;
  try
    if Assigned(FPrintMenuItemManager) then
    begin
      Result := FPrintMenuItemManager.Initialise;
      if Result then
      begin
        SetMenuPrintState(msDisable);
        SetMenuPrintSettingsState(msEnable);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPrintManager.StudyHasChanged: boolean;
const OPNAME = 'TPrintManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    SetMenuPrintState(msDisable);
    SetMenuPrintSettingsState(msEnable);
    Result := True;
    if Assigned(FPrintMenuItemManager) then
      Result := FPrintMenuItemManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
