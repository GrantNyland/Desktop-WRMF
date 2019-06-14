//
//
//  UNIT      : Contains the class TFileEditToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFileEditToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TFileEditToolBarState = (gesNoRecords, gesOneRecord, gesFirstRecord, gesMiddleRecord, gesLastRecord);
  TFileEditToolBar = class(TChildToolBar)
  protected
    FSaveFile,
    FValidateFile,
    FImportFile,
    FExportFile: TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnSaveFileClick(Sender: TObject);
    procedure OnValidateFileClick(Sender: TObject);
    procedure OnImportFileClick(Sender: TObject);
    procedure OnExportFileClick(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetValidateFileState(AState: boolean);
    procedure SetSaveFileState(AState: boolean);
    procedure SetImportFileState(AState: boolean);
    procedure SetExportFileState(AState: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TFileEditToolBar.CreateMemberObjects;
const OPNAME = 'TFileEditToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSaveFile := CreateButton('SaveFile');
    FSaveFile.OnClick := OnSaveFileClick;
    FSaveFile.Enabled := False;

    FValidateFile := CreateButton('ValidateFile');
    FValidateFile.OnClick := OnValidateFileClick;
    FValidateFile.Enabled := False;

    FExportFile := CreateButton('ExportFile');
    FExportFile.OnClick := OnExportFileClick;
    FExportFile.Enabled := False;

    FImportFile := CreateButton('ImportFile');
    FImportFile.OnClick := OnImportFileClick;
    FImportFile.Enabled := False;

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.AssignHelpContext;
const OPNAME = 'TFileEditToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FSaveFile,HC_SaveFileButton);
    SetControlHelpContext(FSaveFile,HC_ValidateFileButton);
    SetControlHelpContext(FExportFile,HC_ExportFileButton);
    SetControlHelpContext(FImportFile,HC_ImportFileButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFileEditToolBar.SetHorizontalPositions;
const OPNAME = 'TFileEditToolBar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FSaveFile,     True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FValidateFile, True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportFile,   True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FExportFile,   True, False, LButtonCount, LGaps);
    Width := FExportFile.Left + FExportFile.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TFileEditToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    if Assigned(FValidateFile) then
      FValidateFile.LanguageHasChanged;
    if Assigned(FSaveFile) then
      FSaveFile.LanguageHasChanged;
    if Assigned(FImportFile) then
      FImportFile.LanguageHasChanged;
    if Assigned(FExportFile) then
      FExportFile.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.SetSaveFileState(AState: boolean);
const OPNAME = 'TFileEditToolBar.SetSaveFileState';
begin
  try
    SetButtonEnabled(FSaveFile, AState, 'SaveFileDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.SetValidateFileState(AState: boolean);
const OPNAME = 'TFileEditToolBar.SetValidateFileState';
begin
  try
    SetButtonEnabled(FValidateFile, AState, 'ValidateFileDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.SetExportFileState(AState: boolean);
const OPNAME = 'TFileEditToolBar.SetExportFileState';
begin
  try
    SetButtonEnabled(FExportFile, AState, 'ExportFileDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.SetImportFileState(AState: boolean);
const OPNAME = 'TFileEditToolBar.SetImportFileState';
begin
  try
    SetButtonEnabled(FImportFile, AState, 'ImportFileDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.OnSaveFileClick(Sender: TObject);
const OPNAME = 'TFileEditToolBar.OnSaveFileClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meSaveFile));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.OnValidateFileClick(Sender: TObject);
const OPNAME = 'TFileEditToolBar.OnValidateFileClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateFile,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.OnExportFileClick(Sender: TObject);
const OPNAME = 'TFileEditToolBar.OnExportFileClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeExportFile,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditToolBar.OnImportFileClick(Sender: TObject);
const OPNAME = 'TFileEditToolBar.OnImportFileClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeImportFile,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
