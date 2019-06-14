//
//
//  UNIT      : Contains the class TFileEditToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCModelToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TYRCModelToolBar = class(TChildToolBar)
  protected
    FValidateFiles,
    FImportFiles,
    FExportFiles,
    FClearModelData,
    FRunModel : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnValidateFilesClick(Sender: TObject);
    procedure OnExportFilesClick(Sender: TObject);
    procedure OnImportFilesClick(Sender: TObject);
    procedure OnClearModelDataClick(Sender: TObject);
    procedure OnRunModelClick(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetValidateFilesState(AState: boolean);
    procedure SetImportFilesState(AState: boolean);
    procedure SetExportFilesState(AState: boolean);
    procedure SetClearModelDataState(AState: boolean);
    procedure SetRunModelState(AState: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TYRCModelToolBar.CreateMemberObjects;
const OPNAME = 'TYRCModelToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FValidateFiles := CreateButton('ValidateFiles');
    FValidateFiles.OnClick := OnValidateFilesClick;
    FValidateFiles.Enabled := False;

    FExportFiles := CreateButton('ExportFiles');
    FExportFiles.OnClick := OnExportFilesClick;
    FExportFiles.Enabled := False;

    FImportFiles := CreateButton('ImportFiles');
    FImportFiles.OnClick := OnImportFilesClick;
    FImportFiles.Enabled := False;

    FClearModelData := CreateButton('ClearModelData');
    FClearModelData.OnClick := OnClearModelDataClick;
    FClearModelData.Enabled := False;

    FRunModel := CreateButton('RunModel');
    FRunModel.OnClick := OnRunModelClick;
    FRunModel.Enabled := False;

    SetHorizontalPositions;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.AssignHelpContext;
const OPNAME = 'TYRCModelToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FValidateFiles,HC_ValidateButton);
    SetControlHelpContext(FExportFiles,HC_ExportButton);
    SetControlHelpContext(FImportFiles,HC_ImportButton);
    SetControlHelpContext(FImportFiles,HC_ClearModelDataButton);
    SetControlHelpContext(FRunModel,HC_RunModelButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCModelToolBar.SetHorizontalPositions;
const OPNAME = 'TYRCModelToolBar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;

    SetButtonHorizontalPosition(FValidateFiles,  True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportFiles,  True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FExportFiles,  True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FClearModelData,  True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FRunModel,  True, False, LButtonCount, LGaps);
    Width := FRunModel.Left + FRunModel.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TYRCModelToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    if Assigned(FValidateFiles) then
      FValidateFiles.LanguageHasChanged;

    if Assigned(FImportFiles) then
      FImportFiles.LanguageHasChanged;

    if Assigned(FExportFiles) then
      FExportFiles.LanguageHasChanged;

    if Assigned(FClearModelData) then
      FClearModelData.LanguageHasChanged;

    if Assigned(FRunModel) then
      FRunModel.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.SetValidateFilesState(AState: boolean);
const OPNAME = 'TYRCModelToolBar.SetValidateFilesState';
begin
  try
    SetButtonEnabled(FValidateFiles, AState, 'ValidateFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.SetExportFilesState(AState: boolean);
const OPNAME = 'TYRCModelToolBar.SetExportFilesState';
begin
  try
    SetButtonEnabled(FExportFiles, AState, 'ExportFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.SetImportFilesState(AState: boolean);
const OPNAME = 'TYRCModelToolBar.SetImportFilesState';
begin
  try
    SetButtonEnabled(FImportFiles, AState, 'ImportFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.SetClearModelDataState(AState: boolean);
const OPNAME = 'TYRCModelToolBar.SetClearModelDataState';
begin
  try
    SetButtonEnabled(FClearModelData, AState, 'ClearFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.SetRunModelState(AState: boolean);
const OPNAME = 'TYRCModelToolBar.SetRunModelState';
begin
  try
    SetButtonEnabled(FRunModel, AState, 'RunModelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.OnValidateFilesClick(Sender: TObject);
const OPNAME = 'TYRCModelToolBar.OnValidateFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.OnExportFilesClick(Sender: TObject);
const OPNAME = 'TYRCModelToolBar.OnExportFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeExportFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.OnImportFilesClick(Sender: TObject);
const OPNAME = 'TYRCModelToolBar.OnImportFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeImportFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.OnClearModelDataClick(Sender: TObject);
const OPNAME = 'TYRCModelToolBar.OnClearModelDataClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeClearModelData,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelToolBar.OnRunModelClick(Sender: TObject);
const OPNAME = 'TYRCModelToolBar.OnRunModelClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeRunModel,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
