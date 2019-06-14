//
//
//  UNIT      : Contains TDDTSMenuItemToolBar Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDDTSMenuItemToolBar;


interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TDDTSMenuItemToolBar = class(TChildToolBar)
  protected
    FValidateFiles               : TAbstractSpeedButton;
    FImportFiles                 : TAbstractSpeedButton;
    FExportFiles                 : TAbstractSpeedButton;
    FClearModelData              : TAbstractSpeedButton;
    FValidateModelData           : TAbstractSpeedButton;
    FRunModel                    : TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;

    procedure OnValidateFilesClick(Sender: TObject);
    procedure OnExportFilesClick(Sender: TObject);
    procedure OnImportFilesClick(Sender: TObject);
    procedure OnClearModelDataClick(Sender: TObject);
    procedure OnValidateModelDataClick(Sender: TObject);
    procedure OnRunModelClick(Sender: TObject);
  public
    function LanguageHasChanged: boolean; override;

    procedure SetValidateFilesState(AState: boolean);
    procedure SetImportFilesState(AState: boolean);
    procedure SetExportFilesState(AState: boolean);
    procedure SetClearModelDataState(AState: boolean);
    procedure SetValidateModelDataClickState(AState: boolean);
    procedure SetRunModelState(AState: boolean);
    procedure TabHasChanged(AGridTabSelected: boolean);


    property ValidateFiles       : TAbstractSpeedButton read FValidateFiles;
    property ImportFiles         : TAbstractSpeedButton read FImportFiles;
    property ExportFiles         : TAbstractSpeedButton read FExportFiles;
    property ClearModelData      : TAbstractSpeedButton read FClearModelData;
    property ValidateModelData   : TAbstractSpeedButton read FValidateModelData;
    property RunModel            : TAbstractSpeedButton read FRunModel;


  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TDDTSMenuItemToolBar.CreateMemberObjects;
const OPNAME = 'TDDTSMenuItemToolBar.CreateMemberObjects';
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

    FValidateModelData := CreateButton('ValidateModelData');
    FValidateModelData.OnClick := OnValidateModelDataClick;
    FValidateModelData.Enabled := False;

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.AssignHelpContext;
const OPNAME = 'TDDTSMenuItemToolBar.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSMenuItemToolBar.SetHorizontalPositions;
const OPNAME = 'TDDTSMenuItemToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;

    SetButtonHorizontalPosition(FValidateFiles,           True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportFiles,             True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FExportFiles,             True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FClearModelData,          True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FValidateModelData,       True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FRunModel,                True, False, LButtonCount, LGaps);

    Width := FRunModel.Left + FRunModel.Width
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSMenuItemToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSMenuItemToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    FValidateFiles.LanguageHasChanged;
    FImportFiles.LanguageHasChanged;
    FExportFiles.LanguageHasChanged;
    FClearModelData.LanguageHasChanged;
    FValidateModelData.LanguageHasChanged;
    FRunModel.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.SetValidateFilesState(AState: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.SetValidateFilesState';
begin
  try
    SetButtonEnabled(FValidateFiles, AState, 'ValidateFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.SetExportFilesState(AState: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.SetExportFilesState';
begin
  try
    SetButtonEnabled(FExportFiles, AState, 'ExportFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.SetImportFilesState(AState: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.SetImportFilesState';
begin
  try
    SetButtonEnabled(FImportFiles, AState, 'ImportFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.SetClearModelDataState(AState: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.SetClearModelDataState';
begin
  try
    SetButtonEnabled(FClearModelData, AState, 'ClearFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.SetValidateModelDataClickState(AState: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.SetValidateModelDataClickState';
begin
  try
    SetButtonEnabled(FValidateModelData, AState, 'ValidateModelDataDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.SetRunModelState(AState: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.SetRunModelState';
begin
  try
    SetButtonEnabled(FRunModel, AState, 'RunModelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.OnValidateFilesClick(Sender: TObject);
const OPNAME = 'TDDTSMenuItemToolBar.OnValidateFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.OnExportFilesClick(Sender: TObject);
const OPNAME = 'TDDTSMenuItemToolBar.OnExportFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeExportFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.OnImportFilesClick(Sender: TObject);
const OPNAME = 'TDDTSMenuItemToolBar.OnImportFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeImportFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.OnClearModelDataClick(Sender: TObject);
const OPNAME = 'TDDTSMenuItemToolBar.OnClearModelDataClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeClearModelData,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.OnValidateModelDataClick(Sender: TObject);
const OPNAME = 'TDDTSMenuItemToolBar.OnValidateModelDataClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateModelData,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.OnRunModelClick(Sender: TObject);
const OPNAME = 'TDDTSMenuItemToolBar.OnRunModelClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeRunModel,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSMenuItemToolBar.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TDDTSMenuItemToolBar.TabHasChanged';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
