//
//
//  UNIT      : Contains the class TFileEditToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldModelToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TYieldModelToolBar = class(TChildToolBar)
  protected
    FGridTabSelected             : Boolean;
    FValidateFiles               : TAbstractSpeedButton;
    FImportFiles                 : TAbstractSpeedButton;
    FExportFiles                 : TAbstractSpeedButton;
    FClearModelData              : TAbstractSpeedButton;
    FValidateModelData           : TAbstractSpeedButton;
    FRunModel                    : TAbstractSpeedButton;
    FCreateReservoir             : TAbstractSpeedButton;
    FDeleteReservoir             : TAbstractSpeedButton;
    FCreateNodeWithInflow        : TAbstractSpeedButton;
    FDeleteNodeWithInflow        : TAbstractSpeedButton;
    FCreateNodeWithoutInflow     : TAbstractSpeedButton;
    FDeleteNodeWithoutInflow     : TAbstractSpeedButton;
    FCreateChannel               : TAbstractSpeedButton;
    //FCopyChannel                 : TAbstractSpeedButton;
    FDeleteChannel               : TAbstractSpeedButton;
    FCreateAllocDef              : TAbstractSpeedButton;
    FDeleteAllocDef              : TAbstractSpeedButton;
    FCreateSwitchDef             : TAbstractSpeedButton;
    FDeleteSwitchDef             : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnValidateFilesClick(Sender: TObject);
    procedure OnExportFilesClick(Sender: TObject);
    procedure OnImportFilesClick(Sender: TObject);
    procedure OnClearModelDataClick(Sender: TObject);
    procedure OnValidateModelDataClick(Sender: TObject);
    procedure OnRunModelClick(Sender: TObject);
    procedure OnClickCreateReservoir(Sender: TObject);
    procedure OnClickDeleteReservoir(Sender: TObject);
    procedure OnClickCreateNodeWithInflow(Sender: TObject);
    procedure OnClickDeleteNodeWithInflow(Sender: TObject);
    procedure OnClickCreateNodeWithoutInflow(Sender: TObject);
    procedure OnClickDeleteNodeWithoutInflow(Sender: TObject);
    procedure OnClickCreateChannel(Sender: TObject);
    procedure OnClickCopyChannel(Sender: TObject);
    procedure OnClickDeleteChannel(Sender: TObject);
    procedure OnClickCreateAllocDef(Sender: TObject);
    procedure OnClickDeleteAllocDef(Sender: TObject);
    procedure OnClickCreateSwitchDef(Sender: TObject);
    procedure OnClickDeleteSwitchDef(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetValidateFilesState(AState: boolean);
    procedure SetImportFilesState(AState: boolean);
    procedure SetExportFilesState(AState: boolean);
    procedure SetClearModelDataState(AState: boolean);
    procedure SetValidateModelDataClickState(AState: boolean);
    procedure SetRunModelState(AState: boolean);
    procedure SetCreateReservoir(AEnabled: boolean);
    procedure SetDeleteReservoir(AEnabled: boolean);
    procedure SetCreateNodeWithInflow(AEnabled: boolean);
    procedure SetDeleteNodeWithInflow(AEnabled: boolean);
    procedure SetCreateNodeWithoutInflow(AEnabled: boolean);
    procedure SetDeleteNodeWithoutInflow(AEnabled: boolean);
    procedure SetCreateChannel(AEnabled: boolean);
    //procedure SetCopyChannel(AEnabled: boolean);
    procedure SetDeleteChannel(AEnabled: boolean);
    procedure SetCreateAllocDef(AEnabled: boolean);
    procedure SetDeleteAllocDef(AEnabled: boolean);
    procedure SetCreateSwitchDef(AEnabled: boolean);
    procedure SetDeleteSwitchDef(AEnabled: boolean);
    procedure TabHasChanged(AGridTabSelected: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TYieldModelToolBar.CreateMemberObjects;
const OPNAME = 'TYieldModelToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGridTabSelected := False;
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

    FCreateReservoir         := CreateButton('CreateReservoir');
    FDeleteReservoir         := CreateButton('DeleteReservoir');
    FCreateNodeWithInflow    := CreateButton('CreateNodeWithInflow');
    FDeleteNodeWithInflow    := CreateButton('DeleteNodeWithInflow');
    FCreateNodeWithoutInflow := CreateButton('CreateNodeWithoutInflow');
    FDeleteNodeWithoutInflow := CreateButton('DeleteNodeWithoutInflow');
    FCreateChannel           := CreateButton('CreateChannel');
    //FCopyChannel             := CreateButton('CopyChannel');
    FDeleteChannel           := CreateButton('DeleteChannel');

    FCreateReservoir.OnClick         := OnClickCreateReservoir;
    FDeleteReservoir.OnClick         := OnClickDeleteReservoir;
    FCreateNodeWithInflow.OnClick    := OnClickCreateNodeWithInflow;
    FDeleteNodeWithInflow.OnClick    := OnClickDeleteNodeWithInflow;
    FCreateNodeWithoutInflow.OnClick := OnClickCreateNodeWithoutInflow;
    FDeleteNodeWithoutInflow.OnClick := OnClickDeleteNodeWithoutInflow;
    FCreateChannel.OnClick           := OnClickCreateChannel;
    //FCopyChannel.OnClick             := OnClickCopyChannel;
    FDeleteChannel.OnClick           := OnClickDeleteChannel;

    //Planning
    FCreateAllocDef                  := CreateButton('CreateAllocDef');
    FDeleteAllocDef                  := CreateButton('DeleteAllocDef');
    FCreateSwitchDef                 := CreateButton('CreateSwitchDef');
    FDeleteSwitchDef                 := CreateButton('DeleteSwitchDef');
    FCreateAllocDef.OnClick          := OnClickCreateAllocDef;
    FDeleteAllocDef.OnClick          := OnClickDeleteAllocDef;
    FCreateSwitchDef.OnClick         := OnClickCreateSwitchDef;
    FDeleteSwitchDef.OnClick         := OnClickDeleteSwitchDef;
    SetHorizontalPositions;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.AssignHelpContext;
const OPNAME = 'TYieldModelToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FValidateFiles,HC_ValidateButton);
    SetControlHelpContext(FExportFiles,HC_ExportButton);
    SetControlHelpContext(FImportFiles,HC_ImportButton);
    SetControlHelpContext(FImportFiles,HC_ClearModelDataButton);
    //tControlHelpContext(FRunModel,HC_ValidateModelData);
    SetControlHelpContext(FRunModel,HC_RunModelButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelToolBar.SetHorizontalPositions;
const OPNAME = 'TYieldModelToolBar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
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

    SetButtonHorizontalPosition(FCreateReservoir,         True,  TRUE, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteReservoir,         True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCreateNodeWithInflow,    True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteNodeWithInflow,    True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCreateNodeWithoutInflow, True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteNodeWithoutInflow, True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCreateChannel,           True, False, LButtonCount, LGaps);
    //SetButtonHorizontalPosition(FCopyChannel,             True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteChannel,           True, False, LButtonCount, LGaps);

    //PLanning
    SetButtonHorizontalPosition(FCreateAllocDef,     True,  TRUE, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteAllocDef,     True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCreateSwitchDef,    True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteSwitchDef,    True, False, LButtonCount, LGaps);
    if not FGridTabSelected then
      Width := FRunModel.Left + FRunModel.Width
    else
      Width := FDeleteSwitchDef.Left + FDeleteSwitchDef.Width;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TYieldModelToolBar.LanguageHasChanged';
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
    FCreateReservoir.LanguageHasChanged;
    FDeleteReservoir.LanguageHasChanged;
    FCreateNodeWithInflow.LanguageHasChanged;
    FDeleteNodeWithInflow.LanguageHasChanged;
    FCreateNodeWithoutInflow.LanguageHasChanged;
    FDeleteNodeWithoutInflow.LanguageHasChanged;
    FCreateChannel.LanguageHasChanged;
    //FCopyChannel.LanguageHasChanged;
    FDeleteChannel.LanguageHasChanged;
    //PLanning
    FCreateAllocDef.LanguageHasChanged;
    FDeleteAllocDef.LanguageHasChanged;
    FCreateSwitchDef.LanguageHasChanged;
    FDeleteSwitchDef.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetValidateFilesState(AState: boolean);
const OPNAME = 'TYieldModelToolBar.SetValidateFilesState';
begin
  try
    SetButtonEnabled(FValidateFiles, AState, 'ValidateFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetExportFilesState(AState: boolean);
const OPNAME = 'TYieldModelToolBar.SetExportFilesState';
begin
  try
    SetButtonEnabled(FExportFiles, AState, 'ExportFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetImportFilesState(AState: boolean);
const OPNAME = 'TYieldModelToolBar.SetImportFilesState';
begin
  try
    SetButtonEnabled(FImportFiles, AState, 'ImportFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetClearModelDataState(AState: boolean);
const OPNAME = 'TYieldModelToolBar.SetClearModelDataState';
begin
  try
    SetButtonEnabled(FClearModelData, AState, 'ClearFilesDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetValidateModelDataClickState(AState: boolean);
const OPNAME = 'TYieldModelToolBar.SetValidateModelDataClickState';
begin
  try
    SetButtonEnabled(FValidateModelData, AState, 'ValidateModelDataDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetRunModelState(AState: boolean);
const OPNAME = 'TYieldModelToolBar.SetRunModelState';
begin
  try
    SetButtonEnabled(FRunModel, AState, 'RunModelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnValidateFilesClick(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnValidateFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnExportFilesClick(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnExportFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeExportFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnImportFilesClick(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnImportFilesClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeImportFiles,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClearModelDataClick(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClearModelDataClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeClearModelData,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnValidateModelDataClick(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnValidateModelDataClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeValidateModelData,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnRunModelClick(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnRunModelClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeRunModel,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
//____________________________________________________________

procedure TYieldModelToolBar.SetCreateChannel(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCreateChannel';
begin
  try
    SetButtonEnabled(FCreateChannel, AEnabled, 'ActionCreateChannelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TYieldModelToolBar.SetCopyChannel(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCopyChannel';
begin
  try
    SetButtonEnabled(FCopyChannel, AEnabled, 'ActionCopyChannelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }

procedure TYieldModelToolBar.SetCreateNodeWithInflow(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCreateNodeWithInflow';
begin
  try
    SetButtonEnabled(FCreateNodeWithInflow, AEnabled, 'ActionCreateNodeWithInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetCreateNodeWithoutInflow(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCreateNodeWithoutInflow';
begin
  try
    SetButtonEnabled(FCreateNodeWithoutInflow, AEnabled, 'ActionCreateNodeWithoutInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetCreateReservoir(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCreateReservoir';
begin
  try
    SetButtonEnabled(FCreateReservoir, AEnabled, 'ActionCreateReservoirDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetDeleteChannel(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetDeleteChannel';
begin
  try
    SetButtonEnabled(FDeleteChannel, AEnabled, 'ActionDeleteChannelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetDeleteNodeWithInflow(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetDeleteNodeWithInflow';
begin
  try
    SetButtonEnabled(FDeleteNodeWithInflow, AEnabled, 'ActionDeleteNodeWithInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetDeleteNodeWithoutInflow(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetDeleteNodeWithoutInflow';
begin
  try
    SetButtonEnabled(FDeleteNodeWithoutInflow, AEnabled, 'ActionDeleteNodeWithoutInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetDeleteReservoir(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetDeleteReservoir';
begin
  try
    SetButtonEnabled(FDeleteReservoir, AEnabled, 'ActionDeleteReservoirDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickCreateChannel(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCreateChannel';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateChannel, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickCopyChannel(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCopyChannel';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCopyChannel, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYieldModelToolBar.OnClickCreateNodeWithInflow(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCreateNodeWithInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateNodeWithInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickCreateNodeWithoutInflow(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCreateNodeWithoutInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateNodeWithoutInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickCreateReservoir(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCreateReservoir';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateReservoir, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickDeleteChannel(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickDeleteChannel';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteChannel, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickDeleteNodeWithInflow(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickDeleteNodeWithInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteNodeWithInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickDeleteNodeWithoutInflow(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickDeleteNodeWithoutInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteNodeWithoutInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickDeleteReservoir(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickDeleteReservoir';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteReservoir, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetCreateAllocDef(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCreateAllocDef';
begin
  try
    SetButtonEnabled(FCreateAllocDef, AEnabled, 'ActionCreateAllocDefDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetDeleteAllocDef(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetDeleteAllocDef';
begin
  try
    SetButtonEnabled(FDeleteAllocDef, AEnabled, 'ActionDeleteAllocDefDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickCreateAllocDef(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCreateAllocDef';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateAllocDef, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickDeleteAllocDef(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickDeleteAllocDef';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteAllocDef, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetCreateSwitchDef(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetCreateSwitchDef';
begin
  try
    SetButtonEnabled(FCreateSwitchDef, AEnabled, 'ActionCreateSwitchDefDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.SetDeleteSwitchDef(AEnabled: boolean);
const OPNAME = 'TYieldModelToolBar.SetDeleteSwitchDef';
begin
  try
    SetButtonEnabled(FDeleteSwitchDef, AEnabled, 'ActionDeleteSwitchDefDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickCreateSwitchDef(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickCreateSwitchDef';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateSwitchDef, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.OnClickDeleteSwitchDef(Sender: TObject);
const OPNAME = 'TYieldModelToolBar.OnClickDeleteSwitchDef';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteSwitchDef, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelToolBar.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TYieldModelToolBar.TabHasChanged';
begin
  try
    FGridTabSelected := AGridTabSelected;
    FCreateReservoir.Visible         := AGridTabSelected;
    FDeleteReservoir.Visible         := AGridTabSelected;
    FCreateNodeWithInflow.Visible    := AGridTabSelected;
    FDeleteNodeWithInflow.Visible    := AGridTabSelected;
    FCreateNodeWithoutInflow.Visible := AGridTabSelected;
    FDeleteNodeWithoutInflow.Visible := AGridTabSelected;
    FCreateChannel.Visible           := AGridTabSelected;
    //FCopyChannel.Visible             := AGridTabSelected;
    FDeleteChannel.Visible           := AGridTabSelected;
    FCreateAllocDef.Visible          := AGridTabSelected;
    FDeleteAllocDef.Visible          := AGridTabSelected;
    FCreateSwitchDef.Visible         := AGridTabSelected;
    FDeleteSwitchDef.Visible         := AGridTabSelected;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
