//
//
//  UNIT      : Contains the class TYRCToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TYRCToolBar = class(TChildToolBar)
    FDeleteChart,
    FDeleteTargetDraft,
    //FChartPrintMode,
    FSaveChart,
    FTogglePlaneMode,
    //FToggleChartMode,
    FResetChartData,
    FLoadFromDB,
    FLoadFromCoefFile,
    FToggleCurveManipulation,
    FLoadFromFile : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnSaveChart(Sender: TObject);
    procedure OnTogglePlaneMode(Sender: TObject);
    procedure OnToggleChartMode(Sender: TObject);
    procedure OnResetChartData(Sender: TObject);
    procedure OnLoadFromDB(Sender: TObject);
    procedure OnLoadCoefficientFile(Sender: TObject);
    procedure OnLoadFromFile(Sender: TObject);
    procedure OnToggleCurveManipulation(Sender : TObject);
    procedure OnDeleteTargetDraft(Sender : TObject);
    procedure OnDeleteChart(Sender : TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetDeleteTargetDraft(AEnabledState: boolean);
    procedure SetSaveChartState(AEnabledState: boolean);
    procedure SetTogglePlaneModeState(AEnabled: boolean);
    procedure SetToggleChartModeState(AEnabled: boolean);
    procedure SetToggleCurveManipulationState(AEnabled : boolean);
    procedure SetMenuTogglePlaneDown(ADown: boolean);
    //procedure SetMenuToggleChartDown(ADown: boolean);
    procedure SetMenuToggleCurveManipulationDown(ADown: boolean);
    procedure SetResetChartDataState(AEnabledState: boolean);
    procedure SetLoadFromDBState(AEnabledState: boolean);
    procedure SetLoadFromFileState(AEnabledState: boolean);
    procedure SetLoadCoefficientFileState(AEnabledState: boolean);
    procedure SetDeleteChartState(AEnabledState: boolean);
    function InPlaneMode: boolean;
    function GetMenuToggleCurveManipulationDown : boolean;
    //function InRegressionMode: boolean;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TYRCToolBar.CreateMemberObjects;
const OPNAME = 'TYRCToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
//    FToggleChartMode := nil;
    FResetChartData   := nil;

    FSaveChart := CreateButton('YRCSaveChart');
    FSaveChart.OnClick := OnSaveChart;
    FSaveChart.Enabled := False;

    FLoadFromDB := CreateButton('YRCLoadFromDB');
    FLoadFromDB.OnClick := OnLoadFromDB;
    FLoadFromDB.Enabled := False;

    FLoadFromFile := CreateButton('YRCLoadFromFile');
    FLoadFromFile.OnClick := OnLoadFromFile;
    FLoadFromFile.Enabled := False;

    FLoadFromCoefFile := CreateButton('YRCLoadFromCoefFile');
    FLoadFromCoefFile.OnClick := OnLoadCoefficientFile;
    FLoadFromCoefFile.Enabled := False;

    FResetChartData := CreateButton('YRCResetChartData');
    FResetChartData.OnClick := OnResetChartData;
    FResetChartData.Enabled := False;

    FTogglePlaneMode := CreateButton('YRCTogglePlaneMode');
    FTogglePlaneMode.GroupIndex := 121;
    FTogglePlaneMode.AllowAllUp := True;
    FTogglePlaneMode.OnClick := OnTogglePlaneMode;
    FTogglePlaneMode.Enabled := False;
    {
    FToggleChartMode := CreateButton('YRCToggleChartMode');
    FToggleChartMode.GroupIndex := 122;
    FToggleChartMode.AllowAllUp := True;
    FToggleChartMode.OnClick := OnToggleChartMode;
    FToggleChartMode.Enabled := False;
    }

    FToggleCurveManipulation := CreateButton('YRCToggleCurveManipulation');
    FToggleCurveManipulation.GroupIndex := 123;
    FToggleCurveManipulation.AllowAllUp := True;
    FToggleCurveManipulation.OnClick := OnToggleCurveManipulation;
    FToggleCurveManipulation.Enabled := False;

    FDeleteTargetDraft := CreateButton('YRCDeleteTargetDraft');
    FDeleteTargetDraft.OnClick := OnDeleteTargetDraft;
    FDeleteTargetDraft.Enabled := False;

    FDeleteChart := CreateButton('YRCDeleteChart');
    FDeleteChart.OnClick := OnDeleteChart;
    FDeleteChart.Enabled := False;

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.AssignHelpContext;
const OPNAME = 'TYRCToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_YRCChart);
    SetControlHelpContext(FTogglePlaneMode,HC_YRCTogglePlaneMode);
    //SetControlHelpContext(FToggleChartMode,HC_YRCToggleChartMode);
    //SetControlHelpContext(FToggleCurveManipulation,HC_YRCToggleCurveManipulation);
    SetControlHelpContext(FResetChartData,HC_YRCResetChartData);
    SetControlHelpContext(FSaveChart,HC_YRCSaveChart);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCToolBar.SetHorizontalPositions;
const OPNAME = 'TYRCToolBar.SetHorizontalPositions';
var
  LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FLoadFromFile,  True, False, LButtonCount, LGaps);
    Width := Width + FLoadFromFile.Left + FLoadFromFile.Width;
    SetButtonHorizontalPosition(FLoadFromDB,  True, False, LButtonCount, LGaps);
    Width := Width + FLoadFromDB.Left + FLoadFromDB.Width;
    SetButtonHorizontalPosition(FLoadFromCoefFile,  True, False, LButtonCount, LGaps);
    Width := Width + FLoadFromCoefFile.Left + FLoadFromCoefFile.Width;
    SetButtonHorizontalPosition(FResetChartData,  True, False, LButtonCount, LGaps);
    Width := Width + FResetChartData.Left + FResetChartData.Width;
    SetButtonHorizontalPosition(FTogglePlaneMode,  True, False, LButtonCount, LGaps);
    Width := FTogglePlaneMode.Left + FTogglePlaneMode.Width;
    //SetButtonHorizontalPosition(FToggleChartMode,  True, False, LButtonCount, LGaps);
    //Width := FToggleChartMode.Left + FToggleChartMode.Width;
    SetButtonHorizontalPosition(FSaveChart,  True, False, LButtonCount, LGaps);
    Width := FSaveChart.Left + FSaveChart.Width;
    SetButtonHorizontalPosition(FToggleCurveManipulation,  True, False, LButtonCount, LGaps);
    Width := FToggleCurveManipulation.Left + FToggleCurveManipulation.Width;
    SetButtonHorizontalPosition(FDeleteTargetDraft,  True, False, LButtonCount, LGaps);
    Width := FDeleteTargetDraft.Left + FDeleteTargetDraft.Width;
    SetButtonHorizontalPosition(FDeleteChart,  True, False, LButtonCount, LGaps);
    Width := FDeleteChart.Left + FDeleteChart.Width;
    //SetButtonHorizontalPosition(FChartPrintMode,  True, False, LButtonCount, LGaps);
    //Width := FChartPrintMode.Left + FChartPrintMode.Width;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnTogglePlaneMode(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnTogglePlaneMode';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meTogglePlaneMode));
    //FTogglePlaneMode.Down := not FTogglePlaneMode.Down;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnToggleChartMode(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnToggleChartMode';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meToggleChartMode));
    //FToggleChartMode.Down := not FToggleChartMode.Down;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnResetChartData(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnResetChartData';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meResetChartData));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnSaveChart(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnSaveChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeYRCSaveChart,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TYRCToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    if Assigned(FSaveChart) then
      FSaveChart.LanguageHasChanged;

    if Assigned(FLoadFromDB) then
      FLoadFromDB.LanguageHasChanged;

    if Assigned(FLoadFromFile) then
      FLoadFromFile.LanguageHasChanged;

    if Assigned(FLoadFromCoefFile) then
      FLoadFromCoefFile.LanguageHasChanged;

    if Assigned(FTogglePlaneMode) then
      FTogglePlaneMode.LanguageHasChanged;

    if Assigned(FToggleCurveManipulation) then
      FToggleCurveManipulation.LanguageHasChanged;

    if Assigned(FResetChartData) then
      FResetChartData.LanguageHasChanged;

    //if Assigned(FToggleChartMode) then
    //  FToggleChartMode.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetToggleChartModeState(AEnabled: boolean);
const OPNAME = 'TYRCToolBar.SetToggleChartModeState';
begin
  try
//    SetButtonEnabled(FToggleChartMode, AEnabled, 'ToggleChartModeDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetTogglePlaneModeState(AEnabled: boolean);
const OPNAME = 'TYRCToolBar.SetTogglePlaneModeState';
begin
  try
    SetButtonEnabled(FTogglePlaneMode, AEnabled, 'TogglePlaneModeDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TYRCToolBar.SetMenuToggleChartDown(ADown: boolean);
const OPNAME = 'TYRCToolBar.SetMenuToggleChartDown';
begin
  try
    if Assigned(FToggleChartMode) and (FToggleChartMode.Enabled) then
    begin
      FToggleChartMode.Down :=  ADown;
      OnToggleChartMode(FToggleChartMode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
procedure TYRCToolBar.SetMenuTogglePlaneDown(ADown: boolean);
const OPNAME = 'TYRCToolBar.SetMenuTogglePlaneDown';
begin
  try
    if Assigned(FTogglePlaneMode) and (FTogglePlaneMode.Enabled) and (FTogglePlaneMode.Down <>  ADown)then
    begin
      FTogglePlaneMode.Down :=  ADown;
      OnTogglePlaneMode(FTogglePlaneMode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetMenuToggleCurveManipulationDown(ADown: boolean);
const OPNAME = 'TYRCToolBar.SetMenuToggleCurveManipulationDown';
begin
  try
    if Assigned(FToggleCurveManipulation) and
      (FToggleCurveManipulation.Enabled) and
      (FToggleCurveManipulation.Down <>  ADown) then
    begin
      FToggleCurveManipulation.Down :=  ADown;
      OnToggleCurveManipulation(FToggleCurveManipulation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetToggleCurveManipulationState(AEnabled: boolean);
const OPNAME = 'TYRCToolBar.SetToggleCurveManipulationState';
begin
  try
    SetButtonEnabled(FToggleCurveManipulation, AEnabled, 'ToggleCurveManipulationDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCToolBar.SetResetChartDataState(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetResetChartDataState';
begin
  try
    SetButtonEnabled(FResetChartData, AEnabledState, 'ResetChartDataDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetSaveChartState(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetSaveChartState';
begin
  try
    SetButtonEnabled(FSaveChart, AEnabledState, 'SaveChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCToolBar.InPlaneMode: boolean;
const OPNAME = 'TYRCToolBar.InPlaneMode';
begin
  Result := True;
  try
    Result := Assigned(FTogglePlaneMode) and FTogglePlaneMode.Enabled  and  (not FTogglePlaneMode.Down);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TYRCToolBar.InRegressionMode: boolean;
const OPNAME = 'TYRCToolBar.InRegressionMode';
begin
  Result := False;
  try
    Result := Assigned(FToggleChartMode) and FToggleChartMode.Enabled  and  (not FToggleChartMode.Down);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
procedure TYRCToolBar.OnLoadFromDB(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnLoadFromDB';
begin
  try
    FAppModules.Model.ProcessEvent(CmeYRCLoadFromDB,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnLoadFromFile(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnLoadFromFile';
begin
  try
    FAppModules.Model.ProcessEvent(CmeYRCLoadFromFile,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnToggleCurveManipulation(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnToggleCurveManipulation';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meToggleCurveManipulation));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnDeleteTargetDraft(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnDeleteTargetDraft';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meDeleteTargetDraft));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetDeleteTargetDraft(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetDeleteTargetDraft';
begin
  try
    SetButtonEnabled(FDeleteTargetDraft, AEnabledState, 'DeleteTargetDraftDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetLoadFromDBState(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetLoadFromDBState';
begin
  try
    SetButtonEnabled(FLoadFromDB, AEnabledState, 'LoadFromDBDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetLoadFromFileState(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetLoadFromFileState';
begin
  try
    SetButtonEnabled(FLoadFromFile, AEnabledState, 'LoadFromFileDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCToolBar.GetMenuToggleCurveManipulationDown: boolean;
const OPNAME = 'TYRCToolBar.GetMenuToggleCurveManipulationDown';
begin
  Result := False;
  try
    Result := FToggleCurveManipulation.Down;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TYRCToolBar.InPrintMode: boolean;
const OPNAME = 'TYRCToolBar.InPrintMode';
begin
  Result := False;
  try
    Result := Assigned(FChartPrintMode) and FChartPrintMode.Enabled  and  FChartPrintMode.Down;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TYRCToolBar.OnDeleteChart(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnDeleteChart';
begin
  try
    FAppModules.Model.ProcessEvent(CmeYRCDeleteChart,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetDeleteChartState(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetDeleteChartState';
begin
  try
    SetButtonEnabled(FDeleteChart, AEnabledState, 'DeleteChartDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.OnLoadCoefficientFile(Sender: TObject);
const OPNAME = 'TYRCToolBar.OnLoadCoefficientFile';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meLoadCoefficientFile));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCToolBar.SetLoadCoefficientFileState(AEnabledState: boolean);
const OPNAME = 'TYRCToolBar.SetLoadCoefficientFileState';
begin
  try
    SetButtonEnabled(FLoadFromCoefFile, AEnabledState, 'LoadFromCoefFileDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
