//
//
//  UNIT      : Contains TYRCMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/09/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCMenuItemManager;

interface

uses
  Classes,
  VCL.Menus,
  UAbstractObject,
  UGUIConstants,
  UYRCToolBar,
  UMenuItemManager;

type
  TYRCPopupMenu = class(TPopupMenu)
  protected
    FAppModules                : TAppModules;

    FCurveFitted               : TMenuItem;
    FAddYValueAtXEquals100     : TMenuItem;
    FForceCurveThrough100      : TMenuItem;
    FAddAdditionalPoints       : TMenuItem;
    FEditYValue                : TMenuItem;
    FManipulateCurvePoints     : TMenuItem;
    FRegressionStartEditing    : TMenuItem;
    FRegressionEndEditing      : TMenuItem;
    FDeterministicStartEditing : TMenuItem;
    FDeterministicEndEditing   : TMenuItem;

    FHideRawDataPoints         : TMenuItem;
    FHideFittedPoints          : TMenuItem;
    FHideRawDataLines          : TMenuItem;

    FMenuSeparator             : TMenuItem;
    FReturnValue : string;
    FOnClick     : TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialize;
    procedure Popup(X, Y: Integer); override;
    procedure OnYValueChange(AYVAlue : string);
    procedure DisableAllItems;
    procedure EnableAllItems;
    procedure ToggleMenuItem(AMenuItem : TMenuItem; AEnabled : boolean);
    property ReturnValue : string read FReturnValue write FReturnValue;
    property CurveFitted               : TMenuItem read FCurveFitted;
    property AddAdditionalPoints       : TMenuItem read FAddAdditionalPoints;
    property EditYValue                : TMenuItem read FEditYValue;
    property ManipulateCurvePoints     : TMenuItem read FManipulateCurvePoints;
    property AddYValueAtXEquals100     : TMenuItem read FAddYValueAtXEquals100;
    property ForceCurveThrough100      : TMenuItem read FForceCurveThrough100;
    property RegressionStartEditing    : TMenuItem read FRegressionStartEditing;
    property RegressionEndEditing      : TMenuItem read FRegressionEndEditing;
    property DeterministicStartEditing : TMenuItem read FDeterministicStartEditing;
    property DeterministicEndEditing   : TMenuItem read FDeterministicEndEditing;
    property HideRawDataPoints         : TMenuItem read FHideRawDataPoints;
    property HideFittedPoints          : TMenuItem read FHideFittedPoints;
    property HideRawDataLines          : TMenuItem read FHideRawDataLines;
    property AppModules                : TAppModules read FAppModules write FAppModules;
  end;

  TYRCMenuItemManager = class(TMenuItemManager)
  protected
    FToolBar: TYRCToolBar;
    FYRCPopupMenu : TYRCPopupMenu;

    FInRegressionMode : boolean;
    FInEditMode       : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    procedure SetMenuTogglePlaneMode(AAction: TMenuSetAction);
    procedure SetMenuToggleChartMode(AAction: TMenuSetAction);
    procedure SetMenuToggleCurveManipulationMode(AAction: TMenuSetAction);
    procedure SetMenuTogglePlaneDown(ADown: boolean);
    procedure SetMenuToggleCurveManipulationDown(ADown: boolean);
    //procedure SetMenuToggleChartDown(ADown: boolean);

    procedure SetMenuDeleteTargetDraft(AAction: TMenuSetAction);
    //procedure SetMenuChartPrintMode(AAction: TMenuSetAction);
    procedure SetMenuEditYValueMode(AAction: TMenuSetAction);
    procedure SetMenuStartRegressionEditMode(AAction: TMenuSetAction);
    procedure SetMenuEndRegressionEditMode(AAction: TMenuSetAction);
    procedure SetMenuStartDeterministicEditMode(AAction: TMenuSetAction);
    procedure SetMenuEndDeterministicEditMode(AAction: TMenuSetAction);

    procedure SetMenuResetChartData(AAction: TMenuSetAction);
    procedure SetMenuSaveChart(AAction: TMenuSetAction);
    procedure SetMenuDeleteChart(AAction: TMenuSetAction);
    procedure SetLoadFromDB(AAction: TMenuSetAction);
    procedure SetLoadFromFile(AAction: TMenuSetAction);
    procedure SetLoadCoefficientFile(AAction: TMenuSetAction);

    procedure SetAll(AAction: TMenuSetAction);
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function InPlaneMode: boolean;
    function RegressionMode: boolean;
    property ToolBar: TYRCToolBar read FToolBar;
    property YRCPopupMenu : TYRCPopupMenu read FYRCPopupMenu;

    property InRegressionMode : boolean read FInRegressionMode write FInRegressionMode;
    property InEditMode       : boolean read FInEditMode write FInEditMode;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

const
  // View menu items.
  CYRCChartSep                : array[0..1] of WideString = ('View','YRCChartSep');
  CYRCResetChartData          : array[0..1] of WideString = ('View','YRCResetChartData');
  CYRCTogglePlaneMode         : array[0..1] of WideString = ('View','YRCTogglePlaneMode');
  CYRCToggleChartMode         : array[0..1] of WideString = ('View','YRCToggleChartMode');
  CYRCSaveChart               : array[0..1] of WideString = ('View','YRCSaveChart');
  CYRCDeleteChart             : array[0..1] of WideString = ('View','YRCDeleteChart');
  CYRCLoadFromDB              : array[0..1] of WideString = ('View','YRCLoadFromDB');
  CYRCLoadFromFile            : array[0..1] of WideString = ('View','YRCLoadFromFile');
  CYRCLoadCoefFile            : array[0..1] of WideString = ('View','YRCLoadCoefFile');
  CYRCToggleCurveManipulation : array[0..1] of WideString = ('View','YRCToggleCurveManipulation');

  CYRCDeleteTargetDraft       : array[0..1] of WideString = ('Edit','YRCDeleteTargetDraft');
  //CYRCChartPrintMode          : array[0..1] of WideString = ('Edit','YRCChartPrintMode');
  CYRCEditYValue              : array[0..1] of WideString = ('Edit','YRCEditYValue');
  CYRCStartRegressionEdit     : array[0..1] of WideString = ('Edit','YRCStartRegressionEdit');
  CYRCEndRegressionEdit       : array[0..1] of WideString = ('Edit','YRCEndRegressionEdit');
  CYRCStartDeterministicEdit  : array[0..1] of WideString = ('Edit','YRCStartDeterministicEdit');
  CYRCEndDeterministicEdit    : array[0..1] of WideString = ('Edit','YRCEndDeterministicEdit');


{ TYRCMenuItemManager }

procedure TYRCMenuItemManager.AddMenuItems;
const OPNAME = 'TYRCMenuItemManager.AddMenuItems';
begin
  try
    // View menu items.
    AddMenuItemEntry(CYRCChartSep,                1700, CmeSeparator);
    AddMenuItemEntry(CYRCLoadFromFile,            1710, CmeYRCLoadFromFile, nil);
    AddMenuItemEntry(CYRCLoadFromDB,              1715, CmeYRCLoadFromDB, nil);

    AddMenuItemEntry(CYRCLoadCoefFile,            1717, CmeCustomModelEvent, TModelMenuData.Create(meLoadCoefficientFile));
    AddMenuItemEntry(CYRCResetChartData,          1720, CmeCustomModelEvent, TModelMenuData.Create(meResetChartData));
    AddMenuItemEntry(CYRCTogglePlaneMode,         1725, CmeCustomModelEvent, TModelMenuData.Create(meTogglePlaneMode));
    AddMenuItemEntry(CYRCToggleChartMode,         1730, CmeCustomModelEvent, TModelMenuData.Create(meToggleChartMode));
    AddMenuItemEntry(CYRCSaveChart,               1740, CmeYRCSaveChart, nil);
    AddMenuItemEntry(CYRCDeleteChart,             1745, CmeYRCDeleteChart, nil);

    AddMenuItemEntry(CYRCToggleCurveManipulation, 1750, CmeCustomModelEvent, TModelMenuData.Create(meToggleCurveManipulation));

    AddMenuItemEntry(CYRCDeleteTargetDraft,       1755, CmeCustomModelEvent, TModelMenuData.Create(meDeleteTargetDraft));
    AddMenuItemEntry(CYRCEditYValue,              1760, CmeCustomModelEvent, TModelMenuData.Create(meEditYValue));
    AddMenuItemEntry(CYRCStartRegressionEdit,     1770, CmeCustomModelEvent, TModelMenuData.Create(meStartRegressionEdit));
    AddMenuItemEntry(CYRCEndRegressionEdit,       1780, CmeCustomModelEvent, TModelMenuData.Create(meEndRegressionEdit));
    AddMenuItemEntry(CYRCStartDeterministicEdit,  1790, CmeCustomModelEvent, TModelMenuData.Create(meStartDeterministicEdit));
    AddMenuItemEntry(CYRCEndDeterministicEdit,    1800, CmeCustomModelEvent, TModelMenuData.Create(meEndDeterministicEdit));

    Disable;
    Hide;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.CreateMemberObjects;
const OPNAME = 'TYRCMenuItemManager.CreateMemberObjects';
begin
  inherited;
  try
    FToolBar := TYRCToolBar.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TYRCMenuItemManager.DestroyMemberObjects';
begin
  inherited;
  FreeAndNil(FToolBar);
end;

function TYRCMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TYRCMenuItemManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    SetAll(msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TYRCMenuItemManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result then
      Result := FToolBar.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCMenuItemManager.SetMenuResetChartData(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuResetChartData';
begin
  try
    FToolBar.SetResetChartDataState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCResetChartData, AAction, 'ResetChartDataDisabled');
    else
      FAppModules.SetMenuItem(CYRCResetChartData, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuSaveChart(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuSaveChart';
begin
  try
    FToolBar.SetSaveChartState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCSaveChart, AAction, 'SaveChartDisabled');
    else
      FAppModules.SetMenuItem(CYRCSaveChart, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuDeleteChart(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuDeleteChart';
begin
  try
    FToolBar.SetDeleteChartState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCDeleteChart, AAction, 'DeleteChartDisabled');
    else
      FAppModules.SetMenuItem(CYRCDeleteChart, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuTogglePlaneMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuTogglePlaneMode';
begin
  try
    FToolBar.SetTogglePlaneModeState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCTogglePlaneMode, AAction, 'TogglePlaneModeDisabled');
    else
      FAppModules.SetMenuItem(CYRCTogglePlaneMode, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuToggleChartMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuToggleChartMode';
begin
  try
    FToolBar.SetToggleChartModeState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCToggleChartMode, AAction, 'ToggleChartModeDisabled');
    else
      FAppModules.SetMenuItem(CYRCToggleChartMode, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TYRCMenuItemManager.SetMenuToggleChartDown(ADown: boolean);
const OPNAME = 'TYRCMenuItemManager.SetMenuToggleChartDown';
begin
  try
    FToolBar.SetMenuToggleChartDown(ADown);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
procedure TYRCMenuItemManager.SetMenuTogglePlaneDown(ADown: boolean);
const OPNAME = 'TYRCMenuItemManager.SetMenuTogglePlaneDown';
begin
  try
    FToolBar.SetMenuTogglePlaneDown(ADown);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetAll(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetAll';
begin
  try
    SetMenuResetChartData(AAction);
    SetMenuTogglePlaneMode(AAction);
    SetMenuToggleChartMode(AAction);
    SetMenuSaveChart(AAction);
    SetLoadFromDB(AAction);
    SetMenuDeleteChart(AAction);
    SetLoadFromFile(AAction);
    SetLoadCoefficientFile(AAction);
    SetMenuToggleCurveManipulationMode(AAction);

    SetMenuDeleteTargetDraft(AAction);
    //SetMenuChartPrintMode(AAction);
    SetMenuEditYValueMode(AAction);
    SetMenuStartRegressionEditMode(AAction);
    SetMenuEndRegressionEditMode(AAction);
    SetMenuStartDeterministicEditMode(AAction);
    SetMenuEndDeterministicEditMode(AAction);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMenuItemManager.InPlaneMode: boolean;
const OPNAME = 'TYRCMenuItemManager.InPlaneMode';
begin
  Result := False;
  try
    Result := FToolBar.InPlaneMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMenuItemManager.RegressionMode: boolean;
const OPNAME = 'TYRCMenuItemManager.RegressionMode';
begin
  Result := False;
  try
    Result := FInRegressionMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetLoadFromDB(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetLoadFromDB';
begin
  try
    FToolBar.SetLoadFromDBState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCLoadFromDB, AAction, 'LoadFromDBDisabled');
    else
      FAppModules.SetMenuItem(CYRCLoadFromDB, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetLoadFromFile(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetLoadFromFile';
begin
  try
    FToolBar.SetLoadFromFileState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCLoadFromFile, AAction, 'LoadFromFileDisabled');
    else
      FAppModules.SetMenuItem(CYRCLoadFromFile, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuToggleCurveManipulationDown(ADown: boolean);
const OPNAME = 'TYRCMenuItemManager.SetMenuToggleCurveManipulationDown';
begin
  try
    FToolBar.SetMenuToggleCurveManipulationDown(ADown);
    if ADown then
      FAppModules.SetMenuItem(CYRCToggleCurveManipulation, msChecked)
    else
      FAppModules.SetMenuItem(CYRCToggleCurveManipulation, msUnChecked);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuToggleCurveManipulationMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuToggleCurveManipulationMode';
begin
  try
    FToolBar.SetToggleCurveManipulationState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCToggleCurveManipulation, AAction, 'ToggleCurveManipulationDisabled');
    else
      FAppModules.SetMenuItem(CYRCToggleCurveManipulation, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuEditYValueMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuEditYValueMode';
begin
  try
    FAppModules.SetMenuItem(CYRCEditYValue, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuEndDeterministicEditMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuEndDeterministicEditMode';
begin
  try
    FAppModules.SetMenuItem(CYRCEndDeterministicEdit, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuEndRegressionEditMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuEndRegressionEditMode';
begin
  try
    FAppModules.SetMenuItem(CYRCEndRegressionEdit, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuStartDeterministicEditMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuStartDeterministicEditMode';
begin
  try
    FAppModules.SetMenuItem(CYRCStartDeterministicEdit, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuStartRegressionEditMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuStartRegressionEditMode';
begin
  try
    FAppModules.SetMenuItem(CYRCStartRegressionEdit, AAction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetMenuDeleteTargetDraft(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuDeleteTargetDraft';
begin
  try
    FToolBar.SetDeleteTargetDraft(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCDeleteTargetDraft, AAction, 'DeleteTargetDraftDisabled');
    else
      FAppModules.SetMenuItem(CYRCDeleteTargetDraft, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMenuItemManager.SetLoadCoefficientFile(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetLoadCoefficientFile';
begin
  try
    FToolBar.SetLoadCoefficientFileState(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCLoadCoefFile, AAction, 'LoadFromCoefFileDisabled');
    else
      FAppModules.SetMenuItem(CYRCLoadCoefFile, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TYRCMenuItemManager.SetMenuChartPrintMode(AAction: TMenuSetAction);
const OPNAME = 'TYRCMenuItemManager.SetMenuChartPrintMode';
begin
  try
    FToolBar.SetChartPrintMode(AAction = msEnable);
    case AAction of
      msDisable : FAppModules.SetMenuItem(CYRCChartPrintMode, AAction, 'ChartPrintModeDisabled');
    else
      FAppModules.SetMenuItem(CYRCChartPrintMode, AAction);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{ TYRCPopupMenu }

constructor TYRCPopupMenu.Create(AOwner: TComponent);
const OPNAME = 'TYRCPopupMenu.Create';
begin
  try
    inherited Create(AOwner);
    FReturnValue := '';
    AutoPopup    := False;
    FOnClick     := nil;
    FAppModules  := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCPopupMenu.DisableAllItems;
const OPNAME = 'TYRCPopupMenu.DisableAllItems';
begin
  try
    FCurveFitted.Enabled               := False;
    FAddYValueAtXEquals100.Enabled     := False;
    FForceCurveThrough100.Enabled      := False;
    FAddAdditionalPoints.Enabled       := False;
    FManipulateCurvePoints.Enabled     := False;
    FHideRawDataPoints.Enabled         := False;
    FHideFittedPoints.Enabled          := False;
    FHideRawDataLines.Enabled          := False;
    FRegressionStartEditing.Enabled    := False;
    FRegressionEndEditing.Enabled      := False;
    FDeterministicStartEditing.Enabled := False;
    FDeterministicEndEditing.Enabled   := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPopupMenu.EnableAllItems;
const OPNAME = 'TYRCPopupMenu.EnableAllItems';
begin
  try
    FCurveFitted.Enabled               := True;
    FAddYValueAtXEquals100.Enabled     := True;
    FForceCurveThrough100.Enabled      := True;
    FAddAdditionalPoints.Enabled       := True;
    FManipulateCurvePoints.Enabled     := True;
    FHideRawDataPoints.Enabled         := True;
    FHideFittedPoints.Enabled          := True;
    FHideRawDataLines.Enabled          := True;
    FRegressionStartEditing.Enabled    := True;
    FRegressionEndEditing.Enabled      := False;
    FDeterministicStartEditing.Enabled := True;
    FDeterministicEndEditing.Enabled   := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPopupMenu.Initialize;
const OPNAME = 'TYRCPopupMenu.Initialize';
var
  LEditMenuItemsRegression,
  LEditMenuItemsDeterministic : array [0..1] of TMenuItem;
begin
  try
    if Assigned(FAppModules) then
    begin
      FCurveFitted               := TMenuItem.Create(Self);
      FAddYValueAtXEquals100     := TMenuItem.Create(Self);
      FForceCurveThrough100      := TMenuItem.Create(Self);
      FAddAdditionalPoints       := TMenuItem.Create(Self);
      FEditYValue                := TMenuItem.Create(Self);
      FManipulateCurvePoints     := TMenuItem.Create(Self);
      FRegressionStartEditing    := TMenuItem.Create(Self);
      FRegressionEndEditing      := TMenuItem.Create(Self);
      FDeterministicStartEditing := TMenuItem.Create(Self);
      FDeterministicEndEditing   := TMenuItem.Create(Self);

      FHideRawDataPoints         := TMenuItem.Create(Self);
      FHideFittedPoints          := TMenuItem.Create(Self);
      FHideRawDataLines          := TMenuItem.Create(Self);

      FMenuSeparator             := TMenuItem.Create(Self);

      FCurveFitted.Caption               := FAppModules.Language.GetString('YRCPopup.CurveFitted');
      FRegressionStartEditing.Caption    := FAppModules.Language.GetString('YRCPopup.StartEdit');
      FRegressionEndEditing.Caption      := FAppModules.Language.GetString('YRCPopup.EndEdit');
      FDeterministicStartEditing.Caption := FAppModules.Language.GetString('YRCPopup.StartEdit');
      FDeterministicEndEditing.Caption   := FAppModules.Language.GetString('YRCPopup.EndEdit');

      FHideRawDataPoints.Caption         := FAppModules.Language.GetString('YRCPopup.HideAllRawPoints');
      FHideFittedPoints.Caption          := FAppModules.Language.GetString('YRCPopup.HideFittedPoints');
      FHideRawDataLines.Caption          := FAppModules.Language.GetString('YieldReliability.Caption');

      FMenuSeparator.Caption             := FAppModules.Language.GetString('YRCPopup.Seperator');

      FRegressionStartEditing.Tag    := 1;
      FRegressionEndEditing.Tag      := 2;
      FDeterministicStartEditing.Tag := 3;
      FDeterministicEndEditing.Tag   := 4;

      FHideRawDataPoints.Tag         := 5;
      FHideRawDataLines.Tag          := 6;
      FHideFittedPoints.Tag          := 7;

      LEditMenuItemsRegression[0]      := FRegressionStartEditing;
      LEditMenuItemsRegression[1]      := FRegressionEndEditing;
      LEditMenuItemsDeterministic[0]   := FDeterministicStartEditing;
      LEditMenuItemsDeterministic[1]   := FDeterministicEndEditing;

      FAddYValueAtXEquals100.Caption                    := FAppModules.Language.GetString('YRCPopup.YValue') + FReturnValue;
      FForceCurveThrough100.Caption                     := FAppModules.Language.GetString('YRCPopup.ForceCurve');
      FEditYValue.Caption                               := FAppModules.Language.GetString('YRCPopup.EditYValue');
      FAddYValueAtXEquals100.Add(FEditYValue);
      FAddYValueAtXEquals100.Add(FForceCurveThrough100);

      FAddAdditionalPoints.Caption                       := FAppModules.Language.GetString('YRCPopup.AddRegressionPoints');
      FAddAdditionalPoints.Add(LEditMenuItemsRegression);

      FManipulateCurvePoints.Caption                     := FAppModules.Language.GetString('YRCPopup.ManipulateDeterministic');
      FManipulateCurvePoints.Add(LEditMenuItemsDeterministic);

      Self.Items.Add(FHideRawDataPoints);
      Self.Items.Add(FHideRawDataLines);
      Self.Items.Add(FHideFittedPoints);

      Self.Items.Add(FMenuSeparator);

      Self.Items.Add(FCurveFitted);
      Self.Items.Add(FAddYValueAtXEquals100);
      Self.Items.Add(FAddAdditionalPoints);
      Self.Items.Add(FManipulateCurvePoints);
      FRegressionEndEditing.Enabled       := False;
      FDeterministicEndEditing.Enabled    := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPopupMenu.OnYValueChange(AYVAlue: string);
const OPNAME = 'TYRCPopupMenu.OnYValueChange';
begin
  try
    FAddYValueAtXEquals100.Caption := FAppModules.Language.GetString('YieldReliability.Caption1') + AYVAlue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPopupMenu.Popup(X, Y: Integer);
const OPNAME = 'TYRCPopupMenu.Popup';
begin
  try
    inherited Popup(X, Y);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPopupMenu.ToggleMenuItem(AMenuItem: TMenuItem;AEnabled: boolean);
const OPNAME = 'TYRCPopupMenu.ToggleMenuItem';
begin
  try
    AMenuItem.Enabled := AEnabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
