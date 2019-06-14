unit URainfallCatchmentSummaryToolBar;

interface

uses
  VCL.stdctrls,
  VCL.controls,
  VCL.Dialogs,
  UHelpContexts,
  UAbstractComponent,
  UChildToolBar;

type
  TRainfallCatchmentSummaryToolBar = class(TChildToolBar)

  protected
    FExportDataBtn           : TAbstractSpeedButton;
    FToggleTopGridBtn        : TAbstractSpeedButton;
    FToggleMiddleGridBtn     : TAbstractSpeedButton;
    FToggleBottomGridBtn     : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;
    procedure OnClickShowTopGrid(ASender: TObject);
    procedure OnClickShowMiddleGrid(ASender: TObject);
    procedure OnClickShowBottomGrid(ASender: TObject);

  public
    function LanguageHasChanged      : Boolean; override;
    property ToggleTopGridBtn        : TAbstractSpeedButton read FToggleTopGridBtn;
    property ToggleMiddleGridBtn     : TAbstractSpeedButton read FToggleMiddleGridBtn;
    property ToggleBottomGridBtn     : TAbstractSpeedButton read FToggleBottomGridBtn;
    property ExportDataBtn           : TAbstractSpeedButton read FExportDataBtn;
    procedure SetExportDataBtn (AEnabled: Boolean);
    procedure TabHasChanged(AGridTabSelected: boolean);
    procedure SetToggleTopGrid(AEnabled : boolean);
    procedure SetToggleMiddleGrid(AEnabled : boolean);
    procedure SetToggleBottomGrid(AEnabled : boolean);

  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ URainfallCatchmentSummaryToolBar }

procedure TRainfallCatchmentSummaryToolBar.CreateMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FExportDataBtn         := CreateButton('RDEXPORTDATA');
    FToggleTopGridBtn      := CreateButton('RDAVRGRAINFALL');
    FToggleMiddleGridBtn   := CreateButton('RDINPUTRAINFALL');
    FToggleBottomGridBtn   := CreateButton('RDRESULT');

    FToggleTopGridBtn.Hint    := FAppModules.Language.GetString('ButtonHint.ShowStationUsedGrid');
    FToggleMiddleGridBtn.Hint := FAppModules.Language.GetString('ButtonHint.ShowCatchmentOutput');
    FToggleBottomGridBtn.Hint := FAppModules.Language.GetString('ButtonHint.ShowRUNGrid');
    FExportDataBtn.Hint       := FAppModules.Language.GetString('ButtonHint.ExportData');

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallCatchmentSummaryToolBar.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FToggleTopGridBtn.LanguageHasChanged;
    FToggleMiddleGridBtn.LanguageHasChanged;
    FToggleBottomGridBtn.LanguageHasChanged;
    FExportDataBtn.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCatchmentSummaryToolBar.AssignHelpContext;
const OPNAME = 'TRainfallCatchmentSummaryToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FExportDataBtn, -1);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.SetExportDataBtn(AEnabled: Boolean);
const OPNAME = 'TRainfallCatchmentSummaryToolBar.SetExportDataBtn';
begin
  try
    SetButtonEnabled(FExportDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.SetHorizontalPositions;
const OPNAME = 'TRainfallCatchmentSummaryToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FToggleTopGridBtn,    True,   True , lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleMiddleGridBtn, True,   False, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleBottomGridBtn, True,   False, lButtonCount, lGaps);

    SetButtonHorizontalPosition(FExportDataBtn,    True,   True , LButtonCount, LGaps);

    Width := FExportDataBtn.Left + FExportDataBtn.Width;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TRainfallCatchmentSummaryToolBar.TabHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.SetToggleBottomGrid(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryToolBar.SetToggleBottomGrid';
begin
  try
    SetButtonEnabled(FToggleBottomGridBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.SetToggleMiddleGrid(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryToolBar.SetToggleMiddleGrid';
begin
  try
    SetButtonEnabled(FToggleMiddleGridBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.SetToggleTopGrid(AEnabled: boolean);
const OPNAME = 'TRainfallCatchmentSummaryToolBar.SetToggleTopGrid';
begin
  try
    SetButtonEnabled(FToggleTopGridBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.OnClickShowBottomGrid(ASender: TObject);
const OPNAME = 'TRainfallCatchmentSummaryToolBar.OnClickShowBottomGrid';
begin
  try
    FAppModules.Model.ProcessEvent(CmeGenerateWRYMData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.OnClickShowMiddleGrid(ASender: TObject);
const OPNAME ='TRainfallCatchmentSummaryToolBar.OnClickShowMiddleGrid';
begin
  try
    ShowMessage(FAppModules.Language.GetString('Message.MenuClicked'));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryToolBar.OnClickShowTopGrid(ASender: TObject);
const OPNAME ='TRainfallCatchmentSummaryToolBar.OnClickShowTopGrid';
begin
  try
    FAppModules.Model.ProcessEvent(CmeGenerateWRYMData, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
