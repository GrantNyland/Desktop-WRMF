
//
//  UNIT      : Contains the class TRainfallGaugeSelectionToolBar.
//  AUTHOR    : Philemon Setshedi
//  DATE      : 2004/02/11
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit URainfallGaugeSelectionToolBar;

interface
                                                         
uses
  // Delphi
  VCL.stdctrls,
  VCL.controls,
  // DWAF
  UAbstractComponent,
  UChildToolBar;

const
  C_ButtonSize = 28;
  C_ButtonGap  = 8;

type
  TRainfallDataToolBarState = (
    geFileIsLoaded,
    geFileIsNotLoaded,
    geSelectionIsAvailable,
    geSelectionIsNotAvailable);

  TRainfallGaugeSelectionToolBar = class(TChildToolBar)
  protected
    FCreateReportBtn   : TAbstractSpeedButton;
    FToggleTreeBtn     : TAbstractSpeedButton;
    FImportUserDataBtn : TAbstractSpeedButton;
    FClearUserDataBtn  : TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetState(AState: TRainfallDataToolBarState);
    procedure SetSaveAllState(AState : boolean);
    procedure SetCreateReport(AEnabled: boolean);
    procedure SetToggleTree (AEnabled : boolean);
    procedure SetImportUserData (AEnabled : boolean);
    procedure SetClearUserData (AEnabled : boolean);
    procedure GISNotLoaded(ALoaded: boolean);
    property CreateReportBtn : TAbstractSpeedButton read FCreateReportBtn;
    property ToggleTreeBtn   : TAbstractSpeedButton read FToggleTreeBtn;
    property ImportUserDataBtn : TAbstractSpeedButton read FImportUserDataBtn;
    property ClearUserDataBtn : TAbstractSpeedButton read FClearUserDataBtn;

  end;

implementation

uses
  windows,
  SysUtils,
  UErrorHandlingOperations;

procedure TRainfallGaugeSelectionToolBar.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreateReportBtn  := CreateButton('RDCreateReport');
    FCreateReportBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDCreateReport');

    FToggleTreeBtn := CreateButton('RDGaugeToggleTree');
    FToggleTreeBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDGaugeToggleTree');

    FImportUserDataBtn      := CreateButton('RDImportUserData');
    FImportUserDataBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDImportUserData');

    FClearUserDataBtn      := CreateButton('RDClearUserData');
    FClearUserDataBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDClearUserData');

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetHorizontalPositions;
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    
    SetButtonHorizontalPosition(FCreateReportBtn,   True,  True,  LButtonCount, LGaps);
    SetButtonHorizontalPosition(FToggleTreeBtn,     True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportUserDataBtn, TRUE,  FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FClearUserDataBtn,  TRUE,  FALSE, lButtonCount, lGaps);
    Width := FClearUserDataBtn.Left + FClearUserDataBtn.Width;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FCreateReportBtn.LanguageHasChanged;
    FToggleTreeBtn.LanguageHasChanged;
    FImportUserDataBtn.LanguageHasChanged;
    FClearUserDataBtn.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetState(AState: TRainfallDataToolBarState);
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetState';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.GISNotLoaded(ALoaded: boolean);
const OPNAME = 'TRainfallGaugeSelectionToolBar.GISNotLoaded';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetSaveAllState(AState: boolean);
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetSaveAllState';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetCreateReport(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetCreateReport';
begin
  try
    SetButtonEnabled(FCreateReportBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetToggleTree';
begin
  try
    SetButtonEnabled(FToggleTreeBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetImportUserData (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetImportUserData';
begin
  try
    SetButtonEnabled(FImportUserDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionToolBar.SetClearUserData (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeSelectionToolBar.SetClearUserData';
begin
  try
    SetButtonEnabled(FClearUserDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
