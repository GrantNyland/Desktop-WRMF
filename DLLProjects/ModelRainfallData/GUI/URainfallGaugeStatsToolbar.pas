{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeStatsToolBar Class                     *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 13/01/2005                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeStatsToolbar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TRainfallGaugeStatsToolBar = class(TChildToolBar)
  protected
    FCreateFilesBtn : TAbstractSpeedButton;
    FCreateSplitBtn : TAbstractSpeedButton;
    FUpdateSplitBtn : TAbstractSpeedButton;
    FDeleteSplitBtn : TAbstractSpeedButton;
    FToggleGridBtn  : TAbstractSpeedButton;
    FToggleGraphBtn : TAbstractSpeedButton;
    FToggleTreeBtn  : TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetCreateFiles(AEnabled: boolean);
    procedure SetCreateSplit(AEnabled: boolean);
    procedure SetUpdateSplit(AEnabled: boolean);
    procedure SetDeleteSplit(AEnabled: boolean);
    procedure SetToggleGrid (AEnabled : boolean);
    procedure SetToggleGraph (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    property ToggleGridBtn  : TAbstractSpeedButton read FToggleGridBtn;
    property ToggleGraphBtn : TAbstractSpeedButton read FToggleGraphBtn;
    property ToggleTreeBtn  : TAbstractSpeedButton read FToggleTreeBtn;
    property CreateFilesBtn : TAbstractSpeedButton read FCreateFilesBtn;
    property CreateSplitBtn : TAbstractSpeedButton read FCreateSplitBtn;
    property UpdateSplitBtn : TAbstractSpeedButton read FUpdateSplitBtn;
    property DeleteSplitBtn : TAbstractSpeedButton read FDeleteSplitBtn;
  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TRainfallGaugeStatsToolBar.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeStatsToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreateFilesBtn  := CreateButton('RDCreateFiles');
    FCreateSplitBtn  := CreateButton('RDCreateSplit');
    FUpdateSplitBtn  := CreateButton('RDUpdateSplit');
    FDeleteSplitBtn  := CreateButton('RDDeleteSplit');
    FToggleGridBtn   := CreateButton('RDStatsToggleGrid');
    FToggleGraphBtn  := CreateButton('RDStatsToggleGraph');
    FToggleTreeBtn   := CreateButton('RDStatsToggleTree');

    FCreateFilesBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDCreateFiles');
    FCreateSplitBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDCreateSplit');
    FUpdateSplitBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDUpdateSplit');
    FDeleteSplitBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDDeleteSplit');
    FToggleGridBtn.Hint  := FAppModules.Language.GetString('ButtonHint.RDStatsToggleGrid');
    FToggleGraphBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDStatsToggleGraph');
    FToggleTreeBtn.Hint  := FAppModules.Language.GetString('ButtonHint.RDStatsToggleTree' );

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsToolBar.AssignHelpContext;
const OPNAME = 'TRainfallGaugeStatsToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FCreateFilesBtn, -1);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeStatsToolBar.SetHorizontalPositions;
const OPNAME = 'TRainfallGaugeStatsToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FToggleGridBtn,  True,   TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleGraphBtn, True,  False, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleTreeBtn,  True,  False, LButtonCount, LGaps);

    SetButtonHorizontalPosition(FCreateFilesBtn, True,   TRUE, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCreateSplitBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FUpdateSplitBtn, True,  False, LButtonCount, LGaps);    
    SetButtonHorizontalPosition(FDeleteSplitBtn, True,  False, LButtonCount, LGaps);

    Width := FDeleteSplitBtn.Left + FDeleteSplitBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeStatsToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FCreateFilesBtn.LanguageHasChanged;
    FCreateSplitBtn.LanguageHasChanged;
    FUpdateSplitBtn.LanguageHasChanged;
    FDeleteSplitBtn.LanguageHasChanged;
    FToggleGridBtn.LanguageHasChanged;
    FToggleGraphBtn.LanguageHasChanged;
    FToggleTreeBtn.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsToolBar.SetCreateFiles(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetCreateFiles';
begin
  try
    SetButtonEnabled(FCreateFilesBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsToolBar.SetCreateSplit(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetCreateSplit';
begin
  try
    SetButtonEnabled(FCreateSplitBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRainfallGaugeStatsToolBar.SetUpdateSplit(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetUpdateSplit';
begin
  try
    SetButtonEnabled(FUpdateSplitBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsToolBar.SetDeleteSplit(AEnabled: boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetDeleteSplit';
begin
  try
    SetButtonEnabled(FDeleteSplitBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsToolBar.SetToggleGrid (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetToggleGrid';
begin
  try
    SetButtonEnabled(FToggleGridBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsToolBar.SetToggleGraph (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetToggleGraph';
begin
  try
    SetButtonEnabled(FToggleGraphBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsToolBar.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallGaugeStatsToolBar.SetToggleTree';
begin
  try
    SetButtonEnabled(FToggleTreeBtn, aEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
