{******************************************************************************}
{*  UNIT      : Contains TRainfallGraphToolBar Class                          *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 17/01/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}


unit URainfallGraphToolBar;

interface
                                                            
uses
  VCL.stdctrls,
  VCL.controls,
  UAbstractComponent,
  UChildToolBar;

type

  TRainfallGraphToolBar = class ( TChildToolBar )
  protected
    FToggleGridBtn       : TAbstractSpeedButton;
    FToggleGraphBtn      : TAbstractSpeedButton;
    FToggleTreeBtn       : TAbstractSpeedButton;
    FCreatePATFilesBtn   : TAbstractSpeedButton;
    FHighLightBtn        : TAbstractSpeedButton;
    FSelectRAWFlagsBtn   : TAbstractSpeedButton;
    FFlagDataBtn         : TAbstractSpeedButton;
    FUnFlagDataBtn       : TAbstractSpeedButton;
    FFlagSetupBtn        : TAbstractSpeedButton;
    FFlagClickBtn        : TAbstractSpeedButton;
    FWeatherEventsBtn    : TAbstractSpeedButton;
    FFirstRecordBtn      : TAbstractSpeedButton;
    FPrevRecordBtn       : TAbstractSpeedButton;
    FNextRecordBtn       : TAbstractSpeedButton;
    FLastRecordBtn       : TAbstractSpeedButton;
    FParameterChangesBtn : TAbstractSpeedButton;
    FPatchChangeListBtn  : TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnClickParameterChange(ASender: TObject);
  public
    function Initialise : Boolean; override;
    function LanguageHasChanged : boolean; override;
    procedure SetToggleGrid (AEnabled : boolean);
    procedure SetToggleGraph (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    procedure SetCreatePATFiles (AEnabled : boolean);
    procedure SetHighLight (AEnabled : boolean);
    procedure SetSelectRAWFlags (AEnabled : boolean);
    procedure SetFlagData (AEnabled : boolean);
    procedure SetUnFlagData (AEnabled : boolean);
    procedure SetFlagSetup (AEnabled : boolean);
    procedure SetFlagClick (AEnabled : boolean);
    procedure SetWeatherEvents (AEnabled : boolean);
    procedure SetFirstRecord (AEnabled : boolean);
    procedure SetPrevRecord (AEnabled : boolean);
    procedure SetNextRecord (AEnabled : boolean);
    procedure SetLastRecord (AEnabled : boolean);
    procedure SetAllButtons (AEnabled : boolean);
    procedure SetParameterChange (AEnabled : boolean);
    procedure SetPatchChangelistBtn (AEnabled : boolean);

    property ToggleGridBtn           : TAbstractSpeedButton read FToggleGridBtn;
    property ToggleGraphBtn          : TAbstractSpeedButton read FToggleGraphBtn;
    property ToggleTreeBtn           : TAbstractSpeedButton read FToggleTreeBtn;
    property CreatePATFilesBtn       : TAbstractSpeedButton read FCreatePATFilesBtn;
    property HighLightBtn            : TAbstractSpeedButton read FHighLightBtn;
    property SelectRAWFlagsBtn       : TAbstractSpeedButton read FSelectRAWFlagsBtn;
    property FlagDataBtn             : TAbstractSpeedButton read FFlagDataBtn;
    property UnFlagDataBtn           : TAbstractSpeedButton read FUnFlagDataBtn;
    property FlagSetupBtn            : TAbstractSpeedButton read FFlagSetupBtn;
    property FlagClickBtn            : TAbstractSpeedButton read FFlagClickBtn;
    property WeatherEventsBtn        : TAbstractSpeedButton read FWeatherEventsBtn;
    property FirstRecordBtn          : TAbstractSpeedButton read FFirstRecordBtn;
    property PrevRecordBtn           : TAbstractSpeedButton read FPrevRecordBtn;
    property NextRecordBtn           : TAbstractSpeedButton read FNextRecordBtn;
    property LastRecordBtn           : TAbstractSpeedButton read FLastRecordBtn;
    property ParameterChangesBtn     : TAbstractSpeedButton read FParameterChangesBtn;
    property PatchChangeListBtn      : TAbstractSpeedButton read FPatchChangeListBtn;
end;

implementation

uses
  windows,
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TRainfallGraphToolBar }

procedure TRainfallGraphToolBar.CreateMemberObjects;
const OPNAME = 'TRainfallGraphToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FToggleGridBtn     := CreateButton('RDGraphToggleGrid');
    FToggleGraphBtn    := CreateButton('RDGraphToggleGraph');
    FToggleTreeBtn     := CreateButton('RDGraphToggleTree');
    FCreatePATFilesBtn := CreateButton('RDCreatePATFiles');
    FHighLightBtn      := CreateButton('RDHighLightOutliers');
    FSelectRAWFlagsBtn := CreateButton('RDSelectRAWFlags');
    FFlagDataBtn       := CreateButton('RDFlagDataBlock');
    FUnFlagDataBtn     := CreateButton('RDUnFlagDataBlock');
    FFlagSetupBtn      := CreateButton('RDFlagSetup');
    FFlagClickBtn      := CreateButton('RDFlagClick');

    FFlagClickBtn.GroupIndex := 301;
    FFlagClickBtn.AllowAllUp := True;

    FWeatherEventsBtn    := CreateButton('RDWeatherEvents');
    FFirstRecordBtn      := CreateButton('RDFirstWeatherRecord');
    FPrevRecordBtn       := CreateButton('RDPriorWeatherRecord');
    FNextRecordBtn       := CreateButton('RDNextWeatherRecord');
    FLastRecordBtn       := CreateButton('RDLastWeatherRecord');
    FPatchChangelistBtn  := CreateButton('CLPatchChangelists');

    FParameterChangesBtn := CreateButton('CLParameter');
    FParameterChangesBtn.OnClick := OnClickParameterChange;
    FParameterChangesBtn.Enabled := False;

    FToggleGridBtn.Hint       := FAppModules.Language.GetString('ButtonHint.RDGraphToggleGrid');
    FToggleGraphBtn.Hint      := FAppModules.Language.GetString('ButtonHint.RDGraphToggleGraph');
    FToggleTreeBtn.Hint       := FAppModules.Language.GetString('ButtonHint.RDGraphToggleTree');
    FCreatePATFilesBtn.Hint   := FAppModules.Language.GetString('ButtonHint.RDCreatePATFiles');
    FHighLightBtn.Hint        := FAppModules.Language.GetString('ButtonHint.RDHighLightOutliers');
    FSelectRAWFlagsBtn.Hint   := FAppModules.Language.GetString('ButtonHint.RDSelectRAWFlags');
    FFlagDataBtn.Hint         := FAppModules.Language.GetString('ButtonHint.RDFlagDataBlock');
    FUnFlagDataBtn.Hint       := FAppModules.Language.GetString('ButtonHint.RDUnFlagDataBlock');
    FFlagSetupBtn.Hint        := FAppModules.Language.GetString('ButtonHint.RDFlagSetup');
    FFlagClickBtn.Hint        := FAppModules.Language.GetString('ButtonHint.RDFlagClick');
    FWeatherEventsBtn.Hint    := FAppModules.Language.GetString('ButtonHint.RDWeatherEvents');
    FFirstRecordBtn.Hint      := FAppModules.Language.GetString('ButtonHint.RDFirstWeatherRecord');
    FPrevRecordBtn.Hint       := FAppModules.Language.GetString('ButtonHint.RDPriorWeatherRecord');
    FNextRecordBtn.Hint       := FAppModules.Language.GetString('ButtonHint.RDNextWeatherRecord');
    FLastRecordBtn.Hint       := FAppModules.Language.GetString('ButtonHint.RDLastWeatherRecord');
    FParameterChangesBtn.Hint := FAppModules.Language.GetString('ButtonHint.CLParameter');
    FPatchChangeListBtn.Hint := FAppModules.Language.GetString('ButtonHint.CLPatchChangelists');

    SetHorizontalPositions;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphToolBar.Initialise : Boolean;
const OPNAME = 'TRainfallGraphToolBar.Initialise';
begin
  Result := True;
  try
    FParameterChangesBtn.Enabled := false;
 except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphToolBar.LanguageHasChanged : boolean;
const OPNAME = 'TRainfallGraphToolBar.LanguageHasChanged';
begin
  Result := True;
  inherited LanguageHasChanged;
  try
    FToggleGridBtn.LanguageHasChanged;
    FToggleGraphBtn.LanguageHasChanged;
    FToggleTreeBtn.LanguageHasChanged;
    FCreatePATFilesBtn.LanguageHasChanged;
    FHighLightBtn.LanguageHasChanged;
    FSelectRAWFlagsBtn.LanguageHasChanged;
    FFlagDataBtn.LanguageHasChanged;
    FUnFlagDataBtn.LanguageHasChanged;
    FFlagSetupBtn.LanguageHasChanged;
    FFlagClickBtn.LanguageHasChanged;
    FWeatherEventsBtn.LanguageHasChanged;
    FFirstRecordBtn.LanguageHasChanged;
    FPrevRecordBtn.LanguageHasChanged;
    FNextRecordBtn.LanguageHasChanged;
    FLastRecordBtn.LanguageHasChanged;
    FParameterChangesBtn.LanguageHasChanged;
    FPatchChangeListBtn.LanguageHasChanged;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphToolBar.SetToggleGrid (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetToggleGrid';
begin
  try
    SetButtonEnabled(FToggleGridBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphToolBar.SetToggleGraph (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetToggleGraph';
begin
  try
    SetButtonEnabled(FToggleGraphBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphToolBar.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetToggleTree';
begin
  try
    SetButtonEnabled(FToggleTreeBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetCreatePATFiles (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetCreatePATFiles';
begin
  try
    SetButtonEnabled(FCreatePATFilesBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetHighLight (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetHighLight';
begin
  try
    SetButtonEnabled(FHighLightBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetSelectRAWFlags (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetSelectRAWFlags';
begin
  try
    SetButtonEnabled(FSelectRAWFlagsBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphToolBar.SetFlagData (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetFlagData';
begin
  try
    SetButtonEnabled(FFlagDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetUnFlagData (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetUnFlagData';
begin
  try
    SetButtonEnabled(FUnFlagDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetFlagSetup (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetFlagSetup';
begin
  try
    SetButtonEnabled(FFlagSetupBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetFlagClick (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetFlagClick';
begin
  try
    SetButtonEnabled(FFlagClickBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetWeatherEvents (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetWeatherEvents';
begin
  try
    SetButtonEnabled(FWeatherEventsBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetFirstRecord (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetFirstRecord';
begin
  try
    SetButtonEnabled(FFirstRecordBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetPrevRecord (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetPrevRecord';
begin
  try
    SetButtonEnabled(FPrevRecordBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetNextRecord (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetNextRecord';
begin
  try
    SetButtonEnabled(FNextRecordBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetLastRecord (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetLastRecord';
begin
  try
    SetButtonEnabled(FLastRecordBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetParameterChange(AEnabled: boolean);
const OPNAME = 'TRainfallGraphToolBar.SetParameterChange';
begin
  try
    SetButtonEnabled(FParameterChangesBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphToolBar.SetAllButtons (AEnabled : boolean);
const OPNAME = 'TRainfallGraphToolBar.SetAllButtons';
begin
  try
    SetButtonEnabled(FToggleGridBtn,     AEnabled, '');
    SetButtonEnabled(FToggleGraphBtn,    AEnabled, '');
    SetButtonEnabled(FToggleTreeBtn,     AEnabled, '');
    SetButtonEnabled(FCreatePATFilesBtn, AEnabled, '');
    SetButtonEnabled(FHighLightBtn,      AEnabled, '');
    SetButtonEnabled(FFlagDataBtn,       AEnabled, '');
    SetButtonEnabled(FUnFlagDataBtn,     AEnabled, '');
    SetButtonEnabled(FFlagSetupBtn,      AEnabled, '');
    SetButtonEnabled(FFlagClickBtn,      AEnabled, '');
    SetButtonEnabled(FWeatherEventsBtn,  AEnabled, '');
    SetButtonEnabled(FFirstRecordBtn,    AEnabled, '');
    SetButtonEnabled(FPrevRecordBtn,     AEnabled, '');
    SetButtonEnabled(FNextRecordBtn,     AEnabled, '');
    SetButtonEnabled(FLastRecordBtn,     AEnabled, '');
    SetButtonEnabled(FParameterChangesBtn,AEnabled, '');
    SetButtonEnabled(FPatchChangelistBtn,AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.SetHorizontalPositions;
const OPNAME = 'TRainfallGraphToolBar.SetHorizontalPositions';
var
   lButtonCount,
   lGaps        : integer;
begin
  try
    inherited SetHorizontalPositions;
    lButtonCount := -1;
    lGaps := 0;
    SetButtonHorizontalPosition(FToggleGridBtn,     TRUE,  TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleGraphBtn,    TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleTreeBtn,     TRUE, FALSE, lButtonCount, lGaps);

    SetButtonHorizontalPosition(FCreatePATFilesBtn, TRUE,  TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FHighLightBtn,      TRUE, FALSE, lButtonCount, lGaps);

    SetButtonHorizontalPosition(FSelectRAWFlagsBtn, TRUE,  TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FFlagDataBtn,       TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FUnFlagDataBtn,     TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FFlagSetupBtn,      TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FFlagClickBtn,      TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FParameterChangesBtn,        TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FPatchChangelistBtn,        TRUE, FALSE, lButtonCount, lGaps);

    SetButtonHorizontalPosition(FWeatherEventsBtn,  TRUE,  TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FFirstRecordBtn,    TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FPrevRecordBtn,     TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FNextRecordBtn,     TRUE, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FLastRecordBtn,     TRUE, FALSE, lButtonCount, lGaps);

    Width := FLastRecordBtn.Left + FLastRecordBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphToolBar.OnClickParameterChange(ASender: TObject);
const OPNAME = 'TRainfallGraphToolBar.OnClickParameterChange';
begin
  try
     FAppModules.Model.ProcessEvent(CmeChangeParameter,nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphToolBar.SetPatchChangelistBtn(AEnabled: boolean);
const OPNAME = 'TRainfallGraphToolBar.SetPatchChangelistBtn';
begin
  try
    SetButtonEnabled(FPatchChangelistBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
