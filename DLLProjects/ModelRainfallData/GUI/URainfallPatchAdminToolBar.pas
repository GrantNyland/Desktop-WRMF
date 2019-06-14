//
//
//  UNIT      : Contains TRainfallPatchAdminToolBar Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 03/12/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallPatchAdminToolBar;

interface

uses
  VCL.stdctrls,
  VCL.controls,
  UAbstractComponent,
  UChildToolBar;

type

  TRainfallPatchAdminToolBar = class(TChildToolBar)
  protected
    FCreatePatchBtn          : TAbstractSpeedButton;
    FDeletePatchBtn          : TAbstractSpeedButton;
    FRenamePatchBtn          : TAbstractSpeedButton;
    FAddGaugeToPatchBtn      : TAbstractSpeedButton;
    FRemoveGaugeFromPatchBtn : TAbstractSpeedButton;
    FToggleGridBtn           : TAbstractSpeedButton;
    FToggleGraphBtn          : TAbstractSpeedButton;
    FToggleTreeBtn           : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
  public
    function Initialise : Boolean; override;
    function LanguageHasChanged : boolean; override;
    procedure SetCreatePatch (AEnabled : boolean);
    procedure SetDeletePatch (AEnabled : boolean);
    procedure SetRenamePatch (AEnabled : boolean);
    procedure SetAddGaugeToPatch (AEnabled : boolean);
    procedure SetRemoveGaugeFromPatch (AEnabled : boolean);
    procedure SetToggleGrid (AEnabled : boolean);
    procedure SetToggleGraph (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    property CreatePatchBtn          : TAbstractSpeedButton read FCreatePatchBtn;
    property DeletePatchBtn          : TAbstractSpeedButton read FDeletePatchBtn;
    property RenamePatchBtn          : TAbstractSpeedButton read FRenamePatchBtn;
    property AddGaugeToPatchBtn      : TAbstractSpeedButton read FAddGaugeToPatchBtn;
    property RemoveGaugeFromPatchBtn : TAbstractSpeedButton read FRemoveGaugeFromPatchBtn;
    property ToggleGridBtn           : TAbstractSpeedButton read FToggleGridBtn;
    property ToggleGraphBtn          : TAbstractSpeedButton read FToggleGraphBtn;
    property ToggleTreeBtn           : TAbstractSpeedButton read FToggleTreeBtn;    

end;

implementation

uses
  windows,
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TRainfallPatchAdminToolBar }

procedure TRainfallPatchAdminToolBar.CreateMemberObjects;
const OPNAME = 'TRainfallPatchAdminToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreatePatchBtn          := CreateButton('RDCreatePatch');
    FDeletePatchBtn          := CreateButton('RDDeletePatch');
    FRenamePatchBtn          := CreateButton('RDRenamePatch');
    FAddGaugeToPatchBtn      := CreateButton('RDAddGaugeToPatch');
    FRemoveGaugeFromPatchBtn := CreateButton('RDRemoveGaugeFromPatch');
    FToggleGridBtn           := CreateButton('RDAdminToggleGrid');
    FToggleGraphBtn          := CreateButton('RDAdminToggleGraph');
    FToggleTreeBtn           := CreateButton('RDAdminToggleTree' );

    FCreatePatchBtn.Hint          := FAppModules.Language.GetString('ButtonHint.RDCreatePatch');
    FDeletePatchBtn.Hint          := FAppModules.Language.GetString('ButtonHint.RDDeletePatch');
    FRenamePatchBtn.Hint          := FAppModules.Language.GetString('ButtonHint.RDRenamePatch');
    FAddGaugeToPatchBtn.Hint      := FAppModules.Language.GetString('ButtonHint.RDAddGaugeToPatch');
    FRemoveGaugeFromPatchBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDRemoveGaugeFromPatch');
    FToggleGridBtn.Hint           := FAppModules.Language.GetString('ButtonHint.RDAdminToggleGrid');
    FToggleGraphBtn.Hint          := FAppModules.Language.GetString('ButtonHint.RDAdminToggleGraph');
    FToggleTreeBtn.Hint           := FAppModules.Language.GetString('ButtonHint.RDAdminToggleTree');

    SetHorizontalPositions;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminToolBar.Initialise : Boolean;
const OPNAME = 'TRainfallPatchAdminToolBar.Initialise';
begin
  Result := True;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminToolBar.LanguageHasChanged : boolean;
const OPNAME = 'TRainfallPatchAdminToolBar.LanguageHasChanged';
begin
  Result := True;
  inherited LanguageHasChanged;
  try
    FCreatePatchBtn.LanguageHasChanged;
    FDeletePatchBtn.LanguageHasChanged;
    FRenamePatchBtn.LanguageHasChanged;
    FAddGaugeToPatchBtn.LanguageHasChanged;
    FRemoveGaugeFromPatchBtn.LanguageHasChanged;
    FToggleGridBtn.LanguageHasChanged;
    FToggleGraphBtn.LanguageHasChanged;
    FToggleTreeBtn.LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetCreatePatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetCreatePatch';
begin
  try
    SetButtonEnabled(FCreatePatchBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetDeletePatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetDeletePatch';
begin
  try
    SetButtonEnabled(FDeletePatchBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetRenamePatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetRenamePatch';
begin
  try
    SetButtonEnabled(FRenamePatchBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetAddGaugeToPatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetAddGaugeToPatch';
begin
  try
    SetButtonEnabled(FAddGaugeToPatchBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetRemoveGaugeFromPatch (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetRemoveGaugeFromPatch';
begin
  try
    SetButtonEnabled(FRemoveGaugeFromPatchBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetToggleGrid (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetToggleGrid';
begin
  try
    SetButtonEnabled(FToggleGridBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetToggleGraph (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetToggleGraph';
begin
  try
    SetButtonEnabled(FToggleGraphBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetToggleTree (AEnabled : boolean);
const OPNAME = 'TRainfallPatchAdminToolBar.SetToggleTree';
begin
  try
    SetButtonEnabled(FToggleTreeBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminToolBar.SetHorizontalPositions;
const OPNAME = 'TRainfallPatchAdminToolBar.SetHorizontalPositions';
var
   lButtonCount,
   lGaps        : integer;
begin
  inherited SetHorizontalPositions;
  try
    lButtonCount := -1;
    lGaps := 0;
    SetButtonHorizontalPosition(FToggleGridBtn,           True,  TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleGraphBtn,          True, False, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleTreeBtn,           True, False, LButtonCount, LGaps);

    SetButtonHorizontalPosition(FCreatePatchBtn,          True,  TRUE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FDeletePatchBtn,          True, False, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FRenamePatchBtn,          True, False, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FAddGaugeToPatchBtn,      True, False, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FRemoveGaugeFromPatchBtn, True, False, lButtonCount, lGaps);

    Width := FRemoveGaugeFromPatchBtn.Left + FRemoveGaugeFromPatchBtn.Width;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
