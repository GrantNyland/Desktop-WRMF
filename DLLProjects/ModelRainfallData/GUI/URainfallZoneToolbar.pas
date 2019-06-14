{******************************************************************************}
{*  UNIT      : Contains TRainfallZoneToolBar Class                           *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 13/01/2005                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit URainfallZoneToolbar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TRainfallZoneToolBar = class(TChildToolBar)
  protected
    FAddToZoneBtn       : TAbstractSpeedButton;
    FRemoveFromZoneBtn  : TAbstractSpeedButton;
    FCreateCatchmentZoneBtn  : TAbstractSpeedButton;
    FDeleteCatchmentZoneBtn  : TAbstractSpeedButton;
    FToggleTreeBtn      : TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetCreateCatchmentZone(AEnabled : boolean);
    procedure SetDeleteCatchmentZone(AEnabled : boolean);
    procedure SetAddToZone (AEnabled : boolean);
    procedure SetRemoveFromZone (AEnabled : boolean);
    procedure SetToggleTree (AEnabled : boolean);
    property CreateCatchmentZoneBtn  : TAbstractSpeedButton read FCreateCatchmentZoneBtn;
    property DeleteCatchmentZoneBtn  : TAbstractSpeedButton read FDeleteCatchmentZoneBtn; 
    property AddToZoneBtn      : TAbstractSpeedButton read FAddToZoneBtn;
    property RemoveFromZoneBtn : TAbstractSpeedButton read FRemoveFromZoneBtn;
    property ToggleTreeBtn     : TAbstractSpeedButton read FToggleTreeBtn;

  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TRainfallZoneToolBar.CreateMemberObjects;
const OPNAME = 'TRainfallZoneToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FAddToZoneBtn      := CreateButton('RDAddGaugeToZone');
    FRemoveFromZoneBtn := CreateButton('RDRemoveGaugeFromZone');
    FToggleTreeBtn     := CreateButton('RDZoneToggleTree');
    FCreateCatchmentZoneBtn  := CreateButton('RDCreateCatchmentZone');
    FDeleteCatchmentZoneBtn  := CreateButton('RDDeleteCatchmentZone');


    FAddToZoneBtn.Hint      := FAppModules.Language.GetString('ButtonHint.RDAddGaugeToZone');
    FRemoveFromZoneBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDRemoveGaugeFromZone');
    FToggleTreeBtn.Hint     := FAppModules.Language.GetString('ButtonHint.RDZoneToggleTree');
    FCreateCatchmentZoneBtn.Hint := FAppModules.Language.GetString('ButtonHint.RDCreateCatchmentZone');
    FDeleteCatchmentZoneBtn.Hint  := FAppModules.Language.GetString('ButtonHint.RDDeleteCatchmentZone');
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneToolBar.AssignHelpContext;
const OPNAME = 'TRainfallZoneToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(FAddToZoneBtn, -1);
    SetControlHelpContext(FRemoveFromZoneBtn, -1);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallZoneToolBar.SetHorizontalPositions;
const OPNAME = 'TRainfallZoneToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FCreateCatchmentZoneBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteCatchmentZoneBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FAddToZoneBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FRemoveFromZoneBtn, True, FALSE, lButtonCount, lGaps);
    SetButtonHorizontalPosition(FToggleTreeBtn, True, False, LButtonCount, LGaps);

    Width := FToggleTreeBtn.Left + FToggleTreeBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallZoneToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FCreateCatchmentZoneBtn.LanguageHasChanged;
    FDeleteCatchmentZoneBtn.LanguageHasChanged;
    FAddToZoneBtn.LanguageHasChanged;
    FRemoveFromZoneBtn.LanguageHasChanged;
    FToggleTreeBtn.LanguageHasChanged;     
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneToolBar.SetAddToZone(AEnabled: boolean);
const OPNAME = 'TRainfallZoneToolBar.SetAddToZone';
begin
  try
    SetButtonEnabled(FAddToZoneBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneToolBar.SetCreateCatchmentZone(AEnabled : boolean);
const OPNAME = 'TRainfallZoneToolBar.SetCreateCatchmentZone';
begin
  try
    SetButtonEnabled(FCreateCatchmentZoneBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneToolBar.SetDeleteCatchmentZone(AEnabled : boolean);
const OPNAME = 'TRainfallZoneToolBar.SetDeleteCatchmentZone';
begin
  try
    SetButtonEnabled(FDeleteCatchmentZoneBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



procedure TRainfallZoneToolBar.SetRemoveFromZone (AEnabled : boolean);
const OPNAME = 'TRainfallZoneToolBar.SetRemoveFromZone';
begin
  try
    SetButtonEnabled(FRemoveFromZoneBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneToolBar.SetToggleTree (AEnabled : boolean );
const OPNAME = 'TRainfallZoneToolBar.SetToggleTree';
begin
  try
    SetButtonEnabled(FToggleTreeBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
