{******************************************************************************}
{*  UNIT      : Contains the class TTariffCalculationDataDialog.   *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/06/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UTariffCalculationDataDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TTariffCalculationDataDialog = class(TAbstractScrollablePanel)
  private
    //FChannelNumberLabel     : TLabel;
    //FChannelNumberEdit      : TFieldEdit;
    FTariffLabel            : TLabel;
    FTariffEdit             : TFieldEdit;
    FEscalationFactorsLabel : TLabel;
    FEscalationFactorsEdit  : TFieldEdit;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    //property ChannelNumberEdit      : TFieldEdit  read FChannelNumberEdit;
    property TariffEdit             : TFieldEdit  read FTariffEdit;
    property EscalationFactorsEdit  : TFieldEdit  read FEscalationFactorsEdit;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TTariffCalculationDataDialog                                              *}
{******************************************************************************}

procedure TTariffCalculationDataDialog.CreateMemberObjects;
const OPNAME = 'TTariffCalculationDataDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                             Left  Top Width Height
    //FChannelNumberLabel          := CreateFieldLabel     (lOwner, lParent,  10, 10, 140,  21);
    //FChannelNumberEdit           := CreateFieldEdit      (FAppModules, lOwner, lParent, 160, 10, 40,  21, 0, TRUE);

    FTariffLabel                 := CreateFieldLabel     (lOwner, lParent,  10, 40, 140,  21);
    FTariffEdit                  := CreateFieldEdit      (FAppModules, lOwner, lParent, 160, 40, 40,  21, 0, TRUE);

    FEscalationFactorsLabel      := CreateFieldLabel     (lOwner, lParent,  10, 70, 140,  21);
    FEscalationFactorsEdit       := CreateFieldEdit      (FAppModules, lOwner, lParent, 160, 70, 120,  21, 0, TRUE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataDialog.Resize;
const OPNAME = 'TTariffCalculationDataDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataDialog.Initialise: boolean;
const OPNAME = 'TTariffCalculationDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTariffCalculationDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TTariffCalculationDataDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    //FChannelNumberLabel.Caption     := FAppModules.Language.GetString('TTariffCalculationDataDialog.ChannelNumber') + ' :';
    FTariffLabel.Caption            := FAppModules.Language.GetString('TTariffCalculationDataDialog.Tariff') + ' :';
    FEscalationFactorsLabel.Caption := FAppModules.Language.GetString('TTariffCalculationDataDialog.EscalationFactors') + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataDialog.RestoreColourState;
const OPNAME = 'TTariffCalculationDataDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTariffCalculationDataDialog.AssignHelpContext;
const OPNAME = 'TTariffCalculationDataDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self                    , HC_Introduction);
    //SetControlHelpContext(FChannelNumberEdit      , HC_Introduction);
    SetControlHelpContext(FTariffEdit             , HC_Introduction);
    SetControlHelpContext(FEscalationFactorsEdit  , HC_Introduction);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
