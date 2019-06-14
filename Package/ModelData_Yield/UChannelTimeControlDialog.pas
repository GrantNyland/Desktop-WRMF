{******************************************************************************}
{*  UNIT      : Contains the class TChannelTimeControlDialog.                 *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/03/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UChannelTimeControlDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TChannelTimeControlDialog = class(TAbstractScrollablePanel)
  private
  protected
    FDateActiveGBox         : TScrollBox;
    FEconomicDataGBox       : TScrollBox;
    FLblYears               : TLabel;
    FLblAnnualEscalation    : TLabel;
    FLblStartDate           : TLabel;
    FCbxStartYear           : TFieldComboBox;
    FCbxStartMonth          : TFieldComboBox;
    FLblEndDate             : TLabel;
    FCbxEndYear             : TFieldComboBox;
    FCbxEndMonth            : TFieldComboBox;
    FLblEconomicLife        : TLabel;
    FEdtEconomicLife        : TFieldEdit;
    FLblEconomicLifeUnits   : TLabel;
    FLblCapitalCost         : TLabel;
    FEdtCapitalCost         : TFieldEdit;
    FLblCapitalCostUnits    : TLabel;
    FLblFixedOMCost         : TLabel;
    FEdtFixedOMCost         : TFieldEdit;
    FLblFixedOMCostUnits    : TLabel;
    FLblVariableOMCost      : TLabel;
    FEdtVariableOMCost      : TFieldEdit;
    FLblVariableOMCostUnits : TLabel;

    FLblNrYearsConstruct    : TLabel;
    FEdtNrYearsConstruct    : TFieldEdit;
    FLblYear                : TLabel;
    FLblCostSchedule        : TLabel;
    FGrdCostSchedule        : TFieldStringGrid;

    FLblYearsInAnalysis     : TLabel;
    FEdtYearsInAnalysis     : TFieldEdit;
    FGrdEscalationCost      : TFieldStringGrid;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;

    property CbxStartYear         : TFieldComboBox   read FCbxStartYear;
    property CbxStartMonth        : TFieldComboBox   read FCbxStartMonth;
    property CbxEndYear           : TFieldComboBox   read FCbxEndYear;
    property CbxEndMonth          : TFieldComboBox   read FCbxEndMonth;
    property EdtEconomicLife      : TFieldEdit       read FEdtEconomicLife;
    property EdtCapitalCost       : TFieldEdit       read FEdtCapitalCost;
    property EdtFixedOMCost       : TFieldEdit       read FEdtFixedOMCost;
    property EdtVariableOMCost    : TFieldEdit       read FEdtVariableOMCost;
    property EdtNrYearsConstruct  : TFieldEdit       read FEdtNrYearsConstruct;
    property GrdCostSchedule      : TFieldStringGrid read FGrdCostSchedule;
    property EdtYearsInAnalysis   : TFieldEdit       read FEdtYearsInAnalysis;
    property GrdEscalationCost    : TFieldStringGrid read FGrdEscalationCost;

  end;

implementation

uses
  SysUtils,
  VCL.Grids,
  UConstants,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{******************************************************************************}
{* TChannelTimeControlDialog                                                  *}
{******************************************************************************}

procedure TChannelTimeControlDialog.CreateMemberObjects;
const OPNAME = 'TChannelTimeControlDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FDateActiveGBox                 := TScrollBox.Create(lOwner);
    FEconomicDataGBox               := TScrollBox.Create(lOwner);
    FDateActiveGBox.Parent          := lParent;
    FEconomicDataGBox.Parent        := lParent;
    FDateActiveGBox.Align           := alTop;
    FEconomicDataGBox.Align         := alTop;
    FDateActiveGBox.Height          := 100;
    FEconomicDataGBox.Height        := 350;

    FLblStartDate := TLabel.Create(lOwner);
    with FLblStartDate do
    begin
      Parent    := FDateActiveGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 10;
      Width     := 150;
      Height    := 21;
    end;
    FCbxStartYear  := CreateFieldComboBox(FAppModules, lOwner, FDateActiveGBox, 170,  10,  80, 21, 3, TRUE, csDropDownList);
    FCbxStartMonth := CreateFieldComboBox(FAppModules, lOwner, FDateActiveGBox, 260,  10,  80, 21, 3, TRUE, csDropDownList);
    FLblEndDate := TLabel.Create(lOwner);
    with FLblEndDate do
    begin
      Parent    := FDateActiveGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 35;
      Width     := 150;
      Height    := 21;
    end;
    FCbxEndYear  := CreateFieldComboBox(FAppModules, lOwner, FDateActiveGBox, 170,  35,  80, 21, 3, TRUE, csDropDownList);
    FCbxEndMonth := CreateFieldComboBox(FAppModules, lOwner, FDateActiveGBox, 260,  35,  80, 21, 3, TRUE, csDropDownList);

    FCbxStartYear.DropDownCount   := 25;
    FCbxEndYear.DropDownCount     := 25;

    FLblEconomicLife := TLabel.Create(lOwner);
    with FLblEconomicLife do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 60;
      Width     := 150;
      Height    := 21;
    end;
    FEdtEconomicLife := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtEconomicLife do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 170;
      Top       := 60;
      Width     := 70;
      Height    := 21;
    end;
    FLblEconomicLifeUnits := TLabel.Create(lOwner);
    with FLblEconomicLifeUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 245;
      Top       := 60;
      Width     := 40;
      Height    := 21;
    end;

    FLblCapitalCost := TLabel.Create(lOwner);
    with FLblCapitalCost do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 85;
      Width     := 150;
      Height    := 21;
    end;
    FEdtCapitalCost := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtCapitalCost do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 170;
      Top       := 85;
      Width     := 70;
      Height    := 21;
    end;
    FLblCapitalCostUnits := TLabel.Create(lOwner);
    with FLblCapitalCostUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 245;
      Top       := 85;
      Width     := 50;
      Height    := 21;
    end;

    FLblFixedOMCost := TLabel.Create(lOwner);
    with FLblFixedOMCost do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 110;
      Width     := 150;
      Height    := 21;
    end;
    FEdtFixedOMCost := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtFixedOMCost do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 170;
      Top       := 110;
      Width     := 70;
      Height    := 21;
    end;
    FLblFixedOMCostUnits := TLabel.Create(lOwner);
    with FLblFixedOMCostUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 245;
      Top       := 110;
      Width     := 85;
      Height    := 21;
    end;

    FLblVariableOMCost := TLabel.Create(lOwner);
    with FLblVariableOMCost do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 135;
      Width     := 150;
      Height    := 21;
    end;
    FEdtVariableOMCost := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtVariableOMCost do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 170;
      Top       := 135;
      Width     := 70;
      Height    := 21;
    end;
    FLblVariableOMCostUnits := TLabel.Create(lOwner);
    with FLblVariableOMCostUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 245;
      Top       := 135;
      Width     := 85;
      Height    := 21;
    end;

    FLblNrYearsConstruct := TLabel.Create(lOwner);
    with FLblNrYearsConstruct do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 160;
      Width     := 150;
      Height    := 21;
    end;
    FEdtNrYearsConstruct := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtNrYearsConstruct do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 170;
      Top       := 160;
      Width     := 70;
      Height    := 21;
    end;

    FLblYear := TLabel.Create(lOwner);
    with FLblYear do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 185;
      Width     := 150;
      Height    := 21;
    end;
    FLblCostSchedule := TLabel.Create(lOwner);
    with FLblCostSchedule do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 205;
      Width     := 150;
      Height    := 21;
    end;

    FGrdCostSchedule := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdCostSchedule do
    begin
      Parent           := FEconomicDataGBox;
      Left             := 170;
      Top              := 185;
      Width            := 186;
      Height           := 46;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    FLblYearsInAnalysis := TLabel.Create(lOwner);
    with FLblYearsInAnalysis do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 235;
      Width     := 150;
      Height    := 21;
    end;
    FEdtYearsInAnalysis := TFieldEdit.Create(lOwner, FAppModules);
    with FEdtYearsInAnalysis do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 170;
      Top       := 235;
      Width     := 70;
      Height    := 21;
    end;

    FLblYears := TLabel.Create(lOwner);
    with FLblYears do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 260;
      Width     := 150;
      Height    := 21;
    end;

    FLblAnnualEscalation := TLabel.Create(lOwner);
    with FLblAnnualEscalation do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 280;
      Width     := 150;
      Height    := 21;
    end;

    FGrdEscalationCost := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdEscalationCost do
    begin
      Parent           := FEconomicDataGBox;
      Left             := 170;
      Top              := 260;
      Width            := 186;
      Height           := 46;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TChannelTimeControlDialog.Initialise: boolean;
const OPNAME = 'TChannelTimeControlDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FEconomicDataGBox.Visible        := False;
    {FLblEconomicLife.Visible        := False;
    FEdtEconomicLife.Visible        := False;
    FLblEconomicLifeUnits.Visible   := False;
    FLblCapitalCost.Visible         := False;
    FEdtCapitalCost.Visible         := False;
    FLblCapitalCostUnits.Visible    := False;
    FLblFixedOMCost.Visible         := False;
    FEdtFixedOMCost.Visible         := False;
    FLblFixedOMCostUnits.Visible    := False;
    FLblVariableOMCost.Visible      := False;
    FEdtVariableOMCost.Visible      := False;
    FLblVariableOMCostUnits.Visible := False;

    FLblNrYearsConstruct.Visible    := False;
    FEdtNrYearsConstruct.Visible    := False;
    FLblYear.Visible                := False;
    FLblCostSchedule.Visible        := False;
    FGrdCostSchedule.Visible        := False;

    FLblYearsInAnalysis.Visible     := False;
    FEdtYearsInAnalysis.Visible     := False;
    FGrdEscalationCost.Visible      := False;

    FLblYears.Visible               := False;
    FLblAnnualEscalation.Visible    := False; }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlDialog.Resize;
const OPNAME = 'TChannelTimeControlDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChannelTimeControlDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblStartDate.Caption           := FAppModules.Language.GetString('Weather.StartDate');
    FLblEndDate.Caption             := FAppModules.Language.GetString('Weather.EndDate');
    FLblEconomicLife.Caption        := FAppModules.Language.GetString('LabelCaption.EconomicLife');
    FLblEconomicLifeUnits.Caption   := FAppModules.Language.GetString('LabelCaption.EconomicLifeUnits');
    FLblCapitalCost.Caption         := FAppModules.Language.GetString('LabelCaption.CapitalCost');
    FLblCapitalCostUnits.Caption    := FAppModules.Language.GetString('LabelCaption.CapitalCostUnits');
    FLblFixedOMCost.Caption         := FAppModules.Language.GetString('LabelCaption.FixedOandMCost');
    FLblFixedOMCostUnits.Caption    := FAppModules.Language.GetString('LabelCaption.OandMCostUnits');
    FLblVariableOMCost.Caption      := FAppModules.Language.GetString('LabelCaption.VariableOandMCost');
    FLblVariableOMCostUnits.Caption := FAppModules.Language.GetString('LabelCaption.OandMCostUnits');
    FLblNrYearsConstruct.Caption    := FAppModules.Language.GetString('LabelCaption.NrYearsConstruct');
    FLblYear.Caption                := FAppModules.Language.GetString('LabelCaption.Year');
    FLblCostSchedule.Caption        := FAppModules.Language.GetString('LabelCaption.CostSchedule');
    FLblYearsInAnalysis.Caption     := FAppModules.Language.GetString('LabelCaption.YearsInAnalysis');
    FLblYears.Caption               := FAppModules.Language.GetString('LabelCaption.Years');
    FLblAnnualEscalation.Caption    := FAppModules.Language.GetString('LabelCaption.AnnualEscalation');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlDialog.RestoreColourState;
const OPNAME = 'TChannelTimeControlDialog.RestoreColourState';
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

procedure TChannelTimeControlDialog.AssignHelpContext;
const OPNAME = 'TChannelTimeControlDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                   HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FSelectPenaltyStruct,   HC_ReservoirPenaltyStructures);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

