{******************************************************************************}
{*  UNIT      : Contains the class TDisbenefitFunctionDefinitionDataDialog.   *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/06/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UDisbenefitFunctionDefinitionDataDialog;

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

  TDisbenefitFunctionDefinitionDataDialog = class(TAbstractScrollablePanel)
  private
    FDateActiveGBox                 : TScrollBox;
    FEconomicDataGBox               : TScrollBox;
    FNrOfEconomicVariablesLabel     : TLabel;
    FNrOfEconomicVariablesEdit      : TFieldEdit;
    FEquationFunctionXLabel         : TLabel;
    FEquationFunctionXEdit          : TFieldEdit;
    FEquationFunctionYLabel         : TLabel;
    FEquationFunctionYEdit          : TFieldEdit;
    FEquationFunctionNonSupplyLabel : TLabel;
    FEquationFunctionNonSupplyEdit  : TFieldEdit;
    FEquationFunctionCostYLabel     : TLabel;
    FEquationFunctionCostYEdit      : TFieldEdit;
    FEscalationRateLabel            : TLabel;
    FGridHeadingLabel               : TLabel;
    FGrdEscalationRate              : TFieldStringGrid;

    FCbxYearActive                 : TFieldComboBox;
    FCbxMonthActive                : TFieldComboBox;
    FCbxYearObsolete               : TFieldComboBox;
    FCbxMonthObsolete              : TFieldComboBox;

    FYearActiveLabel                 : TLabel;
    FMonthActiveLabel                : TLabel;
    FYearObsoleteLabel               : TLabel;
    FMonthObsoleteLabel              : TLabel;
    FWQConstraintLabel               : TLabel;
    FTDSConcentrationLabel           : TLabel;

    FWQConstraintEdit                : TFieldEdit;
    FTDSConcentrationGrid            : TFieldStringGrid;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property CbxYearActive    : TFieldComboBox        read FCbxYearActive;
    property CbxMonthActive   : TFieldComboBox        read FCbxMonthActive;
    property CbxYearObsolete  : TFieldComboBox        read FCbxYearObsolete;
    property CbxMonthObsolete : TFieldComboBox        read FCbxMonthObsolete;


    property NrOfEconomicVariablesEdit     : TFieldEdit        read FNrOfEconomicVariablesEdit;
    property EquationFunctionXEdit         : TFieldEdit        read FEquationFunctionXEdit;
    property EquationFunctionYEdit         : TFieldEdit        read FEquationFunctionYEdit;
    property EquationFunctionNonSupplyEdit : TFieldEdit        read FEquationFunctionNonSupplyEdit;
    property EquationFunctionCostYEdit     : TFieldEdit        read FEquationFunctionCostYEdit;
    property GrdEscalationRate             : TFieldStringGrid  read FGrdEscalationRate;

    property WQConstraintEdit              : TFieldEdit        read FWQConstraintEdit;
    property TDSConcentrationGrid          : TFieldStringGrid  read FTDSConcentrationGrid;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Grids,
  UErrorHandlingOperations,
 UControlCreationUtilities;

{******************************************************************************}
{* TDisbenefitFunctionDefinitionDataDialog                                              *}
{******************************************************************************}

procedure TDisbenefitFunctionDefinitionDataDialog.CreateMemberObjects;
const OPNAME = 'TDisbenefitFunctionDefinitionDataDialog.CreateMemberObjects';
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

    FYearActiveLabel                := CreateFieldLabel     (lOwner, FDateActiveGBox,  100, 20, 140,  21);
    FMonthActiveLabel               := CreateFieldLabel     (lOwner, FDateActiveGBox,  100, 20, 140,  21);
    FYearObsoleteLabel              := CreateFieldLabel     (lOwner, FDateActiveGBox,  100, 50, 140,  21);
    FMonthObsoleteLabel             := CreateFieldLabel     (lOwner, FDateActiveGBox,  100, 50, 140,  21);

    FCbxYearActive                 := CreateFieldComboBox  (FAppModules, lOwner, FDateActiveGBox, 170, 20, 80,  21, 0,TRUE, csDropDownList);
    FCbxMonthActive                := CreateFieldComboBox  (FAppModules, lOwner, FDateActiveGBox, 260, 20, 80,  21, 0, TRUE, csDropDownList);
    FCbxYearObsolete               := CreateFieldComboBox  (FAppModules, lOwner, FDateActiveGBox, 170, 50, 80,  21, 0, TRUE, csDropDownList);
    FCbxMonthObsolete              := CreateFieldComboBox  (FAppModules, lOwner, FDateActiveGBox, 260, 50, 80,  21, 0, TRUE, csDropDownList);

    FCbxYearActive.DropDownCount   := 25;
    FCbxYearObsolete.DropDownCount := 25;

    //                                                                             Left  Top Width Height
    FNrOfEconomicVariablesLabel     := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 20, 140,  21);
    FEquationFunctionXLabel         := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 50, 140,  21);
    FEquationFunctionYLabel         := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 80, 140,  21);
    FEquationFunctionNonSupplyLabel := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 110, 140,  21);
    FEquationFunctionCostYLabel     := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 140, 140,  21);
    FWQConstraintLabel              := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 180, 140,  21);

    FNrOfEconomicVariablesEdit      := CreateFieldEdit      (FAppModules, lOwner, FEconomicDataGBox, 170, 20, 40,  21, 0, TRUE);
    FEquationFunctionXEdit          := CreateFieldEdit      (FAppModules, lOwner, FEconomicDataGBox, 170, 50, 40,  21, 0, TRUE);
    FEquationFunctionYEdit          := CreateFieldEdit      (FAppModules, lOwner, FEconomicDataGBox, 170, 80, 40,  21, 0, TRUE);
    FEquationFunctionNonSupplyEdit  := CreateFieldEdit      (FAppModules, lOwner, FEconomicDataGBox, 170, 110, 40,  21, 0, TRUE);
    FEquationFunctionCostYEdit      := CreateFieldEdit      (FAppModules, lOwner, FEconomicDataGBox, 170, 140, 40,  21, 0, TRUE);
    FWQConstraintEdit               := CreateFieldEdit      (FAppModules, lOwner, FEconomicDataGBox, 170, 180, 40,  21, 0, TRUE);

    FGridHeadingLabel               := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 220, 140,  21);
    FEscalationRateLabel            := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 240, 140,  21);
    FTDSConcentrationLabel          := CreateFieldLabel     (lOwner, FEconomicDataGBox,  10, 280, 140,  21);

    FGrdEscalationRate              := CreateFieldStringGrid(FAppModules, lOwner, FEconomicDataGBox,   170,  220, 55, 48, 0, TRUE);
    FTDSConcentrationGrid           := CreateFieldStringGrid(FAppModules, lOwner, FEconomicDataGBox,   170, 280, 213,  22, 5, TRUE);

    with FEscalationRateLabel do
    begin
      Alignment := taLeftJustify;
    end;
    with FGridHeadingLabel do
    begin
      Alignment := taLeftJustify;
    end;
    with FGrdEscalationRate do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    with FTDSConcentrationGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 4;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataDialog.Resize;
const OPNAME = 'TDisbenefitFunctionDefinitionDataDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataDialog.Initialise: boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FEconomicDataGBox.Visible := False;
    FMonthActiveLabel.Visible := False;
    FMonthObsoleteLabel.Visible := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNrOfEconomicVariablesLabel.Caption     := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.NrOfEconomicVariables') + ' :';
    FEquationFunctionXLabel.Caption         := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.EquationFunctionX') + ' :';
    FEquationFunctionYLabel.Caption         := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.EquationFunctionY') + ' :';
    FEquationFunctionNonSupplyLabel.Caption := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.EquationFunctionNonSupply') + ' :';
    FEquationFunctionCostYLabel.Caption     := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.EquationCostY') + ' :';
    FEscalationRateLabel.Caption            := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.EscalationRate') + ' :';
    FGridHeadingLabel.Caption               := FAppModules.Language.GetString('TField.Year') + ' :';

    FYearActiveLabel.Caption     := FAppModules.Language.GetString('Weather.StartDate');
    FMonthActiveLabel.Caption    := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.MonthActiveLabel');
    FYearObsoleteLabel.Caption   := FAppModules.Language.GetString('Weather.EndDate');
    FMonthObsoleteLabel.Caption  := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.MonthObsoleteLabel');

    FTDSConcentrationLabel.Caption := 'TDS Concentration Factors:';
    FWQConstraintLabel.Caption     := 'Water Quality Constraint:';
    //FDateActiveGBox.Caption        := 'Date Active';
    //FEconomicDataGBox.Caption      := 'Economic Data';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataDialog.RestoreColourState;
const OPNAME = 'TDisbenefitFunctionDefinitionDataDialog.RestoreColourState';
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

procedure TDisbenefitFunctionDefinitionDataDialog.AssignHelpContext;
const OPNAME = 'TDisbenefitFunctionDefinitionDataDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self                           , HC_TimeSeriesDemands);
    SetControlHelpContext(FNrOfEconomicVariablesEdit     , HC_TimeSeriesDemands);
    SetControlHelpContext(FEquationFunctionXEdit         , HC_TimeSeriesDemands);
    SetControlHelpContext(FEquationFunctionYEdit         , HC_TimeSeriesDemands);
    SetControlHelpContext(FEquationFunctionNonSupplyEdit , HC_TimeSeriesDemands);
    SetControlHelpContext(FEquationFunctionCostYEdit     , HC_TimeSeriesDemands);
    SetControlHelpContext(FGrdEscalationRate             , HC_TimeSeriesDemands);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
