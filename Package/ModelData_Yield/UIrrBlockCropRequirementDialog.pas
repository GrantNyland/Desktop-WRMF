{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockCropRequirementDialog                           *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockCropRequirementDialog;

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

  TIrrBlockCropRequirementDialog = class(TAbstractScrollablePanel)
  protected
    FgboxProperties                      : TGroupBox;
    FCropWaterUseTypeLabel               : TLabel;
    FcomboxCropWaterUseType              : TFieldComboBox;
    FNumberOfCropTypesLabel              : TLabel;
    FNumberOfCropTypesEdit               : TFieldEdit;
    FRainAboveRainFactorSpecValueLabel   : TLabel;
    FRainAboveRainFactorSpecValueEdit    : TFieldEdit;
    FRainBelowRainFactorLabel            : TLabel;
    FRainBelowRainFactorEdit             : TFieldEdit;

    FgboxWaterUsage                      : TGroupBox;
    FpnlMessage                          : TPanel;
    FCropDescr0Label                     : TLabel;
    FCropDescr1Label                     : TLabel;
    //FMonthlyWaterUsageLabel              : TLabel;
    FMonthlyWaterUsageGrid               : TFieldStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property lblCropWaterUseType               : TLabel    read FCropWaterUseTypeLabel;
    property cmbBoxCropWaterUseType            : TFieldComboBox    read FcomboxCropWaterUseType;
    property NumberOfCropTypesEdit             : TFieldEdit        read FNumberOfCropTypesEdit;
    property RainAboveRainFactorSpecValueEdit  : TFieldEdit        read FRainAboveRainFactorSpecValueEdit;
    property RainBelowRainFactorEdit           : TFieldEdit        read FRainBelowRainFactorEdit;
    property MonthlyWaterUsageGrid             : TFieldStringGrid  read FMonthlyWaterUsageGrid;
    property gboxWaterUsage                    : TGroupBox         read FgboxWaterUsage;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TIrrBlockCropRequirementDialog                                                           *}
{******************************************************************************}

procedure TIrrBlockCropRequirementDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockCropRequirementDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;


    FgboxProperties                   := TGroupBox.Create(lOwner);
    FgboxProperties.Parent            := Self;
    FgboxProperties.Align             := alTop;
    FgboxProperties.Height            := 150;
    lParent                           := FgboxProperties;
                                                                            //Left  Top Width Height
    FCropWaterUseTypeLabel             := CreateFieldLabel(lOwner, lParent, 10,  15,  420, 21);
    FcomboxCropWaterUseType            := CreateFieldComboBox(FAppModules, lOwner, lParent, 430, 15, 150, 21,11, TRUE, csDropDownList);

    FNumberOfCropTypesLabel            := CreateFieldLabel(lOwner, lParent, 10, 40, 200, 21);
    FNumberOfCropTypesEdit             := CreateFieldEdit(FAppModules, lOwner, lParent, 430, 40, 100, 20, 8, TRUE);

    FRainAboveRainFactorSpecValueLabel := CreateFieldLabel(lOwner, lParent, 10, 65,  420, 21);
    FRainAboveRainFactorSpecValueEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 430,  65,  100,  20, 8, TRUE);

    FRainBelowRainFactorLabel          := CreateFieldLabel(lOwner, lParent, 10,  90,  420, 21);
    FRainBelowRainFactorEdit           := CreateFieldEdit(FAppModules, lOwner, lParent, 430,  90,  100,  20, 8, TRUE);

    FgboxWaterUsage                    := TGroupBox.Create(lOwner);
    FgboxWaterUsage.Parent             := Self;
    FgboxWaterUsage.Align              := alClient;
    lParent                            := FgboxWaterUsage;

    FpnlMessage                        := TPanel.Create(lOwner);
    FpnlMessage.Parent                 := lParent;
    FpnlMessage.Align                  := alTop;
    FpnlMessage.Height                 := 80;
    FpnlMessage.BevelInner             := bvNone;
    FpnlMessage.BevelOuter             := bvNone;


    FCropDescr0Label                   := CreateFieldLabel(lOwner, FpnlMessage, 10, 15, 600, 21);
    FCropDescr1Label                   := CreateFieldLabel(lOwner, FpnlMessage, 10, 40, 600, 21);

    FMonthlyWaterUsageGrid                       := CreateFieldStringGrid(FAppModules, lOwner, lParent, 10, 0, 100, 100, 3, TRUE);
    FMonthlyWaterUsageGrid.Align                 := alClient;
    FMonthlyWaterUsageGrid.Constraints.MinHeight := 100;
    FMonthlyWaterUsageGrid.Constraints.MinWidth  := 200;
    FMonthlyWaterUsageGrid.FixedCols             := 1;
    FMonthlyWaterUsageGrid.ColCount              := 15;
    FMonthlyWaterUsageGrid.DefaultColWidth       := 40;
    FMonthlyWaterUsageGrid.DefaultRowHeight      := 20;
    FMonthlyWaterUsageGrid.RowCount              := 2;
    FMonthlyWaterUsageGrid.ColWidths[0]          := 100;
    FMonthlyWaterUsageGrid.ColWidths[1]          := 200;
    FMonthlyWaterUsageGrid.Options               := [goColSizing,      goFixedVertLine,
                                                                    goFixedHorzLine,  goVertLine,
                                                                    goHorzLine,       goRangeSelect,
                                                                    goEditing];
    //FMonthlyWaterUsageGrid.Anchors               := [akLeft, akRight, akTop, akBottom];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementDialog.Resize;
const OPNAME = 'TIrrBlockCropRequirementDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockCropRequirementDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockCropRequirementDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FgboxProperties.Caption                      := 'Properties';
    FNumberOfCropTypesLabel.Caption              := FAppModules.Language.GetString('TField.IrrigationBlockNumberOfCropTypes');
    FCropDescr0Label.Caption                     := FAppModules.Language.GetString('TField.IrrigationBlockCropDescr0');
    FCropDescr1Label.Caption                     := FAppModules.Language.GetString('TField.IrrigationBlockCropDescr1');
    FRainAboveRainFactorSpecValueLabel.Caption   := FAppModules.Language.GetString('TField.IrrigationBlockRainAboveRainFactorSpecValue');
    FRainBelowRainFactorLabel.Caption            := FAppModules.Language.GetString('TField.IrrigationBlockRainBelowRainFactor');

    FgboxWaterUsage.Caption := FAppModules.Language.GetString('TField.IrrigationBlockWaterUsageFactor');
    if (FcomboxCropWaterUseType.ItemIndex = 1) then
      FgboxWaterUsage.Caption := FAppModules.Language.GetString('TField.IrrigationBlockRepresentativeCropEvapotranspiration');
    FCropWaterUseTypeLabel.Caption               := FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseType');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementDialog.RestoreColourState;
const OPNAME = 'TIrrBlockCropRequirementDialog.RestoreColourState';
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

procedure TIrrBlockCropRequirementDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockCropRequirementDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                              HC_Irrigation);
    SetControlHelpContext(FRainAboveRainFactorSpecValueEdit, HC_Irrigation);
    SetControlHelpContext(FRainBelowRainFactorEdit,          HC_Irrigation);
    SetControlHelpContext(FNumberOfCropTypesEdit,            HC_Irrigation);
    SetControlHelpContext(FMonthlyWaterUsageGrid,            HC_Irrigation);
    SetControlHelpContext(FcomboxCropWaterUseType,       HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
