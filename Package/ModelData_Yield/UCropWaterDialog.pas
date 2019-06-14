{******************************************************************************}
{*  UNIT      : Contains the class TCropWaterDialog                           *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UCropWaterDialog;

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

  TCropWaterDialog = class(TAbstractScrollablePanel)
  protected
    FNumberOfCropTypesLabel              : TLabel;
    FMonthlyWaterUsageLabel              : TLabel;
    FCropDescr0Label                     : TLabel;
    FCropDescr1Label                     : TLabel;
    FCropWaterUseTypeLabel               : TLabel;
    FRainAboveRainFactorSpecValueLabel   : TLabel;
    FRainAboveRainFactorSpecValueEdit    : TFieldEdit;
    FRainBelowRainFactorLabel            : TLabel;
    FRainBelowRainFactorEdit             : TFieldEdit;
    FNumberOfCropTypesEdit               : TFieldEdit;
    FMonthlyWaterUsageGrid               : TFieldStringGrid;
    FCropWaterUseTypeRadioGroup          : TFieldRadioGroup;

    procedure CreateMemberObjects; override;

    procedure AssignHelpContext; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property NumberOfCropTypesEdit             : TFieldEdit        read FNumberOfCropTypesEdit;
    property RainAboveRainFactorSpecValueEdit  : TFieldEdit        read FRainAboveRainFactorSpecValueEdit;
    property RainBelowRainFactorEdit           : TFieldEdit        read FRainBelowRainFactorEdit;
    property MonthlyWaterUsageGrid             : TFieldStringGrid  read FMonthlyWaterUsageGrid;
    property CropWaterUseTypeRadioGroup        : TFieldRadioGroup  read FCropWaterUseTypeRadioGroup;
    property MonthlyWaterUsageLabel            : TLabel            read FMonthlyWaterUsageLabel;
    property CropWaterUseTypeLabel            : TLabel            read FCropWaterUseTypeLabel;
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
{* TCropWaterDialog                                                           *}
{******************************************************************************}

procedure TCropWaterDialog.CreateMemberObjects;
const OPNAME = 'TCropWaterDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
  lWidth,
  lHeight,
  lGridTop    : Integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
                                                                            //Left  Top Width Height
    FCropWaterUseTypeLabel             := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FCropWaterUseTypeRadioGroup        := CreateFieldRadioGroup(FAppModules, lOwner, lParent,  430,  5, 370,  30, 5, True);

    FNumberOfCropTypesLabel            := CreateFieldLabel(lOwner, lParent, 10, 40, 200, 21);
    FNumberOfCropTypesEdit             := CreateFieldEdit(FAppModules, lOwner, lParent, 430, 40, 100, 20, 8, TRUE);

    FRainAboveRainFactorSpecValueLabel := CreateFieldLabel(lOwner, lParent, 10, 65,  420, 21);
    FRainAboveRainFactorSpecValueEdit  := CreateFieldEdit(FAppModules, lOwner, lParent, 430,  65,  100,  20, 8, TRUE);

    FRainBelowRainFactorLabel          := CreateFieldLabel(lOwner, lParent, 10,  90,  420, 21);
    FRainBelowRainFactorEdit           := CreateFieldEdit(FAppModules, lOwner, lParent, 430,  90,  100,  20, 8, TRUE);

    FCropDescr0Label                   := CreateFieldLabel(lOwner, lParent, 10, 115, 600, 21);
    FCropDescr1Label                   := CreateFieldLabel(lOwner, lParent, 10, 140, 600, 21);
    FMonthlyWaterUsageLabel            := CreateFieldLabel(lOwner, lParent, 10, 165, 300, 21);

    lGridTop  := FMonthlyWaterUsageLabel.Top + FMonthlyWaterUsageLabel.Height + 5;
    lWidth    := lParent.Width - 20;
    lHeight   := lParent.Height - lGridTop - 10;
    FMonthlyWaterUsageGrid   := CreateFieldStringGrid(FAppModules, lOwner, lParent, 10, lGridTop, lWidth, lHeight, 3, TRUE);

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
    FMonthlyWaterUsageGrid.Anchors               := [akLeft, akRight, akTop, akBottom];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterDialog.DestroyMemberObjects;
const OPNAME = 'TCropWaterDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TCropWaterDialog.Resize;
const OPNAME = 'TCropWaterDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterDialog.Initialise: boolean;
const OPNAME = 'TCropWaterDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FCropWaterUseTypeRadioGroup.Columns          := 2;
    FCropWaterUseTypeRadioGroup.Items.Clear;
    FCropWaterUseTypeRadioGroup.Items.Add(' ');
    FCropWaterUseTypeRadioGroup.Items.Add(' ');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterDialog.LanguageHasChanged: boolean;
const OPNAME = 'TCropWaterDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNumberOfCropTypesLabel.Caption              := FAppModules.Language.GetString('TField.IrrigationBlockNumberOfCropTypes');
    FCropDescr0Label.Caption                     := FAppModules.Language.GetString('TField.IrrigationBlockCropDescr0');
    FCropDescr1Label.Caption                     := FAppModules.Language.GetString('TField.IrrigationBlockCropDescr1');
    FRainAboveRainFactorSpecValueLabel.Caption   := FAppModules.Language.GetString('TField.IrrigationBlockRainAboveRainFactorSpecValue');
    FRainBelowRainFactorLabel.Caption            := FAppModules.Language.GetString('TField.IrrigationBlockRainBelowRainFactor');

    if (FCropWaterUseTypeRadioGroup.ItemIndex = 0) then
      FMonthlyWaterUsageLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockWaterUsageFactor')
    else
      FMonthlyWaterUsageLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockRepresentativeCropEvapotranspiration');
    FCropWaterUseTypeLabel.Caption               := FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseType');

  //  FCropWaterUseTypeRadioGroup.Items[0]         := FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeMonthly');
   // FCropWaterUseTypeRadioGroup.Items[1]         := FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeEvapotrans');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterDialog.RestoreColourState;
const OPNAME = 'TCropWaterDialog.RestoreColourState';
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

procedure TCropWaterDialog.AssignHelpContext;
const OPNAME = 'TCropWaterDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                              HC_Irrigation);
    SetControlHelpContext(FRainAboveRainFactorSpecValueEdit, HC_Irrigation);
    SetControlHelpContext(FRainBelowRainFactorEdit,          HC_Irrigation);
    SetControlHelpContext(FNumberOfCropTypesEdit,            HC_Irrigation);
    SetControlHelpContext(FMonthlyWaterUsageGrid,            HC_Irrigation);
    SetControlHelpContext(FCropWaterUseTypeRadioGroup,       HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
