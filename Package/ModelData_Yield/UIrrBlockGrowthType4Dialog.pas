{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockEfficienciesDialog              *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockGrowthType4Dialog;

interface

uses
  Classes,
  Vcl.Forms,
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

  TIrrBlockGrowthType4Dialog = class(TAbstractScrollablePanel)
  protected
    //________________________________________________________SupplyCapacity________________________________________
    FgboxIrrSupplyCapacity               : TGroupBox;
    FsboxIrrSupplyCapacity               : TScrollBox;

    FlblIrrSupplyCapacityPointsCount     : TLabel;
    FedtIrrSupplyCapacityPointsCount     : TFieldEdit;

    FlblMethodIrrSupplyCapacity          : TLabel;
    FcmbMethodIrrSupplyCapacity          : TFieldComboBox;

    FlblBreakpointYearsIrrSupplyCapacity : TLabel;
    FgrdBreakpointYearsIrrSupplyCapacity : TFieldStringGrid;

    FlblBreakpointIrrSupplyCapacity      : TLabel;
    FgrdBreakpointIrrSupplyCapacity      : TFieldStringGrid;

    //________________________________________________________IrrigationEfficiency________________________________________
    FgboxIrrIrrigationEfficiency               : TGroupBox;
    FsboxIrrIrrigationEfficiency               : TScrollBox;

    FlblIrrIrrigationEfficiencyPointsCount     : TLabel;
    FedtIrrIrrigationEfficiencyPointsCount     : TFieldEdit;

    FlblMethodIrrIrrigationEfficiency          : TLabel;
    FcmbMethodIrrIrrigationEfficiency          : TFieldComboBox;

    FlblBreakpointYearsIrrIrrigationEfficiency : TLabel;
    FgrdBreakpointYearsIrrIrrigationEfficiency : TFieldStringGrid;

    FlblBreakpointIrrIrrigationEfficiency      : TLabel;
    FgrdBreakpointIrrIrrigationEfficiency      : TFieldStringGrid;

    //________________________________________________________ReturnFlowFactors________________________________________
    FgboxIrrReturnFlowFactors               : TGroupBox;
    FsboxIrrReturnFlowFactors               : TScrollBox;

    FlblIrrReturnFlowFactorsPointsCount     : TLabel;
    FedtIrrReturnFlowFactorsPointsCount     : TFieldEdit;

    FlblMethodIrrReturnFlowFactors          : TLabel;
    FcmbMethodIrrReturnFlowFactors          : TFieldComboBox;

    FlblBreakpointYearsIrrReturnFlowFactors : TLabel;
    FgrdBreakpointYearsIrrReturnFlowFactors : TFieldStringGrid;

    FlblBreakpointIrrReturnFlowFactors      : TLabel;
    FgrdBreakpointIrrReturnFlowFactors      : TFieldStringGrid;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    //________________________________________________________SupplyCapacity________________________________________
    property edtIrrSupplyCapacityPointsCount      : TFieldEdit         read FedtIrrSupplyCapacityPointsCount;
    property cmbMethodIrrSupplyCapacity           : TFieldComboBox     read FcmbMethodIrrSupplyCapacity;
    property grdBreakpointYearsIrrSupplyCapacity  : TFieldStringGrid   read FgrdBreakpointYearsIrrSupplyCapacity;
    property grdBreakpointIrrSupplyCapacity       : TFieldStringGrid   read FgrdBreakpointIrrSupplyCapacity;

    //________________________________________________________SupplyCapacity________________________________________
    property edtIrrIrrigationEfficiencyPointsCount      : TFieldEdit         read FedtIrrIrrigationEfficiencyPointsCount;
    property cmbMethodIrrIrrigationEfficiency           : TFieldComboBox     read FcmbMethodIrrIrrigationEfficiency;
    property grdBreakpointYearsIrrIrrigationEfficiency  : TFieldStringGrid   read FgrdBreakpointYearsIrrIrrigationEfficiency;
    property grdBreakpointIrrIrrigationEfficiency       : TFieldStringGrid   read FgrdBreakpointIrrIrrigationEfficiency;

    //________________________________________________________ReturnFlowFactors________________________________________
    property edtIrrReturnFlowFactorsPointsCount      : TFieldEdit         read FedtIrrReturnFlowFactorsPointsCount;
    property cmbMethodIrrReturnFlowFactors           : TFieldComboBox     read FcmbMethodIrrReturnFlowFactors;
    property grdBreakpointYearsIrrReturnFlowFactors  : TFieldStringGrid   read FgrdBreakpointYearsIrrReturnFlowFactors;
    property grdBreakpointIrrReturnFlowFactors       : TFieldStringGrid   read FgrdBreakpointIrrReturnFlowFactors;



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
{* TIrrBlockGrowthType4Dialog                                                         *}
{******************************************************************************}

procedure TIrrBlockGrowthType4Dialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockGrowthType4Dialog.CreateMemberObjects';
var
  lOwner    : TComponent;
  lParent   : TWinControl;
 // lWidth,
 // lHeight,
 // lGridTop,
 // lTop      : Integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    //________________________________________________________SupplyCapacity________________________________________
    FgboxIrrSupplyCapacity                                           := TGroupBox.Create(lOwner);
    FgboxIrrSupplyCapacity.Parent                                    := Self;
    FgboxIrrSupplyCapacity.Align                                     := alTop;
    FgboxIrrSupplyCapacity.Height                                    := 150;

    FsboxIrrSupplyCapacity                                           := TScrollBox.Create(lOwner);
    FsboxIrrSupplyCapacity.Parent                                    := FgboxIrrSupplyCapacity;
    FsboxIrrSupplyCapacity.AutoScroll                                := True;
    FsboxIrrSupplyCapacity.Align                                     := alClient;
    lParent                                                          := FsboxIrrSupplyCapacity;

    FlblIrrSupplyCapacityPointsCount                                 := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtIrrSupplyCapacityPointsCount                                 := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrSupplyCapacity                                      := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FcmbMethodIrrSupplyCapacity                                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE, csDropDownList);

    FlblBreakpointYearsIrrSupplyCapacity                             := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsIrrSupplyCapacity                             := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 55,10, 25, 3, TRUE);
    FgrdBreakpointYearsIrrSupplyCapacity.Anchors                     := [akLeft, akRight, akTop];
    FgrdBreakpointYearsIrrSupplyCapacity.ScrollBars                  := ssNone;
    FgrdBreakpointYearsIrrSupplyCapacity.FixedCols                   := 0;
    FgrdBreakpointYearsIrrSupplyCapacity.ColCount                    := 1;
    FgrdBreakpointYearsIrrSupplyCapacity.DefaultColWidth             := 50;
    FgrdBreakpointYearsIrrSupplyCapacity.DefaultRowHeight            := 20;
    FgrdBreakpointYearsIrrSupplyCapacity.RowCount                    := 1;
    FgrdBreakpointYearsIrrSupplyCapacity.Options                     := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointIrrSupplyCapacity                                  := CreateFieldLabel(lOwner, lParent, 10,  85,  420, 21);
    FgrdBreakpointIrrSupplyCapacity                                  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 85, 10, 25, 3, TRUE);
    FgrdBreakpointIrrSupplyCapacity.Anchors                          := [akLeft, akRight, akTop];
    FgrdBreakpointIrrSupplyCapacity.ScrollBars                       := ssNone;
    FgrdBreakpointIrrSupplyCapacity.FixedCols                        := 0;
    FgrdBreakpointIrrSupplyCapacity.ColCount                         := 1;
    FgrdBreakpointIrrSupplyCapacity.DefaultColWidth                  := 50;
    FgrdBreakpointIrrSupplyCapacity.DefaultRowHeight                 := 20;
    FgrdBreakpointIrrSupplyCapacity.RowCount                         := 1;
    FgrdBreakpointIrrSupplyCapacity.Options                          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    //________________________________________________________IrrigationEfficiency________________________________________
    FgboxIrrIrrigationEfficiency                                           := TGroupBox.Create(lOwner);
    FgboxIrrIrrigationEfficiency.Parent                                    := Self;
    FgboxIrrIrrigationEfficiency.Align                                     := alTop;
    FgboxIrrIrrigationEfficiency.Height                                    := 150;

    FsboxIrrIrrigationEfficiency                                           := TScrollBox.Create(lOwner);
    FsboxIrrIrrigationEfficiency.Parent                                    := FgboxIrrIrrigationEfficiency;
    FsboxIrrIrrigationEfficiency.AutoScroll                                := True;
    FsboxIrrIrrigationEfficiency.Align                                     := alClient;
    lParent                                                                  := FsboxIrrIrrigationEfficiency;

    FlblIrrIrrigationEfficiencyPointsCount                                 := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtIrrIrrigationEfficiencyPointsCount                                 := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrIrrigationEfficiency                                      := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FcmbMethodIrrIrrigationEfficiency                                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE, csDropDownList);

    FlblBreakpointYearsIrrIrrigationEfficiency                             := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsIrrIrrigationEfficiency                             := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 55,10, 25, 3, TRUE);
    FgrdBreakpointYearsIrrIrrigationEfficiency.Anchors                     := [akLeft, akRight, akTop];
    FgrdBreakpointYearsIrrIrrigationEfficiency.ScrollBars                  := ssNone;
    FgrdBreakpointYearsIrrIrrigationEfficiency.FixedCols                   := 0;
    FgrdBreakpointYearsIrrIrrigationEfficiency.ColCount                    := 1;
    FgrdBreakpointYearsIrrIrrigationEfficiency.DefaultColWidth             := 50;
    FgrdBreakpointYearsIrrIrrigationEfficiency.DefaultRowHeight            := 20;
    FgrdBreakpointYearsIrrIrrigationEfficiency.RowCount                    := 1;
    FgrdBreakpointYearsIrrIrrigationEfficiency.Options                     := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointIrrIrrigationEfficiency                                  := CreateFieldLabel(lOwner, lParent, 10,  85,  420, 21);
    FgrdBreakpointIrrIrrigationEfficiency                                  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 85, 10, 25, 3, TRUE);
    FgrdBreakpointIrrIrrigationEfficiency.Anchors                          := [akLeft, akRight, akTop];
    FgrdBreakpointIrrIrrigationEfficiency.ScrollBars                       := ssNone;
    FgrdBreakpointIrrIrrigationEfficiency.FixedCols                        := 0;
    FgrdBreakpointIrrIrrigationEfficiency.ColCount                         := 1;
    FgrdBreakpointIrrIrrigationEfficiency.DefaultColWidth                  := 50;
    FgrdBreakpointIrrIrrigationEfficiency.DefaultRowHeight                 := 20;
    FgrdBreakpointIrrIrrigationEfficiency.RowCount                         := 1;
    FgrdBreakpointIrrIrrigationEfficiency.Options                          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    //________________________________________________________ReturnFlowFactors________________________________________
    FgboxIrrReturnFlowFactors                                           := TGroupBox.Create(lOwner);
    FgboxIrrReturnFlowFactors.Parent                                    := Self;
    FgboxIrrReturnFlowFactors.Align                                     := alTop;
    FgboxIrrReturnFlowFactors.Height                                    := 150;

    FsboxIrrReturnFlowFactors                                           := TScrollBox.Create(lOwner);
    FsboxIrrReturnFlowFactors.Parent                                    := FgboxIrrReturnFlowFactors;
    FsboxIrrReturnFlowFactors.AutoScroll                                := True;
    FsboxIrrReturnFlowFactors.Align                                     := alClient;
    lParent                                                          := FsboxIrrReturnFlowFactors;

    FlblIrrReturnFlowFactorsPointsCount                                 := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtIrrReturnFlowFactorsPointsCount                                 := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrReturnFlowFactors                                      := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FcmbMethodIrrReturnFlowFactors                                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE, csDropDownList);

    FlblBreakpointYearsIrrReturnFlowFactors                             := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsIrrReturnFlowFactors                             := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 55,10, 25, 3, TRUE);
    FgrdBreakpointYearsIrrReturnFlowFactors.Anchors                     := [akLeft, akRight, akTop];
    FgrdBreakpointYearsIrrReturnFlowFactors.ScrollBars                  := ssNone;
    FgrdBreakpointYearsIrrReturnFlowFactors.FixedCols                   := 0;
    FgrdBreakpointYearsIrrReturnFlowFactors.ColCount                    := 1;
    FgrdBreakpointYearsIrrReturnFlowFactors.DefaultColWidth             := 50;
    FgrdBreakpointYearsIrrReturnFlowFactors.DefaultRowHeight            := 20;
    FgrdBreakpointYearsIrrReturnFlowFactors.RowCount                    := 1;
    FgrdBreakpointYearsIrrReturnFlowFactors.Options                     := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointIrrReturnFlowFactors                                  := CreateFieldLabel(lOwner, lParent, 10,  85,  420, 21);
    FgrdBreakpointIrrReturnFlowFactors                                  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 85, 10, 25, 3, TRUE);
    FgrdBreakpointIrrReturnFlowFactors.Anchors                          := [akLeft, akRight, akTop];
    FgrdBreakpointIrrReturnFlowFactors.ScrollBars                       := ssNone;
    FgrdBreakpointIrrReturnFlowFactors.FixedCols                        := 0;
    FgrdBreakpointIrrReturnFlowFactors.ColCount                         := 1;
    FgrdBreakpointIrrReturnFlowFactors.DefaultColWidth                  := 50;
    FgrdBreakpointIrrReturnFlowFactors.DefaultRowHeight                 := 20;
    FgrdBreakpointIrrReturnFlowFactors.RowCount                         := 1;
    FgrdBreakpointIrrReturnFlowFactors.Options                          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Dialog.Resize;
const OPNAME = 'TIrrBlockGrowthType4Dialog.Resize';
begin
  inherited Resize;
  try
    //________________________________________________________SupplyCapacity________________________________________
    grdBreakpointYearsIrrSupplyCapacity.Width := (grdBreakpointYearsIrrSupplyCapacity.DefaultColWidth * grdBreakpointYearsIrrSupplyCapacity.ColCount)+5;
    grdBreakpointIrrSupplyCapacity.Width      := (grdBreakpointIrrSupplyCapacity.DefaultColWidth      * grdBreakpointIrrSupplyCapacity.ColCount)+5;
    FsboxIrrSupplyCapacity.HorzScrollBar.Range  := grdBreakpointYearsIrrSupplyCapacity.Left + grdBreakpointYearsIrrSupplyCapacity.Width;

    //________________________________________________________IrrigationEfficiency________________________________________
    grdBreakpointYearsIrrIrrigationEfficiency.Width := (grdBreakpointYearsIrrIrrigationEfficiency.DefaultColWidth * grdBreakpointYearsIrrIrrigationEfficiency.ColCount)+5;
    grdBreakpointIrrIrrigationEfficiency.Width      := (grdBreakpointIrrIrrigationEfficiency.DefaultColWidth      * grdBreakpointIrrIrrigationEfficiency.ColCount)+5;
    FsboxIrrIrrigationEfficiency.HorzScrollBar.Range  := grdBreakpointYearsIrrIrrigationEfficiency.Left + grdBreakpointYearsIrrIrrigationEfficiency.Width;

    //________________________________________________________ReturnFlowFactors________________________________________
    grdBreakpointYearsIrrReturnFlowFactors.Width := (grdBreakpointYearsIrrReturnFlowFactors.DefaultColWidth * grdBreakpointYearsIrrReturnFlowFactors.ColCount)+5;
    grdBreakpointIrrReturnFlowFactors.Width      := (grdBreakpointIrrReturnFlowFactors.DefaultColWidth      * grdBreakpointIrrReturnFlowFactors.ColCount)+5;
    FsboxIrrReturnFlowFactors.HorzScrollBar.Range  := grdBreakpointYearsIrrReturnFlowFactors.Left + grdBreakpointYearsIrrReturnFlowFactors.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Dialog.Initialise: boolean;
const OPNAME = 'TIrrBlockGrowthType4Dialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Dialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockGrowthType4Dialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    //________________________________________________________SupplyCapacity________________________________________
    FgboxIrrSupplyCapacity.Caption               := 'Irrigation Supply Capacity:';
    FlblIrrSupplyCapacityPointsCount.Caption     := 'Points Count:';
    FlblMethodIrrSupplyCapacity.Caption          := 'Interpolation Method:';
    FlblBreakpointYearsIrrSupplyCapacity.Caption := 'Breakpoint Years:';
    FlblBreakpointIrrSupplyCapacity.Caption      := 'Breakpoint Supply Capacity:';
    //________________________________________________________IrrigationEfficiency________________________________________
    FgboxIrrIrrigationEfficiency.Caption               := 'Irrigation Efficiency:';
    FlblIrrIrrigationEfficiencyPointsCount.Caption     := 'Points Count:';
    FlblMethodIrrIrrigationEfficiency.Caption          := 'Interpolation Method:';
    FlblBreakpointYearsIrrIrrigationEfficiency.Caption := 'Breakpoint Years:';
    FlblBreakpointIrrIrrigationEfficiency.Caption      := 'Breakpoint Irrigation Efficiency:';
    //________________________________________________________ReturnFlowFactors________________________________________
    FgboxIrrReturnFlowFactors.Caption               := 'Return Flow Factors:';
    FlblIrrReturnFlowFactorsPointsCount.Caption     := 'Points Count:';
    FlblMethodIrrReturnFlowFactors.Caption          := 'Interpolation Method:';
    FlblBreakpointYearsIrrReturnFlowFactors.Caption := 'Breakpoint Years:';
    FlblBreakpointIrrReturnFlowFactors.Caption      := 'Breakpoint Return Flow Factors:';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Dialog.RestoreColourState;
const OPNAME = 'TIrrBlockGrowthType4Dialog.RestoreColourState';
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

procedure TIrrBlockGrowthType4Dialog.AssignHelpContext;
const OPNAME = 'TIrrBlockGrowthType4Dialog.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
