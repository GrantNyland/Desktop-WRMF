{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockEfficienciesDialog              *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockGrowthDialog;

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

  TIrrBlockGrowthDialog = class(TAbstractScrollablePanel)
  protected
    //________________________________________________________IrrigatedAreas________________________________________
    FgboxIrrIrrigatedAreas               : TGroupBox;
    FsboxIrrIrrigatedAreas               : TScrollBox;

    FlblIrrIrrigatedAreasPointsCount     : TLabel;
    FedtIrrIrrigatedAreasPointsCount     : TFieldEdit;

    FlblMethodIrrIrrigatedAreas          : TLabel;
    FcmbMethodIrrIrrigatedAreas          : TFieldComboBox;

    FlblBreakpointYearsIrrIrrigatedAreas : TLabel;
    FgrdBreakpointYearsIrrIrrigatedAreas : TFieldStringGrid;

    FlblBreakpointIrrIrrigatedAreas      : TLabel;
    FgrdBreakpointIrrIrrigatedAreas      : TFieldStringGrid;

    //________________________________________________________MaximumWaterAllocation________________________________________
    FgboxIrrMaximumWaterAllocation               : TGroupBox;
    FsboxIrrMaximumWaterAllocation               : TScrollBox;

    FlblIrrMaximumWaterAllocationPointsCount     : TLabel;
    FedtIrrMaximumWaterAllocationPointsCount     : TFieldEdit;

    FlblMethodIrrMaximumWaterAllocation          : TLabel;
    FcmbMethodIrrMaximumWaterAllocation          : TFieldComboBox;

    FlblBreakpointYearsIrrMaximumWaterAllocation : TLabel;
    FgrdBreakpointYearsIrrMaximumWaterAllocation : TFieldStringGrid;

    FlblBreakpointIrrMaximumWaterAllocation      : TLabel;
    FgrdBreakpointIrrMaximumWaterAllocation      : TFieldStringGrid;
    FlblBreakpointIrrMaxWaterAllocGrowth         : TLabel;
    FgrdBreakpointIrrMaxWaterAllocGrowth         : TFieldStringGrid;

    //________________________________________________________ReturnFlowVolume________________________________________
    FgboxIrrReturnFlowVolume               : TGroupBox;
    FsboxIrrReturnFlowVolume               : TScrollBox;

    FlblIrrReturnFlowVolumePointsCount     : TLabel;
    FedtIrrReturnFlowVolumePointsCount     : TFieldEdit;

    FlblMethodIrrReturnFlowVolume          : TLabel;
    FcmbMethodIrrReturnFlowVolume          : TFieldComboBox;

    FlblBreakpointYearsIrrReturnFlowVolume : TLabel;
    FgrdBreakpointYearsIrrReturnFlowVolume : TFieldStringGrid;

    FlblBreakpointIrrReturnFlowVolume      : TLabel;
    FgrdBreakpointIrrReturnFlowVolume      : TFieldStringGrid;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    //________________________________________________________IrrigatedAreas________________________________________
    property edtIrrIrrigatedAreasPointsCount      : TFieldEdit         read FedtIrrIrrigatedAreasPointsCount;
    property cmbMethodIrrIrrigatedAreas           : TFieldComboBox     read FcmbMethodIrrIrrigatedAreas;
    property grdBreakpointYearsIrrIrrigatedAreas  : TFieldStringGrid   read FgrdBreakpointYearsIrrIrrigatedAreas;
    property grdBreakpointIrrIrrigatedAreas       : TFieldStringGrid   read FgrdBreakpointIrrIrrigatedAreas;

    //________________________________________________________IrrigatedAreas________________________________________
    property edtIrrMaximumWaterAllocationPointsCount      : TFieldEdit         read FedtIrrMaximumWaterAllocationPointsCount;
    property cmbMethodIrrMaximumWaterAllocation           : TFieldComboBox     read FcmbMethodIrrMaximumWaterAllocation;
    property grdBreakpointYearsIrrMaximumWaterAllocation  : TFieldStringGrid   read FgrdBreakpointYearsIrrMaximumWaterAllocation;
    property grdBreakpointIrrMaximumWaterAllocation       : TFieldStringGrid   read FgrdBreakpointIrrMaximumWaterAllocation;
    property grdBreakpointIrrMaxWaterAllocGrowth          : TFieldStringGrid   read FgrdBreakpointIrrMaxWaterAllocGrowth;

    //________________________________________________________ReturnFlowVolume________________________________________
    property edtIrrReturnFlowVolumePointsCount      : TFieldEdit         read FedtIrrReturnFlowVolumePointsCount;
    property cmbMethodIrrReturnFlowVolume           : TFieldComboBox     read FcmbMethodIrrReturnFlowVolume;
    property grdBreakpointYearsIrrReturnFlowVolume  : TFieldStringGrid   read FgrdBreakpointYearsIrrReturnFlowVolume;
    property grdBreakpointIrrReturnFlowVolume       : TFieldStringGrid   read FgrdBreakpointIrrReturnFlowVolume;



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
{* TIrrBlockGrowthDialog                                                         *}
{******************************************************************************}

procedure TIrrBlockGrowthDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockGrowthDialog.CreateMemberObjects';
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
    //________________________________________________________IrrigatedAreas________________________________________
    FgboxIrrIrrigatedAreas                                           := TGroupBox.Create(lOwner);
    FgboxIrrIrrigatedAreas.Parent                                    := Self;
    FgboxIrrIrrigatedAreas.Align                                     := alTop;
    FgboxIrrIrrigatedAreas.Height                                    := 150;

    FsboxIrrIrrigatedAreas                                           := TScrollBox.Create(lOwner);
    FsboxIrrIrrigatedAreas.Parent                                    := FgboxIrrIrrigatedAreas;
    FsboxIrrIrrigatedAreas.AutoScroll                                := True;
    FsboxIrrIrrigatedAreas.Align                                     := alClient;
    lParent                                                          := FsboxIrrIrrigatedAreas;

    FlblIrrIrrigatedAreasPointsCount                                 := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtIrrIrrigatedAreasPointsCount                                 := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrIrrigatedAreas                                      := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FcmbMethodIrrIrrigatedAreas                                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE, csDropDownList);

    FlblBreakpointYearsIrrIrrigatedAreas                             := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsIrrIrrigatedAreas                             := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 55,10, 25, 3, TRUE);
    FgrdBreakpointYearsIrrIrrigatedAreas.Anchors                     := [akLeft, akRight, akTop];
    FgrdBreakpointYearsIrrIrrigatedAreas.ScrollBars                  := ssNone;
    FgrdBreakpointYearsIrrIrrigatedAreas.FixedCols                   := 0;
    FgrdBreakpointYearsIrrIrrigatedAreas.ColCount                    := 1;
    FgrdBreakpointYearsIrrIrrigatedAreas.DefaultColWidth             := 50;
    FgrdBreakpointYearsIrrIrrigatedAreas.DefaultRowHeight            := 20;
    FgrdBreakpointYearsIrrIrrigatedAreas.RowCount                    := 1;
    FgrdBreakpointYearsIrrIrrigatedAreas.Options                     := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointIrrIrrigatedAreas                                  := CreateFieldLabel(lOwner, lParent, 10,  85,  420, 21);
    FgrdBreakpointIrrIrrigatedAreas                                  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 85, 10, 25, 3, TRUE);
    FgrdBreakpointIrrIrrigatedAreas.Anchors                          := [akLeft, akRight, akTop];
    FgrdBreakpointIrrIrrigatedAreas.ScrollBars                       := ssNone;
    FgrdBreakpointIrrIrrigatedAreas.FixedCols                        := 0;
    FgrdBreakpointIrrIrrigatedAreas.ColCount                         := 1;
    FgrdBreakpointIrrIrrigatedAreas.DefaultColWidth                  := 50;
    FgrdBreakpointIrrIrrigatedAreas.DefaultRowHeight                 := 20;
    FgrdBreakpointIrrIrrigatedAreas.RowCount                         := 1;
    FgrdBreakpointIrrIrrigatedAreas.Options                          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    //________________________________________________________MaximumWaterAllocation________________________________________
    FgboxIrrMaximumWaterAllocation                                           := TGroupBox.Create(lOwner);
    FgboxIrrMaximumWaterAllocation.Parent                                    := Self;
    FgboxIrrMaximumWaterAllocation.Align                                     := alTop;
    FgboxIrrMaximumWaterAllocation.Height                                    := 180;

    FsboxIrrMaximumWaterAllocation                                           := TScrollBox.Create(lOwner);
    FsboxIrrMaximumWaterAllocation.Parent                                    := FgboxIrrMaximumWaterAllocation;
    FsboxIrrMaximumWaterAllocation.AutoScroll                                := True;
    FsboxIrrMaximumWaterAllocation.Align                                     := alClient;
    lParent                                                                  := FsboxIrrMaximumWaterAllocation;

    FlblIrrMaximumWaterAllocationPointsCount                                 := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtIrrMaximumWaterAllocationPointsCount                                 := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrMaximumWaterAllocation                                      := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FcmbMethodIrrMaximumWaterAllocation                                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE, csDropDownList);

    FlblBreakpointYearsIrrMaximumWaterAllocation                             := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsIrrMaximumWaterAllocation                             := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 55,10, 25, 3, TRUE);
    FgrdBreakpointYearsIrrMaximumWaterAllocation.Anchors                     := [akLeft, akRight, akTop];
    FgrdBreakpointYearsIrrMaximumWaterAllocation.ScrollBars                  := ssNone;
    FgrdBreakpointYearsIrrMaximumWaterAllocation.FixedCols                   := 0;
    FgrdBreakpointYearsIrrMaximumWaterAllocation.ColCount                    := 1;
    FgrdBreakpointYearsIrrMaximumWaterAllocation.DefaultColWidth             := 50;
    FgrdBreakpointYearsIrrMaximumWaterAllocation.DefaultRowHeight            := 20;
    FgrdBreakpointYearsIrrMaximumWaterAllocation.RowCount                    := 1;
    FgrdBreakpointYearsIrrMaximumWaterAllocation.Options                     := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointIrrMaximumWaterAllocation                                  := CreateFieldLabel(lOwner, lParent, 10,  85,  420, 21);
    FgrdBreakpointIrrMaximumWaterAllocation                                  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 85, 10, 25, 3, TRUE);
    FgrdBreakpointIrrMaximumWaterAllocation.Anchors                          := [akLeft, akRight, akTop];
    FgrdBreakpointIrrMaximumWaterAllocation.ScrollBars                       := ssNone;
    FgrdBreakpointIrrMaximumWaterAllocation.FixedCols                        := 0;
    FgrdBreakpointIrrMaximumWaterAllocation.ColCount                         := 1;
    FgrdBreakpointIrrMaximumWaterAllocation.DefaultColWidth                  := 50;
    FgrdBreakpointIrrMaximumWaterAllocation.DefaultRowHeight                 := 20;
    FgrdBreakpointIrrMaximumWaterAllocation.RowCount                         := 1;
    FgrdBreakpointIrrMaximumWaterAllocation.Options                          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    FlblBreakpointIrrMaxWaterAllocGrowth                                     := CreateFieldLabel(lOwner, lParent, 10,  115,  420, 21);
    FgrdBreakpointIrrMaxWaterAllocGrowth                                     := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 115, 10, 25, 3, TRUE);
    FgrdBreakpointIrrMaxWaterAllocGrowth.Anchors                             := [akLeft, akRight, akTop];
    FgrdBreakpointIrrMaxWaterAllocGrowth.ScrollBars                          := ssNone;
    FgrdBreakpointIrrMaxWaterAllocGrowth.FixedCols                           := 0;
    FgrdBreakpointIrrMaxWaterAllocGrowth.ColCount                            := 1;
    FgrdBreakpointIrrMaxWaterAllocGrowth.DefaultColWidth                     := 50;
    FgrdBreakpointIrrMaxWaterAllocGrowth.DefaultRowHeight                    := 20;
    FgrdBreakpointIrrMaxWaterAllocGrowth.RowCount                            := 1;
    FgrdBreakpointIrrMaxWaterAllocGrowth.Options                             := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];



    //________________________________________________________ReturnFlowVolume________________________________________
    FgboxIrrReturnFlowVolume                                           := TGroupBox.Create(lOwner);
    FgboxIrrReturnFlowVolume.Parent                                    := Self;
    FgboxIrrReturnFlowVolume.Align                                     := alTop;
    FgboxIrrReturnFlowVolume.Height                                    := 150;

    FsboxIrrReturnFlowVolume                                           := TScrollBox.Create(lOwner);
    FsboxIrrReturnFlowVolume.Parent                                    := FgboxIrrReturnFlowVolume;
    FsboxIrrReturnFlowVolume.AutoScroll                                := True;
    FsboxIrrReturnFlowVolume.Align                                     := alClient;
    lParent                                                            := FsboxIrrReturnFlowVolume;

    FlblIrrReturnFlowVolumePointsCount                                 := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtIrrReturnFlowVolumePointsCount                                 := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrReturnFlowVolume                                      := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FcmbMethodIrrReturnFlowVolume                                      := CreateFieldComboBox(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE, csDropDownList);

    FlblBreakpointYearsIrrReturnFlowVolume                             := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsIrrReturnFlowVolume                             := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 55,10, 25, 3, TRUE);
    FgrdBreakpointYearsIrrReturnFlowVolume.Anchors                     := [akLeft, akRight, akTop];
    FgrdBreakpointYearsIrrReturnFlowVolume.ScrollBars                  := ssNone;
    FgrdBreakpointYearsIrrReturnFlowVolume.FixedCols                   := 0;
    FgrdBreakpointYearsIrrReturnFlowVolume.ColCount                    := 1;
    FgrdBreakpointYearsIrrReturnFlowVolume.DefaultColWidth             := 50;
    FgrdBreakpointYearsIrrReturnFlowVolume.DefaultRowHeight            := 20;
    FgrdBreakpointYearsIrrReturnFlowVolume.RowCount                    := 1;
    FgrdBreakpointYearsIrrReturnFlowVolume.Options                     := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointIrrReturnFlowVolume                                  := CreateFieldLabel(lOwner, lParent, 10,  85,  420, 21);
    FgrdBreakpointIrrReturnFlowVolume                                  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 85, 10, 25, 3, TRUE);
    FgrdBreakpointIrrReturnFlowVolume.Anchors                          := [akLeft, akRight, akTop];
    FgrdBreakpointIrrReturnFlowVolume.ScrollBars                       := ssNone;
    FgrdBreakpointIrrReturnFlowVolume.FixedCols                        := 0;
    FgrdBreakpointIrrReturnFlowVolume.ColCount                         := 1;
    FgrdBreakpointIrrReturnFlowVolume.DefaultColWidth                  := 50;
    FgrdBreakpointIrrReturnFlowVolume.DefaultRowHeight                 := 20;
    FgrdBreakpointIrrReturnFlowVolume.RowCount                         := 1;
    FgrdBreakpointIrrReturnFlowVolume.Options                          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthDialog.Resize;
const OPNAME = 'TIrrBlockGrowthDialog.Resize';
begin
  inherited Resize;
  try
    //________________________________________________________IrrigatedAreas________________________________________
    grdBreakpointYearsIrrIrrigatedAreas.Width := (grdBreakpointYearsIrrIrrigatedAreas.DefaultColWidth * grdBreakpointYearsIrrIrrigatedAreas.ColCount)+5;
    grdBreakpointIrrIrrigatedAreas.Width      := (grdBreakpointIrrIrrigatedAreas.DefaultColWidth      * grdBreakpointIrrIrrigatedAreas.ColCount)+5;
    FsboxIrrIrrigatedAreas.HorzScrollBar.Range  := grdBreakpointYearsIrrIrrigatedAreas.Left + grdBreakpointYearsIrrIrrigatedAreas.Width;

    //________________________________________________________MaximumWaterAllocation________________________________________
    grdBreakpointYearsIrrMaximumWaterAllocation.Width := (grdBreakpointYearsIrrMaximumWaterAllocation.DefaultColWidth * grdBreakpointYearsIrrMaximumWaterAllocation.ColCount)+5;
    grdBreakpointIrrMaximumWaterAllocation.Width      := (grdBreakpointIrrMaximumWaterAllocation.DefaultColWidth      * grdBreakpointIrrMaximumWaterAllocation.ColCount)+5;
    grdBreakpointIrrMaxWaterAllocGrowth.Width      := (grdBreakpointIrrMaxWaterAllocGrowth.DefaultColWidth      * grdBreakpointIrrMaxWaterAllocGrowth.ColCount)+5;

    FsboxIrrMaximumWaterAllocation.HorzScrollBar.Range  := grdBreakpointYearsIrrMaximumWaterAllocation.Left + grdBreakpointYearsIrrMaximumWaterAllocation.Width;

    //________________________________________________________ReturnFlowVolume________________________________________
    grdBreakpointYearsIrrReturnFlowVolume.Width := (grdBreakpointYearsIrrReturnFlowVolume.DefaultColWidth * grdBreakpointYearsIrrReturnFlowVolume.ColCount)+5;
    grdBreakpointIrrReturnFlowVolume.Width      := (grdBreakpointIrrReturnFlowVolume.DefaultColWidth      * grdBreakpointIrrReturnFlowVolume.ColCount)+5;
    FsboxIrrReturnFlowVolume.HorzScrollBar.Range  := grdBreakpointYearsIrrReturnFlowVolume.Left + grdBreakpointYearsIrrReturnFlowVolume.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockGrowthDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockGrowthDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    //________________________________________________________IrrigatedAreas________________________________________
    FgboxIrrIrrigatedAreas.Caption               := 'Irrigated Areas:';
    FlblIrrIrrigatedAreasPointsCount.Caption     := 'Points Count:';
    FlblMethodIrrIrrigatedAreas.Caption          := 'Interpolation Method:';
    FlblBreakpointYearsIrrIrrigatedAreas.Caption := 'Breakpoint Years:';
    FlblBreakpointIrrIrrigatedAreas.Caption      := 'Breakpoint Irrigated Area:';
    //________________________________________________________MaximumWaterAllocation________________________________________
    FgboxIrrMaximumWaterAllocation.Caption               := 'Maximum Water Allocation:';
    FlblIrrMaximumWaterAllocationPointsCount.Caption     := 'Points Count:';
    FlblMethodIrrMaximumWaterAllocation.Caption          := 'Interpolation Method:';
    FlblBreakpointYearsIrrMaximumWaterAllocation.Caption := 'Breakpoint Years:';
    FlblBreakpointIrrMaximumWaterAllocation.Caption      := 'Breakpoint Maximum Water Allocation:';
    FlblBreakpointIrrMaxWaterAllocGrowth.Caption         := 'Breakpoint Maximum Water Allocation Growth:';
    //________________________________________________________ReturnFlowVolume________________________________________
    FgboxIrrReturnFlowVolume.Caption               := 'Return Flow Volume:';
    FlblIrrReturnFlowVolumePointsCount.Caption     := 'Points Count:';
    FlblMethodIrrReturnFlowVolume.Caption          := 'Interpolation Method:';
    FlblBreakpointYearsIrrReturnFlowVolume.Caption := 'Breakpoint Years:';
    FlblBreakpointIrrReturnFlowVolume.Caption      := 'Breakpoint Return Flow Volume:';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthDialog.RestoreColourState;
const OPNAME = 'TIrrBlockGrowthDialog.RestoreColourState';
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

procedure TIrrBlockGrowthDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockGrowthDialog.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
