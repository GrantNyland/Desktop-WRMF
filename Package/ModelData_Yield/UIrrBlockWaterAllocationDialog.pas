{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockWaterAllocationDialog              *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockWaterAllocationDialog;

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

  TIrrBlockWaterAllocationDialog = class(TAbstractScrollablePanel)
  protected

    FlblAllocatedAreaPointsCount                     : TLabel;
    FedtAllocatedAreaPointsCount                     : TFieldEdit;

    FlblMethodIrrigatedAreas                         : TLabel;
    FedtMethodIrrigatedAreas                         : TFieldEdit;

    FlblBreakpointYearsDefined                       : TLabel;
    FgrdBreakpointYearsDefined                       : TFieldStringGrid;

    FlblBreakpointArea                               : TLabel;
    FgrdBreakpointArea                               : TFieldStringGrid;

    FlblMaxWaterAllocationCount                      : TLabel;
    FedtMaxWaterAllocationCount                      : TFieldEdit;

    FlblMethodMaxWaterAllocation                     : TLabel;
    FedtMethodMaxWaterAllocation                     : TFieldEdit;


    FlblBreakpointYearsMaxWaterAllocation            : TLabel;
    FgrdBreakpointYearsMaxWaterAllocation            : TFieldStringGrid;

    FlblBreakpointMaxWaterAllocation                 : TLabel;
    FgrdBreakpointMaxWaterAllocation                 : TFieldStringGrid;

    FlblBreakpointMaxWaterAllocationGrowth           : TLabel;
    FgrdBreakpointMaxWaterAllocationGrowth           : TFieldStringGrid;


    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property edtAllocatedAreaPointsCount : TFieldEdit read FedtAllocatedAreaPointsCount;
    property edtMethodIrrigatedAreas : TFieldEdit read FedtMethodIrrigatedAreas;
    property grdBreakpointArea : TFieldStringGrid read FgrdBreakpointArea;
    property edtMaxWaterAllocationCount : TFieldEdit read FedtMaxWaterAllocationCount;
    property edtMethodMaxWaterAllocation : TFieldEdit read FedtMethodMaxWaterAllocation;
    property grdBreakpointYearsMaxWaterAllocation : TFieldStringGrid read FgrdBreakpointYearsMaxWaterAllocation;
    property grdBreakpointMaxWaterAllocation : TFieldStringGrid read FgrdBreakpointMaxWaterAllocation;
     property grdBreakpointMaxWaterAllocationGrowth : TFieldStringGrid read FgrdBreakpointMaxWaterAllocationGrowth;

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
{* TIrrBlockWaterAllocationDialog                                                         *}
{******************************************************************************}

procedure TIrrBlockWaterAllocationDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockWaterAllocationDialog.CreateMemberObjects';
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
    lParent := ControlsParent;
    //                                                                      Left  Top Width Height


    FlblAllocatedAreaPointsCount                            := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtAllocatedAreaPointsCount                            := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblMethodIrrigatedAreas                                := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FedtMethodIrrigatedAreas                                := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 30, 100, 21,11, TRUE);

    FlblBreakpointYearsDefined                              := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FgrdBreakpointYearsDefined                              := CreateFieldStringGrid(FAppModules, lOwner, lParent, 230, 55, 100, 25, 3, TRUE);
    FgrdBreakpointYearsDefined.Anchors                      := [akLeft, akRight, akTop];
    FgrdBreakpointYearsDefined.ScrollBars                   := ssNone;
    FgrdBreakpointYearsDefined.FixedCols                    := 0;
    FgrdBreakpointYearsDefined.ColCount                     := 12;
    FgrdBreakpointYearsDefined.DefaultColWidth              := 50;
    FgrdBreakpointYearsDefined.DefaultRowHeight             := 20;
    FgrdBreakpointYearsDefined.RowCount                     := 1;
    FgrdBreakpointYearsDefined.Options                      := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointArea                                      := CreateFieldLabel(lOwner, lParent, 10,  80,  420, 21);
    FgrdBreakpointArea                                      := CreateFieldStringGrid(FAppModules, lOwner, lParent, 230, 80, 100, 25, 3, TRUE);
    FgrdBreakpointArea.Anchors                              := [akLeft, akRight, akTop];
    FgrdBreakpointArea.ScrollBars                           := ssNone;
    FgrdBreakpointArea.FixedCols                            := 0;
    FgrdBreakpointArea.ColCount                             := 12;
    FgrdBreakpointArea.DefaultColWidth                      := 50;
    FgrdBreakpointArea.DefaultRowHeight                     := 20;
    FgrdBreakpointArea.RowCount                             := 1;
    FgrdBreakpointArea.Options                              := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];



    FlblMaxWaterAllocationCount                             := CreateFieldLabel(lOwner, lParent, 10,  105,  420, 21);
    FedtMaxWaterAllocationCount                             := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 105, 100, 21,11, TRUE);

    FlblMethodMaxWaterAllocation                            := CreateFieldLabel(lOwner, lParent, 10,  130,  420, 21);
    FedtMethodMaxWaterAllocation                            := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 130, 100, 21,11, TRUE);


    FlblBreakpointYearsMaxWaterAllocation                   := CreateFieldLabel(lOwner, lParent, 10,  155,  420, 21);
    FgrdBreakpointYearsMaxWaterAllocation                   := CreateFieldStringGrid(FAppModules, lOwner, lParent, 230, 155, 100, 25, 3, TRUE);
    FgrdBreakpointYearsMaxWaterAllocation.Anchors           := [akLeft, akRight, akTop];
    FgrdBreakpointYearsMaxWaterAllocation.ScrollBars        := ssNone;
    FgrdBreakpointYearsMaxWaterAllocation.FixedCols         := 0;
    FgrdBreakpointYearsMaxWaterAllocation.ColCount          := 12;
    FgrdBreakpointYearsMaxWaterAllocation.DefaultColWidth   := 50;
    FgrdBreakpointYearsMaxWaterAllocation.DefaultRowHeight  := 20;
    FgrdBreakpointYearsMaxWaterAllocation.RowCount          := 1;
    FgrdBreakpointYearsMaxWaterAllocation.Options           := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointMaxWaterAllocation                        := CreateFieldLabel(lOwner, lParent, 10,  180,  420, 21);
    FgrdBreakpointMaxWaterAllocation                        := CreateFieldStringGrid(FAppModules, lOwner, lParent, 230, 180, 100, 25, 3, TRUE);
    FgrdBreakpointMaxWaterAllocation.Anchors                := [akLeft, akRight, akTop];
    FgrdBreakpointMaxWaterAllocation.ScrollBars             := ssNone;
    FgrdBreakpointMaxWaterAllocation.FixedCols              := 0;
    FgrdBreakpointMaxWaterAllocation.ColCount               := 12;
    FgrdBreakpointMaxWaterAllocation.DefaultColWidth        := 50;
    FgrdBreakpointMaxWaterAllocation.DefaultRowHeight       := 20;
    FgrdBreakpointMaxWaterAllocation.RowCount               := 1;
    FgrdBreakpointMaxWaterAllocation.Options                := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointMaxWaterAllocationGrowth                  := CreateFieldLabel(lOwner, lParent, 10,  205,  420, 21);
    FgrdBreakpointMaxWaterAllocationGrowth                  := CreateFieldStringGrid(FAppModules, lOwner, lParent, 230, 205, 100, 25, 3, TRUE);
    FgrdBreakpointMaxWaterAllocationGrowth.Anchors          := [akLeft, akRight, akTop];
    FgrdBreakpointMaxWaterAllocationGrowth.ScrollBars       := ssNone;
    FgrdBreakpointMaxWaterAllocationGrowth.FixedCols        := 0;
    FgrdBreakpointMaxWaterAllocationGrowth.ColCount         := 12;
    FgrdBreakpointMaxWaterAllocationGrowth.DefaultColWidth  := 50;
    FgrdBreakpointMaxWaterAllocationGrowth.DefaultRowHeight := 20;
    FgrdBreakpointMaxWaterAllocationGrowth.RowCount         := 1;
    FgrdBreakpointMaxWaterAllocationGrowth.Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationDialog.Resize;
const OPNAME = 'TIrrBlockWaterAllocationDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockWaterAllocationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockWaterAllocationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    FlblAllocatedAreaPointsCount.Caption := 'Allocated Area Points Count';
    FlblMethodIrrigatedAreas.Caption := 'Method Irrigated Areas';
    FlblBreakpointYearsDefined.Caption := 'Breakpoint Years Defined';
    FlblBreakpointArea.Caption := 'Breakpoint Area';
    FlblMaxWaterAllocationCount.Caption := 'Max Water Allocation Count';
    FlblMethodMaxWaterAllocation.Caption := 'Method Max Water Allocation';
    FlblBreakpointYearsMaxWaterAllocation.Caption := 'Breakpoint Years Max Water Allocation';
    FlblBreakpointMaxWaterAllocation.Caption := 'Breakpoint Max Water Allocation';
    FlblBreakpointMaxWaterAllocationGrowth.Caption := 'Breakpoint Max Water Allocation Growth';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationDialog.RestoreColourState;
const OPNAME = 'TIrrBlockWaterAllocationDialog.RestoreColourState';
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

procedure TIrrBlockWaterAllocationDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockWaterAllocationDialog.AssignHelpContext';
begin
  try
 
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
