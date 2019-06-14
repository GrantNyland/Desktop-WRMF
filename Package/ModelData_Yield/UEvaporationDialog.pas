{******************************************************************************}
{*  UNIT      : Contains the class TEvaporationDialog                         *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UEvaporationDialog;

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

  TEvaporationDialog = class(TAbstractScrollablePanel)
  protected
    FIrrigationBlockCatchmentFileNameLabel          : TLabel;
    FIrrigationBlockCatchmentMAPLabel               : TLabel;
    FIrrigationBlockCatchmentMAPEdit                : TFieldEdit;
    FIrrigationBlockCatchmentFileNameEdit           : TFieldEdit;

    FIrrigationBlockRainCatchmentScalingFactorLabel : TLabel;
    FIrrigationBlockRainCatchmentScalingFactorEdit  : TFieldEdit;

    FIrrigationBlockRainfallFactorLabel             : TLabel;
    FIrrigationBlockRainfallFactorGrid              : TFieldStringGrid;

    FIrrigationBlockPanEvaporationLabel             : TLabel;
    FIrrigationBlockPanEvaporationGrid              : TFieldStringGrid;

    FIrrigationBlockAPanConvFactorLabel             : TLabel;
    FIrrigationBlockAPanConvFactorGrid              : TFieldStringGrid;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property IrrigationBlockCatchmentMAPEdit                  : TFieldEdit      read FIrrigationBlockCatchmentMAPEdit;
    property IrrigationBlockCatchmentFileNameEdit              : TFieldEdit      read FIrrigationBlockCatchmentFileNameEdit;
    property IrrigationBlockRainCatchmentScalingFactorEdit    : TFieldEdit      read FIrrigationBlockRainCatchmentScalingFactorEdit;

    property IrrigationBlockRainfallFactorGrid : TFieldStringGrid read FIrrigationBlockRainfallFactorGrid;
    property IrrigationBlockPanEvaporationGrid : TFieldStringGrid read FIrrigationBlockPanEvaporationGrid;
    property IrrigationBlockAPanConvFactorGrid : TFieldStringGrid read FIrrigationBlockAPanConvFactorGrid;
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
{* TEvaporationDialog                                                         *}
{******************************************************************************}

procedure TEvaporationDialog.CreateMemberObjects;
const OPNAME = 'TEvaporationDialog.CreateMemberObjects';
var
  lOwner    : TComponent;
  lParent   : TWinControl;
  lWidth,
  lHeight,
  lGridTop,
  lTop      : Integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                      Left  Top Width Height
    FIrrigationBlockCatchmentFileNameLabel             := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FIrrigationBlockCatchmentFileNameEdit              := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 200, 21,11, TRUE);
    FIrrigationBlockCatchmentMAPLabel                  := CreateFieldLabel(lOwner, lParent, 465,  5,  70, 21);
    FIrrigationBlockCatchmentMAPEdit                   := CreateFieldEdit(FAppModules, lOwner, lParent, 540,  5,  100,  20, 8, TRUE);

    FIrrigationBlockRainCatchmentScalingFactorLabel    := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FIrrigationBlockRainCatchmentScalingFactorEdit     := CreateFieldEdit(FAppModules, lOwner, lParent, 230,  30,  100,  20, 8, TRUE);

    //rainfall factor
    FIrrigationBlockRainfallFactorLabel := CreateFieldLabel(lOwner, lParent, 10,  55,   420, 21);
    lGridTop  := FIrrigationBlockRainfallFactorLabel.Top + FIrrigationBlockRainfallFactorLabel.Height + 5;
    lWidth    := lParent.Width  - 20;
    lHeight   := 100;
    FIrrigationBlockRainfallFactorGrid  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, lGridTop, lWidth, lHeight, 3, TRUE);
    FIrrigationBlockRainfallFactorGrid.Anchors              := [akLeft, akRight, akTop];
    FIrrigationBlockRainfallFactorGrid.FixedCols            := 0;
    FIrrigationBlockRainfallFactorGrid.ColCount             := 12;
    FIrrigationBlockRainfallFactorGrid.DefaultColWidth      := 50;
    FIrrigationBlockRainfallFactorGrid.DefaultRowHeight     := 20;
    FIrrigationBlockRainfallFactorGrid.RowCount             := 2;
    FIrrigationBlockRainfallFactorGrid.Options              := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    //Pan evaporation
    lTop := lGridTop + lHeight + 5;
    FIrrigationBlockPanEvaporationLabel := CreateFieldLabel(lOwner, lParent, 10,  lTop,  420, 21);
    lGridTop  := FIrrigationBlockPanEvaporationLabel.Top + FIrrigationBlockPanEvaporationLabel.Height + 5;
    lWidth    := lParent.Width  - 20;
    lHeight   := 100;
    FIrrigationBlockPanEvaporationGrid  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, lGridTop, lWidth, lHeight, 3, TRUE);
    FIrrigationBlockPanEvaporationGrid.Anchors               := [akLeft, akRight, akTop];
    FIrrigationBlockPanEvaporationGrid.FixedCols             := 0;
    FIrrigationBlockPanEvaporationGrid.ColCount              := 12;
    FIrrigationBlockPanEvaporationGrid.DefaultColWidth       := 50;
    FIrrigationBlockPanEvaporationGrid.DefaultRowHeight      := 20;
    FIrrigationBlockPanEvaporationGrid.RowCount              := 2;
    FIrrigationBlockPanEvaporationGrid.Options               := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    //APan conversion factor
    lTop := lGridTop + lHeight + 5;
    FIrrigationBlockAPanConvFactorLabel := CreateFieldLabel(lOwner, lParent, 10,  lTop,  420, 21);
    lGridTop  := FIrrigationBlockAPanConvFactorLabel.Top + FIrrigationBlockAPanConvFactorLabel.Height + 5;
    lWidth    := lParent.Width  - 20;
    lHeight   := 100;
    FIrrigationBlockAPanConvFactorGrid  := CreateFieldStringGrid(FAppModules, lOwner, lParent,  10, lGridTop, lWidth, lHeight, 3, TRUE);
    FIrrigationBlockAPanConvFactorGrid.Anchors              := [akLeft, akRight, akTop];
    FIrrigationBlockAPanConvFactorGrid.FixedCols            := 0;
    FIrrigationBlockAPanConvFactorGrid.ColCount             := 12;
    FIrrigationBlockAPanConvFactorGrid.DefaultColWidth      := 50;
    FIrrigationBlockAPanConvFactorGrid.DefaultRowHeight     := 20;
    FIrrigationBlockAPanConvFactorGrid.RowCount             := 2;
    FIrrigationBlockAPanConvFactorGrid.Options              := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEvaporationDialog.Resize;
const OPNAME = 'TEvaporationDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEvaporationDialog.Initialise: boolean;
const OPNAME = 'TEvaporationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEvaporationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TEvaporationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FIrrigationBlockRainfallFactorLabel.Caption     := FAppModules.Language.GetString('TField.IrrigationBlockRainfallFactor');
    FIrrigationBlockPanEvaporationLabel.Caption     := FAppModules.Language.GetString('TField.IrrigationBlockPanEvaporation');
    FIrrigationBlockAPanConvFactorLabel.Caption     := FAppModules.Language.GetString('TField.IrrigationBlockAPanConvFactor');
    FIrrigationBlockCatchmentFileNameLabel.Caption  := FAppModules.Language.GetString('TField.IrrigationBlockCatchmentFileName');
    FIrrigationBlockCatchmentMAPLabel.Caption       := FAppModules.Language.GetString('TField.IrrigationBlockMAP');
    FIrrigationBlockRainCatchmentScalingFactorLabel.Caption     := FAppModules.Language.GetString('TField.IrrigationBlockRainCatchmentScalingFactor');    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEvaporationDialog.RestoreColourState;
const OPNAME = 'TEvaporationDialog.RestoreColourState';
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

procedure TEvaporationDialog.AssignHelpContext;
const OPNAME = 'TEvaporationDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                                           HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockCatchmentMAPEdit,               HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockCatchmentFileNameEdit,          HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockRainCatchmentScalingFactorEdit, HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockRainfallFactorGrid,             HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockPanEvaporationGrid,             HC_Irrigation);
    SetControlHelpContext(FIrrigationBlockAPanConvFactorGrid,             HC_Irrigation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
