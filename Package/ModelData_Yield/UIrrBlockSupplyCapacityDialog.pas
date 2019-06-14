{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockSupplyCapacityDialog              *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockSupplyCapacityDialog;

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

  TIrrBlockSupplyCapacityDialog = class(TAbstractScrollablePanel)
  protected

    FlblSupplyCapacity                                : TLabel;
    FedtSupplyCapacity                                : TFieldEdit;

    FlblSupplyCapacityPointsCount                     : TLabel;
    FedtSupplyCapacityPointsCount                     : TFieldEdit;

    FlblMethodSupplyCapacity                          : TLabel;
    FedtMethodSupplyCapacity                          : TFieldEdit;

    FlblBreakpointYearsSupplyCapacity                 : TLabel;
    FgrdBreakpointYearsSupplyCapacity                 : TFieldStringGrid;

    FlblBreakpointSupplyCapacity                      : TLabel;
    FgrdBreakpointSupplyCapacity                      : TFieldStringGrid;

    procedure CreateMemberObjects;        override;
    procedure AssignHelpContext;          override;
  public
    procedure Resize;                     override;
    procedure RestoreColourState;         override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean;         override;

    property edtSupplyCapacity                 : TFieldEdit         read FedtSupplyCapacity;
    property edtSupplyCapacityPointsCount      : TFieldEdit         read FedtSupplyCapacityPointsCount;
    property edtMethodSupplyCapacity           : TFieldEdit         read FedtMethodSupplyCapacity;
    property grdBreakpointYearsSupplyCapacity  : TFieldStringGrid   read FgrdBreakpointYearsSupplyCapacity;
    property grdBreakpointSupplyCapacity       : TFieldStringGrid   read FgrdBreakpointSupplyCapacity;


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
{* TIrrBlockSupplyCapacityDialog                                                         *}
{******************************************************************************}

procedure TIrrBlockSupplyCapacityDialog.CreateMemberObjects;
const OPNAME = 'TIrrBlockSupplyCapacityDialog.CreateMemberObjects';
var
  lOwner    : TComponent;
  lParent   : TWinControl;
 // lWidth,
 // lHeight,
  //lGridTop,
 // lTop      : Integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                      Left  Top Width Height



    FlblSupplyCapacity                                := CreateFieldLabel(lOwner, lParent, 10,  5,  420, 21);
    FedtSupplyCapacity                                := CreateFieldEdit(FAppModules, lOwner, lParent, 230, 5, 100, 21,11, TRUE);

    FlblSupplyCapacityPointsCount                     := CreateFieldLabel(lOwner, lParent, 10,  30,  420, 21);
    FedtSupplyCapacityPointsCount                     := CreateFieldEdit(FAppModules, lOwner, lParent, 230,  30,  100,  20, 8, TRUE);

    FlblMethodSupplyCapacity                          := CreateFieldLabel(lOwner, lParent, 10,  55,  420, 21);
    FedtMethodSupplyCapacity                               := CreateFieldEdit(FAppModules, lOwner, lParent, 230,  55,  100,  20, 8, TRUE);

    FlblBreakpointYearsSupplyCapacity                      := CreateFieldLabel(lOwner, lParent, 10,  80,   420, 21);
    FgrdBreakpointYearsSupplyCapacity                      := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 80, 100, 25, 3, TRUE);
    FgrdBreakpointYearsSupplyCapacity.Anchors              := [akLeft, akRight, akTop];
    FgrdBreakpointYearsSupplyCapacity.ScrollBars           := ssNone;
    FgrdBreakpointYearsSupplyCapacity.FixedCols            := 0;
    FgrdBreakpointYearsSupplyCapacity.ColCount             := 12;
    FgrdBreakpointYearsSupplyCapacity.DefaultColWidth      := 50;
    FgrdBreakpointYearsSupplyCapacity.DefaultRowHeight     := 20;
    FgrdBreakpointYearsSupplyCapacity.RowCount             := 1;
    FgrdBreakpointYearsSupplyCapacity.Options              := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FlblBreakpointSupplyCapacity                            := CreateFieldLabel(lOwner, lParent, 10,  105,   420, 21);
    FgrdBreakpointSupplyCapacity                            := CreateFieldStringGrid(FAppModules, lOwner, lParent,  230, 105, 100, 25, 3, TRUE);
    FgrdBreakpointSupplyCapacity.Anchors                    := [akLeft, akRight, akTop];
    FgrdBreakpointSupplyCapacity.ScrollBars                 := ssNone;
    FgrdBreakpointSupplyCapacity.FixedCols                  := 0;
    FgrdBreakpointSupplyCapacity.ColCount                   := 12;
    FgrdBreakpointSupplyCapacity.DefaultColWidth            := 50;
    FgrdBreakpointSupplyCapacity.DefaultRowHeight           := 20;
    FgrdBreakpointSupplyCapacity.RowCount                   := 1;
    FgrdBreakpointSupplyCapacity.Options                    := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];



  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityDialog.Resize;
const OPNAME = 'TIrrBlockSupplyCapacityDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityDialog.Initialise: boolean;
const OPNAME = 'TIrrBlockSupplyCapacityDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockSupplyCapacityDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FlblSupplyCapacity.Caption := 'Supply Capacity';
    FlblSupplyCapacityPointsCount.Caption := 'Supply Capacity Points Count';
    FlblMethodSupplyCapacity.Caption := 'Method';
    FlblBreakpointYearsSupplyCapacity.Caption := 'Breakpoint Years';
    FlblBreakpointSupplyCapacity.Caption := 'Breakpoint Supply Capacity';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityDialog.RestoreColourState;
const OPNAME = 'TIrrBlockSupplyCapacityDialog.RestoreColourState';
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

procedure TIrrBlockSupplyCapacityDialog.AssignHelpContext;
const OPNAME = 'TIrrBlockSupplyCapacityDialog.AssignHelpContext';
begin
  try
  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
