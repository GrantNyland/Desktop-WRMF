//
//  UNIT      : Contains the class TMineMonthlyDataDialog.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/12
//  COPYRIGHT : Copyright © 2007 DWAF
//

unit UMineMonthlyDataDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  Windows,
  VCL.Graphics,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TMineMonthlyDataDialog = class(TAbstractScrollablePanel)
  protected
    FGrdMothlyData  : TFieldStringGrid;
    FChtMonthlyData : TAbstractChart;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;

    property GrdMothlyData  : TFieldStringGrid read FGrdMothlyData;
    property ChtMonthlyData : TAbstractChart   read FChtMonthlyData;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  VCLTee.Chart,
  //UDBConstants,
  UErrorHandlingOperations;

{ TMineMonthlyDataDialog }

procedure TMineMonthlyDataDialog.CreateMemberObjects;
const OPNAME = 'TMineMonthlyDataDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    // Create

    FGrdMothlyData := TFieldStringGrid.Create(Self,FAppModules);
    FChtMonthlyData := TAbstractChart.Create(Self, FAppModules);
    FGrdMothlyData.Visible := True;
    // Set Parent

    FGrdMothlyData.Parent := ControlsParent;
    FChtMonthlyData.Parent := ControlsParent;

    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataDialog.Initialise: boolean;
const OPNAME = 'TMineMonthlyDataDialog.Initialise';
begin
  Result := inherited Initialise;

  try
    FGrdMothlyData.Reset;
    FChtMonthlyData.Initialise;

    // Set default Grid properties

    FGrdMothlyData.RowCount := 13;
    FGrdMothlyData.ColCount := 2;
    FGrdMothlyData.FixedRows := 1;
    FGrdMothlyData.FixedCols := 1;
    FGrdMothlyData.DefaultRowHeight := 18;
    FGrdMothlyData.ColWidths[1]  := 120;

    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataDialog.Resize;
const OPNAME = 'TMineMonthlyDataDialog.Resize';
var
  LClientWidth : integer;
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Check that construction is complete before laying out controls.

    if Assigned(FChtMonthlyData) then
    begin

      LClientWidth := Self.ClientWidth;

      // Position the string Grid

      FGrdMothlyData.Left := C_ControlBorder;
      FGrdMothlyData.Top := C_ControlBorder;

      // Resize the Grid

      FGrdMothlyData.Width  := LClientWidth div 3 - 2 * (C_ControlBorder + 40);
      FGrdMothlyData.Height := (FGrdMothlyData.DefaultRowHeight + 2) * FGrdMothlyData.RowCount;

      // Set the Grid column widths
      if Populated then
      begin
        if (FGrdMothlyData.ColCount >= 1) then
          FGrdMothlyData.ColWidths[0] := FGrdMothlyData.Width div 3 - 2 * FGrdMothlyData.ColCount;
        if (FGrdMothlyData.ColCount >= 2) then
          FGrdMothlyData.ColWidths[1] := FGrdMothlyData.Width - FGrdMothlyData.ColWidths[0] - 3 * FGrdMothlyData.ColCount;
      end;
      // Position the Chart

      VCL.Controls.TControl(FChtMonthlyData).Left := FGrdMothlyData.Left + FGrdMothlyData.Width + 2 * C_ControlBorder;
      VCL.Controls.TControl(FChtMonthlyData).Top := C_ControlBorder;

      // Resize the Chart

      FChtMonthlyData.Width := LClientWidth - FGrdMothlyData.Width - 5 * C_ControlBorder;
      FChtMonthlyData.Height := Self.ClientHeight - 6 * C_ControlBorder;

      if FScrollBox.VertScrollBar.IsScrollBarVisible then
        FChtMonthlyData.Width := FChtMonthlyData.Width - 3 * C_ControlBorder;

    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TMineMonthlyDataDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataDialog.SaveState: boolean;
const OPNAME = 'TMineMonthlyDataDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataDialog.AssignHelpContext;
const OPNAME = 'TMineMonthlyDataDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,            HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FGrdMothlyData, HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(VCL.Controls.TControl(FChtMonthlyData), HC_PhysicalReservoirCharacteristics);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.






