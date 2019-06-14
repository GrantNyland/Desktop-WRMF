unit UGroundWaterEvaporationDialog;

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
  VCLTee.Chart,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TGroundWaterEvaporationDialog = class(TAbstractScrollablePanel)
  protected
    FGrdMonthlyData : TFieldStringGrid;
    FChtMonthlyData : TAbstractChart;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;

    property GrdMonthlyData : TFieldStringGrid read FGrdMonthlyData;
    property ChtMonthlyData : TAbstractChart   read FChtMonthlyData;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  //UDBConstants,
  UErrorHandlingOperations;

{ TMineMonthlyDataDialog }

procedure TGroundWaterEvaporationDialog.CreateMemberObjects;
const OPNAME = 'TGroundWaterEvaporationDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    // Create
    FGrdMonthlyData := TFieldStringGrid.Create(Self,FAppModules);
    FChtMonthlyData := TAbstractChart.Create(Self, FAppModules);
    FGrdMonthlyData.Visible := True;

    // Set Parent
    FGrdMonthlyData.Parent := ControlsParent;
    FChtMonthlyData.Parent :=  ControlsParent;

    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationDialog.Initialise: boolean;
const OPNAME = 'TGroundWaterEvaporationDialog.Initialise';
begin
  Result := inherited Initialise;

  try
    FGrdMonthlyData.Reset;
    FChtMonthlyData.Initialise;

    // Set default Grid properties

    FGrdMonthlyData.RowCount := 13;
    FGrdMonthlyData.ColCount := 2;
    FGrdMonthlyData.FixedRows := 1;
    FGrdMonthlyData.FixedCols := 1;
    FGrdMonthlyData.DefaultRowHeight := 18;
    FGrdMonthlyData.ColWidths[1]  := 140;

    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationDialog.Resize;
const OPNAME = 'TGroundWaterEvaporationDialog.Resize';
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

      FGrdMonthlyData.Left := C_ControlBorder;
      FGrdMonthlyData.Top := C_ControlBorder;

      // Resize the Grid

      FGrdMonthlyData.Width  := LClientWidth div 3 - 2 * (C_ControlBorder + 40);
      FGrdMonthlyData.Height := (FGrdMonthlyData.DefaultRowHeight + 2) * FGrdMonthlyData.RowCount;

      // Set the Grid column widths
      if Populated then
      begin
        if (FGrdMonthlyData.ColCount >= 1) then
          FGrdMonthlyData.ColWidths[0] := FGrdMonthlyData.Width div 3 - 2 * FGrdMonthlyData.ColCount;
        if (FGrdMonthlyData.ColCount >= 2) then
          FGrdMonthlyData.ColWidths[1] := FGrdMonthlyData.Width - FGrdMonthlyData.ColWidths[0] - 3 * FGrdMonthlyData.ColCount;
      end;
      // Position the Chart

      VCL.Controls.TControl(FChtMonthlyData).Left := FGrdMonthlyData.Left + FGrdMonthlyData.Width + 2 * C_ControlBorder;
      VCL.Controls.TControl(FChtMonthlyData).Top := C_ControlBorder;

      // Resize the Chart

      FChtMonthlyData.Width := LClientWidth - FGrdMonthlyData.Width - 5 * C_ControlBorder;
      FChtMonthlyData.Height := Self.ClientHeight - 6 * C_ControlBorder;

      if FScrollBox.VertScrollBar.IsScrollBarVisible then
        FChtMonthlyData.Width := FChtMonthlyData.Width - 3 * C_ControlBorder;

    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGroundWaterEvaporationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationDialog.SaveState: boolean;
const OPNAME = 'TGroundWaterEvaporationDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationDialog.AssignHelpContext;
const OPNAME = 'TGroundWaterEvaporationDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,            HC_WaterResourcesYieldModel);
{    SetControlHelpContext(FGrdMonthlyData, HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FChtMonthlyData, HC_PhysicalReservoirCharacteristics);}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

