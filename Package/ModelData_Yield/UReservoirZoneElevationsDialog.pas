//
//
//  UNIT      : Contains the class TReservoirZoneElevationsDialog.
//  AUTHOR    : Valentino Naicker (arivia.kom)
//  DATE      : 2003/06/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirZoneElevationsDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  VCLTee.Series,

  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent;

type

  TReservoirZoneElevationsDialog = class(TAbstractScrollablePanel)
  private
  protected

    FNoOfDrawDownZones : integer;
    FPenaltyArray : Array of Double;

    FPanTop : TAbstractPanel;
    FPanGrid : TAbstractPanel;
    FPanChart : TAbstractPanel;
    FPanResAffected : TPanel;
    //FPanBottom : TAbstractPanel;

    FGrdZone : TFieldStringGrid;
    FChtElevation : TAbstractChart;

    FEdtFullSupplyLevel : TFieldEdit;
    FEdtDeadStorageLevel : TFieldEdit;
    FEdtBottomOfReservoirLevel : TFieldEdit;

    FLblFullSupplyLevel : TLabel;
    FLblDeadStorageLevel : TLabel;
    FLblBottomOfReservoirLevel : TLabel;

    FViewType: TFieldRadioGroup;

    procedure SetNoOfDrawDownZones(const AValue: integer);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;

  public

    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;

    property NoOfDrawDownZones : integer read FNoOfDrawDownZones write SetNoOfDrawDownZones;

    property EdtFullSupplyLevel : TFieldEdit read FEdtFullSupplyLevel;
    property EdtDeadStorageLevel : TFieldEdit read FEdtDeadStorageLevel;
    property EdtBottomOfReservoirLevel : TFieldEdit read FEdtBottomOfReservoirLevel;

    property GrdZone : TFieldStringGrid read FGrdZone;
    property ChtElevation : TAbstractChart read FChtElevation;

    property ReservoirsAffectedPanel : TPanel read FPanResAffected;
    procedure RestoreColourState; override;

  end;

implementation

uses

  // Delphi VCL, RTL, etc
  VCLTee.Chart,
  VCLTee.TeeProcs,
  SysUtils,
  UHelpContexts,
  VCL.Graphics,
  VCLTee.TeEngine,

  // arivia.kom
  UErrorHandlingOperations;

{ TReservoirEvaporationDialog }

procedure TReservoirZoneElevationsDialog.CreateMemberObjects;
const OPNAME = 'TReservoirZoneElevationsDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanTop := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanGrid := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanChart:= TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanResAffected := TPanel.Create(ControlsOwner);
    //FPanBottom := TAbstractPanel.Create(ControlsOwner, FAppModules);

    FViewType         := TFieldRadioGroup.Create(ControlsOwner, FAppModules);
    FViewType.Parent  := Self;
    FViewType.Height  := 34;

    FPanTop.Parent := Self;
    FPanGrid.Parent := Self;
    FPanChart.Parent := Self;
    FPanResAffected.Parent := Self;
    //FPanBottom.Parent := Self;

    FPanTop.Align := alTop;
    FPanGrid.Align := alTop;
    FPanChart.Align := alTop;
    FViewType.Align  := alTop;
    FPanResAffected.Align := alTop;
    //FPanBottom.Align := alTop;

    FChtElevation := TAbstractChart.Create(ControlsOwner, FAppModules);
    FChtElevation.Parent := FPanChart;
    FChtElevation.Align := alClient;

    FChtElevation.AnimatedZoom := True;
    //FChtElevation.BackWall.Brush.Color := clWhite;
    //FChtElevation.BackWall.Brush.Style := bsClear;
    FChtElevation.MarginBottom := 10;
    FChtElevation.MarginTop := 10;

    FChtElevation.Title.Visible := False;
    FChtElevation.Legend.Visible := False;

    FChtElevation.RightAxis.Axis.Visible := False;
    FChtElevation.TopAxis.Axis.Visible := False;
    FChtElevation.BottomAxis.Axis.Visible := True;
    FChtElevation.LeftAxis.Axis.Visible := True;

    FChtElevation.View3D := False;
    FChtElevation.Align := alBottom;
    FChtElevation.BevelOuter := bvNone;

    FGrdZone := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FGrdZone.Parent := FPanGrid;
    FGrdZone.Visible := True;
    FGrdZone.DblClickColAutoSize := False;

    FGrdZone.Top := C_ControlBorder;
    FGrdZone.Left := C_ControlBorder;
    FGrdZone.ColCount := 13;
    FGrdZone.RowCount := 2;
    FGrdZone.FixedCols := 1;
    FGrdZone.FixedRows := 1;
    FGrdZone.DefaultRowHeight := 18;

    FEdtFullSupplyLevel := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEdtDeadStorageLevel := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEdtBottomOfReservoirLevel := TFieldEdit.Create(ControlsOwner, FAppModules);

    FEdtFullSupplyLevel.Parent := FPanTop;
    FEdtDeadStorageLevel.Parent := FPanTop;
    FEdtBottomOfReservoirLevel.Parent := FPanTop;

    FLblFullSupplyLevel := TLabel.Create(ControlsOwner);
    FLblDeadStorageLevel := TLabel.Create(ControlsOwner);
    FLblBottomOfReservoirLevel := TLabel.Create(ControlsOwner);

    FLblFullSupplyLevel.Parent := FPanTop;
    FLblDeadStorageLevel.Parent := FPanTop;
    FLblBottomOfReservoirLevel.Parent := FPanTop;

    FLblFullSupplyLevel.Left := C_ControlBorder;
    FLblDeadStorageLevel.Left := C_ControlBorder;
    FLblBottomOfReservoirLevel.Left := C_ControlBorder;

    FEdtFullSupplyLevel.Height := C_EditBoxHeight;
    FEdtDeadStorageLevel.Height := C_EditBoxHeight;
    FEdtBottomOfReservoirLevel.Height := C_EditBoxHeight;

    FLblFullSupplyLevel.Height := C_LabelHeight;
    FLblDeadStorageLevel.Height := C_LabelHeight;
    FLblBottomOfReservoirLevel.Height := C_LabelHeight;

    FEdtFullSupplyLevel.Top := C_ControlBorder;
    FEdtDeadStorageLevel.Top := C_ControlBorder;
    FEdtBottomOfReservoirLevel.Top := C_ControlBorder;
    //FEdtBottomOfReservoirLevel.Top := 2 * C_ControlBorder + C_EditBoxHeight;

    FLblFullSupplyLevel.Top := FEdtFullSupplyLevel.Top + C_ControlBorder div 2;
    FLblDeadStorageLevel.Top := FEdtDeadStorageLevel.Top + C_ControlBorder div 2;
    FLblBottomOfReservoirLevel.Top := FEdtBottomOfReservoirLevel.Top + C_ControlBorder div 2;

    FPanResAffected.Alignment := taLeftJustify;
    FLblFullSupplyLevel.Alignment := taRightJustify;
    FLblDeadStorageLevel.Alignment := taRightJustify;
    FLblBottomOfReservoirLevel.Alignment := taRightJustify;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsDialog.Initialise: boolean;
const OPNAME = 'TReservoirZoneElevationsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrdZone.Reset;
    FChtElevation.Initialise;
    FChtElevation.LeftAxis.AxisValuesFormat   := '###0.###';

    FEdtFullSupplyLevel.Reset;
    FEdtDeadStorageLevel.Reset;
    FEdtBottomOfReservoirLevel.Reset;

    FViewType.Enabled   := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsDialog.Resize;
const OPNAME = 'TReservoirZoneElevationsDialog.Resize';
var
  LIndex : integer;
  LClientHeight : integer;
  LUnitWidth,
  LLabelWidth,
  LEditWidth: integer;
begin
  // Call the ancestor.
  inherited Resize;
  try

    FPanTop.Height := 2 * C_ControlBorder + C_EditBoxHeight;
    //FPanBottom.Height := 4 * C_ControlBorder + 2 * C_EditBoxHeight;
    FPanResAffected.Height := C_ControlBorder + C_EditBoxHeight;

    FGrdZone.Width := FPanGrid.ClientWidth;

    LClientHeight := Self.ClientHeight;

    FPanGrid.Height := (LClientHeight - FPanTop.Height - FPanResAffected.Height - FViewType.Height - FHintDisplay.Height) div 2;
    FPanChart.Height := (LClientHeight - FPanTop.Height - FPanResAffected.Height - FViewType.Height - FHintDisplay.Height) div 2;
    FGrdZone.Height := FPanGrid.ClientHeight;

    for LIndex := 0 to FGrdZone.ColCount - 1 do
      FGrdZone.ColWidths[LIndex] := ((FPanGrid.ClientWidth - 2 * FGrdZone.ColCount) div FGrdZone.ColCount);

    LUnitWidth  := FPanTop.Width div 3;
    LLabelWidth := LUnitWidth - (LUnitWidth div 4);
    LEditWidth  := LUnitWidth div 4;
    LLabelWidth := LLabelWidth - C_LabelOffset - C_LabelOffset;
    LEditWidth  := LEditWidth -1;
    
    FLblFullSupplyLevel.Width        := LLabelWidth;
    FLblDeadStorageLevel.Width       := LLabelWidth;
    FLblBottomOfReservoirLevel.Width := LLabelWidth;
    FEdtFullSupplyLevel.Width        := LEditWidth;
    FEdtDeadStorageLevel.Width       := LEditWidth;
    FEdtBottomOfReservoirLevel.Width := LEditWidth;

    FLblFullSupplyLevel.Left        := C_LabelOffset;
    FEdtFullSupplyLevel.Left        := FLblFullSupplyLevel.Left + FLblFullSupplyLevel.Width + C_LabelOffset;
    FLblDeadStorageLevel.Left       := FEdtFullSupplyLevel.Left + FEdtFullSupplyLevel.Width + C_LabelOffset;
    FEdtDeadStorageLevel.Left       := FLblDeadStorageLevel.Left + FLblDeadStorageLevel.Width + C_LabelOffset;
    FLblBottomOfReservoirLevel.Left := FEdtDeadStorageLevel.Left + FEdtDeadStorageLevel.Width + C_LabelOffset;
    FEdtBottomOfReservoirLevel.Left := FLblBottomOfReservoirLevel.Left + FLblBottomOfReservoirLevel.Width + C_LabelOffset;

    {FEdtFullSupplyLevel.Left := Trunc(0.25 * FPanTop.ClientWidth);
    FEdtDeadStorageLevel.Left := Trunc(0.25 * FPanBottom.ClientWidth);
    FEdtBottomOfReservoirLevel.Left := Trunc(0.25 * FPanBottom.ClientWidth);

    FEdtFullSupplyLevel.Width := Trunc(0.10 * Self.ClientWidth);
    FEdtDeadStorageLevel.Width := Trunc(0.10 * Self.ClientWidth);
    FEdtBottomOfReservoirLevel.Width := Trunc(0.10 * Self.ClientWidth);
    }
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirZoneElevationsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblFullSupplyLevel.Caption := FAppModules.Language.GetString('LabelText.FullSupplyLevel');
    FLblDeadStorageLevel.Caption := FAppModules.Language.GetString('LabelText.DeadStorageLevel');
    FLblBottomOfReservoirLevel.Caption := FAppModules.Language.GetString('LabelText.BottomOfReservoirLevel');
    FGrdZone.Cells[0,0] := FAppModules.Language.GetString('GridHeading.Zone');

    FChtElevation.BottomAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.Month');
    FChtElevation.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.Elevation');

    FViewType.Caption := FAppModules.language.GetString('TReservoirZoneElevation.ViewAs');
    FViewType.Items.Clear;
    FViewType.Items.Add(FAppModules.language.GetString('TReservoirZoneElevation.Height'));
    FViewType.Items.Add(FAppModules.language.GetString('TReservoirZoneElevation.Volume'));
    FViewType.Items.Add(FAppModules.language.GetString('TReservoirZoneElevation.Percentage'));
    FViewType.Columns := 3;
    FViewType.ItemIndex := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsDialog.SaveState: boolean;
const OPNAME = 'TReservoirZoneElevationsDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsDialog.SetNoOfDrawDownZones(
  const AValue: integer);
const OPNAME = 'TReservoirZoneElevationsDialog.SetNoOfDrawDownZones';
var
  LIndex : Integer;
begin
  try
    FNoOfDrawDownZones := AValue;
    if AValue > 0 then
    begin
      FGrdZone.RowCount := AValue + 1;
      for LIndex := 1 to FGrdZone.RowCount - 1 do
        FGrdZone.Cells[0, LIndex] := Format('%d',[LIndex]);
    end
    else
    begin
      FGrdZone.RowCount := 2;
      for LIndex := 0 to FGrdZone.ColCount - 1 do
        FGrdZone.Cells[LIndex,1] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsDialog.DestroyMemberObjects;
const OPNAME = 'TReservoirZoneElevationsDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FPenaltyArray := nil; // Finalise
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsDialog.RestoreColourState;
const OPNAME = 'TReservoirZoneElevationsDialog.RestoreColourState';
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

procedure TReservoirZoneElevationsDialog.AssignHelpContext;
const OPNAME = 'TReservoirZoneElevationsDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                       HC_ReservoirStorageZones);
    SetControlHelpContext(FEdtFullSupplyLevel,        HC_ReservoirStorageZones);
    SetControlHelpContext(FEdtDeadStorageLevel,       HC_ReservoirStorageZones);
    SetControlHelpContext(FEdtBottomOfReservoirLevel, HC_ReservoirStorageZones);
    SetControlHelpContext(FGrdZone,                   HC_ReservoirStorageZones);
    SetControlHelpContext(VCL.Controls.TControl(FChtElevation),              HC_ReservoirStorageZones);
    SetControlHelpContext(FViewType,                  HC_ReservoirStorageZones);
    SetControlHelpContext(FPanTop,                    HC_ReservoirStorageZones);
    SetControlHelpContext(FPanGrid,                   HC_ReservoirStorageZones);
    SetControlHelpContext(FPanChart,                  HC_ReservoirStorageZones);
    SetControlHelpContext(FPanResAffected,            HC_ReservoirStorageZones);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
end.







