{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreEvaporationDialog           *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/11/16                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreEvaporationDialog;

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

  TYMDemandCentreEvaporationDialog = class(TAbstractScrollablePanel)
  protected
    FGrdEvaporation : TFieldStringGrid;
    FChtEvaporation : TAbstractChart;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DrawTotalLines(ASender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
  public
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;

    property GrdEvaporation : TFieldStringGrid read FGrdEvaporation;
    property ChtEvaporation : TAbstractChart read FChtEvaporation;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{ TYMDemandCentreEvaporationDialog }

procedure TYMDemandCentreEvaporationDialog.AssignHelpContext;
const OPNAME = 'TYMDemandCentreEvaporationDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,             HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(FGrdEvaporation,  HC_ReturnFlowsFromLargeUrbanCentres);
    SetControlHelpContext(VCL.Controls.TControl(FChtEvaporation),  HC_ReturnFlowsFromLargeUrbanCentres);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYMDemandCentreEvaporationDialog.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreEvaporationDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    // Create

    FGrdEvaporation := TFieldStringGrid.Create(Self,FAppModules);
    FChtEvaporation := TAbstractChart.Create(Self, FAppModules);
    FGrdEvaporation.Visible := True;
    FGrdEvaporation.OnDrawCell := DrawTotalLines;
    // Set Parent

    FGrdEvaporation.Parent := ControlsParent;
    FChtEvaporation.Parent := ControlsParent;

    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationDialog.DrawTotalLines(ASender: TObject;
  ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
const OPNAME = 'TYMDemandCentreEvaporationDialog.DrawTotalLines';
begin
  try
    if (ARow = (GrdEvaporation.RowCount - 1)) then
    begin
      GrdEvaporation.Canvas.Pen.Color := clBlack;
      GrdEvaporation.Canvas.Pen.Style := psSolid;
      GrdEvaporation.Canvas.MoveTo(ARect.Left,  ARect.Top - 1);
      GrdEvaporation.Canvas.LineTo(ARect.Right, ARect.Top - 1);
      GrdEvaporation.Canvas.MoveTo(ARect.Left,  ARect.Bottom);
      GrdEvaporation.Canvas.LineTo(ARect.Right, ARect.Bottom);
      GrdEvaporation.Canvas.MoveTo(ARect.Left,  ARect.Bottom - 2);
      GrdEvaporation.Canvas.LineTo(ARect.Right, ARect.Bottom - 2);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYMDemandCentreEvaporationDialog.Initialise: boolean;
const OPNAME = 'TYMDemandCentreEvaporationDialog.Initialise';
begin
  Result := inherited Initialise;

  try
    FGrdEvaporation.Reset;
    FChtEvaporation.Initialise;

    // Set default Grid properties

    FGrdEvaporation.RowCount := 14;
    FGrdEvaporation.ColCount := 2;
    FGrdEvaporation.FixedRows := 1;
    FGrdEvaporation.FixedCols := 1;
    FGrdEvaporation.DefaultRowHeight := 18;

    Result := True;
    // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreEvaporationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FGrdEvaporation) then
    begin
      FGrdEvaporation.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
      FGrdEvaporation.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.SymonsPanEvaporation');
      FGrdEvaporation.Cells[0,13]  := FAppModules.Language.GetString('GridHeading.Total');
    end;
    if Assigned(FChtEvaporation) then
    begin
      FChtEvaporation.LeftAxis.Title.Caption := FAppModules.Language.GetString('ChartTitle.Evaporation');
      FChtEvaporation.BottomAxis.Title.Caption := FAppModules.Language.GetString('ChartTitle.Month');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationDialog.Resize;
const OPNAME = 'TYMDemandCentreEvaporationDialog.Resize';
var
  LClientWidth : integer;
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Check that construction is complete before laying out controls.

    if Assigned(FChtEvaporation) then
    begin

      LClientWidth := Self.ClientWidth;

      // Position the string Grid

      FGrdEvaporation.Left := C_ControlBorder;
      FGrdEvaporation.Top := C_ControlBorder;

      // Resize the Grid

      FGrdEvaporation.Width  := LClientWidth div 3 - 2 * C_ControlBorder;
      FGrdEvaporation.Height := (FGrdEvaporation.DefaultRowHeight + 2) * FGrdEvaporation.RowCount;

      // Set the Grid column widths
      if Populated then
      begin
        if (FGrdEvaporation.ColCount >= 1) then
          FGrdEvaporation.ColWidths[0] := FGrdEvaporation.Width div 3 - 2 * FGrdEvaporation.ColCount;
        if (FGrdEvaporation.ColCount >= 2) then
          FGrdEvaporation.ColWidths[1] := FGrdEvaporation.Width - FGrdEvaporation.ColWidths[0] - 3 * FGrdEvaporation.ColCount;
      end;
      // Position the Chart

      VCL.Controls.TControl(FChtEvaporation).Left := FGrdEvaporation.Left + FGrdEvaporation.Width + 2 * C_ControlBorder;
      VCL.Controls.TControl(FChtEvaporation).Top := C_ControlBorder;

      // Resize the Chart

      FChtEvaporation.Width := LClientWidth - FGrdEvaporation.Width - 5 * C_ControlBorder;
      FChtEvaporation.Height := Self.ClientHeight - 6 * C_ControlBorder;

      if FScrollBox.VertScrollBar.IsScrollBarVisible then
        FChtEvaporation.Width := FChtEvaporation.Width - 3 * C_ControlBorder;

    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationDialog.SaveState: boolean;
const OPNAME = 'TYMDemandCentreEvaporationDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.