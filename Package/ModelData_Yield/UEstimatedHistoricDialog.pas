//
//
//  UNIT      : Contains TEstimatedHistoricDialog Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 12/07/2007
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UEstimatedHistoricDialog;

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
  TEstimatedHistoricDialog = class(TAbstractScrollablePanel)
  protected
    FGroupBox : TGroupBox;
    FlblHeading : TLabel;
    FlblMin : TLabel;
    FlblMax : TLabel;
    FHistoricGrid: TFieldStringGrid;
    FlblIterativeCalc : TLabel;
    procedure CreateMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property HistoricGrid: TFieldStringGrid read FHistoricGrid;
    property lblMin : TLabel read FlblMin;
    property lblMax : TLabel read FlblMax;
end;
implementation
uses
  SysUtils,
  UHelpContexts,
  UConstants,
  UControlCreationUtilities,
  UErrorHandlingOperations, VCL.Grids, Math;

{ TEstimatedHistoricDialog }

procedure TEstimatedHistoricDialog.CreateMemberObjects;
const OPNAME = 'TEstimatedHistoricDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGroupBox                := TGroupBox.Create ( ControlsOwner );
    FGroupBox.Parent         := ControlsParent;
    FGroupBox.Align          := alTop;
    FGroupBox.Height         := 90;

    FlblHeading              := TLabel.Create(ControlsOwner);
    FlblHeading.Parent       := FGroupBox;
    FlblHeading.Top          := 10;
    FlblHeading.Left         := 10;
    FlblHeading.Font.Style   := [fsUnderline, fsBold];

    FlblMin                  := TLabel.Create(ControlsOwner);
    FlblMin.Parent           := FGroupBox;
    FlblMin.Top              := 30;
    FlblMin.Left             := 10;

    FlblMax                  := TLabel.Create(ControlsOwner);
    FlblMax.Parent           := FGroupBox;
    FlblMax.Top              := 50;
    FlblMax.Left             := 10;

    FlblIterativeCalc        := TLabel.Create(ControlsOwner);
    FlblIterativeCalc.Parent := FGroupBox;
    FlblIterativeCalc.Top    := 70;
    FlblIterativeCalc.Left   := 10;
    FlblIterativeCalc.Font.Style := [fsUnderline, fsBold];

    FHistoricGrid                  := TFieldStringGrid.Create(ControlsOwner, FAppModules);;
    FHistoricGrid.Parent           := ControlsParent;
    //FHistoricGrid.Color          := clBtnFace;
    FHistoricGrid.Align            := alClient;
    FHistoricGrid.Alignment        := taCenter;
    FHistoricGrid.Options          := FHistoricGrid.Options - [goEditing	];
    FHistoricGrid.ColCount         := 3;
    FHistoricGrid.RowCount         := 2;
    FHistoricGrid.FixedCols        := 0;
    FHistoricGrid.FixedRows        := 1;
    FHistoricGrid.Width := (FHistoricGrid.DefaultColWidth*FHistoricGrid.ColCount)+C_ControlOffset;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricDialog.Initialise: boolean;
const OPNAME = 'TEstimatedHistoricDialog.Initialise';
var
  lIndex: integer;
begin
  Result := inherited Initialise;
  try
    for lIndex := 0 to FHistoricGrid.ColCount - 1 do
      FHistoricGrid.ColWidths [ lIndex ] := FHistoricGrid.ColWidths [ lIndex ] + 60;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricDialog.LanguageHasChanged: boolean;
const OPNAME = 'TEstimatedHistoricDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FlblHeading.Caption := FAppModules.Language.GetString('EstimatedHistoricDialog.HystoricFirmYield');
    FlblIterativeCalc.Caption := FAppModules.Language.GetString('EstimatedHistoricDialog.IterativeCalculation');
    FHistoricGrid.Cells[0,0] := FAppModules.Language.GetString('GridHeading.Iteration');
    FHistoricGrid.Cells[1,0] := FAppModules.Language.GetString('GridHeading.TargetDraft');
    FHistoricGrid.Cells[2,0] := FAppModules.Language.GetString('GridHeading.Result');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
