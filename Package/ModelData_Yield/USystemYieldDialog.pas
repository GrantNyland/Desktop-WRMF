//
//
//  UNIT      : Contains the class TSystemYieldDialog.
//  AUTHOR    : Sam Dhlamini(RIVIA)
//  DATE      : 2005/02/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit USystemYieldDialog;

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

  TSystemYieldDialog = class(TAbstractScrollablePanel)
  protected

    FGroupBox : TGroupBox;

    FlblYield : TLabel;
    FlblDeficit : TLabel;
    FlblTargetDraft : TLabel;
    FlblNB : TLabel;
    FHistoricGrid : TFieldStringGrid;

    FStochasticGrid         : TFieldStringGrid;
    FStochasticLongTermGrid : TFieldStringGrid;

    FlblStochastic : TLabel;
    FlblNumberOfSeqAnalysed : TLabel;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property GroupBox : TGroupBox read FGroupBox;
    property HistoricGrid: TFieldStringGrid read FHistoricGrid;
    property StochasticGrid: TFieldStringGrid read FStochasticGrid;
    property StochasticLongTermGrid: TFieldStringGrid read FStochasticLongTermGrid;
    property lblStochastic : TLabel read FlblStochastic;
    property lblNumberOfSeqAnalysed : TLabel read FlblNumberOfSeqAnalysed;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UConstants,
  UControlCreationUtilities,
  UErrorHandlingOperations, VCL.Grids, Math;

{ TSystemYieldDialog }

procedure TSystemYieldDialog.CreateMemberObjects;
const OPNAME = 'TSystemYieldDialog.CreateMemberObjects';
begin
  inherited;
  try
    FGroupBox               := TGroupBox.Create ( ControlsOwner );
    FlblYield               := TLabel.Create ( ControlsOwner );
    FlblDeficit             := TLabel.Create ( ControlsOwner );
    FlblTargetDraft         := TLabel.Create ( ControlsOwner );
    FlblNB                  := TLabel.Create ( ControlsOwner );

    FlblStochastic          := TLabel.Create ( ControlsOwner );
    FlblNumberOfSeqAnalysed := TLabel.Create ( ControlsOwner );

    FHistoricGrid           := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FStochasticGrid         := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FStochasticLongTermGrid := TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FHistoricGrid.DisabledColor           := clBtnFace;
    FStochasticGrid.DisabledColor         := clBtnFace;
    FStochasticLongTermGrid.DisabledColor := clBtnFace;

    FGroupBox.Parent                 := ControlsParent;

    FlblStochastic.Parent            := ControlsParent;
    FlblNumberOfSeqAnalysed.Parent   := ControlsParent;

    FlblYield.Parent                 := FGroupBox;
    FlblDeficit.Parent               := FGroupBox;
    FlblTargetDraft.Parent           := FGroupBox;
    FlblNB.Parent                    := FGroupBox;

    FHistoricGrid.Parent             := ControlsParent;

    FStochasticGrid.Parent           := ControlsParent;

    FStochasticLongTermGrid.Parent   := ControlsParent;

    FGroupBox.Visible                := False;

    FHistoricGrid.Visible            := False;
    FStochasticGrid.Visible          := False;
    FStochasticLongTermGrid.Visible  := False;

    FlblStochastic.Visible           := False;
    FlblNumberOfSeqAnalysed.Visible  := False;
    
    FHistoricGrid.RowCount           := 2; { come back }
    FStochasticGrid.RowCount         := 0;  { come back }

    FHistoricGrid.ColCount           := 3;
    FStochasticGrid.ColCount         := 0;

    FStochasticLongTermGrid.ColCount := 4;

    FHistoricGrid.FixedRows           := 1;
    FStochasticLongTermGrid.FixedRows := 1;
    FStochasticLongTermGrid.FixedCols := 0;
    FHistoricGrid.FixedCols           := 0;
    FStochasticGrid.Enabled           := (FAppModules.User.UserRights in CUR_EditData) and
                                         (FAppModules.StudyArea <> nil) and
                                         (not (FAppModules.StudyArea.ScenarioLocked));
    FHistoricGrid.Enabled             := (FAppModules.User.UserRights in CUR_EditData) and
                                         (FAppModules.StudyArea <> nil) and
                                         (not (FAppModules.StudyArea.ScenarioLocked));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldDialog.DestroyMemberObjects;
const OPNAME = 'TSystemYieldDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldDialog.Initialise: boolean;
const OPNAME = 'TSystemYieldDialog.Initialise';
var
  lIndex: integer;
begin
  Result := inherited Initialise;
  try

    FGroupBox.Caption := '';
    FlblYield.WordWrap := True;
    FlblDeficit.WordWrap := True;
    FlblTargetDraft.WordWrap := True;

    for lIndex := 0 to FStochasticLongTermGrid.ColCount - 1 do
      FStochasticLongTermGrid.ColWidths [ lIndex ] := FStochasticLongTermGrid.ColWidths [ lIndex ] + 60;

    for lIndex := 0 to FHistoricGrid.ColCount - 1 do
      FHistoricGrid.ColWidths [ lIndex ] := FHistoricGrid.ColWidths [ lIndex ] + 60;

//    FlblYield.Caption := 'Yield (Million m3/a)';
//    FlblDeficit.Caption := 'Deficit (Proportion)';
//    FlblTargetDraft.Caption := 'TD (Million m3/a)';

    FlblNB.Font.Style := [ fsBold ];
    FlblNB.Caption := FAppModules.Language.GetString ( 'Message.NBFormula');

    FlblStochastic.Caption := '';
    FlblStochastic.Font.Style := [ fsUnderline, fsBold ];

    FlblYield.Top := 20;
    FlblDeficit.Top := FlblYield.Top;
    FlblTargetDraft.Top := FlblYield.Top;

    FlblStochastic.Left := 20;
    FlblNumberOfSeqAnalysed.Left := FlblStochastic.Left;

    FlblTargetDraft.Left := 10;
    FlblDeficit.Left := FlblTargetDraft.Width + 50;
    FlblYield.Left := FlblTargetDraft.Left + FlblDeficit.Left + 60;
    FlblNB.Left := FlblTargetDraft.Left;

    FGroupBox.Top := 50;
    FGroupBox.Left := 50;
    FStochasticLongTermGrid.RowCount := 2;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldDialog.Resize;
const OPNAME = 'TSystemYieldDialog.Resize';
var
  lIndex: integer;
begin
  inherited Resize;
  try
//    FGroupBox.Width := ClientHeight div 2;
//    FGroupBox.Height := ( ClientHeight div 2 ) + 100;
//    FHistoricGrid.Height := FGroupBox.Height - 100;
//    FHistoricGrid.Width := FGroupBox.Width - 10;
    FHistoricGrid.Top := 10;
    FHistoricGrid.Left := 10;

    FlblStochastic.Top := 20;

    FlblNumberOfSeqAnalysed.Top := FlblStochastic.Top + FlblStochastic.Height + 10;

    FStochasticLongTermGrid.Top := FlblNumberOfSeqAnalysed.Top + FlblNumberOfSeqAnalysed.Height + 10;
    FStochasticLongTermGrid.Left := FlblStochastic.Left;

    FStochasticLongTermGrid.Height := ( FStochasticLongTermGrid.RowCount *
                                      FStochasticLongTermGrid.RowHeights [ 0 ] ) + 20;

    FStochasticLongTermGrid.width := ( FStochasticLongTermGrid.ColWidths[0] * FStochasticLongTermGrid.ColCount ) + 10;
    FStochasticLongTermGrid.Alignment := taCenter;

    FHistoricGrid.Alignment := taCenter;
    FHistoricGrid.Height := ( FHistoricGrid.RowCount * FHistoricGrid.RowHeights [0] ) + 20;
    FHistoricGrid.Width := ( FHistoricGrid.ColWidths[0] * FHistoricGrid.ColCount ) + 10;

    FlblNB.Top := ( FHistoricGrid.Height ) + 60;

    for lIndex := 0 to FStochasticLongTermGrid.RowCount do
      FStochasticLongTermGrid.IsColumnEnabled [ lIndex ] := False;

    for lIndex := 0 to FHistoricGrid.RowCount do
      FHistoricGrid.IsColumnEnabled [ lIndex ] := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSystemYieldDialog.LanguageHasChanged';
begin
  Result := True;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldDialog.AssignHelpContext;
const OPNAME = 'TSystemYieldDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
