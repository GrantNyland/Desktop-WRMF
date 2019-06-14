unit UCorrelateForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.Grids, UMarkStringGrid, UHotBtn, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCLTee.Chart,UAbstractObject;

type
  TfmCorrelate = class(TFrame)
    Panel2: TPanel;
    pnlCorrelateMenu: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnCorr: THotBtn;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    grdCompare: TMarkStringGrid;
    stbCorrelate: TStatusBar;
    procedure grdCompareMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnCorrClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    procedure SetGridValues(Sender : TObject);
    procedure CorrelateThreadEnd(Sender : TObject);
    procedure SetGridColour(AColumn, ARow: Integer);
    procedure SetGridTitle(Loop : Integer);
  public
    { Public declarations }
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmCorrelate: TfmCorrelate;

implementation

uses
  UDataModule,
  UThreadCorrelate,
  UStomsaMainForm,
  UErrorHandlingOperations;

var
  CorrelateThread    : TCorrelate;

{$R *.DFM}

constructor TfmCorrelate.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmCorrelate.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmCorrelate.AfterConstruction;
const OPNAME = 'TfmCorrelate.AfterConstruction';
begin
  inherited;
  try
    SetGridValues(Self);
    if fmData.DataStorage.Automatic then
    begin
      grdCompareMouseDown(Self,mbLeft,[],0,0);
      btnCorrClick(self);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmCorrelate.SetGridTitle(Loop : Integer);
const OPNAME = 'TfmCorrelate.SetGridTitle';
begin
  try
    with fmData.DataStorage.CurrentRecord do
    begin
      grdCompare.Cells[0,Loop] := ChangeFileExt(FileName,'');
      grdCompare.Cells[Loop,0] := ChangeFileExt(FileName,'');
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmCorrelate.SetGridValues;
const OPNAME = 'TfmCorrelate.SetGridValues';
Var
  Loop : Integer;
  Loop1, Loop2 : Integer;
begin
  try
    grdCompare.RowCount := fmData.DataStorage.IncFileCount + 1;
    grdCompare.ColCount := fmData.DataStorage.IncFileCount + 1;

    for Loop1 := 1 to grdCompare.ColCount do
      for Loop2 := Loop1 to grdCompare.RowCount do
        grdCompare.SetColour(Loop1,Loop2,clGray);

    Loop := 0;
    if fmData.DataStorage.First then
    repeat
      Inc(Loop);
      SetGridTitle(Loop);
    until NOT(fmData.DataStorage.Next);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmCorrelate.SetGridColour(AColumn, ARow : Integer);
const OPNAME = 'TfmCorrelate.SetGridColour';
var
  CurrentColour : TColor;
begin
  try
    if AColumn <> ARow then
    begin
      CurrentColour := grdCompare.GetColour(AColumn,ARow);
      if CurrentColour = clBlack then
        CurrentColour := grdCompare.Color
      else
        CurrentColour := clBlack;
      grdCompare.SetColour(AColumn,ARow,CurrentColour);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmCorrelate.grdCompareMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TfmCorrelate.grdCompareMouseDown';
var
  Loop, Loop2, TheCol, TheRow : Integer;
begin
  try
    with grdCompare do
    begin
      MouseToCell(X,Y,TheCol,TheRow);
      if (TheRow = 0) and (TheCol <> 0) then
        for Loop := 1 to (TheCol) do
          SetGridColour(TheCol,Loop)
      else if (TheCol = 0) and (TheRow <> 0) then
        for Loop := (TheRow) to (ColCount - 1) do
          SetGridColour(Loop,TheRow)
      else if (TheCol = 0) and (TheRow = 0) then
        for Loop := 1 to (ColCount - 1) do
          for Loop2 := 1 to (Loop) do
            SetGridColour(Loop,Loop2)
      else if (TheCol > TheRow) then
        SetGridColour(TheCol,TheRow);

      Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmCorrelate.CorrelateThreadEnd(Sender : TObject);
const OPNAME = 'TfmCorrelate.CorrelateThreadEnd';
begin
  try
    btnCorr.Enabled := true;
    fmStomsaMainForm.imgCorrelated.Visible := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmCorrelate.btnCorrClick(Sender: TObject);
const OPNAME = 'TfmCorrelate.btnCorrClick';
begin
  try
    btnCorr.Enabled := false;
    fmStomsaMainForm.imgCorrelated.Visible := False;
    CorrelateThread := TCorrelate.Create(FAppModules);
    CorrelateThread.OnTerminate := CorrelateThreadEnd;
    fmData.DataStorage.Automatic := false;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
