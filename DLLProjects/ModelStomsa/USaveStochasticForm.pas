unit USaveStochasticForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.ExtCtrls,
{$WARN UNIT_PLATFORM OFF}
  VCL.FileCtrl,
{$WARN UNIT_PLATFORM ON}
  VCL.ComCtrls, UHotBtn;

type
  TfmSaveStochastic = class(TForm)
    dirStochastic: TDirectoryListBox;
    Panel1: TPanel;
    btnSave: THotBtn;
    btnExit: THotBtn;
    stbStochastic: TStatusBar;
    procedure btnSaveClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSaveStochastic: TfmSaveStochastic;

implementation

uses
  UDataModule,
  UErrorHandlingOperations;

{$R *.DFM}

procedure TfmSaveStochastic.btnSaveClick(Sender: TObject);
const OPNAME = 'TfmSaveStochastic.btnSaveClick';
begin
  try
    if fmData.DataStorage.First then
    repeat
      //only download the gauges selected in the verify form.
      //if fmData.DataStorage.CurrentRecord.GenerateFlows then
      if fmData.DataStorage.SaveExtraData and fmData.DataStorage.CurrentRecord.FlowsGenerated then
      begin
        stbStochastic.Panels[0].Text := 'Storing stochastic files for gauge ' + fmData.DataStorage.CurrentRecord.FileName;
        stbStochastic.Refresh;
        fmData.DataStorage.WriteStochasticFiles(dirStochastic.Directory + '\' +
                                                fmData.DataStorage.CurrentRecord.FileName);
      end;
    until NOT(fmData.DataStorage.Next);
    Close;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSaveStochastic.btnExitClick(Sender: TObject);
const OPNAME = 'TfmSaveStochastic.btnExitClick';
begin
  try
    Close;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSaveStochastic.FormKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TfmSaveStochastic.FormKeyPress';
begin
  try
    if key = #27 then
      Self.Close;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSaveStochastic.FormActivate(Sender: TObject);
const OPNAME = 'TfmSaveStochastic.FormActivate';
var
  TheDirectory : string;
begin
  try
    GetDir(0,TheDirectory);
    dirStochastic.Directory := TheDirectory;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
