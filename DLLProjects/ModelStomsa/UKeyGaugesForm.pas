unit UKeyGaugesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.CheckLst, UHotBtn, VCL.ExtCtrls, VCL.ComCtrls,UAbstractObject;

type
  TfmKeyGauges = class(TFrame)
    Panel1: TPanel;
    btnSelectAll: THotBtn;
    btnDeselectAll: THotBtn;
    stbGaugeState: TStatusBar;
    Panel2: TPanel;
    clsGauges: TCheckListBox;
    Splitter1: TSplitter;
    edtKeyGauges: TRichEdit;
    procedure clsGaugesClickCheck(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    procedure ShowCount;
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmKeyGauges: TfmKeyGauges;

implementation

uses
  UDataModule,
  UErrorHandlingOperations;

{$R *.DFM}

constructor TfmKeyGauges.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmKeyGauges.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmKeyGauges.AfterConstruction;
const OPNAME = 'TfmKeyGauges.AfterConstruction';
var
  LastItem : Integer;
begin
  inherited;
  try
    //only want to refresh if data has really changed for the key gauges
    if fmData.DataStorage.KeyGaugesHaveChanged then
    begin
      clsGauges.Clear;
      //load up the items and display them
      with clsGauges.Items do
      begin
        if fmData.DataStorage.First then
        begin
          lastItem := clsGauges.Items.Add(ChangeFileExt(fmData.DataStorage.CurrentRecord.Filename,''));
          clsGauges.Checked[LastItem] := fmData.DataStorage.CurrentRecord.KeyGauge;
        end;

        while fmData.DataStorage.Next do
        begin
          lastItem := clsGauges.Items.Add(ChangeFileExt(fmData.DataStorage.CurrentRecord.Filename,''));
          clsGauges.Checked[LastItem] := fmData.DataStorage.CurrentRecord.KeyGauge;
        end;
      end;
      ShowCount;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmKeyGauges.ShowCount;
const OPNAME = 'TfmKeyGauges.ShowCount';
begin
  try
    stbGaugeState.Panels[0].Text := 'Gauges Selected: ' + IntToStr(fmData.DataStorage.KeyGaugeCount);
    fmData.DataStorage.KeyGaugesHaveChanged := false;
    if(fmData.DataStorage.KeyGaugeCount > 200) then
    begin
      ShowMessage('There are more than 200 key gauges selected, STOMSA is not able to create the PARAM.DAT.');
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmKeyGauges.clsGaugesClickCheck(Sender: TObject);
const OPNAME = 'TfmKeyGauges.clsGaugesClickCheck';
begin
  try
    //select or deselect the relevant item
    if clsGauges.ItemIndex <> -1 then
    begin
      fmData.DataStorage.GotoIndex(clsGauges.ItemIndex + 1);
      with fmData.DataStorage.CurrentRecord do
        KeyGauge := clsGauges.Checked[clsGauges.ItemIndex];
      fmData.DataStorage.PARAMFileCreated := False;
      //DSR fmData.DataStorage.UpdateCurrentRecord;
    end;
    ShowCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmKeyGauges.btnSelectAllClick(Sender: TObject);
const OPNAME = 'TfmKeyGauges.btnSelectAllClick';
var
  loop : integer;
begin
  try
    for loop := 1 to fmData.DataStorage.IncFileCount do
    begin
      clsGauges.Checked[Loop-1] := True;
      fmData.DataStorage.GotoIndex(loop);
      with fmData.DataStorage.CurrentRecord do
        KeyGauge := true;
      fmData.DataStorage.PARAMFileCreated := False;
      //DSR fmData.DataStorage.UpdateCurrentRecord;
    end;
    ShowCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmKeyGauges.btnDeselectAllClick(Sender: TObject);
const OPNAME = 'TfmKeyGauges.btnDeselectAllClick';
var
  loop : integer;
begin
  try
    for loop := 1 to fmData.DataStorage.IncFileCount do
    begin
      clsGauges.Checked[Loop-1] := False;
      fmData.DataStorage.GotoIndex(loop);
      with fmData.DataStorage.CurrentRecord do
        KeyGauge := False;
      fmData.DataStorage.PARAMFileCreated := False;
      //DSR fmData.DataStorage.UpdateCurrentRecord;
    end;
    ShowCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
