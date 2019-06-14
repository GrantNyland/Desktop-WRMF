unit UGenerateForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.ExtCtrls, VCL.Grids, VCL.ActnList,  VCL.ComCtrls, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
  VCLTee.Chart, VCLTee.TeeShape, VCL.Samples.Spin, VCL.ImgList, VCL.CheckLst,
  UStomsaData, UStomsaGlobalData, UMarkStringGrid, UHotBtn, UAbstractObject;

type
  TfmGenerate = class(TFrame)
    stbVerify: TStatusBar;
    Panel1: TPanel;
    pnlVerifyMenu: TPanel;
    btnGenerate: THotBtn;     
    btnSelectAll: THotBtn;
    btnDeselectAll: THotBtn;
    Panel4: TPanel;
    chkSaveData: TCheckBox;
    Label1: TLabel;
    spnYearCount: TSpinEdit;
    prgFlowGenerate: TProgressBar;
    btnStats: THotBtn;
    btnStochasticSave: THotBtn;
    Panel12: TPanel;
    grdGenerateGauges: TMarkStringGrid;
    procedure btnStatsClick(Sender: TObject);
    procedure btnCorrClick(Sender: TObject);
    procedure chkSaveDataClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnStochasticSaveClick(Sender: TObject);
    procedure grdGenerateGaugesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
  protected
    FAppModules: TAppModules;
    NRSEQ        : arrayT1I;  //Moved here to avoid stack overflow access violation
    NYRS         : ArrayT2I;
    XH           : array12_100F;
    FlowD        : array101_400_12F;  //Y2K
    procedure GetStats;
    procedure SetGridTitle(Loop : Integer);
    procedure UnlockControls(action : Boolean);
    procedure SetGridValues(Sender : TObject);
    procedure GenerateThreadEnd(Sender : TObject);
    procedure CorrelateThreadEnd(Sender : TObject);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmGenerate: TfmGenerate;

implementation


uses
  UDataModule,
  UGraphUnit,
  UThreadFlowGenerate,
  UThreadCorrelate,
  USaveStochasticForm,
  UCorrelateForm,
  UStomsaMainForm,
  UStomsaDLLManager,
  UStomsaModelManager,
  UErrorHandlingOperations;


var
  FlowGenerateThread : TFlowGenerate;
  CorrelateThread    : TCorrelate;
  FlowsToGenerate    : Boolean;

{$R *.DFM}

constructor TfmGenerate.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmGenerate.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmGenerate.AfterConstruction;
const OPNAME = 'TfmGenerate.AfterConstruction';
begin
  inherited;
  try
    with grdGenerateGauges do
    begin
      Cells[1,0] := 'Gauge';
      Cells[2,0] := 'Years Generated';
      Cells[3,0] := 'Stats Calculated';
      ColWidths[0] := 30;
    end;

    if fmData.DataStorage.StochasticDataHasChanged then
    begin
      SetGridValues(Self);
      if fmData.DataStorage.Automatic then
      begin
        btnSelectAllClick(self);
        btnGenerateClick(self);
      end;
      fmData.DataStorage.StochasticDataHasChanged := false;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.BeforeDestruction;
const OPNAME = 'TfmGenerate.BeforeDestruction';
begin
  inherited;
  try
    if(FlowGenerateThread <> nil) then
      FlowGenerateThread.Terminate;
    if(CorrelateThread <> nil) then
      CorrelateThread.Terminate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.btnStochasticSaveClick(Sender: TObject);
const OPNAME = 'TfmGenerate.btnStochasticSaveClick';
begin
  try
    fmSaveStochastic := TfmSaveStochastic.Create(Self);
    try
      fmSaveStochastic.ShowModal;
    finally
      fmSaveStochastic.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.UnlockControls(action : Boolean);
const OPNAME = 'TfmGenerate.UnlockControls';
begin
  try
    btnSelectAll.Enabled := action;
    btnDeselectAll.Enabled := action;
    btnGenerate.Enabled := action;
    btnStats.Enabled := action;
    btnStochasticSave.Enabled := action;
    grdGenerateGauges.Enabled := action;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.GenerateThreadEnd(Sender : TObject);
const OPNAME = 'TfmGenerate.GenerateThreadEnd';
begin
  try
    SetGridValues(self);
    if FlowsToGenerate then
      btnGenerateClick(self)
    else if fmData.DataStorage.Automatic then
    begin
      UnlockControls(true);
      btnStatsClick(self);
    end
    else
      UnlockControls(true);
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE GenerateThreadEnd

procedure TfmGenerate.SetGridTitle(Loop : Integer);
const OPNAME = 'TfmGenerate.SetGridTitle';
begin
  try
    FlowsToGenerate := false;
    with fmData.DataStorage.CurrentRecord do
    begin
      grdGenerateGauges.Cells[1,Loop] := Format('%-12S',[ChangeFileExt(FileName,'')]);
      grdGenerateGauges.Cells[2,Loop] := Format('%4S',[IntToStr(GeneratedYearCount)]);
      if StatisticsCalculated then
      begin
        grdGenerateGauges.Cells[3,Loop] := 'X';
        if GenerateFlows then
        begin
          grdGenerateGauges.SetColour(0,Loop,clBlue);
          FlowsToGenerate := True;
        end
        else
          grdGenerateGauges.SetColour(0,Loop,grdGenerateGauges.FixedColor);
      end
      else
      begin
        grdGenerateGauges.Cells[3,Loop] := '';
        if GenerateFlows then
        begin
          grdGenerateGauges.SetColour(0,Loop,clBlue);
          FlowsToGenerate := True;
        end
        else
          grdGenerateGauges.SetColour(0,Loop,grdGenerateGauges.FixedColor);
      end;

      if RecordLength > spnYearCount.Value then
        spnYearCount.Value := RecordLength;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.SetGridValues;
const OPNAME = 'TfmGenerate.SetGridValues';
Var
  Loop : Integer;
begin
  try
    stbVerify.Panels[0].Text := '';
    chkSaveData.Checked := fmData.DataStorage.SaveExtraData;

    grdGenerateGauges.RowCount := fmData.DataStorage.IncFileCount + 1;

    Loop := 0;
    if fmData.DataStorage.First then
    repeat
      Inc(Loop);
      SetGridTitle(Loop);
    until NOT(fmData.DataStorage.Next);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.GetStats;
const OPNAME = 'TfmGenerate.GetStats';
var
  Loop, Loop2,
  Loop3        : Integer;
begin
  try
    with fmData.DataStorage.CurrentRecord do
    begin
      stbVerify.Panels[0].Text := 'Generating statistics for ' + FileName;
      stbVerify.Refresh;

      NYRS[1] := RecordLength;//Number of historical years
//      NYRS[2] := GeneratedYearCount;//Number of stochastic years
      NYRS[2] := RecordLength; // Make the same as historic for testing purposes

//          to use for the sumulated statistics - deafalt should be the same
//          as the historical for the gauge or if that is mode than the
//          simulate, the simulated should be used
      NRSEQ[1] :=41;

      for Loop := 1 to 100 do//Load monthly historical data
      begin
        if Loop <= RecordLength then
        begin
          for Loop2 := 1 to 12 do
            XH[Loop2,Loop] := MonthlyRawIncData[Loop,Loop2];
        end
        else
        begin
          for Loop2 := 1 to 12 do
            XH[Loop2,Loop] := 0.0;
        end;
      end;//for Loop

      for Loop := 1 to 101 do
        for Loop2 := 1 to 100 do
          for Loop3 := 1 to 12 do
            FlowD[Loop,Loop2,Loop3] := 0.0;
      SequenceCount:=101;
      for Loop := 1 to 101 do      // The stochastic tests are only done on the first 101 sequences
        for Loop2 := 1 to RecordLength do
          for Loop3 := 1 to 12 do
            FlowD[Loop,Loop2,Loop3] := GeneratedFlows[Loop,Loop2,Loop3];

      // Call the DLL.
      TStomsaModelManager(FAppModules.Model).DLLManager.FLOWSTAT(
              @NRSEQ[1],
              @LWD[1],
              @NYRS[1],
              @LPD[1],
              @RILHD[1],
              @RILSD[1,1],
              @RJLHD[1],
              @RJLSD[1,1],
              @XH[1,1],
              @FLOWD[1,1,1],
              @APAD[1,1],
              @GPAD[1,1],
              @HGPAD[1],
              @TSAMD[1],
              @TSTMD[1],
              @TSKMD[1],
              @TSASD[1],
              @TSTSD[1],
              @TSKSD[1],
              @TSCVD[1],
              @TSTVD[1],
              @TSKVD[1],
              @GJAD[1],
              @PGD[1],
              @HSAD[1],
              @PSD[1],
              @X1D[1],
              @PMAD[1,1],
              @X2D[1],
              @PMSD[1,1],
              @HSKD[1],
              @HJAKD[1],
              @RELD[1],
              @AMIND[1],
              @WMIND[1,1],
              @QRHD[1],
              @QRSD[1,1],
              @RSHD[1],
              @RSSD[1,1],
              @MNGPAD[1],
              @MDXJD[1]);

//     FLOWSTAT(NRSEQ, LWD, NYRS, LPD, RILHD, RILSD, RJLHD, RJLSD, XH, FLOWD, APAD,
//              GPAD, HGPAD, TSAMD, TSTMD, TSKMD, TSASD, TSTSD, TSKSD, TSCVD,
//              TSTVD, TSKVD, GJAD, PGD, HSAD, PSD, X1D, PMAD, X2D, PMSD, HSKD,
//              HJAKD, RELD, AMIND, WMIND, QRHD, QRSD, RSHD, RSSD, MNGPAD, MDXJD);

      StatisticsCalculated := true;
      //DSR fmData.DataStorage.UpdateCurrentRecord;

      stbVerify.Panels[0].Text := 'Statistics generated for ' + FileName;
      stbVerify.Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.btnStatsClick(Sender: TObject);
const OPNAME = 'TfmGenerate.btnStatsClick';
begin
  try
    if fmData.DataStorage.First then
      if fmData.DataStorage.CurrentRecord.FlowsGenerated then
        GetStats;
    while (fmData.DataStorage.Next) do
      if fmData.DataStorage.CurrentRecord.FlowsGenerated then
        GetStats;
    stbVerify.Panels[0].Text := 'Finished generating statistics';
    stbVerify.Refresh;
    SetGridValues(Self);
    if fmData.DataStorage.Automatic then
    begin
      if fmCorrelate = nil then
      begin
        fmCorrelate := TfmCorrelate.Create(Self,FAppModules);
      end;
      fmStomsaMainForm.AddFrame(fmCorrelate);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.CorrelateThreadEnd(Sender : TObject);
const OPNAME = 'TfmGenerate.CorrelateThreadEnd';
begin
  try
    UnlockControls(true);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.btnCorrClick(Sender: TObject);
const OPNAME = 'TfmGenerate.btnCorrClick';
begin
  try
    UnlockControls(false);
    CorrelateThread := TCorrelate.Create(FAppModules);
    CorrelateThread.OnTerminate := CorrelateThreadEnd;
    fmData.DataStorage.Automatic := false;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.chkSaveDataClick(Sender: TObject);
//Adjust the flag in the object to determine if we will save all this additional data
const OPNAME = 'TfmGenerate.chkSaveDataClick';
begin
  try
    fmData.DataStorage.SaveExtraData := chkSaveData.Checked;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.btnSelectAllClick(Sender: TObject);
const OPNAME = 'TfmGenerate.btnSelectAllClick';
var
  loop : integer;
begin
  try
    for loop := 1 to (grdGenerateGauges.RowCount - 1) do
    begin
      grdGenerateGauges.SetColour(0,Loop,clBlue);
      fmData.DataStorage.GotoIndex(Loop);
      with fmData.DataStorage.CurrentRecord do
        GenerateFlows := true;
      //DSR fmData.DataStorage.UpdateCurrentRecord;
    end;
    grdGenerateGauges.Refresh;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.btnDeselectAllClick(Sender: TObject);
const OPNAME = 'TfmGenerate.btnDeselectAllClick';
var
  Loop : integer;
begin
  try
    for Loop := 1 to (grdGenerateGauges.RowCount - 1) do
    begin
      grdGenerateGauges.SetColour(0,Loop,grdGenerateGauges.FixedColor);
      fmData.DataStorage.GotoIndex(Loop);
      with fmData.DataStorage.CurrentRecord do
        GenerateFlows := false;
      //DSR fmData.DataStorage.UpdateCurrentRecord;
    end;
    grdGenerateGauges.Refresh;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.btnGenerateClick(Sender: TObject);
const OPNAME = 'TfmGenerate.btnGenerateClick';
var
  FlowsOutstanding : Integer;
begin
  try
    UnlockControls(false);
    FlowsOutstanding := 0;
    if fmData.DataStorage.First then
    repeat
      if fmData.DataStorage.CurrentRecord.GenerateFlows then
        Inc(FlowsOutstanding);
    until NOT(fmData.DataStorage.Next);
    stbVerify.Panels[0].Text := IntToStr(FlowsOutstanding) + ' Gauges remaining for generation';

    fmData.DataStorage.First;
    FlowGenerateThread := TFlowGenerate.Create(spnYearCount.Value, FAppModules);
    FlowGenerateThread.OnTerminate := GenerateThreadEnd;
    //FlowGenerateThread.Resume; ....depricated....
    FlowGenerateThread.Start;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGenerate.grdGenerateGaugesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TfmGenerate.grdGenerateGaugesSelectCell';
begin
  try
    CanSelect := false;
    if ARow > 0 then
    begin
      fmData.DataStorage.GotoIndex(ARow);
      if fmData.DataStorage.CurrentRecord.GenerateFlows then
      begin
        with fmData.DataStorage.CurrentRecord do
          GenerateFlows := false;
        grdGenerateGauges.SetColour(0,ARow,grdGenerateGauges.FixedColor);
      end
      else
      begin
        with fmData.DataStorage.CurrentRecord do
          GenerateFlows := True;
        grdGenerateGauges.SetColour(0,ARow,clBlue);
      end;

      grdGenerateGauges.Refresh;
      //DSR fmData.DataStorage.UpdateCurrentRecord;
      CanSelect := true
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
