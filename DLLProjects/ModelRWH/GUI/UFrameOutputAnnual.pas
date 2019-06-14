unit UFrameOutputAnnual;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  VCLTee.TeEngine,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.Chart,
  UAbstractObject,
  CheckCombo,
  UColourButtons,
  URWHDataObject,
  UOutputAnnualFileAgent, VclTee.TeeGDIPlus;
type
  TfrmOutputAnnual = class(TFrame)
    Splitter1: TSplitter;
    pnlTop: TPanel;
    strgrdData: TStringGrid;
    strgrdTotals: TStringGrid;
    pnlOptions: TPanel;
    lbTankVolume: TLabel;
    cmbboxRoofSize: TCheckedComboBox;
    btnPrint: TColourBitBtn;
    edtMinX: TEdit;
    lblMinx: TLabel;
    chartData: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    Series6: TLineSeries;
    Series7: TLineSeries;
    Series8: TLineSeries;
    Series9: TLineSeries;
    Series10: TLineSeries;
    lblNote: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure cmbboxRoofSizeChange(Sender: TObject);
    procedure edtMinXExit(Sender: TObject);
    procedure chartDataDblClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
  private
    { Private declarations }
    FOutputAnnualFileAgent : TOutputAnnualFileAgent;
    procedure PopulateDialog;
    procedure ClearDialog;
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function AfterInitialise : boolean;
    function Finalise   : boolean;
    function LanguageHasChanged : boolean;
  end;

var
  frmOutputAnnual : TfrmOutputAnnual;

implementation

{$R *.dfm}

uses
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TfrmOutputAnnual }

procedure TfrmOutputAnnual.AfterConstruction;
const OPNAME = 'TfrmOutputAnnual.AfterConstruction';
begin
  inherited;
  try
    FOutputAnnualFileAgent := TOutputAnnualFileAgent.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.BeforeDestruction;
const OPNAME = 'TfrmOutputAnnual.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FOutputAnnualFileAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputAnnual.Initialise: boolean;
const OPNAME = 'TfrmOutputAnnual.Initialise';
begin
  Result := False;
  try
    FOutputAnnualFileAgent.Initialise;
    ClearDialog;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputAnnual.Finalise: boolean;
const OPNAME = 'TfrmOutputAnnual.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputAnnual.AfterInitialise: boolean;
const OPNAME = 'TfrmOutputAnnual.AfterInitialise';
begin
  Result := False;
  try
    ClearDialog;
    PopulateDialog;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.ClearDialog;
const OPNAME = 'TfrmOutputAnnual.ClearDialog';
begin
  try
    cmbboxRoofSize.Items.Clear;
    edtMinX.Text := '0';
    strgrdData.RowCount := 2;
    strgrdData.Rows[1].Clear;
    strgrdTotals.Rows[0].Clear;
    strgrdData.Cells[0,0] := 'Reliability';
    Series1.Clear;
    Series2.Clear;
    Series3.Clear;
    Series4.Clear;
    Series5.Clear;
    Series6.Clear;
    Series7.Clear;
    Series8.Clear;
    Series9.Clear;
    Series10.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputAnnual.LanguageHasChanged: boolean;
const OPNAME = 'TfrmOutputAnnual.LanguageHasChanged';
begin
  Result := False;
  try
    lbTankVolume.Caption  := lbTankVolume.Caption + '('+VolumeUnitName+')';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.FrameResize(Sender: TObject);
const OPNAME = 'TfrmOutputAnnual.FrameResize';
begin
  try
    pnlTop.Height := Self.ClientHeight div 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.cmbboxRoofSizeChange(Sender: TObject);
const OPNAME = 'TfrmOutputAnnual.cmbboxRoofSizeChange';
begin
  try
    Series1.Visible :=  cmbboxRoofSize.IsChecked(0);
    Series2.Visible :=  cmbboxRoofSize.IsChecked(1);
    Series3.Visible :=  cmbboxRoofSize.IsChecked(2);
    Series4.Visible :=  cmbboxRoofSize.IsChecked(3);
    Series5.Visible :=  cmbboxRoofSize.IsChecked(4);
    Series6.Visible :=  cmbboxRoofSize.IsChecked(5);
    Series7.Visible :=  cmbboxRoofSize.IsChecked(6);
    Series8.Visible :=  cmbboxRoofSize.IsChecked(7);
    Series9.Visible :=  cmbboxRoofSize.IsChecked(8);
    Series10.Visible :=  cmbboxRoofSize.IsChecked(9);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.edtMinXExit(Sender: TObject);
const OPNAME = 'TfrmOutputAnnual.edtMinXExit';
var
  LMin : integer;
begin
  try
    LMin := StrToIntDef(edtMinX.Text,0);
    if(LMin < chartData.BottomAxis.Maximum) then
      chartData.BottomAxis.Minimum := LMin;
    edtMinX.Text := IntToStr(Trunc(chartData.BottomAxis.Minimum));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.PopulateDialog;
const OPNAME = 'TfrmOutputMonthly.PopulateDialog';
var
  LRecord : TOutputAnnualRecord;
  LColCount : integer;
  LCount    : integer;
  LIndex    : integer;
  LCursor   : TCursor;
  LStr      : string;
  LFileName : string;
  LStation  : TRainfallStation;
  LMultiplier : integer;
begin
  try
    LCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      LStation      :=  RWHModelData.SelectedRainfallStation;
      if(LStation = nil) then Exit;
      LFileName     := RWHModelData.GetRainfallStationOutputAnnualFileName(LStation.StationNumber);
      if not FileExists(LFileName) then Exit;

      FOutputAnnualFileAgent.LoadFile(LFileName);
      strgrdData.ColCount := FOutputAnnualFileAgent.OutputFileHeader.TankCount+1;


      LMultiplier := 1000;

      cmbboxRoofSize.Clear;
      for LIndex := 1 to FOutputAnnualFileAgent.OutputFileHeader.TankCount do
      begin
        if(FOutputAnnualFileAgent.OutputFileHeader.FTankSizes[LIndex] <> NullFloat) then
        begin
          LStr := FormatFloat('0.000',FOutputAnnualFileAgent.OutputFileHeader.FTankSizes[LIndex]*LMultiplier);
          cmbboxRoofSize.Items.Add(LStr);
          LStr := LStr + '('+VolumeUnitName+')';
          strgrdData.Cells[LIndex,0] := LStr;
          if(LIndex = 1) then
            Series1.Title := LStr;
          if(LIndex = 2) then
            Series2.Title := LStr;
          if(LIndex = 3) then
            Series3.Title := LStr;
          if(LIndex = 4) then
            Series4.Title := LStr;
          if(LIndex = 5) then
            Series5.Title := LStr;
          if(LIndex = 6) then
            Series6.Title := LStr;
          if(LIndex = 7) then
            Series7.Title := LStr;
          if(LIndex = 8) then
            Series8.Title := LStr;
          if(LIndex = 9) then
            Series9.Title := LStr;
          if(LIndex = 10) then
            Series10.Title := LStr;
        end;
      end;
      cmbboxRoofSize.SetCheckedAll(nil);

      chartData.Title.Text.Clear;
      chartData.Title.Text.Add('Assurance of Supply');
      LStr := FormatFloat('0.###', FOutputAnnualFileAgent.OutputFileHeader.FHouseHold_DemandPP * LMultiplier);
      LStr := LStr + '('+VolumeUnitName+')';
      chartData.Title.Text.Add('Demand : '+ LStr);

      LStr := FormatFloat('0.###', FOutputAnnualFileAgent.OutputFileHeader.FRoof_Area)+'(m²)';
      chartData.Title.Text.Add('Roof Size : '+LStr);

      LCount := strgrdData.RowCount;
      LColCount := FOutputAnnualFileAgent.OutputFileHeader.TankCount;
      LRecord := FOutputAnnualFileAgent.Next;
      while (LRecord  <> nil) do
      begin
        strgrdData.Cells[0,LCount-1] := IntToStr(LRecord.Reliability);
        for LIndex := 1 to LColCount do
        begin
          strgrdData.Cells[LIndex,LCount-1] := IntToStr(LRecord.DaysSupplied[LIndex]);
        end;
        if(LColCount > 0) then
          Series1.AddXY(LRecord.Reliability,LRecord.DaysSupplied[1]);
        if(LColCount > 1) then
          Series2.AddXY(LRecord.Reliability,LRecord.DaysSupplied[2]);
        if(LColCount > 2) then
          Series3.AddXY(LRecord.Reliability,LRecord.DaysSupplied[3]);
        if(LColCount > 3) then
          Series4.AddXY(LRecord.Reliability,LRecord.DaysSupplied[4]);
        if(LColCount > 4) then
          Series5.AddXY(LRecord.Reliability,LRecord.DaysSupplied[5]);
        if(LColCount > 5) then
          Series6.AddXY(LRecord.Reliability,LRecord.DaysSupplied[6]);
        if(LColCount > 6) then
          Series7.AddXY(LRecord.Reliability,LRecord.DaysSupplied[7]);
        if(LColCount > 7) then
          Series8.AddXY(LRecord.Reliability,LRecord.DaysSupplied[8]);
        if(LColCount > 8) then
          Series9.AddXY(LRecord.Reliability,LRecord.DaysSupplied[9]);
        if(LColCount > 9) then
          Series10.AddXY(LRecord.Reliability,LRecord.DaysSupplied[10]);

        LRecord := FOutputAnnualFileAgent.Next;
        if(LRecord <> nil) then
        begin
          LCount := LCount + 1;
          strgrdData.RowCount := LCount;
        end;
      end;
      cmbboxRoofSizeChange(nil);
    finally
      Screen.Cursor := LCursor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.chartDataDblClick(Sender: TObject);
const OPNAME = 'TfrmOutputAnnual.chartDataDblClick';
begin
  try
    //EditChartProperties(chartData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputAnnual.btnPrintClick(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.btnPrintGraphClick';
begin
  try
    chartData.PrintLandscape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
