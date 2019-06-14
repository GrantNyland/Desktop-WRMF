unit UFrameOutputDaily;

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
  UOutputDailyFileAgent;

type
  TfrmOutputDaily = class(TFrame)
    chartData: TChart;
    Series1: TLineSeries;
    Splitter1: TSplitter;
    pnlTop: TPanel;
    strgrdData: TStringGrid;
    strgrdTotals: TStringGrid;
    pnlOptions: TPanel;
    lbTankVolume: TLabel;
    btnPrintGraph: TColourBitBtn;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    cmbboxTankSize: TComboBox;
    lblNote: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure cmbboxTankSizeChange(Sender: TObject);
    procedure btnPrintGraphClick(Sender: TObject);
    procedure chartDataDblClick(Sender: TObject);
  protected
    { Private declarations }
    FOutputDailyFileAgent : TOutputDailyFileAgent;
    procedure ClearDialog;
    procedure PopulateDialog;
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
  frmOutputDaily : TfrmOutputDaily;

implementation

{$R *.dfm}

uses
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TfrmOutputDaily }

procedure TfrmOutputDaily.AfterConstruction;
const OPNAME = 'TfrmOutputDaily.AfterConstruction';
begin
  inherited;
  try
    FOutputDailyFileAgent := TOutputDailyFileAgent.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.BeforeDestruction;
const OPNAME = 'TfrmOutputDaily.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FOutputDailyFileAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputDaily.Initialise: boolean;
const OPNAME = 'TfrmOutputDaily.Initialise';
begin
  Result := False;
  try
    FOutputDailyFileAgent.Initialise;
    ClearDialog;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputDaily.Finalise: boolean;
const OPNAME = 'TfrmOutputDaily.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputDaily.AfterInitialise: boolean;
const OPNAME = 'TfrmOutputDaily.AfterInitialise';
var
  LFileName : string;
  LStation      : TRainfallStation;
  LIndex        : integer;
  LMultiplier   : integer;
begin
  Result := False;
  try
    ClearDialog;
    LStation      :=  RWHModelData.SelectedRainfallStation;
    if(LStation = nil) then Exit;
    LFileName     := RWHModelData.GetRainfallStationOutputDailyFileName(LStation.StationNumber);
    if not FileExists(LFileName) then Exit;

    LMultiplier := 1000;

    FOutputDailyFileAgent.LoadFile(LFileName);
    for LIndex := 1 to 10 do
    begin
      if(FOutputDailyFileAgent.OutputFileHeader.FTankSizes[LIndex] <> NullFloat) then
        cmbboxTankSize.Items.Add(FormatFloat('0.000',FOutputDailyFileAgent.OutputFileHeader.FTankSizes[LIndex]*LMultiplier));
    end;

    strgrdData.Cells[0,0] :='Row ID';
    strgrdData.Cells[1,0] :='Date';
    strgrdData.Cells[2,0] :='Rainfall' +' (mm)';
    strgrdData.Cells[3,0] :='Tank Level' + ' ('+VolumeUnitName+')';;
    strgrdData.Cells[4,0] :='Tank Spill' +' (mm)';
    strgrdData.Cells[5,0] :='Deficit' + ' ('+VolumeUnitName+')';;
    //strgrdData.Cells[6,0] := FrameWork.LanguageManager.TranslatedString['Decide'];

    Series1.Title           := 'Rainfall (mm)';
    Series2.Title           := 'Tank Level ('+VolumeUnitName+')';
    Series3.Title           := 'Spill (mm)';
    Series4.Title           := 'Deficit  ('+VolumeUnitName+')';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputDaily.LanguageHasChanged: boolean;
const OPNAME = 'TfrmOutputDaily.LanguageHasChanged';
begin
  Result := False;
  try
    lbTankVolume.Caption    := lbTankVolume.Caption + '('+VolumeUnitName+')';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.FrameResize(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.FrameResize';
begin
  try
    pnlTop.Height := Self.ClientHeight div 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.cmbboxTankSizeChange(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.cmbboxTankSizeChange';
begin
  try
    PopulateDialog;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.ClearDialog;
const OPNAME = 'TfrmOutputDaily.ClearDialog';
begin
  try
    Series1.Clear;
    Series2.Clear;
    Series3.Clear;
    Series4.Clear;
    strgrdData.RowCount := 2;
    strgrdData.Rows[1].Clear;
    strgrdTotals.Rows[0].Clear;
    chartData.Title.Caption := '';
    cmbboxTankSize.ItemIndex := -1;
    cmbboxTankSize.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.PopulateDialog;
const OPNAME = 'TfrmOutputDaily.cmbboxTankSizeChange';
var
  LRecord   : TOutputDailyRecord;
  LTankSize : string;
  LCount    : integer;
  LRowID    : integer;
  LIndex    : integer;
  LCursor   : TCursor;
  LMultiplier   : integer;
begin
  try
    LCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      LIndex        := cmbboxTankSize.ItemIndex;
      LRowID        := 0;
      LTankSize     := cmbboxTankSize.Items[LIndex];
      LCount        := strgrdData.RowCount;

      Series1.Clear;
      Series2.Clear;
      Series3.Clear;
      Series4.Clear;

      LMultiplier := 1000;

      chartData.Title.Caption := 'Tank Size: '+LTankSize+ ' ('+VolumeUnitName+')';
      chartData.LeftAxis.Title.Caption := '';
      if Series1.Visible then
        chartData.LeftAxis.Title.Caption := Series1.Title
      else if Series2.Visible then
        chartData.LeftAxis.Title.Caption := Series2.Title
      else if Series3.Visible then
        chartData.LeftAxis.Title.Caption := Series3.Title
      else if Series4.Visible then
        chartData.LeftAxis.Title.Caption := Series4.Title;

      FOutputDailyFileAgent.StartReadingTankVolume(StrToFloat(LTankSize)/LMultiplier);
      LRecord := FOutputDailyFileAgent.Next;
      while (LRecord  <> nil) do
      begin
        strgrdData.Cells[0,LCount-1] := IntToStr(LRowID);
        strgrdData.Cells[1,LCount-1] := DateToStr(LRecord.Date);
        strgrdData.Cells[2,LCount-1] := FormatFloat('##0.000',LRecord.Rainfall);
        strgrdData.Cells[3,LCount-1] := FormatFloat('##0.000',LRecord.Storage*LMultiplier);
        strgrdData.Cells[4,LCount-1] := FormatFloat('##0.000',LRecord.Spill);
        strgrdData.Cells[5,LCount-1] := FormatFloat('##0.000',LRecord.Deficit*LMultiplier);
        //strgrdData.Cells[6,LCount-1] := IntToStr(LRecord.Decide);

        Series1.AddXY(LRecord.Date,LRecord.Rainfall);
        Series2.AddXY(LRecord.Date,LRecord.Storage*LMultiplier);
        Series3.AddXY(LRecord.Date,LRecord.Spill);
        Series4.AddXY(LRecord.Date,LRecord.Deficit*LMultiplier);

        LRecord := FOutputDailyFileAgent.Next;
        if(LRecord <> nil) then
        begin
          LRowID := LRowID + 1;
          LCount := LCount + 1;
          strgrdData.RowCount := LCount;
        end;
      end;
    finally
      Screen.Cursor := LCursor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.btnPrintGraphClick(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.btnPrintGraphClick';
begin
  try
    chartData.PrintLandscape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputDaily.chartDataDblClick(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.chartDataDblClick';
begin
  try
    //EditChartProperties(chartData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
