unit UFrameOutputMonthly;

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
  UOutputMonthlyFileAgent;

type
  TfrmOutputMonthly = class(TFrame)
    chartData: TChart;
    Series1: TLineSeries;
    Splitter1: TSplitter;
    pnlTop: TPanel;
    strgrdData: TStringGrid;
    strgrdTotals: TStringGrid;
    pnlOptions: TPanel;
    lbTankVolume: TLabel;
    btnPrintGraph: TColourBitBtn;
    cmbboxTankSize: TComboBox;
    lblNote: TLabel;
    rgGraphType: TRadioGroup;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    Series6: TLineSeries;
    Series7: TLineSeries;
    Series8: TLineSeries;
    Series9: TLineSeries;
    Series10: TLineSeries;
    Series11: TLineSeries;
    Series12: TLineSeries;
    Series13: TLineSeries;
    procedure FrameResize(Sender: TObject);
    procedure cmbboxTankSizeChange(Sender: TObject);
    procedure btnPrintGraphClick(Sender: TObject);
    procedure chartDataDblClick(Sender: TObject);
    procedure rgGraphTypeClick(Sender: TObject);
  protected
    { Private declarations }
    FOutputMonthlyFileAgent : TOutputMonthlyFileAgent;
    FMonthNumbers        : array[1..12] of integer;
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
  frmOutputMonthly : TfrmOutputMonthly;

implementation

{$R *.dfm}

uses
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TfrmOutputMonthly }

procedure TfrmOutputMonthly.AfterConstruction;
const OPNAME = 'TfrmOutputMonthly.AfterConstruction';
begin
  inherited;
  try
    FOutputMonthlyFileAgent := TOutputMonthlyFileAgent.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.BeforeDestruction;
const OPNAME = 'TfrmOutputMonthly.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FOutputMonthlyFileAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputMonthly.Initialise: boolean;
const OPNAME = 'TfrmOutputMonthly.Initialise';
begin
  Result := False;
  try
    ClearDialog;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.ClearDialog;
const OPNAME = 'TfrmOutputMonthly.ClearDialog';
begin
  try
    FOutputMonthlyFileAgent.Initialise;
    cmbboxTankSize.ItemIndex := -1;
    cmbboxTankSize.Items.Clear;
    strgrdData.RowCount := 2;
    strgrdData.Rows[1].Clear;
    strgrdTotals.Rows[0].Clear;
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
    Series11.Clear;
    Series12.Clear;
    Series13.Clear;
    rgGraphTypeClick(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputMonthly.Finalise: boolean;
const OPNAME = 'TfrmOutputMonthly.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputMonthly.LanguageHasChanged: boolean;
const OPNAME = 'TfrmOutputMonthly.LanguageHasChanged';
begin
  Result := False;
  try
    lbTankVolume.Caption  := lbTankVolume.Caption + '('+VolumeUnitName+')';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputMonthly.AfterInitialise: boolean;
const OPNAME = 'TfrmOutputMonthly.AfterInitialise';
var
  LFileName     : string;
  LStation      : TRainfallStation;
  LIndex        : integer;
  LMultiplier   : integer;
begin
  Result := False;
  try
    ClearDialog;
    LStation      :=  RWHModelData.SelectedRainfallStation;
    if(LStation = nil) then Exit;
    LFileName     := RWHModelData.GetRainfallStationOutputMonthlyFileName(LStation.StationNumber);
    if not FileExists(LFileName) then Exit;

    LMultiplier := 1000;

    FOutputMonthlyFileAgent.LoadFile(LFileName);
    cmbboxTankSize.Items.Clear;
    for LIndex := 1 to 10 do
    begin
      if(FOutputMonthlyFileAgent.OutputFileHeader.FTankSizes[LIndex] <> NullFloat) then
        cmbboxTankSize.Items.Add(FormatFloat('0.000',FOutputMonthlyFileAgent.OutputFileHeader.FTankSizes[LIndex]*LMultiplier));
    end;

    strgrdData.Cells[0,0] := 'Year';
    for LIndex := 1 to 12 do
      strgrdData.Cells[LIndex,0] := FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[LIndex];

    Series1.Title :=  'Days Supplied';
    Series2.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[1];
    Series3.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[2];
    Series4.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[3];
    Series5.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[4];
    Series6.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[5];
    Series7.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[6];
    Series8.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[7];
    Series9.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[8];
    Series10.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[9];
    Series11.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[10];
    Series12.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[11];
    Series13.Title :=  FOutputMonthlyFileAgent.OutputFileHeader.FMonthNames[12];
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.FrameResize(Sender: TObject);
const OPNAME = 'TfrmOutputMonthly.FrameResize';
begin
  try
    pnlTop.Height := Self.ClientHeight div 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.cmbboxTankSizeChange(Sender: TObject);
const OPNAME = 'TfrmOutputMonthly.cmbboxTankSizeChange';
var
  LRecord      : TOutputMonthlyRecord;
  LRoofSize    : string;
  LPos         : integer;
  LCount       : integer;
  LIndex       : integer;
  LCursor      : TCursor;
  LDate        : TDate;
  LXPos         : double;
  LMultiplier  : integer;
  LSorted02    : TStringList;
  LSorted03    : TStringList;
  LSorted04    : TStringList;
  LSorted05    : TStringList;
  LSorted06    : TStringList;
  LSorted07    : TStringList;
  LSorted08    : TStringList;
  LSorted09    : TStringList;
  LSorted10    : TStringList;
  LSorted11    : TStringList;
  LSorted12    : TStringList;
  LSorted13    : TStringList;
begin
  try
    LCursor   := Screen.Cursor;
    LSorted02 := TStringList.Create; LSorted02.Sorted := True; LSorted02.Duplicates := dupAccept;
    LSorted03 := TStringList.Create; LSorted03.Sorted := True; LSorted03.Duplicates := dupAccept;
    LSorted04 := TStringList.Create; LSorted04.Sorted := True; LSorted04.Duplicates := dupAccept;
    LSorted05 := TStringList.Create; LSorted05.Sorted := True; LSorted05.Duplicates := dupAccept;
    LSorted06 := TStringList.Create; LSorted06.Sorted := True; LSorted06.Duplicates := dupAccept;
    LSorted07 := TStringList.Create; LSorted07.Sorted := True; LSorted07.Duplicates := dupAccept;
    LSorted08 := TStringList.Create; LSorted08.Sorted := True; LSorted08.Duplicates := dupAccept;
    LSorted09 := TStringList.Create; LSorted09.Sorted := True; LSorted09.Duplicates := dupAccept;
    LSorted10 := TStringList.Create; LSorted10.Sorted := True; LSorted10.Duplicates := dupAccept;
    LSorted11 := TStringList.Create; LSorted11.Sorted := True; LSorted11.Duplicates := dupAccept;
    LSorted12 := TStringList.Create; LSorted12.Sorted := True; LSorted12.Duplicates := dupAccept;
    LSorted13 := TStringList.Create; LSorted13.Sorted := True; LSorted13.Duplicates := dupAccept;
    try
      Screen.Cursor := crHourGlass;
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
      Series11.Clear;
      Series12.Clear;
      Series13.Clear;


      strgrdData.RowCount := 2;
      strgrdData.Rows[1].Clear;
      strgrdTotals.Rows[0].Clear;

      for LIndex := 1 to 12 do
        FMonthNumbers[LIndex] := 0;

      LRoofSize     := cmbboxTankSize.Items[cmbboxTankSize.ItemIndex];
      LCount        := strgrdData.RowCount;

      LMultiplier := 1000;

      chartData.Title.Caption := 'Tank Size: '+LRoofSize+ ' ('+VolumeUnitName+')';
      FOutputMonthlyFileAgent.StartReadingTankVolume(StrToFloat(LRoofSize)/LMultiplier);
      LRecord := FOutputMonthlyFileAgent.Next;
      while (LRecord  <> nil) do
      begin
        strgrdData.Cells[0,LCount-1] := IntToStr(LRecord.Year);
        for LIndex := 1 to 12 do
        begin
          strgrdData.Cells[LIndex,LCount-1] := IntToStr(LRecord.DaysSupplied[LIndex]);
          LDate := EncodeDate(LRecord.Year,FOutputMonthlyFileAgent.OutputFileHeader.FMonthNumbers[LIndex],1);
          Series1.AddXY(LDate,LRecord.DaysSupplied[LIndex]);
          FMonthNumbers[LIndex] := FMonthNumbers[LIndex]+ LRecord.DaysSupplied[LIndex];
        end;
        LSorted02.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[1]));
        LSorted03.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[2]));
        LSorted04.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[3]));
        LSorted05.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[4]));
        LSorted06.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[5]));
        LSorted07.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[6]));
        LSorted08.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[7]));
        LSorted09.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[8]));
        LSorted10.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[9]));
        LSorted11.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[10]));
        LSorted12.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[11]));
        LSorted13.Add(FormatFloat('000000000.#',LRecord.DaysSupplied[12]));

        LRecord := FOutputMonthlyFileAgent.Next;
        if(LRecord <> nil) then
        begin
          LCount := LCount + 1;
          strgrdData.RowCount := LCount;
        end;
      end;

      LCount     := LSorted02.Count;
      LIndex     := LCount -1;
      for LPos := 0 to LCount-1 do
      begin
        LXPos := (LPos/LCount)*100.0;
        Series2.AddXY(LXPos,StrToFloatDef(LSorted02[LIndex],0));
        Series3.AddXY(LXPos,StrToFloatDef(LSorted03[LIndex],0));
        Series4.AddXY(LXPos,StrToFloatDef(LSorted04[LIndex],0));
        Series5.AddXY(LXPos,StrToFloatDef(LSorted05[LIndex],0));
        Series6.AddXY(LXPos,StrToFloatDef(LSorted06[LIndex],0));
        Series7.AddXY(LXPos,StrToFloatDef(LSorted07[LIndex],0));
        Series8.AddXY(LXPos,StrToFloatDef(LSorted08[LIndex],0));
        Series9.AddXY(LXPos,StrToFloatDef(LSorted09[LIndex],0));
        Series10.AddXY(LXPos,StrToFloatDef(LSorted10[LIndex],0));
        Series11.AddXY(LXPos,StrToFloatDef(LSorted11[LIndex],0));
        Series12.AddXY(LXPos,StrToFloatDef(LSorted12[LIndex],0));
        Series13.AddXY(LXPos,StrToFloatDef(LSorted13[LIndex],0));
        LIndex := LIndex-1;
      end;
    finally
      Screen.Cursor := LCursor;
      LSorted02.Free;
      LSorted03.Free;
      LSorted04.Free;;
      LSorted05.Free;;
      LSorted06.Free;;
      LSorted07.Free;;
      LSorted08.Free;;
      LSorted09.Free;;
      LSorted10.Free;;
      LSorted11.Free;;
      LSorted12.Free;;
      LSorted13.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.btnPrintGraphClick(Sender: TObject);
const OPNAME = 'TfrmOutputMonthly.btnPrintGraphClick';
begin
  try
    chartData.PrintLandscape;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.chartDataDblClick(Sender: TObject);
const OPNAME = 'TfrmOutputMonthly.chartDataDblClick';
begin
  try
    //EditChartProperties(chartData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputMonthly.rgGraphTypeClick(Sender: TObject);
const OPNAME = 'TfrmOutputMonthly.rgGraphTypeClick';
begin
  try
    Series1.Visible := (rgGraphType.ItemIndex = 0);
    Series2.Visible := not Series1.Visible;
    Series3.Visible := Series2.Visible;
    Series4.Visible := Series2.Visible;
    Series5.Visible := Series2.Visible;
    Series6.Visible := Series2.Visible;
    Series7.Visible := Series2.Visible;
    Series8.Visible := Series2.Visible;
    Series9.Visible := Series2.Visible;
    Series10.Visible := Series2.Visible;
    Series11.Visible := Series2.Visible;
    Series12.Visible := Series2.Visible;
    Series13.Visible := Series2.Visible;

    if Series1.Visible then
      chartData.BottomAxis.Title.Caption := 'Date'
    else
      chartData.BottomAxis.Title.Caption := 'Reliability';
    chartData.BottomAxis.AdjustMaxMin;
    chartData.LeftAxis.AdjustMaxMin;
    //chartData.BottomAxis.Visible := Series1.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
