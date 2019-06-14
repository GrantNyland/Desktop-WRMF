unit UOutputManagerForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,

  VCLTee.TeEngine,
  VCLTee.TeeProcs,
  VCLTee.Chart,

  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  Math,
  VCL.Printers,
  VCL.Samples.Spin,
  VCL.ComCtrls,
  VCL.Buttons,
  VCL.Grids,
  UMarkStringGrid,
  UStomsaGlobalData,
  UAbstractObject, VclTee.TeeGDIPlus;

type
  TfmOutputManager = class(TFrame)
    grdPrintSelection: TMarkStringGrid;
    pnlOutputMenu: TPanel;
    chtPrint: TChart;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    scrResolution: TScrollBar;
    Label2: TLabel;
    lblResolution: TLabel;
    stbOutput: TStatusBar;
    btnPrint: TSpeedButton;
    GroupBox2: TGroupBox;
    Panel2: TPanel;
    Label3: TLabel;
    Panel1: TPanel;
    Label4: TLabel;
    Panel3: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    edtRunRef: TEdit;
    btnSave: TSpeedButton;
    btnSelectAll: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure grdPrintSelectionMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnPrintClick(Sender: TObject);
    procedure scrResolutionChange(Sender: TObject);
    procedure edtRunRefChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    procedure PrintGraph10(GraphIndex: Integer);
    procedure PrintGraph234(GraphIndex: Integer);
    procedure PrintGraph89(GraphIndex: Integer);
    procedure PrintNatural(GraphIndex: Integer);
    procedure SetGridColour(AColumn, ARow: Integer);
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    procedure AfterConstruction; override;
  end;

var
  fmOutputManager: TfmOutputManager;

implementation

uses
  UDataModule,
  UGraphUnit,
  UErrorHandlingOperations;

var
  PrintDate, PrintTime : String;

{$R *.DFM}

constructor TfmOutputManager.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmOutputManager.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmOutputManager.AfterConstruction;
const OPNAME = 'TfmOutputManager.AfterConstruction';
var
  LineCounter : Integer;
begin
  inherited;
  try
    with grdPrintSelection do
    begin
      Cells[0,0] := 'Gauge\Graph';
      Cells[1,0] := 'Natural Data - Mean and SD';
      Cells[2,0] := 'Cumul. Distrb.+Correlogram';
      Cells[3,0] := 'N-Month Sums';
      Cells[4,0] := 'Yield Capacity Test';
    end;
    scrResolution.Position := 100;

    edtRunRef.Text := fmData.DataStorage.RunReference;

    with grdPrintSelection, fmData.DataStorage.CurrentRecord do
    begin
      grdPrintSelection.RowCount := fmData.DataStorage.IncFileCount + 1;
      LineCounter := 1;
      if fmData.DataStorage.First then
        Cells[0,LineCounter] := FileName;
      Inc(LineCounter);
      while fmData.DataStorage.Next do
      begin
        Cells[0,LineCounter] := FileName;
        Inc(LineCounter);
      end;//while fmData.

      if fmData.DataStorage.CurrentRecord.StatisticsCalculated then
      begin
        grdPrintSelection.ColCount := 5;
      end
      else if fmData.DataStorage.TimeSeriesFittedCount = fmData.DataStorage.IncFileCount then
      begin
        grdPrintSelection.ColCount := 3;
      end
      else if fmData.DataStorage.MarginalFittedCount = fmData.DataStorage.IncFileCount then
      begin
        grdPrintSelection.ColCount := 2;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmOutputManager.SetGridColour(AColumn, ARow : Integer);
const OPNAME = 'TfmOutputManager.SetGridColour';
var
  //CurrentColour : TColor;
  CellText, CurrentText :String;
begin
  try
    CurrentText := grdPrintSelection.Cells[AColumn,ARow];
    if CurrentText = 'X' then
      //CurrentColour := grdPrintSelection.Color
      //CurrentColour := clWhite;
      CellText:=' '
    else
      //CurrentColour := clBlack;
      CellText:='X';
      //grdPrintSelection.SetColour(AColumn,ARow,CurrentColour);
    grdPrintSelection.SetTextPos(AColumn,ARow,'C','C');
    grdPrintSelection.Cells[AColumn,ARow]:=CellText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmOutputManager.grdPrintSelectionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TfmOutputManager.grdPrintSelectionMouseDown';
var
  Loop, Loop2, TheCol, TheRow : Integer;
begin
  try
    with grdPrintSelection do
    begin
      MouseToCell(X,Y,TheCol,TheRow);
      if (TheRow = 0) and (TheCol <> 0) then
        for Loop := 1 to (RowCount - 1) do
          SetGridColour(TheCol,Loop)
      else if (TheCol = 0) and (TheRow <> 0) then
        for Loop := 1 to (ColCount - 1) do
          SetGridColour(Loop,TheRow)
      else if (TheCol = 0) and (TheRow = 0) then
        for Loop := 1 to (ColCount - 1) do
          for Loop2 := 1 to (RowCount -1) do
            SetGridColour(Loop,Loop2)
      else
        SetGridColour(TheCol,TheRow);

      Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmOutputManager.PrintNatural(GraphIndex : Integer);
const OPNAME = 'TfmOutputManager.PrintNatural';
begin
  try
    if fmData.DataStorage.GotoIndex(GraphIndex) then
    begin
      Printer.Orientation := poPortrait;
      Printer.BeginDoc;
      stbOutput.Panels[0].Text := 'Printing Natural Data Charts for ' + fmData.DataStorage.CurrentRecord.FileName;
      DrawBorders(PrintDate, PrintTime);

      with fmData.DataStorage.CurrentRecord, Printer do
      begin

        MeanBoxPlot(chtPrint, 'Monthly and Annual Mean');
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.04),Round(PageWidth*0.95),Round(PageHeight*0.43)));

        StandardDeviationBoxPlot(chtPrint, 'Montly and Annual Standard Deviation');
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.44),Round(PageWidth*0.95),Round(PageHeight*0.85)));

      end;//with fmData.
      Printer.EndDoc;
    end;//if fmData.DataStorage.Index
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE PrintMarginal

procedure TfmOutputManager.PrintGraph234(GraphIndex : Integer);
const OPNAME = 'TfmOutputManager.PrintGraph234';
var
  LLeftTitle,
  LBottomTitle,
  LTopTitle : String;
begin
  try
    if fmData.DataStorage.GotoIndex(GraphIndex) then
    begin
      Printer.Orientation := poPortrait;
      Printer.BeginDoc;
      LLeftTitle     :=  'Annual Flows (million m³/a)';
      LBottomTitle   :=  'Exceedance Probability (%)';
      LTopTitle      :=  'Transformed Flow Data';
      stbOutput.Panels[0].Text := 'Printing Distribution Graphs for ' + fmData.DataStorage.CurrentRecord.FileName;
      DrawBorders(PrintDate, PrintTime);
      with fmData.DataStorage.CurrentRecord, Printer do
      begin
        StandardisedFlowData(chtPrint,'Sampled Cumulative Distributions', RecordLength, ZeroCount, DefaultCurve, Standardised, NormalisedFlows);
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.04),Round(PageWidth*0.95),Round(PageHeight*0.29)));

        NaturalFlowData(chtPrint,LLeftTitle,LBottomTitle,LTopTitle, RecordLength, ZeroCount, DefaultCurve, NaturalFlows, Fitted, False);
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.32),Round(PageWidth*0.95),Round(PageHeight*0.57)));

        StreamFlowCorrelation(chtPrint,'Correlogram of Normalised Annual Streamflow',NCorr,NLAG[1]);
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.60),Round(PageWidth*0.95),Round(PageHeight*0.85)));

        chtPrint.LeftAxis.Automatic := true;
      end;//with fmData.
      Printer.EndDoc;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//

procedure TfmOutputManager.PrintGraph89(GraphIndex : Integer);
const OPNAME = 'TfmOutputManager.PrintGraph89';
begin
  try
    //Graph 8 = N-month run sums
    //Graph 9 = Deficit curves
    if fmData.DataStorage.GotoIndex(GraphIndex) then
    begin
      Printer.Orientation := poPortrait;
      Printer.BeginDoc;
      stbOutput.Panels[0].Text := 'Printing Deficit Graphs for ' + fmData.DataStorage.CurrentRecord.FileName;
      DrawBorders(PrintDate, PrintTime);
      with fmData.DataStorage.CurrentRecord, Printer do
      begin
        RunSumsBoxPlot(chtPrint,'Box Plot of N-Month Sums');
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.04),Round(PageWidth*0.95),Round(PageHeight*0.43)));

        MaximumDeficitBoxPlot(chtPrint,'Maximum Deficit');
  //        FSize:=chtPrint.Title.Font.Size;
        chtPrint.Title.Font.Size:=1;
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.03),Round(PageHeight*0.46),Round(PageWidth*0.36),Round(PageHeight*0.85)));

        MaxDeficitDurationBoxPlot(chtPrint,'Duration of Maximum Deficit');
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.365),Round(PageHeight*0.46),Round(PageWidth*0.67),Round(PageHeight*0.85)));

        LongestDepletionBoxPlot(chtPrint,'Duration of Longest Depletion');
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.68),Round(PageHeight*0.46),Round(PageWidth*0.99),Round(PageHeight*0.85)));


      end;//with fmData.
      Printer.EndDoc;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//

procedure TfmOutputManager.PrintGraph10(GraphIndex : Integer);
const OPNAME = 'TfmOutputManager.PrintGraph10';
begin
  try
    //Graph 10 = Yield Capacity Test
    if fmData.DataStorage.GotoIndex(GraphIndex) then
    begin
      Printer.Orientation := poLandscape;
      Printer.BeginDoc;
      stbOutput.Panels[0].Text := 'Printing Yield Capacity Test Graph for ' + fmData.DataStorage.CurrentRecord.FileName;
      DrawBorders(PrintDate, PrintTime);
      with fmData.DataStorage.CurrentRecord, Printer do
      begin
        YieldCapacityTest(chtPrint,'Yield Capacity Test');
        chtPrint.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.04),Round(PageWidth*0.95),Round(PageHeight*{0.43}0.85)));
      end;//with fmData.
    end;//if fmData.DataStorage.Index
    Printer.EndDoc;
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE PrintMarginal

procedure TfmOutputManager.btnPrintClick(Sender: TObject);
const OPNAME = 'TfmOutputManager.btnPrintClick';
var
  ColLoop, RowLoop : Integer;
  CellText:string;
begin
  try
    //scroll through the grid printing the various stuff that is highlighted in black,
    //change those colours to blue once printing is complete
    PrintDate := DateToStr(Date);
    PrintTime := TimeToStr(Time);

    Printer.Title := 'STOMSA';
    with grdPrintSelection do
    begin
      for RowLoop := 1 to (RowCount - 1) do
      begin
        for ColLoop := 1 to (ColCount - 1) do
        begin
          CellText := Cells[ColLoop,RowLoop];
          if CellText= 'X' then
          begin
            Cells[ColLoop,RowLoop] :='0';
            //SetColour(ColLoop,RowLoop,TheColour);
            Case ColLoop of
              1 : PrintNatural(RowLoop);
              2 : PrintGraph234(RowLoop);
              3 : PrintGraph89(RowLoop);
              4 : PrintGraph10(RowLoop);
            end;//Case
          end;//if TheColour
        end;//for RowLoop
      end;//for ColLoop
      Refresh;
        stbOutput.Panels[0].Text := 'Printing Complete';
    end;//with grdPrintSelection
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE btnPrintClick

procedure TfmOutputManager.scrResolutionChange(Sender: TObject);
const OPNAME = 'TfmOutputManager.scrResolutionChange';
begin
  try
    lblResolution.Caption := IntToStr(scrResolution.Position);
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE scrResolutionChange

procedure TfmOutputManager.edtRunRefChange(Sender: TObject);
const OPNAME = 'TfmOutputManager.edtRunRefChange';
begin
  try
    fmData.DataStorage.RunReference := edtRunRef.Text;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmOutputManager.btnSaveClick(Sender: TObject);
const OPNAME = 'TfmOutputManager.btnSaveClick';
begin
  try
  //save all the data in required format
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
