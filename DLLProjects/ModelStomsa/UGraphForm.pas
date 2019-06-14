unit UGraphForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.ExtCtrls, VCL.StdCtrls, VCLTee.Chart, VCLTee.TeeProcs, VCLTee.TeEngine, VCL.Buttons, VCL.ExtDlgs,UAbstractObject,
  VCL.Samples.Spin, VCL.ComCtrls;

type
  TfmGraph = class(TFrame)
    dlgPrintSingleGraph: TPrintDialog;
    dlgSaveChart: TSavePictureDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Bevel1: TBevel;
    Label1: TLabel;
    btnCreate: TSpeedButton;
    btnPrint: TSpeedButton;
    btnDownload: TSpeedButton;
    lblCorrelateGauges: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cmbGauges: TComboBox;
    lstCorrelations: TListBox;
    SpinEditDimondXSize: TSpinEdit;
    SpinEditDimondYSize: TSpinEdit;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label2: TLabel;
    cmbGraphGroup: TComboBox;
    lstGraph: TListBox;
    btnLegend: TButton;
    chtAllPurpose: TChart;
    Panel2: TPanel;
    RichEdit1: TRichEdit;
    Label3: TLabel;
    cmbFiles: TComboBox;
    btnDataSelection: TButton;
    procedure cmbGraphClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure lstGraphClick(Sender: TObject);
    procedure lstCorrelationsClick(Sender: TObject);
    procedure chtAllPurposeDblClick(Sender: TObject);
    procedure SpinEditDimondYSizeChange(Sender: TObject);
    procedure SpinEditDimondXSizeChange(Sender: TObject);
    procedure cmbGraphGroupChange(Sender: TObject);
    procedure cmbGaugesChange(Sender: TObject);
    procedure btnLegendClick(Sender: TObject);
    procedure cmbFilesChange(Sender: TObject);
    procedure btnDataSelectionClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    FLastDimondZoomXPerc : integer;
    FLastDimondZoomYPerc : integer;
    FShowingCorrelation  : boolean;
    procedure EditChartProperties;
    procedure ShowTheGraph;
    procedure ShowTheCorrelation;
    procedure PopulateGraphTypes;
    procedure OnBottomAxisOnDrawLabel(Sender: TChartAxis; Var X, Y, Z: Integer; Var Text: String; Var DrawLabel: Boolean);
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
  end;

var
  fmGraph: TfmGraph;

implementation

uses
  UDataModule,
  UGraphUnit,
  VCL.Printers,
  VCLTee.TeeEdit,
  VCLTee.Series,
  VCLTee.TeeShape,
  UHelpContexts,
  UDataComponent,
  UAbstractComponent,
  UErrorHandlingOperations;

{$R *.DFM}

constructor TfmGraph.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmGraph.Create';
begin
  inherited Create(AOwner);
  try
    FShowingCorrelation          := False;
    FAppModules                  := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TfmGraph.Destroy;
const OPNAME = 'TfmGraph.Create';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

procedure TfmGraph.AfterConstruction;
const OPNAME = 'TfmGraph.AfterConstruction';
var
  LastGraph : Integer;
begin
  inherited;
  try
    LastGraph := -1;
    FLastDimondZoomXPerc := 100;
    FLastDimondZoomYPerc := 100;
    if cmbGauges.Items.Count > 0 then
      LastGraph := cmbGauges.ItemIndex;

    cmbGauges.Clear;
    cmbFiles.Clear;
    //with fmData.DataStorage.CurrentRecord do
    //begin
      if fmData.DataStorage.First then
      begin
        cmbGauges.Items.Add(fmData.DataStorage.CurrentRecord.FileName);
        cmbFiles.Items.Add(ChangeFileExt(fmData.DataStorage.CurrentRecord.FileName,'.ANS'));
      end;

      while fmData.DataStorage.Next do
      begin
        cmbGauges.Items.Add(fmData.DataStorage.CurrentRecord.FileName);
        cmbFiles.Items.Add(ChangeFileExt(fmData.DataStorage.CurrentRecord.FileName,'.ANS'));
      end;

      if LastGraph <> -1 then
      begin
        cmbGauges.ItemIndex := LastGraph;
        ShowTheGraph;
      end;
      PopulateGraphTypes;
    //end;//with fmData
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.ShowTheCorrelation;
const OPNAME = 'TfmGraph.ShowTheCorrelation';
begin
  try
    SpinEditDimondXSize.Value := 100;
    SpinEditDimondYSize.Value := 100;
    FLastDimondZoomXPerc  := SpinEditDimondXSize.Value;
    FLastDimondZoomYPerc  := SpinEditDimondYSize.Value;
    if (cmbGauges.ItemIndex >= 0) then
    begin
      with fmData.DataStorage.CurrentRecord do
      begin
        if FlowCorrelationData.GotoIndex(lstCorrelations.ItemIndex + 1) then
        begin
          btnLegend.Enabled        := True;
          btnDataSelection.Enabled := True;
          FShowingCorrelation      := True;
          chtAllPurpose.BottomAxis.OnDrawLabel := OnBottomAxisOnDrawLabel;
          FlowCorrelation(chtAllPurpose, 'Correlation of ' + FileName + ' and ' + FlowCorrelationData.CurrentData.FileName);
          chtAllPurpose.Show;
        end;
      end;//with fmData
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.ShowTheGraph;
const OPNAME = 'TfmGraph.ShowTheGraph';
var
  LLeftTitle,
  LBottomTitle,
  LTopTitle : String;
begin
  try
    chtAllPurpose.Hide;
    lstCorrelations.Clear;
    FShowingCorrelation       := False;
    btnLegend.Enabled         := False;
    btnDataSelection.Enabled  := False;
    SpinEditDimondXSize.Value := 100;
    SpinEditDimondYSize.Value := 100;
    FLastDimondZoomXPerc  := SpinEditDimondXSize.Value;
    FLastDimondZoomYPerc  := SpinEditDimondYSize.Value;
    chtAllPurpose.LeftAxis.AxisValuesFormat := '#,##0.###';

    if (cmbGauges.ItemIndex >= 0) and (lstGraph.ItemIndex >= 0) then
    begin
      if fmData.DataStorage.GotoIndex(cmbGauges.ItemIndex+1) then
      begin
        with fmData.DataStorage.CurrentRecord do
        begin
          chtAllPurpose.Show;
          case cmbGraphGroup.ItemIndex of
           0: begin
                case lstGraph.ItemIndex of
                  0  : begin
                         Self.HelpContext          := HC_NaturalFlowData;
                         lstGraph.HelpContext      := HC_NaturalFlowData;
                         chtAllPurpose.HelpContext := HC_NaturalFlowData;

                         LLeftTitle     :=  'Annual Flows (million m³/a)';
                         LBottomTitle   :=  'Exceedance Probability (%)';
                         LTopTitle      :=  'Annual Flow Duration Curve';
                         NaturalFlowData(chtAllPurpose,LLeftTitle,LBottomTitle,LTopTitle,RecordLength,ZeroCount, DefaultCurve,
                                         NaturalFlows,Fitted,False);
                       end;
                  1  : begin
                         Self.HelpContext          := HC_TransformedFlowData;
                         lstGraph.HelpContext      := HC_TransformedFlowData;
                         chtAllPurpose.HelpContext := HC_TransformedFlowData;

                         LLeftTitle     :=  'Annual Flows (million m³/a)';
                         LBottomTitle   :=  'Standard deviation from the mean';
                         LTopTitle      :=  'Transformed Annual Flow Duration Curve';
                         NaturalFlowData(chtAllPurpose,LLeftTitle,LBottomTitle,LTopTitle,RecordLength,ZeroCount, DefaultCurve,
                                         NaturalFlows,Fitted,True);
                       end;
                  2  : begin
                         Self.HelpContext          := HC_StandardisedFlowData;
                         lstGraph.HelpContext      := HC_StandardisedFlowData;
                         chtAllPurpose.HelpContext := HC_StandardisedFlowData;

                         StandardisedFlowData(chtAllPurpose, 'Standardised Flow Data', RecordLength, ZeroCount, DefaultCurve,
                                            Standardised, NormalisedFlows);
                       end;
                  3  : begin
                         Self.HelpContext          := HC_NormalisedCorrelogram;
                         lstGraph.HelpContext      := HC_NormalisedCorrelogram;
                         chtAllPurpose.HelpContext := HC_NormalisedCorrelogram;

                         chtAllPurpose.LeftAxis.AxisValuesFormat := '#,##0.00';
                         StreamFlowCorrelation(chtAllPurpose,'Correlogram of Normalised Annual Streamflow',NCorr,NLAG[1]);
                       end;
                  4  : begin
                         Self.HelpContext          := HC_ResidualCorrelogramWithoutGuageCorrelation;
                         lstGraph.HelpContext      := HC_ResidualCorrelogramWithoutGuageCorrelation;
                         chtAllPurpose.HelpContext := HC_ResidualCorrelogramWithoutGuageCorrelation;

                         chtAllPurpose.LeftAxis.AxisValuesFormat := '#,##0.00';
                         ResidualStreamFlowCorrelation(chtAllPurpose,'Correlogram of Normalised Residual Streamflow',PCorr,NLAG[1],UserTimeSeriesModel);
                       end;
                end;
              end;
           1: begin
                case lstGraph.ItemIndex of
                  0  : begin
                         Self.HelpContext          := HC_MeanBoxPlot;
                         lstGraph.HelpContext      := HC_MeanBoxPlot;
                         chtAllPurpose.HelpContext := HC_MeanBoxPlot;

                         chtAllPurpose.BottomAxis.OnDrawLabel := OnBottomAxisOnDrawLabel;
                         MeanBoxPlot(chtAllPurpose,'Monthly and Annual Means');
                         btnLegend.Enabled        := True;
                         btnDataSelection.Enabled := True;
                       end;
                  1  : begin
                         Self.HelpContext          := HC_StandardDeviationBoxPlot;
                         lstGraph.HelpContext      := HC_StandardDeviationBoxPlot;
                         chtAllPurpose.HelpContext := HC_StandardDeviationBoxPlot;

                         chtAllPurpose.BottomAxis.OnDrawLabel := OnBottomAxisOnDrawLabel;
                         StandardDeviationBoxPlot(chtAllPurpose,'Monthly and Annual Standard Deviations');
                         btnLegend.Enabled        := True;
                         btnDataSelection.Enabled := True;
                       end;
                end;
              end;
           2: begin
                case lstGraph.ItemIndex of
                  0  : begin
                         Self.HelpContext          := HC_RunSumsBoxPlot;
                         lstGraph.HelpContext      := HC_RunSumsBoxPlot;
                         chtAllPurpose.HelpContext := HC_RunSumsBoxPlot;

                         RunSumsBoxPlot(chtAllPurpose,'N-month Run Sums');
                         btnLegend.Enabled         := True;
                         btnDataSelection.Enabled  := True;
                       end;
                  1  : begin
                         Self.HelpContext          := HC_MaximumDeficitBoxPlot;
                         lstGraph.HelpContext      := HC_MaximumDeficitBoxPlot;
                         chtAllPurpose.HelpContext := HC_MaximumDeficitBoxPlot;

                         MaximumDeficitBoxPlot(chtAllPurpose,'Maximum Deficit');
                         btnLegend.Enabled         := True;
                         btnDataSelection.Enabled  := True;
                       end;
                  2  : begin
                         Self.HelpContext          := HC_MaximumDeficitDuration;
                         lstGraph.HelpContext      := HC_MaximumDeficitDuration;
                         chtAllPurpose.HelpContext := HC_MaximumDeficitDuration;

                         MaxDeficitDurationBoxPlot(chtAllPurpose,'Max. Deficit Duration');
                         btnLegend.Enabled         := True;
                         btnDataSelection.Enabled  := True;
                       end;
                  3  : begin
                         Self.HelpContext          := HC_LongestDepletionBoxPlot;
                         lstGraph.HelpContext      := HC_LongestDepletionBoxPlot;
                         chtAllPurpose.HelpContext := HC_LongestDepletionBoxPlot;

                         LongestDepletionBoxPlot(chtAllPurpose,'Longest Depletion Duration');
                         btnLegend.Enabled         := True;
                         btnDataSelection.Enabled  := True;
                       end;
                  4  : begin
                         Self.HelpContext          := HC_YieldCapacityTest;
                         lstGraph.HelpContext      := HC_YieldCapacityTest;
                         chtAllPurpose.HelpContext := HC_YieldCapacityTest;

                         YieldCapacityTest(chtAllPurpose,'Yield Capacity Test'); //not here
                       end;
                  5  : begin
                         Self.HelpContext          := HC_YieldCapacityTest;
                         lstGraph.HelpContext      := HC_YieldCapacityTest;
                         chtAllPurpose.HelpContext := HC_YieldCapacityTest;

                         CapacityYieldBoxPlot(chtAllPurpose,'Capacity yield box plot');
                         btnLegend.Enabled         := True;
                         btnDataSelection.Enabled  := True;
                       end;
                  6  : begin
                         Self.HelpContext          := HC_AnnualCrossCorrelations;
                         lstGraph.HelpContext      := HC_AnnualCrossCorrelations;
                         chtAllPurpose.HelpContext := HC_AnnualCrossCorrelations;

                         AnnualCorrelations(chtAllPurpose,'Annual Correlations');
                         btnLegend.Enabled         := True;
                         btnDataSelection.Enabled  := True;
                       end;
                end;
              end;
          end;

          if FlowCorrelationData.First then
          begin
            repeat
              lstCorrelations.Items.Add(FlowCorrelationData.CurrentData.FileName);
            until NOT(FlowCorrelationData.Next);
          end;
        end;//with fmData
      end;
    end
    else
      chtAllPurpose.Hide;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.cmbGraphClick(Sender: TObject);
const OPNAME = 'TfmGraph.cmbGraphClick';
begin
  try
    ShowTheGraph;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.btnPrintClick(Sender: TObject);
const OPNAME = 'TfmGraph.btnPrintClick';
begin
  try
    with Printer do
    begin
      if dlgPrintSingleGraph.Execute then
      begin
        Printer.BeginDoc;
        DrawBorders(DateToStr(Date),TimeToStr(Time));
        chtAllPurpose.PrintPartialCanvas(Canvas,Rect(Round(PageWidth*0.05),Round(PageHeight*0.04),Round(PageWidth*0.95),Round(PageHeight*0.85)));
        Printer.EndDoc;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.btnCreateClick(Sender: TObject);
const OPNAME = 'TfmGraph.btnCreateClick';
var
  NewGraphForm : TfmGraph;
begin
  try
    Application.CreateForm(TfmGraph,NewGraphForm);
    NewGraphForm.show;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.btnDownloadClick(Sender: TObject);
const OPNAME = 'TfmGraph.btnDownloadClick';
begin
  try
    if dlgSaveChart.Execute then
    begin
      if Uppercase(ExtractFileExt(dlgSaveChart.FileName)) = '.BMP' then
        chtAllPurpose.SaveToBitmapFile(dlgSaveChart.FileName)
      else if Uppercase(ExtractFileExt(dlgSaveChart.FileName)) = '.EMF' then
        chtAllPurpose.SaveToMetafileEnh(dlgSaveChart.FileName)
      else if Uppercase(ExtractFileExt(dlgSaveChart.FileName)) = '.WMF' then
        chtAllPurpose.SaveToMetafile(dlgSaveChart.FileName)
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.lstGraphClick(Sender: TObject);
const OPNAME = 'TfmGraph.lstGraphClick';
begin
  try
    chtAllPurpose.BottomAxis.OnDrawLabel := nil;
    ShowTheGraph;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.lstCorrelationsClick(Sender: TObject);
const OPNAME = 'TfmGraph.lstCorrelationsClick';
begin
  try
    ShowTheCorrelation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.OnBottomAxisOnDrawLabel(Sender: TChartAxis; var X, Y, Z: Integer; var Text: String; var DrawLabel: Boolean);
const OPNAME = 'TfmGraph.OnBottomAxisOnDrawLabel';
begin
  try
    if(Text = '13') then
      Text := 'Annual';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.chtAllPurposeDblClick(Sender: TObject);
const OPNAME = 'TfmGraph.chtAllPurposeDblClick';
begin
  try
    EditChartProperties;
    Abort;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.EditChartProperties;
const OPNAME = 'TfmGraph.EditChartProperties';
var
  LChartEditor: TChartEditor;
begin
  try
    LChartEditor := TChartEditor.Create(nil);
    try
      LChartEditor.Chart := chtAllPurpose;
      LChartEditor.RememberPosition := True;
      //LChartEditor.TreeView := True;
      LChartEditor.Execute;
    finally
      LChartEditor.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.SpinEditDimondXSizeChange(Sender: TObject);
const OPNAME = 'TfmGraph.SpinEditDimondXSizeChange';
var
  LShapeSeries    : TChartShape;
  LIndex          : integer;
  LOriginalWidth  : double;
  LNewWidth       : double;
begin
  try
    if(SpinEditDimondXSize.Value <= 0) then Exit;
    for LIndex := 0 to chtAllPurpose.SeriesList.Count-1 do
    begin
      if chtAllPurpose.SeriesList.Items[LIndex].ClassNameIs('TChartShape') then
      begin
        LShapeSeries := TChartShape(chtAllPurpose.SeriesList.Items[LIndex]);
        if(LShapeSeries.Style = chasDiamond) then
        begin
          LOriginalWidth := (100*(LShapeSeries.X1 - LShapeSeries.X0)) / FLastDimondZoomXPerc;
          LNewWidth      := LOriginalWidth * (SpinEditDimondXSize.Value / 100);
          LShapeSeries.X1 := LShapeSeries.X0 + LNewWidth;
        end;
      end;
    end;
    FLastDimondZoomXPerc  := SpinEditDimondXSize.Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.SpinEditDimondYSizeChange(Sender: TObject);
const OPNAME = 'TfmGraph.SpinEditDimondYSizeChange';
var
  LShapeSeries    : TChartShape;
  LIndex          : integer;
  LOriginalHeight : double;
  LNewHeight      : double;
begin
  try
    if(SpinEditDimondYSize.Value <= 0) then Exit;
    for LIndex := 0 to chtAllPurpose.SeriesList.Count-1 do
    begin
      if chtAllPurpose.SeriesList.Items[LIndex].ClassNameIs('TChartShape') then
      begin
        LShapeSeries := TChartShape(chtAllPurpose.SeriesList.Items[LIndex]);
        if(LShapeSeries.Style = chasDiamond) then
        begin
          LOriginalHeight := (100*(LShapeSeries.Y1 - LShapeSeries.Y0)) / FLastDimondZoomYPerc;
          LNewHeight      := LOriginalHeight * (SpinEditDimondYSize.Value / 100);
          LNewHeight      := LNewHeight - LOriginalHeight;
          if(FLastDimondZoomYPerc  > SpinEditDimondYSize.Value) then
          begin
            LShapeSeries.Y0 := LShapeSeries.Y0 - LNewHeight;
            LShapeSeries.Y1 := LShapeSeries.Y1 + LNewHeight;
          end
          else
          begin
            LShapeSeries.Y0 := LShapeSeries.Y0 + LNewHeight;
            LShapeSeries.Y1 := LShapeSeries.Y1 - LNewHeight;
          end

        end;
      end;
    end;
    FLastDimondZoomYPerc  := SpinEditDimondYSize.Value;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.cmbGraphGroupChange(Sender: TObject);
const OPNAME = 'TfmGraph.cmbGraphGroupChange';
begin
  try
    btnLegend.Enabled        := False;
    btnDataSelection.Enabled := False;
    PopulateGraphTypes;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.cmbGaugesChange(Sender: TObject);
const OPNAME = 'TfmGraph.cmbGaugesChange';
begin
  try
    btnLegend.Enabled        := False;
    btnDataSelection.Enabled := False;
    ShowTheGraph;
    cmbFiles.ItemIndex := cmbGauges.ItemIndex;
    cmbFilesChange(nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.PopulateGraphTypes;
const OPNAME = 'TfmGraph.PopulateGraphTypes';
begin
  try
    if(cmbGraphGroup.Items.Count = 0) then
    begin
     cmbGraphGroup.Items.Add('Curve Fitting');
     cmbGraphGroup.Items.Add('Verification');
     cmbGraphGroup.Items.Add('Validation');
     cmbGraphGroup.ItemIndex := 0;
    end;

    lstGraph.Clear;
    case cmbGraphGroup.ItemIndex of
     0: begin
          lstGraph.Items.Add('Natural Flow Data');
          lstGraph.Items.Add('Transformed Flow Data');
          lstGraph.Items.Add('Standardised Flow Data');
          lstGraph.Items.Add('Normalised Correlogram');
          lstGraph.Items.Add('Residual Correlogram');
        end;
     1: begin
          lstGraph.Items.Add('Mean Box Plot');
          lstGraph.Items.Add('Standard Deviation Box Plot');
        end;
     2: begin
          lstGraph.Items.Add('Run Sums Box Plot');
          lstGraph.Items.Add('Maximum Deficit Box Plot');
          lstGraph.Items.Add('Maximum Deficit Duration Box Plot');
          lstGraph.Items.Add('Longest Depletion Box Plot');
          lstGraph.Items.Add('Yield Capacity Test');
          lstGraph.Items.Add('Capacity yield box plot');
          lstGraph.Items.Add('Annual Correlations');
        end;
    end;

    if(lstGraph.Items.Count > 0) then
    begin
      lstGraph.ItemIndex := 0;
      ShowTheGraph;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.btnLegendClick(Sender: TObject);
const OPNAME = 'TfmGraph.btnLegendClick';
var
  LForm : TAbstractForm;
  LImage: TImage;
begin
  try
    LForm := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    try
      LImage := TImage.Create(LForm);
      LImage.Parent      := LForm;
      LImage.Transparent := True;
      LImage.AutoSize    := True;
      LImage.Center      := True;
      LImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'BOXPLOTLEGEND');
      LForm.ClientHeight := LImage.Height;
      LForm.ClientWidth  := LImage.Height;
      LImage.Align       := alClient;
      LForm.Caption      := FAppModules.Language.GetString('FormCaption.BoxPlotSeries');
      LForm.LanguageHasChanged;
      LForm.ShowModal;
    finally
      FreeAndNil(LForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmGraph.cmbFilesChange(Sender: TObject);
const OPNAME = 'TfmGraph.cmbFilesChange';
var
  LFileName : string;
begin
  try
    LFileName := IncludeTrailingPathDelimiter(fmData.DataStorage.CurrentRecord.Directory) + cmbFiles.Text;
    if FileExists(LFileName) then
      RichEdit1.Lines.LoadFromFile(LFileName)
    else
      RichEdit1.Lines.Text := LFileName + ' does not exists';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmGraph.btnDataSelectionClick(Sender: TObject);
const OPNAME = 'TfmGraph.btnDataSelectionClick';
var
  LForm : TAbstractForm;
  LSeriesSelector:TBoxPlotSeriesSelector;
begin
  try
    LForm                     := TAbstractForm.CreateWithoutDFM(nil,FAppModules);
    LSeriesSelector           := TBoxPlotSeriesSelector.Create(LForm,FAppModules);
    LSeriesSelector.Parent := LForm;
    LSeriesSelector.Align  := alClient;
    try
      LSeriesSelector.LanguageHasChanged;
      LForm.Caption := 'Select Box plot line series %';
      LForm.ShowModal;
      if(LForm.ModalResult = mrOk) then
      begin
        fmData.DataStorage.BoxPlotDataSelection.CommaText  := LSeriesSelector.SeriesValuesCommatext;
        if FShowingCorrelation then
          ShowTheCorrelation
        else
          ShowTheGraph;
      end;
    finally
      FreeAndNil(LForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
