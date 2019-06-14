unit UGraphUnit;

interface

uses

  Classes,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeShape,
  VCLTee.TeEngine,
  VCLTee.TeeBoxPlot,
  VCL.Graphics,
  VCL.Printers,
  VCL.Controls,
  Contnrs,
  UStomsaData,
  UStomsaGlobalData,
  Math;


  procedure DrawBorders(PrintDate, PrintTime : string);
  {Chart 1}
  function NaturalFlowData(TheChart : TChart; LeftTitle,BottomTitle,TopTitle : String; RecordLength, ZeroCount,
                           DefaultCurve : Integer; NaturalFlows : array1000F; Fitted : FittedArray; TransformedUnits : Boolean) : Boolean;
  {Chart 2}
  function StandardisedFlowData(TheChart : TChart; Title : String; RecordLength, ZeroCount, DefaultCurve : Integer;
                                Standardised : array1000F; NormalisedFlows : FittedArray) : Boolean;
  {Chart 3}
  function StreamFlowCorrelation(TheChart : TChart; Title : String; NCorr : ArrayT2_30; Lag : Integer) : Boolean;
  {Chart 4}
  function ResidualStreamFlowCorrelation(TheChart : TChart; Title : String; PCorr : arrayT9_2_30; Lag, TimeSeries : Integer) : Boolean;
  {Chart 6}
  function MeanBoxPlot(TheChart : TChart; Title : String) : Boolean;
  {Chart 7}
  function StandardDeviationBoxPlot(TheChart : TChart; Title : String) : Boolean;
  {Chart 8}
  function RunSumsBoxPlot(TheChart : TChart; Title : String) : Boolean;
  {Chart 9a}
  function MaximumDeficitBoxPlot(TheChart : TChart; Title : String) : Boolean;
  {Graph 9b}
  function MaxDeficitDurationBoxPlot(TheChart : TChart; Title : String) : Boolean;
  {Graph 9c}
  function LongestDepletionBoxPlot(TheChart : TChart; Title : String) : Boolean;
  {Graph 10}
  function YieldCapacityTest(TheChart : TChart; Title : String) : Boolean;
  {Graph 11}
  function FlowCorrelation(TheChart : TChart; Title : String) : Boolean;
  {Graph 12}
  function AnnualCorrelations(TheChart : TChart; Title : String) : Boolean;
  {Graph 13}
  function CapacityYieldBoxPlot(TheChart : TChart; Title : String) : Boolean;
  function CreateBoxPlotSeries(TheChart : TChart): TBoxSeries;

implementation

uses
  SysUtils,
  UUtilities,
  UStomsaStatistics,
  UBoxChart,
  UBoxPlotData,
  UDataModule,
  UErrorHandlingOperations;

var
  ABoxChart : TBoxChart;

procedure RemoveAllSeries(TheChart : TChart);
const OPNAME = 'UGraphUnit.RemoveAllSeries';
var
  Loop : Integer;
begin
  try
    for Loop := (TheChart.SeriesCount - 1) downto 0 do
      TheChart.SeriesList[Loop].Destroy;

    TheChart.Title.Text.Clear;

    TheChart.Legend.Visible := false;
    TheChart.Legend.Alignment := laBottom;

    TheChart.LeftAxis.Logarithmic := False;
    TheChart.LeftAxis.Automatic := true;
    TheChart.LeftAxis.Increment := 0;

    TheChart.RightAxis.Logarithmic := False;
    TheChart.RightAxis.Automatic := true;
    TheChart.RightAxis.Increment := 0;
    TheChart.RightAxis.Grid.Style := psClear;

    TheChart.BottomAxis.Automatic := true;
    TheChart.BottomAxis.Inverted := False;
    TheChart.BottomAxis.Increment := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure DrawBorders(PrintDate, PrintTime : string);
const OPNAME = 'UGraphUnit.DrawBorders';
var
  OldPen : TPen;
  Idummy : integer;
begin
  try
    with Printer do
    begin
      OldPen := Canvas.Pen;

      Canvas.Font.Name := 'ARIAL';

      Canvas.Pen.Width := 8;
      //Main page border
      Canvas.Rectangle(Round(PageWidth*0.01),Round(PageHeight*0.01),Round(PageWidth*0.99),Round(PageHeight*0.99));
      //Text block Border
      Canvas.Rectangle(Round(PageWidth*0.01),Round(PageHeight*0.88),Round(PageWidth*0.99),Round(PageHeight*0.95));
      //Vertical Line
      Canvas.MoveTo(Round(PageWidth*0.60),Round(PageHeight*0.88));
      Canvas.LineTo(Round(PageWidth*0.60),Round(PageHeight*0.95));
      //Bottom Text Block
      Canvas.TextOut(Round(PageWidth*0.03),Round(PageHeight*0.97),'');//some text
      Canvas.TextOut(Round(PageWidth*0.80),Round(PageHeight*0.91),PrintDate + ' ' + PrintTime);
      //Top Text Block
      IDummy :=round(280/PageHeight);
      Canvas.Font.Size := Canvas.Font.Size * 2 * IDummy;
      Canvas.TextOut(Round(PageWidth*0.03),Round(PageHeight*0.89),fmData.DataStorage.RunReference);
      Canvas.TextOut(Round(PageWidth*0.03),Round(PageHeight*0.92),fmData.DataStorage.CurrentRecord.FileName);

      //Restore all the stored values
      Canvas.Pen := OldPen;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function AnnualCorrelations(TheChart : TChart; Title : String) : Boolean;
//GRAPH 12
const OPNAME = 'UGraphUnit.AnnualCorrelations';
var
  Loop : Integer;
  MeanLine : TLineSeries;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := '???';
    TheChart.LeftAxis.Automatic := false;
    TheChart.LeftAxis.Minimum := -1;
    TheChart.LeftAxis.Maximum := 1;
    TheChart.LeftAxis.Increment := 0.1;

    TheChart.BottomAxis.Title.Caption := 'Months';

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    MeanLine := TLineSeries.Create(TheChart);
    MeanLine.SeriesColor := clBlack;
    MeanLine.ParentChart := TheChart;

    Loop := 0;
    if fmData.DataStorage.First then
    begin
      repeat
        with fmData.DataStorage.CurrentRecord.FlowCorrelationData.CurrentData do
        begin
          if fmData.DataStorage.CurrentRecord.FlowCorrelationData.First then
          repeat
            Inc(Loop);
            ABoxChart.AddPoint(Loop,PO[1,13],PO[2,13],PO[3,13],
                                  PO[4,13],PO[5,13],PO[6,13],PO[7,13],RSHO[13],0.1,0.2,aBothVertAxis);
            MeanLine.AddXY(Loop,RMEANO[13],'',clTeeColor);
          until  NOT(fmData.DataStorage.CurrentRecord.FlowCorrelationData.Next);
        end;
      until NOT(fmData.DataStorage.Next);
    end;
    
    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function FlowCorrelation(TheChart : TChart; Title : String) : Boolean;
//GRAPH 11
const OPNAME = 'UGraphUnit.FlowCorrelation';
var
  Loop : Integer;
  MeanLine : TLineSeries;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
  LValues              : array[1..7] of double;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := '???';
    TheChart.LeftAxis.Automatic := false;
    TheChart.LeftAxis.Minimum := -1;
    TheChart.LeftAxis.Maximum := 1;
    TheChart.LeftAxis.Increment := 0.1;

    TheChart.BottomAxis.Title.Caption := 'Months';

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    MeanLine := TLineSeries.Create(TheChart);
    MeanLine.SeriesColor := clBlack;
    MeanLine.ParentChart := TheChart;

    with fmData.DataStorage.CurrentRecord.FlowCorrelationData.CurrentData do
    begin
      for Loop := 1 to 12 do
      begin
        ABoxChart.AddPoint(Loop,PO[1,Loop],PO[2,Loop],PO[3,Loop],
                                PO[4,Loop],PO[5,Loop],PO[6,Loop],PO[7,Loop],RSHO[Loop],0.25,0.2,aLeftAxis);
        MeanLine.AddXY(Loop,RMEANO[Loop],'',clTeeColor);
      end;
      ABoxChart.AddPoint(13,PO[1,13],PO[2,13],PO[3,13],
                              PO[4,13],PO[5,13],PO[6,13],PO[7,13],RSHO[13],0.25,0.2,aRightAxis);
      MeanLine.AddXY(13,RMEANO[13],'',clTeeColor);

      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 12 do
          begin
            for LIndex := 1 to 7 do
                LValues[LIndex] := PO[LIndex,Loop];

            LCommatextData := DoubleArrayToCommaText(LValues,3);
            LCommatextData := IntToStr(Loop)+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;

    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function YieldCapacityTest(TheChart : TChart; Title : String) : Boolean;
//GRAPH 10
const OPNAME = 'UGraphUnit.YieldCapacityTest';
var
  Loop1, Loop2 : Integer;
  CapacityPoints : TPointSeries;
  MeanLine, MedianLine, HistoricalLine : TLineSeries;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(Title);
    TheChart.Legend.Visible := true;
    TheChart.LeftAxis.Title.Caption := 'Yield (% MAR)';
    TheChart.BottomAxis.Title.Caption := 'Capacity (% MAR)';
    TheChart.BottomAxis.Automatic := false;
    TheChart.BottomAxis.Minimum := 0;
    TheChart.BottomAxis.Maximum := 500;

    CapacityPoints := TPointSeries.Create(TheChart);
    CapacityPoints.Title := 'Simulated';
    CapacityPoints.Pointer.Brush.Color := clRed;
    CapacityPoints.Pointer.HorizSize := 1;
    CapacityPoints.Pointer.VertSize := 1;
    CapacityPoints.Pointer.Style := psRectangle;
    CapacityPoints.ParentChart := TheChart;

    MeanLine := TLineSeries.Create(TheChart);
    MeanLine.Title := 'Mean';
    MeanLine.SeriesColor := clBlack;
    MeanLine.ParentChart := TheChart;

    MedianLine := TLineSeries.Create(TheChart);
    MedianLine.Title := 'Meadian';
    MedianLine.SeriesColor := clGreen;
    MedianLine.ParentChart := TheChart;

    HistoricalLine := TLineSeries.Create(TheChart);
    HistoricalLine.Title := 'Historical';
    HistoricalLine.SeriesColor := clBlue;
    HistoricalLine.ParentChart := TheChart;

    with fmData.DataStorage.CurrentRecord do
    begin
      for Loop1 := 1 to 20 do
      begin
        for Loop2 := 1 to 101 do
        begin
          CapacityPoints.AddXY(GPAD[Loop2,Loop1],APAD[Loop2,Loop1],'',clTeeColor);
        end;//for Loop2
        if MNGPAD[Loop1] > 0.0 then
         MeanLine.AddXY(MNGPAD[Loop1],APAD[1,Loop1],'',clTeeColor);
        if MDXJD[Loop1] > 0.0 then
         MedianLine.AddXY(MDXJD[Loop1],APAD[1,Loop1],'',clTeeColor);
        if HGPAD[Loop1] > 0.0 then
          HistoricalLine.AddXY(HGPAD[Loop1],APAD[1,Loop1],'',clTeeColor);
      end;//for Loop1
    end;

    VCL.Controls.TControl(TheChart).Refresh;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function LongestDepletionBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 9c
const OPNAME = 'UGraphUnit.LongestDepletionBoxPlot';
var
  Loop : Integer;
  MaxVal:double;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
  LValues              : array[1..7] of double;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := 'Duration (months)';
    TheChart.BottomAxis.Title.Caption := 'Yield (% MAR)';
    TheChart.BottomAxis.Automatic := false;
    TheChart.BottomAxis.Minimum := 0;
    TheChart.BottomAxis.Maximum := 90;
    TheChart.BottomAxis.Minimum := 30;
    TheChart.BottomAxis.Increment := 10;
    TheChart.BottomAxis.Inverted := true;

    with fmData.DataStorage.CurrentRecord do
    begin
      //Find the maximum value
      MaxVal:=0.0;
      for Loop :=1 to 5 do
        MaxVal:= Max(MaxVal,RJLSD[1,Loop]);

      for Loop := 1 to 5 do
        //RELD is ascending order, is RSSD also?
        ABoxChart.AddPoint(RELD[loop],RJLSD[1,Loop],RJLSD[2,Loop],RJLSD[3,Loop],
                                      RJLSD[4,Loop],RJLSD[5,Loop],RJLSD[6,Loop],RJLSD[7,Loop],
                                      RILHD[Loop],2.0,MaxVal/60,aLeftAxis);
      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 5 do
          begin
            for LIndex := 1 to 7 do
                LValues[LIndex] := RJLSD[LIndex,Loop];

            LCommatextData := DoubleArrayToCommaText(LValues,3);
            LCommatextData := FormatFloat('##0.000',RELD[loop])+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;

    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function MaxDeficitDurationBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 9b
const OPNAME = 'UGraphUnit.MaxDeficitDurationBoxPlot';
var
  Loop : Integer;
  MaxVal : double;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
  LValues              : array[1..7] of double;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := 'Duration (Months)';
    TheChart.BottomAxis.Title.Caption := 'Yield (% MAR)';
    TheChart.BottomAxis.Automatic := false;
    TheChart.BottomAxis.Minimum := 0;
    TheChart.BottomAxis.Maximum := 90;
    TheChart.BottomAxis.Minimum := 30;
    TheChart.BottomAxis.Increment := 10;
    TheChart.BottomAxis.Inverted := true;

    with fmData.DataStorage.CurrentRecord do
    begin
      MaxVal:=0.0;
      for Loop := 1 to 5 do
        MaxVal:= Max(MaxVal,RILSD[1,Loop]);
      for Loop := 1 to 5 do
        //RELD is ascending order, is RSSD also?
        ABoxChart.AddPoint(RELD[loop],RILSD[1,Loop],RILSD[2,Loop],RILSD[3,Loop],
                                      RILSD[4,Loop],RILSD[5,Loop],RILSD[6,Loop],
                                      RILSD[7,Loop],RILHD[Loop],2.0,MaxVal/60,aLeftAxis);
      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 5 do
          begin
            for LIndex := 1 to 7 do
                LValues[LIndex] := RILSD[LIndex,Loop];

            LCommatextData := DoubleArrayToCommaText(LValues,3);
            LCommatextData := FormatFloat('##0.000',RELD[loop])+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;

    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function MaximumDeficitBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 9a
const OPNAME = 'UGraphUnit.MaximumDeficitBoxPlot';
var
  Loop : Integer;
  MaxVal:double;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
  LValues              : array[1..7] of double;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := 'Deficit (million m³)';
    TheChart.BottomAxis.Title.Caption := 'Yield (% MAR)';
    TheChart.BottomAxis.Automatic := false;
    TheChart.BottomAxis.Minimum := 0;
    TheChart.BottomAxis.Maximum := 90;
    TheChart.BottomAxis.Minimum := 30;
    TheChart.BottomAxis.Increment := 10;
    TheChart.BottomAxis.Inverted := true;

    with fmData.DataStorage.CurrentRecord do
    begin
      MaxVal:=0.0;
      for Loop := 1 to 5 do
        MaxVal:=Max(MaxVal,RSSD[1,Loop]);
      for Loop := 1 to 5 do
        //RELD is ascending order, is RSSD also?
        ABoxChart.AddPoint(RELD[loop],RSSD[1,Loop],RSSD[2,Loop],RSSD[3,Loop],
                                      RSSD[4,Loop],RSSD[5,Loop],RSSD[6,Loop],RSSD[7,Loop],
                                      RSHD[Loop],2.0,MaxVal/60,aLeftAxis);
      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 5 do
          begin
            for LIndex := 1 to 7 do
                LValues[LIndex] := RSSD[LIndex,Loop];

            LCommatextData := DoubleArrayToCommaText(LValues,3);
            LCommatextData := FormatFloat('##0.000',RELD[loop])+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;
    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function RunSumsBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 8
const OPNAME = 'UGraphUnit.RunSumsBoxPlot';
var
  Markers              : TPointSeries;
  Loop                 : Integer;
  MaxVal,
  Height               : double;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
  LValues              : array[1..7] of double;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Logarithmic := true;
    TheChart.LeftAxis.Title.Caption := 'Log of Streamflow (million m³/a)';
    //TheChart.LeftAxis.Increment := 100;
    TheChart.BottomAxis.Title.Caption := 'Months';
    TheChart.BottomAxis.Automatic := False;
    TheChart.BottomAxis.Minimum := 0;
    TheChart.BottomAxis.Maximum := 114;
    TheChart.BottomAxis.Increment := 6;
    TheChart.BottomAxis.Inverted := false;
    with fmData.DataStorage.CurrentRecord do
    begin
      MaxVal:=0.0;
      for Loop := 1 to 10 do
      MaxVal:=Max(MaxVal,WMIND[1,Loop])
    end;
    If MaxVal > 10 then
       TheChart.LeftAxis.Increment := 10;
    If MaxVal > 100 then
       TheChart.LeftAxis.Increment := 100;
    If MaxVal > 1500 then
       TheChart.LeftAxis.Increment := 200;
    If MaxVal > 2000 then
       TheChart.LeftAxis.Increment := 500;
    If MaxVal > 5000 then
       TheChart.LeftAxis.Increment := 1000;


    with fmData.DataStorage.CurrentRecord do
    begin
      Markers := TPointSeries.Create(TheChart);
      Markers.SeriesColor := clRed;
      Markers.ParentChart := TheChart;
      Markers.Pointer.Style := psDiamond;
      for Loop := 1 to 10 do
      begin
        Height:=0.0;
        Markers.AddXY(LPD[loop]+4.0,AMIND[Loop],'',clTeeColor);
        ABoxChart.AddPoint(LPD[loop],WMIND[1,Loop],WMIND[2,Loop],WMIND[3,Loop],
                                     WMIND[4,Loop],WMIND[5,Loop],WMIND[6,Loop],
                                     WMIND[7,Loop],AMIND[Loop],3.0,Height,aLeftAxis)
      end;
        //LWD is number of years
      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 10 do
          begin
            for LIndex := 1 to 7 do
                LValues[LIndex] := WMIND[LIndex,Loop];

            LCommatextData := DoubleArrayToCommaText(LValues,3);
            LCommatextData := FormatFloat('##0.000',LPD[loop])+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;
    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function StandardDeviationBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 7
const OPNAME = 'UGraphUnit.StandardDeviationBoxPlot';
var
  Loop : Integer;
  MaxVal : double;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := 'Monthly Streamflow (million m³)';
    TheChart.BottomAxis.Title.Caption := 'Months';
    TheChart.BottomAxis.Labels := true;

    with fmData.DataStorage.CurrentRecord do
    begin
      MaxVal:=0.0;
      for Loop := 1 to 12 do
         MaxVal:=Max(MaxVal,PMSD[Loop,1]);
      for Loop := 1 to 12 do
            ABoxChart.AddPoint(Loop,PMSD[Loop,1],PMSD[Loop,2],PMSD[Loop,3],
                                    PMSD[Loop,4],PMSD[Loop,5],PMSD[Loop,6],
                                    PMSD[Loop,7],X2D[Loop],0.25,MaxVal/60,aLeftAxis);

      ABoxChart.AddPoint(13,PSD[1],PSD[2],PSD[3],PSD[4],PSD[5],PSD[6],PSD[7],HSAD[1],
                         0.3,PSD[1]/120,aRightAxis);

      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 12 do
          begin
            LCommatextData := DoubleArrayToCommaText(PMSD[Loop],3);
            LCommatextData := IntToStr(Loop)+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;

    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function CreateBoxPlotSeries(TheChart : TChart): TBoxSeries;
const OPNAME = 'UGraphUnit.CreateBoxPlotSeries';
begin
  Result := nil;
  try
    Result := TBoxSeries.Create(TheChart);
    Result.ParentChart         := VCLTee.TeEngine.TCustomAxisPanel(TheChart);
    Result.Box.HorizSize       := 3;
    Result.Marks.Visible       := False;
    Result.Box.Color           := clRed;
    Result.Box.Pen.Color       := clRed;
    Result.ExtrOut.Style       := psCross;
    Result.ExtrOut.HorizSize   := 7;
    Result.ExtrOut.VertSize    := 1;
    Result.ExtrOut.Pen.Color   := clRed;
    Result.MildOut.HorizSize   := 7;
    Result.MildOut.VertSize    := 1;
    Result.MildOut.Style       := psCross;
    Result.MildOut.Brush.Color := clRed;
    Result.MildOut.Pen.Color   := clRed;
    Result.WhiskerPen.Color    := clRed;
    Result.WhiskerPen.Style    := psSolid;
    Result.MedianPen.Style     := psSolid;
    Result.MedianPen.Color     := clBlue;
    Result.XValues.DateTime    := False;
    Result.ExtrOut.Visible     := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function CapacityYieldBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 7
const OPNAME = 'UGraphUnit.CapacityYieldBoxPlot';
var
  LWidth               : integer;
  Loop1                : integer;
  Loop2                : Integer;
  LBoxValues           : TBoxPlotData;
  LBoxSeries           : TBoxSeries;
  LIQR                 : double;
  LLengthBoxValueArray : Integer;
  LMedianIndex         : Integer;
  LMedian              : double;
  LDataContainer       : TObjectList;
  LLineData            : TStringList;
  LMedianLine,
  LHistoricalLine      : TLineSeries;
  LTopMargin,
  LBottomMargin        : TChartShape;
begin
  Result := False;
  try
    LBoxValues      := TBoxPlotData.Create;
    LDataContainer  := TObjectList.Create;
    try
      LBoxValues.UseDateValue := False;

      RemoveAllSeries(TheChart);
      TheChart.Title.Text.Add(Title);
      TheChart.LeftAxis.Title.Caption := 'Capacity (% MAR)';
      TheChart.BottomAxis.Title.Caption := 'Yield (% MAR)';
      TheChart.LeftAxis.Automatic := false;
      TheChart.LeftAxis.Minimum := 0;
      TheChart.LeftAxis.Maximum := 500;
      TheChart.RightAxis.Visible    := False;
      TheChart.Legend.Visible       := True;

      LMedianLine := TLineSeries.Create(TheChart);
      LMedianLine.Title := 'Meadian';
      LMedianLine.SeriesColor := clGreen;
      LMedianLine.ParentChart := TheChart;

      LHistoricalLine := TLineSeries.Create(TheChart);
      LHistoricalLine.Title := 'Historical';
      LHistoricalLine.SeriesColor := clBlue;
      LHistoricalLine.ParentChart := TheChart;

      with fmData.DataStorage.CurrentRecord do
      begin
        for Loop1 := 1 to 20 do
        begin
          LLineData := TStringList.Create;
          LLineData.Add(FormatFloat('0.000',APAD[1,Loop1]));
          LDataContainer.Add(LLineData);
        end;

        for Loop1 := 1 to 20 do
        begin
          LLineData := TStringList(LDataContainer.Items[Loop1-1]);
          for Loop2 := 1 to 41 do
          begin
            LLineData.Add(FormatFloat('0.000',GPAD[Loop2,Loop1]));
            if MDXJD[Loop1] > 0.0 then
             LMedianLine.AddXY(APAD[1,Loop1],MDXJD[Loop1],'',clTeeColor);
            if HGPAD[Loop1] > 0.0 then
              LHistoricalLine.AddXY(APAD[1,Loop1],HGPAD[Loop1],'',clTeeColor);
          end;
        end;
      end;

      for Loop1 := 0 to LDataContainer.Count - 1 do
      begin
        LLineData := TStringList(LDataContainer.Items[Loop1]);
        LBoxValues.Populate(LLineData.CommaText);
        LLengthBoxValueArray := Length(LBoxValues.YValues);
        if LBoxValues.Populated then
        begin
          LBoxSeries := CreateBoxPlotSeries(TheChart);
          LBoxSeries.XValues.DateTime := False;
          LBoxSeries.MildOut.Visible  := False;
          LBoxSeries.ExtrOut.Visible  := False;
          LBoxSeries.ShowInLegend     := False;
          LBoxSeries.Position         := LBoxValues.XValueDouble;

          LBoxSeries.AddArray(LBoxValues.YValues);
          LBoxSeries.RecalcStats;
          LBoxSeries.UseCustomValues := True;

          LMedianIndex    := LLengthBoxValueArray div 2;
          if Odd(LLengthBoxValueArray) then
            LMedian := LBoxValues.YValues[LMedianIndex]
          else
            LMedian := 0.5 * (LBoxValues.YValues[LMedianIndex-1] + LBoxValues.YValues[LMedianIndex]);

          LBoxSeries.Median       := LMedian;
          LBoxSeries.Quartile1    := LBoxValues.PercValue(25);
          LBoxSeries.Quartile3    := LBoxValues.PercValue(75);

          LIQR  := LBoxValues.PercValue(95) - LBoxValues.PercValue(5);

          LBoxSeries.InnerFence1    := LBoxSeries.Quartile1 - (LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.InnerFence3    := LBoxSeries.Quartile3 + (LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.OuterFence1    := LBoxSeries.Quartile1 - (2 * LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.OuterFence3    := LBoxSeries.Quartile3 + (2 * LBoxSeries.WhiskerLength * LIQR);
          LBoxSeries.AdjacentPoint1 := LBoxValues.PercValue(5);
          LBoxSeries.AdjacentPoint3 := LBoxValues.PercValue(95);

          LWidth                 := Floor(LBoxSeries.WhiskerLength);
          LWidth                 := Max(1,LWidth);
          LTopMargin             := TChartShape.Create(TheChart);
          LTopMargin.VertAxis    := aLeftAxis;
          LTopMargin.Style       := chasHorizLine;
          LTopMargin.Pen.Color   := clRed;
          LTopMargin.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);
          LTopMargin.Active      := true;
          LTopMargin.ShowInLegend  := False;

          LTopMargin.X0          := LBoxSeries.Position - LWidth;
          LTopMargin.X1          := LBoxSeries.Position + LWidth;
          LTopMargin.Y0          := LBoxValues.PercValue(100) - LWidth;
          LTopMargin.Y1          := LBoxValues.PercValue(100) + LWidth;

          LBottomMargin             := TChartShape.Create(TheChart);
          LBottomMargin.VertAxis    := aLeftAxis;
          LBottomMargin.Style       := chasHorizLine;
          LBottomMargin.Pen.Color   := clRed;
          LBottomMargin.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);
          LBottomMargin.Active      := true;
          LBottomMargin.ShowInLegend  := False;

          LBottomMargin.X0          := LBoxSeries.Position - LWidth;
          LBottomMargin.X1          := LBoxSeries.Position + LWidth;
          LBottomMargin.Y0          := LBoxValues.PercValue(0) - LWidth;
          LBottomMargin.Y1          := LBoxValues.PercValue(0) + LWidth;
        end;
      end;

      VCL.Controls.TControl(TheChart).Refresh;
      Result := true;
    finally
      LBoxValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function MeanBoxPlot(TheChart : TChart; Title : String) : Boolean;
//GRAPH 6
const OPNAME = 'UGraphUnit.MeanBoxPlot';
var

  Loop                 : Integer;
  MaxVal               : double;
  LIndex               : Integer;
  LBoxValues           : TBoxPlotData;
  LLineSeriesList      : TObjectList;
  LLineSeries          : TLineSeries;
  LLineSeriesPerc      : double;
  LLineSeriesPercValue : double;
  LCommatextData       : string;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    ABoxChart := TBoxChart.Create;
    ABoxChart.TheChart := TheChart;
    ABoxChart.HideAllPoints;

    TheChart.Title.Text.Add(Title);
    TheChart.LeftAxis.Title.Caption := 'Monthly Streamflow (million m³)';
    TheChart.BottomAxis.Title.Caption := 'Months';
    TheChart.BottomAxis.Automatic := true;
    TheChart.BottomAxis.Inverted := false;
    TheChart.BottomAxis.Increment := 0;

    with fmData.DataStorage.CurrentRecord do
    begin
      MaxVal:=0.0;
      for Loop := 1 to 12 do
         MaxVal:=Max(MaxVal,PMAD[Loop,1]);
      for Loop := 1 to 12 do
        ABoxChart.AddPoint(Loop,PMAD[Loop,1],PMAD[Loop,2],PMAD[Loop,3],
                                PMAD[Loop,4],PMAD[Loop,5],PMAD[Loop,6],PMAD[Loop,7],X1D[Loop],
                                0.25,MaxVal/60, aLeftAxis);

      ABoxChart.AddPoint(13,PGD[1],PGD[2],PGD[3],PGD[4],PGD[5],PGD[6],PGD[7],GJAD[1],
                         0.3,PGD[1]/120,aRightAxis);

      if(fmData.DataStorage.BoxPlotDataSelection.Count > 0) then
      begin
        LBoxValues      := TBoxPlotData.Create;
        LLineSeriesList := TObjectList.Create(False);
        try
          LBoxValues.UseDateValue := False;
          for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count -1 do
          begin
            LLineSeries := TLineSeries.Create(TheChart);
            LLineSeries.ParentChart := TheChart;
            LLineSeriesList.Add(LLineSeries);
          end;

          for Loop := 1 to 12 do
          begin
            LCommatextData := DoubleArrayToCommaText(PMAD[Loop],3);
            LCommatextData := IntToStr(Loop)+','+LCommatextData;
            LBoxValues.Populate(LCommatextData);
            for LIndex := 0 to fmData.DataStorage.BoxPlotDataSelection.Count-1do
            begin
              LLineSeriesPerc      := StrToFloat(fmData.DataStorage.BoxPlotDataSelection[LIndex]);
              LLineSeriesPercValue := LBoxValues.PercValue(LLineSeriesPerc);
              LLineSeries := TLineSeries(LLineSeriesList.Items[LIndex]);
              LLineSeries.AddXY(LBoxValues.XValueDouble,LLineSeriesPercValue);
            end;
          end;
        finally
          LBoxValues.Free;
          LLineSeriesList.Free;
        end;
      end;
    end;
    VCL.Controls.TControl(TheChart).Refresh;
    ABoxChart.Destroy;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function ResidualStreamFlowCorrelation(TheChart : TChart; Title : String; PCorr : arrayT9_2_30; Lag, TimeSeries : Integer) : Boolean;
const OPNAME = 'UGraphUnit.ResidualStreamFlowCorrelation';
var
  Loop : Integer;
  Correlation,
  Partial : TBarSeries;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(Title);
    TheChart.Legend.Visible := true;

    TheChart.LeftAxis.Title.Caption := 'Correlation Coefficients';
    TheChart.LeftAxis.Automatic := false;
    TheChart.LeftAxis.Minimum := -1;
    TheChart.LeftAxis.Maximum := 1;
    TheChart.LeftAxis.Increment := 0.25;
    TheChart.BottomAxis.Title.Caption := 'Lag (Years)';

    Correlation := TBarSeries.Create(TheChart);
    Correlation.Title := 'Correlogram';
    Correlation.SeriesColor := clRed;
    Correlation.ParentChart := TheChart;
    Correlation.Marks.Visible := false;

    Partial := TBarSeries.Create(TheChart);
    Partial.Title := 'Residual Correlogram';
    Partial.BarBrush.Style := bsClear;
    Partial.ParentChart := TheChart;
    Partial.Marks.Visible := false;

    for Loop :=1 to Lag do
    begin
     Correlation.AddXY(Loop,PCorr[TimeSeries,1,Loop],'',clTeeColor);
     Partial.AddXY(Loop,PCorr[TimeSeries,2,Loop],'',clTeeColor);
    end;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function StreamFlowCorrelation(TheChart : TChart; Title : String; NCorr : ArrayT2_30; Lag : Integer) : Boolean;
const OPNAME = 'UGraphUnit.StreamFlowCorrelation';
var
  Loop : Integer;
  Correlation,
  Partial : TBarSeries;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(Title);
    TheChart.Legend.Visible := true;

    TheChart.LeftAxis.Title.Caption := 'Correlation Coefficients';
    TheChart.BottomAxis.Title.Caption := 'Lag (Years)';
    TheChart.LeftAxis.Automatic := false;
    TheChart.LeftAxis.Minimum := -1;
    TheChart.LeftAxis.Maximum := 1;
    TheChart.LeftAxis.Increment := 0.25;

    Correlation := TBarSeries.Create(TheChart);
    Correlation.Title := 'Correlogram';
    Correlation.SeriesColor := clRed;
    Correlation.ParentChart := TheChart;
    Correlation.Marks.Visible := false;

    Partial := TBarSeries.Create(TheChart);
    Partial.Title := 'Residual Correlogram';
    Partial.BarBrush.Style := bsClear;
    Partial.ParentChart := TheChart;
    Partial.Marks.Visible := false;

    for Loop :=1 to Lag do
    begin
     Correlation.AddXY(Loop,NCORR[1,Loop],'',clTeeColor);
     Partial.AddXY(Loop,NCORR[2,Loop],'',clTeeColor);
    end;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function StandardisedFlowData(TheChart : TChart; Title : String; RecordLength, ZeroCount, DefaultCurve : Integer;
                              Standardised : array1000F; NormalisedFlows : FittedArray) : Boolean;
{Output the standardised/normalised data to the specified chart}
const OPNAME = 'UGraphUnit.StandardisedFlowData';
var
  Loop, Loop2      : Integer;
  NormalisedLines  : Array[1..4] of TLineSeries;
  StandardisedLine : TLineSeries;
  xv, Xin          : extended;
begin
  Result := False;
  try
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(Title);

    TheChart.LeftAxis.Title.Caption := 'Streamflow standard deviation from the mean (million m³/a)';   
    TheChart.BottomAxis.Title.Caption := 'Standard Deviations from Median';

    StandardisedLine := TLineSeries.Create(TheChart);
    StandardisedLine.SeriesColor := clBlack;
    StandardisedLine.ParentChart := TheChart;

    for Loop := 1 to 4 do
    begin
      NormalisedLines[Loop] := TLineSeries.Create(TheChart);
      if Loop = DefaultCurve then
        NormalisedLines[Loop].SeriesColor := clRed
      else
        NormalisedLines[Loop].SeriesColor := clBlack;
      NormalisedLines[Loop].ParentChart := TheChart;
    end;

    for loop := 1 to (RecordLength - ZeroCount) do
    begin
      xv := Loop/(RecordLength-ZeroCount+1);
      xin := xnorm(xv);
      //StandardisedLine.AddXY(xin,Standardised[Loop],'',clTeeColor);

      for loop2 := 1 to 4 do
        NormalisedLines[loop2].AddXY(xin,NormalisedFlows[loop2-1,Loop-1],'',clTeeColor);
    end;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function NaturalFlowData(TheChart : TChart; LeftTitle,BottomTitle,TopTitle : String;RecordLength, ZeroCount,
  DefaultCurve : Integer; NaturalFlows : array1000F; Fitted : FittedArray; TransformedUnits : Boolean) : Boolean;
{Output the natural/fitted data to the specified chart}
const OPNAME = 'UGraphUnit.NaturalFlowData';
const  LineType : array[1..4] of string = ('LN2', 'LN3', 'SB4', 'SB3');
var
  Loop, Effective,
  Loop2            : Integer;
  Offset           : Extended;
  NaturalPoints    : TPointSeries;
  FittedLines      : Array[1..4] of TLineSeries;
begin
  Result := False;
  try
    //destroy any existing series on the chart
    RemoveAllSeries(TheChart);

    TheChart.Title.Text.Add(TopTitle);
    TheChart.Legend.Visible := true;

    TheChart.LeftAxis.Title.Caption := LeftTitle;//'Annual Flows (million m³/a)';
    if TransformedUnits then
    begin
      TheChart.BottomAxis.Automatic := true;
    end
    else
    begin
      TheChart.BottomAxis.Automatic := false;
      TheChart.BottomAxis.Minimum := 0;
      TheChart.BottomAxis.Maximum := 100;
    end;
    TheChart.BottomAxis.Title.Caption := BottomTitle;//'Exceedance Probability (%)';

    //create all the ones that I need
    NaturalPoints := TPointSeries.Create(TheChart);
    NaturalPoints.Title := 'Raw Data';
    NaturalPoints.Pointer.Brush.Color := clTeal;
    NaturalPoints.Pointer.HorizSize := 3;
    NaturalPoints.Pointer.VertSize := 3;
    NaturalPoints.Pointer.Style := psCircle;
    NaturalPoints.ParentChart := TheChart;

    for Loop := 1 to 4 do
    begin
      FittedLines[Loop] := TLineSeries.Create(TheChart);
      if Loop = DefaultCurve then
      begin
        FittedLines[Loop].SeriesColor := clRed;
        FittedLines[Loop].Title := LineType[Loop] + ' Selected';
      end
      else
      begin
        FittedLines[Loop].SeriesColor := clBlack;
        FittedLines[Loop].LinePen.Style := psDot;
        FittedLines[Loop].Title := LineType[Loop];
      end;
      FittedLines[Loop].ParentChart := TheChart;
    end;

    for loop := 1 to RecordLength do
    begin
      Offset := Loop/(RecordLength+1);
      if loop > ZeroCount then //    PVR
      begin
        Effective := Loop - ZeroCount;

        if TransformedUnits then
        begin
          Offset := XNorm(Offset);
          NaturalPoints.AddXY(Offset,NaturalFlows[Effective],'',clTeeColor);
          for Loop2 := 1 to 4 do
            FittedLines[Loop2].AddXY(Offset,Fitted[Loop2-1,Loop-1],'',clTeeColor);
        end
        else
        begin
          NaturalPoints.AddXY(Offset*100,NaturalFlows[Effective],'',clTeeColor);
          for Loop2 := 1 to 4 do
            FittedLines[Loop2].AddXY(Offset*100,Fitted[Loop2-1,Loop-1],'',clTeeColor);
        end;//if TransformedUnits

      end;//if loop >
    end;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
