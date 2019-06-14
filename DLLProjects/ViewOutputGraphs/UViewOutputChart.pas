//
//
//  UNIT      : Contains TViewOutputChart Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/25
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewOutputChart;

interface

uses
  VCL.Graphics,
  Classes,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  DB,
  UAbstractComponent;

type
  TChartSeriesType = (cstLineLabels, cstLineDateTime, cstLineXY);
  TViewOutputChart = class(TAbstractChart)
  protected
    function AddNewSeries(AType: TChartSeriesType; ASeriesName: string): TCustomSeries; virtual;
    function AddSeriesDataLineLabels(ASeriesName: string; AXNames: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean; virtual;
    function AddSeriesDataLineDateTime(ASeriesName: string; AXNames: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean; virtual;
    function AddSeriesDataLineXY(ASeriesName: string; AXValues: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean; virtual;
    procedure PostProccessLineLabels(ASeries: TChartSeries); virtual;
    procedure PostProccessLineLineDateTime(ASeries: TChartSeries); virtual;
    procedure PostProccessLineXY(ASeries: TChartSeries); virtual;
    procedure SetSeriesPropertyLabels(ASeries: TChartSeries); virtual;
    procedure SetSeriesPropertyLineDateTime(ASeries: TChartSeries); virtual;
    procedure SetSeriesPropertyLineXY(ASeries: TChartSeries); virtual;
  public
    procedure GenerateExportGraphOutput(ABitmap: TBitmap); virtual;
    procedure CreateSeries(AType: TChartSeriesType; AGraphData: TDataSet); virtual;
  end;

implementation

uses
  SysUtils,
  Types,
  UErrorHandlingOperations;

procedure TViewOutputChart.GenerateExportGraphOutput(ABitmap: TBitmap);
const OPNAME = 'TViewOutputChart.GenerateExportGraphOutput';
var LUserRect : TRect;
    LPoint   : TPoint;
begin
  try
    // For the export of the graph, we only need to call the Draw method supplied
    // by the graph itself
    LPoint := TPoint.Create(0,0);
    ABitmap.Width := ClientWidth;
    ABitmap.Height := ClientHeight;
    LUserRect := TRect.Create(LPoint,ABitmap.width,ABitmap.Height);
    Draw(ABitmap.Canvas, LUserRect);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TViewOutputChart.CreateSeries(AType: TChartSeriesType; AGraphData: TDataSet);
const OPNAME = 'TViewOutputChart.CreateSeries';
var
  LLineSeries: TLineSeries;
  LEndOfData: boolean;
  LSeriesName: string;
  LXValues: TStringList;
begin
  try

    // Set ancestor properties.
    View3D := False;

    // Loop for all records of the current series.
    LXValues := TStringList.Create;
    try
      LEndOfData := AGraphData.EOF;
      while (not LEndOfData) do
      begin
        LXValues.CommaText := Trim(AGraphData.FieldByName('XValues').AsString);
        LSeriesName        := Trim(AGraphData.FieldByName('SeriesName').AsString);
        LLineSeries        := TLineSeries(AddNewSeries(AType, LSeriesName));
        case AType of
          cstLineLabels   : LEndOfData := AddSeriesDataLineLabels(  LSeriesName, LXValues, LLineSeries, AGraphData);
          cstLineDateTime : LEndOfData := AddSeriesDataLineDateTime(LSeriesName, LXValues, LLineSeries, AGraphData);
          cstLineXY       : LEndOfData := AddSeriesDataLineXY(      LSeriesName, LXValues, LLineSeries, AGraphData);
        else
          raise Exception.CreateFmt('Unknown chart type [%d].', [integer(AType)]);
        end;
      end;

    // Clean up.
    finally
      LXValues.Free;
    end;

    // Show the legend if there were more than one series.
    Legend.Visible := (CountActiveSeries > 1);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewOutputChart.AddNewSeries(AType: TChartSeriesType; ASeriesName: string): TCustomSeries;
const OPNAME = 'TViewOutputChart.AddNewSeries';
begin
  Result := nil;
  try
    Result := TLineSeries.Create(self);
    Result.Title := ASeriesName;
    if (AType in [cstLineDateTime]) then
    begin
      Result.XValues.DateTime := True;
    end;
    AddSeries(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewOutputChart.AddSeriesDataLineLabels(ASeriesName: string; AXNames: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TViewOutputChart.AddSeriesDataLineLabels';
var LIndex: integer;
begin
  Result := True;
  try
    while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString)) do
    begin
      for LIndex := 0 to AXNames.Count - 1 do
        if (not AGraphData.FieldByName(AXNames[LIndex]).IsNull) then
          ALineSeries.AddY(AGraphData.FieldByName(AXNames[LIndex]).AsFloat, AXNames[LIndex]);
      AGraphData.Next;
    end;
    SetSeriesPropertyLabels(TChartSeries(ALineSeries));
    PostProccessLineLabels(TChartSeries(ALineSeries));
    Result := AGraphData.EOF;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewOutputChart.AddSeriesDataLineDateTime(ASeriesName: string; AXNames: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TViewOutputChart.AddSeriesDataLineDateTime';
var LYear, LIndex: integer;
begin
  Result := True;
  try
    while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString)) do
    begin
      LYear := AGraphData.FieldByName('Year').AsInteger;
      for LIndex := 0 to AXNames.Count - 1 do
        ALineSeries.AddXY(EncodeDate(LYear, 1 + LIndex, 1), AGraphData.FieldByName(AXNames[LIndex]).AsFloat);
      AGraphData.Next;
    end;
    SetSeriesPropertyLineDateTime(TChartSeries(ALineSeries));
    PostProccessLineLineDateTime(TChartSeries(ALineSeries));
    Result := AGraphData.EOF;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewOutputChart.AddSeriesDataLineXY(ASeriesName: string; AXValues: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TViewOutputChart.AddSeriesDataLineXY';
var
  LIndex: integer;
  LYValues: TStringList;
begin
  Result := True;
  try
    LYValues := TStringList.Create;
    try
      while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString)) do
      begin
        LYValues.CommaText := Trim(AGraphData.FieldByName('YValues').AsString);
        for LIndex := 0 to LYValues.Count - 1 do
          if (AXValues[LIndex] <> '') and (LYValues[LIndex] <> '') then
            ALineSeries.AddXY(StrToFloat(AXValues[LIndex]), StrToFloat(LYValues[LIndex]));
        AGraphData.Next;
      end;
      SetSeriesPropertyLineXY(TChartSeries(ALineSeries));
      PostProccessLineXY(TChartSeries(ALineSeries));
      Result := AGraphData.EOF;
    finally
      LYValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewOutputChart.PostProccessLineLabels(ASeries: TChartSeries);
const OPNAME = 'TViewOutputChart.PostProccessLineLabels';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewOutputChart.PostProccessLineLineDateTime(ASeries: TChartSeries);
const OPNAME = 'TViewOutputChart.PostProccessLineLineDateTime';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewOutputChart.PostProccessLineXY(ASeries: TChartSeries);
const OPNAME = 'TViewOutputChart.PostProccessLineXY';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewOutputChart.SetSeriesPropertyLabels(ASeries: TChartSeries);
const OPNAME = 'TViewOutputChart.SetSeriesPropertyLabels';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewOutputChart.SetSeriesPropertyLineDateTime(ASeries: TChartSeries);
const OPNAME = 'TViewOutputChart.SetSeriesPropertyLineDateTime';
begin
  try
    Self.BottomAxis.LabelsAngle     := 90;
    Self.BottomAxis.TickLength      := 6;
    Self.BottomAxis.MinorTickCount  := 4;
    Self.BottomAxis.TitleSize       := 1;
    Self.LeftAxis.AxisValuesFormat  := '###0.00';
    Self.LeftAxis.TitleSize         := 1;
    Self.RightAxis.AxisValuesFormat := '###0.00';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewOutputChart.SetSeriesPropertyLineXY(ASeries: TChartSeries);
const OPNAME = 'TViewOutputChart.SetSeriesPropertyLineXY';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
