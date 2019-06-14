//
//
//  UNIT      : Contains TViewHydrologyDataChart Class
//  AUTHOR    : Sam Dhlamini (Arivia.Kom)
//  DATE      : 2002/03/25
//  COPYRIGHT : Copyright © 2002 DWAF
//
//

unit UHydrologyViewDataChart;

interface

uses
  Graphics,
  Classes,
  Series,
  Chart,
  TeEngine,
  DB,
  UAbstractComponent;

type
  TChartSeriesType = (cstLineLabels, cstLineDateTime, cstLineXY);
  TViewHydrologyDataChart = class ( TAbstractChart )
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
  UErrorHandlingOperations;

procedure TViewHydrologyDataChart.GenerateExportGraphOutput(ABitmap: TBitmap);
const OPNAME = 'TViewHydrologyDataChart.GenerateExportGraphOutput';
begin
  try

    // For the export of the graph, we only need to call the Draw method supplied
    // by the graph itself
    ABitmap.Width := ClientWidth;
    ABitmap.Height := ClientHeight;
    Draw(ABitmap.Canvas, Bounds(0, 0, ClientWidth, ClientHeight));

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.CreateSeries(AType: TChartSeriesType; AGraphData: TDataSet);
const OPNAME = 'TViewHydrologyDataChart.CreateSeries';
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
        LXValues.CommaText := Trim(AGraphData.FieldByName('XValues').AsString));
        LSeriesName        := Trim(AGraphData.FieldByName('SeriesName').AsString));
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
    Legend.Visible := (SeriesList.CountActive > 1);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewHydrologyDataChart.AddNewSeries(AType: TChartSeriesType; ASeriesName: string): TCustomSeries;
const OPNAME = 'TViewHydrologyDataChart.AddNewSeries';
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

function TViewHydrologyDataChart.AddSeriesDataLineLabels(ASeriesName: string; AXNames: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TViewHydrologyDataChart.AddSeriesDataLineLabels';
var LIndex: integer;
begin
  Result := True;
  try
    while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString))) do
    begin
      for LIndex := 0 to AXNames.Count - 1 do
        if (not AGraphData.FieldByName(AXNames[LIndex]).IsNull) then
          ALineSeries.AddY(AGraphData.FieldByName(AXNames[LIndex]).AsFloat, AXNames[LIndex]);
      AGraphData.Next;
    end;
    SetSeriesPropertyLabels(ALineSeries);
    PostProccessLineLabels(ALineSeries);
    Result := AGraphData.EOF;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewHydrologyDataChart.AddSeriesDataLineDateTime(ASeriesName: string; AXNames: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TViewHydrologyDataChart.AddSeriesDataLineDateTime';
var LYear, LIndex: integer;
begin
  Result := True;
  try
    while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString))) do
    begin
      LYear := AGraphData.FieldByName('Year').AsInteger;
      for LIndex := 0 to AXNames.Count - 1 do
        ALineSeries.AddXY(EncodeDate(LYear, 1 + LIndex, 1), AGraphData.FieldByName(AXNames[LIndex]).AsFloat);
      AGraphData.Next;
    end;
    SetSeriesPropertyLineDateTime(ALineSeries);
    PostProccessLineLineDateTime(ALineSeries);
    Result := AGraphData.EOF;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewHydrologyDataChart.AddSeriesDataLineXY(ASeriesName: string; AXValues: TStringList; ALineSeries: TLineSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TViewHydrologyDataChart.AddSeriesDataLineXY';
var
  LIndex: integer;
  LYValues: TStringList;
begin
  Result := True;
  try
    LYValues := TStringList.Create;
    try
      while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString))) do
      begin
        LYValues.CommaText := Trim(AGraphData.FieldByName('YValues').AsString));
        for LIndex := 0 to LYValues.Count - 1 do
          if (AXValues[LIndex] <> '') and (LYValues[LIndex] <> '') then
            ALineSeries.AddXY(StrToFloat(AXValues[LIndex]), StrToFloat(LYValues[LIndex]));
        AGraphData.Next;
      end;
      SetSeriesPropertyLineXY(ALineSeries);
      PostProccessLineXY(ALineSeries);
      Result := AGraphData.EOF;
    finally
      LYValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.PostProccessLineLabels(ASeries: TChartSeries);
const OPNAME = 'TViewHydrologyDataChart.PostProccessLineLabels';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.PostProccessLineLineDateTime(ASeries: TChartSeries);
const OPNAME = 'TViewHydrologyDataChart.PostProccessLineLineDateTime';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.PostProccessLineXY(ASeries: TChartSeries);
const OPNAME = 'TViewHydrologyDataChart.PostProccessLineXY';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.SetSeriesPropertyLabels(ASeries: TChartSeries);
const OPNAME = 'TViewHydrologyDataChart.SetSeriesPropertyLabels';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.SetSeriesPropertyLineDateTime(ASeries: TChartSeries);
const OPNAME = 'TViewHydrologyDataChart.SetSeriesPropertyLineDateTime';
begin
  try
    Self.BottomAxis.LabelsAngle := 90;
    Self.BottomAxis.TickLength  := 6;
    Self.BottomAxis.MinorTickCount  := 4;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewHydrologyDataChart.SetSeriesPropertyLineXY(ASeries: TChartSeries);
const OPNAME = 'TViewHydrologyDataChart.SetSeriesPropertyLineXY';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
