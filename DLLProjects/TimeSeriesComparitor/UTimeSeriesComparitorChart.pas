//
//
//  UNIT      : Contains TTimeSeriesComparitorChart Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/25
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTimeSeriesComparitorChart;

interface

uses
  Graphics,
  Classes,
  Series,
  Chart,
  TeEngine,
  DB,
  UAbstractComponent,
  UTimeSeriesComparitorSeriesList,
  UTimeSeriesComparitorLinkClasses;

type
  TTimeSeriesComparitorChart = class(TAbstractChart)
  protected
    //function AddNewSeries(AType: TChartSeriesType; ASeriesName: string): TCustomSeries; virtual;
    function AddSeriesDataLineLabels(ASeriesName: string; AXNames: TStringList; ALineSeriesData: TTimeSeriesComparitorSeries; AGraphData: TDataSet): boolean; virtual;
    function AddSeriesDataLineDateTime(ASeriesName: string; AXNames: TStringList; ALineSeriesData: TTimeSeriesComparitorSeries; AGraphData: TDataSet): boolean; virtual;
    function AddSeriesDataLineXY(ASeriesName: string; AXValues: TStringList; ALineSeriesData: TTimeSeriesComparitorSeries; AGraphData: TDataSet): boolean; virtual;
    procedure PostProccessLineLabels(ASeries: TTimeSeriesComparitorSeries); virtual;
    procedure PostProccessLineLineDateTime(ASeries: TTimeSeriesComparitorSeries); virtual;
    procedure PostProccessLineXY(ASeries: TTimeSeriesComparitorSeries); virtual;
    procedure SetSeriesPropertyLabels(ASeries: TTimeSeriesComparitorSeries); virtual;
    procedure SetSeriesPropertyLineDateTime(ASeries: TTimeSeriesComparitorSeries); virtual;
    procedure SetSeriesPropertyLineXY(ASeries: TTimeSeriesComparitorSeries); virtual;
  public
    procedure GenerateExportGraphOutput(ABitmap: TBitmap); virtual;
    property OnClick;
    //procedure CreateSeries(AType: TChartSeriesType; AGraphData: TDataSet); virtual;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TTimeSeriesComparitorChart.GenerateExportGraphOutput(ABitmap: TBitmap);
const OPNAME = 'TTimeSeriesComparitorChart.GenerateExportGraphOutput';
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

{procedure TTimeSeriesComparitorChart.CreateSeries(AType: TChartSeriesType; AGraphData: TDataSet);
const OPNAME = 'TTimeSeriesComparitorChart.CreateSeries';
var
  LLineSeries: TTimeSeriesComparitorSeries;
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
        LSeriesName := Trim(AGraphData.FieldByName('SeriesName').AsString);
        LLineSeries := TTimeSeriesComparitorSeries(AddNewSeries(AType, LSeriesName));
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
end;}

{function TTimeSeriesComparitorChart.AddNewSeries(AType: TChartSeriesType; ASeriesName: string): TCustomSeries;
const OPNAME = 'TTimeSeriesComparitorChart.AddNewSeries';
begin
  Result := nil;
  try
    Result := TTimeSeriesComparitorSeries.Create;
    Result.Title := ASeriesName;
    if (AType in [cstLineDateTime]) then
    begin
      Result.XValues.DateTime := True;
    end;
    AddSeries(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TTimeSeriesComparitorChart.AddSeriesDataLineLabels(ASeriesName: string; AXNames: TStringList; ALineSeriesData: TTimeSeriesComparitorSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.AddSeriesDataLineLabels';
var LIndex: integer;
begin
  Result := True;
  try
    while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString)) do
    begin
      for LIndex := 0 to AXNames.Count - 1 do
        if (not AGraphData.FieldByName(AXNames[LIndex]).IsNull) then
          ALineSeriesData.LineSeries.AddY(AGraphData.FieldByName(AXNames[LIndex]).AsFloat, AXNames[LIndex]);
      AGraphData.Next;
    end;
    SetSeriesPropertyLabels(ALineSeriesData);
    PostProccessLineLabels(ALineSeriesData);
    Result := AGraphData.EOF;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorChart.AddSeriesDataLineDateTime(ASeriesName: string; AXNames: TStringList; ALineSeriesData: TTimeSeriesComparitorSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.AddSeriesDataLineDateTime';
var LYear, LIndex: integer;
begin
  Result := True;
  try
    while (not AGraphData.EOF) and (ASeriesName = Trim(AGraphData.FieldByName('SeriesName').AsString)) do
    begin
      LYear := AGraphData.FieldByName('Year').AsInteger;
      for LIndex := 0 to AXNames.Count - 1 do
        ALineSeriesData.LineSeries.AddXY(EncodeDate(LYear, 1 + LIndex, 1), AGraphData.FieldByName(AXNames[LIndex]).AsFloat);
      AGraphData.Next;
    end;
    SetSeriesPropertyLineDateTime(ALineSeriesData);
    PostProccessLineLineDateTime(ALineSeriesData);
    Result := AGraphData.EOF;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorChart.AddSeriesDataLineXY(ASeriesName: string; AXValues: TStringList; ALineSeriesData: TTimeSeriesComparitorSeries; AGraphData: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorChart.AddSeriesDataLineXY';
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
            ALineSeriesData.LineSeries.AddXY(StrToFloat(AXValues[LIndex]), StrToFloat(LYValues[LIndex]));
        AGraphData.Next;
      end;
      SetSeriesPropertyLineXY(ALineSeriesData);
      PostProccessLineXY(ALineSeriesData);
      Result := AGraphData.EOF;
    finally
      LYValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.PostProccessLineLabels(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.PostProccessLineLabels';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.PostProccessLineLineDateTime(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.PostProccessLineLineDateTime';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.PostProccessLineXY(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.PostProccessLineXY';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetSeriesPropertyLabels(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.SetSeriesPropertyLabels';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetSeriesPropertyLineDateTime(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.SetSeriesPropertyLineDateTime';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorChart.SetSeriesPropertyLineXY(ASeries: TTimeSeriesComparitorSeries);
const OPNAME = 'TTimeSeriesComparitorChart.SetSeriesPropertyLineXY';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
