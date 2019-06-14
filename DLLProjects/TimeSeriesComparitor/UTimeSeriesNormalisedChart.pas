//
//
//  UNIT      : Contains TTimeSeriesNormalisedChart Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 2002/03/25
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTimeSeriesNormalisedChart;

interface

uses
  Classes,
  TeEngine,
  UTimeSeriesComparitorChart;
  //UTimeSeriesComparitorData;

type
  TLineMode = (lmThick,lmThin);
  TTimeSeriesNormalisedChart = class(TTimeSeriesComparitorChart)
  protected
    FChartName: string;
    FChartInView: boolean;
    FCurrentData: TTimeSeriesComparitorChartData;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure PostProccessLineLabels(ASeries: TTimeSeriesComparitorSeriesData); override;
    procedure PostProccessLineLineDateTime(ASeries: TTimeSeriesComparitorSeriesData); override;
    procedure PostProccessLineXY(ASeries: TTimeSeriesComparitorSeriesData); override;
    procedure SetSeriesPropertyLabels(ASeries: TTimeSeriesComparitorSeriesData); override;
    procedure SetSeriesPropertyLineDateTime(ASeries: TTimeSeriesComparitorSeriesData); override;
    procedure SetSeriesPropertyLineXY(ASeries: TTimeSeriesComparitorSeriesData); override;
    procedure AddCurrentSeries(ASeries: TTimeSeriesComparitorSeriesData);
  public
    function AddSeries: boolean;
    function DeleteSeries: boolean;
    procedure RestoreChart;
    procedure RedrawCurrentSeries(ALineMode: TLineMode);
    property Data: TTimeSeriesComparitorChartData read FCurrentData write FCurrentData;
    property ChartName: string read FChartName write FChartName;
    property ChartInView: boolean read FChartInView write FChartInView;
  end;

implementation

uses
  Math,
  Series,
  SysUtils,
  UErrorHandlingOperations;

procedure TTimeSeriesNormalisedChart.CreateMemberObjects;
const OPNAME = 'TTimeSeriesNormalisedChart.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCurrentData := nil;
    ChartName    := '';
    Self.View3D  := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesNormalisedChart.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FCurrentData := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.AddCurrentSeries(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.AddCurrentSeries';
begin
  try
    if Assigned(FCurrentData) and Assigned(ASeries) then
    begin
      FCurrentData.AddSeries(ASeries);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.PostProccessLineLabels(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.PostProccessLineLabels';
var
  LCount: integer;
  LMaxValue: double;
begin
  inherited;
  try
    if Assigned(ASeries) then
    begin
      LMaxValue := Abs(Max(Abs(ASeries.YValues.MaxValue),Abs(ASeries.YValues.MinValue)));
      if (LMaxValue > 0.0) then
      begin
        for LCount := 0 to ASeries.YValues.Count -1 do
          ASeries.YValues.Value[LCount] := (ASeries.YValues.Value[LCount] / LMaxValue) * 100.0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.PostProccessLineLineDateTime(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.PostProccessLineLineDateTime';
var
  LCount: integer;
  LMaxValue: double;
begin
  inherited;
  try
    if Assigned(ASeries) then
    begin
      LMaxValue := Abs(Max(Abs(ASeries.YValues.MaxValue),Abs(ASeries.YValues.MinValue)));
      if (LMaxValue > 0.0) then
      begin
        for LCount := 0 to ASeries.YValues.Count -1 do
          ASeries.YValues.Value[LCount] := (ASeries.YValues.Value[LCount] / LMaxValue) * 100.0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.PostProccessLineXY(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.PostProccessLineXY';
var
  LCount: integer;
  LMaxValue: double;
begin
  inherited;
  try
    if Assigned(ASeries) then
    begin
      LMaxValue := Abs(Max(Abs(ASeries.YValues.MaxValue),Abs(ASeries.YValues.MinValue)));
      if (LMaxValue > 0.0) then
      begin
        for LCount := 0 to ASeries.YValues.Count -1 do
          ASeries.YValues.Value[LCount] := (ASeries.YValues.Value[LCount] / LMaxValue) * 100.0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.SetSeriesPropertyLabels(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.SetSeriesPropertyLabels';
begin
  try
    AddCurrentSeries(ASeries);
    if Assigned(ASeries) then
    begin
      ASeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.SetSeriesPropertyLineDateTime(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.SetSeriesPropertyLineDateTime';
begin
  try
    AddCurrentSeries(ASeries);
    if Assigned(ASeries) then
    begin
      ASeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.SetSeriesPropertyLineXY(ASeries: TTimeSeriesComparitorSeriesData);
const OPNAME = 'TTimeSeriesNormalisedChart.SetSeriesPropertyLineXY';
begin
  try
    AddCurrentSeries(ASeries);
    if Assigned(ASeries) then
    begin
      ASeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesNormalisedChart.AddSeries: boolean;
const OPNAME = 'TTimeSeriesNormalisedChart.AddSeries';
begin
  Result := False;
  try
    if Assigned(FCurrentData) then
    begin
     FCurrentData.Added := True;
     Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesNormalisedChart.DeleteSeries: boolean;
const OPNAME = 'TTimeSeriesNormalisedChart.DeleteSeries';
var
  LCount: integer;
  LSeries: TTimeSeriesComparitorSeriesData;
begin
  Result := False;
  try
    if Assigned(FCurrentData) then
    begin
      for LCount := 0 to FCurrentData.SeriesCount -1 do
      begin
        LSeries := FCurrentData.GetSeries(LCount);
        if Assigned(LSeries) then
        begin
          LSeries.Active := False;
          Self.RemoveSeries(LSeries);
          LSeries.Free;
        end;
      end;
      FCurrentData.DeleteSeries;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.RedrawCurrentSeries(ALineMode: TLineMode);
const OPNAME = 'TTimeSeriesNormalisedChart.RedrawCurrentSeries';
var
 LCount: integer;
 LChartSeries: TTimeSeriesComparitorSeriesData;
 LLineSeries: TLineSeries;
begin
  try
    if Assigned(FCurrentData) then
    begin
      for LCount := 0 to FCurrentData.SeriesCount -1 do
      begin
        LChartSeries := FCurrentData.GetSeries(LCount);
        if (LChartSeries is TLineSeries) then
        begin
          LLineSeries := TLineSeries(LChartSeries);
          LLineSeries.Active := False;
          case ALineMode of
            lmThick: LLineSeries.LinePen.Width := 2;
            lmThin:  LLineSeries.LinePen.Width := 1;
          end;//case
          LLineSeries.ParentChart := Self;
          LLineSeries.Active := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesNormalisedChart.RestoreChart;
const OPNAME = 'TTimeSeriesNormalisedChart.RestoreChart';
begin
  try
    Self.RemoveAllSeries;
    //Self.ChartName := '';
    Self.FChartInView := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
