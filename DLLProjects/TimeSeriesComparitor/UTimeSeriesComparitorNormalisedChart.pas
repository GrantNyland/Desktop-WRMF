//
//
//  UNIT      : Contains TTimeSeriesComparitorNormalisedChart Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 2002/03/25
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTimeSeriesComparitorNormalisedChart;

interface

uses
  Classes,
  ComCtrls,
  TeEngine,
  UTimeSeriesComparitorChart,
  UTimeSeriesComparitorNodeData,
  UTimeSeriesComparitorLineSeries,
  UTimeSeriesComparitorSeriesManager;

type
  TLineMode = (lmThick,lmThin);
  TTimeSeriesComparitorNormalisedChart = class(TTimeSeriesComparitorChart)
  protected
    FChartName    : string;
    FChartInView  : boolean;
    FCurrentData  : TTimeSeriesComparitorNodeData;
    FSeriesManager: TimeSeriesComparitorSeriesManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure PostProccessLineLabels(ASeries: TChartSeries); override;
    procedure PostProccessLineLineDateTime(ASeries: TChartSeries); override;
    procedure PostProccessLineXY(ASeries: TChartSeries); override;
    procedure SetSeriesPropertyLabels(ASeries: TChartSeries); override;
    procedure SetSeriesPropertyLineDateTime(ASeries: TChartSeries); override;
    procedure SetSeriesPropertyLineXY(ASeries: TChartSeries); override;
    procedure AddCurrentSeries(ASeries: TChartSeries);
  public
    function AddSeries: boolean;
    function DeleteSeries: boolean;
    function CurrentSeries: TTimeSeriesComparitorLineSeries;

    procedure RestoreChart;
    procedure RedrawCurrentSeries(ALineMode: TLineMode);

    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);

    property Data: TTimeSeriesComparitorNodeData read FCurrentData write FCurrentData;
    property ChartName: string read FChartName write FChartName;
    property ChartInView: boolean read FChartInView write FChartInView;
  end;

implementation

uses
  Math,
  Series,
  SysUtils,
  UErrorHandlingOperations;

procedure TTimeSeriesComparitorNormalisedChart.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSeriesManager := TimeSeriesComparitorSeriesManager.Create(FAppModules);
    //FDataManager   := TTimeSeriesComparitorChartDataManager.Create;
    FCurrentData   := nil;
    ChartName      := '';
    Self.View3D    := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNormalisedChart.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FCurrentData := nil;
    FreeAndNil(FSeriesManager);
    //FreeAndNil(FDataManager);
    
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNormalisedChart.AddCurrentSeries(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.AddCurrentSeries';
begin
  try
    if Assigned(FCurrentData) and Assigned(ASeries) then
    begin
      FCurrentData.AddSeries(ASeries);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNormalisedChart.PostProccessLineLabels(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.PostProccessLineLabels';
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

procedure TTimeSeriesComparitorNormalisedChart.PostProccessLineLineDateTime(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.PostProccessLineLineDateTime';
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

procedure TTimeSeriesComparitorNormalisedChart.PostProccessLineXY(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.PostProccessLineXY';
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

procedure TTimeSeriesComparitorNormalisedChart.SetSeriesPropertyLabels(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.SetSeriesPropertyLabels';
begin
  try
    AddCurrentSeries(ASeries);
    if Assigned(ASeries) then
    begin
      ASeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNormalisedChart.SetSeriesPropertyLineDateTime(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.SetSeriesPropertyLineDateTime';
begin
  try
    AddCurrentSeries(ASeries);
    if Assigned(ASeries) then
    begin
      ASeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNormalisedChart.SetSeriesPropertyLineXY(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.SetSeriesPropertyLineXY';
begin
  try
    AddCurrentSeries(ASeries);
    if Assigned(ASeries) then
    begin
      ASeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorNormalisedChart.AddSeries: boolean;
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.AddSeries';
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

function TTimeSeriesComparitorNormalisedChart.DeleteSeries: boolean;
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.DeleteSeries';
var
  LCount: integer;
  LSeries: TChartSeries;
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

procedure TTimeSeriesComparitorNormalisedChart.RedrawCurrentSeries(ALineMode: TLineMode);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.RedrawCurrentSeries';
var
 LCount: integer;
 LChartSeries: TChartSeries;
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

procedure TTimeSeriesComparitorNormalisedChart.RestoreChart;
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.RestoreChart';
begin
  try
    Self.RemoveAllSeries;
    //Self.ChartName := '';
    Self.FChartInView := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNormalisedChart.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.DoTreeNodeAboutToChange';
begin
{        if Assigned(FViewManager.CurrentChart.Data) then
        begin
          if(FViewManager.CurrentChart.Data.SeriesCount > 0) then
          begin
            if not FViewManager.CurrentChart.Data.Added then
              FViewManager.CurrentChart.DeleteSeries
            else
              FViewManager.CurrentChart.RedrawCurrentSeries(lmThin);
          end;
        end;
        FViewManager.CurrentChart.Data := nil;
}
end;

procedure TTimeSeriesComparitorNormalisedChart.DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.DoTreeNodeHasChanged';
{var
  LViewDataTreeNodeData: TViewDataTreeNodeData;
  LTimeComparitorData: TTimeSeriesComparitorNodeData;
}
begin
{
    if Assigned(FTreeView.Selected) then
    begin
      if Assigned(FTreeView.Selected.Data) then
      begin
        if Assigned(FViewManager.CurrentChart()) then
        begin
          LViewDataTreeNodeData := TViewDataTreeNodeData(FTreeView.Selected.Data);
          LTimeComparitorData   := TTimeSeriesComparitorNodeData(LViewDataTreeNodeData.Data);
          FViewManager.CurrentChart.Data := LTimeComparitorData;
        end;
      end;
    end;

    if Assigned(FViewManager.CurrentChart()) and Assigned(FViewManager.CurrentChart.Data) then
    begin
      if not FViewManager.CurrentChart.Data.Added then
        inherited DoTreeNodeHasChanged(ASender,ANode);

      FViewManager.CurrentChart.RedrawCurrentSeries(lmThick);
    end;
 }
end;

function TTimeSeriesComparitorNormalisedChart.CurrentSeries: TTimeSeriesComparitorLineSeries;
const OPNAME = 'TTimeSeriesComparitorNormalisedChart.CurrentSeries';
begin
  Result := nil;
  try
    Result := FSeriesManager.CurrentSeries;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
