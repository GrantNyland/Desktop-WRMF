//
//
//  UNIT      : Contains TTimeSeriesComparitorNodeData Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/25
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorNodeData;

interface
uses
  TeEngine,
  classes,
  Contnrs,
  Controls,
  ComCtrls,
  UAbstractObject,
  UTimeSeriesComparitorTreeItemData;

type

  TTimeSeriesComparitorNodeData = class(TTimeSeriesComparitorTreeItemData)
  protected
    FAdded: boolean;
    FSeriesList: TObjectList;
    FNode: TTreeNode;
    procedure SetAdded(AAdded: boolean);
  public
    constructor Create(AAppModules: TAppModules);
    destructor Destroy; override;
    procedure AddSeries(ASeries: TChartSeries);
    procedure DeleteSeries;
    function SeriesCount: integer;
    function GetSeries(AIndex: integer): TChartSeries;
    property Added: boolean read FAdded write SetAdded;
    property Node: TTreeNode read FNode write FNode;
  end;

implementation


uses
  SysUtils,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorNodeData }

constructor TTimeSeriesComparitorNodeData.Create(AAppModules: TAppModules);
const OPNAME = 'TTimeSeriesComparitorNodeData.Create';
begin
  try
    inherited Create(AAppModules);
    FAdded := False;
    FSeriesList := TObjectList.Create(False);
    FNode := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TTimeSeriesComparitorNodeData.Destroy;
const OPNAME = 'TTimeSeriesComparitorNodeData.Destroy';
begin
  try
    FSeriesList.Free;
    FNode := nil;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNodeData.AddSeries(ASeries: TChartSeries);
const OPNAME = 'TTimeSeriesComparitorNodeData.AddSeries';
begin
  try
    FSeriesList.Add(ASeries);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNodeData.DeleteSeries;
const OPNAME = 'TTimeSeriesComparitorNodeData.DeleteSeries';
begin
  try
    FSeriesList.Clear;
    FAdded := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorNodeData.SeriesCount: integer;
const OPNAME = 'TTimeSeriesComparitorNodeData.SeriesCount';
begin
  Result := 0;
  try
    Result := FSeriesList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorNodeData.GetSeries(AIndex: integer): TChartSeries;
const OPNAME = 'TTimeSeriesComparitorNodeData.GetSeries';
begin
  Result := Nil;;
  try
    if (AIndex >= 0) and (AIndex < FSeriesList.Count) then
    begin
      if Assigned(FSeriesList.Items[AIndex]) then
        Result := TChartSeries(FSeriesList.Items[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorNodeData.SetAdded(AAdded: boolean);
const OPNAME = 'TTimeSeriesComparitorNodeData.SetAdded';
begin
  try
    FAdded := AAdded  and (SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
