//
//
//  UNIT      : Contains TYRCContainerAbstractSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerAbstractSeries;

interface

uses
  Classes,
  Contnrs,
  VCL.Graphics,
  VCLTee.Series,
  UYRCSeries,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  UAbstractYRCData,
  UAbstractObject,
  UAbstractComponent,
  UAbstractModelData,
  UYRCModelDataObject;

type
  TGetSeriesColour  = function (ASeriesIndex: integer): TColor of object;
  TYRCContainerAbstractSeries = class(TAbstractAppObject)
  protected
    FOwnerChart: TChart;
    procedure CreateMemberObjects; override;
    procedure RepopulateSeriesData;virtual;
    function YRCGraphDataObject:TAbstractYRCGraphDataObject;
    function ClearSeriesList: boolean; virtual;
    procedure BringSeriesToFront(ASeries: TChartSeries); virtual;
    function DisplayAllSeries:boolean; virtual;
    function HideAllSeries:boolean; virtual;
  public
    function CalcMaxYValue: double;virtual;
    function CalcMinYValue: double;virtual;
    function CreateSeries(AOwner: TChart): boolean; virtual;
    procedure ChartSelectionHasChanged; virtual;
  end;

implementation

uses
  Math,
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerAbstractSeries }

procedure TYRCContainerAbstractSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerAbstractSeries.CreateMemberObjects';
begin
  inherited;
  try
    FOwnerChart               := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractSeries.BringSeriesToFront(ASeries: TChartSeries);
const OPNAME = 'TYRCContainerAbstractSeries.BringSeriesToFront';
var
  LChart: TChart;
begin
  try
    if Assigned(ASeries) then
    begin
      LChart := TChart(ASeries.ParentChart);
      if Assigned(LChart) then
      begin
        ASeries.Active := False;
        LChart.RemoveSeries(ASeries);
        LChart.AddSeries(ASeries);
        ASeries.Active := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerAbstractSeries.CreateSeries';
begin
  Result := False;
  try
    if Assigned(AOwner) then
    begin
      FOwnerChart := AOwner;
      Result := ClearSeriesList;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.DisplayAllSeries: boolean;
const OPNAME = 'TYRCContainerAbstractSeries.DisplayAllSeries';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.HideAllSeries: boolean;
const OPNAME = 'TYRCContainerAbstractSeries.HideAllSeries';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.ClearSeriesList: boolean;
const OPNAME = 'TYRCContainerAbstractSeries.ClearSeriesList';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.YRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYRCContainerAbstractSeries.YRCGraphDataObject';
begin
  Result := nil;
  try
    Result := TYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerAbstractSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerAbstractSeries.ChartSelectionHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerAbstractSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerAbstractSeries.RepopulateSeriesData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.CalcMaxYValue: double;
const OPNAME = 'TYRCContainerAbstractSeries.CalcMaxYValue';
begin
  Result := 0.0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerAbstractSeries.CalcMinYValue: double;
const OPNAME = 'TYRCContainerAbstractSeries.CalcMinYValue';
begin
  Result := 0.0;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
