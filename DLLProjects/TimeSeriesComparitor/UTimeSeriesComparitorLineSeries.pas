//
//
//  UNIT      : Contains TTimeSeriesComparitorLineSeries Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorLineSeries;

interface

uses
  DB,
  Classes,
  Graphics,
  Series;

type
  TLineWidth = (lmThick,lmThin);
  TTimeSeriesComparitorLineSeries = class(TLineSeries)
  protected
    FIdentifier    : integer;
    FAddedToChart  : boolean;
    FNodeProperty  : TStringList;
    FSeriesColor   : TColor;

    FLineWidth     : TLineWidth;
    FChartMaximum  : double;

    procedure SetChartMaximum(AChartMaximum: double);
    procedure ChangeSeriesColor(AColor: TColor);
    procedure SetLineWidth(ALineWidth: TLineWidth);
    procedure SetAddedToChart(AAddedToChart  : boolean);
    procedure SetNodeProperty(ANodeProperty  : TStringList);
    procedure SetSeriesName(AName: string);
    function GetSeriesName: string;
  public
    constructor Create(AOwner: TComponent); override;
    function PopulatePropertiesFromDataset(ADataSet: TDataSet): boolean;
    function PopulatePointsFromDataset(ADataSet: TDataSet): boolean;

    property Identifier     : integer     read FIdentifier    write FIdentifier;
    property AddedToChart   : boolean     read FAddedToChart  write SetAddedToChart;
    property NodeProperty   : TStringList read FNodeProperty  write SetNodeProperty;
    property LineWidth      : TLineWidth  read FLineWidth     write SetLineWidth;
    property SeriesColor    : TColor      read FSeriesColor   write ChangeSeriesColor;
    property ChartMaximum   : double      read FChartMaximum  write SetChartMaximum;
    property SeriesName     : string      read GetSeriesName  write SetSeriesName;
  end;
implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorLineSeries }

constructor TTimeSeriesComparitorLineSeries.Create(AOwner: TComponent);
const OPNAME = 'TTimeSeriesComparitorLineSeries.Create';
begin
  inherited Create(AOwner);
  try
    FIdentifier    := -1;
    FAddedToChart  := False;
    FNodeProperty  := TStringList.Create;
    FSeriesColor   := clNone;
    FLineWidth     := lmThick;
    FChartMaximum  := -1.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorLineSeries.PopulatePointsFromDataset(ADataSet: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorLineSeries.PopulatePointsFromDataset';
begin
  Result := False;
  try

    //Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorLineSeries.PopulatePropertiesFromDataset(ADataSet: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorLineSeries.PopulatePropertiesFromDataset';
begin
  Result := False;
  try
    if Assigned(ADataSet) and (ADataSet.Active) and (not ADataSet.Eof) and (not ADataSet.Bof) then
    begin
      FIdentifier         := ADataSet.FieldByName('SeriesID').AsInteger;
      FSeriesColor        := TColor(ADataSet.FieldByName('Color').AsInteger);
      FNodeProperty.Text  := Trim(ADataSet.FieldByName('SeriesProperty').AsString;
      AddedToChart        := True;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorLineSeries.SetAddedToChart(AAddedToChart: boolean);
const OPNAME = 'TTimeSeriesComparitorLineSeries.SetAddedToChart';
begin
  try
    if (AAddedToChart <> FAddedToChart) then
    begin
      FAddedToChart := AAddedToChart;
      if AAddedToChart then
        LineWidth     := lmThin
      else
        LineWidth     := lmThick;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorLineSeries.SetChartMaximum(AChartMaximum: double);
const OPNAME = 'TTimeSeriesComparitorLineSeries.SetChartMaximum';
begin
  try
    if ((Int(AChartMaximum * 1000000.0)) <> (Int(FChartMaximum * 1000000.0))) then
    begin
      FChartMaximum := AChartMaximum;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorLineSeries.SetLineWidth(ALineWidth: TLineWidth);
const OPNAME = 'TTimeSeriesComparitorLineSeries.SetLineWidth';
begin
  try
    if (ALineWidth <> Self.FLineWidth) then
    begin
      Self.FLineWidth := ALineWidth;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorLineSeries.ChangeSeriesColor(AColor: TColor);
const OPNAME = 'TTimeSeriesComparitorLineSeries.ChangeSeriesColor';
begin
  try
    if (AColor <> Self.SeriesColor) then
    begin
      Self.SeriesColor := AColor;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorLineSeries.SetNodeProperty(ANodeProperty: TStringList);
const OPNAME = 'TTimeSeriesComparitorLineSeries.SetNodeProperty';
begin
  try
    if Assigned(ANodeProperty) then
    begin
      FNodeProperty.Assign(ANodeProperty);
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTimeSeriesComparitorLineSeries.GetSeriesName: string;
const OPNAME = 'TTimeSeriesComparitorLineSeries.GetSeriesName';
begin
  Result := '';
  try
    Result := Self.Title;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTimeSeriesComparitorLineSeries.SetSeriesName(AName: string);
const OPNAME = 'TTimeSeriesComparitorLineSeries.SetSeriesName';
begin
  try
    Self.Title := AName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
