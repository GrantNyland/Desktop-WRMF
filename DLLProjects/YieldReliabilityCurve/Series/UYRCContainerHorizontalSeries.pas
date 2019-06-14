//
//
//  UNIT      : Contains TYRCContainerHorizontalSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerHorizontalSeries;

interface

uses
  Classes,
  Contnrs,
  Graphics,
  Series,
  Chart,
  TeEngine,
  TeeShape,
  UYRCDataObject,
  UAbstractObject,
  UYRCContainerAbstractSeries;

type

  TYRCContainerHorizontalSeries = class(TYRCContainerAbstractSeries)
  protected
    function InitialiseSeries: boolean; override;
  public
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerHorizontalSeries }

function TYRCContainerHorizontalSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerHorizontalSeries.ShowSeries';
var
  LHorizontalSeries: TLineSeries;
  LCount: integer;
  LSeriesValues : TLoadCasesValues;
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);;
  try
    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      LSeriesValues   := TLoadCasesValues.Create;
      LSeriesValues.CopyValues(AYRCDataObject.HorizontalSeriesValues);
      FLoadCases.AddValues(LSeriesValues);
      for LCount := 0 to LSeriesValues.Count -1 do
      begin
        LHorizontalSeries := TLineSeries.Create(AOwner);
        FLineSeriesList.AddObject(IntToStr(LCount),LHorizontalSeries);
        AOwner.AddSeries(LHorizontalSeries);
      end;
      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerHorizontalSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerHorizontalSeries.InitialiseSeries';
var
  LCount: integer;
  LHorizontalSeries: TLineSeries;
  LXYZValue : TXYZValue;
  LSeriesValues : TLoadCasesValues;
begin
  Result := False;
  try
    LSeriesValues := FLoadCases.Values[0];
    for LCount := 0 to LSeriesValues.Count - 1 do
    begin
      LHorizontalSeries := TLineSeries(FLineSeriesList.Objects[LCount]);
      LXYZValue         := LSeriesValues.Values[LCount];

      if Assigned(LHorizontalSeries) and Assigned(LXYZValue) then
      begin
        LHorizontalSeries.ShowInLegend := False;
        LHorizontalSeries.AddXY(0.0,LXYZValue.YValue);
        LHorizontalSeries.AddXY(LXYZValue.XValue,LXYZValue.YValue);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
