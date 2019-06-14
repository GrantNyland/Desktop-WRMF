//
//
//  UNIT      : Contains TYRCContainerPointsSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerPointsSeries;

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

  TYRCContainerPointsSeries = class(TYRCContainerAbstractSeries)
  protected
    function InitialiseSeries: boolean; override;
  public
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerPointsSeries }

function TYRCContainerPointsSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerPointsSeries.ShowSeries';
var
  LCount: integer;
  LPointsSeries: TPointSeries;
  LSeriesValues,
  LCurrentSeriesValues : TLoadCasesValues;
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);;
  try
    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      for LCount := 0 to AYRCDataObject.TargetDraftSeriesValues.Count -1 do
      begin
        LCurrentSeriesValues := AYRCDataObject.TargetDraftSeriesValues.Values[LCount];
        if Assigned(LCurrentSeriesValues) then
        begin
          LSeriesValues   := TLoadCasesValues.Create;
          LSeriesValues.CopyValues(LCurrentSeriesValues);
          FLoadCases.AddValues(LSeriesValues);
          LPointsSeries := TPointSeries.Create(AOwner);
          FLineSeriesList.AddObject(IntToStr(LCount),LPointsSeries);
          AOwner.AddSeries(LPointsSeries);
        end;
      end;
      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerPointsSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerPointsSeries.InitialiseSeries';
var
  LCount,
  LSeriesPos,
  LValuesCount: integer;
  LPointsSeries: TPointSeries;
  LXYZValue : TXYZValue;
  LSeriesValues : TLoadCasesValues;
begin
  Result := False;
  try
    LSeriesPos := -1;

    for LValuesCount := 0 to FLoadCases.Count -1 do
    begin
      LSeriesValues := FLoadCases.Values[LValuesCount];
      if Assigned(LSeriesValues) then
      begin
        LSeriesPos := LSeriesPos + 1;
        LPointsSeries := TPointSeries(FLineSeriesList.Objects[LSeriesPos]);
        if Assigned(LPointsSeries)then
        begin
          LPointsSeries.Marks.ArrowLength := 0;
          LPointsSeries.Marks.Visible := False;
          LPointsSeries.PercentFormat := '##0.##,%';
          //LPointsSeries.SeriesColor := clGreen
          LPointsSeries.Pointer.InflateMargins := True;
          LPointsSeries.Pointer.Style := psDiagCross;
          LPointsSeries.Pointer.Visible := True;
          LPointsSeries.XValues.DateTime := False;
          LPointsSeries.XValues.Name := 'X';
          LPointsSeries.XValues.Multiplier := 1;
          LPointsSeries.XValues.Order := loAscending;
          LPointsSeries.YValues.DateTime := False ;
          LPointsSeries.YValues.Name := 'Y';
          LPointsSeries.YValues.Multiplier := 1;
          LPointsSeries.YValues.Order := loNone;
          LPointsSeries.ShowInLegend := False;

          for LCount := 0 to LSeriesValues.Count - 1 do
          begin
            LXYZValue     := LSeriesValues.Values[LCount];
            if Assigned(LXYZValue) then
            begin
              LPointsSeries.AddXY(LXYZValue.XValue,LXYZValue.YValue);
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
