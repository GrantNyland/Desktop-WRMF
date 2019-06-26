//
//
//  UNIT      : Contains TYRCContainerPlaneSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerPlaneSeries;

interface

uses
  Classes,
  Contnrs,
  VCL.Graphics,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  UYRCSeries,
  UAbstractObject,
  UAbstractYRCData,
  UYRCContainerAbstractLineSeries,
  UAbstractModelData;

type

  TYRCContainerPlaneSeries = class(TYRCContainerAbstractLineSeries)
  protected
    procedure ConfigureSeries(ALineSeries: TYRCLineSeries);
    procedure PopulateLineSeriesWithPlane(ASeriesIndex: integer;ALineSeries: TYRCLineSeries; APlane:TAbstractYRCPlane);override;
    procedure RepopulateSeriesData;override;
    procedure ShowSelectedPlane;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure ChartSelectionHasChanged; override;
  end;

implementation

uses
  Math,
  SysUtils,
  UYRCContainerAbstractSeries,
  UErrorHandlingOperations;


{ TYRCContainerPlaneSeries }
function TYRCContainerPlaneSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerPlaneSeries.CreateSeries';
var
  //LCount: integer;
  LPlaneIndex: integer;
  LPlane:TAbstractYRCPlane;
  LPlaneLineSeries: TYRCLineSeries;
begin
  Result := inherited CreateSeries(AOwner);
  try
    if Result then
    begin
      //FYieldFormatStr := YRCGraphDataObject.YRCLanguageStrings.YieldFormatStr;
      LPlaneIndex := 0;
      LPlane := TAbstractYRCPlane(YRCGraphDataObject.YRCPlane[LPlaneIndex]);
      while Assigned(LPlane) do
      begin
        LPlaneLineSeries := TYRCLineSeries.Create(AOwner);
        LPlaneLineSeries.Title        := Format('%d years', [LPlane.PlaneYears]);
        LPlaneLineSeries.PlaneIndex := LPlaneIndex;
        LPlaneLineSeries.TargetDraftIndex := -1;
        ConfigureSeries(LPlaneLineSeries);
        FLineSeriesList.AddObject(IntToStr(LPlaneIndex),LPlaneLineSeries);
        LPlaneLineSeries.SelectSeriesColor(LPlaneIndex);
        AOwner.AddSeries(LPlaneLineSeries);
        PopulateLineSeriesWithPlane(LPlaneIndex,LPlaneLineSeries,LPlane);
        LPlaneIndex := LPlaneIndex + 1;
        LPlane := TAbstractYRCPlane(YRCGraphDataObject.YRCPlane[LPlaneIndex]);
      end;

    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerPlaneSeries.ConfigureSeries(ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerPlaneSeries.ConfigureSeries';
begin
  try
    if assigned(ALineSeries) then
    begin
      ALineSeries.Active                 := False;
      //ALineSeries.OnGetMarkText          := OnGetSeriesMarkText;
      ALineSeries.Pointer.InflateMargins := True;
      ALineSeries.Pointer.Style          := psCircle;
      ALineSeries.Pointer.Visible        := True;
      ALineSeries.LinePen.Width          := 2;
      ALineSeries.ShowInLegend           := True;
      ALineSeries.XValues.Order          := loNone;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPlaneSeries.ShowSelectedPlane;
const OPNAME = 'TYRCContainerPlaneSeries.ShowSelectedPlane';
var
 LCount : integer;
 LLineSeries : TYRCLineSeries;
begin
  try
    for LCount := 0 to FLineSeriesList.Count-1 do
    begin
      LLineSeries := LineSeriesByIndex[LCount];
      if Assigned(LLineSeries) then
      begin
        if (LCount = YRCGraphDataObject.PlaneIndex) then
        begin
          RepaintLineSeries(LLineSeries,pwThick);
        end
        else
        begin
          RepaintLineSeries(LLineSeries,pwThin);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerPlaneSeries.PopulateLineSeriesWithPlane(ASeriesIndex: integer; ALineSeries: TYRCLineSeries;
          APlane:TAbstractYRCPlane);
const OPNAME = 'TYRCContainerPlaneSeries.PopulateLineSeriesWithPlane';
var
  LIndex: integer;
begin
  inherited;
  try
    if Assigned(ALineSeries) and Assigned(APlane) then
    begin
      ALineSeries.Clear;
      for LIndex := 0 to High(APlane.YXPointArrayObject.YRCRecordPointArray) do
      begin
        ALineSeries.AddXY(APlane.YXPointArrayObject.YRCRecordPointArray[LIndex].XTValue,APlane.YXPointArrayObject.YRCRecordPointArray[LIndex].YValue);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerPlaneSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerPlaneSeries.RepopulateSeriesData';
var
  LIndex: integer;
  LPlane:TAbstractYRCPlane;
  LSeries: TYRCLineSeries;
begin
  inherited;
  try
    for LIndex := 0 to FLineSeriesList.Count -1 do
    begin
      LSeries := LineSeriesByIndex[LIndex];
      LSeries.Clear;
    end;
    for LIndex := 0 to YRCGraphDataObject.PlanesCount -1 do
    begin
      LPlane := YRCGraphDataObject.YRCPlane[LIndex];
      if(LPlane <> nil) then
      begin
        LSeries := LineSeriesByIndex[LIndex];
        if(LSeries <> nil) then
          PopulateLineSeriesWithPlane(LIndex,LSeries,LPlane);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerPlaneSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerPlaneSeries.ChartSelectionHasChanged';
begin
  inherited;
  try
    HideAllSeries;
    case YRCGraphDataObject.YRCChartProperties.ChartMode of
      cmPlane        :
        begin
          RepopulateSeriesData;
          DisplayAllSeries;
          ShowSelectedPlane;
        end;
      cmView         :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
              end;
            tdmDeterministic :
              begin
              end;
            tdmRegression    :
              begin
              end;
          end;//case
        end;
      cmManipulating :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
              end;
            tdmDeterministic :
              begin
              end;
            tdmRegression    :
              begin
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
