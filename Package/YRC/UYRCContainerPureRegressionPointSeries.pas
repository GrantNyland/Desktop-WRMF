//
//
//  UNIT      : Contains TYRCContainerPureRegressionPointSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerPureRegressionPointSeries;

interface

uses
  Classes,
  Contnrs,
  VCL.Graphics,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  UAbstractObject,
  UAbstractYRCData,
  UAbstractModelData,
  UYRCSeries,
  UYRCContainerAbstractPointSeries;

type
  TYRCContainerPureRegressionPointSeries = class(TYRCContainerAbstractPointSeries)
  protected
    procedure SetPointSeriesProperties(APointSeries: TYRCPointSeries);override;
    procedure PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer;APointSeries: TYRCPointSeries; ATargetDraft:TAbstractYRCTargetDraft);override;
    procedure RepopulateSeriesData;override;
    procedure ShowOnlySelectedTargetDraft;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure ChartSelectionHasChanged; override;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UErrorHandlingOperations,
  UYRCContainerAbstractSeries;


{ TYRCContainerPureRegressionPointSeries }

function TYRCContainerPureRegressionPointSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerPureRegressionPointSeries.CreateSeries';
var
  LTargetDraftIndex: integer;
  LSeries: TYRCPointSeries;
  LPlane:TAbstractYRCPlane;
  LTargetDraft :TAbstractYRCTargetDraft;
begin
  Result := inherited CreateSeries(AOwner);
  try
    LPlane := YRCGraphDataObject.SelectedPlane;
    if Assigned(LPlane) then
    begin
      for LTargetDraftIndex := 0 to LPlane.TargetDraftCount -1 do
      begin
        LTargetDraft := TAbstractYRCTargetDraft(LPlane.TargetDraft[LTargetDraftIndex]);
        if Assigned(LTargetDraft) then
        begin
          LSeries := TYRCPointSeries.Create(nil);
          FPointSeriesList.AddObject(IntToStr(LTargetDraftIndex),LSeries);
          LSeries.SeriesIndex := FPointSeriesList.Count - 1;
          LSeries.TargetDraftIndex := LTargetDraftIndex;
          LSeries.PlaneIndex := LPlane.PlaneID;
          LSeries.SelectSeriesColor(LTargetDraftIndex);
          LSeries.SelectSeriesPointStyle(LTargetDraftIndex);
          AOwner.AddSeries(LSeries);
          LSeries.ParentChart := AOwner;
          SetPointSeriesProperties(LSeries);
          PopulatePointSeriesWithTargetDraft(LSeries.SeriesIndex,LSeries,LTargetDraft);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionPointSeries.PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer; APointSeries: TYRCPointSeries;
  ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerPureRegressionPointSeries.PopulatePointSeriesWithTargetDraft';
var
  LPointsArray :TYRCRecordPointArray;
  LPointIndex: integer;
begin
  inherited PopulatePointSeriesWithTargetDraft(ASeriesIndex,APointSeries,ATargetDraft);
  LPointsArray := nil;
  try
    if Assigned(APointSeries) and Assigned(ATargetDraft) then
    begin
      APointSeries.Clear;
      LPointsArray := ATargetDraft.PureRegressionPointsArray.YRCRecordPointArray;
      for LPointIndex := (Low(LPointsArray) + 1) to High(LPointsArray) do
      begin
        APointSeries.AddXY(LPointsArray[LPointIndex].XTValue,LPointsArray[LPointIndex].YValue);
      end;
      if(ATargetDraft.YValueAt100 <> NullFloat) then
        APointSeries.AddXY(100,ATargetDraft.YValueAt100);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionPointSeries.SetPointSeriesProperties(APointSeries: TYRCPointSeries);
const OPNAME = 'TYRCContainerPureRegressionPointSeries.SetPointSeriesProperties';
begin
  inherited SetPointSeriesProperties(APointSeries);
  try
    if Assigned(APointSeries) then
    begin
      APointSeries.Active       := False;
      APointSeries.ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionPointSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerPureRegressionPointSeries.RepopulateSeriesData';
var
  LTargetDraftIndex: integer;
  LSeries: TYRCPointSeries;
  LPlane:TAbstractYRCPlane;
  LTargetDraft :TAbstractYRCTargetDraft;
begin
  inherited;
  try
    LPlane := YRCGraphDataObject.SelectedPlane;
    if Assigned(LPlane) then
    begin
      for LTargetDraftIndex := 0 to LPlane.TargetDraftCount -1 do
      begin
        LTargetDraft := TAbstractYRCTargetDraft(LPlane.TargetDraft[LTargetDraftIndex]);
        if Assigned(LTargetDraft) then
        begin
          LSeries := PointSeriesByIndex[LTargetDraftIndex];
          if(LSeries <> nil) then
            PopulatePointSeriesWithTargetDraft(LSeries.SeriesIndex,LSeries,LTargetDraft);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionPointSeries.ShowOnlySelectedTargetDraft;
const OPNAME = 'TYRCContainerPureRegressionPointSeries.ShowOnlySelectedTargetDraft';
var
  LIndex: integer;
  LSeries: TYRCPointSeries;
begin
  try
    for LIndex := 0 to  FPointSeriesList.Count -1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      case YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts of
        stdAll     :
          begin
            LSeries.Active := True
          end;
        stdSelected:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
              LSeries.Active := True
            else
              LSeries.Active := False;
          end;
        stdAdjacent:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex-1)) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex+1))then
              LSeries.Active := True
            else
              LSeries.Active := False;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerPureRegressionPointSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerPureRegressionPointSeries.ChartSelectionHasChanged';
begin
  inherited;
  try
    HideAllSeries;
    case YRCGraphDataObject.YRCChartProperties.ChartMode of
      cmPlane        :
        begin
        end;
      cmView         :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
                RepopulateSeriesData;
                if YRCGraphDataObject.YRCChartProperties.HideRawpoints then
                  HideAllSeries
                else
                  ShowOnlySelectedTargetDraft;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                if YRCGraphDataObject.YRCChartProperties.HideRawpoints then
                  HideAllSeries
                else
                  ShowOnlySelectedTargetDraft;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                if YRCGraphDataObject.YRCChartProperties.HideRawpoints then
                  HideAllSeries
                else
                  ShowOnlySelectedTargetDraft;
              end;
          end;//case
        end;
      cmManipulating :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
                RepopulateSeriesData;
                if YRCGraphDataObject.YRCChartProperties.HideRawpoints then
                  HideAllSeries
                else
                  ShowOnlySelectedTargetDraft;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                if YRCGraphDataObject.YRCChartProperties.HideRawpoints then
                  HideAllSeries
                else
                  ShowOnlySelectedTargetDraft;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                if YRCGraphDataObject.YRCChartProperties.HideRawpoints then
                  HideAllSeries
                else
                  ShowOnlySelectedTargetDraft;
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
