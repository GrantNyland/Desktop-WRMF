//
//
//  UNIT      : Contains TYRCContainerRegressionPointSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerRegressionPointSeries;

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
  UAbstractModelData,
  UYRCContainerAbstractPointSeries;

type
  TYRCContainerRegressionPointSeries = class(TYRCContainerAbstractPointSeries)
  protected
    procedure SetPointSeriesProperties(APointSeries: TYRCPointSeries);override;
    procedure PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer;APointSeries: TYRCPointSeries; ATargetDraft:TAbstractYRCTargetDraft);override;
    procedure RepopulateSeriesData;override;
    procedure ShowOnlySelectedTargetDraft;
    procedure ShowSelectedTargetDraft;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure ChartSelectionHasChanged; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations, UYRCContainerAbstractSeries;


{ TYRCContainerRegressionPointSeries }

function TYRCContainerRegressionPointSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerRegressionPointSeries.CreateSeries';
var
  LTargetDraftIndex: integer;
  LSeries: TYRCPointSeries;
  LPlane :TAbstractYRCPlane;
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

procedure TYRCContainerRegressionPointSeries.PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer; APointSeries: TYRCPointSeries;
  ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerRegressionPointSeries.PopulatePointSeriesWithTargetDraft';
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
      LPointsArray := ATargetDraft.RegressionPointsArray.YRCRecordPointArray;
      for LPointIndex := Low(LPointsArray) to High(LPointsArray) do
      begin
        APointSeries.AddXY(LPointsArray[LPointIndex].XTValue,LPointsArray[LPointIndex].YValue);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionPointSeries.SetPointSeriesProperties(APointSeries: TYRCPointSeries);
const OPNAME = 'TYRCContainerRegressionPointSeries.SetPointSeriesProperties';
begin
  inherited SetPointSeriesProperties(APointSeries);
  try
    if Assigned(APointSeries) then
    begin
      APointSeries.Active := False;
      APointSeries.ShowInLegend  := False;
      APointSeries.Pointer.Style := psCircle;
      APointSeries.XValues.Order := loNone;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TYRCContainerRegressionPointSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerRegressionPointSeries.RepopulateSeriesData';
var
  LTargetDraftIndex: integer;
  LSeries: TYRCPointSeries;
  LPlane :TAbstractYRCPlane;
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

procedure TYRCContainerRegressionPointSeries.ShowOnlySelectedTargetDraft;
const OPNAME = 'TYRCContainerRegressionPointSeries.ShowOnlySelectedTargetDraft';
var
  LIndex: integer;
  LSeries: TYRCPointSeries;
  LActive: Boolean;
  LPlane :TAbstractYRCPlane;
  LTargetDraft:TAbstractYRCTargetDraft;
begin
  try
    LPlane := YRCGraphDataObject.SelectedPlane;
    for LIndex := 0 to  FPointSeriesList.Count -1 do
    begin
      LActive      := True;
      if(LPlane <> nil) then
      begin
        LTargetDraft := LPlane.TargetDraft[LIndex];
        if(LTargetDraft <> nil) then
          LActive := (LTargetDraft.TargetDraftSavedMode in [tdmNone,tdmRegression]);
      end;

      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      case YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts of
        stdAll     :
          begin
            LSeries.Active := LActive;
          end;
        stdSelected:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
              LSeries.Active := LActive
            else
              LSeries.Active := False;
          end;
        stdAdjacent:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex-1)) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex+1))then
              LSeries.Active := LActive
            else
              LSeries.Active := False;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionPointSeries.ShowSelectedTargetDraft;
const OPNAME = 'TYRCContainerRegressionPointSeries.ShowSelectedTargetDraft';
var
  LIndex: integer;
  LSeries: TYRCPointSeries;
begin
  try
    for LIndex := 0 to  FPointSeriesList.Count -1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
        RepaintPointSeries(LSeries,pwThick)
      else
        RepaintPointSeries(LSeries,pwThin);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionPointSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerRegressionPointSeries.ChartSelectionHasChanged';
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
                if YRCGraphDataObject.YRCChartProperties.HideFittedPoints then
                  HideAllSeries
                else
                begin
                  RepopulateSeriesData;
                  DisplayAllSeries;
                  ShowOnlySelectedTargetDraft;
                end;
              end;
            tdmDeterministic :
              begin
                if YRCGraphDataObject.YRCChartProperties.HideFittedPoints then
                  HideAllSeries
                else
                begin
                  RepopulateSeriesData;
                  DisplayAllSeries;
                  ShowOnlySelectedTargetDraft;
                end;
              end;
            tdmRegression    :
              begin
                if YRCGraphDataObject.YRCChartProperties.HideFittedPoints then
                  HideAllSeries
                else
                begin
                  RepopulateSeriesData;
                  DisplayAllSeries;
                  ShowOnlySelectedTargetDraft;
                end;
              end;
          end;//case
        end;
      cmManipulating :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowOnlySelectedTargetDraft;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowOnlySelectedTargetDraft;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowOnlySelectedTargetDraft;
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
