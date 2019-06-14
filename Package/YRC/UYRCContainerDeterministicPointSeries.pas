//
//
//  UNIT      : Contains TYRCContainerDeterministicPointSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerDeterministicPointSeries;

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
  TYRCContainerDeterministicPointSeries = class(TYRCContainerAbstractPointSeries)
  protected
    procedure SetPointSeriesProperties(APointSeries: TYRCPointSeries);override;
    procedure PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer;APointSeries: TYRCPointSeries; ATargetDraft:TAbstractYRCTargetDraft);override;
    procedure BringSelectedSeriesToFront;
    procedure RepopulateSeriesData;override;
    procedure ShowOnlySelectedTargetDraft;
    procedure ShowSelectedTargetDraft;
    procedure RestorePointSeries;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure ChartSelectionHasChanged; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations, UYRCContainerAbstractSeries;


{ TYRCContainerDeterministicPointSeries }

function TYRCContainerDeterministicPointSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerDeterministicPointSeries.CreateSeries';
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

procedure TYRCContainerDeterministicPointSeries.PopulatePointSeriesWithTargetDraft(ASeriesIndex: integer; APointSeries: TYRCPointSeries;
  ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerDeterministicPointSeries.PopulatePointSeriesWithTargetDraft';
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
      LPointsArray := ATargetDraft.DeterministicPointsArray.YRCRecordPointArray;
      for LPointIndex := Low(LPointsArray) to High(LPointsArray) do
      begin
        APointSeries.AddXY(LPointsArray[LPointIndex].XTValue,LPointsArray[LPointIndex].YValue);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicPointSeries.SetPointSeriesProperties(APointSeries: TYRCPointSeries);
const OPNAME = 'TYRCContainerDeterministicPointSeries.SetPointSeriesProperties';
begin
  inherited SetPointSeriesProperties(APointSeries);
  try
    if Assigned(APointSeries) then
    begin
      APointSeries.Active := False;
      APointSeries.ShowInLegend  := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicPointSeries.BringSelectedSeriesToFront;
const OPNAME = 'TYRCContainerDeterministicPointSeries.BringSelectedSeriesToFront';
var
  LPointSeries: TYRCPointSeries;
begin
  try
    LPointSeries := PointSeriesByIndex[YRCGraphDataObject.SelectedTargetDraftIndex];
    if Assigned(LPointSeries) and LPointSeries.Active then
      BringSeriesToFront(LPointSeries);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicPointSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerDeterministicPointSeries.RepopulateSeriesData';
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

procedure TYRCContainerDeterministicPointSeries.ShowOnlySelectedTargetDraft;
const OPNAME = 'TYRCContainerDeterministicPointSeries.ShowOnlySelectedTargetDraft';
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
      LActive      := False;
      if(LPlane <> nil) then
      begin
        LTargetDraft := LPlane.TargetDraft[LIndex];
        if(LTargetDraft <> nil) then
          LActive := (LTargetDraft.TargetDraftSavedMode = tdmDeterministic);
      end;

      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      case YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts of
        stdAll     :
          begin
            LSeries.Active := LActive
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

procedure TYRCContainerDeterministicPointSeries.ShowSelectedTargetDraft;
const OPNAME = 'TYRCContainerDeterministicPointSeries.ShowSelectedTargetDraft';
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

procedure TYRCContainerDeterministicPointSeries.RestorePointSeries;
const OPNAME = 'TYRCContainerDeterministicPointSeries.RestorePointSeries';
var
  LIndex: integer;
  LSeries: TYRCPointSeries;
begin
  try
    for LIndex := 0 to  FPointSeriesList.Count -1 do
    begin
      LSeries := TYRCPointSeries(FPointSeriesList.Objects[LIndex]);
      RepaintPointSeries(LSeries,pwThin);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerDeterministicPointSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerDeterministicPointSeries.ChartSelectionHasChanged';
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
                  RestorePointSeries;
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
                  RestorePointSeries;
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
                  RestorePointSeries;
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
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
                BringSelectedSeriesToFront;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
                BringSelectedSeriesToFront;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
                BringSelectedSeriesToFront;
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
