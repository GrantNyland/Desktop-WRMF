//
//
//  UNIT      : Contains TYRCContainerRegressionLineSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright � 2002 DWAF
//
//
unit UYRCContainerRegressionLineSeries;

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
  UCurveFittingOperations,
  UAbstractModelData,
  UYRCContainerAbstractLineSeries;

type
  TYRCContainerRegressionLineSeries = class(TYRCContainerAbstractLineSeries)
  protected
    procedure SetLineSeriesProperties(ALineSeries: TYRCLineSeries); override;
    procedure PopulateLineSeriesWithTargetDraft(ASeriesIndex: integer;ALineSeries: TYRCLineSeries; ATargetDraft:TAbstractYRCTargetDraft); override;
    procedure RepopulateSeriesData;override;
    procedure ShowOnlySelectedTargetDraft;
    procedure ShowSelectedTargetDraft;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure ChartSelectionHasChanged; override;
  end;

implementation

uses
  Math,
  SysUtils,
  UErrorHandlingOperations, UYRCContainerAbstractSeries;


{ TYRCContainerRegressionLineSeries }

function TYRCContainerRegressionLineSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerRegressionLineSeries.CreateSeries';
var
  LTargetDraftIndex: integer;
  LSeries: TYRCLineSeries;
  LPlane :TAbstractYRCPlane;
  LTargetDraft :TAbstractYRCTargetDraft;
begin
  Result := inherited CreateSeries(AOwner);
  try
    LPlane := YRCGraphDataObject.SelectedPlane;;
    if Assigned(LPlane) then
    begin
      for LTargetDraftIndex := 0 to LPlane.TargetDraftCount -1 do
      begin
        LTargetDraft := LPlane.TargetDraft[LTargetDraftIndex];
        if Assigned(LTargetDraft) then
        begin
          LSeries := TYRCLineSeries.Create(nil);
          FLineSeriesList.AddObject(IntToStr(LTargetDraftIndex),LSeries);
          LSeries.SeriesIndex := FLineSeriesList.Count - 1;
          LSeries.TargetDraftIndex := LTargetDraftIndex;
          LSeries.PlaneIndex := LPlane.PlaneID;
          LSeries.SelectSeriesColor(LTargetDraftIndex);
          AOwner.AddSeries(LSeries);
          LSeries.ParentChart := AOwner;
          SetLineSeriesProperties(LSeries);
          PopulateLineSeriesWithTargetDraft(LSeries.SeriesIndex,LSeries,LTargetDraft);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionLineSeries.PopulateLineSeriesWithTargetDraft(ASeriesIndex: integer; ALineSeries: TYRCLineSeries;
  ATargetDraft: TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerRegressionLineSeries.PopulateLineSeriesWithTargetDraft';
var
  LStep   : double;
  LYValue : double;
  LXValue : double;
  A,
  B,
  C,
  D : double;
begin
  inherited PopulateLineSeriesWithTargetDraft(ASeriesIndex,ALineSeries,ATargetDraft);
  try
    ALineSeries.Clear;
    if(Length(ATargetDraft.PureRegressionPointsArray.YRCRecordPointArray) = 1) then Exit;
    if Assigned(ALineSeries)  and
       Assigned(ATargetDraft) and
       ATargetDraft.RegressionConstants.Valid then
    begin
      A := ATargetDraft.RegressionConstants.ConstantsArray[0];
      B := ATargetDraft.RegressionConstants.ConstantsArray[1];
      C := ATargetDraft.RegressionConstants.ConstantsArray[2];
      D := ATargetDraft.RegressionConstants.ConstantsArray[3];

      LXValue := ATargetDraft.TargetDraftXTValue;
      LStep   := Abs((100.00 - LXValue)/100.0);
      while (LXValue <= (100.00 + LStep)) do
      begin
        if(LXValue > 100.00) then
           LXValue := 100.00;

        // Y = Ax*3 + Bx*2 +Cx + D
        LYValue := A + (B * LXValue) + (C * Power(LXValue, 2)) + (D * Power(LXValue, 3));
        if (LYValue > 0.0000) then
          ALineSeries.AddXY(LXValue,LYValue);
        if(LXValue >= 100.00) then Break;
        LXValue := LXValue + LStep;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
{var
  LCount: integer;
  LIndex: integer;
  LYValue: double;
  LXValue: double;
begin
  inherited PopulateLineSeriesWithTargetDraft(ASeriesIndex,ALineSeries,ATargetDraft);
  try
    if Assigned(ALineSeries) and Assigned(ATargetDraft) and ATargetDraft.RegressionConstants.Valid then
    begin
      ALineSeries.Clear;
      LCount := Ceil(ATargetDraft.TargetDraftXTValue);
      for LCount := LCount to 100 do
      begin
        // Y = Ax*3 + Bx*2 +Cx + D
        LXValue := LCount;
        LYValue := 0.0;
        for LIndex := 0 to 3 do
          LYValue := LYValue + (ATargetDraft.RegressionConstants.ConstantsArray[LIndex] * Power(LXValue,LIndex));

        if (LYValue > 0.0) then
          ALineSeries.AddXY(LXValue,LYValue);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end; }
end;

procedure TYRCContainerRegressionLineSeries.SetLineSeriesProperties(ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerRegressionLineSeries.SetLineSeriesProperties';
begin
  inherited SetLineSeriesProperties(ALineSeries);
  try
    if Assigned(ALineSeries) then
    begin
      ALineSeries.Active          := False;
      ALineSeries.ShowInLegend    := False;
      ALineSeries.Pointer.Visible := False;
      ALineSeries.LinePen.Style   := psSolid;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCContainerRegressionLineSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerRegressionLineSeries.RepopulateSeriesData';
var
  LTargetDraftIndex: integer;
  LSeries: TYRCLineSeries;
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
          LSeries := LineSeriesByIndex[LTargetDraftIndex];
          if(LSeries <> nil) then
            PopulateLineSeriesWithTargetDraft(LSeries.SeriesIndex,LSeries,LTargetDraft);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionLineSeries.ShowSelectedTargetDraft;
const OPNAME = 'TYRCContainerRegressionLineSeries.ShowSelectedTargetDraft';
var
  LIndex: integer;
  LLineSeries : TYRCLineSeries;
begin
  try
    for LIndex := 0 to  FLineSeriesList.Count -1 do
    begin
      LLineSeries := TYRCLineSeries(FLineSeriesList.Objects[LIndex]);
      if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
        RepaintLineSeries(LLineSeries,pwThick)
      else
        RepaintLineSeries(LLineSeries,pwThin);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionLineSeries.ShowOnlySelectedTargetDraft;
const OPNAME = 'TYRCContainerRegressionLineSeries.ShowOnlySelectedTargetDraft';
var
  LIndex: integer;
  LLineSeries : TYRCLineSeries;
  LActive: Boolean;
  LPlane :TAbstractYRCPlane;
  LTargetDraft:TAbstractYRCTargetDraft;
begin
  try
    LPlane := YRCGraphDataObject.SelectedPlane;
    for LIndex := 0 to  FLineSeriesList.Count -1 do
    begin
      LActive      := True;
      if(LPlane <> nil) then
      begin
        LTargetDraft := LPlane.TargetDraft[LIndex];
        if(LTargetDraft <> nil) then
          LActive := (LTargetDraft.TargetDraftSavedMode in [tdmNone,tdmRegression]);
      end;

      LLineSeries := TYRCLineSeries(FLineSeriesList.Objects[LIndex]);
      case YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts of
        stdAll     :
          begin
            LLineSeries.Active := LActive
          end;
        stdSelected:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
              LLineSeries.Active := LActive
            else
              LLineSeries.Active := False;
          end;
        stdAdjacent:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex-1)) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex+1))then
              LLineSeries.Active := LActive
            else
              LLineSeries.Active := False;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerRegressionLineSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerRegressionLineSeries.ChartSelectionHasChanged';
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
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowSelectedTargetDraft;
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
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
                ShowSelectedTargetDraft;
                ShowOnlySelectedTargetDraft;
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
