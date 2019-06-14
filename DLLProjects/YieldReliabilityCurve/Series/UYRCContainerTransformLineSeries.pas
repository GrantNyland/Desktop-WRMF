//
//
//  UNIT      : Contains TYRCContainerPureRegressionLineSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerTransformLineSeries;

interface

uses
  Classes,
  Contnrs,
  Graphics,
  Series,
  Chart,
  TeEngine,
  TeeShape,
  UYRCSeries,
  UAbstractObject,
  UAbstractYRCData,
  UCurveFittingOperations,
  UAbstractModelData,
  UYRCContainerAbstractLineSeries;

type
  TYRCContainerTransformLineSeries = class(TYRCContainerAbstractLineSeries)
  protected
    procedure SetLineSeriesProperties(ASeriesIndex: integer;ALineSeries: TYRCLineSeries); override;
    procedure PopulateLineSeries(ASeriesIndex: integer;ALineSeries: TYRCLineSeries; ATargetDraft:TAbstractYRCTargetDraft); override;
  public
    function CreateSeries(AOwner: TChart; AYRCGraphDataObject:TAbstractYRCGraphDataObject): boolean; override;
    procedure OnTargetDraftSelectionChange(ASelectionIndeces: TIntegerArray);override;
    procedure OnPlotPlaneSelectionChange (APlaneIndex : integer);override;
    function ReloadSeries(ASeriesIndex: integer): boolean; override;
    function ApplySelectedPlotPlane: boolean;override;
    function ShowPlotPlaneSelection: boolean;override;
    function IndexofVisibleSeries : integer;
  end;

implementation

uses
  Math,
  SysUtils,
  UAbstractYRCModelDataObject,
  UErrorHandlingOperations, UYRCContainerAbstractSeries;

{ TYRCContainerTransformLineSeries }

function TYRCContainerTransformLineSeries.CreateSeries(AOwner: TChart; AYRCGraphDataObject: TAbstractYRCGraphDataObject): boolean;
const OPNAME = 'TYRCContainerTransformLineSeries.CreateSeries';
var
  LTargetDraftIndex,
  LPlaneIndex,
  LTmpPlaneIndex    : integer;
  LSeries           : TYRCLineSeries;
  LPlane            : TAbstractYRCPlane;
  LTargetDraft      : TAbstractYRCTargetDraft;
begin
  Result := inherited CreateSeries(AOwner,AYRCGraphDataObject);
  try
    Result := Result and ClearSeriesList;
    if Result and
       //(AYRCGraphDataObject.SelectedPlane <> nil) and
       //(FPlotPlaneIndex >= 0) and
       //(FPlotPlaneIndex < AYRCGraphDataObject.PlanesCount) and
       (AYRCGraphDataObject.PlanesCount > 0) then
    begin
      LTmpPlaneIndex := AYRCGraphDataObject.PlaneIndex;
      for LPlaneIndex := 0 to AYRCGraphDataObject.PlanesCount - 1 do
      begin
        LPlane := AYRCGraphDataObject.YRCPlane[LPlaneIndex];
        AYRCGraphDataObject.PlaneIndex := LPlaneIndex;
        if Assigned(LPlane) then
        begin
          for LTargetDraftIndex := 0 to LPlane.TargetDraftCount -1 do
          begin
            LTargetDraft := TAbstractYRCTargetDraft(LPlane.TargetDraft[LTargetDraftIndex]);
            if Assigned(LTargetDraft) then
            begin
              LSeries := TYRCLineSeries.Create(nil);
              FLineSeriesList.AddObject(IntToStr(LTargetDraftIndex),LSeries);
              LSeries.SeriesIndex := FLineSeriesList.Count - 1;
              LSeries.TargetDraftIndex := LTargetDraftIndex;
              LSeries.PlaneIndex := LPlaneIndex;
              AOwner.AddSeries(LSeries);
              LSeries.Active := False;
              LSeries.ParentChart := AOwner;
              SetLineSeriesProperties(LSeries.SeriesIndex,LSeries);
              PopulateLineSeries(LSeries.SeriesIndex,LSeries,LTargetDraft);
            end;
          end;
        end;
      end;
      AYRCGraphDataObject.PlaneIndex := LTmpPlaneIndex;
      Result := True;
      FSeriesCreated := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTransformLineSeries.PopulateLineSeries (ASeriesIndex : integer;
                                                                    ALineSeries  : TYRCLineSeries;
                                                                    ATargetDraft : TAbstractYRCTargetDraft);
const OPNAME = 'TYRCContainerTransformLineSeries.PopulateLineSeries';
var
  LCount  : integer;
  //LIndex  : integer;
  LYValue : double;
  LXValue : double;
  A,
  B,
  C,
  D : double;
begin
  inherited PopulateLineSeries(ASeriesIndex,ALineSeries,ATargetDraft);
  try
    if Assigned(ALineSeries)  and
       Assigned(ATargetDraft) and
       ATargetDraft.PureRegressionConstants.Valid then
    begin
      ALineSeries.Clear;
      A := ATargetDraft.PureRegressionConstants.ConstantsArray[0];
      B := ATargetDraft.PureRegressionConstants.ConstantsArray[1];
      C := ATargetDraft.PureRegressionConstants.ConstantsArray[2];
      D := ATargetDraft.PureRegressionConstants.ConstantsArray[3];

      if (FTransformed) then
        LCount := 0
      else
        LCount := Ceil(ATargetDraft.TargetDraftXValue);

      for LCount := LCount to 100 do
      begin
        if (FTransformed) then
          LXValue := (LCount / 100)
        else
          LXValue := LCount;
        // Y = Ax*3 + Bx*2 +Cx + D
        LYValue := A + (B * LXValue) + (C * Power(LXValue, 2)) + (D * Power(LXValue, 3));
        if (LYValue >= 0.0000) then
          ALineSeries.AddXY(LXValue,LYValue);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTransformLineSeries.SetLineSeriesProperties(ASeriesIndex: integer; ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerTransformLineSeries.SetLineSeriesProperties';
begin
  inherited SetLineSeriesProperties(ASeriesIndex,ALineSeries);
  try
    if Assigned(ALineSeries) then
    begin
      ALineSeries.ShowInLegend    := False;
      ALineSeries.SeriesColor     := clBlue;
      ALineSeries.LinePen.Style   := psSolid;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTransformLineSeries.ShowPlotPlaneSelection: boolean;
const OPNAME = 'TYRCContainerTransformLineSeries.ShowPlotPlaneSelection';
var
  LTargetDraftCount : integer;
begin
  Result := inherited ShowPlotPlaneSelection;
  try
    FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
    if(FPlaneIndex > 0) and (FPlaneIndex <= FYRCGraphDataObject.PlanesCount) then
    begin
      LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
      Result := Result and HideAllSeries(FPlaneIndex, LTargetDraftCount);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerTransformLineSeries.ApplySelectedPlotPlane: boolean;
const OPNAME = 'TYRCContainerTransformLineSeries.ApplySelectedPlotPlane';
var
  LTargetDraftCount : integer;
begin
  Result := inherited ApplySelectedPlotPlane;
  try
    if Result and (not FSeriesCreated) then
      Result := CreateSeries(FOwnerChart,FYRCGraphDataObject);
    FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
    if(FPlaneIndex > 0) and (FPlaneIndex <= FYRCGraphDataObject.PlanesCount) then
    begin
      LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
      Result := Result and HideAllSeries(FPlaneIndex, LTargetDraftCount);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTransformLineSeries.OnPlotPlaneSelectionChange (APlaneIndex : integer);
const OPNAME = 'TYRCContainerTransformLineSeries.OnPlotPlaneSelectionChange';
begin
  inherited OnPlotPlaneSelectionChange(APlaneIndex);
  try
    //if (not FSeriesCreated) then
    ClearSeriesList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTransformLineSeries.OnTargetDraftSelectionChange(ASelectionIndeces: TIntegerArray);
const OPNAME = 'TYRCContainerTransformLineSeries.OnTargetDraftSelectionChange';
var
  LTargetDraftCount : integer;
begin
  inherited OnTargetDraftSelectionChange(ASelectionIndeces);
  try
    if (Length(ASelectionIndeces) > 0) then
    begin
      FPlaneIndex := FYRCGraphDataObject.PlaneIndex;  
      LTargetDraftCount := FYRCGraphDataObject.YRCPlane[FPlaneIndex].TargetDraftCount;
      HideAllSeries(FPlaneIndex, LTargetDraftCount);
      RedrawLineSeries(ASelectionIndeces[0]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTransformLineSeries.ReloadSeries(ASeriesIndex: integer): boolean;
const OPNAME = 'TYRCContainerTransformLineSeries.ReloadSeries';
var
  LLineSeries: TYRCLineSeries;
  LTargetDraft: TAbstractYRCTargetDraft;
  LPlane : TAbstractYRCPlane;
begin
  Result := False;
  try
    //if FLineSeriesList.Count = 0 then
    //  CreateSeries(FOwnerChart, FYRCGraphDataObject);
    LLineSeries := GetLineSeriesByIndex(ASeriesIndex);
    LPlane := FYRCGraphDataObject.SelectedPlane;
    if Assigned(LPlane) then
    begin
      LTargetDraft := LPlane.TargetDraft[ASeriesIndex];
      //LTargetDraft := TAbstractYRCTargetDraft(FYRCGraphDataObject.YRCTargetDraft[FPlotPlaneIndex,ASeriesIndex]);
      if Assigned(LLineSeries) and Assigned(LTargetDraft) then
      begin
        PopulateLineSeries(ASeriesIndex,LLineSeries,LTargetDraft);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYRCContainerTransformLineSeries.IndexofVisibleSeries: integer;
const OPNAME = 'TYRCContainerTransformLineSeries.IndexofVisibleSeries';
var
  LCount : integer;
  LLineSeries : TYRCLineSeries;
begin
  Result := -1;
  try
    for LCount := 0 to FLineSeriesList.Count - 1 do
    begin
      LLineSeries := TYRCLineSeries(FLineSeriesList.Objects[LCount]);
      if Assigned(LLineSeries) and
        (LLineSeries.Active) then
      begin
        Result := LLineSeries.SeriesIndex;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
