//
//
//  UNIT      : Contains TYRCContainerTargetDraftVerticalSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerTargetDraftVerticalSeries;

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

  TYRCContainerTargetDraftVerticalSeries = class(TYRCContainerAbstractLineSeries)
  protected
    FYearsFormatStr: string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ConfigureLinesSeries(ALineSeries: TYRCLineSeries);
    procedure OnGetLineSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    procedure RepopulateSeriesData;override;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure MaxYValueHasChanged;
    procedure ChartSelectionHasChanged; override;
  end;

implementation

uses
  SysUtils,Math,
  UErrorHandlingOperations,
  UYRCContainerAbstractSeries;


{ TYRCContainerTargetDraftVerticalSeries }

procedure TYRCContainerTargetDraftVerticalSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.CreateMemberObjects';
begin
  inherited;
  try
    FYearsFormatStr := '';
    //FYieldFormatStr  := '';
    //FCurrentPlane := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftVerticalSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTargetDraftVerticalSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.CreateSeries';
var
  LPlane:TAbstractYRCPlane;
  LLineSeries : TYRCLineSeries;
  LCount : integer;
  LPointValues: TYRCPoint;
begin
  Result := inherited CreateSeries(AOwner);
  try
    if Result then
    begin
      //FYieldFormatStr := YRCGraphDataObject.YRCLanguageStrings.YieldFormatStr;
      FYearsFormatStr := YRCGraphDataObject.YRCLanguageStrings.YearsFormatStr;
      if (YRCGraphDataObject.PlanesCount > 0) then
      begin
        LPlane := TAbstractYRCPlane(YRCGraphDataObject.YRCPlane[YRCGraphDataObject.PlaneIndex]);
        if Assigned(LPlane) then
        begin
          for LCount := 0 to High(LPlane.AssurancePointArrayObject.YRCRecordPointArray) do
          begin
            LPointValues := LPlane.AssurancePointArrayObject.YRCRecordPointArray[LCount];
            if (LPointValues.YValue > 0.000) then
            begin
              LLineSeries := TYRCLineSeries.Create(AOwner);
              //LLineSeries.Active := False;
              LLineSeries.SeriesIndex := (FLineSeriesList.Count - 1);
              FLineSeriesList.AddObject(IntToStr(FLineSeriesList.Count+1),LLineSeries);
              LLineSeries.PlaneIndex := LPlane.PlaneID;
              //LLineSeries.SeriesIndex := (FLineSeriesList.Count - 1);
              //LLineSeries.TargetDraftIndex := LCount;
              AOwner.AddSeries(LLineSeries);
              LLineSeries.Title := 'RecurenceInterval'+ IntToStr(FLineSeriesList.Count);
              ConfigureLinesSeries(LLineSeries);

              //if (LCount > 0) then
              //begin
                //if (LPointValues.XValue > 0.000) then
                //begin
                  //AOwner.LeftAxis.AutomaticMaximum := False;
                  LLineSeries.AddXY(LPointValues.XValue,0);
                  LLineSeries.AddXY(LPointValues.XValue,AOwner.LeftAxis.Maximum + (AOwner.LeftAxis.Maximum * 0.15));
                  Self.RepaintLineSeries(LLineSeries,pwThin);
                //end;
              //end
              //else
              //begin
              //  LLineSeries.AddXY(LPointValues.XValue,0);
              //  LLineSeries.AddXY(LPointValues.XValue,AOwner.LeftAxis.Maximum);
              //  Self.RepaintLineSeries(LLineSeries,pwThin);
              //end;
            end;
          end;
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTargetDraftVerticalSeries.ConfigureLinesSeries(ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.ConfigureLinesSeries';
begin
  try
    if assigned(ALineSeries) then
    begin
      ALineSeries.Active                 := True;
      ALineSeries.XValues.Order          := loNone;
      ALineSeries.Marks.Visible          := True;
      ALineSeries.Marks.Clip             := True;
      ALineSeries.ShowInLegend           := False;
      ALineSeries.SeriesColor            := clRed;
      ALineSeries.OnGetMarkText          := OnGetLineSeriesMarkText;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftVerticalSeries.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer;
 var MarkText: String);
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.OnGetLineSeriesMarkText';
var
  LPointsWithSeries: array of integer;
  LYearsMarkFormatStr: string;
  LCount,
  LIndex: integer;
  LSeries: TChartSeries;
  LPlane:TAbstractYRCPlane;
  LPointValues: TYRCPoint;
  LZoomPerc,
  LYValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    if (ValueIndex <> 1) then
      Exit;

    if not Assigned(FOwnerChart) then
      Exit;

    if not Assigned(YRCGraphDataObject()) then
      Exit;

    LPlane := YRCGraphDataObject.SelectedPlane;
    if not Assigned(LPlane) then
      Exit;

    LIndex := 0;
    SetLength(LPointsWithSeries,Length(LPlane.AssurancePointArrayObject.YRCRecordPointArray));
    try
      for LCount := Low(LPointsWithSeries) to High(LPointsWithSeries) do
      begin
        LPointsWithSeries[LCount] := -1;
        if(LPlane.AssurancePointArrayObject.YRCRecordPointArray[LCount].YValue > 0.0) then
        begin
          LPointsWithSeries[LIndex] := LCount;
          LIndex := LIndex + 1;
        end;
      end;

      LIndex := -1;
      for LCount := 0 to FLineSeriesList.Count - 1 do
      begin
       LSeries := TChartSeries(FLineSeriesList.Objects[LCount]);
       if(LSeries = Sender) then
       begin
         LIndex := LCount;
         Break;
       end;
      end;

      LYearsMarkFormatStr := '';
      if (LIndex >= 0) and (Trim(FYearsFormatStr) <> '') and
         (LPointsWithSeries[LIndex] >= 0) then
      begin
        LPointValues := LPlane.AssuranceYearsArrayObject.YRCRecordPointArray[LPointsWithSeries[LIndex]];
        LYearsMarkFormatStr := FAppModules.Language.GetString(FYearsFormatStr);
        LYearsMarkFormatStr := Format(LYearsMarkFormatStr,[LPointValues.XValue,LPointValues.YValue]);
      end;

      if (LYearsMarkFormatStr <> '') then
      begin
        LYValue  := Sender.YValues.MaxValue - Sender.YValues.MinValue;
        LYValue  := LYValue/3.0;
        LZoomPerc := Sender.ParentChart.BottomAxis.Maximum - Sender.ParentChart.BottomAxis.Minimum;
        LZoomPerc := LZoomPerc/100;
        LXPos   := Sender.CalcXPosValue(Sender.XValue[ValueIndex] - LZoomPerc);
        //LYPos   := Sender.CalcYPosValue(Sender.XValue[ValueIndex]) - 80;
        LYPos   := Sender.CalcYPosValue(LYValue);
        FOwnerChart.BottomAxis.DrawAxisLabel(LXPos,LYPos,90,LYearsMarkFormatStr);
      end;
    finally
      Finalize(LPointsWithSeries);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTargetDraftVerticalSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.RepopulateSeriesData';
begin
  inherited;
  try
    CreateSeries(FOwnerChart);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTargetDraftVerticalSeries.MaxYValueHasChanged;
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.MaxYValueHasChanged';
begin
  try
    CreateSeries(FOwnerChart);
    ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged';
begin
  inherited;
  try
    HideAllSeries;
    case YRCGraphDataObject.YRCChartProperties.ChartMode of
      cmPlane        :
        begin
          RepopulateSeriesData;
          DisplayAllSeries;
        end;
      cmView         :
        begin
          case YRCGraphDataObject.YRCChartProperties.ChartEditMode of
            tdmNone          :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
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
              end;
            tdmDeterministic :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
              end;
            tdmRegression    :
              begin
                RepopulateSeriesData;
                DisplayAllSeries;
              end;
          end;//case
        end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
