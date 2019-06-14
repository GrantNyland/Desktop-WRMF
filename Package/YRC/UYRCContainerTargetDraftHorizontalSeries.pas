//
//
//  UNIT      : Contains TYRCContainerTargetDraftHorizontalSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerTargetDraftHorizontalSeries;
                                                     
interface

uses
  Classes,
  Contnrs,
  VCL.Graphics,
  VCL.Controls,
  VCLTee.Series,
  UYRCSeries,
  VCLTee.Chart,
  Types,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  UAbstractObject,
  UAbstractYRCData,
  UYRCContainerAbstractLineSeries;

type

  TYRCContainerTargetDraftHorizontalSeries = class(TYRCContainerAbstractLineSeries)
  protected
    FSeriesMarksVisible : boolean;
    FHorizontalLineSeriesContainer: TObjectList;
    FOnSeriesClickPointer : TSeriesClickPointerEvent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnAfterDrawValues(Sender: TObject);

    function GetOnSeriesClickPointer : TSeriesClickPointerEvent;
    procedure SetOnSeriesClickPointer(const AValue : TSeriesClickPointerEvent);
    procedure ConfigureTargetDraftSeries(ALineSeries: TYRCLineSeries);
    procedure ConfigureHorizontalSeries(ALineSeries: TYRCLineSeries);
    procedure OnGetTargetDraftSeriesMarkText(Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    procedure RepopulateSeriesData;override;
    procedure ShowOnlySelectedTargetDraft;
    procedure ShowSelectedTargetDraft;
    procedure RestoreCustomLabelsData;
  public
    function CreateSeries(AOwner: TChart): boolean; override;
    procedure ChartSelectionHasChanged; override;
    procedure ShowSeriesMarks(AVisible:boolean);
    procedure ChartDataHasBeenReset;
    procedure SaveCustomLabelsData;
    property OnSeriesClickPointer : TSeriesClickPointerEvent read GetOnSeriesClickPointer write SetOnSeriesClickPointer;
  end;

implementation

uses
  SysUtils,Math,
  UUtilities,
  UErrorHandlingOperations,
  UYRCContainerAbstractSeries;


{ TYRCContainerTargetDraftHorizontalSeries }

procedure TYRCContainerTargetDraftHorizontalSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.CreateMemberObjects';
begin
  inherited;
  try
    //FYearsFormatStr      := '';
    //FYieldFormatStr      := '';
    FSeriesMarksVisible    := True;
    FHorizontalLineSeriesContainer := TObjectList.Create(False);
    FOnSeriesClickPointer := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FHorizontalLineSeriesContainer.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerTargetDraftHorizontalSeries.CreateSeries(AOwner: TChart): boolean;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.CreateSeries';
var
  LPlane:TAbstractYRCPlane;
  LLineSeries : TYRCLineSeries;
  LCount: integer;
begin
  Result := inherited CreateSeries(AOwner);
  try
    FHorizontalLineSeriesContainer.Clear;
    if Result then
    begin
      //FYieldFormatStr := YRCGraphDataObject.YRCLanguageStrings.YieldFormatStr;
      //FYearsFormatStr := YRCGraphDataObject.YRCLanguageStrings.YearsFormatStr;
      LPlane := YRCGraphDataObject.SelectedPlane;
      if Assigned(LPlane) then
      begin
        LLineSeries := TYRCLineSeries.Create(AOwner);
        //LLineSeries.Active := False;
        LLineSeries.PlaneIndex := LPlane.PlaneID;
        LLineSeries.OnSeriesClickPointer := FOnSeriesClickPointer;
        FLineSeriesList.AddObject('0',LLineSeries);
        AOwner.AddSeries(LLineSeries);
        ConfigureTargetDraftSeries(LLineSeries);
        LLineSeries.Title := 'FirmYield'+ IntToStr(LPlane.PlaneYears);
        for LCount := 0 to High(LPlane.YXPointArrayObject.YRCRecordPointArray) do
          LLineSeries.AddXY(LPlane.YXPointArrayObject.YRCRecordPointArray[LCount].XTValue,
                            LPlane.YXPointArrayObject.YRCRecordPointArray[LCount].YValue);

        for LCount := 0 to High(LPlane.YXPointArrayObject.YRCRecordPointArray) do
        begin
          LLineSeries := TYRCLineSeries.Create(AOwner);
          FHorizontalLineSeriesContainer.Add(LLineSeries);
          //LLineSeries.Active := False;
          FLineSeriesList.AddObject(IntToStr(LCount+1),LLineSeries);
          LLineSeries.PlaneIndex := LPlane.PlaneID;
          LLineSeries.TargetDraftIndex := LCount;
          LLineSeries.OnSeriesClickPointer := FOnSeriesClickPointer;
          LLineSeries.AfterDrawValues :=  OnAfterDrawValues;
          LLineSeries.SelectSeriesColor(LCount);
          AOwner.AddSeries(LLineSeries);
          LLineSeries.Title := 'PlaneYXPoint '+ IntToStr(LCount);
          ConfigureHorizontalSeries(LLineSeries);
          LLineSeries.AddXY(0.0,LPlane.YXPointArrayObject.YRCRecordPointArray[LCount].YValue);
          LLineSeries.AddXY(LPlane.YXPointArrayObject.YRCRecordPointArray[LCount].XTValue,
                            LPlane.YXPointArrayObject.YRCRecordPointArray[LCount].YValue);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.ConfigureTargetDraftSeries(ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ConfigureTargetDraftSeries';
begin
  try
    if assigned(ALineSeries) then
    begin
      ALineSeries.Active                 := False;
      ALineSeries.XValues.Order          := loNone;
      ALineSeries.SeriesColor            := clBlack;
      ALineSeries.Pointer.Brush.Color    := clBlack;
      ALineSeries.Pointer.InflateMargins := True;
      ALineSeries.Pointer.Style          := psCircle;
      ALineSeries.Pointer.Visible        := True;
      ALineSeries.LinePen.Width          := 2;
      ALineSeries.ShowInLegend           := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.ConfigureHorizontalSeries(ALineSeries: TYRCLineSeries);
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ConfigureHorizontalSeries';
begin
  try
    if assigned(ALineSeries) then
    begin
      ALineSeries.Active := False;
      ALineSeries.XValues.Order          := loNone;
      //ALineSeries.SeriesColor            := clBlack;
      ALineSeries.Pointer.Visible        := False;
      ALineSeries.Pointer.Style          := psCircle;
      ALineSeries.Marks.Visible          := False;
      ALineSeries.ShowInLegend           := False;
      ALineSeries.OnGetMarkText          := OnGetTargetDraftSeriesMarkText;
      ALineSeries.Marks.Arrow.Color      := clBlack;
      ALineSeries.Marks.ArrowLength      := 6;
      ALineSeries.Marks.Style            := smsLabel;
      ALineSeries.Marks.Transparent      := False;
      ALineSeries.Marks.Visible          := True;
      ALineSeries.Marks.ArrowLength      := 20;
      ALineSeries.Marks.BackColor        := clWhite;
      ALineSeries.Marks.Clip             := True;
      //ALineSeries.Marks.DrawEvery        := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.OnGetTargetDraftSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer;
          var MarkText: String);
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.OnGetTargetDraftSeriesMarkText';
var
  LYieldValue : double;
  LPlane:TAbstractYRCPlane;
  LLineSeries : TYRCLineSeries;
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  MarkText := '';
  try
    if not FSeriesMarksVisible then Exit;
    if (ValueIndex = 0) then Exit;

    LLineSeries := TYRCLineSeries(Sender);
    LPlane := YRCGraphDataObject.YRCPlane[LLineSeries.PlaneIndex];
    if(LPlane <> nil) then
    begin
      LTargetDraft := LPlane.TargetDraft[LLineSeries.TargetDraftIndex];
      if(LTargetDraft <> nil) and (LTargetDraft.LabelProperties.Custom) and (LTargetDraft.LabelProperties.Text <> '')then
      begin
        MarkText    := LTargetDraft.LabelProperties.Text;
        Exit;
      end;
    end;

    LYieldValue := Sender.YValues.Value[ValueIndex];
    MarkText := Trim(SmartFloatFormatForFiles(LYieldValue,10,6));
    if (Length(MarkText) > 2) then
      if(pos('.0',MarkText) = (Length(MarkText) - 1)) then
        MarkText := Copy(MarkText,1,Length(MarkText)-2);
      if(pos('.',MarkText) > 0) then
        MarkText := RightTrimChars(MarkText,'0');
    MarkText := MarkText  + FAppModules.Language.GetString('YieldReliability.MarkText');

    if(Sender.Marks[ValueIndex].ShapeBounds.Left = 0) then
    begin
      Sender.Marks[ValueIndex].ShapeBounds.Right := Sender.Marks[ValueIndex].ShapeBounds.Right + 5;
      Sender.Marks[ValueIndex].ShapeBounds.Left := 5;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCContainerTargetDraftHorizontalSeries.GetOnSeriesClickPointer: TSeriesClickPointerEvent;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.GetOnSeriesClickPointer';
begin
  Result := nil;
  try
    Result := FOnSeriesClickPointer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.SetOnSeriesClickPointer(const AValue: TSeriesClickPointerEvent);
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.SetOnSeriesClickPointer';
begin
  try
    FOnSeriesClickPointer := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.RepopulateSeriesData;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.RepopulateSeriesData';
begin
  inherited;
  try
    CreateSeries(FOwnerChart);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.ShowSelectedTargetDraft;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ShowSelectedTargetDraft';
var
  LIndex: integer;
  LLineSeries : TYRCLineSeries;
begin
  try
    for LIndex := 0 to  FHorizontalLineSeriesContainer.Count -1 do
    begin
      LLineSeries := TYRCLineSeries(FHorizontalLineSeriesContainer.Items[LIndex]);
      if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
        RepaintLineSeries(LLineSeries,pwThick)
      else
        RepaintLineSeries(LLineSeries,pwThin);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.ShowOnlySelectedTargetDraft;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ShowOnlySelectedTargetDraft';
var
  LIndex: integer;
  LLineSeries : TYRCLineSeries;
begin
  try
    for LIndex := 0 to  FHorizontalLineSeriesContainer.Count -1 do
    begin
      LLineSeries := TYRCLineSeries(FHorizontalLineSeriesContainer.Items[LIndex]);
      case YRCGraphDataObject.YRCChartProperties.ShowTargetDrafts of
        stdAll     :
          begin
            LLineSeries.Active := True
          end;
        stdSelected:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) then
              LLineSeries.Active := True
            else
              LLineSeries.Active := False;
          end;
        stdAdjacent:
          begin
            if(LIndex = YRCGraphDataObject.SelectedTargetDraftIndex) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex-1)) or
              (LIndex = (YRCGraphDataObject.SelectedTargetDraftIndex+1))then
              LLineSeries.Active := True
            else
              LLineSeries.Active := False;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.ChartSelectionHasChanged;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ChartSelectionHasChanged';
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

procedure TYRCContainerTargetDraftHorizontalSeries.ShowSeriesMarks(AVisible: boolean);
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ShowSeriesMarks';
var
  LIndex: integer;
  LLineSeries : TYRCLineSeries;
begin
  try
    FSeriesMarksVisible := AVisible;
    for LIndex := 0 to  FHorizontalLineSeriesContainer.Count -1 do
    begin
      LLineSeries := TYRCLineSeries(FHorizontalLineSeriesContainer.Items[LIndex]);
      LLineSeries.Repaint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.ChartDataHasBeenReset;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.ShowSeriesMarks';
var
  LCount,
  LIndex: integer;
  LLineSeries : TYRCLineSeries;
begin
  try
    for LIndex := 0 to  FHorizontalLineSeriesContainer.Count -1 do
    begin
      LLineSeries := TYRCLineSeries(FHorizontalLineSeriesContainer.Items[LIndex]);
      for LCount := 0 to LLineSeries.Marks.Items.Count-1 do
        LLineSeries.Marks.Positions[LCount].Custom := False;
      LLineSeries.Repaint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.SaveCustomLabelsData;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.SaveCustomLabelsData';
var
  LIndex: integer;
  LPlane:TAbstractYRCPlane;
  LLineSeries : TYRCLineSeries;
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  try
    for LIndex := 1 to FLineSeriesList.Count-1 do
    begin
      LLineSeries := TYRCLineSeries(FLineSeriesList.Objects[LIndex]);
      LPlane := YRCGraphDataObject.YRCPlane[LLineSeries.PlaneIndex];
      if Assigned(LPlane) then
      begin
        LTargetDraft := LPlane.TargetDraft[LLineSeries.TargetDraftIndex];
        if(LTargetDraft <> nil) and (LLineSeries.Marks.Positions.Count > 1)then
        begin
          LTargetDraft.LabelProperties.Custom   := True;
          LTargetDraft.LabelProperties.LeftTopX := Trunc(LLineSeries.Marks.Positions[1].LeftTop.X);
          LTargetDraft.LabelProperties.LeftTopY := Trunc(LLineSeries.Marks.Positions[1].LeftTop.Y);
          LTargetDraft.LabelProperties.ArrowToX := Trunc(LLineSeries.Marks.Positions[1].ArrowTo.X);
          LTargetDraft.LabelProperties.ArrowToY := Trunc(LLineSeries.Marks.Positions[1].ArrowTo.Y);
          LTargetDraft.LabelProperties.Text     := LLineSeries.Labels[1];
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.RestoreCustomLabelsData;
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.RestoreCustomLabelsData';
var
  LIndex: integer;
  LPlane:TAbstractYRCPlane;
  LLineSeries : TYRCLineSeries;
  LTargetDraft : TAbstractYRCTargetDraft;
begin
  try
    for LIndex := 1 to FLineSeriesList.Count-1 do
    begin
      LLineSeries := TYRCLineSeries(FLineSeriesList.Objects[LIndex]);
      LPlane := YRCGraphDataObject.YRCPlane[LLineSeries.PlaneIndex];
      if Assigned(LPlane) then
      begin
        LTargetDraft := LPlane.TargetDraft[LLineSeries.TargetDraftIndex];
        if(LTargetDraft <> nil) and (LTargetDraft.LabelProperties.Custom) and (LLineSeries.Marks.Positions.Count > 1)then
        begin
          LLineSeries.Marks.Positions[1].Custom    := LTargetDraft.LabelProperties.Custom;
          LLineSeries.Marks.Positions[1].LeftTop.X := LTargetDraft.LabelProperties.LeftTopX;
          LLineSeries.Marks.Positions[1].LeftTop.Y := LTargetDraft.LabelProperties.LeftTopY;
          LLineSeries.Marks.Positions[1].ArrowTo.X := LTargetDraft.LabelProperties.ArrowToX;
          LLineSeries.Marks.Positions[1].ArrowTo.Y := LTargetDraft.LabelProperties.ArrowToY;
          //LLineSeries.ParentChart.Refresh;
          LLineSeries.Active := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerTargetDraftHorizontalSeries.OnAfterDrawValues(Sender: TObject);
const OPNAME = 'TYRCContainerTargetDraftHorizontalSeries.RestoreCustomLabelsData';
begin
  try
    RestoreCustomLabelsData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
