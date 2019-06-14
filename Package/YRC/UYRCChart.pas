//
//
//  UNIT      : Contains TYRCChart Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCChart;

interface

uses
  VCL.Controls,
  VCL.ComCtrls,
  VCL.Menus,
  VCL.Dialogs,
  Contnrs,
  VCL.Graphics,
  VCL.StdCtrls,
  Classes,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  UITypes,
  UYRCSeries,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UAbstractYRCData,
  UYRCMenuItemManager,
  UYRCContainerPlaneSeries,
  UYRCContainerTargetDraftHorizontalSeries,
  UYRCContainerTargetDraftVerticalSeries,
  UYRCContainerPureRegressionPointSeries,
  UYRCContainerPureRegressionLineSeries,
  UYRCContainerRegressionPointSeries,
  UYRCContainerRegressionLineSeries,
  UYRCContainerDeterministicPointSeries,
  UYRCContainerDeterministicLineSeries;

type
  TYRCChart = class(TFieldChart)
  protected
    FDragPointIndex : integer;
    FYRCContainerPlaneSeries                          : TYRCContainerPlaneSeries;
    FYRCContainerTargetDraftVerticalSeries            : TYRCContainerTargetDraftVerticalSeries;
    FYRCContainerTargetDraftHorizontalSeries          : TYRCContainerTargetDraftHorizontalSeries;
    FYRCContainerPureRegressionPointSeries            : TYRCContainerPureRegressionPointSeries;
    FYRCContainerPureRegressionLineSeries             : TYRCContainerPureRegressionLineSeries;
    FYRCContainerRegressionPointSeries                : TYRCContainerRegressionPointSeries;
    FYRCContainerRegressionLineSeries                 : TYRCContainerRegressionLineSeries;
    FYRCContainerDeterministicPointSeries             : TYRCContainerDeterministicPointSeries;
    FYRCContainerDeterministicLineSeries              : TYRCContainerDeterministicLineSeries;

    FOnSeriesClickPointer : TSeriesClickPointerEvent;
    FYRCPopupMenu : TYRCPopupMenu;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetOnSeriesClickPointer : TSeriesClickPointerEvent;
    procedure SetOnSeriesClickPointer(const AValue : TSeriesClickPointerEvent);
    procedure ChartToBitMap(ABitMapNew: vcl.Graphics.TBitMap);
    function  SetChartProperties: boolean;
    function YRCGraphDataObject:TAbstractYRCGraphDataObject;
    procedure ChartSelectionHasChanged;
  public
    function LanguageHasChanged: boolean; override;
    function InitialiseChart: boolean;
    function RepopulateChart: boolean;

    procedure ZoomIndexHasChanged;
    procedure ZoomHasChanged;
    function  IsZoomAtItsIndex:boolean;
    procedure MaxYValueHasChanged;
    procedure ShowSeriesMarksHasChanged;
    procedure AssuranceIntervalHasChanged;
    procedure TargetDraftSelectionHasChanged;
    procedure RawPointsVisibilityHasChanged;
    procedure RawLinesVisibilityHasChanged;
    procedure FittedPointsVisibilityHasChanged;
    procedure ChartModeHasChanged;
    procedure EditModeHasChanged;
    procedure ChartDataHasBeenReset;
    procedure ApplySelectedPlotPlane;
    procedure SaveCustomLabelsData;

    procedure SelectedPlottingBaseHasChanged;
    procedure SelectedPeriodLengthHasChanged;

    function SaveState: boolean; override;
    procedure RestoreZoom;

    procedure DoCopyToClipboard; override;
    procedure DoExport(AFileName: string = ''); override;


    procedure StopEditing;
    procedure StartEditing;

    procedure OnUpdateDeterministicPoint(AXPointValue,AYPointValue: Single);
    procedure AddRegressionPoint(AXPointValue,AYPointValue: Single);
    procedure DeleteRegressionPoint(APointIndex: integer);

    function CalculateChartValuesFromPoint(AXPointValue,AYPointValue: Single; var AXValue,AYValue: double): boolean;
    function RefreshRegressionSeries: boolean;
    function RefreshDeterministicSeries: boolean;
    function  PointWithinChartRect(X,Y: Single) : boolean;


    property DragPointIndex       : integer                   read FDragPointIndex         write FDragPointIndex;
    property OnSeriesClickPointer : TSeriesClickPointerEvent  read GetOnSeriesClickPointer write SetOnSeriesClickPointer;
    property YRCPopupMenu         : TYRCPopupMenu             read FYRCPopupMenu;
  end;

implementation

uses
  Math,
  SysUtils,
  Windows,
  Types,
  VCL.Clipbrd,
  VCL.Printers,
  VCLTee.TeeProcs,
  UUtilities,
  UAbstractModelData,
  UAbstractFileNamesObject,
  UAbstractYRCModelDataObject,
  UErrorHandlingOperations;

procedure TYRCChart.CreateMemberObjects;
const OPNAME = 'TYRCChart.CreateMemberObjects';
var
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  inherited;
  try
    FDragPointIndex                   := -1;
    FOnSeriesClickPointer             := nil;
    FieldProperty                     := FAppModules.FieldProperties.FieldProperty('YRCChart');

    FYRCContainerPlaneSeries                 := TYRCContainerPlaneSeries.Create(FAppModules);
    FYRCContainerTargetDraftHorizontalSeries := TYRCContainerTargetDraftHorizontalSeries.Create(FAppModules);
    FYRCContainerTargetDraftVerticalSeries   := TYRCContainerTargetDraftVerticalSeries.Create(FAppModules);
    FYRCContainerPureRegressionPointSeries   := TYRCContainerPureRegressionPointSeries.Create(FAppModules);
    FYRCContainerPureRegressionLineSeries    := TYRCContainerPureRegressionLineSeries.Create(FAppModules);
    FYRCContainerRegressionPointSeries       := TYRCContainerRegressionPointSeries.Create(FAppModules);
    FYRCContainerRegressionLineSeries        := TYRCContainerRegressionLineSeries.Create(FAppModules);
    FYRCContainerDeterministicPointSeries    := TYRCContainerDeterministicPointSeries.Create(FAppModules);
    FYRCContainerDeterministicLineSeries     := TYRCContainerDeterministicLineSeries.Create(FAppModules);

    lFieldIndex := '';
    lKeyValues  := 'Model='           + QuotedStr(FAppModules.StudyArea.ModelCode) +
                   ',StudyAreaName='  + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                   ',SubArea='        + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                   ',Scenario='       + QuotedStr(FAppModules.StudyArea.ScenarioCode);
    HasMetaData := (FAppModules.MetaData.FindMetaData(FFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil);
    FYRCPopupMenu              := TYRCPopupMenu.Create(Self);
    FYRCPopupMenu.AppModules   := FAppModules;
    FYRCPopupMenu.Initialize;
    Self.PopupMenu   := FYRCPopupMenu; // TCustomPopupMenu(FYRCPopupMenu);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.DestroyMemberObjects;
const OPNAME = 'TYRCChart.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FYRCContainerPlaneSeries.Free;
    FYRCContainerTargetDraftHorizontalSeries.Free;
    FYRCContainerTargetDraftVerticalSeries.Free;
    FYRCContainerPureRegressionPointSeries.Free;
    FYRCContainerPureRegressionLineSeries.Free;
    FYRCContainerRegressionPointSeries.Free;
    FYRCContainerRegressionLineSeries.Free;
    FYRCContainerDeterministicPointSeries.Free;
    FYRCContainerDeterministicLineSeries.Free;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.SetChartProperties: boolean;
const OPNAME = 'TYRCChart.SetChartProperties';
begin
  Result := False;
  try

    // Set chart properties.
    Self.View3D := False;
    Self.Color  := clWindow;
    // Set chart margins.
    Self.MarginBottom := YRCGraphDataObject.YRCChartProperties.MarginBottom;
    Self.MarginLeft   := YRCGraphDataObject.YRCChartProperties.MarginLeft;
    Self.MarginRight  := YRCGraphDataObject.YRCChartProperties.MarginRight;
    Self.MarginTop    := YRCGraphDataObject.YRCChartProperties.MarginTop;

    // Set chart BottomAxis.
    Self.BottomAxis.Automatic        := True;
    Self.BottomAxis.AutomaticMaximum := False;
    Self.BottomAxis.AutomaticMinimum := False;
    Self.BottomAxis.ExactDateTime    := False;
    //Self.BottomAxis.Increment      := YRCGraphDataObject.YRCChartProperties.BottomAxisIncrement;
    Self.BottomAxis.Maximum          := YRCGraphDataObject.YRCChartProperties.BottomAxisMaximum;
    Self.BottomAxis.MinorTickCount   := YRCGraphDataObject.YRCChartProperties.BottomAxisMinorTickCount;
    Self.BottomAxis.Visible          := True;

    // Set chart LeftAxis.
    //Self.LeftAxis.Automatic          := True;
    Self.LeftAxis.AutomaticMaximum   := False;
    Self.LeftAxis.AutomaticMinimum   := False;
    Self.LeftAxis.Minimum            := 0.0;
    //Self.LeftAxis.LabelsSeparation   := 20;

    //Self.LeftAxis.ExactDateTime      := False;
    //Self.LeftAxis.Increment          := YRCGraphDataObject.YRCChartProperties.LeftAxisIncrement;
    //Self.LeftAxis.Maximum            := YRCGraphDataObject.YRCChartProperties.LeftAxisMaximum;
    Self.LeftAxis.MinorTickCount     := YRCGraphDataObject.YRCChartProperties.LeftAxisMinorTickCount;
    //Self.LeftAxis.Visible            := True;
    Self.LeftAxis.AxisValuesFormat   := '######0.###';

    // Set chart LeftAxis.
    Self.RightAxis.Automatic         := False;
    Self.RightAxis.AutomaticMaximum  := False;
    Self.RightAxis.AutomaticMinimum  := False;
    Self.RightAxis.ExactDateTime     := False;
    Self.RightAxis.Increment         := YRCGraphDataObject.YRCChartProperties.RightAxisIncrement;
    Self.RightAxis.Maximum           := YRCGraphDataObject.YRCChartProperties.RightAxisMaximum;
    Self.RightAxis.MinorTickCount    := YRCGraphDataObject.YRCChartProperties.RightAxisMinorTickCount;
    Self.RightAxis.Visible           := True;


    // Set chart Legend.
    Self.Legend.Alignment            := laBottom;
    Self.Legend.TextStyle            := ltsPlain;
    Self.Legend.TopPos               := YRCGraphDataObject.YRCChartProperties.LegendTopPos;
    Self.Legend.VertMargin           := YRCGraphDataObject.YRCChartProperties.LegendVertMargin;
    Self.Legend.Color                := clWhite;
    Self.Legend.ColorWidth           := 0;

    // Set chart title.
    Self.Title.Alignment             := taCenter;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.LanguageHasChanged: boolean;
const OPNAME = 'TYRCChart.LanguageHasChanged';
var
  LChartCaptionStr: TStringList;
  LFile: TAbstractModelFileName;
  LFileName,
  LChartCaption:string;
  LSeqCount,
  LPLottingBase,
  LPeriodLength: integer;
  LFieldProperty: TAbstractFieldProperty;
begin
  Result := False;
  try
    if (YRCGraphDataObject() = nil) then Exit;

    Result := inherited LanguageHasChanged;
    if Result and Assigned(FAppModules.FieldProperties()) then
    begin
      LFieldProperty := FAppModules.FieldProperties.FieldProperty('TYield1');
      Self.BottomAxis.Title.Caption := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.BottomAxisCaption);
      if Assigned(LFieldProperty) then
        Self.LeftAxis.Title.Caption   := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.LeftAxisCaption) +
        '('+LFieldProperty.FieldUnits+')'
      else
        Self.LeftAxis.Title.Caption   := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.LeftAxisCaption);

      LChartCaption := '';
      LChartCaptionStr := TStringList.Create;
      try
        LChartCaptionStr.Add(FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.CaptionFormatStr1));
        LChartCaptionStr.Add(FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.CaptionFormatStr2));
        LChartCaption := LChartCaptionStr.Text;
      finally
        LChartCaptionStr.Free;
      end;


      if(Trim(LChartCaption) <> '') then
      begin

        LPLottingBase := YRCGraphDataObject.PlottingBase;
        LPeriodLength := YRCGraphDataObject.PeriodLength;
        LSeqCount     := YRCGraphDataObject.SequencesCount;
        LFileName     := '';
        LFile         := TAbstractModelData(FAppModules.Model.ModelData).FileNamesObject.GetSumOutFile;
        if(LFile <> nil) then
          LFileName     := ExtractFileName(LFile.ShortName);
        LChartCaption := Format(LChartCaption,[LFileName,LSeqCount,LPLottingBase,LPeriodLength]);
      end;

      Self.Title.Text.Text := LChartCaption;

      Result := FYRCContainerPlaneSeries.LanguageHasChanged and
                FYRCContainerTargetDraftHorizontalSeries.LanguageHasChanged and
                FYRCContainerTargetDraftVerticalSeries.LanguageHasChanged and
                FYRCContainerPureRegressionPointSeries.LanguageHasChanged and
                FYRCContainerPureRegressionLineSeries.LanguageHasChanged  and
                FYRCContainerRegressionPointSeries.LanguageHasChanged and
                FYRCContainerRegressionLineSeries.LanguageHasChanged and
                FYRCContainerDeterministicPointSeries.LanguageHasChanged and
                FYRCContainerDeterministicLineSeries.LanguageHasChanged;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.InitialiseChart: boolean;
const OPNAME = 'TYRCChart.InitialiseChart';
begin
  Result := False;
  try
    // Clear the series.
    Self.RemoveAllSeries;

    // Set chart properties.
    Result := SetChartProperties;

    // Recreate the series.
    RepopulateChart;

    MaxYValueHasChanged;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{procedure TYRCChart.SaveChart;
const OPNAME = 'TYRCChart.SaveChart';
var
  LPrimaryKey : array of string;
begin
  try
    if (YRCGraphDataObject() <> nil) then
    begin
      SetLength(LPrimaryKey,6);
      try
        LPrimaryKey[0] := FAppModules.Database.DatabaseName;
        LPrimaryKey[1] := FAppModules.StudyArea.ModelCode;
        LPrimaryKey[2] := FAppModules.StudyArea.StudyAreaCode;
        LPrimaryKey[3] := FAppModules.StudyArea.SubAreaCode;
        LPrimaryKey[4] := FAppModules.StudyArea.ScenarioCode;
        LPrimaryKey[5] := '0';
        YRCGraphDataObject.SaveDataToDB(LPrimaryKey);
      finally
        SetLength(LPrimaryKey,0);
        LPrimaryKey := nil;
      end;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TYRCChart.CalculateChartValuesFromPoint(AXPointValue, AYPointValue: Single;
         var AXValue, AYValue: double): boolean;
const OPNAME = 'TYRCChart.CalculateChartValuesFromPoint';
begin
  Result := False;
  AXValue := 0.0;
  AYValue := 0.0;
  try
    AXValue := Self.BottomAxis.CalcPosPoint(Trunc(AXPointValue));
    AYValue := Self.LeftAxis.CalcPosPoint(Trunc(AYPointValue));
    Result := True;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.AddRegressionPoint(AXPointValue,AYPointValue: Single);
const OPNAME = 'TYRCChart.AddRegressionPoint';
var
 LPointArray: TYRCRecordPointArray;
 LXValue,
 LYValue: double;
 LTargetDraft: TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := YRCGraphDataObject.YRCTargetDraft[YRCGraphDataObject.PlaneIndex,YRCGraphDataObject.SelectedTargetDraftIndex];
    if Assigned(LTargetDraft) then
    begin
      if CalculateChartValuesFromPoint(AXPointValue,AYPointValue,LXValue,LYValue) then
      begin
        SetLength(LPointArray,1);
        try
          LPointArray[0].XValue  := ConvertXValueToPlottingBase(LXValue,YRCGraphDataObject.PlottingBase,YRCGraphDataObject.PeriodLength);
          LPointArray[0].XTValue := LXValue;
          LPointArray[0].YValue  := LYValue;

          if LTargetDraft.AddRegressionPoints(LPointArray) then
          begin
            if RefreshRegressionSeries then
              Self.CancelMouse := True;
          end;
        finally
          LPointArray := nil;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.RefreshRegressionSeries: boolean;
const OPNAME = 'TYRCChart.RefreshRegressionSeries';
begin
  Result := False;
  try
    FYRCContainerRegressionPointSeries.ChartSelectionHasChanged;
    FYRCContainerRegressionLineSeries.ChartSelectionHasChanged;
    FYRCContainerDeterministicPointSeries.ChartSelectionHasChanged;
    FYRCContainerDeterministicLineSeries.ChartSelectionHasChanged;
    FYRCContainerPureRegressionPointSeries.ChartSelectionHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.RefreshDeterministicSeries: boolean;
const OPNAME = 'TYRCChart.RefreshDeterministicSeries';
begin
  Result := False;
  try
    FYRCContainerDeterministicPointSeries.ChartSelectionHasChanged;
    FYRCContainerDeterministicLineSeries.ChartSelectionHasChanged;
    FYRCContainerRegressionPointSeries.ChartSelectionHasChanged;
    FYRCContainerRegressionLineSeries.ChartSelectionHasChanged;
    FYRCContainerPureRegressionPointSeries.ChartSelectionHasChanged;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.DeleteRegressionPoint(APointIndex: integer);
const OPNAME = 'TYRCChart.DeleteRegressionPoint';
var
 LTargetDraft: TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := YRCGraphDataObject.YRCTargetDraft[YRCGraphDataObject.PlaneIndex,YRCGraphDataObject.SelectedTargetDraftIndex];
    if Assigned(LTargetDraft) then
    begin
      if LTargetDraft.DeleteRegressionPoint(APointIndex) then
      begin
        RefreshRegressionSeries;
        Self.CancelMouse := True;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.OnUpdateDeterministicPoint(AXPointValue,AYPointValue: Single);
const OPNAME = 'TYRCChart.OnUpdateDeterministicPoint';
var
 LXValue,
 LXTValue,
 LYValue: double;
 LTargetDraft: TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := YRCGraphDataObject.YRCTargetDraft[YRCGraphDataObject.PlaneIndex,YRCGraphDataObject.SelectedTargetDraftIndex];
    if Assigned(LTargetDraft) then
    begin
      if CalculateChartValuesFromPoint(AXPointValue,AYPointValue,LXTValue,LYValue) then
      begin
        LXValue := ConvertXValueToPlottingBase(LXTValue,YRCGraphDataObject.PlottingBase,YRCGraphDataObject.PeriodLength);
        if LTargetDraft.UpdateDeterministicPoint(FDragPointIndex,LXValue,LXTValue,LYValue) then
        begin
          RefreshDeterministicSeries;
          Self.CancelMouse := True;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.DoCopyToClipboard;
const OPNAME = 'TYRCChart.DoCopyToClipboard';
var
  LBitmap: vcl.Graphics.TBitmap;
  LBMPFormat: word;
  LData: THandle; //cardinal;
  LPalette: HPALETTE;
begin
  try
    // Create a bitmap for the clipboard output,
    // Generate the export format and store it on the clipboard
    LBitmap := vcl.Graphics.TBitmap.Create;
    try
      ChartToBitMap(LBitmap);
      LBitmap.SaveToClipboardFormat(LBMPFormat, LData, LPalette);
      ClipBoard.SetAsHandle(LBMPFormat, LData);
    finally
      FreeAndNil(LBitmap);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.DoExport(AFileName: string = '');
const OPNAME = 'TYRCChart.DoExport';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.ChartToBitMap(ABitMapNew: vcl.Graphics.TBitMap);
const OPNAME = 'TYRCChart.ChartToBitMap';
var LUserRect : TRectF;
    LPointF   : TPointF;
begin
  try
    // For the export of the graph, we only need to call the Draw method supplied
    // by the graph itself
    ABitMapNew.Width  := Trunc(Self.Width);
    ABitMapNew.Height := Trunc(Self.Height);
    LPointF := TPointF.Create(0,0);
    LUserRect := TRectF.Create(LPointF,ABitMapNew.width,ABitMapNew.Height);
    Self.Draw(ABitMapNew.Canvas, Bounds(0, 0, ABitMapNew.Width, ABitMapNew.Height));

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.PointWithinChartRect(X, Y: Single): boolean;
const OPNAME = 'TYRCChart.PointWithinChartRect';
begin
  Result := False;
  try
    Result := (X >= Self.ChartRect.Left) and
              (X <= Self.ChartRect.Right) and
              (Y >= Self.ChartRect.Top) and
              (Y <= Self.ChartRect.Bottom);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TYRCChart.DoExit;
const OPNAME = 'TYRCChart.DoExit';
begin
  inherited;
  try
    if (FChartEditMode = tdmDeterministic) then
    begin
      FChartEditMode := tdmNone;
      FDragPointIndex := -1;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;

end;}

procedure TYRCChart.StopEditing;
const OPNAME = 'TYRCChart.StopEditing';
begin
  try
    //FChartEditMode  := tdmNone;
    FDragPointIndex := -1;
    ChartSelectionHasChanged;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
 end;

procedure TYRCChart.StartEditing;
const OPNAME = 'TYRCChart.StartEditing';
begin
  try
    ChartSelectionHasChanged;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.GetOnSeriesClickPointer: TSeriesClickPointerEvent;
const OPNAME = 'TYRCChart.GetOnSeriesClickPointer';
begin
  Result := nil;
  try
    Result := FOnSeriesClickPointer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.SetOnSeriesClickPointer(const AValue: TSeriesClickPointerEvent);
const OPNAME = 'TYRCChart.SetOnSeriesClickPointer';
begin
  try
    FOnSeriesClickPointer := AValue;
    FYRCContainerTargetDraftHorizontalSeries.OnSeriesClickPointer := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChart.YRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYRCChart.YRCGraphDataObject';
begin
  Result := nil;
  try
    Result := TAbstractYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.RepopulateChart:boolean;
const OPNAME = 'TYRCChart.RepopulateChart';
begin
  Result := False;
  try
    // Create Plane lines.
    Result :=  FYRCContainerPlaneSeries.CreateSeries(Self);

    // Create horizontal lines.
    Result :=  Result and FYRCContainerTargetDraftHorizontalSeries.CreateSeries(Self);

    // Create vertical lines.
    Result :=  Result and FYRCContainerTargetDraftVerticalSeries.CreateSeries(Self);

    // Create pure regression point series.
    Result :=  Result and FYRCContainerPureRegressionPointSeries.CreateSeries(Self);

    // Create pure regression line series.
    Result :=  Result and FYRCContainerPureRegressionLineSeries.CreateSeries(Self);

    // Create regression point series.
    Result :=  Result and FYRCContainerRegressionPointSeries.CreateSeries(Self);

    // Create regression line series.
    Result :=  Result and FYRCContainerRegressionLineSeries.CreateSeries(Self);

    // Create Deterministic point series.
    Result :=  Result and FYRCContainerDeterministicPointSeries.CreateSeries(Self);

    // Create Deterministic line series.
    Result :=  Result and FYRCContainerDeterministicLineSeries.CreateSeries(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.ChartSelectionHasChanged;
const OPNAME = 'TYRCChart.ChartSelectionHasChanged';
begin
  try
    FYRCContainerPlaneSeries.ChartSelectionHasChanged;
    FYRCContainerTargetDraftHorizontalSeries.ChartSelectionHasChanged;
    FYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged;
    FYRCContainerPureRegressionPointSeries.ChartSelectionHasChanged;
    FYRCContainerPureRegressionLineSeries.ChartSelectionHasChanged;
    FYRCContainerRegressionPointSeries.ChartSelectionHasChanged;
    FYRCContainerRegressionLineSeries.ChartSelectionHasChanged;
    FYRCContainerDeterministicPointSeries.ChartSelectionHasChanged;
    FYRCContainerDeterministicLineSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//_________________________________________________________________________________________________________________
procedure TYRCChart.ZoomIndexHasChanged;
const OPNAME = 'TYRCChart.ZoomIndexHasChanged';
begin
  try
    if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 0) then
    begin
      Self.BottomAxis.Automatic := False;
      Self.BottomAxis.Minimum   := 0.00;
      Self.BottomAxis.Maximum   := 100.00;
      Self.LeftAxis.Minimum     := 0.00;
      Self.LeftAxis.Maximum     := YRCGraphDataObject.YRCChartProperties.LeftAxisMaximum;
      Self.BottomAxis.AdjustMaxMin;
      Self.LeftAxis.AdjustMaxMin;
    end
    else if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 1) then
    begin
      Self.BottomAxis.Automatic := False;
      Self.BottomAxis.Maximum   := 100.00;
      Self.BottomAxis.Minimum   := 75.00;
      Self.BottomAxis.AdjustMaxMin;
    end
    else if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 2) then
    begin
      Self.BottomAxis.Automatic := False;
      Self.BottomAxis.Maximum   := 100.00;
      Self.BottomAxis.Minimum   := YRCGraphDataObject.YRCChartProperties.ZoomValue;
      Self.BottomAxis.AdjustMaxMin;
    end
    else if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 3) then
    begin
      Self.BottomAxis.Automatic := False;
      Self.BottomAxis.Maximum   := YRCGraphDataObject.YRCChartProperties.BottomAxisMaximum;
      Self.BottomAxis.Minimum   := YRCGraphDataObject.YRCChartProperties.BottomAxisMinimum;
      Self.BottomAxis.AdjustMaxMin;
    end;

    Self.LeftAxis.Automatic := False;
    Self.LeftAxis.Maximum   := YRCGraphDataObject.YRCChartProperties.LeftAxisMaximum;
    Self.LeftAxis.Minimum   := YRCGraphDataObject.YRCChartProperties.LeftAxisMinimum;

    Self.RightAxis.Automatic := False;
    Self.RightAxis.Maximum   := YRCGraphDataObject.YRCChartProperties.RightAxisMaximum;
    Self.RightAxis.Minimum   := YRCGraphDataObject.YRCChartProperties.RightAxisMinimum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChart.IsZoomAtItsIndex: boolean;
const OPNAME = 'TYRCChart.IsZoomAtItsIndex';
begin
  Result := False;
  try
    if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 0) then
    begin
      Result := (Self.BottomAxis.Minimum   = 0.00) and
                (Self.BottomAxis.Maximum   = 100.00);
    end
    else if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 1) then
    begin
      Result := (Self.BottomAxis.Minimum   = 75.00) and
                (Self.BottomAxis.Maximum   = 100.00);
    end
    else if (YRCGraphDataObject.YRCChartProperties.ZoomIndex = 2) then
    begin
      Result := (Self.BottomAxis.Minimum  = YRCGraphDataObject.YRCChartProperties.ZoomValue) and
                (Self.BottomAxis.Maximum  = 100.00);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.MaxYValueHasChanged;
const OPNAME = 'TYRCChart.MaxYValueHasChanged';
begin
  try
    Self.LeftAxis.Maximum    := YRCGraphDataObject.YRCChartProperties.MaxYValue;
    FYRCContainerTargetDraftVerticalSeries.MaxYValueHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.AssuranceIntervalHasChanged;
const OPNAME = 'TYRCChart.AssuranceIntervalHasChanged';
begin
  try
    FYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.ShowSeriesMarksHasChanged;
const OPNAME = 'TYRCChart.ShowSeriesMarksHasChanged';
begin
  try
    FYRCContainerTargetDraftHorizontalSeries.ShowSeriesMarks(YRCGraphDataObject.YRCChartProperties.ShowFirmYieldLabels);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.TargetDraftSelectionHasChanged;
const OPNAME = 'TYRCChart.TargetDraftSelectionHasChanged';
begin
  try
    ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.RawLinesVisibilityHasChanged;
const OPNAME = 'TYRCChart.RawLinesVisibilityHasChanged';
begin
  try
    FYRCContainerPureRegressionLineSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.RawPointsVisibilityHasChanged;
const OPNAME = 'TYRCChart.RawPointsVisibilityHasChanged';
begin
  try
    FYRCContainerPureRegressionPointSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.FittedPointsVisibilityHasChanged;
const OPNAME = 'TYRCChart.FittedPointsVisibilityHasChanged';
begin
  try
    FYRCContainerRegressionPointSeries.ChartSelectionHasChanged;
    FYRCContainerDeterministicPointSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.ChartDataHasBeenReset;
const OPNAME = 'TYRCChart.ChartDataHasBeenReset';
begin
  try
    ChartSelectionHasChanged;
    FYRCContainerTargetDraftHorizontalSeries.ChartDataHasBeenReset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.ApplySelectedPlotPlane;
const OPNAME = 'TYRCChart.ApplySelectedPlotPlane';
begin
  try
    ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.ChartModeHasChanged;
const OPNAME = 'TYRCChart.ChartModeHasChanged';
begin
  try
    ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChart.EditModeHasChanged;
const OPNAME = 'TYRCChart.EditModeHasChanged';
begin
  try
    ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCChart.SelectedPlottingBaseHasChanged;
const OPNAME = 'TYRCChart.SelectedPlottingBaseHasChanged';
begin
  try
    LanguageHasChanged;
    FYRCContainerPlaneSeries.ChartSelectionHasChanged;
    FYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.SelectedPeriodLengthHasChanged;
const OPNAME = 'TYRCChart.SelectedPeriodLengthHasChanged';
begin
  try
    LanguageHasChanged;
    FYRCContainerPlaneSeries.ChartSelectionHasChanged;
    FYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCChart.SaveState: boolean;
const OPNAME = 'TYRCChart.SaveState';
begin
  Result := False;
  try
    YRCGraphDataObject.YRCChartProperties.MarginBottom              := Self.MarginBottom;
    YRCGraphDataObject.YRCChartProperties.MarginLeft                := Self.MarginLeft;
    YRCGraphDataObject.YRCChartProperties.MarginRight               := Self.MarginRight;
    YRCGraphDataObject.YRCChartProperties.MarginTop                 := Self.MarginTop;
    YRCGraphDataObject.YRCChartProperties.BottomAxisMinimum         := Self.BottomAxis.Minimum;
    YRCGraphDataObject.YRCChartProperties.BottomAxisIncrement       := Self.BottomAxis.Increment;
    YRCGraphDataObject.YRCChartProperties.BottomAxisMaximum         := Self.BottomAxis.Maximum;
    YRCGraphDataObject.YRCChartProperties.BottomAxisMinorTickCount  := Self.BottomAxis.MinorTickCount;
    YRCGraphDataObject.YRCChartProperties.LeftAxisMinimum           := Self.LeftAxis.Minimum;
    YRCGraphDataObject.YRCChartProperties.LeftAxisIncrement         := Self.LeftAxis.Increment;
    YRCGraphDataObject.YRCChartProperties.LeftAxisMaximum           := Self.LeftAxis.Maximum;
    YRCGraphDataObject.YRCChartProperties.LeftAxisMinorTickCount    := Self.LeftAxis.MinorTickCount;
    YRCGraphDataObject.YRCChartProperties.RightAxisMinimum          := Self.RightAxis.Minimum;
    YRCGraphDataObject.YRCChartProperties.RightAxisIncrement        := Self.RightAxis.Increment;
    YRCGraphDataObject.YRCChartProperties.RightAxisMaximum          := Self.RightAxis.Maximum;
    YRCGraphDataObject.YRCChartProperties.RightAxisMinorTickCount   := Self.RightAxis.MinorTickCount;
    YRCGraphDataObject.YRCChartProperties.LegendTopPos              := Self.Legend.TopPos;
    YRCGraphDataObject.YRCChartProperties.LegendVertMargin          := Self.Legend.VertMargin;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.RestoreZoom;
const OPNAME = 'TYRCChart.RestoreZoom';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.ZoomHasChanged;
const OPNAME = 'TYRCChart.ZoomHasChanged';
begin
  try
    FYRCContainerTargetDraftVerticalSeries.ChartSelectionHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCChart.SaveCustomLabelsData;
const OPNAME = 'TYRCChart.SaveCustomLabelsData';
begin
  try
    FYRCContainerTargetDraftHorizontalSeries.SaveCustomLabelsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
