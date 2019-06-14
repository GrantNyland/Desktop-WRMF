//
//
//  UNIT      : Contains TTimeSeriesComparitorChartPanel Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorChartPanel;

interface
uses
  VCLTee.TeEngine,
  VCLTee.Series,
  Windows,
  Types,
  UITypes,
  VCL.Grids,
  Classes,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.ComCtrls,
  Contnrs,
  VCL.StdCtrls,
  VCLTee.Chart,
  VCLTee.TeeProcs,
  UTimeSeriesComparitorXYAxisReset,
  UAbstractObject,
  UAbstractComponent;

type
  TChartAction = (caNone,caZoom,caUnZoom,caScroll);
  TAbstractChartPanel = class(TAbstractPanel)
  protected
    FZoomActiveChartOnly : boolean;
  public
    function ShowChartInPanel(AChart    : TAbstractChart;
                              ASelected : Boolean): boolean; virtual; abstract;
    function RemoveChartFromPanel(AChart: TAbstractChart): boolean;virtual; abstract;
    function ChartCount: integer;virtual; abstract;
    function SeriesCount: integer;virtual; abstract;
    function HeaderCaption: TPanel;virtual; abstract;
    function FooterCaption: TPanel;virtual; abstract;
    procedure SetChartName; virtual; abstract;
    procedure SynchroniseChartsXAxis; virtual; abstract;
    procedure FixChartsXAxis; virtual; abstract;
    property ZoomActiveChartOnly : boolean read FZoomActiveChartOnly write FZoomActiveChartOnly;
  end;

  TTimeSeriesComparitorChartPanel = class(TAbstractChartPanel)
  protected
    FChartList: TObjectList;
    FHeaderCaption,
    FFooterCaption: TPanel;
    FCurrentAction : TChartAction;
    FActionChart,
    FCurrentChart: TAbstractChart;

    FSetChartManualy : boolean;
    FCurrentView : TObject;
    FMinXValue : double;
    FMinYValue : TDateTime;
    FMaxXValue : double;
    FMaxYValue : TDateTime;
    FInc       : double;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ReduceSecondAspectRatioToFirst(AX1, AY1 : integer; var AX2,AY2 : integer);

    procedure SynchroniseCharts;
    procedure OnZoomChart(Sender: TObject);
    procedure OnUndoZoomChart(Sender: TObject);
    procedure OnScrollChart(Sender: TObject);
    procedure OnAfterDrawChart(Sender: TObject);
    procedure SetChartMinMaxValues(aInc, aMinX, aMaxY, aMaxX, aMinY : double);
    procedure SetChartLegend(ALegendAlignment : TLegendAlignment; ALegendVisible : boolean);
    procedure ChartOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChartOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure DoOnClick(Sender : TObject; var aMinXValue : double; var aMinYValue  : TDateTime;
                           var aMaxXValue : double; var aMaxYValue : TDateTime; var aIncValue : double );
    function PopulateChartCaptions(ASource: TStringGrid; AFromDisplay: boolean): boolean;

  public
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;

    function ShowChartInPanel(AChart    : TAbstractChart;
                              ASelected : Boolean): boolean; override;
    function RemoveChartFromPanel(AChart: TAbstractChart): boolean;override;
    procedure SetChartName; override;
    procedure SynchroniseChartsXAxis; override;
    procedure FixChartsXAxis; override;
    function HeaderCaption: TPanel; override;
    function FooterCaption: TPanel;override;

    function CanCopyToClipboard: boolean;override;
    function CanExport: boolean;override;
    function CanPrint: boolean;override;

    function ChartCount: integer;override;
    function SeriesCount: integer;override;
    procedure DoChartOnDblClick(Sender : TObject);
    procedure SetInitialValues(aSetXYAxisDialog : TTimeSeriesComparitorXYAxisReset; aInc, aMinX, aMaxY, aMaxX, aMinY : double);

    procedure DoCopyToClipboard;override;
    procedure DoExport(AFileName: string = '');override;
    procedure DoPrint;override;
  end;

implementation
uses
  Math,
  VCL.Printers,
  VCL.Dialogs,
  VCL.Clipbrd,
  VCL.Graphics,
  SysUtils,
  VCLTee.TeExport,
  VCL.Forms,

  UConstants,
  UTimeSeriesComparitorCaptionSelector,
  UTimeSeriesComparitorLinkClasses,
  UErrorHandlingOperations;

const
  ChartActiveColor   = clBtnFace;
  ChartInActiveColor = clWhite;
{ TTimeSeriesComparitorChartPanel }

procedure TTimeSeriesComparitorChartPanel.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartPanel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCurrentAction := caNone ;
    FActionChart   := nil;
    FCurrentChart  := nil;

    //Self.BevelOuter := bvRaised;
    Self.Caption := '';
    //Self.Color   := clRed;

    FChartList            := TObjectList.Create(False);
    FHeaderCaption        := TPanel.Create(Self);
    FFooterCaption        := TPanel.Create(Self);
    //FHeaderCaption.Color  := clRed;
    //FFooterCaption.Color  := clRed;
    FHeaderCaption.Align  := alTop;
    FFooterCaption.Align  := alBottom;
    FHeaderCaption.Parent := Self;
    FFooterCaption.Parent := Self;
    FHeaderCaption.Height := 18;
    FFooterCaption.Height := 18;
    FHeaderCaption.Alignment := taCenter;
    FFooterCaption.Alignment := taCenter;
    FHeaderCaption.Visible   := False;
    FFooterCaption.Visible   := False;
    FHeaderCaption.Caption   := '';
    FFooterCaption.Caption   := '';
    FHeaderCaption.Font.Color := clBlue;
    FFooterCaption.Font.Color := clBlue;

    FHeaderCaption.BevelInner := PanelBevelInnerInActive;
    FFooterCaption.BevelInner := PanelBevelInnerInActive;

    FHeaderCaption.BevelOuter := PanelBevelInnerActive;
    FFooterCaption.BevelOuter := PanelBevelInnerActive;

    Self.BevelInner :=   TBevelCut(bvLowered);  //    bvLowered;
    Self.BevelOuter :=   PanelBevelInnerInActive;

    FHeaderCaption.Color   := ChartInActiveColor;
    FFooterCaption.Color   := ChartInActiveColor;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TTimeSeriesComparitorChartPanel.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartPanel.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChartList);
    inherited DestroyMemberObjects;
    if Assigned ( FCurrentView ) then
      FreeAndnil ( FCurrentView );
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartPanel.LanguageHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartPanel.ShowChartInPanel (AChart: TAbstractChart;
                                                           ASelected : Boolean) : boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.ShowChartInPanel';
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      AChart.Parent := VCLTee.Chart.TChart(Self);
      AChart.Visible := True;
      if FChartList.IndexOf(AChart) < 0 then
        FChartList.Add(AChart);
      FHeaderCaption.Visible  := FChartList.Count > 1;
      FFooterCaption.Visible  := FChartList.Count > 1;
      Resize;
      if ASelected then
        AChart.Color := ChartActiveColor
      else
        AChart.Color := ChartInActiveColor;

      AChart.OnZoom      := OnZoomChart;
      AChart.OnUndoZoom  := OnUndoZoomChart;
      AChart.OnScroll    := OnScrollChart;

      AChart.OnMouseUp   := ChartOnMouseUp;
      AChart.OnMouseDown := ChartOnMouseDown;
      AChart.OnAfterDraw := OnAfterDrawChart;
      //AChart.OnDblClick  := DoChartOnDblClick;
      FCurrentChart      := AChart;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartPanel.RemoveChartFromPanel(AChart: TAbstractChart): boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.RemoveChartFromPanel';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      LIndex := FChartList.IndexOf(AChart);
      if(LIndex >= 0) then
      begin
        TAbstractChart(FChartList[LIndex]).Parent := nil;
        FChartList.Delete(LIndex);
        FCurrentChart := nil;
        if(FActionChart = AChart) then
          FActionChart:= nil;

        FHeaderCaption.Visible  := FChartList.Count > 1;
        FFooterCaption.Visible  := FChartList.Count > 1;
        Resize;

        AChart.OnZoom      := nil;
        AChart.OnUndoZoom  := nil;
        AChart.OnScroll    := nil;
        AChart.OnMouseUp   := nil;
        AChart.OnMouseDown := nil;
        AChart.OnAfterDraw := nil;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartPanel.Resize;
const OPNAME = 'TTimeSeriesComparitorChartPanel.Resize';
var
  LCount,
  LTop,
  LHeight: integer;
  LChart: TAbstractChart;
begin
  inherited Resize;
  try
    if not FHeaderCaption.Visible then
    begin
      LTop := 0;
      LHeight := Self.Height div Max(1,FChartList.Count);
    end
    else
    begin
      LTop := FHeaderCaption.Top + FHeaderCaption.Height;
      LHeight := FFooterCaption.Top - FHeaderCaption.Top - FHeaderCaption.Height;
      LHeight := LHeight div Max(1,FChartList.Count);
    end;

    for LCount := 0 to FChartList.Count - 1 do
    begin
      LChart := TAbstractChart(FChartList[LCount]);
      TControl(LChart).Top  := LTop;
      TControl(LChart).Left := 0;
      LChart.Width := Self.Width;
      LChart.Height := LHeight;
      LTop := LTop + LHeight;

      LChart.BevelInner := PanelBevelInnerInActive;
      LChart.BevelOuter := PanelBevelOuterInActive;
    end;
    FHeaderCaption.BringToFront;
    FFooterCaption.BringToFront;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartPanel.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    for LIndex := 0 to  FChartList.Count - 1 do
      RemoveChartFromPanel(TAbstractChart(FChartList[0]));
    FChartList.Clear;
    FActionChart := nil;
    FCurrentChart:= nil;
    FHeaderCaption.Visible := False;
    FFooterCaption.Visible  := False;
    FSetChartManualy := False;

    FMinXValue := 0;
    FMinYValue := 0;
    FMaxXValue := 0;
    FMaxYValue := 0;
    FInc       := 0;
    FCurrentView := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartPanel.CanCopyToClipboard: boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := SeriesCount > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChartPanel.CanExport: boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.CanExport';
begin
  Result := False;
  try
    Result := SeriesCount > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChartPanel.CanPrint: boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.CanPrint';
begin
  Result := False;
  try
    Result := SeriesCount > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.DoCopyToClipboard;
const OPNAME = 'TTimeSeriesComparitorChartPanel.DoCopyToClipboard';
var
  LBitmap: TBitmap;
begin
  try
    LBitmap := TBitmap.Create;
    if Assigned(FCurrentChart) then
      FCurrentChart.Color := ChartInActiveColor;
    try
      LBitmap.Width  := Self.Width;
      LBitmap.Height := Self.Height;
      Self.PaintTo(LBitmap.Canvas,0,0);
      Clipboard.Assign(LBitmap);
    finally
      FreeAndNil(LBitmap);
      if Assigned(FCurrentChart) then
         FCurrentChart.Color := ChartActiveColor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.DoExport(AFileName: string = '');
const OPNAME = 'TTimeSeriesComparitorChartPanel.DoExport';
{var
  LBitmap : TBitmap;
  LDialog : TSaveDialog;}
begin
  try
    if Assigned(FCurrentChart) then
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FCurrentChart));
    {LBitmap := TBitmap.Create;
    if Assigned(FCurrentChart) then
       FCurrentChart.Color := ChartInActiveColor;
    try
      LBitmap.Width  := Self.Width;
      LBitmap.Height := Self.Height;
      Self.PaintTo(LBitmap.Canvas,0,0);

      LDialog := TSaveDialog.Create(nil);
      try
        LDialog.DefaultExt  := 'BMP';
        LDialog.Filter      := 'Bitmap files (*.bmp)|*.BMP';
        LDialog.FilterIndex := 1;
        if LDialog.Execute then
          LBitmap.SaveToFile(LDialog.FileName);
      finally
        FreeAndNil(LDialog);
      end;
    finally
      FreeAndNil(LBitmap);
      if Assigned(FCurrentChart) then
         FCurrentChart.Color := ChartActiveColor;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.DoPrint;
const OPNAME = 'TTimeSeriesComparitorChartPanel.DoPrint';
var
  LPrinterOrientation:TPrinterOrientation;
var
  LBitmap: TBitmap;
  LX1     : integer;
  LY1    : integer;
  LX2     : integer;
  LY2     : integer;
begin
  try
    if (Printer.Printers.Count > 0) then
    begin
      LBitmap := TBitmap.Create;
      if Assigned(FCurrentChart) then
        FCurrentChart.Color := ChartInActiveColor;
      try
        LBitmap.Width  := Self.Width;
        LBitmap.Height := Self.Height;
        Self.PaintTo(LBitmap.Canvas,0,0);

        LPrinterOrientation := Printer.Orientation;
        try
          Printer.Orientation := poLandscape;
          LX1 := LBitmap.Width;
          LY1 := LBitMap.Height;
          LX2 := Printer.PageWidth;
          LY2 := Printer.PageHeight;
          ReduceSecondAspectRatioToFirst(LX1,LY1,LX2,LY2);
          if LX2 < Printer.PageWidth then
          begin
            LX1 := ((Printer.PageWidth - LX2) div 2);
            LY1 := 0;
            LX2 := LX2 + ((Printer.PageWidth - LX2) div 2);
            LY2 := LY2;
          end;
          if LY2 < Printer.PageHeight then
          begin
            LX1 := 0;
            LY1:= ((Printer.PageHeight - LY2) div 2);
            LX2 := LX2;
            LY2 := LY2 + ((Printer.PageHeight - LY2) div 2);
          end;
          Printer.BeginDoc;
          Printer.Canvas.Stretchdraw(Rect(LX1, LY1 ,LX2 , LY2), LBitMap);
          Printer.EndDoc;
        finally
          Printer.Orientation := LPrinterOrientation;
        end;
      finally
        FreeAndNil(LBitmap);
        if Assigned(FCurrentChart) then
          FCurrentChart.Color := ChartActiveColor;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChartPanel.FooterCaption: TPanel;
const OPNAME = 'TTimeSeriesComparitorChartPanel.FooterCaption';
begin
  Result := nil;
  try
    Result := FFooterCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChartPanel.HeaderCaption: TPanel;
const OPNAME = 'TTimeSeriesComparitorChartPanel.HeaderCaption';
begin
  Result := nil;
  try
    Result := FHeaderCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChartPanel.SeriesCount: integer;
const OPNAME = 'TTimeSeriesComparitorChartPanel.SeriesCount';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to  FChartList.Count - 1 do
      Result := Result + TAbstractChart(FChartList[LIndex]).SeriesCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTimeSeriesComparitorChartPanel.ChartCount: integer;
const OPNAME = 'TTimeSeriesComparitorChartPanel.ChartCount';
begin
  Result := 0;
  try
    Result := FChartList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.ReduceSecondAspectRatioToFirst(AX1, AY1 : integer; var AX2,AY2 : integer);
const OPNAME = 'TTimeSeriesComparitorChartPanel.ReduceSecondAspectRatioToFirst';
var
  LAspectRatio : Double;
begin
  // This function "reduces" the denominator or numerator of second ratio ( AX2/AY2 )to give the same
  // ratio as the first ( AX1/AY2 ).
  try
    if (AY1 <> 0) and (AY2 <> 0) then
    begin
      LAspectRatio := (AX1 / AY1);
      if LAspectRatio > (AX2 / AY2) then
      begin
        AY2 := Trunc(AX2 / LAspectRatio);
      end else begin
        AX2 := Trunc(AY2 * LAspectRatio);
      end;
    end else begin
      AX2 := 0;
      AY2 := 0;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartPanel.OnScrollChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorChartPanel.OnScrollChart';
begin
  try
    FCurrentAction := caScroll;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.OnUndoZoomChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorChartPanel.OnUndoZoomChart';
begin
  try
    FCurrentAction := caUnZoom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.OnZoomChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorChartPanel.OnZoomChart';
begin
  try
    FCurrentAction := caZoom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.SynchroniseCharts;
const OPNAME = 'TTimeSeriesComparitorChartPanel.SynchroniseCharts';
var
  LIndex: integer;
begin
  try
    if not Assigned(FActionChart) then Exit;
    if (FCurrentAction = caNone) then Exit;
    if ZoomActiveChartOnly then Exit;

    for LIndex := 0 to  FChartList.Count - 1 do
    begin
      if (FActionChart <> FChartList[LIndex]) then
      begin
        TAbstractChart(FChartList[LIndex]).OnZoom     := nil;
        TAbstractChart(FChartList[LIndex]).OnUndoZoom := nil;
        TAbstractChart(FChartList[LIndex]).OnScroll   := nil;
        try
          TAbstractChart(FChartList[LIndex]).LeftAxis.SetMinMax(FActionChart.LeftAxis.Minimum,FActionChart.LeftAxis.Maximum);
          TAbstractChart(FChartList[LIndex]).BottomAxis.SetMinMax(FActionChart.BottomAxis.Minimum,FActionChart.BottomAxis.Maximum);
          TAbstractChart(FChartList[LIndex]).RightAxis.SetMinMax(FActionChart.RightAxis.Minimum,FActionChart.RightAxis.Maximum);
          TAbstractChart(FChartList[LIndex]).TopAxis.SetMinMax(FActionChart.TopAxis.Minimum,FActionChart.TopAxis.Maximum);
          TAbstractChart(FChartList[LIndex]).DepthAxis.SetMinMax(FActionChart.DepthAxis.Minimum,FActionChart.DepthAxis.Maximum);
        finally
          TAbstractChart(FChartList[LIndex]).OnZoom     := OnZoomChart;
          TAbstractChart(FChartList[LIndex]).OnUndoZoom := OnUndoZoomChart;
          TAbstractChart(FChartList[LIndex]).OnScroll   := OnScrollChart;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.ChartOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
          X, Y: Integer);
const OPNAME = 'TTimeSeriesComparitorChartPanel.ChartOnMouseDown';
begin
  try
    FActionChart   := TAbstractChart(Sender);
    FCurrentChart  := TAbstractChart(Sender);
    FCurrentAction := caNone;
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
        (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
    begin
      if ssRight in Shift then
      begin
       // FCurrentChart.EditChartProperties;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.SetChartMinMaxValues ( aInc, aMinX, aMaxY, aMaxX, aMinY : double );
const OPNAME = 'TTimeSeriesComparitorChartPanel.SetChartMinMaxValues';
begin
  try
    FActionChart.LeftAxis.SetMinMax(aMinX,aMaxX);
    FActionChart.BottomAxis.SetMinMax(aMinY,aMaxY);
    FActionChart.RightAxis.SetMinMax(aMinX,aMaxX);
    FActionChart.LeftAxis.Minimum    := aMinX;
    FActionChart.LeftAxis.Maximum    := aMaxX;
    FActionChart.RightAxis.Minimum   := aMinX;
    FActionChart.RightAxis.Maximum   := aMaxX;
    FActionChart.LeftAxis.Increment  := aInc;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean);
const OPNAME = 'TTimeSeriesComparitorChartPanel.SetChartLegend';
begin
  try
    FActionChart.Legend.Alignment := ALegendAlignment;
    FActionChart.Legend.Visible   := ALegendVisible;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartPanel.ChartOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
          X, Y: Integer);
const OPNAME = 'TTimeSeriesComparitorChartPanel.ChartOnMouseUp';
begin
  try
    SynchroniseCharts;
    FCurrentAction := caNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.OnAfterDrawChart(Sender: TObject);
const OPNAME = 'TTimeSeriesComparitorChartPanel.OnAfterDrawChart';
begin
  try
    if (FSetChartManualy) then
    begin
      FCurrentAction := caNone;
      FActionChart.CancelMouse := True;
    end
    else
      if(FActionChart <> nil) then
        FActionChart.CancelMouse := False;
    SynchroniseCharts;
    FSetChartManualy := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.SetChartName;
const OPNAME = 'TTimeSeriesComparitorChartPanel.SetChartName';
var
  LPoint1,
  LPoint2: TPoint;
  LCaptionSelector: TTimeSeriesComparitorCaptionSelector;
begin
  try
    LCaptionSelector := TTimeSeriesComparitorCaptionSelector.CreateWithoutDFM(nil,FAppModules);
    try
      LCaptionSelector.Initialise;
      if (FChartList.Count > 1) then
       LCaptionSelector.SetChartCount(FChartList.Count + 2)
      else
       LCaptionSelector.SetChartCount(FChartList.Count);
      LCaptionSelector.LanguageHasChanged;
      PopulateChartCaptions(LCaptionSelector.CaptionsGrid,False);

      LPoint1.X := 0;
      LPoint1.Y := 0;
      //
      LPoint2 := Self.ClientToScreen(LPoint1);
      LCaptionSelector.Left   := LPoint2.X;
      LCaptionSelector.Top    := LPoint2.Y;
      LCaptionSelector.Width  := Self.Width;
      LCaptionSelector.Height := Self.Height;

      LCaptionSelector.ShowModal;

      if(LCaptionSelector.ModalResult = mrOk) then
        PopulateChartCaptions(LCaptionSelector.CaptionsGrid,True);
    finally
      FreeAndNil(LCaptionSelector);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartPanel.PopulateChartCaptions(ASource: TStringGrid; AFromDisplay: boolean): boolean;
const OPNAME = 'TTimeSeriesComparitorChartPanel.PopulateChartCaptions';
var
  LCount,
  LStart : integer;
  LChart: TAbstractChart;
begin
  Result := False;
  try
    if Assigned(ASource) then
    begin
      if AFromDisplay then
      begin
        LStart := 1;
        if (FChartList.Count > 1) then
        begin
          LStart := 3;
          FHeaderCaption.Caption := ASource.Rows[1].Strings[2];
          FFooterCaption.Caption := ASource.Rows[2].Strings[2];
        end;
        for LCount := 0 to FChartList.Count -1 do
        begin
          LChart := TAbstractChart(FChartList.Items[LCount]);
          LChart.Title.Text.Text := ASource.Rows[LCount+LStart].Strings[2];
        end;
      end
      else
      begin
        LStart := 1;
        if (FChartList.Count > 1) then
        begin
          LStart := 3;
          ASource.Rows[1].Strings[0] := FAppModules.Language.GetString('TSCCaptionChartPanel.Grid_Caption1');
          ASource.Rows[1].Strings[1] := FHeaderCaption.Caption;
          ASource.Rows[2].Strings[0] := FAppModules.Language.GetString('TSCCaptionChartPanel.Grid_Caption2');
          ASource.Rows[2].Strings[1] := FFooterCaption.Caption;
        end;
        for LCount := 0 to FChartList.Count -1 do
        begin
          LChart := TAbstractChart(FChartList.Items[LCount]);
          ASource.Rows[LCount+LStart].Strings[0] := LChart.Name + ' ' + FAppModules.Language.GetString('TSCCaptionChartPanel.Grid_Caption3');
          ASource.Rows[LCount+LStart].Strings[1] := Trim(LChart.Title.Text.Text);
          ASource.Rows[LCount+LStart].Strings[2] := Trim(LChart.Title.Text.Text);
        end;
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartPanel.SynchroniseChartsXAxis;
const OPNAME = 'TTimeSeriesComparitorChartPanel.SynchroniseChartsXAxis';
var
  LIndex: integer;
  LChart: TAbstractChart;
  LMinX,
  LMaxX: double;
  LFound: boolean;
begin
  try
    if ZoomActiveChartOnly then Exit;

    if(FChartList.Count > 1) then
    begin
      LMinX  := 0.0;
      LMaxX  := 0.0;
      LFound := False;

      for LIndex := 0 to  FChartList.Count - 1 do
      begin
        LChart := TAbstractChart(FChartList[LIndex]);
        if(LChart.SeriesCount > 0) then
        begin
          LMinX  := LChart.BottomAxis.Minimum;
          LMaxX  := LChart.BottomAxis.Maximum;
          LFound := True;
          Break;
        end;
      end;

      if LFound then
      begin
        for LIndex := 0 to  FChartList.Count - 1 do
        begin
          LChart := TAbstractChart(FChartList[LIndex]);
          if(LChart.SeriesCount > 0) then
          begin
            if(LChart.BottomAxis.Minimum <  LMinX) then
              LMinX := LChart.BottomAxis.Minimum;
            if(LChart.BottomAxis.Maximum >  LMaxX) then
              LMaxX := LChart.BottomAxis.Maximum;
          end;
        end;

        for LIndex := 0 to  FChartList.Count - 1 do
        begin
          LChart := TAbstractChart(FChartList[LIndex]);
          LChart.BottomAxis.Maximum := LMaxX;
          LChart.BottomAxis.Minimum := LMinX;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.FixChartsXAxis;
const OPNAME = 'TTimeSeriesComparitorChartPanel.FixChartsXAxis';
var
  LCount,
  LIndex: integer;
  LChart: TAbstractChart;
  LMinX,
  LMaxX: double;
  LFound: boolean;
begin
  try
    for LIndex := 0 to  FChartList.Count - 1 do
    begin
      LMinX  := 9999999999.99;
      LMaxX  := 0.0;
      LFound := False;
      LChart := TAbstractChart(FChartList[LIndex]);
      if(LChart.SeriesCount > 0) then
      begin
        for LCount := 0 to LChart.SeriesCount - 1 do
        begin
          if(LChart.Series[LCount].XValues.MinValue < LMinX) then
          begin
             LMinX := LChart.Series[LCount].XValues.MinValue;
             LFound := True;
          end;
          if(LChart.Series[LCount].XValues.MaxValue > LMaxX) then
          begin
             LMaxX := LChart.Series[LCount].XValues.MaxValue;
             LFound := True;
          end;
        end;
        if LFound then
        begin
          LChart.BottomAxis.Maximum := LMaxX;
          LChart.BottomAxis.Minimum := LMinX;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTimeSeriesComparitorChartPanel.DoChartOnDblClick ( Sender : TObject );
const OPNAME = 'TTimeSeriesComparitorChartPanel.DoChartOnDblClick';
var
  lSetXYAxisDialog : TTimeSeriesComparitorXYAxisReset;
begin
  FSetChartManualy := False;
  try
    if (Assigned(FActionChart)) and (Sender is TAbstractChart) then
    begin
      try
        TAbstractChart(Sender).CancelMouse := True;
        lSetXYAxisDialog := TTimeSeriesComparitorXYAxisReset.CreateWithoutDFM(nil, FAppModules);
        lSetXYAxisDialog.SetOnKeyPress(OnKeyPress);
        lSetXYAxisDialog.Width  := Self.Width div 3;
        lSetXYAxisDialog.Height := Self.Height div 2;
        lSetXYAxisDialog.Position := poScreenCenter;
        lSetXYAxisDialog.Initialise;
        lSetXYAxisDialog.LanguageHasChanged;
        lSetXYAxisDialog.MinXValue := FActionChart.LeftAxis.Minimum;
        lSetXYAxisDialog.MaxYValue := FActionChart.BottomAxis.Maximum;
        lSetXYAxisDialog.MaxXValue := FActionChart.LeftAxis.Maximum;
        lSetXYAxisDialog.MinYValue := FActionChart.BottomAxis.Minimum;
        lSetXYAxisDialog.IncValue  := FActionChart.LeftAxis.Increment;
        if Assigned(lSetXYAxisDialog) and (FCurrentView <>  FAppModules.Model.ViewData.CurrentTreeNodeData)  then
          SetInitialValues(lSetXYAxisDialog, FActionChart.LeftAxis.Increment,
                             FActionChart.LeftAxis.Minimum, FActionChart.BottomAxis.Maximum,
                             FActionChart.LeftAxis.Maximum, FActionChart.BottomAxis.Minimum
                            );
        lSetXYAxisDialog.OnClickHandle := DoOnClick;
        lSetXYAxisDialog.SetEnabled((FAppModules.User.UserRights in CUR_EditData) and
                                    (FAppModules.StudyArea <> nil) and
                                    (not (FAppModules.StudyArea.ScenarioLocked)));
        lSetXYAxisDialog.ShowModal;
        if (lSetXYAxisDialog.ModalResult = mrOk) then
        begin
          SetChartMinMaxValues(lSetXYAxisDialog.IncValue, lSetXYAxisDialog.MinXValue, lSetXYAxisDialog.MaxYValue,
                               lSetXYAxisDialog.MaxXValue, lSetXYAxisDialog.MinYValue);
          FSetChartManualy := True;
        end;
      finally
        FreeAndNil(lSetXYAxisDialog);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartPanel.OnKeyPress(Sender: TObject;var Key: Char);
const OPNAME = 'TTimeSeriesComparitorChartPanel.OnKeyPress';
begin
  try
    if (not(FAppModules.User.UserRights in CUR_EditData)) and
       (FAppModules.StudyArea <> nil) and
       (FAppModules.StudyArea.ScenarioLocked) then
      Key := #0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartPanel.DoOnClick (  Sender : TObject; var aMinXValue : double;
                                                       var aMinYValue  : TDateTime; var aMaxXValue : double;
                                                       var aMaxYValue : TDateTime; var aIncValue : double );
const OPNAME = 'TTimeSeriesComparitorChartPanel.DoOnClick';
begin
  try
    aMinXValue := FMinXValue;
    aMinYValue := FMinYValue;
    aMaxXValue := FMaxXValue;
    aMaxYValue := FMaxYValue;
    aIncValue  := FInc;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartPanel.SetInitialValues ( aSetXYAxisDialog : TTimeSeriesComparitorXYAxisReset;
                                                             aInc, aMinX, aMaxY, aMaxX, aMinY: double );
const OPNAME = 'TTimeSeriesComparitorChartPanel.SetInitialValues';
begin
  try
    if (aSetXYAxisDialog <> nil) then
    begin
      FMinXValue := aSetXYAxisDialog.MinXValue;
      FMinYValue := aSetXYAxisDialog.MinYValue;
      FMaxXValue := aSetXYAxisDialog.MaxXValue;
      FMaxYValue := aSetXYAxisDialog.MaxYValue;
      FInc       := aSetXYAxisDialog.IncValue;
      FCurrentView := FAppModules.Model.ViewData.CurrentTreeNodeData;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
