unit UBoxChart;

interface
  uses VCLTee.Chart, VCLTee.TeeShape, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeCanvas, VCL.Graphics;

type
  BoxChartRecordPointer = ^TBoxChartRecord;
  TBoxChartRecord = record
    //General Data
    X_Coordinate : Double;
    TopMargin, BottomMargin,
    TopBar, BottomBar,
    TopSpoke, BottomSpoke,
    TopBox, BottomBox,
    HistoricalMarker : TChartShape;
    Next : BoxChartRecordPointer;
  end;
  {$M+}
  TBoxChart = class
  protected
    BoxChartRecordHeadPtr,
    BoxChartRecordPtr : BoxChartRecordPointer;
    LabelSeries : TFastLineSeries;
    FNumberOfPoints : Integer;
    FTheChart : TChart;
    procedure AmendPoint(X : Double; Perc_0, Perc_5, Perc_25, Perc_50,
              Perc_75, Perc_95, Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
    procedure CreatePoint(X : Double; Perc_0, Perc_5, Perc_25, Perc_50,
              Perc_75, Perc_95, Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
  public
    constructor Create;
    procedure SetTheChart(AChart : TChart);
  published
    property NumberOfPoints : Integer read FNumberOfPoints write FNumberOfPoints;
    property TheChart : TChart read FTheChart write SetTheChart;
    function AddPoint(X : Double; Perc_0, Perc_5, Perc_25, Perc_50,Perc_75, Perc_95,
                      Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis) : boolean;
    function RemovePoint(X : Integer) : boolean;
    function HideAllPoints : boolean;
    function RefreshAll : boolean;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

constructor TBoxChart.Create;
const OPNAME = 'TBoxChart.Create';
begin
  inherited Create;
  try
    NumberOfPoints := 0;
    TheChart := nil;

    BoxChartRecordHeadPtr := nil;
    BoxChartRecordPtr := BoxChartRecordHeadPtr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxChart.SetTheChart(AChart : TChart);
const OPNAME = 'TBoxChart.SetTheChart';
begin
  try
    if FTheChart <> AChart then
    begin
      FTheChart := AChart;
      LabelSeries := TFastLineSeries.Create(FTheChart);
      LabelSeries.LinePen.Style := psSolid;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxChart.CreatePoint(X : Double; Perc_0, Perc_5, Perc_25, Perc_50,
          Perc_75, Perc_95, Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
const OPNAME = 'TBoxChart.CreatePoint';
begin
  try
    Inc(FNumberOfPoints);

    BoxChartRecordPtr.X_Coordinate := X;

    LabelSeries.Add(X,'OCT',clTeeColor);

    with BoxChartRecordPtr^ do
    begin
      HistoricalMarker := TChartShape.Create(TheChart);
      HistoricalMarker.VertAxis := TheAxis;
      HistoricalMarker.Style := chasDiamond;
      HistoricalMarker.Brush.Color := clBlack;
      HistoricalMarker.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      BottomMargin := TChartShape.Create(TheChart);
      BottomMargin.VertAxis := TheAxis;
      BottomMargin.Style := chasHorizLine;
      BottomMargin.Pen.Color := clBlack;
      BottomMargin.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      BottomBar := TChartShape.Create(TheChart);
      BottomBar.VertAxis := TheAxis;
      BottomBar.Style := chasHorizLine;
      BottomBar.Pen.Color := clBlack;
      BottomBar.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      BottomSpoke := TChartShape.Create(TheChart);
      BottomSpoke.VertAxis := TheAxis;
      BottomSpoke.Style := chasVertLine;
      BottomSpoke.Pen.Color := clBlack;
      BottomSpoke.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      BottomBox := TChartShape.Create(TheChart);
      BottomBox.VertAxis := TheAxis;
      BottomBox.Style := chasRectangle;
      BottomBox.Brush.Style := bsClear;
      BottomBox.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      TopBox := TChartShape.Create(TheChart);
      TopBox.VertAxis := TheAxis;
      TopBox.Style := chasRectangle;
      TopBox.Brush.Color := clRed;
      TopBox.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      TopSpoke := TChartShape.Create(TheChart);
      TopSpoke.VertAxis := TheAxis;
      TopSpoke.Style := chasVertLine;
      TopSpoke.Pen.Color := clBlack;
      TopSpoke.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      TopBar := TChartShape.Create(TheChart);
      TopBar.VertAxis := TheAxis;
      TopBar.Style := chasHorizLine;
      TopBar.Pen.Color := clBlack;
      TopBar.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);

      TopMargin := TChartShape.Create(TheChart);
      TopMargin.VertAxis := TheAxis;
      TopMargin.Style := chasHorizLine;
      TopMargin.Pen.Color := clBlack;
      TopMargin.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(TheChart);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxChart.AmendPoint(X : Double; Perc_0, Perc_5, Perc_25, Perc_50,
          Perc_75, Perc_95, Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
const OPNAME = 'TBoxChart.AmendPoint';
begin
  try
    with BoxChartRecordPtr^ do
    begin
      HistoricalMarker.Active := true;
      HistoricalMarker.X0 := X + Width;
      HistoricalMarker.X1 := X + Width*2;
      //need to adjust these values to fit on the screen correctly

      HistoricalMarker.Y0 := Historical - Height;
      HistoricalMarker.Y1 := Historical + Height;

      BottomMargin.Active := true;
      BottomMargin.X0 := X-Width;
      BottomMargin.X1 := X+Width;
      BottomMargin.Y0 := Perc_0 - Width;
      BottomMargin.Y1 := Perc_0 + Width;

      BottomBar.Active := true;
      BottomBar.X0 := X-Width;
      BottomBar.X1 := X+Width;
      BottomBar.Y0 := Perc_5 + Width;
      BottomBar.Y1 := Perc_5 - Width;

      BottomSpoke.Active := true;
      BottomSpoke.X0 := X - Width;
      BottomSpoke.X1 := X + Width;
      BottomSpoke.Y0 := Perc_25;
      BottomSpoke.Y1 := Perc_5;

      BottomBox.Active := true;
      BottomBox.X0 := X - Width;
      BottomBox.X1 := X + Width;
      BottomBox.Y0 := Perc_50;
      BottomBox.Y1 := Perc_25;

      TopBox.Active := true;
      TopBox.X0 := X - Width;
      TopBox.X1 := X + Width;
      TopBox.Y0 := Perc_75;
      TopBox.Y1 := Perc_50;

      TopSpoke.Active := true;
      TopSpoke.X0 := X - Width;
      TopSpoke.X1 := X + Width;
      TopSpoke.Y0 := Perc_75;
      TopSpoke.Y1 := Perc_95;

      TopBar.Active := true;
      TopBar.X0 := X - Width;
      TopBar.X1 := X + Width;
      TopBar.Y0 := Perc_95 - Width;
      TopBar.Y1 := Perc_95 + Width;

      TopMargin.Active := true;
      TopMargin.X0 := X - Width;
      TopMargin.X1 := X + Width;
      TopMargin.Y0 := Perc_100 - Width;
      TopMargin.Y1 := Perc_100 + Width;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// ————————— PUBLIC METHODS —————————
function TBoxChart.AddPoint(X : Double; Perc_0, Perc_5, Perc_25, Perc_50,
                           Perc_75, Perc_95, Perc_100, Historical, Width, Height : Double;
                           TheAxis : TVertAxis) : boolean;
const OPNAME = 'TBoxChart.AddPoint';
var
  PrevBoxChartRecordPtr : BoxChartRecordPointer;
begin
  Result := False;
  try
    if TheChart <> nil then
    begin
      PrevBoxChartRecordPtr := nil;

      if BoxChartRecordHeadPtr = nil then
      begin
        //Insert a new box at the specificed x-coordinate
        new(BoxChartRecordHeadPtr);
        BoxChartRecordHeadPtr.Next := nil;
        BoxChartRecordPtr := BoxChartRecordHeadPtr;
        CreatePoint(X , Perc_0, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95, Perc_100, Historical, Width, Height,TheAxis);
      end
      else
      begin
        BoxChartRecordPtr := BoxChartRecordHeadPtr;
        while (BoxChartRecordPtr <> nil) and (BoxChartRecordPtr.X_Coordinate < X) do
        begin
          PrevBoxChartRecordPtr := BoxChartRecordPtr;
          BoxChartRecordPtr := BoxChartRecordPtr.Next;
        end;

        if (BoxChartRecordPtr = BoxChartRecordHeadPtr) and (X <> BoxChartRecordHeadPtr.X_Coordinate) then
        begin
          //if at the head and not matching x-coordinates then create new head and point to it
          new(BoxChartRecordPtr);
          BoxChartRecordPtr.Next := BoxChartRecordHeadPtr;
          BoxChartRecordHeadPtr := BoxChartRecordPtr;
          CreatePoint(X , Perc_0, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95, Perc_100, Historical, Width, Height,TheAxis);
        end
        else if BoxChartRecordPtr = nil then
        begin
          //At the end of the list, append a new record
          new(BoxChartRecordPtr);
          BoxChartRecordPtr.Next := nil;
          PrevBoxChartRecordPtr.Next := BoxChartRecordPtr;
          CreatePoint(X , Perc_0, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95, Perc_100, Historical, Width, Height,TheAxis);
        end
        else if (BoxChartRecordPtr.X_Coordinate = X) then
        begin
          //We have found a match simply update
        end
        else
        begin
          //At an in-between point, insert new record
          New(BoxChartRecordPtr);
          BoxChartRecordPtr.Next := PrevBoxChartRecordPtr.Next;
          PrevBoxChartRecordPtr.Next := BoxChartRecordPtr;
          CreatePoint(X , Perc_0, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95, Perc_100, Historical, Width, Height,TheAxis);
        end;
      end;
      AmendPoint(X , Perc_0, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95, Perc_100, Historical, Width, Height,TheAxis);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBoxChart.RemovePoint(X : Integer) : boolean;
const OPNAME = 'TBoxChart.RemovePoint';
begin
  Result := False;
  try
    if TheChart <> nil then
    begin
      dec(FNumberOfPoints);
      //need to free the item from the linked list
      Result := true;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBoxChart.HideAllPoints : boolean;
const OPNAME = 'TBoxChart.HideAllPoints';
begin
  Result := False;
  try
    BoxChartRecordPtr := BoxChartRecordHeadPtr;
    while BoxChartRecordPtr <> nil do
    begin
      BoxChartRecordHeadPtr.TopMargin.Active := false;
      BoxChartRecordPtr.TopBar.Active := false;
      BoxChartRecordPtr.BottomBar.Active := false;
      BoxChartRecordPtr.TopSpoke.Active := false;
      BoxChartRecordPtr.BottomSpoke.Active := false;
      BoxChartRecordPtr.TopBox.Active := false;
      BoxChartRecordPtr.BottomBox.Active := false;
      BoxChartRecordHeadPtr.BottomMargin.Active := false;

      BoxChartRecordPtr := BoxChartRecordPtr.Next;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBoxChart.RefreshAll : boolean;
const OPNAME = 'TBoxChart.RefreshAll';
begin
  Result := False;
  try
    BoxChartRecordPtr := BoxChartRecordHeadPtr;
    while BoxChartRecordPtr <> nil do
    begin
      BoxChartRecordHeadPtr:= BoxChartRecordPtr.Next;
      Dispose(BoxChartRecordPtr);
      BoxChartRecordPtr := BoxChartRecordHeadPtr;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
