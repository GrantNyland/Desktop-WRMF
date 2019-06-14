unit UBoxPlot13Chart;

interface
  uses
       VCLTee.Chart,
       VCLTee.Series,
       VCLTee.TeeShape,
       VCLTee.TeEngine,
       VCL.Controls,
       VCL.Graphics,
       UDataEditComponent;

type
  BoxChartRecordPointer = ^TBoxChartRecord;
  TBoxChartRecord = record
    //General Data
    FXCoordinate  : Double;
    FHorzLine95,
    FVertLine75To95,
    FBox50To75,
    FBox50To25,
    FVertLine25To5,
    FHorzLine5: TChartShape;
    Next          : BoxChartRecordPointer;
  end;
  {$M+}
  TBoxPlotChart = class(TObject)
  protected
    FBoxChartRecordHeadPtr,
    FBoxChartRecordPtr     : BoxChartRecordPointer;
    FNumberOfPoints        : Integer;
    FTheChart              : TFieldChart;

    FHorzLine100,
    FHorzLine99Point5,
    FHorzLine99,
    FCircle98,

    FCircle2,
    FHorzLine1,
    FHorzLine0Point5,
    FHorzLine0        : TLineSeries;
    FSeriesCreated    : boolean;

    procedure AmendPoint(X : TDate; Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
    procedure CreatePoint(X : TDate; Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SetTheChart(AChart : TFieldChart);
  published
    function AddPoint(X : TDate; Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis) : boolean;
    function RemovePoint(X : Integer) : boolean;
    function HideAllPoints : boolean;
    function RefreshAll : boolean;
    
    property NumberOfPoints : Integer read FNumberOfPoints write FNumberOfPoints;
    property TheChart : TFieldChart read FTheChart write SetTheChart;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TBoxPlotChart.AfterConstruction;
const OPNAME = 'TBoxPlotChart.AfterConstruction';
begin
  inherited Create;
  try
    FNumberOfPoints := 0;
    FTheChart := nil;
    FSeriesCreated := False;

    FBoxChartRecordHeadPtr := nil;
    FBoxChartRecordPtr := FBoxChartRecordHeadPtr;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxPlotChart.BeforeDestruction;
const OPNAME = 'TBoxPlotChart.BeforeDestruction';
begin
  inherited Create;
  try
      {FHorzLine100.ParentChart        := nil;
      FHorzLine99Point5.ParentChart   := nil;
      FHorzLine99.ParentChart         := nil;
      FCircle98.ParentChart           := nil;

      FCircle2.ParentChart            := nil;
      FHorzLine1.ParentChart          := nil;
      FHorzLine0Point5.ParentChart    := nil;
      FHorzLine0.ParentChart          := nil;

      FreeAndNil(FHorzLine100);
      FreeAndNil(FHorzLine99Point5);
      FreeAndNil(FHorzLine99);
      FreeAndNil(FCircle98);

      FreeAndNil(FCircle2);
      FreeAndNil(FHorzLine1);
      FreeAndNil(FHorzLine0Point5);
      FreeAndNil(FHorzLine0);}

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxPlotChart.SetTheChart(AChart : TFieldChart);
const OPNAME = 'TBoxPlotChart.SetTheChart';
begin
  try
    if FTheChart <> AChart then
    begin
      FTheChart                       := AChart;
      if not FSeriesCreated then
      begin
        FHorzLine100      := TLineSeries.Create(AChart);
        FHorzLine99Point5 := TLineSeries.Create(AChart);
        FHorzLine99       := TLineSeries.Create(AChart);
        FCircle98         := TLineSeries.Create(AChart);

        FCircle2          := TLineSeries.Create(AChart);
        FHorzLine1        := TLineSeries.Create(AChart);
        FHorzLine0Point5  := TLineSeries.Create(AChart);
        FHorzLine0        := TLineSeries.Create(AChart);
        FSeriesCreated    := True;
      end;

      FHorzLine100.ParentChart        := AChart;
      FHorzLine99Point5.ParentChart   := AChart;
      FHorzLine99.ParentChart         := AChart;
      FCircle98.ParentChart           := AChart;

      FCircle2.ParentChart            := AChart;
      FHorzLine1.ParentChart          := AChart;
      FHorzLine0Point5.ParentChart    := AChart;
      FHorzLine0.ParentChart          := AChart;

      FHorzLine100.XValues.DateTime      := True;
      FHorzLine99Point5.XValues.DateTime := True;
      FHorzLine99.XValues.DateTime       := True;
      FCircle98.XValues.DateTime         := True;

      FCircle2.XValues.DateTime          := True;
      FHorzLine1.XValues.DateTime        := True;
      FHorzLine0Point5.XValues.DateTime  := True;
      FHorzLine0.XValues.DateTime        := True;
      AChart.BottomAxis.DateTimeFormat   := 'yyyy/mm';

      FHorzLine100.Pointer.Visible        := False;
      FHorzLine99Point5.Pointer.Visible   := False;
      FHorzLine99.Pointer.Visible         := False;
      FCircle98.Pointer.Visible           := True;

      FCircle2.Pointer.Visible            := True;
      FHorzLine1.Pointer.Visible          := False;
      FHorzLine0Point5.Pointer.Visible    := False;
      FHorzLine0.Pointer.Visible          := False;

      FHorzLine100.Color              := clRed;
      FHorzLine100.Pointer.Style      := psTriangle;

      FHorzLine99Point5.Color         := clPurple;
      FHorzLine99Point5.Pointer.Style := psRectangle;

      FHorzLine99.Color               := clGreen;
      FHorzLine99.Pointer.Style       := psDiamond;

      FCircle98.Color                 := clBlue;
      FCircle98.Pointer.Style         := psCircle;

      FHorzLine0.Color               := clRed;
      FHorzLine0.Pointer.Style       := psTriangle;

      FHorzLine0Point5.Color         := clPurple;
      FHorzLine0Point5.Pointer.Style := psRectangle;

      FHorzLine1.Color               := clGreen;
      FHorzLine1.Pointer.Style       := psDiamond;

      FCircle2.Color                 := clBlue;
      FCircle2.Pointer.Style         := psCircle;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxPlotChart.CreatePoint(X : TDate; Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
const OPNAME = 'TBoxPlotChart.CreatePoint';
begin
  try
    Inc(FNumberOfPoints);
    FBoxChartRecordPtr.FXCoordinate := X;
    with FBoxChartRecordPtr^ do
    begin
      FHorzLine95             := TChartShape.Create(FTheChart);
      FHorzLine95.VertAxis    := VCLTee.TeEngine.TVertAxis(TheAxis);
      FHorzLine95.Style       := chasHorizLine;
      FHorzLine95.Pen.Color   := clBlack;
      FHorzLine95.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FTheChart);
      FHorzLine95.XValues.DateTime := True;

      FHorzLine5             := TChartShape.Create(FTheChart);
      FHorzLine5.VertAxis    := VCLTee.TeEngine.TVertAxis(TheAxis);
      FHorzLine5.Style       := chasHorizLine;
      FHorzLine5.Pen.Color   := clBlack;
      FHorzLine5.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FTheChart);
      FHorzLine5.XValues.DateTime := True;

      FVertLine75To95             := TChartShape.Create(FTheChart);
      FVertLine75To95.VertAxis    := VCLTee.TeEngine.TVertAxis(TheAxis);
      FVertLine75To95.Style       := chasVertLine;
      FVertLine75To95.Pen.Color   := clBlack;
      FVertLine75To95.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FTheChart);
      FVertLine75To95.XValues.DateTime := True;

      FVertLine25To5             := TChartShape.Create(FTheChart);
      FVertLine25To5.VertAxis    := VCLTee.TeEngine.TVertAxis(TheAxis);
      FVertLine25To5.Style       := chasVertLine;
      FVertLine25To5.Pen.Color   := clBlack;
      FVertLine25To5.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FTheChart);
      FVertLine25To5.XValues.DateTime := True;

      FBox50To75             := TChartShape.Create(FTheChart);
      FBox50To75.VertAxis    := VCLTee.TeEngine.TVertAxis(TheAxis);
      FBox50To75.Style       := chasRectangle;
      FBox50To75.Brush.Style := bsClear;
      FBox50To75.Color       := clBlue;
      FBox50To75.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FTheChart);
      FBox50To75.XValues.DateTime := True;

      FBox50To25             := TChartShape.Create(FTheChart);
      FBox50To25.VertAxis    := VCLTee.TeEngine.TVertAxis(TheAxis);
      FBox50To25.Style       := chasRectangle;
      FBox50To25.Brush.Style := bsClear;
      FBox50To25.Color       := clRed;
      FBox50To25.ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FTheChart);
      FBox50To25.XValues.DateTime := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TBoxPlotChart.AmendPoint(X : TDate; Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height : Double; TheAxis : TVertAxis);
const OPNAME = 'TBoxPlotChart.AmendPoint';
var
  LWidth : integer;
begin
  try
    FHorzLine100.AddXY(X,Perc_100);
    FHorzLine99Point5.AddXY(X,Perc_99Point5);
    FHorzLine99.AddXY(X,Perc_99);
    FCircle98.AddXY(X,Perc_98);
    FCircle2.AddXY(X,Perc_2);
    FHorzLine1.AddXY(X,Perc_1);
    FHorzLine0Point5.AddXY(X,Perc_0Point5);
    FHorzLine0.AddXY(X,Perc_0);

    with FBoxChartRecordPtr^ do
    begin
      LWidth           := Trunc(Width * 0.625);
      FHorzLine95.Active := True;
      FHorzLine95.X0     := X-LWidth;
      FHorzLine95.X1     := X+LWidth;
      FHorzLine95.Y0     := Perc_95;
      FHorzLine95.Y1     := Perc_95;

      FHorzLine5.Active := True;
      FHorzLine5.X0     := X-LWidth;
      FHorzLine5.X1     := X+LWidth;
      FHorzLine5.Y0     := Perc_5;
      FHorzLine5.Y1     := Perc_5;

      FVertLine75To95.Active := True;
      FVertLine75To95.X0     := X;
      FVertLine75To95.X1     := X;
      FVertLine75To95.Y0     := Perc_75;
      FVertLine75To95.Y1     := Perc_95;

      FVertLine25To5.Active := True;
      FVertLine25To5.X0     := X;
      FVertLine25To5.X1     := X;
      FVertLine25To5.Y0     := Perc_5;
      FVertLine25To5.Y1     := Perc_25;

      FBox50To75.Active := True;
      FBox50To75.X0     := X-LWidth;
      FBox50To75.X1     := X+LWidth;
      FBox50To75.Y0     := Perc_50;
      FBox50To75.Y1     := Perc_75;

      FBox50To25.Active := True;
      FBox50To25.X0     := X-LWidth;
      FBox50To25.X1     := X+LWidth;
      FBox50To25.Y0     := Perc_25;
      FBox50To25.Y1     := Perc_50;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

// ————————— PUBLIC METHODS —————————
function TBoxPlotChart.AddPoint(X : TDate; Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height : Double;
                           TheAxis : TVertAxis) : boolean;
const OPNAME = 'TBoxPlotChart.AddPoint';
var
  PrevBoxChartRecordPtr : BoxChartRecordPointer;
begin
  Result := False;
  try
    if FTheChart <> nil then
    begin
      PrevBoxChartRecordPtr := nil;

      if FBoxChartRecordHeadPtr = nil then
      begin
        //Insert a new box at the specificed x-coordinate
        new(FBoxChartRecordHeadPtr);
        FBoxChartRecordHeadPtr.Next := nil;
        FBoxChartRecordPtr := FBoxChartRecordHeadPtr;
        CreatePoint(X , Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height,TheAxis);
      end
      else
      begin
        FBoxChartRecordPtr := FBoxChartRecordHeadPtr;
        while (FBoxChartRecordPtr <> nil) and (FBoxChartRecordPtr.FXCoordinate < X) do
        begin
          PrevBoxChartRecordPtr := FBoxChartRecordPtr;
          FBoxChartRecordPtr := FBoxChartRecordPtr.Next;
        end;

        if (FBoxChartRecordPtr = FBoxChartRecordHeadPtr) and (X <> FBoxChartRecordHeadPtr.FXCoordinate) then
        begin
          //if at the head and not matching x-coordinates then create new head and point to it
          new(FBoxChartRecordPtr);
          FBoxChartRecordPtr.Next := FBoxChartRecordHeadPtr;
          FBoxChartRecordHeadPtr := FBoxChartRecordPtr;
          CreatePoint(X , Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height,TheAxis);
        end
        else if FBoxChartRecordPtr = nil then
        begin
          //At the end of the list, append a new record
          new(FBoxChartRecordPtr);
          FBoxChartRecordPtr.Next := nil;
          PrevBoxChartRecordPtr.Next := FBoxChartRecordPtr;
          CreatePoint(X , Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height,TheAxis);
        end
        else if (FBoxChartRecordPtr.FXCoordinate = X) then
        begin
          //We have found a match simply update
        end
        else
        begin
          //At an in-between point, insert new record
          New(FBoxChartRecordPtr);
          FBoxChartRecordPtr.Next := PrevBoxChartRecordPtr.Next;
          PrevBoxChartRecordPtr.Next := FBoxChartRecordPtr;
          CreatePoint(X , Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height,TheAxis);
        end;
      end;
      AmendPoint(X , Perc_0, Perc_0Point5, Perc_1, Perc_2, Perc_5, Perc_25, Perc_50, Perc_75, Perc_95,
                      Perc_98, Perc_99, Perc_99Point5,Perc_100, Historical, Width, Height,TheAxis);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBoxPlotChart.RemovePoint(X : Integer) : boolean;
const OPNAME = 'TBoxPlotChart.RemovePoint';
begin
  Result := False;
  try
    if FTheChart <> nil then
    begin
      dec(FNumberOfPoints);
      //need to free the item from the linked list
      Result := true;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBoxPlotChart.HideAllPoints : boolean;
const OPNAME = 'TBoxPlotChart.HideAllPoints';
var
  LBoxChartRecordPtr     : BoxChartRecordPointer;
begin
  Result := False;
  try
    FHorzLine100.Active       := False;
    FHorzLine99Point5.Active  := False;
    FHorzLine99.Active        := False;
    FCircle98.Active          := False;
    FCircle2.Active           := False;
    FHorzLine1.Active         := False;
    FHorzLine0Point5.Active   := False;
    FHorzLine0.Active         := False;

    LBoxChartRecordPtr := FBoxChartRecordPtr;
    FBoxChartRecordPtr  := FBoxChartRecordHeadPtr;
    while FBoxChartRecordPtr <> nil do
    begin
      FBoxChartRecordPtr.FHorzLine95.Active        := False;
      FBoxChartRecordPtr.FVertLine75To95.Active    := False;
      FBoxChartRecordPtr.FBox50To75.Active         := False;
      FBoxChartRecordPtr.FBox50To25.Active         := False;
      FBoxChartRecordPtr.FVertLine25To5.Active     := False;
      FBoxChartRecordPtr.FHorzLine5.Active         := False;
      FBoxChartRecordPtr := FBoxChartRecordPtr.Next;
    end;
    FBoxChartRecordPtr  := LBoxChartRecordPtr;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TBoxPlotChart.RefreshAll : boolean;
const OPNAME = 'TBoxPlotChart.RefreshAll';
begin
  Result := False;
  try
    FBoxChartRecordPtr := FBoxChartRecordHeadPtr;
    while FBoxChartRecordPtr <> nil do
    begin
      FBoxChartRecordHeadPtr:= FBoxChartRecordPtr.Next;
      Dispose(FBoxChartRecordPtr);
      FBoxChartRecordPtr := FBoxChartRecordHeadPtr;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
