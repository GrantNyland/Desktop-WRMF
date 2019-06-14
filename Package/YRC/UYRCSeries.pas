//
//
//  UNIT      : Contains TYRCPointSeries and TYRCLineSeries Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//  COMMENT   : This unit gives more control over TChartSeries.
//
unit UYRCSeries;

interface

uses
  Classes,
  Contnrs,
  VCL.Controls,
  VCL.Graphics,
  Messages,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeShape,
  UAbstractObject;

  const
  ColorArray : array [0..12] of TColor = (clRed,clMaroon,clGreen,clNavy,clPurple,clTeal,clOlive,clLime,clBlue,clFuchsia,clAqua,clBlack,clYellow);
  PointStyleArray : array [0..7] of TSeriesPointerStyle = (psTriangle,psRectangle,psDownTriangle,psDiagCross,psLeftTriangle,psStar,psRightTriangle,psCross);
type

  {TChartSelection = class(TAbstractAppObject)
  private
  protected
    FPlottingBase                : integer;
    FPeriodLength                : integer;
    FTargetDraftIndex            : integer;
    FZoomIndex                   : integer;
    FZoomValue                   : double;

    FShowTargetDrafts            : TShowTargetDrafts;
    FChartMode                   : TChartMode;
    FChartEditMode               : TChartEditMode;

    FHideRawPoints               : boolean;
    FHideRawLines                : boolean;
    FShowFirmYieldLabels         : boolean;
    FShowCursorPosition          : boolean;
    FAssuranceVisible            : boolean;
    FMaxYValue                   : double;
    procedure CreateMemberObjects; override;
  public
    procedure Reset;
    procedure SaveToINIFile;
    procedure LoadFromINIFile;

    property PlottingBase         : integer           read FPlottingBase        write FPlottingBase;
    property PeriodLength         : integer           read FPeriodLength        write FPeriodLength;
    property TargetDraftIndex     : integer           read FTargetDraftIndex    write FTargetDraftIndex;
    property ZoomIndex            : integer           read FZoomIndex           write FZoomIndex;
    property ZoomValue            : double            read FZoomValue           write FZoomValue;

    property ShowTargetDrafts     : TShowTargetDrafts read FShowTargetDrafts    write FShowTargetDrafts;
    property ChartMode            : TChartMode        read FChartMode           write FChartMode;
    property ChartEditMode        : TChartEditMode    read FChartEditMode       write FChartEditMode;

    property HideRawPoints        : boolean           read FHideRawpoints       write FHideRawpoints;
    property HideRawLines         : boolean           read FHideRawLines        write FHideRawLines;
    property ShowFirmYieldLabels  : boolean           read FShowFirmYieldLabels write FShowFirmYieldLabels;
    property ShowCursorPosition   : boolean           read FShowCursorPosition  write FShowCursorPosition;
    property AssuranceVisible     : boolean           read FAssuranceVisible    write FAssuranceVisible;
    property MaxYValue            : double            read FMaxYValue           write FMaxYValue;

  end;}

  TYRCPointSeries = class(TPointSeries)
  protected
    FSeriesIndex,
    FTargetDraftIndex,
    FPlaneIndex : integer;
  public
    constructor Create(AOwner : TComponent); override;
    procedure SelectSeriesColor(AIndex: integer);
    procedure SelectSeriesPointStyle(AIndex: integer);

    property SeriesIndex : integer read FSeriesIndex write FSeriesIndex;
    property TargetDraftIndex : integer read FTargetDraftIndex write FTargetDraftIndex;
    property PlaneIndex : integer read FPlaneIndex write FPlaneIndex;
  end;

type
  TYRCLineSeries = class(TLineSeries)
  protected
    FSeriesIndex,
    FTargetDraftIndex,
    FPlaneIndex : integer;
    FOnSeriesClickPointer : TSeriesClickPointerEvent;
    function GetOnSeriesClickPointer : TSeriesClickPointerEvent;
    procedure SetOnSeriesClickPointer(const AValue : TSeriesClickPointerEvent);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SelectSeriesColor(AIndex: integer);
    property SeriesIndex : integer read FSeriesIndex write FSeriesIndex;
    property TargetDraftIndex : integer read FTargetDraftIndex write FTargetDraftIndex;
    property PlaneIndex : integer read FPlaneIndex write FPlaneIndex;
    property OnSeriesClickPointer : TSeriesClickPointerEvent read GetOnSeriesClickPointer write SetOnSeriesClickPointer;
  end;

implementation

uses
  Windows,
  SysUtils,
  UConstants,
  UErrorHandlingOperations;

{ TYRCPointSeries }

constructor TYRCPointSeries.Create(AOwner: TComponent);
const OPNAME = 'TYRCPointSeries.Create';
begin
  try
    inherited Create(AOwner);
    FSeriesIndex       := -1;
    FTargetDraftIndex  := -1;
    FPlaneIndex        := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{destructor TYRCPointSeries.Destroy;
const OPNAME = 'TYRCPointSeries.Destroy';
begin
  try
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TYRCPointSeries.SelectSeriesColor(AIndex: integer);
const OPNAME = 'TYRCPointSeries.SelectSeriesColor';
var
  LIndex: integer;
begin
  try
    LIndex := AIndex mod High(ColorArray);
    Self.Color := ColorArray[LIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPointSeries.SelectSeriesPointStyle(AIndex: integer);
const OPNAME = 'TYRCPointSeries.SelectSeriesPointStyle';
var
  LIndex: integer;
begin
  try
    LIndex := AIndex mod High(PointStyleArray);
    Self.Pointer.Style := VCLTee.TeEngine.TSeriesPointerStyle(LIndex);                   //PointStyleArray[LIndex];
    Self.Pointer.HorizSize := 2;
    Self.Pointer.VertSize := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCLineSeries }

constructor TYRCLineSeries.Create(AOwner: TComponent);
const OPNAME = 'TYRCLineSeries.Create';
begin
  try
    inherited Create(AOwner);
    FSeriesIndex := -1;
    FTargetDraftIndex := -1;
    FPlaneIndex       := -1;
    FOnSeriesClickPointer := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TYRCLineSeries.Destroy;
const OPNAME = 'TYRCLineSeries.Destroy';
begin
  try
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLineSeries.GetOnSeriesClickPointer: TSeriesClickPointerEvent;
const OPNAME = 'TYRCLineSeries.GetOnSeriesClickPointer';
begin
  Result := nil;
  try
    FOnSeriesClickPointer := OnClickPointer;
    Result := FOnSeriesClickPointer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLineSeries.SelectSeriesColor(AIndex: integer);
const OPNAME = 'TYRCLineSeries.SelectSeriesColor';
var
  LIndex: integer;
begin
  try
    LIndex := AIndex mod High(ColorArray);
    Self.Color := ColorArray[LIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLineSeries.SetOnSeriesClickPointer(const AValue: TSeriesClickPointerEvent);
const OPNAME = 'TYRCLineSeries.SetOnSeriesClickPointer';
begin
  try
    FOnSeriesClickPointer := AValue;
    OnClickPointer        := FOnSeriesClickPointer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TChartSelection }

{procedure TChartSelection.CreateMemberObjects;
const OPNAME = 'TChartSelection.CreateMemberObjects';
begin
  inherited;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChartSelection.Reset;
const OPNAME = 'TChartSelection.Reset';
begin
  try
    FPlottingBase        := NullInteger;
    FPeriodLength        := NullInteger;
    FTargetDraftIndex    := NullInteger;
    FZoomIndex           := NullInteger;
    FZoomValue           := NullFloat;

    FShowTargetDrafts    := stdAll;
    FChartMode           := cmView;
    FChartEditMode       := tdmNone;

    FHideRawPoints       := True;
    FHideRawLines        := True;
    FShowFirmYieldLabels := False;
    FShowCursorPosition  := False;
    FAssuranceVisible    := False;
    FMaxYValue           := NullFloat;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChartSelection.LoadFromINIFile;
const OPNAME = 'TChartSelection.LoadFromINIFile';
var
  LRealNum: string;
  LValue: integer;
begin
  try
    FPlottingBase        :=  FAppModules.ViewIni.ReadInteger(ClassName,'PlottingBase',NullInteger);
    FPeriodLength        :=  FAppModules.ViewIni.ReadInteger(ClassName,'PeriodLength',NullInteger);
    FTargetDraftIndex    :=  FAppModules.ViewIni.ReadInteger(ClassName,'TargetDraftIndex',NullInteger);
    FZoomIndex           :=  FAppModules.ViewIni.ReadInteger(ClassName,'ZoomIndex',NullInteger);
    LRealNum             :=  FAppModules.ViewIni.ReadString(ClassName,'ZoomValue','');
    FZoomValue           := StrToFloatDef(LRealNum,NullFloat);

    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'ShowTargetDrafts',0);
    FShowTargetDrafts    :=  TShowTargetDrafts(LValue);
    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'ChartMode',1);
    FChartMode           :=  TChartMode(LValue);
    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'ChartEditMode',0);
    FChartEditMode       :=  TChartEditMode(LValue);

    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'HideRawPoints',1);
    FHideRawPoints       :=  (LValue = 1);
    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'HideRawLines',1);
    FHideRawLines        :=  (LValue = 1);
    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'ShowFirmYieldLabels',0);
    FShowFirmYieldLabels :=  (LValue = 1);
    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'ShowCursorPosition',0);
    FShowCursorPosition  :=  (LValue = 1);
    LValue               :=  FAppModules.ViewIni.ReadInteger(ClassName,'AssuranceVisible',0);
    FAssuranceVisible    :=  (LValue = 1);
    LRealNum             :=  FAppModules.ViewIni.ReadString(ClassName,'MaxYValue','');
    FMaxYValue           := StrToFloatDef(LRealNum,NullFloat);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChartSelection.SaveToINIFile;
const OPNAME = 'TChartSelection.SaveToINIFile';
begin
  try
    FAppModules.ViewIni.WriteInteger(ClassName,'PlottingBase',FPlottingBase);
    FAppModules.ViewIni.WriteInteger(ClassName,'PeriodLength',FPeriodLength);
    FAppModules.ViewIni.WriteInteger(ClassName,'TargetDraftIndex',FTargetDraftIndex);
    FAppModules.ViewIni.WriteInteger(ClassName,'ZoomIndex',FZoomIndex);
    FAppModules.ViewIni.WriteString(ClassName,'ZoomValue',FormatFloat('#0.0#',FMaxYValue));

    FAppModules.ViewIni.WriteInteger(ClassName,'ShowTargetDrafts',Ord(FShowTargetDrafts));
    FAppModules.ViewIni.WriteInteger(ClassName,'ChartMode',Ord(FChartMode));
    FAppModules.ViewIni.WriteInteger(ClassName,'ChartEditMode',Ord(FChartEditMode));

    FAppModules.ViewIni.WriteInteger(ClassName,'HideRawPoints',Ord(FHideRawPoints));
    FAppModules.ViewIni.WriteInteger(ClassName,'HideRawLines',Ord(FHideRawLines));
    FAppModules.ViewIni.WriteInteger(ClassName,'ShowFirmYieldLabels',Ord(FShowFirmYieldLabels));
    FAppModules.ViewIni.WriteInteger(ClassName,'ShowCursorPosition',Ord(FShowCursorPosition));
    FAppModules.ViewIni.WriteInteger(ClassName,'AssuranceVisible',Ord(FAssuranceVisible));
    FAppModules.ViewIni.WriteString(ClassName,'MaxYValue',FormatFloat('##0.0#####',FMaxYValue));
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.
