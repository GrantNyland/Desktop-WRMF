
{******************************************************************************}
{*  UNIT      : Contains TfrmViewGaugeData Class                             *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 14/03/2014                                                    *}
{*  COPYRIGHT : Copyright © 2015 DWAF                                         *}
{******************************************************************************}

unit UFrameViewGaugeData;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Win.StdVCL,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  VCLTee.Chart,
  VCLTee.GanttCh,
  VCLTee.TeEngine,
  VCLTee.TeeProcs,

  Vcl.AxCtrls,
  UColourButtons,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  URWHDataObject,
  Vcl.Menus;

type

  TfrmViewGaugeData = class(TFrame)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    pnlClient: TPanel;
    pnlChart: TPanel;
    pnlGrid: TPanel;
    stgDailyData: TStringGrid;
   // stgDailyData      : TStringGrid;


  protected
    { Private declarations }
  //  F
    FRecordLengthChart : TAbstractChart;
    FChartSeries       : TGanttSeries;
    FAppModules        : TAppModules;

    //FstgDailyData : TFieldStringGrid;
    procedure ClearDialog;
    procedure PopulateObject;
    procedure PopulateDialog;
    procedure SetAppModules(AAppModules : TAppModules);
    function RWHModelData : TRWHModelData;

  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function Finalise   : boolean;
    function LanguageHasChanged : boolean;
    property AppModules : TAppModules read FAppModules write SetAppModules;
    property RecordLengthChart : TAbstractChart read FRecordLengthChart;
    property ChartSeries       : TGanttSeries   read FChartSeries;
  //  property stgDailyData      : TFieldStringGrid    read FstgDailyData;
  end;

var
  frmViewGaugeData : TfrmViewGaugeData;

implementation

{$R *.dfm}

uses

  UConstants,
  UUtilities,
  UErrorHandlingOperations;

{ TfrmViewGaugeData }

procedure TfrmViewGaugeData.AfterConstruction;
const OPNAME = 'TfrmViewGaugeData.AfterConstruction';
begin
  inherited;
  try

   (* FstgDailyData                  := TFieldStringGrid.Create(Self, FAppModules);
    FstgDailyData.Parent           := pnlGrid;
    FstgDailyData.FixedCols        := 0 ;
    FstgDailyData.Top              := 0;

    FstgDailyData.Align            := alClient;
    FstgDailyData.Options          := FstgDailyData.Options - [goEditing{, goRangeSelect}];
    FstgDailyData.WrapHeaderText   := True;
                                                    *)

    FRecordLengthChart                  := TAbstractChart.Create(Self, FAppModules);
    FRecordLengthChart.Parent           := pnlChart;
    FRecordLengthChart.Align            := alClient;
    FRecordLengthChart.BevelOuter       := bvNone;
    FRecordLengthChart.Title.Font.Color := clBlack;
    FRecordLengthChart.Title.Font.Style := [fsBold];
    FRecordLengthChart.View3D           := False;
    FRecordLengthChart.Legend.Visible   := False;
    FRecordLengthChart.BottomAxis.AxisValuesFormat   := '';
    FRecordLengthChart.BottomAxis.Increment          := 1;
    FRecordLengthChart.BottomAxis.LabelsAngle        := 90;
    FRecordLengthChart.BottomAxis.MinorTicks.Visible := FALSE;

    FChartSeries := TGanttSeries.Create(FRecordLengthChart);
    FRecordLengthChart.AddSeries(FChartSeries);
    FChartSeries.XValues.DateTime := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewGaugeData.BeforeDestruction;
const OPNAME = 'TfrmViewGaugeData.AfterConstruction';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewGaugeData.SetAppModules(AAppModules: TAppModules);
const OPNAME = 'TfrmViewGaugeData.SetAppModules';
begin
  try
    FAppModules := AAppModules;
    if(AAppModules <> nil) then
  //    FEditRunConfig  := TRWHRunConfig.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmViewGaugeData.Initialise: boolean;
const OPNAME = 'TfrmViewGaugeData.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmViewGaugeData.Finalise: boolean;
const OPNAME = 'TfrmViewGaugeData.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmViewGaugeData.LanguageHasChanged: boolean;
const OPNAME = 'TfrmViewGaugeData.LanguageHasChanged';
begin
  Result := False;
  try
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TfrmViewGaugeData.ClearDialog;
const OPNAME = 'TfrmViewGaugeData.ClearDialog';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewGaugeData.PopulateDialog;
const OPNAME = 'TfrmViewGaugeData.PopulateDialog';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewGaugeData.PopulateObject;
const OPNAME = 'TfrmViewGaugeData.PopulateObject';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmViewGaugeData.RWHModelData: TRWHModelData;
const OPNAME = 'TfrmViewGaugeData.RWHModelData';
begin
  Result := nil;
  try
    Result :=  TRWHModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
