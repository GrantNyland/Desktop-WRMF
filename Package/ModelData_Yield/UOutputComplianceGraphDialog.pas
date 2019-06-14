//
//
//  UNIT      : Contains the class TOutputComplianceGraphDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputComplianceGraphDialog;

interface

uses
  Classes,
  Windows,

  VCLTee.Chart,
  VCLTee.Series,

  VCLTee.TeeBoxPlot,
  VCLTee.TeeProcs,
  VCLTee.TeeTools,
  VCLTee.TeEngine,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCL.Forms,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

 const
   C_PointStyle: array [1..12]of TSeriesPointerStyle = (psCircle,psRectangle,psTriangle,psDiagCross
                                                   ,psStar,psCircle,psCross,psRectangle,psDiagCross
                                                   ,psDiamond,psRectangle,psDownTriangle);
   C_Colors    : array [1..12]of TColor              = (clMaroon,clFuchsia,clYellow,clLime,clPurple,clBlue
                                                       ,clSkyBlue,clOlive,clNavy,clGray,clRed,clGreen);
type
  TOPLevelAreaSeries = array of TAreaSeries;
  TOPLevelLineSeries = array of TLineSeries;
  TIFRPointSeries = array of TPointSeries;
  TInterceptValues = array of double;
  TZoneElevationSeries = array of TLineSeries;

  TReservoirChannelComparitor = class(TAbstractPanel)
  protected
    FReservoirChart                   : TFieldChart;
    FChannelChart                     : TFieldChart;
    FOperatingLevels                  : integer;
    FChannelCount                     : integer;
    FReservoirZoneElevationTimeSeries : TAreaSeries;
    FReservoirZoneElevationFSLSeries  : TAreaSeries;
    FReservoirZoneElevationOPLSeries  : TOPLevelAreaSeries;
    FReservoirZoneElevationDSLSeries  : TAreaSeries;
    FReservoirZoneElevationBORSeries  : TAreaSeries;
    FChannelFlowTimeSeries            : TOPLevelLineSeries;
    FChannelFlowCapabilitySeries      : TOPLevelLineSeries;
    FReservoirFullStorageVolumeSeries : TLineSeries;
    FReservoirDeadStorageVolumeSeries : TLineSeries;
    FReservoirOperatingLevelSeries    : TOPLevelLineSeries;
    FColorBands                       : TPointSeries;

    FChannelsPanel                    : TPanel;
    FChannelsComboBox                 : TFieldComboBox;
    FChannelsLabel                    : TLabel;

    FDisplayString                    : String;

    procedure ConfigureChart(AChart : TFieldChart);
    procedure ConfigureSeries(ASeries : TAreaSeries);
    procedure OnGetLineSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    procedure OnScrollPerformed(Sender: TObject);
    procedure OnUndoZoomPerformed(Sender: TObject);
    procedure OnZoomPerformed(Sender: TObject);
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
    procedure DrawIntercepts(AValues : TInterceptValues);
    procedure ClearChart;
    procedure PrepareChart;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function  LanguageHasChanged : boolean; override;
    function  StudyHasChanged : boolean; override;
    function  Initialise: boolean; override;
    procedure RefreshChartAxisDisplay(AChart : TFieldChart);
    property ReservoirChart                   : TFieldChart    read FReservoirChart;
    property ChannelChart                     : TFieldChart    read FChannelChart;
    property OperatingLevels                  : integer        read FOperatingLevels write FOperatingLevels;
    property ChannelCount                     : integer        read FChannelCount    write FChannelCount;
    property ReservoirZoneElevationTimeSeries : TAreaSeries    read FReservoirZoneElevationTimeSeries;
    property ReservoirZoneElevationFSLSeries  : TAreaSeries    read FReservoirZoneElevationFSLSeries;
    property ReservoirZoneElevationOPLSeries  : TOPLevelAreaSeries read FReservoirZoneElevationOPLSeries;
    property ReservoirZoneElevationDSLSeries  : TAreaSeries    read FReservoirZoneElevationDSLSeries;
    property ReservoirZoneElevationBORSeries  : TAreaSeries    read FReservoirZoneElevationBORSeries;
    property ChannelFlowTimeSeries            : TOPLevelLineSeries read FChannelFlowTimeSeries;
    property ChannelFlowCapabilitySeries      : TOPLevelLineSeries read FChannelFlowCapabilitySeries;
    property ReservoirOperatingLevelSeries    : TOPLevelLineSeries read FReservoirOperatingLevelSeries;
    property ReservoirFullStorageVolumeSeries : TLineSeries        read FReservoirFullStorageVolumeSeries;
    property ReservoirDeadStorageVolumeSeries : TLineSeries        read FReservoirDeadStorageVolumeSeries;
    property ColorBands                       : TPointSeries       read FColorBands;
    property ChannelsComboBox                 : TFieldComboBox     read FChannelsComboBox;
    property DisplayString                    : String             read FDisplayString write FDisplayString;
  end;

  TReservoirStorageVolumeComparitor = class(TAbstractPanel)
  private
    FDisplayString: String;
  protected
    FReservoirChart                    : TFieldChart;
    FReservoirStorageVolumeSimSeries   : TAreaSeries;
    FReservoirStorageVolumeActSeries   : TLineSeries;
    FReservoirFullStorageVolumeSeries  : TLineSeries;
    FReservoirDeadStorageVolumeSeries  : TLineSeries;
    FZoneElevationSeries               : TZoneElevationSeries;
    procedure ConfigureZonesSeries(ALineSeries : TLineSeries);
    function GetZoneElevationSeriesByIndex (AIndex : integer) : TLineSeries;
    procedure OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateZoneSeries (ACount : integer);
    procedure ClearChart;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure ConfigureSeries;
    procedure RefreshChartAxisDisplay(AMax : double);
    function  LanguageHasChanged : boolean; override;
    function  StudyHasChanged : boolean; override;
    function  Initialise: boolean; override;
    procedure Resize; override;
    property ReservoirChart                    : TFieldChart            read FReservoirChart;
    property ReservoirStorageVolumeSimSeries   : TAreaSeries            read FReservoirStorageVolumeSimSeries;
    property ReservoirStorageVolumeActSeries   : TLineSeries            read FReservoirStorageVolumeActSeries;
    property ReservoirDeadStorageVolumeSeries  : TLineSeries            read FReservoirDeadStorageVolumeSeries;
    property ReservoirFullStorageVolumeSeries  : TLineSeries            read FReservoirFullStorageVolumeSeries;
    property ZoneElevationSeriesByIndex[AIndex : integer] : TLineSeries read GetZoneElevationSeriesByIndex;
    property DisplayString                     : String                 read FDisplayString write FDisplayString;
   end;

  TBoxSeriesArray = array of TBoxSeries;

  TReservoirStorageGraph = class(TAbstractPanel)
  private
    FDisplayString: String;
  protected
    FReservoirStorageChart  : TFieldChart;
    FReservoirSimTimeSeries : TBoxSeriesArray;
    FReservoirActTimeSeries : TLineSeries;
    FReservoirFSVSeries     : TLineSeries;
    FReservoirDSVSeries     : TLineSeries;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
    procedure OnBeforeDrawValues(Sender: TObject);
  public
    procedure Resize; override;
    procedure ConfigureChart(AChart : TFieldChart);
    procedure ConfigureBoxSeries(ABoxSeries : TBoxSeries);
    procedure ConfigureActLineSeries(ALineSeries : TLineSeries);
    procedure ConfigureSVLineSeries(ALineSeries : TLineSeries);
    procedure PrepareChart;
    procedure ClearChart;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property ReservoirStorageChart : TFieldChart      read FReservoirStorageChart;
    property ReservoirSimTimeSeries : TBoxSeriesArray read FReservoirSimTimeSeries;
    property ReservoirActTimeSeries : TLineSeries     read FReservoirActTimeSeries;
    property ReservoirFSVSeries     : TLineSeries     read FReservoirFSVSeries;
    property ReservoirDSVSeries     : TLineSeries     read FReservoirDSVSeries;
    property DisplayString          : String          read FDisplayString write FDisplayString;
  end;

  TIFRChannelComplienceGraph = class(TAbstractPanel)
  protected
    FRequirementFlowValuesChart        : TFieldChart;

    FLineSeriesArrayOne             : TOPLevelLineSeries;
    FLineSeriesArrayTwo             : TOPLevelLineSeries;

    FPointSeriesArrayOne            : TIFRPointSeries;
    FPointSeriesArrayTwo            : TIFRPointSeries;

    FLineSeriesOne                  : TLineSeries;
    FLineSeriesTwo                  : TLineSeries;

    FPointSeriesOne                 : TPointSeries;
    FPointSeriesTwo                 : TPointSeries;
    FActualBarSeries                : TBarSeries;
    FRequiredBarSeries              : TBarSeries;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure Resize; override;
    procedure ClearChart;
    procedure ConfigureLineSeries(ALineSeries: TLineSeries; AColor : TColor);
    procedure ConfigurePointSeries(APointSeries: TPointSeries; AColor : TColor);
    procedure PrepareChart;
    property RequirementFlowValuesChart    : TFieldChart            read FRequirementFlowValuesChart;

    property LineSeriesArrayOne      : TOPLevelLineSeries    read FLineSeriesArrayOne;
    property LineSeriesArrayTwo      : TOPLevelLineSeries    read FLineSeriesArrayTwo;
    property PointSeriesArrayOne     : TIFRPointSeries       read FPointSeriesArrayOne;
    property PointSeriesArrayTwo     : TIFRPointSeries       read FPointSeriesArrayTwo;

    property LineSeriesOne            : TLineSeries           read FLineSeriesOne;
    property LineSeriesTwo            : TLineSeries           read FLineSeriesTwo;
    property PointSeriesOne           : TPointSeries          read FPointSeriesOne;
    property PointSeriesTwo        : TPointSeries           read FPointSeriesTwo;
    property ActualBarSeries           : TBarSeries          read FActualBarSeries;
    property RequiredBarSeries         : TBarSeries          read FRequiredBarSeries;
  end;

  TOutputComplianceGraphDialog = class(TAbstractScrollablePanel)
  private
  protected
    FSelectorPanel              : TPanel;
    FViewDataType               : TFieldComboBox;
    FViewDataLabel              : TLabel;
    FBtnDataSelection           : TFieldButton;
    FReservoirChannelComparitor : TReservoirChannelComparitor;
    FReservoirStorageComparitor : TReservoirStorageVolumeComparitor;
    FReservoirStorageGraph      : TReservoirStorageGraph;
    FIFRChannelComplienceGraph  : TIFRChannelComplienceGraph;
    FUnitsLabel                 : TLabel;
    FDisplayString              : String;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DestroyMemberObjects; override;
  public
    procedure SetDisplayString(AValue: String);
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property cmbViewDataType            : TFieldComboBox                    read FViewDataType;
    property ReservoirChannelComparitor : TReservoirChannelComparitor       read FReservoirChannelComparitor;
    property ReservoirStorageComparitor : TReservoirStorageVolumeComparitor read FReservoirStorageComparitor;
    property ReservoirStorageGraph      : TReservoirStorageGraph            read FReservoirStorageGraph;
    property IFRChannelComplienceGraph  : TIFRChannelComplienceGraph        read FIFRChannelComplienceGraph;
    property BtnDataSelection           : TFieldButton                      read FBtnDataSelection;
    property DisplayString              : String                            read FDisplayString write SetDisplayString;
    property UnitsLabel                 : TLabel                            read FUnitsLabel;
  end;

  implementation

uses
  SysUtils,
  VCLTee.TeExport,
  UConstants,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

var
  C_OPLevelColors : array [0..9] of TColor = (clYellow, clRed,
                                              clMaroon, clLime,
                                              clGray,   clOlive,
                                              clNavy,   clPurple,
                                              clOlive,  clAqua);


{ TOutputComplianceGraphDialog }

procedure TOutputComplianceGraphDialog.CreateMemberObjects;
const OPNAME = 'TOutputComplianceGraphDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel                      := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent               := ControlsParent;

    FViewDataLabel                      := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent               := FSelectorPanel;
    FViewDataLabel.Alignment            := taCenter;
    FViewDataLabel.AutoSize             := False;

    FViewDataType                       := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent                := FSelectorPanel;

    FUnitsLabel                         := TLabel.Create(ControlsOwner);
    FUnitsLabel.Parent                  := FSelectorPanel;

    FBtnDataSelection                   := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent            := FSelectorPanel;

    FReservoirChannelComparitor         := TReservoirChannelComparitor.Create(ControlsOwner, FAppModules);
    FReservoirChannelComparitor.Parent  := ControlsParent;
    FReservoirChannelComparitor.Visible := False;

    FReservoirStorageComparitor         := TReservoirStorageVolumeComparitor.Create(ControlsOwner, FAppModules);
    FReservoirStorageComparitor.Parent  := ControlsParent;
    FReservoirStorageComparitor.Visible := False;

    FReservoirStorageGraph              := TReservoirStorageGraph.Create(ControlsOwner, FAppModules);
    FReservoirStorageGraph.Parent       := ControlsParent;
    FReservoirStorageGraph.Visible      := False;

    FIFRChannelComplienceGraph          := TIFRChannelComplienceGraph.Create(ControlsOwner, FAppModules);
    FIFRChannelComplienceGraph.Parent   := ControlsParent;
    FIFRChannelComplienceGraph.Visible  := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphDialog.Initialise: boolean;
const OPNAME = 'TOutputComplianceGraphDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataType.Clear;
    FSelectorPanel.BorderStyle := bsSingle;
    FReservoirChannelComparitor.Initialise;
    FReservoirStorageComparitor.Initialise;
    FReservoirStorageGraph.Initialise;
    FIFRChannelComplienceGraph.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputComplianceGraphDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FViewDataType.StudyHasChanged;
    FReservoirChannelComparitor.StudyHasChanged;
    FReservoirStorageComparitor.StudyHasChanged;
    FReservoirStorageGraph.StudyHasChanged;
    FIFRChannelComplienceGraph.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComplianceGraphDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FViewDataType.LanguageHasChanged;
    FViewDataLabel.Caption          := FAppModules.Language.GetString('LabelText.ViewData');
    FBtnDataSelection.Caption       := FAppModules.Language.GetString('LabelText.SelectData');
    FReservoirChannelComparitor.LanguageHasChanged;
    FReservoirStorageComparitor.LanguageHasChanged;
    FReservoirStorageGraph.LanguageHasChanged;
    FIFRChannelComplienceGraph.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphDialog.AssignHelpContext;
const OPNAME = 'TOutputComplianceGraphDialog.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
    SetControlHelpContext(Self,                   HC_ResultOutput);
    SetControlHelpContext(FViewDataType, HC_ResultOutput);
    SetControlHelpContext(FReservoirChannelComparitor, HC_ResultOutput);
    SetControlHelpContext(FReservoirStorageComparitor, HC_ResultOutput);
    SetControlHelpContext(FReservoirStorageGraph, HC_ResultOutput);
    SetControlHelpContext(FIFRChannelComplienceGraph, HC_ResultOutput);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGraphDialog.Resize;
const OPNAME = 'TOutputComplianceGraphDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align                := alTop;
      FSelectorPanel.ClientHeight         := 30;

      FViewDataType.Align                 := alLeft;
      FViewDataType.Width                 := 300;

      FUnitsLabel.Top                     := 5;
      FUnitsLabel.Left                    := 370;
      FUnitsLabel.Width                   := 60;
      FUnitsLabel.Font.Style              := [fsBold];

      FViewDataLabel.Align                := alLeft;
      FViewDataLabel.Width                := 60;
      FViewDataLabel.Layout               := tlCenter;

      FBtnDataSelection.Align             := alRight;
      FBtnDataSelection.Width             := 80;

      FReservoirChannelComparitor.Align   := alClient;
      FReservoirStorageComparitor.Align   := alClient;
      FReservoirStorageGraph.Align        := alClient;
      FIFRChannelComplienceGraph.Align    := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TOutputComplianceGraphDialog.CanExport: boolean;
const OPNAME = 'TOutputComplianceGraphDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FReservoirChannelComparitor) and (FReservoirChannelComparitor.Visible)) or
              (Assigned(FReservoirStorageComparitor) and (FReservoirStorageComparitor.Visible)) or
              (Assigned(FReservoirStorageGraph)      and (FReservoirStorageGraph.Visible))      or
              (Assigned(FIFRChannelComplienceGraph)  and (FIFRChannelComplienceGraph.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComplianceGraphDialog.CanPrint: boolean;
const OPNAME = 'TOutputComplianceGraphDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FReservoirChannelComparitor) and (FReservoirChannelComparitor.Visible)) or
              (Assigned(FReservoirStorageComparitor) and (FReservoirStorageComparitor.Visible)) or
              (Assigned(FReservoirStorageGraph)      and (FReservoirStorageGraph.Visible))      or
              (Assigned(FIFRChannelComplienceGraph)  and (FIFRChannelComplienceGraph.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGraphDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputComplianceGraphDialog.DoExport';
begin
  try
    if FReservoirChannelComparitor.Visible then FReservoirChannelComparitor.DoExport(AFileName);
    if FReservoirStorageComparitor.Visible then FReservoirStorageComparitor.DoExport(AFileName);
    if FReservoirStorageGraph.Visible then FReservoirStorageGraph.DoExport(AFileName);
    if FIFRChannelComplienceGraph.Visible then FIFRChannelComplienceGraph.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGraphDialog.DoPrint;
const OPNAME = 'TOutputComplianceGraphDialog.DoPrint';
begin
  try
    if FReservoirChannelComparitor.Visible then FReservoirChannelComparitor.DoPrint;
    if FReservoirStorageComparitor.Visible then FReservoirStorageComparitor.DoPrint;
    if FReservoirStorageGraph.Visible then FReservoirStorageGraph.DoPrint;
    if FIFRChannelComplienceGraph.Visible then FIFRChannelComplienceGraph.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGraphDialog.DestroyMemberObjects;
const OPNAME = 'TOutputComplianceGraphDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphDialog.SetDisplayString(AValue: String);
const OPNAME = 'TOutputComplianceGraphDialog.SetDisplayString';
begin
  try
    FDisplayString := AValue;
    FReservoirStorageGraph.DisplayString := FDisplayString;
    FReservoirChannelComparitor.DisplayString := FDisplayString;
    FReservoirStorageComparitor.DisplayString := FDisplayString;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TReservoirStorageGraph }

procedure TReservoirStorageGraph.AssignHelpContext;
const OPNAME = 'TReservoirStorageGraph.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
    SetControlHelpContext(Self,                   HC_ResultOutput);
    SetControlHelpContext(VCL.Controls.TControl(FReservoirStorageChart), HC_ResultOutput);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.ClearChart;
const OPNAME = 'TReservoirStorageGraph.ClearChart';
var
  LIndex : integer;
begin
  try
    FReservoirActTimeSeries.Active := False;
    FReservoirActTimeSeries.Clear;

    FReservoirFSVSeries.Active     := False;
    FReservoirFSVSeries.Clear;

    FReservoirDSVSeries.Active     := False;
    FReservoirDSVSeries.Clear;

    for LIndex := 0 to Length(FReservoirSimTimeSeries) - 1 do
    begin
      FReservoirSimTimeSeries[LIndex].Active := False;
      FReservoirSimTimeSeries[LIndex].Clear;
    end;
    FReservoirStorageChart.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.ConfigureBoxSeries(ABoxSeries: TBoxSeries);
const OPNAME = 'TReservoirStorageGraph.ConfigureBoxSeries';
begin
  try
    if Assigned(ABoxSeries) then
    begin
      ABoxSeries.ShowInLegend                := False;
      ABoxSeries.XValues.DateTime            := True;
      ABoxSeries.Marks.Callout.Brush.Color   := clBlack;
      ABoxSeries.Marks.Visible               := False;
      ABoxSeries.SeriesColor                 := clBlue;
      ABoxSeries.ClickableLine               := False;
      ABoxSeries.Pointer.Brush.Color         := clBlue;
      ABoxSeries.Pointer.Draw3D              := False;
      ABoxSeries.Pointer.Gradient.EndColor   := clBlue;
      ABoxSeries.Pointer.Gradient.StartColor := clRed;
      ABoxSeries.Pointer.Gradient.Visible    := True;
      ABoxSeries.Pointer.InflateMargins      := True;
      ABoxSeries.Pointer.Style               := VCLTee.TeEngine.TSeriesPointerStyle(psRectangle);
      ABoxSeries.Pointer.Visible             := True;
      ABoxSeries.XValues.Order               := VCLTee.TeEngine.TChartListOrder(loAscending);
      ABoxSeries.YValues.Order               := VCLTee.TeEngine.TChartListOrder(loAscending);
      ABoxSeries.ExtrOut.InflateMargins      := True;
      ABoxSeries.ExtrOut.Style               := VCLTee.TeEngine.TSeriesPointerStyle(psRectangle);
      ABoxSeries.ExtrOut.Visible             := True;
      ABoxSeries.ExtrOut.HorizSize           := 15;
      ABoxSeries.ExtrOut.VertSize            := 1;
      ABoxSeries.MildOut.InflateMargins      := True;
      ABoxSeries.MildOut.Style               := VCLTee.TeEngine.TSeriesPointerStyle(psRectangle);
      ABoxSeries.MildOut.Visible             := True;
      ABoxSeries.MildOut.HorizSize           := 15;
      ABoxSeries.MildOut.VertSize            := 1;
      ABoxSeries.WhiskerLength               := 1.50;
      ABoxSeries.WhiskerPen.Width            := 2;
      ABoxSeries.MedianPen.Style             := psSolid;
      ABoxSeries.MedianPen.Width             := 2;
      ABoxSeries.UseCustomValues             := False;
      ABoxSeries.BeforeDrawValues            := OnBeforeDrawValues;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.ConfigureChart(AChart: TFieldChart);
const OPNAME = 'TReservoirStorageGraph.ConfigureChart';
begin
  try
    AChart.Legend.Visible            := False;
    AChart.View3D                    := False;
    //AChart.View3DWalls               := False;
    AChart.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';
    AChart.BottomAxis.LabelsAngle    := 90;
    AChart.LeftAxis.AxisValuesFormat := '###,###,##0.00';
    AChart.LeftAxis.TitleSize        := 1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.ConfigureActLineSeries(ALineSeries: TLineSeries);
const OPNAME = 'TReservoirStorageGraph.ConfigureActLineSeries';
begin
  try
    ALineSeries.SeriesColor      := clRed;
    ALineSeries.LinePen.Width    := 1;
    ALineSeries.LinePen.Style    := psDot;
    ALineSeries.ShowInLegend     := False;
    ALineSeries.XValues.DateTime := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.CreateMemberObjects;
const OPNAME = 'TReservoirStorageGraph.CreateMemberObjects';
var
  LIndex : integer;
begin
  inherited CreateMemberObjects;
  try
    FReservoirStorageChart := TFieldChart.Create(Self, FAppModules);
    FReservoirStorageChart.Parent := Self;

    SetLength(FReservoirSimTimeSeries, 12);
    for LIndex := 0 to 11 do
    begin
      FReservoirSimTimeSeries[LIndex]             := TBoxSeries.Create(FReservoirStorageChart);
      FReservoirSimTimeSeries[LIndex].ParentChart := VCLTee.TeEngine.TCustomAxisPanel(FReservoirStorageChart);
      ConfigureBoxSeries(FReservoirSimTimeSeries[LIndex]);
      FReservoirStorageChart.AddSeries(FReservoirSimTimeSeries[LIndex]);
    end;
    FReservoirActTimeSeries             := TLineSeries.Create(FReservoirStorageChart);
    FReservoirActTimeSeries.ParentChart := FReservoirStorageChart;
    ConfigureActLineSeries(FReservoirActTimeSeries);
    FReservoirStorageChart.AddSeries(FReservoirActTimeSeries);

    FReservoirFSVSeries                 := TLineSeries.Create(FReservoirStorageChart);
    FReservoirFSVSeries.ParentChart     := FReservoirStorageChart;
    ConfigureSVLineSeries(FReservoirFSVSeries);
    FReservoirStorageChart.AddSeries(FReservoirFSVSeries);

    FReservoirDSVSeries                 := TLineSeries.Create(FReservoirStorageChart);
    FReservoirDSVSeries.ParentChart     := FReservoirStorageChart;
    ConfigureSVLineSeries(FReservoirDSVSeries);
    FReservoirStorageChart.AddSeries(FReservoirDSVSeries);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.DestroyMemberObjects;
const OPNAME = 'TReservoirStorageGraph.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirStorageGraph.Initialise: boolean;
const OPNAME = 'TReservoirStorageGraph.Initialise';
begin
  Result := False;
  try
    ConfigureChart(FReservoirStorageChart);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirStorageGraph.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirStorageGraph.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.PrepareChart;
const OPNAME = 'TReservoirStorageGraph.PrepareChart';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.Resize;
const OPNAME = 'TReservoirStorageGraph.Resize';
begin
  inherited Resize;
  try
    FReservoirStorageChart.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirStorageGraph.StudyHasChanged: boolean;
const OPNAME = 'TReservoirStorageGraph.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.ConfigureSVLineSeries(ALineSeries: TLineSeries);
const OPNAME = 'TReservoirStorageGraph.ConfigureSVLineSeries';
begin
  try
    ALineSeries.SeriesColor      := clBlack;
    ALineSeries.LinePen.Width    := 1;
    ALineSeries.LinePen.Style    := psDash;
    ALineSeries.ShowInLegend     := False;
    ALineSeries.XValues.DateTime := True;
    ALineSeries.Pointer.Visible  := False;
    ALineSeries.Marks.Visible    := True;
    ALineSeries.Marks.Clip       := True;
    ALineSeries.OnGetMarkText    := OnGetLineSeriesMarkText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirStorageGraph.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TReservoirStorageGraph.OnGetLineSeriesMarkText';
var
  LDisplayStr: string;
  LXValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    If not Self.Visible then Exit;
    if (ValueIndex <> 0) then Exit;

    if not (Assigned(FReservoirStorageChart)) then
      Exit;
    LDisplayStr := '';
    if Sender = FReservoirFSVSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.Supply' + FDisplayString)
    else
    if Sender = FReservoirDSVSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.Storage' + FDisplayString);

    if (LDisplayStr <> '') then
    begin
      LXValue  := Sender.XValues.MaxValue - Sender.XValues.MinValue;
      LXValue  := LXValue/2.5;
      LYPos   := Sender.CalcYPosValue(Sender.YValue[ValueIndex] + 1.5);
      //LYPos   := Sender.CalcYPosValue(Sender.XValue[ValueIndex]) - 80;
      LXPos   := Sender.CalcXPosValue(LXValue);
      FReservoirStorageChart.Canvas.Font.Style := [fsBold];
      if Sender = FReservoirFSVSeries then
        FReservoirStorageChart.BottomAxis.DrawAxisLabel(LXPos+150,LYPos, 0,LDisplayStr)
      else
        FReservoirStorageChart.BottomAxis.DrawAxisLabel(LXPos,LYPos, 0,LDisplayStr)
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageGraph.OnBeforeDrawValues(Sender: TObject);
const OPNAME = 'TReservoirStorageGraph.OnBeforeDrawValues';
begin
  try
    If not Self.Visible then Exit;
    if (Sender <> nil) and
       (Sender is TBoxSeries) then
    begin
      TBoxSeries(Sender).SampleValues.Sort;
      TBoxSeries(Sender).RecalcStats;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageGraph.DoExport(AFileName: string = '');
const OPNAME = 'TReservoirStorageGraph.DoExport';
begin
  try
    FReservoirStorageChart.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageGraph.DoPrint;
const OPNAME = 'TReservoirStorageGraph.DoPrint';
begin
  try
    FReservoirStorageChart.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirChannelComparitor }

procedure TReservoirChannelComparitor.ConfigureChart(AChart: TFieldChart);
const OPNAME = 'TReservoirChannelComparitor.ConfigureChart';
begin
  try
    AChart.View3D                    := False;
    //AChart.View3DWalls               := False;
    AChart.Legend.Visible            := False;
    AChart.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';
    AChart.LeftAxis.AxisValuesFormat := '######0';
    AChart.LeftAxis.TitleSize        := 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.ConfigureSeries(ASeries: TAreaSeries);
const OPNAME = 'TReservoirChannelComparitor.ConfigureSeries';
begin
  try
    ASeries.XValues.DateTime         := True;
    ASeries.MultiArea                := maNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.CreateMemberObjects;
const OPNAME = 'TReservoirChannelComparitor.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FReservoirChart                   := TFieldChart.Create(Self, FAppModules);
    FChannelChart                     := TFieldChart.Create(Self, FAppModules);
    FReservoirChart.Parent            := Self;
    FChannelChart.Parent              := Self;
    ConfigureChart(FReservoirChart);
    ConfigureChart(FChannelChart);
    FReservoirZoneElevationFSLSeries  := TAreaSeries.Create(FReservoirChart);
    FReservoirZoneElevationTimeSeries := TAreaSeries.Create(FReservoirChart);
    FReservoirZoneElevationDSLSeries  := TAreaSeries.Create(FReservoirChart);
    FReservoirZoneElevationBORSeries  := TAreaSeries.Create(FReservoirChart);
    FReservoirFullStorageVolumeSeries := TLineSeries.Create(FReservoirChart);
    FReservoirDeadStorageVolumeSeries := TLineSeries.Create(FReservoirChart);
    FColorBands                       := TPointSeries.Create(FChannelChart);

    FChannelsPanel                      := TPanel.Create(Self);
    FChannelsPanel.Parent               := Self;
    FChannelsComboBox                   := TFieldComboBox.Create(Self,FAppModules);
    FChannelsComboBox.Parent            := FChannelsPanel;

    FChannelsLabel                      := TLabel.Create(Self);
    FChannelsLabel.Parent               := FChannelsPanel;

    FReservoirChart.OnScroll   := OnScrollPerformed;
    FReservoirChart.OnUndoZoom := OnUndoZoomPerformed;
    FReservoirChart.OnZoom     := OnZoomPerformed;

    FChannelChart.OnScroll   := OnScrollPerformed;
    FChannelChart.OnUndoZoom := OnUndoZoomPerformed;
    FChannelChart.OnZoom     := OnZoomPerformed;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.DestroyMemberObjects;
const OPNAME = 'TReservoirChannelComparitor.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.DrawIntercepts(AValues: TInterceptValues);
const OPNAME = 'TReservoirChannelComparitor.DrawIntercepts';
var
  LIndex : integer;
  LTopY,
  LBottomY : integer;
begin
  try
    if (Length(AValues) > 0) then
    begin
      LTopY                            := VCL.Controls.TControl(FReservoirChart).ClientRect.Top;
      LBottomY                         := VCL.Controls.TControl(FReservoirChart).ClientRect.Bottom;
      FReservoirChart.Canvas.Pen.Color := clBlack;
      FReservoirChart.Canvas.Pen.Style := psDot;
      for LIndex := 0 to Length(AValues) - 1 do
      begin
        FReservoirChart.Canvas.MoveTo(Round(AValues[LIndex]),
                                      Round(LTopY));
        FReservoirChart.Canvas.LineTo(Round(AValues[LIndex]),
                                      Round(LBottomY));
      end;
      LTopY                          := VCL.Controls.TControl(FChannelChart).ClientRect.Top;
      LBottomY                       := VCL.Controls.TControl(FChannelChart).ClientRect.Bottom;
      FChannelChart.Canvas.Pen.Color := clBlack;
      FChannelChart.Canvas.Pen.Style := psDot;
      for LIndex := 0 to Length(AValues) - 1 do
      begin
        FChannelChart.Canvas.MoveTo(Round(AValues[LIndex]),
                                    Round(LTopY));
        FChannelChart.Canvas.LineTo(Round(AValues[LIndex]),
                                    Round(LBottomY));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirChannelComparitor.Initialise: boolean;
const OPNAME = 'TReservoirChannelComparitor.Initialise';
begin
  Result := False;
  try
    ClearChart;
    FChannelsComboBox.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.ClearChart;
const OPNAME = 'TReservoirChannelComparitor.ClearChart';
var
  LIndex : integer;
  LClear : boolean;
begin
  try
    LClear := Assigned(FReservoirZoneElevationTimeSeries) and
              Assigned(FReservoirZoneElevationFSLSeries) and
              Assigned(FReservoirZoneElevationDSLSeries) and
              Assigned(FReservoirZoneElevationBORSeries) and
              Assigned(FReservoirFullStorageVolumeSeries) and
              Assigned(FReservoirDeadStorageVolumeSeries) and
              Assigned(FColorBands);

    if not LClear then Exit;

    FReservoirZoneElevationTimeSeries.Active := False;
    FReservoirZoneElevationFSLSeries.Active  := False;
    FReservoirZoneElevationDSLSeries.Active  := False;
    FReservoirZoneElevationBORSeries.Active  := False;
    FReservoirFullStorageVolumeSeries.Active := False;
    FReservoirDeadStorageVolumeSeries.Active := False;
    FColorBands.Active                       := False;

    FReservoirZoneElevationTimeSeries.Clear;
    FReservoirZoneElevationFSLSeries.Clear;
    FReservoirZoneElevationDSLSeries.Clear;
    FReservoirZoneElevationBORSeries.Clear;
    FReservoirFullStorageVolumeSeries.Clear;
    FReservoirDeadStorageVolumeSeries.Clear;
    FColorBands.Clear;

    FReservoirZoneElevationTimeSeries.ParentChart := nil;
    FReservoirZoneElevationFSLSeries.ParentChart  := nil;
    FReservoirZoneElevationDSLSeries.ParentChart  := nil;
    FReservoirZoneElevationBORSeries.ParentChart  := nil;
    FReservoirFullStorageVolumeSeries.ParentChart := nil;
    FReservoirDeadStorageVolumeSeries.ParentChart := nil;
    FColorBands.ParentChart                       := nil;

    if FOperatingLevels > 0 then
    begin
      for LIndex := 0 to FOperatingLevels - 1 do
      begin
        if (Length(FReservoirOperatingLevelSeries) > 0) and
           (Length(FReservoirZoneElevationOPLSeries) > 0) then
        begin
          FReservoirOperatingLevelSeries[LIndex].Active := False;
          FReservoirOperatingLevelSeries[LIndex].Clear;
          FReservoirOperatingLevelSeries[LIndex].ParentChart := nil;
          FReservoirOperatingLevelSeries[LIndex]             := nil;

          FReservoirZoneElevationOPLSeries[LIndex].Active := False;
          FReservoirZoneElevationOPLSeries[LIndex].Clear;
          FReservoirZoneElevationOPLSeries[LIndex].ParentChart := nil;
          FReservoirZoneElevationOPLSeries[LIndex]             := nil;
        end;
      end;
    end;
    if FChannelCount > 0 then
    begin
      for LIndex := 0 to FChannelCount - 1 do
      begin
        if (Length(FChannelFlowTimeSeries) > 0) then
        begin
          FChannelFlowTimeSeries[LIndex].Active            := False;
          FChannelFlowTimeSeries[LIndex].Clear;
          FChannelFlowTimeSeries[LIndex].ParentChart       := nil;
          FChannelFlowTimeSeries[LIndex]                   := nil;

          FChannelFlowCapabilitySeries[LIndex].Active      := False;
          FChannelFlowCapabilitySeries[LIndex].Clear;
          FChannelFlowCapabilitySeries[LIndex].ParentChart := nil;
          FChannelFlowCapabilitySeries[LIndex]             := nil;
        end;
      end;
    end;
    SetLength(FReservoirOperatingLevelSeries, 0);
    SetLength(FReservoirZoneElevationOPLSeries, 0);

    SetLength(FChannelFlowTimeSeries, 0);
    SetLength(FChannelFlowCapabilitySeries, 0);

    FReservoirChart.RemoveAllSeries;
    FChannelChart.RemoveAllSeries;

    FReservoirChart.LeftAxis.Title.Caption := '';
    FReservoirChart.BottomAxis.Title.Caption := '';
    FReservoirChart.Title.Text.Clear;

    FChannelChart.LeftAxis.Title.Caption := '';
    FChannelChart.BottomAxis.Title.Caption := '';
    FChannelChart.Title.Text.Clear;

    FReservoirChart.UndoZoom;
    FChannelChart.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirChannelComparitor.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirChannelComparitor.LanguageHasChanged';
begin
  Result := False;
  try
    FChannelsLabel.Caption := FAppModules.Language.GetString('Channel.Channel');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.Resize;
const OPNAME = 'TReservoirChannelComparitor.Resize';
begin
  inherited Resize;
  try
    FReservoirChart.Align  := alNone;
    FReservoirChart.Align  := alNone;

    FReservoirChart.Align  := alTop;
    FReservoirChart.Height := (Self.ClientHeight div 2);

    FChannelsPanel.Height  := 30;
    FChannelsPanel.Top     := VCL.Controls.TControl(FReservoirChart).Height + C_ControlBorder;
    FChannelsPanel.Left    := VCL.Controls.TControl(FReservoirChart).Left;
    FChannelsPanel.Width   := Self.ClientWidth;

    FChannelsLabel.Align   := alLeft;
    FChannelsLabel.Width   := 60;
    FChannelsLabel.Layout  := tlCenter;

    FChannelsComboBox.Top  := FChannelsLabel.Top;
    FChannelsComboBox.Left := FChannelsLabel.Width + C_LabelOffset;
    FChannelsComboBox.Width:= 300;
    VCL.Controls.TControl(FChannelChart).Top      := VCL.Controls.TControl(FReservoirChart).Height + FChannelsPanel.Height;
    FChannelChart.Height   := FReservoirChart.Height - FChannelsPanel.Height;
    FChannelChart.Width    := Self.ClientWidth;
    //FChannelChart.Align    := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirChannelComparitor.StudyHasChanged: boolean;
const OPNAME = 'TReservoirChannelComparitor.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.PrepareChart;
const OPNAME = 'TReservoirChannelComparitor.PrepareChart';
var
  LIndex : integer;
begin
  try
    FReservoirZoneElevationFSLSeries.ParentChart := FReservoirChart;
    FReservoirZoneElevationFSLSeries.SeriesColor := clGreen;
    FReservoirZoneElevationFSLSeries.AreaLinesPen.Visible := False;
    ConfigureSeries(FReservoirZoneElevationFSLSeries);

    if FOperatingLevels > 0 then
    begin
      SetLength(FReservoirZoneElevationOPLSeries,
                FOperatingLevels);
      for LIndex := 0 to FOperatingLevels - 1 do
      begin
        FReservoirZoneElevationOPLSeries[LIndex]             := TAreaSeries.Create(FReservoirChart);
        FReservoirZoneElevationOPLSeries[LIndex].ParentChart := FReservoirChart;
        if (LIndex < Length(C_OPLevelColors)) then
          FReservoirZoneElevationOPLSeries[LIndex].SeriesColor := C_OPLevelColors[LIndex];
        FReservoirZoneElevationOPLSeries[LIndex].AreaLinesPen.Visible := False;
        ConfigureSeries(FReservoirZoneElevationOPLSeries[LIndex]);
      end;
    end;

    FReservoirZoneElevationDSLSeries.ParentChart := FReservoirChart;
    FReservoirZoneElevationDSLSeries.SeriesColor := clFuchsia;
    FReservoirZoneElevationDSLSeries.AreaLinesPen.Visible := False;
    ConfigureSeries(FReservoirZoneElevationDSLSeries);

    FReservoirZoneElevationBORSeries.ParentChart := FReservoirChart;
    FReservoirZoneElevationBORSeries.SeriesColor := clWhite;
    FReservoirZoneElevationBORSeries.AreaLinesPen.Visible := False;
    ConfigureSeries(FReservoirZoneElevationBORSeries);

    FReservoirZoneElevationTimeSeries.ParentChart := FReservoirChart;
    FReservoirZoneElevationTimeSeries.SeriesColor := clSkyBlue;
    FReservoirZoneElevationTimeSeries.AreaLinesPen.Visible := False;
    ConfigureSeries(FReservoirZoneElevationTimeSeries);

    FColorBands.ParentChart          := FChannelChart;
    FColorBands.AreaLinesPen.Visible := False;
    FColorBands.ColorEachPoint       := True;
    FColorBands.Pointer.Style        := psRectangle;
    FColorBands.Pointer.VertSize     := VCL.Controls.TControl(FChannelChart).ClientHeight;
    FColorBands.Pointer.HorizSize    := 2;
    FColorBands.Pointer.Brush.Style  := bsSolid;
    FColorBands.Pointer.Pen.Style    := psClear;
    FColorBands.XValues.DateTime     := True;

    FReservoirFullStorageVolumeSeries.ParentChart      := FReservoirChart;
    FReservoirFullStorageVolumeSeries.SeriesColor      := clBlack;
    FReservoirFullStorageVolumeSeries.LinePen.Style    := psDash;
    FReservoirFullStorageVolumeSeries.LinePen.Width    := 1;
    FReservoirFullStorageVolumeSeries.XValues.DateTime := True;
    FReservoirFullStorageVolumeSeries.XValues.Order    := loNone;
    FReservoirFullStorageVolumeSeries.Pointer.Visible  := False;
    FReservoirFullStorageVolumeSeries.Marks.Visible    := True;
    FReservoirFullStorageVolumeSeries.Marks.Clip       := True;
    FReservoirFullStorageVolumeSeries.OnGetMarkText    := OnGetLineSeriesMarkText;

    if (FOperatingLevels > 0) then
    begin
      SetLength(FReservoirOperatingLevelSeries,   FOperatingLevels);
      for LIndex := 0 to FOperatingLevels - 1 do
      begin
        FReservoirOperatingLevelSeries[LIndex]                  := TLineSeries.Create(FReservoirChart);
        FReservoirOperatingLevelSeries[LIndex].ParentChart      := FReservoirChart;
        FReservoirOperatingLevelSeries[LIndex].SeriesColor      := clBlack;
        FReservoirOperatingLevelSeries[LIndex].LinePen.Style    := psDash;
        FReservoirOperatingLevelSeries[LIndex].LinePen.Width    := 1;
        FReservoirOperatingLevelSeries[LIndex].Tag              := LIndex + 1;
        FReservoirOperatingLevelSeries[LIndex].OnGetMarkText    := OnGetLineSeriesMarkText;
        FReservoirOperatingLevelSeries[LIndex].XValues.DateTime := True;
        FReservoirOperatingLevelSeries[LIndex].XValues.Order    := loNone;
        FReservoirOperatingLevelSeries[LIndex].Pointer.Visible  := False;
        FReservoirOperatingLevelSeries[LIndex].Marks.Visible    := True;
        FReservoirOperatingLevelSeries[LIndex].Marks.Clip       := True;
      end;
    end;

    FReservoirDeadStorageVolumeSeries.ParentChart      := FReservoirChart;
    FReservoirDeadStorageVolumeSeries.SeriesColor      := clBlack;
    FReservoirDeadStorageVolumeSeries.LinePen.Style    := psSolid;
    FReservoirDeadStorageVolumeSeries.LinePen.Width    := 1;
    FReservoirDeadStorageVolumeSeries.XValues.DateTime := True;
    FReservoirDeadStorageVolumeSeries.XValues.Order    := loNone;
    FReservoirDeadStorageVolumeSeries.Pointer.Visible  := False;
    FReservoirDeadStorageVolumeSeries.Marks.Visible    := True;
    FReservoirDeadStorageVolumeSeries.Marks.Clip       := True;
    FReservoirDeadStorageVolumeSeries.OnGetMarkText := OnGetLineSeriesMarkText;

    if FChannelCount > 0 then
    begin
      SetLength(FChannelFlowTimeSeries, FChannelCount);
      SetLength(FChannelFlowCapabilitySeries, FChannelCount);
      for LIndex := 0 to FChannelCount - 1 do
      begin
        FChannelFlowTimeSeries[LIndex]                        := TLineSeries.Create(FChannelChart);
        FChannelFlowTimeSeries[LIndex].ParentChart            := FChannelChart;
        FChannelFlowTimeSeries[LIndex].XValues.DateTime       := True;
        FChannelFlowTimeSeries[LIndex].LinePen.Style          := psSolid;
        FChannelFlowTimeSeries[LIndex].LinePen.Width          := 2;

        FChannelFlowCapabilitySeries[LIndex]                  := TLineSeries.Create(FChannelChart);
        FChannelFlowCapabilitySeries[LIndex].ParentChart      := FChannelChart;
        FChannelFlowCapabilitySeries[LIndex].SeriesColor      := FChannelFlowTimeSeries[LIndex].SeriesColor;
        FChannelFlowCapabilitySeries[LIndex].XValues.DateTime := True;
        FChannelFlowCapabilitySeries[LIndex].LinePen.Style    := psDash;
        FChannelFlowCapabilitySeries[LIndex].LinePen.Width    := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.RefreshChartAxisDisplay(AChart: TFieldChart);
const OPNAME = 'TReservoirChannelComparitor.RefreshChartAxisDisplay';
var
  LDiff,
  LMin,
  LMax : double;
begin
  try
    AChart.LeftAxis.Automatic := True;
    AChart.LeftAxis.CalcMinMax(LMin, LMax);
    LDiff := (LMax - LMin);
    AChart.LeftAxis.Automatic := False;
    if (LMin > AChart.LeftAxis.Maximum) then
    begin
      AChart.LeftAxis.Maximum := LMax + (LDiff * 0.2);
      AChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
    end
    else
    if (LMax < AChart.LeftAxis.Minimum) then
    begin
      AChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
      AChart.LeftAxis.Maximum := LMax + (LDiff * 0.2);
    end
    else
    begin
      AChart.LeftAxis.Maximum := LMax + (LDiff * 0.2);
      AChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
    end;;
    AChart.LeftAxis.Increment := (LDiff * 0.1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TReservoirChannelComparitor.OnGetLineSeriesMarkText';
var
  LDisplayStr: string;
  LXValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    If not Self.Visible then Exit;
    if (ValueIndex <> 0) then Exit;

    if not (Assigned(FReservoirChart) and Assigned(FChannelChart))  then
      Exit;
    LDisplayStr := '';
    if Sender = FReservoirFullStorageVolumeSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.Supply' + FDisplayString)
    else
    if Sender = FReservoirDeadStorageVolumeSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.Storage' + FDisplayString)
    else
      LDisplayStr := Format('Operating Level %d', [Sender.Tag]);

    if (LDisplayStr <> '') then
    begin
      LXValue  := Sender.XValues.MaxValue - Sender.XValues.MinValue;
      LXValue  := LXValue/2.5;
      LYPos   := Sender.CalcYPosValue(Sender.YValue[ValueIndex] + 1.5);
      //LYPos   := Sender.CalcYPosValue(Sender.XValue[ValueIndex]) - 80;
      LXPos   := Sender.CalcXPosValue(LXValue);
      FReservoirChart.Canvas.Font.Style := [fsBold];
      if Sender = FReservoirFullStorageVolumeSeries then
        FReservoirChart.BottomAxis.DrawAxisLabel(LXPos+150,LYPos, 0,LDisplayStr)
      else
        FReservoirChart.BottomAxis.DrawAxisLabel(LXPos,LYPos, 0,LDisplayStr)

    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.DoExport(AFileName: string = '');
const OPNAME = 'TReservoirChannelComparitor.DoExport';
begin
  try
    if Assigned(FReservoirChart) and
       Assigned(FChannelChart) then
    begin
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FReservoirChart));
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FChannelChart));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.DoPrint;
const OPNAME = 'TReservoirChannelComparitor.DoPrint';
begin
  try
    if Assigned(FReservoirChart) and
       Assigned(FChannelChart) then
    begin
      FReservoirChart.DoPrint;
      FChannelChart.DoPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirChannelComparitor.OnScrollPerformed(Sender: TObject);
const OPNAME = 'TReservoirChannelComparitor.OnScrollPerformed';
begin
  try
    if(Sender = FReservoirChart) then
    begin
      FChannelChart.BottomAxis.Automatic := False;
      FChannelChart.BottomAxis.Minimum :=   FReservoirChart.BottomAxis.Minimum;
      FChannelChart.BottomAxis.Maximum :=   FReservoirChart.BottomAxis.Maximum;
    end
    else if(Sender = FChannelChart) then
    begin
      FReservoirChart.BottomAxis.Automatic := False;
      FReservoirChart.BottomAxis.Minimum := FChannelChart.BottomAxis.Minimum;
      FReservoirChart.BottomAxis.Maximum := FChannelChart.BottomAxis.Maximum;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.OnZoomPerformed(Sender: TObject);
const OPNAME = 'TReservoirChannelComparitor.OnZoomPerformed';
begin
  try
    if(Sender = FReservoirChart) then
    begin
      FChannelChart.BottomAxis.Automatic := False;
      FChannelChart.BottomAxis.Minimum :=   FReservoirChart.BottomAxis.Minimum;
      FChannelChart.BottomAxis.Maximum :=   FReservoirChart.BottomAxis.Maximum;
    end
    else if(Sender = FChannelChart) then
    begin
      FReservoirChart.BottomAxis.Automatic := False;
      FReservoirChart.BottomAxis.Minimum := FChannelChart.BottomAxis.Minimum;
      FReservoirChart.BottomAxis.Maximum := FChannelChart.BottomAxis.Maximum;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirChannelComparitor.OnUndoZoomPerformed(Sender: TObject);
const OPNAME = 'TReservoirChannelComparitor.OnUndoZoomPerformed';
begin
  try
    FReservoirChart.BottomAxis.Automatic := True;
    FChannelChart.BottomAxis.Automatic := True;
    FReservoirChart.BottomAxis.AdjustMaxMin;
    FChannelChart.BottomAxis.AdjustMaxMin;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TReservoirStorageVolumeComparitor }

procedure TReservoirStorageVolumeComparitor.ClearChart;
const OPNAME = 'TReservoirStorageVolumeComparitor.ClearChart';
var
  LIndex : integer;
begin
  try
    FReservoirStorageVolumeSimSeries.Active  := False;
    FReservoirDeadStorageVolumeSeries.Active := False;
    FReservoirFullStorageVolumeSeries.Active := False;
    FReservoirStorageVolumeActSeries.Active  := False;

    FReservoirStorageVolumeSimSeries.Clear;
    FReservoirFullStorageVolumeSeries.Clear;
    FReservoirDeadStorageVolumeSeries.Clear;
    FReservoirStorageVolumeActSeries.Clear;

    for LIndex := low(FZoneElevationSeries) to high(FZoneElevationSeries) do
    begin
      FZoneElevationSeries[LIndex].Active := False;
      FZoneElevationSeries[LIndex].Clear;
      FZoneElevationSeries[LIndex].ParentChart := nil;
    end;

    FReservoirChart.LeftAxis.Title.Caption := '';
    FReservoirChart.BottomAxis.Title.Caption := '';
    FReservoirChart.Title.Text.Clear;

    FReservoirChart.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.ConfigureSeries;
const OPNAME = 'TReservoirStorageVolumeComparitor.ConfigureSeries';
begin
  try
    FReservoirStorageVolumeSimSeries.SeriesColor  := clSkyBlue;
    FReservoirStorageVolumeActSeries.SeriesColor  := clRed;
    FReservoirFullStorageVolumeSeries.SeriesColor := clBlack;
    FReservoirDeadStorageVolumeSeries.SeriesColor := clBlack;

    FReservoirStorageVolumeSimSeries.XValues.DateTime  := True;
    FReservoirStorageVolumeActSeries.XValues.DateTime  := True;
    FReservoirFullStorageVolumeSeries.XValues.DateTime := True;
    FReservoirDeadStorageVolumeSeries.XValues.DateTime := True;

    FReservoirStorageVolumeSimSeries.DrawArea           := True;
    FReservoirStorageVolumeSimSeries.AreaLinesPen.Visible := False;
    FReservoirStorageVolumeSimSeries.Pointer.Visible    := False;
    FReservoirStorageVolumeSimSeries.MultiArea          := maNone;
    FReservoirStorageVolumeSimSeries.AreaLinesPen.Width := 1;

    FReservoirStorageVolumeActSeries.LinePen.Style    := psDash;
    FReservoirStorageVolumeActSeries.LinePen.Width    := 1;

    FReservoirFullStorageVolumeSeries.LinePen.Style   := psDash;
    FReservoirFullStorageVolumeSeries.LinePen.Width   := 1;
    FReservoirFullStorageVolumeSeries.XValues.Order   := loNone;
    FReservoirFullStorageVolumeSeries.Pointer.Visible := False;
    FReservoirFullStorageVolumeSeries.Marks.Visible   := True;
    FReservoirFullStorageVolumeSeries.Marks.Clip      := True;
    FReservoirFullStorageVolumeSeries.OnGetMarkText   := OnGetLineSeriesMarkText;

    FReservoirDeadStorageVolumeSeries.LinePen.Style   := psDash;
    FReservoirDeadStorageVolumeSeries.LinePen.Width   := 1;
    FReservoirDeadStorageVolumeSeries.XValues.Order   := loNone;
    FReservoirDeadStorageVolumeSeries.Pointer.Visible := False;
    FReservoirDeadStorageVolumeSeries.Marks.Visible   := True;
    FReservoirDeadStorageVolumeSeries.Marks.Clip      := True;

    FReservoirDeadStorageVolumeSeries.OnGetMarkText := OnGetLineSeriesMarkText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.CreateMemberObjects;
const OPNAME = 'TReservoirStorageVolumeComparitor.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FReservoirChart                   := TFieldChart.Create(Self, FAppModules);
    FReservoirChart.Parent            := Self;

    FReservoirStorageVolumeSimSeries  := TAreaSeries.Create(FReservoirChart);
    FReservoirStorageVolumeActSeries  := TLineSeries.Create(FReservoirChart);
    FReservoirFullStorageVolumeSeries := TLineSeries.Create(FReservoirChart);
    FReservoirDeadStorageVolumeSeries := TLineSeries.Create(FReservoirChart);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.CreateZoneSeries (ACount : integer);
const OPNAME = 'TReservoirStorageVolumeComparitor.CreateZoneSeries';
var
  LIndex      : integer;
  lLineSeries : TLineSeries;
begin
  try
    SetLength(FZoneElevationSeries, ACount);
    for LIndex := 0 to ACount - 1 do
    begin
      lLineSeries := TLineSeries.Create(FReservoirChart);
      FZoneElevationSeries[LIndex] := lLineSeries;
      lLineSeries.ParentChart := FReservoirChart;
      FReservoirChart.AddSeries(lLineSeries);
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.DestroyMemberObjects;
const OPNAME = 'TReservoirStorageVolumeComparitor.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirStorageVolumeComparitor.Initialise: boolean;
const OPNAME = 'TReservoirStorageVolumeComparitor.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FReservoirChart.Legend.Visible := False;
    FReservoirChart.View3D         := False;
    //FReservoirChart.View3DWalls    := False;
    FReservoirChart.LeftAxis.AxisValuesFormat := '###,###,##0.00';
    FReservoirChart.BottomAxis.AxisValuesFormat := 'dd/mm/yyyy';
    FReservoirChart.BottomAxis.LabelsAngle := 90;

    ConfigureSeries;

    for LIndex := low(FZoneElevationSeries) to high(FZoneElevationSeries) do
    begin
      ConfigureZonesSeries(FZoneElevationSeries[LIndex]);
      FReservoirChart.AddSeries(FZoneElevationSeries[LIndex]);
    end;

    FReservoirChart.AddSeries(FReservoirStorageVolumeSimSeries);
    FReservoirChart.AddSeries(FReservoirFullStorageVolumeSeries);
    FReservoirChart.AddSeries(FReservoirDeadStorageVolumeSeries);
    FReservoirChart.AddSeries(FReservoirStorageVolumeActSeries);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirStorageVolumeComparitor.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirStorageVolumeComparitor.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.RefreshChartAxisDisplay(AMax : double);
const OPNAME = 'TReservoirStorageVolumeComparitor.RefreshChartAxisDisplay';
var
  LDiff,
  LMin,
  LMax : double;
begin
  try
    FReservoirChart.LeftAxis.Automatic := True;
    FReservoirChart.LeftAxis.CalcMinMax(LMin, LMax);
    LDiff := (AMax - LMin);
    FReservoirChart.LeftAxis.Automatic := False;
    if (LMin > FReservoirChart.LeftAxis.Maximum) then
    begin
      FReservoirChart.LeftAxis.Maximum := AMax + (LDiff * 0.2);
      FReservoirChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
    end
    else
    if (AMax < FReservoirChart.LeftAxis.Minimum) then
    begin
      FReservoirChart.LeftAxis.Minimum := LMin - (LDiff * 0.2);
      FReservoirChart.LeftAxis.Maximum := AMax + (LDiff * 0.2);
    end;
    FReservoirChart.LeftAxis.Increment := (LDiff * 0.1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.Resize;
const OPNAME = 'TReservoirStorageVolumeComparitor.Resize';
begin
  inherited Resize;
  try
    FReservoirChart.Align  := alNone;
    FReservoirChart.Align  := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirStorageVolumeComparitor.StudyHasChanged: boolean;
const OPNAME = 'TReservoirStorageVolumeComparitor.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.OnGetLineSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TReservoirStorageVolumeComparitor.OnGetLineSeriesMarkText';
var
  LDisplayStr: string;
  LXValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    If not Self.Visible then Exit;
    if (ValueIndex <> 0) then Exit;

    if not (Assigned(FReservoirChart)) then
      Exit;
    LDisplayStr := '';
    if Sender = FReservoirFullStorageVolumeSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.Supply' + FDisplayString)
    else
    if Sender = FReservoirDeadStorageVolumeSeries then
      LDisplayStr := FAppModules.Language.GetString('ComplienceString.Storage' + FDisplayString);


    if (LDisplayStr <> '') then
    begin
      LXValue  := Sender.XValues.MaxValue - Sender.XValues.MinValue;
      LXValue  := LXValue/2.5;
      LYPos   := Sender.CalcYPosValue(Sender.YValue[ValueIndex] + 1.5);
      //LYPos   := Sender.CalcYPosValue(Sender.XValue[ValueIndex]) - 80;
      LXPos   := Sender.CalcXPosValue(LXValue);
      FReservoirChart.Canvas.Font.Style := [fsBold];
      if Sender = FReservoirFullStorageVolumeSeries then
        FReservoirChart.BottomAxis.DrawAxisLabel(LXPos+150,LYPos, 0,LDisplayStr)
      else
        FReservoirChart.BottomAxis.DrawAxisLabel(LXPos,LYPos, 0,LDisplayStr);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.DoExport(AFileName: string = '');
const OPNAME = 'TReservoirStorageVolumeComparitor.DoExport';
begin
  try
    FReservoirChart.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.DoPrint;
const OPNAME = 'TReservoirStorageVolumeComparitor.DoPrint';
begin
  try
    FReservoirChart.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirStorageVolumeComparitor.ConfigureZonesSeries(ALineSeries: TLineSeries);
const OPNAME = 'TReservoirStorageVolumeComparitor.ConfigureZonesSeries';
begin
  try
//    ALineSeries.ParentChart := FReservoirChart;
//    FReservoirChart.AddSeries(ALineSeries);

    ALineSeries.Active           := True;
    ALineSeries.SeriesColor      := clRed;
    ALineSeries.LinePen.Width    := 1;
    ALineSeries.LinePen.Style    := psSolid;
    ALineSeries.ShowInLegend     := True;
    ALineSeries.XValues.DateTime := True;
    ALineSeries.Pointer.Visible  := True;
    ALineSeries.Marks.Visible    := True;
    ALineSeries.Marks.Clip       := True;
    ALineSeries.OnGetMarkText    := OnGetLineSeriesMarkText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirStorageVolumeComparitor.GetZoneElevationSeriesByIndex(AIndex: integer): TLineSeries;
const OPNAME = 'TReservoirStorageVolumeComparitor.GetZoneElevationSeriesByIndex';
begin
  Result := nil;
  try
    Result := FZoneElevationSeries[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIFRChannelComplienceGraph }

procedure TIFRChannelComplienceGraph.CreateMemberObjects;
const OPNAME = 'TIFRChannelComplienceGraph.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FRequirementFlowValuesChart                  := TFieldChart.Create(Self, FAppModules);
    FRequirementFlowValuesChart.Parent           := Self;
    FRequirementFlowValuesChart.Title.Font.Style := [fsBold];

    FLineSeriesOne                     := TLineSeries.Create(FRequirementFlowValuesChart);
    FLineSeriesOne.ParentChart         := FRequirementFlowValuesChart;

    FPointSeriesOne                    := TPointSeries.Create(FRequirementFlowValuesChart);
    FPointSeriesOne.ParentChart        := FRequirementFlowValuesChart;

    FLineSeriesTwo                     := TLineSeries.Create(FRequirementFlowValuesChart);
    FLineSeriesTwo.ParentChart         := FRequirementFlowValuesChart;

    FPointSeriesTwo                    := TPointSeries.Create(FRequirementFlowValuesChart);
    FPointSeriesTwo.ParentChart        := FRequirementFlowValuesChart;

    FRequiredBarSeries                         := TBarSeries.Create(FRequirementFlowValuesChart);
    FRequiredBarSeries.ParentChart             := FRequirementFlowValuesChart;
//    FRequiredBarSeries.Marks.Visible           := False;
    FRequiredBarSeries.SeriesColor             := clBlue;
    FRequiredBarSeries.BarWidthPercent         := 50;

    FActualBarSeries                         := TBarSeries.Create(FRequirementFlowValuesChart);
    FActualBarSeries.ParentChart             := FRequirementFlowValuesChart;
//    FActualBarSeries.Marks.Visible           := False;
    FActualBarSeries.SeriesColor             := clRed;
    FActualBarSeries.BarWidthPercent         := 50;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.DestroyMemberObjects;
const OPNAME = 'TIFRChannelComplienceGraph.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.DoExport(AFileName: string);
const OPNAME = 'TIFRChannelComplienceGraph.DoExport';
begin
  try
    FRequirementFlowValuesChart.DoExport(AFileName);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.DoPrint;
const OPNAME = 'TIFRChannelComplienceGraph.DoPrint';
begin
  try
    FRequirementFlowValuesChart.DoPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRChannelComplienceGraph.Initialise: boolean;
const OPNAME = 'TIFRChannelComplienceGraph.Initialise';
begin
  Result := False;
  try
    FRequirementFlowValuesChart.Legend.Visible            := False;
    FRequirementFlowValuesChart.View3D                    := False;
//    FRequirementFlowValuesChart.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';
//    FRequirementFlowValuesChart.BottomAxis.LabelsAngle    := 90;
    FRequirementFlowValuesChart.LeftAxis.AxisValuesFormat := '###,###,##0.00';
    FRequirementFlowValuesChart.BottomAxis.AxisValuesFormat := '##0';


    FRequirementFlowValuesChart.LeftAxis.TitleSize        := 1;

    FRequiredBarSeries.Clear;
    FRequiredBarSeries.MultiBar                := mbSide;
//    FRequiredBarSeries.OffsetPercent           := 100;
    FRequiredBarSeries.SideMargins             := True;
    FRequiredBarSeries.Marks.Visible           := True;
    FRequiredBarSeries.UseYOrigin              := True;

    FActualBarSeries.Clear;
    FActualBarSeries.MultiBar                := mbSide;
//    FActualBarSeries.OffsetPercent           := 100;
    FActualBarSeries.SideMargins             := True;
    FActualBarSeries.Marks.Visible           := True;
    FRequiredBarSeries.UseYOrigin            := True;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRChannelComplienceGraph.LanguageHasChanged: boolean;
const OPNAME = 'TIFRChannelComplienceGraph.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.Resize;
const OPNAME = 'TIFRChannelComplienceGraph.Resize';
begin
  try
    FRequirementFlowValuesChart.Align := alClient;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.ClearChart;
const OPNAME = 'TIFRChannelComplienceGraph.ClearChart';
var
  LIndex : integer;
begin
  try
    FLineSeriesOne.Active              := False;
    FLineSeriesOne.Clear;
    FLineSeriesTwo.Active              := False;
    FLineSeriesTwo.Clear;

    FPointSeriesOne.Active             := False;
    FPointSeriesOne.Clear;
    FPointSeriesTwo.Active             := False;
    FPointSeriesTwo.Clear;
    FActualBarSeries.Active            := False;
    FActualBarSeries.Clear;

    FRequiredBarSeries.Active          := False;
    FRequiredBarSeries.Clear;

    FLineSeriesOne.ParentChart              := nil;
    FLineSeriesTwo.ParentChart              := nil;
    FPointSeriesOne.ParentChart             := nil;
    FPointSeriesOne.ParentChart             := nil;
    FActualBarSeries.ParentChart            := nil;
    FRequiredBarSeries.ParentChart          := nil;

    for LIndex := 0 to 11 do
    begin
      if (Length(FLineSeriesArrayOne) > 0) AND
         (Length(FLineSeriesArrayTwo) > 0) AND
         (Length(FPointSeriesArrayOne) > 0) AND
         (Length(FPointSeriesArrayTwo) > 0) then
      begin
        FLineSeriesArrayOne[LIndex].Active                := False;
        FLineSeriesArrayOne[LIndex].Clear;

        FPointSeriesArrayOne[LIndex].Active               := False;
        FPointSeriesArrayOne[LIndex].Clear;

        FLineSeriesArrayTwo[LIndex].Active                := False;
        FLineSeriesArrayTwo[LIndex].Clear;

        FPointSeriesArrayOne[LIndex].Active                := False;
        FPointSeriesArrayOne[LIndex].Clear;

        FPointSeriesArrayTwo[LIndex].Active                := False;
        FPointSeriesArrayTwo[LIndex].Clear;

        FLineSeriesArrayOne[LIndex].ParentChart           := nil;
        FLineSeriesArrayOne[LIndex]                       := nil;
        FLineSeriesArrayTwo[LIndex].ParentChart           := nil;
        FLineSeriesArrayTwo[LIndex]                       := nil;

        FPointSeriesArrayOne[LIndex].ParentChart          := nil;
        FPointSeriesArrayOne[LIndex]                      := nil;
        FPointSeriesArrayTwo[LIndex].ParentChart          := nil;
        FPointSeriesArrayTwo[LIndex]                      := nil;
      end;
    end;
    SetLength(FLineSeriesArrayOne,0);
    SetLength(FPointSeriesArrayOne,0);

    SetLength(FLineSeriesArrayTwo,0);
    SetLength(FPointSeriesArrayTwo,0);

    FRequirementFlowValuesChart.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TIFRChannelComplienceGraph.ConfigureLineSeries(ALineSeries: TLineSeries; AColor : TColor);
const OPNAME = 'TIFRChannelComplienceGraph.ConfigureLineSeries';
begin
  try
    ALineSeries.SeriesColor      := AColor;
    ALineSeries.LinePen.Width    := 1;
    ALineSeries.LinePen.Style    := psDot;
    ALineSeries.ShowInLegend     := False;
    ALineSeries.XValues.DateTime := True;
    ALineSeries.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRChannelComplienceGraph.ConfigurePointSeries(APointSeries: TPointSeries; AColor : TColor);
const OPNAME = 'TIFRChannelComplienceGraph.ConfigurePointSeries';
begin
  try
    APointSeries.SeriesColor := AColor;
    APointSeries.Marks.Visible := False;
    APointSeries.LinePen.Width := 1;
    APointSeries.Pointer.Size := 1;
    APointSeries.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.PrepareChart;
const OPNAME = 'TIFRChannelComplienceGraph.PrepareChart';
var
  LIndex : integer;
begin
  try
    SetLength(FLineSeriesArrayOne,MaxMonths);
    SetLength(FPointSeriesArrayOne,MaxMonths);
    SetLength(FLineSeriesArrayTwo,MaxMonths);
    SetLength(FPointSeriesArrayTwo,MaxMonths);

    for LIndex := 0 to 11 do
    begin
      FLineSeriesArrayOne[LIndex]                            := TLineSeries.Create(FRequirementFlowValuesChart);
      FLineSeriesArrayOne[LIndex].ParentChart                := FRequirementFlowValuesChart;
      FLineSeriesArrayOne[LIndex].Tag                        := LIndex + 1;
      FLineSeriesArrayOne[LIndex].Pen.Width                  := 1;

      FLineSeriesArrayTwo[LIndex]                            := TLineSeries.Create(FRequirementFlowValuesChart);
      FLineSeriesArrayTwo[LIndex].ParentChart                := FRequirementFlowValuesChart;
      FLineSeriesArrayTwo[LIndex].Tag                        := LIndex + 1;
      FLineSeriesArrayTwo[LIndex].Pen.Width                  := 1;

      FPointSeriesArrayOne[LIndex]                           := TPointSeries.Create(FRequirementFlowValuesChart);
      FPointSeriesArrayOne[LIndex].ParentChart               := FRequirementFlowValuesChart;
      FPointSeriesArrayOne[LIndex].Tag                       := LIndex + 1;
      FPointSeriesArrayOne[Lindex].Pointer.VertSize          := 3;
      FPointSeriesArrayOne[Lindex].Pointer.HorizSize         := 3;

      FPointSeriesArrayTwo[LIndex]                            := TPointSeries.Create(FRequirementFlowValuesChart);
      FPointSeriesArrayTwo[LIndex].ParentChart                := FRequirementFlowValuesChart;
      FPointSeriesArrayTwo[LIndex].Tag                        := LIndex + 1;
      FPointSeriesArrayTwo[Lindex].Pointer.VertSize           := 3;
      FPointSeriesArrayTwo[Lindex].Pointer.HorizSize          := 3;
      
    end;

    FLineSeriesOne.ParentChart                       := FRequirementFlowValuesChart;
    FLineSeriesOne.AreaLinesPen.Visible              := False;

    FPointSeriesOne.ParentChart                      := FRequirementFlowValuesChart;
    FPointSeriesOne.AreaLinesPen.Visible             := False;
    FPointSeriesOne.Pointer.VertSize                 := 3;
    FPointSeriesOne.Pointer.HorizSize                := 3;

    FLineSeriesTwo.ParentChart                       := FRequirementFlowValuesChart;
    FLineSeriesTwo.AreaLinesPen.Visible              := False;

    FLineSeriesTwo.ParentChart                       := FRequirementFlowValuesChart;
    FLineSeriesTwo.AreaLinesPen.Visible              := False;
    FLineSeriesTwo.Pointer.VertSize                  := 3;
    FLineSeriesTwo.Pointer.HorizSize                 := 3;

    FActualBarSeries.ParentChart                           := FRequirementFlowValuesChart;
    FActualBarSeries.Marks.Visible                         := False;

    FRequiredBarSeries.ParentChart                           := FRequirementFlowValuesChart;
    FRequiredBarSeries.Marks.Visible                         := False;

    //FBarSeries.SeriesColor                           := clBlue;
    //FBarSeries.BarWidthPercent                       := 100;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRChannelComplienceGraph.StudyHasChanged: boolean;
const OPNAME = 'TIFRChannelComplienceGraph.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRChannelComplienceGraph.AssignHelpContext;
const OPNAME = 'TIFRChannelComplienceGraph.AssignHelpContext';
begin
  inherited AssignHelpContext;
  try
    SetControlHelpContext(Self,                        HC_ResultOutput);
    SetControlHelpContext(VCL.Controls.TControl(FRequirementFlowValuesChart), HC_ResultOutput);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
