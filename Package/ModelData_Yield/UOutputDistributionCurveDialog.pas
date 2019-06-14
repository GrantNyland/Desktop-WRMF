//
//
//  UNIT      : Contains the class TOutputDistributionCurveDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputDistributionCurveDialog;

interface

uses
  Classes,
  Windows,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeCanvas,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCLTee.Series,
  VCL.Graphics,
//  VCLTee.TeEngine,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

const
  C_PointStyle: array [0..11]of TSeriesPointerStyle = (psCircle,psRectangle,psTriangle,psDiagCross
                                                 ,psStar,psCircle,psCross,psRectangle,psDiagCross
                                                 ,psDiamond,psRectangle,psDownTriangle);
  C_Colors    : array [0..11]of TColor              = (clMaroon,clFuchsia,clYellow,clLime,clPurple,clBlue
                                                     ,clSkyBlue,clOlive,clNavy,clGray,clRed,clGreen);

type
  TAssuranceForm = class(TAbstractForm)
  protected
    FBttomPanel : TPanel;
    FBtnOk : TBitBtn;
    FCansel : TBitBtn;
    procedure CreateMemberObjects;override;
    procedure Resize; override;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: Boolean; override;
end;


type
  TIFRLineSeriesArray   = array of TLineSeries;
  TIFRPointSeriesArray  = array of TPointSeries;

  TIFRCurve = class(TAbstractPanel)
  protected
    FIFRCurveChart               : TFieldChart;

    FSuppliedIFRLineSeries       : TLineSeries;
    FRequiredIFRLineSeries       : TLineSeries;
    FDefinedIFRLineSeries        : TLineSeries;
    FReferenceFlowLineSeries     : TLineSeries;
    FReleaseVsInflowLineSeries   : TLineSeries;

    FSuppliedIFRPointSeries      : TPointSeries;
    FRequiredIFRPointSeries      : TPointSeries;
    FDefinedIFRPointSeries       : TPointSeries;
    FReferenceFlowPointSeries    : TPointSeries;
    FReleaseVsInflowPointSeries  : TPointSeries;

    FDefinedLineSeriesArray      : TIFRLineSeriesArray;
    FDefinedPointSeriesArray     : TIFRPointSeriesArray;


    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

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
    property IFRCurveChart                : TFieldChart            read FIFRCurveChart;
    property SuppliedIFRLineSeries        : TLineSeries            read FSuppliedIFRLineSeries;
    property RequiredIFRLineSeries        : TLineSeries            read FRequiredIFRLineSeries;
    property DefinedIFRLineSeries         : TLineSeries            read FDefinedIFRLineSeries;
    property ReferenceFlowLineSeries      : TLineSeries            read FReferenceFlowLineSeries;
    property ReleaseVsInflowLineSeries    : TLineSeries            read FReleaseVsInflowLineSeries;

    property SuppliedIFRPointSeries       : TPointSeries           read FSuppliedIFRPointSeries;
    property RequiredIFRPointSeries       : TPointSeries           read FRequiredIFRPointSeries;
    property DefinedIFRPointSeries        : TPointSeries           read FDefinedIFRPointSeries;
    property ReferenceFlowPointSeries     : TPointSeries           read FReferenceFlowPointSeries;
    property ReleaseVsInflowPointSeries   : TPointSeries           read FReleaseVsInflowPointSeries;

    property DefinedLineSeriesArray       : TIFRLineSeriesArray    read FDefinedLineSeriesArray;
    property DefinedPointSeriesArray      : TIFRPointSeriesArray   read FDefinedPointSeriesArray;

  end;

  TOutputDistributionCurveDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel        : TPanel;
    FViewDataLabel        : TLabel;
    FViewDataType         : TFieldComboBox;
    FCurvesDescrLabel     : TLabel;
    FDataChart            : TFieldChart;
    FDemandLineSeries,
    FLineSeries           : TLineSeries;
    FBtnDataSelection     : TFieldButton;
    FBtnRISelector        : TFieldButton;
    FBtnCompliance        : TFieldButton;
    FIFRCurves            : TIFRCurve;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property Chart                : TFieldChart         read FDataChart;
    property LineSeries           : TLineSeries         read FLineSeries;
    property DemandLineSeries     : TLineSeries         read FDemandLineSeries;
    property cmbViewDataType      : TFieldComboBox      read FViewDataType;
    property BtnDataSelection     : TFieldButton        read FBtnDataSelection;
    property BtnRISelector        : TFieldButton        read FBtnRISelector;
    property BtnCompliance        : TFieldButton        read FBtnCompliance;
    property ViewDataType         : TFieldComboBox      read FViewDataType;
    property ViewDataLabel        : TLabel              read FViewDataLabel;
    property CurvesDescrLabel     : TLabel              read FCurvesDescrLabel;
    property IFRCurves            : TIFRCurve           read FIFRCurves;
  end;

  implementation

uses
  SysUtils,
  VCLTee.TeExport,
  VCLTee.TeeProcs,
  UHelpContexts,
  UConstants,
  UControlCreationUtilities,
  UErrorHandlingOperations;



{ TOutputDistributionCurveDialog }

procedure TOutputDistributionCurveDialog.CreateMemberObjects;
const OPNAME = 'TOutputDistributionCurveDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel                := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent         := ControlsParent;

    FViewDataLabel                := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent         := FSelectorPanel;
    FViewDataLabel.Alignment      := taCenter;
    FViewDataLabel.AutoSize       := False;

    FViewDataType                 := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent          := FSelectorPanel;

    FCurvesDescrLabel             := TLabel.Create(ControlsOwner);
    FCurvesDescrLabel.Parent      := FSelectorPanel;
//    FCurvesDescrLabel.Alignment  := taCenter;
    FCurvesDescrLabel.AutoSize    := False;

    FBtnDataSelection             := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent      := FSelectorPanel;

    FBtnRISelector                := TFieldButton.Create(ControlsOwner, FAppModules, '');
    FBtnRISelector.Parent         := FSelectorPanel;

    FBtnCompliance                := TFieldButton.Create(ControlsOwner, FAppModules, '');
    FBtnCompliance.Parent         := FSelectorPanel;

    FDataChart                    := TFieldChart.Create(ControlsOwner, FAppModules);
    FDataChart.Parent             := ControlsParent;
    FDataChart.View3D             := False;
    FDataChart.Legend.Visible     := False;
    FDataChart.LeftAxis.TitleSize := 1;

    FIFRCurves                    := TIFRCurve.Create(ControlsOwner, FAppModules);
    FIFRCurves.Parent             := ControlsParent;
    FIFRCurves.Visible            := False;

    FLineSeries                    := TLineSeries.Create(FDataChart);
    FDemandLineSeries              := TLineSeries.Create(FDataChart);

    FViewDataLabel.Visible            := False;
    FViewDataType.Visible             := False;
    FCurvesDescrLabel.Visible         := False;
    FIFRCurves.FIFRCurveChart.Visible := False;

    FDataChart.AddSeries(FLineSeries);
    FDataChart.AddSeries(FDemandLineSeries);
    FDemandLineSeries.SeriesColor      := clLime;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveDialog.Initialise: boolean;
const OPNAME = 'TOutputDistributionCurveDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataType.Clear;
    FDataChart.Initialise;
    FSelectorPanel.BorderStyle := bsSingle;
    FIFRCurves.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputDistributionCurveDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FViewDataType.StudyHasChanged;
    FDataChart.StudyHasChanged;
    FIFRCurves.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDistributionCurveDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataChart.LanguageHasChanged;
    FDataChart.LanguageHasChanged;
    FViewDataType.LanguageHasChanged;
    FIFRCurves.LanguageHasChanged;
    FViewDataLabel.Caption    := FAppModules.Language.GetString('LabelText.ViewData');
    FBtnDataSelection.Caption := FAppModules.Language.GetString('LabelText.SelectData');
    FBtnRISelector.Caption    := FAppModules.Language.GetString('ButtonCaption.RISelector');
    FBtnCompliance.Caption    := FAppModules.Language.GetString('ButtonCaption.Compliance');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveDialog.AssignHelpContext;
const OPNAME = 'TOutputDistributionCurveDialog.AssignHelpContext';
begin
  try
    FIFRCurves.Align := alClient;
    //SetControlHelpContext(Self,           HC_SupplyDistributionCurves);
    //SetControlHelpContext(FViewDataType,  HC_SupplyDistributionCurves);
   // SetControlHelpContext(FDataChart,     HC_SupplyDistributionCurves);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDistributionCurveDialog.Resize;
const OPNAME = 'TOutputDistributionCurveDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align          := alTop;
      FSelectorPanel.ClientHeight   := 30;

      FViewDataType.Align           := alLeft;
      FViewDataType.Width           := 160;

      FViewDataLabel.Align          := alLeft;
      FViewDataLabel.Width          := 60;
      FViewDataLabel.Layout         := tlCenter;

      FCurvesDescrLabel.Align       := alClient;
      FCurvesDescrLabel.Width       := 400;
      FCurvesDescrLabel.Layout      := tlCenter;
      FCurvesDescrLabel.Font.Style  := [fsBold];
      FCurvesDescrLabel.Font.Color  := clBlue;

      FBtnDataSelection.Align       := alRight;
      FBtnDataSelection.Width       := 150;

      FBtnRISelector.Align          := AlRight;
      FBtnRISelector.Width          := 150;

      FBtnCompliance.Align          := AlRight;
      FBtnCompliance.Width          := 150;

      FDataChart.Align              := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDistributionCurveDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDistributionCurveDialog.DoExport';
begin
  try
    if FIFRCurves.Visible then FIFRCurves.DoExport(AFileName);
    TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FDataChart));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveDialog.DoPrint;
const OPNAME = 'TOutputDistributionCurveDialog.DoPrint';
begin
  try
    if FIFRCurves.Visible then FIFRCurves.DoPrint;
    FDataChart.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveDialog.CanExport: boolean;
const OPNAME = 'TOutputDistributionCurveDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FIFRCurves)  and (FIFRCurves.Visible)) or FDataChart.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDistributionCurveDialog.CanPrint: boolean;
const OPNAME = 'TOutputDistributionCurveDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FIFRCurves)  and (FIFRCurves.Visible)) or FDataChart.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDistributionCurveDialog.DestroyMemberObjects;
const OPNAME = 'TOutputDistributionCurveDialog.DestroyMemberObjects';
begin
  try
    FreeAndNil(FIFRCurves);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TIFRCurve }

procedure TIFRCurve.ClearChart;
const OPNAME = 'TIFRCurve.ClearChart';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to 11 do
    begin
      if (Length(FDefinedLineSeriesArray) > 0)   AND
         (Length(FDefinedPointSeriesArray) > 0) then
      begin
        FDefinedLineSeriesArray[LIndex].Active         := False;
        FDefinedLineSeriesArray[LIndex].Clear;
        FDefinedPointSeriesArray[LIndex].Active        := False;
        FDefinedPointSeriesArray[LIndex].Clear;

        FDefinedLineSeriesArray[LIndex].ParentChart    := nil;
        FDefinedLineSeriesArray[LIndex]                := nil;
        FDefinedPointSeriesArray[LIndex].ParentChart   := nil;
        FDefinedPointSeriesArray[LIndex]               := nil;
      end;
    end;
    FSuppliedIFRLineSeries.Active             := False;
    FSuppliedIFRLineSeries.Clear;
    FRequiredIFRLineSeries.Active             := False;
    FRequiredIFRLineSeries.Clear;
    FDefinedIFRLineSeries.Active              := False;
    FDefinedIFRLineSeries.Clear;
    FReferenceFlowLineSeries.Active           := False;
    FReferenceFlowLineSeries.Clear;
    FReleaseVsInflowLineSeries.Active         := False;
    FReleaseVsInflowLineSeries.Clear;

    FSuppliedIFRPointSeries.Active            := False;
    FSuppliedIFRPointSeries.Clear;
    FRequiredIFRPointSeries.Active            := False;
    FRequiredIFRPointSeries.Clear;
    FDefinedIFRPointSeries.Active             := False;
    FDefinedIFRPointSeries.Clear;
    FReferenceFlowPointSeries.Active          := False;
    FReferenceFlowPointSeries.Clear;
    FReleaseVsInflowPointSeries.Active        := False;
    FReleaseVsInflowPointSeries.Clear;

    FSuppliedIFRLineSeries.ParentChart        := nil;
    FRequiredIFRLineSeries.ParentChart        := nil;
    FDefinedIFRLineSeries.ParentChart         := nil;
    FReferenceFlowLineSeries.ParentChart      := nil;
    FReleaseVsInflowLineSeries.ParentChart    := nil;

    FSuppliedIFRPointSeries.ParentChart       := nil;
    FRequiredIFRPointSeries.ParentChart       := nil;
    FDefinedIFRPointSeries.ParentChart        := nil;
    FReferenceFlowPointSeries.ParentChart     := nil;
    FReleaseVsInflowPointSeries.ParentChart   := nil;

    SetLength(FDefinedLineSeriesArray,0);
    SetLength(FDefinedPointSeriesArray,0);

    FIFRCurveChart.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRCurve.ConfigureLineSeries(ALineSeries: TLineSeries;AColor: TColor);
const OPNAME = 'TIFRCurve.ConfigureLineSeries';
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

procedure TIFRCurve.ConfigurePointSeries(APointSeries: TPointSeries;aColor: TColor);
const OPNAME = 'TIFRCurve.ConfigurePointSeries';
begin
  try
    APointSeries.SeriesColor     := AColor;
    APointSeries.Marks.Visible   := False;
    APointSeries.LinePen.Width   := 1;
    APointSeries.Pointer.Size    := 1;
    APointSeries.Clear;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRCurve.CreateMemberObjects;
const OPNAME = 'TIFRCurve.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIFRCurveChart                           := TFieldChart.Create(Self, FAppModules);
    FIFRCurveChart.Parent                    := Self ;
    FIFRCurveChart.Title.Font.Style          := [fsBold];

    FSuppliedIFRLineSeries                   := TLineSeries.Create(FIFRCurveChart);
    FSuppliedIFRLineSeries.ParentChart       := FIFRCurveChart;

    FRequiredIFRLineSeries                   := TLineSeries.Create(FIFRCurveChart);
    FRequiredIFRLineSeries.ParentChart       := FIFRCurveChart;

    FDefinedIFRLineSeries                    := TLineSeries.Create(FIFRCurveChart);
    FDefinedIFRLineSeries.ParentChart        := FIFRCurveChart;

    FReferenceFlowLineSeries                 := TLineSeries.Create(FIFRCurveChart);
    FReferenceFlowLineSeries.ParentChart     := FIFRCurveChart;

    FReleaseVsInflowLineSeries               := TLineSeries.Create(FIFRCurveChart);
    FReleaseVsInflowLineSeries.ParentChart   := FIFRCurveChart;

    FSuppliedIFRPointSeries                  := TPointSeries.Create(FIFRCurveChart);
    FSuppliedIFRPointSeries.ParentChart      := FIFRCurveChart;

    FRequiredIFRPointSeries                  := TPointSeries.Create(FIFRCurveChart);
    FRequiredIFRPointSeries.ParentChart      := FIFRCurveChart;

    FDefinedIFRPointSeries                   := TPointSeries.Create(FIFRCurveChart);
    FDefinedIFRPointSeries.ParentChart       := FIFRCurveChart;

    FReferenceFlowPointSeries                := TPointSeries.Create(FIFRCurveChart);
    FReferenceFlowPointSeries.ParentChart    := FIFRCurveChart;

    FReleaseVsInflowPointSeries              := TPointSeries.Create(FIFRCurveChart);
    FReleaseVsInflowLineSeries.ParentChart   := FIFRCurveChart;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRCurve.DestroyMemberObjects;
const OPNAME = 'TIFRCurve.DestroyMemberObjects';
begin
  try
    ClearChart;
    FIFRCurveChart := nil;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRCurve.DoExport(AFileName: string);
const OPNAME = 'TIFRCurve.DoExport';
begin
  try
    FIFRCurveChart.DoExport(AFileName);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRCurve.DoPrint;
const OPNAME = 'TIFRCurve.DoPrint';
begin
  try
    FIFRCurveChart.DoPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRCurve.Initialise: boolean;
const OPNAME = 'TIFRCurve.Initialise';
begin
  Result := False;
  try
    FIFRCurveChart.Legend.Visible              := False;
    FIFRCurveChart.View3D                      := False;
    FIFRCurveChart.LeftAxis.AxisValuesFormat   := '###,###,##0.00';
    FIFRCurveChart.BottomAxis.AxisValuesFormat := '##0';
    FIFRCurveChart.LeftAxis.TitleSize        := 1;

    FReferenceFlowLineSeries.LinePen.Width := 1;
    FReferenceFlowLineSeries.Clear;
    FReferenceFlowLineSeries.Color := clRed;

    FRequiredIFRLineSeries.LinePen.Width          := 1;
    FRequiredIFRLineSeries.Clear;
    FRequiredIFRLineSeries.Color                  := clRed;

    FReferenceFlowPointSeries.Pointer.Style := psCircle;
    FReferenceFlowPointSeries.Color := clRed;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRCurve.LanguageHasChanged: boolean;
const OPNAME = 'TIFRCurve.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRCurve.PrepareChart;
const OPNAME = 'TIFRCurve.PrepareChart';
var
  LIndex : integer;
begin
  try
    SetLength(FDefinedLineSeriesArray,MaxMonths);
    SetLength(FDefinedPointSeriesArray,MaxMonths);
    for LIndex := 0 to 11 do
    begin
      FDefinedLineSeriesArray[LIndex]               := TLineSeries.Create(FIFRCurveChart);
      FDefinedLineSeriesArray[LIndex].ParentChart   := FIFRCurveChart;
      FDefinedLineSeriesArray[LIndex].Tag           := LIndex + 1;
      FDefinedLineSeriesArray[LIndex].Pen.Width     := 1;

      FDefinedPointSeriesArray[LIndex]              := TPointSeries.Create(FIFRCurveChart);
      FDefinedPointSeriesArray[LIndex].ParentChart  := FIFRCurveChart;
      FDefinedPointSeriesArray[LIndex].Tag          := LIndex + 1;
      FDefinedPointSeriesArray[LIndex].Pen.Width    := 1;
    end;
    FSuppliedIFRLineSeries.ParentChart               := FIFRCurveChart;
    FSuppliedIFRLineSeries.AreaLinesPen.Visible      := False;

    FRequiredIFRLineSeries.ParentChart               := FIFRCurveChart;
    FRequiredIFRLineSeries.AreaLinesPen.Visible      := False;

    FDefinedIFRLineSeries.ParentChart                := FIFRCurveChart;
    FDefinedIFRLineSeries.AreaLinesPen.Visible       := False;

    FReferenceFlowLineSeries.ParentChart             := FIFRCurveChart;
    FReferenceFlowLineSeries.AreaLinesPen.Visible    := False;

    FReleaseVsInflowLineSeries.ParentChart           := FIFRCurveChart;
    FReleaseVsInflowLineSeries.AreaLinesPen.Visible  := False;

    FSuppliedIFRPointSeries.ParentChart              := FIFRCurveChart;
    FSuppliedIFRPointSeries.AreaLinesPen.Visible     := False;
    FSuppliedIFRPointSeries.Pointer.VertSize         := 3;
    FSuppliedIFRPointSeries.Pointer.HorizSize        := 3;

    FRequiredIFRPointSeries.ParentChart              := FIFRCurveChart;
    FRequiredIFRPointSeries.AreaLinesPen.Visible     := False;
    FRequiredIFRPointSeries.Pointer.VertSize         := 3;
    FRequiredIFRPointSeries.Pointer.HorizSize        := 3;

    FDefinedIFRPointSeries.ParentChart               := FIFRCurveChart;
    FDefinedIFRPointSeries.AreaLinesPen.Visible      := False;
    FDefinedIFRPointSeries.Pointer.VertSize          := 3;
    FDefinedIFRPointSeries.Pointer.HorizSize         := 3;

    FReferenceFlowPointSeries.ParentChart            := FIFRCurveChart;
    FReferenceFlowPointSeries.AreaLinesPen.Visible   := False;
    FReferenceFlowPointSeries.Pointer.VertSize       := 3;
    FReferenceFlowPointSeries.Pointer.HorizSize      := 3;

    FReleaseVsInflowPointSeries.ParentChart          := FIFRCurveChart;
    FReleaseVsInflowPointSeries.AreaLinesPen.Visible := False;
    FReleaseVsInflowPointSeries.Pointer.VertSize     := 3;
    FReleaseVsInflowPointSeries.Pointer.HorizSize    := 3;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRCurve.Resize;
const OPNAME = 'TIFRCurve.Resize';
begin
  try
    FIFRCurveChart.Align := alClient;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRCurve.StudyHasChanged: boolean;
const OPNAME = 'TIFRCurve.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TAssuranceForm }

procedure TAssuranceForm.CreateMemberObjects;
const OPNAME = 'TIFRCurve.StudyHasChanged';
begin
  try
    inherited;
    FBttomPanel := TPanel.Create(Self);
    FBttomPanel.Parent := Self;
    FBtnOk := TBitBtn.Create(Self);
    FBtnOk.Parent := FBttomPanel;
    FCansel := TBitBtn.Create(Self);
    FCansel.Parent := FBttomPanel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAssuranceForm.Initialise: Boolean;
const OPNAME = 'TAssuranceForm.Initialise';
begin
  Result := False;
  try
    Self.Width := 620;
    Self.Height := 450;
    FBttomPanel.Width := 30;
    FBttomPanel.Align := alBottom;
    FCansel.Top := 2;
    FBtnOk.Top := FCansel.Top;
    Self.Position := poScreenCenter;
    FBtnOk.ModalResult := mrOk;
    FCansel.ModalResult := mrCancel;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TAssuranceForm.LanguageHasChanged: Boolean;
const OPNAME = 'TAssuranceForm.LanguageHasChanged';
begin
  Result := False;
  try
    FBtnOk.Caption := 'OK';
    FCansel.Caption := 'Cancel';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAssuranceForm.Resize;
const OPNAME = 'TAssuranceForm.Resize';
begin
  try
  inherited;
    FCansel.Left := FBttomPanel.Width-(FCansel.Width +10);
    FBtnOk.Left :=  FBttomPanel.Width-(FBtnOk.Width + FCansel.Width +35);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
