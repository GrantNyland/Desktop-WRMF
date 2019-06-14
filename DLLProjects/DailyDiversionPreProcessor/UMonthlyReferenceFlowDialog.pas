//
//
//  UNIT      : Contains TMonthlyReferenceFlowDialog Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 05/09/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UMonthlyReferenceFlowDialog;

interface
uses

  VCL.Buttons,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.Forms,
  VCL.ComCtrls,
  Types,
  VCL.Menus,

  VCL.Graphics,
  VCL.Grids,
  Classes,
  Windows,
  VCL.StdCtrls,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeEngine,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;
type

  TMonthlyReferenceFlowDialog = class(TAbstractScrollablePanel)
  protected
    FPnlBottom                 : TPanel;

    FgbEdits                   : TGroupBox;
    FgbDailyData               : TGroupBox;
    FrgbDailyDataQuality       : TRadioGroup;
    //FrgbThreshold            : TRadioGroup;
    //FedtThreshold            : TEdit;
    FlblThreshold              : TLabel;
    FlblThresholdDays          : TLabel;
    FlblThresholdPercentage    : TLabel;
    FlblStationNo              : TLabel;
    FedtStationNo              : TFieldEdit;
    FlblPlace                  : TLabel;
    FedtPlace                  : TFieldEdit;
    FlblLatitude               : TLabel;
    FedtLatidute               : TFieldEdit;
    FlblLongitude              : TLabel;
    FedtLongitude              : TFieldEdit;
    FlblCatchmentArea          : TLabel;
    FedtCatchmentArea          : TFieldEdit;
    FlblCatchmentFactor        : TLabel;
    FedtCatchmentFactor        : TFieldEdit;

    FtbsMonthlyFlowGraph       : TTabSheet;
    FtbsDailyFlowGraph         : TTabSheet;

    FtbsMonthlyFlowGrid        : TTabSheet;
    FtbsDailyFlowGrid          : TTabSheet;

    FstrgrdDailyFlow           : TFieldStringGrid;
    FstrgrdMonthlyFlow         : TFieldStringGrid;
    FdtpDiversionDate          : TFieldDateTimePicker;
    FpgcAvgFlowFactor          : TAbstractPageControl;
    FMonthlyGraph              : TAbstractChart;
    FMonthlyLineSeries         : TLineSeries;
    FDailyGraph                : TAbstractChart;
    FDailyLineSeries           : TLineSeries;
    FBtnAddDailyData           : TFieldBitBtn;
    FBtnDeleteDailyData        : TFieldBitBtn;
    //FlblDataQualityDesc      : TLabel;
    FgrdMonthlyThresholdValues : TFieldStringGrid;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: Boolean; override;

    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;

    property edtStationNo              : TFieldEdit           read FedtStationNo;
    property edtPlace                  : TFieldEdit           read FedtPlace;
    property edtLatidute               : TFieldEdit           read FedtLatidute;
    property edtLongitude              : TFieldEdit           read FedtLongitude;
    property edtCatchmentArea          : TFieldEdit           read FedtCatchmentArea;
    property edtCatchmentFactor        : TFieldEdit           read FedtCatchmentFactor;
    property MonthlyGraph              : TAbstractChart       read FMonthlyGraph;
    property MonthlyLineSeries         : TLineSeries          read FMonthlyLineSeries;
    property DailyGraph                : TAbstractChart       read FDailyGraph;
    property DailyLineSeries           : TLineSeries          read FDailyLineSeries;
    property strgrdDailyFlow           : TFieldStringGrid     read FstrgrdDailyFlow;
    property strgrdMonthlyFlow         : TFieldStringGrid     read FstrgrdMonthlyFlow;
    property BtnAddDailyData           : TFieldBitBtn         read FBtnAddDailyData;
    property BtnDeleteDailyData        : TFieldBitBtn         read FBtnDeleteDailyData;
    property dtpDiversionDate          : TFieldDateTimePicker read FdtpDiversionDate;
    property rgbDailyDataQuality       : TRadioGroup          read FrgbDailyDataQuality;
    //property rgbThreshold            : TRadioGroup          read FrgbThreshold;
    //property edtThreshold            : TEdit                read FedtThreshold;
    property lblThreshold              : TLabel               read FlblThreshold;
    property lblThresholdDays          : TLabel               read FlblThresholdDays;
    property lblThresholdPercentage    : TLabel               read FlblThresholdPercentage;

    property pgcAvgFlowFactor          : TAbstractPageControl read FpgcAvgFlowFactor;
    property tbsMonthlyFlowGraph       : TTabSheet            read FtbsMonthlyFlowGraph;
    property tbsDailyFlowGraph         : TTabSheet            read FtbsDailyFlowGraph;
    property tbsMonthlyFlowGrid        : TTabSheet            read FtbsMonthlyFlowGrid;
    property tbsDailyFlowGrid          : TTabSheet            read FtbsDailyFlowGrid;
    //property lblDataQualityDesc      : TLabel               read FlblDataQualityDesc;
    property grdMonthlyThresholdValues : TFieldStringGrid     read FgrdMonthlyThresholdValues;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  UDatasetType,
  VCL.Printers,
  UConstants,
  UHelpContexts,
  VoaimsCom_TLB,
  VCLTee.TeExport,
  VCLTee.TeeProcs,
  UErrorHandlingOperations;

{ TMonthlyReferenceFlowDialog }

procedure TMonthlyReferenceFlowDialog.CreateMemberObjects;
const OPNAME = 'TMonthlyReferenceFlowDialog.CreateMemberObjects';
begin
  inherited;
  try
    FPnlBottom               := TPanel.Create(ControlsOwner);
    FPnlBottom.Parent        := ControlsParent;

    FpgcAvgFlowFactor        := TAbstractPageControl.Create(ControlsOwner,FAppModules);
    FtbsMonthlyFlowGraph          := TTabSheet.Create(ControlsOwner);
    FtbsDailyFlowGraph            := TTabSheet.Create(ControlsOwner);
    FgrdMonthlyThresholdValues   := TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FtbsMonthlyFlowGrid := TTabSheet.Create(ControlsOwner);
    FtbsDailyFlowGrid := TTabSheet.Create(ControlsOwner);


    FgbEdits                 := TGroupBox.Create(ControlsOwner);
    FgbDailyData             := TGroupBox.Create(ControlsOwner);
    FrgbDailyDataQuality     := TRadioGroup.Create(ControlsOwner);
    //FrgbThreshold            := TRadioGroup.Create(ControlsOwner);
    //FedtThreshold            := TEdit.Create(ControlsOwner);
    FlblThreshold            := TLabel.Create(ControlsOwner);
    FlblThresholdDays        := TLabel.Create(ControlsOwner);
    FlblThresholdPercentage  := TLabel.Create(ControlsOwner);

    FlblStationNo            := TLabel.Create(ControlsOwner);
    FlblPlace                := TLabel.Create(ControlsOwner);
    FlblLatitude             := TLabel.Create(ControlsOwner);
    FlblLongitude            := TLabel.Create(ControlsOwner);
    FlblCatchmentArea        := TLabel.Create(ControlsOwner);
    FlblCatchmentFactor       := TLabel.Create(ControlsOwner);

    FedtStationNo            := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtPlace                := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtLatidute             := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtLongitude            := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtCatchmentArea        := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtCatchmentFactor       := TFieldEdit.Create(ControlsOwner,FAppModules);

    FstrgrdDailyFlow         :=  TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdMonthlyFlow       :=  TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FBtnAddDailyData          := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnAddDailyData.Parent  := FgbDailyData;

    FBtnDeleteDailyData         := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnDeleteDailyData.Parent  := FgbDailyData;

    //FlblDataQualityDesc         := TLabel.Create(ControlsOwner);
    //FlblDataQualityDesc.Parent  := FgbEdits;

    FpgcAvgFlowFactor.Parent    := FPnlBottom;
    FtbsDailyFlowGraph.Parent   := FPnlBottom;

    FgbEdits.Parent              := ControlsParent;
    FlblStationNo.Parent         := FgbEdits;
    FlblPlace.Parent             := FgbEdits;
    FlblLatitude.Parent          := FgbEdits;
    FlblLongitude.Parent         := FgbEdits;
    FlblCatchmentArea.Parent     := FgbEdits;
    FlblCatchmentFactor.Parent    := FgbEdits;
    FgrdMonthlyThresholdValues.Parent := FgbEdits;

    FedtStationNo.Parent         := FgbEdits;
    FedtPlace.Parent             := FgbEdits;
    FedtLatidute.Parent          := FgbEdits;
    FedtLongitude.Parent         := FgbEdits;
    FedtCatchmentArea.Parent     := FgbEdits;
    FedtCatchmentFactor.Parent    := FgbEdits;

    FMonthlyGraph                     := TAbstractChart.Create(Self, FAppModules);
    FMonthlyGraph.Parent              := FtbsMonthlyFlowGraph;

    FMonthlyLineSeries                := TLineSeries.Create(FMonthlyGraph);
    FMonthlyLineSeries.ParentChart    := FMonthlyGraph;

    FDailyGraph                       := TAbstractChart.Create(Self, FAppModules);
    FDailyGraph.Parent                := FtbsDailyFlowGraph;

    FDailyLineSeries                  := TLineSeries.Create(FDailyGraph);
    FDailyLineSeries.ParentChart      := FDailyGraph;

    FstrgrdMonthlyFlow.Parent         := FtbsMonthlyFlowGrid;
    FgbDailyData.Parent               := FtbsDailyFlowGrid;
    FrgbDailyDataQuality.Parent       := FgbEdits;
    //FrgbThreshold.Parent              := FgbEdits;
    //FedtThreshold.Parent              := FgbEdits;
    FlblThreshold.Parent              := FgbEdits;
    FlblThresholdDays.Parent          := FgbEdits;
    FlblThresholdPercentage.Parent    := FgbEdits;
    FstrgrdDailyFlow.Parent           := FgbDailyData;

    FdtpDiversionDate        := TFieldDateTimePicker.Create(ControlsOwner,FAppModules);
    FdtpDiversionDate.Parent := FgbDailyData;


  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMonthlyReferenceFlowDialog.DestroyMemberObjects;
const OPNAME = 'TMonthlyReferenceFlowDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMonthlyReferenceFlowDialog.Initialise: boolean;
const OPNAME = 'TMonthlyReferenceFlowDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FPnlBottom.Align      := alClient;
    FPnlBottom.BevelOuter := bvNone;

    FMonthlyGraph.Align                         := alClient;
    FMonthlyGraph.BevelOuter                    := bvNone;
    FMonthlyGraph.Legend.Visible                := False;
    FMonthlyGraph.AxisVisible                   := True;
    FMonthlyGraph.AllowZoom                     := True;
    FMonthlyGraph.AllowPanning                  := pmBoth;
    FMonthlyGraph.Gradient.Visible              := False;
    FMonthlyGraph.View3D                        := False;
    FMonthlyGraph.Title.Visible                 := True;
    FMonthlyGraph.Title.Font.Style              := [fsBold];
    FMonthlyGraph.Title.Font.Color              := clBlack;
    FMonthlyGraph.LeftAxis.Title.Angle          := 90;
    FMonthlyGraph.BottomAxis.Automatic          := True;

    FMonthlyGraph.BottomAxis.LabelsAngle        := 90;
    FMonthlyGraph.BottomAxis.TickLength         := 6;
    FMonthlyGraph.ScaleLastPage                 := False;
    FMonthlyGraph.BottomAxis.MinorTickCount     := 4;


    FMonthlyLineSeries.Marks.Visible := False;
    FMonthlyLineSeries.LinePen.Width := 2;
    FMonthlyLineSeries.Clear;


    FDailyGraph.Align                         := alClient;
    FDailyGraph.BevelOuter                    := bvNone;
    FDailyGraph.Legend.Visible                := False;
    FDailyGraph.AxisVisible                   := True;
    FDailyGraph.AllowZoom                     := True;
    FDailyGraph.AllowPanning                  := pmBoth;
    FDailyGraph.Gradient.Visible              := False;
    FDailyGraph.View3D                        := False;
    FDailyGraph.Title.Visible                 := True;
    FDailyGraph.Title.Font.Style              := [fsBold];
    FDailyGraph.Title.Font.Color              := clBlack;
    FDailyGraph.LeftAxis.Title.Angle          := 90;
    FDailyGraph.BottomAxis.LabelsAngle        := 90;
    FDailyGraph.BottomAxis.Automatic          := True;
    FDailyGraph.BottomAxis.TickLength         := 6;
    FDailyGraph.ScaleLastPage                 := False;
    FDailyGraph.BottomAxis.MinorTickCount     := 4;



    FstrgrdDailyFlow.ColCount         := 4;
    FstrgrdDailyFlow.RowCount         := 2;
    FstrgrdDailyFlow.FixedCols        := 0;
    FstrgrdDailyFlow.FixedRows        := 1;

    FstrgrdMonthlyFlow.ColCount         := 13;
    FstrgrdMonthlyFlow.RowCount         := 2;
    FstrgrdMonthlyFlow.FixedCols        := 0;
    FstrgrdMonthlyFlow.FixedRows        := 1;

    FDailyLineSeries.Marks.Visible := False;
    FDailyLineSeries.LinePen.Width := 2;
    FDailyLineSeries.Clear;
    FstrgrdMonthlyFlow.Align := alClient;

    FdtpDiversionDate.Top            := 22;
    FdtpDiversionDate.Left           := 12;
    FdtpDiversionDate.Width          := 95;
    FdtpDiversionDate.Height         := 21;
    FdtpDiversionDate.Visible        := False;

    FrgbDailyDataQuality.Items.Add('Include All Imported Data');
    FrgbDailyDataQuality.Items.Add('Exclude Suspect');
    FrgbDailyDataQuality.Items.Add('Infill Gaps');

    {FrgbThreshold.Columns := 2;
    FrgbThreshold.Items.Add('Days');
    FrgbThreshold.Items.Add('Percentage');
    FrgbThreshold.Visible := False;
    FrgbThreshold.ItemIndex := 0;
    }
    //FedtThreshold.Visible := False;

    FlblThreshold.Visible := False;
    FlblThresholdDays.Visible := False;
    FlblThresholdPercentage.Visible := False;

    FgrdMonthlyThresholdValues.ScrollBars       := ssNone;
    FgrdMonthlyThresholdValues.ColCount         := 12;
    FgrdMonthlyThresholdValues.RowCount         := 3;
    FgrdMonthlyThresholdValues.FixedCols        := 0;
    FgrdMonthlyThresholdValues.FixedRows        := 1;
    FgrdMonthlyThresholdValues.DefaultRowHeight := 20;
    FgrdMonthlyThresholdValues.DefaultColWidth  := 40;
    FgrdMonthlyThresholdValues.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    FgrdMonthlyThresholdValues.Visible          := False;
    FrgbDailyDataQuality.Columns                := 3;
    Resize;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMonthlyReferenceFlowDialog.AssignHelpContext;
const OPNAME = 'TMonthlyReferenceFlowDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(Self                     , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtStationNo            , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtPlace                , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtLatidute             , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtLongitude            , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtCatchmentArea        , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtCatchmentFactor      , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FtbsDailyFlowGrid        , HC_StreamflowDiversionsPreprocessor);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMonthlyReferenceFlowDialog.Resize;
const OPNAME = 'TMonthlyReferenceFlowDialog.Resize';
begin
  inherited Resize;
  try
    FgbEdits.Height := 150;
    FgbEdits.Align  := alTop;

    FlblStationNo.Top   := C_GroupBoxOffset  + 3;
    FlblStationNo.Width := 130;
    FedtStationNo.Top   := C_GroupBoxOffset + 3;
    FedtStationNo.Width := 60;

    FlblPlace.Top    := FlblStationNo.Top + FlblStationNo.Height + C_LabelOffset + 3;
    FedtPlace.Top    := FlblPlace.Top  + 3;
    FedtPlace.Width  := 150;

    FlblLatitude.Top   := FlblPlace.Top + FlblPlace.Height + C_LabelOffset + 3;
    FedtLatidute.Top := FlblLatitude.Top  + 3;
    FedtLatidute.Width := 150;

    FlblLongitude.Top  := FlblLatitude.Top + FlblLatitude.Height + C_LabelOffset + 3;
    FedtLongitude.Top := FlblLongitude.Top  + 3;
    FedtLongitude.Width := 150;

    FlblCatchmentArea.Top :=  FlblLongitude.Top + FlblLongitude.Height + C_LabelOffset + 3;
    FedtCatchmentArea.Top := FlblCatchmentArea.Top  + 3;
    FedtCatchmentArea.Width := 60;

    FlblCatchmentFactor.Top := FlblCatchmentArea.Top + FlblCatchmentArea.Height + C_LabelOffset + 3;
    FedtCatchmentFactor.Top := FlblCatchmentFactor.Top + 3;
    FedtCatchmentFactor.Width := 60;

    FlblStationNo.Left       := C_ControlOffset;
    FedtStationNo.Left       := FlblStationNo.Left  + FlblStationNo.Width + C_LabelOffset;
    FlblPlace.Left           := C_ControlOffset;
    FedtPlace.Left           := FlblStationNo.Left  + FlblStationNo.Width + C_LabelOffset;
    FlblLatitude.Left        := C_ControlOffset;
    FedtLatidute.Left        := FlblStationNo.Left  + FlblStationNo.Width + C_LabelOffset;
    FlblLongitude.Left       := C_ControlOffset;
    FedtLongitude.Left       := FlblStationNo.Left  + FlblStationNo.Width + C_LabelOffset;
    FlblCatchmentArea.Left   := C_ControlOffset;
    FedtCatchmentArea.Left   := FlblStationNo.Left  + FlblStationNo.Width + C_LabelOffset;
    FlblCatchmentFactor.Left  := C_ControlOffset;
    FedtCatchmentFactor.Left  := FlblStationNo.Left  + FlblStationNo.Width + C_LabelOffset;

    FpgcAvgFlowFactor.Align  := alClient;
    FtbsMonthlyFlowGraph.Align    := alClient;
    FtbsDailyFlowGraph.Align      := alClient;
    FtbsMonthlyFlowGrid.Align    := alClient;
    FtbsDailyFlowGrid.Align    := alClient;

    FBtnAddDailyData.Left    := 10;
    FBtnAddDailyData.Top     := 20;
    FBtnAddDailyData.Width   := 75;
    FBtnAddDailyData.Height  := 25;
    FBtnAddDailyData.ShowHint:= True;
    FBtnAddDailyData.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');

    FBtnDeleteDailyData.Left    := 95;
    FBtnDeleteDailyData.Top     := 20;
    FBtnDeleteDailyData.Width   := 75;
    FBtnDeleteDailyData.Height  := 25;
    FBtnDeleteDailyData.ShowHint:= True;
    FBtnDeleteDailyData.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');

    FgbDailyData.Height := Self.ClientHeight - (4*(FBtnAddDailyData.Height + FBtnAddDailyData.Top + C_ControlOffset));
    FrgbDailyDataQuality.Top := C_ControlOffset+C_LabelOffset+2;
    FrgbDailyDataQuality.Height := 40;
    FstrgrdDailyFlow.Top := FBtnAddDailyData.Height + FBtnAddDailyData.Top + C_ControlOffset;
    FstrgrdDailyFlow.Left := FBtnAddDailyData.Left;
    FstrgrdDailyFlow.DefaultRowHeight := 15;
    FstrgrdDailyFlow.DefaultColWidth  := 95;
    FstrgrdDailyFlow.Width := (FstrgrdDailyFlow.DefaultColWidth * FstrgrdDailyFlow.ColCount);
    FstrgrdDailyFlow.Height := FgbDailyData.Height - ((FgbDailyData.Top + FBtnAddDailyData.Top + FBtnAddDailyData.Height + C_ControlOffset ));
    FstrgrdDailyFlow.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];

    FgbDailyData.Width := FstrgrdDailyFlow.DefaultColWidth * FstrgrdDailyFlow.ColCount + 20; //(FBtnDeleteDailyData.Width + FBtnAddDailyData.Width)*2;
    FrgbDailyDataQuality.Left := FedtPlace.Left + {FlblPlace.Width +} FedtPlace.Width + C_ControlOffset;

    //FlblDataQualityDesc.Top := FrgbDailyDataQuality.Top;
    //FlblDataQualityDesc.Left := FrgbDailyDataQuality.Left+ FrgbDailyDataQuality.Width + C_LabelOffset;
    //FlblDataQualityDesc.Width := FrgbDailyDataQuality.Width + (4*C_LabelOffset);

    {FrgbThreshold.Top := FrgbDailyDataQuality.Top + FrgbDailyDataQuality.Height;
    FrgbThreshold.Left := FrgbDailyDataQuality.Left;
    FrgbThreshold.Height := 40;

    FedtThreshold.Top := FrgbDailyDataQuality.Top + FrgbDailyDataQuality.Height + C_LabelOffset;
    FedtThreshold.Left := FrgbDailyDataQuality.Left;
    FedtThreshold.Width := 40;


    }

    FlblThreshold.Top := FrgbDailyDataQuality.Height + FrgbDailyDataQuality.Top+C_LabelOffset;
    FlblThreshold.Left := FrgbDailyDataQuality.Left;


    FgrdMonthlyThresholdValues.Top   := FlblThreshold.Top + FlblThreshold.Height;
    FgrdMonthlyThresholdValues.Width := 3 + (1 + FgrdMonthlyThresholdValues.DefaultColWidth) * FgrdMonthlyThresholdValues.ColCount;
    FgrdMonthlyThresholdValues.Height := 3 + (1 + FgrdMonthlyThresholdValues.DefaultRowHeight) * FgrdMonthlyThresholdValues.RowCount;
    FgrdMonthlyThresholdValues.Left := FrgbDailyDataQuality.Left;
    FrgbDailyDataQuality.Width := FgrdMonthlyThresholdValues.Width;

    FlblThresholdDays.Top := 5+FgrdMonthlyThresholdValues.Top + FgrdMonthlyThresholdValues.DefaultRowHeight;
    FlblThresholdDays.Left := FgrdMonthlyThresholdValues.Left +FgrdMonthlyThresholdValues.Width;

    FlblThresholdPercentage.Top := 10+FgrdMonthlyThresholdValues.Top + FgrdMonthlyThresholdValues.DefaultRowHeight*2;
    FlblThresholdPercentage.Left := FgrdMonthlyThresholdValues.Left +FgrdMonthlyThresholdValues.Width;

    {FlblDataQualityDesc.WordWrap  := True;
    FlblDataQualityDesc.Font.Color := clBlack;
    FlblDataQualityDesc.Font.Size  := 8;
    FlblDataQualityDesc.AutoSize := True;
     }
    FtbsDailyFlowGrid.PageControl    := FpgcAvgFlowFactor;
    FtbsDailyFlowGraph.PageControl    := FpgcAvgFlowFactor;
    FtbsMonthlyFlowGrid.PageControl    := FpgcAvgFlowFactor;
    FtbsMonthlyFlowGraph.PageControl  := FpgcAvgFlowFactor;

    FstrgrdMonthlyFlow.DefaultRowHeight := 15;
    FstrgrdMonthlyFlow.DefaultColWidth  := 85;
    FstrgrdMonthlyFlow.Width            := (FstrgrdMonthlyFlow.DefaultColWidth * 3) + 15;
    FstrgrdMonthlyFlow.Height           := (FstrgrdMonthlyFlow.DefaultRowHeight * 3) + 15;
    FstrgrdMonthlyFlow.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyReferenceFlowDialog.LanguageHasChanged: Boolean;
const OPNAME = 'TMonthlyReferenceFlowDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FgbEdits.Caption            := '';
    FlblStationNo.Caption       := FAppModules.Language.GetString('LabelText.StationNo');
    FlblPlace.Caption           := FAppModules.Language.GetString('LabelText.Place');
    FlblLatitude.Caption        := FAppModules.Language.GetString('LabelText.Latitude');
    FlblLongitude.Caption       := FAppModules.Language.GetString('LabelText.Longitude');
    FlblCatchmentArea.Caption   := FAppModules.Language.GetString('LabelText.CatchmentArea');
    FlblCatchmentFactor.Caption := FAppModules.Language.GetString('LabelText.CatchmentFactor');
     
    FMonthlyGraph.LeftAxis.Title.Caption   := FAppModules.Language.GetString('TMonthlyReferenceFlowDialog.MonthlyReferenceFlow');
    FDailyGraph.LeftAxis.Title.Caption     := FAppModules.Language.GetString('TMonthlyReferenceFlowDialog.DailyReferenceFlow');
    FDailyGraph.BottomAxis.Title.Caption   := FAppModules.Language.GetString('TMonthlyReferenceFlowDialog.Time');
    FMonthlyGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('TMonthlyReferenceFlowDialog.Time');
    
    FtbsMonthlyFlowGraph.Caption := FAppModules.Language.GetString('TabCaption.MonthlyFlowGraph');
    FtbsDailyFlowGraph.Caption   := FAppModules.Language.GetString('TabCaption.DailyFlowGraph');
    FtbsDailyFlowGrid.Caption    := FAppModules.Language.GetString('TabCaption.DailyFlowGrid');
    FtbsMonthlyFlowGrid.Caption  := FAppModules.Language.GetString('TabCaption.MonthlyFlowGrid');
    FlblThreshold.Caption        := 'Days in Month to be infilled';
    FlblThresholdDays.Caption    := 'Day(s)';
    FlblThresholdPercentage.Caption        := '%';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyReferenceFlowDialog.CanExport: boolean;
const OPNAME = 'TMonthlyReferenceFlowDialog.CanExport';
begin
  Result := False;
  try
    Result := FpgcAvgFlowFactor.ActivePageIndex in [1,3];
    Result := Result and (Assigned(FMonthlyGraph) and FMonthlyGraph.Visible) or
              (Assigned(FDailyGraph) and FDailyGraph.Visible) 
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyReferenceFlowDialog.CanPrint: boolean;
const OPNAME = 'TMonthlyReferenceFlowDialog.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FpgcAvgFlowFactor) and FpgcAvgFlowFactor.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowDialog.DoExport(AFileName: string);
const OPNAME = 'TMonthlyReferenceFlowDialog.DoExport';
begin
  try
    case FpgcAvgFlowFactor.ActivePageIndex of
      1: TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FDailyGraph));
      3: TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FMonthlyGraph));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowDialog.DoPrint;
const OPNAME = 'TMonthlyReferenceFlowDialog.DoPrint';
begin
  try
    case FpgcAvgFlowFactor.ActivePageIndex of
      0: FstrgrdDailyFlow.DoPrint('');
      1: FDailyGraph.DoPrint;
      2: FstrgrdMonthlyFlow.DoPrint('');
      3: FMonthlyGraph.DoPrint; 
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
