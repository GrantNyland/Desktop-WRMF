//
//
//  UNIT      : Contains TMonthlyDiversionFlowDialog Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 01/09/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UMonthlyDiversionFlowDialog;

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

  TMonthlyDiversionFlowDialog = class(TAbstractScrollablePanel)
  protected
    FPnlBottom                    : TPanel;
    FgbEdits                      : TGroupBox;
    FgbDailyInstreamData          : TGroupBox;

    FlblMonthlyCompensationValues : TLabel;
    FgrdMonthlyCompensationValues : TFieldStringGrid;
    FlblCapacityOfDiversion       : TLabel;
    FedtCapacityOfDiversion       : TFieldEdit;
    FlblScaleFactor               : TLabel;
    FedtScaleFactor               : TFieldEdit;
    FrgrpImportIFR                : TFieldRadioGroup;

    FtbsMonthlyInstreamFlowGraph  : TTabSheet;
    FtbsDailyInstreamFlowGraph    : TTabSheet;
    FtbsMonthlyInstreamFlowGrid   : TTabSheet;
    FtbsDailyInstreamFlowGrid     : TTabSheet;

    FstrgrdDailyInstreamFlow      : TFieldStringGrid;
    FstrgrdMonthlyInstreamFlow    : TFieldStringGrid;
    FpgcInstreamFlow              : TAbstractPageControl;

    FMonthlyInstreamGraph         : TAbstractChart;
    FMonthlyInstreamLineSeries    : TLineSeries;
    FDailyInstreamGraph           : TAbstractChart;
    FDailyInstreamLineSeries      : TLineSeries;

    FBtnAddDailyInstreamData      : TFieldBitBtn;
    FBtnDeleteDailyInstreamData   : TFieldBitBtn;
    FdtpDiversionDate             : TFieldDateTimePicker;
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

    property grdMonthlyCompensationValues : TFieldStringGrid     read FgrdMonthlyCompensationValues;
    property edtCapacityOfDiversion       : TFieldEdit           read FedtCapacityOfDiversion;
    property edtScaleFactor               : TFieldEdit           read FedtScaleFactor;
    property rgrpImportIFR                : TFieldRadioGroup     read FrgrpImportIFR;
    property strgrdDailyInstreamFlow      : TFieldStringGrid     read FstrgrdDailyInstreamFlow;
    property strgrdMonthlyInstreamFlow    : TFieldStringGrid     read FstrgrdMonthlyInstreamFlow;
    property MonthlyInstreamLineSeries    : TLineSeries          read FMonthlyInstreamLineSeries;
    property DailyInstreamLineSeries      : TLineSeries          read FDailyInstreamLineSeries;
    property BtnAddDailyInstreamData      : TFieldBitBtn         read FBtnAddDailyInstreamData;
    property BtnDeleteDailyInstreamData   : TFieldBitBtn         read FBtnDeleteDailyInstreamData;
    property dtpDiversionDate             : TFieldDateTimePicker read FdtpDiversionDate;
    property pgcInstreamFlow              : TAbstractPageControl read FpgcInstreamFlow;
    property tbsMonthlyInstreamFlowGraph  : TTabSheet            read FtbsMonthlyInstreamFlowGraph;
    property tbsDailyInstreamFlowGraph    : TTabSheet            read FtbsDailyInstreamFlowGraph;
    property tbsMonthlyInstreamFlowGrid   : TTabSheet            read FtbsMonthlyInstreamFlowGrid;
    property tbsDailyInstreamFlowGrid     : TTabSheet            read FtbsDailyInstreamFlowGrid;
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

{ TMonthlyDiversionFlowDialog }

procedure TMonthlyDiversionFlowDialog.CreateMemberObjects;
const OPNAME = 'TMonthlyDiversionFlowDialog.CreateMemberObjects';
begin
  inherited;
  try
    FPnlBottom               := TPanel.Create(ControlsOwner);
    FgbEdits                 := TGroupBox.Create(ControlsOwner);
    FlblMonthlyCompensationValues   := TLabel.Create(ControlsOwner);
    FgrdMonthlyCompensationValues   := TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FgbEdits.Parent              := ControlsParent;
    FPnlBottom.Parent            := ControlsParent;
    FlblMonthlyCompensationValues.Parent := FgbEdits;
    FgrdMonthlyCompensationValues.Parent := FgbEdits;

    FlblCapacityOfDiversion              := TLabel.Create(ControlsOwner);
    FedtCapacityOfDiversion              := TFieldEdit.Create(ControlsOwner,FAppModules);
    FlblScaleFactor                      := TLabel.Create(ControlsOwner);
    FedtScaleFactor                      := TFieldEdit.Create(ControlsOwner,FAppModules);
    FrgrpImportIFR                       := TFieldRadioGroup.Create(ControlsOwner,FAppModules);

    FtbsMonthlyInstreamFlowGraph := TTabSheet.Create(ControlsOwner);
    FtbsDailyInstreamFlowGraph   := TTabSheet.Create(ControlsOwner);
    FtbsMonthlyInstreamFlowGrid  := TTabSheet.Create(ControlsOwner);
    FtbsDailyInstreamFlowGrid    := TTabSheet.Create(ControlsOwner);

    FstrgrdDailyInstreamFlow      :=  TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdMonthlyInstreamFlow    :=  TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FpgcInstreamFlow              := TAbstractPageControl.Create(ControlsOwner,FAppModules);

    FMonthlyInstreamGraph         := TAbstractChart.Create(ControlsOwner, FAppModules);
    FMonthlyInstreamLineSeries    := TLineSeries.Create(FMonthlyInstreamGraph);
    FDailyInstreamGraph           := TAbstractChart.Create(ControlsOwner, FAppModules);
    FDailyInstreamLineSeries      := TLineSeries.Create(FDailyInstreamGraph);
    FgbDailyInstreamData                  := TGroupBox.Create(ControlsOwner);
    FBtnAddDailyInstreamData              := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FBtnDeleteDailyInstreamData           := TFieldBitBtn.Create(ControlsOwner,FAppModules);

    FgbDailyInstreamData.Parent := FtbsDailyInstreamFlowGrid;

    FlblCapacityOfDiversion.Parent       := FgbEdits;
    FedtCapacityOfDiversion.Parent       := FgbEdits;
    FlblScaleFactor.Parent               := FgbEdits;
    FedtScaleFactor.Parent               := FgbEdits;
    FrgrpImportIFR.Parent                := FgbEdits;

    FpgcInstreamFlow.Parent              := FPnlBottom;

    FtbsMonthlyInstreamFlowGraph.Parent  := FpgcInstreamFlow;
    FtbsDailyInstreamFlowGraph.Parent    := FpgcInstreamFlow;
    FtbsMonthlyInstreamFlowGrid.Parent   := FpgcInstreamFlow;
    FtbsDailyInstreamFlowGrid.Parent     := FpgcInstreamFlow;

    FstrgrdDailyInstreamFlow.Parent      :=  FgbDailyInstreamData;
    FstrgrdMonthlyInstreamFlow.Parent    :=  FtbsMonthlyInstreamFlowGrid;

    FMonthlyInstreamGraph.Parent           := FtbsMonthlyInstreamFlowGraph;
    FMonthlyInstreamLineSeries.ParentChart := FMonthlyInstreamGraph;

    FDailyInstreamGraph.Parent             := FtbsDailyInstreamFlowGraph;
    FDailyInstreamLineSeries.ParentChart   := FDailyInstreamGraph;

    FBtnAddDailyInstreamData.Parent     := FgbDailyInstreamData;
    FBtnDeleteDailyInstreamData.Parent  := FgbDailyInstreamData;

    FdtpDiversionDate        := TFieldDateTimePicker.Create(ControlsOwner,FAppModules);
    FdtpDiversionDate.Parent := FgbDailyInstreamData;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMonthlyDiversionFlowDialog.DestroyMemberObjects;
const OPNAME = 'TMonthlyDiversionFlowDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMonthlyDiversionFlowDialog.Initialise: boolean;
const OPNAME = 'TMonthlyDiversionFlowDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FgrdMonthlyCompensationValues.ScrollBars       := ssNone;
    FgrdMonthlyCompensationValues.ColCount         := 12;
    FgrdMonthlyCompensationValues.RowCount         := 2;
    FgrdMonthlyCompensationValues.FixedCols        := 0;
    FgrdMonthlyCompensationValues.FixedRows        := 1;
    FgrdMonthlyCompensationValues.DefaultRowHeight := 20;
    FgrdMonthlyCompensationValues.DefaultColWidth  := 40;
    FgrdMonthlyCompensationValues.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];

    FDailyInstreamGraph.Align                         := alClient;
    FDailyInstreamGraph.BevelOuter                    := bvNone;
    FDailyInstreamGraph.Legend.Visible                := False;
    FDailyInstreamGraph.AxisVisible                   := True;
    FDailyInstreamGraph.AllowZoom                     := True;
    FDailyInstreamGraph.AllowPanning                  := pmBoth;
    FDailyInstreamGraph.Gradient.Visible              := False;
    FDailyInstreamGraph.View3D                        := False;
    FDailyInstreamGraph.Title.Visible                 := True;
    FDailyInstreamGraph.Title.Font.Style              := [fsBold];
    FDailyInstreamGraph.Title.Font.Color              := clBlack;
    FDailyInstreamGraph.LeftAxis.Title.Angle          := 90;
    FDailyInstreamGraph.BottomAxis.LabelsAngle        := 90;
    FDailyInstreamGraph.BottomAxis.Automatic          := True;
    FDailyInstreamGraph.BottomAxis.TickLength         := 6;
    FDailyInstreamGraph.ScaleLastPage                 := False;
    FDailyInstreamGraph.BottomAxis.MinorTickCount     := 4;

    FDailyInstreamLineSeries.Marks.Visible := False;
    FDailyInstreamLineSeries.LinePen.Width := 2;
    FDailyInstreamLineSeries.Clear;

    FMonthlyInstreamGraph.Align                         := alClient;
    FMonthlyInstreamGraph.BevelOuter                    := bvNone;
    FMonthlyInstreamGraph.Legend.Visible                := False;
    FMonthlyInstreamGraph.AxisVisible                   := True;
    FMonthlyInstreamGraph.AllowZoom                     := True;
    FMonthlyInstreamGraph.AllowPanning                  := pmBoth;
    FMonthlyInstreamGraph.Gradient.Visible              := False;
    FMonthlyInstreamGraph.View3D                        := False;
    FMonthlyInstreamGraph.Title.Visible                 := True;
    FMonthlyInstreamGraph.Title.Font.Style              := [fsBold];
    FMonthlyInstreamGraph.Title.Font.Color              := clBlack;
    FMonthlyInstreamGraph.LeftAxis.Title.Angle          := 90;
    FMonthlyInstreamGraph.BottomAxis.Automatic          := True;

    FMonthlyInstreamGraph.BottomAxis.LabelsAngle        := 90;
    FMonthlyInstreamGraph.BottomAxis.TickLength         := 6;
    FMonthlyInstreamGraph.ScaleLastPage                 := False;
    FMonthlyInstreamGraph.BottomAxis.MinorTickCount     := 4;

    FMonthlyInstreamLineSeries.Marks.Visible := False;
    FMonthlyInstreamLineSeries.LinePen.Width := 2;
    FMonthlyInstreamLineSeries.Clear;

    FstrgrdDailyInstreamFlow.ColCount         := 6;
    FstrgrdDailyInstreamFlow.RowCount         := 2;
    FstrgrdDailyInstreamFlow.FixedCols        := 0;
    FstrgrdDailyInstreamFlow.FixedRows        := 1;

    FstrgrdMonthlyInstreamFlow.ColCount         := 13;
    FstrgrdMonthlyInstreamFlow.RowCount         := 2;
    FstrgrdMonthlyInstreamFlow.FixedCols        := 0;
    FstrgrdMonthlyInstreamFlow.FixedRows        := 1;

    FtbsMonthlyInstreamFlowGraph.Align := alClient;
    FtbsDailyInstreamFlowGraph.Align   := alClient;
    FtbsMonthlyInstreamFlowGrid.Align  := alClient;
    FtbsDailyInstreamFlowGrid.Align    := alClient;

    FMonthlyInstreamGraph.Align := alClient;
    FDailyInstreamGraph.Align := alClient;

    FstrgrdMonthlyInstreamFlow.Align := alClient;

    FtbsDailyInstreamFlowGrid.PageControl := FpgcInstreamFlow;
    FtbsDailyInstreamFlowGraph.PageControl := FpgcInstreamFlow;
    FtbsMonthlyInstreamFlowGrid.PageControl := FpgcInstreamFlow;
    FtbsMonthlyInstreamFlowGraph.PageControl := FpgcInstreamFlow;

    FdtpDiversionDate.Top            := 22;
    FdtpDiversionDate.Left           := 12;
    FdtpDiversionDate.Width          := 95;
    FdtpDiversionDate.Height         := 21;
    FdtpDiversionDate.Visible        := False;

    FrgrpImportIFR.Items.Clear;
    FrgrpImportIFR.Columns := 2;
    FrgrpImportIFR.Items.Add('Import csv File');
    FrgrpImportIFR.Items.Add('Create From F14');
    Resize;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMonthlyDiversionFlowDialog.AssignHelpContext;
const OPNAME = 'TMonthlyDiversionFlowDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(Self,                          HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FgrdMonthlyCompensationValues, HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtCapacityOfDiversion,       HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtScaleFactor,               HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FstrgrdDailyInstreamFlow,      HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FstrgrdMonthlyInstreamFlow,    HC_StreamflowDiversionsPreprocessor);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMonthlyDiversionFlowDialog.Resize;
const OPNAME = 'TMonthlyDiversionFlowDialog.Resize';
begin
  inherited Resize;
  try
    FgbEdits.Align  := alTop;
    FgbEdits.Height := 150;

    FPnlBottom.Align      := alClient;
    FPnlBottom.BevelOuter := bvNone;
    FpgcInstreamFlow.Align := alClient;

    FlblMonthlyCompensationValues.Top   := 4*(C_GroupBoxOffset  + 3);
    FlblMonthlyCompensationValues.Width := 150;
    FgrdMonthlyCompensationValues.Top   := C_GroupBoxOffset + 3;
    FgrdMonthlyCompensationValues.Width := 3 + (1 + FgrdMonthlyCompensationValues.DefaultColWidth) * FgrdMonthlyCompensationValues.ColCount;
    FgrdMonthlyCompensationValues.Height := 3 + (1 + FgrdMonthlyCompensationValues.DefaultRowHeight) * FgrdMonthlyCompensationValues.RowCount;

    FlblMonthlyCompensationValues.Left := C_ControlOffset;
    FgrdMonthlyCompensationValues.Left := FlblMonthlyCompensationValues.Left + FlblMonthlyCompensationValues.Width + C_ControlOffset + 15;

    FlblCapacityOfDiversion.Top := FgrdMonthlyCompensationValues.Height + C_ControlOffset + 3;
    FedtCapacityOfDiversion.Top := FlblCapacityOfDiversion.Top;
    FlblScaleFactor.Top := FlblCapacityOfDiversion.Top + FlblCapacityOfDiversion.Height + C_ControlOffset + 3;
    FedtScaleFactor.Top := FlblScaleFactor.Top;
    FrgrpImportIFR.Top := FlblScaleFactor.Top + FedtScaleFactor.Height; // + C_ControlOffset
    FrgrpImportIFR.Width := FgrdMonthlyCompensationValues.Width div 2;

    FlblCapacityOfDiversion.Left := C_ControlOffset;

    FlblScaleFactor.Left := C_ControlOffset;
    FedtCapacityOfDiversion.Left := FgrdMonthlyCompensationValues.Left;
    FedtScaleFactor.Left := FgrdMonthlyCompensationValues.Left;
    FrgrpImportIFR.Left  := FgrdMonthlyCompensationValues.Left;
    FrgrpImportIFR.Height := 40;

    FBtnAddDailyInstreamData.Left    := 10;
    FBtnAddDailyInstreamData.Top     := 20;
    FBtnAddDailyInstreamData.Width   := 75;
    FBtnAddDailyInstreamData.Height  := 25;
    FBtnAddDailyInstreamData.ShowHint:= True;
    FBtnAddDailyInstreamData.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');

    FBtnDeleteDailyInstreamData.Left    := 95;
    FBtnDeleteDailyInstreamData.Top     := 20;
    FBtnDeleteDailyInstreamData.Width   := 75;
    FBtnDeleteDailyInstreamData.Height  := 25;
    FBtnDeleteDailyInstreamData.ShowHint:= True;
    FBtnDeleteDailyInstreamData.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');

    FgbDailyInstreamData.Height := Self.ClientHeight - (4*(FBtnAddDailyInstreamData.Height + FBtnAddDailyInstreamData.Top + C_ControlOffset));

    FstrgrdDailyInstreamFlow.Top := FBtnAddDailyInstreamData.Height + FBtnAddDailyInstreamData.Top + C_ControlOffset;
    FstrgrdDailyInstreamFlow.Left := FBtnAddDailyInstreamData.Left;
    FstrgrdDailyInstreamFlow.DefaultRowHeight := 15;
    FstrgrdDailyInstreamFlow.DefaultColWidth  := 95;
    FstrgrdDailyInstreamFlow.Width := (FstrgrdDailyInstreamFlow.DefaultColWidth * FstrgrdDailyInstreamFlow.ColCount)+50;
    FstrgrdDailyInstreamFlow.Height := FgbDailyInstreamData.Height - ((FgbDailyInstreamData.Top + FBtnAddDailyInstreamData.Top + FBtnAddDailyInstreamData.Height + C_ControlOffset ));
    FstrgrdDailyInstreamFlow.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];
    FgbDailyInstreamData.Width := (FstrgrdDailyInstreamFlow.DefaultColWidth * FstrgrdDailyInstreamFlow.ColCount) + 70;

    FstrgrdMonthlyInstreamFlow.DefaultRowHeight := 15;
    FstrgrdMonthlyInstreamFlow.DefaultColWidth  := 85;
    FstrgrdMonthlyInstreamFlow.Width            := (FstrgrdMonthlyInstreamFlow.DefaultColWidth * 3) + 15;
    FstrgrdMonthlyInstreamFlow.Height           := (FstrgrdMonthlyInstreamFlow.DefaultRowHeight * 3) + 15;
//    FstrgrdMonthlyInstreamFlow.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDiversionFlowDialog.LanguageHasChanged: Boolean;
const OPNAME = 'TMonthlyDiversionFlowDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FgbEdits.Caption     := '';
    FlblMonthlyCompensationValues.Caption := FAppModules.Language.GetString('LabelText.MonthlyCompensationValues');
    FlblCapacityOfDiversion.Caption := FAppModules.Language.GetString('LabelText.CapacityOfDiversion');
    FlblScaleFactor.Caption := FAppModules.Language.GetString('LabelText.ScalingFactor');
    FtbsDailyInstreamFlowGrid.Caption := FAppModules.Language.GetString('TabCaption.DailyIFRGrid');
    FtbsMonthlyInstreamFlowGrid.Caption := FAppModules.Language.GetString('TabCaption.MonthlyIFRGrid');
    FtbsDailyInstreamFlowGraph.Caption := FAppModules.Language.GetString('TabCaption.DailyIFRGraph');
    FtbsMonthlyInstreamFlowGraph.Caption := FAppModules.Language.GetString('TabCaption.MonthlyIFRGraph');

    FMonthlyInstreamGraph.LeftAxis.Title.Caption   := FAppModules.Language.GetString('TMonthlyDiversionFlowDialog.MonthlyDiversionFlow');
    FDailyInstreamGraph.LeftAxis.Title.Caption     := FAppModules.Language.GetString('TMonthlyDiversionFlowDialog.DailyDiversionFlow');
    FDailyInstreamGraph.BottomAxis.Title.Caption   := FAppModules.Language.GetString('TMonthlyDiversionFlowDialog.Time');
    FMonthlyInstreamGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('TMonthlyDiversionFlowDialog.Time');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDiversionFlowDialog.CanExport: boolean;
const OPNAME = 'TMonthlyDiversionFlowDialog.CanExport';
begin
  Result := False;
  try
    Result := FpgcInstreamFlow.ActivePageIndex in [1,3];
    Result := Result and (Assigned(FDailyInstreamGraph) and FDailyInstreamGraph.Visible) or
                         (Assigned(FMonthlyInstreamGraph) and FMonthlyInstreamGraph.Visible) 
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDiversionFlowDialog.CanPrint: boolean;
const OPNAME = 'TMonthlyDiversionFlowDialog.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FpgcInstreamFlow) and FpgcInstreamFlow.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowDialog.DoExport(AFileName: string);
const OPNAME = 'TMonthlyDiversionFlowDialog.DoExport';
begin
  try
    case FpgcInstreamFlow.ActivePageIndex of
      1: TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FDailyInstreamGraph));
      3: TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FMonthlyInstreamGraph));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowDialog.DoPrint;
const OPNAME = 'TMonthlyDiversionFlowDialog.DoPrint';
begin
  try
    case FpgcInstreamFlow.ActivePageIndex of
      0: FstrgrdDailyInstreamFlow.DoPrint('');
      1: FDailyInstreamGraph.DoPrint;
      2: FstrgrdMonthlyInstreamFlow.DoPrint('');
      3: FMonthlyInstreamGraph.DoPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
