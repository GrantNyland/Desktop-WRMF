//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyIFRDataDialog;

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
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  Classes,
  Windows,
  VCL.StdCtrls,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;
type

  TDailyIFRDataDialog = class(TAbstractScrollablePanel)
  protected

    FgbEdits : TGroupBox;
    FlblChannel : TLabel;
    FcbxChannel : TFieldComboBox;
    FlblScaleFactor : TLabel;
    FedtScaleFactor : TFieldEdit;

    FcbxViewMonth : TFieldComboBox;

    FlblCurrentMonth : TLabel;
    FcbxCurrentMonth : TFieldComboBox;

    FtbsGrid : TTabSheet;
    FtbsGraph : TTabSheet;
    FpgcIFR : TAbstractPageControl;

    FpnlMonthlyGraph : TPanel;

    FMonthlyIFRGrid : TFieldStringGrid;
    FMonthlyIFRGraph : TAbstractChart;
    FMonthlyIFRSeries : TLineSeries;
    FMonthlyFlowIFRSeries : TLineSeries;

    FDailyIFRGrid : TFieldStringGrid;
    FbtnGenerateDailyIFR : TFieldBitBtn;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: Boolean; override;
    property cbxChannel : TFieldComboBox read FcbxChannel;
    property edtScaleFactor : TFieldEdit read FedtScaleFactor;
    property cbxCurrentMonth : TFieldComboBox read FcbxCurrentMonth;
    property MonthlyIFRGrid : TFieldStringGrid read FMonthlyIFRGrid;
    property MonthlyIFRGraph : TAbstractChart read FMonthlyIFRGraph;
    property MonthlyIFRSeries : TLineSeries read FMonthlyIFRSeries;
    property MonthlyFlowIFRSeries : TLineSeries read FMonthlyFlowIFRSeries;
    property btnGenerateDailyIFR : TFieldBitBtn     read FbtnGenerateDailyIFR;
    property DailyIFRGrid : TFieldStringGrid read FDailyIFRGrid;
    property pgcIFR : TAbstractPageControl read FpgcIFR;

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
  UErrorHandlingOperations;

{ TDailyIFRDataDialog }

procedure TDailyIFRDataDialog.AssignHelpContext;
const OPNAME = 'TDailyIFRDataDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(Self            , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FcbxChannel     , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FedtScaleFactor , HC_StreamflowDiversionsPreprocessor);
    SetControlHelpContext(FcbxViewMonth   , HC_StreamflowDiversionsPreprocessor);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDailyIFRDataDialog.CreateMemberObjects;
const OPNAME = 'TDailyIFRDataDialog.CreateMemberObjects';
begin
  try

    inherited CreateMemberObjects;
    FgbEdits := TGroupBox.Create(ControlsOwner);
    FlblChannel := TLabel.Create(ControlsOwner);
    FcbxChannel := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FlblScaleFactor := TLabel.Create(ControlsOwner);
    FedtScaleFactor := TFieldEdit.Create(ControlsOwner,FAppModules);

    FlblCurrentMonth := TLabel.Create(ControlsOwner);
    FcbxCurrentMonth := TFieldComboBox.Create(ControlsOwner,FAppModules);

    FpgcIFR := TAbstractPageControl.Create(ControlsOwner,FAppModules);
    FtbsGrid := TTabSheet.Create(ControlsOwner);
    FtbsGraph := TTabSheet.Create(ControlsOwner);
    FpnlMonthlyGraph := TPanel.Create(ControlsOwner);
    FMonthlyIFRGrid := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FMonthlyIFRGraph := TAbstractChart.Create(ControlsOwner,FAppModules);
    FMonthlyIFRSeries := TLineSeries.Create(FMonthlyIFRGraph);
    FMonthlyFlowIFRSeries := TLineSeries.Create(FMonthlyIFRGraph);
    FbtnGenerateDailyIFR := TFieldBitBtn.Create(ControlsOwner,FAppModules);

    FDailyIFRGrid := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FgbEdits.Parent := ControlsParent;
    FlblChannel.Parent := FgbEdits;
    FcbxChannel.Parent := FgbEdits;
    FlblScaleFactor.Parent := FgbEdits;
    FedtScaleFactor.Parent := FgbEdits;


    FlblCurrentMonth.Parent := FgbEdits;
    FcbxCurrentMonth.Parent := FgbEdits;

    FpgcIFR.Parent := ControlsParent;
    FtbsGrid.Parent := FpgcIFR;
    FtbsGraph.Parent := FpgcIFR;

    FpnlMonthlyGraph.Parent := FtbsGraph;

    FMonthlyIFRGrid.Parent := FtbsGrid;
    FMonthlyIFRGraph.Parent := FpnlMonthlyGraph;
    FMonthlyIFRSeries.ParentChart := FMonthlyIFRGraph;
    FMonthlyFlowIFRSeries.ParentChart := FMonthlyIFRGraph;

    FDailyIFRGrid.Parent := FtbsGrid;

    FbtnGenerateDailyIFR.Parent := FgbEdits;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataDialog.DestroyMemberObjects;
const OPNAME = 'TDailyIFRDataDialog.DestroyMemberObjects';
begin
  try

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataDialog.Initialise: boolean;
const OPNAME = 'TDailyIFRDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FtbsGrid.PageControl := FpgcIFR;
    FtbsGraph.PageControl := FpgcIFR;
    FMonthlyIFRGrid.RowCount := 2;
    FMonthlyIFRGrid.ColCount := 3;

    FDailyIFRGrid.RowCount := 2;
    FDailyIFRGrid.ColCount := 2;
    

    MonthlyIFRGraph.Align                         := alClient;
    MonthlyIFRGraph.BevelOuter                    := bvNone;
    MonthlyIFRGraph.Legend.Visible                := False;
    MonthlyIFRGraph.AxisVisible                   := True;
    MonthlyIFRGraph.AllowZoom                     := True;
    MonthlyIFRGraph.AllowPanning                  := pmBoth;
    MonthlyIFRGraph.Gradient.Visible              := False;
    MonthlyIFRGraph.View3D                        := False;
    MonthlyIFRGraph.Title.Visible                 := True;
    MonthlyIFRGraph.Title.Font.Style              := [fsBold];
    MonthlyIFRGraph.Title.Font.Color              := clBlack;
    MonthlyIFRGraph.LeftAxis.Title.Angle          := 90;
    MonthlyIFRGraph.BottomAxis.Automatic          := True;
    MonthlyIFRGraph.BottomAxis.TickLength         := 6;
    MonthlyIFRGraph.ScaleLastPage                 := False;
    MonthlyIFRGraph.BottomAxis.MinorTickCount     := 4;
    MonthlyIFRGraph.LeftAxis.MinimumOffset        := 10;

    FMonthlyIFRSeries.Marks.Visible := False;
    FMonthlyIFRSeries.LinePen.Width := 2;
    FMonthlyIFRSeries.Clear;

    FMonthlyFlowIFRSeries.Marks.Visible := False;
    FMonthlyFlowIFRSeries.LinePen.Width := 2;
    FMonthlyFlowIFRSeries.Clear;

    FbtnGenerateDailyIFR.Glyph.LoadFromResourceName(HImagesInstance,'EXPORTFILE');
    FbtnGenerateDailyIFR.NumGlyphs  := 2;
    FbtnGenerateDailyIFR.Enabled    := False;
    Self.ShowHint := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDailyIFRDataDialog.LanguageHasChanged: Boolean;
const OPNAME = 'TDailyIFRDataDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FtbsGrid.Caption        := FAppModules.Language.GetString('TabCaption.Grid');
    FtbsGraph.Caption       := FAppModules.Language.GetString('TabCaption.Graph');
    FlblChannel.Caption     := FAppModules.Language.GetString('LabelText.Channel');
    FlblScaleFactor.Caption := FAppModules.Language.GetString('LabelText.CatchmentScaleFactor');
    FMonthlyIFRGrid.Cells [0,0] := FAppModules.Language.GetString('GridHeading.Count');
    FMonthlyIFRGrid.Cells [1,0] := FAppModules.Language.GetString('GridHeading.Flow');
    FMonthlyIFRGrid.Cells [2,0] := FAppModules.Language.GetString('GridHeading.IFR');

    FDailyIFRGrid.Cells [0,0] := FAppModules.Language.GetString('GridHeading.Date');
    FDailyIFRGrid.Cells [1,0] := FAppModules.Language.GetString('GridHeading.DailyIFR');
    FlblCurrentMonth.Caption  := FAppModules.Language.GetString('LabelText.SelectAMonth');
    FbtnGenerateDailyIFR.Caption := FAppModules.Language.GetString('ButtonCaption.GenerateIFR');
    MonthlyIFRGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
    MonthlyIFRGraph.LeftAxis.Title.Caption := FAppModules.Language.GetString('TIFRSiteDialog.DefinedIFRS');
    Resize;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDailyIFRDataDialog.Resize;
const OPNAME = 'TDailyIFRDataDialog.Resize';
begin
  inherited Resize;
  try
    FgbEdits.Align := alTop;
    FgbEdits.Height := 50;

    FlblChannel.Top := C_ControlOffset+5;
    FlblChannel.Left := C_ControlOffset;

    FcbxChannel.Top := C_ControlOffset;
    FcbxChannel.Left := FlblChannel.Width + FlblChannel.Left + C_ControlOffset;
    FcbxChannel.Width := 150;

    FlblScaleFactor.Top := C_ControlOffset+5;
    FlblScaleFactor.Left := FcbxChannel.Left + FcbxChannel.Width + C_ControlOffset;
    FlblScaleFactor.AutoSize := True;
    FedtScaleFactor.Top := C_ControlOffset;

    FedtScaleFactor.Left := FlblScaleFactor.Left + FlblScaleFactor.Width + C_ControlOffset;
    FedtScaleFactor.Width := 60;

    FlblCurrentMonth.Top      := C_ControlOffset+5 ;
    FlblCurrentMonth.Left     := FedtScaleFactor.Left + FedtScaleFactor.Width + C_ControlOffset;
    FlblCurrentMonth.AutoSize := True;;


    FcbxCurrentMonth.Top      := C_ControlOffset;
    FcbxCurrentMonth.Left     := FlblCurrentMonth.Left + FlblCurrentMonth.Width + C_ControlOffset;
    FcbxCurrentMonth.Width    := 60;

    FbtnGenerateDailyIFR.Top  := C_ControlOffset;
    FbtnGenerateDailyIFR.Width        := 110;
    FbtnGenerateDailyIFR.Left := Self.ClientWidth - FbtnGenerateDailyIFR.Width - 10;

    FpgcIFR.Align             := alClient;
    FpgcIFR.TabPosition       := tpBottom;
    FtbsGrid.Align            := alClient;
    FtbsGraph.Align           := alClient;

    FpnlMonthlyGraph.Top       := C_LabelOffset;
    FpnlMonthlyGraph.Left      := C_LabelOffset;
    FpnlMonthlyGraph.Height    := Self.ClientHeight - ((FgbEdits.Height)*2);
    FpnlMonthlyGraph.Width     := Self.ClientWidth;

    FMonthlyIFRGrid.Top       := C_LabelOffset;
    FMonthlyIFRGrid.Left      := C_LabelOffset;
    FMonthlyIFRGrid.Height    := Self.ClientHeight - ((FgbEdits.Height)*2);
    FMonthlyIFRGrid.Width     := Self.ClientWidth div 2;

    FMonthlyIFRGraph.Align    := alClient;

    FDailyIFRGrid.Top       := C_LabelOffset;
    FDailyIFRGrid.Left      := FMonthlyIFRGrid.Width + C_LabelOffset;
    FDailyIFRGrid.Height    := Self.ClientHeight - ((FgbEdits.Height)*2);
    FDailyIFRGrid.Width     := (Self.ClientWidth div 2)-C_ControlOffset;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
