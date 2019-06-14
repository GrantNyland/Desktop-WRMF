{******************************************************************************}
{*  UNIT      : Contains TRainfallPatchAdminDialog Class                      *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 03/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}


unit URainfallPatchAdminDialog;

interface
                                            
uses
  Classes,
  VCLTee.Chart,
  VCLTee.GanttCh,
  VCLTee.TeEngine,
  VCLTee.TeeProcs,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,
  VCL.Graphics,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallCommonGaugeSheet,
  URainfallPatchAdminMenuItemManager,
  UMenuItemManager,
  UGenericModelLinkClasses;

type

  TRainfallPatchAdminDialog = class (TRainfallCommonGaugeSheet)
  protected
    FPanelButton       : TAbstractPanel;
    FPanelSpacer       : TAbstractPanel;
    FLblPatchMsg       : TLabel;
    FHorSplitter       : TSplitter;
    FPanelChart        : TPanel;
    FRecordLengthChart : TAbstractChart;
    FChartSeries       : TGanttSeries;
    FIncludeUnreliableChkBox : TCheckBox;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    property HorSplitter       : TSplitter      read FHorSplitter;
    property RecordLengthChart : TAbstractChart read FRecordLengthChart;
    property ChartSeries       : TGanttSeries   read FChartSeries;
    property PatchMsgLbl       : TLabel         read FLblPatchMsg;
    property PanelChart        : TPanel         read FPanelChart;
    property PanelButton       : TAbstractPanel read FPanelButton;
    property IncludeUnreliableChkBox : TCheckBox    read FIncludeUnreliableChkBox;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  VCL.Printers,
  UConstants,
  UDatasetType,
  UErrorHandlingOperations;

{ TRainfallPatchAdminDialog }

procedure TRainfallPatchAdminDialog.CreateMemberObjects;
const OPNAME = 'TRainfallPatchAdminDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanelSpacer             := TAbstractPanel.Create(FPanelLeft, FAppModules);
    FPanelSpacer.Parent      := FPanelLeft;
    FPanelSpacer.Align       := alTop;
    FPanelSpacer.Height      := 20;
    FPanelSpacer.BorderStyle := bsNone;
    FPanelSpacer.BevelInner  := bvNone;
    FPanelSpacer.BevelOuter  := bvNone;

    FPanelButton             := TAbstractPanel.Create(FPanelClient, FAppModules);
    FPanelButton.Parent      := FPanelClient;
    FPanelButton.Align       := alTop;
    FPanelButton.Height      := 20;
    FPanelButton.BorderStyle := bsNone;
    FPanelButton.BevelInner  := bvNone;
    FPanelButton.BevelOuter  := bvNone;

    FIncludeUnreliableChkBox           := TCheckBox.Create(Self);
    FIncludeUnreliableChkBox.Parent    := FPanelButton;
    FIncludeUnreliableChkBox.Left      := 10;
    FIncludeUnreliableChkBox.Top       := 3;
    FIncludeUnreliableChkBox.Width     := 200;
    FIncludeUnreliableChkBox.Height    := 13;
    FIncludeUnreliableChkBox.Alignment := taRightJustify;
    FIncludeUnreliableChkBox.Caption   := FAppModules.Language.GetString('CheckBox.UnreliableData');

    FlblPatchMsg            := TLabel.Create(FPanelButton);
    FlblPatchMsg.Parent     := FPanelButton;
    FlblPatchMsg.Top        := 3;
    FlblPatchMsg.Width      := 400;
    FlblPatchMsg.Height     := 16;
    FlblPatchMsg.AutoSize   := FALSE;
    FlblPatchMsg.WordWrap   := FALSE;
    FlblPatchMsg.Caption    := '';
    FlblPatchMsg.Layout     := tlCenter;
    FlblPatchMsg.Font.Color := clRed;
    FlblPatchMsg.Font.Style := [fsBold];
    FlblPatchMsg.Left       := 210;

    FPanelGrid.Align     := alTop;
    FPanelGrid.Height    := 80;
    FPanelGrid.Top       := FPanelButton.Top + FPanelButton.Height;

    FHorSplitter         := TSplitter.Create(Self);
    FHorSplitter.Parent  := FPanelClient;
    FHorSplitter.Align   := alTop;
    FHorSplitter.Top     := FPanelGrid.Top + FPanelGrid.Height;
    FHorSplitter.Height  := 4;
    FHorSplitter.Beveled := TRUE;

    FPanelGrid.Align     := alTop;

    FPanelChart             := TPanel.Create(Self);
    FPanelChart.Parent      := FPanelClient;
    FPanelChart.Align       := alClient;
    FPanelChart.BorderStyle := bsNone;
    FPanelChart.BevelInner  := bvNone;
    FPanelChart.BevelOuter  := bvNone;

    FRecordLengthChart                  := TAbstractChart.Create(Self, FAppModules);
    FRecordLengthChart.Parent           := FPanelChart;
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
    FChartSeries.XValues.DateTime := FALSE;

    FTvwGauges.MultiSelect := False;
    FTvwGauges.ReadOnly := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallPatchAdminDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallPatchAdminDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallPatchAdminDialog.Initialise: boolean;
const OPNAME = 'TRainfallPatchAdminDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.



