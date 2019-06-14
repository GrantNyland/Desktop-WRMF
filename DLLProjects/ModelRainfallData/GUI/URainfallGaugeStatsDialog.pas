{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeStatsDialog Class                      *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 29/04/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeStatsDialog;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  Classes,
  VCL.Graphics,
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
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallCommonGaugeSheet,
  URainfallGaugeStatsMenuItemManager,
  UMenuItemManager,
  UGenericModelLinkClasses;

type

  TRainfallGaugeStatsDialog = class (TRainfallCommonGaugeSheet)
  protected
    FPanelButton             : TPanel;
    FPanelSpacer             : TPanel;
    FGraphType1RadioButton   : TRadioButton;
    FGraphType2RadioButton   : TRadioButton;
    FIncludeUnreliableChkBox : TCheckBox;
    FHorSplitter             : TSplitter;
    FGraphCntrlPanel         : TPanel;
    FPanelGraph              : TPanel;
    FRainfallGraph           : TAbstractChart;
    FLineSeries              : TLineSeries;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    function Initialise: boolean; override;
    property PanelButton           : TPanel         read FPanelButton;
    property HorSplitter           : TSplitter      read FHorSplitter;
    property GraphType1RadioButton : TRadioButton   read FGraphType1RadioButton;
    property GraphType2RadioButton : TRadioButton   read FGraphType2RadioButton;
    property PanelGraph            : TPanel         read FPanelGraph;
    property RainfallGraph         : TAbstractChart read FRainfallGraph;
    property LineSeries            : TLineSeries    read FLineSeries;
    property IncludeUnreliableChkBox : TCheckBox    read FIncludeUnreliableChkBox;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  VCL.FileCtrl,
  UDatasetType,
  VCL.Printers,
  UConstants,
  UErrorHandlingOperations;

{ TRainfallGaugeStatsDialog }

procedure TRainfallGaugeStatsDialog.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeStatsDialog.CreateMemberObjects';
begin
  inherited;
  try
    FPanelSpacer             := TPanel.Create(Self);
    FPanelSpacer.Parent      := FPanelLeft;
    FPanelSpacer.Align       := alTop;
    FPanelSpacer.Height      := 20;
    FPanelSpacer.BorderStyle := bsNone;
    FPanelSpacer.BevelInner  := bvNone;
    FPanelSpacer.BevelOuter  := bvNone;

    FPanelButton             := TPanel.Create(Self);
    FPanelButton.Parent      := FPanelClient;                  
    FPanelButton.Align       := alTop;
    FPanelButton.Height      := 20;
    FPanelButton.Top         := 0;
    FPanelButton.BorderStyle := bsNone;
    FPanelButton.BevelInner  := bvNone;
    FPanelButton.BevelOuter  := bvNone;

    FPanelGrid.Align     := alTop;
    FPanelGrid.Height    := 32;
    FPanelGrid.Top       := FPanelButton.Top + FPanelButton.Height;

    FIncludeUnreliableChkBox           := TCheckBox.Create(Self);
    FIncludeUnreliableChkBox.Parent    := FPanelButton;
    FIncludeUnreliableChkBox.Left      := 10;
    FIncludeUnreliableChkBox.Top       := 3;
    FIncludeUnreliableChkBox.Width     := 200;
    FIncludeUnreliableChkBox.Height    := 13;
    FIncludeUnreliableChkBox.Alignment := taRightJustify;
    FIncludeUnreliableChkBox.Caption   := FAppModules.Language.GetString('CheckBox.UnreliableData');

    FHorSplitter         := TSplitter.Create(Self);
    FHorSplitter.Parent  := FPanelClient;
    FHorSplitter.Align   := alTop;
    FHorSplitter.Top     := FPanelGrid.Top + FPanelGrid.Height;
    FHorSplitter.Height  := 4;
    FHorSplitter.Beveled := TRUE;

    FPanelGrid.Align     := alTop;

    FPanelGraph             := TPanel.Create(Self);
    FPanelGraph.Parent      := FPanelClient;
    FPanelGraph.Align       := alClient;
    FPanelGraph.BorderStyle := bsNone;
    FPanelGraph.BevelInner  := bvNone;
    FPanelGraph.BevelOuter  := bvNone;

    FGraphCntrlPanel             := TPanel.Create(Self);
    FGraphCntrlPanel.Parent      := FPanelGraph;
    FGraphCntrlPanel.Height      := 20;
    FGraphCntrlPanel.Align       := alTop;
    FGraphCntrlPanel.BorderStyle := bsNone;
    FGraphCntrlPanel.BevelInner  := bvNone;
    FGraphCntrlPanel.BevelOuter  := bvNone;                         

    FGraphType1RadioButton         := TRadioButton.Create(Self);
    FGraphType1RadioButton.Parent  := FGraphCntrlPanel;
    FGraphType1RadioButton.Top     := 5;
    FGraphType1RadioButton.Left    := 10;
    FGraphType1RadioButton.Width   := 115;
    FGraphType1RadioButton.Caption := FAppModules.Language.GetString('RadioCaption.CumulativeMass');                           

    FGraphType2RadioButton         := TRadioButton.Create(Self);
    FGraphType2RadioButton.Parent  := FGraphCntrlPanel;
    FGraphType2RadioButton.Top     := 5;
    FGraphType2RadioButton.Left    := 130;
    FGraphType2RadioButton.Caption := FAppModules.Language.GetString('RadioCaption.QSum');

    FRainfallGraph                               := TAbstractChart.Create(Self, FAppModules);
    FRainfallGraph.Parent                        := FPanelGraph;
    FRainfallGraph.Align                         := alClient;
    FRainfallGraph.BevelOuter                    := bvNone;
    FRainfallGraph.Legend.Visible                := FALSE;
    FRainfallGraph.AxisVisible                   := TRUE;
    FRainfallGraph.AllowZoom                     := TRUE;
    FRainfallGraph.AllowPanning                  := pmHorizontal;
    FRainfallGraph.Gradient.Visible              := FALSE;
    FRainfallGraph.View3D                        := FALSE;
    FRainfallGraph.Title.Visible                 := TRUE;
    FRainfallGraph.Title.Font.Style              := [fsBold];
    FRainfallGraph.Title.Font.Color              := clBlack;
    FRainfallGraph.LeftAxis.Title.Caption        := FAppModules.Language.GetString('RainfallGraph.LeftTitle');
    FRainfallGraph.LeftAxis.Title.Angle          := 90;
    FRainfallGraph.BottomAxis.Title.Caption      := FAppModules.Language.GetString('RainfallGraph.BottomTitle');
    FRainfallGraph.BottomAxis.LabelsAngle        := 90;
    FRainfallGraph.BottomAxis.TickLength         := 6;
    FRainfallGraph.BottomAxis.MinorTicks.Visible := FALSE;

    FLineSeries := TLineSeries.Create(FRainfallGraph);
    FLineSeries.ParentChart   := FRainfallGraph;
    FLineSeries.Marks.Visible := FALSE;
    FLineSeries.LinePen.Width := 2;
    FLineSeries.Clear;

    FTvwGauges.MultiSelect := TRUE;
    FTvwGauges.ReadOnly    := True;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeStatsDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeStatsDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeStatsDialog.Initialise: boolean;
const OPNAME = 'TRainfallGaugeStatsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FGraphType1RadioButton.Checked := TRUE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeStatsDialog.AssignHelpContext;
const OPNAME = 'TRainfallGaugeStatsDialog.AssignHelpContext';
begin
  inherited;
  try
     SetControlHelpContext(FGraphType1RadioButton, HC_CumulativeMassPlot);
     SetControlHelpContext(FGraphType2RadioButton, HC_QsumPlot);
     SetControlHelpContext(FTvwGauges, HC_DisplayMultipleGauges);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


end.



