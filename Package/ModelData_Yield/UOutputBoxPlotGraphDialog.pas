//
//
//  UNIT      : Contains the class TOutputBoxPlotGraphDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
                                                        
unit UOutputBoxPlotGraphDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCLTee.Series,
  Contnrs,
  VCLTee.TeeShape,
  VCL.Forms,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCLTee.TeeBoxPlot,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutputBoxPlotGraphDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel        : TPanel;
    FViewDataType         : TFieldComboBox;
    FViewDataLabel        : TLabel;
    FBtnDataSelection     : TFieldButton;
    FBtnLegend            : TFieldButton;
    FDataChart            : TFieldChart;
    FShowDamLevelChkBox   : TCheckBox;
    FDamLevelSeries       : TLineSeries;

    procedure CreateMemberObjects; override;
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
    function CreateBoxPlotSeries: TBoxSeries;

    property cmbViewDataType     : TFieldComboBox          read FViewDataType;
    property Chart               : TFieldChart             read FDataChart;
    property BtnDataSelection    : TFieldButton            read FBtnDataSelection;
    property BtnLegend           : TFieldButton            read FBtnLegend;
    property ViewDataLabel       : TLabel                  read FViewDataLabel;
    property ShowDamLevelChkBox  : TCheckBox               read FShowDamLevelChkBox;
    property DamLevelSeries      : TLineSeries             read FDamLevelSeries;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  VCLTee.TeExport,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TOutputBoxPlotGraphDialog }

procedure TOutputBoxPlotGraphDialog.CreateMemberObjects;
const OPNAME = 'TOutputBoxPlotGraphDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;

    FViewDataLabel              := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent       := FSelectorPanel;
    FViewDataLabel.Alignment    := taCenter;
    FViewDataLabel.AutoSize     := False;

    FViewDataType               := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent        := FSelectorPanel;

    FShowDamLevelChkBox         := TCheckBox.Create(ControlsOwner);
    FShowDamLevelChkBox.Parent  := FSelectorPanel;

    FBtnDataSelection           := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent    := FSelectorPanel;

    FBtnLegend                  := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnLegend.Parent           := FSelectorPanel;

    FDataChart                   := TFieldChart.Create(ControlsOwner, FAppModules);
    FDataChart.Parent            := ControlsParent;

    FDamLevelSeries              := TLineSeries.Create(FDataChart);
    FDamLevelSeries.ParentChart  := FDataChart;
    FDamLevelSeries.Name         := 'DamVolume';
    FDamLevelSeries.Color        := clBlack;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphDialog.Initialise: boolean;
const OPNAME = 'TOutputBoxPlotGraphDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataType.Clear;
    FDataChart.Initialise;
    FDataChart.Title.Alignment               := taCenter;
    FDataChart.View3D                        := False;
    FDataChart.RightWall.Visible             := True;
    FDataChart.BottomAxis.Title.Font.Size    := 10;
    FDataChart.BottomAxis.Title.Font.Style   := [fsBold];
    FDataChart.LeftAxis.Title.Font.Size      := 10;
    FDataChart.LeftAxis.Title.Font.Style     := [fsBold];
    FDataChart.LeftAxis.MinimumOffset        := 10;
    FDataChart.LeftAxis.MaximumOffset        := 10;
    FDataChart.Legend.Visible                := False;

    FDataChart.Title.Text.Clear;
    FDataChart.Title.Caption := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputBoxPlotGraphDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDataChart.StudyHasChanged;
    FViewDataType.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputBoxPlotGraphDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputBoxPlotGraphDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataChart.LanguageHasChanged;
    FViewDataType.LanguageHasChanged;
    FShowDamLevelChkBox.Caption := 'Plot Actual Volume';
    FViewDataLabel.Caption      := FAppModules.Language.GetString('LabelText.ViewData');
    FBtnDataSelection.Caption   := FAppModules.Language.GetString('LabelText.SelectData');
    FBtnLegend.Caption          := FAppModules.Language.GetString('ButtonCaption.OutputPlotGraphLegend');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputBoxPlotGraphDialog.AssignHelpContext;
const OPNAME = 'TOutputBoxPlotGraphDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,           HC_BoxandwhiskerPlots);
    SetControlHelpContext(FViewDataType,  HC_BoxandwhiskerPlots);
    SetControlHelpContext(VCL.Controls.TControl(FDataChart),     HC_BoxandwhiskerPlots);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputBoxPlotGraphDialog.Resize;
const OPNAME = 'TOutputBoxPlotGraphDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 30;

      FViewDataType.Align         := alLeft;
      FViewDataType.Width         := 300;

      FViewDataLabel.Align        := alLeft;
      FViewDataLabel.Width        := 60;
      FViewDataLabel.Layout       := tlCenter;

      FBtnDataSelection.Align     := alRight;
      FBtnDataSelection.Width     := 80;

      FBtnLegend.Width            := FBtnDataSelection.Width;
      FBtnLegend.Top              := FBtnDataSelection.Top;
      FBtnLegend.Height           := FBtnDataSelection.Height;
      FBtnLegend.Left             := FBtnDataSelection.Left - FBtnLegend.Width -10;

      FShowDamLevelChkBox.Left   := FBtnLegend.Left - 200;
      FShowDamLevelChkBox.Top    := FBtnLegend.Top+5;
      FShowDamLevelChkBox.Width  := 200;

      FDataChart.Align            := alNone;
      FDataChart.Align            := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputBoxPlotGraphDialog.CanExport: boolean;
const OPNAME = 'TOutputBoxPlotGraphDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FDataChart)            and (FDataChart.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputBoxPlotGraphDialog.CanPrint: boolean;
const OPNAME = 'TOutputBoxPlotGraphDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FDataChart)            and (FDataChart.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputBoxPlotGraphDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputBoxPlotGraphDialog.DoExport';
begin
  try
    if FDataChart.Visible then
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FDataChart));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputBoxPlotGraphDialog.DoPrint;
const OPNAME = 'TOutputBoxPlotGraphDialog.DoPrint';
begin
  try
    if FDataChart.Visible then FDataChart.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputBoxPlotGraphDialog.CreateBoxPlotSeries: TBoxSeries;
const OPNAME = 'TOutputBoxPlotGraphDialog.CreateBoxPlotSeries';
begin
  Result := nil;
  try
    Result := TBoxSeries.Create(FDataChart);
    Result.ParentChart         := VCLTee.TeEngine.TCustomAxisPanel(FDataChart);
    Result.Box.HorizSize       := 3;
    Result.Marks.Visible       := False;
    Result.Box.Color           := clRed;
    Result.Box.Pen.Color       := clRed;
    Result.ExtrOut.Style       := psCross;
    Result.ExtrOut.HorizSize   := 7;
    Result.ExtrOut.VertSize    := 1;
    Result.ExtrOut.Pen.Color   := clRed;
    Result.MildOut.HorizSize   := 7;
    Result.MildOut.VertSize    := 1;
    Result.MildOut.Style       := psCross;
    Result.MildOut.Brush.Color := clRed;
    Result.MildOut.Pen.Color   := clRed;
    Result.WhiskerPen.Color    := clRed;
    Result.WhiskerPen.Style    := psSolid;
    Result.MedianPen.Style     := psSolid;
    Result.MedianPen.Color     := clBlue;
    Result.XValues.DateTime    := False;
    Result.ExtrOut.Visible     := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
