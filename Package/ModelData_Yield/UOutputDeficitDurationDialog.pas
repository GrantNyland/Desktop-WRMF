
//
//
//  UNIT      : Contains the class TOutputDeficitDurationDialog.
//  AUTHOR    : Oagilwe Segola (ARIVIA)
//  DATE      : 2009/01/22
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputDeficitDurationDialog;

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
  VCL.Graphics,
  VCLTee.TeEngine,
  VCLTee.TeeBoxPlot,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutputDeficitDurationDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel    : TPanel;
    FBarChart        : TFieldChart;
    FBoxPlotChart     : TFieldChart;
    FBarSeries        : TBarSeries;
    FBtnDataSelection : TFieldButton;
    FRadioGrpView     : TRadioGroup;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Resize; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function CreateBoxPlotSeries: TBoxSeries;

    property BarChart         : TFieldChart  read FBarChart;
    property BoxPlotChart     : TFieldChart  read FBoxPlotChart;
    property BarSeries        : TBarSeries   read FBarSeries;
    property BtnDataSelection : TFieldButton read FBtnDataSelection;
    property RadioGrpView     : TRadioGroup  read FRadioGrpView;
  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations;
  
{ TOutputDeficitDurationDialog }

procedure TOutputDeficitDurationDialog.CreateMemberObjects;
const OPNAME = 'TOutputDeficitDurationDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel                := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent         := ControlsParent;

    FBtnDataSelection             := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent      := FSelectorPanel;

    FBarChart                    := TFieldChart.Create(ControlsOwner, FAppModules);
    FBarChart.Parent             := ControlsParent;
    FBarChart.View3D             := False;
    FBarChart.Legend.Visible     := False;
    FBarChart.LeftAxis.TitleSize := 1;

    FBoxPlotChart                    := TFieldChart.Create(ControlsOwner, FAppModules);
    FBoxPlotChart.Parent             := ControlsParent;
    FBoxPlotChart.View3D             := False;
    FBoxPlotChart.Legend.Visible     := False;
    FBoxPlotChart.LeftAxis.TitleSize := 1;
    FBoxPlotChart.Visible            := False;

    FBarSeries                    := TBarSeries.Create(FBarChart);
    FBarChart.AddSeries(FBarSeries);

    FRadioGrpView                 := TRadioGroup.Create(ControlsOwner);
    FRadioGrpView.Parent          := FSelectorPanel;
    FRadioGrpView.Visible         := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationDialog.Initialise: boolean;
const OPNAME = 'TOutputDeficitDurationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBarChart.Initialise;
    FBoxPlotChart.Initialise;
    FRadioGrpView.Items.Clear;
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputDeficitDurationDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FBarChart.StudyHasChanged;
    FBoxPlotChart.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDeficitDurationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDeficitDurationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FBarChart.LanguageHasChanged;
    FBoxPlotChart.LanguageHasChanged;
    FBtnDataSelection.Caption := FAppModules.Language.GetString('LabelText.SelectData');
    FRadioGrpView.Caption     := FAppModules.Language.GetString('OutputGraphType.Caption');
    FRadioGrpView.Items.Clear;
    FRadioGrpView.Items.Add(FAppModules.Language.GetString('OutputGraphType.Bar'));
    FRadioGrpView.Items.Add(FAppModules.Language.GetString('OutputGraphType.BoxPlot'));
    FRadioGrpView.ItemIndex := 0;
    FRadioGrpView.Columns   := 2;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDeficitDurationDialog.Resize;
const OPNAME = 'TOutputDeficitDurationDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 30;

      FBtnDataSelection.Align     := alRight;
      FBtnDataSelection.Width     := 150;

      FBarChart.Align            := alClient;
      FBoxPlotChart.Align         := alClient;
      FRadioGrpView.Align         := alLeft;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDeficitDurationDialog.DestroyMemberObjects;
const OPNAME = 'TOutputDeficitDurationDialog.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputDeficitDurationDialog.CreateBoxPlotSeries: TBoxSeries;
const OPNAME = 'TOutputDeficitDurationDialog.CreateBoxPlotSeries';
begin
  Result := nil;
  try
    Result := TBoxSeries.Create(FBoxPlotChart);
    Result.ParentChart         := VCLTee.TeEngine.TCustomAxisPanel(FBoxPlotChart);
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

