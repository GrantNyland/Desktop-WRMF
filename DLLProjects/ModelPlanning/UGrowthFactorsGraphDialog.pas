{******************************************************************************}
{*  UNIT      : Contains the class TGrowthFactorsGraphDialog   *}
{*  AUTHOR    : Sam M. Dhlamini                                                 *}
{*  DATE      : 2013/11/29                                                    *}
{*  COPYRIGHT : Copyright © 2013 DWAF                                         *}
{******************************************************************************}
unit UGrowthFactorsGraphDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Dialogs,
  VCL.Forms,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TGrowthFactorsGraphDialog = class(TAbstractScrollablePanel)
  private


    FGrowthGraph             : TAbstractChart;
    FFactorsLineSeries       : TLineSeries;
    FDemandLineSeries        : TLineSeries;

    FGrowthTypeRadioGroup     : TRadioGroup;
    FGraphCntrlPanel          : TPanel;

  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property GrowthGraph           : TAbstractChart read FGrowthGraph;
    property FactorsLineSeries     : TLineSeries read FFactorsLineSeries;
    property DemandLineSeries      : TLineSeries read FDemandLineSeries;
    property GrowthTypeRadioGroup  : TRadioGroup read FGrowthTypeRadioGroup;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Grids,
  UErrorHandlingOperations,
 UControlCreationUtilities;

{******************************************************************************}
{* TGrowthFactorsGraphDialog                                              *}
{******************************************************************************}

procedure TGrowthFactorsGraphDialog.CreateMemberObjects;
const OPNAME = 'TGrowthFactorsGraphDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FGraphCntrlPanel             := TPanel.Create(lOwner);
    FGraphCntrlPanel.Parent      := lParent;
    FGraphCntrlPanel.Align       := alTop;
    FGraphCntrlPanel.Height      := 50;
    FGrowthGraph                               := TAbstractChart.Create(lOwner, FAppModules);
    FGrowthGraph.Parent                        := lParent;
    FGrowthGraph.Left                          := FGraphCntrlPanel.Left;
    FGrowthGraph.Top                           := FGraphCntrlPanel.Top +FGraphCntrlPanel.Height+10;
    FGrowthGraph.Align                         := alClient;
    FGrowthGraph.BevelOuter                    := bvNone;
    FGrowthGraph.Legend.Visible                := FALSE;
    FGrowthGraph.AxisVisible                   := TRUE;
    FGrowthGraph.AllowZoom                     := TRUE;
    FGrowthGraph.AllowPanning                  := pmHorizontal;
    FGrowthGraph.Gradient.Visible              := FALSE;
    FGrowthGraph.View3D                        := FALSE;
    FGrowthGraph.Title.Visible                 := TRUE;
    FGrowthGraph.Title.Font.Style              := [fsBold];
    FGrowthGraph.Title.Font.Color              := clBlack;
    FGrowthGraph.LeftAxis.Title.Angle          := 90;
    FGrowthGraph.BottomAxis.LabelsAngle        := 90;
    FGrowthGraph.BottomAxis.DateTimeFormat     := 'yyyy/mm';
    FGrowthGraph.BottomAxis.MinorTicks.Visible := FALSE;



     FFactorsLineSeries              := TLineSeries.Create(FGrowthGraph);
     FFactorsLineSeries.SeriesColor   := clRed;
     FFactorsLineSeries.ParentChart   := FGrowthGraph;
     FFactorsLineSeries.Marks.Visible := FALSE;
     FFactorsLineSeries.LinePen.Width := 2;
     FFactorsLineSeries.XValues.DateTime := True;
     FFactorsLineSeries.Clear;

     FDemandLineSeries              := TLineSeries.Create(FGrowthGraph);
     FDemandLineSeries.SeriesColor   := clBlue;
     FDemandLineSeries.ParentChart   := FGrowthGraph;
     FDemandLineSeries.Marks.Visible := FALSE;
     FDemandLineSeries.LinePen.Width := 2;
     FDemandLineSeries.XValues.DateTime := True;
     FDemandLineSeries.Clear;

     FGraphCntrlPanel.BorderStyle := bsNone;
     FGraphCntrlPanel.BevelInner  := bvNone;
     FGraphCntrlPanel.BevelOuter  := bvNone;



    FGrowthTypeRadioGroup   := TRadioGroup.Create(lOwner);
    FGrowthTypeRadioGroup.Parent  := FGraphCntrlPanel;
    FGrowthTypeRadioGroup.Top := 0;
    FGrowthTypeRadioGroup.Width := 250;
    FGrowthTypeRadioGroup.Left := 0;
    FGrowthTypeRadioGroup.Height := FGraphCntrlPanel.Height-10;
  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphDialog.Resize;
const OPNAME = 'TGrowthFactorsGraphDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphDialog.Initialise: boolean;
const OPNAME = 'TGrowthFactorsGraphDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrowthTypeRadioGroup.Items.Add('Growth Factors :');
    FGrowthTypeRadioGroup.Items.Add('Projection Factors:');
    FGrowthTypeRadioGroup.ItemIndex := 0;
    FGrowthTypeRadioGroup.Columns := 2;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGrowthFactorsGraphDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphDialog.RestoreColourState;
const OPNAME = 'TGrowthFactorsGraphDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphDialog.AssignHelpContext;
const OPNAME = 'TGrowthFactorsGraphDialog.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
