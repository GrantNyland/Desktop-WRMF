{******************************************************************************}
{*  UNIT      : Contains the class TChannelGrowthValidator   *}
{*  AUTHOR    : Sam M. Dhlamini                                                 *}
{*  DATE      : 2013/11/29                                                    *}
{*  COPYRIGHT : Copyright © 2013 DWAF                                         *}
{******************************************************************************}
unit UChannelGrowthDialog;

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

  TChannelGrowthDialog = class(TAbstractScrollablePanel)
  private

    FGridHeadingLabel        : TLabel;
    FFactorLabel             : TLabel;
    FstrgrdFactors           : TFieldStringGrid;
    FGrowthGraph             : TAbstractChart;
    FFactorsLineSeries       : TLineSeries;
    FDemandLineSeries        : TLineSeries;
    //FGraphType1RadioButton   : TRadioButton;
    //FGraphType2RadioButton   : TRadioButton;
    FGraphType               : TRadioGroup;
    FArcRadioGroup           : TRadioGroup;
    FGraphCntrlPanel         : TPanel;
  protected
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property strgrdFactors         : TFieldStringGrid  read FstrgrdFactors;
    property GrowthGraph           : TAbstractChart read FGrowthGraph;
    property FactorsLineSeries     : TLineSeries read FFactorsLineSeries;
    property DemandLineSeries      : TLineSeries read FDemandLineSeries;

    //property GraphType1RadioButton : TRadioButton   read FGraphType1RadioButton;
    //property GraphType2RadioButton : TRadioButton   read FGraphType2RadioButton;
    property GraphType             : TRadioGroup    read FGraphType;
    property FactorLabel           : TLabel         read FFactorLabel;
    property ArcRadioGroup         : TRadioGroup    read FArcRadioGroup;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Grids,
  UErrorHandlingOperations,
 UControlCreationUtilities;

{******************************************************************************}
{* TChannelGrowthDialog                                              *}
{******************************************************************************}

procedure TChannelGrowthDialog.CreateMemberObjects;
const OPNAME = 'TChannelGrowthDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FGraphCntrlPanel             := TPanel.Create(Self);
    FGraphCntrlPanel.Parent      := lParent;
    FGraphCntrlPanel.Height      := 50;
    FGraphCntrlPanel.Align         := alTop;
    //                                                                             Left  Top Width Height


     FGridHeadingLabel               := CreateFieldLabel     (lOwner, lParent,  10, FGraphCntrlPanel.Height, 140,  21);
    with FGridHeadingLabel do
    begin
      Alignment := taLeftJustify;
    end;

     FFactorLabel               := CreateFieldLabel     (lOwner, lParent,  10, FGraphCntrlPanel.Height+30, 140,  21);
    with FFactorLabel do
    begin
      Alignment := taLeftJustify;
    end;

    FstrgrdFactors        := CreateFieldStringGrid(FAppModules, lOwner, lParent,   160,  FGraphCntrlPanel.Height, 55, 48, 0, TRUE);
    with FstrgrdFactors do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;




    FGrowthGraph                               := TAbstractChart.Create(Self, FAppModules);
    FGrowthGraph.Parent                        := lParent;
    FGrowthGraph.Left                          := 160;
    FGrowthGraph.Top                           := FstrgrdFactors.Top + FstrgrdFactors.Height+10;
    FGrowthGraph.width                         := 850;
    FGrowthGraph.Height                        := 200;
  //  FGrowthGraph.Align                         := alClient;
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
   // FGrowthGraph.BottomAxis.TickLength         := 6;
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


   {

    FGraphType1RadioButton         := TRadioButton.Create(Self);
    FGraphType1RadioButton.Parent  := FGraphCntrlPanel;
    FGraphType1RadioButton.Top     := 5;
    FGraphType1RadioButton.Left    := 10;
    FGraphType1RadioButton.Width   := 115;
    FGraphType1RadioButton.Caption := 'Growth Factors';

    FGraphType2RadioButton         := TRadioButton.Create(Self);
    FGraphType2RadioButton.Parent  := FGraphCntrlPanel;
    FGraphType2RadioButton.Top     := 5;
    FGraphType2RadioButton.Left    := 130;
    FGraphType2RadioButton.Caption := 'Demand Projections';

    }
    FGraphType               := TRadioGroup.Create(lOwner);
    FGraphType.Parent  := lParent;
    FGraphType.Top  := 0;
    FGraphType.Width := 250;
    FGraphType.Left := 160;
    FGraphType.Height := FGraphCntrlPanel.Height-10;

    FArcRadioGroup     := TRadioGroup.Create(lOwner);
    FArcRadioGroup.Parent  := lParent;
    FArcRadioGroup.Top := 0;
    FArcRadioGroup.Left  := FGraphType.Left + FGraphType.Width + 10;
    FArcRadioGroup.Height := FGraphCntrlPanel.Height-10;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthDialog.Resize;
const OPNAME = 'TChannelGrowthDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthDialog.Initialise: boolean;
const OPNAME = 'TChannelGrowthDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    FGraphType.Items.Add('Growth Factors');
    FGraphType.Items.Add('Demand Projections');

    FGraphType.ItemIndex := 0;
    FGraphType.Columns := 2;
    FArcRadioGroup.Visible := False;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChannelGrowthDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    FGridHeadingLabel.Caption               := FAppModules.Language.GetString('TField.Year') + ' :';
    FFactorLabel.Caption := 'Growth Factors : ';


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthDialog.RestoreColourState;
const OPNAME = 'TChannelGrowthDialog.RestoreColourState';
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

procedure TChannelGrowthDialog.AssignHelpContext;
const OPNAME = 'TChannelGrowthDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self                           , HC_TimeSeriesDemands);

    SetControlHelpContext(FstrgrdFactors             , HC_TimeSeriesDemands);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
