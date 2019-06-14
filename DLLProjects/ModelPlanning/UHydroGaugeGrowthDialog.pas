{******************************************************************************}
{*  UNIT      : Contains the class THydroGaugeGrowthDialog   *}
{*  AUTHOR    : Sam M. Dhlamini                                                 *}
{*  DATE      : 2013/11/29                                                    *}
{*  COPYRIGHT : Copyright © 2013 DWAF                                         *}
{******************************************************************************}
unit UHydroGaugeGrowthDialog;

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

  THydroGaugeGrowthDialog = class(TAbstractScrollablePanel)
  private

    FGridHeadingLabel        : TLabel;
    FFactorLabel             : TLabel;
    FstrgrdFactors           : TFieldStringGrid;
    FGrowthGraph             : TAbstractChart;
    FFactorsLineSeries       : TLineSeries;
    FDemandLineSeries        : TLineSeries;
   //FGraphType1RadioButton   : TRadioButton;
   // FGraphType2RadioButton   : TRadioButton;

    FFileRadioGroup           : TRadioGroup;
    FGrowthTypeRadioGroup     : TRadioGroup;
    FGraphCntrlPanel          : TPanel;
    FGauges                   : TListBox;
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

    property FileRadioGroup           : TRadioGroup read FFileRadioGroup;
    property GrowthTypeRadioGroup     : TRadioGroup read FGrowthTypeRadioGroup;
    property FactorLabel              : TLabel         read FFactorLabel;
    property GaugeListBox             : TListBox    read  FGauges;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Grids,
  UErrorHandlingOperations,
 UControlCreationUtilities;

{******************************************************************************}
{* THydroGaugeGrowthDialog                                              *}
{******************************************************************************}

procedure THydroGaugeGrowthDialog.CreateMemberObjects;
const OPNAME = 'THydroGaugeGrowthDialog.CreateMemberObjects';
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
    FGraphCntrlPanel.Align       := alTop;
    FGraphCntrlPanel.Height      := 50;
   // FGraphCntrlPanel.Width       := 750;

     FGauges                   := TListBox.Create(lOwner);
     FGauges.Parent            := lParent;
     FGauges.Left              := 10;
     FGauges.Width             := 200;
     FGauges.Height            := 300;
     FGauges.Top               := FGraphCntrlPanel.Height+5;

    //                                                                             Left  Top Width Height

    FGridHeadingLabel               := CreateFieldLabel     (lOwner, lParent,  FGauges.Width +30, FGraphCntrlPanel.Height+10, 140,  21);
    with FGridHeadingLabel do
    begin
      Alignment := taLeftJustify;
    end;

    FFactorLabel               := CreateFieldLabel     (lOwner, lParent,  FGauges.Width +30, FGraphCntrlPanel.Height+30, 140,  21);
    with FFactorLabel do
    begin
      Alignment := taLeftJustify;
    end;

    FstrgrdFactors        := CreateFieldStringGrid(FAppModules, lOwner, lParent,   FGauges.Width +140,  FGraphCntrlPanel.Height+10, 55, 48, 0, TRUE);
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
    FGrowthGraph.Left                          := FstrgrdFactors.Left;
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


          (*

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

    FArcRadioGroup     := TRadioGroup.Create(lOwner);
    FArcRadioGroup.Parent  := lParent;
    FArcRadioGroup.Top  := FGraphCntrlPanel.Height + FGraphCntrlPanel.Top+ 5;
    FArcRadioGroup.Left    := FGraphCntrlPanel.Left; //FGraphType1RadioButton.Width + FGraphType2RadioButton.Width + 10;
    FArcRadioGroup.Height      := 40;
                    *)
    FGrowthTypeRadioGroup   := TRadioGroup.Create(Self);
    FGrowthTypeRadioGroup.Parent  := FGraphCntrlPanel;
    FGrowthTypeRadioGroup.Top := 0;
    FGrowthTypeRadioGroup.Width := 250;
    FGrowthTypeRadioGroup.Left := 0;
    FGrowthTypeRadioGroup.Height := FGraphCntrlPanel.Height-10;

    FFileRadioGroup         :=  TRadioGroup.Create(Self);
    FFileRadioGroup.Parent  := FGraphCntrlPanel;
    FFileRadioGroup.Top := 0;
    FFileRadioGroup.Left  := FGrowthTypeRadioGroup.Width + 10;
    FFileRadioGroup.Height := FGraphCntrlPanel.Height-10;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthDialog.Resize;
const OPNAME = 'THydroGaugeGrowthDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthDialog.Initialise: boolean;
const OPNAME = 'THydroGaugeGrowthDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    //FGraphType1RadioButton.Checked := True;
    FGrowthTypeRadioGroup.Items.Add('Growth Factors :');
    FGrowthTypeRadioGroup.Items.Add('Demand Projections :');
    FGrowthTypeRadioGroup.ItemIndex := 0;
    FGrowthTypeRadioGroup.Columns := 2;

    FFileRadioGroup.Items.Add('AFF');
    FFileRadioGroup.Items.Add('IRR');
    FFileRadioGroup.Items.Add('URB');
    FFileRadioGroup.ItemIndex := 0;
    FFileRadioGroup.Columns := 3;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthDialog.LanguageHasChanged: boolean;
const OPNAME = 'THydroGaugeGrowthDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    FGridHeadingLabel.Caption               := FAppModules.Language.GetString('TField.Year') + ' :';
    FFactorLabel.Caption := 'Growth Factors : ';


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthDialog.RestoreColourState;
const OPNAME = 'THydroGaugeGrowthDialog.RestoreColourState';
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

procedure THydroGaugeGrowthDialog.AssignHelpContext;
const OPNAME = 'THydroGaugeGrowthDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self                           , HC_TimeSeriesDemands);

    SetControlHelpContext(FstrgrdFactors             , HC_TimeSeriesDemands);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
