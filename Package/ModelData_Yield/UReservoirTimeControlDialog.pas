{******************************************************************************}
{*  UNIT      : Contains the class TReservoirTimeControlDialog.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/02/24                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UReservoirTimeControlDialog;

interface

uses
  Classes,
  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Forms,


  VCLTee.GanttCh,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TReservoirTimeControlDialog = class(TAbstractScrollablePanel)
  private
  protected
    FEconomicDataGBox       : TScrollBox;
    FDateActiveG1Box        : TScrollBox;
    FDateActiveG2Box        : TScrollBox;

    FLblEconomicLife      : TLabel;
    FEdtEconomicLife      : TFieldEdit;
    FLblEconomicLifeUnits : TLabel;
    FLblCapitalCost       : TLabel;
    FEdtCapitalCost       : TFieldEdit;
    FLblCapitalCostUnits  : TLabel;
    FLblOandMCost         : TLabel;
    FEdtOandMCost         : TFieldEdit;
    FLblOandMCostUnits    : TLabel;
    FLblNrYearsConstruct  : TLabel;
    FEdtNrYearsConstruct  : TFieldEdit;
    FLblYear              : TLabel;
    FLblCostSchedule      : TLabel;
    FGrdCostSchedule      : TFieldStringGrid;
    FPnlChild             : TPanel;
    FLblBaseNodeNumber    : TLabel;
    FBaseNodeNumberCbx    : TFieldComboBox;

    FLblStartDate         : TLabel;
    FStartDateYearCbx     : TFieldComboBox;
    FStartDateMonthCbx    : TFieldComboBox;
//    FLblStartDateVal      : TLabel;

    FLblEndDate           : TLabel;
    FEndDateYearCbx       : TFieldComboBox;
    FEndDateMonthCbx      : TFieldComboBox;
//    FLblEndDateVal        : TLabel;
    
    //FPnlParent            : TPanel;
    FLblReplacements      : TLabel;
    FGrdDates             : TFieldStringGrid;
    FCbxStartYear         : TFieldComboBox;
    FCbxStartMonth        : TFieldComboBox;
    FCbxEndYear           : TFieldComboBox;
    FCbxEndMonth          : TFieldComboBox;
    FChartPanel           : TPanel;
    FRecordLengthChart    : TAbstractChart;
    FChartSeries          : TGanttSeries;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnGridResize(Sender: TObject);
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;

    property EdtEconomicLife      : TFieldEdit       read FEdtEconomicLife;
    property EdtCapitalCost       : TFieldEdit       read FEdtCapitalCost;
    property EdtOandMCost         : TFieldEdit       read FEdtOandMCost;
    property EdtNrYearsConstruct  : TFieldEdit       read FEdtNrYearsConstruct;
    property GrdCostSchedule      : TFieldStringGrid read FGrdCostSchedule;
    property BaseNodeNumberCbx    : TFieldComboBox   read FBaseNodeNumberCbx;

    property StartDateYearCbx     : TFieldComboBox   read FStartDateYearCbx;
    property StartDateMonthCbx    : TFieldComboBox   read FStartDateMonthCbx;
//    property LblStartDateVal      : TLabel           read FLblStartDateVal;

    property EndDateYearCbx       : TFieldComboBox   read FEndDateYearCbx;
    property EndDateMonthCbx      : TFieldComboBox   read FEndDateMonthCbx;
//    property LblEndDateVal        : TLabel           read FLblEndDateVal;

    property PnlChild             : TScrollBox           read FDateActiveG1Box;
    property PnlParent            : TScrollBox           read FDateActiveG2Box;
    property GrdDates             : TFieldStringGrid read FGrdDates;
    property CbxStartYear         : TFieldComboBox   read FCbxStartYear;
    property CbxStartMonth        : TFieldComboBox   read FCbxStartMonth;
    property CbxEndYear           : TFieldComboBox   read FCbxEndYear;
    property CbxEndMonth          : TFieldComboBox   read FCbxEndMonth;
    property ChartSeries          : TGanttSeries     read FChartSeries;
    property ChartPanel           : TPanel           read FChartPanel;
    property RecordLengthChart : TAbstractChart read FRecordLengthChart;
  end;

implementation

uses
  SysUtils,
  VCL.Grids,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{******************************************************************************}
{* TReservoirTimeControlDialog                                                *}
{******************************************************************************}

procedure TReservoirTimeControlDialog.CreateMemberObjects;
const OPNAME = 'TReservoirTimeControlDialog.CreateMemberObjects';
var
  LOwner  : TComponent;
  LParent : TWinControl;
begin
  inherited;
  try
    LOwner  := ControlsOwner;
    LParent := ControlsParent;

    FEconomicDataGBox        := TScrollBox.Create(lOwner);
    FDateActiveG1Box         := TScrollBox.Create(lOwner);
    FDateActiveG2Box         := TScrollBox.Create(lOwner);

    FEconomicDataGBox.Parent := lParent;
    FDateActiveG1Box.Parent  := lParent;
    FDateActiveG2Box.Parent  := lParent;

    FEconomicDataGBox.Align  := alTop;
    FDateActiveG1Box.Align   := alTop;
    FDateActiveG2Box.Align   := alTop;

    FEconomicDataGBox.Height := 200;
    FDateActiveG1Box.Height  := 100;
    FDateActiveG2Box.Height  := 300;


    FLblEconomicLife := TLabel.Create(lOwner);
    with FLblEconomicLife do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Alignment := taRightJustify;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 10;
      Width     := 160;
      Height    := 21;
    end;

    FEdtEconomicLife := TFieldEdit.Create(LOwner, FAppModules);
    with FEdtEconomicLife do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 180;
      Top       := 10;
      Width     := 70;
      Height    := 21;
    end;

    FLblEconomicLifeUnits := TLabel.Create(LOwner);
    with FLblEconomicLifeUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 255;
      Top       := 10;
      Width     := 40;
      Height    := 21;
    end;

    FLblCapitalCost := TLabel.Create(LOwner);
    with FLblCapitalCost do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 40;
      Width     := 160;
      Height    := 21;
    end;

    FEdtCapitalCost := TFieldEdit.Create(LOwner, FAppModules);
    with FEdtCapitalCost do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 180;
      Top       := 40;
      Width     := 70;
      Height    := 21;
    end;

    FLblCapitalCostUnits := TLabel.Create(LOwner);
    with FLblCapitalCostUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 255;
      Top       := 40;
      Width     := 50;
      Height    := 21;
    end;

    FLblOandMCost := TLabel.Create(LOwner);
    with FLblOandMCost do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 70;
      Width     := 160;
      Height    := 21;
    end;

    FEdtOandMCost := TFieldEdit.Create(LOwner, FAppModules);
    with FEdtOandMCost do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 180;
      Top       := 70;
      Width     := 70;
      Height    := 21;
    end;

    FLblOandMCostUnits := TLabel.Create(LOwner);
    with FLblOandMCostUnits do
    begin
      Parent    := FEconomicDataGBox;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 255;
      Top       := 70;
      Width     := 85;
      Height    := 21;
    end;

    FLblNrYearsConstruct := TLabel.Create(LOwner);
    with FLblNrYearsConstruct do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 100;
      Width     := 160;
      Height    := 21;
    end;

    FEdtNrYearsConstruct := TFieldEdit.Create(LOwner, FAppModules);
    with FEdtNrYearsConstruct do
    begin
      Parent    := FEconomicDataGBox;
      Left      := 180;
      Top       := 100;
      Width     := 70;
      Height    := 21;
    end;

    FLblYear := TLabel.Create(LOwner);
    with FLblYear do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 130;
      Width     := 160;
      Height    := 21;
    end;

    FLblCostSchedule := TLabel.Create(LOwner);
    with FLblCostSchedule do
    begin
      Parent    := FEconomicDataGBox;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 10;
      Top       := 150;
      Width     := 160;
      Height    := 21;
    end;

    FGrdCostSchedule := TFieldStringGrid.Create(LOwner, FAppModules);
    with FGrdCostSchedule do
    begin
      Parent           := FEconomicDataGBox;
      Left             := 180;
      Top              := 130;
      Width            := 186;
      Height           := 46;
      ColCount         := 2;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    end;

    {FPnlChild             := TPanel.Create(LOwner);
    with FPnlChild do
    begin
      Parent     := LParent;
      Left       := 5;
      Top        := 180;
      Width      := 450;
      Height     := 100;
      BevelOuter := VCL.Controls.TBevelCut(bvNone);
    end;}

    FLblBaseNodeNumber := TLabel.Create(LOwner);
    with FLblBaseNodeNumber do
    begin
      Parent    := FDateActiveG1Box;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 5;
      Top       := 5;
      Width     := 160;
      Height    := 21;
    end;

    FLblStartDate := TLabel.Create(LOwner);
    with FLblStartDate do
    begin
      Parent    := FDateActiveG1Box;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 5;
      Top       := 30;
      Width     := 160;
      Height    := 21;
    end;

    FLblEndDate := TLabel.Create(LOwner);
    with FLblEndDate do
    begin
      Parent    := FDateActiveG1Box;
      Alignment := taRightJustify;
      AutoSize  := FALSE;
      Layout    := tlCenter;
      Left      := 5;
      Top       := 55;
      Width     := 160;
      Height    := 21;
    end;

    FBaseNodeNumberCbx     := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG1Box,  175,  5,  200, 21, 3, TRUE, csDropDownList);
    FStartDateYearCbx      := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG1Box,  175,  30,  80, 21, 3, TRUE, csDropDownList);
    FStartDateMonthCbx     := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG1Box,  260,  30,  80, 21, 3, TRUE, csDropDownList);
    FEndDateYearCbx        := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG1Box,  175,  60,  80, 21, 3, TRUE, csDropDownList);
    FEndDateMonthCbx       := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG1Box,  260,  60,  80, 21, 3, TRUE, csDropDownList);

    FStartDateYearCbx.DropDownCount   := 25;
    FEndDateYearCbx.DropDownCount     := 25;

    {FPnLParent := TPanel.Create(LOwner);
    with FPnLParent do
    begin
      Parent     := LParent;
      Left       := 5;
      Top        := 180;
      Width      := 450;
      Height     := 150;
      BevelOuter := VCL.Controls.TBevelCut(bvNone);
    end;}
    
    FLblReplacements := TLabel.Create(LOwner);
    with FLblReplacements do
    begin
      Parent    := FDateActiveG2Box;
      Left      := 5;
      Top       := 5;
      Width     := 160;
      Height    := 21;
    end;
    FGrdDates := CreateFieldStringGrid(FAppModules, LOwner, FDateActiveG2Box,
                                         5, 25, 427, 109, 0, TRUE);
    with FGrdDates do
    begin
      ColCount         := 5;
      RowCount         := 2;
      FixedCols        := 0;
      FixedRows        := 1;
      DefaultRowHeight := 20;
      ScrollBars       := ssBoth;
      DefaultColWidth  := 80;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
      OnResize         := OnGridResize;
    end;

    FCbxStartYear          := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG2Box,  12,  81,  82, 21, 3, TRUE, csDropDownList);
    FCbxStartMonth         := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG2Box, 168,  81,  82, 21, 3, TRUE, csDropDownList);
    FCbxEndYear            := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG2Box,  12,  81,  82, 21, 3, TRUE, csDropDownList);
    FCbxEndMonth           := CreateFieldComboBox(FAppModules, LOwner, FDateActiveG2Box, 168,  81,  82, 21, 3, TRUE, csDropDownList);

    FCbxStartYear.DropDownCount   := 25;
    FCbxEndYear.DropDownCount     := 25;

    FChartPanel := TPanel.Create(LOwner);
    FChartPanel.Parent      := FDateActiveG2Box;
    FChartPanel.Width       := FGrdDates.Width;
    FChartPanel.Top         := 250;
    FChartPanel.Height      := 180;
    FChartPanel.Left        := 6;
    FChartPanel.BorderStyle := bsNone;
    FChartPanel.BevelInner  := bvNone;
    FChartPanel.BevelOuter  := bvNone;

    FRecordLengthChart                  := TAbstractChart.Create(Self, FAppModules);
    FRecordLengthChart.Parent           := FChartPanel;
    FRecordLengthChart.Align            := alClient;
    FRecordLengthChart.BevelOuter       := bvNone;
    FRecordLengthChart.Title.Font.Color := clBlack;
    FRecordLengthChart.Title.Font.Style := [fsBold];
    FRecordLengthChart.View3D           := False;
    FRecordLengthChart.Legend.Visible   := False;
    FRecordLengthChart.BottomAxis.AxisValuesFormat   := '';
    FRecordLengthChart.BottomAxis.Increment          := 2;
    FRecordLengthChart.BottomAxis.LabelsAngle        := 90;
    FRecordLengthChart.LeftAxis.SetMinMax(0,FGrdDates.ColCount + 2);
    FRecordLengthChart.BottomAxis.MinorTicks.Visible := FALSE;

    FChartSeries := TGanttSeries.Create(FRecordLengthChart);
    FChartSeries.XValues.DateTime := FALSE;
    FRecordLengthChart.AddSeries(FChartSeries);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlDialog.Initialise: boolean;
const OPNAME = 'TReservoirTimeControlDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FCbxStartYear.Visible     := FALSE;
    FCbxStartMonth.Visible    := FALSE;
    FCbxEndYear.Visible       := FALSE;
    FCbxEndMonth.Visible      := FALSE;
    FEconomicDataGBox.Visible := FALSE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlDialog.Resize;
const OPNAME = 'TReservoirTimeControlDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlDialog.OnGridResize(Sender: TObject);
const OPNAME = 'TReservoirTimeControlDialog.OnGridResize';
begin
  try
    FChartPanel.Top          := GrdDates.Top + GrdDates.Height + 5;
    FChartPanel.Width        := FGrdDates.Width;
    FDateActiveG2Box.Height  := FChartPanel.Top + FChartPanel.Height + 15;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirTimeControlDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLblEconomicLife.Caption      := FAppModules.Language.GetString('LabelCaption.EconomicLife');
    FLblEconomicLifeUnits.Caption := FAppModules.Language.GetString('LabelCaption.EconomicLifeUnits');
    FLblCapitalCost.Caption       := FAppModules.Language.GetString('LabelCaption.CapitalCost');
    FLblCapitalCostUnits.Caption  := FAppModules.Language.GetString('LabelCaption.CapitalCostUnits');
    FLblOandMCost.Caption         := FAppModules.Language.GetString('LabelCaption.OandMCost');
    FLblOandMCostUnits.Caption    := FAppModules.Language.GetString('LabelCaption.OandMCostUnits');
    FLblNrYearsConstruct.Caption  := FAppModules.Language.GetString('LabelCaption.NrYearsConstruct');
    FLblYear.Caption              := FAppModules.Language.GetString('LabelCaption.Year');
    FLblCostSchedule.Caption      := FAppModules.Language.GetString('LabelCaption.CostSchedule');

    FLblBaseNodeNumber.Caption := FAppModules.Language.GetString('LabelCaption.BaseNodeNumber');
    FLblStartDate.Caption      := FAppModules.Language.GetString('Weather.StartDate');
    FLblEndDate.Caption        := FAppModules.Language.GetString('Weather.EndDate');
    FLblReplacements.Caption   := FAppModules.Language.GetString('LabelCaption.Replacements');

    FGrdDates.Cells[0, 0] := FAppModules.Language.GetString('GridHeading.Replacement');
    FGrdDates.Cells[1, 0] := FAppModules.Language.GetString('TField.AllocDefStartYearDescr');
    FGrdDates.Cells[2, 0] := FAppModules.Language.GetString('TField.AllocDefStartMonthDescr');
    FGrdDates.Cells[3, 0] := FAppModules.Language.GetString('TField.AllocDefEndYearDescr');
    FGrdDates.Cells[4, 0] := FAppModules.Language.GetString('TField.AllocDefEndMonthDescr');
//    FNodenumberLabel.Caption      := FAppModules.Language.GetString('TField.NodeCount');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlDialog.RestoreColourState;
const OPNAME = 'TReservoirTimeControlDialog.RestoreColourState';
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

procedure TReservoirTimeControlDialog.AssignHelpContext;
const OPNAME = 'TReservoirTimeControlDialog.AssignHelpContext';
begin
  try
{
    SetControlHelpContext(Self,                   HC_PhysicalReservoirCharacteristics);
    SetControlHelpContext(FSelectPenaltyStruct,   HC_ReservoirPenaltyStructures);
}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

