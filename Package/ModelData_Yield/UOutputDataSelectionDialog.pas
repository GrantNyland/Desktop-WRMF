//
//
//  UNIT      : Contains the class TOutputDataSelectionDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/10/27
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputDataSelectionDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UDataComponent;

type

  TOutputDataSelectionDialog = class(TAbstractScrollablePanel)
  protected
    FPnlTop           : TPanel;
    FLblLoadCase      : TLabel;
    FLblSequence      : TLabel;
    FLblMonth         : TLabel;

    FLoadCaseSelector : TLoadCaseSelectorNavigator;
    FSequenceSelector : TSequenceSelectorNavigator;
    FMonthSelector    : TMonthSelectorNavigator;
    FLblCalendarDate  : TLabel;

    FSelectMonthCombobox   : TComboBox;
    FDecisionMonthCombobox : TComboBox;
    FSelectMonthlabel      : TLabel;

    FBtnMoreLess      : TButton;
    FPnlBottom        : TPanel;
    FLblUnits         : TLabel;
    FLblTimeStep      : TLabel;
    FLblValueType     : TLabel;

    FUnitsGroupBox    : TGroupBox;
    FMetersPerSecond  : TRadioButton;
    FMillion          : TRadioButton;

    FPercentage       : TRadioButton;
    FLivePercentage   : TRadioButton;

    FMeters           : TRadioButton;
    FMCM              : TRadioButton;
    FMegaLitersPerDay : TRadioButton;

    FTimestepGroupBox   : TGroupBox;
    FMonthly            : TRadioButton;
    FAnnually           : TRadioButton;
    FMonthlyCummulative : TRadioButton;
    FAnnualCummulative  : TRadioButton;
    FSequence           : TRadioButton;

    FValueTypeGroupBox : TGroupBox;
    FSupply            : TRadioButton;
    FDemand            : TRadioButton;
    FDeficit           : TRadioButton;
    FDemandAndSupply   : TRadioButton;
    FAllocated         : TRadioButton;

    FChkHighlight      : TCheckBox;

    FLblPeriodSelect      : TLabel;
    FPeriodSelectGroupBox : TGroupBox;
    FPeriodSequence       : TRadioButton;
    FPeriodSelection      : TRadioButton;
    FLblPeriodStart       : TLabel;
    FDatePeriodStart      : TDateTimePicker;
    FLblPeriodEnd         : TLabel;
    FDatePeriodEnd        : TDateTimePicker;

    lblPlotOptions        : TLabel;
    FgbCondenced          : TGroupBox;
    FlblCondenced         : TLabel;
    FCondenced            : TRadioButton;
    FNonCondenced         : TRadioButton;
    FlblCumulative        : TLabel;
    FCumulative           : TRadioButton;
    FNonCumulative        : TRadioButton;
    FlblYearsToSkip       : TLabel;
    FedtYearsToSkip       : TEdit;

    FLblSensitivityBox      : TLabel;
    FSensitivityGroupBox : TGroupBox;
    FApplySensitivity    : TRadioButton;
    FAbsoluteSensitivity : TRadioButton;
    FPercSensitivity     : TRadioButton;
    FedtSensitivity      : TEdit;

    FedtPercSensitivity      : TEdit;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function ToggleMoreLess : Boolean;
    procedure SetPeriodControlsState(AEnabled:boolean);
    procedure SetPlotOptionsState(AEnabled:boolean);
    property LoadCaseSelector     : TLoadCaseSelectorNavigator   read FLoadCaseSelector;
    property SequenceSelector     : TSequenceSelectorNavigator   read FSequenceSelector;
    property MonthSelector        : TMonthSelectorNavigator      read FMonthSelector;
    property LblMonth             : TLabel                       read FLblMonth;
    property LblCalendarDate      : TLabel                       read FLblCalendarDate;
    property SelectMonthCombobox  : TComboBox               read FSelectMonthCombobox;
    property SelectMonthlabel      : TLabel                 read FSelectMonthlabel;
    property DecisionMonthCombobox: TComboBox               read FDecisionMonthCombobox;
    property MetersPerSecond      : TRadioButton            read FMetersPerSecond;
    property Million              : TRadioButton            read FMillion ;
    property Percentage           : TRadioButton            read FPercentage;
    property LivePercentage       : TRadioButton            read FLivePercentage;

    property Meters               : TRadioButton            read FMeters;
    property MCM                  : TRadioButton            read FMCM;
    property MegaLitersPerDay     : TRadioButton            read FMegaLitersPerDay;

    property Monthly              : TRadioButton            read FMonthly;
    property Annually             : TRadioButton            read FAnnually;
    property MonthlyCummulative   : TRadioButton            read FMonthlyCummulative;
    property AnnualCummulative    : TRadioButton            read FAnnualCummulative;
    property Sequence             : TRadioButton            read FSequence;

    property Supply               : TRadioButton            read FSupply;
    property Demand               : TRadioButton            read FDemand;
    property Deficit              : TRadioButton            read FDeficit;
    property DemandAndSupply      : TRadioButton            read FDemandAndSupply;
    property Allocated            : TRadioButton            read FAllocated;

    property ChkHighlight         : TCheckBox               read FChkHighlight;
    property BtnMoreLess          : TButton                 read FBtnMoreLess;

    property PeriodSequence       : TRadioButton           read FPeriodSequence;
    property PeriodSelection      : TRadioButton           read FPeriodSelection;
    property DatePeriodStart      : TDateTimePicker        read FDatePeriodStart;
    property DatePeriodEnd        : TDateTimePicker        read FDatePeriodEnd;

    property lblCondenced         : TLabel                 read FlblCondenced;
    property lblCumulative        : TLabel                 read FlblCumulative;

    property Condenced            : TRadioButton           read FCondenced;
    property NonCondenced         : TRadioButton           read FNonCondenced;

    property Cumulative           : TRadioButton           read FCumulative;
    property NonCumulative        : TRadioButton           read FNonCumulative;

    property lblYearsToSkip       : TLabel                 read FlblYearsToSkip;
    property edtYearsToSkip       : TEdit                  read FedtYearsToSkip;
    property ApplySensitivity     : TRadioButton           read FApplySensitivity;
    property AbsoluteSensitivity  : TRadioButton           read FAbsoluteSensitivity;
    property PercSensitivity      : TRadioButton           read FPercSensitivity;

    property edtSensitivity       : TEdit                  read FedtSensitivity;
    property edtPercSensitivity   : TEdit                  read FedtPercSensitivity;
    property LblSensitivityBox    : TLabel                 read FLblSensitivityBox;
    property SensitivityGroupBox  : TGroupBox              read FSensitivityGroupBox;

  end;

implementation

uses
  System.UITypes,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TOutputDataSelectionDialog }
procedure TOutputDataSelectionDialog.CreateMemberObjects;
const OPNAME = 'TOutputDataSelectionDialog.CreateMemberObjects';
begin
  inherited;
  try

    FPnlTop                     := TPanel.Create(ControlsOwner);
    FPnlTop.Parent              := ControlsParent;
    FpnlTop.BevelOuter          := bvNone;
    FpnlTop.BevelInner          := bvLowered;

    FLblLoadCase                := TLabel.Create(ControlsOwner);
    FLblLoadCase.Parent         := FPnlTop;

    FLoadCaseSelector           := TLoadCaseSelectorNavigator.Create(ControlsOwner,FAppModules);
    FLoadCaseSelector.Parent    := FPnlTop;

    FLblSequence                := TLabel.Create(ControlsOwner);
    FlblSequence.Parent         := FPnlTop;

    FSequenceSelector           := TSequenceSelectorNavigator.Create(ControlsOwner,FAppModules);
    FSequenceSelector.Parent    := FPnlTop;

    FLblMonth                   := TLabel.Create(ControlsOwner);
    FlblMonth.Parent            := FPnlTop;

    FMonthSelector              := TMonthSelectorNavigator.Create(ControlsOwner,FAppModules);
    FMonthSelector.Parent       := FPnlTop;

    FLblCalendarDate            := TLabel.Create(ControlsOwner);
    FLblCalendarDate.Parent     := FPnlTop;
    FLblCalendarDate.Font.Color := clRed;
    FLblCalendarDate.Font.Style := FLblCalendarDate.Font.Style + [fsBold];

    FSelectMonthlabel           := TLabel.Create(ControlsOwner);
    FSelectMonthlabel.Parent    := FPnlTop;

    FSelectMonthCombobox        := TComboBox.Create(ControlsOwner);
    FSelectMonthCombobox.Parent := FPnlTop;

    FDecisionMonthCombobox        := TComboBox.Create(ControlsOwner);
    FDecisionMonthCombobox.Parent := FPnlTop;

    FBtnMoreLess                := TButton.Create(ControlsOwner);
    FBtnMoreLess.Parent         := FPnlTop;

    FPnlBottom                  := TPanel.Create(ControlsOwner);
    FPnlBottom.Parent           := ControlsParent;
    FPnlBottom.BevelOuter       := bvNone;
    FPnlBottom.BevelInner       := bvLowered;

    FLblUnits                   := TLabel.Create(ControlsOwner);
    FLblUnits.Parent            := FPnlBottom;

    FLblTimeStep                := TLabel.Create(ControlsOwner);
    FLblTimeStep.Parent         := FPnlBottom;

    FLblValueType               := TLabel.Create(ControlsOwner);
    FLblValueType.Parent        := FPnlBottom;

    FUnitsGroupBox              := TGroupBox.Create(ControlsOwner);
    FUnitsGroupBox.Parent       := FPnlBottom;

    FMetersPerSecond            := TRadioButton.Create(ControlsOwner);
    FMetersPerSecond.Parent     := FUnitsGroupBox;

    FMillion                    := TRadioButton.Create(ControlsOwner);
    FMillion.Parent             := FUnitsGroupBox;

    FPercentage                 := TRadioButton.Create(ControlsOwner);
    FPercentage.Parent          := FUnitsGroupBox;

    FLivePercentage             := TRadioButton.Create(ControlsOwner);
    FLivePercentage.Parent      := FUnitsGroupBox;

    FMeters                     := TRadioButton.Create(ControlsOwner);
    FMeters.Parent              := FUnitsGroupBox;

    FMCM                        := TRadioButton.Create(ControlsOwner);
    FMCM.Parent                 := FUnitsGroupBox;

    FMegaLitersPerDay           := TRadioButton.Create(ControlsOwner);
    FMegaLitersPerDay.Parent    := FUnitsGroupBox;

    FTimestepGroupBox           := TGroupBox.Create(ControlsOwner);
    FTimestepGroupBox.Parent    := FPnlBottom;

    FMonthly                    := TRadioButton.Create(ControlsOwner);
    FMonthly.Parent             := FTimestepGroupBox;

    FAnnually                   := TRadioButton.Create(ControlsOwner);
    FAnnually.Parent            := FTimestepGroupBox;

    FMonthlyCummulative         := TRadioButton.Create(ControlsOwner);
    FMonthlyCummulative.Parent  := FTimestepGroupBox;

    FAnnualCummulative          := TRadioButton.Create(ControlsOwner);
    FAnnualCummulative.Parent   := FTimestepGroupBox;

    FSequence                   := TRadioButton.Create(ControlsOwner);
    FSequence.Parent            := FTimestepGroupBox;

    FValueTypeGroupBox          := TGroupBox.Create(ControlsOwner);
    FValueTypeGroupBox.Parent   := FPnlBottom;

    FSupply                     := TRadioButton.Create(ControlsOwner);
    FSupply.Parent              := FValueTypeGroupBox;

    FDemand                     := TRadioButton.Create(ControlsOwner);
    FDemand.Parent              := FValueTypeGroupBox;

    FDeficit                    := TRadioButton.Create(ControlsOwner);
    FDeficit.Parent             := FValueTypeGroupBox;

    FDemandAndSupply            := TRadioButton.Create(ControlsOwner);
    FDemandAndSupply.Parent     := FValueTypeGroupBox;

    FAllocated                  := TRadioButton.Create(ControlsOwner);
    FAllocated.Parent           := FValueTypeGroupBox;

    FChkHighlight               := TCheckBox.Create(ControlsOwner);
    FChkHighlight.Parent        := FpnlBottom;

    FLblPeriodSelect            := TLabel.Create(ControlsOwner);
    FLblPeriodSelect.Parent     := FPnlBottom;

    FPeriodSelectGroupBox        := TGroupBox.Create(ControlsOwner);
    FPeriodSelectGroupBox.Parent := FPnlBottom;

    FPeriodSequence             := TRadioButton.Create(ControlsOwner);
    FPeriodSequence.Parent      := FPeriodSelectGroupBox;
    FPeriodSelection            := TRadioButton.Create(ControlsOwner);
    FPeriodSelection.Parent     := FPeriodSelectGroupBox;
    FLblPeriodStart             := TLabel.Create(ControlsOwner);
    FLblPeriodStart.Parent      := FPeriodSelectGroupBox;
    FDatePeriodStart            := TDateTimePicker.Create(ControlsOwner);
    FDatePeriodStart.Parent     := FPeriodSelectGroupBox;
    FLblPeriodEnd               := TLabel.Create(ControlsOwner);
    FLblPeriodEnd.Parent        := FPeriodSelectGroupBox;
    FDatePeriodEnd              := TDateTimePicker.Create(ControlsOwner);
    FDatePeriodEnd.Parent       := FPeriodSelectGroupBox;

    FSensitivityGroupBox        := TGroupBox.Create(ControlsOwner);
    FSensitivityGroupBox.Parent := FPnlBottom;

    FLblSensitivityBox           := TLabel.Create(ControlsOwner);
    FLblSensitivityBox.Parent    := FPnlBottom;
    FApplySensitivity           := TRadioButton.Create(ControlsOwner);
    FApplySensitivity.Parent    := FSensitivityGroupBox;

    FAbsoluteSensitivity        := TRadioButton.Create(ControlsOwner);
    FAbsoluteSensitivity.Parent := FSensitivityGroupBox;

    FPercSensitivity             := TRadioButton.Create(ControlsOwner);
    FPercSensitivity.Parent      := FSensitivityGroupBox;

    FedtSensitivity             := TEdit.Create(ControlsOwner);
    FedtSensitivity.Parent      := FSensitivityGroupBox;

    FedtPercSensitivity             := TEdit.Create(ControlsOwner);
    FedtPercSensitivity.Parent      := FSensitivityGroupBox;

    if(FAppModules.Model.ModelName = CPlanning) then
    begin

      lblPlotOptions        := TLabel.Create(ControlsOwner);
      lblPlotOptions.Parent := FPnlBottom;

      FgbCondenced          := TGroupBox.Create(ControlsOwner);
      FgbCondenced.Parent   := FPnlBottom;

      FlblCondenced         := TLabel.Create(ControlsOwner);
      FlblCondenced.Parent  := FgbCondenced;

      FCondenced            := TRadioButton.Create(ControlsOwner);
      FCondenced.Parent     := FgbCondenced;

      FNonCondenced         := TRadioButton.Create(ControlsOwner);
      FNonCondenced.Parent  := FgbCondenced;

      FlblCumulative        := TLabel.Create(ControlsOwner);
      FlblCumulative.Parent := FgbCondenced;


      FCumulative           := TRadioButton.Create(ControlsOwner);
      FCumulative.Parent    := FgbCondenced;

      FNonCumulative        := TRadioButton.Create(ControlsOwner);
      FNonCumulative.Parent := FgbCondenced;

      FlblYearsToSkip             := TLabel.Create(ControlsOwner);
      FlblYearsToSkip.Parent      := FgbCondenced;

      FedtYearsToSkip             := TEdit.Create(ControlsOwner);
      FedtYearsToSkip.Parent      := FgbCondenced;
    end;

    //Temporary measure for now. Just hide them
    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      lblPlotOptions.Visible        := False;
      FgbCondenced.Visible          := False;
      FLblPeriodSelect.Visible      := False;
      FPeriodSelectGroupBox.Visible := False;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionDialog.Resize;
const OPNAME = 'TOutputDataSelectionDialog.Resize';
{var
  lRdbLeft  : integer;
  lRdbSpace : integer;}
begin
  inherited Resize;
  try
    FpnlTop.Align           := alTop;
    FpnlTop.Height          := 135;
    FpnlBottom.Align        := alClient;

    FlblLoadCase.Left       := 10;
    FlblLoadCase.Top        := 10;
    FlblLoadCase.Width      := 100;
    FLblLoadCase.Height     := 23;
    FlblLoadCase.Alignment  := taRightJustify;
    FLblLoadCase.Layout     := tlCenter;
    FlblLoadCase.AutoSize   := False;
    FlblLoadCase.Font.Style := [fsBold];

    FlblSequence.Left       := 10;
    FlblSequence.Top        := 40;
    FlblSequence.Width      := 100;
    FlblSequence.Height     := 23;
    FlblSequence.Alignment  := taRightJustify;
    FlblSequence.Layout     := tlCenter;
    FlblSequence.AutoSize   := False;
    FlblSequence.Font.Style := [fsBold];

    FlblMonth.Left          := 10;
    FlblMonth.Top           := 70;
    FlblMonth.Width         := 100;
    FlblMonth.Height        := 23;
    FlblMonth.Alignment     := taRightJustify;
    FlblMonth.Layout        := tlCenter;
    FlblMonth.AutoSize      := False;
    FlblMonth.Font.Style    := [fsBold];

    FLoadCaseSelector.Left  := 120;
    FLoadCaseSelector.Top   := 10;

    FDecisionMonthCombobox.Top    := FLoadCaseSelector.Top;
    FDecisionMonthCombobox.Width  := 80;
    FDecisionMonthCombobox.Left   := FLoadCaseSelector.Left;
    FDecisionMonthCombobox.Style  := csDropDownList;

    FSequenceSelector.Left  := 120;
    FSequenceSelector.Top   := 40;

    FMonthSelector.Left     := 120;
    FMonthSelector.Top      := 70;

    FLblCalendarDate.Left     := FMonthSelector.Left + FMonthSelector.Width + 10;
    FLblCalendarDate.Top      := 75;

    FSelectMonthlabel.Left          := 10;
    FSelectMonthlabel.Top           := 100;
    FSelectMonthlabel.Width         := 100;
    FSelectMonthlabel.Height        := 23;
    FSelectMonthlabel.Alignment     := taRightJustify;
    FSelectMonthlabel.Layout        := tlCenter;
    FSelectMonthlabel.AutoSize      := False;
    FSelectMonthlabel.Font.Style    := [fsBold];

    FSelectMonthCombobox.Top    := 100;
    FSelectMonthCombobox.Width  := 80;
    FSelectMonthCombobox.Left   := 120;
    FSelectMonthCombobox.Style  := csDropDownList;

    FBtnMoreLess.Top    := 105;
    FBtnMoreLess.Width  := 75;
    FBtnMoreLess.Left   := 450;

    FlblUnits.Left       := 10;
    FlblUnits.Top        := 10;
    FlblUnits.Width      := 100;
    FlblUnits.Height     := 23;
    FlblUnits.Alignment  := taRightJustify;
    FlblUnits.Layout     := tlCenter;
    FlblUnits.AutoSize   := False;
    FlblUnits.Font.Style := [fsBold];

    FlblValueType.Left       := 10;
    FlblValueType.Top        := 50;
    FlblValueType.Width      := 100;
    FlblValueType.Height     := 23;
    FlblValueType.Alignment  := taRightJustify;
    FlblValueType.Layout     := tlCenter;
    FlblValueType.AutoSize   := False;
    FlblValueType.Font.Style := [fsBold];

    FlblTimeStep.Left       := 10;
    FlblTimeStep.Top        := 100;
    FlblTimeStep.Width      := 100;
    FlblTimeStep.Height     := 23;
    FlblTimeStep.Alignment  := taRightJustify;
    FlblTimeStep.Layout     := tlCenter;
    FlblTimeStep.AutoSize   := False;
    FlblTimeStep.Font.Style := [fsBold];

    FUnitsGroupBox.Left       := 120;
    FUnitsGroupBox.Top        := 5;
    FUnitsGroupBox.Width      := 600;
    FUnitsGroupBox.Height     := 35;

    FMetersPerSecond.Align    := alLeft;
    FMillion.Align            := alLeft;
    FPercentage.Align         := alLeft;
    FLivePercentage.Align     := alLeft;

    FMeters.Align             := alLeft;
    FMCM.Align                := alLeft;
    FMegaLitersPerDay.Align   := alLeft;

    FMetersPerSecond.Width    := 50;
    FMillion.Width            := 100;
    FPercentage.Width         := 120;
    FLivePercentage.Width     := 120;
    FMeters.Width             := 100;
    FMCM.Width                := 50;
    FMegaLitersPerDay.Width   := 100;

    {lRdbLeft  := 10;
    lRdbSpace := 100;
    if (FMetersPerSecond.Visible) then
    begin
      FMetersPerSecond.Left  := lRdbLeft;
      FMetersPerSecond.Top   := 15;
      FMetersPerSecond.Width := lRdbSpace - 50;
      lRdbLeft               := lRdbLeft + FMetersPerSecond.Width+5;
    end;

    if (FMillion.Visible) then
    begin
      FMillion.Left  := lRdbLeft;
      FMillion.Top   := 15;
      FMillion.Width := lRdbSpace - 5;
      lRdbLeft       := lRdbLeft + FMillion.Width+5;
    end;

    if (FMeters.Visible) then
    begin
      FMeters.Left  := lRdbLeft;
      FMeters.Top   := 15;
      FMeters.Width := lRdbSpace - 25;
      lRdbLeft      := lRdbLeft + FMeters.Width+5;
    end;

    if (FMCM.Visible) then
    begin
      FMCM.Left    := lRdbLeft ;
      FMCM.Top     := 15;
      FMCM.Width   := lRdbSpace - 25;
      lRdbLeft     := lRdbLeft + FMCM.Width+5;
    end;

    if (FPercentage.Visible) then
    begin
      FPercentage.Left  := lRdbLeft;
      FPercentage.Top   := 15;
      FPercentage.Width := lRdbSpace - 5;
      lRdbLeft          := lRdbLeft + FPercentage.Width+5;
    end;

    if (FMegaLitersPerDay.Visible) then
    begin
      FMegaLitersPerDay.Left    := lRdbLeft;
      FMegaLitersPerDay.Top     := 15;
      FMegaLitersPerDay.Width   := lRdbSpace;
      //lRdbLeft                  := lRdbLeft + lRdbSpace;
    end;}

    FValueTypeGroupBox.Left   := 120;
    FValueTypeGroupBox.Top    := 45;
    FValueTypeGroupBox.Width  := 600;
    FValueTypeGroupBox.Height := 35;

    FSupply.Left     := 10;
    FSupply.Top      := 15;

    FDemand.Left     := 80;
    FDemand.Top      := 15;

    FDeficit.Left    := 150;
    FDeficit.Top     := 15;

    FDemandAndSupply.Left    := 220;
    FDemandAndSupply.Top     := 15;
    FDemandAndSupply.Width   := 120;

    FAllocated.Left    := 345;
    FAllocated.Top     := 15;

    FTimestepGroupBox.Left    := 120;
    FTimestepGroupBox.Top     := 85;
    FTimestepGroupBox.Width   := 600;
    FTimestepGroupBox.Height  := 35;

    FMonthly.Left     := 10;
    FMonthly.Top      := 15;

    FAnnually.Left    := 80;
    FAnnually.Top     := 15;

    FMonthlyCummulative.Left := 150;
    FMonthlyCummulative.Top  := 15;

    FAnnualCummulative.Left := 270;
    FAnnualCummulative.Top  := 15;

    FSequence.Left    := 390;
    FSequence.Top     := 15;
    FSequence.Width   := 70;

    FChkHighlight.Left       := 8;
    FChkHighlight.Top        := 130;
    FChkHighlight.Width      := 125;
    FChkHighlight.Alignment  := taLeftJustify;
    FChkHighlight.Font.Style := [fsBold];

    FLblPeriodSelect.Left       := 5;
    FLblPeriodSelect.Top        := 150;
    FLblPeriodSelect.Width      := 95;
    FLblPeriodSelect.Height     := 23;
    FLblPeriodSelect.Alignment  := taRightJustify;
    FLblPeriodSelect.Layout     := tlCenter;
    FLblPeriodSelect.AutoSize   := False;
    FLblPeriodSelect.Font.Style := [fsBold];

    FPeriodSelectGroupBox.Left   := 120;
    FPeriodSelectGroupBox.Top    := 150;
    FPeriodSelectGroupBox.Width  := 600;
    FPeriodSelectGroupBox.Height := 70;

    FPeriodSequence.Left  := 10;
    FPeriodSequence.Top   := 40;
    FPeriodSequence.Width := 80;

    FPeriodSelection.Left  := 10;
    FPeriodSelection.Top   := 15;
    FPeriodSelection.Width := 100;

    FLblPeriodStart.Left       := 160;
    FLblPeriodStart.Top        := 15;
    FLblPeriodStart.Width      := 70;
    FLblPeriodStart.Alignment  := taRightJustify;
    FLblPeriodStart.AutoSize   := False;

    FDatePeriodStart.Left  := 150;
    FDatePeriodStart.Top   := 35;
    FDatePeriodStart.Width := 100;

    FLblPeriodEnd.Left       := 290;
    FLblPeriodEnd.Top        := 15;
    FLblPeriodEnd.Width      := 70;
    FLblPeriodEnd.Alignment  := taRightJustify;
    FLblPeriodEnd.AutoSize   := False;

    FDatePeriodEnd.Left  := 290;
    FDatePeriodEnd.Top   := 35;
    FDatePeriodEnd.Width := 100;


    FSensitivityGroupBox.Left   := 120;
    FSensitivityGroupBox.Top    := FLblPeriodSelect.Top + FPeriodSelectGroupBox.Height+15;
    FSensitivityGroupBox.Width  := 600;
    FSensitivityGroupBox.Height := 80;


    FLblSensitivityBox.Left        := 5;
    FLblSensitivityBox.Top         := FLblPeriodSelect.Top + FPeriodSelectGroupBox.Height+15;
    FLblSensitivityBox.Width      := 95;
    FLblSensitivityBox.Height     := 23;
    FLblSensitivityBox.Alignment  := taRightJustify;
    FLblSensitivityBox.Layout     := tlCenter;
    FLblSensitivityBox.AutoSize   := False;
    FLblSensitivityBox.Font.Style := [fsBold];

    FApplySensitivity.Left        := 15;
    FApplySensitivity.Top         := 15;
    FApplySensitivity.Alignment   := taRightJustify;
    FApplySensitivity.Width       := 50;

    FAbsoluteSensitivity.Left        := FApplySensitivity.Left + FApplySensitivity.Width+15;
    FAbsoluteSensitivity.Top         := 15;
    FAbsoluteSensitivity.Alignment   := taRightJustify;
    FAbsoluteSensitivity.Width       := 70;

    FedtSensitivity.Top           := 13;
    FedtSensitivity.Left          := FAbsoluteSensitivity.Left + FAbsoluteSensitivity.Width+5;
    FedtSensitivity.Width         := 50;

    FPercSensitivity.Left        := FedtSensitivity.Left + FedtSensitivity.Width+5;
    FPercSensitivity.Top         := 15;
    FPercSensitivity.Alignment   := taRightJustify;
    FPercSensitivity.Width       := 75;

    FedtPercSensitivity.Top           := 13;
    FedtPercSensitivity.Left          := FPercSensitivity.Left + FPercSensitivity.width + 15;
    FedtPercSensitivity.Width         := 50;


    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      lblPlotOptions.Left       := 5;
      lblPlotOptions.Top        := FSensitivityGroupBox.Top+FSensitivityGroupBox.Height+15;
      lblPlotOptions.Width      := 95;
      lblPlotOptions.Height     := 23;
      lblPlotOptions.Alignment  := taRightJustify;
      lblPlotOptions.Layout     := tlCenter;
      lblPlotOptions.AutoSize   := False;
      lblPlotOptions.Font.Style := [fsBold];


      FgbCondenced.Left          := 120;
      FgbCondenced.Top           := FSensitivityGroupBox.Top+FSensitivityGroupBox.Height+10;
      FgbCondenced.Width         := 600;
      FgbCondenced.Height        := 70;

      FlblCondenced.Left         := 10;
      FlblCondenced.Top          := 15;

      FCondenced.Left            := 100;
      FCondenced.Top             := 15;
      FCondenced.Width           := 35;

      FNonCondenced.Left         := 145;
      FNonCondenced.Top          := 15;
      FNonCondenced.Width        := 35;

      FlblCumulative.Left        := 10;
      FlblCumulative.Top         := 40;

      FCumulative.Left           := 100;
      FCumulative.Top            := 40;
      FCumulative.Width          := 35;


      FNonCumulative.Left        := 145;
      FNonCumulative.Top         := 40;
      FNonCumulative.Width       := 35;

      FlblYearsToSkip.Left       := 200;
      FlblYearsToSkip.Top        := 17;
      FlblYearsToSkip.Width      := 150;

      FedtYearsToSkip.Left       := 350;
      FedtYearsToSkip.Top        := 15;
      FedtYearsToSkip.Width      := 30;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelectionDialog.Initialise: boolean;
const OPNAME = 'TOutputDataSelectionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FLoadCaseSelector.Initialise;
    FSequenceSelector.Initialise;
    FMonthSelector.Initialise;

    SetPeriodControlsState(False);
    FUnitsGroupBox.Caption := '';
    FLblCalendarDate.Caption := '';

    FMetersPerSecond.Caption := FAppModules.Language.GetString('TField.m³/s');
    FMillion.Caption         := FAppModules.Language.GetString('TField.Million');

    FPercentage.Caption      := FAppModules.Language.GetString('TField.Percentage');
    FLivePercentage.Caption  := FAppModules.Language.GetString('TField.LivePercentage');
    if FLivePercentage.Visible then
      FPercentage.Caption := FAppModules.Language.GetString('TField.TotalPercentage');

    FMeters.Caption          := FAppModules.Language.GetString('TField.Meters');
    FMCM.Caption             := FAppModules.Language.GetString('TField.MCM');
    FMegaLitersPerDay.Caption:= FAppModules.Language.GetString('TField.MegaLitersPerDay');

    FTimestepGroupBox.Caption  := '';
    FMonthly.Caption           := FAppModules.Language.GetString('TField.Monthly');
    FAnnually.Caption          := FAppModules.Language.GetString('TField.Annually');
    FMonthlyCummulative.Caption := FAppModules.Language.GetString('TField.MonthlyCumulative');
    FAnnualCummulative.Caption := FAppModules.Language.GetString('TField.AnnualCumulative');
    FSequence.Caption          := FAppModules.Language.GetString('TField.Sequence');

    FValueTypeGroupBox.Caption := '';
    FSupply.Caption            := FAppModules.Language.GetString('TField.Supply');
    FDemand.Caption            := FAppModules.Language.GetString('TField.Demand');
    FDeficit.Caption           := FAppModules.Language.GetString('TField.Deficits');
    FDemandAndSupply.Caption   := FAppModules.Language.GetString('TField.DemandAndSupply');
    FAllocated.Caption         := FAppModules.Language.GetString('TField.Allocated');
    FLblSensitivityBox.Caption := FAppModules.Language.GetString('TField.Sensitivity');
    FApplySensitivity.Caption  := FAppModules.Language.GetString('TField.NoneSensitivity');
    FAbsoluteSensitivity.Caption  := FAppModules.Language.GetString('TField.absoluteSensitivity');
    FPercSensitivity.Caption  := FAppModules.Language.GetString('TField.PercSensitivity');


    FChkHighlight.Checked := False;

    FlblLoadCase.Font.Style          := [fsBold];
    FlblSequence.Font.Style          := [fsBold];
    FlblMonth.Font.Style             := [fsBold];
    FSelectMonthlabel.Font.Style     := [fsBold];
    FlblUnits.Font.Style             := [fsBold];
    FlblTimeStep.Font.Style          := [fsBold];
    FlblValueType.Font.Style         := [fsBold];
    FChkHighlight.Font.Style         := [fsBold];

    FPnlBottom.Visible               := FALSE;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelectionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDataSelectionDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FpnlTop.Caption              := '';
    FlblLoadCase.Caption         := FAppModules.Language.GetString('LabelText.LoadCase');
    FlblSequence.Caption         := FAppModules.Language.GetString('LabelText.Sequence');
    FlblMonth.Caption            := FAppModules.Language.GetString('LabelText.Month');
    FSelectMonthlabel.Caption    := FAppModules.Language.GetString('LabelText.SelectMonth');



    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      FlblLoadCase.Caption         := FAppModules.Language.GetString('LabelText.DecisionMonth');
      lblPlotOptions.Caption       := 'Plot Options';
      FlblCondenced.Caption        := 'Plot Condenced?';
      FlblCumulative.Caption       := 'Plot Cumulative?';

      FCondenced.Caption           := FAppModules.Language.GetString('LabelText.Yes');
      FNonCondenced.Caption        := FAppModules.Language.GetString('LabelText.No');

      FCumulative.Caption           := FAppModules.Language.GetString('LabelText.Yes');
      FNonCumulative.Caption        := FAppModules.Language.GetString('LabelText.No');


      FlblYearsToSkip.Caption := 'Enter No. Of Years To skip :';


    end;

    FLoadCaseSelector.LanguageHasChanged;
    FSequenceSelector.LanguageHasChanged;
    FMonthSelector.LanguageHasChanged;

    FBtnMoreLess.Caption  := FAppModules.Language.GetString('LabelText.More');
    FPnlBottom .Caption   := '';
    FlblUnits.Caption     := FAppModules.Language.GetString('LabelText.Units');
    FlblTimeStep.Caption  := FAppModules.Language.GetString('LabelText.TimeStep');
    FlblValueType.Caption := FAppModules.Language.GetString('LabelText.ValueType');

    FPeriodSequence.Caption   := FAppModules.Language.GetString('LabelText.PeriodSequence');
    FPeriodSelection.Caption  := FAppModules.Language.GetString('LabelText.PeriodSelection');
    FLblPeriodSelect.Caption  := FAppModules.Language.GetString('LabelText.AveragePeriod');
    FLblPeriodStart.Caption   := FAppModules.Language.GetString('LabelText.StartDate');
    FLblPeriodEnd.Caption     := FAppModules.Language.GetString('LabelText.EndDate');

    FChkHighlight.Caption  := FAppModules.Language.GetString('LabelText.Highlight');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionDialog.AssignHelpContext;
const OPNAME = 'TOutputDataSelectionDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDataSelectionDialog.ToggleMoreLess : Boolean;
const OPNAME = 'TOutputDataSelectionDialog.ToggleMoreLess';
begin
  Result := FALSE;
  try
    FPnlBottom.Visible := (NOT FPnlBottom.Visible);
    if (FPnlBottom.Visible) then
    begin
      Self.ClientHeight := Self.ClientHeight + 220;
      FBtnMoreLess.Caption := FAppModules.Language.GetString('LabelText.Less');
    end
    else
    begin
      Self.ClientHeight := Self.ClientHeight - 220;
      FBtnMoreLess.Caption := FAppModules.Language.GetString('LabelText.More');
    end;
    Result := FPnlBottom.Visible;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDataSelectionDialog.SetPeriodControlsState(AEnabled: boolean);
const OPNAME = 'TOutputDataSelectionDialog.SetPeriodControlsState';
begin
  try
    FLblPeriodStart.Enabled := AEnabled;
    FDatePeriodStart.Enabled := AEnabled;
    FLblPeriodEnd.Enabled := AEnabled;
    FDatePeriodEnd.Enabled := AEnabled;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDataSelectionDialog.SetPlotOptionsState(AEnabled: boolean);
const OPNAME = 'TOutputDataSelectionDialog.SetPlotOptionsState';
begin
  try
    FlblCondenced.Enabled    :=  AEnabled;
    FlblCumulative.Enabled   :=  AEnabled;
    FCondenced.Enabled       :=  AEnabled;
    FNonCondenced.Enabled    :=  AEnabled;
    FCumulative.Enabled      :=  AEnabled;
    FNonCumulative.Enabled   :=  AEnabled;
    FlblYearsToSkip.Enabled  :=  AEnabled;
    FedtYearsToSkip.Enabled  :=  AEnabled;
    if not AEnabled then
      FedtYearsToSkip.Color := clBtnFace
    else
      FedtYearsToSkip.Color := clWindow;
      
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
