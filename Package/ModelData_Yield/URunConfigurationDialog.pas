{******************************************************************************}
{*  UNIT      : Contains the class TRunConfigurationDialog.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit URunConfigurationDialog;

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
  UDataComponent;

type

  TRunConfigurationDialog = class(TAbstractScrollablePanel)
  private
  protected
    FPeriodGroupBox                : TGroupBox;
    FRunGroupBox                   : TGroupBox;
    FRunSequencePageControl        : TPageControl;
    FHistoricTabSheet              : TTabSheet;
    FStochasticTabSheet            : TTabSheet;

    FStartYearLabel                : TLabel;
    FStartYearNumberLabel          : TLabel;
    FNumberOfYearsLabel            : TLabel;
    FNumberOfPeriodsLabel          : TLabel;
    FStartMonthLabel               : TLabel;
    FStartYearEdit                 : TFieldEdit;
    FStartYearLabelEdit            : TFieldEdit;
    FNumberOfYearsEdit             : TFieldEdit;
    FNumberOfPeriodsEdit           : TFieldEdit;
    FHydroUnitsCodeLabel           : TLabel;
    FHydroUnitsCodeEdit            : TFieldEdit;
    FStartMonthEdit                : TFieldEdit;
    FMonthNamesLabel               : TLabel;
    FDaysInMonthLabel              : TLabel;
    FCalendarStartMonthLabel       : TLabel;
    FCalendarStartMonthEdit        : TFieldEdit;
    FMonthsGrid                    : TFieldStringGrid;

    FHistoricFirmYieldChkBox       : TFieldChkBox;
    FStochasticFirmYieldChkBox     : TFieldChkBox;
    FLimitOptionChkBox             : TFieldChkBox;
    FRigorouslabel                 : TLabel;
    FParamFileName                 : TFieldEdit;

    FMultiplePeriodsChkBox         : TFieldChkBox;
    FReduceSequencesChkBox         : TFieldChkBox;
    FTargetRecurrenceIntervalLabel : TLabel;
    FTargetRecurrenceIntervalEdit  : TFieldEdit;
    FSortDescendingLabel           : TLabel;
    FStartTypeRadioGroup           : TFieldRadioGroup;

    FNrOfSequencesLabel            : TLabel;
    FNrOfSequencesEdit             : TFieldEdit;
    FStartSequenceLabel            : TLabel;
    FStartSequenceEdit             : TFieldEdit;
    FSequenceOrderLabel            : TLabel;
    FSequenceOrderGrid             : TFieldStringGrid;
    FSequenceStartYearLabel        : TLabel;
    FSequenceStartYearEdit         : TFieldEdit;
    FTitleGroupBox                 : TGroupBox;
    FRunTitle1Label                : TLabel;
    FRunTitle2Label                : TLabel;
    FRunTitle3Label                : TLabel;
    FRunTitle1Edit                 : TFieldEdit;
    FRunTitle2Edit                 : TFieldEdit;
    FRunTitle3Edit                 : TFieldEdit;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property PeriodGroupBox               : TGroupBox            read FPeriodGroupBox;
    property RunGroupBox                  : TGroupBox            read FRunGroupBox;
    property RunSequencePageControl       : TPageControl         read FRunSequencePageControl;
    property HistoricTabSheet             : TTabSheet            read FHistoricTabSheet;
    property StochasticTabSheet           : TTabSheet            read FStochasticTabSheet;
    property StartYearLabel               : TLabel               read FStartYearLabel;
    property StartYearNumberLabel         : TLabel               read FStartYearNumberLabel;
    property NumberOfYearsLabel           : TLabel               read FNumberOfYearsLabel;
    property NumberOfPeriodsLabel         : TLabel               read FNumberOfPeriodsLabel;
    property StartMonthLabel              : TLabel               read FStartMonthLabel;
    property StartYearEdit                : TFieldEdit           read FStartYearEdit;
    property StartYearLabelEdit           : TFieldEdit           read FStartYearLabelEdit;
    property NumberOfYearsEdit            : TFieldEdit           read FNumberOfYearsEdit;
    property NumberOfPeriodsEdit          : TFieldEdit           read FNumberOfPeriodsEdit;
    property StartMonthEdit               : TFieldEdit           read FStartMonthEdit;
    property MonthNamesLabel              : TLabel               read FMonthNamesLabel;
    property DaysInMonthLabel             : TLabel               read FDaysInMonthLabel;
    property HydroUnitsCodeEdit           : TFieldEdit           read FHydroUnitsCodeEdit;
    property CalendarStartMonthLabel      : TLabel               read FCalendarStartMonthLabel;
    property CalendarStartMonthEdit       : TFieldEdit           read FCalendarStartMonthEdit;
    property MonthsGrid                   : TFieldStringGrid     read FMonthsGrid;
    property ParamFileName                : TFieldEdit           read FParamFileName;
    property HistoricFirmYieldChkBox      : TFieldChkBox         read FHistoricFirmYieldChkBox;
    property StochasticFirmYieldChkBox    : TFieldChkBox         read FStochasticFirmYieldChkBox;
    property LimitOptionChkBox            : TFieldChkBox         read FLimitOptionChkBox;
    property MultiplePeriodsChkBox        : TFieldChkBox         read FMultiplePeriodsChkBox;
    property ReduceSequencesChkBox        : TFieldChkBox         read FReduceSequencesChkBox;
    property TargetRecurrenceIntervalLabel: TLabel               read FTargetRecurrenceIntervalLabel;
    property TargetRecurrenceIntervalEdit : TFieldEdit           read FTargetRecurrenceIntervalEdit;
    property StartTypeRadioGroup          : TFieldRadioGroup     read FStartTypeRadioGroup;
    property NrOfSequencesLabel           : TLabel               read FNrOfSequencesLabel;
    property NrOfSequencesEdit            : TFieldEdit           read FNrOfSequencesEdit;
    property StartSequenceLabel           : TLabel               read FStartSequenceLabel;
    property StartSequenceEdit            : TFieldEdit           read FStartSequenceEdit;
    property SequenceOrderLabel           : TLabel               read FSequenceOrderLabel;
    property SequenceOrderGrid            : TFieldStringGrid     read FSequenceOrderGrid;
    property SequenceStartYearLabel       : TLabel               read FSequenceStartYearLabel;
    property SequenceStartYearEdit        : TFieldEdit           read FSequenceStartYearEdit;
    property SortDescendingLabel          : TLabel               read FSortDescendingLabel;
    property Rigorouslabel                : TLabel               read FRigorouslabel;
    property RunTitle1Edit                : TFieldEdit           read FRunTitle1Edit;
    property RunTitle2Edit                : TFieldEdit           read FRunTitle2Edit;
    property RunTitle3Edit                : TFieldEdit           read FRunTitle3Edit;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TRunConfigurationDialog                                                    *}
{******************************************************************************}

procedure TRunConfigurationDialog.CreateMemberObjects;
const OPNAME = 'TRunConfigurationDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                              Left  Top Width Height
    FPeriodGroupBox          := CreateFieldGroupBox (lOwner, lParent,   5,   5, 255, 485, 0, FALSE);
    FRunGroupBox             := CreateFieldGroupBox (lOwner, lParent, 265,   5, 430, 330, 1, FALSE);
    // FPeriodGroupBox
    FStartYearLabel          := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10,  20, 175,  21);
    FStartYearNumberLabel    := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10,  45, 175,  21);
    FNumberOfYearsLabel      := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10,  70, 175,  21);
    FNumberOfPeriodsLabel    := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10,  95, 175,  21);
    FStartMonthLabel         := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10, 120, 175,  21);
    FStartYearEdit           := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190,  20,  40,  21, 0, TRUE);
    FStartYearEdit.MaxLength := 4;
    FStartYearLabelEdit      := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190,  45,  40,  21, 1, TRUE);
    FNumberOfYearsEdit       := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190,  70,  40,  21, 2, TRUE);
    FNumberOfPeriodsEdit     := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190,  95,  40,  21, 3, TRUE);
    FStartMonthEdit          := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190, 120,  40,  21, 4, TRUE);
    FHydroUnitsCodeLabel     := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10, 145,  175,  21);
    FHydroUnitsCodeEdit      := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190, 145,   40, 21, 5, TRUE);
    FCalendarStartMonthLabel := CreateFieldLabel     (lOwner, FPeriodGroupBox,  10,  170, 170,  21);
    FCalendarStartMonthEdit  := CreateFieldEdit      (FAppModules, lOwner, FPeriodGroupBox, 190, 170,  40,  21, 2, TRUE);
    FMonthNamesLabel         := CreateFieldLabel     (lOwner, FPeriodGroupBox,  35, 210,  85,  21);
    FDaysInMonthLabel        := CreateFieldLabel     (lOwner, FPeriodGroupBox, 130, 210,  85,  21);
     FMonthsGrid              := CreateFieldStringGrid(FAppModules, lOwner, FPeriodGroupBox,  10, 230, 216, 207, 6, TRUE);
    with FMonthsGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 3;
      RowCount         := 12;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultRowHeight := 16;
      DefaultColWidth  := 95;
      ColWidths[0]     := 20;
    end;

    FRunSequencePageControl := TPageControl.Create(lOwner);
    with FRunSequencePageControl do
    begin
      Parent := FRunGroupBox;
      Left   := 5;
      Top    := 20;
      Width  := 420;
      Height := 300;
    end;
    FHistoricTabSheet          := TTabSheet.Create(lOwner);
    FHistoricTabSheet.Parent   := FRunSequencePageControl;
    FStochasticTabSheet        := TTabSheet.Create(lOwner);
    FStochasticTabSheet.Parent := FRunSequencePageControl;
    FParamFileName          := CreateFieldEdit      (FAppModules, lOwner, FRunGroupBox, 168,  25, 250,  21, 7, TRUE);
    FParamFileName.Visible  := False;
    // FStochasticTabSheet
    FLimitOptionChkBox      := CreateFieldChkBox    (FAppModules, lOwner, FStochasticTabSheet,   3,  2, 172,  21, 0, TRUE, taLeftJustify);
    FStochasticFirmYieldChkBox
      := CreateFieldChkBox    (FAppModules, lOwner, FStochasticTabSheet,   3,  25,  172,  21, 0, TRUE, taLeftJustify);
    FMultiplePeriodsChkBox
      := CreateFieldChkBox    (FAppModules, lOwner, FStochasticTabSheet,   3,  48, 172,  21, 0, TRUE, taLeftJustify);
    FTargetRecurrenceIntervalLabel
      := CreateFieldLabel                   (lOwner, FStochasticTabSheet,   210, 2, 130, 21);
    FTargetRecurrenceIntervalEdit
      := CreateFieldEdit      (FAppModules, lOwner, FStochasticTabSheet,   350, 2, 30, 21, 1, TRUE);
    FReduceSequencesChkBox
      := CreateFieldChkBox    (FAppModules, lOwner, FStochasticTabSheet,   3,  70, 172,  21, 1, TRUE, taLeftJustify);
    FSortDescendingLabel
      := CreateFieldLabel                  (lOwner, FStochasticTabSheet, 210,  70, 200,  30);
    FSortDescendingLabel.WordWrap := TRUE;
    FSortDescendingLabel.Font.Color := clRed;

    FStartTypeRadioGroup
      := CreateFieldRadioGroup(FAppModules, lOwner, FStochasticTabSheet,   5,  95, 190,  90, 2, FALSE);
    FRigorouslabel
      := CreateFieldLabel                  (lOwner, FStochasticTabSheet, 210,  110, 200,  30);
    FRigorouslabel.WordWrap := TRUE;
    FRigorouslabel.Font.Color := clRed;
    FNrOfSequencesLabel
      := CreateFieldLabel                  (lOwner, FStochasticTabSheet,   5, 193, 175,  21);
    FNrOfSequencesEdit
      := CreateFieldEdit      (FAppModules, lOwner, FStochasticTabSheet, 185, 193,  40,  21, 3, TRUE);
    FStartSequenceLabel
      := CreateFieldLabel                  (lOwner, FStochasticTabSheet,   5, 218, 175,  21);
    FStartSequenceEdit
      := CreateFieldEdit      (FAppModules, lOwner, FStochasticTabSheet,   5, 245,  40,  21, 4, TRUE);
    FSequenceOrderLabel
      := CreateFieldLabel                  (lOwner, FStochasticTabSheet,   5, 218, 300,  21);
    FSequenceOrderGrid
      := CreateFieldStringGrid(FAppModules, lOwner, FStochasticTabSheet,   5, 245, 413,  22, 5, TRUE);
    with FSequenceOrderGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 10;
      RowCount         := 1;
      FixedCols        := 0;
      FixedRows        := 0;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
    end;
    // FHistoricTabSheet
    FHistoricFirmYieldChkBox
      := CreateFieldChkBox (FAppModules, lOwner, FHistoricTabSheet,   3,   5, 160,  21, 0, TRUE, taLeftJustify);
    FSequenceStartYearLabel
      := CreateFieldLabel               (lOwner, FHistoricTabSheet,   5,  54,  100,  21);
    FSequenceStartYearEdit
      := CreateFieldEdit   (FAppModules, lOwner, FHistoricTabSheet, 145,  54,  40,  21, 0, TRUE);
    FSequenceStartYearEdit.MaxLength := FStartYearEdit.MaxLength;
                                                               //Left  Top Width Height
    FTitleGroupBox  := CreateFieldGroupBox (lOwner, lParent,   265, 340, 430,  150, 0, FALSE);
    FRunTitle1Label := CreateFieldLabel    (FTitleGroupBox, FTitleGroupBox,  15,  20, 45, 21);
    FRunTitle2Label := CreateFieldLabel    (FTitleGroupBox, FTitleGroupBox,  15,  45, 45, 21);
    FRunTitle3Label := CreateFieldLabel    (FTitleGroupBox, FTitleGroupBox,  15,  70, 45, 21);

    FRunTitle1Edit := CreateFieldEdit      (FAppModules, FTitleGroupBox, FTitleGroupBox, 50,  20, 375, 21, 0, TRUE);
    FRunTitle2Edit := CreateFieldEdit      (FAppModules, FTitleGroupBox, FTitleGroupBox, 50,  45, 375, 21, 0, TRUE);
    FRunTitle3Edit := CreateFieldEdit      (FAppModules, FTitleGroupBox, FTitleGroupBox, 50,  70, 375, 21, 0, TRUE);

    FRunTitle1Edit.MaxLength := 80;
    FRunTitle2Edit.MaxLength := 80;
    FRunTitle3Edit.MaxLength := 80;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationDialog.Resize;
const OPNAME = 'TRunConfigurationDialog.Resize';
begin
  inherited;
  try
    FHistoricTabSheet.PageControl := FRunSequencePageControl;
    FStochasticTabSheet.PageControl := FRunSequencePageControl;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDialog.Initialise: boolean;
const OPNAME = 'TRunConfigurationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TRunConfigurationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FPeriodGroupBox.Caption                 := ' ' + FAppModules.Language.GetString('RunParameters.PeriodInfo')  + ' ';
    FRunGroupBox.Caption                    := ' ' + FAppModules.Language.GetString('RunParameters.RunInfo')  + ' ';
    FHistoricTabSheet.Caption               := ' ' + FAppModules.Language.GetString('RunParameters.HistoricSeq')  + ' ';
    FStochasticTabSheet.Caption             := ' ' + FAppModules.Language.GetString('RunParameters.StochasticSeq')  + ' ';
    // FPeriodGroupBox
    FStartYearLabel.Caption                 := FAppModules.Language.GetString('TField.StartYearO')  + ' :';;
    FStartYearNumberLabel.Caption           := FAppModules.Language.GetString('TField.StartYearG')  + ' :';
    FNumberOfYearsLabel.Caption             := FAppModules.Language.GetString('TField.YearsCount')  + ' :';
    FNumberOfPeriodsLabel.Caption           := FAppModules.Language.GetString('TField.NumPeriods')   + ' :';
    FStartMonthLabel.Caption                := FAppModules.Language.GetString('TField.StartMonthNo') + ' :';
    FMonthNamesLabel.Caption                := FAppModules.Language.GetString('TField.MonthNames');
    FHydroUnitsCodeLabel.Caption            := FAppModules.Language.GetString('TField.HydroUnitsCode') + ' :';
    FDaysInMonthLabel.Caption               := FAppModules.Language.GetString('TField.DaysInMonth');
    // FRunGroupBox
    FHistoricFirmYieldChkBox.Caption        := FAppModules.Language.GetString('TField.CalcHistoryOpt') + ' :';
    FLimitOptionChkBox.Caption              := FAppModules.Language.GetString('TField.LimitOpt') + ' :';
    // FStochasticTabSheet
    FStochasticFirmYieldChkBox.Caption      := FAppModules.Language.GetString('TField.CalcStochasticOpt')+ ':';
    FMultiplePeriodsChkBox.Caption          := FAppModules.Language.GetString('TField.MultPeriodOpt') + ' :';
    FReduceSequencesChkBox.Caption          := FAppModules.Language.GetString('TField.ReduceSeqOpt') + ' :';
    FStartTypeRadioGroup.Caption            := ' ' + FAppModules.Language.GetString('TRunConfigurationDialog.StartTypeRadioGroup') + ' ';
    FTargetRecurrenceIntervalLabel.Caption  := FAppModules.Language.GetString('TField.TargetRecurrenceInterval');
    FNrOfSequencesLabel.Caption             := FAppModules.Language.GetString('TField.HydroSeqCount') + ' :';
    FStartSequenceLabel.Caption             := FAppModules.Language.GetString('RunParameters.SequenceStartNumber') + ' :';
    if (FAppModules.Model.ModelName = CYield) then
      FSequenceOrderLabel.Caption             := FAppModules.Language.GetString('RunParameters.YieldSequenceOrder') + ' :'
    else
      FSequenceOrderLabel.Caption           := FAppModules.Language.GetString('RunParameters.SequenceOrder') + ' :';
    FSequenceStartYearLabel.Caption         := FAppModules.Language.GetString('RunParameters.SequenceStartYear') + ' :';
    FSortDescendingLabel.Caption            := FAppModules.Language.GetString('TRunConfigurationDialog.LoadCaseDescending');
    FRigorouslabel.Caption                  := FAppModules.Language.GetString('TRunConfigurationDialog.Rigorouslabel');
    FCalendarStartMonthLabel.Caption        := FAppModules.Language.GetString('TField.CalendarStartMonth')   + ' :';
    FStartTypeRadioGroup.Hints.Clear;
    FStartTypeRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.StartTypeGenerateRandomly'));
    FStartTypeRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.StartTypeUseHistoricalSequence'));
    FStartTypeRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.StartTypeBootstrap'));
    FStartTypeRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.OperationalUse'));

    FTitleGroupBox.Caption                  := ' ' + FAppModules.Language.GetString('RunParameters.Rundescription');
    FRunTitle1Label.Caption := FAppModules.Language.GetString('TField.Title1');
    FRunTitle2Label.Caption := FAppModules.Language.GetString('TField.Title2');
    FRunTitle3Label.Caption := FAppModules.Language.GetString('TField.Title3');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationDialog.RestoreColourState;
const OPNAME = 'TRunConfigurationDialog.RestoreColourState';
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

procedure TRunConfigurationDialog.AssignHelpContext;
const OPNAME = 'TRunConfigurationDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                          HC_RunControl);
    SetControlHelpContext(FPeriodGroupBox,               HC_AnalysisPeriod);
    SetControlHelpContext(FMonthsGrid,                   HC_AnalysisPeriod);
    SetControlHelpContext(FNumberOfPeriodsEdit,          HC_AnalysisPeriod);
    SetControlHelpContext(FStartMonthEdit,               HC_AnalysisPeriod);
    SetControlHelpContext(FStartYearEdit,                HC_AnalysisPeriod);
    SetControlHelpContext(FStartYearLabelEdit,           HC_AnalysisPeriod);
    SetControlHelpContext(FNumberOfYearsEdit,            HC_AnalysisPeriod);

    SetControlHelpContext(FTitleGroupBox,                HC_RunDescription);
    SetControlHelpContext(FRunTitle1Edit,                HC_RunDescription);
    SetControlHelpContext(FRunTitle2Edit,                HC_RunDescription);
    SetControlHelpContext(FRunTitle3Edit,                HC_RunDescription);


    SetControlHelpContext(FHistoricTabSheet,             HC_DeterminingTheSystemYield);
    SetControlHelpContext(FHistoricFirmYieldChkBox,      HC_DeterminingTheSystemYield);
    SetControlHelpContext(FSequenceStartYearEdit,        HC_AnalysisPeriod);

    SetControlHelpContext(FRunGroupBox,                  HC_StochasticRunOptions);
    SetControlHelpContext(FStochasticTabSheet,           HC_StochasticRunOptions);
    SetControlHelpContext(FStartTypeRadioGroup,          HC_StochasticRunOptions);
    SetControlHelpContext(FMultiplePeriodsChkBox,        HC_StochasticRunOptions);
    SetControlHelpContext(FNrOfSequencesEdit,            HC_StochasticRunOptions);
    SetControlHelpContext(FSequenceOrderGrid,            HC_StochasticRunOptions);
    SetControlHelpContext(FReduceSequencesChkBox,        HC_StochasticRunOptions);
    SetControlHelpContext(FLimitOptionChkBox,            HC_StochasticRunOptions);
    SetControlHelpContext(FStochasticFirmYieldChkBox,    HC_DeterminingTheSystemYield);
    SetControlHelpContext(FTargetRecurrenceIntervalEdit, HC_DeterminingTheSystemYield);
    SetControlHelpContext(FStartSequenceEdit,            HC_StochasticRunOptions);
    SetControlHelpContext(FCalendarStartMonthEdit,       HC_StochasticRunOptions);

   except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
