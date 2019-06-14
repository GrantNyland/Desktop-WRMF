{******************************************************************************}
{*  UNIT      : Contains the class TOutputConfigurationDialog.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/09                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UOutputConfigurationDialog;

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

  TOutputConfigurationDialog = class(TAbstractScrollablePanel)
  private
  protected
    FSummaryLevelRadioGroup  : TFieldRadioGroup;
    FInputDataChkBox         : TFieldChkBox;
    FYieldResultsChkBox      : TFieldChkBox;
    FPlotFileChkBox          : TFieldChkBox;
    FCreatePlotFileChkBox    : TFieldChkBox;
    FDebugGroupBox           : TGroupBox;
    FInputGroupBox           : TGroupBox;
    FDebugLevelLabel         : TLabel;
    FDebugLevelCbx           : TFieldComboBox;
    FExpertUseOnlyLabel      : TLabel;
    FDebugStartDateLabel     : TLabel;
    FDebugStartYearCbx       : TFieldComboBox;
    FDebugStartMonthCbx      : TFieldComboBox;
    FDebugStartPeriodEdit    : TFieldEdit;
    FDebugEndDateLabel       : TLabel;
    FDebugEndYearCbx         : TFieldComboBox;
    FDebugEndMonthCbx        : TFieldComboBox;
    FDebugEndPeriodEdit      : TFieldEdit;
    FControlParameters       : TGroupBox;
    FDetailedOptionChkBox    : TFieldChkBox;
    FSupplyOptionChkBox      : TFieldChkBox;
    FEconomicOption          : TFieldChkBox;
    FPlanningSummary         : TFieldChkBox;
    FInputSummary            : TFieldChkBox;
    FAnnualSummaryRadioGroup : TFieldRadioGroup;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property SummaryLevelRadioGroup   : TFieldRadioGroup  read FSummaryLevelRadioGroup;
    property InputDataChkBox          : TFieldChkBox      read FInputDataChkBox;
    property YieldResultsChkBox       : TFieldChkBox      read FYieldResultsChkBox;
    property PlotFileChkBox           : TFieldChkBox      read FPlotFileChkBox;
    property CreatePlotFileChkBox     : TFieldChkBox      read FCreatePlotFileChkBox;
    property DebugGroupBox            : TGroupBox         read FDebugGroupBox;
    property InputGroupBox            : TGroupBox         read FInputGroupBox;
    property DebugLevelLabel          : TLabel            read FDebugLevelLabel;
    property DebugStartDateLabel      : TLabel            read FDebugStartDateLabel;
    property DebugEndDateLabel        : TLabel            read FDebugEndDateLabel;
    property DebugLevelCbx            : TFieldComboBox    read FDebugLevelCbx;
    property DebugStartYearCbx        : TFieldComboBox    read FDebugStartYearCbx;
    property DebugStartMonthCbx       : TFieldComboBox    read FDebugStartMonthCbx;
    property DebugEndYearCbx          : TFieldComboBox    read FDebugEndYearCbx;
    property DebugEndMonthCbx         : TFieldComboBox    read FDebugEndMonthCbx;
    property DebugStartPeriodEdit     : TFieldEdit        read FDebugStartPeriodEdit;
    property DebugEndPeriodEdit       : TFieldEdit        read FDebugEndPeriodEdit;

    property ControlParametersGroupBox: TGroupBox         read FControlParameters;
    property DetailedOptionChkBox     : TFieldChkBox      read FDetailedOptionChkBox;
    property SupplyOptionChkBox       : TFieldChkBox      read FSupplyOptionChkBox;
    property EconomicOptionChkBox     : TFieldChkBox      read FEconomicOption;
    property PlanningSummaryChkBox    : TFieldChkBox      read FPlanningSummary;
    property InputSummaryChkBox       : TFieldChkBox      read FInputSummary;
    property AnnualSummaryRadioGroup  : TFieldRadioGroup  read FAnnualSummaryRadioGroup;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities,
  VCL.ImgList;

{******************************************************************************}
{* TOutputConfigurationDialog                                                 *}
{******************************************************************************}

procedure TOutputConfigurationDialog.CreateMemberObjects;
const OPNAME = 'TOutputConfigurationDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                                    Left  Top  Width Height
    FSummaryLevelRadioGroup  := CreateFieldRadioGroup (FAppModules, lOwner, lParent,          10,    10, 200, 105, 0, FALSE);
    FInputGroupBox           := CreateFieldGroupBox   (lOwner, lParent,                       220,   10, 270, 105, 4, FALSE);
    FInputDataChkBox         := CreateFieldChkBox     (FAppModules, lOwner, FInputGroupBox ,  10,    10, 240,  21, 1, TRUE, taLeftJustify);
    FYieldResultsChkBox      := CreateFieldChkBox     (FAppModules, lOwner, FInputGroupBox,   10,    40, 240,  21, 2, TRUE, taLeftJustify);
    FPlotFileChkBox          := CreateFieldChkBox     (FAppModules, lOwner, FInputGroupBox,   10,    70, 240,  21, 3, TRUE, taLeftJustify);
    FDebugGroupBox           := CreateFieldGroupBox   (lOwner, lParent,                       10,   125, 480, 105, 4, FALSE);
    FDebugLevelLabel         := CreateFieldLabel      (lOwner, FDebugGroupBox,                10,    25, 175,  21);
    FDebugLevelCbx           := CreateFieldComboBox   (FAppModules, lOwner, FDebugGroupBox,  160,    25,  40,  21, 0, TRUE, csDropDownList);
    FExpertUseOnlyLabel      := CreateFieldLabel      (lOwner, FDebugGroupBox,               210,    25, 175,  21);
    FDebugStartDateLabel     := CreateFieldLabel      (lOwner, FDebugGroupBox,                10,    50, 175,  21);
    FDebugStartYearCbx       := CreateFieldComboBox   (FAppModules, lOwner, FDebugGroupBox,  160,    50,  80,  21, 0, TRUE, csDropDownList);
    FDebugStartMonthCbx      := CreateFieldComboBox   (FAppModules, lOwner, FDebugGroupBox,  240,    50,  80,  21, 0, TRUE, csDropDownList);
    FDebugStartPeriodEdit    := CreateFieldEdit       (FAppModules, lOwner, FDebugGroupBox,  330,    50,  50,  21, 0, TRUE);
    FDebugEndDateLabel       := CreateFieldLabel      (lOwner, FDebugGroupBox,                10,    75, 175,  21);
    FDebugEndYearCbx         := CreateFieldComboBox   (FAppModules, lOwner, FDebugGroupBox,  160,    75,  80,  21, 0, TRUE, csDropDownList);
    FDebugEndMonthCbx        := CreateFieldComboBox   (FAppModules, lOwner, FDebugGroupBox,  240,    75,  80,  21, 0, TRUE, csDropDownList);
    FDebugEndPeriodEdit      := CreateFieldEdit       (FAppModules, lOwner, FDebugGroupBox,  330,    75,  50,  21, 0, TRUE);
    FControlParameters       := CreateFieldGroupBox   (lOwner, lParent,                       10,   240, 200, 170, 4, FALSE);
    FDetailedOptionChkBox    := CreateFieldChkBox     (FAppModules, lOwner, FControlParameters, 10,  20, 160,  21, 3, TRUE, taLeftJustify);
    FSupplyOptionChkBox      := CreateFieldChkBox     (FAppModules, lOwner, FControlParameters, 10,  50, 160,  21, 3, TRUE, taLeftJustify);
    FEconomicOption          := CreateFieldChkBox     (FAppModules, lOwner, FControlParameters, 10,  80, 160,  21, 3, TRUE, taLeftJustify);
    FPlanningSummary         := CreateFieldChkBox     (FAppModules, lOwner, FControlParameters, 10,  80, 160,  21, 3, TRUE, taLeftJustify);
    FInputSummary            := CreateFieldChkBox     (FAppModules, lOwner, FControlParameters, 10, 110, 160,  21, 3, TRUE, taLeftJustify);
    FCreatePlotFileChkBox    := CreateFieldChkBox     (FAppModules, lOwner, FControlParameters, 10, 140, 160,  21, 3, TRUE, taLeftJustify);
    FAnnualSummaryRadioGroup := CreateFieldRadioGroup (FAppModules, lOwner, lParent, 220, 240, 270, 170, 0, FALSE);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationDialog.Resize;
const OPNAME = 'TOutputConfigurationDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationDialog.Initialise: boolean;
const OPNAME = 'TOutputConfigurationDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputConfigurationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FSummaryLevelRadioGroup.Caption    := ' ' + FAppModules.Language.GetString('TField.SummaryLevelDescr') + ' ';
    FInputDataChkBox.Caption           := FAppModules.Language.GetString('TField.SummaryOut') + ' :';
    FYieldResultsChkBox.Caption        := FAppModules.Language.GetString('TField.StoreYield') + ' :';
    FPlotFileChkBox.Caption            := FAppModules.Language.GetString('TField.PlotOpt') + ' :';
    FCreatePlotFileChkBox.Caption      := FAppModules.Language.GetString('TField.PlotOpt') + ' :';
    FSummaryLevelRadioGroup.Hints.Clear;
    SummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescBrief'));
    SummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescAdditional'));
    SummaryLevelRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelDescFull'));
    FDebugGroupBox.Caption                  := ' ' + FAppModules.Language.GetString('RunParameters.DebugInfo')  + ' ';
    FDebugLevelLabel.Caption                := FAppModules.Language.GetString('TField.DebugLevel') + ' :';
    FDebugStartDateLabel.Caption            := FAppModules.Language.GetString('TField.DebugInit') + ' :';
    FDebugEndDateLabel.Caption              := FAppModules.Language.GetString('TField.DebugFinal') + ' :';
    FExpertUseOnlyLabel.Caption             := FAppModules.Language.GetString('TRunConfigurationDialog.ForExpertUseOnly');
    FDebugEndYearCbx.Hint                   := FAppModules.Language.GetString('TRunConfigurationDialog.DebugEndDatePicker');
    FControlParameters.Caption              := ' ' + FAppModules.Language.GetString('RunParameters.ControlParameters')  + ' ';
    FDetailedOptionChkBox.Caption           := FAppModules.Language.GetString('TField.DetailedOption') + ' :';
    FSupplyOptionChkBox.Caption             := FAppModules.Language.GetString('TField.SupplyOption') + ' :';
    FEconomicOption.Caption                 := FAppModules.Language.GetString('TField.EconomicOption') + ' :';
    FPlanningSummary.Caption                := FAppModules.Language.GetString('TField.PlanningSummary') + ' :';
    FInputSummary.Caption                   := FAppModules.Language.GetString('TField.InputSummary') + ' :';
    FAnnualSummaryRadioGroup.Caption        := ' ' + FAppModules.Language.GetString('TField.AnnualSummaryDescr') + ' ';
    FAnnualSummaryRadioGroup.Hints.Clear;
    AnnualSummaryRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryFlow'));
    AnnualSummaryRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryDemand'));
    AnnualSummaryRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryBoth'));
    AnnualSummaryRadioGroup.Hints.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryNeither'));

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationDialog.RestoreColourState;
const OPNAME = 'TOutputConfigurationDialog.RestoreColourState';
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

procedure TOutputConfigurationDialog.AssignHelpContext;
const OPNAME = 'TOutputConfigurationDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                     HC_OutputFileOptions);
    SetControlHelpContext(FSummaryLevelRadioGroup,  HC_OutputFileOptions);
    SetControlHelpContext(FDebugGroupBox,           HC_OutputFileOptions);
    SetControlHelpContext(FDebugLevelCbx  ,         HC_OutputFileOptions);
    SetControlHelpContext(FDebugStartYearCbx,       HC_OutputFileOptions);
    SetControlHelpContext(FDebugEndYearCbx,         HC_OutputFileOptions);
    SetControlHelpContext(FInputGroupBox,           HC_ResultOutputControl);
    SetControlHelpContext(FInputDataChkBox,         HC_OutputFileOptions);
    SetControlHelpContext(FYieldResultsChkBox,      HC_OutputFileOptions);
    SetControlHelpContext(FPlotFileChkBox,          HC_OutputFileOptions);
    SetControlHelpContext(FControlParameters,       HC_OutputFileOptions);
    SetControlHelpContext(FDetailedOptionChkBox,    HC_OutputFileOptions);
    SetControlHelpContext(FSupplyOptionChkBox,      HC_OutputFileOptions);
    SetControlHelpContext(FEconomicOption,          HC_OutputFileOptions);
    SetControlHelpContext(FPlanningSummary,         HC_OutputFileOptions);
    SetControlHelpContext(FInputSummary,            HC_OutputFileOptions);
    SetControlHelpContext(FAnnualSummaryRadioGroup, HC_OutputFileOptions);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
