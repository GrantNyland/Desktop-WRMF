{******************************************************************************}
{*  UNIT      : Contains the class TRunConfigurationValidator.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/15                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit URunConfigurationValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Forms,
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  URunConfigurationDialog;

type
  TRunConfigurationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnEditControlClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnReduceSequencesClick(Sender: TObject);
    procedure OnHistoricFirmYieldClick(Sender: TObject);
    procedure OnStochasticFirmYieldClick(Sender: TObject);
    procedure OnMultiplePeriodsClick(Sender: TObject);
    procedure OnLimitOptionClick(Sender: TObject);
    procedure OnPageControlChange(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure DisplaySequenceControls;
    procedure DisableModelControls;
    procedure RePopulateMonthsGrid;
    procedure RePopulateSequenceOrderGrid;
    procedure UpdateStartYear;
    procedure UpdateStartYearLabel;
    procedure UpdateNumberOfYears;
    procedure UpdateNumberOfPeriods;
    procedure UpdateStartMonth;
    procedure UpdateCalculateFirmYield(ASender: TObject);
    procedure UpdateLimitOption;
    procedure UpdateRunSequenceType;
    procedure CopySpecifiedInflowFiles;
    procedure UpdateSequenceStartYear;
    procedure UpdateMultiplePeriods;
    procedure UpdateReduceSequences;
    procedure UpdateGeneratedFlowFlag;
    procedure UpdateNrOfSequences;
    procedure UpdateTargetRecurrenceInterval;
    procedure UpdateStartSequence;
    procedure UpdateRunTitle1;
    procedure UpdateRunTitle2;
    procedure UpdateRunTitle3;
    procedure UpdateHydroUnitsCode;
    procedure UpdateMonthName(AMonth : integer; AName : string);
    procedure UpdateDaysInMonth(AMonth : integer; ADays : string);
    procedure UpdateSequenceOrder(AIndex : integer; AValue : string);
    procedure ValidateStartYearOther(AConfiguration : IRunConfigurationData);
    procedure ValidateStartYearGregorian(AConfiguration : IRunConfigurationData);
    procedure ValidateNumberOfYears(AConfiguration : IRunConfigurationData);
    procedure ValidateNumPeriods(AConfiguration : IRunConfigurationData);
    procedure ValidateStartMonthNo(AConfiguration : IRunConfigurationData);
    procedure ValidateMonthNames(AConfiguration : IRunConfigurationData);
    procedure ValidateMonthDays(AConfiguration : IRunConfigurationData);
    procedure ValidateHydroSeqCount(AConfiguration : IRunConfigurationData);
    procedure ValidateNumberOfSequenceInAnalysis(AConfiguration : IRunConfigurationData);
    procedure ValidateTargetRecurrenceInterval(AConfiguration : IRunConfigurationData);
    procedure ValidateRunTitle1(AConfiguration : IRunConfigurationData);
    procedure ValidateRunTitle2(AConfiguration : IRunConfigurationData);
    procedure ValidateRunTitle3(AConfiguration : IRunConfigurationData);
    procedure ValidateStartYearSequence(AConfiguration : IRunConfigurationData);
    procedure ValidateHydroUnitsCode(AConfiguration : IRunConfigurationData);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function RunConfigurationDialog : TRunConfigurationDialog;
    procedure DoContextValidation(AValidationType: TDialogValidationType);override;
  end;

implementation

uses
  VCL.Dialogs,
  SysUtils,
  UUtilities,
  VCL.Graphics,
  UConstants,
  Windows,
  UYieldModelDataObject,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, VCL.Grids;

{******************************************************************************}
{* TRunConfigurationValidator                                                 *}
{******************************************************************************}

procedure TRunConfigurationValidator.CreateMemberObjects;
const OPNAME = 'TRunConfigurationValidator.CreateMemberObjects';
var
  lPanel     : TRunConfigurationDialog;
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lButton    : TFieldButton;
  lChkBox    : TFieldChkBox;
  lRadioGrp  : TFieldRadioGroup;
  lDatePick  : TFieldDateTimePicker;
  lParent    : TControl;
begin
  try
    inherited CreateMemberObjects;
    FPanel  := TRunConfigurationDialog.Create(FPanelOwner,FAppModules);
    lPanel  := RunConfigurationDialog;
    with lPanel do
    begin
      StartYearEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('StartYearO');
      StartYearLabelEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('StartYearG');
      NumberOfYearsEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('YearsCount');
      NumberOfPeriodsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NumPeriods');
      StartMonthEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('StartMonthNo');
      MonthsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      MonthsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      MonthsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Days'));
      MonthsGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      MonthsGrid.OnColEnter         := OnStringGridColEnter;
      MonthsGrid.OnEnter            := OnEditControlEnter;

      ParamFileName.FieldProperty             := FAppModules.FieldProperties.FieldProperty('ParamFile');
      HistoricFirmYieldChkBox.FieldProperty   := FAppModules.FieldProperties.FieldProperty('CalcHistoryOpt');

      StochasticFirmYieldChkBox.FieldProperty := FAppmodules.FieldProperties.FieldProperty('CalcHistoryOpt');

      LimitOptionChkBox.FieldProperty         := FAppModules.FieldProperties.FieldProperty('LimitOpt');

      RunSequencePageControl.OnChange         := OnPageControlChange;

      SequenceStartYearEdit.FieldProperty     := FAppModules.FieldProperties.FieldProperty('SequenceStartYear');

      MultiplePeriodsChkBox.FieldProperty     := FAppModules.FieldProperties.FieldProperty('MultPeriodOpt');

      ReduceSequencesChkBox.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ReduceSeqOpt');

      StartTypeRadioGroup.FieldProperty       := FAppModules.FieldProperties.FieldProperty('StartType');
      StartTypeRadioGroup.OnClick             := OnEditControlClick;
      StartTypeRadioGroup.OnEnter             := OnEditControlEnter;
      StartTypeRadioGroup.OnExit              := OnEditControltExit;

      TargetRecurrenceIntervalEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('TargetRecurrenceInterval');
      TargetRecurrenceIntervalEdit.OnEnter        := OnEditControlEnter;
      TargetRecurrenceIntervalEdit.OnExit         := OnEditControltExit;

      if (FAppModules.Model.ModelName = CYield) then
        NrOfSequencesEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('HydroSeqCount')
      else
        NrOfSequencesEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('PlanningHydroSeqCount');
        
      StartSequenceEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('StartSequenceNumber');
      SequenceOrderGrid.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
      SequenceOrderGrid.OnColEnter            := OnStringGridColEnter;
      SequenceOrderGrid.OnEnter               := OnEditControlEnter;

      RunTitle1Edit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Title1');
      RunTitle1Edit.OnEnter          := OnEditControlEnter;
      RunTitle1Edit.OnExit           := OnEditControltExit;

      RunTitle2Edit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Title2');
      RunTitle2Edit.OnEnter          := OnEditControlEnter;
      RunTitle2Edit.OnExit           := OnEditControltExit;

      RunTitle3Edit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Title3');
      RunTitle3Edit.OnEnter          := OnEditControlEnter;
      RunTitle3Edit.OnExit           := OnEditControltExit;

      HydroUnitsCodeEdit.FieldProperty        := FAppModules.FieldProperties.FieldProperty('HydroUnitsCode');
      HydroUnitsCodeEdit.OnEnter              := OnEditControlEnter;
      HydroUnitsCodeEdit.OnExit               := OnEditControltExit;

      CalendarStartMonthEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('CalendarStartMonth');
      CalendarStartMonthEdit.OnEnter       := OnEditControlEnter;
      CalendarStartMonthEdit.OnExit        := OnEditControltExit;
    end;

    lParent := lPanel.ControlsParent;
    for lIndex := 0 to lParent.ComponentCount - 1 do
    begin
      lComponent := lParent.Components[lIndex];
      if (lComponent.ClassNameIs('TFieldEdit')) then
      begin
        lFieldEdit         := TFieldEdit(lComponent);
        lFieldEdit.OnEnter := OnEditControlEnter;
        lFieldEdit.OnExit  := OnEditControltExit;
      end
      else if (lComponent.ClassNameIs('TFieldComboBox')) then
      begin
        lFieldCbx         := TFieldComboBox(lComponent);
        lFieldCbx.OnEnter := OnEditControlEnter;
        lFieldCbx.OnExit  := OnEditControltExit;
      end
      else if (lComponent.ClassNameIs('TFieldButton')) then
      begin
        lButton         := TFieldButton(lComponent);
        lButton.OnEnter := OnEditControlEnter;
        lButton.OnExit  := OnEditControltExit;
      end
      else if (lComponent.ClassNameIs('TFieldChkBox')) then
      begin
        lChkBox         := TFieldChkBox(lComponent);
        lChkBox.OnEnter := OnEditControlEnter;
        lChkBox.OnClick := OnEditControlClick;
      end
      else if (lComponent.ClassNameIs('TFieldRadioGroup')) then
      begin
        lRadioGrp         := TFieldRadioGroup(lComponent);
        lRadioGrp.OnEnter := OnEditControlEnter;
        lRadioGrp.OnClick := OnEditControlClick;
      end
      else if (lComponent.ClassNameIs('TFieldDateTimePicker')) then
      begin
        lDatePick         := TFieldDateTimePicker(lComponent);
        lDatePick.OnEnter := OnEditControlEnter;
        lDatePick.OnExit  := OnEditControltExit;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.DestroyMemberObjects;
const OPNAME = 'TRunConfigurationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationValidator.Initialise: boolean;
const OPNAME = 'TRunConfigurationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with RunConfigurationDialog.StartTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.StartRandomly'));
      Items.Add(FAppModules.Language.GetString('RunParameters.StartHistorical'));
      Items.Add(FAppModules.Language.GetString('RunParameters.StartBootstrap'));
      if (FAppModules.StudyArea.ModelVersion = '7') then
        Items.Add(FAppModules.Language.GetString('RunParameters.OperationalUse'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TRunConfigurationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.ClearDataViewer;
const OPNAME = 'TRunConfigurationValidator.ClearDataViewer';
var
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lChkBox    : TFieldChkBox;
  lRadioGrp  : TFieldRadioGroup;
  lDatePick  : TFieldDateTimePicker;
begin
  inherited ClearDataViewer;
  try
    with RunConfigurationDialog do
    begin
      for lIndex := 0 to 11 do
      begin
        MonthsGrid.Cells[1, lIndex] := '';
        MonthsGrid.Cells[2, lIndex] := '-1';
      end;
      for lIndex := 0 to 9 do
        SequenceOrderGrid.Cells[lIndex, 0] := '-1';
      for lIndex := 0 to ControlsParent.ComponentCount - 1 do
      begin
        lComponent := ControlsParent.Components[lIndex];
        if (lComponent.ClassNameIs('TFieldEdit')) then
        begin
          lFieldEdit := TFieldEdit(lComponent);
          if (lFieldEdit.FieldProperty <> nil) then
          begin
            case lFieldEdit.FieldProperty.FieldDataType of
            1 : lFieldEdit.SetFieldValue(''); //String
            2 : lFieldEdit.SetFieldValue('-1'); //Float
            3 : lFieldEdit.SetFieldValue('-1'); //Integer
            else
            end;
          end
        end
        else if (lComponent.ClassNameIs('TFieldChkBox')) then
        begin
          lChkBox := TFieldChkBox(lComponent);
          lChkBox.Checked := FALSE;
        end
        else if (lComponent.ClassNameIs('TFieldComboBox')) then
        begin
          lFieldCbx := TFieldComboBox(lComponent);
          lFieldCbx.ItemIndex := -1;
        end
        else if (lComponent.ClassNameIs('TFieldRadioGroup')) then
        begin
          lRadioGrp := TFieldRadioGroup(lComponent);
          lRadioGrp.ItemIndex := -1;
        end
        else if (lComponent.ClassNameIs('TFieldDateTimePicker')) then
        begin
          lDatePick := TFieldDateTimePicker(lComponent);
          lDatePick.Date := 0;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.PopulateDataViewer;
const OPNAME = 'TRunConfigurationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.RePopulateDataViewer;
const OPNAME = 'TRunConfigurationValidator.RePopulateDataViewer';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with RunConfigurationDialog do
      begin
        lFieldIndex := '';

        lFieldProperty := StartYearEdit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        StartYearEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        StartYearEdit.SetFieldValue(IntToStr(lConfigData.StartYearOther));

        lFieldProperty := StartYearLabelEdit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        StartYearLabelEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        StartYearLabelEdit.SetFieldValue(IntToStr(lConfigData.StartYearGregorian));

        lFieldProperty := NumberOfYearsEdit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        NumberOfYearsEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        NumberOfYearsEdit.SetFieldValue(IntToStr(lConfigData.YearsInAnalysis));

        lFieldProperty := NumberOfPeriodsEdit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        NumberOfPeriodsEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        NumberOfPeriodsEdit.SetFieldValue(IntToStr(lConfigData.PeriodsInAnalysis));
        NumberOfPeriodsEdit.IsEnabled := (lConfigData.YearsInAnalysis <= 1);

        lFieldProperty := StartMonthEdit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        StartMonthEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        StartMonthEdit.SetFieldValue(IntToStr(lConfigData.StartMonthNumber));

        lFieldProperty := HydroUnitsCodeEdit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        HydroUnitsCodeEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        HydroUnitsCodeEdit.SetFieldValue(lConfigData.HydroUnitsCode);

        CalendarStartMonthEdit.SetFieldValue(lConfigData.CalendarStartMonth);

        RePopulateMonthsGrid;

        lFieldProperty := RunTitle1Edit.FieldProperty;
        lKeyValues     := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        RunTitle1Edit.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        RunTitle1Edit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        RunTitle1Edit.SetFieldValue(lConfigData.YieldRunTitle1);

        lFieldProperty := RunTitle2Edit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        RunTitle2Edit.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        RunTitle2Edit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        RunTitle2Edit.SetFieldValue(lConfigData.YieldRunTitle2);

        lFieldProperty := RunTitle3Edit.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        RunTitle3Edit.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        RunTitle3Edit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        RunTitle3Edit.SetFieldValue(lConfigData.YieldRunTitle3);

        lFieldProperty := HistoricFirmYieldChkBox.FieldProperty;
        lKeyValues     := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        HistoricFirmYieldChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        HistoricFirmYieldChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
        HistoricFirmYieldChkBox.Checked := ((lConfigData.CalculateHistoricFirmYield = 1) OR (lConfigData.CalculateHistoricFirmYield = 2));

        StochasticFirmYieldChkBox.HasChanges  := HistoricFirmYieldChkBox.HasChanges;
        StochasticFirmYieldChkBox.HasMetaData := HistoricFirmYieldChkBox.HasMetaData;
        StochasticFirmYieldChkBox.Checked     := HistoricFirmYieldChkBox.Checked;

        TargetRecurrenceIntervalEdit.IsEnabled := HistoricFirmYieldChkBox.Checked;

        if (FAppModules.StudyArea.ModelVersion <> '6') then
        begin
          TargetRecurrenceIntervalEdit.Visible := True;
          TargetRecurrenceIntervalLabel.Visible := TargetRecurrenceIntervalEdit.Visible;
          StochasticFirmYieldChkBox.Enabled := true;
        end
        else
        begin
          TargetRecurrenceIntervalEdit.Visible := False;
          TargetRecurrenceIntervalLabel.Visible := TargetRecurrenceIntervalEdit.Visible;
          StochasticFirmYieldChkBox.Enabled := False;
        end;

        if (lConfigData.RunSequenceType = 'H') then
        begin
          RunSequencePageControl.ActivePage := RunSequencePageControl.Pages[0];
          lFieldProperty := SequenceStartYearEdit.FieldProperty;
          lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          SequenceStartYearEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          SequenceStartYearEdit.SetFieldValue(lConfigData.HistoricSequenceStartYear);
        end
        else if (lConfigData.RunSequenceType = 'S') then
        begin
          RunSequencePageControl.ActivePage := RunSequencePageControl.Pages[1];
        end
        else
          RunSequencePageControl.ActivePage := RunSequencePageControl.Pages[0];

        lFieldProperty := LimitOptionChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        LimitOptionChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        LimitOptionChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        LimitOptionChkBox.Checked := lConfigdata.LimitOption;

        lFieldProperty := MultiplePeriodsChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        MultiplePeriodsChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        MultiplePeriodsChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        MultiplePeriodsChkBox.Checked := lConfigData.MultiplePeriodLengths;
        MultiplePeriodsChkBox.Enabled := (lConfigData.YearsInAnalysis <= 10);

        lFieldProperty := ReduceSequencesChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        ReduceSequencesChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        ReduceSequencesChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        ReduceSequencesChkBox.Checked := lConfigdata.ReduceSequences;
        SortDescendingLabel.Visible   := ReduceSequencesChkBox.Checked;

        lFieldProperty := StartTypeRadioGroup.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        StartTypeRadioGroup.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        StartTypeRadioGroup.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        StartTypeRadioGroup.ItemIndex := lConfigData.GeneratedFlowFlag;

        if(lConfigdata.NumberOfSequencesInAnalysis = NullInteger) then
          NrOfSequencesEdit.Text := ''
        else
        begin
          lFieldProperty := NrOfSequencesEdit.FieldProperty;
          lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          NrOfSequencesEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          NrOfSequencesEdit.SetFieldValue(IntToStr(lConfigdata.NumberOfSequencesInAnalysis));
        end;

        if(lConfigdata.StartSequenceNumber = NullInteger) then
          StartSequenceEdit.Text := ''
        else
        begin
          lFieldProperty := StartSequenceEdit.FieldProperty;
          lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          StartSequenceEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          StartSequenceEdit.SetFieldValue(IntToStr(lConfigdata.StartSequenceNumber));
        end;

        if(lConfigdata.TargetRecurrenceInterval = NullInteger) then
          TargetRecurrenceIntervalEdit.SetFieldValue(0)
        else
        begin
          lFieldProperty := TargetRecurrenceIntervalEdit.FieldProperty;
          lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          TargetRecurrenceIntervalEdit.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          TargetRecurrenceIntervalEdit.SetFieldValue(IntToStr(lConfigdata.TargetRecurrenceInterval));
        end;

        DisableModelControls;
        DisplaySequenceControls;
        RePopulateSequenceOrderGrid;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.DisplaySequenceControls;
const OPNAME = 'TRunConfigurationValidator.DisplaySequenceControls';
var
  lConfigData : IRunConfigurationData;
  lCount      : integer;
  lIndex      : integer;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lCount := lConfigData.NumberOfSequencesInAnalysis;
        if (lCount <= 10) then
        begin
          StartSequenceLabel.Visible := FALSE;
          StartSequenceEdit.Visible  := FALSE;
          SequenceOrderLabel.Visible := TRUE;
          with SequenceOrderGrid do
          begin
            Visible  := TRUE;
            ColCount := lCount;
            Width    := lCount * (DefaultColWidth + 1) + 3;
            ClearFieldProperties;
            for lIndex := 1 to lCount do
            begin
              SequenceOrderGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Seq'));
              if (lConfigData.RunSequenceType = 'H') then
                SequenceOrderGrid.Cells[lIndex-1,0] := IntToStr(lConfigData.SequenceToBeAnalysedByIndex[lIndex])
              else
                SequenceOrderGrid.Cells[lIndex-1,0] := IntToStr(lConfigData.SequenceToBeAnalysedByIndex[lIndex]+1);
            end;
          end
        end
        else
        begin
          if (lConfigData.RunSequenceType = 'H') then
            StartSequenceEdit.SetFieldValue(IntToStr(lConfigdata.StartSequenceNumber))
          else
            StartSequenceEdit.SetFieldValue(IntToStr(lConfigdata.StartSequenceNumber+1));
          SequenceOrderLabel.Visible := FALSE;
          SequenceOrderGrid.Visible  := FALSE;
          StartSequenceLabel.Visible := TRUE;
          StartSequenceEdit.Visible  := TRUE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.DisableModelControls;
const OPNAME = 'TRunConfigurationValidator.DisableModelControls';
begin
  try
    if (FAppModules.Model.ModelName = CPlanning) then
    begin
      RunConfigurationDialog.RunTitle1Edit.IsEnabled           := False;
      RunConfigurationDialog.RunTitle2Edit.IsEnabled           := False;
      RunConfigurationDialog.RunTitle3Edit.IsEnabled           := False;
      RunConfigurationDialog.HydroUnitsCodeEdit.IsEnabled      := False;
      RunConfigurationDialog.HistoricFirmYieldChkBox.IsEnabled := False;
      RunConfigurationDialog.SequenceStartYearEdit.IsEnabled   := False;
      RunConfigurationDialog.LimitOptionChkBox.IsEnabled       := False;
      RunConfigurationDialog.MultiplePeriodsChkBox.IsEnabled   := False;
      RunConfigurationDialog.ReduceSequencesChkBox.IsEnabled   := False;
      //RunConfigurationDialog.StartTypeRadioGroup.Enabled       := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.RePopulateMonthsGrid;
const OPNAME = 'TRunConfigurationValidator.RePopulateMonthsGrid';
var
  lCol           : integer;
  lKeyValues     : string;
  lFieldIndex    : string;
  lIndexA        : integer;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with RunConfigurationDialog do
      begin
        for lCol := 1 to MonthsGrid.ColCount-1 do
        begin
          lFieldProperty := MonthsGrid.FieldProperty(lCol);
          for lIndexA := 1 to 12 do
          begin
            lFieldIndex := IntToStr(lCol)+ ',' + IntToStr(lIndexA);
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
            MonthsGrid.HasMetaData[lCol, lIndexA-1] :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

            MonthsGrid.Cells[0, lIndexA-1] := IntToStr(lIndexA);
            MonthsGrid.Cells[1, lIndexA-1] := lConfigData.MonthNameByIndex[lIndexA];
            MonthsGrid.Cells[2, lIndexA-1] := FloatToStr(lConfigData.MonthDaysByIndex[lIndexA]);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.RePopulateSequenceOrderGrid;
const OPNAME = 'TRunConfigurationValidator.RePopulateSequenceOrderGrid';
var
  lConfigData : IRunConfigurationData;
  lIndexA     : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with RunConfigurationDialog do
      begin
        for lIndexA := 0 to 9 do
        begin
          if (lConfigData.RunSequenceType = 'H') then
            SequenceOrderGrid.Cells[lIndexA, 0] := IntToStr(lConfigdata.SequenceToBeAnalysedByIndex[lIndexA+1])
          else
            SequenceOrderGrid.Cells[lIndexA, 0] := IntToStr(lConfigdata.SequenceToBeAnalysedByIndex[lIndexA+1]+1);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationValidator.SaveState: boolean;
const OPNAME = 'TRunConfigurationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationValidator.RunConfigurationDialog : TRunConfigurationDialog;
const OPNAME = 'TRunConfigurationValidator.RunConfigurationDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TRunConfigurationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRunConfigurationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'CHANGELISTAPPLY') OR
       ((UpperCase(AFieldName) = 'CHANGELIST') AND (AContext = sdccDelete)) OR
       (UpperCase(AFieldName) = 'CHANGELISTCOPY')  OR
       (UpperCase(AFieldName) = 'ELEMENTACTIVE') OR
       (UpperCase(AFieldName) = 'ELEMENTORDER') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunConfigurationValidator.StudyHasChanged: boolean;
const OPNAME = 'TRunConfigurationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    with RunConfigurationDialog do
    begin
      if (Sender = HistoricFirmYieldChkBox) then
      begin
        FPanel.Hint := FAppModules.language.GetString('TField.CalcHistoryOpt');
        FPanel.ShowCurrentHint;
      end
      else
      if (Sender = StochasticFirmYieldChkBox) then
      begin
        FPanel.Hint := FAppModules.language.GetString('TField.CalcStochasticOpt');
        FPanel.ShowCurrentHint;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunConfigurationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with RunConfigurationDialog do
    begin
      if ((MonthsGrid = ASender) AND (ACol = 1)) then
        UpdateMonthName(ARow+1, Trim(MonthsGrid.Cells[ACol, ARow]))
      else
      if ((MonthsGrid = ASender) AND (ACol = 2)) then
        UpdateDaysInMonth(ARow+1, Trim(MonthsGrid.Cells[ACol, ARow]))
      else
      if (SequenceOrderGrid = ASender) then
        UpdateSequenceOrder(ACol+1, Trim(SequenceOrderGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateSequenceOrder(AIndex : integer;
                                                         AValue : string);
const OPNAME = 'TRunConfigurationValidator.UpdateSequenceOrder';
var
  lConfigData : IRunConfigurationData;
  lIntVal     : integer;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with RunConfigurationDialog do
      begin
      if (FAppModules.FieldProperties.ValidateFieldProperty('Seq', AValue,
        lMessage, AIndex)) then
      begin
        SequenceOrderGrid.ValidationError[0, 0, gveCellField] := '';
        lIntVal := StrToInt(Trim(AValue));
        if (lConfigData.RunSequenceType = 'H') then
          lConfigData.SequenceToBeAnalysedByIndex[AIndex] := lIntVal
        else
          lConfigData.SequenceToBeAnalysedByIndex[AIndex] := lIntVal-1;
        RePopulateSequenceOrderGrid;
        DoContextValidation(dvtSequenceInAnalysis);
      end
      else
        SequenceOrderGrid.ValidationError[0, 0, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateMonthName(AMonth : integer;
                                                     AName  : string);
const OPNAME = 'TRunConfigurationValidator.UpdateMonthName';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with RunConfigurationDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('Month', AName,
            lMessage, AMonth)) then
        begin
          MonthsGrid.ValidationError[1, AMonth-1, gveCellField] := '';
          lConfigData.MonthNameByIndex[AMonth] := Trim(AName);
          RePopulateMonthsGrid;
          DoContextValidation(dvtMonthNames);
        end
        else
          MonthsGrid.ValidationError[1, AMonth-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateDaysInMonth(AMonth : integer;
                                                       ADays  : string);
const OPNAME = 'TRunConfigurationValidator.UpdateDaysInMonth';
var
  lConfigData : IRunConfigurationData;
  lRealVal    : double;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with RunConfigurationDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('Days', ADays,
            lMessage, AMonth)) then
        begin
          MonthsGrid.ValidationError[2, AMonth-1, gveCellField] := '';
          lRealVal := StrToFloat(Trim(ADays));
          if (AMonth = 5) then
            if (StrToFloat(ADays) > 29.00) then
            begin
              lMessage := 'February cannot have nore than 29 days.';
              MonthsGrid.ValidationError[2, AMonth-1, gveCellField] := lMessage;
              Exit;
            end;
          lConfigData.MonthDaysByIndex[AMonth] := lRealVal;
          RePopulateMonthsGrid;
          DoContextValidation(dvtMonthsDays);
        end
        else
          MonthsGrid.ValidationError[2, AMonth-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TRunConfigurationValidator.OnRunTypeClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnRunTypeClick';
begin
  try
    with RunConfigurationDialog do
    begin
      if (RunTypeRadioGroup.ItemIndex = 0) then
      begin
        RunSequencePageControl.Pages[1].TabVisible := False;
        RunSequencePageControl.Pages[0].TabVisible := True;
        RunSequencePageControl.ActivePage := HistoricTabSheet;
        RunSequencePageControl.TabIndex := 0;
      end
      else if (RunTypeRadioGroup.ItemIndex = 1) then
      begin
        RunSequencePageControl.Pages[0].TabVisible := False;
        RunSequencePageControl.Pages[1].TabVisible := True;
        RunSequencePageControl.ActivePage := StochasticTabSheet;
        RunSequencePageControl.TabIndex := 0;
      end;
    end;
    OnEditControlEnter(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TRunConfigurationValidator.OnReduceSequencesClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnReduceSequencesClick';
begin
  try
    if(RunConfigurationDialog.ReduceSequencesChkBox.HasValueChanged) then
      UpdateReduceSequences;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnHistoricFirmYieldClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnHistoricFirmYieldClick';
begin
  try
    with RunConfigurationDialog do
    begin
      if(HistoricFirmYieldChkBox.HasValueChanged) then
        UpdateCalculateFirmYield(Sender);
      TargetRecurrenceIntervalEdit.IsEnabled := StochasticFirmYieldChkBox.Checked;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnStochasticFirmYieldClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnStochasticFirmYieldClick';
begin
  try
    with RunConfigurationDialog do
    begin
      if(StochasticFirmYieldChkBox.HasValueChanged) then
        UpdateCalculateFirmYield(Sender);
      TargetRecurrenceIntervalEdit.IsEnabled  := StochasticFirmYieldChkBox.Checked;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnMultiplePeriodsClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnMultiplePeriodsClick';
begin
  try
    if(RunConfigurationDialog.MultiplePeriodsChkBox.HasValueChanged) then
      UpdateMultiplePeriods;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnLimitOptionClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnLimitOptionClick';
begin
  try
    if(RunConfigurationDialog.LimitOptionChkBox.HasValueChanged) then
      UpdateLimitOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with RunConfigurationDialog do
    begin
      if ((Sender = StartYearEdit) AND (StartYearEdit.HasValueChanged)) then
        UpdateStartYear
      else
      if ((Sender = StartYearLabelEdit) AND (StartYearLabelEdit.HasValueChanged))then
        UpdateStartYearLabel
      else
      if ((Sender = NumberOfYearsEdit) AND (NumberOfYearsEdit.HasValueChanged)) then
        UpdateNumberOfYears
      else
      if ((Sender = NumberOfPeriodsEdit) AND (NumberOfPeriodsEdit.HasValueChanged)) then
        UpdateNumberOfPeriods
      else
      if ((Sender = StartMonthEdit) AND (StartMonthEdit.HasValueChanged)) then
        UpdateStartMonth
      else
      if ((Sender = SequenceStartYearEdit) AND (SequenceStartYearEdit.HasValueChanged)) then
        UpdateSequenceStartYear
      else
      if ((Sender = NrOfSequencesEdit) AND NrOfSequencesEdit.HasValueChanged) then
        UpdateNrOfSequences
      else
      if ((sender = TargetRecurrenceIntervalEdit) AND (TargetRecurrenceIntervalEdit.HasValueChanged)) then
        updateTargetRecurrenceInterval
      else
      if ((Sender = StartSequenceEdit) AND (StartSequenceEdit.HasValueChanged)) then
        UpdateStartSequence
      else
      if ((Sender = RunTitle1Edit) AND (RunTitle1Edit.HasValueChanged)) then
        UpdateRunTitle1
      else
      if ((sender = RunTitle2Edit) AND (RunTitle2Edit.HasValueChanged)) then
        UpdateRunTitle2
      else
      if ((sender = RunTitle3Edit) AND (RunTitle3Edit.HasValueChanged)) then
        UpdateRunTitle3
      else
      if ((sender = HydroUnitsCodeEdit) AND (HydroUnitsCodeEdit.HasValueChanged)) then
        UpdateHydroUnitsCode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateStartYear;
const OPNAME = 'TRunConfigurationValidator.UpdateStartYear';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'StartYearO', StartYearEdit.Text, lMessage)) then
        begin
          StartYearEdit.FieldValidationError := lMessage;
          lConfigData.StartYearOther := StrToInt(Trim(StartYearEdit.Text));
          StartYearEdit.SetFieldValue(lConfigData.StartYearOther);
          RePopulateDataViewer;
          DoContextValidation(dvtStartYearO);
        end
        else
          StartYearEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateStartYearLabel;
const OPNAME = 'TRunConfigurationValidator.UpdateStartYearLabel';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'StartYearG', StartYearLabelEdit.Text, lMessage)) then
        begin
          StartYearLabelEdit.FieldValidationError := lMessage;
          lConfigData.StartYearGregorian := StrToInt(Trim(StartYearLabelEdit.Text));
          StartYearLabelEdit.SetFieldValue(IntToStr(lConfigData.StartYearGregorian));
          DoContextValidation(dvtStartYearG);
        end
        else
          StartYearLabelEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateNumberOfYears;
const OPNAME = 'TRunConfigurationValidator.UpdateNumberOfYears';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'YearsCount', NumberOfYearsEdit.Text, lMessage)) then
        begin
          NumberOfYearsEdit.FieldValidationError := lMessage;
          lConfigData.YearsInAnalysis := StrToInt(Trim(NumberOfYearsEdit.Text));
          NumberOfYearsEdit.SetFieldValue(IntToStr(lConfigData.YearsInAnalysis));
          NumberOfPeriodsEdit.IsEnabled := (lConfigData.YearsInAnalysis <= 1);
          NumberOfPeriodsEdit.SetFieldValue(IntToStr(lConfigData.PeriodsInAnalysis));
          lConfigData.EndDebugPeriod := lConfigData.PeriodsInAnalysis;
          DoContextValidation(dvtNumberOfYears);
          RePopulateDataViewer;
        end
        else
          NumberOfYearsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateNumberOfPeriods;
const OPNAME = 'TRunConfigurationValidator.UpdateNumberOfPeriods';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('NumPeriods',
            NumberOfPeriodsEdit.Text, lMessage)) then
        begin
          NumberOfPeriodsEdit.FieldValidationError := lMessage;
          lConfigData.PeriodsInAnalysis := StrToInt(Trim(NumberOfPeriodsEdit.Text));
          NumberOfPeriodsEdit.SetFieldValue(IntToStr(lConfigData.PeriodsInAnalysis));
          DoContextValidation(dvtNumberOfPeriods);
        end
        else
          NumberOfPeriodsEdit.FieldValidationError := lMessage;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateStartMonth;
const OPNAME = 'TRunConfigurationValidator.UpdateStartMonth';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('StartMonthNo',
            StartMonthEdit.Text, lMessage)) then
        begin
          StartMonthEdit.FieldValidationError := lMessage;
          lConfigData.StartMonthNumber := StrToInt(Trim(StartMonthEdit.Text));
          StartMonthEdit.SetFieldValue(IntToStr(lConfigData.StartMonthNumber));
          DoContextValidation(dvtStartMonthNo);
        end
        else
          StartMonthEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateCalculateFirmYield(ASender: TObject);
const OPNAME = 'TRunConfigurationValidator.UpdateCalculateFirmYield';
var
  lConfigData    : IRunConfigurationData;
  lCalcFirmYield : Integer;
  LSenderChecked : Boolean;
  LModelVersion  : String;
  LTargetRI      : integer;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        LTargetRI := 0;
        LSenderChecked := TFieldChkBox(ASender).Checked;
        if LSenderChecked then
        begin
          LModelVersion := FAppModules.StudyArea.ModelVersion;
          if (LModelVersion <> '6') then
          begin
            if (lConfigData.RunSequenceType = 'H') then
              lCalcFirmYield := 1
            else
            begin
              lCalcFirmYield := 2;
            end;
          end
          else
            lCalcFirmYield := 1;
        end
        else
          lCalcFirmYield := 0;

        lConfigData.CalculateHistoricFirmYield := lCalcFirmYield;
        HistoricFirmYieldChkBox.Checked        := LSenderChecked;
        lConfigData.TargetRecurrenceInterval   := LTargetRI;
        TargetRecurrenceIntervalEdit.SetFieldValue(LTargetRI);
        StochasticFirmYieldChkBox.Checked      := HistoricFirmYieldChkBox.Checked;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateLimitOption;
const OPNAME = 'TRunConfigurationValidator.UpdateLimitOption';
var
  lConfigData  : IRunConfigurationData;
  lLimitOption : Boolean;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lLimitOption := lConfigData.LimitOption;
        if ((lLimitOption  AND (NOT LimitOptionChkBox.Checked)) OR
            ((NOT lLimitOption) AND LimitOptionChkBox.Checked)) then
        begin
          lConfigData.LimitOption := LimitOptionChkBox.Checked;
          LimitOptionChkBox.Checked := lConfigData.LimitOption;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateRunSequenceType;
const OPNAME = 'TRunConfigurationValidator.UpdateRunSequenceType';
var
  lConfigData : IRunConfigurationData;
  lOldType    : string;
  lNewType    : string;
  LIndex      : integer;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lOldType := lConfigData.RunSequenceType;
        if (RunSequencePageControl.ActivePageIndex = 0) then
        begin
          lNewType := 'H';
          lConfigData.GeneratedFlowFlag := 0;
        end
        else if (RunSequencePageControl.ActivePageIndex = 1) then
        begin
          lNewType := 'S';
          lConfigData.OutputSummaryLevel := 0;
        end
        else
          lNewType := '';
        if (lOldType <> lNewType) then
        begin
          lConfigData.RunSequenceType := lNewType;
          for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.SpecifiedDemandFeatureList.
                             SpecifiedDemandFeatureCount-1 do
            TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
            SpecifiedDemandFeatureList.SpecifiedDemandFeatureByIndex[LIndex].StochasticIndicator := lNewType;
          RePopulateDataViewer;
        end;
      end;
      CopySpecifiedInflowFiles;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateSequenceStartYear;
const OPNAME = 'TRunConfigurationValidator.UpdateSequenceStartYear';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
  LHistoricStartYear: integer;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('SequenceStartYear',
            SequenceStartYearEdit.Text, lMessage)) then
        begin
          SequenceStartYearEdit.FieldValidationError := lMessage;
          LHistoricStartYear := StrToInt(Trim(SequenceStartYearEdit.Text));
          if (lConfigData.HistoricSequenceStartYear  <> LHistoricStartYear) then
          begin
            lConfigData.HistoricSequenceStartYear := LHistoricStartYear;
            DoContextValidation(dvtSequenceStartYear);
            RePopulateDataViewer;
          end
        end
        else
          SequenceStartYearEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateMultiplePeriods;
const OPNAME = 'TRunConfigurationValidator.UpdateMultiplePeriods';
var
  lConfigData : IRunConfigurationData;
  lMultiplePeriods : Boolean;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lMultiplePeriods := lConfigData.MultiplePeriodLengths;
        if ((lMultiplePeriods  AND (NOT MultiplePeriodsChkBox.Checked)) OR
            ((NOT lMultiplePeriods) AND MultiplePeriodsChkBox.Checked)) then
        begin
          lConfigData.MultiplePeriodLengths := MultiplePeriodsChkBox.Checked;
          MultiplePeriodsChkBox.Checked := lConfigdata.MultiplePeriodLengths;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateReduceSequences;
const OPNAME = 'TRunConfigurationValidator.UpdateReduceSequences';
var
  lConfigData : IRunConfigurationData;
  lReduceSeq  : Boolean;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lReduceSeq := lConfigData.ReduceSequences;
        if ((lReduceSeq  AND (NOT ReduceSequencesChkBox.Checked)) OR
            ((NOT lReduceSeq) AND ReduceSequencesChkBox.Checked)) then
        begin
          lConfigData.ReduceSequences := ReduceSequencesChkBox.Checked;
          ReduceSequencesChkBox.Checked := lConfigdata.ReduceSequences;
          SortDescendingLabel.Visible   := ReduceSequencesChkBox.Checked;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateGeneratedFlowFlag;
const OPNAME = 'TRunConfigurationValidator.UpdateGeneratedFlowFlag';
var
  lConfigData : IRunConfigurationData;
  lOldFlow    : integer;
  lNewFlow    : integer;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lOldFlow := lConfigData.GeneratedFlowFlag;
        lNewFlow := StartTypeRadioGroup.ItemIndex;
        if (lOldFlow <> lNewFlow) then
        begin
          lConfigData.GeneratedFlowFlag := lNewFlow;
          StartTypeRadioGroup.ItemIndex := lConfigData.GeneratedFlowFlag;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateNrOfSequences;
const OPNAME = 'TRunConfigurationValidator.UpdateNrOfSequences';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('HydroSeqCount',
            NrOfSequencesEdit.Text, lMessage)) then
        begin
          NrOfSequencesEdit.FieldValidationError := lMessage;
          lConfigData.NumberOfSequencesInAnalysis := StrToInt(Trim(NrOfSequencesEdit.Text));
          NrOfSequencesEdit.SetFieldValue(IntToStr(lConfigData.NumberOfSequencesInAnalysis));
          DisplaySequenceControls;
          DoContextValidation(dvtNumberOfSequences);
          DoContextValidation(dvtSequenceInAnalysis);
        end
        else
          NrOfSequencesEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.UpdateStartSequence;
const OPNAME = 'TRunConfigurationValidator.UpdateStartSequence';
var
  lConfigData : IRunConfigurationData;
  lMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('StartSequenceNumber',
            StartSequenceEdit.Text, lMessage)) then
        begin
          StartSequenceEdit.FieldValidationError := lMessage;
          if (lConfigData.RunSequenceType = 'H') then
          begin
            lConfigData.StartSequenceNumber := StrToInt(Trim(StartSequenceEdit.Text));
            StartSequenceEdit.SetFieldValue(IntToStr(lConfigData.StartSequenceNumber));
          end
          else
          begin
            lConfigData.StartSequenceNumber := (StrToInt(Trim(StartSequenceEdit.Text))-1);
            StartSequenceEdit.SetFieldValue(IntToStr(lConfigData.StartSequenceNumber+1));
          end;
          DoContextValidation(dvtSequenceInAnalysis);
        end
        else
          StartSequenceEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunConfigurationValidator.ValidateHydroSeqCount(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateHydroSeqCount';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'NumberOfSequences');
      NrOfSequencesEdit.FieldValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateMonthDays(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateMonthDays';
var
  lCol       : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (AConfiguration <> nil) then
    begin
      with RunConfigurationDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AConfiguration.Validate(FErrorMessage,'MonthsDays')) then
          begin
            for lCol := 1 to 12 do
              MonthsGrid.ValidationError[2, lCol-1, gveCellContext] := '';
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to 12 do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                MonthsGrid.ValidationError[2, lCol-1 , gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                MonthsGrid.ValidationError[2, lCol-1 , gveCellContext] := ''
            end;
            FAllErrorMessages.AddStrings(lErrorMsgs);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateNumberOfSequenceInAnalysis(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateNumberOfSequenceInAnalysis';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    with RunConfigurationDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AConfiguration.Validate(FErrorMessage,'SequenceInAnalysis')) then
        begin
          if (AConfiguration.NumberOfSequencesInAnalysis <= 10) then
          begin
            for lCol := 1 to  AConfiguration.NumberOfSequencesInAnalysis do
              SequenceOrderGrid.ValidationError[lCol-1, 0, gveCellContext] := '';
          end;
        end
        else
        begin
          if (AConfiguration.NumberOfSequencesInAnalysis <= 10) then
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1  to  AConfiguration.NumberOfSequencesInAnalysis do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                SequenceOrderGrid.ValidationError[lCol-1, 0, gveCellContext] := lErrorMessages.Strings[lIndex]
              else
                SequenceOrderGrid.ValidationError[lCol-1, 0, gveCellContext] := '';
            end;
              FAllErrorMessages.AddStrings(lErrorMessages);
          end;
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateNumberOfYears(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateNumberOfYears';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'NumberOfYears');
      NumberOfYearsEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateNumPeriods(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateNumPeriods';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'NumberOfPeriods');
      NumberOfPeriodsEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateStartMonthNo(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateStartMonthNo';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'StartMonthNo');
      StartMonthEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateStartYearGregorian(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateStartYearGregorian';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'StartYearGregorian');
      StartYearLabelEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do  HandleError(E, OPNAME);end;
end;

procedure TRunConfigurationValidator.ValidateStartYearOther(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateStartYearOther';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'StartYearOther');
      StartYearEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TRunConfigurationValidator.DoContextValidation';
var
  LConfiguration : IRunConfigurationData;
begin
  try
    LConfiguration := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (LConfiguration <> nil) then
    begin
      if (AValidationType in [dvtConfigurationAll, dvtStartYearO]) then
      begin
        ValidateStartYearOther(LConfiguration);
        ValidateStartYearSequence(LConfiguration);
      end;
      if (AValidationType in [dvtConfigurationAll, dvtStartYearG]) then
      begin
        ValidateStartYearGregorian(LConfiguration);
        ValidateStartYearSequence(LConfiguration);
      end;
      if (AValidationType in [dvtConfigurationAll, dvtNumberOfYears, dvtRunType]) then
      begin
        ValidateNumberOfYears(LConfiguration);
        ValidateStartYearSequence(LConfiguration);
      end;
      if (AValidationType in [dvtConfigurationAll, dvtNumberOfPeriods]) then
        ValidateNumPeriods(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtStartMonthNo]) then
        ValidateStartMonthNo(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtMonthNames]) then
        ValidateMonthNames(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtMonthsDays]) then
        ValidateMonthDays(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtNumberOfSequences]) then
      begin
        ValidateHydroSeqCount(LConfiguration);
        ValidateNumberOfSequenceInAnalysis(LConfiguration);
      end;
      if (AValidationType in [dvtConfigurationAll,dvtSequenceInAnalysis]) then
        ValidateNumberOfSequenceInAnalysis(LConfiguration);
      if (AValidationType in [dvtConfigurationAll,dvtTargetRecurrenceInterval]) then
        ValidateTargetRecurrenceInterval(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtRunTitle1 ]) then
        ValidateRunTitle1(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtRunTitle2 ]) then
        ValidateRunTitle2(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtRunTitle3 ]) then
        ValidateRunTitle3(LConfiguration);
      if (AValidationType in [dvtConfigurationAll, dvtSequenceStartYear]) then
        ValidateStartYearSequence(LConfiguration);
      if (AValidationType = dvtConfigurationAll) or (AValidationType = dvtHydroUnitsCode) then
        ValidateHydroUnitsCode(LConfiguration);
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateMonthNames(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateMonthNames';
var
  lCol       : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    with RunConfigurationDialog do
    begin
      lErrorCols := TStringList.Create;
      lErrorMsgs := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AConfiguration.Validate(FErrorMessage,'MonthNames')) then
        begin
          for lCol := 1 to 12 do
              MonthsGrid.ValidationError[1, lCol-1, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              MonthsGrid.ValidationError[1, lCol-1 , gveCellContext] := lErrorMsgs.Strings[lIndex]
            else
              MonthsGrid.ValidationError[1, lCol-1 , gveCellContext] := ''
          end;
          FAllErrorMessages.AddStrings(lErrorMsgs);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMsgs);
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.UpdateTargetRecurrenceInterval;
const OPNAME = 'TRunConfigurationValidator.UpdateTargetRecurrenceInterval';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('TargetRecurrenceInterval',
                       TargetRecurrenceIntervalEdit.Text, LMessage)) then
        begin
          TargetRecurrenceIntervalEdit.ContextValidationError := LMessage;
          LConfigData.TargetRecurrenceInterval := StrToInt(Trim(TargetRecurrenceIntervalEdit.Text));
          TargetRecurrenceIntervalEdit.SetFieldValue(IntToStr(LConfigData.TargetRecurrenceInterval));
          DoContextValidation(dvtTargetRecurrenceInterval);
        end
        else
          TargetRecurrenceIntervalEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnEditControlClick';
begin
  try
    with RunConfigurationDialog do
    begin
      if(Sender = StartTypeRadioGroup) then
      begin
        if((StartTypeRadioGroup.ItemIndex = 0) OR  (StartTypeRadioGroup.ItemIndex = 1 ))then
        begin
          Rigorouslabel.Visible := True;
        end
        else
          Rigorouslabel.Visible := False;
        if(StartTypeRadioGroup.HasValueChanged) then
          UpdateGeneratedFlowFlag;
        OnEditControlEnter(Sender);
      end
      else
      if((Sender = ReduceSequencesChkBox) AND (ReduceSequencesChkBox.HasValueChanged)) then
        UpdateReduceSequences
      else
      if((Sender = HistoricFirmYieldChkBox) AND (HistoricFirmYieldChkBox.HasValueChanged)) then
      begin
        UpdateCalculateFirmYield(Sender);
        TargetRecurrenceIntervalEdit.IsEnabled := StochasticFirmYieldChkBox.Checked;
      end
      else
      if((Sender = StochasticFirmYieldChkBox) AND (StochasticFirmYieldChkBox.HasValueChanged)) then
      begin
        UpdateCalculateFirmYield(Sender);
        TargetRecurrenceIntervalEdit.IsEnabled  := StochasticFirmYieldChkBox.Checked;
      end
      else
      if((Sender = MultiplePeriodsChkBox) AND (MultiplePeriodsChkBox.HasValueChanged)) then
        UpdateMultiplePeriods
      else
      if((Sender = LimitOptionChkBox) AND (LimitOptionChkBox.HasValueChanged)) then
        UpdateLimitOption;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateTargetRecurrenceInterval(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateTargetRecurrenceInterval';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'TargetRecurrenceInterval');
      TargetRecurrenceIntervalEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.UpdateRunTitle1;
const OPNAME = 'TRunConfigurationValidator.UpdateRunTitle1';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('Title1',
                       RunTitle1Edit.Text, LMessage)) then
        begin
          RunTitle1Edit.ContextValidationError := LMessage;
          LConfigData.YieldRunTitle1 := Trim(RunTitle1Edit.Text);
          RunTitle1Edit.SetFieldValue(LConfigData.YieldRunTitle1);
          DoContextValidation(dvtRunTitle1);
        end
        else
          RunTitle1Edit.ContextValidationError := LMessage;
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.UpdateRunTitle2;
const OPNAME = 'TRunConfigurationValidator.UpdateRunTitle2';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty('Title2',
                                      RunTitle2Edit.Text, LMessage) then
        begin
          RunTitle2Edit.ContextValidationError := LMessage;
          LConfigData.YieldRunTitle2 := Trim(RunTitle2Edit.Text);
          RunTitle2Edit.SetFieldValue(LConfigData.YieldRunTitle2);
          DoContextValidation(dvtRunTitle2);
        end
        else
          RunTitle2Edit.ContextValidationError := LMessage;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.UpdateRunTitle3;
const OPNAME = 'TRunConfigurationValidator.UpdateRunTitle3';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty('Title3',
                                      RunTitle3Edit.Text, LMessage) then
        begin
          RunTitle3Edit.ContextValidationError := LMessage;
          LConfigData.YieldRunTitle3 := Trim(RunTitle3Edit.Text);
          RunTitle3Edit.SetFieldValue(LConfigData.YieldRunTitle3);
          DoContextValidation(dvtRunTitle3);
        end
        else
          RunTitle3Edit.ContextValidationError := LMessage;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.UpdateHydroUnitsCode;
const OPNAME = 'TRunConfigurationValidator.UpdateHydroUnitsCode';
var
  LConfigData : IRunConfigurationData;
  LMessage    : string;
begin
  try
    with RunConfigurationDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LConfigData <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty('HydroUnitsCode',
                                      HydroUnitsCodeEdit.Text, LMessage) then
        begin
          HydroUnitsCodeEdit.ContextValidationError := LMessage;
          LConfigData.HydroUnitsCode := Trim(HydroUnitsCodeEdit.Text);
          HydroUnitsCodeEdit.SetFieldValue(LConfigData.HydroUnitsCode);
          DoContextValidation(dvtHydroUnitsCode);
        end
        else
          HydroUnitsCodeEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateRunTitle1(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateRunTitle1';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'RunTitle1');
      RunTitle1Edit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateRunTitle2(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateRunTitle2';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'RunTitle2');
      RunTitle2Edit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateRunTitle3(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateRunTitle3';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'RunTitle3');
      RunTitle3Edit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateStartYearSequence(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateStartYearSequence';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'SequenceStartYear');
      SequenceStartYearEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.ValidateHydroUnitsCode(AConfiguration: IRunConfigurationData);
const OPNAME = 'TRunConfigurationValidator.ValidateHydroUnitsCode';
begin
  try
    with RunConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage, 'HydroUnitsCode');
      HydroUnitsCodeEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.OnPageControlChange(Sender: TObject);
const OPNAME = 'TRunConfigurationValidator.OnPageControlChange';
begin
  try
    UpdateRunSequenceType;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TRunConfigurationValidator.ProcessMetaDataEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        with RunConfigurationDialog do
        begin
          lFieldIndex         := '';
          lFieldProperty      := nil;
          if (FActiveControl  = StartYearEdit) then
             lFieldProperty   := StartYearEdit.FieldProperty
          else
          if (FActiveControl  = StartYearLabelEdit) then
             lFieldProperty   := StartYearLabelEdit.FieldProperty
          else
          if (FActiveControl  = NumberOfYearsEdit) then
             lFieldProperty   := NumberOfYearsEdit.FieldProperty
          else
          if (FActiveControl = NumberOfPeriodsEdit) then
             lFieldProperty   :=  NumberOfPeriodsEdit.FieldProperty
          else
          if (FActiveControl  = StartMonthEdit) then
             lFieldProperty   := StartMonthEdit.FieldProperty
          else
          if (FActiveControl  = MonthsGrid) then
          begin
            lFieldIndex       := IntToStr(MonthsGrid.Col)+ ',' + IntToStr(MonthsGrid.Row+1);
            lFieldProperty    := MonthsGrid.FieldProperty(MonthsGrid.Col);
          end
          else
          if (FActiveControl  = RunTitle1Edit) then
             lFieldProperty   := RunTitle1Edit.FieldProperty
          else
          if (FActiveControl  = RunTitle2Edit) then
             lFieldProperty   := RunTitle2Edit.FieldProperty
          else
          if (FActiveControl  = RunTitle3Edit) then
             lFieldProperty   := RunTitle3Edit.FieldProperty
          else
          if (FActiveControl  = HistoricFirmYieldChkBox) then
             lFieldProperty   := HistoricFirmYieldChkBox.FieldProperty
          else
          if (FActiveControl  = SequenceStartYearEdit) then
             lFieldProperty   := SequenceStartYearEdit.FieldProperty
          else
          if (FActiveControl  = LimitOptionChkBox) then
            lFieldProperty    := LimitOptionChkBox.FieldProperty
          else
          if (FActiveControl = TargetRecurrenceIntervalEdit) then
             lFieldProperty   := TargetRecurrenceIntervalEdit.FieldProperty
          else
          if (FActiveControl  = StochasticFirmYieldChkBox) then
            lFieldProperty    := StochasticFirmYieldChkBox.FieldProperty
          else
          if (FActiveControl = MultiplePeriodsChkBox) then
            lFieldProperty := MultiplePeriodsChkBox.FieldProperty
          else
          if (FActiveControl = ReduceSequencesChkBox) then
            lFieldProperty := ReduceSequencesChkBox.FieldProperty
          else
          if (FActiveControl = StartTypeRadioGroup) then
             lFieldProperty := StartTypeRadioGroup.FieldProperty
          else
          if (FActiveControl = NrOfSequencesEdit) then
             lFieldProperty := NrOfSequencesEdit.FieldProperty
          else
          if (FActiveControl = SequenceOrderGrid) then
          begin
            lFieldIndex := IntToStr(SequenceOrderGrid.Col+1);
            lFieldProperty := SequenceOrderGrid.FieldProperty(SequenceOrderGrid.Col);
          end;
          if (FActiveControl = StartSequenceEdit) then
            lFieldProperty := StartSequenceEdit.FieldProperty;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunConfigurationValidator.ProcessParameterChangeEvent: boolean;
const OPNAME = 'TRunConfigurationValidator.ProcessParameterChangeEvent';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lRunConfigData : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if ((FPanel.Visible) AND (FActiveControl <> nil)) then
    begin
      lRunConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lRunConfigData <> nil ) then
      begin
        with RunConfigurationDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl =  LimitOptionChkBox) then
            lFieldProperty := LimitOptionChkBox.FieldProperty
          else
          if (FActiveControl = MultiplePeriodsChkBox) then
            lFieldProperty := MultiplePeriodsChkBox.FieldProperty
          else
          if (FActiveControl = ReduceSequencesChkBox) then
            lFieldProperty := ReduceSequencesChkBox.FieldProperty
          else
          if (FActiveControl = HistoricFirmYieldChkBox) then
            lFieldProperty := HistoricFirmYieldChkBox.FieldProperty
          else
          if (FActiveControl = StochasticFirmYieldChkBox) then
            lFieldProperty := StochasticFirmYieldChkBox.FieldProperty
          else
          if (FActiveControl = StartTypeRadioGroup) then
            lFieldProperty := StartTypeRadioGroup.FieldProperty
          else
          if (FActiveControl = RunTitle1Edit) then
            lFieldProperty := RunTitle1Edit.FieldProperty
          else
          if (FActiveControl = RunTitle2Edit) then
            lFieldProperty := RunTitle2Edit.FieldProperty
          else
          if (FActiveControl = RunTitle3Edit) then
            lFieldProperty := RunTitle3Edit.FieldProperty;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lRunConfigData.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.Changes.ShowParameterChanges(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            FAppModules.Changes.SetParameterChanges(TRUE);
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRunConfigurationValidator.CopySpecifiedInflowFiles;
const OPNAME = 'TRunConfigurationValidator.CopySpecifiedInflowFiles';
var
  LInflowFeatureList : ISpecifiedInflowFeatureList;
  LInflowFeature     : ISpecifiedInflowFeature;
  LIndex             : integer;
  LRunSequenceType,
  LFileName,
  LFileExt,
  LFileNamePrefix,
  LPrefix,
  LNameAndExt,
  LSuffix,
  LNewFileName       : string;
begin
  try
    LInflowFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.SpecifiedInflowFeatureList;
    for LIndex := 0 to LInflowFeatureList.SpecifiedInflowFeatureCount - 1 do
    begin
      LInflowFeature  := LInflowFeatureList.SpecifiedInflowFeatureByIndex[LIndex];
      LFileName       := LInflowFeature.InflowFileName;
      if(LFileName <> '') then
      begin
        LNameAndExt      := ExtractFileName(LFileName);
        LFileNamePrefix  := Copy(LNameAndExt,0,Length(LNameAndExt) - 4);
        LPrefix          := Copy(LFileNamePrefix,0,Length(LFileNamePrefix) - 1);
        LSuffix          := Copy(LFileNamePrefix,Length(LFileNamePrefix),1);
        LFileExt         := ExtractFileExt(LNameAndExt);
        LRunSequenceType := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType;
        LNewFileName     := IncludeTrailingPathDelimiter(ExtractFilePath(LFileName)) + LPrefix + LRunSequenceType + LFileExt;
        if not (FileExists(LNewFileName)) then
        begin
          CopyFile(PChar(LFileName),PChar(LNewFileName),False);
          LFileName := LNewFileName;
        end;  
      end; 
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.




