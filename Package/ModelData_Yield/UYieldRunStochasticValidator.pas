{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunStochasticValidator.              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/22                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYieldRunStochasticValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  URunConfigurationData,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UYieldRunStochasticDialog;

type
  TYieldRunStochasticValidator = class(TAbstractYieldDataDialogValidator)
  private
    FSystemAction : Boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlClick(Sender: TObject);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnLoadCaseSelectedClick(Sender: TObject);
    procedure OnSetHistoricButtonClick (Sender : TObject);
    procedure OnSetStochasticButtonClick (Sender : TObject);
    procedure RePopulateDataViewer;
    procedure RepopulateSummaryOutputLevel;
    procedure RepopulateMultiplePeriods;
    procedure RepopulateReduceSequences;
    procedure RepopulateStartType;
    procedure DisplaySequenceControls;
    procedure RePopulateSequenceOrderGrid;
    procedure RepopulateNumberOfYears;
    procedure RepopulateLoadCasesGrids;
    procedure RepopulateDemandChannelGrid;
    procedure UpdateSummaryLevel;
    procedure UpdateMultiplePeriods;
    procedure UpdateReduceSequences;
    procedure UpdateStartType;
    procedure UpdateNrOfSequences;
    procedure UpdateStartSequence;
    procedure UpdateSequenceOrder(AIndex : integer; AValue : string);
    procedure UpdateNumberOfYears;
    procedure UpdateTargetYield(AIndex : integer; AValue : string);
    procedure UpdateMaximumYield(AIndex : integer; AValue : string);
    procedure UpdatePowerDemand(AIndex : integer; AValue : string);
    procedure ValidateSummaryLevel (ARunConfigData : TRunConfigurationData);
    procedure ValidateMultiplePeriods (ARunConfigData : TRunConfigurationData);
    procedure ValidateReduceSequences (ARunConfigData : TRunConfigurationData);
    procedure ValidateNumberOfSequences (ARunConfigData : TRunConfigurationData);
    procedure ValidateSequencesInAnalysis(ARunConfigData: TRunConfigurationData);
    procedure ValidateStartType(ARunConfigData: TRunConfigurationData);
    procedure ValidateNumberOfYears(ARunConfigData: TRunConfigurationData);
    procedure ValidateTargetYield (ARunConfigData : TRunConfigurationData);
    procedure ValidateMaximumYield (ARunConfigData : TRunConfigurationData);
    procedure ValidateTargetPower (ARunConfigData : TRunConfigurationData);
    procedure ValidateNrOfLoadCases (ARunConfigData : TRunConfigurationData);
    procedure ValidateSpecifiedDemandIndicators;
    function DetermineWizardStatusStep1 : integer;
    function DetermineWizardStatusStep2 : integer;
    function DetermineWizardStatusStep3 : integer;
    function DetermineWizardStatusStep4 : integer;
    function DetermineWizardStatusStep5 : integer;
    function DetermineWizardStatusStep6 : integer;
    function DetermineWizardStatusStep7 : integer;
    function DetermineWizardStatusStep8 : integer;
    function DetermineWizardStatusStep9 : integer;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure DoContextValidation(AValidationType: TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    procedure ShowWizardStep (ASequence : integer = 0); override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function YieldRunStochasticDialog : TYieldRunStochasticDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UConstants,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TYieldRunStochasticValidator                                               *}
{******************************************************************************}

procedure TYieldRunStochasticValidator.CreateMemberObjects;
const OPNAME = 'TYieldRunStochasticValidator.CreateMemberObjects';
var
  lPanel     : TYieldRunStochasticDialog;
  lIndex     : integer;
begin
  try
    inherited CreateMemberObjects;
    FSystemAction := FALSE;
    FPanel  := TYieldRunStochasticDialog.Create(FPanelOwner,FAppModules);
    lPanel := YieldRunStochasticDialog;
    with lPanel do
    begin
      RunTypeRadioGroup.FieldProperty      := FAppModules.FieldProperties.FieldProperty('RunType');
      RunTypeRadioGroup.OnEnter            := OnEditControlEnter;
      RunTypeRadioGroup.OnExit             := OnEditControltExit;
      RunTypeRadioGroup.Enabled            := FALSE;

      SummaryLevelRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('SummaryLevel');
      SummaryLevelRadioGroup.OnEnter       := OnEditControlEnter;
      SummaryLevelRadioGroup.OnClick       := OnEditControlClick;

      MultiplePeriodsChkBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('MultPeriodOpt');
      MultiplePeriodsChkBox.OnEnter        := OnEditControlEnter;
      MultiplePeriodsChkBox.OnClick        := OnEditControltExit;

      ReduceSequencesChkBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('ReduceSeqOpt');
      ReduceSequencesChkBox.OnEnter        := OnEditControlEnter;
      ReduceSequencesChkBox.OnClick        := OnEditControltExit;

      StartTypeRadioGroup.FieldProperty    := FAppModules.FieldProperties.FieldProperty('StartType');
      StartTypeRadioGroup.OnEnter          := OnEditControlEnter;
      StartTypeRadioGroup.OnClick          := OnEditControltExit;

      NrOfSequencesEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('HydroSeqCount');
      NrOfSequencesEdit.OnEnter            := OnEditControlEnter;
      NrOfSequencesEdit.OnExit             := OnEditControltExit;

      StartSequenceEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('Seq');
      StartSequenceEdit.OnEnter            := OnEditControlEnter;
      StartSequenceEdit.OnExit             := OnEditControltExit;

      SequenceOrderGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      SequenceOrderGrid.OnColEnter         := OnStringGridColEnter;

      NumberOfYearsEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('YearsCount');
      NumberOfYearsEdit.OnEnter            := OnEditControlEnter;
      NumberOfYearsEdit.OnExit             := OnEditControltExit;

      WaterLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TYield'));
      WaterLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MYield'));
      WaterLoadCasesGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      WaterLoadCasesGrid.OnColEnter         := OnStringGridColEnter;

      PowerLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TPower'));
      PowerLoadCasesGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      PowerLoadCasesGrid.OnColEnter         := OnStringGridColEnter;

      for lIndex := 1 to 10 do
      begin
        LoadCaseSelectedChkBox(lIndex).FieldProperty := FAppModules.FieldProperties.FieldProperty('LoadCasesCount');
        LoadCaseSelectedChkBox(lIndex).OnEnter := OnEditControlEnter;
        LoadCaseSelectedChkBox(lIndex).OnExit  := OnEditControltExit;
        LoadCaseSelectedChkBox(lIndex).OnClick := OnLoadCaseSelectedClick;
      end;

      SetHistoricButton.OnClick   := OnSetHistoricButtonClick;
      SetStochasticButton.OnClick := OnSetStochasticButtonClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.DestroyMemberObjects;
const OPNAME = 'TYieldRunStochasticValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.Initialise: boolean;
const OPNAME = 'TYieldRunStochasticValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with YieldRunStochasticDialog.RunTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.Historical'));
      Items.Add(FAppModules.Language.GetString('RunParameters.Stochastic'));
    end;
    with YieldRunStochasticDialog.SummaryLevelRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelBrief'));
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelAdditional'));
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelFull'));
    end;
    with YieldRunStochasticDialog.StartTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.StartRandomly'));
      Items.Add(FAppModules.Language.GetString('RunParameters.StartHistorical'));
      Items.Add(FAppModules.Language.GetString('RunParameters.StartBootstrap'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunStochasticValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ClearDataViewer;
const OPNAME = 'TYieldRunStochasticValidator.ClearDataViewer';
var
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with YieldRunStochasticDialog do
    begin
      SummaryLevelRadioGroup.ItemIndex := -1;
      SummaryLevelRadioGroup.ItemIndex := -1;

      MultiplePeriodsChkBox.Checked   := FALSE;
      ReduceSequencesChkBox.Checked   := FALSE;
      StartTypeRadioGroup.ItemIndex   := -1;
      NumberOfYearsEdit.Text    := '-1';

      FSystemAction := TRUE;
      for lIndex := 0 to 9 do
      begin
        WaterLoadCasesGrid.Cells[0, lIndex] := '-1';
        WaterLoadCasesGrid.Cells[1, lIndex] := '-1';
        PowerLoadCasesGrid.Cells[0, lIndex] := '-1';
      end;
      FSystemAction := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.PopulateDataViewer;
const OPNAME = 'TYieldRunStochasticValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RePopulateDataViewer;
const OPNAME = 'TYieldRunStochasticValidator.RePopulateDataViewer';
var
  lConfigData    : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        if (lConfigData.RunSequenceType = 'H') then
          lConfigData.RunSequenceType := 'S';

        RunTypeRadioGroup.ItemIndex   := 1;
        lConfigData.GeneratedFlowFlag := 0;

        RepopulateSummaryOutputLevel;
        RepopulateMultiplePeriods;
        RepopulateReduceSequences;
        RepopulateStartType;
        DisplaySequenceControls;
        RePopulateSequenceOrderGrid;
        RepopulateNumberOfYears;
        RePopulateLoadCasesGrids;
        RePopulateDemandChannelGrid;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RepopulateSummaryOutputLevel;
const OPNAME = 'TYieldRunStochasticValidator.RepopulateSummaryOutputLevel';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunStochasticDialog do
    begin
      SummaryLevelRadioGroup.ItemIndex  := lConfigData.OutputSummaryLevel;
      SummaryOutputWarningLabel.Visible := lConfigData.OutputSummaryLevel <> 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RepopulateMultiplePeriods;
const OPNAME = 'TYieldRunStochasticValidator.RepopulateMultiplePeriods';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunStochasticDialog do
    begin
      MultiplePeriodsChkBox.Checked := lConfigData.MultiplePeriodLengths;
      MultiplePeriodsWarningLabel.Visible :=
        lConfigData.MultiplePeriodLengths AND
        (lConfigData.NumberOfSequencesInAnalysis > 10);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RepopulateReduceSequences;
const OPNAME = 'TYieldRunStochasticValidator.RepopulateReduceSequences';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunStochasticDialog do
    begin
      ReduceSequencesChkBox.Checked := lConfigData.ReduceSequences;
      ReduceSequencesWarningLabel.Visible := (NOT lConfigData.ReduceSequences);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RepopulateStartType;
const OPNAME = 'TYieldRunStochasticValidator.RepopulateStartType';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunStochasticDialog do
    begin
      StartTypeRadioGroup.ItemIndex := lConfigData.GeneratedFlowFlag;
      StartTypeWarningLabel.Visible := (lConfigData.GeneratedFlowFlag <> 0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.DisplaySequenceControls;
const OPNAME = 'TYieldRunStochasticValidator.DisplaySequenceControls';
var
  lConfigData : TRunConfigurationData;
  lCount      : integer;
  lIndex      : integer;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        NrOfSequencesEdit.SetFieldValue(IntToStr(lConfigData.NumberOfSequencesInAnalysis));
        StartSequenceEdit.SetFieldValue(IntToStr(lConfigData.StartSequenceNumber));
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
              SequenceOrderGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Seq'));
          end
        end
        else
        begin
          SequenceOrderLabel.Visible := FALSE;
          SequenceOrderGrid.Visible  := FALSE;
          StartSequenceLabel.Visible := TRUE;
          StartSequenceEdit.Visible  := TRUE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RePopulateSequenceOrderGrid;
const OPNAME = 'TYieldRunStochasticValidator.RePopulateSequenceOrderGrid';
var
  lConfigData : TRunConfigurationData;
  lIndexA     : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        for lIndexA := 0 to 9 do
          SequenceOrderGrid.Cells[lIndexA, 0] := IntToStr(lConfigData.SequenceToBeAnalysedByIndex[lIndexA+1]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RepopulateNumberOfYears;
const OPNAME = 'TYieldRunStochasticValidator.RepopulateNumberOfYears';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunStochasticDialog do
    begin
      NumberOfYearsEdit.SetFieldValue(IntToStr(lConfigData.YearsInAnalysis));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.SaveState: boolean;
const OPNAME = 'TYieldRunStochasticValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.YieldRunStochasticDialog : TYieldRunStochasticDialog;
const OPNAME = 'TYieldRunStochasticValidator.YieldRunStochasticDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYieldRunStochasticDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldRunStochasticValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.StudyHasChanged: boolean;
const OPNAME = 'TYieldRunStochasticValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYieldRunStochasticValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    with YieldRunStochasticDialog do
    begin
      if (Sender = SummaryLevelRadioGroup) then
        UpdateSummaryLevel
      else
      if (Sender = StartTypeRadioGroup) then
        UpdateStartType;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYieldRunStochasticValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with YieldRunStochasticDialog do
    begin
      if (Sender = SummaryLevelRadioGroup) then
        UpdateSummaryLevel
      else
      if (Sender = MultiplePeriodsChkBox) then
        UpdateMultiplePeriods
      else
      if (Sender = ReduceSequencesChkBox) then
        UpdateReduceSequences
      else
      if (Sender = StartTypeRadioGroup) then
        UpdateStartType
      else
      if ((Sender = NrOfSequencesEdit) AND (NrOfSequencesEdit.HasValueChanged)) then
        UpdateNrOfSequences
      else
      if ((Sender = StartSequenceEdit) AND (StartSequenceEdit.HasValueChanged)) then
        UpdateStartSequence
      else
      if ((Sender = NumberOfYearsEdit) AND (NumberOfYearsEdit.HasValueChanged)) then
        UpdateNumberOfYears;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateSummaryLevel;
const OPNAME = 'TYieldRunStochasticValidator.UpdateSummaryLevel';
var
  lConfigData : TRunConfigurationData;
  lOldSummary : integer;
  lNewSummary : integer;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lOldSummary := lConfigData.OutputSummaryLevel;
        lNewSummary := SummaryLevelRadioGroup.ItemIndex;
        if (lOldSummary <> lNewSummary) then
        begin
          lConfigData.OutputSummaryLevel := lNewSummary;
          RepopulateSummaryOutputLevel;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateMultiplePeriods;
const OPNAME = 'TYieldRunStochasticValidator.UpdateMultiplePeriods';
var
  lConfigData : TRunConfigurationData;
  lMultiplePeriods : Boolean;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lMultiplePeriods := lConfigData.MultiplePeriodLengths;
        if ((lMultiplePeriods  AND (NOT MultiplePeriodsChkBox.Checked)) OR
            ((NOT lMultiplePeriods) AND MultiplePeriodsChkBox.Checked)) then
        begin
          lConfigData.MultiplePeriodLengths := MultiplePeriodsChkBox.Checked;
          RepopulateMultiplePeriods;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateReduceSequences;
const OPNAME = 'TYieldRunStochasticValidator.UpdateReduceSequences';
var
  lConfigData : TRunConfigurationData;
  lReduceSeq  : Boolean;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lReduceSeq := lConfigData.ReduceSequences;
        if ((lReduceSeq  AND (NOT ReduceSequencesChkBox.Checked)) OR
            ((NOT lReduceSeq) AND ReduceSequencesChkBox.Checked)) then
        begin
          lConfigData.ReduceSequences := ReduceSequencesChkBox.Checked;
          RepopulateReduceSequences;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateStartType;
const OPNAME = 'TYieldRunStochasticValidator.UpdateStartType';
var
  lConfigData : TRunConfigurationData;
  lOldFlow    : integer;
  lNewFlow    : integer;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lOldFlow := lConfigData.GeneratedFlowFlag;
        lNewFlow := StartTypeRadioGroup.ItemIndex;
        if (lOldFlow <> lNewFlow) then
        begin
          lConfigData.GeneratedFlowFlag := lNewFlow;
          RepopulateStartType;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateNrOfSequences;
const OPNAME = 'TYieldRunStochasticValidator.UpdateNrOfSequences';
var
  lConfigData : TRunConfigurationData;
  lMessage    : string;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
              'HydroSeqCount', Trim(NrOfSequencesEdit.Text), lMessage)) then
        begin
          lConfigData.NumberOfSequencesInAnalysis := StrToInt(Trim(NrOfSequencesEdit.Text));
          NrOfSequencesEdit.SetFieldValue(IntToStr(lConfigData.NumberOfSequencesInAnalysis));
          DoContextValidation(dvtNumberOfSequences);
          DisplaySequenceControls;
          RepopulateMultiplePeriods;
        end
        else
          NrOfSequencesEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateStartSequence;
const OPNAME = 'TYieldRunStochasticValidator.UpdateStartSequence';
var
  lConfigData : TRunConfigurationData;
  lMessage    : string;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
              'Seq', Trim(StartSequenceEdit.Text), lMessage, 1)) then
        begin
          lConfigData.StartSequenceNumber := StrToInt(Trim(StartSequenceEdit.Text));
          StartSequenceEdit.SetFieldValue(IntToStr(lConfigData.StartSequenceNumber));
          DoContextValidation(dvtSequenceInAnalysis);
        end
        else
          StartSequenceEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateSequenceOrder(AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TYieldRunStochasticValidator.UpdateSequenceOrder';
var
  lConfigData : TRunConfigurationData;
  lIntVal     : integer;
  lMessage    : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        SequenceOrderGrid.ValidationError[0, 0, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
              'Seq', AValue, lMessage, AIndex)) then
        begin
          lIntVal := StrToInt(AValue);
          lConfigData.SequenceToBeAnalysedByIndex[AIndex] := lIntVal;
          DoContextValidation(dvtSequenceInAnalysis);
        end
        else
          SequenceOrderGrid.ValidationError[0, 0, gveCellField] := lMessage;
      end;
    end;
    RePopulateSequenceOrderGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateNumberOfYears;
const OPNAME = 'TYieldRunStochasticValidator.UpdateNumberOfYears';
var
  lConfigData : TRunConfigurationData;
  lMessage    : string;
begin
  try
    with YieldRunStochasticDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'YearsCount', NumberOfYearsEdit.Text, lMessage)) then
        begin
          lConfigData.YearsInAnalysis := StrToInt(Trim(NumberOfYearsEdit.Text));
          NumberOfYearsEdit.SetFieldValue(lConfigData.YearsInAnalysis);
          DoContextValidation(dvtNumberOfYears);
          RepopulateNumberOfYears;
        end
        else
          NumberOfYearsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TYieldRunStochasticValidator.OnEditControlClick';
begin
  with YieldRunStochasticDialog do
  begin
    if (Sender = SummaryLevelRadioGroup) then
        OnEditControlEnter(Sender)
    else
    if (Sender = StartTypeRadioGroup) then
        OnEditControlEnter(Sender);
  end;
end;

procedure TYieldRunStochasticValidator.RePopulateLoadCasesGrids;
const OPNAME = 'TYieldRunStochasticValidator.RePopulateLoadCasesGrids';
var
  lConfigData      : TRunConfigurationData;
  lFeatureList     : IMasterControlFeatureList;
  lFeature         : IMasterControlFeature;
  lFieldProperty   : TAbstractFieldProperty;
  lCount           : integer;
  lIndex           : integer;
  lValue           : double;
  lChannelType     : string;
  lActiveLoadCases : integer;
begin
  try
    with YieldRunStochasticDialog do
    begin
      WaterLoadCasesGrid.Enabled := FALSE;
      WaterLoadCasesGrid.Color   := clBtnFace;
      PowerLoadCasesGrid.Enabled := FALSE;
      PowerLoadCasesGrid.Color   := clBtnFace;
      lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.MasterControlFeatureList;
      for lCount := 0 to lFeatureList.MasterControlFeatureCount -1 do
      begin
        lFeature := lFeatureList.MasterControlFeatureByIndex[lCount];
        if ((lFeature <> nil) AND (lConfigData <> nil)) then
        begin
          lActiveLoadCases := lConfigData.NumberOfActiveLoadCases;
          FSystemAction := TRUE;
          for lIndex := 0 to 9 do
          begin
            lValue := lConfigData.TargetYieldByIndex[lIndex+1];
            if (lValue <> NullFloat) then
            begin
              //WaterLoadCasesGrid.Cells[0, lIndex] := Format('%6.2f', [lValue]);
              lFieldProperty := WaterLoadCasesGrid.FieldProperty(0);
              WaterLoadCasesGrid.Cells[0,lIndex] := SmartFloatFormat(lValue,lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals);
              LoadCaseSelectedChkBox(lIndex+1).Enabled := TRUE;
              LoadCaseSelectedChkBox(lIndex+1).Checked := lIndex < lActiveLoadCases;
            end
            else
            begin
              WaterLoadCasesGrid.Cells[0, lIndex] := '';
              LoadCaseSelectedChkBox(lIndex+1).Enabled := FALSE;
              LoadCaseSelectedChkBox(lIndex+1).Checked := FALSE;
            end;
            lValue := lConfigData.MaximumYieldByIndex[lIndex+1];
            if (lValue >= 0) then
            begin
              //WaterLoadCasesGrid.Cells[1, lIndex] := Format('%6.2f', [lValue])
              lFieldProperty := WaterLoadCasesGrid.FieldProperty(1);
              WaterLoadCasesGrid.Cells[1, lIndex] := SmartFloatFormat(lValue, lFieldProperty.FieldWidth, lFieldProperty.NumberOfDecimals);
            end
            else
              WaterLoadCasesGrid.Cells[1, lIndex] := '';
            lValue := lConfigData.TargetPowerByIndex[lIndex+1];
            if (lValue >= 0) then
            begin
              //PowerLoadCasesGrid.Cells[0, lIndex] := Format('%6.2f', [lValue]);
              lFieldProperty := PowerLoadCasesGrid.FieldProperty(0);
              PowerLoadCasesGrid.Cells[0, lIndex] := SmartFloatFormat(lValue, lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals);
            end
            else
              PowerLoadCasesGrid.Cells[0, lIndex] := '';
          end;
          FSystemAction := FALSE;
          lChannelType := lFeature.MasterControlType;
          if (lChannelType = 'W') then
          begin
            WaterLoadCasesGrid.Enabled := TRUE;
            WaterLoadCasesGrid.Color   := clWindow;
           end
          else
          if (lChannelType = 'P') then
          begin
            PowerLoadCasesGrid.Enabled := TRUE;
            PowerLoadCasesGrid.Color   := clWindow;
          end;
        end;
        if (WaterLoadCasesGrid.Row < 0) then
          WaterLoadCasesGrid.Row := 0;
        if (PowerLoadCasesGrid.Row < 0) then
          PowerLoadCasesGrid.Row := 0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.RePopulateDemandChannelGrid;
const OPNAME = 'TYieldRunStochasticValidator.RePopulateDemandChannelGrid';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lFeature     : ISpecifiedDemandFeature;
  lIndex       : integer;
begin
  try
    with YieldRunStochasticDialog do
    begin
      for lIndex := 1 to DemandChannelGrid.RowCount - 1 do
      begin
        DemandChannelGrid.Cells[0, lIndex] := '';
        DemandChannelGrid.Cells[1, lIndex] := '';
        DemandChannelGrid.Cells[2, lIndex] := '';
      end;
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.SpecifiedDemandFeatureList;
      DemandChannelGrid.RowCount := lFeatureList.SpecifiedDemandFeatureCount + 1;
      for lIndex := 0 to lFeatureList.SpecifiedDemandFeatureCount - 1 do
      begin
        lFeature := lFeatureList.SpecifiedDemandFeatureByIndex[lIndex];
        DemandChannelGrid.Cells[0, lIndex+1] := IntToStr(lIndex + 1);
        if (lFeature.Channel <> nil) then
          DemandChannelGrid.Cells[1, lIndex+1] := lFeature.Channel.ChannelName;
        DemandChannelGrid.Cells[2, lIndex+1]   := lFeature.StochasticIndicator;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TYieldRunStochasticValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with YieldRunStochasticDialog do
    begin
      if (SequenceOrderGrid = ASender) then
        UpdateSequenceOrder(ACol+1, Trim(SequenceOrderGrid.Cells[ACol, ARow]))
      else
      if ((ASender = WaterLoadCasesGrid) AND (ACol = 0)) then
        UpdateTargetYield(ARow+1, Trim(WaterLoadCasesGrid.Cells[ACol, ARow]))
      else
      if ((ASender = WaterLoadCasesGrid) AND (ACol = 1)) then
        UpdateMaximumYield(ARow+1, Trim(WaterLoadCasesGrid.Cells[ACol, ARow]))
      else
      if ((ASender = PowerLoadCasesGrid) AND (ACol = 0)) then
        UpdatePowerDemand(ARow+1, Trim(PowerLoadCasesGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdatePowerDemand (AIndex : integer;
                                                          AValue : string);
const OPNAME = 'TYieldRunStochasticValidator.UpdatePowerDemand';
var
  lConfigData : TRunConfigurationData;
  lValue      : double;
  lMessage    : string;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        lIndex := AIndex;
        while ((lIndex >= 1) AND
               (lConfigData.TargetPowerByIndex[lIndex-1] = NullFloat)) do
          lIndex := lIndex - 1;
        PowerLoadCasesGrid.ValidationError[0, lIndex-1, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := FloatToStr(NullFloat);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'TPower', AValue, lMessage, lIndex)) then
        begin
          lValue := StrToFloat(Trim(AValue));
          lConfigData.TargetPowerByIndex[lIndex] := lValue;
          if ((lConfigData.TargetYieldByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.TargetYieldByIndex[lIndex] := 0;
          if ((lConfigData.MaximumYieldByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.MaximumYieldByIndex[lIndex] := 0;
          DoContextValidation(dvtTargetPower);
        end
        else
          PowerLoadCasesGrid.ValidationError[0, lIndex-1, gveCellField] := lMessage;
      end;
    end;
    RePopulateLoadCasesGrids;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateTargetYield (AIndex : integer;
                                                          AValue : string);
const OPNAME = 'TYieldRunStochasticValidator.UpdateTargetYield';
var
  lConfigData : TRunConfigurationData;
  lValue      : double;
  lMessage    : string;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        lIndex := AIndex;
        while ((lIndex >= 1) AND
               (lConfigData.TargetYieldByIndex[lIndex-1] = NullFloat)) do
          lIndex := lIndex - 1;
        WaterLoadCasesGrid.ValidationError[0, lIndex-1, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := FloatToStr(NullFloat);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'TYield', AValue, lMessage, lIndex)) then
        begin
          lValue := StrToFloat(Trim(AValue));
          lConfigData.TargetYieldByIndex[lIndex] := lValue;
          if (lConfigData.MaximumYieldByIndex[lIndex] = NullFloat) then
            lConfigData.MaximumYieldByIndex[lIndex] := lValue;
          if ((lConfigData.TargetPowerByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.TargetPowerByIndex[lIndex] := 0;
          DoContextValidation(dvtTargetYield);
        end
        else
          WaterLoadCasesGrid.ValidationError[0, lIndex-1,gveCellField] := lMessage;
      end;
    end;
    RePopulateLoadCasesGrids;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.UpdateMaximumYield (AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TYieldRunStochasticValidator.UpdateMaximumYield';
var
  lValue      : double;
  lMessage    : string;
  lConfigData : TRunConfigurationData;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        lIndex := AIndex;
        while ((lIndex >= 1) AND
               (lConfigData.MaximumYieldByIndex[lIndex-1] = NullFloat)) do
          lIndex := lIndex - 1;
        WaterLoadCasesGrid.ValidationError[1, lIndex-1, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := FloatToStr(NullFloat);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MYield', AValue, lMessage, lIndex)) then
        begin
          lValue := StrToFloat(Trim(AValue));
          lConfigData.MaximumYieldByIndex[lIndex] := lValue;
          if (lConfigData.TargetYieldByIndex[lIndex] = NullFloat) then
            lConfigData.TargetYieldByIndex[lIndex] := lValue;
          if ((lConfigData.TargetPowerByIndex[lIndex] = NullFloat) AND
              (lValue <> NullFloat)) then
            lConfigData.TargetPowerByIndex[lIndex] := 0;
          DoContextValidation(dvtMaximumYield);
        end
        else
          WaterLoadCasesGrid.ValidationError[1, lIndex-1, gveCellField] := lMessage;
      end;
    end;
    RePopulateLoadCasesGrids;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnLoadCaseSelectedClick(Sender: TObject);
const OPNAME = 'TYieldRunStochasticValidator.OnLoadCaseSelectedClick';
var
  lIndexA     : integer;
  lChkBox     : TFieldChkBox;
  lConfigData : TRunConfigurationData;
begin
  try
    if (NOT FSystemAction) then
    begin
      with YieldRunStochasticDialog do
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
        if (lConfigData <> nil) then
        begin
          lChkBox := TFieldChkBox(Sender);
          lIndexA := YieldRunStochasticDialog.IndexOfLoadCaseCheckBox(lChkBox);
          if (lIndexA >= 0) then
          begin
            if (lChkBox.Checked) then
              lConfigData.ActivateLoadCase(lIndexA)
            else
              lConfigData.DeactivateLoadCase(lIndexA);
            DoContextValidation(dvtNrOfLoadCases);  
          end;
        end;
      end;
      RePopulateLoadCasesGrids;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TYieldRunStochasticValidator.DoContextValidation';
var
  lConfigData  : TRunConfigurationData;
begin
  try
    FAllErrorMessages.Clear;
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      if (AValidationType in [dvtRunStochasticWizardStep1, dvtSummaryLevel]) then
        ValidateSummaryLevel(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep2, dvtMultiplePeriods]) then
        ValidateMultiplePeriods(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep3, dvtReduceSequences]) then
        ValidateReduceSequences(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep4, dvtStartType]) then
        ValidateStartType(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep5, dvtNumberOfSequences]) then
        ValidateNumberOfSequences(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep6, dvtSequenceInAnalysis]) then
        ValidateSequencesInAnalysis(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep7, dvtNumberOfYears]) then
        ValidateNumberOfYears(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep8, dvtTargetYield]) then
        ValidateTargetYield(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep8, dvtMaximumYield]) then
        ValidateMaximumYield(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep8, dvtTargetPower]) then
        ValidateTargetPower(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep8, dvtTargetYield,
                              dvtMaximumYield, dvtTargetPower, dvtNrOfLoadCases]) then
        ValidateNrOfLoadCases(lConfigData);
      if (AValidationType in [dvtRunStochasticWizardStep9]) then
        ValidateSpecifiedDemandIndicators;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    case ASequence of
      1 : Result := DetermineWizardStatusStep1;
      2 : Result := DetermineWizardStatusStep2;
      3 : Result := DetermineWizardStatusStep3;
      4 : Result := DetermineWizardStatusStep4;
      5 : Result := DetermineWizardStatusStep5;
      6 : Result := DetermineWizardStatusStep6;
      7 : Result := DetermineWizardStatusStep7;
      8 : Result := DetermineWizardStatusStep8;
      9 : Result := DetermineWizardStatusStep9;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep1 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep1';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep1);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.OutputSummaryLevel = 0) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep2 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep2';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep2);
      if (FAllErrorMessages.Count = 0) then
      begin
        if ((NOT lConfigData.MultiplePeriodLengths) OR
            (lConfigData.NumberOfSequencesInAnalysis <= 10)) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep3 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep3';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep3);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.ReduceSequences) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep4 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep4';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep4);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.GeneratedFlowFlag = 1) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep5 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep5';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep5);
      if (FAllErrorMessages.Count = 0) then
        Result := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep6 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep6';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep6);
      if (FAllErrorMessages.Count = 0) then
        Result := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep7 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep7';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep7);
      if (FAllErrorMessages.Count = 0) then
          Result := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep8 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep8';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunStochasticWizardStep8);
      if (FAllErrorMessages.Count = 0) then
        Result := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunStochasticValidator.DetermineWizardStatusStep9 : integer;
const OPNAME = 'TYieldRunStochasticValidator.DetermineWizardStatusStep9';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lIndicator   : string;
begin
  Result := 0;
  try
    DoContextValidation(dvtRunStochasticWizardStep9);
    lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.SpecifiedDemandFeatureList;
    lIndicator := '';
    if (lFeatureList.SpecifiedDemandFeatureCount > 0) then
    begin
      lIndicator := lFeatureList.SpecifiedDemandFeatureByIndex[0].StochasticIndicator;
      lIndicator := UpperCase(Trim(lIndicator));
    end;
    if (FAllErrorMessages.Count = 0) then
    begin
      if (lIndicator = 'S') then
        Result := 2
      else
        Result := 1;
      YieldRunStochasticDialog.SpecifiedDemandWarningLabel.Visible := Result = 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ShowWizardStep (ASequence : integer = 0);
const OPNAME = 'TYieldRunStochasticValidator.ShowWizardStep';
begin
  inherited ShowWizardStep(ASequence);
  try
    case ASequence of
      1 :
          if (YieldRunStochasticDialog.PageControl.ActivePageIndex <> 0) then
            YieldRunStochasticDialog.PageControl.ActivePageIndex := 0;
      2,3,4,5,6,7 :
          if (YieldRunStochasticDialog.PageControl.ActivePageIndex <> 1) then
            YieldRunStochasticDialog.PageControl.ActivePageIndex := 1;
      8 :
          if (YieldRunStochasticDialog.PageControl.ActivePageIndex <> 2) then
            YieldRunStochasticDialog.PageControl.ActivePageIndex := 2;
      9 :
          if (YieldRunStochasticDialog.PageControl.ActivePageIndex <> 3) then
            YieldRunStochasticDialog.PageControl.ActivePageIndex := 3;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateSummaryLevel (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateSummaryLevel';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'SummaryLevel')) then
          SummaryLevelRadioGroup.InValidationError := FALSE
        else
        begin
          SummaryLevelRadioGroup.InValidationError := TRUE;
          SummaryLevelRadioGroup.ValidationError := FErrorMessage;
          SummaryLevelRadioGroup.ShowErrorState(TRUE);
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateStartType (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateStartType';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'StartType')) then
          StartTypeRadioGroup.InValidationError := FALSE
        else
        begin
          StartTypeRadioGroup.InValidationError := TRUE;
          StartTypeRadioGroup.ValidationError := FErrorMessage;
          StartTypeRadioGroup.ShowErrorState(TRUE);
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateMultiplePeriods (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateMultiplePeriods';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'MultiplePeriods')) then
          MultiplePeriodsChkBox.InValidationError := FALSE
        else
        begin
          MultiplePeriodsChkBox.InValidationError := TRUE;
          MultiplePeriodsChkBox.ValidationError := FErrorMessage;
          MultiplePeriodsChkBox.ShowErrorState(TRUE);
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateReduceSequences (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateReduceSequences';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'ReduceSequences')) then
          ReduceSequencesChkBox.InValidationError := FALSE
        else
        begin
          ReduceSequencesChkBox.InValidationError := TRUE;
          ReduceSequencesChkBox.ValidationError := FErrorMessage;
          ReduceSequencesChkBox.ShowErrorState(TRUE);
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateNumberOfSequences(ARunConfigData: TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateNumberOfSequences';
begin
  try
    with YieldRunStochasticDialog do
    begin
      FErrorMessage := '';
      if (NOT ARunConfigData.Validate(FErrorMessage,'NumberOfSequences')) then
        FAllErrorMessages.Add(FErrorMessage);
      NrOfSequencesEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunStochasticValidator.ValidateSequencesInAnalysis(ARunConfigData: TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateSequencesInAnalysis';
begin
  try
    with YieldRunStochasticDialog do
    begin
      FErrorMessage := '';
      if (ARunConfigData.Validate(FErrorMessage,'SequenceInAnalysis')) then
        SequenceOrderGrid.ValidationError[0, 0 , gveColContext] := ''
      else
      begin
        SequenceOrderGrid.ValidationError[0, 0 , gveColContext] := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunStochasticValidator.ValidateNumberOfYears(ARunConfigData: TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateNumberOfYears';
begin
  try
    with YieldRunStochasticDialog do
    begin
      FErrorMessage := '';
      if (NOT ARunConfigData.Validate(FErrorMessage,'NumberOfYears')) then
        FAllErrorMessages.Add(FErrorMessage);
      NumberOfYearsEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunStochasticValidator.ValidateTargetYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateTargetYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'TargetYield')) then
          WaterLoadCasesGrid.ValidationError[0, 0, gveColContext] := ''
        else
        begin
          WaterLoadCasesGrid.ValidationError[0, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateMaximumYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateMaximumYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'MaximumYield')) then
          WaterLoadCasesGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          WaterLoadCasesGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateTargetPower (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateTargetPower';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'TargetPower')) then
          PowerLoadCasesGrid.ValidationError[0, 0, gveColContext] := ''
        else
        begin
          PowerLoadCasesGrid.ValidationError[0, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateNrOfLoadCases (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunStochasticValidator.ValidateNrOfLoadCases';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunStochasticDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'NrOfLoadCases')) then
        begin
          LoadCaseSelectedChkBox(1).InValidationError := FALSE;
          LoadCaseSelectedChkBox(1).ValidationError := '';
          LoadCaseSelectedChkBox(1).ShowErrorState(FALSE);
        end
        else
        begin
          LoadCaseSelectedChkBox(1).InValidationError := TRUE;
          LoadCaseSelectedChkBox(1).ValidationError := FErrorMessage;
          LoadCaseSelectedChkBox(1).ShowErrorState(TRUE);
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.ValidateSpecifiedDemandIndicators;
const OPNAME = 'TYieldRunStochasticValidator.ValidateSpecifiedDemandIndicators';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lFeature     : ISpecifiedDemandFeature;
  lIndex       : integer;
  lSame        : Boolean;
  lIndicator   : string;
begin
  try
    lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.SpecifiedDemandFeatureList;
    lSame      := TRUE;
    lIndex     := 0;
    lIndicator := '';
    while (lSame AND (lIndex < lFeatureList.SpecifiedDemandFeatureCount)) do
    begin
      lFeature := lFeatureList.SpecifiedDemandFeatureByIndex[lIndex];
      if (lIndicator = '') then
        lIndicator := UpperCase(Trim(lFeature.StochasticIndicator))
      else
      begin
        if (lIndicator <> UpperCase(Trim(lFeature.StochasticIndicator))) then
          lSame := FALSE;
      end;
      lIndex := lIndex + 1;
    end;
    with YieldRunStochasticDialog do
    begin
      FErrorMessage := '';
      if (lSame) then
        DemandChannelGrid.ValidationError[0, 0, gveColContext] := ''
      else
      begin
        FErrorMessage := 'All specified demand indicators must be the same';
        DemandChannelGrid.ValidationError[0, 0, gveColContext] := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnSetHistoricButtonClick (Sender : TObject);
const OPNAME = 'TYieldRunStochasticValidator.OnSetHistoricButtonClick';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lFeature     : ISpecifiedDemandFeature;
  lIndex       : integer;
begin
  try
    lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.SpecifiedDemandFeatureList;
    for lIndex := 0 to lFeatureList.SpecifiedDemandFeatureCount - 1 do
    begin
      lFeature := lFeatureList.SpecifiedDemandFeatureByIndex[lIndex];
      if (UpperCase(Trim(lFeature.StochasticIndicator)) <> 'H') then
        lFeature.StochasticIndicator := 'H';
    end;
    RepopulateDemandChannelGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunStochasticValidator.OnSetStochasticButtonClick (Sender : TObject);
const OPNAME = 'TYieldRunStochasticValidator.OnSetStochasticButtonClick';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lFeature     : ISpecifiedDemandFeature;
  lIndex       : integer;
begin
  try
    lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.SpecifiedDemandFeatureList;
    for lIndex := 0 to lFeatureList.SpecifiedDemandFeatureCount - 1 do
    begin
      lFeature := lFeatureList.SpecifiedDemandFeatureByIndex[lIndex];
      if (UpperCase(Trim(lFeature.StochasticIndicator)) <> 'S') then
        lFeature.StochasticIndicator := 'S';
    end;
    RepopulateDemandChannelGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

