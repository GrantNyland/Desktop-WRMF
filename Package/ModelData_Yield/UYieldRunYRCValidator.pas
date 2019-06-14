{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunYRCValidator.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/22                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYieldRunYRCValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  URunConfigurationData,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UYieldRunYRCDialog;

type
  TYieldRunYRCValidator = class(TAbstractYieldDataDialogValidator)
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
    procedure RePopulateDataViewer;
    procedure RepopulateSummaryOutputLevel;
    procedure RepopulateFirmYieldCalculation;
    procedure RepopulateMultiplePeriods;
    procedure RepopulateNumberOfYears;
    procedure RepopulateLoadCasesGrids;
    procedure UpdateSummaryLevel;
    procedure UpdateCalculateFirmYield;
    procedure UpdateMultiplePeriods;
    procedure UpdateNumberOfYears;
    procedure UpdateTargetYield(AIndex : integer; AValue : string);
    procedure UpdateMaximumYield(AIndex : integer; AValue : string);
    procedure UpdatePowerDemand(AIndex : integer; AValue : string);
    procedure ValidateSummaryLevel (ARunConfigData : TRunConfigurationData);
    procedure ValidateCalcFirmYield (ARunConfigData : TRunConfigurationData);
    procedure ValidateMultiplePeriods (ARunConfigData : TRunConfigurationData);
    procedure ValidateNumberOfYears(AConfiguration: TRunConfigurationData);
    procedure ValidateTargetYield (ARunConfigData : TRunConfigurationData);
    procedure ValidateMaximumYield (ARunConfigData : TRunConfigurationData);
    procedure ValidateTargetPower (ARunConfigData : TRunConfigurationData);
    procedure ValidateNrOfLoadCases (ARunConfigData : TRunConfigurationData);
    function DetermineWizardStatusStep1 : integer;
    function DetermineWizardStatusStep2 : integer;
    function DetermineWizardStatusStep3 : integer;
    function DetermineWizardStatusStep4 : integer;
    function DetermineWizardStatusStep5 : integer;
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
    function YieldRunYRCDialog : TYieldRunYRCDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TYieldRunYRCValidator                                                      *}
{******************************************************************************}

procedure TYieldRunYRCValidator.CreateMemberObjects;
const OPNAME = 'TYieldRunYRCValidator.CreateMemberObjects';
var
  lPanel     : TYieldRunYRCDialog;
  lIndex     : integer;
begin
  try
    inherited CreateMemberObjects;
    FSystemAction := FALSE;
    FPanel  := TYieldRunYRCDialog.Create(FPanelOwner,FAppModules);
    lPanel := YieldRunYRCDialog;
    with lPanel do
    begin
      RunTypeRadioGroup.FieldProperty    := FAppModules.FieldProperties.FieldProperty('RunType');
      RunTypeRadioGroup.OnEnter          := OnEditControlEnter;
      RunTypeRadioGroup.OnExit           := OnEditControltExit;
      RunTypeRadioGroup.Enabled          := FALSE;

      SummaryLevelRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('SummaryLevel');
      SummaryLevelRadioGroup.OnEnter       := OnEditControlEnter;
      SummaryLevelRadioGroup.OnClick       := OnEditControlClick;

      FirmYieldChkBox.FieldProperty      := FAppModules.FieldProperties.FieldProperty('CalcHistoryOpt');
      FirmYieldChkBox.OnEnter            := OnEditControlEnter;
      FirmYieldChkBox.OnClick            := OnEditControltExit;

      MultiplePeriodsChkBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('MultPeriodOpt');
      MultiplePeriodsChkBox.OnEnter        := OnEditControlEnter;
      MultiplePeriodsChkBox.OnClick        := OnEditControltExit;

      NumberOfYearsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YearsCount');
      NumberOfYearsEdit.OnEnter       := OnEditControlEnter;
      NumberOfYearsEdit.OnExit        := OnEditControltExit;

      WaterLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TYield'));
      WaterLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MYield'));
      WaterLoadCasesGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      WaterLoadCasesGrid.OnColEnter         := OnStringGridColEnter;

      PowerLoadCasesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TPower'));
      PowerLoadCasesGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      PowerLoadCasesGrid.OnColEnter         := OnStringGridColEnter;

      for lIndex := 1 to 10 do
      begin
        LoadCaseSelectedChkBox(lIndex).OnEnter := OnEditControlEnter;
        LoadCaseSelectedChkBox(lIndex).OnExit  := OnEditControltExit;
        LoadCaseSelectedChkBox(lIndex).OnClick := OnLoadCaseSelectedClick;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.DestroyMemberObjects;
const OPNAME = 'TYieldRunYRCValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.Initialise: boolean;
const OPNAME = 'TYieldRunYRCValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with YieldRunYRCDialog.RunTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.Historical'));
      Items.Add(FAppModules.Language.GetString('RunParameters.Stochastic'));
    end;
    with YieldRunYRCDialog.SummaryLevelRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelBrief'));
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelAdditional'));
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelFull'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunYRCValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.ClearDataViewer;
const OPNAME = 'TYieldRunYRCValidator.ClearDataViewer';
var
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with YieldRunYRCDialog do
    begin
      SummaryLevelRadioGroup.ItemIndex := -1;
      SummaryLevelRadioGroup.ItemIndex := -1;

      FirmYieldChkBox.Checked   := FALSE;
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

procedure TYieldRunYRCValidator.PopulateDataViewer;
const OPNAME = 'TYieldRunYRCValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.RePopulateDataViewer;
const OPNAME = 'TYieldRunYRCValidator.RePopulateDataViewer';
var
  lConfigData    : TRunConfigurationData;
  lModelVersion  : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
      begin
        lModelVersion := FAppModules.StudyArea.ModelVersion;
        if (lConfigData.RunSequenceType = 'H') then
        begin
          lConfigData.RunSequenceType := 'S';
          if ((lConfigData.CalculateHistoricFirmYield = 1) AND
              (LModelVersion = '6.1')) then
            lConfigData.CalculateHistoricFirmYield := 2;
        end;

        RunTypeRadioGroup.ItemIndex := 1;
        RepopulateSummaryOutputLevel;
        RepopulateFirmYieldCalculation;
        RepopulateMultiplePeriods;
        RepopulateNumberOfYears;
        RePopulateLoadCasesGrids;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.RepopulateSummaryOutputLevel;
const OPNAME = 'TYieldRunYRCValidator.RepopulateSummaryOutputLevel';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunYRCDialog do
    begin
      SummaryLevelRadioGroup.ItemIndex  := lConfigData.OutputSummaryLevel;
      SummaryOutputWarningLabel.Visible := lConfigData.OutputSummaryLevel <> 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.RepopulateFirmYieldCalculation;
const OPNAME = 'TYieldRunYRCValidator.RepopulateFirmYieldCalculation';
var
  lConfigData    : TRunConfigurationData;
  lCalcFirmYield : Integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunYRCDialog do
    begin
      lCalcFirmYield := lConfigData.CalculateHistoricFirmYield;
      FirmYieldChkBox.Checked := lCalcFirmYield <> 0;
      FirmYieldWarningLabel.Visible := lCalcFirmYield <> 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.RepopulateMultiplePeriods;
const OPNAME = 'TYieldRunYRCValidator.RepopulateMultiplePeriods';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunYRCDialog do
    begin
      MultiplePeriodsChkBox.Checked := lConfigData.MultiplePeriodLengths;
      MultiplePeriodsWarningLabel.Visible :=
        lConfigData.MultiplePeriodLengths AND
        (lConfigData.NumberOfSequencesInAnalysis > 10);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.RepopulateNumberOfYears;
const OPNAME = 'TYieldRunYRCValidator.RepopulateNumberOfYears';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunYRCDialog do
    begin
      NumberOfYearsEdit.SetFieldValue(IntToStr(lConfigData.YearsInAnalysis));
//      NumberOfYearsWarningLabel.Visible := lConfigData.YearsInAnalysis <> 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.SaveState: boolean;
const OPNAME = 'TYieldRunYRCValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.YieldRunYRCDialog : TYieldRunYRCDialog;
const OPNAME = 'TYieldRunYRCValidator.YieldRunYRCDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYieldRunYRCDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldRunYRCValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.StudyHasChanged: boolean;
const OPNAME = 'TYieldRunYRCValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYieldRunYRCValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    with YieldRunYRCDialog do
    begin
      if (Sender = SummaryLevelRadioGroup) then
        UpdateSummaryLevel;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYieldRunYRCValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with YieldRunYRCDialog do
    begin
      if (Sender = SummaryLevelRadioGroup) then
        UpdateSummaryLevel
      else
      if (Sender = FirmYieldChkBox) then
        UpdateCalculateFirmYield
      else
      if (Sender = MultiplePeriodsChkBox) then
        UpdateMultiplePeriods
      else
      if ((Sender = NumberOfYearsEdit) AND (NumberOfYearsEdit.HasValueChanged)) then
        UpdateNumberOfYears;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.UpdateSummaryLevel;
const OPNAME = 'TYieldRunYRCValidator.UpdateSummaryLevel';
var
  lConfigData : TRunConfigurationData;
  lOldSummary : integer;
  lNewSummary : integer;
begin
  try
    with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.UpdateCalculateFirmYield;
const OPNAME = 'TYieldRunYRCValidator.UpdateCalculateFirmYield';
var
  lConfigData    : TRunConfigurationData;
  lCalcFirmYield : Integer;
  lModelVersion  : string;
begin
  try
    with YieldRunYRCDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lCalcFirmYield := lConfigData.CalculateHistoricFirmYield;
        if (((lCalcFirmYield <> 0) AND (NOT FirmYieldChkBox.Checked)) OR
            ((lCalcFirmYield = 0) AND FirmYieldChkBox.Checked)) then
        begin
          if (FirmYieldChkBox.Checked) then
          begin
            lModelVersion := FAppModules.StudyArea.ModelVersion;
            if (LModelVersion = '6.1') then
            begin
              if (lConfigData.RunSequenceType = 'H') then
                lConfigData.CalculateHistoricFirmYield := 1
              else
                lConfigData.CalculateHistoricFirmYield := 2;
            end
            else
              lConfigData.CalculateHistoricFirmYield := 1;
          end
          else
            lConfigData.CalculateHistoricFirmYield := 0;
          RepopulateFirmYieldCalculation;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.UpdateMultiplePeriods;
const OPNAME = 'TYieldRunYRCValidator.UpdateMultiplePeriods';
var
  lConfigData : TRunConfigurationData;
  lMultiplePeriods : Boolean;
begin
  try
    with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.UpdateNumberOfYears;
const OPNAME = 'TYieldRunYRCValidator.UpdateNumberOfYears';
var
  lConfigData : TRunConfigurationData;
  lMessage    : string;
begin
  try
    with YieldRunYRCDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'YearsCount', NumberOfYearsEdit.Text, lMessage)) then
        begin
          lConfigData.YearsInAnalysis := StrToInt(Trim(NumberOfYearsEdit.Text));
          DoContextValidation(dvtNumberOfYears);
          RepopulateNumberOfYears;
        end
        else
          NumberOfYearsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TYieldRunYRCValidator.OnEditControlClick';
begin
  if (Sender = YieldRunYRCDialog.SummaryLevelRadioGroup) then
      OnEditControlEnter(Sender);
end;

procedure TYieldRunYRCValidator.RePopulateLoadCasesGrids;
const OPNAME = 'TYieldRunYRCValidator.RePopulateLoadCasesGrids';
var
  lConfigData  : TRunConfigurationData;
  lFeatureList : IMasterControlFeatureList;
  lFeature     : IMasterControlFeature;
  lCount       : integer;
  lIndex       : integer;
  lValue       : double;
  lChannelType : string;
  lActiveLoadCases : integer;
begin
  try
    with YieldRunYRCDialog do
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
              WaterLoadCasesGrid.Cells[0, lIndex] := Format('%6.2f', [lValue]);
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
              WaterLoadCasesGrid.Cells[1, lIndex] := Format('%6.2f', [lValue])
            else
              WaterLoadCasesGrid.Cells[1, lIndex] := '';
            lValue := lConfigData.TargetPowerByIndex[lIndex+1];
            if (lValue >= 0) then
              PowerLoadCasesGrid.Cells[0, lIndex] := Format('%6.2f', [lValue])
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

procedure TYieldRunYRCValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TYieldRunYRCValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with YieldRunYRCDialog do
    begin
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

procedure TYieldRunYRCValidator.UpdatePowerDemand (AIndex : integer;
                                                        AValue : string);
const OPNAME = 'TYieldRunYRCValidator.UpdatePowerDemand';
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
      with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.UpdateTargetYield (AIndex : integer;
                                                        AValue : string);
const OPNAME = 'TYieldRunYRCValidator.UpdateTargetYield';
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
      with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.UpdateMaximumYield (AIndex : integer;
                                                         AValue : string);
const OPNAME = 'TYieldRunYRCValidator.UpdateMaximumYield';
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
      with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.OnLoadCaseSelectedClick(Sender: TObject);
const OPNAME = 'TYieldRunYRCValidator.OnLoadCaseSelectedClick';
var
  lIndexA     : integer;
  lChkBox     : TFieldChkBox;
  lConfigData : TRunConfigurationData;
begin
  try
    if (NOT FSystemAction) then
    begin
      with YieldRunYRCDialog do
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
        if (lConfigData <> nil)then
        begin
          lChkBox := TFieldChkBox(Sender);
          lIndexA := YieldRunYRCDialog.IndexOfLoadCaseCheckBox(lChkBox);
          if (lIndexA >= 0) then
          begin
            if (lChkBox.Checked) then
              lConfigData.ActivateLoadCase(lIndexA)
            else
              lConfigdata.DeactivateLoadCase(lIndexA);
            DoContextValidation(dvtNrOfLoadCases);  
          end;
        end;
      end;
      RePopulateLoadCasesGrids;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TYieldRunYRCValidator.DoContextValidation';
var
  lConfigData  : TRunConfigurationData;
begin
  try
    FAllErrorMessages.Clear;
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      if (AValidationType in [dvtRunYRCWizardStep1, dvtSummaryLevel]) then
        ValidateSummaryLevel(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep2, dvtCalcFirmYield]) then
        ValidateCalcFirmYield(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep3, dvtMultiplePeriods]) then
        ValidateMultiplePeriods(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep4, dvtNumberOfYears]) then
        ValidateNumberOfYears(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep5, dvtTargetYield]) then
        ValidateTargetYield(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep5, dvtMaximumYield]) then
        ValidateMaximumYield(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep5, dvtTargetPower]) then
        ValidateTargetPower(lConfigData);
      if (AValidationType in [dvtRunYRCWizardStep5, dvtTargetYield,
                              dvtMaximumYield, dvtTargetPower, dvtNrOfLoadCases]) then
        ValidateNrOfLoadCases(lConfigData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TYieldRunYRCValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    case ASequence of
      1 : Result := DetermineWizardStatusStep1;
      2 : Result := DetermineWizardStatusStep2;
      3 : Result := DetermineWizardStatusStep3;
      4 : Result := DetermineWizardStatusStep4;
      5 : Result := DetermineWizardStatusStep5;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.DetermineWizardStatusStep1 : integer;
const OPNAME = 'TYieldRunYRCValidator.DetermineWizardStatusStep1';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunYRCWizardStep1);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.OutputSummaryLevel = 0) then
          Result := 2
        else
          Result := 0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.DetermineWizardStatusStep2 : integer;
const OPNAME = 'TYieldRunYRCValidator.DetermineWizardStatusStep2';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      DoContextValidation(dvtRunYRCWizardStep2);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.CalculateHistoricFirmYield = 0) then
          Result := 2
        else
          Result := 0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.DetermineWizardStatusStep3 : integer;
const OPNAME = 'TYieldRunYRCValidator.DetermineWizardStatusStep3';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunYRCWizardStep3);
      if (FAllErrorMessages.Count = 0) then
      begin
        if ((NOT lConfigData.MultiplePeriodLengths) OR
            (lConfigData.YearsInAnalysis < 10)) then
          Result := 2
        else
          Result := 0;
      end;
    end;
    YieldRunYRCDialog.NumberOfYearsWarningLabel.Visible := Result <> 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.DetermineWizardStatusStep4 : integer;
const OPNAME = 'TYieldRunYRCValidator.DetermineWizardStatusStep4';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunYRCWizardStep4);
      if (FAllErrorMessages.Count = 0) then
      begin
        if ((NOT lConfigData.MultiplePeriodLengths) OR
            (lConfigData.YearsInAnalysis < 10)) then
          Result := 2
        else
          Result := 0;
      end;
    end;
    YieldRunYRCDialog.NumberOfYearsWarningLabel.Visible := Result <> 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunYRCValidator.DetermineWizardStatusStep5 : integer;
const OPNAME = 'TYieldRunYRCValidator.DetermineWizardStatusStep5';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunYRCWizardStep5);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.NumberOfActiveLoadCases >= 3) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.ShowWizardStep (ASequence : integer = 0);
const OPNAME = 'TYieldRunYRCValidator.ShowWizardStep';
begin
  inherited ShowWizardStep(ASequence);
  try
    case ASequence of
      1 :
          if (YieldRunYRCDialog.PageControl.ActivePageIndex <> 0) then
            YieldRunYRCDialog.PageControl.ActivePageIndex := 0;
      2,3,4 :
          if (YieldRunYRCDialog.PageControl.ActivePageIndex <> 1) then
            YieldRunYRCDialog.PageControl.ActivePageIndex := 1;
      5 :
          if (YieldRunYRCDialog.PageControl.ActivePageIndex <> 2) then
            YieldRunYRCDialog.PageControl.ActivePageIndex := 2;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.ValidateSummaryLevel (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateSummaryLevel';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.ValidateCalcFirmYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateCalcFirmYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage,'CalcFirmYield')) then
          FirmYieldChkBox.InValidationError := FALSE
        else
        begin
          FirmYieldChkBox.InValidationError := TRUE;
          FirmYieldChkBox.ValidationError := FErrorMessage;
          FirmYieldChkBox.ShowErrorState(TRUE);
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunYRCValidator.ValidateMultiplePeriods (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateMultiplePeriods';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
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

procedure TYieldRunYRCValidator.ValidateNumberOfYears(AConfiguration: TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateNumberOfYears';
begin
  try
    with YieldRunYRCDialog do
    begin
      FErrorMessage := '';
      if (NOT AConfiguration.Validate(FErrorMessage,'NumberOfYears')) then
        FAllErrorMessages.Add(FErrorMessage);
      NumberOfYearsEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunYRCValidator.ValidateTargetYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateTargetYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
      begin
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

procedure TYieldRunYRCValidator.ValidateMaximumYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateMaximumYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage, 'MaximumYield')) then
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

procedure TYieldRunYRCValidator.ValidateTargetPower (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateTargetPower';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage, 'TargetPower')) then
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

procedure TYieldRunYRCValidator.ValidateNrOfLoadCases (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunYRCValidator.ValidateNrOfLoadCases';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunYRCDialog do
      begin
        FErrorMessage := '';
        if (ARunConfigData.Validate(FErrorMessage, 'NrOfLoadCases')) then
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

end.

