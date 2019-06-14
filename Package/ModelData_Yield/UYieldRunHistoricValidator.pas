{******************************************************************************}
{*  UNIT      : Contains the class TYieldRunHistoricValidator.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/22                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UYieldRunHistoricValidator;

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
  UYieldRunHistoricDialog;

type
  TYieldRunHistoricValidator = class(TAbstractYieldDataDialogValidator)
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
    procedure RepopulateFirmYieldCalculation;
    procedure RepopulateNumberOfYears;
    procedure RepopulateStartMonthNumber;
    procedure RepopulateLoadCasesGrids;
    procedure RepopulateDemandChannelGrid;
    procedure UpdateSummaryLevel;
    procedure UpdateCalculateFirmYield;
    procedure UpdateNumberOfYears;
    procedure UpdateStartMonthNumber;
    procedure UpdateTargetYield(AIndex : integer; AValue : string);
    procedure UpdateMaximumYield(AIndex : integer; AValue : string);
    procedure UpdatePowerDemand(AIndex : integer; AValue : string);
    procedure ValidateSummaryLevel (ARunConfigData : TRunConfigurationData);
    procedure ValidateCalcFirmYield (ARunConfigData : TRunConfigurationData);
    procedure ValidateNumberOfYears(AConfiguration: TRunConfigurationData);
    procedure ValidateStartMonthNumber(AConfiguration: TRunConfigurationData);
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
    function YieldRunHistoricDialog : TYieldRunHistoricDialog;
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
{* TYieldRunHistoricValidator                                                 *}
{******************************************************************************}

procedure TYieldRunHistoricValidator.CreateMemberObjects;
const OPNAME = 'TYieldRunHistoricValidator.CreateMemberObjects';
var
  lPanel     : TYieldRunHistoricDialog;
  lIndex     : integer;
begin
  try
    inherited CreateMemberObjects;
    FSystemAction := FALSE;
    FPanel  := TYieldRunHistoricDialog.Create(FPanelOwner,FAppModules);
    lPanel := YieldRunHistoricDialog;
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

      NumberOfYearsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YearsCount');
      NumberOfYearsEdit.OnEnter       := OnEditControlEnter;
      NumberOfYearsEdit.OnExit        := OnEditControltExit;

      StartMonthNumberEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('StartMonthNo');
      StartMonthNumberEdit.OnEnter       := OnEditControlEnter;
      StartMonthNumberEdit.OnExit        := OnEditControltExit;

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

procedure TYieldRunHistoricValidator.DestroyMemberObjects;
const OPNAME = 'TYieldRunHistoricValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.Initialise: boolean;
const OPNAME = 'TYieldRunHistoricValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with YieldRunHistoricDialog.RunTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.Historical'));
      Items.Add(FAppModules.Language.GetString('RunParameters.Stochastic'));
    end;
    with YieldRunHistoricDialog.SummaryLevelRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelBrief'));
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelAdditional'));
      Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelFull'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYieldRunHistoricValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('Viewdata.Properties');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.ClearDataViewer;
const OPNAME = 'TYieldRunHistoricValidator.ClearDataViewer';
var
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with YieldRunHistoricDialog do
    begin
      SummaryLevelRadioGroup.ItemIndex := -1;
      SummaryLevelRadioGroup.ItemIndex := -1;

      FirmYieldChkBox.Checked   := FALSE;
      NumberOfYearsEdit.Text    := '-1';
      StartMonthNumberEdit.Text := '-1';

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

procedure TYieldRunHistoricValidator.PopulateDataViewer;
const OPNAME = 'TYieldRunHistoricValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.RePopulateDataViewer;
const OPNAME = 'TYieldRunHistoricValidator.RePopulateDataViewer';
var
  lConfigData    : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
      begin
        if (lConfigData.RunSequenceType = 'S') then
        begin
          lConfigData.RunSequenceType := 'H';
          if (lConfigData.CalculateHistoricFirmYield = 2) then
            lConfigData.CalculateHistoricFirmYield := 1;
        end;
        RunTypeRadioGroup.ItemIndex := 0;

        RepopulateSummaryOutputLevel;
        RepopulateFirmYieldCalculation;
        RepopulateNumberOfYears;
        RepopulateStartMonthNumber;
        RePopulateLoadCasesGrids;
        RePopulateDemandChannelGrid;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.RepopulateSummaryOutputLevel;
const OPNAME = 'TYieldRunHistoricValidator.RepopulateSummaryOutputLevel';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunHistoricDialog do
    begin
      SummaryLevelRadioGroup.ItemIndex  := lConfigData.OutputSummaryLevel;
      SummaryOutputWarningLabel.Visible := lConfigData.OutputSummaryLevel <> 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.RepopulateFirmYieldCalculation;
const OPNAME = 'TYieldRunHistoricValidator.RepopulateFirmYieldCalculation';
var
  lConfigData    : TRunConfigurationData;
  lCalcFirmYield : Integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunHistoricDialog do
    begin
      lCalcFirmYield := lConfigData.CalculateHistoricFirmYield;
      FirmYieldChkBox.Checked := (lCalcFirmYield <> 0);
      FirmYieldWarningLabel.Visible := (lCalcFirmYield = 0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.RepopulateNumberOfYears;
const OPNAME = 'TYieldRunHistoricValidator.RepopulateNumberOfYears';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunHistoricDialog do
    begin
      NumberOfYearsEdit.SetFieldValue(IntToStr(lConfigData.YearsInAnalysis));
//      NumberOfYearsWarningLabel.Visible := lConfigData.YearsInAnalysis <> 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.RepopulateStartMonthNumber;
const OPNAME = 'TYieldRunHistoricValidator.RepopulateStartMonthNumber';
var
  lConfigData : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    with YieldRunHistoricDialog do
    begin
      StartMonthNumberEdit.SetFieldValue(IntToStr(lConfigData.StartMonthNumber));
      StartMonthNumberWarningLabel.Visible := lConfigData.StartMonthNumber <> 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.SaveState: boolean;
const OPNAME = 'TYieldRunHistoricValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.YieldRunHistoricDialog : TYieldRunHistoricDialog;
const OPNAME = 'TYieldRunHistoricValidator.YieldRunHistoricDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYieldRunHistoricDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldRunHistoricValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.StudyHasChanged: boolean;
const OPNAME = 'TYieldRunHistoricValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYieldRunHistoricValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    with YieldRunHistoricDialog do
    begin
      if (Sender = SummaryLevelRadioGroup) then
        UpdateSummaryLevel;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYieldRunHistoricValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with YieldRunHistoricDialog do
    begin
      if (Sender = SummaryLevelRadioGroup) then
        UpdateSummaryLevel
      else
      if (Sender = FirmYieldChkBox) then
        UpdateCalculateFirmYield
      else
      if ((Sender = StartMonthNumberEdit) AND (StartMonthNumberEdit.HasValueChanged)) then
        UpdateStartMonthNumber
      else
      if ((Sender = NumberOfYearsEdit) AND (NumberOfYearsEdit.HasValueChanged)) then
        UpdateNumberOfYears;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.UpdateSummaryLevel;
const OPNAME = 'TYieldRunHistoricValidator.UpdateSummaryLevel';
var
  lConfigData : TRunConfigurationData;
  lOldSummary : integer;
  lNewSummary : integer;
begin
  try
    with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.UpdateCalculateFirmYield;
const OPNAME = 'TYieldRunHistoricValidator.UpdateCalculateFirmYield';
var
  lConfigData    : TRunConfigurationData;
  lCalcFirmYield : Integer;
  lModelVersion  : string;
begin
  try
    with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.UpdateNumberOfYears;
const OPNAME = 'TYieldRunHistoricValidator.UpdateNumberOfYears';
var
  lConfigData : TRunConfigurationData;
  lMessage    : string;
begin
  try
    with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.UpdateStartMonthNumber;
const OPNAME = 'TYieldRunHistoricValidator.UpdateStartMonthNumber';
var
  lConfigData : TRunConfigurationData;
  lMessage    : string;
begin
  try
    with YieldRunHistoricDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'StartMonthNo', StartMonthNumberEdit.Text, lMessage)) then
        begin
          StartMonthNumberEdit.FieldValidationError := '';
          lConfigData.StartMonthNumber := StrToInt(Trim(StartMonthNumberEdit.Text));
          DoContextValidation(dvtStartMonthNo);
          RepopulateStartMonthNumber;
        end
        else
          StartMonthNumberEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TYieldRunHistoricValidator.OnEditControlClick';
begin
  if (Sender = YieldRunHistoricDialog.SummaryLevelRadioGroup) then
      OnEditControlEnter(Sender);
end;

procedure TYieldRunHistoricValidator.RePopulateLoadCasesGrids;
const OPNAME = 'TYieldRunHistoricValidator.RePopulateLoadCasesGrids';
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
    with YieldRunHistoricDialog do
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
              WaterLoadCasesGrid.Cells[0, lIndex] := SmartFloatFormat(lValue,lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals);
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
              WaterLoadCasesGrid.Cells[1, lIndex] := SmartFloatFormat(lValue,lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals)
            end
            else
              WaterLoadCasesGrid.Cells[1, lIndex] := '';
            lValue := lConfigData.TargetPowerByIndex[lIndex+1];
            if (lValue >= 0) then
            begin
              //PowerLoadCasesGrid.Cells[0, lIndex] := Format('%6.2f', [lValue])
              lFieldProperty := PowerLoadCasesGrid.FieldProperty(0);
              PowerLoadCasesGrid.Cells[0, lIndex] := SmartFloatFormat(lValue,lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals);
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

procedure TYieldRunHistoricValidator.RePopulateDemandChannelGrid;
const OPNAME = 'TYieldRunHistoricValidator.RePopulateDemandChannelGrid';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lFeature     : ISpecifiedDemandFeature;
  lIndex       : integer;
begin
  try
    with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TYieldRunHistoricValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.UpdatePowerDemand (AIndex : integer;
                                                        AValue : string);
const OPNAME = 'TYieldRunHistoricValidator.UpdatePowerDemand';
var
  lConfigData : TRunConfigurationData;
  lValue      : double;
  lMessage    : string;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil)then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.UpdateTargetYield (AIndex : integer;
                                                        AValue : string);
const OPNAME = 'TYieldRunHistoricValidator.UpdateTargetYield';
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
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.UpdateMaximumYield (AIndex : integer;
                                                         AValue : string);
const OPNAME = 'TYieldRunHistoricValidator.UpdateMaximumYield';
var
  lValue      : double;
  lMessage    : string;
  lConfigData : TRunConfigurationData;
  lIndex      : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil)then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.OnLoadCaseSelectedClick(Sender: TObject);
const OPNAME = 'TYieldRunHistoricValidator.OnLoadCaseSelectedClick';
var
  lIndexA     : integer;
  lChkBox     : TFieldChkBox;
  lConfigData : TRunConfigurationData;
begin
  try
    if (NOT FSystemAction) then
    begin
      with YieldRunHistoricDialog do
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
        if (lConfigData <> nil) then
        begin
          lChkBox := TFieldChkBox(Sender);
          lIndexA := YieldRunHistoricDialog.IndexOfLoadCaseCheckBox(lChkBox);
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

procedure TYieldRunHistoricValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TYieldRunHistoricValidator.DoContextValidation';
var
  lConfigData  : TRunConfigurationData;
begin
  try
    FAllErrorMessages.Clear;
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      if (AValidationType in [dvtRunHistoricWizardStep1, dvtSummaryLevel]) then
        ValidateSummaryLevel(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep2, dvtCalcFirmYield]) then
        ValidateCalcFirmYield(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep3, dvtNumberOfYears]) then
        ValidateNumberOfYears(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep4, dvtStartMonthNo]) then
        ValidateStartMonthNumber(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep5, dvtTargetYield]) then
        ValidateTargetYield(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep5, dvtMaximumYield]) then
        ValidateMaximumYield(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep5, dvtTargetPower]) then
        ValidateTargetPower(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep5, dvtTargetYield,
                              dvtMaximumYield, dvtTargetPower, dvtNrOfLoadCases]) then
        ValidateNrOfLoadCases(lConfigData);
      if (AValidationType in [dvtRunHistoricWizardStep6]) then
        ValidateSpecifiedDemandIndicators;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatus';
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
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatusStep1 : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatusStep1';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunHistoricWizardStep1);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.OutputSummaryLevel = 2) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatusStep2 : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatusStep2';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunHistoricWizardStep2);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.CalculateHistoricFirmYield = 1) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatusStep3 : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatusStep3';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunHistoricWizardStep3);
      if (FAllErrorMessages.Count = 0) then
      begin
        if (lConfigData.YearsInAnalysis < lConfigData.MaximumPeriodInHydrologyFiles) then
          Result := 1
        else
          Result := 2;
      end;
    end;
    YieldRunHistoricDialog.NumberOfYearsWarningLabel.Visible := Result <> 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatusStep4 : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatusStep4';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunHistoricWizardStep4);
      if (FAllErrorMessages.Count = 0) then
      begin
        if ((lConfigData.StartMonthNumber >= 1) OR (lConfigData.StartMonthNumber <= 12)) then
          Result := 2
        else
          Result := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatusStep5 : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatusStep5';
var
  lConfigData  : TRunConfigurationData;
begin
  Result := 0;
  try
    lConfigData  := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if ((lConfigData <> nil)) then
    begin
      DoContextValidation(dvtRunHistoricWizardStep5);
      if (FAllErrorMessages.Count = 0) then
        Result := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldRunHistoricValidator.DetermineWizardStatusStep6 : integer;
const OPNAME = 'TYieldRunHistoricValidator.DetermineWizardStatusStep6';
var
  lFeatureList : ISpecifiedDemandFeatureList;
  lIndicator   : string;
begin
  Result := 0;
  try
    DoContextValidation(dvtRunHistoricWizardStep6);
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
      if (lIndicator = 'H') then
        Result := 2
      else
        Result := 1;
      YieldRunHistoricDialog.SpecifiedDemandWarningLabel.Visible := Result = 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.ShowWizardStep (ASequence : integer = 0);
const OPNAME = 'TYieldRunHistoricValidator.ShowWizardStep';
begin
  inherited ShowWizardStep(ASequence);
  try
    case ASequence of
      1 :
          if (YieldRunHistoricDialog.PageControl.ActivePageIndex <> 0) then
            YieldRunHistoricDialog.PageControl.ActivePageIndex := 0;
      2,3,4 :
          if (YieldRunHistoricDialog.PageControl.ActivePageIndex <> 1) then
            YieldRunHistoricDialog.PageControl.ActivePageIndex := 1;
      5 :
          if (YieldRunHistoricDialog.PageControl.ActivePageIndex <> 2) then
            YieldRunHistoricDialog.PageControl.ActivePageIndex := 2;
      6 :
          if (YieldRunHistoricDialog.PageControl.ActivePageIndex <> 3) then
            YieldRunHistoricDialog.PageControl.ActivePageIndex := 3;
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldRunHistoricValidator.ValidateSummaryLevel (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateSummaryLevel';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.ValidateCalcFirmYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateCalcFirmYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.ValidateNumberOfYears(AConfiguration: TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateNumberOfYears';
begin
  try
    with YieldRunHistoricDialog do
    begin
      FErrorMessage := '';
      if (NOT AConfiguration.Validate(FErrorMessage,'NumberOfYears')) then
        FAllErrorMessages.Add(FErrorMessage);
      NumberOfYearsEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunHistoricValidator.ValidateStartMonthNumber(AConfiguration: TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateStartMonthNumber';
begin
  try
    with YieldRunHistoricDialog do
    begin
      FErrorMessage := '';
      if (NOT AConfiguration.Validate(FErrorMessage,'StartMonthNo')) then
        FAllErrorMessages.Add(FErrorMessage);
      StartMonthNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TYieldRunHistoricValidator.ValidateTargetYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateTargetYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.ValidateMaximumYield (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateMaximumYield';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.ValidateTargetPower (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateTargetPower';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.ValidateNrOfLoadCases (ARunConfigData : TRunConfigurationData);
const OPNAME = 'TYieldRunHistoricValidator.ValidateNrOfLoadCases';
begin
  try
    if (ARunConfigData <> nil) then
    begin
      with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.ValidateSpecifiedDemandIndicators;
const OPNAME = 'TYieldRunHistoricValidator.ValidateSpecifiedDemandIndicators';
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
    with YieldRunHistoricDialog do
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

procedure TYieldRunHistoricValidator.OnSetHistoricButtonClick (Sender : TObject);
const OPNAME = 'TYieldRunHistoricValidator.OnSetHistoricButtonClick';
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

procedure TYieldRunHistoricValidator.OnSetStochasticButtonClick (Sender : TObject);
const OPNAME = 'TYieldRunHistoricValidator.OnSetStochasticButtonClick';
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

