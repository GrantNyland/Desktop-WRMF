 {******************************************************************************}
{*  UNIT      : Contains the class TOutputConfigurationValidator.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/16                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UOutputConfigurationValidator;

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
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UOutputConfigurationDialog;

type
  TOutputConfigurationValidator = class(TAbstractYieldDataDialogValidator)
  private
    FReservoirSummaryCount : integer;
    FChannelSummaryCount   : integer;
    FActiveReservoirCount  : integer;
    FChannelAnalysisCount  : integer;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlClick(Sender: TObject);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnDebugLevelChange(Sender: TObject);
    procedure OnDebugStartYearChange(Sender: TObject);
    procedure OnDebugStartMonthChange(Sender: TObject);
    procedure OnDebugEndYearChange(Sender: TObject);
    procedure OnDebugEndMonthChange(Sender: TObject);
    procedure OnAnnualSummaryChange(Sender : TObject);
    procedure PopulateDebugStartMonthCbx;
    procedure PopulateDebugEndMonthCbx;
    function CalcDebugStartPeriod : integer;
    function CalcDebugEndPeriod : integer;
    function ConvertToIndex (AMonth : integer) : integer;
    function ConvertToMonth (AIndex : integer) : integer;
    procedure DisplayDebugStartDate;
    procedure DisplayDebugEndDate;
    procedure PopulateDebugYearControls;
    procedure PopulateDebugLevelCbx;
    procedure DisableModelControls;
    procedure UpdateDebugLevel;
    procedure UpdateDebugStartPeriod;
    procedure UpdateDebugEndDate;

    procedure RePopulateDataViewer;
    procedure UpdateSummaryLevel;
    procedure UpdateOutputInputData;
    procedure UpdateStoreYieldResults;
    procedure UpdateCreatePlotFile;

    procedure UpdateDetailedOption;
    procedure UpdateSupplyOption;
    procedure UpdateEconomicOption;
    procedure UpdatePlanningSummary;
    procedure UpdateInputDataSummary;
    procedure UpdateAnnualSummary;
    procedure UpdateCreatePlotFileInControlParameters;

    procedure DisplayChannelSummaryRequired (ANode    : TTreeNode;
                                             AChannel : IGeneralFlowChannel);
    procedure DisplayChannelAnalysisRequired (ANode   : TTreeNode;
                                             AChannel : IGeneralFlowChannel);
    procedure DisplayReservoirSummaryRequired (ANode : TTreeNode);
    procedure DisplayReservoirActive (ANode : TTreeNode);
    procedure UpdateChannelSummaryRequired (ANode     : TTreeNode;
                                            ARequired : Boolean);
    procedure UpdateChannelAnalysisRequired (ANode     : TTreeNode;
                                             ARequired : Boolean);
    procedure UpdateReservoirSummaryRequired (ANode      : TTreeNode;
                                              ARequired  : Boolean);
    procedure UpdateReservoirActive (ANode   : TTreeNode;
                                     AActive : Boolean);
    procedure ValidateDebugStartPeriod(AConfiguration : IRunConfigurationData);
    procedure ValidateDebugEndPeriod(AConfiguration : IRunConfigurationData);
    procedure ValidateDebugLevel(AConfiguration : IRunConfigurationData);

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
    function OutputConfigurationDialog : TOutputConfigurationDialog;
    procedure DoContextValidation(AValidationType: TDialogValidationType);override;

  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  URunConfigurationData,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math;

{******************************************************************************}
{* TOutputConfigurationValidator                                              *}
{******************************************************************************}

procedure TOutputConfigurationValidator.CreateMemberObjects;
const OPNAME = 'TOutputConfigurationValidator.CreateMemberObjects';
var
  lPanel     : TOutputConfigurationDialog;
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lParent    : TControl;
  lChkBox    : TFieldChkBox;
begin
  try
    inherited CreateMemberObjects;
    FPanel  := TOutputConfigurationDialog.Create(FPanelOwner,FAppModules);
    lPanel := OutputConfigurationDialog;
    with lPanel do
    begin
      SummaryLevelRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('SummaryLevel');
      SummaryLevelRadioGroup.OnEnter       := OnEditControlEnter;
      SummaryLevelRadioGroup.OnClick       := OnEditControlClick;

      InputDataChkBox.FieldProperty        := FAppModules.FieldProperties.FieldProperty('SummaryOut');
      YieldResultsChkBox.FieldProperty     := FAppModules.FieldProperties.FieldProperty('StoreYield');
      PlotFileChkBox.FieldProperty         := FAppModules.FieldProperties.FieldProperty('PlotOpt');

      CreatePlotFileChkBox.FieldProperty   := FAppModules.FieldProperties.FieldProperty('PlotOpt');
      DebugLevelCbx.FieldProperty          := FAppModules.FieldProperties.FieldProperty('DebugLevel');
      DebugLevelCbx.OnChange               := OnDebugLevelChange;
      DebugLevelCbx.OnEnter                := OnEditControlEnter;

      DebugStartYearCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('DebugStartYear');
      DebugStartYearCbx.OnExit             := OnEditControltExit;
      DebugStartYearCbx.OnChange           := OnDebugStartYearChange;
      DebugStartYearCbx.OnEnter            := OnEditControlEnter;

      DebugEndYearCbx.FieldProperty        := FAppModules.FieldProperties.FieldProperty('DebugEndYear');
      DebugEndYearCbx.OnExit               := OnEditControltExit;
      DebugEndYearCbx.OnChange             := OnDebugEndYearChange;
      DebugEndYearCbx.OnEnter              := OnEditControlEnter;

      DebugStartMonthCbx.FieldProperty     := FAppModules.FieldProperties.FieldProperty('DebugStartMonth');
      DebugStartMonthCbx.OnExit            := OnEditControltExit;
      DebugStartMonthCbx.OnChange          := OnDebugStartMonthChange;
      DebugStartMonthCbx.OnEnter           := OnEditControlEnter;

      DebugEndMonthCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('DebugEndMonth');
      DebugEndMonthCbx.OnExit              := OnEditControltExit;
      DebugEndMonthCbx.OnChange            := OnDebugEndMonthChange;
      DebugEndMonthCbx.OnEnter             := OnEditControlEnter;

      DebugStartPeriodEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('DebugInit');
      DebugEndPeriodEdit.FieldProperty     := FAppModules.FieldProperties.FieldProperty('DebugFinal');


      DetailedOptionChkBox.FieldProperty     := FAppModules.FieldProperties.FieldProperty('DetailedOption');
      SupplyOptionChkBox.FieldProperty       := FAppModules.FieldProperties.FieldProperty('SupplyOption');
      EconomicOptionChkBox.FieldProperty     := FAppModules.FieldProperties.FieldProperty('EconomicOption');
      PlanningSummaryChkBox.FieldProperty    := FAppModules.FieldProperties.FieldProperty('PlanningSummary');
      InputSummaryChkBox.FieldProperty       := FAppModules.FieldProperties.FieldProperty('InputSummary');

      AnnualSummaryRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('AnnualSummary');
      AnnualSummaryRadioGroup.OnEnter := OnEditControlEnter;
      AnnualSummaryRadioGroup.OnClick := OnAnnualSummaryChange;

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
      else if (lComponent.ClassNameIs('TFieldChkBox')) then
      begin
        lChkBox         := TFieldChkBox(lComponent);
        lChkBox.OnEnter := OnEditControlEnter;
        lChkBox.OnClick := OnEditControlClick;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.DestroyMemberObjects;
const OPNAME = 'TOutputConfigurationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.Initialise: boolean;
const OPNAME = 'TOutputConfigurationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with OutputConfigurationDialog do
    begin
      EconomicOptionChkBox.Visible :=  False;

      SummaryLevelRadioGroup.Items.Clear;
      SummaryLevelRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelBrief'));
      SummaryLevelRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelAdditional'));
      SummaryLevelRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.SummaryLevelFull'));

      AnnualSummaryRadioGroup.Items.Clear;
      AnnualSummaryRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryQ'));
      AnnualSummaryRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryD'));
      AnnualSummaryRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryY'));
      AnnualSummaryRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.AnnualSummaryN'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputConfigurationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.ClearDataViewer;
const OPNAME = 'TOutputConfigurationValidator.ClearDataViewer';
var
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lChkBox    : TFieldChkBox;
  lRadioGrp  : TFieldRadioGroup;
begin
  inherited ClearDataViewer;
  try
    with OutputConfigurationDialog do
    begin
      FReservoirSummaryCount := 0;
      FChannelSummaryCount   := 0;
      FActiveReservoirCount  := 0;
      FChannelAnalysisCount  := 0;
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
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.PopulateDataViewer;
const OPNAME = 'TOutputConfigurationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.RePopulateDataViewer;
const OPNAME = 'TOutputConfigurationValidator.RePopulateDataViewer';
var
  lKeyValues     : string;
  lFieldIndex    : string;
  lAnnualSummary : string;
  lItemIndex     : integer;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    DisableModelControls;
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData ;
    if (lConfigData <> nil) then
    begin
      with OutputConfigurationDialog do
      begin
        lFieldIndex := '';

        SummaryLevelRadioGroup.ItemIndex := lConfigData.OutputSummaryLevel;
        lFieldProperty := SummaryLevelRadioGroup.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        SummaryLevelRadioGroup.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        SummaryLevelRadioGroup.HasChanges  :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        InputDataChkBox.Checked := lConfigdata.CreateDataFile;
        lFieldProperty := InputDataChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        InputDataChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        InputDataChkBox.HasChanges  :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        YieldResultsChkBox.Checked := lConfigdata.CreateYieldFile;
        lFieldProperty := YieldResultsChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        YieldResultsChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        YieldResultsChkBox.HasChanges  :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        PlotFileChkBox.Checked := lConfigdata.CreatePlotFile;
        lFieldProperty := PlotFileChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        PlotFileChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        PlotFileChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        CreatePlotFileChkBox.Checked := lConfigdata.CreatePlotFile;
        lFieldProperty := CreatePlotFileChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        CreatePlotFileChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        CreatePlotFileChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        DetailedOptionChkBox.Checked := lConfigdata.DetailedOption;
        lFieldProperty := DetailedOptionChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        DetailedOptionChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        DetailedOptionChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        SupplyOptionChkBox.Checked := lConfigdata.SupplyOption;
        lFieldProperty := SupplyOptionChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        SupplyOptionChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        SupplyOptionChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        EconomicOptionChkBox.Checked := lConfigdata.EconomicOption;
        lFieldProperty := EconomicOptionChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        EconomicOptionChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        EconomicOptionChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        PlanningSummaryChkBox.Checked := lConfigdata.PlanningSummary;
        lFieldProperty := PlanningSummaryChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        PlanningSummaryChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        PlanningSummaryChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        InputSummaryChkBox.Checked := lConfigdata.InputSummary;
        lFieldProperty := InputSummaryChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        InputSummaryChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        InputSummaryChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        lItemIndex := 0;
        lAnnualSummary := lConfigData.AnnualSummary;
        if lAnnualSummary = 'Q' then
          lItemIndex := 0
        else if lAnnualSummary = 'D' then
          lItemIndex := 1
        else if lAnnualSummary = 'Y' then
          lItemIndex := 2
        else if lAnnualSummary = 'N' then
          lItemIndex := 3;

        AnnualSummaryRadioGroup.ItemIndex := lItemIndex;
        lFieldProperty := AnnualSummaryRadioGroup.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        AnnualSummaryRadioGroup.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        AnnualSummaryRadioGroup.HasChanges  :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);


        PopulateDebugLevelCbx;
        PopulateDebugYearControls;
        DebugLevelCbx.ItemIndex := lConfigdata.DebugLevel + 3;
        OnDebugLevelChange(Self);
        DisplayDebugStartDate;
        DisplayDebugEndDate;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.SaveState: boolean;
const OPNAME = 'TOutputConfigurationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.OutputConfigurationDialog : TOutputConfigurationDialog;
const OPNAME = 'TOutputConfigurationValidator.OutputConfigurationDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TOutputConfigurationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputConfigurationValidator.StudyDataHasChanged';
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

function TOutputConfigurationValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputConfigurationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with OutputConfigurationDialog do
    begin
     if ((Sender = DebugLevelCbx) AND DebugLevelCbx.HasValueChanged) then
        UpdateDebugLevel
     else
      if (((Sender = DebugStartYearCbx) AND (DebugStartYearCbx.HasValueChanged)) OR
          ((Sender = DebugStartMonthCbx) AND (DebugStartMonthCbx.HasValueChanged)) OR
          ((Sender = DebugStartPeriodEdit) AND (DebugStartPeriodEdit.HasValueChanged))) then
        UpdateDebugStartPeriod
      else
      if (((Sender = DebugEndYearCbx) AND (DebugEndYearCbx.HasValueChanged)) OR
          ((Sender = DebugEndMonthCbx) AND (DebugEndMonthCbx.HasValueChanged)) OR
          ((Sender = DebugEndPeriodEdit) AND (DebugEndPeriodEdit.HasValueChanged))) then
        UpdateDebugEndDate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateSummaryLevel;
const OPNAME = 'TOutputConfigurationValidator.UpdateSummaryLevel';
var
  lConfigData : TRunConfigurationData;
  lOldSummary : integer;
  lNewSummary : integer;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lOldSummary := lConfigData.OutputSummaryLevel;
        lNewSummary := SummaryLevelRadioGroup.ItemIndex;
        if (lOldSummary <> lNewSummary) and (lNewSummary <> -1)then
        begin
          lConfigData.OutputSummaryLevel := lNewSummary;
          SummaryLevelRadioGroup.ItemIndex := lConfigData.OutputSummaryLevel;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateOutputInputData;
const OPNAME = 'TOutputConfigurationValidator.UpdateOutputInputData';
var
  lConfigData  : TRunConfigurationData;
  lOutputInput : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lOutputInput := lConfigData.CreateDataFile;
        if ((lOutputInput AND (NOT InputDataChkBox.Checked)) OR
            ((NOT lOutputInput) AND InputDataChkBox.Checked)) then
        begin
          lConfigData.CreateDataFile := InputDataChkBox.Checked;
          InputDataChkBox.Checked := lConfigData.CreateDataFile;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateStoreYieldResults;
const OPNAME = 'TOutputConfigurationValidator.UpdateStoreYieldResults';
var
  lConfigData : TRunConfigurationData;
  lStoreYield : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lStoreYield := lConfigData.CreateYieldFile;
        if ((lStoreYield AND (NOT YieldResultsChkBox.Checked)) OR
            ((NOT lStoreYield) AND YieldResultsChkBox.Checked)) then
        begin
          lConfigData.CreateYieldFile := YieldResultsChkBox.Checked;
          YieldResultsChkBox.Checked := lConfigData.CreateYieldFile;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateCreatePlotFile;
const OPNAME = 'TOutputConfigurationValidator.UpdateCreatePlotFile';
var
  lConfigData : TRunConfigurationData;
  lPlotFile   : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lPlotFile := lConfigData.CreatePlotFile;
        if ((lPlotFile AND (NOT PlotFileChkBox.Checked)) OR
            ((NOT lPlotFile) AND PlotFileChkBox.Checked)) then
        begin
          lConfigData.CreatePlotFile := PlotFileChkBox.Checked;
          PlotFileChkBox.Checked := lConfigData.CreatePlotFile;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateDetailedOption;
const OPNAME = 'TOutputConfigurationValidator.UpdateDetailedOption';
var
  lConfigData     : TRunConfigurationData;
  lDetailedOption : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lDetailedOption := lConfigData.DetailedOption;
        if ((lDetailedOption AND (NOT DetailedOptionChkBox.Checked)) OR
            ((NOT lDetailedOption) AND DetailedOptionChkBox.Checked)) then
        begin
          lConfigData.DetailedOption := DetailedOptionChkBox.Checked;
          DetailedOptionChkBox.Checked := lConfigData.DetailedOption;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateSupplyOption;
const OPNAME = 'TOutputConfigurationValidator.UpdateSupplyOption';
var
  lConfigData   : TRunConfigurationData;
  lSupplyOption : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lSupplyOption := lConfigData.SupplyOption;
        if ((lSupplyOption AND (NOT SupplyOptionChkBox.Checked)) OR
            ((NOT lSupplyOption) AND SupplyOptionChkBox.Checked)) then
        begin
          lConfigData.SupplyOption := SupplyOptionChkBox.Checked;
          SupplyOptionChkBox.Checked := lConfigData.SupplyOption;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateEconomicOption;
const OPNAME = 'TOutputConfigurationValidator.UpdateEconomicOption';
var
  lConfigData     : TRunConfigurationData;
  lEconomicOption : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lEconomicOption := lConfigData.EconomicOption;
        if ((lEconomicOption AND (NOT EconomicOptionChkBox.Checked)) OR
            ((NOT lEconomicOption) AND EconomicOptionChkBox.Checked)) then
        begin
          lConfigData.EconomicOption := EconomicOptionChkBox.Checked;
          EconomicOptionChkBox.Checked := lConfigData.EconomicOption;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdatePlanningSummary;
const OPNAME = 'TOutputConfigurationValidator.UpdatePlanningSummary';
var
  lConfigData      : TRunConfigurationData;
  lPlanningSummary : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lPlanningSummary := lConfigData.PlanningSummary;
        if ((lPlanningSummary AND (NOT PlanningSummaryChkBox.Checked)) OR
            ((NOT lPlanningSummary) AND PlanningSummaryChkBox.Checked)) then
        begin
          lConfigData.PlanningSummary := PlanningSummaryChkBox.Checked;
          PlanningSummaryChkBox.Checked := lConfigData.PlanningSummary;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateInputDataSummary;
const OPNAME = 'TOutputConfigurationValidator.UpdateInputDataSummary';
var
  lConfigData       : TRunConfigurationData;
  lInputDataSummary : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lInputDataSummary := lConfigData.InputSummary;
        if ((lInputDataSummary AND (NOT InputSummaryChkBox.Checked)) OR
            ((NOT lInputDataSummary) AND InputSummaryChkBox.Checked)) then
        begin
          lConfigData.InputSummary := InputSummaryChkBox.Checked;
          InputSummaryChkBox.Checked := lConfigData.InputSummary;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateAnnualSummary;
const OPNAME = 'TOutputConfigurationValidator.UpdateAnnualSummary';
var
  lConfigData : TRunConfigurationData;
  lOldSummary : string;
  lNewSummary : string;
  LItemIndex  : integer;
begin
  try
    if not OutputConfigurationDialog.AnnualSummaryRadioGroup.Visible then Exit;
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        LItemIndex := 0;
        lOldSummary := lConfigData.AnnualSummary;
        if AnnualSummaryRadioGroup.ItemIndex = 0 then
        begin
          lNewSummary := 'Q';
          LItemIndex  := 0;
        end
        else if AnnualSummaryRadioGroup.ItemIndex = 1 then
        begin
          lNewSummary := 'D';
           LItemIndex  := 1;
        end
        else if AnnualSummaryRadioGroup.ItemIndex = 2 then
        begin
          lNewSummary := 'Y';
           LItemIndex  := 2;
        end
        else if AnnualSummaryRadioGroup.ItemIndex = 3 then
        begin
          lNewSummary := 'N';
          LItemIndex  := 3;
        end;
        if (lOldSummary <> lNewSummary) then
        begin
          lConfigData.AnnualSummary := lNewSummary;
          AnnualSummaryRadioGroup.ItemIndex := LItemIndex;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateCreatePlotFileInControlParameters;
const OPNAME = 'TOutputConfigurationValidator.UpdateCreatePlotFileInControlParameters';
var
  LConfigData : TRunConfigurationData;
  LPlotFile   : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (LConfigData <> nil) then
      begin
        LPlotFile := LConfigData.CreatePlotFile;
        if ((LPlotFile AND (NOT CreatePlotFileChkBox.Checked)) OR
            ((NOT LPlotFile) AND CreatePlotFileChkBox.Checked)) then
        begin
          LConfigData.CreatePlotFile := CreatePlotFileChkBox.Checked;
          CreatePlotFileChkBox.Checked := LConfigData.CreatePlotFile;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.DisplayChannelSummaryRequired (ANode    : TTreeNode;
                                                                       AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputConfigurationValidator.DisplayChannelSummaryRequired';
begin
  try
    if (AChannel.ChannelType = 2) then
      // Master control channel always included in firm yield analysis
    begin
      if (ANode.StateIndex <> 3) then
        FChannelSummaryCount := FChannelSummaryCount + 1;
      ANode.StateIndex := 3;
    end
    else
    begin
      if (AChannel.SummaryOutputRequired = 'Y') then
      begin
        if (ANode.StateIndex <> 3) then
          FChannelSummaryCount := FChannelSummaryCount + 1;
        ANode.StateIndex := 3;
      end
      else
      begin
        if (ANode.StateIndex = 3) then
          FChannelSummaryCount := FChannelSummaryCount - 1;
        ANode.StateIndex := 2;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.DisplayChannelAnalysisRequired (ANode    : TTreeNode;
                                                                        AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputConfigurationValidator.DisplayChannelAnalysisRequired';
begin
  try
    case AChannel.ChannelType of
    2 : // Master control channel always included in firm yield analysis
      begin
        if (ANode.ImageIndex <> 1) then
          FChannelAnalysisCount := FChannelAnalysisCount + 1;
        ANode.ImageIndex    := 1;
        ANode.SelectedIndex := 1;
      end;
    6, 8 : // Minimum flow and Min-max channels may be included in firm yield analysis
      begin
        if (AChannel.SummaryOutputRequired = 'Y') then
        begin
          if (AChannel.RequiresFirmYieldAnalysis = 'Y') then
          begin
            if (ANode.ImageIndex <> 1) then
              FChannelAnalysisCount := FChannelAnalysisCount + 1;
            ANode.ImageIndex    := 1;
            ANode.SelectedIndex := 1;
          end
          else
          begin
            if (ANode.ImageIndex = 1) then
              FChannelAnalysisCount := FChannelAnalysisCount - 1;
            ANode.ImageIndex    := 0;
            ANode.SelectedIndex := 0;
          end;
        end
        else
        begin
          if (ANode.ImageIndex = 1) then
            FChannelAnalysisCount := FChannelAnalysisCount - 1;
          ANode.ImageIndex    := 5;
          ANode.SelectedIndex := 5;
        end;
      end;
    else // All other channels are never included in firm yield analysis
      begin
        ANode.ImageIndex      := 5;
        ANode.SelectedIndex   := 5;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.DisplayReservoirSummaryRequired (ANode : TTreeNode);
const OPNAME = 'TOutputConfigurationValidator.DisplayReservoirSummaryRequired';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (lReservoir.IncludeSummary = 'Y') then
    begin
      if (ANode.StateIndex <> 3) then
        FReservoirSummaryCount := FReservoirSummaryCount + 1;
      ANode.StateIndex := 3;
    end
    else
    begin
      if (ANode.StateIndex = 3) then
        FReservoirSummaryCount := FReservoirSummaryCount - 1;
      ANode.StateIndex := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.DisplayReservoirActive (ANode : TTreeNode);
const OPNAME = 'TOutputConfigurationValidator.DisplayReservoirActive';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (ANode.ImageIndex <> 5) then
    begin
      if (lReservoir.StatusIndicator = 1) then
      begin
        if (ANode.ImageIndex <> 1) then
          FActiveReservoirCount := FActiveReservoirCount + 1;
        ANode.ImageIndex    := 1;
        ANode.SelectedIndex := 1;
      end
      else
      begin
        if (ANode.ImageIndex = 1) then
          FActiveReservoirCount := FActiveReservoirCount - 1;
        ANode.ImageIndex    := 0;
        ANode.SelectedIndex := 0;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationvalidator.UpdateChannelSummaryRequired
                                          (ANode     : TTreeNode;
                                           ARequired : Boolean);
const OPNAME = 'TOutputConfigurationvalidator.UpdateChannelSummaryRequired';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
  lChannelNr   : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    lChannelNr   := Integer(ANode.Data);
    lChannel     := lChannelList.ChannelByChannelNumber[lChannelNr];
    if (ARequired) then
      lChannel.SummaryOutputRequired := 'Y' {RHS comeback}
    else
      lChannel.SummaryOutputRequired := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.UpdateChannelAnalysisRequired
                                          (ANode     : TTreeNode;
                                           ARequired : Boolean);
const OPNAME = 'TOutputConfigurationValidator.UpdateChannelAnalysisRequired';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
  lChannelNr   : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    lChannelNr   := Integer(ANode.Data);
    lChannel     := lChannelList.ChannelByChannelNumber[lChannelNr];
    if (ARequired) then
      lChannel.RequiresFirmYieldAnalysis := 'Y'
    else
      lChannel.RequiresFirmYieldAnalysis := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationvalidator.UpdateReservoirSummaryRequired
                                          (ANode      : TTreeNode;
                                           ARequired  : Boolean);
const OPNAME = 'TOutputConfigurationvalidator.UpdateReservoirSummaryRequired';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (ARequired) then
      lReservoir.IncludeSummary := 'Y'
    else
      lReservoir.IncludeSummary := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationvalidator.UpdateReservoirActive
                                          (ANode   : TTreeNode;
                                           AActive : Boolean);
const OPNAME = 'TOutputConfigurationvalidator.UpdateReservoirActive';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (AActive) then
      lReservoir.StatusIndicator := 1
    else
      lReservoir.StatusIndicator := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnEditControlClick';
begin
  try
    with OutputConfigurationDialog do
    begin
      if((Sender = SummaryLevelRadioGroup) AND
         (NOT SummaryLevelRadioGroup.HasChanges) AND
         (SummaryLevelRadioGroup.HasValueChanged)) then
        UpdateSummaryLevel
      else
      if((Sender = InputDataChkBox) AND
         (NOT InputDataChkBox.HasChanges) AND
         (InputDataChkBox.HasValueChanged)) then
        UpdateOutputInputData
      else
      if((Sender = YieldResultsChkBox) AND
         (NOT YieldResultsChkBox.HasChanges) AND
         (YieldResultsChkBox.HasValueChanged)) then
        UpdateStoreYieldResults
      else
      if((Sender = PlotFileChkBox) AND
         (NOT PlotFileChkBox.HasChanges) AND
         (PlotFileChkBox.HasValueChanged)) then
        UpdateCreatePlotFile
      else
      if((Sender = CreatePlotFileChkBox) AND
         (NOT CreatePlotFileChkBox.HasChanges) AND
         (CreatePlotFileChkBox.HasValueChanged)) then
        UpdateCreatePlotFileInControlParameters
      else
      if((Sender = DetailedOptionChkBox) AND
         (NOT DetailedOptionChkBox.HasChanges) AND
         (DetailedOptionChkBox.HasValueChanged)) then
        UpdateDetailedOption
      else
      if((Sender = SupplyOptionChkBox) AND
         (NOT SupplyOptionChkBox.HasChanges) AND
         (SupplyOptionChkBox.HasValueChanged)) then
        UpdateSupplyOption
      else
      if((Sender = EconomicOptionChkBox) AND
         (NOT EconomicOptionChkBox.HasChanges) AND
         (EconomicOptionChkBox.HasValueChanged)) then
        UpdateEconomicOption
      else
      if ((Sender = PlanningSummaryChkBox) AND
         (NOT PlanningSummaryChkBox.HasChanges) AND
         (PlanningSummaryChkBox.HasValueChanged)) then
        UpdatePlanningSummary
      else
      if ((Sender = InputSummaryChkBox) AND
         (NOT InputSummaryChkBox.HasChanges) AND
         (InputSummaryChkBox.HasValueChanged)) then
        UpdateInputDataSummary;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnDebugStartYearChange(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnDebugStartYearChange';
var
  lMonth  : string;
  lPeriod : integer;
begin
  try
    with OutputConfigurationDialog do
    begin
      lMonth := DebugStartMonthCbx.Items[DebugStartMonthCbx.ItemIndex];
      PopulateDebugStartMonthCbx;
      DebugStartMonthCbx.ItemIndex := DebugStartMonthCbx.Items.IndexOf(lMonth);
      lPeriod := CalcDebugStartPeriod;
      DebugStartPeriodEdit.SetFieldValue(lPeriod);
      DoContextValidation(dvtDebugStartPeriod);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnDebugStartMonthChange(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnDebugStartMonthChange';
var
  lPeriod : integer;
begin
  try
    with OutputConfigurationDialog do
    begin
      lPeriod := CalcDebugStartPeriod;
      DebugStartPeriodEdit.SetFieldValue(lPeriod);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnDebugEndYearChange(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnDebugEndYearChange';
var
  lMonth  : string;
  lPeriod : integer;
begin
  try
    with OutputConfigurationDialog do
    begin
      lMonth := DebugEndMonthCbx.Items[DebugEndMonthCbx.ItemIndex];
      PopulateDebugEndMonthCbx;
      DebugEndMonthCbx.ItemIndex := DebugEndMonthCbx.Items.IndexOf(lMonth);
      lPeriod := CalcDebugEndPeriod;
      DebugEndPeriodEdit.SetFieldValue(lPeriod);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnDebugEndMonthChange(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnDebugEndMonthChange';
var
  lPeriod : integer;
begin
  try
    with OutputConfigurationDialog do
    begin
      lPeriod := CalcDebugEndPeriod;
      DebugEndPeriodEdit.SetFieldValue(lPeriod);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.PopulateDebugStartMonthCbx;
const OPNAME = 'TOutputConfigurationValidator.PopulateDebugStartMonthCbx';
var
  lMonth      : integer;
  lConfigData : IRunConfigurationData;
  lStartMonth : integer;
  lIndex      : integer;
  lNrOfYears  : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    lStartMonth := FAppModules.StudyArea.CalendarStartMonth;
    lNrOfYears  := lConfigData.YearsInAnalysis;
    with OutputConfigurationDialog do
    begin
      DebugStartMonthCbx.Items.Clear;
      if (FAppModules.Model.ModelName = CYield) then
      begin
        for lMonth := 1 to 12 do
        begin
          if (((DebugStartYearCbx.ItemIndex = 0) AND (lMonth >= lStartMonth)) OR
              ((DebugStartYearCbx.ItemIndex = lNrOfYears) AND (lMonth < lStartMonth)) OR
              ((DebugStartYearCbx.ItemIndex > 0) AND (DebugStartYearCbx.ItemIndex < lNrOfYears))) then
          begin
            lIndex := ConvertToIndex(lMonth);
            DebugStartMonthCbx.Items.Add(lConfigData.MonthNameByIndex[lIndex]);
          end;
        end;
      end
      else if (FAppModules.Model.ModelName = CPlanning) then
      begin
        for lMonth := 1 to 12 do
        begin
          lIndex := ConvertToIndex(lMonth);
          DebugStartMonthCbx.Items.Add(lConfigData.MonthNameByIndex[lIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.CalcDebugStartPeriod: integer;
const OPNAME = 'TOutputConfigurationValidator.CalcDebugStartPeriod';
var
  lConfigData : IRunConfigurationData;
  lYear       : integer;
  lMonthName  : string;
  lMonth      : integer;
  lIndex      : integer;
  lFound      : boolean;
begin
  Result := 0;
  try
    with OutputConfigurationDialog do
    begin
      if ((DebugStartYearCbx.ItemIndex >= 0) AND (DebugStartMonthCbx.ItemIndex >= 0)) then
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                         RunConfigurationData;
        lYear       := StrToInt(DebugStartYearCbx.Items[DebugStartYearCbx.ItemIndex]);
        lMonthName  := DebugStartMonthCbx.Items[DebugStartMonthCbx.ItemIndex];
        lFound := FALSE;
        lIndex := 1;
        while ((NOT lFound) AND (lIndex <= 12)) do
        begin
          if (lMonthName = lConfigData.MonthNameByIndex[lIndex]) then
            lFound := TRUE
          else
            lIndex := lIndex + 1;
        end;
        lMonth := ConvertToMonth(lIndex);
        if (lMonth >= FAppModules.StudyArea.CalendarStartMonth) then
          Result := (lYear - lConfigData.StartYearOther) * 12 + lIndex
        else
          Result := (lYear - 1 - lConfigData.StartYearOther) * 12 + lIndex;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.CalcDebugEndPeriod: integer;
const OPNAME = 'TOutputConfigurationValidator.CalcDebugEndPeriod';
var
  lConfigData : IRunConfigurationData;
  lYear       : integer;
  lMonthName  : string;
  lMonth      : integer;
  lIndex      : integer;
  lFound      : boolean;
begin
  Result := 0;
  try
    with OutputConfigurationDialog do
    begin
      if ((DebugEndYearCbx.ItemIndex >= 0) AND (DebugEndMonthCbx.ItemIndex >= 0)) then
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
        lYear       := StrToInt(DebugEndYearCbx.Items[DebugEndYearCbx.ItemIndex]);
        lMonthName  := DebugEndMonthCbx.Items[DebugEndMonthCbx.ItemIndex];
        lFound := FALSE;
        lIndex := 1;
        while ((NOT lFound) AND (lIndex <= 12)) do
        begin
          if (lMonthName = lConfigData.MonthNameByIndex[lIndex]) then
            lFound := TRUE
          else
            lIndex := lIndex + 1;
        end;
        lMonth := ConvertToMonth(lIndex);
        if (lMonth >= FAppModules.StudyArea.CalendarStartMonth) then
          Result := (lYear - lConfigData.StartYearOther) * 12 + lIndex
        else
          Result := (lYear - 1 - lConfigData.StartYearOther) * 12 + lIndex;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TOutputConfigurationValidator.ConvertToIndex(AMonth: integer): integer;
const OPNAME = 'TOutputConfigurationValidator.ConvertToIndex';
var
  lStartMonth : integer;
begin
  Result := 0;
  try
    lStartMonth := FAppModules.StudyArea.CalendarStartMonth;
    Result := ((AMonth + (12 - lStartMonth)) mod 12) + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.PopulateDebugEndMonthCbx;
const OPNAME = 'TOutputConfigurationValidator.PopulateDebugEndMonthCbx';
var
  lMonth      : integer;
  lConfigData : IRunConfigurationData;
  lStartMonth : integer;
  lIndex      : integer;
  lNrOfYears  : integer;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    lStartMonth := FAppModules.StudyArea.CalendarStartMonth;
    lNrOfYears  := lConfigData.YearsInAnalysis;
    with OutputConfigurationDialog do
    begin
      DebugEndMonthCbx.Items.Clear;
      if (FAppModules.Model.ModelName = CYield) then
      begin
        for lMonth := 1 to 12 do
        begin
          if (((DebugEndYearCbx.ItemIndex = 0) AND (lMonth >= lStartMonth)) OR
              ((DebugEndYearCbx.ItemIndex = lNrOfYears) AND (lMonth < lStartMonth)) OR
              ((DebugEndYearCbx.ItemIndex > 0) AND (DebugEndYearCbx.ItemIndex < lNrOfYears))) then
          begin
            lIndex := ConvertToIndex(lMonth);
            DebugEndMonthCbx.Items.Add(lConfigData.MonthNameByIndex[lIndex]);
          end;
        end;
      end
      else if (FAppModules.Model.ModelName = CPlanning) then
      begin
        for lMonth := 1 to 12 do
        begin
          lIndex := ConvertToIndex(lMonth);
          DebugEndMonthCbx.Items.Add(lConfigData.MonthNameByIndex[lIndex]);
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.ConvertToMonth(AIndex: integer): integer;
const OPNAME = 'TOutputConfigurationValidator.ConvertToMonth';
var
  lStartMonth : integer;
begin
  Result := 0;
  try
    lStartMonth := FAppModules.StudyArea.CalendarStartMonth;
    Result := ((AIndex + (lStartMonth - 2)) mod 12) + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.DisplayDebugEndDate;
const OPNAME = 'TOutputConfigurationValidator.DisplayDebugEndDate';
var
  lPeriod        : integer;
  lYear          : integer;
  lMonth         : integer;
  lIndex         : integer;
  lMonthName     : string;
  lFieldIndex    : string;
  lKeyValues     : string;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    with OutputConfigurationDialog do
    begin
      lPeriod  := lConfigdata.EndDebugPeriod;
      lYear    := ((lPeriod-1) div 12) + lConfigData.StartYearOther;
      lMonth   := ((lPeriod-1) mod 12) + 1;
      lIndex   := ConvertToMonth(lMonth);
      if (lIndex < FAppModules.StudyArea.CalendarStartMonth) then
        lYear := lYear + 1;
      DebugEndYearCbx.ItemIndex := DebugEndYearCbx.Items.IndexOf(IntToStr(lYear));
      PopulateDebugEndMonthCbx;
      lMonthName := lConfigData.MonthNameByIndex[lMonth];
      lFieldProperty := DebugEndPeriodEdit.FieldProperty;
      lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
      DebugEndPeriodEdit.HasMetaData :=
        FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
      DebugEndPeriodEdit.SetFieldValue(lPeriod);
      DebugEndMonthCbx.ItemIndex := DebugEndMonthCbx.Items.IndexOf(lMonthName);
      DebugEndPeriodEdit.SetFieldValue(lPeriod);
      DoContextValidation(dvtDebugEndPeriod);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputConfigurationValidator.DisplayDebugStartDate;
const OPNAME = 'TOutputConfigurationValidator.DisplayDebugStartDate';
var
  lPeriod        : integer;
  lYear          : integer;
  lMonth         : integer;
  lIndex         : integer;
  lMonthName     : string;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFieldProperty : TAbstractFieldProperty;
  lConfigData    : IRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).
                     RunConfigurationData;
    with OutputConfigurationDialog do
    begin
      lPeriod  := lConfigdata.StartDebugPeriod;
      lYear    := ((lPeriod-1) div 12) + lConfigData.StartYearOther;
      lMonth   := ((lPeriod-1) mod 12) + 1;
      lIndex   := ConvertToMonth(lMonth);
      if (lIndex < FAppModules.StudyArea.CalendarStartMonth) then
        lYear := lYear + 1;
      DebugStartYearCbx.ItemIndex := DebugStartYearCbx.Items.IndexOf(IntToStr(lYear));
      PopulateDebugStartMonthCbx;
      lMonthName := lConfigData.MonthNameByIndex[lMonth];
      DebugStartMonthCbx.ItemIndex := DebugStartMonthCbx.Items.IndexOf(lMonthName);

      lFieldIndex := '';
      lFieldProperty := DebugStartPeriodEdit.FieldProperty;
      if (lFieldProperty <> nil) then
      begin
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        DebugStartPeriodEdit.HasMetaData :=
           FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
      end;
      DebugStartPeriodEdit.SetFieldValue(lPeriod);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.PopulateDebugYearControls;
const OPNAME = 'TOutputConfigurationValidator.PopulateDebugYearControls';
var
  lIndex         : integer;
  lStartYear     : integer;
  lNrOfYears     : integer;
  lStartMonth    : integer;
  lFieldIndex    : string;
  lKeyValues     : string;
  lConfigData    : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    with OutputConfigurationDialog do
    begin
      DebugStartYearCbx.Items.Clear;
      DebugEndYearCbx.Items.Clear;
      lStartYear  := lConfigData.StartYearOther;
      lStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      lNrOfYears  := lConfigData.YearsInAnalysis;
      for lIndex := 0 to lNrOfYears do
      begin
        if ((lIndex > 0) OR (lStartMonth > 1)) then
        begin
          DebugStartYearCbx.Items.Add(IntToStr(lStartYear + lIndex));
          DebugEndYearCbx.Items.Add(IntToStr(lStartYear + lIndex));

          lFieldProperty := DebugStartYearCbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            DebugStartYearCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          end;

          lFieldProperty := DebugEndYearCbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            DebugEndYearCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          end;

          lFieldProperty := DebugStartMonthCbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            DebugStartMonthCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          end;

          lFieldProperty := DebugEndMonthCbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            DebugEndMonthCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          end;
        end;
      end;

      if (FAppModules.Model.ModelName = CPlanning) then
      begin
        while((DebugStartYearCbx.Items[DebugStartYearCbx.Items.Count - 1] <> '2040') AND
             (DebugEndYearCbx.Items[DebugEndYearCbx.Items.Count - 1] <> '2040')) do
        begin
          lStartYear := StrToInt(DebugStartYearCbx.Items[DebugStartYearCbx.Items.Count - 1]) + 1;
          DebugStartYearCbx.Items.Add(IntToStr(lStartYear));
          DebugEndYearCbx.Items.Add(IntToStr(lStartYear));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.PopulateDebugLevelCbx;
const OPNAME = 'TOutputConfigurationValidator.PopulateDebugLevelCbx';
var
  lIndex         : integer;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFieldProperty : TAbstractFieldProperty;
  lConfigData    : IRunConfigurationData;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      DebugLevelCbx.Items.Clear;
      for lIndex := -3 to 7 do
      begin
        DebugLevelCbx.Items.Add(IntToStr(lIndex));
      end;

      if (lConfigData <> nil) then
      begin
        lFieldProperty := DebugLevelCbx.FieldProperty;
        if (lFieldProperty <> nil) then
        begin
          lFieldProperty := DebugLevelCbx.FieldProperty;
          lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
          DebugLevelCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          DebugLevelCbx.HasChanges  :=
            FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputConfigurationValidator.UpdateDebugLevel;
const OPNAME = 'TOutputConfigurationValidator.UpdateDebugLevel';
var
  lConfigData : IRunConfigurationData;
  lLevel      : integer;
  lMessage    : string;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lLevel := DebugLevelCbx.ItemIndex - 3;
        if (FAppModules.FieldProperties.ValidateFieldProperty('DebugLevel',
            IntToStr(lLevel), lMessage)) then
        begin
          lConfigData.DebugLevel := lLevel;
          DebugLevelCbx.ItemIndex := lConfigdata.DebugLevel + 3;
          DoContextValidation(dvtDebugLevel);
        end
        else
          DebugLevelCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputConfigurationValidator.UpdateDebugStartPeriod;
const OPNAME = 'TOutputConfigurationValidator.UpdateDebugStartPeriod';
var
  lConfigData : IRunConfigurationData;
  lPeriod     : integer;
  lMessage    : string;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('debugInit',
           DebugStartPeriodEdit.Text, lMessage))then
        begin
          if (lConfigdata.DebugLevel > -3) then
          begin
            lPeriod := 0;
            if (Trim(DebugStartPeriodEdit.Text) <> '') then
              lPeriod := StrToInt(Trim(DebugStartPeriodEdit.Text));
//            if ((lPeriod > 0) AND (lPeriod <= lConfigData.YearsInAnalysis * 12)) then
//            begin
              DebugStartPeriodEdit.FieldValidationError := '';
              lConfigData.StartDebugPeriod := lPeriod;
              DisplayDebugStartDate;
              DoContextValidation(dvtDebugStartPeriod);
              DoContextValidation(dvtDebugEndPeriod);
{            end
            else
            begin
              lMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
              lMessage := Format(lMessage,[FAppModules.Language.GetString('TField.DebugInitDescr'),
                        1, lConfigData.YearsInAnalysis * 12]);
              DebugStartPeriodEdit.FieldValidationError := lMessage;
            end;}
          end;
        end
        else
          DebugStartPeriodEdit.FieldValidationError := lMessage;
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputConfigurationValidator.UpdateDebugEndDate;
const OPNAME = 'TOutputConfigurationValidator.UpdateDebugEndDate';
var
  lConfigData : IRunConfigurationData;
  lPeriod     : integer;
  lMessage    : string;
begin
  try
    with OutputConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('DebugFinal',
            DebugEndPeriodEdit.Text, lMessage)) then
        begin
          if (lConfigdata.DebugLevel > -3) then
          begin
            lPeriod := 0;
            if (Trim(DebugEndPeriodEdit.Text) <> '') then
              lPeriod := StrToInt(Trim(DebugEndPeriodEdit.Text));
//            if ((lPeriod >= lConfigData.StartDebugPeriod) AND
//                (lPeriod <= lConfigData.YearsInAnalysis * 12)) then
//            begin
              DebugEndPeriodEdit.FieldValidationError := '';
              lConfigData.EndDebugPeriod := lPeriod;
              DisplayDebugEndDate;
             // DoContextValidation(dvtDebugEndPeriod);
             // DoContextValidation(dvtDebugStartPeriod);
{            end
            else
            begin
              lMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
              lMessage := Format(lMessage,[FAppModules.Language.GetString('TField.DebugFinalDescr'),
                          lConfigData.StartDebugPeriod,
                          lConfigData.YearsInAnalysis * 12]);
              DebugEndPeriodEdit.FieldValidationError := lMessage;
            end;}
          end;
        end
        else
          DebugEndPeriodEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnDebugLevelChange(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnDebugLevelChange';
var
  lEnabled : Boolean;
begin
  try
    with OutputConfigurationDialog do
    begin
      lEnabled := DebugLevelCbx.ItemIndex > 0;
      DebugStartYearCbx.IsEnabled    := lEnabled;
      DebugStartMonthCbx.IsEnabled   := lEnabled;
      DebugStartPeriodEdit.IsEnabled := lEnabled;
      DebugEndYearCbx.IsEnabled      := lEnabled;
      DebugEndMonthCbx.IsEnabled     := lEnabled;
      DebugEndPeriodEdit.IsEnabled   := lEnabled;
      DoContextValidation(dvtDebugLevel);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.ValidateDebugLevel(AConfiguration: IRunConfigurationData);
const OPNAME = 'TOutputConfigurationValidator.ValidateDebugLevel';
begin
  try
    with OutputConfigurationDialog do
    begin
      FErrorMessage := '';
      if AConfiguration.Validate(FErrorMessage,'DebugLevel') then
        DebugLevelCbx.InValidationError := FALSE
      else
      begin
        DebugLevelCbx.InValidationError := TRUE;
        DebugLevelCbx.ValidationError := FErrorMessage;
        DebugLevelCbx.ShowErrorState(TRUE);
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TOutputConfigurationValidator.ValidateDebugStartPeriod(AConfiguration: IRunConfigurationData);
const OPNAME = 'TOutputConfigurationValidator.ValidateDebugStartPeriod';
begin
  try
    with OutputConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'DebugStartPeriod');
      DebugStartPeriodEdit.ContextValidationError := FErrorMessage;
      DebugEndPeriodEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;

procedure TOutputConfigurationValidator.ValidateDebugEndPeriod(AConfiguration: IRunConfigurationData);
const OPNAME = 'TOutputConfigurationValidator.ValidateDebugEndPeriod';
begin
  try
    with OutputConfigurationDialog do
    begin
      FErrorMessage := '';
      AConfiguration.Validate(FErrorMessage,'DebugEndPeriod');
      DebugEndPeriodEdit.ContextValidationError := FErrorMessage;
      DebugStartPeriodEdit.ContextValidationError := FErrorMessage;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;


procedure TOutputConfigurationValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TOutputConfigurationValidator.DoContextValidation';
var
  LConfiguration : IRunConfigurationData;
begin
  try
    LConfiguration := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (LConfiguration <> nil) then
    begin
      if (AValidationType in [dvtConfigurationAll,dvtDebugStartPeriod]) then
        ValidateDebugStartPeriod(LConfiguration);
      if (AValidationType in [dvtConfigurationAll,dvtDebugEndPeriod]) then
        ValidateDebugEndPeriod(LConfiguration);
      if (AValidationType in [dvtConfigurationAll,dvtDebugLevel]) then
        ValidateDebugLevel(LConfiguration);
      end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TOutputConfigurationValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TOutputConfigurationValidator.ProcessMetaDataEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lFeature       : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if ((FActiveControl <> nil) AND (FPanel.Visible)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lFeature <> nil ) then
      begin
        with OutputConfigurationDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = InputDataChkBox) then
            lFieldProperty   := InputDataChkBox.FieldProperty
          else
          if (FActiveControl = SummaryLevelRadioGroup) then
            lFieldProperty := SummaryLevelRadioGroup.FieldProperty
          else
          if (FActiveControl = YieldResultsChkBox) then
            lFieldProperty := YieldResultsChkBox.FieldProperty
          else
          if (FActiveControl = PlotFileChkBox) then
            lFieldProperty := PlotFileChkBox.FieldProperty
          else
          if (FActiveControl = DebugLevelCbx) then
            lFieldProperty := DebugLevelCbx.FieldProperty
          else
          if (FActiveControl = DebugStartYearCbx) then
            lFieldProperty := DebugStartYearCbx.FieldProperty
          else
          if (FActiveControl = DebugStartMonthCbx) then
            lFieldProperty := DebugStartMonthCbx.FieldProperty
          else
          if (FActiveControl = DebugEndYearCbx) then
            lFieldProperty := DebugEndYearCbx.FieldProperty
          else
          if  (FActiveControl = DebugEndMonthCbx) then
             lFieldProperty := DebugEndMonthCbx.FieldProperty
          else
          if (FActiveControl = DebugStartPeriodEdit) then
             lFieldProperty := DebugStartPeriodEdit.FieldProperty
          else
          if (FActiveControl = DebugEndPeriodEdit) then
            lFieldProperty := DebugEndPeriodEdit.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end
        end;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputConfigurationValidator.DisableModelControls;
const OPNAME = 'TOutputConfigurationValidator.DisableModelControls';
begin
  try
    if (FAppModules.Model.ModelName = CYield) then
    begin
      OutputConfigurationDialog.AnnualSummaryRadioGroup.Visible   := False;
      OutputConfigurationDialog.ControlParametersGroupBox.Visible := False;
    end
    else if (FAppModules.Model.ModelName = CPlanning) then
    begin
      OutputConfigurationDialog.AnnualSummaryRadioGroup.Visible   := True;
      OutputConfigurationDialog.ControlParametersGroupBox.Visible := True;
      OutputConfigurationDialog.SummaryLevelRadioGroup.Enabled    := False;
      OutputConfigurationDialog.InputDataChkBox.Enabled           := False;
      OutputConfigurationDialog.YieldResultsChkBox.Enabled        := False;
      OutputConfigurationDialog.PlotFileChkBox.Enabled            := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputConfigurationValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TOutputConfigurationValidator.ProcessParameterChangeEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lFeature       : IRunConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if ((FActiveControl <> nil) AND (FPanel.Visible)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lFeature <> nil ) then
      begin
        with OutputConfigurationDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = InputDataChkBox) then
            lFieldProperty   := InputDataChkBox.FieldProperty
          else
          if (FActiveControl = SummaryLevelRadioGroup) then
            lFieldProperty := SummaryLevelRadioGroup.FieldProperty
          else
          if (FActiveControl = YieldResultsChkBox) then
            lFieldProperty := YieldResultsChkBox.FieldProperty
          else
          if (FActiveControl = DetailedOptionChkBox) then
            lFieldProperty := DetailedOptionChkBox.FieldProperty
          else
          if (FActiveControl = AnnualSummaryRadioGroup) then
            lFieldProperty := AnnualSummaryRadioGroup.FieldProperty
          else
          if (FActiveControl = SupplyOptionChkBox) then
            lFieldProperty := SupplyOptionChkBox.FieldProperty
          else
          if (FActiveControl = EconomicOptionChkBox) then
            lFieldProperty := EconomicOptionChkBox.FieldProperty
          else
          if (FActiveControl = PlanningSummaryChkBox) then
            lFieldProperty := PlanningSummaryChkBox.FieldProperty
          else
          if (FActiveControl = InputSummaryChkBox) then
            lFieldProperty := InputSummaryChkBox.FieldProperty
          else
          if (FActiveControl = PlotFileChkBox) then
            lFieldProperty := PlotFileChkBox.FieldProperty
          else
          if (FActiveControl = DebugLevelCbx) then
            lFieldProperty := DebugLevelCbx.FieldProperty
          else
          if (FActiveControl = CreatePlotFileChkBox) then
            lFieldProperty := CreatePlotFileChkBox.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.Changes.ShowParameterChanges
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            FAppModules.Changes.SetParameterChanges(TRUE);
            Result := TRUE;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputConfigurationValidator.OnAnnualSummaryChange(Sender: TObject);
const OPNAME = 'TOutputConfigurationValidator.OnAnnualSummaryChange';
begin
  try
    UpdateAnnualSummary;
    OnEditControlEnter(Sender);
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

