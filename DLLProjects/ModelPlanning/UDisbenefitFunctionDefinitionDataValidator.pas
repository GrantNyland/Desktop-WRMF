{******************************************************************************}
{*  UNIT      : Contains the class TDisbenefitFunctionDefinitionValidator.    *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/06/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UDisbenefitFunctionDefinitionDataValidator;

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
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UDisbenefitFunctionDefinitionDataDialog;

type

  TDisbenefitFunctionDefinitionDataValidator = class(TAbstractYieldDataDialogValidator)
  private

  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure RePopulateDataViewer;
    procedure PopulateDateComboBoxes;
    procedure PopulateDateValues(ADisbenefitFunction : IDisbenefitFunctionDefinition);

    procedure UpdateNrOfEconomicYears;
    procedure UpdateEquationFunctionX;
    procedure UpdateEquationFunctionY;
    procedure UpdateEquationFunctionNonSupply;
    procedure UpdateEquationFunctionCostY;
    procedure UpdateEscalationRate(AIndex: integer;AValue : string);

    procedure UpdateYearActive;
    procedure UpdateMonthActive;
    procedure UpdateYearAbsolete;
    procedure UpdateMonthAbsolete;
    procedure UpdateWQConstraint;
    procedure UpdateTDSConcentration(AIndex: integer;AValue : string);


    procedure ValidateNrOfEconomicYears(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateEquationFunctionX(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateEquationFunctionY(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateEquationFunctionNonSupply(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateEquationFunctionCostY(ADisbenefit : IDisbenefitFunctionDefinition);

    procedure ValidateYearDemandChannelActive(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateMonthDemandChannelActive(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateYearDemandChannelObsolete(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateMonthDemandChannelObsolete(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateWaterQualityConstraint(ADisbenefit : IDisbenefitFunctionDefinition);
    procedure ValidateTDSConcentration(ADisbenefit: IDisbenefitFunctionDefinition);


  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;

    function DisbenefitFunctionDefinitionDialog: TDisbenefitFunctionDefinitionDataDialog;
  end;

implementation

uses
  VCL.Dialogs,
  SysUtils,
  VCL.Graphics,
  Contnrs,
  UFileNames,
  UUtilities,
  UConstants,
  UAbstractFileNamesObject,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  UParameterData;

{******************************************************************************}
{* TDisbenefitFunctionDefinitionDataValidator                                 *}
{******************************************************************************}

procedure TDisbenefitFunctionDefinitionDataValidator.CreateMemberObjects;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.CreateMemberObjects';
var
  lpPanel : TDisbenefitFunctionDefinitionDataDialog;
begin
  try
    inherited CreateMemberObjects;
    Identifier   := -1;
    FPanel       := TDisbenefitFunctionDefinitionDataDialog.Create(FPanelOwner,FAppModules);
    lpPanel      := DisbenefitFunctionDefinitionDialog;
    with lpPanel do
    begin
      NrOfEconomicVariablesEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('NrOfEconomicVariableYears');
      NrOfEconomicVariablesEdit.OnEnter        := OnEditControlEnter;
      NrOfEconomicVariablesEdit.OnExit         := OnEditControltExit;

      EquationFunctionXEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('EquationFunctionX');
      EquationFunctionXEdit.OnEnter        := OnEditControlEnter;
      EquationFunctionXEdit.OnExit         := OnEditControltExit;

      EquationFunctionYEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('EquationFunctionY');
      EquationFunctionYEdit.OnEnter        := OnEditControlEnter;
      EquationFunctionYEdit.OnExit         := OnEditControltExit;

      EquationFunctionNonSupplyEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('EquationFunctionNonSupply');
      EquationFunctionNonSupplyEdit.OnEnter           := OnEditControlEnter;
      EquationFunctionNonSupplyEdit.OnExit            := OnEditControltExit;

      EquationFunctionCostYEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('EquationFunctionCostY');
      EquationFunctionCostYEdit.OnEnter        := OnEditControlEnter;
      EquationFunctionCostYEdit.OnExit         := OnEditControltExit;

      GrdEscalationRate.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisbenefitEscalationRate'));
      GrdEscalationRate.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdEscalationRate.OnColEnter         := OnStringGridColEnter;


      CbxYearActive.FieldProperty  := FAppModules.FieldProperties.FieldProperty('YearDemandChannelActive');
      CbxYearActive.OnEnter        := OnEditControlEnter;
      CbxYearActive.OnChange         := OnEditControltExit;

      CbxMonthActive.FieldProperty  := FAppModules.FieldProperties.FieldProperty('MonthDemandChannelActive');
      CbxMonthActive.OnEnter        := OnEditControlEnter;
      CbxMonthActive.OnChange         := OnEditControltExit;

      CbxYearObsolete.FieldProperty  := FAppModules.FieldProperties.FieldProperty('YearDemandChannelObsolete');
      CbxYearObsolete.OnEnter        := OnEditControlEnter;
      CbxYearObsolete.OnChange         := OnEditControltExit;

      CbxMonthObsolete.FieldProperty  := FAppModules.FieldProperties.FieldProperty('MonthDemandChannelObsolete');
      CbxMonthObsolete.OnEnter        := OnEditControlEnter;
      CbxMonthObsolete.OnChange         := OnEditControltExit;

      WQConstraintEdit.FieldProperty  :=  FAppModules.FieldProperties.FieldProperty('WaterQualityConstraint');
      WQConstraintEdit.OnEnter        := OnEditControlEnter;
      WQConstraintEdit.OnExit         := OnEditControltExit;




      TDSConcentrationGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      TDSConcentrationGrid.OnColEnter         := OnStringGridColEnter;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.DestroyMemberObjects;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataValidator.Initialise: boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.DefinitionDataDialog');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ClearDataViewer;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      for LIndex := 1 to GrdEscalationRate.RowCount - 1 do
        GrdEscalationRate.Rows[LIndex].Clear;

      for lIndex := 0 to 3 do
        TDSConcentrationGrid.Cells[LIndex, 0] := '0.00';

    end;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.PopulateDataViewer;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    PopulateDateComboBoxes;
    RePopulateDataViewer;
    DoContextValidation(dvtSpecifiedDemandFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.PopulateDateComboBoxes;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.PopulateDateComboBoxes';
var
  lIndex : integer;
  lYear  : word;
  lMonth : word;
  lDay   : word;
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      CbxYearActive.Clear;
      CbxMonthActive.Clear;
      CbxYearObsolete.Clear;
      CbxMonthObsolete.Clear;
      DecodeDate(Now, lYear, lMonth, lDay);
      for lIndex := 1900 to lYear + 1000 do
      begin
        CbxYearActive.Items.Add(IntToStr(lIndex));
        CbxYearObsolete.Items.Add(IntToStr(lIndex));
      end;
      for lIndex := 1 to 12 do
      begin
        CbxMonthActive.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
        CbxMonthObsolete.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.PopulateDateValues(ADisbenefitFunction : IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.PopulateDateValues';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth,
  LEndYear,
  LEndMonth : integer;
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      LStartYear          := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ADisbenefitFunction.YearActive,ADisbenefitFunction.MonthActive);
      LStartMonth         := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ADisbenefitFunction.YearActive,ADisbenefitFunction.MonthActive);
      LEndYear            := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ADisbenefitFunction.YearObsolete,ADisbenefitFunction.MonthObsolete);
      LEndMonth           := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ADisbenefitFunction.YearObsolete,ADisbenefitFunction.MonthObsolete);

      CbxYearActive.SetFieldIndex(CbxYearActive.Items.IndexOf(IntToStr(LStartYear)));
      CbxMonthActive.SetFieldIndex(LStartMonth-1);
      CbxYearObsolete.SetFieldIndex(CbxYearObsolete.Items.IndexOf(IntToStr(LEndYear)));
      CbxMonthObsolete.SetFieldIndex(LEndMonth-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.RePopulateDataViewer;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.RePopulateDataViewer';
var
  lIndex              : integer;
  lNrOfEconomicYears  : integer;
  lFieldProperty1      : TAbstractFieldProperty;
  lFieldProperty2     : TAbstractFieldProperty;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
begin
  try
    if (Identifier >= 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[Identifier];                 //ChannelByIdentifier[FFeatureID];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        with DisbenefitFunctionDefinitionDialog do
        begin
          lNrOfEconomicYears := lDisbenefitFunction.NrOfEconomicYears;
          NrOfEconomicVariablesEdit.SetFieldValue(lNrOfEconomicYears);
          EquationFunctionXEdit.SetFieldValue(FloatToStr(lDisbenefitFunction.EquationDisbenefitX));
          EquationFunctionYEdit.SetFieldValue(FloatToStr(lDisbenefitFunction.EquationDisbenefitY));
          EquationFunctionNonSupplyEdit.SetFieldValue(FloatToStr(lDisbenefitFunction.EquationDisbenefitNonSupply));
          EquationFunctionCostYEdit.SetFieldValue(FloatToStr(lDisbenefitFunction.EquationDisbenefitCost));
          EquationFunctionNonSupplyEdit.SetFieldValue(FloatToStr(lDisbenefitFunction.EquationDisbenefitNonSupply));
          WQConstraintEdit.SetFieldValue(FloatToStr(LDisbenefitFunction.WQConstraint));

          PopulateDateValues(lDisbenefitFunction);

          if (lNrOfEconomicYears = 0) then
            GrdEscalationRate.Rows[1].Clear;
          GrdEscalationRate.ColCount := lNrOfEconomicYears;
          GrdEscalationRate.Width := 3 + (1 + GrdEscalationRate.DefaultColWidth) * GrdEscalationRate.ColCount;
          lFieldProperty1 := FAppModules.FieldProperties.FieldProperty('DisbenefitEscalationRate');
          lFieldProperty2 := FAppModules.FieldProperties.FieldProperty('ChannelEscalationCostValue');
          for lIndex := 0 to lNrOfEconomicYears-1 do
          begin
            GrdEscalationRate.Cells[lIndex, 0] := IntToStr(lIndex + 1);
            GrdEscalationRate.Cells[lIndex, 1] := Format( lFieldProperty2.FormatStringGrid {'%6.2f'}, [lDisbenefitFunction.EscalationRateByIndex[lIndex]]);
            GrdEscalationRate.AddFieldProperty(lFieldProperty1);
          end;
          TDSConcentrationGrid.ColCount := 4;
          TDSConcentrationGrid.Width    := 4 * (TDSConcentrationGrid.DefaultColWidth + 1) + 3;
          for LIndex := 0 to 3 do
          begin
            TDSConcentrationGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TDSConcentration'));
            TDSConcentrationGrid.Cells[LIndex,0] := FloatToStr(LDisbenefitFunction.Get_TDSConcentrationByIndex(LIndex+1));
          end;

        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateNrOfEconomicYears;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateNrOfEconomicYears';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                   .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
           'NrOfEconomicVariableYears', NrOfEconomicVariablesEdit.Text, lMessage)) then
        begin
          NrOfEconomicVariablesEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.NrOfEconomicYears := StrToInt(NrOfEconomicVariablesEdit.Text);
          NrOfEconomicVariablesEdit.SetFieldValue(lDisbenefitFunction.NrOfEconomicYears);
          DoContextValidation(dvtNrOfEconomonicYears);
          RePopulateDataViewer;
        end
        else
          NrOfEconomicVariablesEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataValidator.SaveState: boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataValidator.DisbenefitFunctionDefinitionDialog : TDisbenefitFunctionDefinitionDataDialog;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.DisbenefitFunctionDefinitionDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TDisbenefitFunctionDefinitionDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDisbenefitFunctionDefinitionDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      if ((Sender = NrOfEconomicVariablesEdit) AND (NrOfEconomicVariablesEdit.HasValueChanged)) then
        UpdateNrOfEconomicYears
      else if ((Sender = EquationFunctionXEdit) AND (EquationFunctionXEdit.HasValueChanged)) then
        UpdateEquationFunctionX
      else if ((Sender = EquationFunctionYEdit) AND (EquationFunctionYEdit.HasValueChanged)) then
        UpdateEquationFunctionY
      else if ((Sender = EquationFunctionNonSupplyEdit) AND (EquationFunctionNonSupplyEdit.HasValueChanged)) then
        UpdateEquationFunctionNonSupply
      else if ((Sender = EquationFunctionCostYEdit) AND (EquationFunctionCostYEdit.HasValueChanged)) then
        UpdateEquationFunctionCostY
      else if ((Sender = CbxYearActive) AND (CbxYearActive.HasValueChanged)) then
        UpdateYearActive
      else if ((Sender = CbxMonthActive) AND (CbxMonthActive.HasValueChanged)) then
        UpdateMonthActive
      else if ((Sender = CbxYearObsolete) AND (CbxYearObsolete.HasValueChanged)) then
        UpdateYearAbsolete
      else if ((Sender = CbxMonthObsolete) AND (CbxMonthObsolete.HasValueChanged)) then
        UpdateMonthAbsolete
      else if ((Sender = WQConstraintEdit) AND (WQConstraintEdit.HasValueChanged)) then
        UpdateWQConstraint
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionX;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionX';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'EquationFunctionX', EquationFunctionXEdit.Text, lMessage)) then
        begin
          EquationFunctionXEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.EquationDisbenefitX := StrToFloat(EquationFunctionXEdit.Text);
          DoContextValidation(dvtEquationFunctionX);
          RePopulateDataViewer;
        end
        else
          EquationFunctionXEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionY;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionY';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
           'EquationFunctionY', EquationFunctionYEdit.Text, lMessage)) then
        begin
          EquationFunctionYEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.EquationDisbenefitY := StrToFloat(EquationFunctionYEdit.Text);
          DoContextValidation(dvtEquationFunctionY);
          RePopulateDataViewer;
        end
        else
          EquationFunctionYEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionNonSupply;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionNonSupply';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'EquationFunctionNonSupply', EquationFunctionNonSupplyEdit.Text, lMessage)) then
        begin
          EquationFunctionNonSupplyEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.EquationDisbenefitNonSupply := StrToFloat(EquationFunctionNonSupplyEdit.Text);
          DoContextValidation(dvtEquationFunctionNonSupply);
          RePopulateDataViewer;
        end
        else
          EquationFunctionNonSupplyEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionCostY;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateEquationFunctionCostY';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'EquationFunctionCostY', EquationFunctionCostYEdit.Text, lMessage)) then
        begin
          EquationFunctionCostYEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.EquationDisbenefitCost := StrToFloat(EquationFunctionCostYEdit.Text);
          DoContextValidation(dvtEquationFunctionCostY);
          RePopulateDataViewer;
        end
        else
          EquationFunctionCostYEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateYearActive;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateYearActive';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'YearDemandChannelActive', CbxYearActive.Text, lMessage)) then
        begin
          CbxYearActive.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearActive.Text);
          LCalendarMonth         := CbxMonthActive.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lDisbenefitFunction.YearActive := LHydroYear;
          if(lDisbenefitFunction.MonthActive <> LHydroMonth) then
            lDisbenefitFunction.MonthActive := LHydroMonth;

          DoContextValidation(dvtYearActive);
          PopulateDateValues(lDisbenefitFunction);
        end
        else
          CbxYearActive.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TDisbenefitFunctionDefinitionDataValidator.UpdateMonthActive;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateMonthActive';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MonthDemandChannelActive', IntToStr(CbxMonthActive.ItemIndex+1), lMessage)) then
        begin
          CbxMonthActive.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearActive.Text);
          LCalendarMonth         := CbxMonthActive.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lDisbenefitFunction.MonthActive := LHydroMonth;
          if(lDisbenefitFunction.YearActive <> LHydroYear) then
            lDisbenefitFunction.YearActive := LHydroYear;

          PopulateDateValues(lDisbenefitFunction);
          DoContextValidation(dvtMonthActive);
        end
        else
          CbxMonthActive.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateYearAbsolete;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateYearAbsolete';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'YearDemandChannelObsolete', CbxYearObsolete.Text, lMessage)) then
        begin
          CbxYearObsolete.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearObsolete.Text);
          LCalendarMonth         := CbxMonthObsolete.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lDisbenefitFunction.YearObsolete := LHydroYear;
          if(lDisbenefitFunction.MonthObsolete <> LHydroMonth) then
            lDisbenefitFunction.MonthObsolete := LHydroMonth;

          PopulateDateValues(lDisbenefitFunction);
          DoContextValidation(dvtYearObsolete);
        end
        else
          CbxYearObsolete.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateMonthAbsolete;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateMonthAbsolete';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MonthDemandChannelObsolete', IntToStr(CbxMonthObsolete.ItemIndex+1), lMessage)) then
        begin
          CbxMonthObsolete.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearObsolete.Text);
          LCalendarMonth         := CbxMonthObsolete.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lDisbenefitFunction.MonthObsolete := LHydroMonth;
          if(lDisbenefitFunction.YearObsolete <> LHydroYear) then
            lDisbenefitFunction.YearObsolete := LHydroYear;

          PopulateDateValues(lDisbenefitFunction);
          DoContextValidation(dvtMonthObsolete);
        end
        else
          CbxMonthObsolete.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TDisbenefitFunctionDefinitionDataValidator.UpdateWQConstraint;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateWQConstraint';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
  lMessage            : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with DisbenefitFunctionDefinitionDialog do
    begin
      lChannel   := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'WaterQualityConstraint', WQConstraintEdit.Text, lMessage)) then
        begin
          WQConstraintEdit.FieldValidationError := lMessage;
          lDisbenefitFunction.WQConstraint := StrToFloat(WQConstraintEdit.Text);
          DoContextValidation(dvtWQConstraint);
          RePopulateDataViewer;
        end
        else
          WQConstraintEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.UpdateTDSConcentration(AIndex: integer;AValue : string);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateTDSConcentration';
var
  lValue              : double;
  lStrValue           : string;
  lMessage            : string;
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
begin
  try
    if (DisbenefitFunctionDefinitionDialog.TDSConcentrationGrid.ColCount >= 1) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        with DisbenefitFunctionDefinitionDialog do
        begin
          TDSConcentrationGrid.ValidationError[AIndex, 0, gveCellContext] := '';
          lStrValue := Trim(AValue);
          if (lStrValue = '') then
            lStrValue := '0.0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'TDSConcentration', lStrValue,
                         lMessage, AIndex)) then
          begin
            lValue := StrToFloat(lStrValue);
            lDisbenefitFunction.TDSConcentrationByIndex[AIndex] := lValue;
            DoContextValidation(dvtTDSConcentration);
            RePopulateDataViewer;
          end
          else
            TDSConcentrationGrid.ValidationError[AIndex, 0, gveCellContext] := lMessage;
        end;
      end
      else
       DisbenefitFunctionDefinitionDialog.TDSConcentrationGrid.Cells[0, 0] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDisbenefitFunctionDefinitionDataValidator.UpdateEscalationRate(AIndex: integer;
                                                                          AValue : string );
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateEscalationRate';
var
  lValue              : double;
  lStrValue           : string;
  lMessage            : string;
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
begin
  try
    if (DisbenefitFunctionDefinitionDialog.GrdEscalationRate.ColCount >= 1) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) AND (lChannel.DisbenefitFunction <> nil) then
      begin
        lDisbenefitFunction := lChannel.DisbenefitFunction;
        with DisbenefitFunctionDefinitionDialog do
        begin
          GrdEscalationRate.ValidationError[AIndex, 0, gveCellContext] := '';
          lStrValue := Trim(AValue);
          if (lStrValue = '') then
            lStrValue := '0.0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DisbenefitEscalationRate', lStrValue,
                         lMessage, AIndex)) then
          begin
            lValue := StrToFloat(lStrValue);
            lDisbenefitFunction.EscalationRateByIndex[AIndex] := lValue;
            RePopulateDataViewer;
          end
          else
            GrdEscalationRate.ValidationError[AIndex, 0, gveCellContext] := lMessage;
        end;
      end
      else
       DisbenefitFunctionDefinitionDialog.GrdEscalationRate.Cells[0, 0] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.OnStringGridCellDataHasChanged;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      if (GrdEscalationRate = ASender) then
        UpdateEscalationRate(ACol, Trim(GrdEscalationRate.Cells[ACol, ARow]));

      if (TDSConcentrationGrid = ASender) then
        UpdateTDSConcentration(ACol+1 , Trim(TDSConcentrationGrid.Cells[ACol, ARow]));

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.DoContextValidation';
var
  lChannelList          : IChannelList;
  lChannel              : IGeneralFlowChannel;
  lDisbenefitDefinition : IDisbenefitFunctionDefinition;
begin
  try
    FAllErrorMessages.Clear;
    if (Identifier > 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
          NetworkElementData.ChannelList;
      lChannel := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin
        lDisbenefitDefinition := lChannel.DisbenefitFunction;
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtNrOfEconomonicYears)) then
          ValidateNrOfEconomicYears(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtEquationFunctionX)) then
          ValidateEquationFunctionX(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtEquationFunctionY)) then
          ValidateEquationFunctionY(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtEquationFunctionNonSupply)) then
          ValidateEquationFunctionNonSupply(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtEquationFunctionCostY)) then
          ValidateEquationFunctionCostY(lDisbenefitDefinition);

        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtYearActive)) then
          ValidateYearDemandChannelActive(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtMonthActive)) then
          ValidateMonthDemandChannelActive(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtYearObsolete)) then
          ValidateYearDemandChannelObsolete(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtMonthObsolete)) then
          ValidateMonthDemandChannelObsolete(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtWQConstraint)) then
          ValidateWaterQualityConstraint(lDisbenefitDefinition);
        if ((AValidationType = dvtYieldCharactersticsAll) OR (AValidationType = dvtTDSConcentration)) then
          ValidateTDSConcentration(lDisbenefitDefinition);


      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateNrOfEconomicYears(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateNrOfEconomicYears';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'NrOfEconomicYears')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfEconomicVariablesEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionCostY(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionCostY';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'EquationFunctionCostY')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
        EquationFunctionCostYEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionNonSupply(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionNonSupply';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'EquationFunctionNonSupply')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
        EquationFunctionNonSupplyEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionX(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionX';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'EquationFunctionX')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
        EquationFunctionXEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionY(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateEquationFunctionY';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'EquationFunctionY')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       EquationFunctionCostYEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateYearDemandChannelActive(ADisbenefit : IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateYearDemandChannelActive';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'YearDemandChannelActive')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      CbxYearActive.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateMonthDemandChannelActive(ADisbenefit : IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateMonthDemandChannelActive';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'MonthDemandChannelActive')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      CbxMonthActive.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateYearDemandChannelObsolete(ADisbenefit : IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateYearDemandChannelObsolete';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'YearDemandChannelObsolete')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       CbxYearObsolete.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateMonthDemandChannelObsolete(ADisbenefit : IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateMonthDemandChannelObsolete';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'MonthDemandChannelObsolete')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       CbxMonthObsolete.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateWaterQualityConstraint(ADisbenefit : IDisbenefitFunctionDefinition);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.ValidateWaterQualityConstraint';
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADisbenefit.Validate(FErrorMessage, 'WaterQualityConstraint')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       WQConstraintEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDisbenefitFunctionDefinitionDataValidator.ValidateTDSConcentration(ADisbenefit: IDisbenefitFunctionDefinition);
const OPNAME = 'TRunConfigurationValidator.ValidateTDSConcentration';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    with DisbenefitFunctionDefinitionDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (ADisbenefit.Validate(FErrorMessage,'TDSConcentration')) then
        begin
          for lCol := 0 to  3 do
            TDSConcentrationGrid.ValidationError[lCol, 0, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 0  to  3 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              TDSConcentrationGrid.ValidationError[lCol, 0, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              TDSConcentrationGrid.ValidationError[lCol, 0, gveCellContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E : exception do HandleError(E, OPNAME); end;
end;


end.

