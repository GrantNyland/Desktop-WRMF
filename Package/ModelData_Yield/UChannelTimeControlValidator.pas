{******************************************************************************}
{*  UNIT      : Contains the class TChannelTimeControlValidator.              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/03/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UChannelTimeControlValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,

  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChannelTimeControlDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TChannelTimeControlValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure UpdateStartYear;
    procedure UpdateStartMonth;
    procedure UpdateEndYear;
    procedure UpdateEndMonth;
    procedure UpdateEconomicLife;
    procedure UpdateCapitalCost;
    procedure UpdateFixedOMCost;
    procedure UpdateVariableOMCost;
    procedure UpdateNrYearsConstruct;
    procedure UpdateCostScheduleValue(AIndex : integer;
                                      AValue : string);
    procedure UpdateYearsInAnalysis;
    procedure UpdateEscalationCostValue(AIndex : integer;
                                         AValue : string);
    procedure RePopulateDataViewer;
    procedure PopulateDateComboBoxes;
    procedure PopulateDateValues(ATimeControl   : IChannelTimeControl);

    procedure ValidateStartYear (ATimeControl : IChannelTimeControl);
    procedure ValidateStartMonth (ATimeControl : IChannelTimeControl);
    procedure ValidateEndYear (ATimeControl : IChannelTimeControl);
    procedure ValidateEndMonth (ATimeControl : IChannelTimeControl);
    procedure ValidateEconomicLife (ATimeControl : IChannelTimeControl);
    procedure ValidateCapitalCost (ATimeControl : IChannelTimeControl);
    procedure ValidateFixedOMCost (ATimeControl : IChannelTimeControl);
    procedure ValidateVariableOMCost (ATimeControl : IChannelTimeControl);
    procedure ValidateNrYearsConstruct (ATimeControl : IChannelTimeControl);
    procedure ValidateCostSchedule (ATimeControl : IChannelTimeControl);
    procedure ValidateYearsInAnalysis (ATimeControl : IChannelTimeControl);
    procedure ValidateEscalationCost (ATimeControl : IChannelTimeControl);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ChannelTimeControlDialog : TChannelTimeControlDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;

  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UFileNames,
  UConstants,
  UAbstractFileNamesObject,
  UMonthlyDamLevelsObject,
  UReservoirPenaltyValidator,
  UYieldModelDataGUIForm,
  UReservoirPenaltyDialog,
  UReservoirPenaltyStructureData,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UUtilities,
  Math;

{******************************************************************************}
{* TChannelTimeControlValidator                                               *}
{******************************************************************************}

procedure TChannelTimeControlValidator.CreateMemberObjects;
const OPNAME = 'TChannelTimeControlValidator.CreateMemberObjects';
var
  lPanel : TChannelTimeControlDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    CreateDialog;
    lPanel := ChannelTimeControlDialog;
    with lPanel do
    begin
      CbxStartYear.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelStartYear');
      CbxStartYear.OnEnter       := OnEditControlEnter;
      CbxStartYear.OnChange      := OnEditControltExit;

      CbxStartMonth.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelStartMonth');
      CbxStartMonth.OnEnter       := OnEditControlEnter;
      CbxStartMonth.OnChange      := OnEditControltExit;

      CbxEndYear.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelEndYear');
      CbxEndYear.OnEnter       := OnEditControlEnter;
      CbxEndYear.OnChange      := OnEditControltExit;

      CbxEndMonth.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelEndMonth');
      CbxEndMonth.OnEnter       := OnEditControlEnter;
      CbxEndMonth.OnChange      := OnEditControltExit;

      EdtEconomicLife.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelEconomicLife');
      EdtEconomicLife.OnEnter       := OnEditControlEnter;
      EdtEconomicLife.OnExit        := OnEditControltExit;

      EdtCapitalCost.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelCapitalCost');
      EdtCapitalCost.OnEnter       := OnEditControlEnter;
      EdtCapitalCost.OnExit        := OnEditControltExit;

      EdtFixedOMCost.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelFixedOMCost');
      EdtFixedOMCost.OnEnter       := OnEditControlEnter;
      EdtFixedOMCost.OnExit        := OnEditControltExit;

      EdtVariableOMCost.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelVariableOMCost');
      EdtVariableOMCost.OnEnter       := OnEditControlEnter;
      EdtVariableOMCost.OnExit        := OnEditControltExit;

      EdtNrYearsConstruct.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelYearsToConstruct');
      EdtNrYearsConstruct.OnEnter       := OnEditControlEnter;
      EdtNrYearsConstruct.OnExit        := OnEditControltExit;

      GrdCostSchedule.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdCostSchedule.OnColEnter         := OnStringGridColEnter;

      EdtYearsInAnalysis.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelYearsInAnalysis');
      EdtYearsInAnalysis.OnEnter       := OnEditControlEnter;
      EdtYearsInAnalysis.OnExit        := OnEditControltExit;

      GrdEscalationCost.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdEscalationCost.OnColEnter         := OnStringGridColEnter;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.DestroyMemberObjects;
const OPNAME = 'TChannelTimeControlValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.CreateDialog;
const OPNAME = 'TChannelTimeControlValidator.CreateDialog';
begin
  try
    FPanel  := TChannelTimeControlDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlValidator.Initialise: boolean;
const OPNAME = 'TChannelTimeControlValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChannelTimeControlValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Time control';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ClearDataViewer;
const OPNAME = 'TChannelTimeControlValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with ChannelTimeControlDialog do
    begin
      EdtEconomicLife.SetFieldValue('');
      EdtCapitalCost.SetFieldValue('');
      EdtFixedOMCost.SetFieldValue('');
      EdtVariableOMCost.SetFieldValue('');
      EdtNrYearsConstruct.SetFieldValue('');
      GrdCostSchedule.Rows[1].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.PopulateDataViewer;
const OPNAME = 'TChannelTimeControlValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDateComboBoxes;
    RePopulateDataViewer;
    DoContextValidation(dvtChannelTimeControlAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.PopulateDateComboBoxes;
const OPNAME = 'TChannelTimeControlValidator.PopulateDateComboBoxes';
var
  lIndex : integer;
  lYear  : word;
  lMonth : word;
  lDay   : word;
begin
  try
    with ChannelTimeControlDialog do
    begin
      CbxStartYear.Clear;
      CbxStartMonth.Clear;
      CbxEndYear.Clear;
      CbxEndMonth.Clear;
      DecodeDate(Now, lYear, lMonth, lDay);
      for lIndex := 1900 to lYear + 1000 do
      begin
        CbxStartYear.Items.Add(IntToStr(lIndex));
        CbxEndYear.Items.Add(IntToStr(lIndex));
      end;
      for lIndex := 1 to 12 do
      begin
        CbxStartMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
        CbxEndMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.PopulateDateValues(ATimeControl: IChannelTimeControl);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.PopulateDateValues';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth,
  LEndYear,
  LEndMonth : integer;
begin
  try
    with ChannelTimeControlDialog do
    begin
      LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      LStartYear          := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ATimeControl.StartYear,ATimeControl.StartMonth);
      LStartMonth         := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ATimeControl.StartYear,ATimeControl.StartMonth);
      LEndYear            := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ATimeControl.EndYear,ATimeControl.EndMonth);
      LEndMonth           := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ATimeControl.EndYear,ATimeControl.EndMonth);

      CbxStartYear.SetFieldIndex(CbxStartYear.Items.IndexOf(IntToStr(LStartYear)));
      CbxStartMonth.SetFieldIndex(LStartMonth-1);
      CbxEndYear.SetFieldIndex(CbxEndYear.Items.IndexOf(IntToStr(LEndYear)));
      CbxEndMonth.SetFieldIndex(LEndMonth-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.RePopulateDataViewer;
const OPNAME = 'TChannelTimeControlValidator.RePopulateDataViewer';
var
  lChannel         : IGeneralFlowChannel;
  lTimeControl     : IChannelTimeControl;
  lNrOfYears       : integer;
  lYearsInAnalysis : integer;
  lIndex           : integer;
  lFieldProp       : TAbstractFieldProperty;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
      begin
        lTimeControl := lChannel.TimeControl;
        with ChannelTimeControlDialog do
        begin
          PopulateDateValues(lTimeControl);
          //CbxStartYear.SetFieldIndex(CbxStartYear.Items.IndexOf(IntToStr(lTimeControl.StartYear)));
          //CbxStartMonth.SetFieldIndex(lTimeControl.StartMonth - 1);
          //CbxEndYear.SetFieldIndex(CbxEndYear.Items.IndexOf(IntToStr(lTimeControl.EndYear)));
          //CbxEndMonth.SetFieldIndex(lTimeControl.EndMonth - 1);

          EdtEconomicLife.SetFieldValue(lTimeControl.EconomicLife);
          EdtCapitalCost.SetFieldValue(lTimeControl.CapitalCost);
          EdtFixedOMCost.SetFieldValue(lTimeControl.FixedOMCost);
          EdtVariableOMCost.SetFieldValue(lTimeControl.VariableOMCost);
          lNrOfYears := lTimeControl.YearsToConstruct;
          EdtNrYearsConstruct.SetFieldValue(lNrOfYears);

          if (lNrOfYears = 0) then
            GrdCostSchedule.Rows[1].Clear;
          GrdCostSchedule.ColCount := lNrOfYears;
          GrdCostSchedule.Width := 3 + (1 + GrdCostSchedule.DefaultColWidth) * GrdCostSchedule.ColCount;
          lFieldProp := FAppModules.FieldProperties.FieldProperty('ChannelCostScheduleValue');
          for lIndex := 0 to lNrOfYears-1 do
          begin
            GrdCostSchedule.Cells[lIndex, 0] := IntToStr(lIndex + 1);
            GrdCostSchedule.Cells[lIndex, 1] := Format(lFieldProp.FormatStringGrid, [lTimeControl.CostScheduleByIndex[lIndex]]);
            GrdCostSchedule.AddFieldProperty(lFieldProp);
          end;

          lYearsInAnalysis  := lTimeControl.YearsInAnalysis;
          EdtYearsInAnalysis.SetFieldValue(lYearsInAnalysis);

          if (lNrOfYears = 0 ) then
             GrdEscalationCost.Rows[1].Clear;
          GrdEscalationCost.ColCount := lYearsInAnalysis;
          GrdEscalationCost.Width := 3 + (1 + GrdEscalationCost.DefaultColWidth) * GrdEscalationCost.ColCount;
          lFieldProp := FAppModules.FieldProperties.FieldProperty('ChannelEscalationCostValue');
          for lIndex := 0 to lYearsInAnalysis-1 do
          begin
            GrdEscalationCost.Cells[lIndex, 0] := IntToStr(lIndex + 1);
            GrdEscalationCost.Cells[lIndex, 1] := Format(lFieldProp.FormatStringGrid, [lTimeControl.EscalationCostValueByIndex[lIndex]]);
            GrdEscalationCost.AddFieldProperty(lFieldProp);
          end;
          EdtYearsInAnalysis.SetFieldValue(lTimeControl.YearsInAnalysis);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlValidator.SaveState: boolean;
const OPNAME = 'TChannelTimeControlValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TChannelTimeControlValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TChannelTimeControlValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ChannelTimeControlDialog do
    begin
      if (Sender = EdtEconomicLife) AND (EdtEconomicLife.HasValueChanged) then
        UpdateEconomicLife
      else
      if (Sender = EdtCapitalCost) AND (EdtCapitalCost.HasValueChanged) then
        UpdateCapitalCost
      else
      if (Sender = EdtFixedOMCost) AND (EdtFixedOMCost.HasValueChanged) then
        UpdateFixedOMCost
      else
      if (Sender = EdtVariableOMCost) AND (EdtVariableOMCost.HasValueChanged) then
        UpdateVariableOMCost
      else
      if (Sender = EdtNrYearsConstruct) AND (EdtNrYearsConstruct.HasValueChanged) then
        UpdateNrYearsConstruct
      else
      if (Sender = CbxStartYear) AND (CbxStartYear.HasValueChanged) then
        UpdateStartYear
      else
      if (Sender = CbxStartMonth) AND (CbxStartMonth.HasValueChanged) then
        UpdateStartMonth
      else
      if (Sender = CbxEndYear) AND (CbxEndYear.HasValueChanged) then
        UpdateEndYear
      else
      if (Sender = CbxEndMonth) AND (CbxEndMonth.HasValueChanged) then
        UpdateEndMonth
      else
      if (Sender = EdtYearsInAnalysis) AND (EdtYearsInAnalysis.HasValueChanged) then
        UpdateYearsInAnalysis;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.OnStringGridCellDataHasChanged (ASender    : TObject;
                                                                       ACol, ARow : integer);
const OPNAME = 'TChannelTimeControlValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with ChannelTimeControlDialog do
    begin
      if (GrdCostSchedule = ASender) then
        UpdateCostScheduleValue(ACol, Trim(GrdCostSchedule.Cells[ACol, ARow]))
      else
      if (GrdEscalationCost = ASender) then
        UpdateEscalationCostValue(ACol, Trim(GrdEscalationCost.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlValidator.ChannelTimeControlDialog : TChannelTimeControlDialog;
const OPNAME = 'TChannelTimeControlValidator.ChannelTimeControlDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TChannelTimeControlDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): boolean;
const OPNAME = 'TChannelTimeControlValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'ChannelYearsToConstruct') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelTimeControlValidator.StudyHasChanged: boolean;
const OPNAME = 'TChannelTimeControlValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateStartYear;
const OPNAME = 'TChannelTimeControlValidator.UpdateStartYear';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        CbxStartYear.ValidationError := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxStartYear.FieldProperty.FieldName, Trim(CbxStartYear.Text), lMessage)) then
        begin
          CbxStartYear.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxStartYear.Text);
          LCalendarMonth         := CbxStartMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lTimeControl.StartYear := LHydroYear;
          if(lTimeControl.StartMonth <> LHydroMonth) then
            lTimeControl.StartMonth := LHydroMonth;

          DoContextValidation(dvtChannelStartYear);
          PopulateDateValues(lTimeControl);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateStartMonth;
const OPNAME = 'TChannelTimeControlValidator.UpdateStartMonth';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        CbxStartMonth.ValidationError := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxStartMonth.FieldProperty.FieldName, IntToStr(CbxStartMonth.ItemIndex + 1), lMessage)) then
        begin
          CbxStartMonth.ValidationError := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxStartYear.Text);
          LCalendarMonth         := CbxStartMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lTimeControl.StartMonth := LHydroMonth;
          if(lTimeControl.StartYear <> LHydroYear) then
            lTimeControl.StartYear := LHydroYear;

          DoContextValidation(dvtChannelStartMonth);
          PopulateDateValues(lTimeControl);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateEndYear;
const OPNAME = 'TChannelTimeControlValidator.UpdateEndYear';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        CbxEndYear.ValidationError := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxEndYear.FieldProperty.FieldName, Trim(CbxEndYear.Text), lMessage)) then
        begin
          CbxEndYear.ValidationError := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxEndYear.Text);
          LCalendarMonth         := CbxEndMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lTimeControl.EndYear := LHydroYear;
          if(lTimeControl.EndMonth <> LHydroMonth) then
            lTimeControl.EndMonth := LHydroMonth;

          DoContextValidation(dvtChannelEndYear);
          PopulateDateValues(lTimeControl);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateEndMonth;
const OPNAME = 'TChannelTimeControlValidator.UpdateEndMonth';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        CbxEndMonth.ValidationError := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxEndMonth.FieldProperty.FieldName, IntToStr(CbxEndMonth.ItemIndex + 1), lMessage)) then
        begin
          CbxEndMonth.ValidationError := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxEndYear.Text);
          LCalendarMonth         := CbxEndMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lTimeControl.EndMonth := LHydroMonth;
          if(lTimeControl.EndYear <> LHydroYear) then
            lTimeControl.EndYear := LHydroYear;

          PopulateDateValues(lTimeControl);
          DoContextValidation(dvtChannelEndMonth);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateEconomicLife;
const OPNAME = 'TChannelTimeControlValidator.UpdateEconomicLife';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtEconomicLife.FieldProperty.FieldName, Trim(EdtEconomicLife.Text), lMessage)) then
        begin
          EdtEconomicLife.FieldValidationError := lMessage;
          lTimeControl.EconomicLife := StrToInt(Trim(EdtEconomicLife.Text));
          EdtEconomicLife.SetFieldValue(lTimeControl.EconomicLife);
          DoContextValidation(dvtChannelEconomicLife);
        end
        else
          EdtEconomicLife.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateCapitalCost;
const OPNAME = 'TChannelTimeControlValidator.UpdateCapitalCost';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtCapitalCost.FieldProperty.FieldName, Trim(EdtCapitalCost.Text), lMessage)) then
        begin
          EdtCapitalCost.FieldValidationError := lMessage;
          lTimeControl.CapitalCost := StrToFloat(Trim(EdtCapitalCost.Text));
          EdtCapitalCost.SetFieldValue(lTimeControl.CapitalCost);
          DoContextValidation(dvtChannelCapitalCost);
        end
        else
          EdtCapitalCost.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateFixedOMCost;
const OPNAME = 'TChannelTimeControlValidator.UpdateFixedOMCost';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtFixedOMCost.FieldProperty.FieldName, Trim(EdtFixedOMCost.Text), lMessage)) then
        begin
          EdtFixedOMCost.FieldValidationError := lMessage;
          lTimeControl.FixedOMCost := StrToFloat(Trim(EdtFixedOMCost.Text));
          EdtFixedOMCost.SetFieldValue(lTimeControl.FixedOMCost);
          DoContextValidation(dvtChannelFixedOMCost);
        end
        else
          EdtFixedOMCost.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateVariableOMCost;
const OPNAME = 'TChannelTimeControlValidator.UpdateVariableOMCost';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtVariableOMCost.FieldProperty.FieldName, Trim(EdtVariableOMCost.Text), lMessage)) then
        begin
          EdtVariableOMCost.FieldValidationError := lMessage;
          lTimeControl.VariableOMCost := StrToFloat(Trim(EdtVariableOMCost.Text));
          EdtVariableOMCost.SetFieldValue(lTimeControl.VariableOMCost);
          DoContextValidation(dvtChannelVariableOMCost);
        end
        else
          EdtVariableOMCost.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateNrYearsConstruct;
const OPNAME = 'TChannelTimeControlValidator.UpdateNrYearsConstruct';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('ChannelYearsToConstruct',
            Trim(EdtNrYearsConstruct.Text), lMessage)) then
        begin
          EdtNrYearsConstruct.FieldValidationError := lMessage;
          lTimeControl.YearsToConstruct := StrToInt(Trim(EdtNrYearsConstruct.Text));
          EdtNrYearsConstruct.SetFieldValue(lTimeControl.YearsToConstruct);
          DoContextValidation(dvtChannelYearsToConstruct);
          RePopulateDataViewer;
        end
        else
          EdtNrYearsConstruct.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateCostScheduleValue (AIndex : integer;
                                                                AValue : string);
const OPNAME = 'TChannelTimeControlValidator.UpdateCostScheduleValue';
var
  lChannelList : IChannelList;
  lChannel     : IGeneralFlowChannel;
  lTimeControl : IChannelTimeControl;
  lMessage     : string;
  lValue       : double;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        GrdCostSchedule.ValidationError[AIndex, 1, gveCellContext] :='';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            GrdCostSchedule.FieldProperty(AIndex).FieldName, AValue, lMessage)) then
        begin
          if (Trim(AValue) = '') then
            AValue := '0.0';
          lValue := StrToFloat(AValue);
          lTimeControl.CostScheduleByIndex[AIndex] := lValue;
          DoContextValidation(dvtChannelCostSchedule);
        end
        else
          GrdCostSchedule.ValidationError[AIndex, 1, gveCellContext] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateYearsInAnalysis;
const OPNAME = 'TChannelTimeControlValidator.UpdateYearsInAnalysis';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtYearsInAnalysis.FieldProperty.FieldName, Trim(EdtYearsInAnalysis.Text), lMessage)) then
        begin
          EdtYearsInAnalysis.FieldValidationError := lMessage;
          lTimeControl.YearsInAnalysis := StrToInt(Trim(EdtYearsInAnalysis.Text));
          EdtYearsInAnalysis.SetFieldValue(lTimeControl.YearsInAnalysis);
          RePopulateDataViewer;
          DoContextValidation(dvtChannelYearsInAnalysis);
        end
        else
          EdtYearsInAnalysis.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.UpdateEscalationCostValue (AIndex : integer;
                                                                  AValue : string);
const OPNAME = 'TChannelTimeControlValidator.UpdateEscalationCostValue';
var
  lChannelList : IChannelList;
  lChannel     : IGeneralFlowChannel;
  lTimeControl : IChannelTimeControl;
  lMessage     : string;
  lValue       : double;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lTimeControl := lChannel.TimeControl;
      with ChannelTimeControlDialog do
      begin
        GrdEscalationCost.ValidationError[1, AIndex, gveCellContext] :='';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            GrdEscalationCost.FieldProperty(1).FieldName, AValue, lMessage)) then
        begin
          if (Trim(AValue) = '') then
            AValue := '0.0';
          lValue := StrToFLoat(AValue);
          lTimeControl.EscalationCostValueByIndex[AIndex] := lValue;
          DoContextValidation(dvtChannelEscalationCost);
        end
        else
          GrdEscalationCost.ValidationError[1, AIndex, gveCellContext] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.DoContextValidation (AValidationType : TDialogValidationType);
const OPNAME = 'TChannelTimeControlValidator.DoContextValidation';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
  lTimeControl : IChannelTimeControl;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
        lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
        if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
        begin
          lTimeControl := lChannel.TimeControl;
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelStartYear) OR
             (AValidationType = dvtChannelStartMonth) OR
             (AValidationType = dvtChannelEndYear) OR
             (AValidationType = dvtChannelEndMonth) then
          begin
            ValidateStartYear(lTimeControl);
            ValidateStartMonth(lTimeControl);
            ValidateEndYear(lTimeControl);
            ValidateEndMonth(lTimeControl);
          end;
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelEconomicLife) then
            ValidateEconomicLife(lTimeControl);
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelCapitalCost) then
            ValidateFixedOMCost(lTimeControl);
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelFixedOMCost) then
            ValidateFixedOMCost(lTimeControl);
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelVariableOMCost) then
            ValidateVariableOMCost(lTimeControl);
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelYearsToConstruct) then
            ValidateNrYearsConstruct(lTimeControl);
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelCostSchedule) then
            ValidateCostSchedule(lTimeControl);
          if (AValidationType = dvtChannelTimeControlAll) OR
             (AValidationType = dvtChannelEscalationCost) OR
             (AValidationType = dvtChannelYearsInAnalysis) then
          begin
            ValidateEscalationCost(lTimeControl);
            ValidateYearsInAnalysis(lTimeControl);
          end;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateStartYear (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateStartYear';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelStartYear')) then
      begin
        CbxStartYear.InValidationError := TRUE;
        CbxStartYear.ValidationError := FErrorMessage;
        CbxStartYear.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxStartYear.InValidationError := FALSE;
        CbxStartYear.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateStartMonth (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateStartMonth';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelStartMonth')) then
      begin
        CbxStartMonth.InValidationError := TRUE;
        CbxStartMonth.ValidationError := FErrorMessage;
        CbxStartMonth.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxStartMonth.InValidationError := FALSE;
        CbxStartMonth.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateEndYear (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateEndYear';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelEndYear')) then
      begin
        CbxEndYear.InValidationError := TRUE;
        CbxEndYear.ValidationError := FErrorMessage;
        CbxEndYear.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxEndYear.InValidationError := FALSE;
        CbxEndYear.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateEndMonth (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateEndMonth';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelEndMonth')) then
      begin
        CbxEndMonth.InValidationError := TRUE;
        CbxEndMonth.ValidationError := FErrorMessage;
        CbxEndMonth.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxEndMonth.InValidationError := FALSE;
        CbxEndMonth.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateEconomicLife (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateEconomicLife';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelEconomicLife')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtEconomicLife.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateCapitalCost (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateCapitalCost';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelCapitalCost')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtCapitalCost.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateFixedOMCost (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateFixedOMCost';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelFixedOMCost')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtFixedOMCost.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateVariableOMCost (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateVariableOMCost';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelVariableOMCost')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtVariableOMCost.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateNrYearsConstruct (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateNrYearsConstruct';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelYearsToConstruct')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       EdtNrYearsConstruct.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateCostSchedule (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateCostSchedule';
var
  lErrorCols     : TStringList;
  lCol           : integer;
  lErrorMessages : TStringList;
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';

      for lCol := 0 to ATimeControl.YearsToConstruct - 1 do
        GrdCostSchedule.ValidationError[lCol, 1, gveCellContext] := '';
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelCostSchedule')) then
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 0 to ATimeControl.YearsToConstruct - 1 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lCol)) >= 0) then
              GrdCostSchedule.ValidationError[lCol, 1, gveColContext] := lErrorMessages.Text;
          end;
          FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateYearsInAnalysis (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateYearsInAnalysis';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelYearsInAnalysis')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       EdtYearsInAnalysis.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelTimeControlValidator.ValidateEscalationCost (ATimeControl : IChannelTimeControl);
const OPNAME = 'TChannelTimeControlValidator.ValidateEscalationCost';
begin
  try
    with ChannelTimeControlDialog do
    begin
      FErrorMessage := '';

      GrdEscalationCost.ValidationError[0, 0, gveColContext] := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ChannelCostSchedule')) then
        GrdEscalationCost.ValidationError[0, 0, gveColContext] := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


