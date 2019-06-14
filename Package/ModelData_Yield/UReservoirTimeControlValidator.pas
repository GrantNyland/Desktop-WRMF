{******************************************************************************}
{*  UNIT      : Contains the class TReservoirTimeControlValidator.            *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/02/24                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UReservoirTimeControlValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,
  VCLTee.GanttCh,
  VCLTee.TeEngine,
  VCL.Graphics,

  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UReservoirTimeControlDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TReservoirTimeControlValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnGrdDatesSelectCell (Sender        : TObject;
                                    ACol, ARow    : Integer;
                                    var CanSelect : Boolean);
    procedure UpdateEconomicLife;
    procedure UpdateCapitalCost;
    procedure UpdateOMCost;
    procedure UpdateNrYearsConstruct;
    procedure UpdateCostSchedule (AIndex : integer;
                                  AValue : string);
    procedure UpdateBaseNodeNumber;
    procedure UpdateStartYear;
    procedure UpdateStartMonth;
    procedure UpdateEndYear;
    procedure UpdateEndMonth;

    procedure UpdateStartDateYear;
    procedure UpdateStartDateMonth;
    procedure UpdateEndDateYear;
    procedure UpdateEndDateMonth;

    procedure RePopulateDataViewer;
    procedure PopulateComboBoxes;
    procedure PopulateDateValues1(ATimeControl   : IReservoirTimeControl);
    procedure PopulateDateValues2(ATimeControl   : IReservoirTimeControl);

    procedure ValidateStartYear (ATimeControl : IReservoirTimeControl);
    procedure ValidateStartMonth (ATimeControl : IReservoirTimeControl);
    procedure ValidateEndYear (ATimeControl : IReservoirTimeControl);
    procedure ValidateEndMonth (ATimeControl : IReservoirTimeControl);

    procedure ValidateStartDateYear (ATimeControl : IReservoirTimeControl);
    procedure ValidateStartDateMonth (ATimeControl : IReservoirTimeControl);
    procedure ValidateEndDateYear (ATimeControl : IReservoirTimeControl);
    procedure ValidateEndDateMonth (ATimeControl : IReservoirTimeControl);

    procedure ValidateEconomicLife (ATimeControl : IReservoirTimeControl);
    procedure ValidateCapitalCost (ATimeControl : IReservoirTimeControl);
    procedure ValidateOMCost (ATimeControl : IReservoirTimeControl);
    procedure ValidateNrYearsConstruct (ATimeControl : IReservoirTimeControl);
    procedure ValidateCostSchedule (ATimeControl : IReservoirTimeControl);
    procedure ValidateBaseNodeNumber(ATimeControl : IReservoirTimeControl);
    procedure ValidateReplacements(ATimeControl : IReservoirTimeControl);
    procedure PopulateGraph;
    function AddSeriesToChart (ASeries      : TGanttSeries;
                               AStartDate   : TDateTime;
                               AEndDate     : TDateTime;
                               AYaxis       : double;
                               ADescription : string;
                               AColor       : TColor ) : boolean;
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
    function ReservoirTimeControlDialog : TReservoirTimeControlDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;

  end;

implementation

uses
  SysUtils,
  UConstants,
  UUtilities,
  UAbstractFileNamesObject,
  UMonthlyDamLevelsObject,
  UReservoirPenaltyValidator,
  UYieldModelDataGUIForm,
  UReservoirPenaltyDialog,
  UReservoirPenaltyStructureData,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, VCL.Grids;

{******************************************************************************}
{* TReservoirTimeControlValidator                                             *}
{******************************************************************************}

procedure TReservoirTimeControlValidator.CreateMemberObjects;
const OPNAME = 'TReservoirTimeControlValidator.CreateMemberObjects';
var
  lPanel : TReservoirTimeControlDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    CreateDialog;
    lPanel := ReservoirTimeControlDialog;
    with lPanel do
    begin
      EdtEconomicLife.FieldProperty      := FAppModules.FieldProperties.FieldProperty('ReservoirEconomicLife');
      EdtEconomicLife.OnEnter            := OnEditControlEnter;
      EdtEconomicLife.OnExit             := OnEditControltExit;

      EdtCapitalCost.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ReservoirCapitalCost');
      EdtCapitalCost.OnEnter             := OnEditControlEnter;
      EdtCapitalCost.OnExit              := OnEditControltExit;

      EdtOandMCost.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ReservoirOMCost');
      EdtOandMCost.OnEnter               := OnEditControlEnter;
      EdtOandMCost.OnExit                := OnEditControltExit;

      EdtNrYearsConstruct.FieldProperty  := FAppModules.FieldProperties.FieldProperty('ReservoirYearsToConstruct');
      EdtNrYearsConstruct.OnEnter        := OnEditControlEnter;
      EdtNrYearsConstruct.OnExit         := OnEditControltExit;

      BaseNodeNumberCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('BaseNodeNumber');
      BaseNodeNumberCbx.OnEnter          := OnEditControlEnter;
      BaseNodeNumberCbx.OnChange         := OnEditControltExit;

      GrdCostSchedule.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdCostSchedule.OnColEnter         := OnStringGridColEnter;

      StartDateYearCbx.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ReservoirStartYear');
      StartDateYearCbx.OnEnter           := OnEditControlEnter;
      StartDateYearCbx.OnChange          := OnEditControltExit;

      StartDateMonthCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ReservoirStartMonth');
      StartDateMonthCbx.OnEnter          := OnEditControlEnter;
      StartDateMonthCbx.OnChange         := OnEditControltExit;

      EndDateYearCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ReservoirEndYear');
      EndDateYearCbx.OnEnter             := OnEditControlEnter;
      EndDateYearCbx.OnChange            := OnEditControltExit;

      EndDateMonthCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('ReservoirEndMonth');
      EndDateMonthCbx.OnEnter            := OnEditControlEnter;
      EndDateMonthCbx.OnChange           := OnEditControltExit;

      GrdDates.OnSelectCell              := OnGrdDatesSelectCell;
      GrdDates.ClearFieldProperties;
      GrdDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BaseNodeNumber'));
      GrdDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirStartYear'));
      GrdDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirStartMonth'));
      GrdDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirEndYear'));
      GrdDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirEndMonth'));

      CbxStartYear.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ReservoirStartYear');
      CbxStartYear.OnEnter               := OnEditControlEnter;
      CbxStartYear.OnChange              := OnEditControltExit;

      CbxStartMonth.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ReservoirStartMonth');
      CbxStartMonth.OnEnter              := OnEditControlEnter;
      CbxStartMonth.OnChange             := OnEditControltExit;

      CbxEndYear.FieldProperty           := FAppModules.FieldProperties.FieldProperty('ReservoirEndYear');
      CbxEndYear.OnEnter                 := OnEditControlEnter;
      CbxEndYear.OnChange                := OnEditControltExit;

      CbxEndMonth.FieldProperty          := FAppModules.FieldProperties.FieldProperty('ReservoirEndMonth');
      CbxEndMonth.OnEnter                := OnEditControlEnter;
      CbxEndMonth.OnChange               := OnEditControltExit;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirTimeControlValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.CreateDialog;
const OPNAME = 'TReservoirTimeControlValidator.CreateDialog';
begin
  try
    FPanel  := TReservoirTimeControlDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.Initialise: boolean;
const OPNAME = 'TReservoirTimeControlValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirTimeControlValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Time Control';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ClearDataViewer;
const OPNAME = 'TReservoirTimeControlValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with ReservoirTimeControlDialog do
    begin
      EdtEconomicLife.SetFieldValue('');
      EdtCapitalCost.SetFieldValue('');
      EdtOandMCost.SetFieldValue('');
      EdtNrYearsConstruct.SetFieldValue('');
      GrdCostSchedule.Rows[1].Clear;
      BaseNodeNumberCbx.ItemIndex := -1;
      StartDateYearCbx.ItemIndex  := -1;
      StartDateMonthCbx.ItemIndex := -1;
      EndDateYearCbx.ItemIndex    := -1;
      EndDateMonthCbx.ItemIndex   := -1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.PopulateDataViewer;
const OPNAME = 'TReservoirTimeControlValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateComboBoxes;
    RePopulateDataViewer;
    DoContextValidation(dvtReservoirTimeControlAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.PopulateDateValues1(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.PopulateDateValues1';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth,
  LEndYear,
  LEndMonth : integer;
begin
  try
    with ReservoirTimeControlDialog do
    begin
      LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      LStartYear          := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ATimeControl.StartYear,ATimeControl.StartMonth);
      LStartMonth         := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ATimeControl.StartYear,ATimeControl.StartMonth);
      LEndYear            := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ATimeControl.EndYear,ATimeControl.EndMonth);
      LEndMonth           := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ATimeControl.EndYear,ATimeControl.EndMonth);

      StartDateYearCbx.SetFieldIndex(StartDateYearCbx.Items.IndexOf(IntToStr(LStartYear)));
      StartDateMonthCbx.SetFieldIndex(LStartMonth-1);
      EndDateYearCbx.SetFieldIndex(EndDateYearCbx.Items.IndexOf(IntToStr(LEndYear)));
      EndDateMonthCbx.SetFieldIndex(LEndMonth-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.PopulateDateValues2(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.PopulateDateValues1';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth,
  LEndYear,
  LEndMonth : integer;
begin
  try
    with ReservoirTimeControlDialog do
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

procedure TReservoirTimeControlValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirTimeControlValidator.RePopulateDataViewer';
var
  LReservoir    : IReservoirData;
  LTimeControl  : IReservoirTimeControl;
  LNrOfYears    : integer;
  LIndex        : integer;
  LFieldProp    : TAbstractFieldProperty;
  LOther        : IReservoirData;
  LOtherTime    : IReservoirTimeControl;
  LReplacements : TStringList;
  LConfigData   : IRunConfigurationData;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirByIdentifier[FIdentifier];
      LConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        with ReservoirTimeControlDialog do
        begin
          CbxStartYear.Visible  := FALSE;
          CbxStartMonth.Visible := FALSE;
          CbxEndYear.Visible    := FALSE;
          CbxEndMonth.Visible   := FALSE;

          EdtEconomicLife.SetFieldValue(LTimeControl.EconomicLife);
          EdtCapitalCost.SetFieldValue(LTimeControl.CapitalCost);
          EdtOandMCost.SetFieldValue(LTimeControl.OMCost);
          LNrOfYears := LTimeControl.YearsToConstruct;
          EdtNrYearsConstruct.SetFieldValue(LNrOfYears);

          if (LNrOfYears = 0) then
            GrdCostSchedule.Rows[1].Clear;
          GrdCostSchedule.ColCount := LNrOfYears;
          GrdCostSchedule.Width := 3 + (1 + GrdCostSchedule.DefaultColWidth) * GrdCostSchedule.ColCount;
          lFieldProp := FAppModules.FieldProperties.FieldProperty('ReservoirCostScheduleValue');
          for LIndex := 0 to LNrOfYears-1 do
          begin
            GrdCostSchedule.Cells[LIndex, 0] := IntToStr(LIndex + 1);
            GrdCostSchedule.Cells[LIndex, 1] := Format(lFieldProp.FormatStringGrid, [LTimeControl.CostScheduleByIndex[LIndex]]);
            GrdCostSchedule.AddFieldProperty(LFieldProp);
          end;
          LReplacements := TStringList.Create;
          try
            LReplacements.CommaText := LTimeControl.Replacements;
            if {(LTimeControl.BaseNodeNumber <> LReservoir.ReservoirConfigurationData.ReservoirIdentifier)} (LReplacements.Count = 0) then
            begin
              PnlChild.Visible  := TRUE;
              PnlParent.Visible := FALSE;
              LOther := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                          ReservoirList.ReservoirByIdentifier[LTimeControl.BaseNodeNumber];

              if (LOther <> nil) then
                BaseNodeNumberCbx.ItemIndex := BaseNodeNumberCbx.Items.IndexOf(Trim(LOther.ReservoirConfigurationData.ReservoirName));
              PopulateDateValues1(LTimeControl);
              {if (LTimeControl.StartYear <> 0) then
                StartDateYearCbx.ItemIndex := StartDateYearCbx.Items.IndexOf(IntToStr(LTimeControl.StartYear));
              if (LTimeControl.StartMonth <> 0) then
                StartDateMonthCbx.ItemIndex := StartDateMonthCbx.Items.IndexOf(LConfigData.MonthNameByIndex[LTimeControl.StartMonth]);
              if (LTimeControl.EndYear <> 0) then
                EndDateYearCbx.ItemIndex := EndDateYearCbx.Items.IndexOf(IntToStr(LTimeControl.EndYear));
              if (LTimeControl.EndMonth <> 0) then
                EndDateMonthCbx.ItemIndex := EndDateMonthCbx.Items.IndexOf(LConfigData.MonthNameByIndex[LTimeControl.EndMonth]); }
            end
            else
            begin
              PnlChild.Visible  := FALSE;
              PnlParent.Visible := TRUE;

                GrdDates.RowCount := 1 + LReplacements.Count + 1;
                if GrdDates.RowCount <= 5 then
                  GrdDates.Height   := 3 + (GrdDates.RowCount) * (GrdDates.DefaultRowHeight + 1)
                else
                begin
                  GrdDates.Height := 3 + 5 * (GrdDates.DefaultRowHeight + 1);
                  GrdDates.ScrollBars := ssVertical;
                end;
                for LIndex := 0 to LReplacements.Count do
                begin
                  if (LIndex = 0) then
                    LOther := LReservoir
                  else
                    LOther := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                ReservoirList.ReservoirByIdentifier[StrToInt(LReplacements.Strings[LIndex-1])];
                  if (LOther <> nil) then
                  begin
                    GrdDates.Objects[0, LIndex+1] := TObject(LOther.ReservoirConfigurationData.ReservoirIdentifier);
                    GrdDates.Cells[0, LIndex+1]   := LOther.ReservoirConfigurationData.ReservoirName;
                    LOtherTime := LOther.TimeControl;
                    if (LOtherTime <> nil) then
                    begin
                      PopulateDateValues2(LOtherTime);
                      {if (LOtherTime.StartYear <> 0) then
                        GrdDates.Cells[1, LIndex+1] := IntToStr(LOtherTime.StartYear);
                      if (LOtherTime.StartMonth <> 0) then
                        GrdDates.Cells[2, LIndex+1] := UpperCase(LConfigData.MonthNameByIndex[LOtherTime.StartMonth]);

                      if (LOtherTime.EndYear <> 0) then
                        GrdDates.Cells[3, LIndex+1] := IntToStr(LOtherTime.EndYear);
                      if (LOtherTime.EndMonth <> 0) then
                        GrdDates.Cells[4, LIndex+1] := UpperCase(LConfigData.MonthNameByIndex[LOtherTime.EndMonth]);}
                    end;
                  end;
                end;
                DoContextValidation(dvtReplacements);
              end;
          finally
            FreeAndNil(LReplacements);
          end;
          ChartPanel.Visible := PnlParent.Visible;
        end;
        PopulateGraph;
        DoContextValidation(dvtReservoirTimeControlAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.SaveState: boolean;
const OPNAME = 'TReservoirTimeControlValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReservoirTimeControlValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReservoirTimeControlValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ReservoirTimeControlDialog do
    begin
      if (Sender = EdtEconomicLife) AND (EdtEconomicLife.HasValueChanged) then
        UpdateEconomicLife
      else
      if (Sender = EdtCapitalCost) AND (EdtCapitalCost.HasValueChanged) then
        UpdateCapitalCost
      else
      if (Sender = EdtOandMCost) AND (EdtOandMCost.HasValueChanged) then
        UpdateOMCost
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
      if (Sender = BaseNodeNumberCbx) AND (BaseNodeNumberCbx.HasValueChanged) then
        UpdateBaseNodeNumber
      else
      if (Sender = StartDateYearCbx) AND (StartDateYearCbx.HasValueChanged) then
        UpdateStartDateYear
      else
      if (Sender = StartDateMonthCbx) AND (StartDateMonthCbx.HasValueChanged) then
        UpdateStartDateMonth
      else
      if (Sender = EndDateYearCbx) AND (EndDateYearCbx.HasValueChanged) then
        UpdateEndDateYear
      else
      if (Sender = EndDateMonthCbx) AND (EndDateMonthCbx.HasValueChanged) then
        UpdateEndDateMonth;
    end;
    RePopulateDataViewer
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.OnStringGridCellDataHasChanged (ASender    : TObject;
                                                                         ACol, ARow : integer);
const OPNAME = 'TReservoirTimeControlValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with ReservoirTimeControlDialog do
    begin
      if (GrdCostSchedule = ASender) then
        UpdateCostSchedule(ACol, Trim(GrdCostSchedule.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.ReservoirTimeControlDialog : TReservoirTimeControlDialog;
const OPNAME = 'TReservoirTimeControlValidator.ReservoirTimeControlDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TReservoirTimeControlDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): boolean;
const OPNAME = 'TReservoirTimeControlValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'reservoiryearstoconstruct') OR (AFieldName = 'ReservoirEconomicLife') OR
       (AFieldName = 'ReservoirCapitalCost') OR (AFieldName = 'ReservoirOMCost') OR (AFieldName = 'ReservoirCostSchedule')
       OR (AFieldName = 'ReservoirStartYear')OR (AFieldName = 'ReservoirStartMonth')OR
       (AFieldName = 'ReservoirEndYear')OR (AFieldName = 'ReservoirEndMonth') or
       (AFieldName = 'BaseNodeNumber') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirTimeControlValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateEconomicLife;
const OPNAME = 'TReservoirTimeControlValidator.UpdateEconomicLife';
var
  lReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    LReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
    begin
      LTimeControl := LReservoir.TimeControl;
      with ReservoirTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtEconomicLife.FieldProperty.FieldName, Trim(EdtEconomicLife.Text), lMessage)) then
        begin
          EdtEconomicLife.FieldValidationError := lMessage;
          LTimeControl.EconomicLife := StrToInt(Trim(EdtEconomicLife.Text));
          EdtEconomicLife.SetFieldValue(LTimeControl.EconomicLife);
          DoContextValidation(dvtReservoirEconomicLife);
        end
        else
          EdtEconomicLife.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateCapitalCost;
const OPNAME = 'TReservoirTimeControlValidator.UpdateCapitalCost';
var
  lReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    LReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
    begin
      LTimeControl := LReservoir.TimeControl;
      with ReservoirTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtCapitalCost.FieldProperty.FieldName, Trim(EdtCapitalCost.Text), lMessage)) then
        begin
          EdtCapitalCost.FieldValidationError := lMessage;
          LTimeControl.CapitalCost := StrToFloat(Trim(EdtCapitalCost.Text));
          EdtCapitalCost.SetFieldValue(LTimeControl.CapitalCost);
          DoContextValidation(dvtReservoirCapitalCost);
        end
        else
          EdtCapitalCost.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateOMCost;
const OPNAME = 'TReservoirTimeControlValidator.UpdateOMCost';
var
  lReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    LReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
    begin
      LTimeControl := LReservoir.TimeControl;
      with ReservoirTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtOandMCost.FieldProperty.FieldName, Trim(EdtOandMCost.Text), lMessage)) then
        begin
          EdtOandMCost.FieldValidationError := lMessage;
          LTimeControl.OMCost := StrToFloat(Trim(EdtOandMCost.Text));
          EdtOandMCost.SetFieldValue(LTimeControl.OMCost);
          DoContextValidation(dvtReservoirOMCost);
        end
        else
          EdtOandMCost.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateNrYearsConstruct;
const OPNAME = 'TReservoirTimeControlValidator.UpdateNrYearsConstruct';
var
  lReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    LReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
    begin
      LTimeControl := LReservoir.TimeControl;
      with ReservoirTimeControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtNrYearsConstruct.FieldProperty.FieldName, Trim(EdtNrYearsConstruct.Text), lMessage)) then
        begin
          EdtNrYearsConstruct.FieldValidationError := lMessage;
          LTimeControl.YearsToConstruct := StrToInt(Trim(EdtNrYearsConstruct.Text));
          EdtNrYearsConstruct.SetFieldValue(LTimeControl.YearsToConstruct);
//          RePopulateDataViewer;
          DoContextValidation(dvtReservoirYearsToConstruct);
        end
        else
          EdtNrYearsConstruct.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateCostSchedule (AIndex : integer;
                                                             AValue : string);
const OPNAME = 'TReservoirTimeControlValidator.UpdateCostSchedule';
var
  lReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage     : string;
  lValue       : double;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    LReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
    begin
      LTimeControl := LReservoir.TimeControl;
      with ReservoirTimeControlDialog do
      begin
        GrdCostSchedule.ValidationError[AIndex, 1, gveCellContext] :='';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            GrdCostSchedule.FieldProperty(AIndex).FieldName, AValue, lMessage)) then
        begin
          if (Trim(AValue) = '') then
            AValue := '0.0';
          lValue := StrToFloat(AValue);
          LTimeControl.CostScheduleByIndex[AIndex] := lValue;
          DoContextValidation(dvtReservoirCostSchedule);
        end
        else
          GrdCostSchedule.ValidationError[AIndex, 1, gveCellContext] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateBaseNodeNumber;
const OPNAME = 'TReservoirTimeControlValidator.UpdateBaseNodeNumber';
var
  LReservoirList   : IReservoirDataList;
  LReservoir       : IReservoirData;
  LReservoirID     : integer;
  LTimeControl     : IReservoirTimeControl;
  LMessage         : string;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    LReservoir   := LReservoirList.ReservoirByIdentifier[FIdentifier];
    LTimeControl := LReservoir.TimeControl;
    LReservoirID := Integer(ReservoirTimeControlDialog.BaseNodeNumberCbx.Items.Objects
                    [ReservoirTimeControlDialog.BaseNodeNumberCbx.ItemIndex]);
    LReservoir := nil;
    LReservoir := LReservoirList.ReservoirByIdentifier[LReservoirID];
    if (LReservoir <> nil) and (FAppModules.FieldProperties.ValidateFieldProperty(
        ReservoirTimeControlDialog.BaseNodeNumberCbx.FieldProperty.FieldName, IntToStr(LReservoirID), LMessage)) then
    begin
      ReservoirTimeControlDialog.BaseNodeNumberCbx.ValidationError := LMessage;
      LTimeControl.BaseNodeNumber := LReservoirID;
      ReservoirTimeControlDialog.BaseNodeNumberCbx.ItemIndex := ReservoirTimeControlDialog.BaseNodeNumberCbx.Items.IndexOf(LReservoir.ReservoirConfigurationData.ReservoirName);
      DoContextValidation(dvtBaseNodeNumber);
    end
    else
      ReservoirTimeControlDialog.BaseNodeNumberCbx.ValidationError := LMessage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateStartYear;
const OPNAME = 'TReservoirTimeControlValidator.UpdateStartYear';
var
  lReservoirList : IReservoirDataList;
  lReservoirNr   : integer;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
  lRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      lRow         := GrdDates.Row;
      lReservoirNr := Integer(GrdDates.Objects[0, lRow]);
      LReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        CbxStartYear.ValidationError := '';
        GrdDates.ValidationError[1, lRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxStartYear.FieldProperty.FieldName, Trim(CbxStartYear.Text), lMessage)) then
        begin
          CbxStartYear.ValidationError := lMessage;
          GrdDates.ValidationError[1, lRow, gveCellContext] := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxStartYear.Text);
          LCalendarMonth         := CbxStartMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.StartYear := LHydroYear;
          if(LTimeControl.StartMonth <> LHydroMonth) then
            LTimeControl.StartMonth := LHydroMonth;

          GrdDates.Cells[1, lRow] := IntToStr(LTimeControl.StartYear);
          DoContextValidation(dvtReservoirStartYear);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateStartMonth;
const OPNAME = 'TReservoirTimeControlValidator.UpdateStartMonth';
var
  lReservoirList : IReservoirDataList;
  lReservoirNr   : integer;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
  lRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      lRow         := GrdDates.Row;
      lReservoirNr := Integer(GrdDates.Objects[0, lRow]);
      LReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        CbxStartMonth.ValidationError := '';
        GrdDates.ValidationError[2, lRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxStartMonth.FieldProperty.FieldName, IntToStr(CbxStartMonth.ItemIndex + 1), lMessage)) then
        begin
          CbxStartMonth.ValidationError := lMessage;
          GrdDates.ValidationError[2, lRow, gveCellContext] := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxStartYear.Text);
          LCalendarMonth         := CbxStartMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.StartMonth := LHydroMonth;
          if(LTimeControl.StartYear <> LHydroYear) then
            LTimeControl.StartYear := LHydroYear;

          GrdDates.Cells[2, lRow] := UpperCase(FormatSettings.ShortMonthNames[LTimeControl.StartMonth]);
          DoContextValidation(dvtReservoirStartMonth);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateEndYear;
const OPNAME = 'TReservoirTimeControlValidator.UpdateEndYear';
var
  lReservoirList : IReservoirDataList;
  lReservoirNr   : integer;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
  lRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      lRow         := GrdDates.Row;
      lReservoirNr := Integer(GrdDates.Objects[0, lRow]);
      LReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        CbxEndYear.ValidationError := '';
        GrdDates.ValidationError[3, lRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxEndYear.FieldProperty.FieldName, Trim(CbxEndYear.Text), lMessage)) then
        begin
          CbxEndYear.ValidationError := lMessage;
          GrdDates.ValidationError[3, lRow, gveCellContext] := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxEndYear.Text);
          LCalendarMonth         := CbxEndMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.EndYear := LHydroYear;
          if(LTimeControl.EndMonth <> LHydroMonth) then
            LTimeControl.EndMonth := LHydroMonth;


          GrdDates.Cells[3, lRow] := IntToStr(LTimeControl.EndYear);
          DoContextValidation(dvtReservoirEndYear);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateEndMonth;
const OPNAME = 'TReservoirTimeControlValidator.UpdateEndMonth';
var
  lReservoirList : IReservoirDataList;
  lReservoirNr   : integer;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  lMessage       : string;
  lRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      lRow         := GrdDates.Row;
      lReservoirNr := Integer(GrdDates.Objects[0, lRow]);
      LReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        CbxEndMonth.ValidationError := '';
        GrdDates.ValidationError[4, lRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxEndMonth.FieldProperty.FieldName, IntToStr(CbxEndMonth.ItemIndex + 1), lMessage)) then
        begin
          CbxEndMonth.ValidationError := lMessage;
          GrdDates.ValidationError[4, lRow, gveCellContext] := lMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxEndMonth.Text);
          LCalendarMonth         := CbxEndMonth.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.EndMonth := LHydroMonth;
          if(LTimeControl.EndYear <> LHydroYear) then
            LTimeControl.EndYear := LHydroYear;

          GrdDates.Cells[4, lRow] := UpperCase(FormatSettings.ShortMonthNames[LTimeControl.EndMonth]);
          DoContextValidation(dvtReservoirEndMonth);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateStartDateYear;
const OPNAME = 'TReservoirTimeControlValidator.UpdateStartDateYear';
var
  LReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  LMessage       : string;
  LRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      LRow         := GrdDates.Row;
      LReservoir   := LReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        StartDateYearCbx.ValidationError := '';
        GrdDates.ValidationError[1, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            StartDateYearCbx.FieldProperty.FieldName, Trim(StartDateYearCbx.Text), lMessage)) then
        begin
          StartDateYearCbx.ValidationError := LMessage;
          GrdDates.ValidationError[1, LRow, gveCellContext] := LMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(StartDateYearCbx.Text);
          LCalendarMonth         := StartDateMonthCbx.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.StartYear := LHydroYear;
          if(LTimeControl.StartMonth <> LHydroMonth) then
            LTimeControl.StartMonth := LHydroMonth;

          GrdDates.Cells[1, LRow] := IntToStr(LTimeControl.StartYear);
          DoContextValidation(dvtReservoirStartYear);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateStartDateMonth;
const OPNAME = 'TReservoirTimeControlValidator.UpdateStartDateMonth';
var
  LReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  LMessage       : string;
  LRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      LRow         := GrdDates.Row;
      LReservoir   := LReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        StartDateMonthCbx.ValidationError := '';
        GrdDates.ValidationError[2, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            StartDateMonthCbx.FieldProperty.FieldName, IntToStr(StartDateMonthCbx.ItemIndex + 1), LMessage)) then
        begin
          StartDateMonthCbx.ValidationError := LMessage;
          GrdDates.ValidationError[2, LRow, gveCellContext] := LMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(StartDateYearCbx.Text);
          LCalendarMonth         := StartDateMonthCbx.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.StartMonth := LHydroMonth;
          if(LTimeControl.StartYear <> LHydroYear) then
            LTimeControl.StartYear := LHydroYear;

          GrdDates.Cells[2, LRow] := UpperCase(FormatSettings.ShortMonthNames[LTimeControl.StartMonth]);
          DoContextValidation(dvtReservoirStartMonth);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TReservoirTimeControlValidator.UpdateEndDateYear;
const OPNAME = 'TReservoirTimeControlValidator.UpdateEndDateYear';
var
  LReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  LMessage       : string;
  LRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      LRow         := GrdDates.Row;
      LReservoir   := LReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        EndDateYearCbx.ValidationError := '';
        GrdDates.ValidationError[3, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EndDateYearCbx.FieldProperty.FieldName, Trim(EndDateYearCbx.Text), lMessage)) then
        begin
          EndDateYearCbx.ValidationError := LMessage;
          GrdDates.ValidationError[3, LRow, gveCellContext] := LMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(EndDateYearCbx.Text);
          LCalendarMonth         := EndDateMonthCbx.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.EndYear := LHydroYear;
          if(LTimeControl.EndMonth <> LHydroMonth) then
            LTimeControl.EndMonth := LHydroMonth;

          GrdDates.Cells[3, LRow] := IntToStr(LTimeControl.EndYear);
          DoContextValidation(dvtReservoirEndYear);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.UpdateEndDateMonth;
const OPNAME = 'TReservoirTimeControlValidator.UpdateEndDateMonth';
var
  LReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LTimeControl   : IReservoirTimeControl;
  LMessage       : string;
  LRow           : integer;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
    with ReservoirTimeControlDialog do
    begin
      LRow         := GrdDates.Row;
      LReservoir   := LReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        LTimeControl := LReservoir.TimeControl;
        EndDateMonthCbx.ValidationError := '';
        GrdDates.ValidationError[4, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EndDateMonthCbx.FieldProperty.FieldName, IntToStr(EndDateMonthCbx.ItemIndex + 1), lMessage)) then
        begin
          EndDateMonthCbx.ValidationError := LMessage;
          GrdDates.ValidationError[4, LRow, gveCellContext] := LMessage;

          LCalendarStartMonth    := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(EndDateYearCbx.Text);
          LCalendarMonth         := EndDateMonthCbx.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          LTimeControl.EndMonth := LHydroMonth;
          if(LTimeControl.EndYear <> LHydroYear) then
            LTimeControl.EndYear := LHydroYear;

          GrdDates.Cells[4, LRow] := UpperCase(FormatSettings.ShortMonthNames[LTimeControl.EndMonth]);
          DoContextValidation(dvtReservoirEndMonth);
          PopulateGraph;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TReservoirTimeControlValidator.OnGrdDatesSelectCell (Sender        : TObject;
                                                               ACol, ARow    : Integer;
                                                               var CanSelect : Boolean);
const OPNAME = 'TReservoirTimeControlValidator.OnGrdDatesSelectCell';
var
  LReservoir    : IReservoirData;
  lName         : string;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
      begin
        with ReservoirTimeControlDialog do
        begin
          CbxStartYear.Visible  := FALSE;
          CbxStartMonth.Visible := FALSE;
          CbxEndYear.Visible    := FALSE;
          CbxEndMonth.Visible   := FALSE;
          if (ARow > 0) then
          begin
            if (ACol = 1) then
            begin
              CbxStartYear.Left := 1 + GrdDates.Left + (1 + GrdDates.DefaultColWidth) * ACol;
              CbxStartYear.Top  := 2 + GrdDates.Top  + ((1 + GrdDates.DefaultRowHeight) * (ARow - GrdDates.TopRow + 1));
              lName := Trim(GrdDates.Cells[ACol, ARow]);
              CbxStartYear.ItemIndex := CbxStartYear.Items.IndexOf(lName);
              CbxStartYear.Visible := TRUE;
              if (GrdDates.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxStartYear.ValidationError   := GrdDates.ValidationError[ACol, ARow, gveCellContext];
                CbxStartYear.InValidationError := TRUE;
                CbxStartYear.ShowErrorState(TRUE);
              end
              else
              begin
                CbxStartYear.ValidationError   := '';
                CbxStartYear.InValidationError := FALSE;
                CbxStartYear.ShowErrorState(FALSE);
              end;
            end
            else
            if (ACol = 3) then
            begin
              CbxEndYear.Left := 1 + GrdDates.Left + (1 + GrdDates.DefaultColWidth) * ACol;
              CbxEndYear.Top  := 2 + GrdDates.Top  + ((1 + GrdDates.DefaultRowHeight) * (ARow - GrdDates.TopRow + 1));
              lName := Trim(GrdDates.Cells[ACol, ARow]);
              CbxEndYear.ItemIndex := CbxEndYear.Items.IndexOf(lName);
              CbxEndYear.Visible := TRUE;
              if (GrdDates.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxEndYear.ValidationError   := GrdDates.ValidationError[ACol, ARow, gveCellContext];
                CbxEndYear.InValidationError := TRUE;
                CbxEndYear.ShowErrorState(TRUE);
              end
              else
              begin
                CbxEndYear.ValidationError   := '';
                CbxEndYear.InValidationError := FALSE;
                CbxEndYear.ShowErrorState(FALSE);
              end;
            end
            else
            if (ACol = 2) then
            begin
              CbxStartMonth.Left := 1 + GrdDates.Left + (1 + GrdDates.DefaultColWidth) * ACol;
              CbxStartMonth.Top  := 2 + GrdDates.Top  + ((1 + GrdDates.DefaultRowHeight) * (ARow - GrdDates.TopRow + 1));
              lName := Trim(GrdDates.Cells[ACol, ARow]);
              CbxStartMonth.ItemIndex := CbxStartMonth.Items.IndexOf(lName);
              CbxStartMonth.Visible := TRUE;
              if (GrdDates.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxStartMonth.ValidationError   := GrdDates.ValidationError[ACol, ARow, gveCellContext];
                CbxStartMonth.InValidationError := TRUE;
                CbxStartMonth.ShowErrorState(TRUE);
              end
              else
              begin
                CbxStartMonth.ValidationError   := '';
                CbxStartMonth.InValidationError := FALSE;
                CbxStartMonth.ShowErrorState(FALSE);
              end;
            end
            else
            if (ACol = 4) then
            begin
              CbxEndMonth.Left := 1 + GrdDates.Left + (1 + GrdDates.DefaultColWidth) * ACol;
              CbxEndMonth.Top  := 2 + GrdDates.Top  + ((1 + GrdDates.DefaultRowHeight) * (ARow - GrdDates.TopRow + 1));
              lName := Trim(GrdDates.Cells[ACol, ARow]);
              CbxEndMonth.ItemIndex := CbxEndMonth.Items.IndexOf(lName);
              CbxEndMonth.Visible := TRUE;
              if (GrdDates.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxEndMonth.ValidationError   := GrdDates.ValidationError[ACol, ARow, gveCellContext];
                CbxEndMonth.InValidationError := TRUE;
                CbxEndMonth.ShowErrorState(TRUE);
              end
              else
              begin
                CbxEndMonth.ValidationError   := '';
                CbxEndMonth.InValidationError := FALSE;
                CbxEndMonth.ShowErrorState(FALSE);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.PopulateComboBoxes;
const OPNAME = 'TReservoirTimeControlValidator.PopulateComboBoxes';
var
  LIndex         : integer;
  LYear          : word;
  LMonth         : word;
  LDay           : word;
  LReservoirList : IReservoirDataList;
  LReservoir     : IReservoirData;
  LStartYear     : integer;
  LConfigData    : IRunConfigurationData;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
    LConfigData    := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    with ReservoirTimeControlDialog do
    begin
      CbxStartYear.Clear;
      CbxStartMonth.Clear;
      CbxEndYear.Clear;
      CbxEndMonth.Clear;
      
      BaseNodeNumberCbx.Clear;
      StartDateYearCbx.Clear;
      StartDateMonthCbx.Clear;
      EndDateYearCbx.Clear;
      EndDateMonthCbx.Clear;

      DecodeDate(Now, LYear, LMonth, LDay);

      LStartYear := 1900;
      for LIndex := 0 to 1100 do
      begin
        StartDateYearCbx.Items.Add(IntToStr(LStartYear + LIndex));
        EndDateYearCbx.Items.Add(IntToStr(LStartYear + LIndex));
      end;

      for LIndex := 1900 to LYear + 50 do
      begin
        CbxStartYear.Items.Add(IntToStr(LIndex));
        CbxEndYear.Items.Add(IntToStr(LIndex));
      end;
      
      for LIndex := 1 to 12 do
      begin
        CbxStartMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[LIndex]));
        CbxEndMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[LIndex]));
        StartDateMonthCbx.Items.Add(LConfigData.MonthNameByIndex[LIndex]);
        EndDateMonthCbx.Items.Add(LConfigData.MonthNameByIndex[LIndex]);
      end;
      
      if (LReservoirList <> nil) then
      begin
        for LIndex := 0 to LReservoirList.ReservoirCount - 1 do
        begin
          LReservoir := LReservoirList.ReservoirByIndex[LIndex];
          if(LReservoir <> nil) then
            BaseNodeNumberCbx.Items.AddObject(Trim(LReservoir.ReservoirConfigurationData.ReservoirName),TObject(LReservoir.ReservoirConfigurationData.ReservoirIdentifier));
        end;
      end;
    end;
    PopulateGraph;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.PopulateGraph;
const OPNAME = 'TReservoirTimeControlValidator.PopulateGraph';
var
  lResChannelTime : IReservoirData;
  LResOther       : IReservoirData;
  LReplacements   : TStringList;
  LStartDate      : integer;
  LEndDate        : integer;
  Lindex          : integer;
begin
  try
    LReplacements := TStringList.Create;
    try
      lResChannelTime := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lResChannelTime <> nil) then
      begin
        with ReservoirTimeControlDialog do
        begin
          ChartSeries.Clear;
          RecordLengthChart.Title.Text.Text := FAppModules.Language.GetString('ChartTitle.RecordLengthChart');
          RecordLengthChart.Title.Visible := True;
          LReplacements.CommaText := lResChannelTime.TimeControl.Replacements;
          for Lindex := 0 to LReplacements.Count do
          begin
            if Lindex = 0  then
              LResOther := lResChannelTime
            else
              LResOther := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
              ReservoirList.ReservoirByIdentifier[StrToInt(LReplacements.Strings[LIndex-1])];
            if LResOther <> nil then
            begin
              LStartDate  := LResOther.TimeControl.StartYear;
              LEndDate    := LResOther.TimeControl.EndYear;
              AddSeriesToChart(ChartSeries, LStartDate, LEndDate, Lindex + 1, LResOther.ReservoirConfigurationData.ReservoirName, clLime);
            end;
          end;
        end;
      end;
    finally
      LReplacements.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.DoContextValidation (AValidationType : TDialogValidationType);
const OPNAME = 'TReservoirTimeControlValidator.DoContextValidation';
var
  LReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
  LTimeControl   : IReservoirTimeControl;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        LReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
        if (LReservoir <> nil) AND (LReservoir.TimeControl <> nil) then
        begin
          LTimeControl := LReservoir.TimeControl;
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtReservoirStartYear) OR
             (AValidationType = dvtReservoirStartMonth) OR
             (AValidationType = dvtReservoirEndYear) OR
             (AValidationType = dvtReservoirEndMonth) then
          begin
            ValidateStartYear(LTimeControl);
            ValidateStartMonth(LTimeControl);
            ValidateEndYear(LTimeControl);
            ValidateEndMonth(LTimeControl);

            ValidateStartDateYear(LTimeControl);
            ValidateStartDateMonth(LTimeControl);
            ValidateEndDateYear(LTimeControl);
            ValidateEndDateMonth(LTimeControl);
          end;
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtReservoirEconomicLife) then
            ValidateEconomicLife(LTimeControl);
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtReservoirCapitalCost) then
            ValidateCapitalCost(LTimeControl);
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtReservoirOMCost) then
            ValidateOMCost(LTimeControl);
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtReservoirYearsToConstruct) then
            ValidateNrYearsConstruct(LTimeControl);
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtReservoirCostSchedule) then
            ValidateCostSchedule(LTimeControl);
          if (AValidationType = dvtReservoirTimeControlAll) OR
             (AValidationType = dvtBaseNodeNumber) then
            ValidateBaseNodeNumber(LTimeControl);
          if (AValidationType = dvtReplacements) then
            ValidateReplacements(LTimeControl);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateStartYear (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateStartYear';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirStartYear')) then
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

procedure TReservoirTimeControlValidator.ValidateStartMonth (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateStartMonth';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirStartMonth')) then
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

procedure TReservoirTimeControlValidator.ValidateEndYear (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateEndYear';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirEndYear')) then
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

procedure TReservoirTimeControlValidator.ValidateEndMonth (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateEndMonth';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirEndMonth')) then
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

procedure TReservoirTimeControlValidator.ValidateEconomicLife (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateEconomicLife';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirEconomicLife')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtEconomicLife.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateCapitalCost (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateCapitalCost';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirCapitalCost')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtCapitalCost.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateOMCost (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateOMCost';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirOMCost')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtOandMCost.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateNrYearsConstruct (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateNrYearsConstruct';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirYearsToConstruct')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
       EdtNrYearsConstruct.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateCostSchedule (ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateCostSchedule';
var
  lErrorCols     : TStringList;
  lCol           : integer;
  lErrorMessages : TStringList;
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';

      for lCol := 0 to ATimeControl.YearsToConstruct - 1 do
        GrdCostSchedule.ValidationError[lCol, 1, gveCellContext] := '';
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirCostSchedule')) then
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

procedure TReservoirTimeControlValidator.ValidateBaseNodeNumber(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateBaseNodeNumber';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'BaseNodeNumber')) then
      begin
        BaseNodeNumberCbx.InValidationError := TRUE;
        BaseNodeNumberCbx.ValidationError   := FErrorMessage;
        BaseNodeNumberCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        BaseNodeNumberCbx.InValidationError := FALSE;
        BaseNodeNumberCbx.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateReplacements(ATimeControl : IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateReplacements';
var
  LCol           : integer;
  LRow           : integer;
  LIndex         : integer;
  LErrorMessages : TStringList;
  LErrorCols     : TStringList;
begin
  try
    if (ATimeControl <> nil) then
    begin
      LErrorCols     := TStringList.Create;
      LErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        LErrorCols.Clear;
        if (ATimeControl.Validate(FErrorMessage,'Replacements')) then
        begin
          for LRow := 1 to ReservoirTimeControlDialog.GrdDates.RowCount -1 do
            for LCol := 1 to ReservoirTimeControlDialog.GrdDates.ColCount-1 do
              ReservoirTimeControlDialog.GrdDates.ValidationError[LCol,LRow, gveColContext] := '';
        end
        else
        begin
          for LRow := 1 to ReservoirTimeControlDialog.GrdDates.RowCount -1 do
          begin
            ExtractErrorsAndColumns(FErrorMessage, LErrorMessages, LErrorCols);
            for LCol := 1 to ReservoirTimeControlDialog.GrdDates.ColCount-1 do
            begin
              LIndex := LErrorCols.IndexOf(IntToStr(LCol));
              if (LIndex >= 0) then
              begin
                 ReservoirTimeControlDialog.GrdDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Replacements'));
                 ReservoirTimeControlDialog.GrdDates.ValidationError[LCol, LRow, gveCellContext] := LErrorMessages.Strings[LIndex];
              end
              else
                 ReservoirTimeControlDialog.GrdDates.ValidationError[LCol, LRow, gveCellContext] := '';
            end;
              FAllErrorMessages.AddStrings(LErrorMessages);
          end;
        end;  
      finally
        FreeAndNil(LErrorCols);
        FreeAndNil(LErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirTimeControlValidator.AddSeriesToChart(ASeries      : TGanttSeries;
                                                         AStartDate   : TDateTime;
                                                         AEndDate     : TDateTime;
                                                         AYaxis       : double;
                                                         ADescription : string;
                                                         AColor       : TColor ): boolean;
const OPNAME = 'TReservoirTimeControlValidator.AddSeriesToChart';
begin
  Result := False;
  try
    ASeries.AddGanttColor(AStartDate, AEndDate, AYaxis, ADescription, AColor);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateEndDateMonth(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateEndDateMonth';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirEndMonth')) then
      begin
        EndDateMonthCbx.InValidationError := TRUE;
        EndDateMonthCbx.ValidationError := FErrorMessage;
        EndDateMonthCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        EndDateMonthCbx.InValidationError := FALSE;
        EndDateMonthCbx.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateEndDateYear(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateEndDateYear';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirEndYear')) then
      begin
        EndDateYearCbx.InValidationError := TRUE;
        EndDateYearCbx.ValidationError := FErrorMessage;
        EndDateYearCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        EndDateYearCbx.InValidationError := FALSE;
        EndDateYearCbx.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateStartDateMonth(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateStartDateMonth';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirStartMonth')) then
      begin
        StartDateMonthCbx.InValidationError := TRUE;
        StartDateMonthCbx.ValidationError := FErrorMessage;
        StartDateMonthCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        StartDateMonthCbx.InValidationError := FALSE;
        StartDateMonthCbx.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirTimeControlValidator.ValidateStartDateYear(ATimeControl: IReservoirTimeControl);
const OPNAME = 'TReservoirTimeControlValidator.ValidateStartDateYear';
begin
  try
    with ReservoirTimeControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ATimeControl.Validate(FErrorMessage, 'ReservoirStartYear')) then
      begin
        StartDateYearCbx.InValidationError := TRUE;
        StartDateYearCbx.ValidationError := FErrorMessage;
        StartDateYearCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        StartDateYearCbx.InValidationError := FALSE;
        StartDateYearCbx.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


