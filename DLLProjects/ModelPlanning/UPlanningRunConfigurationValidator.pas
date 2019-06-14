{******************************************************************************}
{*  UNIT      : Contains the class TPlanningRunConfigurationValidator.        *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2006/04/26                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UPlanningRunConfigurationValidator;

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
  UPlanningRunConfigurationDialog;

type
  TPlanningRunConfigurationValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnShortTermOptionClick(Sender : TObject);
    procedure OnWaterQualityOptionClick(Sender : TObject);
    procedure OnHydroPowerOptionClick(Sender : TObject);
    procedure OnAllocationControlOptionClick(Sender : TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure DoAddNrOfDecisionDates(Sender: TObject);
    procedure DoDeleteNrOfDecisionDates(Sender: TObject);
    procedure UpdateWaterQualityOption;
    procedure UpdateHydroPowerOption;
    procedure UpdateAllocationControlOption;
    procedure UpdateNrOfPeriodsPerYear;
    procedure UpdateDecisionMonth (ARowIndex : integer;
                                   AColIndex : integer);
    procedure UpdateDecisionType(ARowIndex : integer;
                                 AColIndex : integer);
    procedure UpdateHydroPowerIndicator(ARowIndex : integer;
                                        AColIndex : integer);
    procedure UpdateShortTermPlanning;

    procedure PopulateDecisionDates;
    procedure RePopulateDataViewer;
    procedure ValidatePeriodsPerYear(AConfiguration: IRunConfigurationData);
    procedure ValidateDecisionMonth(AConfiguration: IRunConfigurationData);
    procedure ValidateDecisionTypes(AConfiguration: IRunConfigurationData);
    procedure ValidateHydroPowerIndicator(AConfiguration: IRunConfigurationData);
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
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function PlanningRunConfigurationDialog : TPlanningRunConfigurationDialog;
  end;

implementation

uses
  VCL.Dialogs,
  SysUtils,
  UUtilities,
  VCL.Graphics,
  UConstants,
  UYieldModelDataObject,
  URunConfigurationData,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations,
  VCL.Grids;

{******************************************************************************}
{* TPlanningRunConfigurationValidator                                                 *}
{******************************************************************************}

procedure TPlanningRunConfigurationValidator.CreateMemberObjects;
const OPNAME = 'TPlanningRunConfigurationValidator.CreateMemberObjects';
var
  lPanel     : TPlanningRunConfigurationDialog;
begin
  try
    inherited CreateMemberObjects;
    FPanel  := TPlanningRunConfigurationDialog.Create(FPanelOwner,FAppModules);
    lPanel  := PlanningRunConfigurationDialog;
    with lPanel do
    begin
      WaterQualityOptionChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('WaterQualityOption');
      WaterQualityOptionChkBox.OnEnter       := OnEditControlEnter;
      WaterQualityOptionChkBox.OnClick       := OnWaterQualityOptionClick;

      NrOfPeriodsPerYearEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PeriodsPerYear');
      NrOfPeriodsPerYearEdit.OnEnter       := OnEditControlEnter;
      NrOfPeriodsPerYearEdit.OnExit        := OnEditControltExit;

      HydroPowerOptionComboBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('HydroPowerOption');
      HydroPowerOptionComboBox.OnEnter       := OnEditControlEnter;
      HydroPowerOptionComboBox.OnClick       := OnHydroPowerOptionClick;

      AllocationControlOptionComboBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('AllocationControlOption');
      AllocationControlOptionComboBox.OnEnter       := OnEditControlEnter;
      AllocationControlOptionComboBox.OnClick       := OnAllocationControlOptionClick;

      NrOfDecisionDatesEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfDecisionDates');
      NrOfDecisionDatesEdit.IsEnabled     := False;

      ShortTermOptionRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('ShortTermPlanningOption');
      ShortTermOptionRadioGroup.OnEnter       := OnEditControlEnter;
      ShortTermOptionRadioGroup.OnClick       := OnShortTermOptionClick;

      BtnAddDecisionDate.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfDecisionDates');
      BtnAddDecisionDate.OnClick       := DoAddNrOfDecisionDates;

      BtnDeleteDecisionDate.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfDecisionDates');
      BtnDeleteDecisionDate.OnClick       := DoDeleteNrOfDecisionDates;

      GrdDecisionDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfDecisionDates'));
      GrdDecisionDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DecisionMonth'));
      GrdDecisionDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DecisionType'));
      GrdDecisionDates.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('HydroPowerIndicator'));
      GrdDecisionDates.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdDecisionDates.OnColEnter         := OnStringGridColEnter;
      GrdDecisionDates.OnEnter            := OnEditControlEnter;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.DestroyMemberObjects;
const OPNAME = 'TPlanningRunConfigurationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningRunConfigurationValidator.Initialise: boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with PlanningRunConfigurationDialog do
    begin
      ShortTermOptionRadioGroup.Items.Clear;
      ShortTermOptionRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.ShortTermPlanningN'));
      ShortTermOptionRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.ShortTermPlanningP'));
      ShortTermOptionRadioGroup.Items.Add(FAppModules.Language.GetString('RunParameters.ShortTermPlanningM'));

      AllocationControlOptionComboBox.Items.Clear;
      AllocationControlOptionComboBox.Items.Add('I');
      AllocationControlOptionComboBox.Items.Add('N');
      AllocationControlOptionComboBox.Items.Add('Y');

      HydroPowerOptionComboBox.Items.Clear;
      HydroPowerOptionComboBox.Items.Add('Y');
      HydroPowerOptionComboBox.Items.Add('N');
      HydroPowerOptionComboBox.Items.Add('X');

      GrdDecisionDates.Cells[0, 0]  := 'Nr';
      GrdDecisionDates.ColWidths[0] := 25;

      GrdDecisionDates.Cells[1, 0]  := FAppModules.Language.GetString('TField.DecisionMonth');
      GrdDecisionDates.Cells[2, 0]  := FAppModules.Language.GetString('TField.DecisionType');
      GrdDecisionDates.Cells[3, 0]  := FAppModules.Language.GetString('TField.HydroIndicator');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningRunConfigurationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Planning Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.ClearDataViewer;
const OPNAME = 'TPlanningRunConfigurationValidator.ClearDataViewer';
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
    with PlanningRunConfigurationDialog do
    begin
      for lIndex := 0 to 11 do
      begin
//        MonthsGrid.Cells[1, lIndex] := '';
//        MonthsGrid.Cells[2, lIndex] := '-1';
      end;
//      for lIndex := 0 to 9 do
//        SequenceOrderGrid.Cells[lIndex, 0] := '-1';
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
//      ParamFileNameEdit.SetFieldValue('');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.PopulateDataViewer;
const OPNAME = 'TPlanningRunConfigurationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TPlanningRunConfigurationValidator.DoContextValidation';
var
  lConfigData: TRunConfigurationData;
begin
  try
    FAllErrorMessages.Clear;
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      if (AValidationType = dvtConfigurationAll) or (AValidationType = dvtPeriodsPerYear) then
        ValidatePeriodsPerYear(lConfigData);
      if (AValidationType = dvtConfigurationAll) or (AValidationType = dvtDecisionMonth) then
        ValidateDecisionMonth(lConfigData);
      if (AValidationType = dvtConfigurationAll) or (AValidationType = dvtDecisionType) then
        ValidateDecisionTypes(lConfigData);
      if (AValidationType = dvtConfigurationAll) or (AValidationType = dvtHydroPowerIndicator) then
        ValidateHydroPowerIndicator(lConfigData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.ValidatePeriodsPerYear(AConfiguration: IRunConfigurationData);
const OPNAME = 'TPlanningRunConfigurationValidator.ValidatePeriodsPerYear';
begin
  try
    with PlanningRunConfigurationDialog do
    begin
      FErrorMessage := '';
      if (NOT AConfiguration.Validate(FErrorMessage, 'PeriodsPerYear')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfPeriodsPerYearEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.ValidateDecisionMonth(AConfiguration: IRunConfigurationData);
const OPNAME = 'TPlanningRunConfigurationValidator.ValidateDecisionMonth';
var
  lCol       : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (AConfiguration <> nil) then
    begin
      with PlanningRunConfigurationDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AConfiguration.Validate(FErrorMessage,'DecisionMonth')) then
          begin
            for lCol := 1 to AConfiguration.NrOfDecisionMonths do
              GrdDecisionDates.ValidationError[0, lCol-1, gveCellContext] := '';
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to AConfiguration.NrOfDecisionMonths do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                GrdDecisionDates.ValidationError[0, lCol-1 , gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                GrdDecisionDates.ValidationError[0, lCol-1 , gveCellContext] := ''
            end;
            FAllErrorMessages.AddStrings(lErrorMsgs);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.ValidateDecisionTypes(AConfiguration: IRunConfigurationData);
const OPNAME = 'TPlanningRunConfigurationValidator.ValidateDecisionTypes';
var
  lCount     : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (AConfiguration <> nil) then
    begin
      with PlanningRunConfigurationDialog do
        begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AConfiguration.Validate(FErrorMessage,'DecisionType')) then
          begin
            for lCount := 1 to AConfiguration.NrOfDecisionMonths do
              GrdDecisionDates.ValidationError[1, lCount, gveCellContext] := '';
         end
         else
         begin
           ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
           for lCount := 1 to AConfiguration.NrOfDecisionMonths do
           begin
             lIndex := lErrorCols.IndexOf(IntToStr(lCount));
             if (lIndex >= 0) then
               GrdDecisionDates.ValidationError[1, lCount , gveCellContext] := lErrorMsgs.Strings[lIndex]
             else
               GrdDecisionDates.ValidationError[1, lCount , gveCellContext] := ''
           end;
           FAllErrorMessages.AddStrings(lErrorMsgs);
         end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.ValidateHydroPowerIndicator(AConfiguration: IRunConfigurationData);
const OPNAME = 'TPlanningRunConfigurationValidator.ValidateHydroPowerIndicator';
var
  lCount     : integer;
  lIndex     : integer;
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
begin
  try
    if (AConfiguration <> nil) then
    begin
      with PlanningRunConfigurationDialog do
        begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AConfiguration.Validate(FErrorMessage,'HydroPowerIndicator')) then
          begin
            for lCount := 1 to AConfiguration.NrOfDecisionMonths do
              GrdDecisionDates.ValidationError[3, lCount, gveCellContext] := '';
         end
         else
         begin
           ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
           for lCount := 1 to AConfiguration.NrOfDecisionMonths do
           begin
             lIndex := lErrorCols.IndexOf(IntToStr(lCount));
             if (lIndex >= 0) then
               GrdDecisionDates.ValidationError[3, lCount, gveCellContext] := lErrorMsgs.Strings[lIndex]
             else
               GrdDecisionDates.ValidationError[3, lCount, gveCellContext] := ''
           end;
           FAllErrorMessages.AddStrings(lErrorMsgs);
         end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningRunConfigurationValidator.SaveState: boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningRunConfigurationValidator.PlanningRunConfigurationDialog : TPlanningRunConfigurationDialog;
const OPNAME = 'TPlanningRunConfigurationValidator.PlanningRunConfigurationDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPlanningRunConfigurationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningRunConfigurationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.StudyDataHasChanged';
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

function TPlanningRunConfigurationValidator.StudyHasChanged: boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with PlanningRunConfigurationDialog do
    begin
      if((Sender = NrOfPeriodsPerYearEdit) AND (NrOfPeriodsPerYearEdit.HasValueChanged)) then
        UpdateNrOfPeriodsPerYear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPlanningRunConfigurationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with PlanningRunConfigurationDialog do
    begin
      if ((GrdDecisionDates = ASender) AND (ACol = 1)) then
        UpdateDecisionMonth(ARow, ACol)
      else
      if ((GrdDecisionDates = ASender) AND (ACol = 2)) then
        UpdateDecisionType(ARow, ACol)
      else
      if ((GrdDecisionDates = ASender) AND (ACol = 3)) then
        UpdateHydroPowerIndicator(ARow, ACol)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.DoAddNrOfDecisionDates(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.DoAddNrOfDecisionDates';
var
  lConfigData : IRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      lConfigData.NrOfDecisionMonths := lConfigData.NrOfDecisionMonths + 1;
      PlanningRunConfigurationDialog.NrOfDecisionDatesEdit.
         SetFieldValue(lConfigData.NrOfDecisionMonths);
      RePopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.DoDeleteNrOfDecisionDates(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.DoDeleteNrOfDecisionDates';
var
  lConfigData : IRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      lConfigData.NrOfDecisionMonths := lConfigData.NrOfDecisionMonths - 1;
      PlanningRunConfigurationDialog.NrOfDecisionDatesEdit.
         SetFieldValue(lConfigData.NrOfDecisionMonths);
      RePopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateWaterQualityOption;
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateWaterQualityOption';
var
  lConfigData         : TRunConfigurationData;
  lWaterQualityOption : Boolean;
begin
  try
    with PlanningRunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lWaterQualityOption := lConfigData.WaterQualityOption;
        if ((lWaterQualityOption AND (NOT WaterQualityOptionChkBox.Checked)) OR
            ((NOT lWaterQualityOption) AND WaterQualityOptionChkBox.Checked)) then
        begin
          lConfigData.WaterQualityOption := WaterQualityOptionChkBox.Checked;
          WaterQualityOptionChkBox.Checked := lConfigData.WaterQualityOption;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateHydroPowerOption;
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateHydroPowerOption';
var
  lHydroPowerOption : string;
  lConfigData       : TRunConfigurationData;
begin
  try
    with PlanningRunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lHydroPowerOption := lConfigData.HydroPowerOption;
        if(lHydroPowerOption <> HydroPowerOptionComboBox.Items[HydroPowerOptionComboBox.ItemIndex]) then
        begin
          lConfigData.HydroPowerOption := HydroPowerOptionComboBox.Items[HydroPowerOptionComboBox.ItemIndex];
          HydroPowerOptionComboBox.ItemIndex := HydroPowerOptionComboBox.Items.IndexOf(lConfigData.HydroPowerOption);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateAllocationControlOption;
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateAllocationControlOption';
var
  lAllocControlOption : string;
  lConfigData         : TRunConfigurationData;
begin
  try
    with PlanningRunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        lAllocControlOption := lConfigData.AllocationControlOption;

          lConfigData.AllocationControlOption := AllocationControlOptionComboBox.Text;
          AllocationControlOptionComboBox.Text := lConfigData.AllocationControlOption;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateNrOfPeriodsPerYear;
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateNrOfPeriodsPerYear';
var
 lMessage    : string;
 lConfigData : TRunConfigurationData;
begin
  try
    with PlanningRunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'PeriodsPerYear', NrOfPeriodsPerYearEdit.Text, lMessage)) then
        begin
          NrOfPeriodsPerYearEdit.FieldValidationError := lMessage;
          lConfigData.PeriodsPerYear := StrToInt(Trim(NrOfPeriodsPerYearEdit.Text));
          NrOfPeriodsPerYearEdit.SetFieldValue(lConfigData.PeriodsPerYear);
          DoContextValidation(dvtPeriodsPerYear);
        end
        else
          NrOfPeriodsPerYearEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateDecisionMonth (ARowIndex : integer;
                                                                  AColIndex : integer);
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateDecisionMonth';
var
  lValue        : integer;
  lConfigData   : TRunConfigurationData;
  lMessage      : string;
  lStrValue     : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with PlanningRunConfigurationDialog do
      begin
        lStrValue := '';
        GrdDecisionDates.ValidationError[AColIndex , ARowIndex, gveCellField] := '';
        lStrValue := Trim(GrdDecisionDates.Cells[AColIndex, ARowIndex]);
        if (lStrValue <> '') then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('DecisionMonth', lStrValue,
              lMessage, AColIndex)) then
            begin
              lValue := StrToInt(lStrValue);
              lConfigData.DecisionMonthByIndex[ARowIndex] := lValue;
              DoContextValidation(dvtDecisionMonth);
              PopulateDecisionDates;
            end
          else
            GrdDecisionDates.ValidationError[AColIndex, ARowIndex, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateDecisionType(ARowIndex,
                                                                AColIndex: integer);
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateDecisionType';
var
  lConfigData   : TRunConfigurationData;
  lMessage      : string;
  lStrValue     : string;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with PlanningRunConfigurationDialog do
      begin
         GrdDecisionDates.ValidationError[AColIndex , ARowIndex, gveCellContext] := '';
         GrdDecisionDates.ValidationError[AColIndex , ARowIndex, gveCellField]   := '';
        lStrValue := Trim(GrdDecisionDates.Cells[AColIndex, ARowIndex]);
        if ((lStrValue <> '') AND ((UpperCase(lStrValue) = 'M')OR (UpperCase(lStrValue) = 'R')))  then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('DecisionType', lStrValue,
              lMessage, AColIndex)) then
            begin
              lConfigData.DecisionTypeByIndex[ARowIndex] := UpperCase(lStrValue);
              DoContextValidation(dvtDecisionType);
              PopulateDecisionDates;
            end
          else
            GrdDecisionDates.ValidationError[AColIndex, ARowIndex, gveCellContext] := lMessage;
        end
        else
          GrdDecisionDates.ValidationError[AColIndex, ARowIndex, gveCellField] :=
            FAppModules.Language.GetString('PlanningRunConfiguration.InvalidDecisionType');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateHydroPowerIndicator(ARowIndex,
                                                                       AColIndex: integer);
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateHydroPowerIndicator';
var
  lConfigData   : TRunConfigurationData;
  lStrValue     : string;
  lIndex        : integer;
  lPrevValue    : string;
  lValid        : boolean;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with PlanningRunConfigurationDialog do
      begin
        lValid := False;
        GrdDecisionDates.ValidationError[AColIndex , ARowIndex, gveCellContext] := '';
        lStrValue := UpperCase(Trim(GrdDecisionDates.Cells[AColIndex, ARowIndex]));
        if (lStrValue <> '') then
        begin
        if (lStrValue = 'H') then
          begin
            for lIndex := 1 to lConfigData.NrOfDecisionMonths do
            begin
              lPrevValue   := UpperCase(lConfigData.HydroPowerIndicatorByIndex[lIndex]);
              if (lPrevValue = lStrValue) then
              begin
                GrdDecisionDates.ValidationError[AColIndex , ARowIndex, gveCellField] :=
                   FAppModules.Language.GetString('PlanningRunConfiguration.InvalidHydroPowerIndicator');
                Exit;
              end
              else
                lValid := True;
            end;
          end
          else
            lValid := True;

         if lValid then
         begin
           lConfigData.HydroPowerIndicatorByIndex[ARowIndex] := UpperCase(lStrValue);
           PopulateDecisionDates;
           DoContextValidation(dvtHydroPowerIndicator);
         end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.UpdateShortTermPlanning;
const OPNAME = 'TPlanningRunConfigurationValidator.UpdateShortTermPlanning';
var
  lConfigData : TRunConfigurationData;
  lOldValue   : string;
  lNewValue   : string;
  LItemIndex  : integer;
begin
  try
    with PlanningRunConfigurationDialog do
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
      if (lConfigData <> nil) then
      begin
        LItemIndex := 0;
        lOldValue := lConfigData.ShortTermPlanningOption;
        if ShortTermOptionRadioGroup.ItemIndex = 0 then
        begin
          lNewValue := 'N';
          LItemIndex  := 0;
        end
        else if ShortTermOptionRadioGroup.ItemIndex = 1 then
        begin
          lNewValue := 'P';
           LItemIndex  := 1;
        end
        else if ShortTermOptionRadioGroup.ItemIndex = 2 then
        begin
          lNewValue := 'M';
           LItemIndex  := 2;
        end;
        if (lOldValue <> lNewValue) then
        begin
          lConfigData.ShortTermPlanningOption := lNewValue;
          ShortTermOptionRadioGroup.ItemIndex := LItemIndex;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.PopulateDecisionDates;
const OPNAME = 'TPlanningRunConfigurationValidator.PopulateDecisionDates';
var
  lNrOfDecisionDates   : integer;
  lDecisionMonth       : integer;
  lDecisionType        : string;
  lHydroPowerIndicator : string;
  lCount               : integer;
  lConfigData          : TRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with PlanningRunConfigurationDialog do
      begin
        lNrOfDecisionDates := lConfigData.NrOfDecisionMonths;
        for lCount := 1 to lNrOfDecisionDates do
        begin
          lDecisionMonth := lConfigData.DecisionMonthByIndex[lCount];
          if (lDecisionMonth <> NullInteger) then
            GrdDecisionDates.Cells[1, lCount] := IntToStr(lDecisionMonth)
          else
            GrdDecisionDates.Cells[1, lCount] := '0';

          lDecisionType := lConfigData.DecisionTypeByIndex[lCount];
          GrdDecisionDates.Cells[2, lCount] := lDecisionType;

          lHydroPowerIndicator := lConfigData.HydroPowerIndicatorByIndex[lCount];
          GrdDecisionDates.Cells[3, lCount] := lHydroPowerIndicator;
        end;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningRunConfigurationValidator.RePopulateDataViewer;
const OPNAME = 'TPlanningRunConfigurationValidator.RePopulateDataViewer';
var
  lIndex         : integer;
  lItemIndex     : integer;
  lFieldIndex    : string;
  lKeyValues     : string;
  lShortTermPlan : string;
  lFieldProperty : TAbstractFieldProperty;
  lConfigData    : IRunConfigurationData;
begin
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (lConfigData <> nil) then
    begin
      with PlanningRunConfigurationDialog do
      begin
        lFieldIndex := '';

        WaterQualityOptionChkBox.Checked := lConfigdata.WaterQualityOption;
        lFieldProperty := WaterQualityOptionChkBox.FieldProperty;
        lKeyValues := lConfigData.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
        WaterQualityOptionChkBox.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        WaterQualityOptionChkBox.HasChanges :=
          FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);

        HydroPowerOptionComboBox.ItemIndex := HydroPowerOptionComboBox.Items.IndexOf(lConfigData.HydroPowerOption);
        AllocationControlOptionComboBox.ItemIndex := AllocationControlOptionComboBox.Items.IndexOf(lConfigData.AllocationControlOption);

        NrOfPeriodsPerYearEdit.SetFieldValue(lConfigData.PeriodsPerYear);
        NrOfDecisionDatesEdit.SetFieldValue(lConfigData.NrOfDecisionMonths);

        lItemIndex := 0;
        lShortTermPlan := lConfigData.ShortTermPlanningOption;
        if lShortTermPlan   = 'N' then
          lItemIndex := 0
        else if lShortTermPlan = 'P' then
          lItemIndex := 1
        else if lShortTermPlan = 'M' then
          lItemIndex := 2;

        ShortTermOptionRadioGroup.ItemIndex := lItemIndex;

        GrdDecisionDates.RowCount := lConfigData.NrOfDecisionMonths + 1;
        if (GrdDecisionDates.RowCount > 1) then
            GrdDecisionDates.FixedRows := 1;

        for lIndex := 1 to lConfigData.NrOfDecisionMonths do
        begin
          GrdDecisionDates.Cells[0, lIndex] := IntToStr(lIndex);
        end;

        if GrdDecisionDates.RowCount <= 10 then
          GrdDecisionDates.Height   := 3 + (GrdDecisionDates.DefaultRowHeight + 1) * (GrdDecisionDates.RowCount)
        else
        begin
          GrdDecisionDates.Height := 3 + 10 * (GrdDecisionDates.DefaultRowHeight + 1);
          GrdDecisionDates.ScrollBars := ssVertical;
        end;
        PopulateDecisionDates;
        ResetButtonState;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningRunConfigurationValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.ProcessParameterChangeEvent';
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
        with PlanningRunConfigurationDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = WaterQualityOptionChkBox) then
            lFieldProperty := WaterQualityOptionChkBox.FieldProperty;
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

function TPlanningRunConfigurationValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TPlanningRunConfigurationValidator.ProcessMetaDataEvent';
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
        with PlanningRunConfigurationDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = WaterQualityOptionChkBox) then
            lFieldProperty := WaterQualityOptionChkBox.FieldProperty
          else
          if(FActiveControl = NrOfPeriodsPerYearEdit) then
            lFieldProperty := NrOfPeriodsPerYearEdit.FieldProperty
          else
          if(FActiveControl = HydroPowerOptionComboBox) then
            lFieldProperty := HydroPowerOptionComboBox.FieldProperty
          else
          if(FActiveControl = AllocationControlOptionComboBox) then
            lFieldProperty := AllocationControlOptionComboBox.FieldProperty
          else
          if(FActiveControl = NrOfDecisionDatesEdit) then
            lFieldProperty := NrOfDecisionDatesEdit.FieldProperty
          else
          if(FActiveControl = ShortTermOptionRadioGroup) then
            lFieldProperty := ShortTermOptionRadioGroup.FieldProperty
          else
          if(FActiveControl = BtnAddDecisionDate) then
            lFieldProperty := BtnAddDecisionDate.FieldProperty
          else
          if(FActiveControl = BtnDeleteDecisionDate) then
            lFieldProperty := BtnDeleteDecisionDate.FieldProperty;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnShortTermOptionClick(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.OnShortTermOptionClick';
begin
  try
    UpdateShortTermPlanning;
    OnEditControlEnter(Sender);
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnAllocationControlOptionClick(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.OnAllocationControlOptionClick';
begin
  try
    if(PlanningRunConfigurationDialog.AllocationControlOptionComboBox.HasValueChanged) then
      UpdateAllocationControlOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnHydroPowerOptionClick(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.OnHydroPowerOptionClick';
begin
  try
    if(PlanningRunConfigurationDialog.HydroPowerOptionComboBox.HasValueChanged) then
      UpdateHydroPowerOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningRunConfigurationValidator.OnWaterQualityOptionClick(Sender: TObject);
const OPNAME = 'TPlanningRunConfigurationValidator.OnWaterQualityOptionClick';
begin
  try
    if((NOT PlanningRunConfigurationDialog.WaterQualityOptionChkBox.HasChanges) AND
       (PlanningRunConfigurationDialog.WaterQualityOptionChkBox.HasValueChanged)) then
      UpdateWaterQualityOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.




