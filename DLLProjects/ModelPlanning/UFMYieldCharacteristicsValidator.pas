{******************************************************************************}
{*  UNIT      : Contains the class TFMYieldCharacteristicsValidator.          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMYieldCharacteristicsValidator;

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
  UFMYieldCharacteristicsDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMYieldCharacteristicsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading  : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure UpdateNrOfStartStoragePercs;
    procedure UpdateNrOfCurveSets;
    procedure UpdateNrOfLoadCases;
    procedure UpdatePeriodLength;
    procedure UpdateStartStoragePerc (AIndex : integer);
    procedure GrdCurveSetsSelectCell (Sender        : TObject;
                                      ACol, ARow    : Integer;
                                      var CanSelect : Boolean);
    procedure ValidateNrOfLoadCases (AAllocDef : IAllocationDefinition);
    procedure ValidateNrOfStartStoragePercs (AAllocDef : IAllocationDefinition);
    procedure ValidateNrOfCurveSets (AAllocDef : IAllocationDefinition);
    procedure ValidateMonthCurveSet (AAllocDef : IAllocationDefinition);
    procedure ValidatePeriodLength (AAllocDef : IAllocationDefinition);
    procedure ValidateStartStoragePercs (AAllocDef : IAllocationDefinition);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMYieldCharacteristicsDialog : TFMYieldCharacteristicsDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math, VCL.Grids;

{******************************************************************************}
{* TFMYieldCharacteristicsValidator                                           *}
{******************************************************************************}

procedure TFMYieldCharacteristicsValidator.CreateMemberObjects;
const OPNAME = 'TFMYieldCharacteristicsValidator.CreateMemberObjects';
var
  lpPanel     : TFMYieldCharacteristicsDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    FHeading    := 'TabCaption.YieldCharacteristics';
    CreateDialog;
    lpPanel := FMYieldCharacteristicsDialog;
    with lpPanel do
    begin
      EdtNrOfLoadCases.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfLoadCases');
      EdtNrOfLoadCases.OnEnter       := OnEditControlEnter;
      EdtNrOfLoadCases.OnExit        := OnEditControltExit;

      EdtPeriodLength.FieldProperty := FAppModules.FieldProperties.FieldProperty('PeriodLength');
      EdtPeriodLength.OnEnter       := OnEditControlEnter;
      EdtPeriodLength.OnExit        := OnEditControltExit;

      EdtNrOfStartStoragePercs.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfStartStoragePercs');
      EdtNrOfStartStoragePercs.OnEnter       := OnEditControlEnter;
      EdtNrOfStartStoragePercs.OnExit        := OnEditControltExit;

      EdtNrOfCurveSets.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfCurveSets');
      EdtNrOfCurveSets.OnEnter       := OnEditControlEnter;
      EdtNrOfCurveSets.OnExit        := OnEditControltExit;

      GrdStartStoragePercs.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdStartStoragePercs.OnColEnter         := OnStringGridColEnter;
      GrdStartStoragePercs.OnEnter            := OnEditControlEnter;

      GrdCurveSets.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdCurveSets.OnColEnter         := OnStringGridColEnter;
      GrdCurveSets.OnEnter            := OnEditControlEnter;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.CreateDialog;
const OPNAME = 'TFMYieldCharacteristicsValidator.CreateDialog';
begin
  try
    FPanel  := TFMYieldCharacteristicsDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.DestroyMemberObjects;
const OPNAME = 'TFMYieldCharacteristicsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsValidator.Initialise: boolean;
const OPNAME = 'TFMYieldCharacteristicsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with FMYieldCharacteristicsDialog do
    begin
      GrdCurveSets.ColWidths[0] := 55;
      GrdStartStoragePercs.ColWidths[0] := 20;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMYieldCharacteristicsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.ClearDataViewer;
const OPNAME = 'TFMYieldCharacteristicsValidator.ClearDataViewer';
var
  lpPanel : TFMYieldCharacteristicsDialog;
  lIndex  : integer;
begin
  inherited ClearDataViewer;
  try
    lpPanel := FMYieldCharacteristicsDialog;
    with lpPanel do
    begin
      EdtNrOfLoadCases.Text         := '';
      EdtPeriodLength.Text          := '';
      EdtNrOfStartStoragePercs.Text := '';
      EdtNrOfCurveSets.Text         := '';

      GrdStartStoragePercs.Rows[0].Clear;
      for lIndex := 1 to GrdCurveSets.RowCount - 1 do
        GrdCurveSets.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.PopulateDataViewer;
const OPNAME = 'TFMYieldCharacteristicsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtYieldCharactersticsAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.RePopulateDataViewer;
const OPNAME = 'TFMYieldCharacteristicsValidator.RePopulateDataViewer';
var
  lAllocDef               : IAllocationDefinition;
  lNrOfStartStoragePercs  : integer;
  lNrOfCurveSets          : integer;
  lIndex                  : integer;
  lWidth                  : integer;
  lFieldPropName          : TAbstractFieldProperty;
  lFieldPropValue         : TAbstractFieldProperty;
  lIntVal                 : integer;
  lMonth                  : integer;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMYieldCharacteristicsDialog do
        begin
          lNrOfStartStoragePercs  := lAllocDef.NrOfStartingPercentages;
          lNrOfCurveSets          := lAllocDef.NrOfCurveSets;

          EdtNrOfLoadCases.SetFieldValue(lAllocDef.NrOfLoadCases);
          EdtPeriodLength.SetFieldValue(lAllocDef.PeriodLength);
          EdtNrOfStartStoragePercs.SetFieldValue(lNrOfStartStoragePercs);
          EdtNrOfCurveSets.SetFieldValue(lNrOfCurveSets);

          GrdStartStoragePercs.Cells[0,0] := '%';
          GrdStartStoragePercs.ColCount := lNrOfStartStoragePercs + 1;
          if (GrdStartStoragePercs.ColCount > 1) then
            GrdStartStoragePercs.FixedCols := 1;
          GrdStartStoragePercs.ClearFieldProperties;
          lFieldPropValue := FAppModules.FieldProperties.FieldProperty('StartStoragePerc');
          lWidth := 3;
          for lIndex := 0 to GrdStartStoragePercs.ColCount - 1 do
          begin
            lWidth := lWidth + GrdStartStoragePercs.ColWidths[lIndex] + 1;
            GrdStartStoragePercs.AddFieldProperty(lFieldPropValue);
          end;
          GrdStartStoragePercs.Width := lWidth;
          for lIndex := 1 to lNrOfStartStoragePercs do
            GrdStartStoragePercs.Cells[lIndex, 0] := Format(lFieldPropValue.FormatStringGrid {'%6.2f'}, [lAllocDef.StartingPercentageByIndex[lIndex]]);

          GrdCurveSets.Cells[0, 0] := 'Curve set';
          GrdCurveSets.ClearFieldProperties;
          lFieldPropName := FAppModules.FieldProperties.FieldProperty('MonthCurveSet');
          for lIndex := 0 to GrdCurveSets.ColCount - 1 do
            GrdCurveSets.AddFieldProperty(lFieldPropName);

          GrdCurveSets.RowCount := 1 + lNrOfCurveSets;
          if (GrdCurveSets.RowCount > 1) then
            GrdCurveSets.FixedRows := 1;
          if lNrOfCurveSets <= 5 then
            GrdCurveSets.Height   := 3 + (GrdCurveSets.RowCount) * (GrdCurveSets.DefaultRowHeight + 1)
          else
          begin
            GrdCurveSets.Height := 3 + 5 * (GrdCurveSets.DefaultRowHeight + 1);
            GrdCurveSets.ScrollBars := ssVertical;
          end;
          for lIndex := 1 to lNrOfCurveSets do
            GrdCurveSets.Cells[0, lIndex] := IntToStr(lIndex);
          for lMonth := 1 to 12 do
          begin
            lIndex := (lMonth + 9) mod 12;
            if (lIndex = 0) then
              lIndex := 12;
            GrdCurveSets.Cells[lMonth, 0] := FormatSettings.ShortMonthNames[lIndex];
            lIntVal := lAllocDef.DecisionCurveSetByMonth[lMonth];
            if (lIntVal > 0) then
              GrdCurveSets.Cells[lMonth, lIntVal] := IntToStr(lIntVal);
          end;
          GrdCurveSets.OnSelectCell := GrdCurveSetsSelectCell;
        end;
      end;
      FMYieldCharacteristicsDialog.Resize;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsValidator.SaveState: boolean;
const OPNAME = 'TFMYieldCharacteristicsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsValidator.FMYieldCharacteristicsDialog:TFMYieldCharacteristicsDialog;
const OPNAME = 'TFMYieldCharacteristicsValidator.FMYieldCharacteristicsDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMYieldCharacteristicsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMYieldCharacteristicsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'NrOfStartStoragePercs') OR
       (AFieldName = 'NrOfCurveSets') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMYieldCharacteristicsValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMYieldCharacteristicsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMYieldCharacteristicsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.OnEditControltExit (Sender : TObject);
const OPNAME = 'TFMYieldCharacteristicsValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMYieldCharacteristicsDialog do
    begin
      if ((Sender = EdtNrOfStartStoragePercs) AND
          (EdtNrOfStartStoragePercs.HasValueChanged)) then
        UpdateNrOfStartStoragePercs
      else
      if ((Sender = EdtNrOfCurveSets) AND
          (EdtNrOfCurveSets.HasValueChanged)) then
        UpdateNrOfCurveSets
      else
      if ((Sender = EdtNrOfLoadCases) AND
          (EdtNrOfLoadCases.HasValueChanged)) then
        UpdateNrOfLoadCases
      else
      if ((Sender = EdtPeriodLength) AND
          (EdtPeriodLength.HasValueChanged)) then
        UpdatePeriodLength;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.OnStringGridCellDataHasChanged
                                                           (ASender    : TObject;
                                                            ACol, ARow : integer);
const OPNAME = 'TFMYieldCharacteristicsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with FMYieldCharacteristicsDialog do
    begin
      if (GrdStartStoragePercs = ASender) then
        UpdateStartStoragePerc(ACol);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.UpdateNrOfStartStoragePercs;
const OPNAME = 'TFMYieldCharacteristicsValidator.UpdateNrOfStartStoragePercs';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMYieldCharacteristicsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtNrOfStartStoragePercs.FieldProperty.FieldName, Trim(EdtNrOfStartStoragePercs.Text), lMessage)) then
        begin
          EdtNrOfStartStoragePercs.FieldValidationError := lMessage;
          lAllocDef.NrOfStartingPercentages := StrToInt(Trim(EdtNrOfStartStoragePercs.Text));
          EdtNrOfStartStoragePercs.SetFieldValue(lAllocDef.NrOfStartingPercentages);
          DoContextValidation(dvtNrOfStartStoragePercs);
          RePopulateDataViewer;
        end
        else
          EdtNrOfStartStoragePercs.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.UpdateNrOfCurveSets;
const OPNAME = 'TFMYieldCharacteristicsValidator.UpdateNrOfCurveSets';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMYieldCharacteristicsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtNrOfCurveSets.FieldProperty.FieldName, Trim(EdtNrOfCurveSets.Text), lMessage)) then
        begin
          EdtNrOfCurveSets.FieldValidationError := lMessage;
          lAllocDef.NrOfCurveSets := StrToInt(Trim(EdtNrOfCurveSets.Text));
          EdtNrOfCurveSets.SetFieldValue(lAllocDef.NrOfCurveSets);
          DoContextValidation(dvtNrOfCurveSets);
          RePopulateDataViewer;
        end
        else
          EdtNrOfCurveSets.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.UpdateNrOfLoadCases;
const OPNAME = 'TFMYieldCharacteristicsValidator.UpdateNrOfLoadCases';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMYieldCharacteristicsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtNrOfLoadCases.FieldProperty.FieldName, Trim(EdtNrOfLoadCases.Text), lMessage)) then
        begin
          EdtNrOfLoadCases.FieldValidationError := lMessage;
          lAllocDef.NrOfLoadCases := StrToInt(Trim(EdtNrOfLoadCases.Text));
          EdtNrOfLoadCases.SetFieldValue(lAllocDef.NrOfLoadCases);
          DoContextValidation(dvtPlanNrOfLoadCases);
          RePopulateDataViewer;
        end
        else
          EdtNrOfLoadCases.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.UpdatePeriodLength;
const OPNAME = 'TFMYieldCharacteristicsValidator.UpdatePeriodLength';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMYieldCharacteristicsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtPeriodLength.FieldProperty.FieldName, Trim(EdtPeriodLength.Text), lMessage)) then
        begin
          EdtPeriodLength.FieldValidationError := lMessage;
          lAllocDef.PeriodLength := StrToInt(Trim(EdtPeriodLength.Text));
          EdtPeriodLength.SetFieldValue(lAllocDef.PeriodLength);
          DoContextValidation(dvtPeriodLength);
          RePopulateDataViewer;
        end
        else
          EdtNrOfCurveSets.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.UpdateStartStoragePerc (AIndex : integer);
const OPNAME = 'TFMYieldCharacteristicsValidator.UpdateStartStoragePerc';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
  lValue    : double;
  lStrValue : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMYieldCharacteristicsDialog do
      begin
        GrdStartStoragePercs.ValidationError[AIndex, 0, gveCellContext] := '';
        lStrValue := Trim(GrdStartStoragePercs.Cells[AIndex, 0]);
        if (lStrValue = '') then
          lStrValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            GrdStartStoragePercs.FieldProperty(AIndex).FieldName, lStrValue, lMessage, AIndex)) then
        begin
          lValue := StrToFloat(lStrValue);
          lAllocDef.StartingPercentageByIndex[AIndex] := lValue;
          DoContextValidation(dvtStartStoragePercs);
          RePopulateDataViewer;
        end
        else
          GrdStartStoragePercs.ValidationError[AIndex, 0, gveCellContext] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.GrdCurveSetsSelectCell (Sender        : TObject;
                                                                   ACol, ARow    : Integer;
                                                                   var CanSelect : Boolean);
const OPNAME = 'TFMYieldCharacteristicsValidator.GrdCurveSetsSelectCell';
var
  lAllocDef : IAllocationDefinition;
  lPrev     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (ARow > 0) then
    begin
      with FMYieldCharacteristicsDialog do
      begin
        lPrev := lAllocDef.DecisionCurveSetByMonth[ACol];
        lAllocDef.DecisionCurveSetByMonth[ACol] := ARow;
        GrdCurveSets.Cells[ACol, ARow]  := IntToStr(ARow);
        if (lPrev <> 0) AND (lPrev <> ARow) then
          GrdCurveSets.Cells[ACol, lPrev] := '';
        DoContextValidation(dvtNrOfCurveSets);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TFMYieldCharacteristicsValidator.DoContextValidation';
var
  lAllocDef : IAllocationDefinition;
begin
  try
    FAllErrorMessages.Clear;
    lAllocDef := nil;
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        if (AValidationType in [dvtYieldCharactersticsAll, dvtPlanNrOfLoadCases]) then
          ValidateNrOfLoadCases(lAllocDef);
        if (AValidationType in [dvtYieldCharactersticsAll, dvtNrOfStartStoragePercs]) then
          ValidateNrOfStartStoragePercs(lAllocDef);
        if (AValidationType in [dvtYieldCharactersticsAll, dvtNrOfCurveSets]) then
          ValidateNrOfCurveSets(lAllocDef);
        if (AValidationType in [dvtYieldCharactersticsAll, dvtNrOfCurveSets]) then
          ValidateMonthCurveSet(lAllocDef);
        if (AValidationType in [dvtYieldCharactersticsAll, dvtPeriodLength]) then
          ValidatePeriodLength(lAllocDef);
        if (AValidationType in [dvtYieldCharactersticsAll, dvtStartStoragePercs]) then
          ValidateStartStoragePercs(lAllocDef);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.ValidateNrOfLoadCases (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMYieldCharacteristicsValidator.ValidateNrOfLoadCases';
begin
  try
    with FMYieldCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'NrOfLoadCases')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtNrOfLoadCases.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.ValidateNrOfStartStoragePercs (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMYieldCharacteristicsValidator.ValidateNrOfStartStoragePercs';
begin
  try
    with FMYieldCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'NrOfStartStoragePercs')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtNrOfStartStoragePercs.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.ValidateNrOfCurveSets (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMYieldCharacteristicsValidator.ValidateNrOfCurveSets';
begin
  try
    with FMYieldCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'NrOfCurveSets')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtNrOfCurveSets.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.ValidateMonthCurveSet (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMYieldCharacteristicsValidator.ValidateMonthCurveSet';
var
  lErrorCols     : TStringList;
  lCol           : integer;
  lErrorMessages : TStringList;
begin
  try
    with FMYieldCharacteristicsDialog do
    begin
      FErrorMessage := '';

      for lCol := 1 to 12 do
        GrdCurveSets.ValidationError[lCol, 0, gveColContext] := '';
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        if (NOT AAllocDef.Validate(FErrorMessage, 'MonthCurveSet')) then
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 12 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lCol)) >= 0) then
              GrdCurveSets.ValidationError[lCol, 0, gveColContext] := lErrorMessages.Text
            else
              GrdCurveSets.ValidationError[lCol, 0, gveCellContext] := '';
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

procedure TFMYieldCharacteristicsValidator.ValidatePeriodLength (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMYieldCharacteristicsValidator.ValidatePeriodLength';
begin
  try
    with FMYieldCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'PeriodLength')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtPeriodLength.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMYieldCharacteristicsValidator.ValidateStartStoragePercs (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMYieldCharacteristicsValidator.ValidateStartStoragePercs';
var
  lErrorCols     : TStringList;
  lCol           : integer;
  lErrorMessages : TStringList;
begin
  try
    with FMYieldCharacteristicsDialog do
    begin
      FErrorMessage := '';

      for lCol := 1 to AAllocDef.NrOfStartingPercentages do
        GrdStartStoragePercs.ValidationError[lCol, 0, gveCellContext] := '';
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        if (NOT AAllocDef.Validate(FErrorMessage, 'StartStoragePerc')) then
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to AAllocDef.NrOfStartingPercentages do
          begin
            if (lErrorCols.IndexOf(IntToStr(lCol)) >= 0) then
              GrdStartStoragePercs.ValidationError[lCol, 0, gveCellContext] := lErrorMessages.Text
            else
              GrdStartStoragePercs.ValidationError[lCol, 0, gveCellContext] := '';
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

end.

