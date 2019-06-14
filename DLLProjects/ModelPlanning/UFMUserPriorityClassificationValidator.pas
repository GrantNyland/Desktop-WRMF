{******************************************************************************}
{*  UNIT      : Contains the class TFMUserPriorityClassificationValidator.    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMUserPriorityClassificationValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Types,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UFMUserPriorityClassificationDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMUserPriorityClassificationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading  : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure RePopulateDistribution;
    procedure UpdateNrOfReliabilityClasses;
    procedure UpdateRIValue (AIndex : integer);
    procedure UpdateRILabel (AIndex : integer);
    procedure UpdateCategoryName (AIndex : integer);
    procedure UpdateDistribution (ARowIndex : integer;
                                  AColIndex : integer);
    procedure UpdateAllocDefName (AIndex : integer);
    procedure UpdateCurtailment (ARowIndex : integer;
                                 AColIndex : integer);
    procedure DoAddRI (Sender: TObject);
    procedure DoDeleteRI(Sender: TObject);
    procedure DoAddUserCategory (Sender: TObject);
    procedure DoDeleteUserCategory(Sender: TObject);
    procedure DoAddAllocLevel (Sender: TObject);
    procedure DoDeleteAllocLevel(Sender: TObject);
    procedure ValidateNrOfReliabilityClasses (AAllocDef : IAllocationDefinition);
    procedure ValidateRIValues (AAllocDef : IAllocationDefinition);
    procedure ValidateRILabels (AAllocDef : IAllocationDefinition);
    procedure ValidateUserCategoryNames (AAllocDef : IAllocationDefinition);
    procedure ValidateDistribution (AAllocDef : IAllocationDefinition);
    procedure ValidateNrOfAllocationLevels (AAllocDef : IAllocationDefinition);
    procedure ValidateAllocationLevelName (AAllocDef : IAllocationDefinition);
    procedure ValidateCurtailment (AAllocDef : IAllocationDefinition);
    procedure DoDrawGrid (Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMUserPriorityClassificationDialog: TFMUserPriorityClassificationDialog;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  UUtilities,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math;

{******************************************************************************}
{* TFMUserPriorityClassificationValidator                                     *}
{******************************************************************************}

procedure TFMUserPriorityClassificationValidator.CreateMemberObjects;
const OPNAME = 'TFMUserPriorityClassificationValidator.CreateMemberObjects';
var
  lpPanel     : TFMUserPriorityClassificationDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    FHeading    := 'TabCaption.UserPriorityClassification';
    CreateDialog;
    lpPanel := FMUserPriorityClassificationDialog;
    with lpPanel do
    begin
      EdtNrOfRIs.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfReliabilityClasses');
      EdtNrOfRIs.IsEnabled     := FALSE;
      EdtNrOfRIs.OnEnter       := OnEditControlEnter;
//      EdtNrOfRIs.OnExit        := OnEditControltExit;

      GrdRIValue.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdRIValue.OnDrawCell         := DoDrawGrid;
      GrdRIValue.OnColEnter         := OnStringGridColEnter;
      GrdRIValue.OnEnter            := OnEditControlEnter;

      GrdRILabel.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdRILabel.OnDrawCell         := DoDrawGrid;
      GrdRILabel.OnColEnter         := OnStringGridColEnter;
      GrdRILabel.OnEnter            := OnEditControlEnter;

      GrdDistribution.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdDistribution.OnDrawCell         := DoDrawGrid;
      GrdDistribution.OnColEnter         := OnStringGridColEnter;
      GrdDistribution.OnEnter            := OnEditControlEnter;

      GrdCurtailment.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdCurtailment.OnDrawCell         := DoDrawGrid;
      GrdCurtailment.OnColEnter         := OnStringGridColEnter;
      GrdCurtailment.OnEnter            := OnEditControlEnter;

      BtnAddRI.OnClick            := DoAddRI;
      BtnDeleteRI.OnClick         := DoDeleteRI;
      BtnAddCategory.OnClick      := DoAddUserCategory;
      BtnDeleteCategory.OnClick   := DoDeleteUserCategory;
      BtnAddAllocLevel.OnClick    := DoAddAllocLevel;
      BtnDeleteAllocLevel.OnClick := DoDeleteAllocLevel;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.CreateDialog;
const OPNAME = 'TFMUserPriorityClassificationValidator.CreateDialog';
begin
  try
    FPanel  := TFMUserPriorityClassificationDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DestroyMemberObjects;
const OPNAME = 'TFMUserPriorityClassificationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationValidator.Initialise: boolean;
const OPNAME = 'TFMUserPriorityClassificationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with FMUserPriorityClassificationDialog do
    begin
      GrdDistribution.ColWidths[0] := 25;
      GrdDistribution.ColWidths[1] := 160;
      GrdCurtailment.ColWidths[0]  := 25;
      GrdCurtailment.ColWidths[1]  := 160;
      GrdRIValue.ColWidths[0]      := 185;
      GrdRILabel.ColWidths[0]      := 185;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMUserPriorityClassificationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ClearDataViewer;
const OPNAME = 'TFMUserPriorityClassificationValidator.ClearDataViewer';
var
  lpPanel : TFMUserPriorityClassificationDialog;
  lIndex  : integer;
begin
  inherited ClearDataViewer;
  try
    lpPanel := FMUserPriorityClassificationDialog;
    with lpPanel do
    begin
      EdtNrOfRIs.Text    := '';
      GrdRIValue.Rows[0].Clear;
      GrdRILabel.Rows[0].Clear;
      for lIndex := 1 to GrdDistribution.RowCount - 1 do
        GrdDistribution.Rows[lIndex].Clear;
      for lIndex := 1 to GrdCurtailment.RowCount - 1 do
        GrdCurtailment.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.PopulateDataViewer;
const OPNAME = 'TFMUserPriorityClassificationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtUserPriorityAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.RePopulateDataViewer;
const OPNAME = 'TFMUserPriorityClassificationValidator.RePopulateDataViewer';
var
  lAllocDef               : IAllocationDefinition;
  lNrOfReliabilityClasses : integer;
  lNrOfUserCategories     : integer;
  lNrOfAllocationLevels   : integer;
  lIndex                  : integer;
  lCount                  : integer;
  lWidth                  : integer;
  lHeight                 : integer;
  lMaxHeight              : integer;
  lFieldPropName          : TAbstractFieldProperty;
  lFieldPropValue         : TAbstractFieldProperty;
  lIntVal                 : integer;
  lStrVal                 : string;
  lAllocLevel             : IAllocationLevel;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMUserPriorityClassificationDialog do
        begin
          GrdRIValue.Cells[0,0] := FAppModules.Language.GetString('PlanningGUI.Value');
          GrdRILabel.Cells[0,0] := FAppModules.Language.GetString('PlanningGUI.Label');
          GrdDistribution.Cells[0,0] := 'Nr.';
          GrdDistribution.Cells[1,0] := FAppModules.Language.GetString('PlanningGUI.Description');
          GrdCurtailment.Cells[0,0]  := 'Nr.';
          GrdCurtailment.Cells[1,0]  := FAppModules.Language.GetString('PlanningGUI.Description');

          lNrOfReliabilityClasses := lAllocDef.NrOfReliabilityClasses;
          lNrOfUserCategories     := lAllocDef.NrOfCategories;
          lNrOfAllocationLevels   := lAllocDef.NrOfAllocationLevels;

          {BtnAddRI.Enabled            := lNrOfReliabilityClasses < 5;
          BtnDeleteRI.Enabled         := lNrOfReliabilityClasses > 0;
          BtnAddCategory.Enabled      := lNrOfUserCategories < 10;
          BtnDeleteCategory.Enabled   := lNrOfUserCategories > 0;
          BtnAddAllocLevel.Enabled    := lNrOfAllocationLevels < 5;
          BtnDeleteAllocLevel.Enabled := lNrOfAllocationLevels > 0; }

          ResetButtonState(lNrOfReliabilityClasses,lNrOfUserCategories,lNrOfAllocationLevels);

          EdtNrOfRIs.SetFieldValue(lNrOfReliabilityClasses);

          GrdRIValue.ColCount := lNrOfReliabilityClasses + 1;
          GrdRILabel.ColCount := lNrOfReliabilityClasses + 1;
          if (GrdRIValue.ColCount > 1) then
          begin
            GrdRIValue.FixedCols := 1;
            GrdRILabel.FixedCols := 1;
          end;
          lWidth := 3;
          lFieldPropValue := FAppModules.FieldProperties.FieldProperty('RIValue');
          lFieldPropName  := FAppModules.FieldProperties.FieldProperty('RILabel');
          GrdRIValue.ClearFieldProperties;
          GrdRILabel.ClearFieldProperties;
          for lIndex := 0 to GrdRIValue.ColCount - 1 do
          begin
            lWidth := lWidth + GrdRIValue.ColWidths[lIndex] + 1;
            GrdRIValue.AddFieldProperty(lFieldPropValue);
            GrdRILabel.AddFieldProperty(lFieldPropName);
          end;
          GrdRIValue.Width := lWidth;
          GrdRILabel.Width := lWidth;

          GrdDistribution.ColCount     := lNrOfReliabilityClasses + 2 + 1;
          GrdDistribution.Cells[GrdDistribution.ColCount - 1, 0] := FAppModules.Language.GetString('PlanningGUI.Total');
          GrdDistribution.RowCount     := lNrOfUserCategories + 1;
          if (GrdDistribution.RowCount > 1) then
            GrdDistribution.FixedRows := 1;
          lWidth := 3;
          lFieldPropName  := FAppModules.FieldProperties.FieldProperty('UserCategoryName');
          lFieldPropValue := FAppModules.FieldProperties.FieldProperty('Distribution');
          GrdDistribution.ClearFieldProperties;
          for lIndex := 0 to GrdDistribution.ColCount - 1 do
          begin
            lWidth := lWidth + GrdDistribution.ColWidths[lIndex] + 1;
            if (lIndex <= 1) then
              GrdDistribution.AddFieldProperty(lFieldPropName)
            else
            if (lIndex < GrdDistribution.ColCount - 1) then
              GrdDistribution.AddFieldProperty(lFieldPropValue);
          end;
          GrdDistribution.Width := lWidth;
          lHeight    := GrdDistribution.RowCount * (GrdDistribution.DefaultRowHeight + 1) + 3;
          lMaxHeight := 6 * (GrdDistribution.DefaultRowHeight + 1) + 3;
          if (lHeight > lMaxHeight) then
          begin
            GrdDistribution.Height := lMaxHeight;
            GrdDistribution.Width  := GrdDistribution.Width + 16;
          end
          else
            GrdDistribution.Height := lHeight;

          GrdCurtailment.ColCount  := lNrOfReliabilityClasses + 2;
          GrdCurtailment.RowCount  := lNrOfAllocationLevels + 1;
          if (GrdCurtailment.RowCount > 1) then
            GrdCurtailment.FixedRows := 1;
          lWidth := 3;
          lFieldPropName  := FAppModules.FieldProperties.FieldProperty('AllocLevelName');
          lFieldPropValue := FAppModules.FieldProperties.FieldProperty('Curtailment');
          GrdCurtailment.ClearFieldProperties;
          for lIndex := 0 to GrdCurtailment.ColCount - 1 do
          begin
            lWidth := lWidth + GrdCurtailment.ColWidths[lIndex] + 1;
            if (lIndex <= 1) then
              GrdCurtailment.AddFieldProperty(lFieldPropName)
            else
              GrdCurtailment.AddFieldProperty(lFieldPropValue);
          end;
          GrdCurtailment.Width  := lWidth;
          GrdCurtailment.Height := GrdCurtailment.RowCount * (GrdCurtailment.DefaultRowHeight + 1) + 3;

          for lIndex := 1 to lAllocDef.NrOfReliabilityClasses do
          begin
            lIntVal := lAllocDef.RecurrenceIntervalByIndex[lIndex];
            lStrVal := lAllocDef.RILabelByIndex[lIndex];
            GrdRIValue.Cells[lIndex, 0] := IntToStr(lIntVal);
            GrdRILabel.Cells[lIndex, 0] := lStrVal;
            GrdDistribution.Cells[lIndex + 1, 0] := '1 : ' + IntToStr(lIntVal);
            GrdCurtailment.Cells[lIndex + 1, 0]  := '1 : ' + IntToStr(lIntVal);
          end;
          RePopulateDistribution;

          for lCount := 1 to lNrOfAllocationLevels do
          begin
            GrdCurtailment.Cells[0, lCount] := IntToStr(lCount);
            lAllocLevel := lAllocDef.AllocationLevelByIndex[lCount - 1];
            GrdCurtailment.Cells[1, lCount] := lAllocLevel.Description;
            for lIndex := 1 to lNrOfReliabilityClasses do
            begin
              GrdCurtailment.Cells[lIndex + 1, lCount] := Format(lFieldPropValue.FormatStringGrid {'%6.2f'}, [lAllocLevel.CurtailmentByIndex[lIndex]]);
            end;
          end;
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.RePopulateDistribution;
const OPNAME = 'TFMUserPriorityClassificationValidator.RePopulateDistribution';
var
  lAllocDef               : IAllocationDefinition;
  lNrOfReliabilityClasses : integer;
  lIndex                  : integer;
  lCount                  : integer;
  lCategory               : IUserCategory;
  lTotal                  : double;
  lDblVal                 : double;
  lFieldPropValue          : TAbstractFieldProperty;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMUserPriorityClassificationDialog do
        begin
          lNrOfReliabilityClasses := lAllocDef.NrOfReliabilityClasses;

          for lCount := 1 to lAllocDef.NrOfCategories do
          begin
            GrdDistribution.Cells[0, lCount] := IntToStr(lCount);
            lCategory := lAllocDef.CategoryByIndex[lCount - 1];
            GrdDistribution.Cells[1, lCount] := lCategory.Description;
            GrdDistribution.Objects[1, lCount] := TObject(lCategory.CategoryID);
            lTotal := 0;
            lFieldPropValue := FAppModules.FieldProperties.FieldProperty('Curtailment');
            for lIndex := 1 to lNrOfReliabilityClasses do
            begin
              lDblVal := lCategory.DistributionByIndex[lIndex];
              lTotal  := lTotal + lDblVal;
              GrdDistribution.Cells[lIndex + 1, lCount] := Format(lFieldPropValue.FormatStringGrid {'%6.2f'}, [lDblVal]);
            end;
            GrdDistribution.Cells[lNrOfReliabilityClasses + 2, lCount] := Format(lFieldPropValue.FormatStringGrid {'%6.2f'}, [lTotal]);
          end;
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationValidator.SaveState: boolean;
const OPNAME = 'TFMUserPriorityClassificationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationValidator.FMUserPriorityClassificationDialog:TFMUserPriorityClassificationDialog;
const OPNAME = 'TFMUserPriorityClassificationValidator.FMUserPriorityClassificationDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMUserPriorityClassificationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMUserPriorityClassificationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'NrOfReliabilityClasses') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMUserPriorityClassificationValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMUserPriorityClassificationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.OnEditControltExit (Sender : TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMUserPriorityClassificationDialog do
    begin
      if ((Sender = EdtNrOfRIs) AND
          (EdtNrOfRIs.HasValueChanged)) then
        UpdateNrOfReliabilityClasses;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.OnStringGridCellDataHasChanged
                                                           (ASender    : TObject;
                                                            ACol, ARow : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with FMUserPriorityClassificationDialog do
    begin
      if (GrdRIValue = ASender) then
        UpdateRIValue(ACol)
      else
      if (GrdRILabel = ASender) then
        UpdateRILabel(ACol)
      else
      if (GrdDistribution = ASender) AND (ACol = 1) then
        UpdateCategoryName(ARow)
      else
      if (GrdDistribution = ASender) AND (ACol > 1) then
        UpdateDistribution(ARow, ACol)
      else
      if (GrdCurtailment = ASender) AND (ACol = 1) then
        UpdateAllocDefName(ARow)
      else
      if (GrdCurtailment = ASender) AND (ACol > 1) then
        UpdateCurtailment(ARow, ACol);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateRIValue (AIndex : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateRIValue';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
  lValue    : integer;
  lStrValue : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMUserPriorityClassificationDialog do
      begin
        GrdRIValue.ValidationError[AIndex, 0, gveCellContext] := '';
        lStrValue := Trim(GrdRIValue.Cells[AIndex, 0]);
        if (lStrValue = '') then
          lStrValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            GrdRIValue.FieldProperty(AIndex).FieldName, lStrValue, lMessage, AIndex)) then
        begin
          lValue := StrToInt(lStrValue);
          lAllocDef.RecurrenceIntervalByIndex[AIndex] := lValue;
          RePopulateDataViewer;
          DoContextValidation(dvtRIValue);
        end
        else
          GrdRIValue.ValidationError[AIndex, 0, gveCellContext] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateRILabel (AIndex : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateRILabel';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lStrValue  : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMUserPriorityClassificationDialog do
      begin
        GrdRILabel.ValidationError[AIndex, 0, gveCellContext] := '';
        lStrValue := Trim(GrdRILabel.Cells[AIndex, 0]);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'RILabel', lStrValue, lMessage, AIndex)) then
        begin
          lAllocDef.RILabelByIndex[AIndex] := lStrValue;
          DoContextValidation(dvtRILabel);
        end
        else
          GrdRILabel.ValidationError[AIndex, 0, gveCellContext] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateCategoryName (AIndex : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateCategoryName';
var
  lAllocDef     : IAllocationDefinition;
  lUserCategory : IUserCategory;
  lMessage      : string;
  lStrValue     : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lUserCategory := lAllocDef.CategoryByIndex[AIndex-1];
      if (lUserCategory <> nil) then
      begin
        with FMUserPriorityClassificationDialog do
        begin
          GrdDistribution.ValidationError[1, AIndex, gveCellContext] := '';
          lStrValue := Trim(GrdDistribution.Cells[1, AIndex]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              GrdDistribution.FieldProperty(1).FieldName, lStrValue, lMessage)) then
          begin
            lUserCategory.Description := lStrValue;
            DoContextValidation(dvtUserCategoryName);
          end
          else
            GrdDistribution.ValidationError[1, AIndex, gveCellContext] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateDistribution (ARowIndex : integer;
                                                                     AColIndex : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateDistribution';
var
  lAllocDef     : IAllocationDefinition;
  lUserCategory : IUserCategory;
  lMessage      : string;
  lStrValue     : string;
  lValue        : double;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lUserCategory := lAllocDef.CategoryByIndex[ARowIndex-1];
      if (lUserCategory <> nil) then
      begin
        with FMUserPriorityClassificationDialog do
        begin
          GrdDistribution.ValidationError[AColIndex, ARowIndex, gveCellContext] := '';
          lStrValue := Trim(GrdDistribution.Cells[AColIndex, ARowIndex]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              GrdDistribution.FieldProperty(AColIndex).FieldName, lStrValue, lMessage, AColIndex-1)) then
          begin
            lValue := StrToFloat(lStrValue);
            lUserCategory.DistributionByIndex[AColIndex-1] := lValue;
            RePopulateDistribution;
            DoContextValidation(dvtDistribution);
          end
          else
            GrdDistribution.ValidationError[AColIndex, ARowIndex, gveCellContext] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateAllocDefName (AIndex : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateAllocDefName';
var
  lAllocDef   : IAllocationDefinition;
  lAllocLevel : IAllocationLevel;
  lMessage    : string;
  lStrValue   : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lAllocLevel := lAllocDef.AllocationLevelByIndex[AIndex-1];
      if (lAllocLevel <> nil) then
      begin
        with FMUserPriorityClassificationDialog do
        begin
          GrdCurtailment.ValidationError[1, AIndex, gveCellContext] := '';
          lStrValue := Trim(GrdCurtailment.Cells[1, AIndex]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              GrdCurtailment.FieldProperty(1).FieldName, lStrValue, lMessage)) then
          begin
            lAllocLevel.Description := lStrValue;
            DoContextValidation(dvtAllocLevelName);
          end
          else
            GrdCurtailment.ValidationError[1, AIndex, gveCellContext] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateCurtailment (ARowIndex : integer;
                                                                    AColIndex : integer);
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateCurtailment';
var
  lAllocDef   : IAllocationDefinition;
  lAllocLevel : IAllocationLevel;
  lMessage    : string;
  lStrValue   : string;
  lValue      : double;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lAllocLevel := lAllocDef.AllocationLevelByIndex[ARowIndex-1];
      if (lAllocLevel <> nil) then
      begin
        with FMUserPriorityClassificationDialog do
        begin
          GrdCurtailment.ValidationError[AColIndex, ARowIndex, gveCellContext] := '';
          lStrValue := Trim(GrdCurtailment.Cells[AColIndex, ARowIndex]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              GrdCurtailment.FieldProperty(AColIndex).FieldName, lStrValue, lMessage, AColIndex-1)) then
          begin
            lValue := StrToFloat(lStrValue);
            lAllocLevel.CurtailmentByIndex[AColIndex-1] := lValue;
            DoContextValidation(dvtCurtailment);
          end
          else
            GrdCurtailment.ValidationError[AColIndex, ARowIndex, gveCellContext] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.UpdateNrOfReliabilityClasses;
const OPNAME = 'TFMUserPriorityClassificationValidator.UpdateNrOfReliabilityClasses';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMUserPriorityClassificationDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtNrOfRIs.FieldProperty.FieldName, Trim(EdtNrOfRIs.Text), lMessage)) then
        begin
          EdtNrOfRIs.FieldValidationError := lMessage;
          lAllocDef.NrOfReliabilityClasses := StrToInt(Trim(EdtNrOfRIs.Text));
          EdtNrOfRIs.SetFieldValue(lAllocDef.NrOfReliabilityClasses);
          RePopulateDataViewer;
          DoContextValidation(dvtNrOfReliabilityClasses);
        end
        else
          EdtNrOfRIs.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoAddRI (Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoAddRI';
var
  lAllocDef : IAllocationDefinition;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lAllocDef.NrOfReliabilityClasses := lAllocDef.NrOfReliabilityClasses + 1;
      FMUserPriorityClassificationDialog.EdtNrOfRIs.SetFieldValue(lAllocDef.NrOfReliabilityClasses);
      RePopulateDataViewer;
      DoContextValidation(dvtNrOfReliabilityClasses);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoDeleteRI(Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoDeleteRI';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
  lIndex    : integer;
  lCount    : integer;
begin
  try
    lIndex    := FMUserPriorityClassificationDialog.GrdRIValue.Col;
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lMessage := 'Are you sure you want to delete Reliability Class ' + IntToStr(lIndex) + '?';
      if (MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        for lCount := lIndex to 4 do
        begin
          lAllocDef.RecurrenceIntervalByIndex[lCount] := lAllocDef.RecurrenceIntervalByIndex[lCount+1];
          lAllocDef.RILabelByIndex[lCount]            := lAllocDef.RILabelByIndex[lCount+1];
        end;
        lAllocDef.NrOfReliabilityClasses := lAllocDef.NrOfReliabilityClasses - 1;
        FMUserPriorityClassificationDialog.EdtNrOfRIs.SetFieldValue(lAllocDef.NrOfReliabilityClasses);
        RePopulateDataViewer;
        DoContextValidation(dvtNrOfReliabilityClasses);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoAddUserCategory (Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoAddUserCategory';
var
  lAllocDef     : IAllocationDefinition;
  lUserCategory : IUserCategory;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lUserCategory := lAllocDef.NewUserCategory;
      RePopulateDataViewer;
      DoContextValidation(dvtNrOfUserCategories);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoDeleteUserCategory(Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoDeleteUserCategory';
var
  lAllocDef : IAllocationDefinition;
  lRow      : integer;
  lID       : integer;
begin
  try
    lRow := FMUserPriorityClassificationDialog.GrdDistribution.Row;
    lID  := Integer(FMUserPriorityClassificationDialog.GrdDistribution.Objects[1, lRow]);
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (lID > 0) then
    begin
      if (lAllocDef.RemoveUserCategory(lID)) then
      begin
        RePopulateDataViewer;
        DoContextValidation(dvtNrOfUserCategories);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoAddAllocLevel (Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoAddAllocLevel';
var
  lAllocDef     : IAllocationDefinition;
  lAllocLevel   : IAllocationLevel;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lAllocLevel := lAllocDef.NewAllocationLevel;
      RePopulateDataViewer;
      DoContextValidation(dvtNrOfAllocationLevels);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoDeleteAllocLevel(Sender: TObject);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoDeleteAllocLevel';
var
  lAllocDef     : IAllocationDefinition;
  lIndex        : integer;
begin
  try
    lIndex := FMUserPriorityClassificationDialog.GrdCurtailment.Row-1;
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (lIndex >= 0) then
    begin
      if (lAllocDef.RemoveAllocationLevel(lIndex)) then
      begin
        RePopulateDataViewer;
        DoContextValidation(dvtNrOfAllocationLevels);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoContextValidation';
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
        if (AValidationType in [dvtUserPriorityAll, dvtNrOfReliabilityClasses]) then
          ValidateNrOfReliabilityClasses(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtNrOfReliabilityClasses, dvtRIValue]) then
          ValidateRIValues(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtNrOfReliabilityClasses, dvtRIValue, dvtRILabel]) then
          ValidateRILabels(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtUserCategoryName]) then
          ValidateUserCategoryNames(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtDistribution]) then
          ValidateDistribution(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtAllocLevelName]) then
          ValidateAllocationLevelName(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtCurtailment]) then
          ValidateCurtailment(lAllocDef);
        if (AValidationType in [dvtUserPriorityAll, dvtNrOfReliabilityClasses, dvtNrOfAllocationLevels]) then
          ValidateNrOfAllocationLevels(lAllocDef);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ValidateNrOfReliabilityClasses (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateNrOfReliabilityClasses';
begin
  try
    with FMUserPriorityClassificationDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'NrOfReliabilityClasses')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtNrOfRIs.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ValidateRIValues (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateRIValues';
var
  lErrorCols     : TStringList;
  lCol           : integer;
  lErrorMessages : TStringList;
  lErrorIndex    : integer;
begin
  try
    with FMUserPriorityClassificationDialog do
    begin
      FErrorMessage := '';

      for lCol := 1 to AAllocDef.NrOfReliabilityClasses do
        GrdRIValue.ValidationError[lCol, 0, gveCellContext] := '';
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        if (NOT AAllocDef.Validate(FErrorMessage, 'RIValue')) then
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to AAllocDef.NrOfReliabilityClasses do
          begin
            lErrorIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lErrorIndex >= 0) then
              GrdRIValue.ValidationError[lCol, 0, gveCellContext] := lErrorMessages.Strings[lErrorIndex]
            else
              GrdRIValue.ValidationError[lCol, 0, gveCellContext] := '';
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

procedure TFMUserPriorityClassificationValidator.ValidateRILabels (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateRILabels';
var
  lErrorCols     : TStringList;
  lCol           : integer;
  lErrorMessages : TStringList;
  lErrorIndex    : integer;
begin
  try
    with FMUserPriorityClassificationDialog do
    begin
      FErrorMessage := '';

      for lCol := 1 to AAllocDef.NrOfReliabilityClasses  do
        GrdRILabel.ValidationError[lCol, 0, gveCellContext] := '';
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        if (NOT AAllocDef.Validate(FErrorMessage, 'RILabel')) then
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to AAllocDef.NrOfReliabilityClasses do
          begin
            lErrorIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lErrorIndex >= 0) then
              GrdRILabel.ValidationError[lCol, 0, gveCellContext] := lErrorMessages.Strings[lErrorIndex]
            else
              GrdRILabel.ValidationError[lCol, 0, gveCellContext] := '';
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

procedure TFMUserPriorityClassificationValidator.ValidateUserCategoryNames
                                                   (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateUserCategoryNames';
var
  lRow           : integer;
  lUserCategory  : IUserCategory;
begin
  try
    with FMUserPriorityClassificationDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to AAllocDef.NrOfCategories do
        GrdDistribution.ValidationError[1, lRow, gveCellContext] := '';
      for lRow := 1 to AAllocDef.NrOfCategories do
      begin
        lUserCategory := AAllocDef.CategoryByIndex[lRow-1];
        FErrorMessage := '';
        if (NOT (lUserCategory.Validate(FErrorMessage, 'UserCategoryName'))) then
        begin
          GrdDistribution.ValidationError[1, lRow, gveCellContext] := FErrorMessage;
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ValidateDistribution
                                                   (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateDistribution';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCol           : integer;
  lCount         : integer;
  lErrorMessages : TStringList;
  lUserCategory  : IUserCategory;
begin
  try
    with FMUserPriorityClassificationDialog do
    begin

      for lRow := 1 to AAllocDef.NrOfCategories do
        for lCol := 0 to AAllocDef.NrOfReliabilityClasses do
          GrdDistribution.ValidationError[lCol+2, lRow, gveCellContext] := '';
      lErrorIndexes  := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        for lRow := 1 to AAllocDef.NrOfCategories do
        begin
          lUserCategory := AAllocDef.CategoryByIndex[lRow-1];
          lErrorIndexes.Clear;
          lErrorMessages.Clear;
          FErrorMessage := '';
          if (NOT (lUserCategory.Validate(FErrorMessage, 'Distribution'))) then
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
            for lCount := 0 to lErrorIndexes.Count - 1 do
            begin
              lCol := StrToInt(lErrorIndexes.Strings[lCount]);
              GrdDistribution.ValidationError[lCol+1, lRow, gveCellContext] := lErrorMessages.Text;
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          end;
        end;
      finally
        FreeAndNil(lErrorIndexes);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ValidateNrOfAllocationLevels (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateNrOfAllocationLevels';
begin
  try
    FMUserPriorityClassificationDialog.LblAllocLevelWarning.Visible :=
      (AAllocDef.NrOfReliabilityClasses <> AAllocDef.NrOfAllocationLevels);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ValidateAllocationLevelName
                                                   (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateAllocationLevelName';
var
  lRow           : integer;
  lAllocLevel    : IAllocationLevel;
begin
  try
    with FMUserPriorityClassificationDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to AAllocDef.NrOfAllocationLevels do
        GrdCurtailment.ValidationError[1, lRow, gveCellContext] := '';

      for lRow := 1 to AAllocDef.NrOfAllocationLevels do
      begin
        lAllocLevel := AAllocDef.AllocationLevelByIndex[lRow-1];
        FErrorMessage := '';
        if (NOT (lAllocLevel.Validate(FErrorMessage, 'AllocLevelName'))) then
        begin
          GrdCurtailment.ValidationError[1, lRow, gveCellContext] := FErrorMessage;
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.ValidateCurtailment
                                                   (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMUserPriorityClassificationValidator.ValidateCurtailment';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCol           : integer;
  lCount         : integer;
  lErrorMessages : TStringList;
  lAllocLevel    : IAllocationLevel;
begin
  try
    with FMUserPriorityClassificationDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to AAllocDef.NrOfAllocationLevels do
        for lCol := 0 to AAllocDef.NrOfReliabilityClasses do
          GrdCurtailment.ValidationError[lCol+2, lRow, gveCellContext] := '';

      lErrorIndexes  := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        for lRow := 1 to AAllocDef.NrOfAllocationLevels do
        begin
          lAllocLevel := AAllocDef.AllocationLevelByIndex[lRow-1];
          lErrorIndexes.Clear;
          lErrorMessages.Clear;
          FErrorMessage := '';
          if (NOT (lAllocLevel.Validate(FErrorMessage, 'Curtailment'))) then
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
            for lCount := 0 to lErrorIndexes.Count - 1 do
            begin
              lCol := StrToInt(lErrorIndexes.Strings[lCount]);
              GrdCurtailment.ValidationError[lCol+1, lRow, gveCellContext] := lErrorMessages.Text;
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          end;
        end;
      finally
        FreeAndNil(lErrorIndexes);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMUserPriorityClassificationValidator.DoDrawGrid(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
                                                            State: TGridDrawState);
const OPNAME = 'TFMUserPriorityClassificationValidator.DoDrawGrid';
begin
  try
    if ((Sender as TFieldStringGrid).ColCount >= 3) then
    begin
      if (ACol = (Sender as TFieldStringGrid).ColCount-2) and (ARow > 0) and
      not(Sender = FMUserPriorityClassificationDialog.GrdDistribution) and
       (not(Sender = FMUserPriorityClassificationDialog.GrdRIValue) or not(Sender = FMUserPriorityClassificationDialog.GrdRILabel)) then
      begin
        (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
        if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
          (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack
        else
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
        (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
      end
      else
      if (Sender = FMUserPriorityClassificationDialog.GrdDistribution) and
      not((Sender = FMUserPriorityClassificationDialog.GrdRIValue) or (Sender = FMUserPriorityClassificationDialog.GrdRILabel)) and
      (ACol = (Sender as TFieldStringGrid).ColCount-3) and (ARow > 0) then
      begin
        (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
        if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
          (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack
        else
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
        (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
      end
      else
      if ((Sender = FMUserPriorityClassificationDialog.GrdRIValue) or (Sender = FMUserPriorityClassificationDialog.GrdRILabel)) and
        ((ACol = (Sender as TFieldStringGrid).ColCount-2) and (ARow = 0)) then
      begin
        (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
        if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
          (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack
        else
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
        (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

