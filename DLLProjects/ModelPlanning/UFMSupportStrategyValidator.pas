{******************************************************************************}
{*  UNIT      : Contains the class TFMSupportStrategyValidator.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMSupportStrategyValidator;

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
  UFMSupportStrategyDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMSupportStrategyValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading  : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnRgpBalancingOptionClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure PopulateSubSystemComboBoxes;
    procedure DoMoveSubSystemUp (Sender: TObject);
    procedure DoMoveSubSystemDown (Sender: TObject);
    procedure DoAddFixedPosition (Sender: TObject);
    procedure DoDeleteFixedPosition (Sender: TObject);
    procedure DoAddSpecificOrder (Sender: TObject);
    procedure DoDeleteSpecificOrder (Sender: TObject);
    procedure OnGrdSubSystemOrderSelectCell (Sender        : TObject;
                                             ACol, ARow    : Integer;
                                             var CanSelect : Boolean);
    procedure OnGrdFixedPositionSelectCell (Sender        : TObject;
                                            ACol, ARow    : Integer;
                                            var CanSelect : Boolean);
    procedure OnGrdSpecificOrderSelectCell (Sender        : TObject;
                                            ACol, ARow    : Integer;
                                            var CanSelect : Boolean);
    procedure OnGrdFixedPositionTopLeftChanged (Sender : TObject);
    procedure OnGrdSpecificOrderTopLeftChanged (Sender : TObject);
    procedure UpdateSupportStrategy;
    procedure UpdateBalancingOption;
    procedure UpdateFixedPosition;
    procedure UpdateFixedSubSystemID;
    procedure UpdateBeforeSubSystemID;
    procedure UpdateAfterSubSystemID;
    procedure ValidateSupportStrategy (AAllocDef : IAllocationDefinition);
    procedure ValidateBalancingOption (AAllocDef : IAllocationDefinition);
    procedure ValidateFixedPosition (AAllocDef : IAllocationDefinition);
    procedure ValidateSpecificOrder (AAllocDef : IAllocationDefinition);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMSupportStrategyDialog : TFMSupportStrategyDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math;

{******************************************************************************}
{* TFMSupportStrategyValidator                                                *}
{******************************************************************************}

procedure TFMSupportStrategyValidator.CreateMemberObjects;
const OPNAME = 'TFMSupportStrategyValidator.CreateMemberObjects';
var
  lpPanel     : TFMSupportStrategyDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    FHeading    := 'TabCaption.SupportStrategy';
    CreateDialog;
    lpPanel := FMSupportStrategyDialog;
    with lpPanel do
    begin
      CbxSupportStrategy.FieldProperty := FAppModules.FieldProperties.FieldProperty('SupportStrategy');
      CbxSupportStrategy.OnEnter       := OnEditControlEnter;
      CbxSupportStrategy.OnExit        := OnEditControltExit;

      RgpBalancingOption.FieldProperty := FAppModules.FieldProperties.FieldProperty('BalancingOption');
      RgpBalancingOption.OnEnter       := OnEditControlEnter;
      RgpBalancingOption.OnClick       := OnRgpBalancingOptionClick;

      CbxPosition.FieldProperty := FAppModules.FieldProperties.FieldProperty('FixedPositionNr');
      CbxPosition.OnEnter       := OnEditControlEnter;
      CbxPosition.OnChange      := OnEditControltExit;

      CbxSubSystem.FieldProperty := FAppModules.FieldProperties.FieldProperty('FixedPosSubSystemID');
      CbxSubSystem.OnEnter       := OnEditControlEnter;
      CbxSubSystem.OnChange      := OnEditControltExit;

      CbxBeforeSubSystem.FieldProperty := FAppModules.FieldProperties.FieldProperty('BeforeSubSystemID');
      CbxBeforeSubSystem.OnEnter       := OnEditControlEnter;
      CbxBeforeSubSystem.OnChange      := OnEditControltExit;

      CbxAfterSubSystem.FieldProperty := FAppModules.FieldProperties.FieldProperty('AfterSubSystemID');
      CbxAfterSubSystem.OnEnter       := OnEditControlEnter;
      CbxAfterSubSystem.OnChange      := OnEditControltExit;

      GrdFixedPosition.OnTopLeftChanged := OnGrdFixedPositionTopLeftChanged;
      GrdFixedPosition.OnSelectCell     := OnGrdFixedPositionSelectCell;
      GrdFixedPosition.ClearFieldProperties;
      GrdFixedPosition.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('FixedPositionNr'));
      GrdFixedPosition.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('FixedPosSubSystemID'));

      GrdSubSystemOrder.OnSelectCell := OnGrdSubSystemOrderSelectCell;
      GrdSubSystemOrder.OnColEnter   := OnStringGridColEnter;
      GrdSubSystemOrder.ClearFieldProperties;
      GrdSubSystemOrder.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SubSystemOrder'));
      GrdSubSystemOrder.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SubSystemOrder'));

      GrdSpecificOrder.OnTopLeftChanged := OnGrdSpecificOrderTopLeftChanged;
      GrdSpecificOrder.OnSelectCell     := OnGrdSpecificOrderSelectCell;
      GrdSpecificOrder.ClearFieldProperties;
      GrdSpecificOrder.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BeforeSubSystemID'));
      GrdSpecificOrder.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AfterSubSystemID'));

      BtnMoveUp.OnClick              := DoMoveSubSystemUp;
      BtnMoveDown.OnClick            := DoMoveSubSystemDown;
      BtnAddFixedPosition.OnClick    := DoAddFixedPosition;
      BtnDeleteFixedPosition.OnClick := DoDeleteFixedPosition;
      BtnAddSpecificOrder.OnClick    := DoAddSpecificOrder;
      BtnDeleteSpecificOrder.OnClick := DoDeleteSpecificOrder;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.CreateDialog;
const OPNAME = 'TFMSupportStrategyValidator.CreateDialog';
begin
  try
    FPanel  := TFMSupportStrategyDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DestroyMemberObjects;
const OPNAME = 'TFMSupportStrategyValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyValidator.Initialise: boolean;
const OPNAME = 'TFMSupportStrategyValidator.Initialise';
var
  lIndex : integer;
begin
  Result := inherited Initialise;
  try
    with FMSupportStrategyDialog do
    begin
      RgpBalancingOption.Items.Clear;
      RgpBalancingOption.Items.Add('0');
      RgpBalancingOption.Items.Add('1');
      CbxSupportStrategy.Clear;
      for lIndex := 1 to 5 do
        CbxSupportStrategy.Items.Add(IntToStr(lIndex));
      GrdSubSystemOrder.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.Position');
      GrdSubSystemOrder.Cells[1, 0] := FAppModules.Language.GetString('PlanningGUI.Subsystem');
      GrdFixedPosition.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.Position');
      GrdFixedPosition.Cells[1, 0] := FAppModules.Language.GetString('PlanningGUI.Subsystem');
      GrdSpecificOrder.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.Before');
      GrdSpecificOrder.Cells[1, 0] := FAppModules.Language.GetString('PlanningGUI.After');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMSupportStrategyValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.ClearDataViewer;
const OPNAME = 'TFMSupportStrategyValidator.ClearDataViewer';
var
  lpPanel : TFMSupportStrategyDialog;
  lIndex  : integer;
begin
  inherited ClearDataViewer;
  try
    lpPanel := FMSupportStrategyDialog;
    with lpPanel do
    begin
      CbxSupportStrategy.ItemIndex  := -1;
      RgpBalancingOption.ItemIndex  := -1;
      for lIndex := 1 to GrdSubSystemOrder.RowCount - 1 do
        GrdSubSystemOrder.Rows[lIndex].Clear;
      for lIndex := 1 to GrdFixedPosition.RowCount - 1 do
        GrdFixedPosition.Rows[lIndex].Clear;
      for lIndex := 1 to GrdSpecificOrder.RowCount - 1 do
        GrdSpecificOrder.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.PopulateDataViewer;
const OPNAME = 'TFMSupportStrategyValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateSubSystemComboBoxes;
    RePopulateDataViewer;
    DoContextValidation(dvtSupportStrategyAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.RePopulateDataViewer;
const OPNAME = 'TFMSupportStrategyValidator.RePopulateDataViewer';
var
  lAllocDef           : IAllocationDefinition;
  lNrInFixedPosition  : integer;
  lNrInSpecificOrder  : integer;
  lNrOfSubSystems     : integer;
  lIndex              : integer;
  lSubSystemID        : integer;
  lSubSystem          : ISubSystem;
  lOrderList          : TStringList;
  lPosition           : integer;
  lDone               : boolean;
  lFixedPosition      : IFixedPosition;
  lSpecificOrder      : ISpecificOrder;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMSupportStrategyDialog do
        begin
          CbxPosition.Visible        := FALSE;
          CbxSubSystem.Visible       := FALSE;
          CbxBeforeSubSystem.Visible := FALSE;
          CbxAfterSubSystem.Visible  := FALSE;

          CbxSupportStrategy.SetFieldIndex(lAllocDef.SupportStrategy - 1);
          if (lAllocDef.BalancingOption < RgpBalancingOption.Items.Count) then
            RgpBalancingOption.ItemIndex := lAllocDef.BalancingOption;

          lNrInFixedPosition  := lAllocDef.NrInFixedPosition;
          lNrInSpecificOrder  := lAllocDef.NrInSpecificOrder;
          lNrOfSubSystems     := lAllocDef.NrOfSubSystems;

          GrdSubSystemOrder.RowCount := 1 + lNrOfSubSystems;
          if (GrdSubSystemOrder.RowCount > 1) then
            GrdSubSystemOrder.FixedRows := 1;
          lOrderList := TStringList.Create;
          try
            for lIndex := 1 to lNrOfSubSystems do
            begin
              lSubSystem := lAllocDef.SubSystemByIndex[lIndex-1];
              lPosition := 0;
              lDone     := FALSE;
              while ((NOT lDone) AND (lPosition < lOrderList.Count)) do
              begin
                if (lSubSystem.Order < StrToInt(lOrderList.Strings[lPosition])) then
                  lDone := TRUE
                else
                  lPosition := lPosition + 1;
              end;
              lOrderList.InsertObject(lPosition, IntToStr(lSubSystem.Order), TObject(lSubSystem.SubSystemID));
            end;
            for lIndex := 0 to lOrderList.Count - 1 do
            begin                                               
              lSubSystem := lAllocDef.SubSystemByID[Integer(lOrderList.Objects[lIndex])];
              GrdSubSystemOrder.Objects[0, lIndex+1] := TObject(lSubSystem.SubSystemID);
              GrdSubSystemOrder.Cells[0, lIndex+1] := IntToStr(lSubSystem.Order);
              GrdSubSystemOrder.Cells[1, lIndex+1] := lSubSystem.Name;
            end;
          finally
            FreeAndNil(lOrderList);
          end;

          ResetButtonState(lNrOfSubSystems,lNrInFixedPosition,lNrInSpecificOrder);

          GrdFixedPosition.RowCount := 1 + lNrInFixedPosition;
          if (GrdFixedPosition.RowCount > 1) then
            GrdFixedPosition.FixedRows := 1;
          for lIndex := 1 to lNrInFixedPosition do
          begin
            lFixedPosition := lAllocDef.FixedPositionByIndex[lIndex-1];
            lSubSystemID   := lFixedPosition.FixedPosSubSystemID;
            lSubSystem     := lAllocDef.SubSystemByID[lSubSystemID];
            GrdFixedPosition.Cells[0, lIndex] := IntToStr(lFixedPosition.FixedPositionNr);
            GrdFixedPosition.Objects[0, lIndex] := TObject(lFixedPosition.FixedPositionID);
            if (lSubSystem <> nil) then
              GrdFixedPosition.Cells[1, lIndex] := lSubSystem.Name
            else
              GrdFixedPosition.Cells[1, lIndex] := FAppModules.Language.GetString('PlanningGUI.Undefined');
          end;

          GrdSpecificOrder.RowCount := 1 + lNrInSpecificOrder;
          if (GrdSpecificOrder.RowCount > 1) then
            GrdSpecificOrder.FixedRows := 1;
          for lIndex := 1 to lNrInSpecificOrder do
          begin
            lSpecificOrder := lAllocDef.SpecificOrderByIndex[lIndex-1];
            GrdSpecificOrder.Objects[0, lIndex] := TObject(lSpecificOrder.SpecificOrderID);
            lSubSystemID   := lSpecificOrder.BeforeSubSystemID;
            lSubSystem   := lAllocDef.SubSystemByID[lSubSystemID];
            if (lSubSystem <> nil) then
              GrdSpecificOrder.Cells[0, lIndex] := lSubSystem.Name
            else
              GrdSpecificOrder.Cells[0, lIndex] := FAppModules.Language.GetString('PlanningGUI.Undefined');
            lSubSystemID := lSpecificOrder.AfterSubSystemID;
            lSubSystem   := lAllocDef.SubSystemByID[lSubSystemID];
            if (lSubSystem <> nil) then
              GrdSpecificOrder.Cells[1, lIndex] := lSubSystem.Name
            else
              GrdSpecificOrder.Cells[1, lIndex] := FAppModules.Language.GetString('PlanningGUI.Undefined');
          end;

        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.PopulateSubSystemComboBoxes;
const OPNAME = 'TFMSupportStrategyValidator.PopulateSubSystemComboBoxes';
var
  lIndex     : integer;
  lSubSystem : ISubSystem;
  lAllocDef  : IAllocationDefinition;
begin
  try
    with FMSupportStrategyDialog do
    begin
      CbxPosition.Clear;
      CbxSubSystem.Clear;
      CbxBeforeSubSystem.Clear;
      CbxAfterSubSystem.Clear;

      if (FIdentifier > 0) then
      begin
        lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                       AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
        if (lAllocDef <> nil) then
        begin
          for lIndex := 0 to lAllocDef.NrOfSubSystems - 1 do
          begin
            lSubSystem := lAllocDef.SubSystemByIndex[lIndex];
            CbxPosition.Items.Add(IntToStr(lIndex + 1));
            CbxSubSystem.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
            CbxBeforeSubSystem.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
            CbxAfterSubSystem.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnGrdSubSystemOrderSelectCell (Sender        : TObject;
                                                                     ACol, ARow    : Integer;
                                                                     var CanSelect : Boolean);
const OPNAME = 'TFMSupportStrategyValidator.OnGrdSubSystemOrderSelectCell';
begin
  try
    with FMSupportStrategyDialog do
    begin
      BtnMoveUp.Enabled   := ARow > 1;
      BtnMoveDown.Enabled := ARow < GrdSubSystemOrder.RowCount-1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnGrdFixedPositionSelectCell (Sender        : TObject;
                                                                    ACol, ARow    : Integer;
                                                                    var CanSelect : Boolean);
const OPNAME = 'TFMSupportStrategyValidator.OnGrdFixedPositionSelectCell';
var
  lAllocDef : IAllocationDefinition;
  lName     : string;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMSupportStrategyDialog do
        begin
          if (ARow > 0) then
          begin
            if (ACol = 0) then
            begin
              CbxPosition.Left := 1 + GrdFixedPosition.Left;
              CbxPosition.Top  := 2 + GrdFixedPosition.Top +
                                   ((1 + GrdFixedPosition.DefaultRowHeight) *
                                    (ARow - GrdFixedPosition.TopRow + 1));
              lName := Trim(GrdFixedPosition.Cells[0, ARow]);
              CbxPosition.ItemIndex := CbxPosition.Items.IndexOf(lName);
              CbxPosition.Visible := TRUE;
              if (GrdFixedPosition.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxPosition.ValidationError   := GrdFixedPosition.ValidationError[ACol, ARow, gveCellContext];
                CbxPosition.InValidationError := TRUE;
                CbxPosition.ShowErrorState(TRUE);
              end
              else
              begin
                CbxPosition.ValidationError   := '';
                CbxPosition.InValidationError := FALSE;
                CbxPosition.ShowErrorState(FALSE);
              end;
            end
            else
            if (ACol = 1) then
            begin
              CbxSubSystem.Left := 1 + GrdFixedPosition.Left + GrdFixedPosition.ColWidths[0];
              CbxSubSystem.Top  := 2 + GrdFixedPosition.Top +
                                   ((1 + GrdFixedPosition.DefaultRowHeight) *
                                    (ARow - GrdFixedPosition.TopRow + 1));
              lName := Trim(GrdFixedPosition.Cells[1, ARow]);
              CbxSubSystem.ItemIndex := CbxSubSystem.Items.IndexOf(lName);
              CbxSubSystem.Visible := TRUE;
              if (GrdFixedPosition.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxSubSystem.ValidationError   := GrdFixedPosition.ValidationError[ACol, ARow, gveCellContext];
                CbxSubSystem.InValidationError := TRUE;
                CbxSubSystem.ShowErrorState(TRUE);
              end
              else
              begin
                CbxSubSystem.ValidationError   := '';
                CbxSubSystem.InValidationError := FALSE;
                CbxSubSystem.ShowErrorState(FALSE);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnGrdSpecificOrderSelectCell (Sender        : TObject;
                                                                    ACol, ARow    : Integer;
                                                                    var CanSelect : Boolean);
const OPNAME = 'TFMSupportStrategyValidator.OnGrdSpecificOrderSelectCell';
var
  lAllocDef : IAllocationDefinition;
  lName     : string;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMSupportStrategyDialog do
        begin
          if (ARow > 0) then
          begin
            if (ACol = 0) then
            begin
              CbxBeforeSubSystem.Left := 1 + GrdSpecificOrder.Left;
              CbxBeforeSubSystem.Top  := 2 + GrdSpecificOrder.Top +
                                         ((1 + GrdSpecificOrder.DefaultRowHeight) *
                                          (ARow - GrdSpecificOrder.TopRow + 1));
              lName := Trim(GrdSpecificOrder.Cells[0, ARow]);
              CbxBeforeSubSystem.ItemIndex := CbxBeforeSubSystem.Items.IndexOf(lName);
              CbxBeforeSubSystem.Visible := TRUE;
              if (GrdSpecificOrder.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxBeforeSubSystem.ValidationError   := GrdSpecificOrder.ValidationError[ACol, ARow, gveCellContext];
                CbxBeforeSubSystem.InValidationError := TRUE;
                CbxBeforeSubSystem.ShowErrorState(TRUE);
              end
              else
              begin
                CbxBeforeSubSystem.ValidationError   := '';
                CbxBeforeSubSystem.InValidationError := FALSE;
                CbxBeforeSubSystem.ShowErrorState(FALSE);
              end;
            end
            else
            if (ACol = 1) then
            begin
              CbxAfterSubSystem.Left := 1 + GrdSpecificOrder.Left + GrdSpecificOrder.ColWidths[0];
              CbxAfterSubSystem.Top  := 2 + GrdSpecificOrder.Top +
                                        ((1 + GrdSpecificOrder.DefaultRowHeight) *
                                         (ARow - GrdSpecificOrder.TopRow + 1));
              lName := Trim(GrdSpecificOrder.Cells[1, ARow]);
              CbxAfterSubSystem.ItemIndex := CbxAfterSubSystem.Items.IndexOf(lName);
              CbxAfterSubSystem.Visible := TRUE;
              if (GrdSpecificOrder.InValidationError[ACol, ARow, gveCellContext]) then
              begin
                CbxAfterSubSystem.ValidationError   := GrdSpecificOrder.ValidationError[ACol, ARow, gveCellContext];
                CbxAfterSubSystem.InValidationError := TRUE;
                CbxAfterSubSystem.ShowErrorState(TRUE);
              end
              else
              begin
                CbxAfterSubSystem.ValidationError   := '';
                CbxAfterSubSystem.InValidationError := FALSE;
                CbxAfterSubSystem.ShowErrorState(FALSE);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnGrdFixedPositionTopLeftChanged (Sender : TObject);
const OPNAME = 'TFMSupportStrategyValidator.OnGrdFixedPositionTopLeftChanged';
begin
  try
    FMSupportStrategyDialog.CbxPosition.Visible := FALSE;
    FMSupportStrategyDialog.CbxSubSystem.Visible := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnGrdSpecificOrderTopLeftChanged (Sender : TObject);
const OPNAME = 'TFMSupportStrategyValidator.OnGrdSpecificOrderTopLeftChanged';
begin
  try
    FMSupportStrategyDialog.CbxBeforeSubSystem.Visible := FALSE;
    FMSupportStrategyDialog.CbxAfterSubSystem.Visible := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyValidator.SaveState: boolean;
const OPNAME = 'TFMSupportStrategyValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyValidator.FMSupportStrategyDialog:TFMSupportStrategyDialog;
const OPNAME = 'TFMSupportStrategyValidator.FMSupportStrategyDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMSupportStrategyDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMSupportStrategyValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'NrOfSubSystems') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportStrategyValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMSupportStrategyValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoMoveSubSystemUp (Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.DoMoveSubSystemUp';
var
  lAllocDef      : IAllocationDefinition;
  lSubSystemID   : integer;
  lSubSystem     : ISubSystem;
  lOther         : ISubSystem;
  lIndex         : integer;
  lDone          : boolean;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        lSubSystemID := Integer(GrdSubSystemOrder.Objects[0, GrdSubSystemOrder.Row]);
        lSubSystem   := lAllocDef.SubSystemByID[lSubSystemID];
        if (lSubSystem <> nil) then
        begin
          lDone  := FALSE;
          lIndex := 0;
          while ((NOT lDone) AND (lIndex < lAllocDef.NrOfSubSystems)) do
          begin
            lOther := lAllocDef.SubSystemByIndex[lIndex];
            if (lOther <> lSubSystem) AND (lOther.Order = lSubSystem.Order - 1) then
            begin
              lDone := TRUE;
              lOther.Order := lOther.Order + 1;
              lSubSystem.Order := lSubSystem.Order - 1;
              GrdSubSystemOrder.Row := lSubSystem.Order;
            end
            else
              lIndex := lIndex + 1;
          end;
          RePopulateDataViewer;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoMoveSubSystemDown (Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.DoMoveSubSystemDown';
var
  lAllocDef      : IAllocationDefinition;
  lSubSystemID   : integer;
  lSubSystem     : ISubSystem;
  lOther         : ISubSystem;
  lIndex         : integer;
  lDone          : boolean;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        lSubSystemID := Integer(GrdSubSystemOrder.Objects[0, GrdSubSystemOrder.Row]);
        lSubSystem   := lAllocDef.SubSystemByID[lSubSystemID];
        if (lSubSystem <> nil) then
        begin
          lDone  := FALSE;
          lIndex := 0;
          while ((NOT lDone) AND (lIndex < lAllocDef.NrOfSubSystems)) do
          begin
            lOther := lAllocDef.SubSystemByIndex[lIndex];
            if (lOther <> lSubSystem) AND (lOther.Order = lSubSystem.Order + 1) then
            begin
              lDone := TRUE;
              lOther.Order := lOther.Order - 1;
              lSubSystem.Order := lSubSystem.Order + 1;
              GrdSubSystemOrder.Row := lSubSystem.Order;
            end
            else
              lIndex := lIndex + 1;
          end;
        end;
      end;
      RePopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoAddFixedPosition (Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.DoAddFixedPosition';
var
  lAllocDef      : IAllocationDefinition;
  lFixedPosition : IFixedPosition;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lFixedPosition   := lAllocDef.NewFixedPosition;
      RePopulateDataViewer;
      DoContextValidation(dvtFixedPositionNr);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoDeleteFixedPosition (Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.DoDeleteFixedPosition';
var
  lAllocDef       : IAllocationDefinition;
  lIndex          : integer;
begin
  try
    lIndex := FMSupportStrategyDialog.GrdFixedPosition.Row-1;
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (lIndex >= 0) then
    begin
      if (lAllocDef.RemoveFixedPosition(lIndex)) then
      begin
        RePopulateDataViewer;
        DoContextValidation(dvtFixedPositionNr);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoAddSpecificOrder (Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.DoAddSpecificOrder';
var
  lAllocDef      : IAllocationDefinition;
  lSpecificOrder : ISpecificOrder;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lSpecificOrder   := lAllocDef.NewSpecificOrder;
      RePopulateDataViewer;
      DoContextValidation(dvtBeforeSubSystemID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoDeleteSpecificOrder (Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.DoDeleteSpecificOrder';
var
  lAllocDef       : IAllocationDefinition;
  lIndex          : integer;
begin
  try
    lIndex := FMSupportStrategyDialog.grdSpecificOrder.Row-1;
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (lIndex >= 0) then
    begin
      if (lAllocDef.RemoveSpecificOrder(lIndex)) then
      begin
        RePopulateDataViewer;
        DoContextValidation(dvtBeforeSubSystemID);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnEditControltExit (Sender : TObject);
const OPNAME = 'TFMSupportStrategyValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMSupportStrategyDialog do
    begin
      if ((Sender = CbxSupportStrategy) AND
          (CbxSupportStrategy.HasValueChanged)) then
        UpdateSupportStrategy
      else
      if ((Sender = CbxPosition) AND
          (CbxPosition.HasValueChanged)) then
        UpdateFixedPosition
      else
      if ((Sender = CbxSubSystem) AND
          (CbxSubSystem.HasValueChanged)) then
        UpdateFixedSubSystemID
      else
      if ((Sender = CbxBeforeSubSystem) AND
          (CbxBeforeSubSystem.HasValueChanged)) then
        UpdateBeforeSubSystemID
      else
      if ((Sender = CbxAfterSubSystem) AND
          (CbxAfterSubSystem.HasValueChanged)) then
        UpdateAfterSubSystemID;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.OnRgpBalancingOptionClick(Sender: TObject);
const OPNAME = 'TFMSupportStrategyValidator.OnRgpBalancingOptionClick';
begin
  try
    if(FMSupportStrategyDialog.RgpBalancingOption.HasValueChanged) then
      UpdateBalancingOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.UpdateSupportStrategy;
const OPNAME = 'TFMSupportStrategyValidator.UpdateSupportStrategy';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lValue     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        if (CbxSupportStrategy.ItemIndex >= 0) then
        begin
          lValue := CbxSupportStrategy.ItemIndex + 1;
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxSupportStrategy.FieldProperty.FieldName,
              IntToStr(lValue), lMessage)) then
          begin
            lAllocDef.SupportStrategy := lValue;
            CbxSupportStrategy.SetFieldIndex(lAllocDef.SupportStrategy-1);
            DoContextValidation(dvtSupportStrategy);
          end
          else
            CbxSupportStrategy.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.UpdateBalancingOption;
const OPNAME = 'TFMSupportStrategyValidator.UpdateBalancingOption';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lValue     : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        if (lAllocDef.BalancingOption <> RgpBalancingOption.ItemIndex) then
        begin
          lValue := IntToStr(RgpBalancingOption.ItemIndex);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              RgpBalancingOption.FieldProperty.FieldName, lValue, lMessage)) then
          begin
            RgpBalancingOption.ValidationError := lMessage;
            lAllocDef.BalancingOption := RgpBalancingOption.ItemIndex;
            RgpBalancingOption.ItemIndex := lAllocDef.BalancingOption;
            DoContextValidation(dvtBalancingOption);
          end
          else
            RgpBalancingOption.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.UpdateFixedPosition;
const OPNAME = 'TFMSupportStrategyValidator.UpdateFixedPosition';
var
  lAllocDef       : IAllocationDefinition;
  lMessage        : string;
  lStrValue       : string;
  lIntValue       : integer;
  lRow            : integer;
  lFixedPosition  : IFixedPosition;
  lFixedPosID     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        lIntValue := 0;
        lRow := GrdFixedPosition.Row;
        lFixedPosID := Integer(GrdFixedPosition.Objects[0, lRow]);
        lFixedPosition := lAllocDef.FixedPositionByID[lFixedPosID];
        if (lFixedPosition <> nil) then
        begin
          if (CbxPosition.ItemIndex >= 0) then
            lIntValue := StrToInt(CbxPosition.Text);
          lStrValue := CbxPosition.Text;
          CbxPosition.ValidationError := '';
          GrdFixedPosition.ValidationError[0, lRow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxPosition.FieldProperty.FieldName, lStrValue, lMessage, lRow)) then
          begin
            GrdFixedPosition.ValidationError[0, lRow, gveCellContext] := lMessage;
            lFixedPosition.FixedPositionNr := lIntValue;
            DoContextValidation(dvtFixedPositionNr);
            RePopulateDataViewer;
          end
          else
          begin
            CbxPosition.ValidationError := lMessage;
            GrdFixedPosition.ValidationError[0, lRow, gveCellContext] := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.UpdateFixedSubSystemID;
const OPNAME = 'TFMSupportStrategyValidator.UpdateFixedSubSystemID';
var
  lAllocDef       : IAllocationDefinition;
  lMessage        : string;
  lStrValue       : string;
  lSubSystemID    : integer;
  lRow            : integer;
  lFixedPosition  : IFixedPosition;
  lFixedPosID     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        lSubSystemID := 0;
        lRow := GrdFixedPosition.Row;
        lFixedPosID := Integer(GrdFixedPosition.Objects[0, lRow]);
        lFixedPosition := lAllocDef.FixedPositionByID[lFixedPosID];
        if (lFixedPosition <> nil) then
        begin
          if (CbxSubSystem.ItemIndex >= 0) then
            lSubSystemID := Integer(CbxSubSystem.Items.Objects[CbxSubSystem.ItemIndex]);
          lStrValue := IntToStr(lSubSystemID);
          CbxSubSystem.ValidationError := '';
          GrdFixedPosition.ValidationError[1, lRow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxSubSystem.FieldProperty.FieldName, lStrValue, lMessage, lRow)) then
          begin
            GrdFixedPosition.ValidationError[1, lRow, gveCellContext] := lMessage;
            lFixedPosition.FixedPosSubSystemID := lSubSystemID;
            DoContextValidation(dvtFixedPosSubSystemID);
            RePopulateDataViewer;
          end
          else
          begin
            CbxSubSystem.ValidationError := lMessage;
            GrdFixedPosition.ValidationError[1, lRow, gveCellContext] := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.UpdateBeforeSubSystemID;
const OPNAME = 'TFMSupportStrategyValidator.UpdateBeforeSubSystemID';
var
  lAllocDef       : IAllocationDefinition;
  lMessage        : string;
  lStrValue       : string;
  lSubSystemID    : integer;
  lRow            : integer;
  lSpecificOrdID  : integer;
  lSpecificOrder  : ISpecificOrder;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        lSubSystemID := 0;
        lRow := GrdSpecificOrder.Row;
        lSpecificOrdID := Integer(GrdSpecificOrder.Objects[0, lRow]);
        lSpecificOrder := lAllocDef.SpecificOrderByID[lSpecificOrdID];
        if (lSpecificOrder <> nil) then
        begin
          if (CbxBeforeSubSystem.ItemIndex >= 0) then
            lSubSystemID := Integer(CbxBeforeSubSystem.Items.Objects[CbxBeforeSubSystem.ItemIndex]);
          lStrValue := IntToStr(lSubSystemID);
          CbxBeforeSubSystem.ValidationError := '';
          GrdSpecificOrder.ValidationError[0, lRow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxBeforeSubSystem.FieldProperty.FieldName, lStrValue, lMessage, lRow)) then
          begin
            GrdSpecificOrder.ValidationError[0, lRow, gveCellContext] := lMessage;
            lSpecificOrder.BeforeSubSystemID := lSubSystemID;
            DoContextValidation(dvtBeforeSubSystemID);
            RePopulateDataViewer;
          end
          else
          begin
            CbxBeforeSubSystem.ValidationError := lMessage;
            GrdSpecificOrder.ValidationError[0, lRow, gveCellContext] := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.UpdateAfterSubSystemID;
const OPNAME = 'TFMSupportStrategyValidator.UpdateAfterSubSystemID';
var
  lAllocDef       : IAllocationDefinition;
  lMessage        : string;
  lStrValue       : string;
  lSubSystemID    : integer;
  lRow            : integer;
  lSpecificOrdID  : integer;
  lSpecificOrder  : ISpecificOrder;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMSupportStrategyDialog do
      begin
        lSubSystemID := 0;
        lRow := GrdSpecificOrder.Row;
        lSpecificOrdID := Integer(GrdSpecificOrder.Objects[0, lRow]);
        lSpecificOrder := lAllocDef.SpecificOrderByID[lSpecificOrdID];
        if (lSpecificOrder <> nil) then
        begin
          if (CbxAfterSubSystem.ItemIndex >= 0) then
            lSubSystemID := Integer(CbxAfterSubSystem.Items.Objects[CbxAfterSubSystem.ItemIndex]);
          lStrValue := IntToStr(lSubSystemID);
          CbxAfterSubSystem.ValidationError := '';
          GrdSpecificOrder.ValidationError[1, lRow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxAfterSubSystem.FieldProperty.FieldName, lStrValue, lMessage, lRow)) then
          begin
            GrdSpecificOrder.ValidationError[1, lRow, gveCellContext] := lMessage;
            lSpecificOrder.AfterSubSystemID := lSubSystemID;
            DoContextValidation(dvtAfterSubSystemID);
            RePopulateDataViewer;
          end
          else
          begin
            CbxAfterSubSystem.ValidationError := lMessage;
            GrdSpecificOrder.ValidationError[1, lRow, gveCellContext] := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TFMSupportStrategyValidator.DoContextValidation';
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
        if (AValidationType in [dvtSupportStrategyAll, dvtSupportStrategy]) then
          ValidateSupportStrategy(lAllocDef);
        if (AValidationType in [dvtSupportStrategyAll, dvtBalancingOption]) then
          ValidateBalancingOption(lAllocDef);
        if (AValidationType in [dvtSupportStrategyAll, dvtFixedPositionNr, dvtFixedPosSubSystemID]) then
          ValidateFixedPosition(lAllocDef);
        if (AValidationType in [dvtSupportStrategyAll, dvtBeforeSubSystemID, dvtAfterSubSystemID]) then
          ValidateSpecificOrder(lAllocDef);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.ValidateSupportStrategy (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMSupportStrategyValidator.ValidateSupportStrategy';
begin
  try
    with FMSupportStrategyDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'SupportStrategy')) then
      begin
        CbxSupportStrategy.InValidationError := TRUE;
        CbxSupportStrategy.ValidationError := FErrorMessage;
        CbxSupportStrategy.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSupportStrategy.InValidationError := FALSE;
        CbxSupportStrategy.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.ValidateBalancingOption (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMSupportStrategyValidator.ValidateBalancingOption';
begin
  try
    with FMSupportStrategyDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'BalancingOption')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        RgpBalancingOption.ValidationError := FErrorMessage;
        RgpBalancingOption.InValidationError := TRUE;
        RgpBalancingOption.ShowErrorState(TRUE);
      end
      else
      begin
        RgpBalancingOption.ValidationError := '';
        RgpBalancingOption.InValidationError := FALSE;
        RgpBalancingOption.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.ValidateFixedPosition (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMSupportStrategyValidator.ValidateFixedPosition';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCol           : integer;
  lCount         : integer;
  lIndexStr      : string;
  lErrorMessages : TStringList;
  lPos           : integer;
begin
  try
    with FMSupportStrategyDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to AAllocDef.NrInFixedPosition do
        for lCol := 1 to 2 do
          GrdFixedPosition.ValidationError[lCol, lRow, gveCellContext] := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'FixedPosition')) then
      begin
        lErrorIndexes  := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
          for lCount := 0 to lErrorIndexes.Count - 1 do
          begin
            lIndexStr := lErrorIndexes.Strings[lCount];
            lPos      := Pos(',', lIndexStr);
            lRow      := StrToInt(Copy(lIndexStr, 1, lPos-1));
            lCol      := StrToInt(Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos));
            GrdFixedPosition.ValidationError[lCol, lRow, gveCellContext] := lErrorMessages.Strings[lCount];
          end;
          FAllErrorMessages.AddStrings(lErrorMessages);
        finally
          FreeAndNil(lErrorIndexes);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportStrategyValidator.ValidateSpecificOrder (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMSupportStrategyValidator.ValidateSpecificOrder';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCol           : integer;
  lCount         : integer;
  lIndexStr      : string;
  lErrorMessages : TStringList;
  lPos           : integer;
begin
  try
    with FMSupportStrategyDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to AAllocDef.NrInSpecificOrder do
        for lCol := 1 to 2 do
          GrdSpecificOrder.ValidationError[lCol, lRow, gveCellContext] := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'SpecificOrder')) then
      begin
        lErrorIndexes  := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
          for lCount := 0 to lErrorIndexes.Count - 1 do
          begin
            lIndexStr := lErrorIndexes.Strings[lCount];
            lPos      := Pos(',', lIndexStr);
            lRow      := StrToInt(Copy(lIndexStr, 1, lPos-1));
            lCol      := StrToInt(Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos));
            GrdSpecificOrder.ValidationError[lCol, lRow, gveCellContext] := lErrorMessages.Strings[lCount];
          end;
          FAllErrorMessages.AddStrings(lErrorMessages);
        finally
          FreeAndNil(lErrorIndexes);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

