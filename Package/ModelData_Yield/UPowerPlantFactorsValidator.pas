{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantFactorsValidator.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantFactorsValidator;

interface

uses
  Classes,
  Types,
  VCL.Grids,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UPowerPlantFactorsDialog;

type
  TPowerPlantFactorsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
                                 Rect: TRect; State: TGridDrawState);
    procedure OnSelectFactorsGridCell (Sender: TObject;
                                       ACol, ARow: Integer;
                                       var CanSelect: Boolean);
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure OnAfterPasteGridData(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure PopulateFactorsGrid;
    procedure UpdateCombinedEfficiency;
    procedure UpdateDesignHead;
    procedure UpdateMaxNetHead;
    procedure UpdateMinNetHead;
    procedure UpdateEfficiencyFactor(AIndex : integer;
                                     AValue : string);
    procedure UpdateNetHeadFactor(AIndex : integer;
                                  AValue : string);
    procedure ValidateCombinedEfficiency (AFeature : IPowerPlant);
    procedure ValidateDesignHead (AFeature : IPowerPlant);
    procedure ValidateMaximumNetHead (AFeature : IPowerPlant);
    procedure ValidateMinimumNetHead (AFeature : IPowerPlant);
    procedure ValidateEfficiencyFactors (AFeature : IPowerPlant);
    procedure ValidateNetHeadFactors (AFeature : IPowerPlant);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function PowerPlantFactorsDialog : TPowerPlantFactorsDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TPowerPlantFactorsValidator                                                *}
{******************************************************************************}

procedure TPowerPlantFactorsValidator.CreateMemberObjects;
const OPNAME = 'TPowerPlantFactorsValidator.CreateMemberObjects';
var
  lPanel : TPowerPlantFactorsDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TPowerPlantFactorsDialog.Create(FPanelOwner,FAppModules);
    lPanel := PowerPlantFactorsDialog;
    with lPanel do
    begin
      CombinedEfficiencyEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('PowerEfficiency');
      CombinedEfficiencyEdit.OnEnter        := OnEditControlEnter;
      CombinedEfficiencyEdit.OnExit         := OnEditControltExit;

      DesignHeadEdit.FieldProperty          := FAppModules.FieldProperties.FieldProperty('DesignHead');
      DesignHeadEdit.OnEnter                := OnEditControlEnter;
      DesignHeadEdit.OnExit                 := OnEditControltExit;

      MaxNetHeadEdit.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MaxNetHead');
      MaxNetHeadEdit.OnEnter                := OnEditControlEnter;
      MaxNetHeadEdit.OnExit                 := OnEditControltExit;

      MinNetHeadEdit.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MinNetHead');
      MinNetHeadEdit.OnEnter                := OnEditControlEnter;
      MinNetHeadEdit.OnExit                 := OnEditControltExit;

      FactorsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      FactorsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('EfficiencyFactor'));
      FactorsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('EfficiencyFactor'));
      FactorsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NetHeadFactors'));
      FactorsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NetHeadFactors'));
      FactorsGrid.OnDrawCell                     := StringGridDrawCell;
      FactorsGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      FactorsGrid.OnColEnter                     := OnStringGridColEnter;
      FactorsGrid.OnSelectCell                   := OnSelectFactorsGridCell;
      FactorsGrid.OnEnter                        := OnEditControlEnter;
      FactorsGrid.ShowGridPopupMenu              := True;
      FactorsGrid.AllowPasteFromExcel            := True;
      FactorsGrid.OnAfterPasteColumnData         := Self.OnAfterPasteColumnData;
      FactorsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;
      FactorsGrid.OnPasteFromExcel               := Self.OnAfterPasteGridData;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.DestroyMemberObjects;
const OPNAME = 'TPowerPlantFactorsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.Initialise: boolean;
const OPNAME = 'TPowerPlantFactorsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantFactorsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.EfficiencyNetHeadFactorCurve');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ClearDataViewer;
const OPNAME = 'TPowerPlantFactorsValidator.ClearDataViewer';
var
  lPanel : TPowerPlantFactorsDialog;
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := PowerPlantFactorsDialog;
    with lPanel do
    begin
      FeatureNameLabel.Caption    := '';
      CombinedEfficiencyEdit.Text := '-1.0';
      DesignHeadEdit.Text         := '-1.0';
      MaxNetHeadEdit.Text         := '-1.0';
      MinNetHeadEdit.Text         := '-1.0';
      for lIndex := 1 to FactorsGrid.RowCount - 1 do
      begin
        FactorsGrid.Cells[0, lIndex] := '';
        FactorsGrid.Cells[1, lIndex] := '-1.0';
        FactorsGrid.Cells[2, lIndex] := '-1.0';
        FactorsGrid.Cells[3, lIndex] := '-1.0';
        FactorsGrid.Cells[4, lIndex] := '-1.0';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.PopulateDataViewer;
const OPNAME = 'TPowerPlantFactorsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtPowerPlantFactors);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.RePopulateDataViewer;
const OPNAME = 'TPowerPlantFactorsValidator.RePopulateDataViewer';
var
  lPowerPlant    : IPowerPlant;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantFactorsDialog do
        begin
          FeatureNameLabel.Caption := lPowerPlant.FeatureName;

          lFieldIndex    := '';
          lFieldProperty := CombinedEfficiencyEdit.FieldProperty;
          lKeyValues     := lPowerPlant.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          CombinedEfficiencyEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          CombinedEfficiencyEdit.SetFieldValue(lPowerPlant.CombinedEfficiency);

          lFieldProperty := DesignHeadEdit.FieldProperty;
          DesignHeadEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          DesignHeadEdit.SetFieldValue(lPowerPlant.DesignHead);

          lFieldProperty := MaxNetHeadEdit.FieldProperty;
          MaxNetHeadEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          MaxNetHeadEdit.SetFieldValue(lPowerPlant.MaximumNetHead);

          lFieldProperty := MinNetHeadEdit.FieldProperty;
          MinNetHeadEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          MinNetHeadEdit.SetFieldValue(lPowerPlant.MinimumNetHead);
          PopulateFactorsGrid;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.PopulateFactorsGrid;
const OPNAME = 'TPowerPlantFactorsValidator.PopulateFactorsGrid';
var
  lIndex      : integer;
  lValue      : double;
  lEfficiency : double;
  lDesignHead : double;
  lPowerPlant : IPowerPlant;
  lFieldProp1 : TAbstractFieldProperty;
  lFieldProp2 : TAbstractFieldProperty;
  lKeyValues  : string;
  lFieldIndex : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantFactorsDialog do
        begin
          lEfficiency := lPowerPlant.CombinedEfficiency;
          lDesignHead := lPowerPlant.DesignHead;
          lFieldProp1 := FactorsGrid.FieldProperty(1);
          lFieldProp2 := FactorsGrid.FieldProperty(3);
          for lIndex := 1 to FactorsGrid.RowCount - 1 do
          begin
            lFieldIndex := IntToStr(lIndex);
            lKeyValues  := lPowerPlant.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
            FactorsGrid.HasMetaData[1, lIndex] :=
              FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
            lKeyValues  := lPowerPlant.GetKeyValues(lFieldProp2.FieldName, lFieldIndex);
            FactorsGrid.HasMetaData[3, lIndex] :=
              FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;

            FactorsGrid.Cells[0, lIndex] := IntToStr(lIndex);
            lValue := lPowerPlant.EfficiencyFactorByIndex[lIndex];
            if (lValue <> NullFloat) then
            begin
              FactorsGrid.Cells[1, lIndex] := Format(lFieldProp1.FormatStringGrid {'%6.3f'}, [lValue]);
              lValue := lValue * lEfficiency;
              FactorsGrid.Cells[2, lIndex] := Format(lFieldProp1.FormatStringGrid {'%6.3f'}, [lValue]);
            end
            else
            begin
              FactorsGrid.Cells[1, lIndex] := '';
              FactorsGrid.Cells[2, lIndex] := '';
            end;
            lValue := lPowerPlant.NetHeadFactorByIndex[lIndex];
            if (lValue <> NullFloat) then
            begin
              FactorsGrid.Cells[3, lIndex] := Format(lFieldProp1.FormatStringGrid {'%6.3f'}, [lValue]);
              lValue := lValue * lDesignHead;
              FactorsGrid.Cells[4, lIndex] := Format(lFieldProp1.FormatStringGrid {'%6.3f'}, [lValue]);
            end
            else
            begin
              FactorsGrid.Cells[3, lIndex] := '';
              FactorsGrid.Cells[4, lIndex] := '';
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.SaveState: boolean;
const OPNAME = 'TPowerPlantFactorsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.PowerPlantFactorsDialog : TPowerPlantFactorsDialog;
const OPNAME = 'TPowerPlantFactorsValidator.PowerPlantFactorsDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPowerPlantFactorsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPowerPlantFactorsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AFieldName = 'PowerPlantName') then
    begin
      if(PowerPlantFactorsDialog.FeatureNameLabel.Caption =  AOldValue) then
        PowerPlantFactorsDialog.FeatureNameLabel.Caption := ANewValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.StudyHasChanged: boolean;
const OPNAME = 'TPowerPlantFactorsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPowerPlantFactorsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPowerPlantFactorsValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with PowerPlantFactorsDialog do
    begin
      if ((Sender = CombinedEfficiencyEdit) AND
          (CombinedEfficiencyEdit.HasValueChanged)) then
        UpdateCombinedEfficiency
      else
      if ((Sender = DesignHeadEdit) AND
          (DesignHeadEdit.HasValueChanged)) then
        UpdateDesignHead
      else
      if ((Sender = MaxNetHeadEdit) AND
          (MaxNetHeadEdit.HasValueChanged)) then
        UpdateMaxNetHead
      else
      if ((Sender = MinNetHeadEdit) AND
          (MinNetHeadEdit.HasValueChanged)) then
        UpdateMinNetHead;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.UpdateCombinedEfficiency;
const OPNAME = 'TPowerPlantFactorsValidator.UpdateCombinedEfficiency';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantFactorsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CombinedEfficiencyEdit.FieldProperty.FieldName,
            CombinedEfficiencyEdit.Text,lMessage)) then
        begin
          CombinedEfficiencyEdit.FieldValidationError := lMessage;
          lPowerPlant.CombinedEfficiency := StrToFloat(Trim(CombinedEfficiencyEdit.Text));
          CombinedEfficiencyEdit.SetFieldValue(lPowerPlant.CombinedEfficiency);
          RePopulateDataViewer;
          DoContextValidation(dvtPowerPlantCombinedEfficiency);
        end
        else
          CombinedEfficiencyEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.UpdateDesignHead;
const OPNAME = 'TPowerPlantFactorsValidator.UpdateDesignHead';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantFactorsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DesignHeadEdit.FieldProperty.FieldName,
            DesignHeadEdit.Text,lMessage)) then
        begin
          DesignHeadEdit.FieldValidationError := lMessage;
          lPowerPlant.DesignHead := StrToFloat(Trim(DesignHeadEdit.Text));
          DesignHeadEdit.SetFieldValue(lPowerPlant.DesignHead);
          RePopulateDataViewer;
          DoContextValidation(dvtPowerPlantDesignHead);
        end
        else
          DesignHeadEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.UpdateMaxNetHead;
const OPNAME = 'TPowerPlantFactorsValidator.UpdateMaxNetHead';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantFactorsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            MaxNetHeadEdit.FieldProperty.FieldName,
            MaxNetHeadEdit.Text,lMessage)) then
        begin
          MaxNetHeadEdit.FieldValidationError := lMessage;
          lPowerPlant.MaximumNetHead := StrToFloat(Trim(MaxNetHeadEdit.Text));
          MaxNetHeadEdit.SetFieldValue(lPowerPlant.MaximumNetHead);
          DoContextValidation(dvtPowerPlantMaximumNetHead);
        end
        else
          MaxNetHeadEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.UpdateMinNetHead;
const OPNAME = 'TPowerPlantFactorsValidator.UpdateMinNetHead';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantFactorsDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            MinNetHeadEdit.FieldProperty.FieldName,
            MinNetHeadEdit.Text,lMessage)) then
        begin
          MinNetHeadEdit.FieldValidationError := lMessage;
          lPowerPlant.MinimumNetHead := StrToFloat(Trim(MinNetHeadEdit.Text));
          MinNetHeadEdit.SetFieldValue(lPowerPlant.MinimumNetHead);
          DoContextValidation(dvtPowerPlantMinimumNetHead);
        end
        else
          MinNetHeadEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.StringGridDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
const OPNAME = 'TPowerPlantFactorsValidator.StringGridDrawCell';
var
  lGrid : TStringGrid;
begin
  try
    lGrid := TStringGrid(Sender);
    with lGrid do
    begin
      if ((ARow > 0) AND ((ACol = 1) OR (ACol = 3))) then
        Canvas.Brush.Color := clWhite
      else
        Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect);
      if ((ARow < FixedRows) OR (ACol < FixedCols)) then
      begin
        with Canvas do
        begin
          Pen.Style   := psSolid;
          Pen.Width   := 1;
          Pen.Color   := clWhite;
          MoveTo(Rect.Left, Rect.Bottom-1);
          LineTo(Rect.Left, Rect.Top);
          LineTo(Rect.Right-1, Rect.Top);
          Pen.Color   := clBtnShadow;
          MoveTo(Rect.Left, Rect.Bottom-1);
          LineTo(Rect.Right-1, Rect.Bottom-1);
          LineTo(Rect.Right-1, Rect.Top);
        end;
      end;
      Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, Cells[ACol, ARow]);
      if gdFocused in State then
        Canvas.DrawFocusRect(Rect);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPowerPlantFactorsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with PowerPlantFactorsDialog do
    begin
      if (FactorsGrid = ASender) then
      begin
        if (ACol = 1) then
          UpdateEfficiencyFactor(ARow, Trim(FactorsGrid.Cells[ACol, ARow]))
        else
        if (ACol = 3) then
          UpdateNetHeadFactor(ARow, Trim(FactorsGrid.Cells[ACol, ARow]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.UpdateEfficiencyFactor(AIndex : integer;
                                                             AValue : string);
const OPNAME = 'TPowerPlantFactorsValidator.UpdateEfficiencyFactor';
var
  lPowerPlant : IPowerPlant;
  lValue      : double;
  lMessage    : string;
  lCount      : integer;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantFactorsDialog do
      begin
        FactorsGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'EfficiencyFactor', AValue,lMessage, AIndex)) then
        begin
          lPowerPlant.EfficiencyFactorByIndex[AIndex] := lValue;
          lCount := lPowerPlant.NetHeadEfficiencyCount;
          lPowerPlant.NetHeadEfficiencyCount := lCount;
          RePopulateDataViewer;
          DoContextValidation(dvtPowerPlantEfficiencyFactors);
        end
        else
          FactorsGrid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.UpdateNetHeadFactor(AIndex : integer;
                                                          AValue : string);
const OPNAME = 'TPowerPlantFactorsValidator.UpdateNetHeadFactor';
var
  lPowerPlant : IPowerPlant;
  lValue      : double;
  lMessage    : string;
  lCount      : integer;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantFactorsDialog do
      begin
        FactorsGrid.ValidationError[3, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'NetHeadFactors', AValue,lMessage, AIndex)) then
        begin
          lPowerPlant.NetHeadFactorByIndex[AIndex] := lValue;
          lCount := lPowerPlant.NetHeadEfficiencyCount;
          lPowerPlant.NetHeadEfficiencyCount := lCount;
          RePopulateDataViewer;
          DoContextValidation(dvtPowerPlantNetHeadFactors);
        end
        else
          FactorsGrid.ValidationError[3, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.OnSelectFactorsGridCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TPowerPlantFactorsValidator.OnSelectFactorsGridCell';
begin
  CanSelect := (ACol = 1) OR (ACol = 3);
end;

procedure TPowerPlantFactorsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TPowerPlantFactorsValidator.DoContextValidation';
var
  lFeature     : IPowerPlant;
  lFeatureList : IPowerPlantList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PowerPlantList;
      lFeature     := lFeatureList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtPowerPlantFactors, dvtPowerPlantCombinedEfficiency]) then
          ValidateCombinedEfficiency(lFeature);
        if (AValidationType in [dvtPowerPlantFactors, dvtPowerPlantDesignHead]) then
          ValidateDesignHead(lFeature);
        if (AValidationType in [dvtPowerPlantFactors, dvtPowerPlantMaximumNetHead]) then
          ValidateMaximumNetHead(lFeature);
        if (AValidationType in [dvtPowerPlantFactors, dvtPowerPlantMinimumNetHead]) then
          ValidateMinimumNetHead(lFeature);
        if (AValidationType in [dvtPowerPlantFactors, dvtPowerPlantEfficiencyFactors]) then
          ValidateEfficiencyFactors(lFeature);
        if (AValidationType in [dvtPowerPlantFactors, dvtPowerPlantNetHeadFactors]) then
          ValidateNetHeadFactors(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TPowerPlantFactorsValidator.DetermineWizardStatus';
var
  lFeature       : IPowerPlant;
  lFeatureList   : IPowerPlantList;
  lFieldProperty : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PowerPlantList;
      lFeature := lFeatureList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtPowerPlantFactors);
        lFieldProperty := FAppModules.FieldProperties.FieldProperty('EfficiencyFactor');
        lNotZero := FALSE;
        lIndex := lFieldProperty.ArrayLow;
        if ((lFeature.DesignHead > 0) OR (lFeature.CombinedEfficiency > 0) OR
            (lFeature.MaximumNetHead > 0) OR (lFeature.MinimumNetHead > 0)) then
          lNotZero := TRUE;
        while ((NOT lNotZero) AND (lIndex <= lFeature.NetHeadEfficiencyCount)) do
        begin
          if ((lFeature.EfficiencyFactorByIndex[lIndex] > 0) OR
              (lFeature.NetHeadFactorByIndex[lIndex] > 0)) then
            lNotZero := TRUE
          else
            lIndex := lIndex + 1;     
        end;
        if (lNotZero) then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ValidateCombinedEfficiency (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantFactorsValidator.ValidateCombinedEfficiency';
begin
  try
    with PowerPlantFactorsDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'PowerEfficiency')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      CombinedEfficiencyEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ValidateDesignHead (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantFactorsValidator.ValidateDesignHead';
begin
  try
    with PowerPlantFactorsDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'DesignHead')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      DesignHeadEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ValidateMaximumNetHead (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantFactorsValidator.ValidateMaximumNetHead';
begin
  try
    with PowerPlantFactorsDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MaxNetHead')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      MaxNetHeadEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ValidateMinimumNetHead (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantFactorsValidator.ValidateMinimumNetHead';
begin
  try
    with PowerPlantFactorsDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MinNetHead')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      MinNetHeadEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ValidateEfficiencyFactors (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantFactorsValidator.ValidateEfficiencyFactors';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    with PowerPlantFactorsDialog do
    begin
      lErrorMessages := TStringList.Create;
      lErrorCols     := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'EfficiencyFactor')) then
        begin
          for lCol := 1 to 10 do
            FactorsGrid.ValidationError[1, lCol, gveCellContext] := '';
        end
      else
      begin
        ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
        for lCol := 1 to 10 do
        begin
          lIndex := lErrorCols.IndexOf(IntToStr(lCol));
          if (lIndex >= 0) then
            FactorsGrid.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
          else
            FactorsGrid.ValidationError[1, lCol, gveCellContext] := '';
        end;
      end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.ValidateNetHeadFactors (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantFactorsValidator.ValidateNetHeadFactors';
var
  lCol           : integer;
  lIndex         :  integer;
  LErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    with PowerPlantFactorsDialog do
    begin
      LErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        LErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'NetHeadFactors')) then
        begin
          for lCol := 1 to 10 do
           FactorsGrid.ValidationError[3, lCol, gveCellContext] := ''
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, LErrorCols);
          for lCol := 1 to 10 do
          begin
            lIndex := LErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              FactorsGrid.ValidationError[3, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              FactorsGrid.ValidationError[3, lCol, gveCellContext] := '';
          end;
        end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(LErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantFactorsValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TPowerPlantFactorsValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IPowerPlant;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with PowerPlantFactorsDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = DesignHeadEdit) then
            lFieldProperty := DesignHeadEdit.FieldProperty
          else
          if (FActiveControl = CombinedEfficiencyEdit) then
            lFieldProperty := CombinedEfficiencyEdit.FieldProperty
          else
          if (FActiveControl = MaxNetHeadEdit) then
            lFieldProperty := MaxNetHeadEdit.FieldProperty
          else
          if (FActiveControl = MinNetHeadEdit) then
            lFieldProperty := MinNetHeadEdit.FieldProperty
          else
          if (FActiveControl = FactorsGrid) then
          begin
            lFieldProperty := FactorsGrid.FieldProperty(FactorsGrid.Col);
            lFieldIndex    := IntToStr(FactorsGrid.Row);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TPowerPlantFactorsValidator.OnAfterPasteColumnData';
var
  LPowerPlant : IPowerPlant;
  LValue      : double;
  LRow,
  LCol        : integer;
begin
  try
    LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if(LPowerPlant <> nil)then
    begin
      if(Sender = PowerPlantFactorsDialog.FactorsGrid) then
      begin
        LCol := PowerPlantFactorsDialog.FactorsGrid.Col;
        if(LCol = 1) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PowerPlantFactorsDialog.FactorsGrid.Cells[LCol,LRow]));
            LPowerPlant.EfficiencyFactorByIndex[LRow] := LValue;
          end;
        end
        else
        if(LCol = 3) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PowerPlantFactorsDialog.FactorsGrid.Cells[LCol,LRow]));
            LPowerPlant.NetHeadFactorByIndex[LRow] := LValue;
          end;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtPowerPlantEfficiencyFactors);
        DoContextValidation(dvtPowerPlantNetHeadFactors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantFactorsValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TPowerPlantFactorsValidator.OnAfterPasteGridData';
var
  LPowerPlant : IPowerPlant;
  LValue      : double;
  LRow        : integer;
begin
  try
    LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if(LPowerPlant <> nil)then
    begin
      if(Sender = PowerPlantFactorsDialog.FactorsGrid) then
      begin
        for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(PowerPlantFactorsDialog.FactorsGrid.Cells[1,LRow]));
          LPowerPlant.EfficiencyFactorByIndex[LRow] := LValue;
          LValue := StrToFloat(Trim(PowerPlantFactorsDialog.FactorsGrid.Cells[3,LRow]));
          LPowerPlant.NetHeadFactorByIndex[LRow] := LValue;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtPowerPlantEfficiencyFactors);
        DoContextValidation(dvtPowerPlantNetHeadFactors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

