{******************************************************************************}
{*  UNIT      : Contains the class TPhysicalFlowConstraintValidator.          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPhysicalFlowConstraintValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Types,
  VCL.Grids,
  VCL.Dialogs,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UPhysicalFlowConstraintDialog;

type
  TPhysicalFlowConstraintValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure PopulateStructureTypeCbx;
    procedure RePopulateDataViewer;
    procedure RePopulateReservoirCbxs;
    procedure RepopulateTypeIndependentData;
    procedure RepopulateGrids(AFeature : IPhysicalFlowConstraint);

    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStructureTypeChange(Sender: TObject);
    procedure OnUpstreamReservoirChange(Sender: TObject);
    procedure OnAfterPasteTwoColsGridData(Sender: TObject);
    procedure OnAfterPasteTwoColsGridColumnData(Sender: TObject);
    procedure OnAfterPasteFourColsGridData(Sender: TObject);
    procedure OnAfterPasteFourColsGridColumnData(Sender: TObject);
    procedure OnAfterPasteTenColsGridData(Sender: TObject);
    procedure OnAfterPasteTenColsGridColumnData(Sender: TObject);
    procedure OnAfterPasteElevationDifferenceGridData(Sender: TObject);
    procedure OnAfterPasteElevationDifferenceGridColumnData(Sender: TObject);    
    procedure UpdateStructureType;
    procedure UpdateFeatureName;
    procedure UpdateUpstreamReservoirNode;
    procedure UpdateDownstreamReservoirNode;
    procedure UpdateSillElevation;
    procedure UpdateGateHeight;
    procedure UpdateDischargeCoefficient;
    procedure UpdateStructureLength;
    procedure UpdateNrOfPoints;
    procedure UpdateWaterLevelAtDownstreamNode;
    procedure UpdateReferenceElevation;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure UpdateElevation(AIndex : integer; AValue : string);
    procedure UpdateDischarge(AIndex : integer; AValue : string);
    procedure UpdatePipeChannelNumber(AIndex : integer; AValue : string);
    procedure UpdateKFactor(AIndex : integer; AValue : string);
    procedure UpdatePumpingHead(AIndex : integer; AValue : string);
    procedure UpdatePumpingDischarge(AIndex : integer; AValue : string);
    procedure UpdateHeadDifferences(AIndex : integer; AValue : string);
    procedure UpdateAquiferFlows(AIndex : integer; AValue : string);
    procedure UpdateDownStreamNodeInflows(AIndex : integer; AValue : string);
    procedure UpdateRiverDepths(AIndex : integer; AValue : string);
    procedure UpdateElevationDifferences(AIndex : integer; AValue : string);
    procedure UpdateMonthlyAverageInflows(AIndex : integer; AValue : string);
    procedure UpdateMonthlyAverageDivertedFlow(ARow,ACol : integer; AValue : string);

    procedure ValidateFeatureName(AFeature : IPhysicalFlowConstraint);
    procedure ValidateStructureType (AFeature : IPhysicalFlowConstraint);
    procedure ValidateUpstreamReservoir (AFeature : IPhysicalFlowConstraint);
    procedure ValidateDownstreamReservoir (AFeature : IPhysicalFlowConstraint);
    procedure ValidateElevationOfSill (AFeature : IPhysicalFlowConstraint);
    procedure ValidateMaximumGateHeight (AFeature : IPhysicalFlowConstraint);
    procedure ValidateDischargeCoefficient (AFeature : IPhysicalFlowConstraint);
    procedure ValidateStructureLength (AFeature : IPhysicalFlowConstraint);
    procedure ValidateWaterLevelAtDownstreamNode (AFeature : IPhysicalFlowConstraint);
    procedure ValidateReferenceElevation (AFeature : IPhysicalFlowConstraint);
    procedure ValidateElevations (AFeature : IPhysicalFlowConstraint);
    procedure ValidateDischarges (AFeature : IPhysicalFlowConstraint);
    procedure ValidatePipeChannelNumbers (AFeature : IPhysicalFlowConstraint);
    procedure ValidateKFactors (AFeature : IPhysicalFlowConstraint);
    procedure ValidateAquiferHeads (AFeature : IPhysicalFlowConstraint);
    procedure ValidateAquiferFlows (AFeature : IPhysicalFlowConstraint);

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
    function PhysicalFlowConstraintDialog : TPhysicalFlowConstraintDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  Math,
  VCL.Graphics,
  UConstants,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TPhysicalFlowConstraintValidator                                           *}
{******************************************************************************}

procedure TPhysicalFlowConstraintValidator.CreateMemberObjects;
const OPNAME = 'TPhysicalFlowConstraintValidator.CreateMemberObjects';
var
  lPanel : TPhysicalFlowConstraintDialog;
  lIndex : integer;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TPhysicalFlowConstraintDialog.Create(FPanelOwner,FAppModules);
    lPanel := PhysicalFlowConstraintDialog;
    with lPanel do
    begin
      StructureTypeCbx.FieldProperty     := FAppModules.FieldProperties.FieldProperty('StructureType');
      StructureTypeCbx.OnChange          := OnStructureTypeChange;
      StructureTypeCbx.OnEnter           := OnEditControlEnter;
      StructureTypeCbx.OnExit            := OnEditControltExit;

      FeatureNameEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('PhysicalFlowConstraintFeatureName');
      FeatureNameEdit.OnEnter            := OnEditControlEnter;
      FeatureNameEdit.OnExit             := OnEditControltExit;

      UpstreamReservoirCbx.FieldProperty   := FAppModules.FieldProperties.FieldProperty('UpStreamReservoirNumber');
      UpstreamReservoirCbx.OnChange        := OnUpstreamReservoirChange; 
      UpstreamReservoirCbx.OnEnter         := OnEditControlEnter;
      UpstreamReservoirCbx.OnExit          := OnEditControltExit;

      DownstreamReservoirCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('DownStreamReservoirNumber');
      DownstreamReservoirCbx.OnEnter       := OnEditControlEnter;
      DownstreamReservoirCbx.OnExit        := OnEditControltExit;


      ElevationOfSillEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('SillElevation');
      ElevationOfSillEdit.OnEnter          := OnEditControlEnter;
      ElevationOfSillEdit.OnExit           := OnEditControltExit;

      GateHeightEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('GateHeight');
      GateHeightEdit.OnEnter               := OnEditControlEnter;
      GateHeightEdit.OnExit                := OnEditControltExit;

      DischargeCoefficientEdit.FieldProperty     := FAppModules.FieldProperties.FieldProperty('DischargeCoefficient');
      DischargeCoefficientEdit.OnEnter           := OnEditControlEnter;
      DischargeCoefficientEdit.OnExit            := OnEditControltExit;

      StructureLengthEdit.FieldProperty          := FAppModules.FieldProperties.FieldProperty('ControlStructureLength');
      StructureLengthEdit.OnEnter                := OnEditControlEnter;
      StructureLengthEdit.OnExit                 := OnEditControltExit;

      WaterLevelAtDownstreamNodeEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('WaterLevelAtDownstreamNode');
      WaterLevelAtDownstreamNodeEdit.OnEnter       := OnEditControlEnter;
      WaterLevelAtDownstreamNodeEdit.OnExit        := OnEditControltExit;

      ReferenceElevationEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ReferenceElevation');
      ReferenceElevationEdit.OnEnter               := OnEditControlEnter;
      ReferenceElevationEdit.OnExit                := OnEditControltExit;      

      TwoColsGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      TwoColsGrid.OnColEnter                     := OnStringGridColEnter;
      TwoColsGrid.OnEnter                        := OnEditControlEnter;
      TwoColsGrid.ShowGridPopupMenu              := True;
      TwoColsGrid.AllowPasteFromExcel            := True;
      TwoColsGrid.OnAfterPasteColumnData         := Self.OnAfterPasteTwoColsGridColumnData;
      TwoColsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteTwoColsGridData;
      TwoColsGrid.OnPasteFromExcel               := Self.OnAfterPasteTwoColsGridData;

      FourColsGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      FourColsGrid.OnColEnter                     := OnStringGridColEnter;
      FourColsGrid.OnEnter                        := OnEditControlEnter;
      FourColsGrid.ShowGridPopupMenu              := True;
      FourColsGrid.AllowPasteFromExcel            := True;
      FourColsGrid.OnAfterPasteColumnData         := Self.OnAfterPasteFourColsGridColumnData;
      FourColsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteFourColsGridData;
      FourColsGrid.OnPasteFromExcel               := Self.OnAfterPasteFourColsGridData;

      TenColsGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      TenColsGrid.OnColEnter                     := OnStringGridColEnter;
      TenColsGrid.OnEnter                        := OnEditControlEnter;
      TenColsGrid.ShowGridPopupMenu              := True;
      TenColsGrid.AllowPasteFromExcel            := True;
      TenColsGrid.OnAfterPasteColumnData         := Self.OnAfterPasteTenColsGridColumnData;
      TenColsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteTenColsGridData;
      TenColsGrid.OnPasteFromExcel               := Self.OnAfterPasteTenColsGridData;

      ElevationDifferenceGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      ElevationDifferenceGrid.OnColEnter                     := OnStringGridColEnter;
      ElevationDifferenceGrid.OnEnter                        := OnEditControlEnter;
      ElevationDifferenceGrid.ShowGridPopupMenu              := True;
      ElevationDifferenceGrid.AllowPasteFromExcel            := True;
      ElevationDifferenceGrid.OnAfterPasteColumnData         := Self.OnAfterPasteElevationDifferenceGridColumnData;
      ElevationDifferenceGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteElevationDifferenceGridData;
      ElevationDifferenceGrid.OnPasteFromExcel               := Self.OnAfterPasteElevationDifferenceGridData;


      //Type10Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      //Type10Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintChannelNumber'));
      //Type10Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintKFactor'));
      //Type10Grid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      //Type10Grid.OnColEnter         := OnStringGridColEnter;
      //Type10Grid.OnEnter            := OnEditControlEnter;

      //Type11Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      //Type11Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintHeadDifferences'));
      //Type11Grid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintAquiferFlows'));
      //Type11Grid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      //Type11Grid.OnColEnter         := OnStringGridColEnter;
      //Type11Grid.OnEnter            := OnEditControlEnter;

      //Type11Grid2.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      for lIndex := 1 to 10 do
      begin
        //Type11Grid2.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintDownStreamNodeInflows'));
        //Type11Grid2.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ConstraintDownStreamNodeInflows'));
      end;
      //Type11Grid2.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      //Type11Grid2.OnColEnter         := OnStringGridColEnter;
      //Type11Grid2.OnEnter            := OnEditControlEnter;
      //Type11Grid2.OnDrawCell         := OnType11Grid2DrawCell;

      NrPointsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PointsElevationNumber');
      NrPointsEdit.OnEnter       := OnEditControlEnter;
      NrPointsEdit.OnExit        := OnEditControltExit;

  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.DestroyMemberObjects;
const OPNAME = 'TPhysicalFlowConstraintValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.Initialise: boolean;
const OPNAME = 'TPhysicalFlowConstraintValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPhysicalFlowConstraintValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.PhysicalFlowConstraint');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.SaveState: boolean;
const OPNAME = 'TPhysicalFlowConstraintValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.PhysicalFlowConstraintDialog : TPhysicalFlowConstraintDialog;
const OPNAME = 'TPhysicalFlowConstraintValidator.PhysicalFlowConstraintDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPhysicalFlowConstraintDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPhysicalFlowConstraintValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.StudyHasChanged: boolean;
const OPNAME = 'TPhysicalFlowConstraintValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ClearDataViewer;
const OPNAME = 'TPhysicalFlowConstraintValidator.ClearDataViewer';
var
  lPanel : TPhysicalFlowConstraintDialog;
  lRow   : integer;
  lCol   : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := PhysicalFlowConstraintDialog;
    with lPanel do
    begin
      FeatureNameEdit.SetFieldValue('');
      UpstreamReservoirCbx.ItemIndex := -1;
      UpstreamReservoirCbx.Items.Clear;
      DownstreamReservoirCbx.ItemIndex := -1;
      DownstreamReservoirCbx.Items.Clear;
      StructureTypeCbx.ItemIndex := -1;
      StructureTypeCbx.Items.Clear;
      ElevationOfSillEdit.Text            := '-1.0';
      GateHeightEdit.Text                 := '-1.0';
      DischargeCoefficientEdit.Text       := '-1.0';
      StructureLengthEdit.Text            := '-1.0';
      WaterLevelAtDownstreamNodeEdit.Text := '-1.0';
      ReferenceElevationEdit.Text         := '-1.0';
      for lRow := 1 to TwoColsGrid.RowCount-1 do
      begin
        TwoColsGrid.Cells[0, lRow] := '';
        TwoColsGrid.Cells[1, lRow] := '-1';
        TwoColsGrid.Cells[2, lRow] := '-1';
      end;
      for lRow := 1 to TwoColsGrid.RowCount-1 do
      begin
        TwoColsGrid.Cells[0, lRow] := '';
        TwoColsGrid.Cells[1, lRow] := '-1';
        TwoColsGrid.Cells[2, lRow] := '-1';
      end;
      ElevationDifferenceGrid.Cells[0, 0] := '';
      for lCol := 1 to ElevationDifferenceGrid.ColCount-1 do
        ElevationDifferenceGrid.Cells[lCol, 0] := '-1';

      {for lRow := 1 to Type10Grid.RowCount-1 do
      begin
        Type10Grid.Cells[0, lRow] := '';
        Type10Grid.Cells[1, lRow] := '-1';
        Type10Grid.Cells[2, lRow] := '-1';
      end;
      for lRow := 1 to Type11Grid.RowCount-1 do
      begin
        Type11Grid.Cells[0, lRow] := '';
        Type11Grid.Cells[1, lRow] := '-1';
        Type11Grid.Cells[2, lRow] := '-1';
      end;
      for lRow := 0 to Type11Grid2.RowCount - 1 do
      begin
        for lCol := 0 to Type11Grid2.ColCount - 1 do
        begin
          if ((lRow < Type11Grid2.FixedRows) OR (lCol < Type11Grid2.FixedCols)) then
            Type11Grid2.Cells[lCol, lRow] := ''
          else
            Type11Grid2.Cells[lCol, lRow] := '-1';
        end;
      end;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.PopulateDataViewer;
const OPNAME = 'TPhysicalFlowConstraintValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateStructureTypeCbx;
    RePopulateDataViewer;
    DoContextValidation(dvtPhysConstraint);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.PopulateStructureTypeCbx;
const OPNAME = 'TPhysicalFlowConstraintValidator.PopulateStructureTypeCbx';
var
  lLanguage      : TAbstractLanguage;
begin
  try
    with PhysicalFlowConstraintDialog do
    begin
      StructureTypeCbx.Items.Clear;
      lLanguage := FAppModules.Language;
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType01'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType02'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType03'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType04'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType05'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType06'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType07'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType08'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType09'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType10'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType11'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType12'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType13'));
      StructureTypeCbx.Items.Add(lLanguage.GetString('NetworkFeatures.PhysicalFlowConstraintType14'));      
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TPhysicalFlowConstraintValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IPhysicalFlowConstraint;
  //lCol           : integer;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with PhysicalFlowConstraintDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = UpstreamReservoirCbx) then
            lFieldProperty := UpstreamReservoirCbx.FieldProperty
          else
          if (FActiveControl = DownstreamReservoirCbx) then
            lFieldProperty := DownstreamReservoirCbx.FieldProperty
          else
          if (FActiveControl = StructureTypeCbx) then
            lFieldProperty := StructureTypeCbx.FieldProperty
          else
          if (FActiveControl = ElevationOfSillEdit) then
            lFieldProperty := ElevationOfSillEdit.FieldProperty
          else
          if (FActiveControl = GateHeightEdit) then
            lFieldProperty := GateHeightEdit.FieldProperty
          else
          if (FActiveControl = DischargeCoefficientEdit) then
            lFieldProperty := DischargeCoefficientEdit.FieldProperty
          else
          if (FActiveControl = StructureLengthEdit) then
            lFieldProperty := StructureLengthEdit.FieldProperty
          else
          if (FActiveControl = WaterLevelAtDownstreamNodeEdit) then
            lFieldProperty := WaterLevelAtDownstreamNodeEdit.FieldProperty
          else
          if (FActiveControl = ReferenceElevationEdit) then
            lFieldProperty := ReferenceElevationEdit.FieldProperty
          else
          if (FActiveControl = TwoColsGrid) then
          begin
            lFieldIndex    := IntToStr(TwoColsGrid.Row);
            lFieldProperty := TwoColsGrid.FieldProperty(TwoColsGrid.Col);
          end
          else
          if (FActiveControl = ElevationDifferenceGrid) then
          begin
            lFieldIndex    := IntToStr(ElevationDifferenceGrid.Row);
            lFieldProperty := ElevationDifferenceGrid.FieldProperty(ElevationDifferenceGrid.Col);
          end;

          {else
          if (FActiveControl = Type10Grid) then
          begin
            lFieldIndex    := IntToStr(Type10Grid.Row);
            lFieldProperty := Type10Grid.FieldProperty(Type10Grid.Col);
          end
          else
          if (FActiveControl = Type11Grid) then
          begin
            lFieldIndex    := IntToStr(Type11Grid.Row);
            lFieldProperty := Type11Grid.FieldProperty(Type11Grid.Col);
          end
          else
          if (FActiveControl = Type11Grid2) then
          begin
            lCol := Type11Grid2.Col;
            lFieldProperty := Type11Grid2.FieldProperty(lCol);
            if ((lCol mod 2) = 1) then
              lCol := (lCol+1) div 2
            else
              lCol := lCol div 2;
            lFieldIndex    := IntToStr(Type11Grid2.Row-1) + ',' + IntToStr(lCol);
          end;}
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
procedure TPhysicalFlowConstraintValidator.RePopulateDataViewer;
const OPNAME = 'TPhysicalFlowConstraintValidator.RePopulateDataViewer';
var
  lConstraint    : IPhysicalFlowConstraint;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with PhysicalFlowConstraintDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lConstraint.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          FeatureNameEdit.SetFieldValue(lConstraint.FeatureName);
          SetFeatureType(lConstraint.StructureType);
          SetNumberOfPoints(lConstraint.NrOfPoints);
          StructureTypeCbx.ItemIndex := lConstraint.StructureType-1;
          StructureTypeCbx.Text      := StructureTypeCbx.Items[StructureTypeCbx.ItemIndex];
          RePopulateReservoirCbxs;
          RepopulateTypeIndependentData;
          RepopulateGrids(lConstraint);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnStructureTypeChange(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnStructureTypeChange';
var
  lConstraint : IPhysicalFlowConstraint;
begin
  lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                 NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
  try
    if PhysicalFlowConstraintDialog.StructureTypeCbx.ItemIndex > 0 then
    begin
      UpdateStructureType;
      if lConstraint.StructureType in [2,3] then
        DoContextValidation(dvtPhysConstraintStructureType);
      RepopulateGrids(lConstraint);
    end
    else if PhysicalFlowConstraintDialog.StructureTypeCbx.ItemIndex = 0 then
    begin
      if (lConstraint.StructureType <> 1) then
        MessageDlg(FAppModules.Language.GetString('Message.StructType1Msg'),mtError,[mbOK], 0);
      PhysicalFlowConstraintDialog.StructureTypeCbx.ItemIndex := lConstraint.StructureType-1;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnUpstreamReservoirChange(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnUpstreamReservoirChange';
var
  lConstraint : IPhysicalFlowConstraint;
begin
  lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                 NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
  try
    UpdateUpstreamReservoirNode;
    if lConstraint.StructureType in [2,3] then
    begin
      DoContextValidation(dvtPhysConstraintElevationOfSill);
      DoContextValidation(dvtPhysConstraintMaximumGateHeight);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPhysicalFlowConstraintValidator.RepopulateTypeIndependentData;
const OPNAME = 'TPhysicalFlowConstraintValidator.RepopulateTypeIndependentData';
var
  lConstraint    : IPhysicalFlowConstraint;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with PhysicalFlowConstraintDialog do
        begin
          StructureTypeCbx.SetFieldIndex(lConstraint.StructureType - 1);

          lFieldIndex    := '';
          lFieldProperty := ElevationOfSillEdit.FieldProperty;
          lKeyValues     := lConstraint.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          ElevationOfSillEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          ElevationOfSillEdit.SetFieldValue(lConstraint.ElevationOfSill);

          lFieldProperty := DischargeCoefficientEdit.FieldProperty;
          DischargeCoefficientEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          DischargeCoefficientEdit.SetFieldValue(lConstraint.DischargeCoefficient);

          lFieldProperty := GateHeightEdit.FieldProperty;
          GateHeightEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          GateHeightEdit.SetFieldValue(lConstraint.MaximumGateHeight);

          lFieldProperty := StructureLengthEdit.FieldProperty;
          StructureLengthEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          StructureLengthEdit.SetFieldValue(lConstraint.StructureLength);

          lFieldProperty := WaterLevelAtDownstreamNodeEdit.FieldProperty;
          WaterLevelAtDownstreamNodeEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          WaterLevelAtDownstreamNodeEdit.SetFieldValue(lConstraint.WaterLevelAtDownstreamNode);

          lFieldProperty := ReferenceElevationEdit.FieldProperty;
          ReferenceElevationEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          ReferenceElevationEdit.SetFieldValue(lConstraint.ReferenceElevation);


        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TPhysicalFlowConstraintValidator.RepopulateGrid;
const OPNAME = 'TPhysicalFlowConstraintValidator.RepopulateGrid';
var
  lConstraint    : IPhysicalFlowConstraint;
  lIndex         : integer;
  lFieldProp4A   : TAbstractFieldProperty;
  lFieldProp4B   : TAbstractFieldProperty;
  lFieldProp10A  : TAbstractFieldProperty;
  lFieldProp10B  : TAbstractFieldProperty;
  lFieldProp11A  : TAbstractFieldProperty;
  lFieldProp11B  : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with PhysicalFlowConstraintDialog do
        begin
          lFieldProp4A  := TwoColsGrid.FieldProperty(1);
          lFieldProp4B  := TwoColsGrid.FieldProperty(2);
          lFieldProp10A := Type10Grid.FieldProperty(1);
          lFieldProp10B := Type10Grid.FieldProperty(2);
          lFieldProp11A := Type11Grid.FieldProperty(1);
          lFieldProp11B := Type11Grid.FieldProperty(2);
          for lIndex := 1 to 10 do
          begin
            lFieldIndex    := IntToStr(lIndex);
            lKeyValues     := lConstraint.GetKeyValues(lFieldProp4A.FieldName, lFieldIndex);

            TwoColsGrid.Cells[0, lIndex] := IntToStr(lIndex);
            Type10Grid.Cells[0, lIndex] := IntToStr(lIndex);
            Type11Grid.Cells[0, lIndex] := IntToStr(lIndex);
            if (lConstraint.ElevationByIndex[lIndex] <> NullFloat) then
            begin
              TwoColsGrid.HasMetaData[1, lIndex] :=
                FAppModules.MetaData.FindMetaData(lFieldProp4A.FieldName, lKeyValues, lFieldIndex) <> nil;
              TwoColsGrid.Cells[1, lIndex] := Format('%10.2f', [lConstraint.ElevationByIndex[lIndex]]);
              Type10Grid.HasMetaData[1, lIndex] :=
                FAppModules.MetaData.FindMetaData(lFieldProp10A.FieldName, lKeyValues, lFieldIndex) <> nil;
              Type10Grid.Cells[1, lIndex] := IntToStr(Trunc(lConstraint.ElevationByIndex[lIndex]));
              Type11Grid.HasMetaData[1, lIndex] :=
                FAppModules.MetaData.FindMetaData(lFieldProp11A.FieldName, lKeyValues, lFieldIndex) <> nil;
              Type11Grid.Cells[1, lIndex] := Format('%10.2f', [lConstraint.ElevationByIndex[lIndex]]);
            end
            else
            begin
              TwoColsGrid.Cells[1, lIndex] := '';
              Type10Grid.Cells[1, lIndex] := '';
              Type11Grid.Cells[1, lIndex] := '';
            end;
            if (lConstraint.DischargeByIndex[lIndex] <> NullFloat) then
            begin
              TwoColsGrid.HasMetaData[2, lIndex] :=
                FAppModules.MetaData.FindMetaData(lFieldProp4B.FieldName, lKeyValues, lFieldIndex) <> nil;
              TwoColsGrid.Cells[2, lIndex] := Format('%10.2f', [lConstraint.DischargeByIndex[lIndex]]);
              Type10Grid.HasMetaData[2, lIndex] :=
                FAppModules.MetaData.FindMetaData(lFieldProp10B.FieldName, lKeyValues, lFieldIndex) <> nil;
              Type10Grid.Cells[2, lIndex] := Format('%10.3f', [lConstraint.DischargeByIndex[lIndex]]);
              Type11Grid.HasMetaData[2, lIndex] :=
                FAppModules.MetaData.FindMetaData(lFieldProp11B.FieldName, lKeyValues, lFieldIndex) <> nil;
              Type11Grid.Cells[2, lIndex] := Format('%10.2f', [lConstraint.DischargeByIndex[lIndex]]);
            end
            else
            begin
              TwoColsGrid.Cells[2, lIndex] := '';
              Type10Grid.Cells[2, lIndex] := '';
              Type11Grid.Cells[2, lIndex] := '';
            end;
          end;
          if (lConstraint.StructureType = 11) then
            RepopulateType11Grid2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.RepopulateType11Grid2;
const OPNAME = 'TPhysicalFlowConstraintValidator.RepopulateType11Grid2';
var
  lConstraint  : IPhysicalFlowConstraint;
  lIndex       : integer;
  lCol         : integer;
  lValue       : double;
  lFieldProp1  : TAbstractFieldProperty;
  lFieldProp2  : TAbstractFieldProperty;
  lKeyValues   : string;
  lFieldIndex  : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with PhysicalFlowConstraintDialog do
        begin
          NrPointsEdit.Text := IntToStr(lConstraint.NrOfType11Points);
          Type11Grid2.RowCount := 2 + lConstraint.NrOfType11Points;
          if (lConstraint.NrOfType11Points > 0) then
            Type11Grid2.FixedRows := 2;
          for lIndex := 1 to 10 do
          begin
            Type11Grid2.Cells[lIndex*2 - 1, 1] := IntToStr(lIndex);
            Type11Grid2.Cells[lIndex*2, 1]     := IntToStr(lIndex);
          end;
          lFieldProp1 := Type11Grid2.FieldProperty(1);
          lFieldProp2 := Type11Grid2.FieldProperty(2);
          for lIndex := 1 to lConstraint.NrOfType11Points do
          begin
            Type11Grid2.Cells[0, lIndex + 1] := IntToStr(lIndex);
            for lCol := 1 to 10 do
            begin
              lFieldIndex := IntToStr(lIndex) + ',' + IntToStr(lCol);
              lKeyValues  := lConstraint.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
              lValue := lConstraint.Type11InFlowByRowCol[lIndex, lCol];
              if (lValue <> NullFloat) then
              begin
                Type11Grid2.HasMetaData[lCol*2 - 1, lIndex + 1] :=
                  FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
                Type11Grid2.Cells[lCol*2 - 1, lIndex + 1] := Format('%10.2f', [lValue]);
              end
              else
                Type11Grid2.Cells[lCol*2 - 1, lIndex + 1] := '';

              lValue := lConstraint.Type11DepthByRowCol[lIndex, lCol];
              if (lValue <> NullFloat) then
              begin
                Type11Grid2.HasMetaData[lCol*2, lIndex + 1] :=
                  FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;
                Type11Grid2.Cells[lCol*2, lIndex + 1] := Format('%10.2f', [lValue]);
              end
              else
                Type11Grid2.Cells[lCol*2, lIndex + 1] := '';
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TPhysicalFlowConstraintValidator.RePopulateReservoirCbxs;
const OPNAME = 'TPhysicalFlowConstraintValidator.RePopulateReservoirCbxs';
var
  lConstraint    : IPhysicalFlowConstraint;
  lReservoirList : IReservoirDataList;
  lIndexA        : integer;
  lReservoir     : IReservoirData;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                       PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
      if (lConstraint <> nil) then
      begin
        with PhysicalFlowConstraintDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
          DownstreamReservoirCbx.Items.AddObject
            (FAppModules.Language.GetString('NetworkFeatures.UseChannelDownstreamNode'), nil);
          UpstreamReservoirCbx.Items.AddObject
            (FAppModules.Language.GetString('NetworkFeatures.UseChannelUpstreamNode'), nil);
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirByIndex[lIndexA];
              DownstreamReservoirCbx.Items.AddObject
                ('(' + IntToStr(lReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ') ' +
                 lReservoir.ReservoirConfigurationData.ReservoirName,
                 TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
              UpstreamReservoirCbx.Items.AddObject
                ('(' + IntToStr(lReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ') ' +
                 lReservoir.ReservoirConfigurationData.ReservoirName,
                 TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
            end;
            lFieldIndex    := '';
            lFieldProperty := UpstreamReservoirCbx.FieldProperty;
            lKeyValues     := lConstraint.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            UpstreamReservoirCbx.HasMetaData :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
            if (lConstraint.UpstreamReservoirNr <> 0) then
              UpstreamReservoirCbx.SetFieldIndex(UpstreamReservoirCbx.Items.IndexOfObject(TObject(lConstraint.UpstreamReservoirNr)))
            else
              UpstreamReservoirCbx.SetFieldIndex(UpstreamReservoirCbx.Items.IndexOfObject(nil));

            lFieldProperty := DownstreamReservoirCbx.FieldProperty;
            DownstreamReservoirCbx.HasMetaData :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
            if (lConstraint.DownstreamReservoirNr <> 0) then
              DownstreamReservoirCbx.SetFieldIndex(DownstreamReservoirCbx.Items.IndexOfObject(TObject(lConstraint.DownstreamReservoirNr)))
            else
              DownstreamReservoirCbx.SetFieldIndex(DownstreamReservoirCbx.Items.IndexOfObject(nil));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateNrOfPoints;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateNrOfPoints';
var
  lConstraint : IPhysicalFlowConstraint;
  lCount      : integer;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (Trim(NrPointsEdit.Text) <> '') then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
            NrPointsEdit.FieldProperty.FieldName,
            NrPointsEdit.Text, lMessage)) then
          begin
            NrPointsEdit.FieldValidationError := lMessage;
            lCount := StrToInt(Trim(NrPointsEdit.Text));
            lConstraint.NrOfPoints := lCount;
            RepopulateGrids(lConstraint);
          end
          else
            NrPointsEdit.FieldValidationError := lMessage;
          end;
        end;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TPhysicalFlowConstraintValidator.OnType11Grid2DrawCell
                                                      (Sender     : TObject;
                                                       ACol, ARow : Integer;
                                                       Rect       : TRect;
                                                       State      : TGridDrawState);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnType11Grid2DrawCell';
var
  lGrid   : TStringGrid;
  lText   : string;
  lPart   : string;
  lPos    : integer;
  lWidth  : integer;
  lHeight : integer;
  lTop    : integer;
  lCellW  : integer;
begin
  try
    lGrid := TStringGrid(Sender);
    with lGrid do
    begin
      if ((ARow < lGrid.FixedRows) OR (ACol < lGrid.FixedCols)) then
        Canvas.Brush.Color := clBtnFace
      else if ((ACol mod 2) = 1) then
        Canvas.Brush.Color := clWhite
      else
        Canvas.Brush.Color := clSkyBlue;
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
      if (ARow > 1) then
        Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, Cells[ACol, ARow])
      else
      begin
        lText := Cells[ACol, ARow];
        if (Length(lText) > 0) then
        begin
          lCellW := Rect.Right - Rect.Left - 4;
          lTop  := Rect.Top + 2;
          lText := WrapText(lText, 6);
          while (Length(lText) > 0) do
          begin
            lPos := Pos(#13#10, lText);
            if (lPos > 0) then
            begin
              lPart := Trim(Copy(lText, 1, lPos-1));
              lText := Copy(lText, lPos + 2, Length(lText) - lPos - 1);
            end
            else
            begin
              lPart := lText;
              lText := '';
            end;
            lHeight := Canvas.TextHeight(lPart);
            lWidth  := Canvas.TextWidth(lPart);
            if ((lTop + lHeight) < (Rect.Bottom - 2)) then
              Canvas.TextOut(Rect.Left + ((lCellW-lWidth) div 2), lTop, lPart);
            lTop    := lTop + lHeight + 1;
          end;
        end;
      end;
      if gdFocused in State then
        Canvas.DrawFocusRect(Rect);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TPhysicalFlowConstraintValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with PhysicalFlowConstraintDialog do
    begin
      if ((Sender = FeatureNameEdit) AND
          (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName
      else
      if ((DownstreamReservoirCbx = Sender) AND
          (DownstreamReservoirCbx.HasValueChanged)) then
        UpdateDownstreamReservoirNode
      else
      if ((StructureTypeCbx = Sender) AND
          (StructureTypeCbx.HasValueChanged)) then
        UpdateStructureType
      else
      if ((ElevationOfSillEdit = Sender) AND
          (ElevationOfSillEdit.HasValueChanged)) then
      begin
        UpdateSillElevation;
        DoContextValidation(dvtPhysConstraintElevationOfSill);
        DoContextValidation(dvtPhysConstraintMaximumGateHeight);        
      end
      else
      if ((GateHeightEdit = Sender) AND
          (GateHeightEdit.HasValueChanged)) then
      begin
        UpdateGateHeight;
        DoContextValidation(dvtPhysConstraintMaximumGateHeight);
      end
      else
      if ((DischargeCoefficientEdit = Sender) AND
          (DischargeCoefficientEdit.HasValueChanged)) then
        UpdateDischargeCoefficient
      else
      if ((StructureLengthEdit = Sender) AND
          (StructureLengthEdit.HasValueChanged)) then
        UpdateStructureLength
      else
      if ((Sender = NrPointsEdit) AND
          (NrPointsEdit.HasValueChanged)) then
        UpdateNrOfPoints
      else
      if ((Sender = WaterLevelAtDownstreamNodeEdit) AND
          (WaterLevelAtDownstreamNodeEdit.HasValueChanged)) then
        UpdateWaterLevelAtDownstreamNode
      else
      if ((Sender = ReferenceElevationEdit) AND
          (ReferenceElevationEdit.HasValueChanged)) then
        UpdateReferenceElevation;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateFeatureName;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateFeatureName';
var
  lFeature  : IPhysicalFlowConstraint;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
          FeatureNameEdit.FieldProperty.FieldName,
          FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
          DoContextValidation(dvtPhysConstraintFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateUpstreamReservoirNode;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateUpstreamReservoirNode';
var
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lConstraint    : IPhysicalFlowConstraint;
  lMessage       : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := nil;
        lReservoirNr   := -1;
        if (UpstreamReservoirCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(UpstreamReservoirCbx.Items.Objects[UpstreamReservoirCbx.ItemIndex]);
          lReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            UpstreamReservoirCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr), lMessage)) then
        begin
          lConstraint.UpstreamReservoirNr := lReservoirNr;
          if (lReservoirList <> nil) then
          begin
            lReservoir := lReservoirList.ReservoirByIdentifier[lConstraint.UpstreamReservoirNr];
            if (lReservoir <> nil) then
              UpstreamReservoirCbx.SetFieldIndex(UpstreamReservoirCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier)))
            else
              UpstreamReservoirCbx.SetFieldIndex(UpstreamReservoirCbx.Items.IndexOfObject(nil));
          end;
        end
        else
          UpstreamReservoirCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateDownstreamReservoirNode;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateDownstreamReservoirNode';
var
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lConstraint    : IPhysicalFlowConstraint;
  lMessage       : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := nil;
        lReservoirNr   := -1;
        if (DownstreamReservoirCbx.ItemIndex >= 0)then
        begin
          lReservoirNr := Integer(DownstreamReservoirCbx.Items.Objects[DownstreamReservoirCbx.ItemIndex]);
          lReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DownstreamReservoirCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr), lMessage)) then
        begin
          lConstraint.DownstreamReservoirNr := lReservoirNr;
          if (lReservoirList <> nil) then
          begin
            lReservoir := lReservoirList.ReservoirByIdentifier[lConstraint.DownstreamReservoirNr];;
            if (lReservoir <> nil) then
              DownstreamReservoirCbx.SetFieldIndex(DownstreamReservoirCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier)))
            else
              DownstreamReservoirCbx.SetFieldIndex(DownstreamReservoirCbx.Items.IndexOfObject(nil));
          end;
        end
        else
          DownstreamReservoirCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateStructureType;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateStructureType';
var
  lConstraint : IPhysicalFlowConstraint;
  lNewType    : integer;
  lOldType    : integer;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (StructureTypeCbx.ItemIndex > 0) then
        begin
          lOldType := lConstraint.StructureType;
          lNewType := -1;
          if (StructureTypeCbx.ItemIndex >= 0) then
              lNewType := StructureTypeCbx.ItemIndex + 1;
          if (lOldType <> lNewType) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                StructureTypeCbx.FieldProperty.FieldName,
                IntToStr(lNewType), lMessage)) then
            begin
              lConstraint.StructureType := StructureTypeCbx.ItemIndex + 1;
              RePopulateDataViewer;
            end
            else
              StructureTypeCbx.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateSillElevation;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateSillElevation';
var
  lConstraint : IPhysicalFlowConstraint;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            ElevationOfSillEdit.FieldProperty.FieldName,
            ElevationOfSillEdit.Text, lMessage)) then
        begin
          ElevationOfSillEdit.FieldValidationError := lMessage;
          lConstraint.ElevationOfSill := StrToFloat(Trim(ElevationOfSillEdit.Text));
          ElevationOfSillEdit.SetFieldValue(lConstraint.ElevationOfSill);
        end
        else
          ElevationOfSillEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateWaterLevelAtDownstreamNode;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateWaterLevelAtDownstreamNode';
var
  lConstraint : IPhysicalFlowConstraint;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            WaterLevelAtDownstreamNodeEdit.FieldProperty.FieldName,
            WaterLevelAtDownstreamNodeEdit.Text, lMessage)) then
        begin
          WaterLevelAtDownstreamNodeEdit.FieldValidationError := lMessage;
          lConstraint.WaterLevelAtDownstreamNode := StrToFloat(Trim(WaterLevelAtDownstreamNodeEdit.Text));
          WaterLevelAtDownstreamNodeEdit.SetFieldValue(lConstraint.WaterLevelAtDownstreamNode);
        end
        else
          WaterLevelAtDownstreamNodeEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateReferenceElevation;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateReferenceElevation';
var
  lConstraint : IPhysicalFlowConstraint;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            ReferenceElevationEdit.FieldProperty.FieldName,
            ReferenceElevationEdit.Text, lMessage)) then
        begin
          ReferenceElevationEdit.FieldValidationError := lMessage;
          lConstraint.ReferenceElevation := StrToFloat(Trim(ReferenceElevationEdit.Text));
          ReferenceElevationEdit.SetFieldValue(lConstraint.ReferenceElevation);
        end
        else
          ReferenceElevationEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateGateHeight;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateGateHeight';
var
  lConstraint : IPhysicalFlowConstraint;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            GateHeightEdit.FieldProperty.FieldName,
            GateHeightEdit.Text, lMessage)) then
        begin
          GateHeightEdit.FieldValidationError:= lMessage;
          lConstraint.MaximumGateHeight := StrToFloat(Trim(GateHeightEdit.Text));
          GateHeightEdit.SetFieldValue(lConstraint.MaximumGateHeight);
        end
        else
          GateHeightEdit.FieldValidationError:= lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateDischargeCoefficient;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateDischargeCoefficient';
var
  lConstraint : IPhysicalFlowConstraint;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DischargeCoefficientEdit.FieldProperty.FieldName,
            DischargeCoefficientEdit.Text, lMessage)) then
        begin
          DischargeCoefficientEdit.FieldValidationError := lMessage;
          lConstraint.DischargeCoefficient := StrToFloat(Trim(DischargeCoefficientEdit.Text));
          DischargeCoefficientEdit.SetFieldValue(lConstraint.DischargeCoefficient);
        end
        else
          DischargeCoefficientEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateStructureLength;
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateStructureLength';
var
  lConstraint : IPhysicalFlowConstraint;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            StructureLengthEdit.FieldProperty.FieldName,
            StructureLengthEdit.Text, lMessage)) then
        begin
          StructureLengthEdit.FieldValidationError := lMessage;
          lConstraint.StructureLength := StrToFloat(Trim(StructureLengthEdit.Text));
          StructureLengthEdit.SetFieldValue(lConstraint.StructureLength);
        end
        else
          StructureLengthEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnStringGridCellDataHasChanged';
var
  lConstraint : IPhysicalFlowConstraint;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil)then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        if (TwoColsGrid = ASender) then
        begin
          if lConstraint.StructureType in  [4,5,7,8,9,14] then
          begin
            if (ACol = 1) then
              UpdateElevation(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]));
            if (ACol = 2) then
              UpdateDischarge(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]));
          end
          else
          if lConstraint.StructureType in  [10] then
          begin
            if (ACol = 1) then
              UpdatePipeChannelNumber(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]))
            else
            if (ACol = 2) then
              UpdateKFactor(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]));
          end
{          else
          if lConstraint.StructureType in  [12] then
          begin
            if (ACol = 1) then
              UpdateElevationDifferences(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]))
            else
            if (ACol = 2) then
              UpdateMonthlyAverageInflows(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]));
          end}
          else
          if lConstraint.StructureType in  [13] then
          begin
            if (ACol = 1) then
              UpdatePumpingHead(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]))
            else
            if (ACol = 2) then
              UpdatePumpingDischarge(ARow, Trim(TwoColsGrid.Cells[ACol, ARow]));
          end;
        end
        else
        if (FourColsGrid = ASender) then
        begin
          if lConstraint.StructureType in  [11]then
          begin
            if (ACol = 1) then
              UpdateHeadDifferences(ARow, Trim(FourColsGrid.Cells[ACol, ARow]))
            else
            if (ACol = 2) then
              UpdateAquiferFlows(ARow, Trim(FourColsGrid.Cells[ACol, ARow]));
            if (ACol = 3) then
              UpdateDownStreamNodeInflows(ARow, Trim(FourColsGrid.Cells[ACol, ARow]))
            else
            if (ACol = 4) then
              UpdateRiverDepths(ARow, Trim(FourColsGrid.Cells[ACol, ARow]));
          end;
        end
        else
        if (TenColsGrid = ASender) then
        begin
          if lConstraint.StructureType in  [12]then
          begin
            if (ACol = 1) then
              UpdateMonthlyAverageInflows(ARow, Trim(TenColsGrid.Cells[ACol, ARow]))
            else
              UpdateMonthlyAverageDivertedFlow(ARow,ACol-1,Trim(TenColsGrid.Cells[ACol, ARow]));
          end;
        end
        else
        if (ElevationDifferenceGrid = ASender) then
        begin
          if lConstraint.StructureType in  [12]then
            UpdateElevationDifferences(ACol, Trim(ElevationDifferenceGrid.Cells[ACol, 0]))
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateElevation(AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateElevation';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TwoColsGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ConstraintElevation', AValue, lMessage, AIndex)) then
        begin
          lConstraint.DischargeCurve.ElevationByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArrays);
        end
        else
          TwoColsGrid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateDischarge(AIndex : integer;
                                                           AValue : string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateDischarge';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TwoColsGrid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ConstraintDischarge', AValue, lMessage, AIndex)) then
        begin
          lConstraint.DischargeCurve.DischargeByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArrays);
        end
        else
          TwoColsGrid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdatePipeChannelNumber(AIndex : integer;
                                                                   AValue : string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdatePipeChannelNumber';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TwoColsGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintChannelNumber', AValue, lMessage, AIndex)) then
        begin
          lConstraint.KFactors.ChannelNumberByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          TwoColsGrid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateKFactor(AIndex : integer;
                                                         AValue : string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateKFactor';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TwoColsGrid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintKFactor', AValue, lMessage, AIndex)) then
        begin
          lConstraint.KFactors.KFactorByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          TwoColsGrid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdatePumpingHead(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdatePumpingHead';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TwoColsGrid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintPumpingHeads', AValue, lMessage, AIndex)) then
        begin
          lConstraint.PumpStation.PumpingHeadByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          TwoColsGrid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdatePumpingDischarge(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdatePumpingDischarge';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TwoColsGrid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintPumpingDischarges', AValue, lMessage, AIndex)) then
        begin
          lConstraint.PumpStation.PumpingDischargeByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          TwoColsGrid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateHeadDifferences(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateHeadDifferences';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FourColsGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintHeadDifferences', AValue, lMessage, AIndex)) then
        begin
          lConstraint.SandAquifer.HeadDifferenceByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          FourColsGrid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateAquiferFlows(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateAquiferFlows';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FourColsGrid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintAquiferFlows', AValue, lMessage, AIndex)) then
        begin
          lConstraint.SandAquifer.AquiferFlowByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          FourColsGrid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateDownStreamNodeInflows(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateDownStreamNodeInflows';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FourColsGrid.ValidationError[3, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintDownStreamNodeInflows', AValue, lMessage, AIndex)) then
        begin
          lConstraint.SandAquifer.DownStreamNodeInflowByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          FourColsGrid.ValidationError[3, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateRiverDepths(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateRiverDepths';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FourColsGrid.ValidationError[4, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintRiverDepths', AValue, lMessage, AIndex)) then
        begin
          lConstraint.SandAquifer.RiverDepthByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          FourColsGrid.ValidationError[4, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateElevationDifferences(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateElevationDifferences';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        ElevationDifferenceGrid.ValidationError[AIndex, 0, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintElevationDifferences', AValue, lMessage, AIndex)) then
        begin
          lConstraint.SubmergedOutlet.ElevationDifferenceByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          ElevationDifferenceGrid.ValidationError[AIndex, 0, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateMonthlyAverageInflows(AIndex: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateMonthlyAverageInflows';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        ElevationDifferenceGrid.ValidationError[AIndex, 0, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintMonthlyAverageInflows', AValue, lMessage, AIndex)) then
        begin
          lConstraint.SubmergedOutlet.MonthlyAverageInflowByIndex[AIndex] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          ElevationDifferenceGrid.ValidationError[AIndex, 0, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.UpdateMonthlyAverageDivertedFlow(ARow, ACol: integer; AValue: string);
const OPNAME = 'TPhysicalFlowConstraintValidator.UpdateMonthlyAverageDivertedFlow';
var
  lConstraint : IPhysicalFlowConstraint;
  lValue      : double;
  lMessage    : string;
begin
  try
    lConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if (lConstraint <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        TenColsGrid.ValidationError[ACol, ARow, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty('ConstraintMonthlyAverageDivertedFlow', AValue, lMessage, ARow,ACol)) then
        begin
          lConstraint.SubmergedOutlet.MonthlyAverageDivertedFlowByIndex[ARow, ACol] := lValue;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end
        else
          TenColsGrid.ValidationError[ACol, ARow, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TPhysicalFlowConstraintValidator.DoContextValidation';
var
  lFeature     : IPhysicalFlowConstraint;
  lFeatureList : IPhysicalFlowConstraintList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PhysicalFlowConstraintList;
      lFeature     := lFeatureList.PhysicalFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType]) then
          ValidateStructureType(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintFeatureName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintUpstreamReservoir]) then
          ValidateUpstreamReservoir(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintDownstreamReservoir]) then
          ValidateDownstreamReservoir(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintElevationOfSill]) then
          ValidateElevationOfSill(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintMaximumGateHeight]) then
          ValidateMaximumGateHeight(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintDischargeCoefficient]) then
          ValidateDischargeCoefficient(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintStructureLength]) then
          ValidateStructureLength(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintArrays]) then
          ValidateElevations(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintArrays]) then
          ValidateDischarges(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintArraysType10]) then
          ValidatePipeChannelNumbers(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintArraysType10]) then
          ValidateKFactors(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintArraysType11]) then
          ValidateAquiferHeads(lFeature);
        if (AValidationType in [dvtPhysConstraint, dvtPhysConstraintStructureType,
                                dvtPhysConstraintArraysType11]) then
          ValidateAquiferFlows(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPhysicalFlowConstraintValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TPhysicalFlowConstraintValidator.DetermineWizardStatus';
var
  lFeature       : IPhysicalFlowConstraint;
  lFeatureList   : IPhysicalFlowConstraintList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PhysicalFlowConstraintList;
      lFeature := lFeatureList.PhysicalFlowConstraintByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtPhysConstraint);
        if (lFeature.StructureType > 1) then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateStructureType(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateStructureType';
begin
  try
    if (AFeature <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'StructureType')) then
          StructureTypeCbx.InValidationError := FALSE
        else
        begin
          StructureTypeCbx.InValidationError := TRUE;
          StructureTypeCbx.ValidationError := FErrorMessage;
          StructureTypeCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateUpstreamReservoir(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateUpstreamReservoir';
begin
  try
    if (AFeature <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'UpStreamReservoirNumber')) then
          UpstreamReservoirCbx.InValidationError := FALSE
        else
        begin
          UpstreamReservoirCbx.InValidationError := TRUE;
          UpstreamReservoirCbx.ValidationError := FErrorMessage;
          UpstreamReservoirCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateDownstreamReservoir(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateDownstreamReservoir';
begin
  try
    if (AFeature <> nil) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'DownStreamReservoirNumber')) then
          DownstreamReservoirCbx.InValidationError := FALSE
        else
        begin
          DownstreamReservoirCbx.InValidationError := TRUE;
          DownstreamReservoirCbx.ValidationError := FErrorMessage;
          DownstreamReservoirCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateElevationOfSill(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateElevationOfSill';
var
   lReservoir     : IReservoirData;
   lReservoirList : IReservoirDataList;
   lReservoirNr   : Integer;
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [2,3])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (NOT AFeature.Validate(FErrorMessage, 'ElevationOfSill')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        if (Trim(FErrorMessage) ='') then
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
            if (UpstreamReservoirCbx.ItemIndex >= 1) then
            begin
              lReservoirNr := Integer(UpstreamReservoirCbx.Items.Objects[UpstreamReservoirCbx.ItemIndex]);
              lReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
                if (StrToFloat(ElevationOfSillEdit.Text)>lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation) or
                (StrToFloat(ElevationOfSillEdit.Text) < lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation) then
                   FErrorMessage := FAppModules.Language.GetString('ContextValidation.SILLOutOfRange');
            end;
        end;
        ElevationOfSillEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateMaximumGateHeight(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateMaximumGateHeight';
var
   lReservoir     : IReservoirData;
   lReservoirList : IReservoirDataList;
   lReservoirNr   : Integer;   
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [2,3,10])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (NOT AFeature.Validate(FErrorMessage, 'MaximumGateHeight')) then
          FAllErrorMessages.Add(Trim(FErrorMessage))
        else
          if (AFeature.StructureType in [2,3]) then
          begin
            FErrorMessage := '';
            if (StrToFloat(GateHeightEdit.Text) <= StrToFloat(ElevationOfSillEdit.Text)) then
              FErrorMessage := FAppModules.Language.GetString('ContextValidation.HOSLNotGreaterThanSILL');
            if (trim(FErrorMessage) ='') then
            begin
              lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
              if (lReservoirList <> nil) then
                if (UpstreamReservoirCbx.ItemIndex >= 1) then
                begin
                  lReservoirNr := Integer(UpstreamReservoirCbx.Items.Objects[UpstreamReservoirCbx.ItemIndex]);
                  lReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
                  begin
                    if (StrToFloat(GateHeightEdit.Text)>lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation) or
                    (StrToFloat(GateHeightEdit.Text) < lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation) then
                       FErrorMessage := FAppModules.Language.GetString('ContextValidation.SILLOutOfRange');
                  end;
                end;
            end;
          end;
      GateHeightEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateDischargeCoefficient(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateDischargeCoefficient';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [2,3])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (NOT AFeature.Validate(FErrorMessage, 'DischargeCoefficient')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        DischargeCoefficientEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateStructureLength(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateStructureLength';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [2,3,6])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (NOT AFeature.Validate(FErrorMessage, 'StructureLength')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        StructureLengthEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateElevations(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateElevations';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [4,5,7,8,9,14])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Elevations')) then
          TwoColsGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          TwoColsGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateDischarges(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateDischarges';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [4,5,7,8,9,14])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Discharges')) then
          TwoColsGrid.ValidationError[2, 0, gveColContext] := ''
        else
        begin
          TwoColsGrid.ValidationError[2, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidatePipeChannelNumbers(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidatePipeChannelNumbers';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType = 10)) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'PipeChannelNumbers')) then
          TwoColsGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          TwoColsGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//////////////////////////////////////////
procedure TPhysicalFlowConstraintValidator.ValidateWaterLevelAtDownstreamNode(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateWaterLevelAtDownstreamNode';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [13])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (NOT AFeature.Validate(FErrorMessage, 'WaterLevelAtDownstreamNode')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        WaterLevelAtDownstreamNodeEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateReferenceElevation(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateReferenceElevation';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType in [12])) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (NOT AFeature.Validate(FErrorMessage, 'ReferenceElevation')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        ReferenceElevationEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
/////////////////////////////////////////

procedure TPhysicalFlowConstraintValidator.ValidateKFactors(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateKFactors';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType = 10)) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'KFactors')) then
          TwoColsGrid.ValidationError[2, 0, gveColContext] := ''
        else
        begin
          TwoColsGrid.ValidationError[2, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateAquiferHeads(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateAquiferHeads';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType = 11)) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'AquiferHeads')) then
          TwoColsGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          TwoColsGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateAquiferFlows(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateAquiferFlows';
begin
  try
    if ((AFeature <> nil) AND (AFeature.StructureType = 11)) then
    begin
      with PhysicalFlowConstraintDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'AquiferFlows')) then
          FourColsGrid.ValidationError[2, 0, gveColContext] := ''
        else
        begin
          FourColsGrid.ValidationError[2, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.ValidateFeatureName(AFeature: IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.ValidateFeatureName';
begin
  try
    with PhysicalFlowConstraintDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.RepopulateGrids(AFeature : IPhysicalFlowConstraint);
const OPNAME = 'TPhysicalFlowConstraintValidator.RepopulateGrids';
var
  LRow,LCol: integer;

begin
  try
    with PhysicalFlowConstraintDialog do
    begin
      NrPointsEdit.Text := IntToStr(AFeature.NrOfPoints);
      SetNumberOfPoints(AFeature.NrOfPoints);
      SetLanguagePerType(AFeature.StructureType);
      case AFeature.StructureType of
        1:
        begin
        end;
        2,3:
        begin
        end;
        6:
        begin
        end;
        4,5,7,8,9,14:
        begin
          for LRow := 1 to AFeature.NrOfPoints do
          begin
            TwoColsGrid.Cells[1,LRow] := FloatToStr(AFeature.DischargeCurve.ElevationByIndex[LRow]);
            TwoColsGrid.Cells[2,LRow] := FloatToStr(AFeature.DischargeCurve.DischargeByIndex[LRow]);
          end;
        end;
        10:
        begin
          for LRow := 1 to AFeature.NrOfPoints do
          begin
            TwoColsGrid.Cells[1,LRow] := FloatToStr(AFeature.KFactors.ChannelNumberByIndex[LRow]);
            TwoColsGrid.Cells[2,LRow] := FloatToStr(AFeature.KFactors.KFactorByIndex[LRow]);
          end;
        end;
        11:
        begin
          for LRow := 1 to AFeature.NrOfPoints do
          begin
            FourColsGrid.Cells[1,LRow] := FloatToStr(AFeature.SandAquifer.HeadDifferenceByIndex[LRow]);
            FourColsGrid.Cells[2,LRow] := FloatToStr(AFeature.SandAquifer.AquiferFlowByIndex[LRow]);
            FourColsGrid.Cells[3,LRow] := FloatToStr(AFeature.SandAquifer.DownStreamNodeInflowByIndex[LRow]);
            FourColsGrid.Cells[4,LRow] := FloatToStr(AFeature.SandAquifer.RiverDepthByIndex[LRow]);
          end;
        end;
        12:
        begin
          for LRow := 1 to AFeature.NrOfPoints do
          begin
{            TwoColsGrid.Cells[1,LRow] := FloatToStr(AFeature.SubmergedOutlet.ElevationDifferenceByIndex[LRow]);
            TwoColsGrid.Cells[2,LRow] := FloatToStr(AFeature.SubmergedOutlet.MonthlyAverageInflowByIndex[LRow]); }
            ElevationDifferenceGrid.Cells[LRow,0] := FloatToStr(AFeature.SubmergedOutlet.ElevationDifferenceByIndex[LRow]);
            TenColsGrid.Cells[1,LRow] := FloatToStr(AFeature.SubmergedOutlet.MonthlyAverageInflowByIndex[LRow]);
            for LCol := 1 to 10 do
              TenColsGrid.Cells[LCol+1,LRow] := FloatToStr(AFeature.SubmergedOutlet.MonthlyAverageDivertedFlowByIndex[LRow,LCol]);
          end;
        end;
        13:
        begin
          for LRow := 1 to AFeature.NrOfPoints do
          begin
            TwoColsGrid.Cells[1,LRow] := FloatToStr(AFeature.PumpStation.PumpingHeadByIndex[LRow]);
            TwoColsGrid.Cells[2,LRow] := FloatToStr(AFeature.PumpStation.PumpingDischargeByIndex[LRow]);
          end;
        end;
      end;//Case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteTwoColsGridColumnData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteTwoColsGridColumnData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LCol,
  LRow        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.TwoColsGrid) then
      begin
        LCol := PhysicalFlowConstraintDialog.TwoColsGrid.Col; 
        if LConstraint.StructureType in [4,5,7,8,9,14] then
        begin
          if(LCol = 1) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.DischargeCurve.ElevationByIndex[LRow] := LValue;
            end;
          end;
          if(LCol = 2) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.DischargeCurve.DischargeByIndex[LRow] := LValue;
            end;
          end;
        end
        else
        if LConstraint.StructureType in [10] then
        begin
          if(LCol = 1) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.KFactors.ChannelNumberByIndex[LRow] := LValue;
            end;
          end
          else
          if(LCol = 2) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.KFactors.KFactorByIndex[LRow] := LValue;
            end;
          end;
        end
        else
        if LConstraint.StructureType in [12] then
        begin
          if(LCol = 1) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.SubmergedOutlet.ElevationDifferenceByIndex[LRow] := LValue;
            end;
          end
          else
          if(LCol = 2) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.SubmergedOutlet.MonthlyAverageInflowByIndex[LRow] := LValue;
              RepopulateGrids(LConstraint);
            end;
          end;
        end
        else
        if LConstraint.StructureType in [13] then
        begin
          if(LCol = 1) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.PumpStation.PumpingHeadByIndex[LRow] := LValue;
            end;
          end
          else
          if(LCol = 2) then
          begin
            for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
            begin
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[LCol,LRow]));
              LConstraint.PumpStation.PumpingDischargeByIndex[LRow] := LValue;
            end;
          end;
          RepopulateGrids(LConstraint);
        end;
        DoContextValidation(dvtPhysConstraintArrays);
        DoContextValidation(dvtPhysConstraintArraysType10);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteTwoColsGridData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteTwoColsGridData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LRow        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.TwoColsGrid) then
      begin
        if LConstraint.StructureType in [4,5,7,8,9,14] then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[1,LRow]));
            LConstraint.DischargeCurve.ElevationByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[2,LRow]));
            LConstraint.DischargeCurve.DischargeByIndex[LRow] := LValue;
          end;
        end
        else
        if LConstraint.StructureType in [10] then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[1,LRow]));
            LConstraint.KFactors.ChannelNumberByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[2,LRow]));
            LConstraint.KFactors.KFactorByIndex[LRow] := LValue;
          end;
        end
        else
        if LConstraint.StructureType in [12] then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[1,LRow]));
            LConstraint.SubmergedOutlet.ElevationDifferenceByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[2,LRow]));
            LConstraint.SubmergedOutlet.MonthlyAverageInflowByIndex[LRow] := LValue;
          end;
        end
        else
        if LConstraint.StructureType in [13] then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[1,LRow]));
            LConstraint.PumpStation.PumpingHeadByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TwoColsGrid.Cells[2,LRow]));
            LConstraint.PumpStation.PumpingDischargeByIndex[LRow] := LValue;
          end;
        end;
        RepopulateGrids(LConstraint);
        DoContextValidation(dvtPhysConstraintArrays);
        DoContextValidation(dvtPhysConstraintArraysType10);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteFourColsGridColumnData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteFourColsGridColumnData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LCol,
  LRow        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.FourColsGrid) then
      begin
        LCol := PhysicalFlowConstraintDialog.FourColsGrid.Col;
        if LConstraint.StructureType in [11] then
        begin
          case LCol of
            1:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[LCol,LRow]));
                  LConstraint.SandAquifer.HeadDifferenceByIndex[LRow] := LValue;
                end;
              end;
            2:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[LCol,LRow]));
                  LConstraint.SandAquifer.AquiferFlowByIndex[LRow] := LValue;
                end;
              end;
            3:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[LCol,LRow]));
                  LConstraint.SandAquifer.DownStreamNodeInflowByIndex[LRow] := LValue;
                end;
              end;
            4:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[LCol,LRow]));
                  LConstraint.SandAquifer.RiverDepthByIndex[LRow] := LValue;
                end;
              end;
          end;
          RepopulateGrids(lConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteFourColsGridData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteFourColsGridData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LRow        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.FourColsGrid) then
      begin
        if LConstraint.StructureType in [11] then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[1,LRow]));
            LConstraint.SandAquifer.HeadDifferenceByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[2,LRow]));
            LConstraint.SandAquifer.AquiferFlowByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[3,LRow]));
            LConstraint.SandAquifer.DownStreamNodeInflowByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.FourColsGrid.Cells[4,LRow]));
            LConstraint.SandAquifer.RiverDepthByIndex[LRow] := LValue;
          end;
          RepopulateGrids(LConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteTenColsGridColumnData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteTenColsGridColumnData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LCol,
  LRow        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.TenColsGrid) then
      begin
        if LConstraint.StructureType in [12] then
        begin
          LCol := PhysicalFlowConstraintDialog.TenColsGrid.Col;
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TenColsGrid.Cells[LCol,LRow]));
            LConstraint.SubmergedOutlet.MonthlyAverageDivertedFlowByIndex[LRow,LCol] := LValue;
          end;
          RepopulateGrids(LConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteTenColsGridData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteTenColsGridData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LCol,
  LRow        : integer;
begin
  //LValue := 0.0;
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.TenColsGrid) then
      begin
        if LConstraint.StructureType in [12] then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            for LCol := TFieldStringGrid(Sender).FixedCols to TFieldStringGrid(Sender).ColCount - 1 do
            begin
              if (LCol = 1) then
                continue
              else
              LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TenColsGrid.Cells[LCol,LRow]));
              LConstraint.SubmergedOutlet.MonthlyAverageDivertedFlowByIndex[LRow,LCol] := LValue;
            end;
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.TenColsGrid.Cells[1,LRow]));
            LConstraint.SubmergedOutlet.MonthlyAverageInflowByIndex[LRow] := LValue;
          end;
          RepopulateGrids(LConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteElevationDifferenceGridData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteElevationDifferenceGridData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LCol        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.ElevationDifferenceGrid) then
      begin
        if LConstraint.StructureType in [12] then
        begin
          for LCol := TFieldStringGrid(Sender).FixedCols to TFieldStringGrid(Sender).ColCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.ElevationDifferenceGrid.Cells[0,LCol]));
            LConstraint.SubmergedOutlet.ElevationDifferenceByIndex[LCol] := LValue;
          end;
          RepopulateGrids(LConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPhysicalFlowConstraintValidator.OnAfterPasteElevationDifferenceGridColumnData(Sender: TObject);
const OPNAME = 'TPhysicalFlowConstraintValidator.OnAfterPasteElevationDifferenceGridColumnData';
var
  LConstraint : IPhysicalFlowConstraint;
  LValue      : double;
  LCol        : integer;
begin
  try
    LConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PhysicalFlowConstraintList.PhysicalFlowConstraintByID[FFeatureID];
    if(LConstraint <> nil)then
    begin
      if(Sender = PhysicalFlowConstraintDialog.ElevationDifferenceGrid) then
      begin
        if LConstraint.StructureType in [12] then
        begin
          for LCol := TFieldStringGrid(Sender).FixedCols to TFieldStringGrid(Sender).ColCount - 1 do
          begin
            LValue := StrToFloat(Trim(PhysicalFlowConstraintDialog.ElevationDifferenceGrid.Cells[LCol,0]));
            LConstraint.SubmergedOutlet.MonthlyAverageInflowByIndex[LCol] := LValue;
          end;
          RepopulateGrids(LConstraint);
          DoContextValidation(dvtPhysConstraintArraysType10);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

