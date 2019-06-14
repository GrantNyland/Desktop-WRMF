{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockCropRequirementValidator.                       *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockCropRequirementValidator;

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
  UIrrBlockCropRequirementDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UIrrigationBlock,
  VCL.Dialogs;

type
  TIrrBlockCropRequirementValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FLastCropNameSelected : string;
    FSelectedCol,
    FSelectedRow          : Integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnBoxCropWaterUseTypeChange (Sender: TObject);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure OnGridSelectCell(ASender: TObject;ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnAfterPasteGridData(Sender: TObject);
    procedure OnAfterPasteColumnData(Sender: TObject);

    procedure RePopulateDataViewer;

    procedure UpdateCropWaterUseType;
    procedure UpdateNumberOfCropTypes;
    procedure UpdateRainAboveRainFactorSpecValue;
    procedure UpdateRainBelowRainFactor;
    procedure UpdateWaterUseCropName(ARow: Integer; AValue : string);
    procedure UpdatePercAreaUnderCropType(ARow: Integer; AValue : double);
    procedure UpdateWaterUseGrid(ACol, ARow: Integer; AValue : double);
    procedure SetFieldEnabledState;

    function ValidatePercAreaTotal: Boolean;
    procedure ValidateNumberOfCropTypes;
    procedure ValidateRainAboveRainFactorSpecValue;
    procedure ValidateRainBelowRainFactor;
    procedure ValidateWaterUseCropName(AFeature: IWaterUsage);
    procedure ValidatePercAreaUnderCropType(AFeature: IWaterUsage);
    procedure ValidateWaterUseGrid(AFeature: IWaterUsage);
    procedure ValidateCropWaterUseType(AFeature: IWaterUsage);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function CropRequirementDialog: TIrrBlockCropRequirementDialog;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  Math,
  UNetworkFeaturesData;

{******************************************************************************}
{* TIrrBlockCropRequirementValidator                                                        *}
{******************************************************************************}

procedure TIrrBlockCropRequirementValidator.CreateMemberObjects;
const OPNAME = 'TIrrBlockCropRequirementValidator.CreateMemberObjects';
var
  LIndex : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrBlockCropRequirementDialog.Create(FPanelOwner,FAppModules);

    with CropRequirementDialog do
    begin
      for LIndex := 0 to ControlsParent.ComponentCount - 1 do
      begin
        if (ControlsParent.Components[lIndex].ClassNameIs('TFieldEdit')) then
        begin
          TFieldEdit(ControlsParent.Components[LIndex]).OnEnter  := OnEditControlEnter;
          TFieldEdit(ControlsParent.Components[LIndex]).OnExit   := OnEditControltExit;
        end;

        if (ControlsParent.Components[LIndex].ClassNameIs('TFieldStringGrid')) then
        begin
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnBeforeCellChange   := OnStringGridCellDataHasChanged;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnSelectCell         := OnGridSelectCell;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnColEnter           := OnStringGridColEnter;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnExit               := OnEditControltExit;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnEnter              := OnEditControlEnter;
          TFieldStringGrid(ControlsParent.Components[LIndex]).ShowGridPopupMenu    := True;
          TFieldStringGrid(ControlsParent.Components[LIndex]).AllowPasteFromExcel  := True;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnPasteFromExcel     := Self.OnAfterPasteGridData;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
          TFieldStringGrid(ControlsParent.Components[LIndex]).OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;
        end;
      end;

      NumberOfCropTypesEdit.FieldProperty            := FAppModules.FieldProperties.FieldProperty('IrrigationBlockNumberOfCropTypes');
      RainAboveRainFactorSpecValueEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockRainAboveRainFactorSpecValue');
      RainBelowRainFactorEdit.FieldProperty          := FAppModules.FieldProperties.FieldProperty('IrrigationBlockRainBelowRainFactor');

      cmbBoxCropWaterUseType.OnEnter        := OnEditControlEnter;
      cmbBoxCropWaterUseType.OnChange       := OnBoxCropWaterUseTypeChange ;
      cmbBoxCropWaterUseType.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockCropWaterUseType');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.DestroyMemberObjects;
const OPNAME = 'TIrrBlockCropRequirementValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementValidator.Initialise: boolean;
const OPNAME = 'TIrrBlockCropRequirementValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with CropRequirementDialog.MonthlyWaterUsageGrid do
    begin
      Cells[0,0] := FAppModules.Language.GetString('GridHeading.CropNumber');
      Cells[1,0] := FAppModules.Language.GetString('GridHeading.CropType');
      Cells[2,0] := FAppModules.Language.GetString('GridHeading.CropTypePercArea');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockCropRequirementValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.CropWater');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ClearDataViewer;
const OPNAME = 'TIrrBlockCropRequirementValidator.ClearDataViewer';
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
    with CropRequirementDialog do
    begin
      for lIndex := 1 to 14 do
        MonthlyWaterUsageGrid.Cells[lIndex, 1] := '';
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
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.PopulateDataViewer;
const OPNAME = 'TIrrBlockCropRequirementValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrBlockCropRequirementValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrBlockCropRequirementValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with CropRequirementDialog do
    begin
      if((Sender = NumberOfCropTypesEdit) and (NumberOfCropTypesEdit.HasValueChanged ))then
        UpdateNumberOfCropTypes;
      if((Sender = RainAboveRainFactorSpecValueEdit) and (RainAboveRainFactorSpecValueEdit.HasValueChanged ))then
        UpdateRainAboveRainFactorSpecValue;
      if((Sender = RainBelowRainFactorEdit) and (RainBelowRainFactorEdit.HasValueChanged ))then
        UpdateRainBelowRainFactor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.RePopulateDataViewer;
const OPNAME = 'TIrrBlockCropRequirementValidator.RePopulateDataViewer';
var
  lConfigData                 : IRunConfigurationData;
  lCnt,
  LCol,
  LRow                        : Integer;
  lIrrigationBlock            : IIrrigationBlock;
  LFieldPropertyCropName      : TAbstractFieldProperty;
  LFieldPropertyPercArea      : TAbstractFieldProperty;
  LFieldPropertyMonthWaterUse : TAbstractFieldProperty;
  LFieldPropertyNil           : TAbstractFieldProperty;
begin
  try
    lConfigData       := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lConfigData <> nil) and (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        LFieldPropertyCropName        := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageCropName');
        LFieldPropertyPercArea        := FAppModules.FieldProperties.FieldProperty('IrrigationBlockPercAreaUnderCropType');
        LFieldPropertyMonthWaterUse   := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageFactor');
        LFieldPropertyNil             := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageFactor');

        NumberOfCropTypesEdit.SetFieldValue(lIrrigationBlock.NumberOfCropTypes);
        RainAboveRainFactorSpecValueEdit.SetFieldValue(lIrrigationBlock.RainAboveRainFactorSpecValue);
        RainBelowRainFactorEdit.SetFieldValue(lIrrigationBlock.RainBelowRainFactor);

        lblCropWaterUseType.Enabled      := (lIrrigationBlock.IrrigationBlockType <> 4);
        cmbBoxCropWaterUseType.IsEnabled := (lIrrigationBlock.IrrigationBlockType <> 4);
        cmbBoxCropWaterUseType.Items.Clear;
        if(lIrrigationBlock.IrrigationBlockType = 4)then
        begin
          //cmbBoxCropWaterUseType.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseType4'));
          cmbBoxCropWaterUseType.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeMonthly'));
          cmbBoxCropWaterUseType.Items.Add( FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeEvapotrans'));
          //lIrrigationBlock.IrrigationBlockType := 4
        end
        else
        begin
          cmbBoxCropWaterUseType.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeMonthly'));
          cmbBoxCropWaterUseType.Items.Add( FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeEvapotrans'));
        end;

        if(lIrrigationBlock.CropWaterUseType = 1) then
          cmbBoxCropWaterUseType.SetFieldIndex(0)
        else if(lIrrigationBlock.CropWaterUseType = 2) then
          cmbBoxCropWaterUseType.SetFieldIndex(1)
        else if(lIrrigationBlock.CropWaterUseType = 4) then
          cmbBoxCropWaterUseType.SetFieldIndex(0);

        NumberOfCropTypesEdit.IsEnabled := (lIrrigationBlock.CropWaterUseType <> 2);

        MonthlyWaterUsageGrid.RowCount  := lIrrigationBlock.NumberOfCropTypes + 1;

        for lCnt := 3 to 14 do
          MonthlyWaterUsageGrid.Cells[lCnt,0]  := lConfigData.MonthNameByIndex[lCnt-2];


        MonthlyWaterUsageGrid.ClearFieldProperties;
        for lRow := 0 to lIrrigationBlock.NumberOfCropTypes do
        begin
          if Assigned(lIrrigationBlock) then
            if Assigned(lIrrigationBlock.WaterUsageFactorByIndex[lRow]) then
            begin
              MonthlyWaterUsageGrid.Rows[lRow+1].Objects[0] := TObject(lIrrigationBlock.WaterUsageFactorByIndex[lRow].Identifier);
              MonthlyWaterUsageGrid.AddFieldProperty(LFieldPropertyNil);
              MonthlyWaterUsageGrid.Cells[0,lRow+1] := IntToStr(lRow+1);
              MonthlyWaterUsageGrid.AddFieldProperty(LFieldPropertyCropName);
              MonthlyWaterUsageGrid.SetFieldValue(1,lRow+1,lIrrigationBlock.WaterUsageFactorByIndex[lRow].CropName);
              MonthlyWaterUsageGrid.AddFieldProperty(LFieldPropertyPercArea);
              MonthlyWaterUsageGrid.SetFieldValue(2,lRow+1, lIrrigationBlock.WaterUsageFactorByIndex[lRow].PercAreaUnderCropType);
              for lCol := 3 to 14 do
              begin
                MonthlyWaterUsageGrid.AddFieldProperty(LFieldPropertyMonthWaterUse);
                MonthlyWaterUsageGrid.SetFieldValue(lCol,lRow+1,lIrrigationBlock.WaterUsageFactorByIndex[lRow].MonthlyWaterUse[lCol-2]);
              end;
            end;
        end;
        if MonthlyWaterUsageGrid.RowCount > 1 then
          MonthlyWaterUsageGrid.FixedRows := 1;
        if(lIrrigationBlock.CropWaterUseType = 2) then
          MonthlyWaterUsageGrid.FixedCols := 3
        else
          MonthlyWaterUsageGrid.FixedCols := 1
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
    try
    with CropRequirementDialog do
    begin
      if (ASender = MonthlyWaterUsageGrid) then
      begin
        case ACol of
          1     : UpdateWaterUseCropName(ARow, Trim(MonthlyWaterUsageGrid.Cells[ACol, ARow]));
          2     : UpdatePercAreaUnderCropType(ARow, StrToFloat(Trim(MonthlyWaterUsageGrid.Cells[ACol, ARow])));
          3..14 : UpdateWaterUseGrid(ACol, ARow, StrToFloat(Trim(MonthlyWaterUsageGrid.Cells[ACol, ARow])));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnGridSelectCell';
begin
  try
    FLastCropNameSelected := '';
    FSelectedCol          := -1;
    FSelectedRow          := -1;
    if ACol = 1 then
      FLastCropNameSelected := CropRequirementDialog.MonthlyWaterUsageGrid.Cells[ACol, ARow];

    if ASender = CropRequirementDialog.MonthlyWaterUsageGrid then
    begin
      FSelectedCol  := ACol;
      FSelectedRow  := ARow;
    end; 

    CanSelect := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementValidator.SaveState: boolean;
const OPNAME = 'TIrrBlockCropRequirementValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrBlockCropRequirementValidator.DoContextValidation';
var
  lIrrigationBlock : IIrrigationBlock;
  LCropName : string;
  LRow : Integer;
begin
  try
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    LRow              := FSelectedRow;
    LCropName         := CropRequirementDialog.MonthlyWaterUsageGrid.Cells[1, LRow];
    if (lIrrigationBlock <> nil) then
    begin
      case AValidationType of
        dvtIrrigationBlockAll                           : begin
                                                            ValidateNumberOfCropTypes;
                                                            ValidateRainAboveRainFactorSpecValue;
                                                            ValidateRainBelowRainFactor;
                                                            ValidateWaterUseCropName(
                                                                lIrrigationBlock.WaterUsageFactorByName(LCropName));
                                                            ValidatePercAreaUnderCropType(
                                                                lIrrigationBlock.WaterUsageFactorByName(LCropName));
                                                            ValidateWaterUseGrid(
                                                                lIrrigationBlock.WaterUsageFactorByName(LCropName));
                                                          end;
        dvtIrrigationBlockNumberOfCropTypes             : ValidateNumberOfCropTypes;
        dvtIrrigationBlockRainAboveRainFactorSpecValue  : ValidateRainAboveRainFactorSpecValue;
        dvtIrrigationBlockRainBelowRainFactor           : ValidateRainBelowRainFactor;
        dvtIrrigationBlockWaterUseCropName              : ValidateWaterUseCropName(lIrrigationBlock.WaterUsageFactorByName(LCropName));
        dvtIrrigationBlockWaterUsePercArea              : ValidatePercAreaUnderCropType(lIrrigationBlock.WaterUsageFactorByName(LCropName));
        dvtIrrigationBlockWaterUseMonthly               : ValidateWaterUseGrid(lIrrigationBlock.WaterUsageFactorByName(LCropName));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockCropRequirementValidator.CropRequirementDialog: TIrrBlockCropRequirementDialog;
const OPNAME = 'TIrrBlockCropRequirementValidator.CropRequirementDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrBlockCropRequirementDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdateNumberOfCropTypes;
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdateNumberOfCropTypes';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        NumberOfCropTypesEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                NumberOfCropTypesEdit.FieldProperty.FieldName,
                NumberOfCropTypesEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.NumberOfCropTypes := StrToInt(NumberOfCropTypesEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockNumberOfCropTypes);
        end else
          NumberOfCropTypesEdit.FieldValidationError := LMessage;        
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdateRainAboveRainFactorSpecValue;
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdateRainAboveRainFactorSpecValue';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        RainAboveRainFactorSpecValueEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                RainAboveRainFactorSpecValueEdit.FieldProperty.FieldName,
                RainAboveRainFactorSpecValueEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.RainAboveRainFactorSpecValue := StrToFloat(RainAboveRainFactorSpecValueEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockRainAboveRainFactorSpecValue);
        end
        else
          RainAboveRainFactorSpecValueEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdateRainBelowRainFactor;
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdateRainBelowRainFactor';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        RainBelowRainFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                RainBelowRainFactorEdit.FieldProperty.FieldName,
                RainBelowRainFactorEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.RainBelowRainFactor := StrToFloat(RainBelowRainFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockRainBelowRainFactor);
        end
        else
          RainBelowRainFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidateNumberOfCropTypes;
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidateNumberOfCropTypes';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        NumberOfCropTypesEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,NumberOfCropTypesEdit.FieldProperty.FieldName) then
          NumberOfCropTypesEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidateRainAboveRainFactorSpecValue;
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidateRainAboveRainFactorSpecValue';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        RainAboveRainFactorSpecValueEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,RainAboveRainFactorSpecValueEdit.FieldProperty.FieldName) then
          RainAboveRainFactorSpecValueEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidateRainBelowRainFactor;
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidateRainBelowRainFactor';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        RainBelowRainFactorEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,RainBelowRainFactorEdit.FieldProperty.FieldName) then
          RainBelowRainFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdatePercAreaUnderCropType(ARow: Integer; AValue: double);
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdatePercAreaUnderCropType';
var
  lIrrigationBlock      : IIrrigationBlock;
  LMessage,
  LCropName,
  LFieldPropertyName    : string;
  LFieldPropertyIndex,
  LCount                : Integer;
begin
  LFieldPropertyIndex := 1;
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        for LCount:=0 to MonthlyWaterUsageGrid.FieldPropertiesCount -1 do
        begin
          LFieldPropertyName := MonthlyWaterUsageGrid.FieldProperty(LCount).FieldName;
          if (UpperCase(LFieldPropertyName) = UpperCase('IrrigationBlockPercAreaUnderCropType')) then
          begin
            LFieldPropertyIndex := LCount;
            Break;
          end;
        end;

        MonthlyWaterUsageGrid.ValidationError[2, ARow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            TAbstractFieldProperty( MonthlyWaterUsageGrid.FieldProperty(LFieldPropertyIndex)).FieldName,
                                    FloatToStr(AValue), LMessage, 2, ARow)) then
        begin
          LCropName := MonthlyWaterUsageGrid.Cells[1, ARow];
          if lIrrigationBlock.WaterUsageFactorByName(LCropName) <> nil then
            lIrrigationBlock.WaterUsageFactorByName(LCropName).PercAreaUnderCropType := AValue;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockWaterUsePercArea);
        end else
          MonthlyWaterUsageGrid.ValidationError[2, ARow, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdateWaterUseCropName(ARow: Integer; AValue: string);
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdateWaterUseCropName';
var
  lIrrigationBlock    : IIrrigationBlock;
  LMessage,
  LCropName,
  LFieldPropertyName  : string;
  LFieldPropertyIndex,
  LCount              : Integer;
begin
  LFieldPropertyIndex := 0;
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        for LCount:=0 to MonthlyWaterUsageGrid.FieldPropertiesCount -1 do
        begin
          LFieldPropertyName := MonthlyWaterUsageGrid.FieldProperty(LCount).FieldName;
          if (UpperCase(LFieldPropertyName) = UpperCase('IrrigationBlockWaterUsageCropName')) then
          begin
            LFieldPropertyIndex := LCount;
            Break;
          end;
        end;

        MonthlyWaterUsageGrid.ValidationError[1, ARow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            TAbstractFieldProperty( MonthlyWaterUsageGrid.FieldProperty(LFieldPropertyIndex)).FieldName,
                                    AValue, LMessage, 1, ARow)) then
        begin
          LCropName := Trim(MonthlyWaterUsageGrid.Cells[1, ARow]);
          if FLastCropNameSelected <> LCropName then
            if lIrrigationBlock.WaterUsageFactorByName(FLastCropNameSelected) <> nil then
              lIrrigationBlock.WaterUsageFactorByName(FLastCropNameSelected).CropName := LCropName;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockWaterUseCropName);
        end else
          MonthlyWaterUsageGrid.ValidationError[1, ARow, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdateWaterUseGrid(ACol, ARow: Integer; AValue: double);
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdateWaterUseGrid';
var
  lIrrigationBlock    : IIrrigationBlock;
  LMessage       : string;
  LWaterUsage   : IWaterUsage;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        MonthlyWaterUsageGrid.ValidationError[ACol, ARow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty('IrrigationBlockWaterUsageFactor', FloatToStr(AValue),
           LMessage, ACol-2, ARow)) then
        begin
          LWaterUsage := lIrrigationBlock.WaterUsageFactorByIndex[ARow-1];
          if(LWaterUsage <> nil) then
          begin
            LWaterUsage.MonthlyWaterUse[ACol-2] := AValue;
            RePopulateDataViewer;
            DoContextValidation (dvtIrrigationBlockWaterUseMonthly);
          end;
        end
        else
          MonthlyWaterUsageGrid.ValidationError[ACol, ARow, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidatePercAreaUnderCropType(AFeature: IWaterUsage);
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidatePercAreaUnderCropType';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  lCol := 2;
  try
    if (AFeature <> nil) then
    begin
      with CropRequirementDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'IrrigationBlockPercAreaUnderCropType')) then
          begin
            MonthlyWaterUsageGrid.ValidationError[LCol, FSelectedRow, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                MonthlyWaterUsageGrid.ValidationError[lCol, FSelectedRow, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                MonthlyWaterUsageGrid.ValidationError[lCol, FSelectedRow, gveCellContext] := ''
            end;
            FAllErrorMessages.AddStrings(lErrorMsgs);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
      ValidatePercAreaTotal;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidateWaterUseCropName(AFeature: IWaterUsage);
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidateWaterUseCropName';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  lCol := 1;
  try
    if (AFeature <> nil) then
    begin
      with CropRequirementDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'IrrigationBlockWaterUsageCropName')) then
          begin
            MonthlyWaterUsageGrid.ValidationError[LCol, FSelectedRow, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                MonthlyWaterUsageGrid.ValidationError[lCol, FSelectedRow, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                MonthlyWaterUsageGrid.ValidationError[lCol, FSelectedRow, gveCellContext] := ''
            end;
            FAllErrorMessages.AddStrings(lErrorMsgs);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
      ValidatePercAreaTotal;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidateWaterUseGrid(AFeature: IWaterUsage);
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidateWaterUseGrid';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  try
    if (AFeature <> nil) then
    begin
      with CropRequirementDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'IrrigationBlockWaterUsageFactor')) then
          begin
            for lCol := 3 to 14 do
              MonthlyWaterUsageGrid.ValidationError[LCol, FSelectedRow, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 3 to 14 do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                MonthlyWaterUsageGrid.ValidationError[lCol, FSelectedRow, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                MonthlyWaterUsageGrid.ValidationError[lCol, FSelectedRow, gveCellContext] := ''
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

function TIrrBlockCropRequirementValidator.ValidatePercAreaTotal : Boolean;
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidatePercAreaTotal';
var
  LTotal : Double;
  LRows,
  LCol,
  LCount : integer;
begin
  Result  := False;
  LTotal  := 0.0;
  LCol    := 2;
  try
    with CropRequirementDialog do
    begin
      LRows := MonthlyWaterUsageGrid.RowCount - 1;
      for LCount := 1 to LRows do
        LTotal := LTotal + StrToFloat(MonthlyWaterUsageGrid.Cells[LCol,LCount]);
    end;

    if LTotal > 100 then
      ShowMessage(FAppModules.Language.GetString('Message.CropWaterMaxPercArea'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockCropRequirementValidator.OnBoxCropWaterUseTypeChange (Sender: TObject);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnBoxCropWaterUseTypeChange ';
begin
  try
    if CropRequirementDialog.cmbBoxCropWaterUseType.ItemIndex = 0 then
      CropRequirementDialog.gboxWaterUsage.Caption := FAppModules.Language.GetString('TField.IrrigationBlockWaterUsageFactor')
    else  if CropRequirementDialog.cmbBoxCropWaterUseType.ItemIndex = 1 then
      CropRequirementDialog.gboxWaterUsage.Caption := FAppModules.Language.GetString('TField.IrrigationBlockRepresentativeCropEvapotranspiration');

    UpdateCropWaterUseType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.UpdateCropWaterUseType;
const OPNAME = 'TIrrBlockCropRequirementValidator.UpdateCropWaterUseType';
var
  lIrrigationBlock  : IIrrigationBlock;
  lOldUseType : integer;
  lNewUseType : integer;
  lMessage    : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        lOldUseType := lIrrigationBlock.CropWaterUseType;
        lNewUseType := 0;
        if (cmbBoxCropWaterUseType.ItemIndex = 0) then
          lNewUseType := 1
        else
        if (cmbBoxCropWaterUseType.ItemIndex = 1) then
          lNewUseType := 2;

        if (lOldUseType <> lNewUseType) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(cmbBoxCropWaterUseType.FieldProperty.FieldName,
              IntToStr(lNewUseType),lMessage)) then
          begin
            lIrrigationBlock.CropWaterUseType := lNewUseType;
            RePopulateDataViewer;
          end
          else
            cmbBoxCropWaterUseType.ValidationError := lMessage;
        end;
      end;
        SetFieldEnabledState;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.ValidateCropWaterUseType(AFeature: IWaterUsage);
const OPNAME = 'TIrrBlockCropRequirementValidator.ValidateCropWaterUseType';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        cmbBoxCropWaterUseType.ValidationError := '';
        if not lIrrigationBlock.Validate(LMessage, cmbBoxCropWaterUseType.FieldProperty.FieldName) then
          cmbBoxCropWaterUseType.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnAfterPasteGridData';
var
  LIrrigationBlock    : IIrrigationBlock;
  LCropName           : string;
  LRow,
  LCol                : Integer;
  LWaterUsage         : IWaterUsage;
  LMonthlyWaterUseValue,
  LPercAreaUnderCropTypeValue : double;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if(LIrrigationBlock <> nil) then
    begin
      if(CropRequirementDialog.cmbBoxCropWaterUseType.ItemIndex = 0) then
      begin
        for LRow := CropRequirementDialog.MonthlyWaterUsageGrid.FixedRows to CropRequirementDialog.MonthlyWaterUsageGrid.RowCount - 1 do
        begin
          LCropName := Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[1, LRow]);
          LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1].CropName := LCropName;
          LPercAreaUnderCropTypeValue := StrToFloat(Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[2,LRow]));
          LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1].PercAreaUnderCropType := LPercAreaUnderCropTypeValue;
          for LCol := CropRequirementDialog.MonthlyWaterUsageGrid.FixedCols to 12 do
          begin
            LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
            if(LWaterUsage <> nil) then
            begin
              LMonthlyWaterUseValue := StrToFloat(Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[LCol + 2,LRow]));
              LWaterUsage.MonthlyWaterUse[(LCol)] := LMonthlyWaterUseValue;
            end;
          end;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtIrrigationBlockWaterUseCropName);
        DoContextValidation(dvtIrrigationBlockWaterUsePercArea);
        DoContextValidation(dvtIrrigationBlockWaterUseMonthly);
      end
      else
      if(CropRequirementDialog.cmbBoxCropWaterUseType.ItemIndex = 1) then
      begin
        for LRow := CropRequirementDialog.MonthlyWaterUsageGrid.FixedRows to CropRequirementDialog.MonthlyWaterUsageGrid.RowCount - 1 do
        begin
          for LCol := CropRequirementDialog.MonthlyWaterUsageGrid.FixedCols to CropRequirementDialog.MonthlyWaterUsageGrid.ColCount - 1 do
          begin
            LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
            if(LWaterUsage <> nil) then
            begin
              LMonthlyWaterUseValue := StrToFloat(Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
              LWaterUsage.MonthlyWaterUse[LCol - 2] := LMonthlyWaterUseValue;
            end;
          end;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtIrrigationBlockWaterUseMonthly);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TIrrBlockCropRequirementValidator.OnAfterPasteColumnData';
var
  LIrrigationBlock    : IIrrigationBlock;
  LRow,
  LCol                : Integer;
  LWaterUsage         : IWaterUsage;
  LValue              : double;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if(LIrrigationBlock <> nil) then
    begin
      if(CropRequirementDialog.cmbBoxCropWaterUseType.ItemIndex = 0) then
      begin
        LCol := CropRequirementDialog.MonthlyWaterUsageGrid.Col;
        if(LCol = 2) then
        begin
          for LRow := CropRequirementDialog.MonthlyWaterUsageGrid.FixedRows to CropRequirementDialog.MonthlyWaterUsageGrid.RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
            LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1].PercAreaUnderCropType := LValue;
          end;
        end
        else
        if(LCol > 2) then
        begin
          for LRow := CropRequirementDialog.MonthlyWaterUsageGrid.FixedRows to CropRequirementDialog.MonthlyWaterUsageGrid.RowCount - 1 do
          begin
            LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
            if(LWaterUsage <> nil) then
            begin
              LValue := StrToFloat(Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
              LWaterUsage.MonthlyWaterUse[(LCol - 2)] := LValue;
            end;
          end;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtIrrigationBlockWaterUseCropName);
        DoContextValidation(dvtIrrigationBlockWaterUsePercArea);
        DoContextValidation(dvtIrrigationBlockWaterUseMonthly);
      end
      else
      if(CropRequirementDialog.cmbBoxCropWaterUseType.ItemIndex = 1) then
      begin
        LCol := CropRequirementDialog.MonthlyWaterUsageGrid.Col;
        for LRow := CropRequirementDialog.MonthlyWaterUsageGrid.FixedRows to CropRequirementDialog.MonthlyWaterUsageGrid.RowCount - 1 do
        begin
          LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
          LValue := StrToFloat(Trim(CropRequirementDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
          LWaterUsage.MonthlyWaterUse[LCol - 2] := LValue;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtIrrigationBlockWaterUseMonthly);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockCropRequirementValidator.SetFieldEnabledState;
const OPNAME = 'TIrrBlockCropRequirementValidator.SetFieldEnabledState';
var
  LIrrigationBlock : IIrrigationBlock;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.
                        IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (LIrrigationBlock <> nil) then
    begin
      with CropRequirementDialog do
      begin
        if LIrrigationBlock.CropWaterUseType = 1 then
        begin
          RainAboveRainFactorSpecValueEdit.IsEnabled := True;
          RainBelowRainFactorEdit.IsEnabled := True
        end
        else
        begin
          RainAboveRainFactorSpecValueEdit.IsEnabled := False;
          RainBelowRainFactorEdit.IsEnabled := False;
        end;
        StudyDataHasChanged(sdccEdit,'IrrigationBlockCropWaterUseType','0',IntToStr(LIrrigationBlock.CropWaterUseType));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

