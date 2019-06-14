{******************************************************************************}
{*  UNIT      : Contains the class TCropWaterValidator.                       *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UCropWaterValidator;

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
  UCropWaterDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UIrrigationBlock,
  VCL.Dialogs;

type
  TCropWaterValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FLastCropNameSelected : string;
    FSelectedCol,
    FSelectedRow          : Integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnCropWaterUseTypeRadioGroupOnClick(Sender: TObject);
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
    function CropWaterDialog: TCropWaterDialog;
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
{* TCropWaterValidator                                                        *}
{******************************************************************************}

procedure TCropWaterValidator.CreateMemberObjects;
const OPNAME = 'TCropWaterValidator.CreateMemberObjects';
var
  LIndex : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TCropWaterDialog.Create(FPanelOwner,FAppModules);

    with CropWaterDialog do
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

      CropWaterDialog.CropWaterUseTypeRadioGroup.OnExit        := OnEditControltExit;
      CropWaterDialog.CropWaterUseTypeRadioGroup.OnClick        := OnCropWaterUseTypeRadioGroupOnClick;
      CropWaterDialog.CropWaterUseTypeRadioGroup.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockCropWaterUseType');

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.DestroyMemberObjects;
const OPNAME = 'TCropWaterValidator.DestroyMemberObjects';
begin
  try
  inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterValidator.Initialise: boolean;
const OPNAME = 'TCropWaterValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with CropWaterDialog.MonthlyWaterUsageGrid do
    begin
      Cells[0,0] := FAppModules.Language.GetString('GridHeading.CropNumber');
      Cells[1,0] := FAppModules.Language.GetString('GridHeading.CropType');
      Cells[2,0] := FAppModules.Language.GetString('GridHeading.CropTypePercArea');
    end;
    CropWaterDialog.CropWaterUseTypeRadioGroup.Items.Clear;
    CropWaterDialog.CropWaterUseTypeRadioGroup.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeMonthly'));
    CropWaterDialog.CropWaterUseTypeRadioGroup.Items.Add( FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeEvapotrans'));


  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterValidator.LanguageHasChanged: boolean;
const OPNAME = 'TCropWaterValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.CropWater');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.ClearDataViewer;
const OPNAME = 'TCropWaterValidator.ClearDataViewer';
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
    with CropWaterDialog do
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

procedure TCropWaterValidator.PopulateDataViewer;
const OPNAME = 'TCropWaterValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TCropWaterValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterValidator.StudyHasChanged: boolean;
const OPNAME = 'TCropWaterValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TCropWaterValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TCropWaterValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with CropWaterDialog do
    begin
      if((Sender = NumberOfCropTypesEdit) and (NumberOfCropTypesEdit.HasValueChanged ))then
        UpdateNumberOfCropTypes;
      if((Sender = RainAboveRainFactorSpecValueEdit) and (RainAboveRainFactorSpecValueEdit.HasValueChanged ))then
        UpdateRainAboveRainFactorSpecValue;
      if((Sender = RainBelowRainFactorEdit) and (RainBelowRainFactorEdit.HasValueChanged ))then
        UpdateRainBelowRainFactor;
      if ((Sender = CropWaterUseTypeRadioGroup) {and (CropWaterUseTypeRadioGroup.HasValueChanged )}) then
      begin
        if CropWaterDialog.CropWaterUseTypeRadioGroup.ItemIndex = 0 then
          CropWaterDialog.MonthlyWaterUsageLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockWaterUsageFactor')
        else
          CropWaterDialog.MonthlyWaterUsageLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockRepresentativeCropEvapotranspiration');

        UpdateCropWaterUseType;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.RePopulateDataViewer;
const OPNAME = 'TCropWaterValidator.RePopulateDataViewer';
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
    try
      lConfigData       := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      if (lConfigData <> nil) and (lIrrigationBlock <> nil) then
      begin
        with CropWaterDialog do
        begin
          //CropWaterUseTypeRadioGroup.OnEnter        := nil;
          //CropWaterUseTypeRadioGroup.OnExit         := nil;
          //CropWaterUseTypeRadioGroup.OnClick        := nil;

          LFieldPropertyCropName        := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageCropName');
          LFieldPropertyPercArea        := FAppModules.FieldProperties.FieldProperty('IrrigationBlockPercAreaUnderCropType');
          LFieldPropertyMonthWaterUse   := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageFactor');
          LFieldPropertyNil             := FAppModules.FieldProperties.FieldProperty('IrrigationBlockWaterUsageFactor');

          NumberOfCropTypesEdit.SetFieldValue(lIrrigationBlock.NumberOfCropTypes);
          RainAboveRainFactorSpecValueEdit.SetFieldValue(lIrrigationBlock.RainAboveRainFactorSpecValue);
          RainBelowRainFactorEdit.SetFieldValue(lIrrigationBlock.RainBelowRainFactor);

          CropWaterUseTypeLabel.Enabled      := (lIrrigationBlock.IrrigationBlockType <> 4);

          CropWaterUseTypeRadioGroup.IsEnabled := (lIrrigationBlock.IrrigationBlockType <> 4);

          {if(lIrrigationBlock.IrrigationBlockType = 4)then
          begin
            //CropWaterUseTypeRadioGroup.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseType4'));
            //CropWaterUseTypeRadioGroup.ItemIndex := 0;
            CropWaterUseTypeRadioGroup.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeMonthly'));
            CropWaterUseTypeRadioGroup.Items.Add( FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeEvapotrans'));

          end
          else
          begin
            CropWaterUseTypeRadioGroup.Items.Add(FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeMonthly'));
            CropWaterUseTypeRadioGroup.Items.Add( FAppModules.Language.GetString('TField.IrrigationBlockCropWaterUseTypeEvapotrans'));
          end; }
          if(lIrrigationBlock.CropWaterUseType = 2) then
            CropWaterUseTypeRadioGroup.ItemIndex := 1
          else
            CropWaterUseTypeRadioGroup.ItemIndex := 0;

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
    finally

      //CropWaterDialog.CropWaterUseTypeRadioGroup.OnExit        := OnEditControltExit;
      //CropWaterDialog.CropWaterUseTypeRadioGroup.OnClick        := OnEditControltExit; //OnCropWaterUseTypeRadioGroupOnClick;
      //CropWaterDialog.CropWaterUseTypeRadioGroup.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockCropWaterUseType');

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TCropWaterValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
    try
    with CropWaterDialog do
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

procedure TCropWaterValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TCropWaterValidator.OnGridSelectCell';
begin
  try
    FLastCropNameSelected := '';
    FSelectedCol          := -1;
    FSelectedRow          := -1;
    if ACol = 1 then
      FLastCropNameSelected := CropWaterDialog.MonthlyWaterUsageGrid.Cells[ACol, ARow];

    if ASender = CropWaterDialog.MonthlyWaterUsageGrid then
    begin
      FSelectedCol  := ACol;
      FSelectedRow  := ARow;
    end; 

    CanSelect := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCropWaterValidator.SaveState: boolean;
const OPNAME = 'TCropWaterValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TCropWaterValidator.DoContextValidation';
var
  lIrrigationBlock : IIrrigationBlock;
  LCropName : string;
  LRow : Integer;
begin
  try
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    LRow              := FSelectedRow;
    LCropName         := CropWaterDialog.MonthlyWaterUsageGrid.Cells[1, LRow];
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

function TCropWaterValidator.CropWaterDialog: TCropWaterDialog;
const OPNAME = 'TCropWaterValidator.CropWaterDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TCropWaterDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.UpdateNumberOfCropTypes;
const OPNAME = 'TCropWaterValidator.UpdateNumberOfCropTypes';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
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

procedure TCropWaterValidator.UpdateRainAboveRainFactorSpecValue;
const OPNAME = 'TCropWaterValidator.UpdateRainAboveRainFactorSpecValue';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
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

procedure TCropWaterValidator.UpdateRainBelowRainFactor;
const OPNAME = 'TCropWaterValidator.UpdateRainBelowRainFactor';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
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

procedure TCropWaterValidator.ValidateNumberOfCropTypes;
const OPNAME = 'TCropWaterValidator.ValidateNumberOfCropTypes';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
      begin
        NumberOfCropTypesEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,NumberOfCropTypesEdit.FieldProperty.FieldName) then
          NumberOfCropTypesEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.ValidateRainAboveRainFactorSpecValue;
const OPNAME = 'TCropWaterValidator.ValidateRainAboveRainFactorSpecValue';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
      begin
        RainAboveRainFactorSpecValueEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,RainAboveRainFactorSpecValueEdit.FieldProperty.FieldName) then
          RainAboveRainFactorSpecValueEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.ValidateRainBelowRainFactor;
const OPNAME = 'TCropWaterValidator.ValidateRainBelowRainFactor';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
      begin
        RainBelowRainFactorEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,RainBelowRainFactorEdit.FieldProperty.FieldName) then
          RainBelowRainFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.UpdatePercAreaUnderCropType(ARow: Integer; AValue: double);
const OPNAME = 'TCropWaterValidator.UpdatePercAreaUnderCropType';
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
      with CropWaterDialog do
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

procedure TCropWaterValidator.UpdateWaterUseCropName(ARow: Integer; AValue: string);
const OPNAME = 'TCropWaterValidator.UpdateWaterUseCropName';
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
      with CropWaterDialog do
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

procedure TCropWaterValidator.UpdateWaterUseGrid(ACol, ARow: Integer; AValue: double);
const OPNAME = 'TCropWaterValidator.UpdateWaterUseGrid';
var
  lIrrigationBlock    : IIrrigationBlock;
  LMessage       : string;
  LWaterUsage   : IWaterUsage;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
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

procedure TCropWaterValidator.ValidatePercAreaUnderCropType(AFeature: IWaterUsage);
const OPNAME = 'TCropWaterValidator.ValidatePercAreaUnderCropType';
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
      with CropWaterDialog do
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

procedure TCropWaterValidator.ValidateWaterUseCropName(AFeature: IWaterUsage);
const OPNAME = 'TCropWaterValidator.ValidateWaterUseCropName';
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
      with CropWaterDialog do
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

procedure TCropWaterValidator.ValidateWaterUseGrid(AFeature: IWaterUsage);
const OPNAME = 'TCropWaterValidator.ValidateWaterUseGrid';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  try
    if (AFeature <> nil) then
    begin
      with CropWaterDialog do
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

function TCropWaterValidator.ValidatePercAreaTotal : Boolean;
const OPNAME = 'TCropWaterValidator.ValidatePercAreaTotal';
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
    with CropWaterDialog do
    begin
      LRows := MonthlyWaterUsageGrid.RowCount - 1;
      for LCount := 1 to LRows do
        LTotal := LTotal + StrToFloat(MonthlyWaterUsageGrid.Cells[LCol,LCount]);
    end;

    if LTotal > 100 then
      ShowMessage(FAppModules.Language.GetString('Message.CropWaterMaxPercArea'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TCropWaterValidator.OnCropWaterUseTypeRadioGroupOnClick(Sender: TObject);
const OPNAME = 'TCropWaterValidator.OnCropWaterUseTypeRadioGroupOnClick';
begin
  try
    if CropWaterDialog.CropWaterUseTypeRadioGroup.ItemIndex = 0 then
      CropWaterDialog.MonthlyWaterUsageLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockWaterUsageFactor')
    else
      CropWaterDialog.MonthlyWaterUsageLabel.Caption := FAppModules.Language.GetString('TField.IrrigationBlockRepresentativeCropEvapotranspiration');

    UpdateCropWaterUseType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.UpdateCropWaterUseType;
const OPNAME = 'TCropWaterValidator.UpdateCropWaterUseType';
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
      with CropWaterDialog do
      begin
        lOldUseType := lIrrigationBlock.CropWaterUseType;
        lNewUseType := 0;
        if (CropWaterUseTypeRadioGroup.ItemIndex = 0) then
          lNewUseType := 1
        else
        if (CropWaterUseTypeRadioGroup.ItemIndex = 1) then
          lNewUseType := 2;

        if (lOldUseType <> lNewUseType) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(CropWaterUseTypeRadioGroup.FieldProperty.FieldName,
              IntToStr(lNewUseType),lMessage)) then
          begin
            lIrrigationBlock.CropWaterUseType := lNewUseType;
            RePopulateDataViewer;
          end
          else
            CropWaterUseTypeRadioGroup.ValidationError := lMessage;
        end;
      end;
        SetFieldEnabledState;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.ValidateCropWaterUseType(AFeature: IWaterUsage);
const OPNAME = 'TCropWaterValidator.ValidateCropWaterUseType';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
      begin
        CropWaterUseTypeRadioGroup.ValidationError := '';
        if not lIrrigationBlock.Validate(LMessage, CropWaterUseTypeRadioGroup.FieldProperty.FieldName) then
          CropWaterUseTypeRadioGroup.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TCropWaterValidator.OnAfterPasteGridData';
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
      if(CropWaterDialog.CropWaterUseTypeRadioGroup.ItemIndex = 0) then
      begin
        for LRow := CropWaterDialog.MonthlyWaterUsageGrid.FixedRows to CropWaterDialog.MonthlyWaterUsageGrid.RowCount - 1 do
        begin
          LCropName := Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[1, LRow]);
          LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1].CropName := LCropName;
          LPercAreaUnderCropTypeValue := StrToFloat(Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[2,LRow]));
          LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1].PercAreaUnderCropType := LPercAreaUnderCropTypeValue;
          for LCol := CropWaterDialog.MonthlyWaterUsageGrid.FixedCols to 12 do
          begin
            LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
            if(LWaterUsage <> nil) then
            begin
              LMonthlyWaterUseValue := StrToFloat(Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[LCol + 2,LRow]));
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
      if(CropWaterDialog.CropWaterUseTypeRadioGroup.ItemIndex = 1) then
      begin
        for LRow := CropWaterDialog.MonthlyWaterUsageGrid.FixedRows to CropWaterDialog.MonthlyWaterUsageGrid.RowCount - 1 do
        begin
          for LCol := CropWaterDialog.MonthlyWaterUsageGrid.FixedCols to CropWaterDialog.MonthlyWaterUsageGrid.ColCount - 1 do
          begin
            LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
            if(LWaterUsage <> nil) then
            begin
              LMonthlyWaterUseValue := StrToFloat(Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
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

procedure TCropWaterValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TCropWaterValidator.OnAfterPasteColumnData';
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
      if(CropWaterDialog.CropWaterUseTypeRadioGroup.ItemIndex = 0) then
      begin
        LCol := CropWaterDialog.MonthlyWaterUsageGrid.Col;
        if(LCol = 2) then
        begin
          for LRow := CropWaterDialog.MonthlyWaterUsageGrid.FixedRows to CropWaterDialog.MonthlyWaterUsageGrid.RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
            LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1].PercAreaUnderCropType := LValue;
          end;
        end
        else
        if(LCol > 2) then
        begin
          for LRow := CropWaterDialog.MonthlyWaterUsageGrid.FixedRows to CropWaterDialog.MonthlyWaterUsageGrid.RowCount - 1 do
          begin
            LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
            if(LWaterUsage <> nil) then
            begin
              LValue := StrToFloat(Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
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
      if(CropWaterDialog.CropWaterUseTypeRadioGroup.ItemIndex = 1) then
      begin
        LCol := CropWaterDialog.MonthlyWaterUsageGrid.Col;
        for LRow := CropWaterDialog.MonthlyWaterUsageGrid.FixedRows to CropWaterDialog.MonthlyWaterUsageGrid.RowCount - 1 do
        begin
          LWaterUsage := LIrrigationBlock.WaterUsageFactorByIndex[LRow - 1];
          LValue := StrToFloat(Trim(CropWaterDialog.MonthlyWaterUsageGrid.Cells[LCol,LRow]));
          LWaterUsage.MonthlyWaterUse[LCol - 2] := LValue;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtIrrigationBlockWaterUseMonthly);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCropWaterValidator.SetFieldEnabledState;
const OPNAME = 'TCropWaterValidator.SetFieldEnabledState';
var
  LIrrigationBlock : IIrrigationBlock;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.
                        IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (LIrrigationBlock <> nil) then
    begin
      with CropWaterDialog do
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

