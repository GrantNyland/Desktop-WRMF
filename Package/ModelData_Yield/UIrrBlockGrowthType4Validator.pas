{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockGrowthType4Validator             *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockGrowthType4Validator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.dialogs,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UIrrigationBlock,
  UIrrBlockGrowthType4Dialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TIrrBlockGrowthType4Validator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnComboBoxChange(Sender: TObject);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;

    procedure RePopulateDataViewer;

    //________________________________________________________SupplyCapacity________________________________________
    procedure UpdateIrrSupplyCapacityPointsCount;
    procedure UpdateIrrSupplyCapacityMethod;
    procedure UpdateIrrSupplyCapacityBreakpointYears;
    procedure UpdateIrrBreakpointSupplyCapacity;
    //________________________________________________________IrrigationEfficiency________________________________
    procedure UpdateIrrIrrigationEfficiencyPointsCount;
    procedure UpdateIrrIrrigationEfficiencyMethod;
    procedure UpdateIrrIrrigationEfficiencyBreakpointYears;
    procedure UpdateIrrBreakpointIrrigationEfficiency;
    //________________________________________________________ReturnFlowFactors_____________________________________
    procedure UpdateIrrReturnFlowFactorsPointsCount;
    procedure UpdateIrrReturnFlowFactorsMethod;
    procedure UpdateIrrReturnFlowFactorsBreakpointYears;
    procedure UpdateIrrBreakpointReturnFlowFactors;

    procedure ValidateSupplyCapacityPointsCount;
    procedure ValidateIrrigationEfficienciesPointsCount;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function IrrBlockGrowthType4Dialog: TIrrBlockGrowthType4Dialog;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UFileNames,
  UUtilities,
  UDataSetType,
  UAbstractFileNamesObject,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  Math,
  Contnrs,
  UParameterData;

{******************************************************************************}
{* TIrrBlockGrowthType4Validator                                                      *}
{******************************************************************************}

function TIrrBlockGrowthType4Validator.IrrBlockGrowthType4Dialog: TIrrBlockGrowthType4Dialog;
const OPNAME = 'TIrrBlockGrowthType4Validator.IrrBlockGrowthType4Dialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrBlockGrowthType4Dialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.CreateMemberObjects;
const OPNAME = 'TIrrBlockGrowthType4Validator.CreateMemberObjects';
var
  lIndex      : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrBlockGrowthType4Dialog.Create(FPanelOwner,FAppModules);

    with IrrBlockGrowthType4Dialog do
    begin
      for lIndex := 0 to ControlsParent.ComponentCount - 1 do
      begin
        if (ControlsParent.Components[lIndex].ClassNameIs('TFieldEdit')) then
        begin
          TFieldEdit(ControlsParent.Components[lIndex]).OnEnter  := OnEditControlEnter;
          TFieldEdit(ControlsParent.Components[lIndex]).OnExit   := OnEditControltExit;
        end;

        if (ControlsParent.Components[lIndex].ClassNameIs('TFieldStringGrid')) then
        begin
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnBeforeCellChange   := OnStringGridCellDataHasChanged;
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnColEnter           := OnStringGridColEnter;
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnExit               := OnEditControltExit;
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnEnter              := OnEditControlEnter;
        end;
      end;

      //________________________________________________________SupplyCapacity________________________________________
      cmbMethodIrrSupplyCapacity.OnChange                                          := OnComboBoxChange;
      edtIrrSupplyCapacityPointsCount.FieldProperty     := FAppModules.FieldProperties.FieldProperty('SupplyCapacityPointsCount');
      cmbMethodIrrSupplyCapacity.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MethodSupplyCapacity');

      //________________________________________________________IrrigationEfficiency________________________________________

      edtIrrIrrigationEfficiencyPointsCount.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationEfficienciesPointsCount');
      cmbMethodIrrIrrigationEfficiency.OnChange            := OnComboBoxChange;
      cmbMethodIrrIrrigationEfficiency.FieldProperty       := FAppModules.FieldProperties.FieldProperty('MethodIrrigationEfficiencies');

      //________________________________________________________ReturnFlowFactors________________________________________
      cmbMethodIrrReturnFlowFactors.OnChange               := OnComboBoxChange;
      edtIrrReturnFlowFactorsPointsCount.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ReturnFlowFactorsCount');
      cmbMethodIrrReturnFlowFactors.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MethodReturnFlowFactors');

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.DestroyMemberObjects;
const OPNAME = 'TIrrBlockGrowthType4Validator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Validator.Initialise: boolean;
const OPNAME = 'TIrrBlockGrowthType4Validator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Validator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockGrowthType4Validator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Type4 Growth Factors';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Validator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrBlockGrowthType4Validator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Validator.StudyHasChanged: boolean;
const OPNAME = 'TIrrBlockGrowthType4Validator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.PopulateDataViewer;
const OPNAME = 'TIrrBlockGrowthType4Validator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.ClearDataViewer;
const OPNAME = 'TIrrBlockGrowthType4Validator.ClearDataViewer';
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
    with IrrBlockGrowthType4Dialog do
    begin

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

procedure TIrrBlockGrowthType4Validator.RePopulateDataViewer;
const OPNAME = 'TIrrBlockGrowthType4Validator.RePopulateDataViewer';
var
  LIndex : integer;
  LPointsCount : integer;
  LFieldProperty : TAbstractFieldProperty;
  LBreakpointIrrigationEfficiency : TAbstractFieldProperty;
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];


    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin

        //________________________________________________________SupplyCapacity______________________________________
        if lIrrigationBlock.SupplyCapacityPointsCount = NullInteger then
          LPointsCount := 0
        else
          LPointsCount := lIrrigationBlock.SupplyCapacityPointsCount;
        grdBreakpointYearsIrrSupplyCapacity.ColCount := Max(LPointsCount,1);
        grdBreakpointIrrSupplyCapacity.ColCount      := Max(LPointsCount,1);

        grdBreakpointYearsIrrSupplyCapacity.Rows[0].Clear;
        grdBreakpointIrrSupplyCapacity.Rows[0].Clear;


        edtIrrSupplyCapacityPointsCount.SetFieldValue(IntToStr(LPointsCount));
        cmbMethodIrrSupplyCapacity.Items.Clear;
        cmbMethodIrrSupplyCapacity.Items.AddObject('1 = Linear',TObject(1));
        cmbMethodIrrSupplyCapacity.Items.AddObject('2 = Exponetial',TObject(2));

        cmbMethodIrrSupplyCapacity.SetFieldIndex(cmbMethodIrrSupplyCapacity.Items.IndexOfObject(TObject(lIrrigationBlock.MethodSupplyCapacity)));


        LFieldProperty := FAppModules.FieldProperties.FieldProperty('BreakpointYearsSupplyCapacity');
        LBreakpointIrrigationEfficiency := FAppModules.FieldProperties.FieldProperty('BreakpointSupplyCapacity');

        for LIndex := 0 to LPointsCount-1 do
        begin
          grdBreakpointYearsIrrSupplyCapacity.AddFieldProperty(LFieldProperty);
          if(lIrrigationBlock.SupplyCapacityBreakpointYearByIndex[LIndex] = NullInteger) then
            grdBreakpointYearsIrrSupplyCapacity.Cells[LIndex, 0] := ''
          else
            grdBreakpointYearsIrrSupplyCapacity.Cells[LIndex, 0] := IntToStr(lIrrigationBlock.SupplyCapacityBreakpointYearByIndex[LIndex]);

          grdBreakpointIrrSupplyCapacity.AddFieldProperty(LBreakpointIrrigationEfficiency);
          if(lIrrigationBlock.SupplyCapacityByIndex[LIndex] =  NullFloat) then
            grdBreakpointIrrSupplyCapacity.Cells[LIndex, 0] := ''
          else
            grdBreakpointIrrSupplyCapacity.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.SupplyCapacityByIndex[LIndex]);
        end;

        //________________________________________________________IrrigationEfficiency______________________________________

        if lIrrigationBlock.IrrigationEfficiencyBreakPointsCount = NullInteger then
          LPointsCount := 0
        else
          LPointsCount := lIrrigationBlock.IrrigationEfficiencyBreakPointsCount;
        grdBreakpointYearsIrrIrrigationEfficiency.ColCount := Max(LPointsCount,1);
        grdBreakpointIrrIrrigationEfficiency.ColCount      := Max(LPointsCount,1);

        grdBreakpointYearsIrrIrrigationEfficiency.Rows[0].Clear;
        grdBreakpointIrrIrrigationEfficiency.Rows[0].Clear;


        edtIrrIrrigationEfficiencyPointsCount.SetFieldValue(IntToStr(LPointsCount));
        cmbMethodIrrIrrigationEfficiency.Items.Clear;

        cmbMethodIrrIrrigationEfficiency.Items.AddObject('1 = Linear',TObject(1));
        cmbMethodIrrIrrigationEfficiency.Items.AddObject('2 = Exponetial',TObject(2));

        //if(lIrrigationBlock.MethodIrrigationEfficiencies = 1) then
      //    cmbMethodIrrIrrigationEfficiency.SetFieldIndex(0)
      //  else if(lIrrigationBlock.MethodIrrigationEfficiencies = 2) then
       //   cmbMethodIrrIrrigationEfficiency.SetFieldIndex(1);
       cmbMethodIrrIrrigationEfficiency.SetFieldIndex(cmbMethodIrrIrrigationEfficiency.Items.IndexOfObject(TObject(lIrrigationBlock.MethodIrrigationEfficiencies)));

        LFieldProperty := FAppModules.FieldProperties.FieldProperty('BreakpointYearsIrrigationEfficiencies');
        LBreakpointIrrigationEfficiency := FAppModules.FieldProperties.FieldProperty('BreakpointIrrigationEfficiencies');

        for LIndex := 0 to LPointsCount-1 do
        begin
          grdBreakpointYearsIrrIrrigationEfficiency.AddFieldProperty(LFieldProperty);
          if(lIrrigationBlock.IrrigationEfficiencyBreakpointYearByIndex[LIndex] = NullInteger) then
            grdBreakpointYearsIrrIrrigationEfficiency.Cells[LIndex, 0] := ''
          else
            grdBreakpointYearsIrrIrrigationEfficiency.Cells[LIndex, 0] := IntToStr(lIrrigationBlock.IrrigationEfficiencyBreakpointYearByIndex[LIndex]);

          grdBreakpointIrrIrrigationEfficiency.AddFieldProperty(LBreakpointIrrigationEfficiency);
          if(lIrrigationBlock.IrrigationEfficiencyByIndex[LIndex] =  NullFloat) then
            grdBreakpointIrrIrrigationEfficiency.Cells[LIndex, 0] := ''
          else
            grdBreakpointIrrIrrigationEfficiency.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.IrrigationEfficiencyByIndex[LIndex]);
        end;

        //________________________________________________________ReturnFlowFactors______________________________________
        if lIrrigationBlock.ReturnFlowFactorsCount = NullInteger then
          LPointsCount :=  0
        else
          LPointsCount := lIrrigationBlock.ReturnFlowFactorsCount;
        grdBreakpointYearsIrrReturnFlowFactors.ColCount := Max(LPointsCount,1);
        grdBreakpointIrrReturnFlowFactors.ColCount      := Max(LPointsCount,1);

        grdBreakpointYearsIrrReturnFlowFactors.Rows[0].Clear;
        grdBreakpointIrrReturnFlowFactors.Rows[0].Clear;


        edtIrrReturnFlowFactorsPointsCount.SetFieldValue(IntToStr(LPointsCount));
        cmbMethodIrrReturnFlowFactors.Items.Clear;
        cmbMethodIrrReturnFlowFactors.Items.AddObject('1 = Linear',TObject(1));
        cmbMethodIrrReturnFlowFactors.Items.AddObject('2 = Exponetial',TObject(2));

        cmbMethodIrrReturnFlowFactors.SetFieldIndex(cmbMethodIrrReturnFlowFactors.Items.IndexOfObject(TObject(lIrrigationBlock.MethodReturnFlowFactors)));

        {if(lIrrigationBlock.MethodReturnFlowFactors = 1) then
          cmbMethodIrrReturnFlowFactors.SetFieldIndex(0)
        else if(lIrrigationBlock.MethodReturnFlowFactors = 2) then
          cmbMethodIrrReturnFlowFactors.SetFieldIndex(1);
                                                 }

        LFieldProperty := FAppModules.FieldProperties.FieldProperty('BreakpointYearsReturnFlowFactor');
        LBreakpointIrrigationEfficiency := FAppModules.FieldProperties.FieldProperty('BreakpointReturnFlowFactor');

        for LIndex := 0 to LPointsCount-1 do
        begin
          grdBreakpointYearsIrrReturnFlowFactors.AddFieldProperty(LFieldProperty);
          if(lIrrigationBlock.ReturnFlowFactorBreakpointYearByIndex[LIndex] = NullInteger) then
            grdBreakpointYearsIrrReturnFlowFactors.Cells[LIndex, 0] := ''
          else
            grdBreakpointYearsIrrReturnFlowFactors.Cells[LIndex, 0] := IntToStr(lIrrigationBlock.ReturnFlowFactorBreakpointYearByIndex[LIndex]);

          grdBreakpointIrrReturnFlowFactors.AddFieldProperty(LBreakpointIrrigationEfficiency);
          if(lIrrigationBlock.ReturnFlowFactorsByIndex[LIndex] =  NullFloat) then
            grdBreakpointIrrReturnFlowFactors.Cells[LIndex, 0] := ''
          else
            grdBreakpointIrrReturnFlowFactors.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.ReturnFlowFactorsByIndex[LIndex]);
        end;


        IrrBlockGrowthType4Dialog.Resize;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrBlockGrowthType4Validator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrBlockGrowthType4Validator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IrrBlockGrowthType4Dialog do
    begin
      //________________________________________________________SupplyCapacity________________________________________
      if ((Sender = edtIrrSupplyCapacityPointsCount) AND (edtIrrSupplyCapacityPointsCount.HasValueChanged)) then
        UpdateIrrSupplyCapacityPointsCount;
      //________________________________________________________IrrigationEfficiency______________________________________
      if ((Sender = edtIrrIrrigationEfficiencyPointsCount) AND (edtIrrIrrigationEfficiencyPointsCount.HasValueChanged)) then
        UpdateIrrIrrigationEfficiencyPointsCount;
      //________________________________________________________ReturnFlowFactors______________________________________
      if ((Sender = edtIrrReturnFlowFactorsPointsCount) AND (edtIrrReturnFlowFactorsPointsCount.HasValueChanged)) then
        UpdateIrrReturnFlowFactorsPointsCount;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.OnComboBoxChange(Sender: TObject);
const OPNAME = 'TIrrBlockGrowthType4Validator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    with IrrBlockGrowthType4Dialog do
    begin
      //________________________________________________________SupplyCapacity________________________________________
      if ((Sender = cmbMethodIrrSupplyCapacity) AND (cmbMethodIrrSupplyCapacity.HasValueChanged)) then
        UpdateIrrSupplyCapacityMethod;
      //________________________________________________________IrrigationEfficiency______________________________________
      if ((Sender = cmbMethodIrrIrrigationEfficiency) AND (cmbMethodIrrIrrigationEfficiency.HasValueChanged)) then
        UpdateIrrIrrigationEfficiencyMethod;
      //________________________________________________________ReturnFlowFactors______________________________________
      if ((Sender = cmbMethodIrrReturnFlowFactors) AND (cmbMethodIrrReturnFlowFactors.HasValueChanged)) then
        UpdateIrrReturnFlowFactorsMethod;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrBlockGrowthType4Validator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
  try
    with IrrBlockGrowthType4Dialog do
    begin
      //________________________________________________________SupplyCapacity________________________________________
      if ((ASender = grdBreakpointYearsIrrSupplyCapacity) {AND (grdBreakpointYearsIrrSupplyCapacity.HasValueChanged)}) then
        UpdateIrrSupplyCapacityBreakpointYears;
      if ((ASender = grdBreakpointIrrSupplyCapacity) {AND (grdBreakpointIrrSupplyCapacity.HasValueChanged)}) then
        UpdateIrrBreakpointSupplyCapacity;
      //________________________________________________________IrrigationEfficiency______________________________________
      if ((ASender = grdBreakpointYearsIrrIrrigationEfficiency) {AND (grdBreakpointYearsIrrIrrigationEfficiency.HasValueChanged)}) then
        UpdateIrrIrrigationEfficiencyBreakpointYears;
      if ((ASender = grdBreakpointIrrIrrigationEfficiency) {AND (grdBreakpointYearsIrrReturnFlowFactors.HasValueChanged)}) then
        UpdateIrrBreakpointIrrigationEfficiency;
      //________________________________________________________ReturnFlowFactors______________________________________
      if ((ASender = grdBreakpointYearsIrrReturnFlowFactors) {AND (grdBreakpointYearsIrrReturnFlowFactors.HasValueChanged)}) then
        UpdateIrrReturnFlowFactorsBreakpointYears;
      if ((ASender = grdBreakpointIrrReturnFlowFactors) {AND (grdBreakpointIrrReturnFlowFactors.HasValueChanged)}) then
        UpdateIrrBreakpointReturnFlowFactors;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthType4Validator.SaveState: boolean;
const OPNAME = 'TIrrBlockGrowthType4Validator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrBlockGrowthType4Validator.DoContextValidation';
begin
  try
    case AValidationType of
      dvtAllType4Growth :
      begin
        ValidateSupplyCapacityPointsCount;
        ValidateIrrigationEfficienciesPointsCount;
      end;
      dvtSupplyCapacityPointsCount : ValidateSupplyCapacityPointsCount;
      dvtIrrBlockEfficiencyPointsCount : ValidateIrrigationEfficienciesPointsCount;
      dvtMethodIrrigationEfficiencies:;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockGrowthType4Validator.ValidateSupplyCapacityPointsCount;
const OPNAME = 'TIrrBlockGrowthType4Validator.ValidateSupplyCapacityPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : WideString;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        edtIrrSupplyCapacityPointsCount.ContextValidationError := '';
        if (not LIrrigationBlock.Validate(LMessage, edtIrrSupplyCapacityPointsCount.FieldProperty.FieldName)) then
          edtIrrSupplyCapacityPointsCount.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockGrowthType4Validator.ValidateIrrigationEfficienciesPointsCount;
const OPNAME = 'TIrrBlockGrowthType4Validator.ValidateIrrigationEfficienciesPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : WideString;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        edtIrrIrrigationEfficiencyPointsCount.ContextValidationError := '';
        if (not LIrrigationBlock.Validate(LMessage, edtIrrIrrigationEfficiencyPointsCount.FieldProperty.FieldName)) then
          edtIrrIrrigationEfficiencyPointsCount.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



//________________________________________________________SupplyCapacity________________________________________
procedure TIrrBlockGrowthType4Validator.UpdateIrrSupplyCapacityPointsCount;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrSupplyCapacityPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        IrrBlockGrowthType4Dialog.edtIrrSupplyCapacityPointsCount.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        edtIrrSupplyCapacityPointsCount.FieldProperty.FieldName,
                        edtIrrSupplyCapacityPointsCount.Text, LMessage) then
        begin
          LIrrigationBlock.SupplyCapacityPointsCount := StrToInt(IrrBlockGrowthType4Dialog.edtIrrSupplyCapacityPointsCount.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtSupplyCapacityPointsCount);
        end
        else
          edtIrrSupplyCapacityPointsCount.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrSupplyCapacityMethod;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrSupplyCapacityMethod';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue : integer;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LValue := integer(TObject(cmbMethodIrrSupplyCapacity.Items.Objects[cmbMethodIrrSupplyCapacity.ItemIndex]));
        IrrBlockGrowthType4Dialog.cmbMethodIrrSupplyCapacity.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        cmbMethodIrrSupplyCapacity.FieldProperty.FieldName,
                        IntToStr(LValue), LMessage) then
        begin
          LIrrigationBlock.MethodSupplyCapacity := LValue;
          RePopulateDataViewer;
          DoContextValidation (dvtSupplyCapacityMethod);
        end
        else
          cmbMethodIrrSupplyCapacity.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrSupplyCapacityBreakpointYears;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrSupplyCapacityBreakpointYears';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue,
  LMessage : string;
  LCol : integer;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LCol := grdBreakpointYearsIrrSupplyCapacity.Col;
        grdBreakpointYearsIrrSupplyCapacity.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointYearsIrrSupplyCapacity.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointYearsSupplyCapacity',LValue,LMessage) then
        begin
          LIrrigationBlock.SupplyCapacityBreakpointYearByIndex[LCol] := StrToInt(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtSupplyCapacityBreakpointYears);
        end
        else
          grdBreakpointYearsIrrSupplyCapacity.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockGrowthType4Validator.UpdateIrrBreakpointSupplyCapacity;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrBreakpointSupplyCapacity';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue,
  LMessage : string;
  LCol : integer;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LCol := grdBreakpointIrrSupplyCapacity.Col;
        grdBreakpointIrrSupplyCapacity.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrSupplyCapacity.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointSupplyCapacity',LValue,LMessage) then
        begin
          LIrrigationBlock.SupplyCapacityByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointSupplyCapacity);
        end
        else
          grdBreakpointIrrSupplyCapacity.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________IrrigationEfficiency________________________________________
procedure TIrrBlockGrowthType4Validator.UpdateIrrIrrigationEfficiencyPointsCount;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrIrrigationEfficiencyPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        IrrBlockGrowthType4Dialog.edtIrrIrrigationEfficiencyPointsCount.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        edtIrrIrrigationEfficiencyPointsCount.FieldProperty.FieldName,
                        edtIrrIrrigationEfficiencyPointsCount.Text, LMessage) then
        begin
          LIrrigationBlock.IrrigationEfficiencyBreakPointsCount := StrToInt(edtIrrIrrigationEfficiencyPointsCount.Text);
          RePopulateDataViewer;
          DoContextValidation(dvtIrrBlockEfficiencyPointsCount);
        end
        else
          edtIrrIrrigationEfficiencyPointsCount.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrIrrigationEfficiencyMethod;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrIrrigationEfficiencyMethod';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue : integer;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LValue := integer(TObject(cmbMethodIrrIrrigationEfficiency.Items.Objects[cmbMethodIrrIrrigationEfficiency.ItemIndex]));
        IrrBlockGrowthType4Dialog.cmbMethodIrrIrrigationEfficiency.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        cmbMethodIrrIrrigationEfficiency.FieldProperty.FieldName,
                        IntToStr(LValue), LMessage) then
        begin
          LIrrigationBlock.MethodIrrigationEfficiencies := LValue;
          RePopulateDataViewer;
          DoContextValidation(dvtMethodIrrigationEfficiencies);
        end
        else
          cmbMethodIrrSupplyCapacity.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//  except on E: Exception do HandleError(E, OPNAME) end;
//end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrIrrigationEfficiencyBreakpointYears;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrIrrigationEfficiencyBreakpointYears';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue,
  LMessage : string;
  LCol : integer;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LCol := grdBreakpointYearsIrrIrrigationEfficiency.Col;
        grdBreakpointYearsIrrIrrigationEfficiency.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointYearsIrrIrrigationEfficiency.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointYearsIrrigationEfficiencies',LValue,LMessage) then
        begin
          LIrrigationBlock.IrrigationEfficiencyBreakpointYearByIndex[LCol] := StrToInt(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointYearsIrrIrrigationEfficiency.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrBreakpointIrrigationEfficiency;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrBreakpointIrrigationEfficiency';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue,
  LMessage : string;
  LCol : integer;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LCol := grdBreakpointIrrIrrigationEfficiency.Col;
        grdBreakpointIrrIrrigationEfficiency.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrIrrigationEfficiency.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointIrrigationEfficiencies',LValue,LMessage) then
        begin
          LIrrigationBlock.IrrigationEfficiencyByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointIrrIrrigationEfficiency.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________ReturnFlowFactors________________________________________
procedure TIrrBlockGrowthType4Validator.UpdateIrrReturnFlowFactorsPointsCount;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrReturnFlowFactorsPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        IrrBlockGrowthType4Dialog.edtIrrReturnFlowFactorsPointsCount.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        edtIrrReturnFlowFactorsPointsCount.FieldProperty.FieldName,
                        edtIrrReturnFlowFactorsPointsCount.Text, LMessage) then
        begin
          LIrrigationBlock.ReturnFlowFactorsCount := StrToInt(edtIrrReturnFlowFactorsPointsCount.Text);
          RePopulateDataViewer;
          DoContextValidation(dvtReturnFlowFactorBreakPointsCount);
        end
        else
          edtIrrReturnFlowFactorsPointsCount.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrReturnFlowFactorsMethod;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrReturnFlowFactorsMethod';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue : integer;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LValue := integer(TObject(cmbMethodIrrReturnFlowFactors.Items.Objects[cmbMethodIrrReturnFlowFactors.ItemIndex]));
        IrrBlockGrowthType4Dialog.cmbMethodIrrReturnFlowFactors.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        cmbMethodIrrReturnFlowFactors.FieldProperty.FieldName,
                        IntToStr(LValue), LMessage) then
        begin
          LIrrigationBlock.MethodReturnFlowFactors := LValue;
          RePopulateDataViewer;
          DoContextValidation(dvtMethodIrrReturnFlowFactors);
        end
        else
          cmbMethodIrrReturnFlowFactors.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrReturnFlowFactorsBreakpointYears;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrReturnFlowFactorsBreakpointYears';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue,
  LMessage : string;
  LCol : integer;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LCol := grdBreakpointYearsIrrReturnFlowFactors.Col;
        grdBreakpointYearsIrrReturnFlowFactors.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointYearsIrrReturnFlowFactors.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointYearsReturnFlowFactor',LValue,LMessage) then
        begin
          LIrrigationBlock.ReturnFlowFactorBreakpointYearByIndex[LCol] := StrToInt(LValue);
          RePopulateDataViewer;
          DoContextValidation(dvtReturnFlowFactorBreakpointYear);
        end
        else
          grdBreakpointYearsIrrIrrigationEfficiency.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthType4Validator.UpdateIrrBreakpointReturnFlowFactors;
const OPNAME = 'TIrrBlockGrowthType4Validator.UpdateIrrBreakpointReturnFlowFactors';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue,
  LMessage : string;
  LCol : integer;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthType4Dialog do
      begin
        LCol := grdBreakpointIrrReturnFlowFactors.Col;
        grdBreakpointIrrReturnFlowFactors.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrReturnFlowFactors.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointReturnFlowFactor',LValue,LMessage) then
        begin
          LIrrigationBlock.ReturnFlowFactorsByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointIrrReturnFlowFactors.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

