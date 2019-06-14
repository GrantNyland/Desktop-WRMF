{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockGrowthValidator             *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockGrowthValidator;

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
  UIrrBlockGrowthDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TIrrBlockGrowthValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnComboBoxChange(Sender: TObject);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;

    procedure RePopulateDataViewer;

    //________________________________________________________IrrigatedAreas________________________________________
    procedure UpdateIrrIrrigatedAreasPointsCount;
    procedure UpdateIrrIrrigatedAreasMethod;
    procedure UpdateIrrIrrigatedAreasBreakpointYears;
    procedure UpdateIrrBreakpointIrrigatedAreas;
    //________________________________________________________MaximumWaterAllocation________________________________
    procedure UpdateIrrMaximumWaterAllocationPointsCount;
    procedure UpdateIrrMaximumWaterAllocationMethod;
    procedure UpdateIrrMaximumWaterAllocationBreakpointYears;
    procedure UpdateIrrBreakpointMaximumWaterAllocation;
    procedure UpdateIrrBreakpointMaximumWaterAllocationGrowth;
    //________________________________________________________ReturnFlowVolume_____________________________________
    procedure UpdateIrrReturnFlowVolumePointsCount;
    procedure UpdateIrrReturnFlowVolumeMethod;
    procedure UpdateIrrReturnFlowVolumeBreakpointYears;
    procedure UpdateIrrBreakpointReturnFlowVolume;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function IrrBlockGrowthDialog: TIrrBlockGrowthDialog;
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
{* TIrrBlockGrowthValidator                                                      *}
{******************************************************************************}

function TIrrBlockGrowthValidator.IrrBlockGrowthDialog: TIrrBlockGrowthDialog;
const OPNAME = 'TIrrBlockGrowthValidator.IrrBlockGrowthDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrBlockGrowthDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.CreateMemberObjects;
const OPNAME = 'TIrrBlockGrowthValidator.CreateMemberObjects';
var
  lIndex      : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrBlockGrowthDialog.Create(FPanelOwner,FAppModules);

    with IrrBlockGrowthDialog do
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

      //________________________________________________________IrrigatedAreas________________________________________
      cmbMethodIrrIrrigatedAreas.OnChange                                          := OnComboBoxChange;
      edtIrrIrrigatedAreasPointsCount.FieldProperty     := FAppModules.FieldProperties.FieldProperty('AllocatedAreaPointsCount');
      cmbMethodIrrIrrigatedAreas.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MethodIrrigatedAreas');

      //________________________________________________________MaximumWaterAllocation________________________________________
      cmbMethodIrrMaximumWaterAllocation.OnChange                                          := OnComboBoxChange;
      edtIrrMaximumWaterAllocationPointsCount.FieldProperty     := FAppModules.FieldProperties.FieldProperty('MaxWaterAllocationCount');
      cmbMethodIrrMaximumWaterAllocation.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MethodMaxWaterAllocation');

      //________________________________________________________ReturnFlowVolume________________________________________
      cmbMethodIrrReturnFlowVolume.OnChange                                          := OnComboBoxChange;
      edtIrrReturnFlowVolumePointsCount.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ReturnFlowVolumePointsCount');
      cmbMethodIrrReturnFlowVolume.FieldProperty          := FAppModules.FieldProperties.FieldProperty('MethodReturnFlowVolume');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.DestroyMemberObjects;
const OPNAME = 'TIrrBlockGrowthValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthValidator.Initialise: boolean;
const OPNAME = 'TIrrBlockGrowthValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockGrowthValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Growth Factors';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrBlockGrowthValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrBlockGrowthValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.PopulateDataViewer;
const OPNAME = 'TIrrBlockGrowthValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.ClearDataViewer;
const OPNAME = 'TIrrBlockGrowthValidator.ClearDataViewer';
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
    with IrrBlockGrowthDialog do
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

procedure TIrrBlockGrowthValidator.RePopulateDataViewer;
const OPNAME = 'TIrrBlockGrowthValidator.RePopulateDataViewer';
var
  LIndex : integer;
  LPointsCount : integer;
  LFieldProperty : TAbstractFieldProperty;
  LBreakpointMaximumWaterAllocation : TAbstractFieldProperty;
  LBreakpointIrrMaxWaterAllocGrowth : TAbstractFieldProperty;

  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    with IrrBlockGrowthDialog do
    begin

      //________________________________________________________IrrigatedAreas______________________________________
      if lIrrigationBlock.AllocatedAreaPointsCount = NullInteger then
        LPointsCount := 0
      else
        LPointsCount := lIrrigationBlock.AllocatedAreaPointsCount;
      grdBreakpointYearsIrrIrrigatedAreas.ColCount := Max(LPointsCount,1);
      grdBreakpointIrrIrrigatedAreas.ColCount      := Max(LPointsCount,1);

      grdBreakpointYearsIrrIrrigatedAreas.Rows[0].Clear;
      grdBreakpointIrrIrrigatedAreas.Rows[0].Clear;


      edtIrrIrrigatedAreasPointsCount.SetFieldValue(IntToStr(LPointsCount));
      cmbMethodIrrIrrigatedAreas.Items.Clear;
      cmbMethodIrrIrrigatedAreas.Items.AddObject('1 = Linear',TObject(1));
      cmbMethodIrrIrrigatedAreas.Items.AddObject('2 = Exponetial',TObject(2));
      cmbMethodIrrIrrigatedAreas.SetFieldIndex(cmbMethodIrrIrrigatedAreas.Items.IndexOfObject(TObject(lIrrigationBlock.MethodIrrigatedAreas)));

      {if(lIrrigationBlock.MethodIrrigatedAreas = 1) then
        cmbMethodIrrIrrigatedAreas.SetFieldIndex(0)
      else if(lIrrigationBlock.MethodIrrigatedAreas = 2) then
        cmbMethodIrrIrrigatedAreas.SetFieldIndex(1);
        }


      LFieldProperty := FAppModules.FieldProperties.FieldProperty('BreakpointYearsDefined');
      LBreakpointMaximumWaterAllocation := FAppModules.FieldProperties.FieldProperty('BreakpointArea');

      for LIndex := 0 to LPointsCount-1 do
      begin
        grdBreakpointYearsIrrIrrigatedAreas.AddFieldProperty(LFieldProperty);
        if(lIrrigationBlock.IrrigatedAreasBreakpointYearByIndex[LIndex] = NullInteger) then
          grdBreakpointYearsIrrIrrigatedAreas.Cells[LIndex, 0] := ''
        else
          grdBreakpointYearsIrrIrrigatedAreas.Cells[LIndex, 0] := IntToStr(lIrrigationBlock.IrrigatedAreasBreakpointYearByIndex[LIndex]);

        grdBreakpointIrrIrrigatedAreas.AddFieldProperty(LBreakpointMaximumWaterAllocation);
        if(lIrrigationBlock.IrrigatedAreaByIndex[LIndex] =  NullFloat) then
          grdBreakpointIrrIrrigatedAreas.Cells[LIndex, 0] := ''
        else
          grdBreakpointIrrIrrigatedAreas.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.IrrigatedAreaByIndex[LIndex]);
      end;

      //________________________________________________________MaximumWaterAllocation______________________________________
      if lIrrigationBlock.MaxWaterAllocationCount = NullInteger then
        LPointsCount :=  0
      else
        LPointsCount := lIrrigationBlock.MaxWaterAllocationCount;
      grdBreakpointYearsIrrMaximumWaterAllocation.ColCount := Max(LPointsCount,1);
      grdBreakpointIrrMaximumWaterAllocation.ColCount      := Max(LPointsCount,1);
      grdBreakpointIrrMaxWaterAllocGrowth.ColCount      := Max(LPointsCount,1);

      grdBreakpointYearsIrrMaximumWaterAllocation.Rows[0].Clear;
      grdBreakpointIrrMaximumWaterAllocation.Rows[0].Clear;
      grdBreakpointIrrMaxWaterAllocGrowth.Rows[0].Clear;

      edtIrrMaximumWaterAllocationPointsCount.SetFieldValue(IntToStr(LPointsCount));
      cmbMethodIrrMaximumWaterAllocation.Items.Clear;
      cmbMethodIrrMaximumWaterAllocation.Items.AddObject('1 = Linear',TObject(1));
      cmbMethodIrrMaximumWaterAllocation.Items.AddObject('2 = Exponetial',TObject(2));
      cmbMethodIrrMaximumWaterAllocation.SetFieldIndex(cmbMethodIrrMaximumWaterAllocation.Items.IndexOfObject(TObject(LIrrigationBlock.MethodMaxWaterAllocation)));

     {
      if(lIrrigationBlock.MethodIrrigationEfficiencies = 1) then
        cmbMethodIrrReturnFlowVolume.SetFieldIndex(0)
      else if(lIrrigationBlock.MethodIrrigationEfficiencies = 2) then
        cmbMethodIrrReturnFlowVolume.SetFieldIndex(1);
      }

      LFieldProperty := FAppModules.FieldProperties.FieldProperty('BreakpointYearsMaxWaterAllocation');
      LBreakpointMaximumWaterAllocation := FAppModules.FieldProperties.FieldProperty('BreakpointMaxWaterAllocation');
      LBreakpointIrrMaxWaterAllocGrowth := FAppModules.FieldProperties.FieldProperty('BreakpointMaxWaterAllocationGrowth');


      for LIndex := 0 to LPointsCount-1 do
      begin
        grdBreakpointYearsIrrMaximumWaterAllocation.AddFieldProperty(LFieldProperty);
        if(lIrrigationBlock.MaximumWaterAllocationBreakpointYearByIndex[LIndex] = NullInteger) then
          grdBreakpointYearsIrrMaximumWaterAllocation.Cells[LIndex, 0] := ''
        else
          grdBreakpointYearsIrrMaximumWaterAllocation.Cells[LIndex, 0] := IntToStr(lIrrigationBlock.MaximumWaterAllocationBreakpointYearByIndex[LIndex]);

        grdBreakpointIrrMaximumWaterAllocation.AddFieldProperty(LBreakpointMaximumWaterAllocation);
        if(lIrrigationBlock.MaximumWaterAllocationByIndex[LIndex] =  NullFloat) then
          grdBreakpointIrrMaximumWaterAllocation.Cells[LIndex, 0] := ''
        else
          grdBreakpointIrrMaximumWaterAllocation.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.MaximumWaterAllocationByIndex[LIndex]);

        grdBreakpointIrrMaxWaterAllocGrowth.AddFieldProperty(LBreakpointIrrMaxWaterAllocGrowth);
        if(lIrrigationBlock.MaximumWaterAllocationGrowthByIndex[LIndex] =  NullFloat) then
          grdBreakpointIrrMaxWaterAllocGrowth.Cells[LIndex, 0] := ''
        else
          grdBreakpointIrrMaxWaterAllocGrowth.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.MaximumWaterAllocationGrowthByIndex[LIndex]);

      end;

      //________________________________________________________ReturnFlowVolume______________________________________
      if lIrrigationBlock.ReturnFlowVolumePointsCount = NullInteger then
        LPointsCount := 0
      else
        LPointsCount := lIrrigationBlock.ReturnFlowVolumePointsCount;

      grdBreakpointYearsIrrReturnFlowVolume.ColCount := Max(LPointsCount,1);
      grdBreakpointIrrReturnFlowVolume.ColCount      := Max(LPointsCount,1);

      grdBreakpointYearsIrrReturnFlowVolume.Rows[0].Clear;
      grdBreakpointIrrReturnFlowVolume.Rows[0].Clear;


      edtIrrReturnFlowVolumePointsCount.SetFieldValue(IntToStr(LPointsCount));
      cmbMethodIrrReturnFlowVolume.Items.Clear;
      cmbMethodIrrReturnFlowVolume.AddItem('1 = Linear',TObject(1));
      cmbMethodIrrReturnFlowVolume.AddItem('2 = Exponetial',TObject(2));
      cmbMethodIrrReturnFlowVolume.SetFieldIndex(cmbMethodIrrReturnFlowVolume.Items.IndexOfObject(TObject(LIrrigationBlock.MethodReturnFlowVolume)));

      {
      if(lIrrigationBlock.MethodReturnFlowVolume = 1) then
        cmbMethodIrrReturnFlowVolume.SetFieldIndex(0)
      else if(lIrrigationBlock.MethodReturnFlowVolume = 2) then
        cmbMethodIrrReturnFlowVolume.SetFieldIndex(1);
       }

      LFieldProperty := FAppModules.FieldProperties.FieldProperty('BreakpointYearsReturnFlowVolume');
      LBreakpointMaximumWaterAllocation := FAppModules.FieldProperties.FieldProperty('BreakpointReturnFlowVolume');

      for LIndex := 0 to LPointsCount-1 do
      begin
        grdBreakpointYearsIrrReturnFlowVolume.AddFieldProperty(LFieldProperty);
        if(lIrrigationBlock.ReturnFlowVolumeBreakpointYearByIndex[LIndex] = NullInteger) then
          grdBreakpointYearsIrrReturnFlowVolume.Cells[LIndex, 0] := ''
        else
          grdBreakpointYearsIrrReturnFlowVolume.Cells[LIndex, 0] := IntToStr(lIrrigationBlock.ReturnFlowVolumeBreakpointYearByIndex[LIndex]);

        grdBreakpointIrrReturnFlowVolume.AddFieldProperty(LBreakpointMaximumWaterAllocation);
        if(lIrrigationBlock.ReturnFlowVolumeByIndex[LIndex] =  NullFloat) then
          grdBreakpointIrrReturnFlowVolume.Cells[LIndex, 0] := ''
        else
          grdBreakpointIrrReturnFlowVolume.Cells[LIndex, 0] := FormatFloat('##0.00',lIrrigationBlock.ReturnFlowVolumeByIndex[LIndex]);
      end;


      IrrBlockGrowthDialog.Resize;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrBlockGrowthValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrBlockGrowthValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IrrBlockGrowthDialog do
    begin
      //________________________________________________________IrrigatedAreas________________________________________
      if ((Sender = edtIrrIrrigatedAreasPointsCount) AND (edtIrrIrrigatedAreasPointsCount.HasValueChanged)) then
        UpdateIrrIrrigatedAreasPointsCount;
      //________________________________________________________MaximumWaterAllocation______________________________________
      if ((Sender = edtIrrMaximumWaterAllocationPointsCount) AND (edtIrrMaximumWaterAllocationPointsCount.HasValueChanged)) then
        UpdateIrrMaximumWaterAllocationPointsCount;
      //________________________________________________________ReturnFlowVolume______________________________________
      if ((Sender = edtIrrReturnFlowVolumePointsCount) AND (edtIrrReturnFlowVolumePointsCount.HasValueChanged)) then
        UpdateIrrReturnFlowVolumePointsCount;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.OnComboBoxChange(Sender: TObject);
const OPNAME = 'TIrrBlockGrowthValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    with IrrBlockGrowthDialog do
    begin
      //________________________________________________________IrrigatedAreas________________________________________
      if ((Sender = cmbMethodIrrIrrigatedAreas) AND (cmbMethodIrrIrrigatedAreas.HasValueChanged)) then
        UpdateIrrIrrigatedAreasMethod;
      //________________________________________________________MaximumWaterAllocation______________________________________
      if ((Sender = cmbMethodIrrMaximumWaterAllocation) AND (cmbMethodIrrMaximumWaterAllocation.HasValueChanged)) then
        UpdateIrrMaximumWaterAllocationMethod;
      //________________________________________________________ReturnFlowVolume______________________________________
      if ((Sender = cmbMethodIrrReturnFlowVolume) AND (cmbMethodIrrReturnFlowVolume.HasValueChanged)) then
        UpdateIrrReturnFlowVolumeMethod;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrBlockGrowthValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
  try
    with IrrBlockGrowthDialog do
    begin
      //________________________________________________________IrrigatedAreas________________________________________
      if ((ASender = grdBreakpointYearsIrrIrrigatedAreas) {AND (grdBreakpointYearsIrrIrrigatedAreas.HasValueChanged)}) then
        UpdateIrrIrrigatedAreasBreakpointYears;
      if ((ASender = grdBreakpointIrrIrrigatedAreas) {AND (grdBreakpointIrrIrrigatedAreas.HasValueChanged)}) then
        UpdateIrrBreakpointIrrigatedAreas;
      //________________________________________________________MaximumWaterAllocation______________________________________
      if ((ASender = grdBreakpointYearsIrrMaximumWaterAllocation) {AND (grdBreakpointYearsIrrMaximumWaterAllocation.HasValueChanged)}) then
        UpdateIrrMaximumWaterAllocationBreakpointYears;
      if ((ASender = grdBreakpointIrrMaximumWaterAllocation) {AND (grdBreakpointYearsIrrReturnFlowVolume.HasValueChanged)}) then
        UpdateIrrBreakpointMaximumWaterAllocation;
      if (ASender = grdBreakpointIrrMaxWaterAllocGrowth) then
        UpdateIrrBreakpointMaximumWaterAllocationGrowth;
      //________________________________________________________ReturnFlowVolume______________________________________
      if ((ASender = grdBreakpointYearsIrrReturnFlowVolume) {AND (grdBreakpointYearsIrrReturnFlowVolume.HasValueChanged)}) then
        UpdateIrrReturnFlowVolumeBreakpointYears;
      if ((ASender = grdBreakpointIrrReturnFlowVolume) {AND (grdBreakpointIrrReturnFlowVolume.HasValueChanged)}) then
        UpdateIrrBreakpointReturnFlowVolume;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockGrowthValidator.SaveState: boolean;
const OPNAME = 'TIrrBlockGrowthValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrBlockGrowthValidator.DoContextValidation';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________IrrigatedAreas________________________________________
procedure TIrrBlockGrowthValidator.UpdateIrrIrrigatedAreasPointsCount;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrIrrigatedAreasPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthDialog do
      begin
        IrrBlockGrowthDialog.edtIrrIrrigatedAreasPointsCount.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        edtIrrIrrigatedAreasPointsCount.FieldProperty.FieldName,
                        edtIrrIrrigatedAreasPointsCount.Text, LMessage) then
        begin
          LIrrigationBlock.AllocatedAreaPointsCount := StrToInt(IrrBlockGrowthDialog.edtIrrIrrigatedAreasPointsCount.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigatedAreasPointsCount);
        end
        else
          edtIrrIrrigatedAreasPointsCount.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockGrowthValidator.UpdateIrrIrrigatedAreasMethod;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrIrrigatedAreasMethod';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue : integer;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthDialog do
      begin
        LValue := integer(TObject(cmbMethodIrrIrrigatedAreas.Items.Objects[cmbMethodIrrIrrigatedAreas.ItemIndex]));
        cmbMethodIrrIrrigatedAreas.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        cmbMethodIrrIrrigatedAreas.FieldProperty.FieldName,
                        IntToStr(LValue), LMessage) then
        begin
          LIrrigationBlock.MethodIrrigatedAreas := LValue;
          RePopulateDataViewer;
          DoContextValidation (dvtSupplyCapacityMethod);
        end
        else
          cmbMethodIrrIrrigatedAreas.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrIrrigatedAreasBreakpointYears;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrIrrigatedAreasBreakpointYears';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointYearsIrrIrrigatedAreas.Col;
        grdBreakpointYearsIrrIrrigatedAreas.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointYearsIrrIrrigatedAreas.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointYearsDefined',LValue,LMessage) then
        begin
          LIrrigationBlock.IrrigatedAreasBreakPointYearByIndex[LCol] := StrToInt(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigatedAreas);
        end
        else
          grdBreakpointYearsIrrIrrigatedAreas.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrBreakpointIrrigatedAreas;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrBreakpointIrrigatedAreas';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointIrrIrrigatedAreas.Col;
        grdBreakpointIrrIrrigatedAreas.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrIrrigatedAreas.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointArea',LValue,LMessage) then
        begin
          LIrrigationBlock.IrrigatedAreaByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointIrrigatedAreas);
        end
        else
          grdBreakpointIrrIrrigatedAreas.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________MaximumWaterAllocation________________________________________
procedure TIrrBlockGrowthValidator.UpdateIrrMaximumWaterAllocationPointsCount;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrMaximumWaterAllocationPointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthDialog do
      begin
        IrrBlockGrowthDialog.edtIrrMaximumWaterAllocationPointsCount.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        edtIrrMaximumWaterAllocationPointsCount.FieldProperty.FieldName,
                        edtIrrMaximumWaterAllocationPointsCount.Text, LMessage) then
        begin
          LIrrigationBlock.MaxWaterAllocationCount := StrToInt(edtIrrMaximumWaterAllocationPointsCount.Text);
          RePopulateDataViewer;
          DoContextValidation(dvtMaxWaterAllocationCount);
        end
        else
          edtIrrMaximumWaterAllocationPointsCount.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockGrowthValidator.UpdateIrrMaximumWaterAllocationMethod;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrMaximumWaterAllocationMethod';
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue : integer;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthDialog do
      begin
        LValue := integer(TObject(cmbMethodIrrMaximumWaterAllocation.Items.Objects[cmbMethodIrrMaximumWaterAllocation.ItemIndex]));
        cmbMethodIrrMaximumWaterAllocation.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        cmbMethodIrrMaximumWaterAllocation.FieldProperty.FieldName,
                        IntToStr(LValue), LMessage) then
        begin
          LIrrigationBlock.MethodMaxWaterAllocation := LValue;
          RePopulateDataViewer;
          DoContextValidation(dvtMethodMaxWaterAllocation);
        end
        else
          cmbMethodIrrMaximumWaterAllocation.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockGrowthValidator.UpdateIrrMaximumWaterAllocationBreakpointYears;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrMaximumWaterAllocationBreakpointYears';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointYearsIrrMaximumWaterAllocation.Col;
        grdBreakpointYearsIrrMaximumWaterAllocation.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointYearsIrrMaximumWaterAllocation.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointYearsMaxWaterAllocation',LValue,LMessage) then
        begin
          LIrrigationBlock.MaximumWaterAllocationBreakpointYearByIndex[LCol] := StrToInt(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointYearsIrrMaximumWaterAllocation.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrBreakpointMaximumWaterAllocation;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrBreakpointMaximumWaterAllocation';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointIrrMaximumWaterAllocation.Col;
        grdBreakpointIrrMaximumWaterAllocation.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrMaximumWaterAllocation.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointMaxWaterAllocation',LValue,LMessage) then
        begin
          LIrrigationBlock.MaximumWaterAllocationByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointIrrMaximumWaterAllocation.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrBreakpointMaximumWaterAllocationGrowth;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrBreakpointMaximumWaterAllocationGrowth';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointIrrMaxWaterAllocGrowth.Col;
        grdBreakpointIrrMaxWaterAllocGrowth.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrMaxWaterAllocGrowth.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointMaxWaterAllocationGrowth',LValue,LMessage) then
        begin
          LIrrigationBlock.MaximumWaterAllocationGrowthByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointIrrMaxWaterAllocGrowth.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;




//________________________________________________________ReturnFlowVolume________________________________________
procedure TIrrBlockGrowthValidator.UpdateIrrReturnFlowVolumePointsCount;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrReturnFlowVolumePointsCount';
var
  LIrrigationBlock  : IIrrigationBlock;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthDialog do
      begin
        edtIrrReturnFlowVolumePointsCount.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        edtIrrReturnFlowVolumePointsCount.FieldProperty.FieldName,
                        edtIrrReturnFlowVolumePointsCount.Text, LMessage) then
        begin
          LIrrigationBlock.ReturnFlowVolumePointsCount := StrToInt(edtIrrReturnFlowVolumePointsCount.Text);
          RePopulateDataViewer;
          DoContextValidation(dvtReturnFlowVolumeCount);
        end
        else
          edtIrrReturnFlowVolumePointsCount.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrReturnFlowVolumeMethod;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrReturnFlowVolumeMethod';
{var
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    lIrrigationBlock.MethodReturnFlowVolume := IrrBlockGrowthDialog.cmbMethodIrrReturnFlowVolume.ItemIndex + 1;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
var
  LIrrigationBlock  : IIrrigationBlock;
  LValue : integer;
  LMessage : string;
begin
  try
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if LIrrigationBlock <> nil then
    begin
      with IrrBlockGrowthDialog do
      begin
        LValue := integer(TObject(cmbMethodIrrReturnFlowVolume.Items.Objects[cmbMethodIrrReturnFlowVolume.ItemIndex]));
        cmbMethodIrrReturnFlowVolume.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        cmbMethodIrrReturnFlowVolume.FieldProperty.FieldName,
                        IntToStr(LValue), LMessage) then
        begin
          LIrrigationBlock.MethodReturnFlowVolume := LValue;
          RePopulateDataViewer;
          DoContextValidation(dvtcmbMethodIrrReturnFlowVolume);
        end
        else
          cmbMethodIrrReturnFlowVolume.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrReturnFlowVolumeBreakpointYears;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrReturnFlowVolumeBreakpointYears';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointYearsIrrReturnFlowVolume.Col;
        grdBreakpointYearsIrrReturnFlowVolume.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointYearsIrrReturnFlowVolume.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointYearsReturnFlowVolume',LValue,LMessage) then
        begin
          LIrrigationBlock.ReturnFlowVolumeBreakpointYearByIndex[LCol] := StrToInt(LValue);
          RePopulateDataViewer;
          //DoContextValidation(dvtReturnFlowFactorBreakpointYear);
        end
        else
          grdBreakpointYearsIrrReturnFlowVolume.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockGrowthValidator.UpdateIrrBreakpointReturnFlowVolume;
const OPNAME = 'TIrrBlockGrowthValidator.UpdateIrrBreakpointReturnFlowVolume';
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
      with IrrBlockGrowthDialog do
      begin
        LCol := grdBreakpointIrrReturnFlowVolume.Col;
        grdBreakpointIrrReturnFlowVolume.ValidationError[LCol, 0, gveCellContext] := '';
        LValue := grdBreakpointIrrReturnFlowVolume.Cells[LCol,0];
        if FAppModules.FieldProperties.ValidateFieldProperty('BreakpointReturnFlowVolume',LValue,LMessage) then
        begin
          LIrrigationBlock.ReturnFlowVolumeByIndex[LCol] := StrToFloat(LValue);
          RePopulateDataViewer;
          //DoContextValidation (dvtBreakpointYearsIrrigationEfficiencies);
        end
        else
          grdBreakpointIrrReturnFlowVolume.ValidationError[LCol, 0, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

