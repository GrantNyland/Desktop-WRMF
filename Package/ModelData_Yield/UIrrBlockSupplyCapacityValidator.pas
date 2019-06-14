{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockSupplyCapacityValidator          *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockSupplyCapacityValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,       VCL.dialogs,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UIrrigationBlock,
  UIrrBlockSupplyCapacityDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TIrrBlockSupplyCapacityValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
   // procedure OnAfterPasteGridData(Sender: TObject);
 //   procedure OnAfterPasteColumnData(Sender: TObject);
    procedure RePopulateDataViewer;
    {
    procedure UpdateIrrigationBlockCatchmentReference;
    procedure UpdateIrrigationBlockRainCatchmentScalingFactor;
    procedure UpdateIrrigationBlockRainfallFactorGrid(AIndex : integer; AValue : double);
    procedure UpdateIrrigationBlockAPanConvFactorGrid(AIndex : integer; AValue : double);
    procedure UpdateIrrigationBlockPanEvaporationGrid(AIndex : integer; AValue : double);

//    procedure ValidateCatchmentRefNumber;
    procedure ValidateIrrigationBlockRainCatchmentScalingFactor;
    procedure ValidateIrrigationBlockRainfallFactorGrid(AFeature: IIrrigationBlock);
    procedure ValidateIrrigationBlockAPanConvFactorGrid(AFeature: IIrrigationBlock);
    procedure ValidateIrrigationBlockPanEvaporationGrid(AFeature: IIrrigationBlock);
    function GetAverageHydrologyFileValue(AINCFileName: string): double;   }
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function IrrBlockSupplyCapacityDialog: TIrrBlockSupplyCapacityDialog;
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
{* TIrrBlockSupplyCapacityValidator                                                      *}
{******************************************************************************}

procedure TIrrBlockSupplyCapacityValidator.CreateMemberObjects;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.CreateMemberObjects';
var
  lIndex      : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrBlockSupplyCapacityDialog.Create(FPanelOwner,FAppModules);

    with IrrBlockSupplyCapacityDialog do
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
         // TFieldStringGrid(ControlsParent.Components[lIndex]).ShowGridPopupMenu    := True;
         // TFieldStringGrid(ControlsParent.Components[lIndex]).AllowPasteFromExcel  := True;
         // TFieldStringGrid(ControlsParent.Components[lIndex]).OnPasteFromExcel     := Self.OnAfterPasteGridData;
      //    TFieldStringGrid(ControlsParent.Components[lIndex]).OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
       //   TFieldStringGrid(ControlsParent.Components[lIndex]).OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;
        end;
      end;
  {    IrrigationBlockRainCatchmentScalingFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockRainCatchmentScalingFactor');
      IrrigationBlockCatchmentFileNameEdit.FieldProperty           := FAppModules.FieldProperties.FieldProperty('IrrigationBlockHydrologyFileName');
      IrrigationBlockCatchmentMAPEdit.FieldProperty                := FAppModules.FieldProperties.FieldProperty('IrrigationBlockMAP'); }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.DestroyMemberObjects;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityValidator.Initialise: boolean;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Supply Capacity'; //FAppModules.Language.GetString('ViewData.EvaporationTransporation');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.ClearDataViewer;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.ClearDataViewer';
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
    with IrrBlockSupplyCapacityDialog do
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

procedure TIrrBlockSupplyCapacityValidator.PopulateDataViewer;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrBlockSupplyCapacityValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrBlockSupplyCapacityValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with TIrrBlockSupplyCapacityDialog do
    begin

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.RePopulateDataViewer;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.RePopulateDataViewer';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrBlockSupplyCapacityValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
  try
    with TIrrBlockSupplyCapacityDialog do
    begin

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityValidator.SaveState: boolean;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockSupplyCapacityValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrBlockSupplyCapacityValidator.DoContextValidation';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockSupplyCapacityValidator.IrrBlockSupplyCapacityDialog: TIrrBlockSupplyCapacityDialog;
const OPNAME = 'TIrrBlockSupplyCapacityValidator.IrrBlockSupplyCapacityDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrBlockSupplyCapacityDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

