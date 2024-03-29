{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockWaterAllocationValidator          *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 2014/06/27                                                    *)
{*  COPYRIGHT : Copyright � 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockWaterAllocationValidator;

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
  UIrrBlockWaterAllocationDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TIrrBlockWaterAllocationValidator = class(TAbstractYieldDataDialogValidator)
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
    function IrrBlockWaterAllocationDialog: TIrrBlockWaterAllocationDialog;
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
{* TIrrBlockWaterAllocationValidator                                                      *}
{******************************************************************************}

procedure TIrrBlockWaterAllocationValidator.CreateMemberObjects;
const OPNAME = 'TIrrBlockWaterAllocationValidator.CreateMemberObjects';
var
  lIndex      : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrBlockWaterAllocationDialog.Create(FPanelOwner,FAppModules);

    with IrrBlockWaterAllocationDialog do
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

procedure TIrrBlockWaterAllocationValidator.DestroyMemberObjects;
const OPNAME = 'TIrrBlockWaterAllocationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationValidator.Initialise: boolean;
const OPNAME = 'TIrrBlockWaterAllocationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockWaterAllocationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Area Water Allocation'; //FAppModules.Language.GetString('ViewData.EvaporationTransporation');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.ClearDataViewer;
const OPNAME = 'TIrrBlockWaterAllocationValidator.ClearDataViewer';
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
    with IrrBlockWaterAllocationDialog do
    begin
     { for lIndex := 0 to 11 do
      begin
        IrrigationBlockRainfallFactorGrid.Cells[lIndex, 1] := '';
        IrrigationBlockPanEvaporationGrid.Cells[lIndex, 1] := '';
        IrrigationBlockAPanConvFactorGrid.Cells[lIndex, 1] := '';
      end;        }
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

procedure TIrrBlockWaterAllocationValidator.PopulateDataViewer;
const OPNAME = 'TIrrBlockWaterAllocationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrBlockWaterAllocationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   { if(AFieldName = 'IrrigationBlockNodeNumber') or
      (AFieldName = 'IrrigationBlockCropWaterUseType') then
      RePopulateDataViewer;       }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrBlockWaterAllocationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrBlockWaterAllocationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrBlockWaterAllocationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IrrBlockWaterAllocationDialog do
    begin
     // if ((Sender = IrrigationBlockRainCatchmentScalingFactorEdit) AND (IrrigationBlockRainCatchmentScalingFactorEdit.HasValueChanged ))then
     //   UpdateIrrigationBlockRainCatchmentScalingFactor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.RePopulateDataViewer;
const OPNAME = 'TIrrBlockWaterAllocationValidator.RePopulateDataViewer';
{var
  lIndex                       : integer;
  lCnt                         : integer;
  LFileAvarage                 : double;
  LParamReference              : IParamReference;
  lReservoirData               : IReservoirData;
  LFileName                    : string;
  lConfigData                  : IRunConfigurationData;
  lIrrigationBlock             : IIrrigationBlock;
  LRainfallFactorFieldProperty : TAbstractFieldProperty;
  LAPanConvFieldProperty       : TAbstractFieldProperty;
  LPanEvaporationFieldProperty : TAbstractFieldProperty; }
begin
  try
   { lConfigData       := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lConfigData <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        if Assigned(lIrrigationBlock) then
        begin

          IrrigationBlockRainCatchmentScalingFactorEdit.SetFieldValue(lIrrigationBlock.RainCatchmentScalingFactor);

          LRainfallFactorFieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockRainfallFactor');
          LAPanConvFieldProperty        := FAppModules.FieldProperties.FieldProperty('IrrigationBlockAPanConvFactor');
          LPanEvaporationFieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockPanEvaporation');

          IrrigationBlockRainfallFactorGrid.ClearFieldProperties;
          IrrigationBlockPanEvaporationGrid.ClearFieldProperties;
          IrrigationBlockAPanConvFactorGrid.ClearFieldProperties;

          grdMaxMeanRainfallFactor.ClearFieldProperties;


          for lCnt := 1 to 12 do
          begin
            IrrigationBlockRainfallFactorGrid.AddFieldProperty(LRainfallFactorFieldProperty);
            IrrigationBlockPanEvaporationGrid.AddFieldProperty(LPanEvaporationFieldProperty);
            IrrigationBlockAPanConvFactorGrid.AddFieldProperty(LAPanConvFieldProperty);
            grdMaxMeanRainfallFactor.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MaxMeanRainfallFactor'));

            IrrigationBlockRainfallFactorGrid.Cells[lCnt-1,0] := lConfigData.MonthNameByIndex[lCnt];
            IrrigationBlockPanEvaporationGrid.Cells[lCnt-1,0] := lConfigData.MonthNameByIndex[lCnt];
            IrrigationBlockAPanConvFactorGrid.Cells[lCnt-1,0] := lConfigData.MonthNameByIndex[lCnt];
            grdMaxMeanRainfallFactor.Cells[lCnt-1,0] := lConfigData.MonthNameByIndex[lCnt];

            IrrigationBlockRainfallFactorGrid.SetFieldValue(lCnt-1,1, lIrrigationBlock.RainfallFactor[lCnt]);
            IrrigationBlockPanEvaporationGrid.SetFieldValue(lCnt-1,1,lIrrigationBlock.PanEvaporation[lCnt]);
            IrrigationBlockAPanConvFactorGrid.SetFieldValue(lCnt-1,1, lIrrigationBlock.APanConvFactor[lCnt]);
            // awaiting data object...
            grdMaxMeanRainfallFactor.SetFieldValue(lCnt-1,1, 0.0);
          end;

          for lIndex := 0 to 11 do
          begin
            if lIrrigationBlock.CropWaterUseType = 1 then
              IrrigationBlockRainfallFactorGrid.IsColumnEnabled[lIndex] := True
            else
              IrrigationBlockRainfallFactorGrid.IsColumnEnabled[lIndex] := False;
          end;

          IrrigationBlockCatchmentFileNameEdit.Text := '';
          IrrigationBlockCatchmentMAPEdit.Text := '';

          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[
                            lIrrigationBlock.HydrologyNodeNumber];
          if (lReservoirData <> nil) then
          begin

            LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).CastCastParameterData.
                               ReferenceDataByCatchNumber[lReservoirData.ReservoirConfigurationData.CatchmentRef];
            if (LParamReference <> nil) then
            begin
              LFileName := LParamReference.FileReference + '.RAN';
              IrrigationBlockCatchmentFileNameEdit.Text := ExtractFileName(LFileName);
              LFileAvarage := GetAverageHydrologyFileValue(LFileName);
              if (LFileAvarage < 0.0) then
                 IrrigationBlockCatchmentMAPEdit.SetFieldValue('')
              else
                IrrigationBlockCatchmentMAPEdit.SetFieldValue(LFileAvarage);
            end;
          end;
        end;
      end;
    end; }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrBlockWaterAllocationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
  try
    with IrrBlockWaterAllocationDialog do
    begin
     { if (ASender = IrrigationBlockRainfallFactorGrid) then
        UpdateIrrigationBlockRainfallFactorGrid(ACol, StrToFloat(Trim(IrrigationBlockRainfallFactorGrid.Cells[ACol, ARow])));

      if (ASender = IrrigationBlockAPanConvFactorGrid) then
        UpdateIrrigationBlockAPanConvFactorGrid(ACol, StrToFloat(Trim(IrrigationBlockAPanConvFactorGrid.Cells[ACol, ARow])));

      if (ASender = IrrigationBlockPanEvaporationGrid) then
        UpdateIrrigationBlockPanEvaporationGrid(ACol, StrToFloat(Trim(IrrigationBlockPanEvaporationGrid.Cells[ACol, ARow])));  }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationValidator.SaveState: boolean;
const OPNAME = 'TIrrBlockWaterAllocationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrBlockWaterAllocationValidator.DoContextValidation';
//var
//  lIrrigationBlock  : IIrrigationBlock;
begin
  try
  {  lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      case AValidationType of
        dvtIrrigationBlockAll                         : begin
                                                          ValidateIrrigationBlockRainCatchmentScalingFactor;
                                                          ValidateIrrigationBlockRainfallFactorGrid(lIrrigationBlock);
                                                          ValidateIrrigationBlockPanEvaporationGrid(lIrrigationBlock);
                                                          ValidateIrrigationBlockAPanConvFactorGrid(lIrrigationBlock);
                                                        end;
        dvtIrrigationBlockRainCatchmentScalingFactor  : ValidateIrrigationBlockRainCatchmentScalingFactor;
        dvtIrrigationBlockRainfallFactor              : ValidateIrrigationBlockRainfallFactorGrid(lIrrigationBlock);
        dvtIrrigationBlockPanEvaporation              : ValidateIrrigationBlockPanEvaporationGrid(lIrrigationBlock);
        dvtIrrigationBlockAPanConvFactor              : ValidateIrrigationBlockAPanConvFactorGrid(lIrrigationBlock);
      end;
    end;   }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockWaterAllocationValidator.IrrBlockWaterAllocationDialog: TIrrBlockWaterAllocationDialog;
const OPNAME = 'TIrrBlockWaterAllocationValidator.IrrBlockWaterAllocationDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrBlockWaterAllocationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 (*
procedure TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockRainCatchmentScalingFactor;
const OPNAME = 'TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockRainCatchmentScalingFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        IrrigationBlockRainCatchmentScalingFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                  IrrigationBlockRainCatchmentScalingFactorEdit.FieldProperty.FieldName,
                  IrrigationBlockRainCatchmentScalingFactorEdit.Text,
                  LMessage) then
        begin
          lIrrigationBlock.RainCatchmentScalingFactor := StrToFloat(IrrigationBlockRainCatchmentScalingFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockRainCatchmentScalingFactor);
        end
        else
          IrrigationBlockRainCatchmentScalingFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockRainfallFactorGrid(AIndex: integer; AValue: double);
const OPNAME = 'TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockRainfallFactorGrid';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        IrrigationBlockRainfallFactorGrid.ValidationError[AIndex, 1, gveCellContext] :='';

        if (FAppModules.FieldProperties.ValidateFieldProperty(
              TAbstractFieldProperty(IrrigationBlockRainfallFactorGrid.FieldProperty(AIndex)).FieldName,
              FloatToStr(AValue),
              LMessage,
              AIndex+1)) then
        begin
          lIrrigationBlock.RainfallFactor[AIndex+1] := AValue;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockRainfallFactor);
        end else
          IrrigationBlockRainfallFactorGrid.ValidationError[AIndex, 1, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockAPanConvFactorGrid(AIndex: integer; AValue: double);
const OPNAME = 'TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockAPanConvFactorGrid';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        IrrigationBlockAPanConvFactorGrid.ValidationError[AIndex, 1, gveCellContext] :='';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            TAbstractFieldProperty(IrrigationBlockAPanConvFactorGrid.FieldProperty(AIndex)).FieldName,
            FloatToStr(AValue),
            LMessage, AIndex+1)) then
        begin
          lIrrigationBlock.APanConvFactor[AIndex+1] := AValue;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockAPanConvFactor);
        end else
          IrrigationBlockAPanConvFactorGrid.ValidationError[AIndex, 1, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockPanEvaporationGrid(AIndex: integer; AValue: double);
const OPNAME = 'TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockPanEvaporationGrid';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        IrrigationBlockPanEvaporationGrid.ValidationError[AIndex, 1, gveCellContext] :='';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            TAbstractFieldProperty(IrrigationBlockPanEvaporationGrid.FieldProperty(AIndex)).FieldName,
            FloatToStr(AValue),
            LMessage, AIndex+1)) then
        begin
          lIrrigationBlock.PanEvaporation[AIndex+1] := AValue;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockPanEvaporation);
        end else
          IrrigationBlockPanEvaporationGrid.ValidationError[AIndex, 1, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockRainCatchmentScalingFactor;
const OPNAME = 'TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockRainCatchmentScalingFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        IrrigationBlockRainCatchmentScalingFactorEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockRainCatchmentScalingFactorEdit.FieldProperty.FieldName)) then
          IrrigationBlockRainCatchmentScalingFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockAPanConvFactorGrid(AFeature: IIrrigationBlock);
const OPNAME = 'TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockAPanConvFactorGrid';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  try
    if (AFeature <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'IrrigationBlockAPanConvFactor')) then
          begin
            for lCol := 1 to IrrigationBlockAPanConvFactorGrid.ColCount do
              IrrigationBlockAPanConvFactorGrid.ValidationError[lCol, 1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to IrrigationBlockAPanConvFactorGrid.ColCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                IrrigationBlockAPanConvFactorGrid.ValidationError[lCol, 1, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                IrrigationBlockAPanConvFactorGrid.ValidationError[lCol, 1, gveCellContext] := ''
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

procedure TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockPanEvaporationGrid(AFeature: IIrrigationBlock);
const OPNAME = 'TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockPanEvaporationGrid';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  try
    if (AFeature <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'IrrigationBlockPanEvaporation')) then
          begin
            for lCol := 1 to IrrigationBlockPanEvaporationGrid.ColCount do
              IrrigationBlockPanEvaporationGrid.ValidationError[lCol, 1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to IrrigationBlockPanEvaporationGrid.ColCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                IrrigationBlockPanEvaporationGrid.ValidationError[lCol, 1, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                IrrigationBlockPanEvaporationGrid.ValidationError[lCol, 1, gveCellContext] := ''
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

procedure TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockRainfallFactorGrid(AFeature: iIrrigationBlock);
const OPNAME = 'TIrrBlockWaterAllocationValidator.ValidateIrrigationBlockRainfallFactorGrid';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
begin
  try
    if (AFeature <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'IrrigationBlockRainfallFactor')) then
          begin
            for lCol := 1 to IrrigationBlockRainfallFactorGrid.ColCount do
              IrrigationBlockRainfallFactorGrid.ValidationError[lCol, 1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to IrrigationBlockRainfallFactorGrid.ColCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                IrrigationBlockRainfallFactorGrid.ValidationError[lCol, 1, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                IrrigationBlockRainfallFactorGrid.ValidationError[lCol, 1, gveCellContext] := ''
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

procedure TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockCatchmentReference;
const OPNAME = 'TIrrBlockWaterAllocationValidator.UpdateIrrigationBlockCatchmentReference';
{var
  lIrrigationBlock  : IIrrigationBlock;
  lMessage  : string;
  lCatchRef : integer;
  lFound    : boolean;
  lIndexA   : integer;}
begin
 { try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrBlockWaterAllocationDialog do
      begin
        lCatchRef := Integer(IrrigationBlockCatchmentFileNameEdit.Items.Objects[IrrigationBlockCatchmentFileNameEdit.ItemIndex]);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            IrrigationBlockCatchmentFileNameEdit.FieldProperty.FieldName,
            IntToStr(lCatchRef), lMessage)) then
        begin
          lIrrigationBlock.CatchmentFileName := lCatchRef;
          lFound  := FALSE;
          lIndexA := 0;
          while (not lFound) do
          begin
            if (Integer(IrrigationBlockCatchmentFileNameEdit.Items.Objects[lIndexA]) = lIrrigationBlock.CatchmentFileName) then
              lFound := TRUE
            else
              lIndexA := lIndexA + 1;
          end;
          if (lFound) then
            IrrigationBlockCatchmentFileNameEdit.SetFieldIndex(lIndexA)
          else
            IrrigationBlockCatchmentFileNameEdit.SetFieldIndex(-1);
          DoContextValidation(dvtIrrigationBlockCatchmentFileName);
        end
        else
          IrrigationBlockCatchmentFileNameEdit.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end; }
  Showmessage(OPNAME);
end;

function TIrrBlockWaterAllocationValidator.GetAverageHydrologyFileValue(AINCFileName: string): double;
const OPNAME = 'TIrrBlockWaterAllocationValidator.GetAverageHydrologyFileValue';
var
  LSQL:string;
  LIndex: integer;
  LDataSet : TAbstractModelDataset;
begin
  Result := -1;
  try
    if(Trim(AINCFileName) <> '') then
    begin
      AINCFileName := Trim(UpperCase(ExtractFileName(AINCFileName)));
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LSQL := 'SELECT HydroTotalValue FROM HydrologyFileData  WHERE  FileName = '+QuotedStr(AINCFileName)+
                ' AND StudyAreaName = '+QuotedStr(FAppModules.StudyArea.StudyAreaCode);
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;

        if not LDataSet.DataSet.Eof then
        begin
          LIndex := 0;
          Result := 0;
          while not LDataSet.DataSet.Eof do
          begin
            Result := Result + LDataSet.DataSet.FieldByName('HydroTotalValue').AsFloat;
            LIndex := LIndex + 1;
            LDataSet.DataSet.Next;
          end;
          Result := Result / LIndex;
        end;
        LDataSet.DataSet.Close;
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockWaterAllocationValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TIrrBlockWaterAllocationValidator.OnAfterPasteGridData';
var
  LIrrigationBlock  : IIrrigationBlock;
  LRow,
  LCol              : integer;
  LValue            : double;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if(LIrrigationBlock <> nil) then
    begin
      if(Sender = IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid) then
      begin
        for LRow := IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.FixedRows to IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.RowCount - 1 do
        begin
          for LCol := IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.FixedCols to IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.ColCount - 1 do
          begin
            LValue := StrToFloat(Trim(IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.Cells[LCol,LRow]));
            LIrrigationBlock.RainfallFactor[LCol + 1] := LValue;
          end;
        end;
      end;

      if(Sender = IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid) then
      begin
        for LRow := IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.FixedRows to IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.RowCount - 1 do
        begin
          for LCol := IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.FixedCols to IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.ColCount - 1 do
          begin
            LValue := StrToFloat(Trim(IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.Cells[LCol,LRow]));
            LIrrigationBlock.APanConvFactor[LCol + 1] := LValue;
          end;
        end;
      end;

      if(Sender = IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid) then
      begin
        for LRow := IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.FixedRows to IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.RowCount - 1 do
        begin
          for LCol := IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.FixedCols to IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.ColCount - 1 do
          begin
            LValue := StrToFloat(Trim(IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.Cells[LCol,LRow]));
            LIrrigationBlock.PanEvaporation[LCol + 1] := LValue;
          end;
        end;
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtIrrigationBlockRainfallFactor);
      DoContextValidation(dvtIrrigationBlockAPanConvFactor);
      DoContextValidation(dvtIrrigationBlockPanEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockWaterAllocationValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TIrrBlockWaterAllocationValidator.OnAfterPasteColumnData';
var
  LIrrigationBlock  : IIrrigationBlock;
  LRow,
  LCol              : integer;
  LValue            : double;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if(LIrrigationBlock <> nil) then
    begin
      if(Sender = IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid) then
      begin
        for LRow := IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.FixedRows to IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.RowCount - 1 do
        begin
          LCol   := IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.Col;
          LValue := StrToFloat(Trim(IrrBlockWaterAllocationDialog.IrrigationBlockRainfallFactorGrid.Cells[LCol,LRow]));
          LIrrigationBlock.RainfallFactor[LCol + 1] := LValue;
        end;
      end;

      if(Sender = IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid) then
      begin
        for LRow := IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.FixedRows to IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.RowCount - 1 do
        begin
          LCol   := IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.Col;
          LValue := StrToFloat(Trim(IrrBlockWaterAllocationDialog.IrrigationBlockAPanConvFactorGrid.Cells[LCol,LRow]));
          LIrrigationBlock.APanConvFactor[LCol + 1] := LValue;
        end;
      end;

      if(Sender = IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid) then
      begin
        for LRow := IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.FixedRows to IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.RowCount - 1 do
        begin
          LCol   := IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.Col;
          LValue := StrToFloat(Trim(IrrBlockWaterAllocationDialog.IrrigationBlockPanEvaporationGrid.Cells[LCol,LRow]));
          LIrrigationBlock.PanEvaporation[LCol + 1] := LValue;
        end;
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtIrrigationBlockRainfallFactor);
      DoContextValidation(dvtIrrigationBlockAPanConvFactor);
      DoContextValidation(dvtIrrigationBlockPanEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
   *)
end.

