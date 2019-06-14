{******************************************************************************}
{*  UNIT      : Contains the class TSoilPropertiesValidator.                 *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit USoilPropertiesValidator;

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
  USoilPropertiesDialog,
  UNetworkFeaturesData,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TSoilPropertiesValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;

    procedure UpdateIrrigationBlockUpperZoneReturnFlow;
    procedure UpdateIrrigationBlockLowerZoneReturnFlow;
    procedure UpdateIrrigationBlockUpperZoneSoilMoistureCapacity;
    procedure UpdateIrrigationBlockLowerZoneSoilMoistureCapacity;
    procedure UpdateIrrigationBlockUpperZoneSoilMoistureTarget;
    procedure UpdateIrrigationBlockInitialSoilMoistureStorage;

    procedure ValidateIrrigationBlockUpperZoneReturnFlow;
    procedure ValidateIrrigationBlockLowerZoneReturnFlow;
    procedure ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
    procedure ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
    procedure ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
    procedure ValidateIrrigationBlockInitialSoilMoistureStorage;

    procedure RePopulateDataViewer;
  public
    function Initialise         : boolean; override;
    function SaveState          : boolean; override;
    function LanguageHasChanged : boolean; override;
    function StudyHasChanged    : boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function SoilPropertiesDialog: TSoilPropertiesDialog;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;    
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
  USelectChannelDialog, UIrrigationBlock,Math;

{******************************************************************************}
{* TSoilPropertiesValidator                                                   *}
{******************************************************************************}

procedure TSoilPropertiesValidator.CreateMemberObjects;
const OPNAME = 'TSoilPropertiesValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TSoilPropertiesDialog.Create(FPanelOwner,FAppModules);

    with TSoilPropertiesDialog(FPanel) do
    begin
      IrrigationBlockUpperZoneReturnFlowEdit.OnEnter            := OnEditControlEnter;
      IrrigationBlockUpperZoneReturnFlowEdit.OnExit             := OnEditControltExit;
      IrrigationBlockUpperZoneReturnFlowEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IrrigationBlockUpperZoneReturnFlow');

      IrrigationBlockLowerZoneReturnFlowEdit.OnEnter            := OnEditControlEnter;
      IrrigationBlockLowerZoneReturnFlowEdit.OnExit             := OnEditControltExit;
      IrrigationBlockLowerZoneReturnFlowEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IrrigationBlockLowerZoneReturnFlow');

      IrrigationBlockUpperZoneSoilMoistureCapacityEdit.OnEnter        := OnEditControlEnter;
      IrrigationBlockUpperZoneSoilMoistureCapacityEdit.OnExit         := OnEditControltExit;
      IrrigationBlockUpperZoneSoilMoistureCapacityEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockUpperZoneSoilMoistureCapacity');

      IrrigationBlockLowerZoneSoilMoistureCapacityEdit.OnEnter        := OnEditControlEnter;
      IrrigationBlockLowerZoneSoilMoistureCapacityEdit.OnExit         := OnEditControltExit;
      IrrigationBlockLowerZoneSoilMoistureCapacityEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockLowerZoneSoilMoistureCapacity');

      IrrigationBlockUpperZoneSoilMoistureTargetEdit.OnEnter          := OnEditControlEnter;
      IrrigationBlockUpperZoneSoilMoistureTargetEdit.OnExit           := OnEditControltExit;
      IrrigationBlockUpperZoneSoilMoistureTargetEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('IrrigationBlockUpperZoneSoilMoistureTarget');

      IrrigationBlockInitialSoilMoistureStorageEdit.OnEnter     := OnEditControlEnter;
      IrrigationBlockInitialSoilMoistureStorageEdit.OnExit      := OnEditControltExit;
      IrrigationBlockInitialSoilMoistureStorageEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IrrigationBlockInitialSoilMoistureStorage');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.DestroyMemberObjects;
const OPNAME = 'TSoilPropertiesValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesValidator.Initialise: boolean;
const OPNAME = 'TSoilPropertiesValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSoilPropertiesValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.SoilProperties');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ClearDataViewer;
const OPNAME = 'TSoilPropertiesValidator.ClearDataViewer';
var
  lpPanel : TSoilPropertiesDialog;
  lIndex  : integer;
  lFieldEdit : TFieldEdit;
  lComponent : TComponent;
begin
  inherited ClearDataViewer;
  try
    lpPanel := SoilPropertiesDialog;
    with lpPanel do
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
        end;
      end;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.PopulateDataViewer;
const OPNAME = 'TSoilPropertiesValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSoilPropertiesValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesValidator.StudyHasChanged: boolean;
const OPNAME = 'TSoilPropertiesValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TSoilPropertiesValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TSoilPropertiesValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with SoilPropertiesDialog do
    begin
      if((Sender = IrrigationBlockUpperZoneReturnFlowEdit) and (IrrigationBlockUpperZoneReturnFlowEdit.HasValueChanged ))then
        UpdateIrrigationBlockUpperZoneReturnFlow;
      if((Sender = IrrigationBlockLowerZoneReturnFlowEdit) and (IrrigationBlockLowerZoneReturnFlowEdit.HasValueChanged ))then
        UpdateIrrigationBlockLowerZoneReturnFlow;
      if((Sender = IrrigationBlockUpperZoneSoilMoistureCapacityEdit) and (IrrigationBlockUpperZoneSoilMoistureCapacityEdit.HasValueChanged ))then
        UpdateIrrigationBlockUpperZoneSoilMoistureCapacity;
      if((Sender = IrrigationBlockLowerZoneSoilMoistureCapacityEdit) and (IrrigationBlockLowerZoneSoilMoistureCapacityEdit.HasValueChanged ))then
        UpdateIrrigationBlockLowerZoneSoilMoistureCapacity;
      if((Sender = IrrigationBlockUpperZoneSoilMoistureTargetEdit) and (IrrigationBlockUpperZoneSoilMoistureTargetEdit.HasValueChanged ))then
        UpdateIrrigationBlockUpperZoneSoilMoistureTarget;
      if((Sender = IrrigationBlockInitialSoilMoistureStorageEdit) and (IrrigationBlockInitialSoilMoistureStorageEdit.HasValueChanged ))then
        UpdateIrrigationBlockInitialSoilMoistureStorage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.RePopulateDataViewer;
const OPNAME = 'TSoilPropertiesValidator.RePopulateDataViewer';
var
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneReturnFlowEdit.SetFieldValue(lIrrigationBlock.UpperZoneReturnFlow);
        IrrigationBlockLowerZoneReturnFlowEdit.SetFieldValue(lIrrigationBlock.LowerZoneReturnFlow);
        IrrigationBlockUpperZoneSoilMoistureCapacityEdit.SetFieldValue(lIrrigationBlock.UpperZoneSoilMoistureCapacity);
        IrrigationBlockLowerZoneSoilMoistureCapacityEdit.SetFieldValue(lIrrigationBlock.LowerZoneSoilMoistureCapacity);
        IrrigationBlockUpperZoneSoilMoistureTargetEdit.SetFieldValue(lIrrigationBlock.UpperZoneSoilMoistureTarget);
        IrrigationBlockInitialSoilMoistureStorageEdit.SetFieldValue(lIrrigationBlock.InitialSoilMoistureStorage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.UpdateIrrigationBlockUpperZoneReturnFlow;
const OPNAME = 'TSoilPropertiesValidator.UpdateIrrigationBlockUpperZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneReturnFlowEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(IrrigationBlockUpperZoneReturnFlowEdit.FieldProperty.FieldName,IrrigationBlockUpperZoneReturnFlowEdit.Text, LMessage) then
        begin
          lIrrigationBlock.UpperZoneReturnFlow := StrToFloat(IrrigationBlockUpperZoneReturnFlowEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtUpperZoneReturnFlow);
        end
        else
          IrrigationBlockUpperZoneReturnFlowEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesValidator.SaveState: boolean;
const OPNAME = 'TSoilPropertiesValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TSoilPropertiesValidator.DoContextValidation';
var
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      case AValidationType of
        dvtIrrigationBlockAll             : begin
                                              ValidateIrrigationBlockUpperZoneReturnFlow;
                                              ValidateIrrigationBlockLowerZoneReturnFlow;
                                              ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
                                              ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
                                              ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
                                              ValidateIrrigationBlockInitialSoilMoistureStorage;
                                            end;
        dvtUpperZoneReturnFlow            : ValidateIrrigationBlockUpperZoneReturnFlow;
        dvtLowerZoneReturnFlow            : ValidateIrrigationBlockLowerZoneReturnFlow;
        dvtUpperZoneSoilMoistureCapacity  : ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
        dvtLowerZoneSoilMoistureCapacity  : ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
        dvtUpperZoneSoilMoistureTarget    : ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
        dvtInitialSoilMoistureStorage     : ValidateIrrigationBlockInitialSoilMoistureStorage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSoilPropertiesValidator.SoilPropertiesDialog: TSoilPropertiesDialog;
const OPNAME = 'TSoilPropertiesValidator.SoilPropertiesDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TSoilPropertiesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.UpdateIrrigationBlockInitialSoilMoistureStorage;
const OPNAME = 'TSoilPropertiesValidator.UpdateIrrigationBlockInitialSoilMoistureStorage';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockInitialSoilMoistureStorageEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                IrrigationBlockInitialSoilMoistureStorageEdit.FieldProperty.FieldName,
                IrrigationBlockInitialSoilMoistureStorageEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.InitialSoilMoistureStorage := StrToFloat(IrrigationBlockInitialSoilMoistureStorageEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtInitialSoilMoistureStorage);
        end
        else
          IrrigationBlockInitialSoilMoistureStorageEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.UpdateIrrigationBlockLowerZoneReturnFlow;
const OPNAME = 'TSoilPropertiesValidator.UpdateIrrigationBlockLowerZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockLowerZoneReturnFlowEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                IrrigationBlockLowerZoneReturnFlowEdit.FieldProperty.FieldName,
                IrrigationBlockLowerZoneReturnFlowEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.LowerZoneReturnFlow := StrToFloat(IrrigationBlockLowerZoneReturnFlowEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtLowerZoneReturnFlow);
        end
        else
          IrrigationBlockLowerZoneReturnFlowEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.UpdateIrrigationBlockLowerZoneSoilMoistureCapacity;
const OPNAME = 'TSoilPropertiesValidator.UpdateIrrigationBlockLowerZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockLowerZoneSoilMoistureCapacityEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                IrrigationBlockLowerZoneSoilMoistureCapacityEdit.FieldProperty.FieldName,
                IrrigationBlockLowerZoneSoilMoistureCapacityEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.LowerZoneSoilMoistureCapacity := StrToFloat(IrrigationBlockLowerZoneSoilMoistureCapacityEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtLowerZoneSoilMoistureCapacity);
        end
        else
          IrrigationBlockLowerZoneSoilMoistureCapacityEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.UpdateIrrigationBlockUpperZoneSoilMoistureCapacity;
const OPNAME = 'TSoilPropertiesValidator.UpdateIrrigationBlockUpperZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneSoilMoistureCapacityEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                IrrigationBlockUpperZoneSoilMoistureCapacityEdit.FieldProperty.FieldName,
                IrrigationBlockUpperZoneSoilMoistureCapacityEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.UpperZoneSoilMoistureCapacity := StrToFloat(IrrigationBlockUpperZoneSoilMoistureCapacityEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtUpperZoneSoilMoistureCapacity);
        end
        else
          IrrigationBlockUpperZoneSoilMoistureCapacityEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.UpdateIrrigationBlockUpperZoneSoilMoistureTarget;
const OPNAME = 'TSoilPropertiesValidator.UpdateIrrigationBlockUpperZoneSoilMoistureTarget';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneSoilMoistureTargetEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                IrrigationBlockUpperZoneSoilMoistureTargetEdit.FieldProperty.FieldName,
                IrrigationBlockUpperZoneSoilMoistureTargetEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.UpperZoneSoilMoistureTarget := StrToFloat(IrrigationBlockUpperZoneSoilMoistureTargetEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtUpperZoneSoilMoistureTarget);
        end
        else
          IrrigationBlockUpperZoneSoilMoistureTargetEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ValidateIrrigationBlockUpperZoneReturnFlow;
const OPNAME='TSoilPropertiesValidator.ValidateIrrigationBlockUpperZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneReturnFlowEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockUpperZoneReturnFlowEdit.FieldProperty.FieldName)) then
          IrrigationBlockUpperZoneReturnFlowEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ValidateIrrigationBlockInitialSoilMoistureStorage;
const OPNAME='TSoilPropertiesValidator.ValidateIrrigationBlockInitialSoilMoistureStorage';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockInitialSoilMoistureStorageEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockInitialSoilMoistureStorageEdit.FieldProperty.FieldName)) then
          IrrigationBlockInitialSoilMoistureStorageEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ValidateIrrigationBlockLowerZoneReturnFlow;
const OPNAME='TSoilPropertiesValidator.ValidateIrrigationBlockLowerZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockLowerZoneReturnFlowEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockLowerZoneReturnFlowEdit.FieldProperty.FieldName)) then
          IrrigationBlockLowerZoneReturnFlowEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
const OPNAME='TSoilPropertiesValidator.ValidateIrrigationBlockLowerZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockLowerZoneSoilMoistureCapacityEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockLowerZoneSoilMoistureCapacityEdit.FieldProperty.FieldName)) then
          IrrigationBlockLowerZoneSoilMoistureCapacityEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
const OPNAME='TSoilPropertiesValidator.ValidateIrrigationBlockUpperZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneSoilMoistureCapacityEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockUpperZoneSoilMoistureCapacityEdit.FieldProperty.FieldName)) then
          IrrigationBlockUpperZoneSoilMoistureCapacityEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSoilPropertiesValidator.ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
const OPNAME='TSoilPropertiesValidator.ValidateIrrigationBlockUpperZoneSoilMoistureTarget';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with SoilPropertiesDialog do
      begin
        IrrigationBlockUpperZoneSoilMoistureTargetEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockUpperZoneSoilMoistureTargetEdit.FieldProperty.FieldName)) then
          IrrigationBlockUpperZoneSoilMoistureTargetEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

