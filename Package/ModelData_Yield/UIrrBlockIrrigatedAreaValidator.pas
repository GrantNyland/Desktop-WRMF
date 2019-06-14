{******************************************************************************}
{*  UNIT      : Contains the class TIrrBlockIrrigatedAreaValidator.                 *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrBlockIrrigatedAreaValidator;

interface

uses
  Classes,
  VCL.Dialogs,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UIrrBlockIrrigatedAreaDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UAbstractFileNamesObject;

type
  TIrrBlockIrrigatedAreaValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    //procedure OnDroughtApplicableClick(Sender: TObject);
    procedure OnCurtailIrrigationAbstractionClick(Sender: TObject);

    procedure RePopulateDataViewer;

    //________________________________________________________Properties________________________________________
   // procedure UpdateMaxWaterAllocation;
    procedure UpdateIrrigationSupplyCapacity;
   // procedure UpdateAllocatedIrrigationArea;
    //procedure UpdateEfficiencyFactor;

    //procedure ValidateMaxWaterAllocation;
    procedure ValidateIrrigationSupplyCapacity;
   // procedure ValidateAllocatedIrrigationArea;
   // procedure ValidateEfficiencyFactor;

    //________________________________________________________Curtailment________________________________________
    //procedure UpdateDroughtApplicable;
    procedure UpdatenCurtailIrrigationAbstraction;
    //procedure ValidateIrrigationDroughtApplicable;
    procedure ValidateCurtailIrrigationAbstraction;

    //________________________________________________________Soil________________________________________
    procedure UpdateIrrigationBlockUpperSoilOutflow;
    procedure UpdateMultiplicationFactor;
    {
    procedure UpdateIrrigationBlockUpperZoneReturnFlow;
    procedure UpdateIrrigationBlockLowerZoneReturnFlow;
    procedure UpdateIrrigationBlockUpperZoneSoilMoistureCapacity;
    procedure UpdateIrrigationBlockLowerZoneSoilMoistureCapacity;
    procedure UpdateIrrigationBlockUpperZoneSoilMoistureTarget;
    procedure UpdateIrrigationBlockInitialSoilMoistureStorage;
    }
    procedure UpdateMaxUpperZoneMoisture;
    procedure UpdateMinUpperZoneMoisture;


    procedure ValidateMultiplicationFactor;
    procedure ValidateIrrigationBlockUpperSoilOutflow;
   { procedure ValidateIrrigationBlockUpperZoneReturnFlow;
    procedure ValidateIrrigationBlockLowerZoneReturnFlow;
    procedure ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
    procedure ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
    procedure ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
    procedure ValidateIrrigationBlockInitialSoilMoistureStorage;
    }
    procedure ValidateMaxUpperZoneMoisture;
    procedure ValidateeMinUpperZoneMoisture;

    //________________________________________________________ReturnFlow________________________________________
  //  procedure UpdateCanalTransportLoss;
   // procedure ValidateCanalTransportLoss;
    procedure UpdateCanalSeepageLoss;
    procedure ValidateCanalSeepageLoss;
    procedure UpdateCanalTransmissionLoss;
    procedure ValidateCanalTransmissionLoss;
  //  procedure UpdateReturnFlowLoss;
  //  procedure ValidateReturnFlowLoss;
//    procedure UpdateReturnFlowFactor;
  //  procedure ValidateReturnFlowFactor;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function IrrigatedAreaDialog: TIrrBlockIrrigatedAreaDialog;
  end;

implementation

uses
  VCL.Grids,
  Contnrs,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UParameterData,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  UIrrigationBlock,
  Math,
  UNetworkFeaturesData,
  UFileNames,
  UNetworkElementData;

{******************************************************************************}
{* TIrrBlockIrrigatedAreaValidator                                                  *}
{******************************************************************************}

procedure TIrrBlockIrrigatedAreaValidator.CreateMemberObjects;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrBlockIrrigatedAreaDialog.Create(FPanelOwner,FAppModules);

    with TIrrBlockIrrigatedAreaDialog(FPanel) do
    begin
      //________________________________________________________Properties________________________________________
    {  AllocatedIrrigationAreaEdit.OnEnter        := OnEditControlEnter;
      AllocatedIrrigationAreaEdit.OnExit         := OnEditControltExit;
      AllocatedIrrigationAreaEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockAllocatedIrrigationArea');
     }
      IrrigationSupplyCapacityEdit.OnEnter             := OnEditControlEnter;
      IrrigationSupplyCapacityEdit.OnExit              := OnEditControltExit;
      IrrigationSupplyCapacityEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('IrrigationSupplyCapacity');

 //     MaxWaterAllocationEdit.OnEnter             := OnEditControlEnter;
  //    MaxWaterAllocationEdit.OnExit              := OnEditControltExit;
  //    MaxWaterAllocationEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('IrrigationBlockMaxWaterAllocation');
       {
      EfficiencyFactorEdit.OnEnter               := OnEditControlEnter;
      EfficiencyFactorEdit.OnExit                := OnEditControltExit;
      EfficiencyFactorEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('IrrigationBlockEfficiencyFactor');
                     }
      //________________________________________________________Curtailment________________________________________
    {  DroughtApplicableChkBox.OnEnter            := OnEditControlEnter;
      DroughtApplicableChkBox.OnClick            := OnDroughtApplicableClick;
      DroughtApplicableChkBox.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IrrigationBlockDroughtApplicable');
           }
      CurtailIrrAbstractionChkBox.OnEnter         := OnEditControlEnter;
      CurtailIrrAbstractionChkBox.OnClick          := OnCurtailIrrigationAbstractionClick;
      CurtailIrrAbstractionChkBox.FieldProperty   := FAppModules.FieldProperties.FieldProperty('CurtailIrrigationAbstraction');

      //________________________________________________________Soil________________________________________
      edtUpperSoilOutflow.OnEnter            := OnEditControlEnter;
      edtUpperSoilOutflow.OnExit             := OnEditControltExit;
      edtUpperSoilOutflow.FieldProperty      := FAppModules.FieldProperties.FieldProperty('UpperSoilOutflow');

      edtMultiplicationFactor.OnEnter            := OnEditControlEnter;
      edtMultiplicationFactor.OnExit             := OnEditControltExit;
      edtMultiplicationFactor.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IrrBlockMultiplicationFactor');

     {
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
       }
      edtMaxUpperZoneMoisture.OnEnter     := OnEditControlEnter;
      edtMaxUpperZoneMoisture.OnExit      := OnEditControltExit;
      edtMaxUpperZoneMoisture.FieldProperty      := FAppModules.FieldProperties.FieldProperty('MaxUpperZoneMoisture');

      edtMinUpperZoneMoisture.OnEnter     := OnEditControlEnter;
      edtMinUpperZoneMoisture.OnExit      := OnEditControltExit;
      edtMinUpperZoneMoisture.FieldProperty      := FAppModules.FieldProperties.FieldProperty('MinUpperZoneMoisture');



      //________________________________________________________ReturnFlow________________________________________
      //CanalTransportLossEdit.OnEnter             := OnEditControlEnter;
     // CanalTransportLossEdit.OnExit              := OnEditControltExit;
      //CanalTransportLossEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('IrrigationBlockCanalTransportLoss');

      edtCanalSeepageLoss.OnEnter             := OnEditControlEnter;
      edtCanalSeepageLoss.OnExit              := OnEditControltExit;
      edtCanalSeepageLoss.FieldProperty       := FAppModules.FieldProperties.FieldProperty('CanalSeepageLoss');

      edtCanalTransmissionLoss.OnEnter             := OnEditControlEnter;
      edtCanalTransmissionLoss.OnExit              := OnEditControltExit;
      edtCanalTransmissionLoss.FieldProperty       := FAppModules.FieldProperties.FieldProperty('CanalTransmissionLoss');

   //   ReturnFlowLossEdit.OnEnter             := OnEditControlEnter;
  //    ReturnFlowLossEdit.OnExit              := OnEditControltExit;
     // ReturnFlowLossEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('IrrigationBlockReturnFlowLoss');

   //   ReturnFlowFactorEdit.OnEnter             := OnEditControlEnter;
   //   ReturnFlowFactorEdit.OnExit              := OnEditControltExit;
    //  ReturnFlowFactorEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('IrrigationBlockReturnFlowFactor');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.DestroyMemberObjects;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaValidator.Initialise: boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption  := FAppModules.Language.GetString('ViewData.IrrigatedArea');
    Result          := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TIrrBlockIrrigatedAreaValidator.OnDroughtApplicableClick(Sender: TObject);
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.OnDroughtApplicableClick';
begin
  try
    UpdateDroughtApplicable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
procedure TIrrBlockIrrigatedAreaValidator.OnCurtailIrrigationAbstractionClick(Sender: TObject);
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.OnCurtailIrrigationAbstractionClick';
begin
  try
    UpdatenCurtailIrrigationAbstraction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ClearDataViewer;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ClearDataViewer';
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
    with IrrigatedAreaDialog do
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

procedure TIrrBlockIrrigatedAreaValidator.PopulateDataViewer;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IrrigatedAreaDialog do
    begin
      //________________________________________________________Properties________________________________________
     // if((Sender = AllocatedIrrigationAreaEdit) and (AllocatedIrrigationAreaEdit.HasValueChanged ))then
     //   UpdateAllocatedIrrigationArea;
      if((Sender = IrrigationSupplyCapacityEdit) and (IrrigationSupplyCapacityEdit.HasValueChanged ))then
        UpdateIrrigationSupplyCapacity;
   //   if((Sender = MaxWaterAllocationEdit) and (MaxWaterAllocationEdit.HasValueChanged ))then
   //     UpdateMaxWaterAllocation;
     // if((Sender = EfficiencyFactorEdit) and (EfficiencyFactorEdit.HasValueChanged ))then
    //    UpdateEfficiencyFactor; }

      //________________________________________________________Soil________________________________________
      if((Sender = edtUpperSoilOutflow) and (edtUpperSoilOutflow.HasValueChanged ))then
        UpdateIrrigationBlockUpperSoilOutflow;
      if((Sender = edtMultiplicationFactor) and (edtMultiplicationFactor.HasValueChanged ))then
        UpdateMultiplicationFactor;

     {
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
        }
      if((Sender = edtMaxUpperZoneMoisture) and (edtMaxUpperZoneMoisture.HasValueChanged ))then
        UpdateMaxUpperZoneMoisture;
      if((Sender = edtMinUpperZoneMoisture) and (edtMinUpperZoneMoisture.HasValueChanged ))then
        UpdateMinUpperZoneMoisture;

      //________________________________________________________ReturnFlow________________________________________
    // if((Sender = CanalTransportLossEdit) and (CanalTransportLossEdit.HasValueChanged ))then
     //   UpdateCanalTransportLoss;
      if((Sender = edtCanalSeepageLoss) and (edtCanalSeepageLoss.HasValueChanged ))then
        UpdateCanalSeepageLoss;
      if((Sender = edtCanalTransmissionLoss) and (edtCanalTransmissionLoss.HasValueChanged ))then
        UpdateCanalTransmissionLoss;
   //   if((Sender = ReturnFlowLossEdit) and (ReturnFlowLossEdit.HasValueChanged ))then
  //      UpdateReturnFlowLoss;
 //     if((Sender = ReturnFlowFactorEdit) and (ReturnFlowFactorEdit.HasValueChanged ))then
 //       UpdateReturnFlowFactor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.RePopulateDataViewer;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.RePopulateDataViewer';
var
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        //________________________________________________________Properties________________________________________
        //AllocatedIrrigationAreaEdit.SetFieldValue(lIrrigationBlock.AllocatedIrrigationArea);
        if lIrrigationBlock.IrrigationSupplyCapacity = NullFloat then
          IrrigationSupplyCapacityEdit.Text := '' //SetFieldValue(0)
        else
          IrrigationSupplyCapacityEdit.SetFieldValue(lIrrigationBlock.IrrigationSupplyCapacity);

      //  MaxWaterAllocationEdit.SetFieldValue(lIrrigationBlock.MaxWaterAllocation);
      //  EfficiencyFactorEdit.SetFieldValue(lIrrigationBlock.EfficiencyFactor);
      //         }
        //________________________________________________________Curtailment________________________________________
      //  DroughtApplicableChkBox.Checked      := (LIrrigationBlock.DroughtApplicable = 1);
        CurtailIrrAbstractionChkBox.Checked := (LIrrigationBlock.CurtailIrrigationAbstraction = 1);

        //________________________________________________________Soil________________________________________

        if lIrrigationBlock.UpperSoilOutflow = NullFloat then
          edtUpperSoilOutflow.Text := '' //.SetFieldValue(0)
        else
          edtUpperSoilOutflow.SetFieldValue(lIrrigationBlock.UpperSoilOutflow);
        edtMultiplicationFactor.SetFieldValue(lIrrigationBlock.MultiplicationFactor);
        {
        IrrigationBlockUpperZoneReturnFlowEdit.SetFieldValue(lIrrigationBlock.UpperZoneReturnFlow);
        IrrigationBlockLowerZoneReturnFlowEdit.SetFieldValue(lIrrigationBlock.LowerZoneReturnFlow);
        IrrigationBlockUpperZoneSoilMoistureCapacityEdit.SetFieldValue(lIrrigationBlock.UpperZoneSoilMoistureCapacity);
        IrrigationBlockLowerZoneSoilMoistureCapacityEdit.SetFieldValue(lIrrigationBlock.LowerZoneSoilMoistureCapacity);
        IrrigationBlockUpperZoneSoilMoistureTargetEdit.SetFieldValue(lIrrigationBlock.UpperZoneSoilMoistureTarget);
        IrrigationBlockInitialSoilMoistureStorageEdit.SetFieldValue(lIrrigationBlock.InitialSoilMoistureStorage); }
        if lIrrigationBlock.MaxUpperZoneMoisture = NullFloat then
          edtMaxUpperZoneMoisture.Text := '' //.SetFieldValue(0)
        else
          edtMaxUpperZoneMoisture.SetFieldValue(lIrrigationBlock.MaxUpperZoneMoisture);
        if lIrrigationBlock.MinUpperZoneMoisture = NullFloat then
          edtMinUpperZoneMoisture.Text := '' //.SetFieldValue(0)
        else
          edtMinUpperZoneMoisture.SetFieldValue(lIrrigationBlock.MinUpperZoneMoisture);
        //________________________________________________________ReturnFlow________________________________________
       // CanalTransportLossEdit.SetFieldValue(lIrrigationBlock.CanalTransportLoss);

        if lIrrigationBlock.CanalTransmissionLoss = NullFloat then
          edtCanalTransmissionLoss.Text := '' //.SetFieldValue(0)
        else
          edtCanalTransmissionLoss.SetFieldValue(lIrrigationBlock.CanalTransmissionLoss);
        if lIrrigationBlock.CanalSeepageLoss = NullFloat then
          edtCanalSeepageLoss.Text := '' //.SetFieldValue(0)
        else
         edtCanalSeepageLoss.SetFieldValue(lIrrigationBlock.CanalSeepageLoss);

     //   ReturnFlowLossEdit.SetFieldValue(lIrrigationBlock.ReturnFlowLoss);
      //  ReturnFlowFactorEdit.SetFieldValue(lIrrigationBlock.ReturnFlowFactor);


        if (lIrrigationBlock.IrrigationBlockType <> 4) then
        begin
          IrrigationSupplyCapacityLabel.Enabled := False;
          IrrigationSupplyCapacityEdit.Enabled := False;
          IrrigationSupplyCapacityEdit.Color := clBtnShadow;

          lblUpperSoilOutflow.Enabled := False;
          edtUpperSoilOutflow.Enabled := False;
          edtUpperSoilOutflow.Color := clBtnFace;

          lblMaxUpperZoneMoisture.Enabled := False;
          edtMaxUpperZoneMoisture.Enabled := False;
          edtMaxUpperZoneMoisture.Color := clBtnFace;

          lblMinUpperZoneMoisture.Enabled := False;
          edtMinUpperZoneMoisture.Enabled := False;
          edtMinUpperZoneMoisture.Color := clBtnFace;

          lblCanalSeepageLoss.Enabled := False;
          edtCanalSeepageLoss.Enabled := False;
          edtCanalSeepageLoss.Color := clBtnFace;

          lblCanalTransmissionLoss.Enabled := False;
          edtCanalTransmissionLoss.Enabled := False;
          edtCanalTransmissionLoss.Color := clBtnFace;

        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaValidator.SaveState: boolean;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.DoContextValidation';
var
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      case AValidationType of
        dvtIrrigationBlockAll:
          begin
            //________________________________________________________Properties________________________________________
           // ValidateAllocatedIrrigationArea;
            ValidateIrrigationSupplyCapacity;
           // ValidateEfficiencyFactor;
         //   ValidateMaxWaterAllocation;
            //ValidateIrrigationDroughtApplicable;

            //________________________________________________________Soil________________________________________

         {   ValidateIrrigationBlockUpperZoneReturnFlow;
            ValidateIrrigationBlockLowerZoneReturnFlow;
            ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
            ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
            ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
            ValidateIrrigationBlockInitialSoilMoistureStorage;   }
            ValidateMultiplicationFactor;
            //________________________________________________________ReturnFlow________________________________________
            ValidateCanalTransmissionLoss;
          //  ValidateCanalTransportLoss;
         //   ValidateReturnFlowLoss;
          //  ValidateReturnFlowFactor;


          end;
        //________________________________________________________Properties________________________________________
       // dvtIrrigationBlockAllocatedIrrigationArea   : ValidateAllocatedIrrigationArea;
      //  dvtIrrigationBlockEfficiencyFactor          : ValidateEfficiencyFactor;
        //                                          : ValidateEfficiencyFactor;
      //  dvtIrrigationBlockMaxWaterAllocation        : ValidateMaxWaterAllocation;
        //dvtIrrigationBlockDroughtApplicable         : ValidateIrrigationDroughtApplicable;

        //________________________________________________________Properties________________________________________
       { dvtUpperZoneReturnFlow            : ValidateIrrigationBlockUpperZoneReturnFlow;
        dvtLowerZoneReturnFlow            : ValidateIrrigationBlockLowerZoneReturnFlow;
        dvtUpperZoneSoilMoistureCapacity  : ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
        dvtLowerZoneSoilMoistureCapacity  : ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
        dvtUpperZoneSoilMoistureTarget    : ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
        dvtInitialSoilMoistureStorage     : ValidateIrrigationBlockInitialSoilMoistureStorage;   }
        dvtIrrBlockMultiplicationFactor   : ValidateMultiplicationFactor;
        //________________________________________________________ReturnFlow________________________________________
      //  dvtIrrigationBlockCanalTransportLoss        : ValidateCanalTransportLoss;
        //                                              ValidateCanalTransmissionLoss;
        //                                              ValidateCanalTransportLoss;
        //                                              ValidateReturnFlowLoss;
        //                                              ValidateReturnFlowFactor;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrBlockIrrigatedAreaValidator.IrrigatedAreaDialog: TIrrBlockIrrigatedAreaDialog;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.IrrigatedAreaDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrBlockIrrigatedAreaDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________Properties________________________________________
{procedure TIrrBlockIrrigatedAreaValidator.UpdateAllocatedIrrigationArea;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateAllocatedIrrigationArea';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        AllocatedIrrigationAreaEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                AllocatedIrrigationAreaEdit.FieldProperty.FieldName,
                AllocatedIrrigationAreaEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.AllocatedIrrigationArea := StrToFloat(AllocatedIrrigationAreaEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockAllocatedIrrigationArea);
        end
        else
          AllocatedIrrigationAreaEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationSupplyCapacity;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationSupplyCapacity';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationSupplyCapacityEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                IrrigationSupplyCapacityEdit.FieldProperty.FieldName,
                IrrigationSupplyCapacityEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.IrrigationSupplyCapacity := StrToFloat(IrrigationSupplyCapacityEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockAllocatedIrrigationArea);
        end
        else
          IrrigationSupplyCapacityEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 {
procedure TIrrBlockIrrigatedAreaValidator.UpdateEfficiencyFactor;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateEfficiencyFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        EfficiencyFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                EfficiencyFactorEdit.FieldProperty.FieldName,
                EfficiencyFactorEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.EfficiencyFactor := StrToFloat(EfficiencyFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockEfficiencyFactor);
        end
        else
          EfficiencyFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
    }
procedure TIrrBlockIrrigatedAreaValidator.UpdateMaxUpperZoneMoisture;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateMaxUpperZoneMoisture';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMaxUpperZoneMoisture.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                edtMaxUpperZoneMoisture.FieldProperty.FieldName,
                edtMaxUpperZoneMoisture.Text,
                LMessage) then
        begin
          lIrrigationBlock.MaxUpperZoneMoisture := StrToFloat(edtMaxUpperZoneMoisture.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtMaxUpperZoneMoisture);
        end
        else
          edtMaxUpperZoneMoisture.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TIrrBlockIrrigatedAreaValidator.UpdateMaxWaterAllocation;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateMaxWaterAllocation';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        MaxWaterAllocationEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                MaxWaterAllocationEdit.FieldProperty.FieldName,
                MaxWaterAllocationEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.MaxWaterAllocation := StrToFloat(MaxWaterAllocationEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockMaxWaterAllocation);
        end
        else
          MaxWaterAllocationEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TIrrBlockIrrigatedAreaValidator.UpdateMinUpperZoneMoisture;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateMinUpperZoneMoisture';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMinUpperZoneMoisture.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                edtMinUpperZoneMoisture.FieldProperty.FieldName,
                edtMinUpperZoneMoisture.Text,
                LMessage) then
        begin
          lIrrigationBlock.MinUpperZoneMoisture := StrToFloat(edtMinUpperZoneMoisture.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtMinUpperZoneMoisture);
        end
        else
          edtMinUpperZoneMoisture.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TIrrBlockIrrigatedAreaValidator.ValidateAllocatedIrrigationArea;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateAllocatedIrrigationArea';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        AllocatedIrrigationAreaEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,AllocatedIrrigationAreaEdit.FieldProperty.FieldName) then
          AllocatedIrrigationAreaEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationSupplyCapacity;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateIrrigationSupplyCapacity';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationSupplyCapacityEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,IrrigationSupplyCapacityEdit.FieldProperty.FieldName) then
          IrrigationSupplyCapacityEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 {
procedure TIrrBlockIrrigatedAreaValidator.ValidateEfficiencyFactor;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateEfficiencyFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        EfficiencyFactorEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,EfficiencyFactorEdit.FieldProperty.FieldName) then
          EfficiencyFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
    }
procedure TIrrBlockIrrigatedAreaValidator.ValidateeMinUpperZoneMoisture;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateMaxWaterAllocation';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMinUpperZoneMoisture.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,edtMinUpperZoneMoisture.FieldProperty.FieldName) then
          edtMinUpperZoneMoisture.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateMaxUpperZoneMoisture;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateMaxUpperZoneMoisture';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMaxUpperZoneMoisture.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,edtMaxUpperZoneMoisture.FieldProperty.FieldName) then
          edtMaxUpperZoneMoisture.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 {
procedure TIrrBlockIrrigatedAreaValidator.ValidateMaxWaterAllocation;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateMaxWaterAllocation';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMaxUpperZoneMoisture.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,edtMaxUpperZoneMoisture.FieldProperty.FieldName) then
          MaxWaterAllocationEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________Curtailment________________________________________
  {
procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationDroughtApplicable;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateIrrigationDroughtApplicable';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        if not lIrrigationBlock.Validate(LMessage, DroughtApplicableChkBox.FieldProperty.FieldName) then
          DroughtApplicableChkBox.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
   }
procedure TIrrBlockIrrigatedAreaValidator.ValidateCurtailIrrigationAbstraction;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateIrrigationDroughtApplicable';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        if not lIrrigationBlock.Validate(LMessage, CurtailIrrAbstractionChkBox.FieldProperty.FieldName) then
          CurtailIrrAbstractionChkBox.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TIrrBlockIrrigatedAreaValidator.UpdateDroughtApplicable;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateDroughtApplicable';
var
  lIrrigationBlock  : IIrrigationBlock;
  lOldApplicable : integer;
  lNewApplicable : integer;
  lMessage    : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        lOldApplicable := lIrrigationBlock.DroughtApplicable;
        if DroughtApplicableChkBox.Checked then
          lNewApplicable := 1
        else
          lNewApplicable := 0;
        if (lOldApplicable <> lNewApplicable) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(DroughtApplicableChkBox.FieldProperty.FieldName,
              IntToStr(lNewApplicable),lMessage)) then
          begin
            lIrrigationBlock.DroughtApplicable := lNewApplicable;
            DroughtApplicableChkBox.Checked := (lIrrigationBlock.DroughtApplicable = 1);
            DoContextValidation(dvtIrrigationBlockDroughtApplicable);
          end
          else
            DroughtApplicableChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
procedure TIrrBlockIrrigatedAreaValidator.UpdatenCurtailIrrigationAbstraction;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdatenCurtailIrrigationAbstraction';
var
  lIrrigationBlock  : IIrrigationBlock;
  lOldApplicable : integer;
  lNewApplicable : integer;
  lMessage    : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        lOldApplicable := lIrrigationBlock.CurtailIrrigationAbstraction;
        if CurtailIrrAbstractionChkBox.Checked then
          lNewApplicable := 1
        else
          lNewApplicable := 0;
        if (lOldApplicable <> lNewApplicable) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(CurtailIrrAbstractionChkBox.FieldProperty.FieldName,
              IntToStr(lNewApplicable),lMessage)) then
          begin
            lIrrigationBlock.CurtailIrrigationAbstraction := lNewApplicable;
            CurtailIrrAbstractionChkBox.Checked := (lIrrigationBlock.CurtailIrrigationAbstraction = 1);
            //DoContextValidation(dvtIrrigationBlockDroughtApplicable);
          end
          else
            CurtailIrrAbstractionChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________________________________Soil________________________________________
 {
procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperZoneReturnFlow;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
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
     }
procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperSoilOutflow;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperSoilOutflow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtUpperSoilOutflow.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(edtUpperSoilOutflow.FieldProperty.FieldName,edtUpperSoilOutflow.Text, LMessage) then
        begin
          lIrrigationBlock.UpperSoilOutflow := StrToFloat(edtUpperSoilOutflow.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtUpperZoneReturnFlow);
        end
        else
          edtUpperSoilOutflow.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrBlockIrrigatedAreaValidator.UpdateMultiplicationFactor;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateMultiplicationFactor';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMultiplicationFactor.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(edtMultiplicationFactor.FieldProperty.FieldName,edtMultiplicationFactor.Text, LMessage) then
        begin
          lIrrigationBlock.MultiplicationFactor := StrToFloat(edtMultiplicationFactor.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrBlockMultiplicationFactor);
        end
        else
          edtMultiplicationFactor.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockInitialSoilMoistureStorage;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockInitialSoilMoistureStorage';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
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

procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockLowerZoneReturnFlow;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockLowerZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
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

procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockLowerZoneSoilMoistureCapacity;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockLowerZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
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

procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperZoneSoilMoistureCapacity;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
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

procedure TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperZoneSoilMoistureTarget;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateIrrigationBlockUpperZoneSoilMoistureTarget';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
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
  }
procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperSoilOutflow;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperSoilOutflow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtUpperSoilOutflow.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, edtUpperSoilOutflow.FieldProperty.FieldName)) then
          edtUpperSoilOutflow.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateMultiplicationFactor;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateMultiplicationFactor';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtMultiplicationFactor.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, edtMultiplicationFactor.FieldProperty.FieldName)) then
          edtMultiplicationFactor.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

 {
procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperZoneReturnFlow;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationBlockUpperZoneReturnFlowEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockUpperZoneReturnFlowEdit.FieldProperty.FieldName)) then
          IrrigationBlockUpperZoneReturnFlowEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockInitialSoilMoistureStorage;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockInitialSoilMoistureStorage';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationBlockInitialSoilMoistureStorageEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockInitialSoilMoistureStorageEdit.FieldProperty.FieldName)) then
          IrrigationBlockInitialSoilMoistureStorageEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockLowerZoneReturnFlow;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockLowerZoneReturnFlow';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationBlockLowerZoneReturnFlowEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockLowerZoneReturnFlowEdit.FieldProperty.FieldName)) then
          IrrigationBlockLowerZoneReturnFlowEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockLowerZoneSoilMoistureCapacity;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockLowerZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationBlockLowerZoneSoilMoistureCapacityEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockLowerZoneSoilMoistureCapacityEdit.FieldProperty.FieldName)) then
          IrrigationBlockLowerZoneSoilMoistureCapacityEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperZoneSoilMoistureCapacity;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperZoneSoilMoistureCapacity';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationBlockUpperZoneSoilMoistureCapacityEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockUpperZoneSoilMoistureCapacityEdit.FieldProperty.FieldName)) then
          IrrigationBlockUpperZoneSoilMoistureCapacityEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperZoneSoilMoistureTarget;
const OPNAME='TIrrBlockIrrigatedAreaValidator.ValidateIrrigationBlockUpperZoneSoilMoistureTarget';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        IrrigationBlockUpperZoneSoilMoistureTargetEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, IrrigationBlockUpperZoneSoilMoistureTargetEdit.FieldProperty.FieldName)) then
          IrrigationBlockUpperZoneSoilMoistureTargetEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
               }
//________________________________________________________ReturnFlow________________________________________
{
procedure TIrrBlockIrrigatedAreaValidator.ValidateCanalTransportLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateCanalTransportLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        CanalTransportLossEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,CanalTransportLossEdit.FieldProperty.FieldName) then
          CanalTransportLossEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.UpdateCanalTransportLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateCanalTransportLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        CanalTransportLossEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                CanalTransportLossEdit.FieldProperty.FieldName,
                CanalTransportLossEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.CanalTransportLoss := StrToFloat(CanalTransportLossEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockCanalTransportLoss);
        end
        else
          CanalTransportLossEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
      }
procedure TIrrBlockIrrigatedAreaValidator.ValidateCanalSeepageLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateCanalSeepageLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtCanalSeepageLoss.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,edtCanalSeepageLoss.FieldProperty.FieldName) then
          edtCanalSeepageLoss.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.UpdateCanalSeepageLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateCanalSeepageLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtCanalSeepageLoss.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                edtCanalSeepageLoss.FieldProperty.FieldName,
                edtCanalSeepageLoss.Text,
                LMessage) then
        begin
          lIrrigationBlock.CanalSeepageLoss := StrToFloat(edtCanalSeepageLoss.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockCanalTransportLoss);
        end
        else
          edtCanalSeepageLoss.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateCanalTransmissionLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateCanalTransmissionLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtCanalTransmissionLoss.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,edtCanalTransmissionLoss.FieldProperty.FieldName) then
          edtCanalTransmissionLoss.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.UpdateCanalTransmissionLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateCanalTransmissionLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        edtCanalTransmissionLoss.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                edtCanalTransmissionLoss.FieldProperty.FieldName,
                edtCanalTransmissionLoss.Text,
                LMessage) then
        begin
          lIrrigationBlock.CanalTransmissionLoss := StrToFloat(edtCanalTransmissionLoss.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtCanalTransmissionLoss);
        end
        else
          edtCanalTransmissionLoss.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

 {
procedure TIrrBlockIrrigatedAreaValidator.ValidateReturnFlowLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateReturnFlowLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        ReturnFlowLossEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,ReturnFlowLossEdit.FieldProperty.FieldName) then
          ReturnFlowLossEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.UpdateReturnFlowLoss;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateReturnFlowLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        ReturnFlowLossEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                ReturnFlowLossEdit.FieldProperty.FieldName,
                ReturnFlowLossEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.ReturnFlowLoss := StrToFloat(ReturnFlowLossEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockCanalTransportLoss);
        end
        else
          ReturnFlowLossEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.ValidateReturnFlowFactor;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.ValidateReturnFlowFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        ReturnFlowFactorEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,ReturnFlowFactorEdit.FieldProperty.FieldName) then
          ReturnFlowFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrBlockIrrigatedAreaValidator.UpdateReturnFlowFactor;
const OPNAME = 'TIrrBlockIrrigatedAreaValidator.UpdateReturnFlowFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigatedAreaDialog do
      begin
        ReturnFlowFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                ReturnFlowFactorEdit.FieldProperty.FieldName,
                ReturnFlowFactorEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.ReturnFlowFactor := StrToFloat(ReturnFlowFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockCanalTransportLoss);
        end
        else
          ReturnFlowLossEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
end.

