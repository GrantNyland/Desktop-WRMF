{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationBlockValidator.                 *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrigationBlockValidator;

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
  UIrrigationBlockDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UAbstractFileNamesObject;

type
  TIrrigationBlockValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnSelectDemandFileNameClick(Sender: TObject);
    procedure OnViewGraphClick(Sender: TObject);
    procedure OnViewGridClick(Sender: TObject);
    procedure OnDroughtApplicableClick(Sender: TObject);

    procedure SetGridGraphBtnState(AIrrigationBlock : IIrrigationBlock);
    procedure RePopulateDataViewer;
    procedure RePopulateNodes;
    procedure PopulateIrrigationBlockDemandFileCbx;

    procedure UpdateBlockNodeNumber;
    procedure UpdateName;
    procedure UpdateDescription;
    procedure UpdateDiversionChannelUpstreamNode;
    procedure UpdateReturnFlowChannelDownstreamNode;
    procedure UpdateMaxWaterAllocation;
    procedure UpdateAllocatedIrrigationArea;
    procedure UpdateFileName;
    procedure UpdateCanalTransportLoss;
    procedure UpdateEfficiencyFactor;
    procedure UpdateHydrologyNodeNumber;
    procedure UpdateDroughtApplicable;
    procedure UpdateXCoord;
    procedure UpdateYCoord;

    procedure ValidateNumber;
    procedure ValidateName;
    procedure ValidateDescription;
    procedure ValidateDiversionUpstreamNode;
    procedure ValidateReturnFlowDownstreamNode;
    procedure ValidateMaxWaterAllocation;
    procedure ValidateAllocatedIrrigationArea;
    procedure ValidateFileName;
    procedure ValidateCanalTransportLoss;
    procedure ValidateEfficiencyFactor;
    procedure ValidateNodeNumber;
    procedure ValidateIrrigationDroughtApplicable;
    procedure ValidateXCoord(AReservoirData: IReservoirData);
    procedure ValidateYCoord(AReservoirData: IReservoirData);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function IrrigationBlockDialog: TIrrigationBlockDialog;
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
{* TIrrigationBlockValidator                                                  *}
{******************************************************************************}

procedure TIrrigationBlockValidator.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrigationBlockDialog.Create(FPanelOwner,FAppModules);

    with TIrrigationBlockDialog(FPanel) do
    begin
      NumberEdit.OnEnter                 := OnEditControlEnter;
      NumberEdit.OnExit                  := OnEditControltExit;
      NumberEdit.FieldProperty           := FAppModules.FieldProperties.FieldProperty('IrrigationBlockBlockNumber');

      NameEdit.OnEnter                   := OnEditControlEnter;
      NameEdit.OnExit                    := OnEditControltExit;
      NameEdit.FieldProperty             := FAppModules.FieldProperties.FieldProperty('IrrigationBlockName');

      DescriptionEdit.OnEnter            := OnEditControlEnter;
      DescriptionEdit.OnExit             := OnEditControltExit;
      DescriptionEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IrrigationBlockDescription');

      UpStreamNodeCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('DiversionChannelUpStreamNode');
      UpStreamNodeCbx.OnEnter            := OnEditControlEnter;
      UpStreamNodeCbx.OnExit             := OnEditControltExit;

      DownStreamNodeCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ReturnFlowChannelDownStreamNode');
      DownStreamNodeCbx.OnEnter          := OnEditControlEnter;
      DownStreamNodeCbx.OnExit           := OnEditControltExit;

      MaxWaterAllocationEdit.OnEnter             := OnEditControlEnter;
      MaxWaterAllocationEdit.OnExit              := OnEditControltExit;
      MaxWaterAllocationEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('IrrigationBlockMaxWaterAllocation');

      AllocatedIrrigationAreaEdit.OnEnter        := OnEditControlEnter;
      AllocatedIrrigationAreaEdit.OnExit         := OnEditControltExit;
      AllocatedIrrigationAreaEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockAllocatedIrrigationArea');

      FileNameCbx.OnEnter                        := OnEditControlEnter;
      FileNameCbx.OnExit                         := OnEditControltExit;
      FileNameCbx.FieldProperty                  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockFileName');

      FileNameSelectBtn.OnEnter                  := OnEditControlEnter;
      FileNameSelectBtn.OnClick                  := OnSelectDemandFileNameClick;
      FileNameSelectBtn.Enabled                  := (FAppModules.User.UserRights in CUR_EditData) and
                                                                   (not FAppModules.StudyArea.ScenarioLocked);

      FileNameGridBtn.OnEnter              := OnEditControlEnter;
      FileNameGridBtn.OnExit               := OnEditControltExit;
      FileNameGridBtn.OnClick              := OnViewGridClick;

      FileNameGraphBtn.OnEnter             := OnEditControlEnter;
      FileNameGraphBtn.OnExit              := OnEditControltExit;
      FileNameGraphBtn.OnClick             := OnViewGraphClick;

      CanalTransportLossEdit.OnEnter       := OnEditControlEnter;
      CanalTransportLossEdit.OnExit        := OnEditControltExit;
      CanalTransportLossEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationBlockCanalTransportLoss');

      EfficiencyFactorEdit.OnEnter         := OnEditControlEnter;
      EfficiencyFactorEdit.OnExit          := OnEditControltExit;
      EfficiencyFactorEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('IrrigationBlockEfficiencyFactor');

      NodeNumberCbx.OnEnter                := OnEditControlEnter;
      NodeNumberCbx.OnExit                 := OnEditControltExit;
      NodeNumberCbx.FieldProperty          := FAppModules.FieldProperties.FieldProperty('IrrigationBlockNodeNumber');

      DroughtApplicableChkBox.OnEnter        := OnEditControlEnter;
      DroughtApplicableChkBox.OnClick        := OnDroughtApplicableClick;
      DroughtApplicableChkBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IrrigationBlockDroughtApplicable');

      IrrigationXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
      IrrigationXCoordEdit.OnEnter       := OnEditControlEnter;
      IrrigationXCoordEdit.OnExit        := OnEditControltExit;

      IrrigationYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
      IrrigationYCoordEdit.OnEnter       := OnEditControlEnter;
      IrrigationYCoordEdit.OnExit        := OnEditControltExit;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlockValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockValidator.Initialise: boolean;
const OPNAME = 'TIrrigationBlockValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrigationBlockValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption  := FAppModules.Language.GetString('ViewData.IrrigationBlock');
    Result          := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ClearDataViewer;
const OPNAME = 'TIrrigationBlockValidator.ClearDataViewer';
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
    with IrrigationBlockDialog do
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

procedure TIrrigationBlockValidator.PopulateDataViewer;
const OPNAME = 'TIrrigationBlockValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrigationBlockAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrigationBlockValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrigationBlockValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrigationBlockValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrigationBlockValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IrrigationBlockDialog do
    begin
      if((Sender = NumberEdit) and (NumberEdit.HasValueChanged ))then
        UpdateBlockNodeNumber;
      if((Sender = NameEdit) and (NameEdit.HasValueChanged ))then
        UpdateName;
      if((Sender = DescriptionEdit) and (DescriptionEdit.HasValueChanged ))then
        UpdateDescription;
      if((Sender = MaxWaterAllocationEdit) and (MaxWaterAllocationEdit.HasValueChanged ))then
        UpdateMaxWaterAllocation;
      if((Sender = AllocatedIrrigationAreaEdit) and (AllocatedIrrigationAreaEdit.HasValueChanged ))then
        UpdateAllocatedIrrigationArea;
      if((Sender = CanalTransportLossEdit) and (CanalTransportLossEdit.HasValueChanged ))then
        UpdateCanalTransportLoss;
      if((Sender = EfficiencyFactorEdit) and (EfficiencyFactorEdit.HasValueChanged ))then
        UpdateEfficiencyFactor;
      if((Sender = NodeNumberCbx) and (NodeNumberCbx.HasValueChanged ))then
        UpdateHydrologyNodeNumber;
      if((Sender = FileNameCbx) and (FileNameCbx.HasValueChanged ))then
        UpdateFileName;
      if((Sender = UpStreamNodeCbx) and (UpStreamNodeCbx.HasValueChanged)) then
        UpdateDiversionChannelUpstreamNode;
      if((Sender = DownStreamNodeCbx) and (DownStreamNodeCbx.HasValueChanged)) then
        UpdateReturnFlowChannelDownstreamNode;
      if ((sender = IrrigationXCoordEdit) AND (IrrigationXCoordEdit.HasValueChanged)) then
       UpdateXCoord;
      if ((sender = IrrigationYCoordEdit) AND (IrrigationYCoordEdit.HasValueChanged)) then
       UpdateYCoord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.RePopulateDataViewer;
const OPNAME = 'TIrrigationBlockValidator.RePopulateDataViewer';
var
  lIrrigationBlock  : IIrrigationBlock;
  lReservoirData    : IReservoirData;
  lReservoir        : IReservoirConfigurationData;
  lReservoirList    : IReservoirDataList;
  lIndexA           : integer;
  lFound            : boolean;
begin
  try
    LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NumberEdit.SetFieldValue(lIrrigationBlock.BlockNodeNumber);
        NameEdit.SetFieldValue(lIrrigationBlock.BlockName);
        DescriptionEdit.SetFieldValue(lIrrigationBlock.BlockDescription);
        MaxWaterAllocationEdit.SetFieldValue(lIrrigationBlock.MaxWaterAllocation);
        AllocatedIrrigationAreaEdit.SetFieldValue(lIrrigationBlock.AllocatedIrrigationArea);
        DroughtApplicableChkBox.Checked := (LIrrigationBlock.DroughtApplicable = 1);

        RePopulateNodes;
        if (lIrrigationBlock.DiversionChannel <> nil) then
        begin
          lReservoirData :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                               ReservoirList.ReservoirOrNodeByIdentifier[lIrrigationBlock.DiversionChannel.UpStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lReservoir := lReservoirData.ReservoirConfigurationData;
            UpStreamNodeCbx.SetFieldIndex
              (UpStreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
        end;

        if (lIrrigationBlock.ReturnFlowChannel <> nil) then
        begin
          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lIrrigationBlock.ReturnFlowChannel.DownStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lReservoir := lReservoirdata.ReservoirConfigurationData;
            DownStreamNodeCbx.SetFieldIndex
              (DownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
        end;

        PopulateIrrigationBlockDemandFileCbx;
        FileNameCbx.ItemIndex  := FileNameCbx.Items.IndexOf(lIrrigationBlock.FileName);
        SetGridGraphBtnState(LIrrigationBlock);

        CanalTransportLossEdit.SetFieldValue(lIrrigationBlock.CanalTransportLoss);
        EfficiencyFactorEdit.SetFieldValue(lIrrigationBlock.EfficiencyFactor);

        NodeNumberCbx.Items.Clear;
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.ReservoirList;
        if (lReservoirList <> nil) then
        begin
          for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
          begin
            lReservoirData  := lReservoirList.ReservoirOrNodeByIndex[lIndexA];
            if(lReservoirData.ReservoirConfigurationData.CatchmentRef <> 0) then
            begin
              NodeNumberCbx.Items.AddObject(lReservoirData.ReservoirConfigurationData.ReservoirName,
                                          TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
            end;

          end;

          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                            .ReservoirList.ReservoirOrNodeByIdentifier[lIrrigationBlock.BlockNodeNumber];
          if (lReservoirData <> nil) then
          begin
            IrrigationXCoordEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.XCoord);
            IrrigationYCoordEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.YCoord);
          end;

          lFound := FALSE;
          lIndexA := 0;
          while ((NOT lFound) AND (lIndexA < NodeNumberCbx.Items.Count)) do
          begin
            if (Integer(NodeNumberCbx.Items.Objects[lIndexA]) = LIrrigationBlock.HydrologyNodeNumber) then
              lFound := TRUE
            else
              lIndexA := lIndexA + 1;
          end;
          if (lFound) then
            NodeNumberCbx.ItemIndex := lIndexA
          else
            NodeNumberCbx.ItemIndex := -1;
        end;

        {lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;

        if (lReservoirList <> nil) then
        begin
          for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
          begin
            lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
            if (lReservoir.ReservoirIdentifier <> 0) and (lReservoir.CatchmentRef <> 0) then
              NodeNumberCbx.Items.AddObject(lReservoir.ReservoirName,TObject(lReservoir.ReservoirIdentifier));
          end;

          for lIndexA := 0 to NodeNumberCbx.Items.Count - 1 do
          begin
            if Integer(NodeNumberCbx.Items.Objects[lIndexA]) = lIrrigationBlock.NodeNumber then
              NodeNumberCbx.ItemIndex := lIndexA;
          end;
        end;}
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockValidator.SaveState: boolean;
const OPNAME = 'TIrrigationBlockValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TIrrigationBlockValidator.ValidateCount(AIrrigationBlock: TIrrigationBlockList);
const OPNAME = 'TIrrigationBlockValidator.ValidateCount';
begin
  try
    with IrrigationBlockDialog do
    begin
      FErrorMessage := '';
      if (NOT AIrrigationBlock.Validate(FErrorMessage, 'ChannelAreaCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      //TODO:ChannelAreaCountEdt.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TIrrigationBlockValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrigationBlockValidator.DoContextValidation';
var
  LReservoir       : IReservoirData;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      case AValidationType of
        dvtIrrigationBlockAll:
          begin
            ValidateAllocatedIrrigationArea;
            ValidateCanalTransportLoss;
            ValidateDescription;
            ValidateEfficiencyFactor;
            ValidateMaxWaterAllocation;
            ValidateName;
            ValidateNodeNumber;
            ValidateNumber;
            ValidateDiversionUpstreamNode;
            ValidateReturnFlowDownstreamNode;
            ValidateFileName;
            ValidateIrrigationDroughtApplicable;
          end;
        dvtIrrigationBlockAllocatedIrrigationArea   : ValidateAllocatedIrrigationArea;
        dvtIrrigationBlockCanalTransportLoss        : ValidateCanalTransportLoss;
        dvtIrrigationBlockDescription               : ValidateDescription;
        dvtIrrigationBlockEfficiencyFactor          : ValidateEfficiencyFactor;
        dvtIrrigationBlockMaxWaterAllocation        : ValidateMaxWaterAllocation;
        dvtIrrigationBlockName                      : ValidateName;
        dvtIrrigationBlockNodeNumber                : ValidateNodeNumber;
        dvtIrrigationBlockNumber                    : ValidateNumber;
        dvtIrrigationBlockDiversionUpstreamNode     : ValidateDiversionUpstreamNode;
        dvtIrrigationBlockReturnFlowDownstreamNode  : ValidateReturnFlowDownstreamNode;
        dvtIrrigationBlockFileName                  : ValidateFileName;
        dvtIrrigationBlockDroughtApplicable         : ValidateIrrigationDroughtApplicable;
      end;
      LReservoir  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                      .ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
      if (LReservoir <> nil) then
      begin
        if (AValidationType = dvtIrrigationBlockAll) then
        begin
          ValidateXCoord(LReservoir);
          ValidateYCoord(LReservoir);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockValidator.IrrigationBlockDialog: TIrrigationBlockDialog;
const OPNAME = 'TIrrigationBlockValidator.IrrigationBlockDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrigationBlockDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateAllocatedIrrigationArea;
const OPNAME = 'TIrrigationBlockValidator.UpdateAllocatedIrrigationArea';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
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

procedure TIrrigationBlockValidator.UpdateCanalTransportLoss;
const OPNAME = 'TIrrigationBlockValidator.UpdateCanalTransportLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
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

procedure TIrrigationBlockValidator.UpdateDescription;
const OPNAME = 'TIrrigationBlockValidator.UpdateDescription';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        DescriptionEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
              DescriptionEdit.FieldProperty.FieldName,
              DescriptionEdit.Text,
              LMessage) then
        begin
          lIrrigationBlock.BlockDescription := DescriptionEdit.Text;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockDescription);
        end
        else
          DescriptionEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateEfficiencyFactor;
const OPNAME = 'TIrrigationBlockValidator.UpdateEfficiencyFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
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

procedure TIrrigationBlockValidator.UpdateMaxWaterAllocation;
const OPNAME = 'TIrrigationBlockValidator.UpdateMaxWaterAllocation';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
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

procedure TIrrigationBlockValidator.UpdateName;
const OPNAME = 'TIrrigationBlockValidator.UpdateName';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NameEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        NameEdit.FieldProperty.FieldName,
                        NameEdit.Text,
                        LMessage) then
        begin
          lIrrigationBlock.BlockName := NameEdit.Text;
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockName);
        end
        else
          NameEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateHydrologyNodeNumber;
const OPNAME = 'TIrrigationBlockValidator.UpdateHydrologyNodeNumber';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NodeNumberCbx.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                NodeNumberCbx.FieldProperty.FieldName,
                IntToStr(Integer(NodeNumberCbx.Items.Objects[NodeNumberCbx.ItemIndex])),
                LMessage) then
        begin
          if NodeNumberCbx.ItemIndex <> -1 then
            lIrrigationBlock.HydrologyNodeNumber := Integer(NodeNumberCbx.Items.Objects[NodeNumberCbx.ItemIndex]);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockNodeNumber);
        end
        else
          NodeNumberCbx.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateBlockNodeNumber;
const OPNAME = 'TIrrigationBlockValidator.UpdateBlockNodeNumber';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NumberEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                NumberEdit.FieldProperty.FieldName,
                NumberEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.BlockNodeNumber := StrToInt( NumberEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockNumber);
        end
        else
          NumberEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateAllocatedIrrigationArea;
const OPNAME = 'TIrrigationBlockValidator.ValidateAllocatedIrrigationArea';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        AllocatedIrrigationAreaEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,AllocatedIrrigationAreaEdit.FieldProperty.FieldName) then
          AllocatedIrrigationAreaEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateCanalTransportLoss;
const OPNAME = 'TIrrigationBlockValidator.ValidateCanalTransportLoss';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        CanalTransportLossEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,CanalTransportLossEdit.FieldProperty.FieldName) then
          CanalTransportLossEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateDescription;
const OPNAME = 'TIrrigationBlockValidator.ValidateDescription';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        DescriptionEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,DescriptionEdit.FieldProperty.FieldName) then
          DescriptionEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TIrrigationBlockValidator.ValidateEfficiencyFactor;
const OPNAME = 'TIrrigationBlockValidator.ValidateEfficiencyFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        EfficiencyFactorEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,EfficiencyFactorEdit.FieldProperty.FieldName) then
          EfficiencyFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateMaxWaterAllocation;
const OPNAME = 'TIrrigationBlockValidator.ValidateMaxWaterAllocation';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        MaxWaterAllocationEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,MaxWaterAllocationEdit.FieldProperty.FieldName) then
          MaxWaterAllocationEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateName;
const OPNAME = 'TIrrigationBlockValidator.ValidateName';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NameEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,NameEdit.FieldProperty.FieldName) then
          NameEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateNodeNumber;
const OPNAME = 'TIrrigationBlockValidator.ValidateNodeNumber';
var
  lIrrigationBlock  : IIrrigationBlock;
  lReservoir        : IReservoirConfigurationData;
  lReservoirList    : IReservoirDataList;
  lIndexA           : integer;
  lCurrentValue     : Integer;
  lItemFound        : Boolean;
begin
  lItemFound := False;
  try
    lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];

    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        FErrorMessage := '';
        if NodeNumberCbx.ItemIndex <> -1 then
        begin
          NodeNumberCbx.ValidationError := '';
          if (lIrrigationBlock.Validate(FErrorMessage,'IrrigationBlockNodeNumber')) then
          begin
            NodeNumberCbx.InValidationError := FALSE;
            NodeNumberCbx.ShowErrorState(FALSE);
          end
          else
          begin
            NodeNumberCbx.InValidationError := TRUE;
            NodeNumberCbx.ValidationError := FErrorMessage;
            NodeNumberCbx.ShowErrorState(TRUE);
          end;

          lCurrentValue := Integer(NodeNumberCbx.Items.Objects[NodeNumberCbx.ItemIndex]);
          lItemFound    := False;
          if lCurrentValue <> -1 then
          begin
            lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
            if (lReservoirList <> nil) then
            begin
              for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
              begin
                lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
                if (lReservoir.ReservoirIdentifier <> 0) and (lReservoir.CatchmentRef <> 0) then
                begin
                  if lCurrentValue = lReservoir.ReservoirIdentifier then
                  begin
                    lItemFound := True;
                    Break;
                  end;
                end;
              end;
            end;
          end;  
        end;

        if not lItemFound then
        begin
          NodeNumberCbx.ValidationError := '';
          if (lIrrigationBlock.Validate(FErrorMessage,'IrrigationHydrologyNodeNumber')) then
          begin
            NodeNumberCbx.InValidationError := FALSE;
            NodeNumberCbx.ShowErrorState(FALSE);
          end
          else
          begin
            NodeNumberCbx.InValidationError := FALSE;
            NodeNumberCbx.ValidationError := FErrorMessage;
            NodeNumberCbx.ShowErrorState(TRUE);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateNumber;
const OPNAME = 'TIrrigationBlockValidator.ValidateNumber';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NumberEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage, NumberEdit.FieldProperty.FieldName) then
          NumberEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.PopulateIrrigationBlockDemandFileCbx;
const OPNAME = 'TIrrigationBlockValidator.PopulateIrrigationBlockDemandFileCbx';
var
  LFileNamesList : TFileNamesList;
  LIrrigationBlockList : IIrrigationBlockList;
  LFileName      : string;
  LIndex         : integer;
begin
  try
    with IrrigationBlockDialog do
    begin
      FileNameCbx.Items.Clear;
      FileNameCbx.Items.Add('');
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
        FileNameCbx.Items.Add(LFileName);
      end;
      LIrrigationBlockList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList;
      for LIndex:= 0 to LIrrigationBlockList.IrrigationBlockCount -1 do
      begin
        LFileName := LIrrigationBlockList.IrrigationBlockByIndex[LIndex].FileName;
        if(LFileName <> '') and (FileNameCbx.Items.IndexOf(LFileName) < 0) then
          FileNameCbx.Items.Add(LFileName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.OnSelectDemandFileNameClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockValidator.OnSelectDemandFileNameClick';
var
  LFileSelector: TOpenDialog;
  LFileName: string;
  lFeature : IIrrigationBlock;
  lMessage : string;
begin
  try
    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Filter  := 'All Files|*.*|(*.ABS)|*.abs|(*.CIR)|*.cir|(*.DEM)|*.dem|(*.IRR)|*.IRR|(*.IRD)|*.ird|(*.URB)|*.urb';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      if LFileSelector.Execute then
      begin
        LFileName := LFileSelector.FileName;
        if(UpperCase(LFileName) <> UpperCase(IrrigationBlockDialog.FileNameCbx.Text)) then
        begin
          lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
          if (lFeature <> nil) then
          begin
            with IrrigationBlockDialog do
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(FileNameCbx.FieldProperty.FieldName,
                                                                    LFileName,lMessage)) then
              begin
                lFeature.FileName := LFileName;
                PopulateIrrigationBlockDemandFileCbx;
                FileNameCbx.ItemIndex := FileNameCbx.Items.IndexOf(lFeature.FileName);
                SetGridGraphBtnState(lFeature);
              end
              else
                FileNameCbx.ValidationError := lMessage;
            end;
          end;
          end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;  
end;

procedure TIrrigationBlockValidator.UpdateDiversionChannelUpstreamNode;
const OPNAME = 'TIrrigationBlockValidator.UpdateDiversionChannelUpstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lIrrBlock      : IIrrigationBlock;
  lMessage       : string;
begin
  try
    lIrrBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;

        if (UpStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(UpStreamNodeCbx.Items.Objects[UpStreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('DiversionChannelUpStreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            if Assigned(lIrrBlock.DiversionChannel) then
            begin
              lIrrBlock.DiversionChannel.UpStreamNodeNumber := lReservoirNr;
              lReservoirNr := lIrrBlock.DiversionChannel.UpStreamNodeNumber;
            end;
            UpStreamNodeCbx.SetFieldIndex(UpStreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtIrrigationBlockDiversionUpstreamNode);
          end
          else
            UpStreamNodeCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateReturnFlowChannelDownstreamNode;
const OPNAME = 'TIrrigationBlockValidator.UpdateReturnFlowChannelDownstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lIrrBlock      : IIrrigationBlock;
  lMessage       : string;
begin
  try
    lIrrBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;
        if (DownStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(DownStreamNodeCbx.Items.Objects[DownStreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'ReturnFlowChannelDownStreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            if Assigned(lIrrBlock.ReturnFlowChannel) then
            begin
              lIrrBlock.ReturnFlowChannel.DownStreamNodeNumber := lReservoirNr;
              lReservoirNr := lIrrBlock.ReturnFlowChannel.DownStreamNodeNumber;
            end;  
            DownStreamNodeCbx.SetFieldIndex(DownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtIrrigationBlockReturnFlowDownstreamNode);
          end
          else
            DownStreamNodeCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateDiversionUpstreamNode;
const OPNAME = 'TIrrigationBlockValidator.ValidateDiversionUpstreamNode';
var
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        FErrorMessage := '';
        if (lIrrigationBlock.Validate(FErrorMessage, 'DiversionUpstreamNode')) then
        begin
          UpStreamNodeCbx.InValidationError := FALSE;
          UpStreamNodeCbx.ShowErrorState(FALSE);
        end
        else
        begin
          UpStreamNodeCbx.InValidationError := FALSE;
          UpStreamNodeCbx.ValidationError := FErrorMessage;
          UpStreamNodeCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateReturnFlowDownstreamNode;
const OPNAME = 'TIrrigationBlockValidator.ValidateReturnFlowDownstreamNode';
var
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        FErrorMessage := '';
        if (lIrrigationBlock.Validate(FErrorMessage, 'ReturnFlowDownstreamNode')) then
        begin
          DownStreamNodeCbx.InValidationError := FALSE;
          DownStreamNodeCbx.ShowErrorState(FALSE);
        end
        else
        begin
          DownStreamNodeCbx.InValidationError := FALSE;
          DownStreamNodeCbx.ValidationError := FErrorMessage;
          DownStreamNodeCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.RePopulateNodes;
const OPNAME = 'TIrrigationBlockValidator.RePopulateNodes';
var
  lReservoirList    : IReservoirDataList;
  lIndexA           : integer;
  lReservoir        : IReservoirConfigurationData;
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      if (lIrrigationBlock <> nil) then
      begin
        with IrrigationBlockDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
              if (lReservoir.ReservoirIdentifier = 0) then
              begin
                if (lIrrigationBlock.ReturnFlowChannel <> nil) then
                  DownStreamNodeCbx.Items.AddObject('(0) ' +
                  UpperCase(lIrrigationBlock.ReturnFlowChannel.SinkName), TObject(lReservoir.ReservoirIdentifier));
                if (lIrrigationBlock.DiversionChannel <> nil) then
                  UpStreamNodeCbx.Items.AddObject('(0) ' +
                  UpperCase(lIrrigationBlock.DiversionChannel.SourceName), TObject(lReservoir.ReservoirIdentifier));
              end
              else
              begin
                if (lReservoir.NodeType <> ntIrrigationNode) then
                begin
                  DownStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                  UpStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=IRRIGATIONBLOCKDEMANDFEATURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=IRRIGATIONBLOCKDEMANDFEATURE');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.SetGridGraphBtnState(AIrrigationBlock : IIrrigationBlock);
const OPNAME = 'TIrrigationBlockValidator.SetGridGraphBtnState';
var
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
  LIndex         : integer;
begin
  try
    IrrigationBlockDialog.FileNameGridBtn.Enabled := False;
    IrrigationBlockDialog.FileNameGraphBtn.Enabled := False;
    if(AIrrigationBlock <> nil) and (AIrrigationBlock.FileName <> '') then
    begin
      LFileNameObject := nil;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        if(UpperCase(AIrrigationBlock.FileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
        begin
          LFileNameObject := LFileNamesList.FileNameObject[LIndex];
          Break;
        end;
      end;
      if(LFileNameObject <> nil) then
      begin
        IrrigationBlockDialog.FileNameGridBtn.Enabled  := LFileNameObject.SavedInDB;
        IrrigationBlockDialog.FileNameGraphBtn.Enabled := LFileNameObject.SavedInDB;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateFileName;
const OPNAME = 'TIrrigationBlockValidator.UpdateFileName';
var
  lIrrigationBlock  : IIrrigationBlock;
  lMessage : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        if(FileNameCbx.Text = '') or
          (FAppModules.FieldProperties.ValidateFieldProperty(FileNameCbx.FieldProperty.FieldName,FileNameCbx.Text,lMessage)) then
        begin
          lIrrigationBlock.FileName  := FileNameCbx.Text;
          if(FileNameCbx.Text = '') then
            FileNameCbx.SetFieldIndex(0)
          else
          begin
            FileNameCbx.SetFieldIndex(FileNameCbx.Items.IndexOf(lIrrigationBlock.FileName));
            DoContextValidation(dvtIrrigationBlockFileName);
          end;
          SetGridGraphBtnState(lIrrigationBlock);
        end
        else
          FileNameCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateFileName;
const OPNAME = 'TIrrigationBlockValidator.ValidateFileName';
var
  lIrrigationBlock  : IIrrigationBlock;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        FErrorMessage := '';
        if (lIrrigationBlock.Validate(FErrorMessage, 'IrrigationBlockFileName')) then
        begin
          FileNameCbx.InValidationError := FALSE;
          FileNameCbx.ValidationError := '';
          FileNameCbx.ShowErrorState(FALSE);
        end
        else
        begin
          FileNameCbx.InValidationError := TRUE;
          FileNameCbx.ValidationError := FErrorMessage;
          FileNameCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.OnDroughtApplicableClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockValidator.OnDroughtApplicableClick';
begin
  try
    UpdateDroughtApplicable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateDroughtApplicable;
const OPNAME = 'TIrrigationBlockValidator.UpdateDroughtApplicable';
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
      with IrrigationBlockDialog do
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

procedure TIrrigationBlockValidator.ValidateIrrigationDroughtApplicable;
const OPNAME = 'TIrrigationBlockValidator.ValidateIrrigationDroughtApplicable';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with IrrigationBlockDialog do
      begin
        NumberEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage, DroughtApplicableChkBox.FieldProperty.FieldName) then
          DroughtApplicableChkBox.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateXCoord(AReservoirData: IReservoirData);
const OPNAME = 'TIrrigationBlockValidator.ValidateXCoord';
begin
  try
    with IrrigationBlockDialog do
    begin
      FErrorMessage := '';
      IrrigationXCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'XCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      IrrigationXCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.ValidateYCoord(AReservoirData: IReservoirData);
const OPNAME = 'TIrrigationBlockValidator.ValidateYCoord';
begin
  try
    with IrrigationBlockDialog do
    begin
      FErrorMessage := '';
      IrrigationYCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'YCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      IrrigationYCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrigationBlockValidator.UpdateXCoord;
const OPNAME = 'TIrrigationBlockValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LIrrigationBlock : IIrrigationBlock;
begin
  try
    if IrrigationBlockDialog.IrrigationXCoordEdit.HasValueChanged then
    begin
      LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                          IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      if (LIrrigationBlock <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[LIrrigationBlock.BlockNodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with IrrigationBlockDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',IrrigationXCoordEdit.Text, lErrorMessage) then
            begin
              IrrigationXCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(IrrigationXCoordEdit.Text);
              IrrigationXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
            end
            else
              IrrigationXCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockValidator.UpdateYCoord;
const OPNAME = 'TIrrigationBlockValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LIrrigationBlock : IIrrigationBlock;
begin
  try
    if IrrigationBlockDialog.IrrigationYCoordEdit.HasValueChanged then
    begin
      LIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                          IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      if (LIrrigationBlock <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[LIrrigationBlock.BlockNodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with IrrigationBlockDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',IrrigationYCoordEdit.Text, lErrorMessage) then
            begin
              IrrigationYCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(IrrigationYCoordEdit.Text);
              IrrigationYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
            end
            else
              IrrigationYCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.

